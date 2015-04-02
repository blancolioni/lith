with Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;

with Lith.Environment;
with Lith.Parser;
with Lith.Objects.Symbols;

with Lith.Machine.SECD;

with Lith.Objects.Numbers;

package body Lith.Machine is

   Trace_Machine : constant Boolean := False;
   Trace_GC      : constant Boolean := False;

   function Get
     (Machine : Root_Lith_Machine'Class;
      Pair    : Lith.Objects.Object)
      return Object_Pair
     with Pre => Lith.Objects.Is_Pair (Pair)
     and then not Machine.Free (Lith.Objects.To_Address (Pair));

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Lith.Objects.External_Object_Interface'Class,
        External_Object_Access);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Machine  : in out Root_Lith_Machine'Class;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use type Lith.Objects.Object;
      Result : Lith.Objects.Object := Machine.Free_List;
   begin

      if Result = Lith.Objects.Nil then
         Machine.G1 := Car;
         Machine.G2 := Cdr;
         Machine.GC;
         Result := Machine.Free_List;
         Machine.G1 := Lith.Objects.Nil;
         Machine.G2 := Lith.Objects.Nil;
         if Result = Lith.Objects.Nil then
            raise Constraint_Error with "out of memory";
         end if;
      end if;

      declare
         Address : constant Lith.Objects.Cell_Address :=
                     Lith.Objects.To_Address (Result);
      begin
         pragma Assert (Machine.Free (Address));
         Machine.Free_List := Machine.Core (Address).Cdr;
         Machine.Core (Address) := (Car, Cdr);
         Machine.Free (Address) := False;
         Machine.Source_Refs (Address) := Machine.Current_Context;
         Machine.Alloc_Count := Machine.Alloc_Count + 1;
         Machine.Allocations := Machine.Allocations + 1;
         return Result;
      end;
   end Allocate;

   ---------
   -- Car --
   ---------

   function Car
     (Machine : in out Root_Lith_Machine'Class)
      return Lith.Objects.Object
   is
   begin
      return Machine.Car (Machine.Pop);
   end Car;

   ---------
   -- Car --
   ---------

   overriding function Car
     (Machine : Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Lith.Objects.Object
   is
   begin
      return Get (Machine, Value).Car;
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr
     (Machine : in out Root_Lith_Machine'Class)
      return Lith.Objects.Object
   is
   begin
      return Machine.Cdr (Machine.Pop);
   end Cdr;

   ---------
   -- Cdr --
   ---------

   overriding function Cdr
     (Machine : Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Lith.Objects.Object
   is
   begin
      return Get (Machine, Value).Cdr;
   end Cdr;

   ----------
   -- Cons --
   ----------

   overriding procedure Cons
     (Machine : in out Root_Lith_Machine)
   is
      Cdr : constant Lith.Objects.Object := Machine.Pop;
      Car : constant Lith.Objects.Object := Machine.Pop;
      T   : constant Lith.Objects.Object :=
              Machine.Cons (Car, Cdr);
   begin
      Machine.Push (T);
   end Cons;

   ----------
   -- Cons --
   ----------

   overriding function Cons
     (Machine  : in out Root_Lith_Machine;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object
   is
   begin
      return Machine.Allocate (Car, Cdr);
   end Cons;

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size : Positive)
      return Lith_Machine
   is
      use Lith.Objects;
      Machine : constant Lith_Machine := new Root_Lith_Machine;
      Last_Address : constant Cell_Address :=
                       Cell_Address (Core_Size - 1);
   begin
      Machine.Core :=
        new Core_Memory_Type (0 .. Last_Address);
      Machine.Marked :=
        new Memory_Tag_Type (0 .. Last_Address);
      Machine.Free :=
        new Memory_Tag_Type (0 .. Last_Address);
      Machine.Source_Refs :=
        new Memory_Source_Reference_Type (0 .. Last_Address);

      Machine.Free.all := (others => True);
      Machine.Marked.all := (others => False);
      Machine.Source_Refs.all := (others => (0, 0));

      for I in Machine.Core'Range loop
         Machine.Core (I) :=
           (Car => Nil, Cdr => To_Object (I + 1));
      end loop;
      Machine.Core (Machine.Core'Last) := (Nil, Nil);
      Machine.Free_List := To_Object (Cell_Address'(0));
      Machine.Stack := Nil;
      Machine.Control := Nil;
      Machine.Dump := Nil;
      Machine.Handlers := Nil;
      Machine.Alloc_Count := 0;
      Machine.Alloc_Limit := Natural (Machine.Core'Length) - 2000;

      return Machine;
   end Create;

   -------------------------------
   -- Create_External_Reference --
   -------------------------------

   overriding function Create_External_Reference
     (Machine : in out Root_Lith_Machine;
      External : Lith.Objects.External_Object_Interface'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Address : External_Object_Address := 0;
      New_Item : constant External_Object_Access :=
                   new Lith.Objects.External_Object_Interface'Class'
                     (External);
      New_Entry : constant External_Object_Record :=
                    (External_Object => New_Item,
                     Free            => False,
                     Marked          => False);
   begin
      for I in 1 .. Machine.External_Objects.Last_Index loop
         if Machine.External_Objects (I).Free then
            Address := I;
            exit;
         end if;
      end loop;

      if Address = 0 then
         Machine.External_Objects.Append (New_Entry);
         Address := Machine.External_Objects.Last_Index;
      else
         Machine.External_Objects (Address) := New_Entry;
      end if;

      return Lith.Objects.To_Object (Address);
   end Create_External_Reference;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Machine     : in out Root_Lith_Machine;
      Expression  : Lith.Objects.Object;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use Ada.Calendar;
      Top : constant Boolean := not Machine.Evaluating;
   begin
      if not Machine.Evaluating then
         Machine.Evaluating := True;
         Machine.Start_Eval := Ada.Calendar.Clock;

         Machine.Make_List
           ((Lith.Objects.To_Object
            (Lith.Objects.Symbols.Get_Symbol
                 ("global-error-handler")),
            Lith.Objects.Nil, Lith.Objects.Nil));
         Machine.Push (Lith.Objects.Nil);
         Machine.Cons;
         Machine.Handlers := Machine.Pop;

      end if;

      Machine.Environment := Environment;
      Machine.Control := Machine.Cons (Expression, Lith.Objects.Nil);
      Lith.Machine.SECD.Evaluate (Machine);

      if Top then
         Machine.Eval_Time := Machine.Eval_Time + (Clock - Machine.Start_Eval);
         Machine.Evaluating := False;
      end if;
      return Machine.Pop;
   end Evaluate;

   --------------------
   -- Finish_Profile --
   --------------------

   procedure Finish_Profile
     (Machine : in out Root_Lith_Machine'Class)
   is

      procedure Process (Key  : Lith.Objects.Symbol_Type;
                         Info : Profile_Info_Record);

      -------------
      -- Process --
      -------------

      procedure Process (Key  : Lith.Objects.Symbol_Type;
                         Info : Profile_Info_Record)
      is
         Target : Profile_Result_Lists.Cursor :=
                    Machine.Procedure_Result.First;
      begin
         while Profile_Result_Lists.Has_Element (Target) loop
            declare
               Result_Item : Profile_Result_Record renames
                               Profile_Result_Lists.Element (Target);
            begin
               if Result_Item.Info.Hit_Count < Info.Hit_Count then
                  Machine.Procedure_Result.Insert
                    (Before   => Target,
                     New_Item => ((0, 0),
                                  Lith.Objects.To_Object (Key),
                                  Info));
                  exit;
               end if;
               Profile_Result_Lists.Next (Target);
            end;
         end loop;

         if not Profile_Result_Lists.Has_Element (Target) then
            Machine.Procedure_Result.Append
              (((0, 0),
               Lith.Objects.To_Object (Key),
               Info));
         end if;
      end Process;

   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Profile complete");
      Machine.Profiling := False;

      Machine.Procedure_Profile.Iterate (Process'Access);

      for Position in Machine.Source_Profile.Iterate loop
         declare
            Info : Profile_Info_Record renames
                     Profile_Source_Maps.Element (Position);
            Target : Profile_Result_Lists.Cursor :=
                       Machine.Source_Result.First;
         begin
            while Profile_Result_Lists.Has_Element (Target) loop
               declare
                  Result_Item : Profile_Result_Record renames
                                  Profile_Result_Lists.Element (Target);
               begin
                  if Result_Item.Info.Hit_Count < Info.Hit_Count then
                     Machine.Source_Result.Insert
                       (Before   => Target,
                        New_Item => (Profile_Source_Maps.Key (Position),
                                     Lith.Objects.No_Value,
                                     Info));
                     exit;
                  end if;
                  Profile_Result_Lists.Next (Target);
               end;
            end loop;

            if not Profile_Result_Lists.Has_Element (Target) then
               Machine.Source_Result.Append
                 ((Profile_Source_Maps.Key (Position),
                  Lith.Objects.No_Value,
                  Info));
            end if;
         end;
      end loop;
   end Finish_Profile;

   --------
   -- GC --
   --------

   procedure GC
     (Machine : in out Root_Lith_Machine'Class)
   is
   begin

      if Trace_GC then
         Ada.Wide_Wide_Text_IO.Put_Line ("Garbage collecting ...");
      end if;
      declare
         use Ada.Calendar;
         Start : constant Time := Clock;
         Old_Alloc_Count : constant Natural := Machine.Alloc_Count;
      begin
         Machine.Alloc_Count := 0;
         Machine.Free_List := Lith.Objects.Nil;
         Machine.Free.all := (others => True);
         for I in 1 .. Machine.External_Objects.Last_Index loop
            Machine.External_Objects (I).Marked := False;
         end loop;

         Lith.Environment.Mark (Machine);
         Machine.Mark (Machine.Stack);
         Machine.Mark (Machine.Environment);
         Machine.Mark (Machine.Control);
         Machine.Mark (Machine.Dump);
         Machine.Mark (Machine.Handlers);
         Machine.Mark (Machine.R1);
         Machine.Mark (Machine.R2);
         Machine.Mark (Machine.G1);
         Machine.Mark (Machine.G2);

         for I in Machine.Core'Range loop
            if Machine.Marked (I) then
               Machine.Marked (I) := False;
               Machine.Free (I) := False;
               Machine.Alloc_Count := Machine.Alloc_Count + 1;
            else
               Machine.Core (I).Cdr := Machine.Free_List;
               Machine.Free_List := Lith.Objects.To_Object (I);
            end if;
         end loop;

         for I in 1 .. Machine.External_Objects.Last_Index loop
            declare
               Item : External_Object_Record renames
                        Machine.External_Objects (I);
            begin
               if Item.Marked then
                  Item.Marked := False;
               elsif not Item.Free then
                  Item.External_Object.Finalize (Machine);
                  Free (Item.External_Object);
                  Item.Free := True;
               end if;
            end;

            Machine.External_Objects (I).Marked := False;
         end loop;

         Machine.Marked.all := (others => False);

         if Trace_GC then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("GC freed"
               & Integer'Wide_Wide_Image
                 (Old_Alloc_Count - Machine.Alloc_Count)
               & " cells in"
               & Duration'Wide_Wide_Image ((Clock - Start) * 1000.0)
               & "ms");
         end if;

         Machine.Collections := Machine.Collections +
           (Old_Alloc_Count - Machine.Alloc_Count);
         Machine.GC_Time := Machine.GC_Time + (Clock - Start);
         Machine.GC_Count := Machine.GC_Count + 1;
      end;
   end GC;

   ---------
   -- Get --
   ---------

   function Get
     (Machine : Root_Lith_Machine'Class;
      Pair    : Lith.Objects.Object)
      return Object_Pair
   is
   begin
      return Machine.Core (Lith.Objects.To_Address (Pair));
   end Get;

   -------------------------
   -- Get_External_Object --
   -------------------------

   overriding function Get_External_Object
     (Machine : Root_Lith_Machine;
      Item    : Lith.Objects.Object)
      return access Lith.Objects.External_Object_Interface'Class
   is
      Address : constant Real_External_Address :=
                  Lith.Objects.To_External_Object_Address (Item);
   begin
      return Machine.External_Objects (Address).External_Object;
   end Get_External_Object;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Machine  : in out Root_Lith_Machine'Class;
      Item     : Lith.Objects.Object)
   is
      use Lith.Objects;
   begin
      if Is_Address (Item) then
         if Machine.Source_Refs (To_Address (Item)).Line /= 0 then
            declare
               use Profile_Source_Maps;
               Ref : constant Source_Reference :=
                       Machine.Source_Refs (To_Address (Item));
               Position : constant Cursor := Machine.Source_Profile.Find (Ref);
            begin
               if Has_Element (Position) then
                  Machine.Source_Profile.Replace_Element
                    (Position,
                     (Hit_Count => Element (Position).Hit_Count + 1));
               else
                  Machine.Source_Profile.Insert (Ref, (Hit_Count => 1));
               end if;
            end;
         end if;
      elsif Is_Symbol (Item)
        and then (Is_Function (Item)
                  or else Lith.Objects.Symbols.Is_Predefined
                    (To_Symbol (Item))
                  or else Lith.Environment.Get (To_Symbol (Item), No_Value)
                  /= No_Value)
      then
         declare
            use Procedure_Profile_Maps;
            Ref      : constant Symbol_Type := To_Symbol (Item);
         begin
            if Machine.Procedure_Profile.Contains (Ref) then
               declare
                  Elem : Profile_Info_Record :=
                           Machine.Procedure_Profile.Element (Ref);
               begin
                  Elem.Hit_Count := Elem.Hit_Count + 1;
                  Machine.Procedure_Profile.Replace (Ref, Elem);
               end;
            else
               Machine.Procedure_Profile.Insert (Ref, (Hit_Count => 1));
            end if;
         end;
      end if;
   end Hit;

   ----------
   -- Load --
   ----------

   overriding function Load (Machine : in out Root_Lith_Machine;
                             Path    : Wide_Wide_String)
                             return Boolean
   is
   begin
      Lith.Parser.Parse_File
        (Machine,
         Ada.Characters.Conversions.To_String (Path));
      return True;
   exception
      when E : others =>
         Ada.Wide_Wide_Text_IO.Put_Line
           (Ada.Wide_Wide_Text_IO.Standard_Error,
            "Cannot load "
            & Path
            & ": "
            & Ada.Characters.Conversions.To_Wide_Wide_String
              (Ada.Exceptions.Exception_Message (E)));
         return False;
   end Load;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Machine : in out Root_Lith_Machine;
      Start   : in     Lith.Objects.Object)
   is

      procedure Check (X : Lith.Objects.Object);

      -----------
      -- Check --
      -----------

      procedure Check (X : Lith.Objects.Object) is
      begin
         if Lith.Objects.Is_Pair (X) then
            declare
               Addr : constant Lith.Objects.Cell_Address :=
                        Lith.Objects.To_Address (X);
            begin
               if not Machine.Marked (Addr) then
                  Machine.Mark (X);
               end if;
            end;
         elsif Lith.Objects.Is_External_Object (X) then
            declare
               Addr : constant Real_External_Address :=
                        Lith.Objects.To_External_Object_Address (X);
            begin
               if not Machine.External_Objects (Addr).Marked then
                  Machine.Mark (X);
               end if;
            end;
         end if;
      end Check;

   begin
      if Lith.Objects.Is_Pair (Start) then
         declare
            Address : constant Lith.Objects.Cell_Address :=
                        Lith.Objects.To_Address (Start);
         begin
            Machine.Marked (Address) := True;
            Check (Machine.Core (Address).Car);
            Check (Machine.Core (Address).Cdr);
         end;
      elsif Lith.Objects.Is_External_Object (Start) then
         declare
            Addr : constant Real_External_Address :=
                     Lith.Objects.To_External_Object_Address (Start);
         begin
            Machine.External_Objects (Addr).Marked := True;
            Machine.External_Objects (Addr).External_Object.Mark (Machine);
         end;
      end if;
   end Mark;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (Machine : in out Root_Lith_Machine;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      SP     : constant Lith.Objects.Object :=
                 (case Stack is
                     when Primary   => Machine.Stack,
                     when Secondary => Machine.Dump);
      Result : constant Lith.Objects.Object :=
                 Machine.Car (SP);
   begin
      if Trace_Machine then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("machine: pop " & Machine.Show (Result));
      end if;
      case Stack is
         when Primary =>
            Machine.Stack := Machine.Cdr (Machine.Stack);
         when Secondary =>
            Machine.Dump := Machine.Cdr (Machine.Dump);
      end case;
      return Result;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary)
   is
      use all type Lith.Objects.Stack_Type;
   begin
      if Trace_Machine then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("machine: push " & Machine.Show (Value));
      end if;
      case Stack is
         when Primary =>
            Machine.Stack := Allocate (Machine, Value, Machine.Stack);
         when Secondary =>
            Machine.Dump := Allocate (Machine, Value, Machine.Dump);
      end case;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Machine : in out Root_Lith_Machine;
      Symbol_Name : Wide_Wide_String)
   is
      use Lith.Objects, Lith.Objects.Symbols;
   begin
      Machine.Push
        (To_Object (Get_Symbol (Symbol_Name)));
   end Push;

   -------------------
   -- Report_Memory --
   -------------------

   procedure Report_Memory
     (Machine : Root_Lith_Machine'Class)
   is

      use Lith.Objects;

      function Hex_Image (Addr : Cell_Address) return String;
      pragma Unreferenced (Hex_Image);

      function Hex_Image (Addr : Cell_Address) return String is

         Tmp     : Cell_Address := Addr;
         Result  : String (1 .. 8);

         function Hex_Digit (Item : Cell_Address) return Character
           with Pre => Item <= 15;

         ---------------
         -- Hex_Digit --
         ---------------

         function Hex_Digit (Item : Cell_Address) return Character is
            Hex_Digits : constant String := "0123456789ABCDEF";
         begin
            return Hex_Digits (Positive (Item + 1));
         end Hex_Digit;

      begin
         for I in reverse Result'Range loop
            Result (I) := Hex_Digit (Tmp mod 16);
            Tmp := Tmp / 16;
         end loop;
         if Result (1 .. 4) = "0000" then
            return Result (5 .. 8);
         else
            return Result (1 .. 4) & " " & Result (5 .. 8);
         end if;
      end Hex_Image;

   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Total number of cells:"
         & Cell_Address'Wide_Wide_Image (Machine.Core'Length));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Allocated cell count: "
         & Natural'Wide_Wide_Image (Machine.Alloc_Count));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Free cell count: "
         & Natural'Wide_Wide_Image
           (Natural (Machine.Core'Length
            - Machine.Alloc_Count)));
   end Report_Memory;

   --------------------
   -- Report_Profile --
   --------------------

   procedure Report_Profile
     (Machine           : in out Root_Lith_Machine'Class;
      Procedure_Profile : Boolean;
      Max_Lines         : Natural := 20)
   is
      Count : Natural := 0;
   begin
      if Procedure_Profile then
         for Rec of Machine.Procedure_Result loop
            Count := Count + 1;
            exit when Count > Max_Lines;

            Ada.Wide_Wide_Text_IO.Put_Line
              (Natural'Wide_Wide_Image
                 (Rec.Info.Hit_Count)
               & "    "
               & Machine.Show (Rec.Proc));
         end loop;
      else
         for Rec of Machine.Source_Result loop
            Count := Count + 1;
            exit when Count > Max_Lines;

            Ada.Wide_Wide_Text_IO.Put_Line
              (Natural'Wide_Wide_Image
                 (Rec.Info.Hit_Count)
               & "    "
               & Machine.Show (Rec.Reference));
         end loop;
      end if;
   end Report_Profile;

   ------------------
   -- Report_State --
   ------------------

   overriding procedure Report_State
     (Machine : in out Root_Lith_Machine)
   is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (" S: " & Machine.Show (Machine.Stack));
      Ada.Wide_Wide_Text_IO.Put_Line
        (" E: " & Machine.Show (Machine.Environment));
      Ada.Wide_Wide_Text_IO.Put_Line
        (" C: " & Machine.Show (Machine.Control));
      Ada.Wide_Wide_Text_IO.Put_Line
        (" D: " & Machine.Show (Machine.Dump));
      Ada.Wide_Wide_Text_IO.Put_Line
        (" H: " & Machine.Show (Machine.Handlers));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("GC:"
         & Natural'Wide_Wide_Image (Machine.GC_Count)
         & " @"
         & Natural'Wide_Wide_Image (Natural (Machine.GC_Time * 1000.0))
         & "ms");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Eval:"
         & Natural'Wide_Wide_Image (Natural (Machine.Eval_Time * 1000.0))
         & "ms");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Allocated cells:"
         & Natural'Wide_Wide_Image (Machine.Allocations));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Reclaimed cells:"
         & Natural'Wide_Wide_Image (Machine.Collections));

   end Report_State;

   -------------
   -- Set_Car --
   -------------

   overriding procedure Set_Car
     (Machine : in out Root_Lith_Machine;
      Pair    : in     Lith.Objects.Object;
      New_Car : in Lith.Objects.Object)
   is
      Address : constant Lith.Objects.Cell_Address :=
                  Lith.Objects.To_Address (Pair);
   begin
      Machine.Core (Address).Car := New_Car;
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   overriding procedure Set_Cdr
     (Machine : in out Root_Lith_Machine;
      Pair    : in     Lith.Objects.Object;
      New_Cdr : in Lith.Objects.Object)
   is
      Address : constant Lith.Objects.Cell_Address :=
                  Lith.Objects.To_Address (Pair);
   begin
      Machine.Core (Address).Cdr := New_Cdr;
   end Set_Cdr;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Machine   : in out Root_Lith_Machine;
      File_Name : Wide_Wide_String;
      Line      : Natural)
   is
      Name : constant String :=
               Ada.Characters.Conversions.To_String (File_Name);
      File : File_Id;
   begin
      if Machine.Source_Files.Contains (Name) then
         File := Machine.Source_Files.Element (Name);
      else
         Machine.Source_File_Names.Append (File_Name);
         File := Machine.Source_File_Names.Last_Index;
         Machine.Source_Files.Insert (Name, File);
      end if;
      Machine.Current_Context := (File, Line_Number (Line));
   end Set_Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Machine : in out Root_Lith_Machine'Class;
      Item    : Lith.Objects.Object)
   is
      use Lith.Objects;
   begin
      if Is_Address (Item)
        and then Machine.Source_Refs (To_Address (Item)).Line /= 0
      then
         Machine.Current_Context :=
           Machine.Source_Refs (To_Address (Item));
      end if;
   end Set_Context;

   ----------
   -- Show --
   ----------

   function Show (Machine : Root_Lith_Machine'Class;
                  Ref     : Source_Reference)
                  return Wide_Wide_String
   is
      function Line_Name return Wide_Wide_String;
      function Source_Name return Wide_Wide_String;

      ---------------
      -- Line_Name --
      ---------------

      function Line_Name return Wide_Wide_String is
         Result : constant Wide_Wide_String :=
                    Line_Number'Wide_Wide_Image (Ref.Line);
      begin
         return Result (2 .. Result'Last);
      end Line_Name;

      -----------------
      -- Source_Name --
      -----------------

      function Source_Name return Wide_Wide_String is
         Path : constant String :=
                  Ada.Characters.Conversions.To_String
                    (Machine.Source_File_Names (Ref.File));
         File : constant String :=
                  Ada.Directories.Simple_Name (Path);
      begin
         return Ada.Characters.Conversions.To_Wide_Wide_String
           (File);
      end Source_Name;

   begin
      if Ref.File = 0 then
         return "no source reference";
      elsif Ref.File = 0 then
         return Source_Name & ":";
      else
         return Source_Name & ":" & Line_Name & ":";
      end if;
   end Show;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Wide_Wide_String
   is
      use Lith.Objects;

      function Is_List return Boolean;

      function List_Image
        (Current : Object)
         return Wide_Wide_String;

      function Hex_Image
        (Value : Natural)
         return Wide_Wide_String;

      function Large_Integer_Image
        (Value : Object)
         return Wide_Wide_String;

      function String_Image
        (Start : Object)
         return Wide_Wide_String;

      ---------------
      -- Hex_Image --
      ---------------

      function Hex_Image
        (Value : Natural)
         return Wide_Wide_String
      is
         Hex_Ds : constant Wide_Wide_String :=
                    "0123456789abcdef";

         function Hex (D : Natural) return Wide_Wide_Character
         is (Hex_Ds (D + 1));

      begin
         if Value < 256 then
            return Hex (Value / 16) & Hex (Value mod 16);
         else
            return Hex_Image (Value / 256) & Hex_Image (Value mod 256);
         end if;
      end Hex_Image;

      -------------
      -- Is_List --
      -------------

      function Is_List return Boolean is
         It : Object := Value;
      begin
         while Is_Pair (It) loop
            It := Machine.Cdr (It);
         end loop;
         return It = Nil;
      end Is_List;

      -------------------------
      -- Large_Integer_Image --
      -------------------------

      function Large_Integer_Image
        (Value : Object)
         return Wide_Wide_String
      is
         use Ada.Strings.Wide_Wide_Unbounded;
         Acc : Unbounded_Wide_Wide_String;
         Base : constant Object := To_Object (Integer'(10));
         Stop : constant Object := To_Object (Integer'(0));
      begin
         Machine.Push (Value);
         while Machine.Top /= Stop loop
            Machine.Push (Base);
            Machine.Swap;
            Lith.Objects.Numbers.Exact_Divide (Machine);
            if not Is_Integer (Machine.Cadr (Machine.Top)) then
               raise Evaluation_Error with
                 "bad large integer: " &
                 Ada.Characters.Conversions.To_String
                 (List_Image (Value))
                 & "; expected an integer element but found "
                 & Ada.Characters.Conversions.To_String
                 (List_Image (Machine.Top));

            end if;
            declare
               Partial : constant Object := Machine.Pop;
               Ch_Pos : constant Natural :=
                          To_Integer (Machine.Cadr (Partial));
            begin
               Acc := Wide_Wide_Character'Val (Ch_Pos + 48) & Acc;
               Machine.Push (Machine.Car (Partial));
            end;
         end loop;

         Machine.Drop;

         return To_Wide_Wide_String (Acc);

      end Large_Integer_Image;

      ----------------
      -- List_Image --
      ----------------

      function List_Image
        (Current : Object)
         return Wide_Wide_String
      is
      begin
         if Is_Pair (Current) then
            if Is_Pair (Machine.Cdr (Current)) then
               return Machine.Show (Machine.Car (Current))
                 & " " & List_Image (Machine.Cdr (Current));
            else
               return Machine.Show (Machine.Car (Current));
            end if;
         else
            return "";
         end if;
      end List_Image;

      ------------------
      -- String_Image --
      ------------------

      function String_Image
        (Start : Object)
         return Wide_Wide_String
      is
      begin
         if Start = Nil then
            return "";
         elsif Is_Character (Machine.Car (Start)) then
            return To_Character (Machine.Car (Start))
              & String_Image (Machine.Cdr (Start));
         else
            raise Constraint_Error
              with "String contains non-character";
         end if;
      end String_Image;

   begin
      if Value = Nil then
         return "()";
      elsif Value = True_Value then
         return "#t";
      elsif Value = False_Value then
         return "#f";
      elsif Value = No_Value then
         return "";
      elsif Is_Integer (Value) then
         return Ada.Strings.Wide_Wide_Fixed.Trim
           (Integer'Wide_Wide_Image (To_Integer (Value)),
            Ada.Strings.Left);
      elsif Is_Symbol (Value) then
         return Lith.Objects.Symbols.Get_Name (To_Symbol (Value));
      elsif Is_Character (Value) then
         declare
            Ch : constant Wide_Wide_Character :=
                   To_Character (Value);
         begin
            if Ada.Wide_Wide_Characters.Handling.Is_Graphic (Ch) then
               return "#\" & Ch;
            else
               return "#\x"
                 & Hex_Image
                 (Wide_Wide_Character'Pos (Ch));
            end if;
         end;
      elsif Is_Function (Value) then
         return Lith.Objects.Hex_Image (Value);
      elsif Is_Apply (Value) then
         return "apply" & Integer'Wide_Wide_Image (-Argument_Count (Value));
      elsif Is_External_Object (Value) then
         return Machine.Get_External_Object (Value).Print (Machine);
      elsif Is_Pair (Value) then
         if Machine.Car (Value) = String_Value then
            return '"' & String_Image (Machine.Cdr (Value)) & '"';
         elsif True
           and then Machine.Car (Value) = Large_Integer_Value
         then
            return Large_Integer_Image (Value);
         elsif Is_List then
            return "(" & List_Image (Value) & ")";
         else
            declare
               Car : constant Object := Machine.Car (Value);
               Cdr : constant Object := Machine.Cdr (Value);
            begin
               return "(" & Machine.Show (Car) & " . "
                 & Machine.Show (Cdr) & ")";
            end;
         end if;
      else
         return "<error: unknown object type ["
           & Hex_Image (Value)
           & "]";
      end if;
   end Show;

   -------------------
   -- Start_Profile --
   -------------------

   procedure Start_Profile
     (Machine : in out Root_Lith_Machine'Class)
   is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Start profiling ...");
      Machine.Source_Profile.Clear;
      Machine.Procedure_Profile.Clear;
      Machine.Source_Result.Clear;
      Machine.Procedure_Result.Clear;
      Machine.Profiling := True;
   end Start_Profile;

   ---------
   -- Top --
   ---------

   overriding function Top
     (Machine : Root_Lith_Machine;
      Index   : Positive := 1;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary)
      return Lith.Objects.Object
   is
      use all type Lith.Objects.Stack_Type;

      SP     : constant Lith.Objects.Object :=
                 (case Stack is
                     when Primary   => Machine.Stack,
                     when Secondary => Machine.Dump);

      function Get_Nth
        (From : Lith.Objects.Object;
         N    : Positive)
         return Lith.Objects.Object
      is (if N = 1
          then Machine.Car (From)
          else Get_Nth (Machine.Cdr (From), N - 1));

   begin
      return Get_Nth (SP, Index);
   end Top;

end Lith.Machine;

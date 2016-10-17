with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;
with Ada.Text_IO;

with Lith.Environment;
with Lith.Parser;
with Lith.Objects.Symbols;

with Lith.Machine.SECD;

with Lith.Memory.Tests;

with Lith.Options;

package body Lith.Machine is

--     function Get
--       (Machine : Root_Lith_Machine'Class;
--        Pair    : Lith.Objects.Object)
--        return Object_Pair
--       with Pre => Lith.Objects.Is_Pair (Pair)
--       and then not Machine.Is_Free (Lith.Objects.To_Address (Pair));

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Lith.Objects.External_Object_Interface'Class,
        External_Object_Access);

   --------------
   -- Add_Hook --
   --------------

   overriding procedure Add_Hook
     (Machine : in out Root_Lith_Machine;
      Name    : String;
      Hook    : Lith.Objects.Evaluation_Hook)
   is
   begin
      Machine.Evaluation_Hooks.Insert (Name, Hook);
   end Add_Hook;

   --------------
   -- After_GC --
   --------------

   overriding procedure After_GC (Machine : in out Root_Lith_Machine) is
   begin
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
      end loop;
   end After_GC;

   --------------
   -- Argument --
   --------------

   overriding function Argument
     (Machine : Root_Lith_Machine;
      Index   : Positive)
      return Lith.Objects.Object
   is
   begin
      return Machine.Args (Index);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   overriding function Argument_Count
     (Machine : Root_Lith_Machine)
      return Natural
   is
   begin
      return Machine.Arg_Count;
   end Argument_Count;

   ---------------
   -- Before_GC --
   ---------------

   overriding procedure Before_GC
     (Machine : in out Root_Lith_Machine)
   is
      procedure Mark (Value : in out Lith.Objects.Object);

      ----------
      -- Mark --
      ----------

      procedure Mark (Value : in out Lith.Objects.Object) is
      begin
         Lith.Memory.Mark (Machine.Core, Value);
      end Mark;
   begin

      for I in Machine.R'Range loop
         Mark (Machine.R (I));
      end loop;

      for I in Machine.Args'Range loop
         Mark (Machine.Args (I));
      end loop;

      Mark (Machine.Stack);
      Mark (Machine.Environment);
      Mark (Machine.Control);
      Mark (Machine.Dump);
      Mark (Machine.Handlers);

      for I in 1 .. Machine.External_Objects.Last_Index loop
         declare
            Item : External_Object_Record renames
                     Machine.External_Objects (I);
         begin
            Item.Marked := False;
         end;
      end loop;

      Lith.Environment.Mark (Machine);

   end Before_GC;

   ---------------
   -- Call_Hook --
   ---------------

   overriding function Call_Hook
     (Machine   : in out Root_Lith_Machine;
      Name      : String;
      Arguments : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      Key : constant String := Name;
   begin
      if Machine.Evaluation_Hooks.Contains (Key) then
         return Machine.Evaluation_Hooks.Element (Key).Call (Arguments);
      else
         return Lith.Objects.No_Value;
      end if;
   end Call_Hook;

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
      return Lith.Memory.Car
        (Machine.Core,
         Lith.Objects.To_Address (Value));
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
      return Lith.Memory.Cdr
        (Machine.Core,
         Lith.Objects.To_Address (Value));
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
      if Lith.Options.Trace_Stack then
         Ada.Text_IO.Put_Line
           ("machine: cons --> " & Machine.Show (T));
      end if;
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
      return Lith.Memory.Allocate (Machine.Core, Car, Cdr);
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
      Machine.Core_Size := Core_Size;
      Machine.Core :=
        Lith.Memory.Create (Lith.Objects.Cell_Address (Core_Size),
                            Lith.Memory.GC_Callback (Machine));

      if False then
         Lith.Memory.Tests.Self_Test (Machine.Core);
      end if;

      Machine.Source_Refs :=
        new Memory_Source_Reference_Type (0 .. Last_Address);

      Machine.Source_Refs.all := (others => (0, 0));

      Machine.Stack := Nil;
      Machine.Control := Nil;
      Machine.Dump := Nil;
      Machine.Handlers := Nil;

      return Machine;
   end Create;

   --------------------
   -- Create_Binding --
   --------------------

   overriding procedure Create_Binding
     (Machine : in out Root_Lith_Machine;
      Name    : Lith.Objects.Symbol_Type;
      Value   : Lith.Objects.Object)
   is
   begin
      Machine.Push (Value);
      Machine.Push (Name);
      Machine.Swap;
      Machine.Cons;
      Machine.Push (Machine.Car (Machine.Environment));
      Machine.Cons;
      Machine.Push (Machine.Cdr (Machine.Environment));
      Machine.Cons;
      Machine.Environment := Machine.Pop;
   end Create_Binding;

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
      Expression  : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use Ada.Calendar;
      Top : constant Boolean := not Machine.Evaluating;
   begin
      if not Machine.Evaluating then
         Machine.Evaluating := True;
         Machine.Start_Eval := Ada.Calendar.Clock;

--           Machine.Make_List
--             ((Lith.Objects.To_Object
--              (Lith.Objects.Symbols.Get_Symbol
--                   ("global-error-handler")),
--              Lith.Objects.Nil, Lith.Objects.Nil));
--           Machine.Push (Lith.Objects.Nil);
--           Machine.Cons;
--           Machine.Handlers := Machine.Pop;

      end if;

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
      Ada.Text_IO.Put_Line
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

   -------------
   -- Is_Free --
   -------------

   function Is_Free (Machine : Root_Lith_Machine'Class;
                     Address : Lith.Objects.Cell_Address)
                     return Boolean
   is
   begin
      return not Lith.Memory.Valid (Machine.Core, Address);
   end Is_Free;

   ----------
   -- Load --
   ----------

   overriding function Load (Machine : in out Root_Lith_Machine;
                             Path    : String)
                             return Boolean
   is
   begin
      Lith.Parser.Parse_File
        (Machine, Path);
      return True;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot load "
            & Path
            & ": "
            & Ada.Exceptions.Exception_Message (E));
         return False;
   end Load;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Machine : in out Root_Lith_Machine;
      Start   : in out Lith.Objects.Object)
   is
   begin
      Lith.Memory.Mark (Machine.Core, Start);
   end Mark;

   --------------------------
   -- Mark_External_Object --
   --------------------------

   overriding procedure Mark_External_Object
     (Machine : in out Root_Lith_Machine;
      External : Lith.Objects.External_Object_Address;
      Mark     : not null access
        procedure (X : in out Lith.Objects.Object))
   is
   begin
      Machine.External_Objects (External).External_Object.Mark
        (Machine, Mark);
      Machine.External_Objects (External).Marked := True;
   end Mark_External_Object;

   ---------------------
   -- New_Environment --
   ---------------------

   overriding procedure New_Environment
     (Machine : in out Root_Lith_Machine)
   is
   begin
      Machine.Push (Lith.Objects.Nil);
      Machine.Push (Machine.Environment);
      Machine.Cons;
      Machine.Environment := Machine.Pop;
   end New_Environment;

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
      if Lith.Options.Trace_Stack then
         declare
            Stack_Name : constant String :=
                           (case Stack is
                               when Primary   => "[S]",
                               when Secondary => "[D]");
         begin
            Ada.Text_IO.Put_Line
              ("machine: pop " & Stack_Name & " " & Machine.Show (Result)
               & " <-- " &
               (case Stack is
                     when Primary => Machine.Show (Machine.Stack),
                     when Secondary => Machine.Show (Machine.Dump)));

         end;
      end if;
      case Stack is
         when Primary =>
            Machine.Stack := Machine.Cdr (Machine.Stack);
         when Secondary =>
            Machine.Dump := Machine.Cdr (Machine.Dump);
      end case;
      return Result;
   end Pop;

   ---------------------
   -- Pop_Environment --
   ---------------------

   overriding procedure Pop_Environment
     (Machine : in out Root_Lith_Machine)
   is
   begin
      Machine.Environment := Machine.Cdr (Machine.Environment);
   end Pop_Environment;

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
      case Stack is
         when Primary =>
            Machine.Stack := Machine.Cons (Value, Machine.Stack);
         when Secondary =>
            Machine.Dump := Machine.Cons (Value, Machine.Dump);
      end case;
      if Lith.Options.Trace_Stack then
         declare
            Stack_Name : constant String :=
                           (case Stack is
                               when Primary   => "[S]",
                               when Secondary => "[D]");
         begin
            Ada.Text_IO.Put_Line
              ("machine: push " & Stack_Name & " " & Machine.Show (Value)
               & " --> " & Machine.Show (Machine.Stack));
         end;
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Machine : in out Root_Lith_Machine;
      Symbol_Name : String)
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
   begin
      Lith.Memory.Report (Machine.Core);
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

            Ada.Text_IO.Put_Line
              (Natural'Image
                 (Rec.Info.Hit_Count)
               & "    "
               & Machine.Show (Rec.Proc));
         end loop;
      else
         for Rec of Machine.Source_Result loop
            Count := Count + 1;
            exit when Count > Max_Lines;

            Ada.Text_IO.Put_Line
              (Natural'Image
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
      Machine.Report_Memory;
      Ada.Text_IO.Put_Line
        (" S: " & Machine.Show (Machine.Stack));
      Ada.Text_IO.Put_Line
        (" E: " & Machine.Show (Machine.Environment));
      Ada.Text_IO.Put_Line
        (" C: " & Machine.Show (Machine.Control));
      Ada.Text_IO.Put_Line
        (" D: " & Machine.Show (Machine.Dump));
      Ada.Text_IO.Put_Line
        (" H: " & Machine.Show (Machine.Handlers));
      Ada.Text_IO.Put_Line
        ("Eval:"
         & Natural'Image (Natural (Machine.Eval_Time * 1000.0))
         & "ms");
   end Report_State;

   --------------------
   -- Reserve_Memory --
   --------------------

   overriding procedure Reserve_Memory
     (Machine : in out Root_Lith_Machine;
      Minimum : Natural)
   is
   begin
      Lith.Memory.Reserve_Memory (Machine.Core, Minimum);
   end Reserve_Memory;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset
     (Machine  : in out Root_Lith_Machine)
   is
   begin
      Machine.Stack := Lith.Objects.Nil;
      Machine.Environment := Lith.Objects.Nil;
      Machine.Control := Lith.Objects.Nil;
      Machine.Dump := Lith.Objects.Nil;
      Machine.Handlers := Lith.Objects.Nil;
   end Reset;

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
      Lith.Memory.Set_Car (Machine.Core, Address, New_Car);
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
      Lith.Memory.Set_Cdr (Machine.Core, Address, New_Cdr);
   end Set_Cdr;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Machine   : in out Root_Lith_Machine;
      File_Name : String;
      Line      : Natural)
   is
      Name : constant String := File_Name;
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
                  return String
   is
      function Line_Name return String;
      function Source_Name return String;

      ---------------
      -- Line_Name --
      ---------------

      function Line_Name return String is
         Result : constant String :=
                    Line_Number'Image (Ref.Line);
      begin
         return Result (2 .. Result'Last);
      end Line_Name;

      -----------------
      -- Source_Name --
      -----------------

      function Source_Name return String is
         Path : constant String := Machine.Source_File_Names (Ref.File);
         File : constant String :=
                  Ada.Directories.Simple_Name (Path);
      begin
         if True then
            return File;
         else
            return Path;
         end if;
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
      return String
   is
      use Lith.Objects;

      function Is_List return Boolean;
      function Is_String return Boolean;

      function List_Image
        (Current : Object)
         return String;

      function Hex_Image
        (Value : Natural)
         return String;

      function String_Image
        (Start : Object)
         return String;

      ---------------
      -- Hex_Image --
      ---------------

      function Hex_Image
        (Value : Natural)
         return String
      is
         Hex_Ds : constant String :=
                    "0123456789abcdef";

         function Hex (D : Natural) return Character
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

      ---------------
      -- Is_String --
      ---------------

      function Is_String return Boolean is
      begin
         if Machine.Car (Value) /= String_Value then
            return False;
         else
            declare
               It : Object := Machine.Cdr (Value);
            begin
               while Is_Pair (It) loop
                  if not Is_Character (Machine.Car (It)) then
                     return False;
                  end if;
                  It := Machine.Cdr (It);
               end loop;
               return True;
            end;
         end if;
      end Is_String;

      ----------------
      -- List_Image --
      ----------------

      function List_Image
        (Current : Object)
         return String
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
         return String
      is
      begin
         if Start = Nil then
            return "";
         elsif Is_Character (Machine.Car (Start)) then
            return To_Character (Machine.Car (Start))
              & String_Image (Machine.Cdr (Start));
         else
            raise Constraint_Error
              with "String contains non-character: "
              & Machine.Show (Start);
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
         return "<>";
      elsif Value = String_Value then
         return "<string>";
      elsif Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Integer'Image (To_Integer (Value)),
            Ada.Strings.Left);
      elsif Is_Symbol (Value) then
         return Lith.Objects.Symbols.Get_Name (To_Symbol (Value));
      elsif Is_Character (Value) then
         declare
            Ch : constant Character :=
                   To_Character (Value);
         begin
            if Ada.Characters.Handling.Is_Graphic (Ch) then
               return "#\" & Ch;
            else
               return "#\x"
                 & Hex_Image
                 (Character'Pos (Ch));
            end if;
         end;
      elsif Is_Function (Value) then
         return Lith.Objects.Hex_Image (Value);
      elsif Is_Apply (Value) then
         return "apply" & Integer'Image (-Argument_Count (Value));
      elsif Is_External_Object (Value) then
         return Machine.Get_External_Object (Value).Print (Machine);
      elsif Is_Pair (Value) then
         if Is_String then
            return '"' & String_Image (Machine.Cdr (Value)) & '"';
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
      Ada.Text_IO.Put_Line
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

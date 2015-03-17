with Ada.Characters.Conversions;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO;

with Lith.Environment;
with Lith.Parser;
with Lith.Symbols;

with Lith.Machine.SECD;

with Lith.Objects.Numbers.Exact;

package body Lith.Machine is

   Trace_Machine : constant Boolean := False;
   Trace_GC      : constant Boolean := False;

   function Get
     (Machine : Root_Lith_Machine'Class;
      Pair    : Lith.Objects.Object)
      return Object_Pair
     with Pre => Lith.Objects.Is_Pair (Pair)
     and then not Machine.Free (Lith.Objects.To_Address (Pair));

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Machine  : in out Root_Lith_Machine'Class;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use type Lith.Objects.Object;
      Result : constant Lith.Objects.Object := Machine.Free_List;
      Address : constant Lith.Objects.Cell_Address :=
                  Lith.Objects.To_Address (Result);
   begin
      pragma Assert (Machine.Free (Address));
      Machine.Free_List := Machine.Core (Address).Cdr;
      Machine.Core (Address) := (Car, Cdr);
      Machine.Free (Address) := False;
      Machine.Alloc_Count := Machine.Alloc_Count + 1;
      if Machine.Alloc_Count > Machine.Alloc_Limit then
         Machine.Mark (Result);
         Machine.GC;
      end if;
      return Result;
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
      Machine.Free.all := (others => True);
      Machine.Marked.all := (others => False);
      for I in Machine.Core'Range loop
         Machine.Core (I) :=
           (Car => Nil, Cdr => To_Object (I + 1));
      end loop;
      Machine.Core (Machine.Core'Last) := (Nil, Nil);
      Machine.Free_List := To_Object (Cell_Address'(0));
      Machine.Stack := Nil;
      Machine.Control := Nil;
      Machine.Dump := Nil;
      Machine.Alloc_Count := 0;
      Machine.Alloc_Limit := Natural (Machine.Core'Length) - 100;
      return Machine;
   end Create;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Machine     : in out Root_Lith_Machine'Class;
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
      end if;

      Machine.Environment := Environment;
      Machine.Control := Machine.Cons (Expression, Lith.Objects.Nil);
      Lith.Machine.SECD.Evaluate (Machine);

      if Top then
         Machine.Eval_Time := Machine.Eval_Time + (Clock - Machine.Start_Eval);
      end if;
      return Machine.Pop;
   end Evaluate;

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
         Lith.Environment.Mark (Machine);
         Machine.Mark (Machine.Stack);
         Machine.Mark (Machine.Environment);
         Machine.Mark (Machine.Control);
         Machine.Mark (Machine.Dump);

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

         Machine.GC_Time := Machine.GC_Time + (Clock - Start);

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

   ----------
   -- Load --
   ----------

   overriding function Load (Machine : in out Root_Lith_Machine;
                             Path    : Wide_Wide_String)
                             return Boolean
   is
   begin
      Lith.Parser.Parse_File
        (Machine'Unchecked_Access,
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
     (Machine : in out Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type)
   is
   begin
      Machine.Push (Lith.Objects.To_Object (Symbol));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Machine : in out Root_Lith_Machine'Class;
      Symbol_Name : Wide_Wide_String)
   is
   begin
      Machine.Push
        (Lith.Objects.To_Object (Lith.Symbols.Get_Symbol (Symbol_Name)));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Machine : in out Root_Lith_Machine'Class;
      Value   : Integer)
   is
   begin
      Machine.Push
        (Lith.Objects.To_Object (Value));
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
        ("GC:"
         & Duration'Wide_Wide_Image (Machine.GC_Time * 1000.0)
         & "ms");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Eval:"
         & Duration'Wide_Wide_Image (Machine.Eval_Time * 1000.0)
         & "ms");
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

      function Large_Integer_Image
        (Value : Object)
         return Wide_Wide_String;

      function String_Image
        (Start : Object)
         return Wide_Wide_String;

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
            Lith.Objects.Numbers.Exact.Divide (Machine);
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
      elsif Is_Integer (Value) then
         return Ada.Strings.Wide_Wide_Fixed.Trim
           (Integer'Wide_Wide_Image (To_Integer (Value)),
            Ada.Strings.Left);
      elsif Is_Symbol (Value) then
         return Lith.Symbols.Get_Name (To_Symbol (Value));
      elsif Is_Character (Value) then
         return "\#" & To_Character (Value);
      elsif Is_Function (Value) then
         return Ada.Characters.Conversions.To_Wide_Wide_String
           (Lith.Objects.Hex_Image (Value));
      elsif Is_Apply (Value) then
         return "apply" & Integer'Wide_Wide_Image (-Argument_Count (Value));
      elsif Is_Pair (Value) then
         if Machine.Car (Value) = Lith.Symbols.String_Atom then
            return '"' & String_Image (Machine.Cdr (Value)) & '"';
         elsif True
           and then Machine.Car (Value) = Lith.Symbols.Large_Integer_Atom
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
           & Ada.Characters.Conversions.To_Wide_Wide_String
           (Hex_Image (Value))
           & "]";
      end if;
   end Show;

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

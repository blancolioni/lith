with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Lith.Environment;
with Lith.Evaluator;
with Lith.Parser;
with Lith.Symbols;

package body Lith.Machine is

   Trace_Machine : constant Boolean := False;

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

   procedure Cons
     (Machine : in out Root_Lith_Machine'Class)
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
      Result : constant Lith.Objects.Object :=
                 Lith.Evaluator.Evaluate
                   (Store => Machine,
                    Expr  => Expression,
                    Env   => Environment);
   begin
      Machine.Push (Result);
--        if Machine.Alloc_Count > Natural (Machine.Core'Length) / 2 then
--           Machine.GC;
--        end if;
      return Machine.Pop;
   end Evaluate;

   --------
   -- GC --
   --------

   procedure GC
     (Machine : in out Root_Lith_Machine'Class)
   is
   begin
      Ada.Text_IO.Put_Line ("Garbage collecting ...");
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

         Ada.Text_IO.Put_Line
           ("GC freed"
            & Integer'Image (Old_Alloc_Count - Machine.Alloc_Count)
            & " cells in"
            & Duration'Image ((Clock - Start) * 1000.0)
            & "ms");
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
                             Path    : String)
                             return Boolean
   is
   begin
      Lith.Parser.Parse_File (Machine'Unchecked_Access, Path);
      return True;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Cannot load " & Path
            & ": "
            & Ada.Exceptions.Exception_Message (E));
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
     (Machine : in out Root_Lith_Machine)
      return Lith.Objects.Object
   is
      Result : constant Lith.Objects.Object := Machine.Car (Machine.Stack);
   begin
      if Trace_Machine then
         Ada.Text_IO.Put_Line ("machine: pop " & Machine.Show (Result));
      end if;
      Machine.Stack := Machine.Cdr (Machine.Stack);
      return Result;
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object)
   is
   begin
      if Trace_Machine then
         Ada.Text_IO.Put_Line ("machine: push " & Machine.Show (Value));
      end if;
      Machine.Stack := Allocate (Machine, Value, Machine.Stack);
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
      Symbol_Name : String)
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

   ------------
   -- Report --
   ------------

   procedure Report
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
      Ada.Text_IO.Put_Line ("Total number of cells:"
                            & Cell_Address'Image (Machine.Core'Length));
      Ada.Text_IO.Put_Line ("Allocated cell count: "
                              & Machine.Alloc_Count'Img);
      Ada.Text_IO.Put_Line ("Free cell count: "
                            & Natural'Image
                              (Natural (Machine.Core'Length
                               - Machine.Alloc_Count)));
      Ada.Text_IO.Put_Line ("Stack: " & Machine.Show (Machine.Stack));
   end Report;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Machine : Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return String
   is
      use Lith.Objects;

      function Is_List return Boolean;

      function List_Image
        (Current : Object)
         return String;

      function String_Image
        (Start : Object)
         return String;

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
         else
            return Character'Val (To_Integer (Machine.Car (Start)))
              & String_Image (Machine.Cdr (Start));
         end if;
      end String_Image;

   begin
      if Value = Nil then
         return "()";
      elsif Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Integer'Image (To_Integer (Value)),
            Ada.Strings.Left);
      elsif Is_Symbol (Value) then
         return Lith.Symbols.Get_Name (To_Symbol (Value));
      elsif Is_Function (Value) then
         return Lith.Objects.Hex_Image (Value);
      elsif Is_Pair (Value) then
         if Machine.Car (Value) = Lith.Symbols.String_Atom then
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
         return "<error: unknown object type [" & Hex_Image (Value) & "]";
      end if;
   end Show;

   ---------
   -- Top --
   ---------

   overriding function Top
     (Machine : in out Root_Lith_Machine)
      return Lith.Objects.Object
   is
   begin
      return Machine.Car (Machine.Stack);
   end Top;

end Lith.Machine;

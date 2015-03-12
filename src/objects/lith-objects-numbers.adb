with Ada.Wide_Wide_Text_IO;

with Lith.Symbols;

package body Lith.Objects.Numbers is

   Highest_Small_Integer : constant := 2 ** (Payload_Bits - 1) - 1;
   Lowest_Small_Integer  : constant := -2 ** (Payload_Bits - 1) + 1;

   type Exact_Number_Type is (Small_Integer, Large_Integer, Rational);
   pragma Unreferenced (Rational);

   function Top_Of_Stack_Type
     (Store : Object_Store'Class;
      Index : Positive := 1)
      return Exact_Number_Type;

   procedure Merge_Types (Store : in out Object_Store'Class);

   procedure Small_Integer_To_Large (Store : in out Object_Store'Class);

   ---------
   -- Add --
   ---------

   procedure Add
     (Store : in out Object_Store'Class)
   is
   begin
      Merge_Types (Store);
      case Top_Of_Stack_Type (Store) is
         when Small_Integer =>
            declare
               X : constant Integer := To_Integer (Store.Top (1));
               Y : constant Integer := To_Integer (Store.Top (2));
            begin
               if (X < 0 and then Y < 0
                   and then Lowest_Small_Integer - X > Y)
                 or else
                   (X > 0 and then Y > 0
                   and then Highest_Small_Integer - X < Y)
               then
                  Small_Integer_To_Large (Store);
                  Store.Push (Store.Pop, Secondary);
                  Small_Integer_To_Large (Store);
                  Store.Push (Store.Pop (Secondary));
                  Add (Store);
               else
                  Store.Push (To_Object (X + Y));
               end if;
            end;
         when Large_Integer =>
            declare
               Xs : constant Array_Of_Objects :=
                     Store.To_Object_Array (Store.Cdr (Store.Top (1)));
               Ys : constant Array_Of_Objects :=
                      Store.To_Object_Array (Store.Cdr (Store.Top (2)));
               Max_Length : constant Natural :=
                              Natural'Max (Xs'Length, Ys'Length) + 1;
               C          : Integer := 0;
               Length     : Natural := 0;
               Z          : Integer;
            begin

               Store.Push (Store.Pop, Secondary);
               Store.Push (Store.Pop, Secondary);
               Store.Push (Lith.Symbols.Large_Integer_Atom);

               for I in 1 .. Max_Length loop
                  if I <= Xs'Last then
                     if I <= Ys'Last then
                        Z := To_Integer (Xs (I)) + To_Integer (Ys (I)) + C;
                     else
                        Z := To_Integer (Xs (I)) + C;
                     end if;
                  elsif I <= Ys'Last then
                     Z := To_Integer (Ys (I)) + C;
                  else
                     Z := C;
                     C := 0;
                  end if;

                  if Z > Highest_Small_Integer then
                     C := 1;
                     Z := Z - Highest_Small_Integer;
                  elsif Z < Lowest_Small_Integer then
                     C := -1;
                     Z := Z + Lowest_Small_Integer;
                  end if;

                  if Z /= 0 then
                     Length := I;
                  end if;

                  Store.Push (To_Object (Z));

               end loop;

               if Length <= 1 then

                  Store.Drop (Max_Length - 1);

                  declare
                     Z : constant Object := Store.Pop;
                  begin
                     Store.Drop (1);
                     Store.Push (Z);
                  end;

               else
                  Store.Push (Nil);
                  for I in 1 .. Max_Length + 1 loop
                     Store.Cons;
                  end loop;
               end if;

               Store.Drop (Count => 2,
                           Stack => Secondary);

            end;
         when others =>
            null;
      end case;
   end Add;

   ------------
   -- Divide --
   ------------

   procedure Divide
     (Store : in out Object_Store'Class)
   is
      pragma Unreferenced (Store);
   begin
      null;
   end Divide;

   ---------------------
   -- Is_Exact_Number --
   ---------------------

   function Is_Exact_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean
   is
   begin
      return Is_Integer (Item)
        or else Is_Integral_Number (Store, Item)
        or else Is_Rational_Number (Store, Item);
   end Is_Exact_Number;

   ------------------------
   -- Is_Integral_Number --
   ------------------------

   function Is_Integral_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean
   is
   begin
      return Is_Integer (Item)
        or else (Is_Pair (Item)
                 and then Store.Car (Item) = Lith.Symbols.Large_Integer_Atom);
   end Is_Integral_Number;

   ------------------------
   -- Is_Rational_Number --
   ------------------------

   function Is_Rational_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean
   is
   begin
      return Is_Pair (Item)
        and then Store.Car (Item) = Lith.Symbols.Rational_Atom;
   end Is_Rational_Number;

   -----------------
   -- Merge_Types --
   -----------------

   procedure Merge_Types (Store : in out Object_Store'Class) is
      X_Type : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 1);
      Y_Type : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 2);
   begin
      if X_Type /= Y_Type then
         if X_Type < Y_Type then
            Small_Integer_To_Large (Store);
         else
            Store.Report_State;
            Store.Push (Store.Top (2));
            Small_Integer_To_Large (Store);
            Store.Push (Store.Pop, Secondary);
            Store.Push (Store.Pop, Secondary);
            Store.Drop;
            Store.Push (Store.Top (2, Secondary));
            Store.Push (Store.Pop (Secondary));
            Store.Drop (Stack => Secondary);
            Store.Report_State;
         end if;
      end if;
   end Merge_Types;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Store : in out Object_Store'Class)
   is
      pragma Unreferenced (Store);
   begin
      null;
   end Multiply;

   ---------------
   -- Remainder --
   ---------------

   procedure Remainder
     (Store : in out Object_Store'Class)
   is
      pragma Unreferenced (Store);
   begin
      null;
   end Remainder;

   ----------------------------
   -- Small_Integer_To_Large --
   ----------------------------

   procedure Small_Integer_To_Large (Store : in out Object_Store'Class) is
      X : constant Object := Store.Pop;
   begin
      Store.Push (Lith.Symbols.Large_Integer_Atom);
      Store.Push (X);
      Store.Push (Nil);
      Store.Cons;
      Store.Cons;
   end Small_Integer_To_Large;

   --------------
   -- Subtract --
   --------------

   procedure Subtract
     (Store : in out Object_Store'Class)
   is
      pragma Unreferenced (Store);
   begin
      null;
   end Subtract;

   -----------------------
   -- Top_Of_Stack_Type --
   -----------------------

   function Top_Of_Stack_Type
     (Store : Object_Store'Class;
      Index : Positive := 1)
      return Exact_Number_Type
   is
      Item : constant Object := Store.Top (Index);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Checking" & Positive'Wide_Wide_Image (Index) & ": "
         & Store.Show (Item));
      if Is_Integer (Item) then
         return Small_Integer;
      elsif Store.Car (Item) = Lith.Symbols.Large_Integer_Atom then
         return Large_Integer;
      else
         raise Constraint_Error with
           "top of stack is not an integer";
      end if;
   end Top_Of_Stack_Type;

end Lith.Objects.Numbers;

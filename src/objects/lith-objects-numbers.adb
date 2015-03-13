with Ada.Wide_Wide_Text_IO;

with Lith.Symbols;

package body Lith.Objects.Numbers is

   Highest_Small_Integer : constant := 2 ** (Payload_Bits - 1) - 1;
   Lowest_Small_Integer  : constant := -2 ** (Payload_Bits - 1) + 1;

   type Exact_Number_Type is (Small_Integer, Large_Integer, Rational);

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
                  Store.Drop (2);
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
                  else
                     C := 0;
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

                  for I in 1 .. Max_Length - Length loop
                     Store.Drop;
                  end loop;
                  Store.Push (Nil);

                  for I in 1 .. Length + 1 loop
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
      X_Type   : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 1);
      Y_Type   : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 2);
      Divisor  : Integer;
      Negative : Boolean;
   begin
      if Y_Type /= Small_Integer then
         raise Constraint_Error with
           "when dividing, the divisor must be a small integer. "
           & " we are so sorry about that.";
      end if;

      if X_Type = Small_Integer then
         declare
            X : constant Integer := To_Integer (Store.Pop);
            Y : constant Integer := To_Integer (Store.Pop);
         begin
            Store.Push (To_Object (X / Y));
            Store.Push (To_Object (X mod Y));
            return;
         end;
      end if;

      Store.Swap;
      Divisor := To_Integer (Store.Pop);

      Negative := False;
      if Divisor = 0 then
         raise Constraint_Error with "division by zero";
      elsif Divisor = 1 then
         return;
      elsif Divisor < 0 then
         Negative := True;
         Divisor := -Divisor;
      end if;

      if False then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("dividing by" & Integer'Wide_Wide_Image (Divisor));
         Store.Report_State;
      end if;

      declare
         Half : constant Integer := 2 ** (Payload_Bits / 2);
         It : constant Array_Of_Objects :=
                Store.To_Object_Array (Store.Cdr (Store.Top));
         R  : Integer := 0;
         Count : Natural := 0;
      begin
         Store.Push (Store.Pop, Secondary);
         Store.Push (Lith.Symbols.Large_Integer_Atom); -- quotient

         for Elem of reverse It loop
            if True then
               declare
                  X : constant Integer := To_Integer (Elem);
                  D : constant Long_Long_Integer :=
                        Long_Long_Integer (R) * Highest_Small_Integer
                        + Long_Long_Integer (X);
                  Q : constant Long_Long_Integer :=
                        D / Long_Long_Integer (Divisor);
               begin
                  Store.Push (To_Object (Integer (Q)), Secondary);
                  R := Integer (D mod Long_Long_Integer (Divisor));
                  Count := Count + 1;
               end;
            else
               declare
                  X    : constant Integer := To_Integer (Elem);
                  X1   : constant Integer := X / Half;
                  X2   : constant Integer := X mod Half;
                  D1   : constant Integer := R * Half + X1;
                  Q1   : constant Integer := D1 / Divisor;
                  R1   : constant Integer := D1 mod Divisor;
                  D2   : constant Integer := R1 * Half + X2;
                  Q2   : constant Integer := D2 / Divisor;
                  R2   : constant Integer := D2 mod Divisor;
               begin
                  Store.Push (To_Object (Q1 * Half + Q2), Secondary);
                  R := R2;
                  Count := Count + 1;
               end;
            end if;

         end loop;

         for I in 1 .. Count loop
            Store.Push (Store.Pop (Secondary));
         end loop;

         while Store.Top = To_Object (Integer'(0)) loop
            Store.Drop;
            Count := Count - 1;
         end loop;

         if Count = 0 then
            Store.Drop;
            Store.Push (To_Object (Integer'(0)));
         elsif Count = 1 then
            Store.Swap;
            Store.Drop;
            if Negative then
               declare
                  X : constant Integer := To_Integer (Store.Pop);
               begin
                  Store.Push (To_Object (-X));
               end;
            end if;
         else
            Store.Push (Nil);

            for I in 1 .. Count + 1 loop
               Store.Cons;
            end loop;

            if Negative then
               Negate (Store);
            end if;
         end if;

         Store.Push (To_Object (R));
         Store.Drop (1, Secondary);

         if False then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("finished dividing by" & Integer'Wide_Wide_Image (Divisor));
            Store.Report_State;
         end if;

      end;

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
            Store.Push (Store.Top (2));
            Small_Integer_To_Large (Store);
            Store.Push (Store.Pop, Secondary);
            Store.Push (Store.Pop, Secondary);
            Store.Drop;
            Store.Push (Store.Top (2, Secondary));
            Store.Push (Store.Pop (Secondary));
            Store.Drop (Stack => Secondary);
         end if;
      end if;
   end Merge_Types;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Store : in out Object_Store'Class)
   is
      X_Type : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 1);
      Y_Type : constant Exact_Number_Type := Top_Of_Stack_Type (Store, 2);
      Multiplier : Integer;
      Negative   : Boolean;
   begin
      if X_Type /= Small_Integer and then Y_Type /= Small_Integer then
         raise Constraint_Error with
           "when multiplying, one of the values must be a small integer. "
           & " we are so sorry about that.";
      end if;

      if X_Type = Small_Integer and then Y_Type = Small_Integer then
         declare
            X : constant Integer := To_Integer (Store.Pop);
            Y : constant Integer := To_Integer (Store.Pop);
         begin
            if Highest_Small_Integer / abs X >= abs Y then
               Store.Push (To_Object (X * Y));
               return;
            elsif X < Y then
               Store.Push (To_Object (Y));
               Multiplier := X;
            else
               Store.Push (To_Object (X));
               Multiplier := Y;
            end if;
         end;
      elsif X_Type /= Small_Integer then
         Store.Push (Store.Pop, Secondary);
         Multiplier := To_Integer (Store.Pop);
         Store.Push (Store.Pop (Secondary));
      else
         Multiplier := To_Integer (Store.Pop);
      end if;

      Negative := False;
      if Multiplier = 0 then
         Store.Drop;
         Store.Push (To_Object (Integer'(0)));
         return;
      elsif Multiplier = 1 then
         return;
      elsif Multiplier < 0 then
         Negative := True;
         Multiplier := -Multiplier;
      end if;

      declare
         Extra_Adds : Natural := 0;
      begin
         while Multiplier > 1 loop
            Store.Push (Store.Top);
            if Multiplier mod 2 = 0 then
               Add (Store);
               Multiplier := Multiplier / 2;
            else
               Multiplier := Multiplier - 1;
               Extra_Adds := Extra_Adds + 1;
            end if;
         end loop;

         if Extra_Adds > 0 then
            for I in 1 .. Extra_Adds loop
               Add (Store);
            end loop;
         end if;
      end;

      if Negative then
         Negate (Store);
      end if;

   end Multiply;

   ------------
   -- Negate --
   ------------

   procedure Negate
     (Store : in out Object_Store'Class)
   is
   begin
      case Top_Of_Stack_Type (Store) is
         when Small_Integer =>
            declare
               X : constant Integer := To_Integer (Store.Pop);
            begin
               Store.Push (To_Object (-1 * X));
            end;
         when Large_Integer =>
            declare
               It : Object := Store.Pop;
               Count : Natural := 0;
            begin
               Store.Push (It, Secondary);
               Store.Push (Lith.Symbols.Large_Integer_Atom);
               It := Store.Cdr (It);  --  skip large integer atom
               while It /= Nil loop
                  Store.Push
                    (To_Object (-1 * To_Integer (Store.Car (It))));
                  Count := Count + 1;
                  It := Store.Cdr (It);
               end loop;
               Store.Push (Nil);
               for I in 1 .. Count + 1 loop
                  Store.Cons;
               end loop;
               Store.Drop (1, Secondary);
            end;
         when Rational =>
            null;
      end case;
   end Negate;

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
   begin
      Store.Push (Store.Pop, Secondary);
      Negate (Store);
      Store.Push (Store.Pop (Secondary));
      Add (Store);
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

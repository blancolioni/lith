with Ada.Unchecked_Conversion;

with Lith.Symbols;

package body Lith.Objects.Numbers is

   subtype Lith_Float is Long_Float;

   Float_Bits : constant Positive := Lith_Float'Size;
   Objects_Per_Float : constant Positive :=
                         Float_Bits / Payload_Bits + 1;
   type Float_Rep is mod 2 ** Float_Bits;

   function Lith_Float_To_Rep is
     new Ada.Unchecked_Conversion (Lith_Float, Float_Rep);
   function Rep_To_Lith_Float is
     new Ada.Unchecked_Conversion (Float_Rep, Lith_Float);

   Highest_Small_Integer : constant Integer := 2 ** (Payload_Bits - 1) - 1;
   Lowest_Small_Integer  : constant Integer := -2 ** (Payload_Bits - 1) + 1;

   type Lith_Number_Type is (Small_Integer, Large_Integer, Rational,
                             Inexact_Real, Inexact_Complex);

   subtype Exact_Number_Type is
     Lith_Number_Type range Small_Integer .. Rational;
   subtype Inexact_Number_Type is
     Lith_Number_Type range Inexact_Real .. Inexact_Complex;

   procedure Merge_Types (Store : in out Object_Store'Class);

   procedure Small_Integer_To_Large (Store : in out Object_Store'Class);

   function Top_Of_Stack_Type
     (Store : Object_Store'Class;
      Index : Positive := 1)
      return Lith_Number_Type;

   function Pop_Float
     (Store : in out Object_Store'Class)
      return Lith_Float;

   procedure Push_Float
     (Store : in out Object_Store'Class;
      Value : Lith_Float);

   ------------------
   -- Ensure_Exact --
   ------------------

   procedure Ensure_Exact
     (Store : in out Object_Store'Class)
   is
      Top_Type : constant Lith_Number_Type := Top_Of_Stack_Type (Store);
   begin
      if Top_Type in Inexact_Number_Type then
         case Inexact_Number_Type (Top_Type) is
            when Inexact_Real =>
               declare
                  F : Lith_Float := Pop_Float (Store);
                  Neg : constant Boolean := F < 0.0;
                  Divider : constant Lith_Float :=
                              Lith_Float (Highest_Small_Integer);
                  Count   : Natural := 0;
               begin

                  while abs F > Lith_Float (Highest_Small_Integer) loop
                     declare
                        D : constant Lith_Float := abs F / Divider;
                        New_F : constant Lith_Float :=
                                  Lith_Float'Truncation (D);
                        R : constant Integer :=
                                  Integer ((D - New_F) * Divider);
                     begin
                        Store.Push (To_Object (R));
                        F := New_F;
                        Count := Count + 1;
                     end;
                  end loop;

                  Store.Push (To_Object (Integer (F)));
                  for I in 1 .. Count loop
                     Store.Push (To_Object (Highest_Small_Integer));
                     Exact_Multiply (Store);
                     Exact_Add (Store);
                  end loop;

                  if Neg then
                     Exact_Negate (Store);
                  end if;
               end;

            when Inexact_Complex =>
               raise Constraint_Error with "complex types not supported";
         end case;
      end if;
   end Ensure_Exact;

   --------------------
   -- Ensure_Inexact --
   --------------------

   procedure Ensure_Inexact
     (Store : in out Object_Store'Class)
   is
      Top_Type : constant Lith_Number_Type := Top_Of_Stack_Type (Store);
   begin
      if Top_Type in Exact_Number_Type then
         case Exact_Number_Type (Top_Type) is
            when Small_Integer =>
               Push_Float (Store, Lith_Float (To_Integer (Store.Pop)));
            when Large_Integer =>
               declare
                  Cells : constant Array_Of_Objects :=
                            Store.To_Object_Array (Store.Cdr (Store.Top));
                  F     : Lith_Float  := 0.0;
               begin
                  for X of reverse Cells loop
                     F := F * Lith_Float (Highest_Small_Integer)
                       + Lith_Float (To_Integer (X));
                  end loop;
                  Store.Drop;
                  Push_Float (Store, F);
               end;
            when Rational =>
               raise Constraint_Error with "rational numbers not supported";
         end case;
      end if;
   end Ensure_Inexact;

   ---------------
   -- Exact_Add --
   ---------------

   procedure Exact_Add
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
                  Exact_Add (Store);
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
   end Exact_Add;

   -------------------
   -- Exact_Compare --
   -------------------

   function Exact_Compare
     (Store : in out Object_Store'Class)
      return Compare
   is
   begin
      Merge_Types (Store);
      case Exact_Number_Type (Top_Of_Stack_Type (Store)) is
         when Small_Integer =>
            declare
               X : constant Integer := To_Integer (Store.Pop);
               Y : constant Integer := To_Integer (Store.Top);
            begin
               if X < Y then
                  return LT;
               elsif X > Y then
                  return GT;
               else
                  return EQ;
               end if;
            end;
         when Large_Integer =>

            declare
               Xs : constant Array_Of_Objects :=
                     Store.To_Object_Array (Store.Cdr (Store.Top (1)));
               Ys : constant Array_Of_Objects :=
                      Store.To_Object_Array (Store.Cdr (Store.Top (2)));
               Result : Compare := EQ;
            begin
               if Xs'Length > Ys'Length then
                  Result := GT;
               elsif Ys'Length > Xs'Length then
                  Result := LT;
               else
                  for I in reverse Xs'Range loop
                     declare
                        X : constant Integer := To_Integer (Xs (I));
                        Y : constant Integer := To_Integer (Ys (I));
                     begin
                        if X > Y then
                           Result := GT;
                           exit;
                        elsif X < Y then
                           Result := LT;
                           exit;
                        end if;
                     end;
                  end loop;
               end if;
               Store.Drop;
               return Result;

            end;
         when Rational =>

            raise Constraint_Error with "rational numbers not supported yet";

      end case;
   end Exact_Compare;

   ------------------
   -- Exact_Divide --
   ------------------

   procedure Exact_Divide
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
            Store.Push (Nil);
            Store.Cons;
            Store.Cons;
            return;
         end;
      end if;

      Store.Swap;
      Divisor := To_Integer (Store.Pop);

      Negative := False;
      if Divisor = 0 then
         raise Constraint_Error with "division by zero";
      elsif Divisor = 1 then
         Store.Push (To_Object (Integer'(0)));
         Store.Push (Nil);
         Store.Cons;
         Store.Cons;
         return;
      elsif Divisor < 0 then
         Negative := True;
         Divisor := -Divisor;
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
                        Long_Long_Integer (R)
                        * Long_Long_Integer (Highest_Small_Integer)
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
               Exact_Negate (Store);
            end if;
         end if;

         Store.Push (To_Object (R));
         Store.Push (Nil);
         Store.Cons;
         Store.Cons;

         Store.Drop (1, Secondary);

      end;

   end Exact_Divide;

   --------------------
   -- Exact_Multiply --
   --------------------

   procedure Exact_Multiply
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
            if X in 0 .. 1 or else Y in 0 .. 1
              or else Highest_Small_Integer / abs X >= abs Y
            then
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
               Exact_Add (Store);
               Multiplier := Multiplier / 2;
            else
               Multiplier := Multiplier - 1;
               Extra_Adds := Extra_Adds + 1;
            end if;
         end loop;

         if Extra_Adds > 0 then
            for I in 1 .. Extra_Adds loop
               Exact_Add (Store);
            end loop;
         end if;
      end;

      if Negative then
         Exact_Negate (Store);
      end if;

   end Exact_Multiply;

   ------------------
   -- Exact_Negate --
   ------------------

   procedure Exact_Negate
     (Store : in out Object_Store'Class)
   is
   begin
      case Exact_Number_Type (Top_Of_Stack_Type (Store)) is
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
   end Exact_Negate;

   --------------------
   -- Exact_Subtract --
   --------------------

   procedure Exact_Subtract
     (Store : in out Object_Store'Class)
   is
   begin
      Store.Push (Store.Pop, Secondary);
      Exact_Negate (Store);
      Store.Push (Store.Pop (Secondary));
      Exact_Add (Store);
   end Exact_Subtract;

   -----------------
   -- Inexact_Add --
   -----------------

   procedure Inexact_Add
     (Store : in out Object_Store'Class)
   is
      X : constant Lith_Float := Pop_Float (Store);
      Y : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, X + Y);
   end Inexact_Add;

   ---------------------
   -- Inexact_Compare --
   ---------------------

   function Inexact_Compare
     (Store : in out Object_Store'Class)
      return Compare
   is
      X : constant Lith_Float := Pop_Float (Store);
      Y : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, Y);
      if X < Y then
         return LT;
      elsif X > Y then
         return GT;
      else
         return EQ;
      end if;
   end Inexact_Compare;

   --------------------
   -- Inexact_Divide --
   --------------------

   procedure Inexact_Divide
     (Store : in out Object_Store'Class)
   is
      X : constant Lith_Float := Pop_Float (Store);
      Y : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, X / Y);
   end Inexact_Divide;

   ----------------------
   -- Inexact_Multiply --
   ----------------------

   procedure Inexact_Multiply
     (Store : in out Object_Store'Class)
   is
      X : constant Lith_Float := Pop_Float (Store);
      Y : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, X * Y);
   end Inexact_Multiply;

   --------------------
   -- Inexact_Negate --
   --------------------

   procedure Inexact_Negate
     (Store : in out Object_Store'Class)
   is
      X : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, -X);
   end Inexact_Negate;

   ----------------------
   -- Inexact_Subtract --
   ----------------------

   procedure Inexact_Subtract
     (Store : in out Object_Store'Class)
   is
      X : constant Lith_Float := Pop_Float (Store);
      Y : constant Lith_Float := Pop_Float (Store);
   begin
      Push_Float (Store, X - Y);
   end Inexact_Subtract;

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

   --------------------
   -- Is_Real_Number --
   --------------------

   function Is_Real_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean
   is
   begin
      return Is_Pair (Item)
        and then Store.Car (Item) = Lith.Symbols.Floating_Point_Atom;
   end Is_Real_Number;

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

   ---------------
   -- Pop_Float --
   ---------------

   function Pop_Float
     (Store : in out Object_Store'Class)
      return Lith_Float
   is
      Rep : Float_Rep := 0;
      It  : Object := Store.Cdr (Store.Top);
   begin
      for I in 1 .. Objects_Per_Float loop
         Rep := Rep * Object_Payload'Modulus
           + Float_Rep (Store.Car (It).Payload);
         It := Store.Cdr (It);
      end loop;
      Store.Drop;
      return Rep_To_Lith_Float (Rep);
   end Pop_Float;

   ----------------
   -- Push_Float --
   ----------------

   procedure Push_Float
     (Store : in out Object_Store'Class;
      Value : Lith_Float)
   is
      Rep      : Float_Rep := Lith_Float_To_Rep (Value);
      Payloads : array (1 .. Objects_Per_Float) of Object_Payload;
   begin
      Store.Push (Lith.Symbols.Floating_Point_Atom);
      for I in reverse Payloads'Range loop
         Payloads (I) := Object_Payload (Rep mod Object_Payload'Modulus);
         Rep := Rep / Object_Payload'Modulus;
      end loop;
      for I in Payloads'Range loop
         Store.Push (Object'(Payloads (I), Integer_Object));
      end loop;
      Store.Push (Nil);
      Store.Cons;
      for I in Payloads'Range loop
         Store.Cons;
      end loop;
   end Push_Float;

   ----------------
   -- Push_Float --
   ----------------

   procedure Push_Float
     (Store : in out Object_Store'Class;
      Text  : Wide_Wide_String)
   is
   begin
      Push_Float (Store, Lith_Float'Wide_Wide_Value (Text));
   end Push_Float;

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

   -----------------------
   -- Top_Of_Stack_Type --
   -----------------------

   function Top_Of_Stack_Type
     (Store : Object_Store'Class;
      Index : Positive := 1)
      return Lith_Number_Type
   is
      Item : constant Object := Store.Top (Index);
   begin
      if Is_Integer (Item) then
         return Small_Integer;
      elsif Store.Car (Item) = Lith.Symbols.Large_Integer_Atom then
         return Large_Integer;
      elsif Store.Car (Item) = Lith.Symbols.Floating_Point_Atom then
         return Inexact_Real;
      else
         raise Constraint_Error with
           "top of stack is not an integer";
      end if;
   end Top_Of_Stack_Type;

end Lith.Objects.Numbers;

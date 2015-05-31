with Lith.Objects.Symbol_Maps;
with Lith.Objects.Symbols;

with Lith.Objects.Large_Integers;
with Lith.Objects.Real;

package body Lith.Primitives.ALU is

   use Lith.Objects;

   type Integer_Op_Function is access
     function
       (Store   : in out Object_Store'Class;
        X, Y    : Integer)
        return Object;

   type Large_Integer_Op_Function is access
     function
       (Store   : in out Object_Store'Class;
        X, Y    : Large_Integers.Large_Integer_Object'Class)
        return Object;

   type Real_Op_Function is access
     function
       (Store : in out Object_Store'Class;
        X, Y  : Lith_Real)
        return Object;

   type Allowed_Compares is
     array (Compare_Result) of Boolean;

   Compare_EQ : constant Allowed_Compares := (False, True, False);
   Compare_NE : constant Allowed_Compares := (True, False, True);
   Compare_LE : constant Allowed_Compares := (True, True, False);
   Compare_LT : constant Allowed_Compares := (True, False, False);
   Compare_GE : constant Allowed_Compares := (False, True, True);
   Compare_GT : constant Allowed_Compares := (False, False, True);

   type Operator_Class is (Arithmetic, Comparison);

   type Operator_Record (Class : Operator_Class := Arithmetic) is
      record
         case Class is
            when Arithmetic =>
               Identity         : Integer;
               Integer_Op       : Integer_Op_Function;
               Large_Integer_Op : Large_Integer_Op_Function;
               Real_Op          : Real_Op_Function;
            when Comparison =>
               Allowed           : Allowed_Compares;
         end case;
      end record;

   package Operator_Maps is
     new Lith.Objects.Symbol_Maps
       (Operator_Record);

   Ops : Operator_Maps.Map;

   procedure Operator
     (Symbol               : Wide_Wide_String;
      Identity             : Integer;
      Integer_Op           : Integer_Op_Function;
      Large_Integer_Op     : Large_Integer_Op_Function;
      Real_Op              : Real_Op_Function);

   procedure Operator
     (Symbol               : Wide_Wide_String;
      Allowed              : Allowed_Compares);

   function Compare_Integer
     (Store   : in out Object_Store'Class;
      Allowed : Allowed_Compares;
      X, Y    : Integer)
      return Boolean;

   function Compare_Large_Integer
     (Store   : in out Object_Store'Class;
      Allowed : Allowed_Compares;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Boolean;

   function Compare_Real
     (Allowed : Allowed_Compares;
      X, Y  : Lith_Real)
      return Boolean;

   function Add_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object;

   function Add_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object;

   function Add_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object;

   function Subtract_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object;

   function Subtract_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object;

   function Subtract_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object;

   function Multiply_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object;

   function Multiply_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
     with Pre => Large_Integers.In_Integer_Range (X)
     or else Large_Integers.In_Integer_Range (Y);

   function Multiply_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object;

   function Divide_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object;

   function Divide_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
     with Pre => Large_Integers.In_Integer_Range (Y);

   function Divide_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object;

   -----------------
   -- Add_Integer --
   -----------------

   function Add_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object
   is
      Z : constant Integer := X + Y;
   begin
      if Lith.Objects.In_Object_Range (Z) then
         return Lith.Objects.To_Object (Z);
      else
         return Large_Integers.To_Object
           (Store, Large_Integers.To_Large_Integer (Z));
      end if;
   end Add_Integer;

   -----------------------
   -- Add_Large_Integer --
   -----------------------

   function Add_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
   is
      use Lith.Objects.Large_Integers;
      Z : Large_Integer_Object'Class := X;
   begin
      Z.Add (Y);
      return To_Object (Store, Z);
   end Add_Large_Integer;

   -------------------
   -- Add_Operators --
   -------------------

   procedure Add_Operators is
   begin
      Operator ("+", 0,
                Add_Integer'Access,
                Add_Large_Integer'Access,
                Add_Real'Access);

      Operator ("-", 0,
                Subtract_Integer'Access,
                Subtract_Large_Integer'Access,
                Subtract_Real'Access);

      Operator ("*", 1,
                Multiply_Integer'Access,
                Multiply_Large_Integer'Access,
                Multiply_Real'Access);

      Operator ("/", 1,
                Divide_Integer'Access,
                Divide_Large_Integer'Access,
                Divide_Real'Access);

      Operator ("<", Compare_LT);
      Operator (">", Compare_GT);
      Operator ("<=", Compare_LE);
      Operator (">=", Compare_GE);
      Operator ("/=", Compare_NE);
      Operator ("=", Compare_EQ);

--        Operator ("-", 0,
--                  Numbers.Exact_Negate'Access,
--                  Numbers.Inexact_Negate'Access,
--                  Lith.Objects.Numbers.Exact_Subtract'Access,
--                  Lith.Objects.Numbers.Inexact_Subtract'Access);
--
--        Operator ("*", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Lith.Objects.Numbers.Exact_Multiply'Access,
--                  Lith.Objects.Numbers.Inexact_Multiply'Access);
--
--        Operator ("floor/", 0, null, null,
--                  Lith.Objects.Numbers.Exact_Divide'Access,
--                  null);
--
--        Operator ("<=", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Exact_Stack_LE'Access, Inexact_Stack_LE'Access);
--        Operator (">=", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Exact_Stack_GE'Access, Inexact_Stack_GE'Access);
--        Operator ("<", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Exact_Stack_LT'Access, Inexact_Stack_LT'Access);
--        Operator (">", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Exact_Stack_GT'Access, Inexact_Stack_GT'Access);
--        Operator ("=", 0, Stack_Identity'Access, Stack_Identity'Access,
--                  Exact_Stack_EQ'Access, Inexact_Stack_EQ'Access);

   end Add_Operators;

   --------------
   -- Add_Real --
   --------------

   function Add_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object
   is
      use Lith.Objects.Real;
      Z : constant Lith_Real := X + Y;
   begin
      return To_Object (Store, Z);
   end Add_Real;

   -----------
   -- Apply --
   -----------

   function Apply
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Op : constant Symbol_Type := To_Symbol (Store.Argument (1));
      X  : constant Object := Store.Argument (2);
      Y  : constant Object := Store.Argument (3);
      Op_Info : constant Operator_Record := Ops.Element (Op);
   begin
      if Is_Integer (X) and then Is_Integer (Y) then
         case Op_Info.Class is
            when Arithmetic =>
               return Op_Info.Integer_Op
                 (Store, To_Integer (X), To_Integer (Y));
            when Comparison =>
               return To_Object
                 (Compare_Integer
                    (Store, Op_Info.Allowed,
                     To_Integer (X), To_Integer (Y)));
         end case;
      elsif Real.Is_Real (Store, X) or else Real.Is_Real (Store, Y) then
         declare
            Real_X : constant Lith_Real := Real.To_Real (Store, X);
            Real_Y : constant Lith_Real := Real.To_Real (Store, Y);
         begin
            case Op_Info.Class is
               when Arithmetic =>
                  return Op_Info.Real_Op (Store, Real_X, Real_Y);
               when Comparison =>
                  return To_Object
                    (Compare_Real (Op_Info.Allowed, Real_X, Real_Y));
            end case;
         end;
      elsif Large_Integers.Is_Large_Integer (Store, X)
        or else Large_Integers.Is_Large_Integer (Store, Y)
      then
         declare
            Large_X : constant Large_Integers.Large_Integer_Object'Class :=
                        Large_Integers.To_Large_Integer (Store, X);
            Large_Y : constant Large_Integers.Large_Integer_Object'Class :=
                        Large_Integers.To_Large_Integer (Store, Y);
         begin
            case Op_Info.Class is
               when Arithmetic =>
                  return Op_Info.Large_Integer_Op
                    (Store, Large_X, Large_Y);
               when Comparison =>
                  return To_Object
                    (Compare_Large_Integer
                       (Store, Op_Info.Allowed,
                        Large_X, Large_Y));
            end case;
         end;
      else
         raise Constraint_Error with
           "bad objects for numeric application";
      end if;
   end Apply;

   ---------------------
   -- Compare_Integer --
   ---------------------

   function Compare_Integer
     (Store   : in out Object_Store'Class;
      Allowed : Allowed_Compares;
      X, Y    : Integer)
      return Boolean
   is
      pragma Unreferenced (Store);
      Result : constant Compare_Result :=
                 (if X < Y then LT
                  elsif X > Y then GT
                  else EQ);
   begin
      return Allowed (Result);
   end Compare_Integer;

   ---------------------------
   -- Compare_Large_Integer --
   ---------------------------

   function Compare_Large_Integer
     (Store   : in out Object_Store'Class;
      Allowed : Allowed_Compares;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
   begin
      return Allowed (X.Compare (Y));
   end Compare_Large_Integer;

   ------------------
   -- Compare_Real --
   ------------------

   function Compare_Real
     (Allowed : Allowed_Compares;
      X, Y  : Lith_Real)
      return Boolean
   is
      Result : constant Compare_Result :=
                 (if X < Y then LT
                  elsif X > Y then GT
                  else EQ);
   begin
      return Allowed (Result);
   end Compare_Real;

   --------------------
   -- Divide_Integer --
   --------------------

   function Divide_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object
   is
   begin
      Store.Push (X / Y);
      Store.Push (X mod Y);
      Store.Push (Nil);
      Store.Cons;
      Store.Cons;
      return Store.Pop;
   end Divide_Integer;

   --------------------------
   -- Divide_Large_Integer --
   --------------------------

   function Divide_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
   is
      use Large_Integers;
      Z : Large_Integer_Object'Class := X;
      R : Integer;
   begin
      Z.Divide (To_Integer (Y), R);
      Store.Push (To_Object (Store, Z));
      Store.Push (To_Object (R));
      Store.Push (Nil);
      Store.Cons;
      Store.Cons;
      return Store.Pop;
   end Divide_Large_Integer;

   -----------------
   -- Divide_Real --
   -----------------

   function Divide_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object
   is
      use Lith.Objects.Real;
      Z : constant Lith_Real := X * Y;
   begin
      return To_Object (Store, Z);
   end Divide_Real;

   ----------------------
   -- Multiply_Integer --
   ----------------------

   function Multiply_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object
   is
   begin
      if X = 0 or else Y = 0 then
         return To_Object (Integer'(0));
      elsif Integer'Last / abs X <= Y
        or else not In_Object_Range (X * Y)
      then
         declare
            Large_X : Large_Integers.Large_Integer_Object'Class :=
                        Large_Integers.To_Large_Integer (X);
         begin
            Large_X.Multiply (Y);
            return Large_Integers.To_Object
              (Store, Large_X);
         end;
      else
         return To_Object (X * Y);
      end if;
   end Multiply_Integer;

   ----------------------------
   -- Multiply_Large_Integer --
   ----------------------------

   function Multiply_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
   is
      use Lith.Objects.Large_Integers;
      A : Large_Integer_Object'Class := X;
      B : Integer;
   begin
      if not Y.In_Integer_Range then
         A := Y;
         B := X.To_Integer;
      else
         B := Y.To_Integer;
      end if;
      A.Multiply (B);
      return To_Object (Store, A);
   end Multiply_Large_Integer;

   -------------------
   -- Multiply_Real --
   -------------------

   function Multiply_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object
   is
      use Lith.Objects.Real;
      Z : constant Lith_Real := X * Y;
   begin
      return To_Object (Store, Z);
   end Multiply_Real;

   --------------
   -- Operator --
   --------------

   procedure Operator
     (Symbol           : Wide_Wide_String;
      Identity         : Integer;
      Integer_Op       : Integer_Op_Function;
      Large_Integer_Op : Large_Integer_Op_Function;
      Real_Op          : Real_Op_Function)
   is
   begin
      Ops.Insert (Lith.Objects.Symbols.Get_Symbol (Symbol),
                  (Arithmetic, Identity, Integer_Op, Large_Integer_Op,
                   Real_Op));
   end Operator;

   --------------
   -- Operator --
   --------------

   procedure Operator
     (Symbol               : Wide_Wide_String;
      Allowed              : Allowed_Compares)
   is
   begin
      Ops.Insert (Lith.Objects.Symbols.Get_Symbol (Symbol),
                  (Comparison, Allowed));
   end Operator;

   ----------------------
   -- Subtract_Integer --
   ----------------------

   function Subtract_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Integer)
      return Object
   is
      Z : constant Integer := X - Y;
   begin
      if Lith.Objects.In_Object_Range (Z) then
         return Lith.Objects.To_Object (Z);
      else
         return Large_Integers.To_Object
           (Store, Large_Integers.To_Large_Integer (Z));
      end if;
   end Subtract_Integer;

   ----------------------------
   -- Subtract_Large_Integer --
   ----------------------------

   function Subtract_Large_Integer
     (Store   : in out Object_Store'Class;
      X, Y    : Large_Integers.Large_Integer_Object'Class)
      return Object
   is
      use Lith.Objects.Large_Integers;
      A : Large_Integer_Object'Class := X;
      B : Large_Integer_Object'Class := Y;
   begin
      B.Negate;
      A.Add (B);
      return To_Object (Store, A);
   end Subtract_Large_Integer;

   -------------------
   -- Subtract_Real --
   -------------------

   function Subtract_Real
     (Store : in out Object_Store'Class;
      X, Y  : Lith_Real)
      return Object
   is
      use Lith.Objects.Real;
      Z : constant Lith_Real := X - Y;
   begin
      return To_Object (Store, Z);
   end Subtract_Real;

end Lith.Primitives.ALU;

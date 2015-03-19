package Lith.Objects.Numbers is

   type Compare is (LT, EQ, GT);

   procedure Ensure_Inexact
     (Store : in out Object_Store'Class);
   --  ensures that the number on top of the stack is inexact

   procedure Ensure_Exact
     (Store : in out Object_Store'Class);
   --  ensures that the number on top of the stack is exact

   procedure Push_Float
     (Store : in out Object_Store'Class;
      Text  : Wide_Wide_String);
   --  Interprets Text as a floating point number and pushes it

   --  exact number arithmetic

   function Is_Exact_Number
     (Store : Object_Store'Class;
      Item  : Object) return Boolean;

   function Is_Real_Number
     (Store : Object_Store'Class;
      Item  : Object) return Boolean;

   function Is_Rational_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean;

   function Is_Integral_Number
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean;

   procedure Exact_Add
     (Store : in out Object_Store'Class);

   procedure Exact_Negate
     (Store : in out Object_Store'Class);

   procedure Exact_Subtract
     (Store : in out Object_Store'Class);

   procedure Exact_Multiply
     (Store : in out Object_Store'Class);

   procedure Exact_Divide
     (Store : in out Object_Store'Class);

   --  pop the top two objects off the stack, perform the indicated operation,
   --  and push the result.  Both objects must be numbers (of any kind).
   --  The result is the simplest type which can hold it.
   --  Divide_Exact pushes two results onto the stack: the top is the
   --  remainder, the second element is the quotient

   function Exact_Compare
     (Store : in out Object_Store'Class)
      return Compare;
   --  Pop k1 off the stack. Let k2 be the new stack top.
   --  Return (if k1 < k2 then LT elsif k1 > k2 then GT else EQ)
   --  k2 remains the top of the stack.

   --  inexact number arithmetic

   procedure Inexact_Add
     (Store : in out Object_Store'Class);

   procedure Inexact_Divide
     (Store : in out Object_Store'Class);

   procedure Inexact_Multiply
     (Store : in out Object_Store'Class);

   procedure Inexact_Subtract
     (Store : in out Object_Store'Class);

   --  pop the top two Real off the stack, perform the indicated operation,
   --  and push the result.

   procedure Inexact_Negate
     (Store : in out Object_Store'Class);
   --  Pop a Real number off the stack, negate it, and push it back

   function Inexact_Compare
     (Store : in out Object_Store'Class)
      return Compare;
   --  Same as Exact_Compare, but for inexact numbers
   --  Result of comparing Complex numbers can only be interpreted
   --  as equal/not equal.

end Lith.Objects.Numbers;

package Lith.Objects.Numbers.Exact is

   --  exact number arithmetic

   function Is_Exact_Number
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

   procedure Add
     (Store : in out Object_Store'Class);

   procedure Negate
     (Store : in out Object_Store'Class);

   procedure Subtract
     (Store : in out Object_Store'Class);

   procedure Multiply
     (Store : in out Object_Store'Class);

   procedure Divide
     (Store : in out Object_Store'Class);

   --  pop the top two objects off the stack, perform the indicated operation,
   --  and push the result.  Both objects must be numbers (of any kind).
   --  The result is the simplest type which can hold it.
   --  Divide pushes two results onto the stack: the top is the remainder,
   --  the second element is the quotient

end Lith.Objects.Numbers.Exact;

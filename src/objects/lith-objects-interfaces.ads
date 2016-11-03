package Lith.Objects.Interfaces is

   type Simple_Evaluator is access
     function (Store       : in out Object_Store'Class)
     return Object;

   procedure Define_Function
     (Name           : String;
      Eval           : Simple_Evaluator);

   procedure Define_Function
     (Name           : String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator);

   procedure Define_Function
     (Store          : not null access Object_Store'Class;
      Name           : String;
      Eval           : Simple_Evaluator);

   type Function_Argument_Type is private;

   function Any_Argument_Type return Function_Argument_Type;
   function Atom_Argument return Function_Argument_Type;
   function Integer_Argument return Function_Argument_Type;
   function String_Argument return Function_Argument_Type;
   function Symbol_Argument return Function_Argument_Type;
   function List_Argument return Function_Argument_Type;

   type Argument_Validator_Function is access
     function (Store : in out Object_Store'Class;
               Value : Object)
     return Boolean;

   type Simple_Argument_Validator_Function is access
     function (Value : Object)
               return Boolean;

   function Custom_Argument
     (Validator : Argument_Validator_Function)
      return Function_Argument_Type;

   function Custom_Argument
     (Validator : Simple_Argument_Validator_Function)
      return Function_Argument_Type;

   type Argument_Type_Array is
     array (Positive range <>) of Function_Argument_Type;

   procedure Define_Function
     (Name      : String;
      Arguments : Argument_Type_Array;
      Eval      : Simple_Evaluator);

   type Root_Function_Interface is interface;

   function Evaluate
     (Fn    : Root_Function_Interface;
      Name  : Symbol_Type;
      Store : in out Object_Store'Class)
      return Object
      is abstract;

   procedure Define_Function
     (Name   : String;
      Eval   : Root_Function_Interface'Class);

   function Evaluate
     (Store       : in out Object_Store'Class;
      Fn          : Function_Type;
      Environment : Object)
      return Object;

   generic
      Type_Name : String;
      Type_Predicate_Name  : String := "?";
   package Registration is
      procedure Create_Standard_Objects
        (Store : in out Object_Store'Class);
   end Registration;

   procedure Bind_Primitives
     (Store : in out Object_Store'Class);

private

   type Function_Argument_Type is
      record
         Simple_Validator : Simple_Argument_Validator_Function;
         Validator        : Argument_Validator_Function;
      end record;

   function Custom_Argument
     (Validator : Argument_Validator_Function)
      return Function_Argument_Type
   is ((null, Validator));

   function Custom_Argument
     (Validator : Simple_Argument_Validator_Function)
      return Function_Argument_Type
   is ((Validator, null));

   function Is_Any (Store : in out Object_Store'Class;
                    Value : Object)
                    return Boolean
   is (True);

   function Any_Argument_Type return Function_Argument_Type
   is (Custom_Argument (Is_Any'Access));

   function Atom_Argument return Function_Argument_Type
   is (Custom_Argument (Is_Atom'Access));

   function Integer_Argument return Function_Argument_Type
   is (Custom_Argument (Is_Integer'Access));

   function String_Argument return Function_Argument_Type
   is (Custom_Argument (Is_String'Access));

   function Symbol_Argument return Function_Argument_Type
   is (Custom_Argument (Is_Symbol'Access));

   function List_Argument return Function_Argument_Type
   is (Custom_Argument (Is_Pair'Access));

end Lith.Objects.Interfaces;

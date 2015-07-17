package Lith.Objects.Interfaces is

   type Simple_Evaluator is access
     function (Store       : in out Object_Store'Class)
     return Object;

   procedure Define_Function
     (Name           : String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator);

   type Root_Function_Interface is interface;

   function Evaluate
     (Fn : Root_Function_Interface;
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

end Lith.Objects.Interfaces;

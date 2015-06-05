package Lith.Objects.Interfaces is

   type Evaluator is access
     function (Store       : in out Object_Store'Class;
               Environment : Object)
               return Object;

   type Simple_Evaluator is access
     function (Store       : in out Object_Store'Class)
     return Object;

   procedure Define_Function
     (Name           : String;
      Argument_Count : Natural;
      Strict         : Boolean;
      Eval           : Evaluator);

   procedure Define_Function
     (Name           : String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator);

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

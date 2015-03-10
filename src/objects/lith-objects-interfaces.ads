package Lith.Objects.Interfaces is

   type Evaluator is access
     function (Store       : in out Object_Store'Class;
               Arguments   : Array_Of_Objects;
               Environment : Object)
               return Object;

   type Simple_Evaluator is access
     function (Store       : in out Object_Store'Class;
               Arguments   : Array_Of_Objects)
     return Object;

   procedure Define_Function
     (Name           : Wide_Wide_String;
      Argument_Count : Natural;
      Strict         : Boolean;
      Eval           : Evaluator);

   procedure Define_Function
     (Name           : Wide_Wide_String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator);

   function Evaluate
     (Store       : in out Object_Store'Class;
      Fn          : Function_Type;
      Arguments   : Array_Of_Objects;
      Environment : Object)
      return Object;

end Lith.Objects.Interfaces;

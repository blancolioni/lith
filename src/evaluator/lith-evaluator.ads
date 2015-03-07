with Lith.Objects;

package Lith.Evaluator is

   Evaluation_Error : exception;

   function Evaluate
     (Store : in out Lith.Objects.Object_Store'Class;
      Expr  : Lith.Objects.Object;
      Env   : Lith.Objects.Object)
      return Lith.Objects.Object;

end Lith.Evaluator;

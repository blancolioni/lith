with Lith.Objects;

package Lith.Primitives is

   procedure Add_Primitives;

   function Evaluate_Define
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object;

end Lith.Primitives;

with Lith.Objects;

package Lith.Primitives.ALU is

   procedure Add_Operators;

   function Apply
     (Store : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

end Lith.Primitives.ALU;

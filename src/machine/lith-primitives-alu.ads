with Lith.Objects;

package Lith.Primitives.ALU is

   procedure Add_Operators;

   function Apply (Op   : Lith.Objects.Symbol_Type;
                   Args : Lith.Objects.Array_Of_Objects)
                   return Lith.Objects.Object;

end Lith.Primitives.ALU;

private with Ada.Containers.Indefinite_Vectors;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Lith.Objects.Symbol_Vectors is

   pragma Elaborate_Body;

   type Vector is tagged private;

   function Element (Container : Vector;
                     Index     : Symbol_Type)
                     return Element_Type;

   procedure New_Element (Container  : in out Vector;
                          Value      : Element_Type;
                          New_Symbol : out Symbol_Type);

private

   package Internal_Symbol_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Element_Type);

   type Vector is tagged
      record
         Internal : Internal_Symbol_Vectors.Vector;
      end record;

end Lith.Objects.Symbol_Vectors;

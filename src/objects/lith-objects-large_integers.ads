private with Ada.Containers.Vectors;

package Lith.Objects.Large_Integers is

   type Large_Integer_Object is
     new External_Object_Interface with private;

   overriding function Name
     (Item : Large_Integer_Object)
      return Wide_Wide_String;

   overriding function Print (Item  : Large_Integer_Object;
                              Store : in out Object_Store'Class)
                              return Wide_Wide_String;

   overriding function Equal (X, Y  : Large_Integer_Object;
                              Store : Object_Store'Class)
                              return Boolean;

   procedure Add (X : in out Large_Integer_Object'Class;
                  Y : Large_Integer_Object'Class);

   procedure Add (X : in out Large_Integer_Object'Class;
                  Y : Integer);

   procedure Multiply (X : in out Large_Integer_Object'Class;
                       Y : Integer);

   procedure Divide (X         : in out Large_Integer_Object'Class;
                     Y         : Integer;
                     Remainder : out Integer);

   function Compare
     (X, Y : Large_Integer_Object'Class)
      return Compare_Result;

   function In_Integer_Range (X : Large_Integer_Object'Class) return Boolean;
   function To_Integer (X : Large_Integer_Object'Class) return Integer
     with Pre => In_Integer_Range (X);

   procedure Negate (X : in out Large_Integer_Object'Class);

   Zero : constant Large_Integer_Object;

   function Is_Zero
     (X : Large_Integer_Object'Class)
      return Boolean;

   function Is_Large_Integer
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean;

   function To_Large_Integer
     (Item : Integer)
      return Large_Integer_Object'Class;

   function To_Large_Integer
     (Store : Object_Store'Class;
      Item  : Object)
      return Large_Integer_Object'Class;

   function To_Object
     (Store : in out Object_Store'Class;
      Value : Large_Integer_Object'Class)
      return Lith.Objects.Object;

private

   Element_Bits : constant := 32;
   type Element_Type is mod 2 ** Element_Bits;

   package Object_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Element_Type);

   type Large_Integer_Object is
     new External_Object_Interface with
      record
         Negative : Boolean;
         Xs       : Object_Vectors.Vector;
      end record;

   Zero : constant Large_Integer_Object :=
            (Negative => False,
             Xs       => Object_Vectors.Empty_Vector);

end Lith.Objects.Large_Integers;

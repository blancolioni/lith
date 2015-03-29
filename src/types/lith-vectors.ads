private with Ada.Containers.Vectors;

with Lith.Objects;

package Lith.Vectors is

   type Lith_Vector_Type is
     new Lith.Objects.External_Object_Interface
   with private;

   overriding function Name
     (Item  : Lith_Vector_Type)
      return Wide_Wide_String;

   overriding function Print
     (Item  : Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return Wide_Wide_String;

   overriding function Equal
     (X, Y  : Lith_Vector_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Finalize
     (Item  : in out Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class);

   overriding procedure Mark
     (Item  : in out Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class);

   procedure Register
     (Store : in out Lith.Objects.Object_Store'Class);

private

   package Object_Vectors is
     new Ada.Containers.Vectors
       (Natural, Lith.Objects.Object, Lith.Objects."=");

   type Lith_Vector_Type is
     new Lith.Objects.External_Object_Interface with
      record
         V : Object_Vectors.Vector;
      end record;

end Lith.Vectors;

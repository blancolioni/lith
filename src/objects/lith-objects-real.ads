package Lith.Objects.Real is

   type Real_Object is
     new External_Object_Interface with private;

   overriding function Name
     (Item : Real_Object)
      return Wide_Wide_String;

   overriding function Print
     (Item  : Real_Object;
      Store : in out Object_Store'Class)
      return Wide_Wide_String;

   overriding function Equal
     (X, Y  : Real_Object;
      Store : Object_Store'Class)
      return Boolean;

   function Value (X : Real_Object'Class) return Lith_Real;

   function To_Real
     (Store : Object_Store'Class;
      X     : Object)
      return Lith_Real;

   function To_Real_Object
     (Value : Lith_Real)
      return Real_Object'Class;

   function To_Object
     (Store : in out Object_Store'Class;
      X     : Lith_Real)
      return Object;

   function Is_Real
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean;

   function To_Real
     (Text : Wide_Wide_String)
      return Real_Object'Class;

private

   type Real_Object is
     new External_Object_Interface with
      record
         Value : Lith_Real;
      end record;

end Lith.Objects.Real;

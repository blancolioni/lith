with Lith.Objects.Large_Integers;

package body Lith.Objects.Real is

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Real_Object;
      Store : Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
   begin
      return X.Value = Y.Value;
   end Equal;

   -------------
   -- Is_Real --
   -------------

   function Is_Real
     (Store : Object_Store'Class;
      Item  : Object)
      return Boolean
   is
   begin
      return Is_External_Object (Item)
        and then Store.Get_External_Object (Item).all in Real_Object'Class;
   end Is_Real;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Real_Object)
      return Wide_Wide_String
   is
      pragma Unreferenced (Item);
   begin
      return "real";
   end Name;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Real_Object;
      Store : in out Object_Store'Class)
      return Wide_Wide_String
   is
      pragma Unreferenced (Store);
   begin
      return Lith_Real'Wide_Wide_Image (Item.Value);
   end Print;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (Store : in out Object_Store'Class;
      X     : Lith_Real)
      return Object
   is
   begin
      return Store.Create_External_Reference
        (Real_Object'(Value => X));
   end To_Object;

   -------------
   -- To_Real --
   -------------

   function To_Real
     (Store : Object_Store'Class;
      X     : Object)
      return Lith_Real
   is
   begin
      if Is_Integer (X) then
         return Lith_Real (To_Integer (X));
      elsif Large_Integers.Is_Large_Integer (Store, X) then
         return Lith_Real
           (Large_Integers.To_Large_Integer (Store, X).To_Integer);
      else
         return Real_Object'Class (Store.Get_External_Object (X).all).Value;
      end if;
   end To_Real;

   -------------
   -- To_Real --
   -------------

   function To_Real
     (Text : Wide_Wide_String)
      return Real_Object'Class
   is
   begin
      return Real_Object'(Value => Lith_Real'Wide_Wide_Value (Text));
   end To_Real;

   --------------------
   -- To_Real_Object --
   --------------------

   function To_Real_Object
     (Value : Lith_Real)
      return Real_Object'Class
   is
   begin
      return Real_Object'(Value => Value);
   end To_Real_Object;

   -----------
   -- Value --
   -----------

   function Value (X : Real_Object'Class) return Lith_Real is
   begin
      return X.Value;
   end Value;

end Lith.Objects.Real;

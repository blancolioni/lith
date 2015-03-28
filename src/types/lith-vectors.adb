with Ada.Strings.Wide_Wide_Unbounded;

with Lith.Objects.Interfaces;

package body Lith.Vectors is

   function Evaluate_Is_Vector
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Vector
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Lith_Vector_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
      use type Object_Vectors.Vector;
   begin
      return X = Y;
   end Equal;

   ------------------------
   -- Evaluate_Is_Vector --
   ------------------------

   function Evaluate_Is_Vector
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Item : constant Object := Arguments (Arguments'First);
      Result : constant Boolean :=
                 Is_External_Object (Item)
                   and then Get_External_Object (Store, Item).all in
                   Lith_Vector_Type'Class;
   begin
      return To_Object (Result);
   end Evaluate_Is_Vector;

   ---------------------
   -- Evaluate_Vector --
   ---------------------

   function Evaluate_Vector
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Result : Lith_Vector_Type;
   begin
      for Arg of Arguments loop
         Result.V.Append (Arg);
      end loop;
      return Store.Create_External_Reference (Result);
   end Evaluate_Vector;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Item  : in out Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Store);
   begin
      null;
   end Finalize;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return Wide_Wide_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String :=
                 Null_Unbounded_Wide_Wide_String;
   begin
      for Element of Item.V loop
         if Result /= Null_Unbounded_Wide_Wide_String then
            Result := Result & " ";
         end if;
         Result := Result & Store.Show (Element);
      end loop;
      return To_Wide_Wide_String (Result);
   end Print;

   --------------
   -- Register --
   --------------

   procedure Register is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("vector?", 1, Evaluate_Is_Vector'Access);
      Define_Function ("vector", 1, Evaluate_Vector'Access);
   end Register;

end Lith.Vectors;

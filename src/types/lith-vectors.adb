with Ada.Strings.Unbounded;

with Lith.Objects.Interfaces;

package body Lith.Vectors is

   function Evaluate_Vector
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Vector_Length
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Vector_Ref
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Vector_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
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

   ---------------------
   -- Evaluate_Vector --
   ---------------------

   function Evaluate_Vector
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Result : Lith_Vector_Type;
   begin
      for I in 1 .. Store.Argument_Count loop
         Result.V.Append (Store.Argument (I));
      end loop;
      return Store.Create_External_Reference (Result);
   end Evaluate_Vector;

   ----------------------------
   -- Evaluate_Vector_Length --
   ----------------------------

   function Evaluate_Vector_Length
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      return To_Object
        (Lith_Vector_Type
           (Store.Get_External_Object
                (Store.Argument (1)).all).V.Last_Index + 1);
   end Evaluate_Vector_Length;

   -------------------------
   -- Evaluate_Vector_Ref --
   -------------------------

   function Evaluate_Vector_Ref
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      V : Object_Vectors.Vector renames Lith_Vector_Type
        (Store.Get_External_Object
           (Store.Argument (1)).all).V;
      Index : constant Integer := To_Integer (Store.Argument (2));
   begin
      return V.Element (Index);
   end Evaluate_Vector_Ref;

   -------------------------
   -- Evaluate_Vector_Set --
   -------------------------

   function Evaluate_Vector_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      V : Object_Vectors.Vector renames Lith_Vector_Type
        (Store.Get_External_Object
           (Store.Argument (1)).all).V;
      Index : constant Integer := To_Integer (Store.Argument (2));
      Value : constant Object := Store.Argument (3);
   begin
      V.Replace_Element (Index, Value);
      return No_Value;
   end Evaluate_Vector_Set;

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

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Item  : in out Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
   is
   begin
      for E of Item.V loop
         Store.Mark (E);
      end loop;
   end Mark;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item  : Lith_Vector_Type)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "vector";
   end Name;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Lith_Vector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String :=
                 Null_Unbounded_String;
   begin
      for Element of Item.V loop
         if Result /= Null_Unbounded_String then
            Result := Result & " ";
         end if;
         Result := Result & Store.Show (Element);
      end loop;
      return To_String (Result);
   end Print;

   --------------
   -- Register --
   --------------

   procedure Register
     (Store : in out Lith.Objects.Object_Store'Class)
   is
      use Lith.Objects.Interfaces;
      package Registration is
        new Lith.Objects.Interfaces.Registration
          ("vector");
   begin
      Registration.Create_Standard_Objects (Store);
      Define_Function ("vector", 1, Evaluate_Vector'Access);
      Define_Function ("vector-length", 1, Evaluate_Vector_Length'Access);
      Define_Function ("vector-ref", 2, Evaluate_Vector_Ref'Access);
      Define_Function ("vector-set!", 2, Evaluate_Vector_Set'Access);
   end Register;

end Lith.Vectors;

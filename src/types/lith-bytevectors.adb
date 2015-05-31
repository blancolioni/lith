with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with Lith.Objects.Interfaces;

package body Lith.Bytevectors is

   function Evaluate_Bytevector
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Bytevector_Length
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Bytevector_Ref
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Bytevector_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Lith_Bytevector_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
   begin
      return X.Bytes.all = Y.Bytes.all;
   end Equal;

   -------------------------
   -- Evaluate_Bytevector --
   -------------------------

   function Evaluate_Bytevector
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      use Ada.Characters.Conversions;
      Result : Lith_Bytevector_Type;
   begin
      for I in 1 .. Store.Argument_Count loop
         declare
            Arg : constant Object := Store.Argument (I);
         begin
            if not Is_Integer (Arg) then
               raise Constraint_Error with
                 "bytevector: expected an integer but found "
                 & To_String (Store.Show (Arg));
            elsif To_Integer (Arg) not in 0 .. 255 then
               raise Constraint_Error with
                 "bytevector: " & To_String (Store.Show (Arg))
                 & " is not in byte range (0 .. 255)";
            end if;
         end;
      end loop;

      Result.Bytes := new Array_Of_Bytes (0 .. Store.Argument_Count - 1);

      for I in 1 .. Store.Argument_Count loop
         Result.Bytes (I - 1) := Byte (To_Integer (Store.Argument (I)));
      end loop;
      return Store.Create_External_Reference (Result);

   end Evaluate_Bytevector;

   --------------------------------
   -- Evaluate_Bytevector_Length --
   --------------------------------

   function Evaluate_Bytevector_Length
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      BV : constant Array_Of_Bytes_Access :=
             Lith_Bytevector_Type
               (Store.Get_External_Object
                  (Store.Argument (1)).all).Bytes;
   begin
      return To_Object (Integer'(BV'Length));
   end Evaluate_Bytevector_Length;

   -----------------------------
   -- Evaluate_Bytevector_Ref --
   -----------------------------

   function Evaluate_Bytevector_Ref
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      BV : constant Array_Of_Bytes_Access :=
             Lith_Bytevector_Type
               (Store.Get_External_Object
                  (Store.Argument (1)).all).Bytes;
      Index : constant Integer :=
                To_Integer (Store.Argument (2));
   begin
      return To_Object (Integer (BV (Index)));
   end Evaluate_Bytevector_Ref;

   -----------------------------
   -- Evaluate_Bytevector_Set --
   -----------------------------

   function Evaluate_Bytevector_Set
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      BV : constant Array_Of_Bytes_Access :=
             Lith_Bytevector_Type
               (Store.Get_External_Object
                  (Store.Argument (1)).all).Bytes;
      Index : constant Integer :=
                To_Integer (Store.Argument (2));
      Value : constant Integer :=
                To_Integer (Store.Argument (3));
   begin
      BV (Index) := Byte (Value);
      return No_Value;
   end Evaluate_Bytevector_Set;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Item  : in out Lith_Bytevector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
   is
      pragma Unreferenced (Store);
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Array_Of_Bytes, Array_Of_Bytes_Access);
   begin
      Free (Item.Bytes);
   end Finalize;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item  : Lith_Bytevector_Type)
      return Wide_Wide_String
   is
      pragma Unreferenced (Item);
   begin
      return "bytevector";
   end Name;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Lith_Bytevector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return Wide_Wide_String
   is
      pragma Unreferenced (Store);
      use Ada.Strings.Wide_Wide_Unbounded;
      Result : Unbounded_Wide_Wide_String :=
                 Null_Unbounded_Wide_Wide_String;
   begin
      for B of Item.Bytes.all loop
         Result := Result & Byte'Wide_Wide_Image (B);
      end loop;

      Result := "#u8(" & Result & ")";
      return To_Wide_Wide_String (Result);
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
          ("bytevector");
   begin
      Registration.Create_Standard_Objects (Store);
      Define_Function ("bytevector", 1, Evaluate_Bytevector'Access);
      Define_Function ("bytevector-length", 1,
                       Evaluate_Bytevector_Length'Access);
      Define_Function ("bytevector-u8-ref", 2,
                       Evaluate_Bytevector_Ref'Access);
      Define_Function ("bytevector-u8-set!", 3,
                       Evaluate_Bytevector_Set'Access);
   end Register;

end Lith.Bytevectors;

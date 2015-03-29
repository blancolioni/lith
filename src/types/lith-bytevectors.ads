with Lith.Objects;

package Lith.Bytevectors is

   type Lith_Bytevector_Type is
     new Lith.Objects.External_Object_Interface
   with private;

   overriding function Name
     (Item  : Lith_Bytevector_Type)
      return Wide_Wide_String;

   overriding function Print
     (Item  : Lith_Bytevector_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return Wide_Wide_String;

   overriding function Equal
     (X, Y  : Lith_Bytevector_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Finalize
     (Item  : in out Lith_Bytevector_Type;
      Store : in out Lith.Objects.Object_Store'Class);

   procedure Register
     (Store : in out Lith.Objects.Object_Store'Class);

private

   type Byte is mod 256;
   type Array_Of_Bytes is array (Natural range <>) of Byte;
   type Array_Of_Bytes_Access is access Array_Of_Bytes;

   type Lith_Bytevector_Type is
     new Lith.Objects.External_Object_Interface with
      record
         Bytes : Array_Of_Bytes_Access;
      end record;

end Lith.Bytevectors;

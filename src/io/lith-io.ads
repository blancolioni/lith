with Lith.Objects;

package Lith.IO is

   type Port_Type is
     abstract new Lith.Objects.External_Object_Interface
   with private;

   overriding function Print (Item : Port_Type) return Wide_Wide_String;
   overriding function Equal (X, Y : Port_Type) return Boolean;
   overriding procedure Finalize (Item : in out Port_Type);

   procedure Close (Port : in out Port_Type)
   is abstract;

   procedure Initialise_IO;

private

   type Port_Type is
     abstract new Lith.Objects.External_Object_Interface
   with record
      Open : Boolean := False;
      Input : Boolean;
   end record;

end Lith.IO;

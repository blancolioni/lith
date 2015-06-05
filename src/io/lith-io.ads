with Lith.Objects;

package Lith.IO is

   type Port_Type is
     abstract new Lith.Objects.External_Object_Interface
   with private;

   overriding function Print
     (Item  : Port_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return String;

   overriding function Equal
     (X, Y  : Port_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean;

   overriding procedure Finalize
     (Item  : in out Port_Type;
      Store : in out Lith.Objects.Object_Store'Class);

   function Is_Input (Item : Port_Type'Class) return Boolean
     with Pre => Item.Is_Open;

   function Is_Output (Item : Port_Type'Class) return Boolean
     with Pre => Item.Is_Open;

   function Is_Open (Item : Port_Type'Class) return Boolean;

   function End_Of_File (Port : Port_Type) return Boolean
                         is abstract;

   procedure Close (Port : in out Port_Type)
   is abstract;

   function Get_Port (Store : Lith.Objects.Object_Store'Class;
                      Item  : Lith.Objects.Object)
                      return Port_Type'Class
     with Pre => Lith.Objects.Is_External_Object (Item)
     and then Store.Get_External_Object (Item).all in Port_Type'Class;

   procedure Initialise_IO;

private

   type Port_Type is
     abstract new Lith.Objects.External_Object_Interface
   with record
      Open   : Boolean := False;
      Input  : Boolean;
      Output : Boolean;
   end record;

end Lith.IO;

with Lith.Objects.Interfaces;

with Lith.IO.Text_IO;

package body Lith.IO is

   function Evaluate_Close_Port
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   -----------
   -- Equal --
   -----------

   overriding function Equal (X, Y : Port_Type) return Boolean is
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return False;
   end Equal;

   -------------------------
   -- Evaluate_Close_Port --
   -------------------------

   function Evaluate_Close_Port
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Port : Port_Type'Class :=
               Port_Type'Class
                 (Store.Get_External_Object
                    (Arguments (Arguments'First)));
   begin
      Port.Close;
      return Lith.Objects.No_Value;
   end Evaluate_Close_Port;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Port_Type) is
   begin
      if Item.Open then
         Port_Type'Class (Item).Close;
      end if;
   end Finalize;

   -------------------
   -- Initialise_IO --
   -------------------

   procedure Initialise_IO is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("close-port", 1, Evaluate_Close_Port'Access);
      Define_Function ("open-output-file", 1,
                       Lith.IO.Text_IO.Evaluate_Open_Output_File'Access);
      Define_Function ("open-input-file", 1,
                       Lith.IO.Text_IO.Evaluate_Open_Input_File'Access);
   end Initialise_IO;

   -----------
   -- Print --
   -----------

   overriding function Print (Item : Port_Type) return Wide_Wide_String is
      pragma Unreferenced (Item);
   begin
      return "#<port>";
   end Print;

end Lith.IO;

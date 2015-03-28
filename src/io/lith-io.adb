with Lith.Objects.Interfaces;

with Lith.IO.Text_IO;

with Lith.Symbols;

package body Lith.IO is

   function Evaluate_Close_Port
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Port_Attribute
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Port_Type;
      Store : Lith.Objects.Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
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
                    (Arguments (Arguments'First)).all);
   begin
      Port.Close;
      return Lith.Objects.No_Value;
   end Evaluate_Close_Port;

   -----------------------------
   -- Evaluate_Port_Attribute --
   -----------------------------

   function Evaluate_Port_Attribute
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Port_Object : constant Object := Arguments (Arguments'First);
      Result : Boolean;
   begin
      if not Is_External_Object (Port_Object) then
         Result := False;
      else
         declare
            Ext : constant access External_Object_Interface'Class :=
                    Store.Get_External_Object (Port_Object);
         begin
            if Ext.all not in Port_Type'Class then
               Result := False;
            else
               declare
                  Port      : constant Port_Type'Class :=
                                Port_Type'Class (Ext.all);
                  Attribute : constant Wide_Wide_String :=
                                Lith.Symbols.Get_Name
                                  (To_Symbol
                                     (Arguments (Arguments'First + 1)));
               begin
                  if Attribute = "input" then
                     Result := Port.Input;
                  elsif Attribute = "output" then
                     Result := Port.Output;
                  elsif Attribute = "textual" then
                     Result := Lith.IO.Text_IO.Is_Text_Port (Port);
                  elsif Attribute = "binary" then
                     Result := False;
                  elsif Attribute = "port" then
                     Result := True;
                  end if;
               end;
            end if;
         end;
      end if;

      return To_Object (Result);

   end Evaluate_Port_Attribute;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Item  : in out Port_Type;
      Store : in out Lith.Objects.Object_Store'Class)
   is
      pragma Unreferenced (Store);
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
      Define_Function ("port-attribute?", 2,
                       Evaluate_Port_Attribute'Access);
   end Initialise_IO;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Port_Type;
      Store : in out Lith.Objects.Object_Store'Class)
      return Wide_Wide_String
   is
      pragma Unreferenced (Store);
      pragma Unreferenced (Item);
   begin
      return "#<port>";
   end Print;

end Lith.IO;

with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;

package body Lith.IO.Text_IO is

   type File_Access is
     access Ada.Wide_Wide_Text_IO.File_Type;

   type Text_Port_Type is
     new Port_Type with
      record
         File : File_Access;
      end record;

   overriding procedure Close (Port : in out Text_Port_Type);
   overriding function Name (Port : Text_Port_Type) return Wide_Wide_String
   is ("textual-port");

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Ada.Wide_Wide_Text_IO.File_Type,
        File_Access);

   -----------
   -- Close --
   -----------

   overriding procedure Close (Port : in out Text_Port_Type) is
   begin
      Ada.Wide_Wide_Text_IO.Close (Port.File.all);
      Free (Port.File);
      Port.Open := False;
   end Close;

   ------------------------------
   -- Evaluate_Open_Input_File --
   ------------------------------

   function Evaluate_Open_Input_File
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Port : Text_Port_Type;
   begin
      Port.File := new Ada.Wide_Wide_Text_IO.File_Type;
      Ada.Wide_Wide_Text_IO.Open
        (Port.File.all, Ada.Wide_Wide_Text_IO.In_File,
         Ada.Characters.Conversions.To_String
           (Lith.Objects.To_String (Store, Store.Argument (1))));
      Port.Input := True;
      Port.Output := False;
      return Store.Create_External_Reference (Port);
   exception
      when others =>
         Free (Port.File);
         Port.Open := False;
         return Lith.Objects.False_Value;
   end Evaluate_Open_Input_File;

   -------------------------------
   -- Evaluate_Open_Output_File --
   -------------------------------

   function Evaluate_Open_Output_File
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Port : Text_Port_Type;
   begin
      Port.File := new Ada.Wide_Wide_Text_IO.File_Type;
      Ada.Wide_Wide_Text_IO.Create
        (Port.File.all, Ada.Wide_Wide_Text_IO.Out_File,
         Ada.Characters.Conversions.To_String
           (Lith.Objects.To_String (Store, Store.Argument (1))));
      Port.Open := True;
      Port.Input := False;
      Port.Output := True;
      return Store.Create_External_Reference (Port);
   exception
      when others =>
         Free (Port.File);
         Port.Open := False;
         return Lith.Objects.False_Value;
   end Evaluate_Open_Output_File;

   ------------------
   -- Is_Text_Port --
   ------------------

   function Is_Text_Port (Port : Port_Type'Class) return Boolean is
   begin
      return Port in Text_Port_Type'Class;
   end Is_Text_Port;

end Lith.IO.Text_IO;

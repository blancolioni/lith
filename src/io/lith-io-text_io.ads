private with Ada.Wide_Wide_Text_IO;
private with Ada.Strings.Wide_Wide_Unbounded;

with Lith.Objects;

package Lith.IO.Text_IO is

   type Text_Port_Type is
     new Port_Type with private;

   overriding function End_Of_File (Port : Text_Port_Type) return Boolean;

   function Peek_Char (Port : Text_Port_Type'Class) return Wide_Wide_Character;
   function Read_Char
     (Port : in out Text_Port_Type'Class)
      return Wide_Wide_Character;

   function Line (Port : Text_Port_Type'Class) return Positive;
   function Col (Port : Text_Port_Type'Class) return Positive;

   procedure Put_Back
     (Port : in out Text_Port_Type'Class;
      Text : Wide_Wide_String);

   function Evaluate_Open_Output_File
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Open_Input_File
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Is_Text_Port (Port : Port_Type'Class) return Boolean;

private

   type File_Access is
     access Ada.Wide_Wide_Text_IO.File_Type;

   type Text_Port_Type is
     new Port_Type with
      record
         Front : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
         File  : File_Access;
      end record;

   overriding procedure Close (Port : in out Text_Port_Type);
   overriding function Name (Port : Text_Port_Type) return Wide_Wide_String
   is ("textual-port");

end Lith.IO.Text_IO;

with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;

package body Lith.IO.Text_IO is

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

   ---------
   -- Col --
   ---------

   function Col (Port : Text_Port_Type'Class) return Positive is
   begin
      return Positive (Ada.Wide_Wide_Text_IO.Col (Port.File.all));
   end Col;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File (Port : Text_Port_Type) return Boolean is
   begin
      return Ada.Wide_Wide_Text_IO.End_Of_File (Port.File.all);
   end End_Of_File;

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
      declare
         Path : constant String :=
                  Ada.Characters.Conversions.To_String
                    (Lith.Objects.To_String (Store, Store.Argument (1)));
      begin
         Ada.Wide_Wide_Text_IO.Open
           (Port.File.all, Ada.Wide_Wide_Text_IO.In_File, Path);
      end;
      Port.Open := True;
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

   ----------
   -- Line --
   ----------

   function Line (Port : Text_Port_Type'Class) return Positive is
   begin
      return Positive (Ada.Wide_Wide_Text_IO.Line (Port.File.all));
   end Line;

   ---------------
   -- Peek_Char --
   ---------------

   function Peek_Char
     (Port : Text_Port_Type'Class)
      return Wide_Wide_Character
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Ch          : Wide_Wide_Character;
      End_Of_Line : Boolean;
   begin
      if Port.Front /= Null_Unbounded_Wide_Wide_String then
         return Element (Port.Front, 1);
      else
         Ada.Wide_Wide_Text_IO.Look_Ahead (Port.File.all, Ch, End_Of_Line);
         if End_Of_Line then
            return Wide_Wide_Character'Val (10);
         else
            return Ch;
         end if;
      end if;
   end Peek_Char;

   --------------
   -- Put_Back --
   --------------

   procedure Put_Back
     (Port : in out Text_Port_Type'Class;
      Text : Wide_Wide_String)
   is
      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      Port.Front := Text & Port.Front;
   end Put_Back;

   ---------------
   -- Read_Char --
   ---------------

   function Read_Char
     (Port : in out Text_Port_Type'Class)
      return Wide_Wide_Character
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      Ch          : Wide_Wide_Character;
   begin
      if Port.Front /= Null_Unbounded_Wide_Wide_String then
         Ch := Element (Port.Front, 1);
         Port.Front :=
           Unbounded_Slice (Port.Front, 2, Length (Port.Front) - 1);
         return Ch;
      elsif Ada.Wide_Wide_Text_IO.End_Of_Line (Port.File.all) then
         Ada.Wide_Wide_Text_IO.Skip_Line (Port.File.all);
         return Wide_Wide_Character'Val (10);
      else
         Ada.Wide_Wide_Text_IO.Get (Port.File.all, Ch);
         return Ch;
      end if;
   end Read_Char;

end Lith.IO.Text_IO;

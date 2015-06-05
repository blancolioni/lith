with Ada.Unchecked_Deallocation;

package body Lith.IO.Text_IO is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Ada.Text_IO.File_Type,
        File_Access);

   -----------
   -- Close --
   -----------

   overriding procedure Close (Port : in out Text_Port_Type) is
   begin
      Ada.Text_IO.Close (Port.File.all);
      Free (Port.File);
      Port.Open := False;
   end Close;

   ---------
   -- Col --
   ---------

   function Col (Port : Text_Port_Type'Class) return Positive is
   begin
      return Positive (Ada.Text_IO.Col (Port.File.all));
   end Col;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File (Port : Text_Port_Type) return Boolean is
   begin
      return Ada.Text_IO.End_Of_File (Port.File.all);
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
      Port.File := new Ada.Text_IO.File_Type;
      declare
         Path : constant String :=
                  Lith.Objects.To_String (Store, Store.Argument (1));
      begin
         Ada.Text_IO.Open
           (Port.File.all, Ada.Text_IO.In_File, Path);
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
      Port.File := new Ada.Text_IO.File_Type;
      Ada.Text_IO.Create
        (Port.File.all, Ada.Text_IO.Out_File,
         Lith.Objects.To_String (Store, Store.Argument (1)));
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
      return Positive (Ada.Text_IO.Line (Port.File.all));
   end Line;

   ---------------
   -- Peek_Char --
   ---------------

   function Peek_Char
     (Port : Text_Port_Type'Class)
      return Character
   is
      use Ada.Strings.Unbounded;
      Ch          : Character;
      End_Of_Line : Boolean;
   begin
      if Port.Front /= Null_Unbounded_String then
         return Element (Port.Front, 1);
      else
         Ada.Text_IO.Look_Ahead (Port.File.all, Ch, End_Of_Line);
         if End_Of_Line then
            return Character'Val (10);
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
      Text : String)
   is
      use Ada.Strings.Unbounded;
   begin
      Port.Front := Text & Port.Front;
   end Put_Back;

   ---------------
   -- Read_Char --
   ---------------

   function Read_Char
     (Port : in out Text_Port_Type'Class)
      return Character
   is
      use Ada.Strings.Unbounded;
      Ch          : Character;
   begin
      if Port.Front /= Null_Unbounded_String then
         Ch := Element (Port.Front, 1);
         Port.Front :=
           Unbounded_Slice (Port.Front, 2, Length (Port.Front) - 1);
         return Ch;
      elsif Ada.Text_IO.End_Of_Line (Port.File.all) then
         Ada.Text_IO.Skip_Line (Port.File.all);
         return Character'Val (10);
      else
         Ada.Text_IO.Get (Port.File.all, Ch);
         return Ch;
      end if;
   end Read_Char;

end Lith.IO.Text_IO;

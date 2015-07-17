with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Text_IO;

with Lith.Parser.Lexical.Characters;
with Lith.Parser.Lexical.Identifiers;

package body Lith.Parser.Lexical is

   package Line_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Source_Stream_Record is
      record
         Identity   : Unbounded_String;
         Lines      : Line_Vectors.Vector;
         Line       : Natural;
         Column     : Natural;
         Tok_Line   : Natural;
         Tok_Column : Natural;
         Ch         : Character;
         Tok        : Token;
         Tok_Text   : Unbounded_String;
         Tok_Char   : Character;
         EOF        : Boolean;
         Is_Port    : Boolean;
         Port       : access Lith.IO.Text_IO.Text_Port_Type'Class;
      end record;

   Empty_Stream : constant Source_Stream_Record :=
                    (Identity      => Null_Unbounded_String,
                     Lines         => Line_Vectors.Empty_Vector,
                     Line          => 0,
                     Column        => 0,
                     Tok_Line      => 0,
                     Tok_Column    => 0,
                     Ch            => ' ',
                     Tok           => Tok_None,
                     Tok_Text      => Null_Unbounded_String,
                     Tok_Char      => ' ',
                     EOF           => True,
                     Is_Port       => False,
                     Port          => null);

   Max_Source_Files : constant := 10;

   Source_Stream_Stack  : array (1 .. Max_Source_Files)
     of aliased Source_Stream_Record;

   Current_Stream_Index : Natural := 0;
   Current_Stream       : access Source_Stream_Record := null;

   procedure Open_Stream (Identity : String);
   procedure Close_Stream;

   procedure Next_Character;
   procedure Start_Token;
   procedure Stop_Token;

   function Peek return Character;

   function Is_White_Space return Boolean;
   function Is_Alphanumeric return Boolean;
   function End_Of_Stream return Boolean;
   function End_Of_Line return Boolean;
   function Hex_Digit (Ch : Character) return Natural;

   function Hex_To_Character
     (Hex : String)
      return Character;

   function Read_Character (Long_Names : Boolean) return Character;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Close_Stream;
   end Close;

   ------------------
   -- Close_Stream --
   ------------------

   procedure Close_Stream is
   begin
      if Current_Stream.Is_Port then
         Current_Stream.Port.Put_Back
           (To_String (Current_Stream.Tok_Text & Current_Stream.Ch));
      end if;
      Current_Stream.all := Empty_Stream;
      Current_Stream_Index := Current_Stream_Index - 1;
      if Current_Stream_Index > 0 then
         Current_Stream := Source_Stream_Stack (Current_Stream_Index)'Access;
      else
         Current_Stream := null;
      end if;
   end Close_Stream;

   -----------------------
   -- Current_File_Name --
   -----------------------

   function Current_File_Name return String is
   begin
      return To_String (Current_Stream.Identity);
   end Current_File_Name;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line return Natural is
   begin
      return Current_Stream.Tok_Line;
   end Current_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line return Boolean is
   begin
      return Current_Stream.Column =
        Current_Stream.Lines.Element (Current_Stream.Line)'Length;
   end End_Of_Line;

   -------------------
   -- End_Of_Stream --
   -------------------

   function End_Of_Stream return Boolean is
   begin
      return Current_Stream.EOF;
   end End_Of_Stream;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      Put_Line
        (Standard_Error,
         To_String (Current_Stream.Identity)
         & ":"
         & Trim (Natural'Image (Current_Stream.Tok_Line), Left)
         & ":"
         & Trim (Natural'Image (Current_Stream.Tok_Column), Left)
         & ": "
         & Message);
   end Error;

   ---------------
   -- Hex_Digit --
   ---------------

   function Hex_Digit (Ch : Character) return Natural is
   begin
      if Ch in '0' .. '9' then
         return Character'Pos (Ch)
           - Character'Pos ('0');
      elsif Ch in 'A' .. 'F' then
         return Character'Pos (Ch)
           - Character'Pos ('A')
           + 10;
      elsif Ch in 'a' .. 'f' then
         return Character'Pos (Ch)
           - Character'Pos ('a')
           + 10;
      else
         raise Constraint_Error with
           "expected a hex digit";
      end if;
   end Hex_Digit;

   ----------------------
   -- Hex_To_Character --
   ----------------------

   function Hex_To_Character
     (Hex : String)
      return Character
   is
      V : Natural := 0;
   begin
      for D of Hex loop
         V := V * 16 + Hex_Digit (D);
      end loop;
      return Character'Val (V);
   end Hex_To_Character;

   ---------------------
   -- Is_Alphanumeric --
   ---------------------

   function Is_Alphanumeric return Boolean is
   begin
      return Ada.Characters.Handling.Is_Alphanumeric
        (Current_Stream.Ch);
   end Is_Alphanumeric;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space return Boolean is
   begin
      return Ada.Characters.Handling.Is_Space
        (Current_Stream.Ch);
   end Is_White_Space;

   --------------------
   -- Next_Character --
   --------------------

   procedure Next_Character is
   begin
      Current_Stream.Tok_Text :=
        Current_Stream.Tok_Text & Current_Stream.Ch;
      if Current_Stream.Line = 0 then
         Current_Stream.Line := 1;
      end if;

      if Current_Stream.Is_Port then

         if Current_Stream.Port.End_Of_File then
            Current_Stream.EOF := True;
            Current_Stream.Ch := ' ';
         else
            Current_Stream.Ch := Current_Stream.Port.Read_Char;
            Current_Stream.Ch := Current_Stream.Port.Peek_Char;
            if Current_Stream.Ch = Character'Val (10) then
               Current_Stream.Ch := ' ';
            else
               Current_Stream.Line := Current_Stream.Port.Line;
               Current_Stream.Column := Current_Stream.Port.Col;
            end if;
         end if;
      else
         if Current_Stream.Line > Current_Stream.Lines.Last_Index then
            Current_Stream.EOF := True;
            Current_Stream.Ch := ' ';
         elsif Current_Stream.Column
           >= Current_Stream.Lines.Element (Current_Stream.Line)'Length
         then
            Current_Stream.Line := Current_Stream.Line + 1;
            Current_Stream.Column := 0;
            Current_Stream.Ch := ' ';
         else
            Current_Stream.Column := Current_Stream.Column + 1;
            Current_Stream.Ch :=
              Current_Stream.Lines.Element (Current_Stream.Line)
              (Current_Stream.Column);
         end if;
      end if;
   end Next_Character;

   ----------
   -- Open --
   ----------

   procedure Open (File_Name : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open_Stream (File_Name);
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            if Line (Line'Last) = Character'Val (13) then
               Current_Stream.Lines.Append (Line (1 .. Line'Last - 1));
            else
               Current_Stream.Lines.Append (Line);
            end if;
         end;
      end loop;
      Close (File);
      Scan;
   end Open;

   ---------------
   -- Open_Port --
   ---------------

   procedure Open_Port
     (Port : not null access Lith.IO.Text_IO.Text_Port_Type'Class)
   is
   begin
      Open_Stream ("text-input-port");
      Current_Stream.Is_Port := True;
      Current_Stream.Port := Port;
      Current_Stream.Ch := Current_Stream.Port.Peek_Char;
      Scan;
   end Open_Port;

   -----------------
   -- Open_Stream --
   -----------------

   procedure Open_Stream (Identity : String) is
   begin
      if Current_Stream_Index = Max_Source_Files then
         raise Program_Error with "maximum source file nesting exceeded";
      end if;
      Current_Stream_Index := Current_Stream_Index + 1;
      Current_Stream := Source_Stream_Stack (Current_Stream_Index)'Access;
      Current_Stream.all := Empty_Stream;
      Current_Stream.Identity := To_Unbounded_String (Identity);
      Current_Stream.EOF := False;
   end Open_Stream;

   -----------------
   -- Open_String --
   -----------------

   procedure Open_String (Expr_String : String) is
   begin
      Open_Stream ("user input");
      Current_Stream.Lines.Append (Expr_String);
      Scan;
   end Open_String;

   ----------
   -- Peek --
   ----------

   function Peek return Character is
   begin
      if Current_Stream.Is_Port then
         Current_Stream.Ch := Current_Stream.Port.Read_Char;
         return Current_Stream.Port.Peek_Char;
      elsif Current_Stream.Line > Current_Stream.Lines.Last_Index then
         return ' ';
      elsif Current_Stream.Column
        >= Current_Stream.Lines.Element (Current_Stream.Line)'Length
      then
         return ' ';
      else
         return
           Current_Stream.Lines.Element (Current_Stream.Line)
           (Current_Stream.Column + 1);
      end if;
   end Peek;

   --------------------
   -- Read_Character --
   --------------------

   function Read_Character (Long_Names : Boolean) return Character is
      Buffer : String (1 .. 20);
      Count  : Natural := 0;
   begin
      if Current_Stream.Ch = 'x' then
         Next_Character;
         while Current_Stream.Ch in '0' .. '9'
           or else Current_Stream.Ch in 'a' .. 'z'
           or else Current_Stream.Ch in 'A' .. 'Z'
         loop
            Count := Count + 1;
            Buffer (Count) := Current_Stream.Ch;
            Next_Character;
         end loop;

         if Count = 0 then
            return 'x';
         end if;

         if not Long_Names then
            if Current_Stream.Ch = ';' then
               Next_Character;
            else
               Error ("missing ';'");
            end if;
         end if;

         return Hex_To_Character (Buffer (1 .. Count));

      elsif Long_Names then
         if Is_White_Space then
            Next_Character;
            return ' ';
         elsif Is_Alphanumeric then
            while Count + 1 in Buffer'Range
              and then Is_Alphanumeric
            loop
               Count := Count + 1;
               Buffer (Count) := Current_Stream.Ch;
               Next_Character;
            end loop;

            return Lith.Parser.Lexical.Characters.Name_To_Character
              (Buffer (1 .. Count));
         else
            declare
               Result : constant Character :=
                          Current_Stream.Ch;
            begin
               Next_Character;
               return Result;
            end;
         end if;
      else
         declare
            V : Integer := -1;
         begin

            case Current_Stream.Ch is
               when 't' =>
                  V := 9;
               when 'n' =>
                  V := 10;
               when 'r' =>
                  V := 13;
               when '"' =>
                  V := 34;
               when '\' =>
                  V := 16#5C#;
               when '|' =>
                  V := 16#7C#;
               when others =>
                  Error ("bad escape");
                  raise Program_Error;
            end case;
            Next_Character;
            return Character'Val (V);
         end;
      end if;
   end Read_Character;

   ----------
   -- Scan --
   ----------

   procedure Scan is
      Tok : Token renames Current_Stream.Tok;
   begin
      while not End_Of_Stream and then Is_White_Space loop
         Next_Character;
      end loop;

      if End_Of_Stream then
         Tok := Tok_End_Of_File;
         return;
      end if;

      Start_Token;

      case Current_Stream.Ch is
         when '(' =>
            Next_Character;
            if Current_Stream.Ch = ')' then
               Next_Character;
               Stop_Token;
               Tok := Tok_Nil;
            else
               Stop_Token;
               Tok := Tok_Left_Paren;
            end if;
         when ')' =>
            Next_Character;
            Stop_Token;
            Tok := Tok_Right_Paren;
         when ''' =>
            Next_Character;
            Stop_Token;
            Tok := Tok_Quote;
         when '`' =>
            Next_Character;
            Stop_Token;
            Tok := Tok_Quasiquote;
         when ',' =>
            Next_Character;
            if Current_Stream.Ch = '@' then
               Next_Character;
               Stop_Token;
               Tok := Tok_Unquote_Splice;
            else
               Stop_Token;
               Tok := Tok_Comma;
            end if;
         when ';' =>
            Stop_Token;
            Current_Stream.Line := Current_Stream.Line + 1;
            Current_Stream.Column := 0;
            Current_Stream.Ch := ' ';
            Scan;
         when '"' =>
            Next_Character;
            Start_Token;

            declare
               Result : Unbounded_String :=
                          Null_Unbounded_String;
            begin
               while not End_Of_Stream
                 and then Current_Stream.Ch /= '"'
               loop
                  if Current_Stream.Ch = '\' then
                     Next_Character;
                     Result := Result
                       & Read_Character (Long_Names => False);
                  elsif End_Of_Line then
                     Error ("unterminated string constant");
                     exit;
                  else
                     Result := Result & Current_Stream.Ch;
                     Next_Character;
                  end if;
               end loop;

               Stop_Token;

               if End_Of_Stream then
                  Error ("unterminated string constant");
               else
                  Next_Character;
               end if;

               Current_Stream.Tok_Text := Result;
               Tok := Tok_String;
            end;

         when others =>
            declare
               use Ada.Strings.Fixed;
               use Ada.Characters.Handling;

               function Is_Id return Boolean
               is (not End_Of_Stream
                   and then (Is_Alphanumeric (Current_Stream.Ch)
                             or else Index ("!$%&*+-./:<=>?@^_~#",
                                            (1 => Current_Stream.Ch)) > 0));
            begin
               if Current_Stream.Ch = '#'
                 and then Peek = '\'
               then
                  Next_Character;
                  Next_Character;
                  Current_Stream.Tok_Char :=
                    Read_Character (Long_Names => True);
                  Stop_Token;
                  Tok := Tok_Character;
               elsif Current_Stream.Ch = '#'
                 and then Peek = '('
               then
                  Next_Character;
                  Next_Character;
                  Stop_Token;
                  Tok := Tok_Start_Vector;
               elsif Current_Stream.Ch = '.'
                 and then Ada.Characters.Handling.Is_Space (Peek)
               then
                  Next_Character;
                  Stop_Token;
                  Tok := Tok_Dot;
               elsif Is_Id then
                  while Is_Id loop
                     Next_Character;
                  end loop;

                  Stop_Token;

                  if Tok_Text = "#u8" and then Current_Stream.Ch = '(' then
                     Next_Character;
                     Tok := Tok_Start_Bytevector;
                  else
                     Tok :=
                       Lith.Parser.Lexical.Identifiers.Classify_Identifier
                         (Tok_Text);
                  end if;

               else
                  Next_Character;
                  Stop_Token;
                  Tok := Tok_Bad_Character;
               end if;
            end;
      end case;

--        Ada.Text_IO.Put_Line
--          (Token'Image (Tok) & " ["
--           & Tok_Text & "]");

   end Scan;

   -----------------
   -- Start_Token --
   -----------------

   procedure Start_Token is
   begin
      Current_Stream.Tok_Line := Current_Stream.Line;
      Current_Stream.Tok_Column := Current_Stream.Column;
      Current_Stream.Tok_Text := Null_Unbounded_String;
   end Start_Token;

   ----------------
   -- Stop_Token --
   ----------------

   procedure Stop_Token is
   begin
      null;
   end Stop_Token;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return Current_Stream.Tok;
   end Tok;

   -------------------------
   -- Tok_Character_Value --
   -------------------------

   function Tok_Character_Value return Character is
   begin
      return Current_Stream.Tok_Char;
   end Tok_Character_Value;

   -----------------------
   -- Tok_Integer_Value --
   -----------------------

   function Tok_Integer_Value return Natural is
   begin
      return Natural'Value
        (To_String
           (Current_Stream.Tok_Text));
   end Tok_Integer_Value;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return To_String (Current_Stream.Tok_Text);
   end Tok_Text;

end Lith.Parser.Lexical;

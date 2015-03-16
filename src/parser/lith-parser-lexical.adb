with Ada.Characters.Conversions;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Wide_Unbounded;  use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;

with Lith.Parser.Lexical.Characters;

package body Lith.Parser.Lexical is

   package Line_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Wide_Wide_String);

   type Source_Stream_Record is
      record
         Identity   : Unbounded_Wide_Wide_String;
         Lines      : Line_Vectors.Vector;
         Line       : Natural;
         Column     : Natural;
         Tok_Line   : Natural;
         Tok_Column : Natural;
         Ch         : Wide_Wide_Character;
         Tok        : Token;
         Tok_Text   : Unbounded_Wide_Wide_String;
         Tok_Char   : Wide_Wide_Character;
         EOF        : Boolean;
      end record;

   Empty_Stream : constant Source_Stream_Record :=
                    (Identity      => Null_Unbounded_Wide_Wide_String,
                     Lines         => Line_Vectors.Empty_Vector,
                     Line          => 0,
                     Column        => 0,
                     Tok_Line      => 0,
                     Tok_Column    => 0,
                     Ch            => ' ',
                     Tok           => Tok_None,
                     Tok_Text      => Null_Unbounded_Wide_Wide_String,
                     Tok_Char      => ' ',
                     EOF           => True);

   Max_Source_Files : constant := 10;

   Source_Stream_Stack  : array (1 .. Max_Source_Files)
     of aliased Source_Stream_Record;

   Current_Stream_Index : Natural := 0;
   Current_Stream       : access Source_Stream_Record := null;

   procedure Open_Stream (Identity : Wide_Wide_String);
   procedure Close_Stream;

   procedure Next_Character;
   procedure Start_Token;
   procedure Stop_Token;

   function Peek return Wide_Wide_Character;

   function Is_White_Space return Boolean;
   function Is_Alphanumeric return Boolean;
   function End_Of_Stream return Boolean;
   function End_Of_Line return Boolean;
   function Hex_Digit (Ch : Wide_Wide_Character) return Natural;

   function Hex_To_Character
     (Hex : Wide_Wide_String)
      return Wide_Wide_Character;

   function Read_Character (Long_Names : Boolean) return Wide_Wide_Character;

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
      Current_Stream.all := Empty_Stream;
      Current_Stream_Index := Current_Stream_Index - 1;
      if Current_Stream_Index > 0 then
         Current_Stream := Source_Stream_Stack (Current_Stream_Index)'Access;
      else
         Current_Stream := null;
      end if;
   end Close_Stream;

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

   procedure Error (Message : Wide_Wide_String) is
      use Ada.Strings, Ada.Strings.Wide_Wide_Fixed;
      use Ada.Wide_Wide_Text_IO;
   begin
      Put_Line
        (Standard_Error,
         To_Wide_Wide_String (Current_Stream.Identity)
         & ":"
         & Trim (Natural'Wide_Wide_Image (Current_Stream.Tok_Line), Left)
         & ":"
         & Trim (Natural'Wide_Wide_Image (Current_Stream.Tok_Column), Left)
         & ": "
         & Message);
   end Error;

   ---------------
   -- Hex_Digit --
   ---------------

   function Hex_Digit (Ch : Wide_Wide_Character) return Natural is
   begin
      if Ch in '0' .. '9' then
         return Wide_Wide_Character'Pos (Ch)
           - Wide_Wide_Character'Pos ('0');
      elsif Ch in 'A' .. 'F' then
         return Wide_Wide_Character'Pos (Ch)
           - Wide_Wide_Character'Pos ('A')
           + 10;
      elsif Ch in 'a' .. 'f' then
         return Wide_Wide_Character'Pos (Ch)
           - Wide_Wide_Character'Pos ('a')
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
     (Hex : Wide_Wide_String)
      return Wide_Wide_Character
   is
      V : Natural := 0;
   begin
      for D of Hex loop
         V := V * 16 + Hex_Digit (D);
      end loop;
      return Wide_Wide_Character'Val (V);
   end Hex_To_Character;

   ---------------------
   -- Is_Alphanumeric --
   ---------------------

   function Is_Alphanumeric return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Alphanumeric
        (Current_Stream.Ch);
   end Is_Alphanumeric;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Space
        (Current_Stream.Ch);
   end Is_White_Space;

   --------------------
   -- Next_Character --
   --------------------

   procedure Next_Character is
   begin
      if Current_Stream.Line = 0 then
         Current_Stream.Line := 1;
      end if;
      if Current_Stream.Line > Current_Stream.Lines.Last_Index then
         Current_Stream.EOF := True;
         Current_Stream.Ch := ' ';
      elsif Current_Stream.Column
        >= Current_Stream.Lines.Element (Current_Stream.Line)'Length
      then
         Current_Stream.Line := Current_Stream.Line + 1;
         Current_Stream.Column := 0;
         Next_Character;
      else
         Current_Stream.Column := Current_Stream.Column + 1;
         Current_Stream.Ch :=
           Current_Stream.Lines.Element (Current_Stream.Line)
           (Current_Stream.Column);
      end if;
   end Next_Character;

   ----------
   -- Open --
   ----------

   procedure Open (File_Name : String) is
      use Ada.Wide_Wide_Text_IO;
      File : File_Type;
   begin
      Open_Stream
        (Ada.Characters.Conversions.To_Wide_Wide_String (File_Name));
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : constant Wide_Wide_String := Get_Line (File);
         begin
            Current_Stream.Lines.Append (Line);
         end;
      end loop;
      Close (File);
      Scan;
   end Open;

   -----------------
   -- Open_Stream --
   -----------------

   procedure Open_Stream (Identity : Wide_Wide_String) is
   begin
      if Current_Stream_Index = Max_Source_Files then
         raise Program_Error with "maximum source file nesting exceeded";
      end if;
      Current_Stream_Index := Current_Stream_Index + 1;
      Current_Stream := Source_Stream_Stack (Current_Stream_Index)'Access;
      Current_Stream.all := Empty_Stream;
      Current_Stream.Identity := To_Unbounded_Wide_Wide_String (Identity);
      Current_Stream.EOF := False;
   end Open_Stream;

   -----------------
   -- Open_String --
   -----------------

   procedure Open_String (Expr_String : Wide_Wide_String) is
   begin
      Open_Stream ("user input");
      Current_Stream.Lines.Append (Expr_String);
      Scan;
   end Open_String;

   ----------
   -- Peek --
   ----------

   function Peek return Wide_Wide_Character is
   begin
      if Current_Stream.Line > Current_Stream.Lines.Last_Index then
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

   function Read_Character (Long_Names : Boolean) return Wide_Wide_Character is
      Buffer : Wide_Wide_String (1 .. 20);
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
         else
            while Count + 1 in Buffer'Range
              and then Is_Alphanumeric
            loop
               Count := Count + 1;
               Buffer (Count) := Current_Stream.Ch;
               Next_Character;
            end loop;

            return Lith.Parser.Lexical.Characters.Name_To_Character
              (Buffer (1 .. Count));

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
            return Wide_Wide_Character'Val (V);
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
               Result : Unbounded_Wide_Wide_String :=
                          Null_Unbounded_Wide_Wide_String;
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

               Current_Stream.Tok_Text := Result;

            end;
            if End_Of_Stream then
               Error ("unterminated string constant");
            else
               Next_Character;
            end if;

            Tok := Tok_String;

         when others =>
            declare
               use Ada.Strings.Wide_Wide_Fixed;
               use Ada.Wide_Wide_Characters.Handling;

               Non_Numeric : Boolean := False;

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
               elsif Current_Stream.Ch = '.'
                 and then Ada.Wide_Wide_Characters.Handling.Is_Space (Peek)
               then
                  Next_Character;
                  Stop_Token;
                  Tok := Tok_Dot;
               elsif Is_Id then
                  while Is_Id loop
                     if not Is_Digit (Current_Stream.Ch) then
                        Non_Numeric := True;
                     end if;
                     Next_Character;
                  end loop;
                  Stop_Token;

                  if Non_Numeric then
                     Tok := Tok_Identifier;
                  else
                     Tok := Tok_Integer;
                  end if;

               else
                  Next_Character;
                  Stop_Token;
                  Tok := Tok_Bad_Character;
               end if;
            end;
      end case;

--        Ada.Wide_Wide_Text_IO.Put_Line
--          (Token'Wide_Wide_Image (Tok) & " ["
--           & Tok_Text & "]");

   end Scan;

   -----------------
   -- Start_Token --
   -----------------

   procedure Start_Token is
   begin
      Current_Stream.Tok_Line := Current_Stream.Line;
      Current_Stream.Tok_Column := Current_Stream.Column;
   end Start_Token;

   ----------------
   -- Stop_Token --
   ----------------

   procedure Stop_Token is
      Start : constant Positive := Current_Stream.Tok_Column;
      Col   : constant Natural  := Current_Stream.Column;
      Line  : constant Wide_Wide_String :=
                Current_Stream.Lines (Current_Stream.Tok_Line);
   begin
      if Current_Stream.Tok_Line = Current_Stream.Line then
         Current_Stream.Tok_Text :=
           To_Unbounded_Wide_Wide_String (Line (Start .. Col - 1));
      else
         Current_Stream.Tok_Text :=
           To_Unbounded_Wide_Wide_String (Line (Start .. Line'Last));
      end if;
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

   function Tok_Character_Value return Wide_Wide_Character is
   begin
      return Current_Stream.Tok_Char;
   end Tok_Character_Value;

   -----------------------
   -- Tok_Integer_Value --
   -----------------------

   function Tok_Integer_Value return Natural is
   begin
      return Natural'Wide_Wide_Value
        (To_Wide_Wide_String
           (Current_Stream.Tok_Text));
   end Tok_Integer_Value;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return Wide_Wide_String is
   begin
      return To_Wide_Wide_String (Current_Stream.Tok_Text);
   end Tok_Text;

end Lith.Parser.Lexical;

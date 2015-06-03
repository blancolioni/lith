with Lith.Parser.Tokens;             use Lith.Parser.Tokens;
with Lith.IO.Text_IO;

private package Lith.Parser.Lexical is

   procedure Open (File_Name : String);
   procedure Open_String (Expr_String : Wide_Wide_String);
   procedure Open_Port
     (Port : not null access Lith.IO.Text_IO.Text_Port_Type'Class);

   procedure Close;

   procedure Scan;

   function Tok return Token;
   function Tok_Text return Wide_Wide_String;
   function Tok_Character_Value return Wide_Wide_Character;
   function Tok_Integer_Value return Natural;

   procedure Error (Message : Wide_Wide_String);

   function Current_File_Name return Wide_Wide_String;
   function Current_Line return Natural;

end Lith.Parser.Lexical;

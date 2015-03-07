with GCS.Styles;                       use GCS.Styles;
with GCS.Lexer;
pragma Elaborate_All (GCS.Lexer);

with Lith.Parser.Tokens;             use Lith.Parser.Tokens;

private package Lith.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String,
                 Tok_Character      => Tok_None,
                 Tok_Integer        => Tok_Integer,
                 Tok_Float          => Tok_Float,
                 First_Keyword      => Tok_Dot,
                 Keywords           => ". @",
                 First_Symbol       => Tok_Left_Paren,
                 Symbols            => "( ) ' ` , ()",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "!$%&*+-./:<=>?@^_~#",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "!$%&*+-./:<=>?@^_~#",
                 Line_Comment_Start => ";",
                 Escape_Character   => '\',
                 Properties         => (Case_Sensitive_Identifiers => True,
                                        others => False));

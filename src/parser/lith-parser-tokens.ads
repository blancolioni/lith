private package Lith.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_String, Tok_Character, Tok_Integer, Tok_Float,

       Tok_Dot, Tok_At,

       Tok_Left_Paren, Tok_Right_Paren, Tok_Quote, Tok_Quasiquote, Tok_Comma,
       Tok_Nil);

end Lith.Parser.Tokens;

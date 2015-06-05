with Lith.Parser.Tokens;

package Lith.Parser.Lexical.Identifiers is

   function Classify_Identifier
     (Text : String)
      return Lith.Parser.Tokens.Token;

   procedure Push_Integer
     (Store : in out Lith.Objects.Object_Store'Class;
      Text  : in     String);

end Lith.Parser.Lexical.Identifiers;

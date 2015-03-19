with Lith.Parser.Tokens;

package Lith.Parser.Lexical.Identifiers is

   function Classify_Identifier
     (Text : Wide_Wide_String)
      return Lith.Parser.Tokens.Token;

   procedure Push_Integer
     (Store : in out Lith.Objects.Object_Store'Class;
      Text  : in     Wide_Wide_String);

end Lith.Parser.Lexical.Identifiers;

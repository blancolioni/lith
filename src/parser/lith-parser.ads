with Lith.Objects;

package Lith.Parser is

   function Parse_Expression
     (Machine : in out Lith.Objects.Object_Store'Class;
      Expr    : Wide_Wide_String)
      return Lith.Objects.Object;

   procedure Parse_File
     (Machine : in out Lith.Objects.Object_Store'Class;
      Path    : String);

end Lith.Parser;

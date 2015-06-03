with Lith.Objects;

package Lith.Parser is

   function Parse_Expression
     (Store : in out Lith.Objects.Object_Store'Class;
      Expr    : Wide_Wide_String)
      return Lith.Objects.Object;

   function Read_Port
     (Store : in out Lith.Objects.Object_Store'Class;
      Port    : Lith.Objects.Object)
      return Lith.Objects.Object;

   procedure Parse_File
     (Store : in out Lith.Objects.Object_Store'Class;
      Path    : String);

end Lith.Parser;

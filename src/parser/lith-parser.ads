with Lith.Machine;
with Lith.Objects;

package Lith.Parser is

   function Parse_Expression
     (Machine : Lith.Machine.Lith_Machine;
      Expr    : Wide_Wide_String)
      return Lith.Objects.Object;

   procedure Parse_File
     (Machine : Lith.Machine.Lith_Machine;
      Path    : String);

end Lith.Parser;

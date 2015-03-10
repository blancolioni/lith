with Ada.Characters.Conversions;
with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with Lith.Environment;
with Lith.Objects;
with Lith.Parser;
with Lith.Symbols;

package body Lith.Repl is

   -------------
   -- Execute --
   -------------

   procedure Execute (Machine : Lith.Machine.Lith_Machine) is
      use Ada.Wide_Wide_Text_IO;
      use Lith.Objects;
   begin
      while Lith.Environment.Get
        (Lith.Symbols.Get_Symbol ("__exit")) = Nil
      loop
         Put ("Lith> ");
         Flush;
         declare
            Expr_Text : constant Wide_Wide_String := Get_Line;
         begin
            if Expr_Text'Length > 0 then
               declare
                  Expr : constant Lith.Objects.Object :=
                           Lith.Parser.Parse_Expression
                             (Machine, Expr_Text);
               begin
                  Put_Line
                    (Machine.Show (Machine.Evaluate (Expr, Lith.Objects.Nil)));
               end;
            end if;
         exception
            when E : others =>
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  Ada.Characters.Conversions.To_Wide_Wide_String
                  (Ada.Exceptions.Exception_Message (E)));
         end;
      end loop;

   exception
      when Ada.Wide_Wide_Text_IO.End_Error =>
         Ada.Wide_Wide_Text_IO.Put_Line ("Exiting");
   end Execute;

end Lith.Repl;

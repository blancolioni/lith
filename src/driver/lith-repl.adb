with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Lith.Environment;
with Lith.Parser;
with Lith.Objects.Symbols;

with Lith.Paths;

package body Lith.Repl is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Store : not null access Lith.Objects.Object_Store'Class)
   is
      use Ada.Text_IO;
      use Lith.Objects;
   begin

      Lith.Parser.Parse_File
        (Store.all,
         Lith.Paths.Config_Path & "/interaction-environment.scm");

      if Ada.Directories.Exists ("auto.l") then
         Lith.Parser.Parse_File
           (Store.all, String'("auto.l"));
      end if;

      while Lith.Environment.Get
        (Lith.Objects.Symbols.Get_Symbol ("__exit")) = False_Value
      loop
         Put ("Lith> ");
         Flush;
         declare
            Expr_Text : constant String := Get_Line;
         begin
            if Expr_Text'Length > 0 then
               declare
                  Expr : constant Lith.Objects.Object :=
                           Lith.Parser.Parse_Expression
                             (Store.all, Expr_Text);
                  Result : constant Object :=
                             Store.Evaluate (Expr);
               begin
                  if Result /= No_Value then
                     Put_Line (Store.Show (Result));
                  end if;
               end;
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Put_Line ("Exiting");
   end Execute;

end Lith.Repl;

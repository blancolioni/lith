with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;

with Lith.Parser.Tokens;             use Lith.Parser.Tokens;
with Lith.Parser.Lexical;            use Lith.Parser.Lexical;

with Lith.Symbols;
with Lith.Objects.Numbers;

package body Lith.Parser is

   procedure Parse_S_Expression
     (Machine    : Lith.Machine.Lith_Machine;
      Quasiquote : Boolean);
   --  Parse a single s-expression, leaving the result on the top of the stack

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Machine : Lith.Machine.Lith_Machine;
      Expr    : Wide_Wide_String)
      return Lith.Objects.Object
   is
   begin
      Open_String (Expr);
      Parse_S_Expression (Machine, Quasiquote => False);
      Close;
      return Machine.Pop;
   end Parse_Expression;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Machine : Lith.Machine.Lith_Machine;
      Path    : Wide_Wide_String)
   is
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         Parse_S_Expression (Machine, Quasiquote => False);

         declare
            Top : constant Lith.Objects.Object := Machine.Pop;
            Result : constant Lith.Objects.Object :=
                       Machine.Evaluate (Top, Lith.Objects.Nil);
         begin
            if False then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Machine.Show (Result));
            end if;
         end;
      end loop;
      Close;
   end Parse_File;

   ------------------------
   -- Parse_S_Expression --
   ------------------------

   procedure Parse_S_Expression
     (Machine    : Lith.Machine.Lith_Machine;
      Quasiquote : Boolean)
   is

      procedure Parse_Rest_Of_List;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      procedure Parse_Rest_Of_List is
      begin
         Parse_S_Expression (Machine, Quasiquote);

         if Tok = Tok_Right_Paren then
            Machine.Push (Lith.Objects.Nil);
            Scan;
         elsif Tok = Tok_End_Of_File then
            Error ("Missing ')'");
            Machine.Push (Lith.Objects.Nil);
         else
            Parse_Rest_Of_List;
         end if;

         Machine.Cons;

      end Parse_Rest_Of_List;

   begin
      case Tok is
         when Tok_Nil =>
            Scan;
            Machine.Push (Lith.Objects.Nil);
         when Tok_Left_Paren =>
            Scan;
            if Quasiquote then
               Machine.Push ("list");
            end if;
            Parse_Rest_Of_List;
            if Quasiquote then
               Machine.Cons;
            end if;
         when Tok_Identifier =>
            if Quasiquote then
               Machine.Push ("quote");
               Machine.Push (Tok_Text);
               Machine.Push (Lith.Objects.Nil);
               Machine.Cons;
               Machine.Cons;
            else
               Machine.Push (Tok_Text);
            end if;
            Scan;
         when Tok_Character =>
            Machine.Push
              (Lith.Objects.To_Object
                 (Tok_Character_Value));
            Scan;
         when Tok_String =>
            Machine.Push (Lith.Symbols.String_Atom);
            for Ch of Tok_Text loop
               Machine.Push (Wide_Wide_Character'Pos (Ch));
            end loop;
            Machine.Push (Lith.Objects.Nil);
            for I in 1 .. Tok_Text'Length + 1 loop
               Machine.Cons;
            end loop;
            Scan;
         when Tok_Quote =>
            Scan;
            if Quasiquote then
               Machine.Push ("quote");
               Machine.Push ("quote");
               Machine.Push (Lith.Objects.Nil);
               Machine.Cons;
               Machine.Cons;
            else
               Machine.Push ("quote");
            end if;
            Parse_S_Expression (Machine, Quasiquote);
            Machine.Push (Lith.Objects.Nil);
            Machine.Cons;
            Machine.Cons;
         when Tok_Quasiquote =>
            Scan;
            Parse_S_Expression (Machine, True);
         when Tok_Comma =>
            Scan;
            Parse_S_Expression (Machine, False);
         when Tok_Integer =>
            declare
               Text : constant String :=
                        Ada.Characters.Conversions.To_String (Tok_Text);
            begin
               Machine.Push (0);
               for I in Text'Range loop
                  declare
                     X : constant Integer :=
                           Character'Pos (Text (I)) - Character'Pos ('0');
                  begin
                     Machine.Push (10);
                     Lith.Objects.Numbers.Multiply (Machine.all);
                     Machine.Push (X);
                     Lith.Objects.Numbers.Add (Machine.all);
                  end;
               end loop;
            end;
            Scan;
         when others =>
            Error ("bad token");
            Machine.Push (Lith.Objects.Nil);
            Scan;
      end case;

   end Parse_S_Expression;

end Lith.Parser;

with Ada.Wide_Wide_Text_IO;

with Lith.Parser.Tokens;             use Lith.Parser.Tokens;
with Lith.Parser.Lexical;            use Lith.Parser.Lexical;

with Lith.Symbols;

package body Lith.Parser is

   procedure Parse_S_Expression
     (Machine   : Lith.Machine.Lith_Machine);
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
      Parse_S_Expression (Machine);
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
         Parse_S_Expression (Machine);

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
     (Machine   : Lith.Machine.Lith_Machine)
   is

      procedure Parse_Rest_Of_List;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      procedure Parse_Rest_Of_List is
      begin
         Parse_S_Expression (Machine);

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
            Parse_Rest_Of_List;
         when Tok_Identifier =>
            Machine.Push
              (Lith.Objects.To_Object (Lith.Symbols.Get_Symbol (Tok_Text)));
            Scan;
         when Tok_Character =>
            Machine.Push
              (Lith.Objects.To_Object
                 (Integer'(Wide_Wide_Character'Pos (Tok_Character_Value))));
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
            Machine.Push ("quote");
            Parse_S_Expression (Machine);
            Machine.Push (Lith.Objects.Nil);
            Machine.Cons;
            Machine.Cons;
         when Tok_Quasiquote =>
            Scan;
            Machine.Push ("quasiquote");
            Parse_S_Expression (Machine);
            Machine.Push (Lith.Objects.Nil);
            Machine.Cons;
            Machine.Cons;
         when Tok_Comma =>
            Scan;
            if Tok = Tok_At then
               Scan;
               Machine.Push (Lith.Symbols.Unquote_Splicing);
            else
               Machine.Push (Lith.Symbols.Unquote);
            end if;
            Parse_S_Expression (Machine);
            Machine.Push (Lith.Objects.Nil);
            Machine.Cons;
            Machine.Cons;
         when Tok_Integer =>
            Machine.Push
              (Lith.Objects.To_Object (Tok_Integer_Value));
            Scan;
         when others =>
            Error ("bad token");
            Machine.Push (Lith.Objects.Nil);
            Scan;
      end case;

   end Parse_S_Expression;

end Lith.Parser;

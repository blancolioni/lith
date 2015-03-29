with Ada.Wide_Wide_Text_IO;

with Lith.Parser.Tokens;             use Lith.Parser.Tokens;
with Lith.Parser.Lexical;            use Lith.Parser.Lexical;
with Lith.Parser.Lexical.Identifiers;

with Lith.Objects.Numbers;
with Lith.Symbols;

package body Lith.Parser is

   procedure Parse_S_Expression
     (Machine : in out Lith.Objects.Object_Store'Class;
      Quasiquote : Boolean);
   --  Parse a single s-expression, leaving the result on the top of the stack

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Machine : in out Lith.Objects.Object_Store'Class;
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
     (Machine : in out Lith.Objects.Object_Store'Class;
      Path    : String)
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
     (Machine : in out Lith.Objects.Object_Store'Class;
      Quasiquote : Boolean)
   is

      use Lith.Symbols;

      procedure Parse_Rest_Of_List;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      procedure Parse_Rest_Of_List is
      begin
         Parse_S_Expression (Machine, Quasiquote);

         if Tok = Tok_Dot then
            Scan;
            Parse_S_Expression (Machine, Quasiquote);
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         elsif Tok = Tok_Right_Paren then
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
               Machine.Push (Get_Symbol ("list"));
            end if;
            Parse_Rest_Of_List;
            if Quasiquote then
               Machine.Cons;
            end if;
         when Tok_Identifier =>
            if Tok_Text = "#t" then
               Machine.Push (Lith.Objects.True_Value);
            elsif Tok_Text = "#f" then
               Machine.Push (Lith.Objects.False_Value);
            elsif Quasiquote then
               Machine.Push (Get_Symbol ("quote"));
               Machine.Push (Get_Symbol (Tok_Text));
               Machine.Push (Lith.Objects.Nil);
               Machine.Cons;
               Machine.Cons;
            else
               Machine.Push (Get_Symbol (Tok_Text));
            end if;
            Scan;
         when Tok_Start_Vector =>
            Scan;
            Machine.Push (Lith.Symbols.Get_Symbol ("vector"));
            Parse_Rest_Of_List;
            Machine.Cons;
         when Tok_Start_Bytevector =>
            Scan;
            Machine.Push (Lith.Symbols.Get_Symbol ("bytevector"));
            Parse_Rest_Of_List;
            Machine.Cons;

         when Tok_Character =>
            Machine.Push
              (Lith.Objects.To_Object
                 (Tok_Character_Value));
            Scan;
         when Tok_String =>
            Machine.Push (Lith.Symbols.String_Atom);
            for Ch of Tok_Text loop
               Machine.Push (Lith.Objects.To_Object (Ch));
            end loop;
            Machine.Push (Lith.Objects.Nil);
            for I in 1 .. Tok_Text'Length + 1 loop
               Machine.Cons;
            end loop;
            Scan;
         when Tok_Quote =>
            Scan;
            if Quasiquote then
               Machine.Push (Get_Symbol ("quote"));
               Machine.Push (Get_Symbol ("quote"));
               Machine.Push (Lith.Objects.Nil);
               Machine.Cons;
               Machine.Cons;
            else
               Machine.Push (Get_Symbol ("quote"));
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
            Lith.Parser.Lexical.Identifiers.Push_Integer
              (Machine, Tok_Text);
            Scan;
         when Tok_Float =>
            Lith.Objects.Numbers.Push_Float (Machine, Tok_Text);
            Scan;
         when others =>
            Error ("bad token");
            Machine.Push (Lith.Objects.Nil);
            Scan;
      end case;

   end Parse_S_Expression;

end Lith.Parser;

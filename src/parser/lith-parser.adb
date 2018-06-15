with Ada.Text_IO;

with Lith.Parser.Tokens;             use Lith.Parser.Tokens;
with Lith.Parser.Lexical;            use Lith.Parser.Lexical;
with Lith.Parser.Lexical.Identifiers;

with Lith.Objects.Real;

with Lith.IO.Text_IO;

package body Lith.Parser is

   function "+" (Left : String) return Lith.Objects.Object
                 renames Lith.Objects.To_Symbol_Object;

   procedure Parse_S_Expression
     (Store : in out Lith.Objects.Object_Store'Class;
      Quasiquote : Boolean);
   --  Parse a single s-expression, leaving the result on the top of the stack

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression
     (Store : in out Lith.Objects.Object_Store'Class;
      Expr    : String)
      return Lith.Objects.Object
   is
   begin
      Open_String (Expr);
      Parse_S_Expression (Store, Quasiquote => False);
      Close;
      return Store.Pop;
   end Parse_Expression;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Store : in out Lith.Objects.Object_Store'Class;
      Path    : String)
   is
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         Parse_S_Expression (Store, Quasiquote => False);

         declare
            Top : constant Lith.Objects.Object := Store.Pop;
            Result : constant Lith.Objects.Object :=
                       Store.Evaluate (Top);
         begin
            if False then
               Ada.Text_IO.Put_Line
                 (Store.Show (Result));
            end if;
            Store.Reset;
         end;
      end loop;
      Close;
   end Parse_File;

   ------------------------
   -- Parse_S_Expression --
   ------------------------

   procedure Parse_S_Expression
     (Store : in out Lith.Objects.Object_Store'Class;
      Quasiquote : Boolean)
   is

      procedure Parse_Rest_Of_List;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      procedure Parse_Rest_Of_List is
         Start_Line : constant Natural := Current_Line;
      begin
         Parse_S_Expression (Store, Quasiquote);

         if Tok = Tok_Dot then
            Scan;
            Parse_S_Expression (Store, Quasiquote);
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
         elsif Tok = Tok_Right_Paren then
            Store.Push (Lith.Objects.Nil);
            Scan;
         elsif Tok = Tok_End_Of_File then
            Error ("Missing ')' in expression started on line"
                   & Natural'Image (Start_Line));
            Store.Push (Lith.Objects.Nil);
         else
            Parse_Rest_Of_List;
         end if;

         Store.Cons;

      end Parse_Rest_Of_List;

   begin

      Store.Set_File_Context (Current_File_Name, Current_Line);

      case Tok is
         when Tok_Nil =>
            Scan;
            Store.Push (Lith.Objects.Nil);
         when Tok_Left_Paren =>
            Scan;
            if Quasiquote then
               Store.Push (Lith.Objects.To_Symbol_Object ("list"));
            end if;
            Parse_Rest_Of_List;
            if Quasiquote then
               Store.Cons;
            end if;
         when Tok_Identifier =>
            if Tok_Text = "#t" then
               Store.Push (Lith.Objects.True_Value);
            elsif Tok_Text = "#f" then
               Store.Push (Lith.Objects.False_Value);
            elsif Tok_Text = "#no-value" then
               Store.Push (Lith.Objects.No_Value);
            elsif Tok_Text = "#string" then
               Store.Push (Lith.Objects.String_Value);
            elsif Quasiquote then
               Store.Push (Lith.Objects.Single_Quote);
               Store.Push (Lith.Objects.To_Symbol_Object (Tok_Text));
               Store.Push (Lith.Objects.Nil);
               Store.Cons;
               Store.Cons;
            else
               Store.Push (+Tok_Text);
            end if;
            Scan;
         when Tok_Start_Vector =>
            Scan;
            Store.Push ("vector");
            Parse_Rest_Of_List;
            Store.Cons;
         when Tok_Start_Bytevector =>
            Scan;
            Store.Push ("bytevector");
            Parse_Rest_Of_List;
            Store.Cons;

         when Tok_Character =>
            Store.Push
              (Lith.Objects.To_Object
                 (Tok_Character_Value));
            Scan;
         when Tok_String =>
            Store.Push_String (Tok_Text);
            Scan;
         when Tok_Quote =>
            Scan;
            if Quasiquote then
               Store.Push (Lith.Objects.Single_Quote);
               Store.Push (Lith.Objects.Single_Quote);
               Store.Push (Lith.Objects.Nil);
               Store.Cons;
               Store.Cons;
            else
               Store.Push (Lith.Objects.Single_Quote);
            end if;
            Parse_S_Expression (Store, Quasiquote);
            Store.Push (Lith.Objects.Nil);
            Store.Cons;
            Store.Cons;
         when Tok_Quasiquote =>
            Scan;
            Parse_S_Expression (Store, True);
         when Tok_Comma =>
            Scan;
            Parse_S_Expression (Store, False);
         when Tok_Integer =>
            Lith.Parser.Lexical.Identifiers.Push_Integer
              (Store, Tok_Text);
            Scan;
         when Tok_Float =>
            declare
               use Lith.Objects;
               Real : constant External_Object_Interface'Class :=
                        Lith.Objects.Real.To_Real (Tok_Text);
            begin
               Store.Push
                 (Store.Create_External_Reference (Real));
            end;
            Scan;
         when others =>
            Error ("bad token");
            Store.Push (Lith.Objects.Nil);
            Scan;
      end case;

      Store.Save_Context;

   end Parse_S_Expression;

   ---------------
   -- Read_Port --
   ---------------

   function Read_Port
     (Store : in out Lith.Objects.Object_Store'Class;
      Port    : Lith.Objects.Object)
      return Lith.Objects.Object
   is
   begin
      Open_Port
        (Lith.IO.Text_IO.Text_Port_Type'Class
           (Store.Get_External_Object (Port).all)'Access);
      Parse_S_Expression (Store, Quasiquote => False);
      Close;
      return Store.Pop;
   end Read_Port;

end Lith.Parser;

with Lith.Objects;

package Lith.Symbols is

   function Get_Symbol (Name : Wide_Wide_String)
                        return Lith.Objects.Symbol_Type;

   function Get_Name (Symbol : Lith.Objects.Symbol_Type)
                      return Wide_Wide_String;

   function Apply_Syntax_Atom return Lith.Objects.Object;
   function Begin_Atom return Lith.Objects.Object;
   function Choice_Atom return Lith.Objects.Object;
   function Define_Atom return Lith.Objects.Object;
   function Dot return Lith.Objects.Object;
   function Ellipsis_Atom return Lith.Objects.Object;
   function Eval_Atom return Lith.Objects.Object;
   function False_Atom return Lith.Objects.Object;
   function If_Atom return Lith.Objects.Object;
   function Import_Atom return Lith.Objects.Object;
   function Internal_Define_Atom return Lith.Objects.Object;
   function Lambda return Lith.Objects.Object;
   function Large_Integer_Atom return Lith.Objects.Object;
   function Lith_Define_Atom return Lith.Objects.Object;
   function Lith_Set_Atom return Lith.Objects.Object;
   function Macro return Lith.Objects.Object;
   function Quote return Lith.Objects.Object;
   function Quasiquote return Lith.Objects.Object;
   function Rational_Atom return Lith.Objects.Object;
   function Set_Atom return Lith.Objects.Object;
   function Stack_To_Control return Lith.Objects.Object;
   function String_Atom return Lith.Objects.Object;
   function True_Atom return Lith.Objects.Object;
   function Unquote return Lith.Objects.Object;
   function Unquote_Splicing return Lith.Objects.Object;
   function Wildcard_Atom return Lith.Objects.Object;

end Lith.Symbols;

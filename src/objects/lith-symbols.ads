with Lith.Objects;

package Lith.Symbols is

   function Get_Symbol (Name : String) return Lith.Objects.Symbol_Type;
   function Get_Name (Symbol : Lith.Objects.Symbol_Type) return String;

   function Begin_Atom return Lith.Objects.Object;
   function Dot return Lith.Objects.Object;
   function False_Atom return Lith.Objects.Object;
   function Lambda return Lith.Objects.Object;
   function Macro return Lith.Objects.Object;
   function Quote return Lith.Objects.Object;
   function Quasiquote return Lith.Objects.Object;
   function String_Atom return Lith.Objects.Object;
   function True_Atom return Lith.Objects.Object;
   function Unquote return Lith.Objects.Object;
   function Unquote_Splicing return Lith.Objects.Object;

end Lith.Symbols;

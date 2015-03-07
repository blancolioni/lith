with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

with Lith.Objects.Symbol_Vectors;

package body Lith.Symbols is

   package Symbol_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Lith.Objects.Symbol_Type,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Lith.Objects."=");

   package Symbol_Names is
     new Lith.Objects.Symbol_Vectors (String);

   Symbol_To_Name_Map : Symbol_Names.Vector;
   Name_To_Symbol_Map : Symbol_Maps.Map;

   ----------------
   -- Begin_Atom --
   ----------------

   function Begin_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("begin"));
   end Begin_Atom;

   ---------
   -- Dot --
   ---------

   function Dot return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("."));
   end Dot;

   ----------------
   -- False_Atom --
   ----------------

   function False_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("#f"));
   end False_Atom;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Symbol : Lith.Objects.Symbol_Type) return String is
   begin
      return Symbol_To_Name_Map.Element (Symbol);
   end Get_Name;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Name : String) return Lith.Objects.Symbol_Type is
      Result : Lith.Objects.Symbol_Type;
   begin
      if not Name_To_Symbol_Map.Contains (Name) then
         Symbol_To_Name_Map.New_Element (Name, Result);
         Name_To_Symbol_Map.Insert (Name, Result);
      else
         Result := Name_To_Symbol_Map.Element (Name);
      end if;
      return Result;
   end Get_Symbol;

   ------------
   -- Lambda --
   ------------

   function Lambda return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("lambda"));
   end Lambda;

   -----------
   -- Macro --
   -----------

   function Macro return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("macro"));
   end Macro;

   ----------------
   -- Quasiquote --
   ----------------

   function Quasiquote return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("quasiquote"));
   end Quasiquote;

   -----------
   -- Quote --
   -----------

   function Quote return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("quote"));
   end Quote;

   -----------------
   -- String_Atom --
   -----------------

   function String_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("#string"));
   end String_Atom;

   ---------------
   -- True_Atom --
   ---------------

   function True_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("#t"));
   end True_Atom;

   -------------
   -- Unquote --
   -------------

   function Unquote return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("unquote"));
   end Unquote;

   ----------------------
   -- Unquote_Splicing --
   ----------------------

   function Unquote_Splicing return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("unquote-splicing"));
   end Unquote_Splicing;

end Lith.Symbols;

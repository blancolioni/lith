with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;

with Lith.Objects.Symbol_Vectors;

package body Lith.Symbols is

   package Symbol_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Wide_Wide_String,
        Element_Type    => Lith.Objects.Symbol_Type,
        Hash            => Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash,
        Equivalent_Keys => "=",
        "="             => Lith.Objects."=");

   package Symbol_Names is
     new Lith.Objects.Symbol_Vectors (Wide_Wide_String);

   Symbol_To_Name_Map : Symbol_Names.Vector;
   Name_To_Symbol_Map : Symbol_Maps.Map;

   ----------------
   -- Begin_Atom --
   ----------------

   function Begin_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("begin"));
   end Begin_Atom;

   -----------------
   -- Choice_Atom --
   -----------------

   function Choice_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("(choice)"));
   end Choice_Atom;

   function Define_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("define"));
   end Define_Atom;

   ---------
   -- Dot --
   ---------

   function Dot return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("."));
   end Dot;

   ---------------
   -- Eval_Atom --
   ---------------

   function Eval_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("eval"));
   end Eval_Atom;

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

   function Get_Name
     (Symbol : Lith.Objects.Symbol_Type)
      return Wide_Wide_String is
   begin
      return Symbol_To_Name_Map.Element (Symbol);
   end Get_Name;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Name : Wide_Wide_String)
      return Lith.Objects.Symbol_Type
   is
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

   -------------
   -- If_Atom --
   -------------

   function If_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("if"));
   end If_Atom;

   ------------
   -- Lambda --
   ------------

   function Lambda return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("lambda"));
   end Lambda;

   ----------------------
   -- Lith_Define_Atom --
   ----------------------

   function Lith_Define_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("lith-define"));
   end Lith_Define_Atom;

   -------------------
   -- Lith_Set_Atom --
   -------------------

   function Lith_Set_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("(lith-set!)"));
   end Lith_Set_Atom;

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

   --------------
   -- Set_Atom --
   --------------

   function Set_Atom return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("set!"));
   end Set_Atom;

   ----------------------
   -- Stack_To_Control --
   ----------------------

   function Stack_To_Control return Lith.Objects.Object is
   begin
      return Lith.Objects.To_Object (Get_Symbol ("(stack->control)"));
   end Stack_To_Control;

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

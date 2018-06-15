with Ada.Characters.Handling;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

with Lith.Objects.Symbol_Vectors;

package body Lith.Symbols is

   use Lith.Objects;

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

   type Builtin_Symbol is
     (Sym_Choice, Sym_Do_Car, Sym_Do_Cdr, Sym_Do_Null,
      Sym_Internal_Define, Sym_Stack_To_Control,
      Sym_Stack_Drop, Sym_Tail_Context, Sym_Unwind_Continue, Sym_Unwind_Dump,
      Sym_Unwind_Protect,
      Sym_Apply_Syntax, Sym_Begin, Sym_Car, Sym_Cdr, Sym_Dynamic_Wind,
      Sym_Ellipsis, Sym_Eval, Sym_If, Sym_Import, Sym_Internal_Apply,
      Sym_Lambda, Sym_Lith_Define, Sym_Macro, Sym_Null, Sym_Quote, Sym_Raise,
      Sym_Set, Sym_Unquote_Splicing, Sym_Unquote, Sym_Wildcard,
      Sym_With_Exception_Handler);

   subtype Anonymous_Builtin_Symbol is
     Builtin_Symbol range Sym_Choice .. Sym_Unwind_Protect;

   subtype Named_Builtin_Symbol is
     Builtin_Symbol range Sym_Apply_Syntax .. Sym_With_Exception_Handler;

   function Get_Builtin_Symbol
     (Symbol : Builtin_Symbol)
      return Lith.Objects.Object;

   function Builtin_Symbol_To_String
     (Symbol : Builtin_Symbol)
      return String;

   function Builtin_Symbol_To_String
     (Symbol : Builtin_Symbol)
      return String
   is
      use Ada.Characters.Handling;
      Image : constant String :=
                Builtin_Symbol'Image (Symbol);
      Result : String :=
                 To_Lower (Image (5 .. Image'Last));
   begin
      for I in Result'Range loop
         if Result (I) = '_' then
            Result (I) := '-';
         end if;
      end loop;
      return Result;
   end Builtin_Symbol_To_String;

   ------------------------
   -- Get_Builtin_Symbol --
   ------------------------

   function Get_Builtin_Symbol
     (Symbol : Builtin_Symbol)
      return Lith.Objects.Object
   is
      Sym : constant Symbol_Type :=
              Unchecked_From_Natural (Builtin_Symbol'Pos (Symbol) + 1);
   begin
      return To_Object (Sym);
   end Get_Builtin_Symbol;

   function Apply_Syntax_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Apply_Syntax));

   function Begin_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Begin));

   function Car_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Car));

   function Cdr_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Cdr));

   function Choice return Object
   is (Get_Builtin_Symbol (Sym_Choice));

   function Do_Car return Object
   is (Get_Builtin_Symbol (Sym_Do_Car));

   function Do_Cdr return Object
   is (Get_Builtin_Symbol (Sym_Do_Cdr));

   function Do_Null return Object
   is (Get_Builtin_Symbol (Sym_Do_Null));

   function Dynamic_Wind_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Dynamic_Wind));

   function Ellipsis_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Ellipsis));

   function Eval_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Eval));

   function If_Symbol return Object
   is (Get_Builtin_Symbol (Sym_If));

   function Import_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Import));

   function Internal_Apply return Object
   is (Get_Builtin_Symbol (Sym_Internal_Apply));

   function Internal_Define return Object
   is (Get_Builtin_Symbol (Sym_Internal_Define));

   function Lambda_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Lambda));

   function Lith_Define_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Lith_Define));

   function Macro_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Macro));

   function Null_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Null));

   function Quote_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Quote));

   function Raise_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Raise));

   function Set_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Set));

   function Stack_To_Control return Object
   is (Get_Builtin_Symbol (Sym_Stack_To_Control));

   function Stack_Drop return Object
   is (Get_Builtin_Symbol (Sym_Stack_Drop));

   function Tail_Context return Object
   is (Get_Builtin_Symbol (Sym_Tail_Context));

   function Unquote_Splicing_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Unquote_Splicing));

   function Unquote_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Unquote));

   function Unwind_Continue return Object
   is (Get_Builtin_Symbol (Sym_Unwind_Continue));

   function Unwind_Dump return Object
   is (Get_Builtin_Symbol (Sym_Unwind_Dump));

   function Unwind_Protect return Object
   is (Get_Builtin_Symbol (Sym_Unwind_Protect));

   function Wildcard_Symbol return Object
   is (Get_Builtin_Symbol (Sym_Wildcard));

   function With_Exception_Handler_Symbol return Object
   is (Get_Builtin_Symbol (Sym_With_Exception_Handler));

   -------------------
   -- Is_Predefined --
   -------------------

   function Is_Predefined
     (Symbol : Lith.Objects.Symbol_Type)
      return Boolean
   is
   begin
      return Unchecked_To_Natural (Symbol)
        <= Builtin_Symbol'Pos (Builtin_Symbol'Last) + 1;
   end Is_Predefined;

   ----------------------
   -- String_To_Symbol --
   ----------------------

   function String_To_Symbol
     (Name : String)
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
   end String_To_Symbol;

   ----------------------
   -- Symbol_To_String --
   ----------------------

   function Symbol_To_String
     (Symbol : Lith.Objects.Symbol_Type)
      return String
   is
   begin
      return Symbol_To_Name_Map.Element (Symbol);
   exception
      when Constraint_Error =>
         return "#error" & Integer'Image (-Unchecked_To_Natural (Symbol));
   end Symbol_To_String;

begin
   for Sym_Name in Builtin_Symbol loop
      declare
         Sym_Value : constant Symbol_Type :=
                       Unchecked_From_Natural
                         (Builtin_Symbol'Pos (Sym_Name) + 1);
         New_Value : Symbol_Type;
         Name : constant String :=
                  (if Sym_Name in Anonymous_Builtin_Symbol
                   then "<" & Builtin_Symbol_To_String (Sym_Name) & ">"
                   elsif Sym_Name = Sym_Ellipsis
                   then "..."
                   elsif Sym_Name = Sym_Wildcard
                   then "_"
                   elsif Sym_Name = Sym_Set
                   then "set!"
                   elsif Sym_Name = Sym_Null
                   then "null?"
                   else Builtin_Symbol_To_String (Sym_Name));
      begin
         Symbol_To_Name_Map.New_Element (Name, New_Value);
         pragma Assert (Sym_Value = New_Value);

         if Sym_Name in Named_Builtin_Symbol then
            Name_To_Symbol_Map.Insert (Name, Sym_Value);
         end if;
      end;
   end loop;

end Lith.Symbols;

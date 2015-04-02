package Lith.Objects.Symbols is

   function Get_Symbol (Name : Wide_Wide_String)
                        return Lith.Objects.Symbol_Type;

   function Get_Name (Symbol : Lith.Objects.Symbol_Type)
                      return Wide_Wide_String;

   function Apply_Syntax_Symbol return Lith.Objects.Object;
   function Begin_Symbol return Lith.Objects.Object;
   function Choice return Lith.Objects.Object;
   function Dynamic_Wind_Symbol return Lith.Objects.Object;
   function Ellipsis_Symbol return Lith.Objects.Object;
   function If_Symbol return Lith.Objects.Object;
   function Import_Symbol return Lith.Objects.Object;
   function Internal_Define return Lith.Objects.Object;
   function Lambda_Symbol return Lith.Objects.Object;
   function Lith_Define_Symbol return Lith.Objects.Object;
   function Macro_Symbol return Lith.Objects.Object;
   function Quote_Symbol return Lith.Objects.Object;
   function Raise_Symbol return Lith.Objects.Object;
   function Set_Symbol return Lith.Objects.Object;
   function Stack_To_Control return Lith.Objects.Object;
   function Stack_Drop return Lith.Objects.Object;
   function Tail_Context return Lith.Objects.Object;
   function Unquote_Splicing_Symbol return Lith.Objects.Object;
   function Unquote_Symbol return Lith.Objects.Object;
   function Unwind_Continue return Lith.Objects.Object;
   function Unwind_Dump return Lith.Objects.Object;
   function Unwind_Protect return Lith.Objects.Object;
   function Wildcard_Symbol return Lith.Objects.Object;
   function With_Exception_Handler_Symbol return Lith.Objects.Object;

end Lith.Objects.Symbols;
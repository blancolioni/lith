with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;

with WL.Random;

with Lith.Objects.Interfaces;
with Lith.Primitives.ALU;
with Lith.Symbols;

package body Lith.Primitives is

   function Evaluate_ALU
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Symbol_To_String
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Car
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Char_To_Integer
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Cons
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Eq
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Eval
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is (Arguments (Arguments'First));

   function Evaluate_Integer_To_Char
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Is_Integer
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Is_Null
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Is_Pair
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Is_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Random
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Set_Car
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Set_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Write_Char
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("#alu", 2, Evaluate_ALU'Access);
      Define_Function ("symbol->string", 1, Evaluate_Symbol_To_String'Access);
      Define_Function ("car", 1, Evaluate_Car'Access);
      Define_Function ("cdr", 1, Evaluate_Cdr'Access);
      Define_Function ("char->integer", 1, Evaluate_Char_To_Integer'Access);
      Define_Function ("cons", 2, Evaluate_Cons'Access);
      Define_Function ("eq?", 2, Evaluate_Eq'Access);
      Define_Function ("eval", 1, Evaluate_Eval'Access);
      Define_Function ("integer->char", 1, Evaluate_Integer_To_Char'Access);
      Define_Function ("load", 1, Evaluate_Load'Access);
      Define_Function ("null?", 1, Evaluate_Is_Null'Access);
      Define_Function ("pair?", 1, Evaluate_Is_Pair'Access);
      Define_Function ("symbol?", 1, Evaluate_Is_Symbol'Access);
      Define_Function ("integer?", 1, Evaluate_Is_Integer'Access);
      Define_Function ("random", 1, Evaluate_Random'Access);
      Define_Function ("set-car!", 2, Evaluate_Set_Car'Access);
      Define_Function ("set-cdr!", 2, Evaluate_Set_Cdr'Access);
      Define_Function ("write-char", 2, Evaluate_Write_Char'Access);
      Lith.Primitives.ALU.Add_Operators;
   end Add_Primitives;

   ------------------
   -- Evaluate_ALU --
   ------------------

   function Evaluate_ALU
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Op : constant Lith.Objects.Symbol_Type :=
             Lith.Objects.To_Symbol (Arguments (Arguments'First));
      Args : constant Lith.Objects.Array_Of_Objects :=
               Store.To_Object_Array
                 (Arguments (Arguments'First + 1));
   begin
      return Lith.Primitives.ALU.Apply (Store, Op, Args);
   end Evaluate_ALU;

   ------------------
   -- Evaluate_Car --
   ------------------

   function Evaluate_Car
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Xs : constant Lith.Objects.Object := Arguments (Arguments'First);
   begin
      if Lith.Objects.Is_Atom (Xs) then
         raise Constraint_Error with
           "car: not a list: "
           & Ada.Characters.Conversions.To_String (Store.Show (Xs));
      else
         return Store.Car (Xs);
      end if;
   end Evaluate_Car;

   ------------------
   -- Evaluate_Cdr --
   ------------------

   function Evaluate_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Xs : constant Lith.Objects.Object := Arguments (Arguments'First);
   begin
      if Lith.Objects.Is_Atom (Xs) then
         raise Constraint_Error with
           "cdr: not a list: "
           & Ada.Characters.Conversions.To_String (Store.Show (Xs));
      else
         return Store.Cdr (Xs);
      end if;
   end Evaluate_Cdr;

   ------------------------------
   -- Evaluate_Char_To_Integer --
   ------------------------------

   function Evaluate_Char_To_Integer
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      return To_Object
        (Integer'
           (Wide_Wide_Character'Pos
                (To_Character (Arguments (Arguments'First)))));
   end Evaluate_Char_To_Integer;

   -------------------
   -- Evaluate_Cons --
   -------------------

   function Evaluate_Cons
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Car : constant Lith.Objects.Object := Arguments (Arguments'First);
      Cdr : constant Lith.Objects.Object := Arguments (Arguments'First + 1);
   begin
      return Store.Cons (Car, Cdr);
   end Evaluate_Cons;

   -----------------
   -- Evaluate_Eq --
   -----------------

   function Evaluate_Eq
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Arguments (Arguments'First) = Arguments (Arguments'Last) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Eq;

   ------------------------------
   -- Evaluate_Integer_To_Char --
   ------------------------------

   function Evaluate_Integer_To_Char
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      return To_Object
        (Wide_Wide_Character'Val
                (To_Integer (Arguments (Arguments'First))));
   end Evaluate_Integer_To_Char;

   -------------------------
   -- Evaluate_Is_Integer --
   -------------------------

   function Evaluate_Is_Integer
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      A : constant Object := Arguments (Arguments'First);
   begin
      if Is_Integer (A)
        or else (Is_Pair (A)
                 and then Store.Car (A) = Lith.Symbols.Large_Integer_Atom)
      then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Integer;

   ----------------------
   -- Evaluate_Is_Null --
   ----------------------

   function Evaluate_Is_Null
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Arguments (Arguments'First) = Nil then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Null;

   ----------------------
   -- Evaluate_Is_Pair --
   ----------------------

   function Evaluate_Is_Pair
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Is_Pair (Arguments (Arguments'First)) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Pair;

   ------------------------
   -- Evaluate_Is_Symbol --
   ------------------------

   function Evaluate_Is_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Is_Symbol (Arguments (Arguments'First)) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Symbol;

   -------------------
   -- Evaluate_Load --
   -------------------

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Path : constant Wide_Wide_String :=
               Store.To_String (Arguments (Arguments'First));
   begin
      if Store.Load (Path) then
         return Lith.Objects.True_Value;
      else
         return Lith.Objects.False_Value;
      end if;
   end Evaluate_Load;

   ---------------------
   -- Evaluate_Random --
   ---------------------

   function Evaluate_Random
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      Max : constant Integer :=
              Lith.Objects.To_Integer (Arguments (Arguments'First));
      Result : constant Integer :=
                 WL.Random.Random_Number (0, Max - 1);
   begin
      return Lith.Objects.To_Object (Result);
   end Evaluate_Random;

   ----------------------
   -- Evaluate_Set_Car --
   ----------------------

   function Evaluate_Set_Car
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
   begin
      Store.Set_Car (Arguments (Arguments'First),
                     Arguments (Arguments'First + 1));
      return Arguments (Arguments'First);
   end Evaluate_Set_Car;

   ----------------------
   -- Evaluate_Set_Cdr --
   ----------------------

   function Evaluate_Set_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
   begin
      Store.Set_Cdr (Arguments (Arguments'First),
                     Arguments (Arguments'First + 1));
      return Arguments (Arguments'First);
   end Evaluate_Set_Cdr;

   -------------------------------
   -- Evaluate_Symbol_To_String --
   -------------------------------

   function Evaluate_Symbol_To_String
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Text : constant Wide_Wide_String :=
               Store.Show (Arguments (Arguments'First));
      Result : Object := Nil;
   begin
      for Ch of reverse Text loop
         Result := Store.Cons (To_Object (Ch), Result);
      end loop;
      Result := Store.Cons (Lith.Symbols.String_Atom, Result);
      return Result;
   end Evaluate_Symbol_To_String;

   -------------------------
   -- Evaluate_Write_Char --
   -------------------------

   function Evaluate_Write_Char
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Ada.Wide_Wide_Text_IO;
      Char : constant Wide_Wide_Character :=
               Lith.Objects.To_Character (Arguments (Arguments'First));
      Code : constant Natural :=
               Wide_Wide_Character'Pos (Char);
   begin
      if Code = 10 then
         Ada.Wide_Wide_Text_IO.New_Line;
      elsif Code in 256 .. 65535 then
         Put (Wide_Wide_Character'Val (Code / 256));
         Put (Wide_Wide_Character'Val (Code mod 256));
      else
         Put (Char);
      end if;
      return Arguments (Arguments'First);
   end Evaluate_Write_Char;

end Lith.Primitives;

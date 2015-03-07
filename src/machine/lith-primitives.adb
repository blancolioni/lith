with Ada.Text_IO;

with Lith.Environment;
with Lith.Evaluator;
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

   function Evaluate_Begin
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object;

   function Evaluate_Car
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Cons
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Define
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
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

   function Evaluate_If
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object;

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Null
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Pair
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Set
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
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
      Define_Function ("begin", 1, False, Evaluate_Begin'Access);
      Define_Function ("car", 1, Evaluate_Car'Access);
      Define_Function ("cdr", 1, Evaluate_Cdr'Access);
      Define_Function ("cons", 2, Evaluate_Cons'Access);
      Define_Function ("define", 2, False, Evaluate_Define'Access);
      Define_Function ("eq?", 2, Evaluate_Eq'Access);
      Define_Function ("eval", 1, Evaluate_Eval'Access);
      Define_Function ("if", 3, False, Evaluate_If'Access);
      Define_Function ("load", 1, Evaluate_Load'Access);
      Define_Function ("null?", 1, Evaluate_Null'Access);
      Define_Function ("pair?", 1, Evaluate_Pair'Access);
      Define_Function ("set!", 2, False, Evaluate_Set'Access);
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
      return Lith.Primitives.ALU.Apply (Op, Args);
   end Evaluate_ALU;

   --------------------
   -- Evaluate_Begin --
   --------------------

   function Evaluate_Begin
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      Result : Lith.Objects.Object;
   begin
      for Arg of Arguments loop
         Result :=
           Lith.Evaluator.Evaluate
             (Store => Store,
              Expr  => Arg,
              Env   => Environment);
      end loop;
      return Result;
   end Evaluate_Begin;

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
           "car: not a list: " & Store.Show (Xs);
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
           "cdr: not a list: " & Store.Show (Xs);
      else
         return Store.Cdr (Xs);
      end if;
   end Evaluate_Cdr;

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

   ---------------------
   -- Evaluate_Define --
   ---------------------

   function Evaluate_Define
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Head : constant Object :=
               Arguments (Arguments'First);
      Value : Object;
   begin
      if Arguments'Length = 2 then
         Value := Arguments (Arguments'First + 1);
      else
         Value := Nil;
         for I in reverse Arguments'First + 1 .. Arguments'Last loop
            Value := Store.Cons (Arguments (I), Value);
         end loop;
         Value := Store.Cons (Lith.Symbols.Begin_Atom,
                              Value);
      end if;

      if Lith.Objects.Is_Symbol (Head) then
         Lith.Environment.Define
           (Lith.Objects.To_Symbol (Head),
            Lith.Evaluator.Evaluate
              (Store, Value, Environment));
         return Head;
      else
         Lith.Environment.Define
           (Lith.Objects.To_Symbol (Store.Car (Head)),
            Store.Cons (Lith.Symbols.Lambda,
              Store.Cons (Store.Cdr (Head),
                Store.Cons (Value,
                  Lith.Objects.Nil))));
         return Store.Car (Head);
      end if;
   end Evaluate_Define;

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
         return Lith.Symbols.True_Atom;
      else
         return Lith.Symbols.False_Atom;
      end if;
   end Evaluate_Eq;

   -----------------
   -- Evaluate_If --
   -----------------

   function Evaluate_If
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use type Lith.Objects.Object;
      Test : constant Lith.Objects.Object :=
               Lith.Evaluator.Evaluate
                 (Store,
                  Arguments (Arguments'First),
                  Environment);
      Result : constant Lith.Objects.Object :=
                 (if Test /= Lith.Symbols.False_Atom
                  then Arguments (Arguments'First + 1)
                  else Arguments (Arguments'First + 2));
   begin
      return Lith.Evaluator.Evaluate
        (Store, Result, Environment);
   end Evaluate_If;

   -------------------
   -- Evaluate_Load --
   -------------------

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      Path : constant String :=
               Store.To_String (Arguments (Arguments'First));
   begin
      if Store.Load (Path) then
         return Lith.Symbols.True_Atom;
      else
         return Lith.Symbols.False_Atom;
      end if;
   end Evaluate_Load;

   -------------------
   -- Evaluate_Null --
   -------------------

   function Evaluate_Null
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Arguments (Arguments'First) = Nil then
         return Lith.Symbols.True_Atom;
      else
         return Lith.Symbols.False_Atom;
      end if;
   end Evaluate_Null;

   -------------------
   -- Evaluate_Pair --
   -------------------

   function Evaluate_Pair
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use Lith.Objects;
   begin
      if Is_Pair (Arguments (Arguments'First)) then
         return Lith.Symbols.True_Atom;
      else
         return Lith.Symbols.False_Atom;
      end if;
   end Evaluate_Pair;

   ------------------
   -- Evaluate_Set --
   ------------------

   function Evaluate_Set
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      Value : constant Lith.Objects.Object :=
                Lith.Evaluator.Evaluate
                  (Store, Arguments (Arguments'First + 1), Environment);
   begin
      Lith.Environment.Replace
        (Name  => Lith.Objects.To_Symbol (Arguments (Arguments'First)),
         Value => Value);
      return Value;
   end Evaluate_Set;

   -------------------------------
   -- Evaluate_Symbol_To_String --
   -------------------------------

   function Evaluate_Symbol_To_String
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Text : constant String := Store.Show (Arguments (Arguments'First));
      Result : Object := Nil;
   begin
      for Ch of reverse Text loop
         Result :=
           Store.Cons ((To_Object (Integer'(Character'Pos (Ch)))), Result);
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
   begin
      Ada.Text_IO.Put
        (Character'Val
           (Lith.Objects.To_Integer (Arguments (Arguments'First))));
      return Arguments (Arguments'First);
   end Evaluate_Write_Char;

end Lith.Primitives;

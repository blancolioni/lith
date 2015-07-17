with Ada.Calendar;
with Ada.Text_IO;

with WL.Random;

with Lith.Objects.Interfaces;
with Lith.Objects.Large_Integers;
with Lith.Objects.Real;
with Lith.Primitives.ALU;
with Lith.Objects.Symbols;

with Lith.IO.Text_IO;
with Lith.Parser;

package body Lith.Primitives is

   Next_Gensym_Index : Natural := 0;
   Jiffy_Start_Time : constant Ada.Calendar.Time :=
                         Ada.Calendar.Clock;
   Jiffies_Per_Second : constant := 1000.0;

   type Predicate_Function is access
     function (Value : Lith.Objects.Object) return Boolean;

   type Predicate_Evaluator is
     new Lith.Objects.Interfaces.Root_Function_Interface with
      record
         Fn : Predicate_Function;
      end record;

   overriding function Evaluate
     (Predicate : Predicate_Evaluator;
      Store     : in out Lith.Objects.Object_Store'Class)
     return Lith.Objects.Object;

   procedure Define_Predicate_Function
     (Name : String;
      Evaluator : Predicate_Function);

   function Evaluate_ALU
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Car
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Char_To_Integer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Cons
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Current_Jiffy
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Eq
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Eval
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is (Store.Argument (1));

   function Evaluate_External_Equal
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
     with Pre => Store.Argument_Count = 2
       and then Lith.Objects.Is_External_Object
       (Store.Argument (1));

   function Evaluate_External_Is_Type
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_External_Print
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
     with Pre => Store.Argument_Count = 1
       and then Lith.Objects.Is_External_Object
       (Store.Argument (1));

   function Evaluate_Gensym
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Integer_To_Char
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Is_External
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Is_Integer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Is_Null
     (Item : Lith.Objects.Object)
     return Boolean;

   function Evaluate_Is_Pair
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Is_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Is_Real
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Jiffies_Per_Second
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Profile_Start_Cost_Centre
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Profile_Finish_Cost_Centre
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Random
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Read
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
     with Pre => Store.Argument_Count = 1
     and then Lith.Objects.Is_External_Object (Store.Argument (1))
     and then Store.Get_External_Object (Store.Argument (1)).all
       in Lith.IO.Text_IO.Text_Port_Type'Class
       and then Lith.IO.Get_Port (Store, Store.Argument (1)).Is_Open
       and then Lith.IO.Get_Port (Store, Store.Argument (1)).Is_Input;

   function Evaluate_Set_Car
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Set_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_String_To_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Symbol_To_String
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   function Evaluate_Write_Char
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object;

   --------------------
   -- Add_Primitives --
   --------------------

   procedure Add_Primitives is
      use Lith.Objects.Interfaces;
   begin
      Define_Function ("#alu", 3, Evaluate_ALU'Access);
      Define_Function ("car", 1, Evaluate_Car'Access);
      Define_Function ("cdr", 1, Evaluate_Cdr'Access);
      Define_Function ("char->integer", 1, Evaluate_Char_To_Integer'Access);
      Define_Function ("cons", 2, Evaluate_Cons'Access);
      Define_Function ("current-jiffy", 2, Evaluate_Current_Jiffy'Access);
      Define_Function ("eq?", 2, Evaluate_Eq'Access);
      Define_Function ("eval", 1, Evaluate_Eval'Access);
      Define_Function ("lith-external-equal", 2,
                       Evaluate_External_Equal'Access);
      Define_Function ("#extern-is-type", 2, Evaluate_External_Is_Type'Access);
      Define_Function ("lith-external-print", 1,
                       Evaluate_External_Print'Access);
      Define_Function ("gensym", 0, Evaluate_Gensym'Access);
      Define_Function ("integer->char", 1, Evaluate_Integer_To_Char'Access);
      Define_Function ("lith-external?", 1, Evaluate_Is_External'Access);
      Define_Function ("lith-read-object", 1, Evaluate_Read'Access);
      Define_Function ("jiffies-per-second", 1,
                       Evaluate_Jiffies_Per_Second'Access);
      Define_Function ("load", 1, Evaluate_Load'Access);
      Define_Function ("pair?", 1, Evaluate_Is_Pair'Access);
      Define_Function ("profile-start-cost-centre", 1,
                       Evaluate_Profile_Start_Cost_Centre'Access);
      Define_Function ("profile-finish-cost-centre", 1,
                       Evaluate_Profile_Finish_Cost_Centre'Access);
      Define_Function ("real?", 1, Evaluate_Is_Real'Access);
      Define_Function ("symbol?", 1, Evaluate_Is_Symbol'Access);
      Define_Function ("random", 1, Evaluate_Random'Access);
      Define_Function ("set-car!", 2, Evaluate_Set_Car'Access);
      Define_Function ("set-cdr!", 2, Evaluate_Set_Cdr'Access);
      Define_Function ("string->symbol", 1, Evaluate_String_To_Symbol'Access);
      Define_Function ("symbol->string", 1, Evaluate_Symbol_To_String'Access);
      Define_Function ("write-char", 2, Evaluate_Write_Char'Access);

      Define_Predicate_Function
        ("null?", Evaluate_Is_Null'Access);
      Define_Function
        ("integer?", 1, Evaluate_Is_Integer'Access);

      Lith.Primitives.ALU.Add_Operators;
   end Add_Primitives;

   -------------------------------
   -- Define_Predicate_Function --
   -------------------------------

   procedure Define_Predicate_Function
     (Name : String;
      Evaluator : Predicate_Function)
   is
   begin
      Lith.Objects.Interfaces.Define_Function
        (Name,
         Predicate_Evaluator'(Fn => Evaluator));
   end Define_Predicate_Function;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Predicate : Predicate_Evaluator;
      Store     : in out Lith.Objects.Object_Store'Class)
     return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object
        (Predicate.Fn
           (Store.Argument (1)));
   end Evaluate;

   ------------------
   -- Evaluate_ALU --
   ------------------

   function Evaluate_ALU
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      return ALU.Apply (Store);
   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("ALU error while evaluting ("
            & Store.Show (Store.Argument (1))
            & " "
            & Store.Show (Store.Argument (2))
            & " "
            & Store.Show (Store.Argument (3))
            & ")");
         raise;
   end Evaluate_ALU;

   ------------------
   -- Evaluate_Car --
   ------------------

   function Evaluate_Car
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Xs : constant Lith.Objects.Object := Store.Argument (1);
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
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Xs : constant Lith.Objects.Object := Store.Argument (1);
   begin
      if Lith.Objects.Is_Atom (Xs) then
         raise Constraint_Error with
           "cdr: not a list: " & Store.Show (Xs);
      else
         return Store.Cdr (Xs);
      end if;
   end Evaluate_Cdr;

   ------------------------------
   -- Evaluate_Char_To_Integer --
   ------------------------------

   function Evaluate_Char_To_Integer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      return To_Object
        (Integer'
           (Character'Pos
                (To_Character (Store.Argument (1)))));
   end Evaluate_Char_To_Integer;

   -------------------
   -- Evaluate_Cons --
   -------------------

   function Evaluate_Cons
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Car : constant Lith.Objects.Object := Store.Argument (1);
      Cdr : constant Lith.Objects.Object := Store.Argument (1 + 1);
   begin
      return Store.Cons (Car, Cdr);
   end Evaluate_Cons;

   ----------------------------
   -- Evaluate_Current_Jiffy --
   ----------------------------

   function Evaluate_Current_Jiffy
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
      use type Ada.Calendar.Time;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Seconds : constant Duration := Now - Jiffy_Start_Time;
   begin
      return Lith.Objects.To_Object
        (Natural (Seconds * Jiffies_Per_Second));
   end Evaluate_Current_Jiffy;

   -----------------
   -- Evaluate_Eq --
   -----------------

   function Evaluate_Eq
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      if Store.Argument (1) = Store.Argument (2) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Eq;

   -----------------------------
   -- Evaluate_External_Equal --
   -----------------------------

   function Evaluate_External_Equal
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      if Is_External_Object (Store.Argument (1 + 1)) then
         declare
            X_Object : constant access External_Object_Interface'Class :=
                         Store.Get_External_Object
                           (Store.Argument (1));
            Y_Object : constant access External_Object_Interface'Class :=
                         Store.Get_External_Object
                           (Store.Argument (1 + 1));
         begin
            if X_Object.Name = Y_Object.Name then
               return To_Object (X_Object.Equal (Y_Object.all, Store));
            else
               return To_Object (False);
            end if;
         end;
      else
         return To_Object (False);
      end if;
   end Evaluate_External_Equal;

   -------------------------------
   -- Evaluate_External_Is_Type --
   -------------------------------

   function Evaluate_External_Is_Type
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      if Is_External_Object (Store.Argument (1)) then
         declare
            X_Object : constant access External_Object_Interface'Class :=
                         Store.Get_External_Object
                           (Store.Argument (1));
            Name_Sym : constant String :=
                         Lith.Objects.Symbols.Get_Name
                           (To_Symbol (Store.Argument (1 + 1)));
         begin
            return To_Object (X_Object.Name = Name_Sym);
         end;
      else
         return To_Object (False);
      end if;
   end Evaluate_External_Is_Type;

   -----------------------------
   -- Evaluate_External_Print --
   -----------------------------

   function Evaluate_External_Print
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Result : constant String :=
                 Store.Get_External_Object (Store.Argument (1)).Print (Store);
   begin
      Store.Push (Lith.Objects.String_Value);
      for Ch of Result loop
         Store.Push (Lith.Objects.To_Object (Ch));
      end loop;
      Store.Push (Lith.Objects.Nil);
      for I in 1 .. Result'Length + 1 loop
         Store.Cons;
      end loop;
      return Store.Pop;
   end Evaluate_External_Print;

   ---------------------
   -- Evaluate_Gensym --
   ---------------------

   function Evaluate_Gensym
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
   begin
      Next_Gensym_Index := Next_Gensym_Index + 1;
      return Lith.Objects.To_Object
        (Lith.Objects.Symbols.Get_Symbol
           ("#:g" & Integer'Image (-Next_Gensym_Index)));
   end Evaluate_Gensym;

   ------------------------------
   -- Evaluate_Integer_To_Char --
   ------------------------------

   function Evaluate_Integer_To_Char
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      return To_Object
        (Character'Val
                (To_Integer (Store.Argument (1))));
   end Evaluate_Integer_To_Char;

   --------------------------
   -- Evaluate_Is_External --
   --------------------------

   function Evaluate_Is_External
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Objects.To_Object
        (Lith.Objects.Is_External_Object
           (Store.Argument (1)));
   end Evaluate_Is_External;

   -------------------------
   -- Evaluate_Is_Integer --
   -------------------------

   function Evaluate_Is_Integer
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      A : constant Object := Store.Argument (1);
   begin
      if Is_Integer (A)
        or else Lith.Objects.Large_Integers.Is_Large_Integer (Store, A)
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
     (Item : Lith.Objects.Object)
      return Boolean
   is
      use type Lith.Objects.Object;
   begin
      return Item = Lith.Objects.Nil;
   end Evaluate_Is_Null;

   ----------------------
   -- Evaluate_Is_Pair --
   ----------------------

   function Evaluate_Is_Pair
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      if Is_Pair (Store.Argument (1)) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Pair;

   ----------------------
   -- Evaluate_Is_Real --
   ----------------------

   function Evaluate_Is_Real
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      return (if Lith.Objects.Real.Is_Real
              (Store, Store.Argument (1))
              then True_Value
              else False_Value);
   end Evaluate_Is_Real;

   ------------------------
   -- Evaluate_Is_Symbol --
   ------------------------

   function Evaluate_Is_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin
      if Is_Symbol (Store.Argument (1)) then
         return True_Value;
      else
         return False_Value;
      end if;
   end Evaluate_Is_Symbol;

   ---------------------------------
   -- Evaluate_Jiffies_Per_Second --
   ---------------------------------

   function Evaluate_Jiffies_Per_Second
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      pragma Unreferenced (Store);
   begin
      return Lith.Objects.To_Object (Integer (Jiffies_Per_Second));
   end Evaluate_Jiffies_Per_Second;

   -------------------
   -- Evaluate_Load --
   -------------------

   function Evaluate_Load
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Path : constant String :=
               Store.To_String (Store.Argument (1));
   begin
      if Store.Load (Path) then
         return Lith.Objects.True_Value;
      else
         return Lith.Objects.False_Value;
      end if;
   end Evaluate_Load;

   -----------------------------------------
   -- Evaluate_Profile_Finish_Cost_Centre --
   -----------------------------------------

   function Evaluate_Profile_Finish_Cost_Centre
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
   begin
      return Store.Call_Hook ("cost-centre",
                              To_Object (Get_Symbol ("finish")));
   end Evaluate_Profile_Finish_Cost_Centre;

   ----------------------------------------
   -- Evaluate_Profile_Start_Cost_Centre --
   ----------------------------------------

   function Evaluate_Profile_Start_Cost_Centre
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
   begin
      return Store.Call_Hook ("cost-centre",
                              To_Object (Get_Symbol ("start")));
   end Evaluate_Profile_Start_Cost_Centre;

   ---------------------
   -- Evaluate_Random --
   ---------------------

   function Evaluate_Random
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Max : constant Integer :=
              Lith.Objects.To_Integer (Store.Argument (1));
      Result : constant Integer :=
                 WL.Random.Random_Number (0, Max - 1);
   begin
      return Lith.Objects.To_Object (Result);
   end Evaluate_Random;

   -------------------
   -- Evaluate_Read --
   -------------------

   function Evaluate_Read
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      return Lith.Parser.Read_Port (Store, Store.Argument (1));
   end Evaluate_Read;

   ----------------------
   -- Evaluate_Set_Car --
   ----------------------

   function Evaluate_Set_Car
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Store.Set_Car (Store.Argument (1),
                     Store.Argument (1 + 1));
      return Store.Argument (1);
   end Evaluate_Set_Car;

   ----------------------
   -- Evaluate_Set_Cdr --
   ----------------------

   function Evaluate_Set_Cdr
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
   begin
      Store.Set_Cdr (Store.Argument (1),
                     Store.Argument (1 + 1));
      return Store.Argument (1);
   end Evaluate_Set_Cdr;

   -------------------------------
   -- Evaluate_String_To_Symbol --
   -------------------------------

   function Evaluate_String_To_Symbol
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      Name : constant String :=
               Store.To_String (Store.Argument (1));
   begin
      return Lith.Objects.To_Object
        (Lith.Objects.Symbols.Get_Symbol (Name));
   end Evaluate_String_To_Symbol;

   -------------------------------
   -- Evaluate_Symbol_To_String --
   -------------------------------

   function Evaluate_Symbol_To_String
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Text : constant String :=
               Store.Show (Store.Argument (1));
      Result : Object := Nil;
   begin
      for Ch of reverse Text loop
         Result := Store.Cons (To_Object (Ch), Result);
      end loop;
      Result := Store.Cons (String_Value, Result);
      return Result;
   end Evaluate_Symbol_To_String;

   -------------------------
   -- Evaluate_Write_Char --
   -------------------------

   function Evaluate_Write_Char
     (Store       : in out Lith.Objects.Object_Store'Class)
      return Lith.Objects.Object
   is
      use Ada.Text_IO;
      Char : constant Character :=
               Lith.Objects.To_Character (Store.Argument (1));
      Code : constant Natural :=
               Character'Pos (Char);
   begin
      if Code = 10 then
         Ada.Text_IO.New_Line;
      elsif Code in 256 .. 65535 then
         Put (Character'Val (Code / 256));
         Put (Character'Val (Code mod 256));
      else
         Put (Char);
      end if;
      return Lith.Objects.No_Value;
   end Evaluate_Write_Char;

end Lith.Primitives;

with Ada.Characters.Conversions;

with Lith.Objects.Numbers;

with Lith.Objects.Symbol_Maps;
with Lith.Objects.Symbols;

package body Lith.Primitives.ALU is

   use Lith.Objects;

   type Stack_Operator_Accumulator is access
     procedure (Store : in out Object_Store'Class);

   type Operator_Record is
      record
         Identity          : Integer;
         Exact_Unit_Proc   : Stack_Operator_Accumulator;
         Inexact_Unit_Proc : Stack_Operator_Accumulator;
         Exact_Proc        : Stack_Operator_Accumulator;
         Inexact_Proc      : Stack_Operator_Accumulator;
      end record;

   package Operator_Maps is
     new Lith.Objects.Symbol_Maps
       (Operator_Record);

   Ops : Operator_Maps.Map;

   procedure Operator (Symbol            : Wide_Wide_String;
                       Identity          : Integer;
                       Exact_Unit_Proc   : Stack_Operator_Accumulator;
                       Inexact_Unit_Proc : Stack_Operator_Accumulator;
                       Exact_Proc        : Stack_Operator_Accumulator;
                       Inexact_Proc      : Stack_Operator_Accumulator);

   procedure Divide_Quotient
     (Store : in out Object_Store'Class)
     with Unreferenced;

   procedure Divide_Mod
     (Store : in out Object_Store'Class)
     with Unreferenced;

   procedure Stack_Identity
     (Store : in out Object_Store'Class)
   is null;

   type Allowed_Compares is
     array (Lith.Objects.Numbers.Compare) of Boolean;

   Compare_EQ : constant Allowed_Compares := (False, True, False);
   Compare_LE : constant Allowed_Compares := (True, True, False);
   Compare_LT : constant Allowed_Compares := (True, False, False);
   Compare_GE : constant Allowed_Compares := (False, True, True);
   Compare_GT : constant Allowed_Compares := (False, False, True);

   procedure Exact_Compare
     (Store   : in out Object_Store'Class;
      Allowed : Allowed_Compares);

   procedure Inexact_Compare
     (Store : in out Object_Store'Class;
      Allowed : Allowed_Compares);

   procedure Exact_Stack_EQ
     (Store : in out Object_Store'Class);

   procedure Exact_Stack_LT
     (Store : in out Object_Store'Class);

   procedure Exact_Stack_LE
     (Store : in out Object_Store'Class);

   procedure Exact_Stack_GT
     (Store : in out Object_Store'Class);

   procedure Exact_Stack_GE
     (Store : in out Object_Store'Class);

   procedure Inexact_Stack_EQ
     (Store : in out Object_Store'Class);

   procedure Inexact_Stack_LT
     (Store : in out Object_Store'Class);

   procedure Inexact_Stack_LE
     (Store : in out Object_Store'Class);

   procedure Inexact_Stack_GT
     (Store : in out Object_Store'Class);

   procedure Inexact_Stack_GE
     (Store : in out Object_Store'Class);

   -------------------
   -- Add_Operators --
   -------------------

   procedure Add_Operators is
   begin
      Operator ("+", 0, Stack_Identity'Access, Stack_Identity'Access,
                Lith.Objects.Numbers.Exact_Add'Access,
                Lith.Objects.Numbers.Inexact_Add'Access);
      Operator ("-", 0,
                Lith.Objects.Numbers.Exact_Negate'Access,
                Lith.Objects.Numbers.Inexact_Negate'Access,
                Lith.Objects.Numbers.Exact_Subtract'Access,
                Lith.Objects.Numbers.Inexact_Subtract'Access);

      Operator ("*", 0, Stack_Identity'Access, Stack_Identity'Access,
                Lith.Objects.Numbers.Exact_Multiply'Access,
                Lith.Objects.Numbers.Inexact_Multiply'Access);

      Operator ("floor/", 0, null, null,
                Lith.Objects.Numbers.Exact_Divide'Access,
                null);

      Operator ("<=", 0, Stack_Identity'Access, Stack_Identity'Access,
                Exact_Stack_LE'Access, Inexact_Stack_LE'Access);
      Operator (">=", 0, Stack_Identity'Access, Stack_Identity'Access,
                Exact_Stack_GE'Access, Inexact_Stack_GE'Access);
      Operator ("<", 0, Stack_Identity'Access, Stack_Identity'Access,
                Exact_Stack_LT'Access, Inexact_Stack_LT'Access);
      Operator (">", 0, Stack_Identity'Access, Stack_Identity'Access,
                Exact_Stack_GT'Access, Inexact_Stack_GT'Access);
      Operator ("=", 0, Stack_Identity'Access, Stack_Identity'Access,
                Exact_Stack_EQ'Access, Inexact_Stack_EQ'Access);

   end Add_Operators;

   -----------
   -- Apply --
   -----------

   function Apply
     (Store : in out Lith.Objects.Object_Store'Class;
      Op    : Lith.Objects.Symbol_Type;
      Args  : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object
   is
      use Lith.Objects.Numbers;
   begin
      if Ops.Contains (Op) then
         declare
            Rec : Operator_Record renames Ops.Element (Op);
         begin
            if Args'Length = 0 then
               return To_Object (Rec.Identity);
            elsif Args'Length = 1 then
               Store.Push (Args (Args'First));
               if Is_Exact_Number (Store, Store.Top) then
                  Rec.Exact_Unit_Proc (Store);
               else
                  Rec.Inexact_Unit_Proc (Store);
               end if;
               return Store.Pop;
            else
               for Arg of reverse Args loop
                  Store.Push (Arg);
               end loop;

               for I in 1 .. Args'Length - 1 loop
                  if Store.Top = False_Value then
                     Store.Drop (Args'Length - I);
                     Store.Push (False_Value);
                     exit;
                  elsif Is_Exact_Number (Store, Store.Top (1))
                    and then Is_Exact_Number (Store, Store.Top (2))
                  then
                     Rec.Exact_Proc (Store);
                  else
                     Ensure_Inexact (Store);
                     Store.Swap;
                     Ensure_Inexact (Store);
                     Store.Swap;
                     Rec.Inexact_Proc (Store);
                  end if;
               end loop;

               return Store.Pop;
            end if;
         end;
      else
         raise Constraint_Error with
           "no such ALU function: "
           & Ada.Characters.Conversions.To_String
           (Lith.Objects.Symbols.Get_Name (Op));
      end if;
   end Apply;

   ----------------
   -- Divide_Mod --
   ----------------

   procedure Divide_Mod
     (Store : in out Object_Store'Class)
   is
   begin
      Lith.Objects.Numbers.Exact_Divide (Store);
      Store.Push (Store.Pop, Lith.Objects.Secondary);
      Store.Drop;
      Store.Push (Store.Pop (Lith.Objects.Secondary));
   end Divide_Mod;

   ---------------------
   -- Divide_Quotient --
   ---------------------

   procedure Divide_Quotient
     (Store : in out Object_Store'Class)
   is
   begin
      Lith.Objects.Numbers.Exact_Divide (Store);
      Store.Drop;
   end Divide_Quotient;

   -------------------
   -- Exact_Compare --
   -------------------

   procedure Exact_Compare
     (Store : in out Object_Store'Class;
      Allowed : Allowed_Compares)
   is
      use Lith.Objects.Numbers;
      Result : constant Compare :=
                 Exact_Compare (Store);
   begin
      if not Allowed (Result) then
         Store.Drop;
         Store.Push (False_Value);
      end if;
   end Exact_Compare;

   --------------------
   -- Exact_Stack_EQ --
   --------------------

   procedure Exact_Stack_EQ
     (Store : in out Object_Store'Class)
   is
   begin
      Exact_Compare (Store, Compare_EQ);
   end Exact_Stack_EQ;

   --------------------
   -- Exact_Stack_GE --
   --------------------

   procedure Exact_Stack_GE
     (Store : in out Object_Store'Class)
   is
   begin
      Exact_Compare (Store, Compare_GE);
   end Exact_Stack_GE;

   --------------------
   -- Exact_Stack_GT --
   --------------------

   procedure Exact_Stack_GT
     (Store : in out Object_Store'Class)
   is
   begin
      Exact_Compare (Store, Compare_GT);
   end Exact_Stack_GT;

   --------------------
   -- Exact_Stack_LE --
   --------------------

   procedure Exact_Stack_LE
     (Store : in out Object_Store'Class)
   is
   begin
      Exact_Compare (Store, Compare_LE);
   end Exact_Stack_LE;

   --------------------
   -- Exact_Stack_LT --
   --------------------

   procedure Exact_Stack_LT
     (Store : in out Object_Store'Class)
   is
   begin
      Exact_Compare (Store, Compare_LT);
   end Exact_Stack_LT;

   ---------------------
   -- Inexact_Compare --
   ---------------------

   procedure Inexact_Compare
     (Store : in out Object_Store'Class;
      Allowed : Allowed_Compares)
   is
      use Lith.Objects.Numbers;
      Result : constant Compare :=
                 Inexact_Compare (Store);
   begin
      if not Allowed (Result) then
         Store.Drop;
         Store.Push (False_Value);
      end if;
   end Inexact_Compare;

   ----------------------
   -- Inexact_Stack_EQ --
   ----------------------

   procedure Inexact_Stack_EQ
     (Store : in out Object_Store'Class)
   is
   begin
      Inexact_Compare (Store, Compare_EQ);
   end Inexact_Stack_EQ;

   ----------------------
   -- Inexact_Stack_GE --
   ----------------------

   procedure Inexact_Stack_GE
     (Store : in out Object_Store'Class)
   is
   begin
      Inexact_Compare (Store, Compare_GE);
   end Inexact_Stack_GE;

   ----------------------
   -- Inexact_Stack_GT --
   ----------------------

   procedure Inexact_Stack_GT
     (Store : in out Object_Store'Class)
   is
   begin
      Inexact_Compare (Store, Compare_GT);
   end Inexact_Stack_GT;

   ----------------------
   -- Inexact_Stack_LE --
   ----------------------

   procedure Inexact_Stack_LE
     (Store : in out Object_Store'Class)
   is
   begin
      Inexact_Compare (Store, Compare_LE);
   end Inexact_Stack_LE;

   ----------------------
   -- Inexact_Stack_LT --
   ----------------------

   procedure Inexact_Stack_LT
     (Store : in out Object_Store'Class)
   is
   begin
      Inexact_Compare (Store, Compare_LT);
   end Inexact_Stack_LT;

   --------------
   -- Operator --
   --------------

   procedure Operator (Symbol            : Wide_Wide_String;
                       Identity          : Integer;
                       Exact_Unit_Proc   : Stack_Operator_Accumulator;
                       Inexact_Unit_Proc : Stack_Operator_Accumulator;
                       Exact_Proc        : Stack_Operator_Accumulator;
                       Inexact_Proc      : Stack_Operator_Accumulator)
   is
   begin
      Ops.Insert (Lith.Objects.Symbols.Get_Symbol (Symbol),
                  (Identity, Exact_Unit_Proc, Inexact_Unit_Proc,
                   Exact_Proc, Inexact_Proc));
   end Operator;

end Lith.Primitives.ALU;

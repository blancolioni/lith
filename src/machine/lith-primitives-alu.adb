with Ada.Characters.Conversions;

with Lith.Objects.Numbers.Exact;
with Lith.Objects.Symbol_Maps;
with Lith.Symbols;

package body Lith.Primitives.ALU is

   use Lith.Objects;

   type Unit_Operator_Evaluator is access
     function (X : Lith.Objects.Object)
               return Lith.Objects.Object;

   type Operator_Accumulator is access
     function (X, Y : Lith.Objects.Object)
               return Lith.Objects.Object;

   type Stack_Operator_Accumulator is access
     procedure (Store : in out Object_Store'Class);

   type Operator_Record is
      record
         Identity     : Integer;
         Unit_Fn      : Unit_Operator_Evaluator;
         Acc_Fn       : Operator_Accumulator;
         Unit_Proc    : Stack_Operator_Accumulator;
         Exact_Proc   : Stack_Operator_Accumulator;
         Inexact_Proc : Stack_Operator_Accumulator;
      end record;

   package Operator_Maps is
     new Lith.Objects.Symbol_Maps
       (Operator_Record);

   Ops : Operator_Maps.Map;

   procedure Operator (Symbol : Wide_Wide_String;
                       Identity : Integer;
                       Unit_Fn  : Unit_Operator_Evaluator;
                       Acc_Fn   : Operator_Accumulator);

   procedure Operator (Symbol       : Wide_Wide_String;
                       Identity     : Integer;
                       Unit_Fn      : Stack_Operator_Accumulator;
                       Exact_Proc   : Stack_Operator_Accumulator;
                       Inexact_Proc : Stack_Operator_Accumulator);

   function Identity_Fn (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X);

   function Acc_Fn_Leq (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is ((if X = Lith.Symbols.False_Atom
        then X
        elsif To_Integer (X) <= To_Integer (Y)
        then Y
        else Lith.Symbols.False_Atom));

   function Acc_Fn_Geq (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is ((if X = Lith.Symbols.False_Atom
        then X
        elsif To_Integer (X) >= To_Integer (Y)
        then Y
        else Lith.Symbols.False_Atom));

   function Acc_Fn_Lt (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is ((if X = Lith.Symbols.False_Atom
        then X
        elsif To_Integer (X) < To_Integer (Y)
        then Y
        else Lith.Symbols.False_Atom));

   function Acc_Fn_Gt (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is ((if X = Lith.Symbols.False_Atom
        then X
        elsif To_Integer (X) > To_Integer (Y)
        then Y
        else Lith.Symbols.False_Atom));

   function Acc_Fn_Eq (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is ((if X = Lith.Symbols.False_Atom
        then X
        elsif To_Integer (X) = To_Integer (Y)
        then Y
        else Lith.Symbols.False_Atom));

   procedure Divide_Quotient
     (Store : in out Object_Store'Class)
     with Unreferenced;

   procedure Divide_Mod
     (Store : in out Object_Store'Class)
     with Unreferenced;

   -------------------
   -- Add_Operators --
   -------------------

   procedure Add_Operators is
   begin
      Operator ("+", 0, null,
                Lith.Objects.Numbers.Exact.Add'Access, null);
      Operator ("-", 0,
                Lith.Objects.Numbers.Exact.Negate'Access,
                Lith.Objects.Numbers.Exact.Subtract'Access,
                null);

      Operator ("*", 0, null,
                Lith.Objects.Numbers.Exact.Multiply'Access,
                null);
      Operator ("floor/", 0, null,
                Lith.Objects.Numbers.Exact.Divide'Access,
                null);
      Operator ("<=", 0, Identity_Fn'Access, Acc_Fn_Leq'Access);
      Operator (">=", 0, Identity_Fn'Access, Acc_Fn_Geq'Access);
      Operator ("<", 0, Identity_Fn'Access, Acc_Fn_Lt'Access);
      Operator (">", 0, Identity_Fn'Access, Acc_Fn_Gt'Access);
      Operator ("=", 0, Identity_Fn'Access, Acc_Fn_Eq'Access);
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
   begin
      if Ops.Contains (Op) then
         declare
            Rec : Operator_Record renames Ops.Element (Op);
         begin
            if Args'Length = 0 then
               return To_Object (Rec.Identity);
            elsif Args'Length = 1 then
               if Rec.Unit_Fn /= null then
                  return Rec.Unit_Fn (Args (Args'First));
               else
                  Store.Push (Args (Args'First));
                  Rec.Unit_Proc (Store);
                  return Store.Pop;
               end if;
            elsif Rec.Acc_Fn /= null then
               declare
                  Acc : Object := Args (Args'First);
               begin
                  for I in Args'First + 1 .. Args'Last loop
                     Acc := Rec.Acc_Fn (Acc, Args (I));
                  end loop;
                  return Acc;
               end;
            else
               for Arg of reverse Args loop
                  Store.Push (Arg);
               end loop;

               for I in 1 .. Args'Length - 1 loop
                  declare
                     use Lith.Objects.Numbers.Exact;
                  begin
                     if Is_Exact_Number (Store, Store.Top (1))
                       and then Is_Exact_Number (Store, Store.Top (2))
                     then
                        Rec.Exact_Proc (Store);
                     else
                        Rec.Inexact_Proc (Store);
                     end if;
                  end;
               end loop;

               return Store.Pop;
            end if;
         end;
      else
         raise Constraint_Error with
           "no such ALU function: "
           & Ada.Characters.Conversions.To_String
           (Lith.Symbols.Get_Name (Op));
      end if;
   end Apply;

   ----------------
   -- Divide_Mod --
   ----------------

   procedure Divide_Mod
     (Store : in out Object_Store'Class)
   is
   begin
      Lith.Objects.Numbers.Exact.Divide (Store);
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
      Lith.Objects.Numbers.Exact.Divide (Store);
      Store.Drop;
   end Divide_Quotient;

   --------------
   -- Operator --
   --------------

   procedure Operator (Symbol : Wide_Wide_String;
                       Identity : Integer;
                       Unit_Fn  : Unit_Operator_Evaluator;
                       Acc_Fn   : Operator_Accumulator)
   is
   begin
      Ops.Insert (Lith.Symbols.Get_Symbol (Symbol),
                  (Identity, Unit_Fn, Acc_Fn, null, null, null));
   end Operator;

   --------------
   -- Operator --
   --------------

   procedure Operator (Symbol       : Wide_Wide_String;
                       Identity     : Integer;
                       Unit_Fn      : Stack_Operator_Accumulator;
                       Exact_Proc   : Stack_Operator_Accumulator;
                       Inexact_Proc : Stack_Operator_Accumulator)
   is
   begin
      Ops.Insert (Lith.Symbols.Get_Symbol (Symbol),
                  (Identity, null, null, Unit_Fn, Exact_Proc, Inexact_Proc));
   end Operator;

end Lith.Primitives.ALU;

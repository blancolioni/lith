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

   type Operator_Record is
      record
         Identity    : Integer;
         Unit_Fn     : Unit_Operator_Evaluator;
         Acc_Fn      : Operator_Accumulator;
      end record;

   package Operator_Maps is
     new Lith.Objects.Symbol_Maps
       (Operator_Record);

   Ops : Operator_Maps.Map;

   procedure Operator (Symbol : String;
                       Identity : Integer;
                       Unit_Fn  : Unit_Operator_Evaluator;
                       Acc_Fn   : Operator_Accumulator);

   function Identity_Fn (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X);

   function Acc_Fn_Add (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is (To_Object (To_Integer (X) + To_Integer (Y)));

   function Unit_Fn_Sub (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X);

   function Acc_Fn_Sub (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is (To_Object (To_Integer (X) - To_Integer (Y)));

   function Unit_Fn_Mul (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X);

   function Acc_Fn_Mul (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is (To_Object (To_Integer (X) * To_Integer (Y)));

   function Unit_Fn_Div (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X); ---  wrong!

   function Acc_Fn_Div (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is (To_Object (To_Integer (X) / To_Integer (Y)));

   function Unit_Fn_Mod (X : Lith.Objects.Object) return Lith.Objects.Object
   is (X);

   function Acc_Fn_Mod (X, Y : Lith.Objects.Object) return Lith.Objects.Object
   is (To_Object (To_Integer (X) mod To_Integer (Y)));

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

   -------------------
   -- Add_Operators --
   -------------------

   procedure Add_Operators is
   begin
      Operator ("+", 0, Identity_Fn'Access, Acc_Fn_Add'Access);
      Operator ("-", 0, Unit_Fn_Sub'Access, Acc_Fn_Sub'Access);
      Operator ("*", 1, Unit_Fn_Mul'Access, Acc_Fn_Mul'Access);
      Operator ("/", 1, Unit_Fn_Div'Access, Acc_Fn_Div'Access);
      Operator ("mod", 1, Unit_Fn_Mod'Access, Acc_Fn_Mod'Access);
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
     (Op   : Lith.Objects.Symbol_Type;
      Args : Lith.Objects.Array_Of_Objects)
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
               return Rec.Unit_Fn (Args (Args'First));
            else
               declare
                  Acc : Object := Args (Args'First);
               begin
                  for I in Args'First + 1 .. Args'Last loop
                     Acc := Rec.Acc_Fn (Acc, Args (I));
                  end loop;
                  return Acc;
               end;
            end if;
         end;
      else
         raise Constraint_Error with
           "no such ALU function: " & Lith.Symbols.Get_Name (Op);
      end if;
   end Apply;

   --------------
   -- Operator --
   --------------

   procedure Operator (Symbol : String;
                       Identity : Integer;
                       Unit_Fn  : Unit_Operator_Evaluator;
                       Acc_Fn   : Operator_Accumulator)
   is
   begin
      Ops.Insert (Lith.Symbols.Get_Symbol (Symbol),
                  (Identity, Unit_Fn, Acc_Fn));
   end Operator;

end Lith.Primitives.ALU;

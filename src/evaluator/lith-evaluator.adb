with Ada.Characters.Conversions;
with Ada.Text_IO;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Symbols;

package body Lith.Evaluator is

   Trace_Eval   : constant Boolean := False;
   Trace_Macros : constant Boolean := False;

   function Evaluate
     (Store        : in out Lith.Objects.Object_Store'Class;
      Expr         : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Quasiquoting : Boolean)
      return Lith.Objects.Object;

   procedure Get
     (Store  : Lith.Objects.Object_Store'Class;
      Symbol : Lith.Objects.Symbol_Type;
      Env    : Lith.Objects.Object;
      Result : out Lith.Objects.Object;
      Found  : out Boolean);

   function Eval_App
     (Store        : in out Lith.Objects.Object_Store'Class;
      Fn           : Lith.Objects.Object;
      Args         : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Quasiquoting : Boolean)
      return Lith.Objects.Object;

   function Create_Environment
     (Store        : in out Lith.Objects.Object_Store'Class;
      Formals      : Lith.Objects.Object;
      Actuals      : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Strict       : Boolean;
      Quasiquoting : Boolean)
      return Lith.Objects.Object;

   ------------------------
   -- Create_Environment --
   ------------------------

   function Create_Environment
     (Store        : in out Lith.Objects.Object_Store'Class;
      Formals      : Lith.Objects.Object;
      Actuals      : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Strict       : Boolean;
      Quasiquoting : Boolean)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Formal_It : Object  := Formals;
      Actual_It : Object  := Actuals;
      Rest      : Boolean := Is_Atom (Formals);
      Rest_Name : Object  := (if Rest then Formals else Nil);
      Acc       : Object  := Nil;
      Result    : Object  := Env;
   begin
      if Trace_Eval then
         Ada.Text_IO.Put_Line
           ("create-env: formals: " & Store.Show (Formals));
         Ada.Text_IO.Put_Line
           ("create-env: actuals: " & Store.Show (Actuals));
      end if;

      if Formals = Nil then
         return Env;
      end if;
      if not Is_Pair (Formals) and then not Is_Symbol (Formals) then
         raise Evaluation_Error with
         Ada.Characters.Conversions.To_String (Store.Show (Formal_It))
           & " cannot be used as a formal argument";
      end if;

      while Actual_It /= Nil loop
         declare
            Value : constant Object :=
                      (if Strict
                       then Evaluate (Store, Store.Car (Actual_It),
                         Env, Quasiquoting)
                       else Store.Car (Actual_It));
         begin
            if Rest then
               Store.Push (Store.Cons (Value, Acc));
               Acc := Store.Pop;
            else
               if Trace_Eval then
                  Ada.Text_IO.Put_Line
                    (Store.Show (Store.Car (Formal_It))
                     & " <-- "
                     & Store.Show (Value));
               end if;

               Store.Push (Store.Cons (Store.Car (Formal_It), Value));
               Store.Push (Store.Cons (Store.Pop, Result));
               Result := Store.Pop;
               Formal_It := Store.Cdr (Formal_It);
            end if;
            Actual_It := Store.Cdr (Actual_It);
            if not Rest and then Is_Pair (Formal_It)
              and then Store.Car (Formal_It) = Lith.Symbols.Dot
            then
               Formal_It := Store.Cdr (Formal_It);
               Rest_Name := Store.Car (Formal_It);
               Rest := True;
            end if;
         end;
      end loop;

      if Rest then
         declare
            T : Object := Nil;
         begin
            while Acc /= Nil loop
               Store.Push (Acc);
               Store.Push (Store.Cons (Store.Car (Store.Top), T));
               T := Store.Pop;
               Acc := Store.Cdr (Store.Pop);
            end loop;
            Store.Push (Store.Cons (Rest_Name, T));
            Store.Push (Store.Cons (Store.Pop, Result));
            Result := Store.Pop;
            if Trace_Eval then
               Ada.Text_IO.Put_Line
                 (Store.Show (Rest_Name)
                  & " <-- "
                  & Store.Show (T));
            end if;
         end;
      end if;

      return Result;

   end Create_Environment;

   --------------
   -- Eval_App --
   --------------

   function Eval_App
     (Store        : in out Lith.Objects.Object_Store'Class;
      Fn           : Lith.Objects.Object;
      Args         : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Quasiquoting : Boolean)
      return Lith.Objects.Object
   is
      use Lith.Objects;
   begin

      if Trace_Eval then
         Ada.Text_IO.Put_Line
           ("apply: [" & Store.Show (Fn) & "] " & Store.Show (Args));
      end if;
      if Is_Integer (Fn) then
         raise Evaluation_Error with
           "Cannot apply integer:" & Integer'Image (To_Integer (Fn));
      elsif Is_Symbol (Fn) then
         if Quasiquoting then
            if Fn = Lith.Symbols.Unquote then
               return Evaluate (Store, Store.Car (Args), Env, False);
            else
               declare
                  Eval_Args : Object :=
                                To_Object_Array (Store, Args);
                  Result    : Object := Nil;
               begin
                  for I in Eval_Args'Range loop
                     Eval_Args (I) :=
                       Evaluate (Store, Eval_Args (I), Env, True);
                  end loop;
                  for I in reverse Eval_Args'Range loop
                     Result := Store.Cons (Eval_Args (I), Result);
                  end loop;
                  Result := Store.Cons (Fn, Result);
                  return Result;
               end;
            end if;
         elsif Fn = Lith.Symbols.Quote then
            return Store.Car (Args);
         elsif Fn = Lith.Symbols.Quasiquote then
            return Evaluate (Store, Store.Car (Args), Env, True);
         elsif Fn = Lith.Symbols.Lambda
           or else Fn = Lith.Symbols.Macro
         then
            return Store.Cons (Fn, Args);
         elsif Fn = Lith.Symbols.String_Atom then
            return Store.Cons (Fn, Args);
         else
            declare
               Value : Object;
               Found : Boolean;
            begin
               Get (Store, To_Symbol (Fn), Env, Value, Found);
               if Found then
                  return Eval_App (Store, Value, Args, Env, Quasiquoting);
               else
--                  return Store.Cons (Fn, Args);
                  raise Evaluation_Error with
                    "undefined function: "
                    & Ada.Characters.Conversions.To_String (Store.Show (Fn));
               end if;
            end;
         end if;
      elsif Is_Function (Fn) then
         return Lith.Objects.Interfaces.Evaluate
           (Store       => Store,
            Fn          => To_Function (Fn),
            Arguments   => To_Object_Array (Store, Args),
            Environment => Env);
      else
         if Is_Pair (Fn) then
            declare
               Inner_Fn : constant Object :=
                            Store.Car (Fn);
            begin
               if not Quasiquoting
                 and then (Inner_Fn = Lith.Symbols.Lambda
                           or else Inner_Fn = Lith.Symbols.Macro)
               then
                  declare
                     Strict : constant Boolean :=
                                Inner_Fn = Lith.Symbols.Lambda;
                     Formals : constant Object :=
                                 Store.Car (Store.Cdr (Fn));
                     Expr    : constant Object :=
                                 Store.Car (Store.Cdr (Store.Cdr (Fn)));
                     Lambda_Env : constant Object :=
                                    Create_Environment
                                      (Store    => Store,
                                       Formals  => Formals,
                                       Actuals  => Args,
                                       Env      => Env,
                                       Strict   => Strict,
                                       Quasiquoting => Quasiquoting);
                     Result : constant Object :=
                                    Evaluate (Store, Expr, Lambda_Env);
                  begin
                     if Strict then
                        return Result;
                     else
                        if Trace_Macros then
                           Ada.Text_IO.Put_Line
                             ("expansion: "
                              & Store.Show (Result));
                        end if;
                        return Evaluate (Store, Result, Env);
                     end if;
                  end;
               end if;
            end;
         end if;

         return Eval_App (Store, Evaluate (Store, Fn, Env), Args, Env,
                         Quasiquoting);
      end if;

   end Eval_App;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Store : in out Lith.Objects.Object_Store'Class;
      Expr  : Lith.Objects.Object;
      Env   : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      State : Object := Store.Cons (Expr, Env);
      Result : Object;
   begin
      Store.Push (State);
      Result := Evaluate (Store, Expr, Env, False);
      State := Store.Pop;
      return Result;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Store        : in out Lith.Objects.Object_Store'Class;
      Expr         : Lith.Objects.Object;
      Env          : Lith.Objects.Object;
      Quasiquoting : Boolean)
      return Lith.Objects.Object
   is
      use Lith.Objects;
      Result : Object;
   begin

      if Trace_Eval then
         Ada.Text_IO.Put_Line
           ("Eval: "
            & (if Quasiquoting then "[qq] " else "")
            & Store.Show (Expr));
         Ada.Text_IO.Put_Line ("  in env: " & Store.Show (Env));
      end if;

      if Expr = Nil then
         Result := Expr;
      elsif Is_Integer (Expr) then
         Result := Expr;
      elsif Is_Symbol (Expr) then
         if Quasiquoting then
            Result := Expr;
         elsif Expr = Lith.Symbols.Quote then
            Result := Expr;
         else
            declare
               Value : Lith.Objects.Object;
               Found : Boolean;
            begin
               Get (Store, Lith.Objects.To_Symbol (Expr), Env, Value, Found);
               if Found then
                  Result := Value; --  Evaluate (Store, Value, Env, False);
               else
                  raise Evaluation_Error with
                    "undefined: "
                    & Ada.Characters.Conversions.To_String
                    (Store.Show (Expr));
               end if;
            end;
         end if;
      elsif Is_Function (Expr) then
         declare
            Args : Object (1 .. 0);
         begin
            Result :=
              Lith.Objects.Interfaces.Evaluate
                (Store       => Store,
                 Fn          => To_Function (Expr),
                 Arguments   => Args,
                 Environment => Env);
         end;
      else
         Result :=
           Eval_App
             (Store,
              Store.Car (Expr),
              Store.Cdr (Expr),
              Env,
              Quasiquoting);
      end if;

      if Trace_Eval then
         Ada.Text_IO.Put_Line
           ("Result: "
            & Store.Show (Result));
      end if;

      return Result;

   end Evaluate;

   ---------
   -- Get --
   ---------

   procedure Get
     (Store  : Lith.Objects.Object_Store'Class;
      Symbol : Lith.Objects.Symbol_Type;
      Env    : Lith.Objects.Object;
      Result : out Lith.Objects.Object;
      Found  : out Boolean)
   is
      use Lith.Objects;
      It : Object := Env;
   begin
      while It /= Nil loop
         declare
            Item : constant Object := Store.Car (It);
         begin
            if To_Symbol (Store.Car (Item)) = Symbol then
               Result := Store.Cdr (Item);
               Found := True;
               return;
            end if;
         end;
         It := Store.Cdr (It);
      end loop;

      Lith.Environment.Get (Symbol, Result, Found);
   end Get;

end Lith.Evaluator;

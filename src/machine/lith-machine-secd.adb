with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_IO;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Symbols;

package body Lith.Machine.SECD is

   Trace_Eval : constant Boolean := False;

   procedure Create_Environment
     (Machine      : in out Root_Lith_Machine'Class;
      Formals      : Lith.Objects.Object;
      Actuals      : Lith.Objects.Array_Of_Objects);

   procedure Get
     (Machine : Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean);

   procedure Get
     (Machine : Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean;
      Global  : out Boolean);

   function Length
     (Machine : Root_Lith_Machine'Class;
      Xs      : Lith.Objects.Object)
     return Natural;

   function Is_Macro
     (Machine : Root_Lith_Machine'Class;
      F       : Lith.Objects.Object)
      return Boolean;
   --  returns True if F ultimately (or immediately) refers to a macro
   --  in the current machine environment

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment
     (Machine      : in out Root_Lith_Machine'Class;
      Formals      : Lith.Objects.Object;
      Actuals      : Lith.Objects.Array_Of_Objects)
   is
      use Lith.Objects;
      Formal_It : Object  := Formals;
      Rest      : Boolean := Is_Atom (Formals);
      Rest_Name : Object  := (if Rest then Formals else Nil);
      Acc       : Object  := Nil;
      Result    : Object  := Nil;
   begin
      if Formals = Nil then
         return;
      end if;
      if not Is_Pair (Formals) and then not Is_Symbol (Formals) then
         raise Evaluation_Error with
         Ada.Characters.Conversions.To_String
           (Machine.Show (Formal_It))
           & " cannot be used as a formal argument";
      end if;

      for Actual of Actuals loop
         if Rest then
            Machine.Push (Machine.Cons (Actual, Acc));
            Acc := Machine.Pop;
         else
            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Machine.Show (Machine.Car (Formal_It))
                  & " <-- "
                  & Machine.Show (Actual));
            end if;

            Machine.Push (Machine.Cons (Machine.Car (Formal_It), Actual));
            Machine.Push (Machine.Cons (Machine.Pop, Result));
            Result := Machine.Pop;
            Formal_It := Machine.Cdr (Formal_It);
         end if;

         if not Rest and then Formal_It /= Nil and then
           not Is_Pair (Formal_It)
         then
            Rest_Name := Formal_It;
            Rest := True;
         end if;
      end loop;

      if Rest then
         declare
            T : Object := Nil;
         begin
            while Acc /= Nil loop
               Machine.Push (Acc);
               Machine.Push (Machine.Cons (Machine.Car (Machine.Top), T));
               T := Machine.Pop;
               Acc := Machine.Cdr (Machine.Pop);
            end loop;
            Machine.Push (Machine.Cons (Rest_Name, T));
            Machine.Push (Machine.Cons (Machine.Pop, Result));
            Result := Machine.Pop;
            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Machine.Show (Rest_Name)
                  & " <-- "
                  & Machine.Show (T));
            end if;
         end;
      end if;

      Machine.Environment := Machine.Cons (Result, Machine.Environment);

   end Create_Environment;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Machine : in out Root_Lith_Machine'Class)
   is
      use Lith.Objects;
      use Lith.Symbols;

      procedure Push_Control (C : Object);
      function Pop_Control return Object with Unreferenced;
      procedure Save_State;
      procedure Restore_State;

      -----------------
      -- Pop_Control --
      -----------------

      function Pop_Control return Object is
         Result : constant Object := Machine.Car (Machine.Control);
      begin
         Machine.Control := Machine.Cdr (Machine.Control);
         return Result;
      end Pop_Control;

      ------------------
      -- Push_Control --
      ------------------

      procedure Push_Control (C : Object) is
      begin
         Machine.Control := Machine.Cons (C, Machine.Control);
      end Push_Control;

      -------------------
      -- Restore_State --
      -------------------

      procedure Restore_State is
      begin
         Machine.Stack := Machine.Car (Machine.Dump);
         Machine.Dump := Machine.Cdr (Machine.Dump);
         Machine.Environment := Machine.Car (Machine.Dump);
         Machine.Dump := Machine.Cdr (Machine.Dump);
         Machine.Control := Machine.Car (Machine.Dump);
         Machine.Dump := Machine.Cdr (Machine.Dump);
      end Restore_State;

      ----------------
      -- Save_State --
      ----------------

      procedure Save_State is
      begin
         if Trace_Eval then
            Ada.Wide_Wide_Text_IO.Put_Line ("saving context ...");
            Machine.Report_State;
         end if;
         Machine.Dump := Machine.Cons (Machine.Control, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Environment, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Stack, Machine.Dump);
      end Save_State;

   begin
      while Machine.Control /= Nil loop
         declare
            C : constant Lith.Objects.Object :=
                  Machine.Car (Machine.Control);
            Cs : constant Lith.Objects.Object :=
                   Machine.Cdr (Machine.Control);
            C_Updated : Boolean := False;
         begin

            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("Eval: " & Machine.Show (C));
               Machine.Report_State;
            end if;

            if C = Nil then
               raise Evaluation_Error with
                 "attempted to evaluate nil";
            elsif C = Lith.Symbols.False_Atom
              or else C = Lith.Symbols.True_Atom
            then
               Machine.Push (C);
            elsif Is_Integer (C) then
               Machine.Push (C);
            elsif Is_Character (C) then
               Machine.Push (C);
            elsif Is_Symbol (C) then
               if C = Choice_Atom then
                  declare
                     Cond : constant Object := Machine.Pop;
                        T, F : Object;
                  begin
                     T := Machine.Pop;
                     F := Machine.Pop;
                     if Cond = False_Atom then
                        Machine.Control :=
                          Machine.Cons (F, Cs);
                     else
                        Machine.Control :=
                             Machine.Cons (T, Cs);
                     end if;
                     C_Updated := True;
                  end;
               elsif C = Lith.Symbols.Stack_To_Control then
                  Machine.Control := Cs;
                  Push_Control (Machine.Pop);
                  C_Updated := True;
               elsif C = Lith_Set_Atom then
                  declare
                     Value : constant Object := Machine.Pop;
                     Name  : constant Object := Machine.Pop;
                     Old_Value : Object;
                     Found     : Boolean;
                     Global    : Boolean;
                  begin
                     Get (Machine, To_Symbol (Name), Old_Value,
                          Found, Global);

                     if (not Found and then Machine.Environment = Nil)
                       or else (Found and then Global)
                     then
                        if Found then
                           Lith.Environment.Replace
                             (To_Symbol (Name), Value);
                        else
                           Lith.Environment.Define
                             (To_Symbol (Name), Value);
                        end if;
                     else
                        Machine.Push (Name);
                        Machine.Push (Value);
                        Machine.Cons;
                        Machine.Environment :=
                          Machine.Cons (Machine.Pop, Machine.Environment);
                     end if;
                     Machine.Push (Name);
                  end;
               else
                  declare
                     Value : Lith.Objects.Object;
                     Found : Boolean;
                  begin
                     Get (Machine, To_Symbol (C), Value, Found);
                     if Found then
                        Machine.Push (Value);
                     else
                        raise Evaluation_Error with
                          "undefined: "
                          & Ada.Characters.Conversions.To_String
                          (Get_Name (To_Symbol (C)));
                     end if;
                  end;
               end if;
            elsif Is_Function (C) then
               Machine.Push (C);
            elsif Is_Apply (C) then
               declare
                  F : constant Object := Machine.Pop;
                  Arguments : Array_Of_Objects (1 .. Argument_Count (C));
               begin

                  Machine.Control := Cs;
                  C_Updated := True;

                  if Is_Pair (F) and then Machine.Car (F) = Macro then
                     for I in reverse Arguments'Range loop
                        Arguments (I) := Machine.Pop;
                     end loop;
                  else
                     for I in Arguments'Range loop
                        Arguments (I) := Machine.Pop;
                     end loop;
                  end if;

                  if Is_Function (F) then
                     Machine.Push
                       (Lith.Objects.Interfaces.Evaluate
                          (Machine, To_Function (F),
                           Arguments, Machine.Environment));
                  elsif Is_Pair (F)
                    and then (Machine.Car (F) = Lambda
                              or else Machine.Car (F) = Macro)
                  then
                     if Machine.Car (F) = Macro then
                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("macro: pushing post-macro");
                        end if;
                        Push_Control (Lith.Symbols.Stack_To_Control);
                     end if;
                     Save_State;
                     Machine.Stack := Nil;
                     Machine.Control :=
                       Machine.Cdr
                         (Machine.Cdr (F));
                     Create_Environment
                       (Machine      => Machine,
                        Formals      => Machine.Cadr (F),
                        Actuals      => Arguments);
                  else
                     raise Evaluation_Error with
                       "bad application: "
                       & Ada.Characters.Conversions.To_String
                       (Machine.Show (F));
                  end if;
               end;
            else
               --  a pair
               declare
                  F : constant Object := Machine.Car (C);
                  Args : constant Object := Machine.Cdr (C);
               begin
                  if F = Quote then
                     if Machine.Cdr (Args) /= Nil then
                        raise Evaluation_Error with
                          "quote: too many arguments";
                     end if;
                     Machine.Push (Machine.Car (Args));
                  elsif F = If_Atom then
                     Machine.Control :=
                       Machine.Cons (Choice_Atom, Cs);
                     Machine.Control :=
                       Machine.Cons (Machine.Car (Args),
                                     Machine.Control);
                     Machine.Push
                       (Machine.Car (Machine.Cdr (Machine.Cdr (Args))));
                     Machine.Push (Machine.Cadr (Args));
                     C_Updated := True;
                  elsif F = Set_Atom then
                     Machine.Control :=
                       Machine.Cons (Lith_Set_Atom, Cs);
                     Machine.Control :=
                       Machine.Cons (Machine.Cadr (Args), Machine.Control);
                     Machine.Push (Machine.Car (Args));
                     C_Updated := True;
                  elsif F = Define_Atom then
                     declare
                        Name : constant Symbol_Type :=
                                 To_Symbol (Machine.Car (Args));
                        Value : constant Object := Machine.Cadr (Args);
                     begin
                        Lith.Environment.Define (Name, Value);
                        Machine.Push (Machine.Cons (To_Object (Name), Value));
                     end;
                  elsif F = Lambda then
                     Machine.Push (C);
                  elsif F = String_Atom then
                     Machine.Push (C);
                  elsif F = Large_Integer_Atom then
                     Machine.Push (C);
                  elsif F = Begin_Atom then
                     Machine.Dump := Machine.Cons (C, Machine.Dump);
                     Machine.Control := Cs;
                     C_Updated := True;
                     declare
                        Arg_Array : constant Array_Of_Objects :=
                                      Machine.To_Object_Array (Args);
                     begin
                        for Arg of reverse Arg_Array loop
                           Push_Control (Arg);
                        end loop;
                        Machine.Dump := Machine.Cdr (Machine.Dump);
                     end;
                  else
                     Machine.Dump := Machine.Cons (C, Machine.Dump);
                     Machine.Control := Cs;
                     C_Updated := True;
                     Push_Control (Apply_Object (Length (Machine, Args)));
                     Push_Control (F);
                     declare
                        It : Object := Args;
                        Macro : constant Boolean := Is_Macro (Machine, F);
                     begin
                        while It /= Nil loop
                           if Macro then
                              Machine.Push (Machine.Car (It));
                           else
                              Push_Control (Machine.Car (It));
                           end if;
                           It := Machine.Cdr (It);
                        end loop;
                     end;
                     Machine.Dump := Machine.Cdr (Machine.Dump);
                  end if;
               end;
            end if;

            if not C_Updated then
               Machine.Control := Cs;
            end if;
         end;

         while Machine.Control = Nil and then
           Machine.Dump /= Nil
         loop
            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line ("restoring context ...");
               Machine.Report_State;
            end if;

            declare
               S : constant Object := Machine.Pop;
            begin
               Restore_State;
               Machine.Push (S);
               if Trace_Eval then
                  Ada.Wide_Wide_Text_IO.Put_Line ("new context:");
                  Machine.Report_State;
               end if;
            end;
         end loop;

      end loop;
   end Evaluate;

   ---------
   -- Get --
   ---------

   procedure Get
     (Machine : Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean)
   is
      Global : Boolean;
      pragma Unreferenced (Global);
   begin
      Get (Machine, Symbol, Result, Found, Global);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Machine : Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean;
      Global  : out Boolean)
   is
      use Lith.Objects;
      Outer : Object := Machine.Environment;
   begin
      while Outer /= Nil loop
         declare
            Inner : Object := Machine.Car (Outer);
         begin
            while Inner /= Nil loop
               declare
                  Item : constant Object := Machine.Car (Inner);
               begin
                  if To_Symbol (Machine.Car (Item)) = Symbol then
                     Result := Machine.Cdr (Item);
                     Found := True;
                     Global := False;
                     return;
                  end if;
               end;
               Inner := Machine.Cdr (Inner);
            end loop;
         end;
         Outer := Machine.Cdr (Outer);
      end loop;

      Lith.Environment.Get (Symbol, Result, Found);
      Global := Found;

   end Get;

   --------------
   -- Is_Macro --
   --------------

   function Is_Macro
     (Machine : Root_Lith_Machine'Class;
      F       : Lith.Objects.Object)
      return Boolean
   is
      use Lith.Objects;
   begin
      if Is_Pair (F)
        and then Machine.Car (F) = Lith.Symbols.Macro
      then
         return True;
      elsif Is_Symbol (F) then
         declare
            Value : Object;
            Found : Boolean;
         begin
            Get (Machine, To_Symbol (F), Value, Found);
            if Found then
               if Is_Macro (Machine, Value) then
                  return True;
               else
                  return False;
               end if;
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Is_Macro;

   ------------
   -- Length --
   ------------

   function Length
     (Machine : Root_Lith_Machine'Class;
      Xs      : Lith.Objects.Object)
      return Natural
   is
      use Lith.Objects;
      It : Object := Xs;
      Result : Natural := 0;
   begin
      while It /= Nil loop
         Result := Result + 1;
         It := Machine.Cdr (It);
      end loop;
      return Result;
   end Length;

end Lith.Machine.SECD;

with Ada.Characters.Conversions;
with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with Lith.Environment;
with Lith.Objects.Interfaces;
with Lith.Parser;
with Lith.Objects.Symbols;

with Lith.Paths;

package body Lith.Machine.SECD is

   Trace_Eval       : Boolean := False;
   Trace_Patterns   : Boolean := False;

   function Import_Libraries
     (Machine    : in out Root_Lith_Machine'Class;
      Import_Set : Lith.Objects.Object)
      return Boolean;

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

   procedure Apply_Syntax
     (Machine      : in out Root_Lith_Machine'Class;
      Call         : Lith.Objects.Object;
      Syntax_Rules : Lith.Objects.Object);

   ------------------
   -- Apply_Syntax --
   ------------------

   procedure Apply_Syntax
     (Machine      : in out Root_Lith_Machine'Class;
      Call         : Lith.Objects.Object;
      Syntax_Rules : Lith.Objects.Object)
   is
      use Lith.Objects;

      function Match (Pat : Object) return Boolean;
      procedure Apply (Env  : Object;
                       Code : Object);
      function Binding (Env  : Object;
                        Name : Object;
                        Default : Object)
                        return Object;

      -----------
      -- Apply --
      -----------

      procedure Apply (Env  : Object;
                       Code : Object)
      is
         use Lith.Objects.Symbols;
      begin
         if Is_Pair (Code) then
            if Is_Pair (Machine.Cdr (Code))
              and then Machine.Cadr (Code) = Ellipsis_Symbol
            then
               Machine.Push
                 (Binding (Env, Ellipsis_Symbol, Nil));
            else
               Apply (Env, Machine.Cdr (Code));
            end if;
            Apply (Env, Machine.Car (Code));
            Machine.Swap;
            Machine.Cons;
         elsif Is_Symbol (Code) then
            Machine.Push (Binding (Env, Code, Code));
         else
            Machine.Push (Code);
         end if;
      end Apply;

      -------------
      -- Binding --
      -------------

      function Binding (Env  : Object;
                        Name : Object;
                        Default : Object)
                        return Object
      is
         It : Object := Env;
      begin
         while It /= Nil loop
            if Machine.Caar (It) = Name then
               return Machine.Car (Machine.Cdar (It));
            end if;
            It := Machine.Cdr (It);
         end loop;
         return Default;
      end Binding;

      -----------
      -- Match --
      -----------

      function Match (Pat : Object) return Boolean is
         Pat_It  : Object := Pat;
         Call_It : Object := Call;
         Count   : Natural := 0;
      begin
         if Trace_Patterns then
            Ada.Wide_Wide_Text_IO.Put_Line
              ("Match: pattern = " & Machine.Show (Pat));
            Ada.Wide_Wide_Text_IO.Put_Line
              ("          call = " & Machine.Show (Call));
         end if;

         while Pat_It /= Nil loop
            if Machine.Car (Pat_It)
              = Lith.Objects.Symbols.Ellipsis_Symbol
            then
               Machine.Push (Lith.Objects.Symbols.Ellipsis_Symbol);
               Machine.Push (Call_It);
               Machine.Push (Nil);
               Machine.Cons;
               Machine.Cons;
               Machine.Push (Nil);
               for I in 1 .. Count + 1 loop
                  Machine.Cons;
               end loop;
               return True;
            elsif Call_It = Nil then
               exit;
            elsif Machine.Car (Pat_It)
              = Lith.Objects.Symbols.Wildcard_Symbol
            then
               Pat_It := Machine.Cdr (Pat_It);
               Call_It := Machine.Cdr (Call_It);
            else
               Machine.Push (Machine.Car (Pat_It));
               Machine.Push (Machine.Car (Call_It));
               Machine.Push (Nil);
               Machine.Cons;
               Machine.Cons;
               Count := Count + 1;
               Pat_It := Machine.Cdr (Pat_It);
               Call_It := Machine.Cdr (Call_It);
            end if;

         end loop;

         if Pat_It = Nil and then Call_It = Nil then
            Machine.Push (Nil);
            for I in 1 .. Count loop
               Machine.Cons;
            end loop;
            return True;
         else
            Machine.Drop (Count);
            return False;
         end if;
      end Match;

      Keywords : constant Object := Machine.Cadr (Syntax_Rules);
      pragma Unreferenced (Keywords);

      Pats : Object := Machine.Cddr (Syntax_Rules);
   begin
      while Pats /= Nil loop
         declare
            Pat  : constant Object := Machine.Caar (Pats);
            Code : constant Object := Machine.Car (Machine.Cdar (Pats));
         begin
            if Match (Machine.Cdr (Pat)) then
               if Trace_Patterns then
                  Ada.Wide_Wide_Text_IO.Put_Line
                    ("found match: "
                     & Machine.Show (Machine.Cdr (Pat))
                     & " --> "
                     & Machine.Show (Code));
               end if;
               Apply (Machine.Top, Code);
               Machine.Swap;
               Machine.Drop;
               if Trace_Patterns then
                  Ada.Wide_Wide_Text_IO.Put_Line
                    ("result: "
                     & Machine.Show (Machine.Top));
               end if;
               return;
            end if;
         end;
         Pats := Machine.Cdr (Pats);
      end loop;
      raise Evaluation_Error with
        "no matching pattern in syntax rules";
   end Apply_Syntax;

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment
     (Machine      : in out Root_Lith_Machine'Class;
      Formals      : Lith.Objects.Object;
      Actuals      : Lith.Objects.Array_Of_Objects)
   is
      use Lith.Objects;
      Formal_It  : Object  := Formals;
      Rest       : Boolean := Is_Atom (Formals);
      Rest_Name  : Object  := (if Rest then Formals else Nil);
      Count      : Natural := 0;
      Rest_Count : Natural := 0;
      Env        : Object := Machine.Car (Machine.Environment);

      procedure Save_Binding
        (Name  : Object;
         Value : Object);

      ------------------
      -- Save_Binding --
      ------------------

      procedure Save_Binding
        (Name  : Object;
         Value : Object)
      is
         It : Object := Env;
      begin
         --  protect against GC when we push Name
         Machine.R1 := Value;
         Machine.Push (Name);
         Machine.Push (Value);
         Machine.Cons;
         Machine.R1 := Nil;

         while It /= Nil
           and then Machine.Caar (It) /= Name
         loop
            It := Machine.Cdr (It);
         end loop;

         if It = Nil then
            Machine.Push (Env);
            Machine.Cons;
            Machine.Push (Machine.Cdr (Machine.Environment));
            Machine.Cons;
            Machine.Environment := Machine.Pop;
            Env := Machine.Car (Machine.Environment);
         else
            Machine.Set_Cdr (Machine.Car (It), Value);
         end if;
      end Save_Binding;

   begin
      if Formals = Nil then
         Machine.Environment := Machine.Cons (Nil, Machine.Environment);
         return;
      end if;

      if not Is_Pair (Formals) and then not Is_Symbol (Formals) then
         raise Evaluation_Error with
         Ada.Characters.Conversions.To_String
           (Machine.Show (Formal_It))
           & " cannot be used as a formal argument";
      end if;

      if Is_Symbol (Formals) then
         Rest := True;
         Rest_Name := Formals;
         Count := 1;
      end if;

      for Actual of Actuals loop

         if Rest then
            Machine.Push (Actual);
            Rest_Count := Rest_Count + 1;
         else

            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Machine.Show (Machine.Car (Formal_It))
                  & " <-- "
                  & Machine.Show (Actual));
            end if;

            if Formal_It = Nil then
               raise Evaluation_Error with
                 "too many arguments: "
                 & Ada.Characters.Conversions.To_String
                 (Machine.Show (Formals))
                 & " at actual: "
                 & Ada.Characters.Conversions.To_String
                 (Machine.Show (Actual));
            end if;

            Save_Binding (Machine.Car (Formal_It), Actual);

            Formal_It := Machine.Cdr (Formal_It);
            Count := Count + 1;

         end if;

         if not Rest and then Formal_It /= Nil and then
           not Is_Pair (Formal_It)
         then
            Rest_Name := Formal_It;
            Rest := True;
            Count := Count + 1;
         end if;

      end loop;

      if Rest then
         Machine.Push (Nil);
         for I in 1 .. Rest_Count loop
            Machine.Cons;
         end loop;

         Save_Binding (Rest_Name, Machine.Pop);
      end if;

      if Trace_Eval then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("create environment: "
            & Machine.Show (Machine.Environment));
      end if;

   end Create_Environment;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Machine : in out Root_Lith_Machine'Class)
   is
      use Lith.Objects;
      use Lith.Objects.Symbols;

      procedure Push_Control (C : Object);
      function Pop_Control return Object with Unreferenced;
      procedure Save_State;
      procedure Restore_State (State : Object := False_Value);
      --  Restore the state to the given state, which is either False,
      --  or desired final state from the dump stack.  Evaluate any
      --  unwinding expressions on the way.  If State is False,
      --  restore a single level of state.

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

      procedure Restore_State (State : Object := False_Value) is
      begin
         loop
            if State = Nil and then Machine.Dump = Nil then
               Machine.Stack := Nil;
               Machine.Environment := Nil;
               Machine.Control := Nil;
               Machine.Push (No_Value);
               return;
            else
               Machine.Stack := Machine.Car (Machine.Dump);
               Machine.Dump := Machine.Cdr (Machine.Dump);
               Machine.Environment := Machine.Car (Machine.Dump);
               Machine.Dump := Machine.Cdr (Machine.Dump);
               Machine.Control := Machine.Car (Machine.Dump);
               Machine.Dump := Machine.Cdr (Machine.Dump);
               exit when State = False_Value or else State = Machine.Dump;
            end if;
         end loop;
      end Restore_State;

      ----------------
      -- Save_State --
      ----------------

      procedure Save_State is
      begin
         if Trace_Eval then
            Ada.Wide_Wide_Text_IO.Put_Line ("saving context ...");
         end if;
         Machine.Dump := Machine.Cons (Machine.Control, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Environment, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Stack, Machine.Dump);
      end Save_State;

   begin

      while Machine.Control /= Nil loop
         declare
            C : Lith.Objects.Object :=
                  Machine.Car (Machine.Control);
            Cs : constant Lith.Objects.Object :=
                   Machine.Cdr (Machine.Control);
            C_Updated : Boolean := False;
            Is_Tail_Context : Boolean := False;
         begin

            Set_Context (Machine, C);

            if Machine.Profiling then
               Machine.Hit (C);
            end if;

            declare
               Trace : Object;
               Found : Boolean;
            begin
               Environment.Get
                 (Get_Symbol ("*trace-eval*"),
                  Trace, Found);

               if Found and then Trace /= False_Value then
                  Trace_Eval := True;
                  Trace_Patterns := True;
               end if;
            end;

            if Trace_Eval then
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("Eval: "
                  & Show (Machine, Machine.Current_Context)
                  & " "
                  & Machine.Show (C));
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("Env: "
                  & Machine.Show (Machine.Environment));
               --  Machine.Report_State;
            end if;

            if Is_Pair (C)
              and then Machine.Car (C) = Tail_Context
            then
               C := Machine.Cdr (C);
               Is_Tail_Context := True;
               if Trace_Eval then
                  Ada.Wide_Wide_Text_IO.Put_Line
                    ("tail-context: " & Machine.Show (C));
               end if;
            end if;

            if C = Nil then
               raise Evaluation_Error with
                 "attempted to evaluate nil";
            elsif C = True_Value or else C = False_Value then
               Machine.Push (C);
            elsif C = No_Value then
               Machine.Push (C);
            elsif Is_Integer (C) then
               Machine.Push (C);
            elsif Is_Character (C) then
               Machine.Push (C);
            elsif Is_Symbol (C) then
               Machine.Hit (C);
               if C = Choice then
                  declare
                     Cond : constant Object := Machine.Pop;
                  begin
                     if Cond = False_Value then
                        Machine.Drop;
                        Machine.Control :=
                          Machine.Cons (Machine.Pop, Cs);
                     else
                        Machine.Control :=
                          Machine.Cons (Machine.Pop, Cs);
                        Machine.Drop;
                     end if;
                     C_Updated := True;
                  end;
               elsif C = Do_Car then
                  Machine.Push (Machine.Car (Machine.Pop));
               elsif C = Do_Cdr then
                  Machine.Push (Machine.Cdr (Machine.Pop));
               elsif C = Do_Null then
                  Machine.Push (To_Object (Machine.Pop = Nil));
               elsif C = Lith.Objects.Symbols.Stack_To_Control then
                  Machine.Control := Cs;
                  Push_Control (Machine.Pop);
                  C_Updated := True;
               elsif C = Stack_Drop then
                  if Trace_Eval then
                     Ada.Wide_Wide_Text_IO.Put_Line
                       ("stack-drop: stack = "
                        & Machine.Show (Machine.Stack));
                  end if;
                  Machine.Drop;
               elsif C = Unwind_Protect then
                  Machine.Push (Machine.Environment);
                  Machine.Environment := Machine.Car (Cs);
                  Machine.Push (Machine.Cadr (Cs));
                  Machine.Push (Unwind_Continue);
                  Machine.Push (Machine.Cddr (Cs));
                  Machine.Cons;
                  Machine.Cons;
                  Machine.Control := Machine.Pop;
                  C_Updated := True;
               elsif C = Unwind_Continue then
                  Machine.Drop;
                  Machine.Environment := Machine.Pop;
               elsif C = Internal_Define then
                  declare
                     Value : constant Object := Machine.Top (1);
                     Name  : constant Object := Machine.Top (2);
                  begin
                     if Machine.Environment = Nil then
                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             (Machine.Show (Name) & " = "
                              & Machine.Show (Value));
                           Ada.Wide_Wide_Text_IO.Put_Line
                             (Machine.Show (Machine.Stack));
                        end if;
                        begin
                           Lith.Environment.Define (To_Symbol (Name), Value);
                        exception
                           when others =>
                              Ada.Wide_Wide_Text_IO.Put_Line
                                ("fail: "
                                 & Machine.Show (Name) & " = "
                                 & Machine.Show (Value));
                              raise;
                        end;

                        Machine.Drop;
                     else
                        Machine.Cons;
                        Machine.Push (Machine.Car (Machine.Environment));
                        Machine.Cons;
                        Machine.Set_Car (Machine.Environment, Machine.Pop);
                        Machine.Push (Name);
                     end if;
                  end;
               elsif C = Set_Symbol then
                  declare
                     Value : constant Object := Machine.Top (1);
                     Name  : constant Object := Machine.Top (2);
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
                        Machine.Drop (2);
                     else
                        --  name and value are still on the stack,
                        --  in the right order.
                        Machine.Cons;
                        Machine.Push (Nil);
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
                        Ada.Characters.Conversions.To_String
                          (Show (Machine, Machine.Current_Context))
                          & " undefined: "
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

                  Machine.R1 := C;
                  Machine.R2 := F;

                  Machine.Control := Cs;
                  C_Updated := True;

                  if Is_Pair (F) and then Machine.Car (F) = Macro_Symbol then
                     for I in reverse Arguments'Range loop
                        Machine.R2 := Machine.Cons (Machine.Pop, Machine.R2);
                        Arguments (I) := Machine.Car (Machine.R2);
                     end loop;
                  else
                     for I in Arguments'Range loop
                        Machine.R2 := Machine.Cons (Machine.Pop, Machine.R2);
                        Arguments (I) := Machine.Car (Machine.R2);
                     end loop;
                  end if;

                  if Is_Function (F) then
                     begin
                        declare
                           Result : constant Object :=
                                      Lith.Objects.Interfaces.Evaluate
                                        (Machine, To_Function (F),
                                         Arguments, Machine.Environment);
                        begin
                           Machine.Push (Result);
                        end;
                     exception
                        when E : others =>
                           Ada.Wide_Wide_Text_IO.Put_Line
                             (Ada.Wide_Wide_Text_IO.Standard_Error,
                              "Error: "
                              & Show (Machine, Machine.Current_Context)
                              & " "
                              & Ada.Characters.Conversions.To_Wide_Wide_String
                                (Ada.Exceptions.Exception_Message (E)));
                           raise;
                     end;

                  elsif Is_Pair (F)
                    and then (Machine.Car (F) = Lambda_Symbol
                              or else Machine.Car (F) = Macro_Symbol)
                  then
                     if Machine.Car (F) = Macro_Symbol then
                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("macro: pushing post-macro");
                        end if;
                        Push_Control (Lith.Objects.Symbols.Stack_To_Control);
                     end if;

                     if Trace_Eval and then Is_Tail_Context then
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("tail-call: " & Machine.Show (F));
                     end if;

                     if Is_Tail_Context then
                        null;
                     else
                        Save_State;
                        Machine.Push (Nil);
                        Machine.Push (Machine.Environment);
                        Machine.Cons;
                        Machine.Environment := Machine.Pop;
                     end if;

                     Machine.Dump := Machine.Cons (C, Machine.Dump);
                     Machine.Control := Nil;

                     declare
                        Arg_Array : constant Array_Of_Objects :=
                                      Machine.To_Object_Array
                                        (Machine.Cdr
                                           (Machine.Cdr (F)));
                        Last : Boolean := True;
                     begin
                        for Arg of reverse Arg_Array loop
                           Machine.Set_Context (Arg);
                           if Last then
                              Machine.Make_List ((Tail_Context, Arg));
                              Push_Control (Machine.Pop);
                              Last := False;
                           else
                              Push_Control (Arg);
                           end if;
                        end loop;
                        Set_Context (Machine, C);
                        Machine.Dump := Machine.Cdr (Machine.Dump);
                     end;

                     C_Updated := True;

                     Create_Environment
                       (Machine      => Machine,
                        Formals      => Machine.Cadr (F),
                        Actuals      => Arguments);
                     Machine.Stack := Nil;
                  else
                     raise Evaluation_Error with
                       "bad application: "
                       & Ada.Characters.Conversions.To_String
                       (Machine.Show (F));
                  end if;
                  Machine.R1 := Nil;
                  Machine.R2 := Nil;
               end;
            else
               --  a pair
               declare
                  F : constant Object := Machine.Car (C);
                  Args : constant Object := Machine.Cdr (C);
               begin
                  if F = Quote_Symbol then
                     if Machine.Cdr (Args) /= Nil then
                        raise Evaluation_Error with
                          "quote: too many arguments";
                     end if;
                     Machine.Push (Machine.Car (Args));
                  elsif F = Car_Symbol or else F = Cdr_Symbol then
                     Machine.Make_List
                       ((Machine.Car (Args),
                        (if F = Car_Symbol then Do_Car else Do_Cdr),
                         Cs));
                     Machine.Control := Machine.Pop;
                     C_Updated := True;
                  elsif F = Null_Symbol then
                     Machine.Make_List ((Machine.Car (Args),
                                        Do_Null,
                                        Cs));
                     Machine.Control := Machine.Pop;
                     C_Updated := True;
                  elsif F = If_Symbol then
                     Machine.Push (Args, Secondary);
                     Machine.Control :=
                       Machine.Cons (Choice, Cs);
                     Machine.Control :=
                       Machine.Cons (Machine.Car (Args),
                                     Machine.Control);
                     declare
                        True_Part  : constant Object :=
                                       Machine.Cadr (Args);
                        False_Part : constant Object :=
                                       (if Machine.Cddr (Args) = Nil
                                        then No_Value
                                        else Machine.Car
                                          (Machine.Cddr (Args)));
                     begin
                        if Is_Tail_Context then
                           Machine.Push (Tail_Context);
                           Machine.Push (False_Part);
                           Machine.Cons;
                           Machine.Push (Tail_Context);
                           Machine.Push (True_Part);
                           Machine.Cons;
                        else
                           Machine.Push (False_Part);
                           Machine.Push (True_Part);
                        end if;
                     end;

                     Machine.Drop (1, Secondary);
                     C_Updated := True;
                  elsif F = Set_Symbol then
                     Machine.Control :=
                       Machine.Cons (Set_Symbol, Cs);
                     Machine.Control :=
                       Machine.Cons (Machine.Cadr (Args), Machine.Control);
                     Machine.Push (Machine.Car (Args));
                     C_Updated := True;
                  elsif F = Lith_Define_Symbol then
                     declare
                        Name : constant Object :=
                                 Machine.Car (Args);
                        Value : constant Object :=
                                  Machine.Cadr (Args);
                     begin
                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("define: "
                              & Machine.Show (Name)
                              & " = "
                              & Machine.Show (Value));
                        end if;

                        Machine.Push (Value);
                        Machine.Push (Internal_Define);
                        Machine.Push (Cs);
                        Machine.Cons;
                        Machine.Cons;
                        Machine.Control := Machine.Pop;

                        Machine.Push (Name);
                        C_Updated := True;
                     end;
                  elsif F = Apply_Syntax_Symbol then
                     declare
                        Syntax : constant Object := Machine.Cadr (Args);
                        Call   : Object;
                        Found  : Boolean;
                     begin
                        Get (Machine, To_Symbol (Machine.Car (Args)),
                             Call, Found);
                        if not Found then
                           raise Evaluation_Error with
                             "syntax arguments not found";
                        end if;

                        if Trace_Patterns then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("applying syntax: "
                              & Machine.Show (Syntax));
                        end if;
                        Apply_Syntax (Machine, Call, Syntax);
                        Machine.Control :=
                          Machine.Cons (Machine.Pop, Cs);
                        C_Updated := True;
                     end;
                  elsif F = Dynamic_Wind_Symbol then

                     Machine.Push (Machine.Car (Args));
                     Machine.Push (Nil);
                     Machine.Cons;
                     Machine.Push (Stack_Drop);
                     Machine.Push (Machine.Cadr (Args));
                     Machine.Push (Nil);
                     Machine.Cons;
                     Machine.Push (Unwind_Protect);
                     Machine.Push (Machine.Environment);
                     Machine.Push (Machine.Car (Machine.Cddr (Args)));
                     Machine.Push (Nil);
                     Machine.Cons;
                     Machine.Push (Cs);
                     Machine.Cons;
                     Machine.Cons;
                     Machine.Cons;
                     Machine.Cons;
                     Machine.Cons;
                     Machine.Cons;
                     Machine.Control := Machine.Pop;
                     if Trace_Eval then
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("dynamic-wind: control = "
                           & Machine.Show (Machine.Control));
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("dynamic-wind: dump = "
                           & Machine.Show (Machine.Dump));
                     end if;
                     C_Updated := True;
                  elsif F = With_Exception_Handler_Symbol then
                     Machine.Push (Machine.Cadr (Args));
                     Machine.Push (Nil);
                     Machine.Cons;
                     Machine.Push (Cs);
                     Machine.Cons;
                     Machine.Control := Machine.Pop;
                     Machine.Make_List
                       ((Machine.Car (Args), Machine.Dump, Nil));
                     Machine.Push (Machine.Handlers);
                     Machine.Cons;
                     Machine.Handlers := Machine.Pop;
                     C_Updated := True;
                  elsif F = Raise_Symbol then
                     declare
                        Handler : Object;
                        Ex      : Object;
                        Found   : Boolean;
                     begin

                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("raise exception");
                        end if;

                        if Machine.Handlers = Nil then
                           Ada.Wide_Wide_Text_IO.Put_Line
                           ("Unhandled exception: "
                            & Machine.Show (Machine.Car (Args)));
                           Restore_State (Nil);
                        else
                           Handler := Machine.Car (Machine.Handlers);

                           --  we happen to know that we are called with one
                           --  argument, 'obj', which allows as to fetch the
                           --  actual exception object for the unwind operation
                           Get (Machine, Get_Symbol ("obj"),
                                Ex, Found);
                           pragma Assert (Found);
                           Machine.Make_List
                             ((Machine.Car (Handler),
                              Machine.Car (Args),
                              Nil));
                           Machine.Make_List
                             ((Unwind_Continue, Ex, Nil));
                           Machine.Push (Nil);
                           Machine.Cons;
                           Machine.Cons;
                           Machine.Control := Machine.Pop;
                           C_Updated := True;
                        end if;

                     end;
                  elsif F = Unwind_Continue then
                     Machine.Drop;
                     Restore_State
                       (Machine.Cadr (Machine.Car (Machine.Handlers)));
                     Machine.Handlers := Machine.Cdr (Machine.Handlers);

                     Machine.Make_List
                       ((Raise_Symbol, Machine.Car (Args), Nil));
                     Machine.Push (Nil);
                     Machine.Cons;
                     if Machine.Control = Nil then
                        Machine.Control := Machine.Pop;
                     else
                        declare
                           It : Object := Machine.Control;
                        begin
                           while Machine.Cdr (It) /= Nil loop
                              It := Machine.Cdr (It);
                           end loop;
                           Machine.Set_Cdr (It, Machine.Pop);
                        end;
                     end if;

                     if Trace_Eval then
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("unwind-continue");
                     end if;
                     C_Updated := True;
                  elsif F = Unwind_Dump then
                     Restore_State (Machine.Car (Args));
                     C_Updated := True;
                  elsif F = Lambda_Symbol or else F = Macro_Symbol then
                     Machine.Push (C);
                  elsif F = String_Value then
                     Machine.Push (C);
                  elsif F = Large_Integer_Value then
                     Machine.Push (C);
                  elsif F = Floating_Point_Value then
                     Machine.Push (C);
                  elsif F = Import_Symbol then

                     Machine.Push (Args);

                     declare
                        Result : constant Boolean :=
                                   Import_Libraries (Machine, Args);
                     begin
                        Machine.Drop;

                        Machine.Push
                          ((if Result then True_Value else False_Value));
                     end;
                  elsif F = Begin_Symbol then

                     if Trace_Eval then
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("begin: "
                           & Machine.Show (Args));
                     end if;

                     Machine.Dump := Machine.Cons (C, Machine.Dump);
                     Machine.Control := Cs;
                     C_Updated := True;
                     declare
                        Arg_Array : constant Array_Of_Objects :=
                                      Machine.To_Object_Array (Args);
                        First     : Boolean := True;

                     begin
                        for Arg of reverse Arg_Array loop
                           if First then
                              if Is_Tail_Context then
                                 Machine.Push (Tail_Context);
                                 Machine.Push (Arg);
                                 Machine.Cons;
                                 Push_Control (Machine.Pop);
                              else
                                 Push_Control (Arg);
                              end if;
                              First := False;
                           else
                              Push_Control (Stack_Drop);
                              Push_Control (Arg);
                           end if;
                        end loop;
                        Machine.Dump := Machine.Cdr (Machine.Dump);
                     end;
                  else
                     Machine.Dump := Machine.Cons (C, Machine.Dump);
                     Machine.Control := Cs;
                     C_Updated := True;

                     if (Is_Pair (F) and then
                         Machine.Car (F) = Lambda_Symbol)
                       or else Is_Macro (Machine, F)
                     then
                        Is_Tail_Context := False;
                     end if;

                     if Is_Tail_Context then
                        Machine.Push (Tail_Context);
                     end if;
                     Machine.Push (Apply_Object (Length (Machine, Args)));
                     if Is_Tail_Context then
                        Machine.Cons;
                     end if;
                     Push_Control (Machine.Pop);
                     Push_Control (F);

                     declare
                        It : Object := Args;
                        Macro : constant Boolean := Is_Macro (Machine, F);
                     begin
                        if Trace_Eval then
                           Ada.Wide_Wide_Text_IO.Put_Line
                             ("Is_Macro: " & Machine.Show (F)
                              & " = "
                              & (if Macro then "yes" else "no"));
                        end if;

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
            end if;

            declare
               S : constant Object := Machine.Pop;
            begin
               Restore_State;
               Machine.Push (S);

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

   ----------------------
   -- Import_Libraries --
   ----------------------

   function Import_Libraries
     (Machine    : in out Root_Lith_Machine'Class;
      Import_Set : Lith.Objects.Object)
      return Boolean
   is
      use Ada.Characters.Conversions;
      use Lith.Objects;

      function Get_Library_Path (Library_Name : Object) return String;

      ----------------------
      -- Get_Library_Path --
      ----------------------

      function Get_Library_Path (Library_Name : Object) return String is
      begin
         if Library_Name = Nil then
            return ".scm";
         else
            declare
               Symbol : constant Symbol_Type :=
                          To_Symbol (Machine.Car (Library_Name));
               Name   : constant String :=
                          To_String (Lith.Objects.Symbols.Get_Name (Symbol));
            begin
               return "/" & Name
                 & Get_Library_Path (Machine.Cdr (Library_Name));
            end;
         end if;
      end Get_Library_Path;

      It : Object := Import_Set;
   begin
      while It /= Nil loop
         declare
            Path : constant String :=
                     Get_Library_Path (Machine.Car (It));
         begin
            Lith.Parser.Parse_File
              (Machine,
               Lith.Paths.Config_Path & Path);
         exception
            when E : others =>
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  "error opening library "
                  & Machine.Show (Machine.Car (It)));
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Ada.Wide_Wide_Text_IO.Standard_Error,
                  To_Wide_Wide_String
                    (Ada.Exceptions.Exception_Message (E)));
               return False;
         end;
         It := Machine.Cdr (It);
      end loop;
      return True;
   end Import_Libraries;

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
        and then Machine.Car (F) = Lith.Objects.Symbols.Macro_Symbol
      then
         return True;
--        elsif Is_Pair (F)
--          and then Machine.Car (F) = Lith.Objects.Symbols.Lambda
--          and then Is_Pair (Machine.Cdr (F))
--          and then Is_Pair (Machine.Cddr (F))
--          and then Is_Pair (Machine.Car (Machine.Cddr (F)))
--          and then Machine.Car (Machine.Car (Machine.Cddr (F)))
--          = Lith.Objects.Symbols.Apply_Syntax_Atom
--        then
--           return True;
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

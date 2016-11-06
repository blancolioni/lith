with Ada.Exceptions;
with Ada.Text_IO;

with Lith.Objects.Interfaces;
with Lith.Parser;
with Lith.Objects.Symbols;

with Lith.Options;

with Lith.Paths;

package body Lith.Machine.SECD is

   function Import_Libraries
     (Machine    : in out Root_Lith_Machine'Class)
      return Boolean;

   procedure Create_Environment
     (Machine      : in out Root_Lith_Machine'Class;
      Formals      : Lith.Objects.Object);

   procedure Get
     (Machine : in out Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean);

   procedure Get
     (Machine   : in out Root_Lith_Machine'Class;
      Symbol    : Lith.Objects.Symbol_Type;
      Result    : out Lith.Objects.Object;
      Found_Env : out Lith.Objects.Object;
      Found     : out Boolean;
      Global    : out Boolean);

   function Is_Macro
     (Machine : in out Root_Lith_Machine'Class;
      F       : Lith.Objects.Object)
      return Boolean;
   --  returns True if F ultimately (or immediately) refers to a macro
   --  in the current machine environment

   procedure Check_Argument_Count
     (Machine  : Root_Lith_Machine'Class;
      Name     : String;
      Min      : Natural;
      Max      : Integer := -1);

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

      Local_Call : Object renames Machine.R (11);

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
         E : Object renames Machine.R (9);
         C : Object renames Machine.R (10);
      begin

         E := Env;
         C := Code;

         if Is_Pair (C) then

            Machine.Push (C, Secondary);

            if Is_Pair (Machine.Cdr (C))
              and then Machine.Cadr (C) = Ellipsis_Symbol
            then
               Machine.Push
                 (Binding (E, Ellipsis_Symbol, Nil));
            else
               Apply (E, Machine.Cdr (C));
            end if;

            C := Machine.Pop (Secondary);

            Apply (E, Machine.Car (C));
            Machine.Swap;
            Machine.Cons;
         elsif Is_Symbol (C) then
            Machine.Push (Binding (E, C, C));
         else
            Machine.Push (C);
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
         Pat_It  : Object renames Machine.R (1);
         Call_It : Object renames Machine.R (2);
         Count   : Natural := 0;
      begin
         Pat_It := Pat;
         Call_It := Local_Call;

         if Lith.Options.Trace_Patterns then
            Ada.Text_IO.Put_Line
              ("Match: pattern = " & Machine.Show (Pat));
            Ada.Text_IO.Put_Line
              ("          call = " & Machine.Show (Local_Call));
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

      Pats : Object renames Machine.R (13);
      Pat  : Object renames Machine.R (14);
      Code : Object renames Machine.R (15);

   begin

      Local_Call := Call;
      Pats := Machine.Cddr (Syntax_Rules);

      while Pats /= Nil loop
         Pat  := Machine.Caar (Pats);
         Code := Machine.Car (Machine.Cdar (Pats));

         if Match (Machine.Cdr (Pat)) then
            if Lith.Options.Trace_Patterns then
               Ada.Text_IO.Put_Line
                 ("found match: "
                  & Machine.Show (Machine.Cdr (Pat))
                  & " --> "
                  & Machine.Show (Code));
               Ada.Text_IO.Put_Line
                 ("applying: "
                  & Machine.Show (Machine.Top));
            end if;

            Apply (Machine.Top, Code);
            Machine.Swap;
            Machine.Drop;

            if Lith.Options.Trace_Patterns then
               Machine.Report_State;
               Ada.Text_IO.Put_Line
                 ("result: "
                  & Machine.Show (Machine.Top));
            end if;
            return;
         end if;

         Pats := Machine.Cdr (Pats);
      end loop;
      raise Evaluation_Error with
        "no matching pattern in syntax rules";
   end Apply_Syntax;

   --------------------------
   -- Check_Argument_Count --
   --------------------------

   procedure Check_Argument_Count
     (Machine  : Root_Lith_Machine'Class;
      Name     : String;
      Min      : Natural;
      Max      : Integer := -1)
   is
   begin
      if Machine.Arg_Count < Min then
         raise Lith.Objects.Evaluation_Error with
           "too few arguments to function " & Name;
      elsif Machine.Arg_Count > Integer'Max (Min, Max) then
         raise Lith.Objects.Evaluation_Error with
           "too many arguments to function " & Name;
      end if;
   end Check_Argument_Count;

   ------------------------
   -- Create_Environment --
   ------------------------

   procedure Create_Environment
     (Machine      : in out Root_Lith_Machine'Class;
      Formals      : Lith.Objects.Object)
   is
      use Lith.Objects;
      Env        : Object renames Machine.R (6);
      Formal_It  : Object renames Machine.R (7);
      Rest       : Boolean := Is_Atom (Formals);
      Rest_Name  : Object  := (if Rest then Formals else Nil);
      Count      : Natural := 0;
      Rest_Count : Natural := 0;

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
         It : Object;
      begin

         if Lith.Options.Trace_Evaluation then
            Ada.Text_IO.Put_Line
              (Machine.Show (Name)
               & " <-- "
               & Machine.Show (Value));
         end if;

         --  protect against GC when we push Name
         Machine.Push (Value);
         Machine.Push (Name);
         Machine.Swap;
         Machine.Cons;

         It := Env;

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
            declare
               B : constant Object := Machine.Pop;
            begin
               Machine.Set_Cdr (Machine.Car (It), Machine.Cdr (B));
            end;
         end if;

      end Save_Binding;

   begin

      Env := Machine.Car (Machine.Environment);
      Formal_It := Formals;

      if Formals = Nil then
         Machine.Environment := Machine.Cons (Nil, Machine.Environment);
         return;
      end if;

      if not Is_Pair (Formals) and then not Is_Symbol (Formals) then
         raise Evaluation_Error with
         Machine.Show (Formal_It)
           & " cannot be used as a formal argument";
      end if;

      if Is_Symbol (Formals) then
         Rest := True;
         Rest_Name := Formals;
         Count := 1;
      end if;

      for I in 1 .. Machine.Arg_Count loop

         declare
            Actual : constant Object := Machine.Args (I);
         begin

            if Rest then
               Machine.Push (Actual);
               Rest_Count := Rest_Count + 1;
            else

               if Formal_It = Nil then
                  raise Evaluation_Error with
                    "too many arguments: "
                    & Machine.Show (Formals)
                    & " at actual: "
                    & Machine.Show (Actual);
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
         end;
      end loop;

      if Rest then
         Machine.Push (Nil);
         for I in 1 .. Rest_Count loop
            Machine.Cons;
         end loop;

         Save_Binding (Rest_Name, Machine.Pop);
      end if;

      if Lith.Options.Trace_Evaluation then
         Ada.Text_IO.Put_Line
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
         if Lith.Options.Trace_Evaluation then
            Ada.Text_IO.Put_Line ("saving context ...");
         end if;
         Machine.Dump := Machine.Cons (Machine.Control, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Environment, Machine.Dump);
         Machine.Dump := Machine.Cons (Machine.Stack, Machine.Dump);
      end Save_State;

      C    : Object renames Machine.R (3);
      Cs   : Object renames Machine.R (4);
      It   : Object renames Machine.R (5);
      F    : Object renames Machine.R (6);
      --  Args : Object renames Machine.R (7);

   begin

      while Machine.Control /= Nil loop
         declare
            Is_Tail_Context : Boolean := False;
         begin

            C := Machine.Car (Machine.Control);
            Cs := Machine.Cdr (Machine.Control);

            Machine.Control := Cs;

            Set_Context (Machine, C);

            if Machine.Profiling then
               Machine.Hit (C);
            end if;

            declare
               Trace : Object;
               Found : Boolean;
            begin
               Machine.Get_Top_Level
                 (Get_Symbol ("*trace-eval*"),
                  Trace, Found);

               if Found and then Trace /= False_Value then
                  Lith.Options.Trace_Evaluation := True;
                  Lith.Options.Trace_Patterns := True;
               end if;
            end;

            if Lith.Options.Trace_Evaluation then
               Ada.Text_IO.Put_Line
                 ("Eval: "
                  & Show (Machine, Machine.Current_Context)
                  & " "
                  & Machine.Show (C));
               Machine.Report_State;
            end if;

            if Is_Pair (C)
              and then Machine.Car (C) = Tail_Context
            then
               C := Machine.Cdr (C);
               Is_Tail_Context := True;
               if Lith.Options.Trace_Evaluation then
                  Ada.Text_IO.Put_Line
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
            elsif Is_External_Object (C) then
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
                  end;
               elsif C = Do_Car then
                  Machine.Push (Machine.Car (Machine.Pop));
               elsif C = Do_Cdr then
                  Machine.Push (Machine.Cdr (Machine.Pop));
               elsif C = Do_Null then
                  Machine.Push (To_Object (Machine.Pop = Nil));
               elsif C = Lith.Objects.Symbols.Stack_To_Control then
                  Push_Control (Machine.Pop);
               elsif C = Stack_Drop then
                  if Lith.Options.Trace_Evaluation then
                     Ada.Text_IO.Put_Line
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
               elsif C = Unwind_Continue then
                  Machine.Drop;
                  Machine.Environment := Machine.Pop;
               elsif C = Internal_Define then
                  declare
                     Value : constant Object := Machine.Top (1);
                     Name  : constant Object := Machine.Top (2);
                  begin
                     if Machine.Environment = Nil then
                        if Lith.Options.Trace_Definitions then
                           Ada.Text_IO.Put_Line
                             (Machine.Show (Name) & " = "
                              & Machine.Show (Value));
                           Ada.Text_IO.Put_Line
                             (Machine.Show (Machine.Stack));
                        end if;
                        begin
                           Machine.Define_Top_Level
                             (To_Symbol (Name), Value);
                        exception
                           when others =>
                              Ada.Text_IO.Put_Line
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
                     Found_Env : Object;
                     Found     : Boolean;
                     Global    : Boolean;
                  begin
                     Get (Machine, To_Symbol (Name), Old_Value,
                          Found_Env, Found, Global);

                     if (not Found and then Machine.Environment = Nil)
                       or else (Found and then Global)
                     then
                        if Found then
                           Machine.Define_Top_Level
                             (To_Symbol (Name), Value);
                        else
                           Machine.Define_Top_Level
                             (To_Symbol (Name), Value);
                        end if;
                     else
                        declare
                           It : Object := Found_Env;
                        begin
                           while It /= Nil
                             and then Machine.Caar (It) /= Name
                           loop
                              It := Machine.Cdr (It);
                           end loop;
                           pragma Assert (It /= Nil and then
                                          Machine.Caar (It) = Name);

                           Machine.Set_Cdr (Machine.Car (It), Value);
                        end;
                     end if;
                     Machine.Drop (2);
                     Machine.Push (Name);
                  end;
               elsif C = Internal_Apply then
                  Machine.R (1) := Machine.Pop;  --  proc
                  Machine.R (2) := Machine.Pop;  --  args

                  Ada.Text_IO.Put_Line
                    ("apply: " & Machine.Show (Machine.R (1))
                     & " " & Machine.Show (Machine.R (2)));

                  declare
                     It : Object := Machine.R (2);
                     Count : Natural := 0;
                  begin
                     while It /= Nil loop
                        Count := Count + 1;
                        Machine.Args (Count) := Machine.Car (It);
                        It := Machine.Cdr (It);
                     end loop;

                     Machine.Push (Machine.R (1));
                     Machine.Push (Apply_Object (Count));
                     Machine.Push (Cs);
                     Machine.Cons;
                     Machine.Control := Machine.Pop;
                     Machine.R (1) := Nil;
                     Machine.R (2) := Nil;
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
                        Show (Machine, Machine.Current_Context)
                          & " undefined: "
                          & Get_Name (To_Symbol (C));
                     end if;
                  end;
               end if;
            elsif Is_Function (C) then
               Machine.Push (C);
            elsif Is_Apply (C) then
               F := Machine.Pop;

               for I in 1 .. Argument_Count (C) loop
                  Machine.Args (I) := Machine.Pop;
               end loop;

               Machine.Arg_Count := Argument_Count (C);

               if Is_Function (F) then
                  begin
                     declare
                        Result : constant Object :=
                                   Lith.Objects.Interfaces.Evaluate
                                     (Machine, To_Function (F),
                                      Machine.Environment);
                     begin
                        Machine.Push (Result);
                     end;
                  exception
                     when E : others =>
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "Error: "
                           & Show (Machine, Machine.Current_Context)
                           & " "
                           & Ada.Exceptions.Exception_Message (E));
                        raise;
                  end;

               elsif Is_Pair (F)
                 and then (Machine.Car (F) = Lambda_Symbol
                           or else Machine.Car (F) = Macro_Symbol)
               then
                  if Machine.Car (F) = Macro_Symbol then
                     if Lith.Options.Trace_Evaluation then
                        Ada.Text_IO.Put_Line
                          ("macro: pushing post-macro");
                     end if;
                     Push_Control (Lith.Objects.Symbols.Stack_To_Control);
                  end if;

                  if Lith.Options.Trace_Evaluation
                    and then Is_Tail_Context
                  then
                     Ada.Text_IO.Put_Line
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

                  Machine.Control := Nil;

                  It := Machine.Cdr (Machine.Cdr (F));

                  declare
                     Body_Count : Natural := 0;
                  begin
                     while It /= Nil loop
                        if Machine.Cdr (It) = Nil then
                           Machine.Push (Tail_Context);
                           Machine.Push (Machine.Car (It));
                           Machine.Cons;
                        else
                           Machine.Push (Machine.Car (It));
                        end if;
                        It := Machine.Cdr (It);
                        Body_Count := Body_Count + 1;
                     end loop;

                     for I in 1 .. Body_Count loop
                        Push_Control (Machine.Pop);
                     end loop;

                  end;

                  Create_Environment
                    (Machine      => Machine,
                     Formals      => Machine.Cadr (F));

                  Machine.Stack := Nil;
               else
                  raise Evaluation_Error with
                    "bad application: "
                    & Machine.Show (F);
               end if;

            else
               --  a pair

               F := Machine.Car (C);

               declare
                  It : Object := Machine.Cdr (C);
               begin
                  Machine.Arg_Count := 0;
                  while It /= Nil loop
                     Machine.Arg_Count := Machine.Arg_Count + 1;
                     Machine.Args (Machine.Arg_Count) := Machine.Car (It);
                     It := Machine.Cdr (It);
                  end loop;
               end;

               if F = Quote_Symbol then
                  if Machine.Arg_Count /= 1 then
                     raise Evaluation_Error with
                       "quote: too many arguments";
                  end if;
                  Machine.Push (Machine.Args (1));
               elsif F = Car_Symbol or else F = Cdr_Symbol then
                  if Machine.Arg_Count /= 1 then
                     raise Evaluation_Error with
                       (if F = Car_Symbol then "car" else "cdr")
                       & ": too many arguments";
                  end if;

                  Machine.Push (Machine.Args (1));
                  Machine.Push ((if F = Car_Symbol then Do_Car else Do_Cdr));
                  Machine.Push (Cs);
                  Machine.Cons;
                  Machine.Cons;
                  Machine.Control := Machine.Pop;

               elsif F = Null_Symbol then
                  Check_Argument_Count (Machine, "null?", 1);

                  Machine.Push (Machine.Args (1));
                  Machine.Push (Do_Null);
                  Machine.Push (Cs);
                  Machine.Cons;
                  Machine.Cons;
                  Machine.Control := Machine.Pop;

               elsif F = If_Symbol then
                  if Machine.Arg_Count not in 2 .. 3 then
                     raise Evaluation_Error with
                       "if: 2 or 3 arguments required";
                  end if;

                  Machine.Control :=
                    Machine.Cons (Choice, Cs);
                  Machine.Control :=
                    Machine.Cons (Machine.Args (1),
                                  Machine.Control);

                  declare
                     True_Part : Object renames Machine.R (1);
                     False_Part : Object renames Machine.R (2);
                  begin
                     True_Part := Machine.Args (2);
                     False_Part := (if Machine.Arg_Count = 2
                                    then No_Value
                                    else Machine.Args (3));

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

               elsif F = Set_Symbol then
                  Check_Argument_Count (Machine, "set!", 2);
                  Machine.Control :=
                    Machine.Cons (Set_Symbol, Cs);
                  Machine.Control :=
                    Machine.Cons (Machine.Args (2), Machine.Control);
                  Machine.Push (Machine.Args (1));

               elsif F = Lith_Define_Symbol then
                  Check_Argument_Count (Machine, "lith-define", 2);
                  declare
                     Name  : constant Object := Machine.Args (1);
                     Value : constant Object := Machine.Args (2);
                  begin
                     if Lith.Options.Trace_Evaluation then
                        Ada.Text_IO.Put_Line
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

                  end;
               elsif F = Apply_Syntax_Symbol then
                  Check_Argument_Count (Machine, "apply-syntax", 2);
                  declare
                     Syntax : constant Object := Machine.Args (2);
                     Call   : Object;
                     Found  : Boolean;
                  begin
                     Get (Machine, To_Symbol (Machine.Args (1)),
                          Call, Found);
                     if not Found then
                        raise Evaluation_Error with
                          "syntax arguments not found";
                     end if;

                     if Lith.Options.Trace_Patterns then
                        Ada.Text_IO.Put_Line
                          ("applying syntax: "
                           & Machine.Show (Syntax));
                     end if;
                     Apply_Syntax (Machine, Call, Syntax);
                     Machine.Control :=
                       Machine.Cons (Machine.Pop, Cs);
                     if Lith.Options.Trace_Patterns then
                        Ada.Text_IO.Put_Line
                          ("control: "
                           & Machine.Show (Machine.Control));
                     end if;
                  end;
               elsif F = Dynamic_Wind_Symbol then

                  Check_Argument_Count (Machine, "dynamic-wind", 3);
                  Machine.Push (Machine.Args (1));
                  Machine.Push (Nil);
                  Machine.Cons;
                  Machine.Push (Stack_Drop);
                  Machine.Push (Machine.Args (2));
                  Machine.Push (Nil);
                  Machine.Cons;
                  Machine.Push (Unwind_Protect);
                  Machine.Push (Machine.Environment);
                  Machine.Push (Machine.Car (Machine.Args (3)));
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
                  if Lith.Options.Trace_Evaluation then
                     Ada.Text_IO.Put_Line
                       ("dynamic-wind: control = "
                        & Machine.Show (Machine.Control));
                     Ada.Text_IO.Put_Line
                       ("dynamic-wind: dump = "
                        & Machine.Show (Machine.Dump));
                  end if;

               elsif F = With_Exception_Handler_Symbol then
                  Check_Argument_Count (Machine, "with-exception-handler", 2);
                  Machine.Push (Machine.Args (2));
                  Machine.Push (Nil);
                  Machine.Cons;
                  Machine.Push (Cs);
                  Machine.Cons;
                  Machine.Control := Machine.Pop;
                  Machine.Push (Machine.Args (1));
                  Machine.Push (Machine.Dump);
                  Machine.Push (Nil);
                  Machine.Cons;
                  Machine.Cons;
                  Machine.Push (Machine.Handlers);
                  Machine.Cons;
                  Machine.Handlers := Machine.Pop;

               elsif F = Raise_Symbol then
                  Check_Argument_Count (Machine, "raise", 1);
                  declare
                     Handler : Object;
                     Ex      : Object;
                     Found   : Boolean;
                  begin

                     if Lith.Options.Trace_Evaluation then
                        Ada.Text_IO.Put_Line
                          ("raise exception");
                     end if;

                     if Machine.Handlers = Nil then
                        Ada.Text_IO.Put_Line
                          ("Unhandled exception: "
                           & Machine.Show (Machine.Args (1)));
                        Restore_State (Nil);
                     else
                        Handler := Machine.Car (Machine.Handlers);

                        --  we happen to know that we are called with one
                        --  argument, 'obj', which allows as to fetch the
                        --  actual exception object for the unwind operation
                        Get (Machine, Get_Symbol ("obj"),
                             Ex, Found);
                        pragma Assert (Found);
                        Machine.Push (Machine.Car (Handler));
                        Machine.Push (Machine.Args (1));
                        Machine.Push (Nil);
                        Machine.Cons;
                        Machine.Cons;

                        Machine.Push (Unwind_Continue);
                        Machine.Push (Ex);
                        Machine.Push (Nil);
                        Machine.Cons;
                        Machine.Cons;

                        Machine.Push (Nil);
                        Machine.Cons;
                        Machine.Cons;
                        Machine.Control := Machine.Pop;

                     end if;

                  end;
               elsif F = Unwind_Continue then
                  Machine.Drop;
                  Restore_State
                    (Machine.Cadr (Machine.Car (Machine.Handlers)));
                  Machine.Handlers := Machine.Cdr (Machine.Handlers);

                  Machine.Push (Raise_Symbol);
                  Machine.Push (Machine.Args (1));
                  Machine.Push (Nil);
                  Machine.Cons;
                  Machine.Cons;

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

                  if Lith.Options.Trace_Evaluation then
                     Ada.Text_IO.Put_Line
                       ("unwind-continue");
                  end if;

               elsif F = Unwind_Dump then
                  Check_Argument_Count (Machine, "unwind-dump", 1);
                  Restore_State (Machine.Args (1));

               elsif F = Lambda_Symbol or else F = Macro_Symbol then
                  Machine.Push (C);
               elsif F = String_Value then
                  Machine.Push (C);
               elsif F = Import_Symbol then
                  declare
                     Result : constant Boolean :=
                                Import_Libraries (Machine);
                  begin
                     Machine.Push
                       ((if Result then True_Value else False_Value));
                  end;
               elsif F = Begin_Symbol then

                  declare
                     First     : Boolean := True;
                  begin
                     for I in reverse 1 .. Machine.Arg_Count loop
                        if First then
                           if Is_Tail_Context then
                              Machine.Push (Tail_Context);
                              Machine.Push (Machine.Args (I));
                              Machine.Cons;
                              Push_Control (Machine.Pop);
                           else
                              Push_Control (Machine.Args (I));
                           end if;
                           First := False;
                        else
                           Push_Control (Stack_Drop);
                           Push_Control (Machine.Args (I));
                        end if;
                     end loop;
                  end;
               else

                  if Is_Macro (Machine, F) then
                     Is_Tail_Context := False;
                  end if;

                  if Is_Tail_Context then
                     Machine.Push (Tail_Context);
                  end if;
                  Machine.Push (Apply_Object (Machine.Arg_Count));
                  if Is_Tail_Context then
                     Machine.Cons;
                  end if;
                  Push_Control (Machine.Pop);
                  Push_Control (F);

                  declare
                     Macro : constant Boolean := Is_Macro (Machine, F);
                  begin
                     if Lith.Options.Trace_Evaluation then
                        Ada.Text_IO.Put_Line
                          ("Is_Macro: " & Machine.Show (F)
                           & " = "
                           & (if Macro then "yes" else "no"));
                     end if;

                     if Macro then
                        for I in reverse 1 .. Machine.Arg_Count loop
                           Machine.Push (Machine.Args (I));
                        end loop;
                     else
                        for I in 1 .. Machine.Arg_Count loop
                           Push_Control (Machine.Args (I));
                        end loop;
                     end if;
                  end;
               end if;
            end if;

         end;

         while Machine.Control = Nil and then
           Machine.Dump /= Nil
         loop
            if Lith.Options.Trace_Evaluation then
               Ada.Text_IO.Put_Line ("restoring context ...");
            end if;

            declare
               S : constant Object := Machine.Pop;
            begin
               Restore_State;
               Machine.Push (S);
            end;
         end loop;

         Machine.Args := (others => Nil);
         Machine.Arg_Count := 0;

      end loop;
   end Evaluate;

   ---------
   -- Get --
   ---------

   procedure Get
     (Machine : in out Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type;
      Result  : out Lith.Objects.Object;
      Found   : out Boolean)
   is
      Global : Boolean;
      Found_Env : Lith.Objects.Object;
      pragma Unreferenced (Global);
      pragma Unreferenced (Found_Env);
   begin
      Get (Machine, Symbol, Result, Found_Env, Found, Global);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Machine   : in out Root_Lith_Machine'Class;
      Symbol    : Lith.Objects.Symbol_Type;
      Result    : out Lith.Objects.Object;
      Found_Env : out Lith.Objects.Object;
      Found     : out Boolean;
      Global    : out Boolean)
   is
      use Lith.Objects;
      Outer : Object := Machine.Environment;
   begin
      while Outer /= Nil loop
         declare
            Inner : Object := Machine.Car (Outer);
         begin
            while Inner /= Nil loop
               if not Is_Pair (Inner) then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "malformed environment at " & Machine.Show (Inner)
                     & " while looking for "
                     & Lith.Objects.Symbols.Get_Name (Symbol));
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "inner environment: "
                     & Machine.Show (Machine.Car (Outer)));
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "outer environment: "
                     & Machine.Show (Machine.Environment));
                  raise Evaluation_Error;
               end if;

               declare
                  Item : constant Object := Machine.Car (Inner);
               begin
                  if To_Symbol (Machine.Car (Item)) = Symbol then
                     Result := Machine.Cdr (Item);
                     Found_Env := Machine.Car (Outer);
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

      Machine.Get_Top_Level
        (Symbol, Result, Found);
      Global := Found;
      Found_Env := Nil;
   end Get;

   ----------------------
   -- Import_Libraries --
   ----------------------

   function Import_Libraries
     (Machine    : in out Root_Lith_Machine'Class)
      return Boolean
   is
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
                          Lith.Objects.Symbols.Get_Name (Symbol);
            begin
               return "/" & Name
                 & Get_Library_Path (Machine.Cdr (Library_Name));
            end;
         end if;
      end Get_Library_Path;

   begin
      for I in 1 .. Machine.Arg_Count loop
         declare
            Arg  : constant Object := Machine.Args (I);
            Path : constant String :=
                     Get_Library_Path (Arg);
         begin
            Lith.Parser.Parse_File
              (Machine,
               Lith.Paths.Config_Path & Path);
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "error opening library "
                  & Machine.Show (Machine.Args (I)));
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));
               return False;
         end;
      end loop;
      return True;
   end Import_Libraries;

   --------------
   -- Is_Macro --
   --------------

   function Is_Macro
     (Machine : in out Root_Lith_Machine'Class;
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

end Lith.Machine.SECD;

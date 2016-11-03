with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO;

with Lith.Parser;
with Lith.Objects.Symbols;

package body Lith.Objects.Interfaces is

   package Validator_Vectors is
     new Ada.Containers.Vectors (Positive, Function_Argument_Type);

   type Simple_Function_Evaluator is
     new Root_Function_Interface with
      record
         Eval           : Simple_Evaluator;
         Arg_Constraint : Validator_Vectors.Vector;
      end record;

   overriding function Evaluate
     (Fn    : Simple_Function_Evaluator;
      Name  : Symbol_Type;
      Store : in out Object_Store'Class)
      return Object;

   package Function_Interface_Holder is
     new Ada.Containers.Indefinite_Holders (Root_Function_Interface'Class);

   type Function_Record is
      record
         Name : Symbol_Type;
         Fn   : Function_Type;
         Eval : Function_Interface_Holder.Holder;
      end record;

   package Function_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Function_Record);

   Defs : Function_Vectors.Vector;

   procedure New_Def
     (Name : String;
      Eval : Root_Function_Interface'Class);

   ---------------------
   -- Bind_Primitives --
   ---------------------

   procedure Bind_Primitives
     (Store : in out Object_Store'Class)
   is
   begin
      for Rec of Defs loop
         Store.Define_Top_Level (Rec.Name, To_Object (Rec.Fn));
      end loop;
   end Bind_Primitives;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name           : String;
      Eval           : Simple_Evaluator)
   is
   begin
      New_Def (Name,
               Simple_Function_Evaluator'
                 (Root_Function_Interface with
                    Eval           => Eval,
                    Arg_Constraint => Validator_Vectors.Empty_Vector));
   end Define_Function;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name           : String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator)
   is
      pragma Unreferenced (Argument_Count);
   begin
      Define_Function (Name, Eval);
   end Define_Function;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Store          : not null access Object_Store'Class;
      Name           : String;
      Eval           : Simple_Evaluator)
   is
   begin
      New_Def (Name,
               Simple_Function_Evaluator'
                 (Root_Function_Interface
                  with Eval      => Eval,
                  Arg_Constraint => Validator_Vectors.Empty_Vector));
      Store.Define_Top_Level
        (Defs.Last_Element.Name,
         To_Object (Defs.Last_Element.Fn));
   end Define_Function;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name   : String;
      Eval   : Root_Function_Interface'Class)
   is
   begin
      New_Def (Name, Eval);
   end Define_Function;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name      : String;
      Arguments : Argument_Type_Array;
      Eval      : Simple_Evaluator)
   is
      Eval_Rec : Simple_Function_Evaluator;
   begin
      Eval_Rec.Eval := Eval;
      for Arg of Arguments loop
         Eval_Rec.Arg_Constraint.Append (Arg);
      end loop;
      Define_Function (Name, Eval_Rec);
   end Define_Function;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Store       : in out Object_Store'Class;
      Fn          : Function_Type;
      Environment : Object)
      return Object
   is
      pragma Unreferenced (Environment);
      Def  : constant Function_Record := Defs.Element (Positive (Fn));
      Eval : constant Root_Function_Interface'Class :=
              Def.Eval.Element;
   begin
      return Eval.Evaluate (Def.Name, Store);
   end Evaluate;

   overriding function Evaluate
     (Fn    : Simple_Function_Evaluator;
      Name  : Symbol_Type;
      Store : in out Object_Store'Class)
      return Object
   is
   begin
      if not Fn.Arg_Constraint.Is_Empty then
         if Store.Argument_Count /= Fn.Arg_Constraint.Last_Index then
            raise Evaluation_Error with
            Symbols.Get_Name (Name) & ": require"
              & Natural'Image (Fn.Arg_Constraint.Last_Index)
              & " arguments, but found"
              & Natural'Image (Store.Argument_Count);
         end if;

         for I in 1 .. Store.Argument_Count loop
            declare
               V : constant Function_Argument_Type :=
                     Fn.Arg_Constraint (I);
            begin
               if (V.Simple_Validator /= null
                   and then not V.Simple_Validator (Store.Argument (I)))
                 or else (V.Validator /= null
                          and then not V.Validator (Store,
                                                    Store.Argument (I)))
               then
                  raise Evaluation_Error with
                  Symbols.Get_Name (Name) & ": argument" & Positive'Image (I)
                    & ": invalid actual (found "
                    & Store.Show (Store.Argument (I)) & ")";
               end if;
            end;
         end loop;
      end if;

      begin
         return Fn.Eval (Store);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "error while evaluating primitive "
               & Symbols.Get_Name (Name)
               & ": " & Ada.Exceptions.Exception_Message (E));
            raise;
      end;

   end Evaluate;

   -------------
   -- New_Def --
   -------------

   procedure New_Def
     (Name : String;
      Eval : Root_Function_Interface'Class)
   is
   begin
      Defs.Append
        ((Symbols.Get_Symbol (Name), Function_Type (Defs.Last_Index + 1),
         Function_Interface_Holder.To_Holder (Eval)));
   end New_Def;

   ------------------
   -- Registration --
   ------------------

   package body Registration is

      -----------------------------
      -- Create_Standard_Objects --
      -----------------------------

      procedure Create_Standard_Objects
        (Store : in out Object_Store'Class)
      is
         Is_Type_Name : constant String :=
           (if Type_Predicate_Name = "?"
            then Type_Name & "?"
            else Type_Predicate_Name);
      begin
         if Type_Predicate_Name /= "" then
            declare
               Is_Type_Fn   : constant Object :=
                                Lith.Parser.Parse_Expression
                                  (Store,
                                   "(lambda (x) (#extern-is-type x '"
                                   & Type_Name
                                   & "))");
            begin
               Store.Define_Top_Level
                 (Lith.Objects.Symbols.Get_Symbol (Is_Type_Name), Is_Type_Fn);
            end;
         end if;
      end Create_Standard_Objects;

   end Registration;

end Lith.Objects.Interfaces;

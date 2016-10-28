with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Holders;

with Lith.Parser;
with Lith.Objects.Symbols;

package body Lith.Objects.Interfaces is

   type Simple_Function_Evaluator is
     new Root_Function_Interface with
      record
         Eval : Simple_Evaluator;
      end record;

   overriding function Evaluate
     (Fn : Simple_Function_Evaluator;
      Store : in out Object_Store'Class)
     return Object
   is (Fn.Eval (Store));

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
      Argument_Count : Natural;
      Eval           : Simple_Evaluator)
   is
      pragma Unreferenced (Argument_Count);
   begin
      New_Def (Name,
               Simple_Function_Evaluator'
                 (Root_Function_Interface with Eval => Eval));
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
      Def : constant Root_Function_Interface'Class :=
         Defs.Element (Positive (Fn)).Eval.Element;
   begin
      return Def.Evaluate (Store);
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

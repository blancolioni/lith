with Ada.Containers.Indefinite_Vectors;

with Lith.Environment;
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

   package Function_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Root_Function_Interface'Class);

   Defs : Function_Vectors.Vector;

   procedure New_Def
     (Name : String;
      Eval : Root_Function_Interface'Class);

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
        Defs (Positive (Fn));
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
      Defs.Append (Eval);
      Lith.Environment.Define
        (Name  => Lith.Objects.Symbols.Get_Symbol (Name),
         Value => To_Object (Function_Type (Defs.Last_Index)));
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
               Lith.Environment.Define
                 (Lith.Objects.Symbols.Get_Symbol (Is_Type_Name), Is_Type_Fn);
            end;
         end if;
      end Create_Standard_Objects;

   end Registration;

end Lith.Objects.Interfaces;

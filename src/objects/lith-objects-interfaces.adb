with Ada.Containers.Vectors;

with Lith.Environment;
with Lith.Parser;
with Lith.Objects.Symbols;

package body Lith.Objects.Interfaces is

   type Function_Record (Simple : Boolean := True) is
      record
         Argument_Count : Natural;
         Strict         : Boolean;
         case Simple is
            when False =>
               Env_Eval    : Evaluator;
            when True =>
               Simple_Eval : Simple_Evaluator;
         end case;
      end record;

   package Function_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Function_Record);

   Defs : Function_Vectors.Vector;

   procedure New_Def
     (Name : Wide_Wide_String;
      Rec  : Function_Record);

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name           : Wide_Wide_String;
      Argument_Count : Natural;
      Strict         : Boolean;
      Eval           : Evaluator)
   is
      New_Item : constant Function_Record :=
                   (Simple => False, Argument_Count => Argument_Count,
                    Strict => Strict, Env_Eval => Eval);
   begin
      New_Def (Name, New_Item);
   end Define_Function;

   ---------------------
   -- Define_Function --
   ---------------------

   procedure Define_Function
     (Name           : Wide_Wide_String;
      Argument_Count : Natural;
      Eval           : Simple_Evaluator)
   is
      New_Item : constant Function_Record :=
                   (Simple => True, Argument_Count => Argument_Count,
                    Strict => True, Simple_Eval => Eval);
   begin
      New_Def (Name, New_Item);
   end Define_Function;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Store       : in out Object_Store'Class;
      Fn          : Function_Type;
      Arguments   : Array_Of_Objects;
      Environment : Object)
      return Object
   is
      Def : constant Function_Record := Defs (Positive (Fn));
   begin
      if Def.Simple then
         return Def.Simple_Eval (Store, Arguments);
      else
         return Def.Env_Eval (Store, Arguments, Environment);
      end if;
   end Evaluate;

   -------------
   -- New_Def --
   -------------

   procedure New_Def
     (Name : Wide_Wide_String;
      Rec  : Function_Record)
   is
   begin
      Defs.Append (Rec);
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
         Is_Type_Name : constant Wide_Wide_String :=
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

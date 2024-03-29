with Lith.Symbols;

package body Lith.Objects is

   ------------------
   -- Apply_Object --
   ------------------

   function Apply_Object (Argument_Count : Natural) return Object is
   begin
      return (Object_Payload (Argument_Count), Apply_Object);
   end Apply_Object;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (Item : Object) return Natural is
   begin
      return Natural (Item.Payload);
   end Argument_Count;

   ---------------------
   -- Clear_Temporary --
   ---------------------

   procedure Clear_Temporary
     (Store     : in out Object_Store'Class;
      Temporary : Temporary_Register)
   is
   begin
      Store.Set_Temporary (Temporary, Nil);
   end Clear_Temporary;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Store  : in out Object_Store'Class;
      Length : Natural)
   is
   begin
      Store.Push_Nil;
      for I in 1 .. Length loop
         Store.Cons;
      end loop;
   end Create_List;

   ----------
   -- Drop --
   ----------

   procedure Drop (Store : in out Object_Store'Class;
                   Count : Natural := 1;
                   Stack : Stack_Type := Primary)
   is
      Unused : Object;
   begin
      for I in 1 .. Count loop
         Unused := Store.Pop (Stack);
      end loop;
   end Drop;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Store : in out Object_Store'Class;
      Expr  : Object)
   is
      Unused : constant Object := Store.Evaluate (Expr);
      pragma Unreferenced (Unused);
   begin
      null;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Store      : in out Object_Store'Class;
      Expression : Object;
      Name       : Symbol_Type;
      Value      : Object)
      return Object
   is
   begin
      --  protect expression from gc
      Store.Set_Temporary (1, Expression);

      Store.New_Evaluation_Environment;
      Store.Add_Binding (Name, Value);
      return X : constant Object :=
        Store.Evaluate_With_Environment
          (Store.Get_Temporary (1))
      do
         Store.Close_Evaluation_Environment;
         Store.Clear_Temporary (1);
      end return;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Store      : in out Object_Store'Class;
      Expression : Object;
      Name       : Symbol_Type;
      Value      : Object)
   is
      Unused : constant Object :=
                 Store.Evaluate (Expression, Name, Value);
      pragma Unreferenced (Unused);
   begin
      null;
   end Evaluate;

   -------------------------------
   -- Evaluate_With_Environment --
   -------------------------------

   procedure Evaluate_With_Environment
     (Store      : in out Object_Store'Class;
      Expression : Object)
   is
      Unused : constant Object :=
                 Store.Evaluate_With_Environment (Expression);
      pragma Unreferenced (Unused);
   begin
      null;
   end Evaluate_With_Environment;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Symbol : Symbol_Type)
                      return String
   is
   begin
      return Lith.Symbols.Symbol_To_String (Symbol);
   end Get_Name;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Name : String)
                        return Symbol_Type
   is
   begin
      return Lith.Symbols.String_To_Symbol (Name);
   end Get_Symbol;

   -------------------
   -- Get_Top_Level --
   -------------------

   function Get_Top_Level
     (Store   : Object_Store'Class;
      Name    : Lith.Objects.Symbol_Type;
      Default : Lith.Objects.Object := Lith.Objects.Nil)
      return Lith.Objects.Object
   is
      Found : Boolean;
      Value : Object;
   begin
      Store.Get_Top_Level (Name, Value, Found);
      if not Found then
         Value := Default;
      end if;
      return Value;
   end Get_Top_Level;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Item : Object) return String is

      Payload : Object_Payload := Item.Payload;
      Tag     : constant Character :=
                  (case Item.Tag is
                      when Integer_Object   => 'i',
                      when Pair_Object      => 'p',
                      when Primitive_Object => 'f',
                      when Symbol_Object    => 's',
                      when Apply_Object     => 'a',
                      when Character_Object => 'c',
                      when Internal_Object  => '-',
                      when External_Object  => 'e');

      Result : String (1 .. 8);

      function Hex_Digit
        (Item : Object_Payload)
         return Character;
      --  Item should be in range 0 .. 15

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit
        (Item : Object_Payload)
         return Character
      is
         Hex_Digits : constant String := "0123456789ABCDEF";
      begin
         return Hex_Digits (Positive (Item + 1));
      end Hex_Digit;

   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digit (Payload mod 16);
         Payload := Payload / 16;
      end loop;
      return Result & "-" & Tag;
   end Hex_Image;

   ----------------
   -- Is_Address --
   ----------------

   function Is_Address (Item : Object) return Boolean is
   begin
      return Item.Tag = Pair_Object;
   end Is_Address;

   --------------
   -- Is_Apply --
   --------------

   function Is_Apply (Item : Object) return Boolean is
   begin
      return Item.Tag = Apply_Object;
   end Is_Apply;

   -------------
   -- Is_Atom --
   -------------

   function Is_Atom (Item : Object) return Boolean is
   begin
      return Item.Tag /= Pair_Object;
   end Is_Atom;

   function Is_Character (Item : Object) return Boolean is
   begin
      return Item.Tag = Character_Object;
   end Is_Character;

   ------------------------
   -- Is_External_Object --
   ------------------------

   function Is_External_Object (Item : Object) return Boolean is
   begin
      return Item.Tag = External_Object;
   end Is_External_Object;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Item : Object) return Boolean is
   begin
      return Item.Tag = Primitive_Object;
   end Is_Function;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Item : Object) return Boolean is
   begin
      return Item.Tag = Integer_Object;
   end Is_Integer;

   -------------
   -- Is_Pair --
   -------------

   function Is_Pair (Item : Object) return Boolean is
   begin
      return Item.Tag = Pair_Object;
   end Is_Pair;

   ---------------
   -- Is_String --
   ---------------

   function Is_String
     (Store    : in out Object_Store'Class;
      Item     : Object)
      return Boolean
   is
   begin
      return Is_Pair (Item) and then Store.Car (Item) = String_Value;
   end Is_String;

   ---------------
   -- Is_Symbol --
   ---------------

   function Is_Symbol (Item : Object) return Boolean is
   begin
      return Item.Tag = Symbol_Object;
   end Is_Symbol;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store  : in out Object_Store'Class;
      Symbol : String)
   is
   begin
      Store.Push (To_Symbol_Object (Symbol));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store : in out Object_Store'Class;
      Value   : Integer)
   is
   begin
      Store.Push (To_Object (Value));
   end Push;

   --------------
   -- Push_Nil --
   --------------

   procedure Push_Nil
     (Store : in out Object_Store'Class)
   is
   begin
      Store.Push (Nil);
   end Push_Nil;

   -----------------
   -- Push_String --
   -----------------

   procedure Push_String
     (Store   : in out Object_Store'Class;
      Value   : String)
   is
   begin
      Store.Push (Lith.Objects.String_Value);
      for Ch of Value loop
         Store.Push (Lith.Objects.To_Object (Ch));
      end loop;
      Store.Create_List (Value'Length + 1);
   end Push_String;

   ---------------
   -- Scan_List --
   ---------------

   procedure Scan_List
     (Store   : in out Object_Store'Class;
      Process : not null access
        procedure (Value : Object))
   is
   begin
      Store.Push (Store.Top);
      while Is_Pair (Store.Top) loop
         Process (Store.Car (Store.Top));
         Store.Push (Store.Cdr (Store.Pop));
      end loop;
      Store.Drop;
   end Scan_List;

   ------------------
   -- Single_Quote --
   ------------------

   function Single_Quote return Object is
   begin
      return Lith.Symbols.Quote_Symbol;
   end Single_Quote;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Store : in out Object_Store'Class)
   is
   begin
      Store.Push (Store.Top (2), Secondary);
      Store.Push (Store.Pop, Secondary);
      Store.Drop;
      Store.Push (Store.Pop (Secondary));
      Store.Push (Store.Pop (Secondary));
   end Swap;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Item : Object) return Character is
   begin
      return Character'Val (Item.Payload);
   end To_Character;

   --------------------------------
   -- To_External_Object_Address --
   --------------------------------

   function To_External_Object_Address
     (Item : Object)
      return External_Object_Address
   is
   begin
      return External_Object_Address (Item.Payload);
   end To_External_Object_Address;

   -----------------
   -- To_Function --
   -----------------

   function To_Function
     (Item : Object)
      return Function_Type
   is
   begin
      return Function_Type (Item.Payload);
   end To_Function;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Item : Object)
      return Integer
   is
   begin
      if Item.Payload < 2 ** 28 then
         return Integer (Item.Payload);
      else
         return Integer (Item.Payload) - 2 ** 29;
      end if;
   end To_Integer;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Address : Cell_Address) return Object is
   begin
      return (Object_Payload (Address), Pair_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Fn : Function_Type) return Object is
   begin
      return (Object_Payload (Fn), Primitive_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Value : Integer) return Object is
   begin
      if Value >= 0 then
         return (Object_Payload (Value), Integer_Object);
      else
         return (Object_Payload (Value + 2 ** 29), Integer_Object);
      end if;
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Symbol : Symbol_Type) return Object is
   begin
      return (Object_Payload (Symbol), Symbol_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Ch : Character) return Object is
   begin
      return (Object_Payload
              (Character'Pos (Ch)),
              Character_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Address : External_Object_Address) return Object is
   begin
      return (Object_Payload (Address), External_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Store : in out Object_Store'Class;
                       Value : String)
                       return Object
   is
   begin
      Store.Push (String_Value);
      for Ch of Value loop
         Store.Push (To_Object (Ch));
      end loop;
      Store.Create_List (Value'Length + 1);
      return Store.Pop;
   end To_Object;

   ---------------
   -- To_String --
   ---------------

   function To_String (Store    : in out Object_Store'Class;
                       Item     : Object)
                       return String
   is
      It : Object := Store.Cdr (Item);
      Result : String (1 .. 100);
      Count  : Natural := 0;
   begin
      while It /= Nil loop
         Count := Count + 1;
         if Is_Character (Store.Car (It)) then
            Result (Count) := To_Character (Store.Car (It));
         else
            raise Constraint_Error with
              "String contains non-character: "
              & Store.Show (Store.Car (It));
         end if;
         It := Store.Cdr (It);
      end loop;
      return Result (1 .. Count);
   end To_String;

   ---------------
   -- To_Symbol --
   ---------------

   function To_Symbol
     (Item : Object)
      return Symbol_Type
   is
   begin
      return Symbol_Type (Item.Payload);
   end To_Symbol;

end Lith.Objects;

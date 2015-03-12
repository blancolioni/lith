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

   ----------
   -- Drop --
   ----------

   procedure Drop (Store : in out Object_Store'Class;
                   Count : Positive := 1;
                   Stack : Stack_Type := Primary)
   is
      Unused : Object;
   begin
      for I in 1 .. Count loop
         Unused := Store.Pop (Stack);
      end loop;
   end Drop;

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
                      when Unused_Tag_5     => '5',
                      when Unused_Tag_6     => '6',
                      when Unused_Tag_7     => '7');

      Result : String (1 .. 8);

      function Hex_Digit (Item : Object_Payload) return Character;
      --  Item should be in range 0 .. 15

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit (Item : Object_Payload) return Character is
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
   -- Is_Symbol --
   ---------------

   function Is_Symbol (Item : Object) return Boolean is
   begin
      return Item.Tag = Symbol_Object;
   end Is_Symbol;

   ----------------
   -- To_Address --
   ----------------

   function To_Address
     (Item : Object)
      return Cell_Address
   is
   begin
      return Cell_Address (Item.Payload);
   end To_Address;

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

   ---------------------
   -- To_Object_Array --
   ---------------------

   function To_Object_Array
     (Store : in out Object_Store'Class;
      List  : Object)
      return Array_Of_Objects
   is
      function Go (It : Object) return Array_Of_Objects;

      --------
      -- Go --
      --------

      function Go (It : Object) return Array_Of_Objects is
      begin
         if It = Nil then
            declare
               Result : Array_Of_Objects (1 .. 0);
            begin
               return Result;
            end;
         else
            return Store.Car (It)
              & To_Object_Array (Store, Store.Cdr (It));
         end if;
      end Go;
   begin
      Store.Push (List);
      declare
         Result : constant Array_Of_Objects := Go (List);
         T      : constant Object := Store.Pop;
         pragma Unreferenced (T);
      begin
         return Result;
      end;
   end To_Object_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String (Store    : Object_Store'Class;
                       Item     : Object)
                       return Wide_Wide_String
   is
      It : Object := Store.Cdr (Item);
      Result : Wide_Wide_String (1 .. 100);
      Count  : Natural := 0;
   begin
      while It /= Nil loop
         Count := Count + 1;
         Result (Count) :=
           Wide_Wide_Character'Val (To_Integer (Store.Car (It)));
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

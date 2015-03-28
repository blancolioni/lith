package Lith.Objects is

   Evaluation_Error : exception;

   Word_Size : constant := 32;

   type Object is private;

   Nil         : constant Object;
   True_Value  : constant Object;
   False_Value : constant Object;
   No_Value    : constant Object;

   function To_Object (X : Boolean) return Object
   is (if X then True_Value else False_Value);

   type Cell_Address is mod 2 ** 32
     with Size => 32;

   function Is_Address (Item : Object) return Boolean;
   function To_Object (Address : Cell_Address) return Object;
   function To_Address (Item : Object) return Cell_Address
     with Pre => Is_Address (Item);

   type Symbol_Type is private;

   function Is_Symbol (Item : Object) return Boolean;
   function To_Object (Symbol : Symbol_Type) return Object;
   function To_Symbol (Item : Object) return Symbol_Type
     with Pre => Is_Symbol (Item);

   type Function_Type is private;

   function Is_Function (Item : Object) return Boolean;
   function To_Object (Fn : Function_Type) return Object;
   function To_Function (Item : Object) return Function_Type
     with Pre => Is_Function (Item);

   function Is_Integer (Item : Object) return Boolean;
   function To_Object (Value : Integer) return Object;
   function To_Integer (Item : Object) return Integer
     with Pre => Is_Integer (Item);

   function Is_Apply (Item : Object) return Boolean;
   function Apply_Object (Argument_Count : Natural) return Object;
   function Argument_Count (Item : Object) return Natural
     with Pre => Is_Apply (Item);

   function Is_Character (Item : Object) return Boolean;
   function To_Object (Ch : Wide_Wide_Character) return Object;
   function To_Character (Item : Object) return Wide_Wide_Character
     with Pre => Is_Character (Item);

   type External_Object_Address is new Natural;
   function Is_External_Object (Item : Object) return Boolean;
   function To_Object (Address : External_Object_Address) return Object;
   function To_External_Object_Address
     (Item : Object)
      return External_Object_Address
     with Pre => Is_External_Object (Item);

   function Is_Atom (Item : Object) return Boolean;
   function Is_Pair (Item : Object) return Boolean;

   type Array_Of_Objects is array (Positive range <>) of Object;

   function Hex_Image (Item : Object) return String;

   type Stack_Type is (Primary, Secondary);

   type External_Object_Interface is interface;

   type Object_Store is limited interface;

   function Print (Item  : External_Object_Interface;
                   Store : in out Object_Store'Class)
                   return Wide_Wide_String
                   is abstract;

   function Equal (X, Y  : External_Object_Interface;
                   Store : Object_Store'Class)
                   return Boolean
                   is abstract;

   procedure Mark (Item  : in out External_Object_Interface;
                   Store : in out Object_Store'Class)
   is null;

   procedure Finalize (Item  : in out External_Object_Interface;
                       Store : in out Object_Store'Class)
   is null;

   function Car (Store : Object_Store;
                 Item  : Object)
                 return Object
   is abstract
     with Pre'Class => Is_Pair (Item);

   function Cdr (Store : Object_Store;
                 Item  : Object)
                 return Object
   is abstract
     with Pre'Class => Is_Pair (Item);

   procedure Set_Car (Store   : in out Object_Store;
                      Pair    : in     Object;
                      New_Car : in Object)
   is abstract
     with Pre'Class => Is_Pair (Pair);

   procedure Set_Cdr (Store   : in out Object_Store;
                      Pair    : in     Object;
                      New_Cdr : in Object)
   is abstract
     with Pre'Class => Is_Pair (Pair);

   function Caar (Store : Object_Store'Class;
                  Item  : Object)
                  return Object
     is (Store.Car (Store.Car (Item)));

   function Cadr (Store : Object_Store'Class;
                  Item  : Object)
                  return Object
     is (Store.Car (Store.Cdr (Item)));

   function Cdar (Store : Object_Store'Class;
                  Item  : Object)
                  return Object
     is (Store.Cdr (Store.Car (Item)));

   function Cddr (Store : Object_Store'Class;
                  Item  : Object)
                  return Object
     is (Store.Cdr (Store.Cdr (Item)));

   function Show (Store    : in out Object_Store;
                  Item     : Object)
                  return Wide_Wide_String
                  is abstract;

   function Load (Store    : in out Object_Store;
                  Path     : Wide_Wide_String)
                  return Boolean
                  is abstract;

   function To_String (Store    : in out Object_Store'Class;
                       Item     : Object)
                       return Wide_Wide_String;

   function Cons (Store : in out Object_Store;
                  Car, Cdr : Object)
                  return Object is abstract;

   procedure Cons (Store : in out Object_Store) is abstract;
   --  Pop cdr then car off the stack.  Push (cons car cdr).

   procedure Mark
     (Store : in out Object_Store;
      Start : in     Lith.Objects.Object)
   is abstract;

   procedure Push (Store     : in out Object_Store;
                   Value     : Object;
                   Stack     : Stack_Type := Primary)
   is abstract;

   function Pop (Store : in out Object_Store;
                 Stack     : Stack_Type := Primary)
                 return Object
                 is abstract;

   function Top (Store : Object_Store;
                 Index : Positive := 1;
                 Stack     : Stack_Type := Primary)
                 return Object
                 is abstract;

   procedure Report_State (Store : in out Object_Store) is abstract;

   function Get_External_Object
     (Store : Object_Store;
      Item  : Object)
      return access External_Object_Interface'Class
     is abstract;

   function Create_External_Reference
     (Store    : in out Object_Store;
      External : External_Object_Interface'Class)
      return Object
      is abstract;

   procedure Drop (Store : in out Object_Store'Class;
                   Count : Natural := 1;
                   Stack : Stack_Type := Primary);

   procedure Swap (Store : in out Object_Store'Class);

   procedure Make_List
     (Store : in out Object_Store'Class;
      Items : Array_Of_Objects);
   --  Create a (possibly improper) list from the items in the array,
   --  where each Item is a car except the last.
   --  The list is proper if and only if the last element of Items
   --  is nil, or is a proper list itself.

   function To_Object_Array
     (Store : in out Object_Store'Class;
      List  : Object)
      return Array_Of_Objects;

private

   Payload_Bits : constant := 29;

   type Object_Payload is mod 2 ** 29;
   type Object_Tag is (Integer_Object,
                       Pair_Object,
                       Primitive_Object,
                       Symbol_Object,
                       Apply_Object,
                       Character_Object,
                       Internal_Object,
                       External_Object);

   type Object is
      record
         Payload : Object_Payload;
         Tag     : Object_Tag;
      end record
     with Pack, Size => 32;

   False_Value : constant Object :=
           (Payload => 0,
            Tag     => Internal_Object);

   Nil : constant Object :=
           (Payload => 1,
            Tag     => Internal_Object);

   True_Value : constant Object :=
                  (Payload => 2,
                   Tag     => Internal_Object);

   No_Value : constant Object :=
                (Payload => 3,
                 Tag     => Internal_Object);

   type Symbol_Type is new Object_Payload;

   type Function_Type is new Object_Payload;

end Lith.Objects;

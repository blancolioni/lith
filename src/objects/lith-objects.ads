package Lith.Objects is

   Word_Size : constant := 32;

   type Object is private;

   Nil : constant Object;

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

   function Is_Atom (Item : Object) return Boolean;
   function Is_Pair (Item : Object) return Boolean;

   type Array_Of_Objects is array (Positive range <>) of Object;

   function Hex_Image (Item : Object) return String;

   type Object_Store is limited interface;

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

   function Show (Store    : Object_Store;
                  Item     : Object)
                  return String
                  is abstract;

   function Load (Store    : in out Object_Store;
                  Path     : String)
                  return Boolean
                  is abstract;

   function To_String (Store    : Object_Store'Class;
                       Item     : Object)
                       return String;

   function Cons (Store : in out Object_Store;
                  Car, Cdr : Object)
                  return Object is abstract;

   procedure Mark
     (Store : in out Object_Store;
      Start : in     Lith.Objects.Object)
   is abstract;

   procedure Push (Store : in out Object_Store;
                   Value : Object)
   is abstract;

   function Pop (Store : in out Object_Store) return Object
   is abstract;

   function Top (Store : in out Object_Store) return Object
   is abstract;

   function To_Object_Array
     (Store : in out Object_Store'Class;
      List  : Object)
      return Array_Of_Objects;

private

   type Object_Payload is mod 2 ** 29;
   type Object_Tag is (Integer_Object,
                       Pair_Object,
                       Primitive_Object,
                       Symbol_Object,
                       Apply_Object,
                       Unused_Tag_5,
                       Unused_Tag_6,
                       Unused_Tag_7);

   type Object is
      record
         Payload : Object_Payload;
         Tag     : Object_Tag;
      end record
     with Pack, Size => 32;

   Nil : constant Object :=
           (Payload => 0,
            Tag     => Primitive_Object);

   type Symbol_Type is new Object_Payload;

   type Function_Type is new Object_Payload;

end Lith.Objects;

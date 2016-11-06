package Lith.Objects is

   Evaluation_Error : exception;

   Word_Size : constant := 32;

   Max_Integer : constant := 2 ** 28 - 1;
   Min_Integer : constant := -2 ** 28;

   type Compare_Result is (LT, EQ, GT);

   type Object is private;

   Nil                  : constant Object;
   Undefined            : constant Object;
   True_Value           : constant Object;
   False_Value          : constant Object;
   No_Value             : constant Object;
   String_Value         : constant Object;
   End_Of_File_Object   : constant Object;
   Error_Value          : constant Object;

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
   function In_Object_Range (X : Integer) return Boolean
   is (X in Min_Integer .. Max_Integer);

   function Is_Apply (Item : Object) return Boolean;
   function Apply_Object (Argument_Count : Natural) return Object;
   function Argument_Count (Item : Object) return Natural
     with Pre => Is_Apply (Item);

   function Is_Character (Item : Object) return Boolean;
   function To_Object (Ch : Character) return Object;
   function To_Character (Item : Object) return Character
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

   function Hex_Image (Item : Object) return String;

   type Stack_Type is (Primary, Secondary);

   type External_Object_Interface is interface;

   type Binding_Type is
      record
         Name  : Symbol_Type;
         Value : Object;
      end record;

   type Array_Of_Bindings is array (Positive range <>) of Binding_Type;

   type Object_Store is limited interface;

   function Name
     (Item : External_Object_Interface)
      return String
      is abstract;

   function Print (Item  : External_Object_Interface;
                   Store : in out Object_Store'Class)
                   return String
                   is abstract;

   function Equal (X, Y  : External_Object_Interface;
                   Store : Object_Store'Class)
                   return Boolean
                   is abstract;

   procedure Mark (Item  : in out External_Object_Interface;
                   Store : in out Object_Store'Class;
                   Mark  : not null access
                     procedure (X : in out Object))
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
                  return String
                  is abstract;

   function Load (Store    : in out Object_Store;
                  Path     : String)
                  return Boolean
                  is abstract;

   function Is_String (Store    : in out Object_Store'Class;
                       Item     : Object)
                       return Boolean;

   function To_String (Store    : in out Object_Store'Class;
                       Item     : Object)
                       return String;

   function To_Object (Store : in out Object_Store'Class;
                       Value : String)
                       return Object;

   function Cons (Store : in out Object_Store;
                  Car, Cdr : Object)
                  return Object is abstract;

   procedure Cons (Store : in out Object_Store) is abstract;
   --  Pop cdr then car off the stack.  Push (cons car cdr).

   procedure Reserve_Memory
     (Store : in out Object_Store;
      Minimum : Natural)
   is abstract;

   procedure Mark
     (Store : in out Object_Store;
      Start : in out Lith.Objects.Object)
   is abstract;

   procedure Push (Store     : in out Object_Store;
                   Value     : Object;
                   Stack     : Stack_Type := Primary)
   is abstract;

   procedure Push
     (Store   : in out Object_Store'Class;
      Symbol  : Lith.Objects.Symbol_Type);

   procedure Push
     (Store   : in out Object_Store'Class;
      Value   : Integer);

   procedure Push_String
     (Store   : in out Object_Store'Class;
      Value   : String);

   procedure Push_Nil
     (Store : in out Object_Store'Class);

   function Pop (Store : in out Object_Store;
                 Stack     : Stack_Type := Primary)
                 return Object
                 is abstract;

   function Top (Store : Object_Store;
                 Index : Positive := 1;
                 Stack     : Stack_Type := Primary)
                 return Object
                 is abstract;

   function Evaluate (Store : in out Object_Store;
                      Expr  : Object)
                      return Object
                      is abstract;

   procedure Evaluate
     (Store : in out Object_Store'Class;
      Expr  : Object);

   procedure New_Environment
     (Store : in out Object_Store)
   is abstract;

   procedure Create_Binding
     (Store : in out Object_Store;
      Name  : Symbol_Type;
      Value : Object)
   is abstract;

   procedure Pop_Environment
     (Store : in out Object_Store)
   is abstract;

   procedure Set_Context
     (Store       : in out Object_Store;
      File_Name   : String;
      Line_Number : Natural)
   is abstract;

   procedure Reset
     (Store : in out Object_Store)
   is abstract;
   --  clear everything except the global environment

   procedure Define_Top_Level
     (Store : in out Object_Store;
      Name  : Lith.Objects.Symbol_Type;
      Value : Lith.Objects.Object)
   is abstract;
   --  define a top-level value

   procedure Get_Top_Level
     (Store : Object_Store;
      Name  : Lith.Objects.Symbol_Type;
      Value : out Lith.Objects.Object;
      Found : out Boolean)
   is abstract;

   function Get_Top_Level
     (Store   : Object_Store'Class;
      Name    : Lith.Objects.Symbol_Type;
      Default : Lith.Objects.Object := Lith.Objects.Nil)
      return Lith.Objects.Object;

   procedure Create_List
     (Store : in out Object_Store'Class;
      Length : Natural);
   --  Turn the Length items on top of stack into a list, and push it
   --  onto the stack.  The new list terminates with Nil, and the
   --  top of the stack is that last element;
   --  Equivalent to: push_nil, cons, ..., cons

   procedure Scan_List
     (Store : in out Object_Store'Class;
      Process : not null access
        procedure (Value : Object));
   --  Call Process for each value of the list found at the top of the stack

   function Argument_Count (Store : Object_Store)
                            return Natural
                            is abstract;
   --  Number of arguments passed to the current function

   function Argument (Store : Object_Store;
                      Index : Positive)
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

--     function Push_List
--       (Store : in out Object_Store'Class;
--        List  : Object)
--        return Natural;
   --  pushes each element of List onto the stack.
   --  returns the number of objects pushed.

--     procedure Reverse_List
--       (Store : in out Object_Store'Class;
--        List  : in out Object);

   type Evaluation_Hook_Interface is interface;

   function Call
     (Hook      : in out Evaluation_Hook_Interface;
      Arguments : Object)
      return Object
      is abstract;

   type Evaluation_Hook is access all Evaluation_Hook_Interface'Class;

   procedure Add_Hook (Store : in out Object_Store;
                       Name    : String;
                       Hook    : Evaluation_Hook)
   is abstract;

   function Call_Hook
     (Store     : in out Object_Store;
      Name      : String;
      Arguments : Object)
      return Object
      is abstract;

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
         Payload : Object_Payload := 1;
         Tag     : Object_Tag     := Internal_Object;
      end record
     with Pack, Size => 32;

   False_Value : constant Object :=
           (Payload => 0,
            Tag     => Internal_Object);

   Nil : constant Object :=
           (Payload => 1,
            Tag     => Internal_Object);

   Undefined : constant Object :=
                 (Payload => 2,
                  Tag     => Internal_Object);

   True_Value : constant Object :=
                  (Payload => 3,
                   Tag     => Internal_Object);

   No_Value : constant Object :=
                (Payload => 4,
                 Tag     => Internal_Object);

   String_Value : constant Object :=
                    (Payload => 5,
                     Tag     => Internal_Object);

   End_Of_File_Object  : constant Object :=
                           (Payload => 6,
                            Tag     => Internal_Object);

   Error_Value  : constant Object :=
                    (Payload => 7,
                     Tag     => Internal_Object);

   type Symbol_Type is new Object_Payload;

   type Function_Type is new Object_Payload;

end Lith.Objects;

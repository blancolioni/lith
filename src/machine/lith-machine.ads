private with Ada.Calendar;
private with Ada.Containers.Vectors;

with Lith.Objects;

package Lith.Machine is

   type Root_Lith_Machine is limited new Lith.Objects.Object_Store
   with private;

   type Lith_Machine is access all Root_Lith_Machine'Class;

   function Create
     (Core_Size : Positive)
      return Lith_Machine;

   overriding procedure Push
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary);

   procedure Push
     (Machine : in out Root_Lith_Machine'Class;
      Symbol  : Lith.Objects.Symbol_Type);

   procedure Push
     (Machine : in out Root_Lith_Machine'Class;
      Symbol_Name : Wide_Wide_String);

   procedure Push
     (Machine : in out Root_Lith_Machine'Class;
      Value   : Integer);

   overriding function Pop
     (Machine : in out Root_Lith_Machine;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary)
      return Lith.Objects.Object;

   overriding function Top
     (Machine : Root_Lith_Machine;
      Index   : Positive := 1;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary)
      return Lith.Objects.Object;

   overriding procedure Cons
     (Machine : in out Root_Lith_Machine);

   overriding function Cons
     (Machine  : in out Root_Lith_Machine;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object;

   function Car
     (Machine : in out Root_Lith_Machine'Class)
      return Lith.Objects.Object;
   --  pop the top of the stack and return its car

   function Cdr
     (Machine : in out Root_Lith_Machine'Class)
      return Lith.Objects.Object;
   --  pop the top of the stack and return its cdr

   overriding function Car
     (Machine : Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Lith.Objects.Object
     with Pre => Lith.Objects.Is_Pair (Value);

   overriding function Cdr
     (Machine : Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Lith.Objects.Object
     with Pre => Lith.Objects.Is_Pair (Value);

   overriding procedure Set_Car
     (Machine : in out Root_Lith_Machine;
      Pair    : in     Lith.Objects.Object;
      New_Car : in Lith.Objects.Object);

   overriding procedure Set_Cdr
     (Machine : in out Root_Lith_Machine;
      Pair    : in     Lith.Objects.Object;
      New_Cdr : in Lith.Objects.Object);

   function Evaluate
     (Machine     : in out Root_Lith_Machine'Class;
      Expression  : Lith.Objects.Object;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object;

   overriding function Show
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object)
      return Wide_Wide_String;

   procedure Report_Memory
     (Machine : Root_Lith_Machine'Class);

   overriding procedure Report_State
     (Machine : in out Root_Lith_Machine);

   overriding function Get_External_Object
     (Machine : Root_Lith_Machine;
      Item    : Lith.Objects.Object)
      return access Lith.Objects.External_Object_Interface'Class;

   overriding function Create_External_Reference
     (Machine : in out Root_Lith_Machine;
      External : Lith.Objects.External_Object_Interface'Class)
      return Lith.Objects.Object;

   overriding procedure Mark
     (Machine : in out Root_Lith_Machine;
      Start   : in     Lith.Objects.Object);

   overriding function Load (Machine : in out Root_Lith_Machine;
                             Path    : Wide_Wide_String)
                             return Boolean;

private

   type Object_Pair is
      record
         Car, Cdr : Lith.Objects.Object;
      end record
     with Pack, Size => 64;

   type Core_Memory_Type is
     array (Lith.Objects.Cell_Address range <>) of Object_Pair;

   type Memory_Tag_Type is
     array (Lith.Objects.Cell_Address range <>) of Boolean
     with Pack;

   type External_Object_Access is
     access all Lith.Objects.External_Object_Interface'Class;

   type External_Object_Record is
      record
         External_Object : External_Object_Access;
         Marked          : Boolean;
         Free            : Boolean;
      end record;

   subtype Real_External_Address is
     Lith.Objects.External_Object_Address
   range 1 .. Lith.Objects.External_Object_Address'Last;

   package External_Object_Vectors is
     new Ada.Containers.Vectors
       (Real_External_Address,
        External_Object_Record);

   type Root_Lith_Machine is limited new Lith.Objects.Object_Store with
      record
         Core             : access Core_Memory_Type;
         Marked           : access Memory_Tag_Type;
         Free             : access Memory_Tag_Type;
         Stack            : Lith.Objects.Object;
         Environment      : Lith.Objects.Object;
         Control          : Lith.Objects.Object;
         Dump             : Lith.Objects.Object;
         Handlers         : Lith.Objects.Object;
         Free_List        : Lith.Objects.Object;
         R1, R2           : Lith.Objects.Object := Lith.Objects.Nil;
         G1, G2           : Lith.Objects.Object := Lith.Objects.Nil;
         Alloc_Count      : Natural;
         Alloc_Limit      : Natural;
         GC_Count         : Natural  := 0;
         GC_Time          : Duration := 0.0;
         Eval_Time        : Duration := 0.0;
         Start_Eval       : Ada.Calendar.Time;
         Evaluating       : Boolean := False;
         Allocations      : Natural := 0;
         Collections      : Natural := 0;
         External_Objects : External_Object_Vectors.Vector;
      end record;

   function Allocate
     (Machine  : in out Root_Lith_Machine'Class;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object;

   procedure GC
     (Machine : in out Root_Lith_Machine'Class);

end Lith.Machine;

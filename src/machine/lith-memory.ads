with Lith.Objects;

package Lith.Memory is

   type Lith_Memory is private;

   function Valid (Memory  : Lith_Memory;
                   Address : Lith.Objects.Cell_Address)
                   return Boolean;

   procedure Get
     (Memory    : Lith_Memory;
      Address   : Lith.Objects.Cell_Address;
      Car, Cdr  : out Lith.Objects.Object)
     with Pre => Valid (Memory, Address);

   function Car (Memory : Lith_Memory;
                 Address : Lith.Objects.Cell_Address)
                 return Lith.Objects.Object
     with Pre => Valid (Memory, Address);

   function Cdr (Memory : Lith_Memory;
                 Address : Lith.Objects.Cell_Address)
                 return Lith.Objects.Object
     with Pre => Valid (Memory, Address);

   procedure Set
     (Memory            : in out Lith_Memory;
      Address           : Lith.Objects.Cell_Address;
      New_Car, New_Cdr  : Lith.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => Lith.Objects."=" (Car (Memory, Address), New_Car)
     and then Lith.Objects."=" (Cdr (Memory, Address), New_Cdr);

   procedure Set_Car
     (Memory  : in out Lith_Memory;
      Address : Lith.Objects.Cell_Address;
      New_Car : Lith.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => Lith.Objects."=" (Car (Memory, Address), New_Car);

   procedure Set_Cdr
     (Memory  : in out Lith_Memory;
      Address : Lith.Objects.Cell_Address;
      New_Cdr : Lith.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => Lith.Objects."=" (Cdr (Memory, Address), New_Cdr);

   function Allocate
     (Memory    : in out Lith_Memory;
      Car, Cdr  : Lith.Objects.Object)
      return Lith.Objects.Object
     with Post => Lith.Objects.Is_Pair (Allocate'Result)
     and then Valid (Memory, Lith.Objects.To_Address (Allocate'Result));

   type GC_Interface is limited interface;

   procedure Before_GC (GC : in out GC_Interface) is null;
   procedure After_GC (GC : in out GC_Interface) is null;
   procedure Mark_External_Object
     (GC       : in out GC_Interface;
      External : Lith.Objects.External_Object_Address;
      Mark     : not null access
        procedure (X : in out Lith.Objects.Object))
   is null;

   type GC_Callback is access all GC_Interface'Class;

   function Create
     (Core_Size : Lith.Objects.Cell_Address;
      Callback  : GC_Callback)
      return Lith_Memory;

   procedure Report
     (Memory : Lith_Memory);

   procedure Mark (Memory : in out Lith_Memory;
                   Item   : in out Lith.Objects.Object);

   procedure Reserve_Memory
     (Memory  : in out Lith_Memory;
      Minimum : Natural);

private

   type Object_Pair is
      record
         Car, Cdr : Lith.Objects.Object;
      end record
     with Pack, Size => 64;

   type Core_Memory_Type is
     array (Lith.Objects.Cell_Address range <>) of Object_Pair;

   type Lith_Memory is
      record
         Core        : access Core_Memory_Type;
         Top         : Lith.Objects.Cell_Address;
         Free        : Lith.Objects.Cell_Address;
         From_Space  : Lith.Objects.Cell_Address;
         To_Space    : Lith.Objects.Cell_Address;
         Space_Size  : Lith.Objects.Cell_Address;
         Scan        : Lith.Objects.Cell_Address;
         Callback    : GC_Callback;
         Alloc_Car   : Lith.Objects.Object;
         Alloc_Cdr   : Lith.Objects.Object;
         Test        : Lith.Objects.Object;
         Alloc_Count : Natural;
         GC_Count    : Natural  := 0;
         GC_Time     : Duration := 0.0;
         Allocations : Natural := 0;
         Collections : Natural := 0;
      end record;

end Lith.Memory;

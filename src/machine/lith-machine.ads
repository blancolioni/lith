private with Ada.Calendar;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;

private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Fixed.Equal_Case_Insensitive;

with Lith.Objects;

private with Lith.Objects.Symbol_Maps;

package Lith.Machine is

   type Root_Lith_Machine is limited new Lith.Objects.Object_Store
   with private;

   type Lith_Machine is access all Root_Lith_Machine'Class;

   function Create
     (Core_Size : Positive)
      return Lith_Machine;

   procedure Start_Profile
     (Machine : in out Root_Lith_Machine'Class);

   procedure Finish_Profile
     (Machine : in out Root_Lith_Machine'Class);

   procedure Report_Profile
     (Machine           : in out Root_Lith_Machine'Class;
      Procedure_Profile : Boolean;
      Max_Lines         : Natural := 20);

   overriding procedure Push
     (Machine : in out Root_Lith_Machine;
      Value   : Lith.Objects.Object;
      Stack   : Lith.Objects.Stack_Type := Lith.Objects.Primary);

   procedure Push
     (Machine : in out Root_Lith_Machine;
      Symbol_Name : Wide_Wide_String);

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

   overriding function Evaluate
     (Machine     : in out Root_Lith_Machine;
      Expression  : Lith.Objects.Object;
      Environment : Lith.Objects.Object)
      return Lith.Objects.Object;

   overriding procedure Set_Context
     (Machine     : in out Root_Lith_Machine;
      File_Name   : Wide_Wide_String;
      Line        : Natural);

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

   type File_Id is range 0 .. 255;
   subtype Real_File_Id is File_Id range 1 .. File_Id'Last;
   type Line_Number is mod 2**24;
   type Source_Reference is
      record
         File : File_Id;
         Line : Line_Number;
      end record
     with Pack, Size => 32;

   function "<" (Left, Right : Source_Reference) return Boolean
   is (Left.File < Right.File
       or else (Left.File = Right.File
                and then Left.Line < Right.Line));

   function Show (Machine : Root_Lith_Machine'Class;
                  Ref     : Source_Reference)
                  return Wide_Wide_String;

   package Source_File_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Real_File_Id,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive);

   package Source_File_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Index_Type   => Real_File_Id,
        Element_Type => Wide_Wide_String);

   type Profile_Info_Record is
      record
         Hit_Count : Natural := 0;
      end record;

   package Profile_Source_Maps is
     new Ada.Containers.Ordered_Maps
       (Source_Reference, Profile_Info_Record);

   package Procedure_Profile_Maps is
     new Lith.Objects.Symbol_Maps (Profile_Info_Record);

   type Profile_Result_Record is
      record
         Reference : Source_Reference;
         Proc      : Lith.Objects.Object;
         Info      : Profile_Info_Record;
      end record;

   package Profile_Result_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Profile_Result_Record);

   type Memory_Source_Reference_Type is
     array (Lith.Objects.Cell_Address range <>) of Source_Reference
     with Pack;

   type Root_Lith_Machine is limited new Lith.Objects.Object_Store with
      record
         Core              : access Core_Memory_Type;
         Marked            : access Memory_Tag_Type;
         Free              : access Memory_Tag_Type;
         Source_Refs       : access Memory_Source_Reference_Type;
         Stack             : Lith.Objects.Object;
         Environment       : Lith.Objects.Object;
         Control           : Lith.Objects.Object;
         Dump              : Lith.Objects.Object;
         Handlers          : Lith.Objects.Object;
         Free_List         : Lith.Objects.Object;
         R1, R2            : Lith.Objects.Object := Lith.Objects.Nil;
         G1, G2            : Lith.Objects.Object := Lith.Objects.Nil;
         Alloc_Count       : Natural;
         Alloc_Limit       : Natural;
         GC_Count          : Natural  := 0;
         GC_Time           : Duration := 0.0;
         Eval_Time         : Duration := 0.0;
         Start_Eval        : Ada.Calendar.Time;
         Evaluating        : Boolean := False;
         Allocations       : Natural := 0;
         Collections       : Natural := 0;
         External_Objects  : External_Object_Vectors.Vector;
         Source_Files      : Source_File_Maps.Map;
         Source_File_Names : Source_File_Vectors.Vector;
         Current_Context   : Source_Reference;
         Source_Profile    : Profile_Source_Maps.Map;
         Procedure_Profile : Procedure_Profile_Maps.Map;
         Source_Result     : Profile_Result_Lists.List;
         Procedure_Result  : Profile_Result_Lists.List;
         Profiling         : Boolean := False;
      end record;

   function Allocate
     (Machine  : in out Root_Lith_Machine'Class;
      Car, Cdr : Lith.Objects.Object)
      return Lith.Objects.Object;

   procedure GC
     (Machine : in out Root_Lith_Machine'Class);

   procedure Set_Context
     (Machine : in out Root_Lith_Machine'Class;
      Item    : Lith.Objects.Object);

   procedure Hit
     (Machine  : in out Root_Lith_Machine'Class;
      Item     : Lith.Objects.Object);

end Lith.Machine;

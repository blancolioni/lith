with Ada.Calendar;
with Ada.Wide_Wide_Text_IO;

package body Lith.Memory is

   use Lith.Objects;

   Trace_GC : constant Boolean := False;

   function From_Space (Memory : Lith_Memory;
                        Item   : Object)
                        return Boolean;

   function To_Space (Memory : Lith_Memory;
                      Item   : Object)
                      return Boolean;

   function Copy (Memory : in out Lith_Memory;
                  Item   : Cell_Address)
                  return Cell_Address;

   function Move (Memory : in out Lith_Memory;
                  Item   : Object)
                  return Object;

   procedure Flip (Memory : in out Lith_Memory);

   procedure GC
     (Memory : in out Lith_Memory);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Memory    : in out Lith_Memory;
      Car, Cdr  : Lith.Objects.Object)
      return Lith.Objects.Object
   is
   begin

      Memory.Alloc_Car := Car;
      Memory.Alloc_Cdr := Cdr;

      if Memory.Free = Memory.Top then
         GC (Memory);
         if Memory.Free = Memory.Top then
            raise Constraint_Error with "out of memory";
         end if;
      end if;

      declare
         Result : constant Object := To_Object (Memory.Free);
      begin
         Memory.Core (Memory.Free) := (Memory.Alloc_Car, Memory.Alloc_Cdr);
         Memory.Free := Memory.Free + 1;
         Memory.Alloc_Count := Memory.Alloc_Count + 1;
         Memory.Allocations := Memory.Allocations + 1;
         return Result;
      end;
   end Allocate;

   ---------
   -- Car --
   ---------

   function Car (Memory : Lith_Memory;
                 Address : Lith.Objects.Cell_Address)
                 return Lith.Objects.Object
   is
   begin
      return Memory.Core (Address).Car;
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr (Memory : Lith_Memory;
                 Address : Lith.Objects.Cell_Address)
                 return Lith.Objects.Object
   is
   begin
      return Memory.Core (Address).Cdr;
   end Cdr;

   ----------
   -- Copy --
   ----------

   function Copy (Memory : in out Lith_Memory;
                  Item   : Cell_Address)
                  return Cell_Address
   is
      Result : constant Cell_Address := Memory.Free;
   begin
      Memory.Core (Memory.Free) := Memory.Core (Item);
      Memory.Free := Memory.Free + 1;
      return Result;
   end Copy;

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size : Lith.Objects.Cell_Address;
      Callback  : GC_Callback)
      return Lith_Memory
   is
      Top_Address : constant Cell_Address := Core_Size - 1;
      Space_Size  : constant Cell_Address := Core_Size / 2;
      To_Space    : constant Cell_Address := 0;
      From_Space  : constant Cell_Address := Space_Size;
   begin
      return Lith_Memory'
        (Core             => new Core_Memory_Type (0 .. Top_Address),
         Top              => To_Space + Space_Size,
         Free             => To_Space,
         From_Space       => From_Space,
         To_Space         => To_Space,
         Space_Size       => Space_Size,
         Scan             => 0,
         Callback         => Callback,
         Alloc_Car        => Nil,
         Alloc_Cdr        => Nil,
         Test             => Nil,
         Alloc_Count      => 0,
         GC_Count         => 0,
         GC_Time          => 0.0,
         Allocations      => 0,
         Collections      => 0);
   end Create;

   ----------
   -- Flip --
   ----------

   procedure Flip (Memory : in out Lith_Memory) is
      T : constant Cell_Address := Memory.To_Space;
   begin
      Memory.To_Space := Memory.From_Space;
      Memory.From_Space := T;
      Memory.Top  := Memory.To_Space + Memory.Space_Size;
      Memory.Free := Memory.To_Space;
      Memory.Scan := Memory.To_Space;

      if Trace_GC then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Flip: To_Space = " &
              Hex_Image (To_Object (Memory.To_Space)));
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Flip: From_Space = " &
              Hex_Image (To_Object (Memory.From_Space)));
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Flip: free = " &
              Hex_Image (To_Object (Memory.Free)));
      end if;
   end Flip;

   ----------------
   -- From_Space --
   ----------------

   function From_Space
     (Memory : Lith_Memory;
      Item   : Object)
      return Boolean
   is
   begin
      return Is_Address (Item) and then
        To_Address (Item) in
        Memory.From_Space .. Memory.From_Space + Memory.Space_Size - 1;
   end From_Space;

   --------
   -- GC --
   --------

   procedure GC
     (Memory : in out Lith_Memory)
   is

      use Ada.Calendar;
      Start           : constant Time := Clock;
      Old_Alloc_Count : constant Natural := Memory.Alloc_Count;

   begin
      if Trace_GC then
         Ada.Wide_Wide_Text_IO.Put_Line ("Garbage collecting ...");
      end if;

      Flip (Memory);

      Mark (Memory, Memory.Alloc_Car);
      Mark (Memory, Memory.Alloc_Cdr);
      Mark (Memory, Memory.Test);

      if Memory.Callback /= null then
         Memory.Callback.Before_GC;
      end if;

      while Memory.Scan < Memory.Free loop
         declare
            Cell    : Object_Pair renames Memory.Core (Memory.Scan);
            New_Car : constant Object := Move (Memory, Cell.Car);
            New_Cdr : constant Object := Move (Memory, Cell.Cdr);
         begin
            Cell := (New_Car, New_Cdr);
            Memory.Scan := Memory.Scan + 1;
         end;
      end loop;

      Memory.Alloc_Count :=
        Natural (Memory.Scan - Memory.To_Space);

      if Trace_GC then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("GC freed"
            & Integer'Wide_Wide_Image
              (Old_Alloc_Count - Memory.Alloc_Count)
            & " cells in"
            & Duration'Wide_Wide_Image ((Clock - Start) * 1000.0)
            & "ms");
      end if;

      Memory.Collections := Memory.Collections +
        (Old_Alloc_Count - Memory.Alloc_Count);
      Memory.GC_Time := Memory.GC_Time + (Clock - Start);
      Memory.GC_Count := Memory.GC_Count + 1;

--        if Memory.GC_Count = 16 then
--           Ada.Wide_Wide_Text_IO.Put_Line
--             ("GC number" & Natural'Wide_Wide_Image (Memory.GC_Count)
--              & " complete");
--        end if;

      if Memory.Callback /= null then
         Memory.Callback.After_GC;
      end if;
   end GC;

   ---------
   -- Get --
   ---------

   procedure Get
     (Memory    : Lith_Memory;
      Address   : Lith.Objects.Cell_Address;
      Car, Cdr  : out Lith.Objects.Object)
   is
      Pair : Object_Pair renames Memory.Core (Address);
   begin
      Car := Pair.Car;
      Cdr := Pair.Cdr;
   end Get;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (Memory : in out Lith_Memory;
      Item   : in out Lith.Objects.Object)
   is
   begin
      Item := Move (Memory, Item);
   end Mark;

   ----------
   -- Move --
   ----------

   function Move (Memory : in out Lith_Memory;
                  Item   : Object)
                  return Object
   is
      Addr : Cell_Address;
   begin
      if Is_External_Object (Item) then
         Memory.Callback.Mark_External_Object
           (To_External_Object_Address (Item));
      end if;

      if not From_Space (Memory, Item) then
         return Item;
      else
         Addr := To_Address (Item);
         if not To_Space (Memory, Memory.Core (Addr).Car) then
            Memory.Core (Addr).Car :=
              To_Object (Copy (Memory, Addr));
         end if;
         return Memory.Core (Addr).Car;
      end if;
   end Move;

   ------------
   -- Report --
   ------------

   procedure Report
     (Memory : Lith_Memory)
   is

      function Hex_Image (Addr : Cell_Address) return String;
      pragma Unreferenced (Hex_Image);

      function Hex_Image (Addr : Cell_Address) return String is

         Tmp     : Cell_Address := Addr;
         Result  : String (1 .. 8);

         function Hex_Digit (Item : Cell_Address) return Character
           with Pre => Item <= 15;

         ---------------
         -- Hex_Digit --
         ---------------

         function Hex_Digit (Item : Cell_Address) return Character is
            Hex_Digits : constant String := "0123456789ABCDEF";
         begin
            return Hex_Digits (Positive (Item + 1));
         end Hex_Digit;

      begin
         for I in reverse Result'Range loop
            Result (I) := Hex_Digit (Tmp mod 16);
            Tmp := Tmp / 16;
         end loop;
         if Result (1 .. 4) = "0000" then
            return Result (5 .. 8);
         else
            return Result (1 .. 4) & " " & Result (5 .. 8);
         end if;
      end Hex_Image;

   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Total number of cells:"
         & Cell_Address'Wide_Wide_Image (Memory.Core'Length));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Allocated cell count: "
         & Natural'Wide_Wide_Image (Memory.Alloc_Count));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Free cell count: "
         & Natural'Wide_Wide_Image
           (Natural (Memory.Core'Length
            - Memory.Alloc_Count)));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("GC:"
         & Natural'Wide_Wide_Image (Memory.GC_Count)
         & " @"
         & Natural'Wide_Wide_Image (Natural (Memory.GC_Time * 1000.0))
         & "ms");
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Allocated cells:"
         & Natural'Wide_Wide_Image (Memory.Allocations));
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Reclaimed cells:"
         & Natural'Wide_Wide_Image (Memory.Collections));

   end Report;

   --------------------
   -- Reserve_Memory --
   --------------------

   procedure Reserve_Memory
     (Memory  : in out Lith_Memory;
      Minimum : Natural)
   is
   begin
      if Natural (Memory.Top) - Natural (Memory.Free) < Minimum then
         Memory.Alloc_Car := Lith.Objects.Nil;
         Memory.Alloc_Cdr := Lith.Objects.Nil;
         GC (Memory);
         if Natural (Memory.Top) - Natural (Memory.Free) < Minimum then
            raise Constraint_Error with "out of memory";
         end if;
      end if;
   end Reserve_Memory;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory            : in out Lith_Memory;
      Address           : Lith.Objects.Cell_Address;
      New_Car, New_Cdr  : Lith.Objects.Object)
   is
      Pair : Object_Pair renames Memory.Core (Address);
   begin
      Pair.Car := New_Car;
      Pair.Cdr := New_Cdr;
   end Set;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car
     (Memory  : in out Lith_Memory;
      Address : Lith.Objects.Cell_Address;
      New_Car : Lith.Objects.Object)
   is
      Pair : Object_Pair renames Memory.Core (Address);
   begin
      Pair.Car := New_Car;
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr
     (Memory  : in out Lith_Memory;
      Address : Lith.Objects.Cell_Address;
      New_Cdr : Lith.Objects.Object)
   is
      Pair : Object_Pair renames Memory.Core (Address);
   begin
      Pair.Cdr := New_Cdr;
   end Set_Cdr;

   --------------
   -- To_Space --
   --------------

   function To_Space
     (Memory : Lith_Memory;
      Item   : Object)
      return Boolean
   is
   begin
      return Is_Address (Item) and then
        To_Address (Item) in
        Memory.To_Space .. Memory.To_Space + Memory.Space_Size - 1;
   end To_Space;

   -----------
   -- Valid --
   -----------

   function Valid (Memory  : Lith_Memory;
                   Address : Lith.Objects.Cell_Address)
                   return Boolean
   is
   begin
      return Address in Memory.To_Space .. Memory.Free;
   end Valid;

end Lith.Memory;

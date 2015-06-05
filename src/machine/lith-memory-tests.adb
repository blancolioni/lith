with Ada.Text_IO;

package body Lith.Memory.Tests is

   ---------------
   -- Self_Test --
   ---------------

   procedure Self_Test (Memory : in out Lith_Memory) is
      use Lith.Objects;
   begin

      Ada.Text_IO.Put_Line ("begin self test");

      for I in 1 .. 10_000 loop

         Memory.Test := Nil;

         for J in 1 .. 100 loop
            declare
               X    : constant Object := To_Object (I * J);
               Item : constant Object :=
                        Allocate (Memory, X, X);
            begin
               Set_Cdr (Memory, To_Address (Item), Memory.Test);
               Memory.Test := Item;
            end;
         end loop;

         declare
            It : Object := Memory.Test;
         begin
            for J in reverse 1 .. 100 loop
               pragma Assert (Car (Memory, To_Address (It))
                              = To_Object (I * J));
               It := Cdr (Memory, To_Address (It));
            end loop;
            pragma Assert (It = Nil);
         end;
      end loop;

      Ada.Text_IO.Put_Line ("self test complete");
   end Self_Test;

end Lith.Memory.Tests;

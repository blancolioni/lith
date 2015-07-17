with Lith.Init;

package body Lith.Library is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise is
   begin
      Lith.Init.Start_Lith;
   end Initialise;

   ----------
   -- Load --
   ----------

   procedure Load (Path : String) is
      Success : constant Boolean :=
                  Lith.Init.Main_Store.Load (Path);
   begin
      if not Success then
         raise Constraint_Error with "load failed";
      end if;
   end Load;

end Lith.Library;
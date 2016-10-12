with Lith.Init;
with Lith.Parser;
with Lith.Repl;

with Lith.Paths;

package body Lith.Library is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True)
   is
   begin
      Lith.Init.Start_Lith (Core_Size);
      if Import_Scheme_Base then
         Lith.Parser.Parse_File
           (Store.all,
            Lith.Paths.Config_Path & "/interaction-environment.scm");
      end if;
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

   ----------------
   -- Start_Repl --
   ----------------

   procedure Start_Repl is
   begin
      Lith.Repl.Execute (Lith.Init.Main_Store);
   end Start_Repl;

   -----------
   -- Store --
   -----------

   function Store return access Lith.Objects.Object_Store'Class is
   begin
      return Lith.Init.Main_Store;
   end Store;

end Lith.Library;

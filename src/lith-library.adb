with Ada.Unchecked_Deallocation;

with Lith.Init;
with Lith.Machine;
with Lith.Parser;
with Lith.Repl;

with Lith.Paths;

package body Lith.Library is

   --------------------
   -- Close_Instance --
   --------------------

   procedure Close_Instance (Instance : in out Object_Store_Instance) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Lith.Objects.Object_Store'Class,
                                        Object_Store_Instance);
   begin
      Free (Instance);
      Instance := null;
   end Close_Instance;

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
           (Library_Store.all,
            Lith.Paths.Config_Path & "/interaction-environment.scm");
      end if;
   end Initialise;

   -------------------
   -- Library_Store --
   -------------------

   function Library_Store return access Lith.Objects.Object_Store'Class is
   begin
      return Lith.Init.Main_Store;
   end Library_Store;

   ----------
   -- Load --
   ----------

   procedure Load
     (Path : String;
      To_Store : access Lith.Objects.Object_Store'Class := null)
   is
      Success : constant Boolean :=
                  (if To_Store = null
                   then Lith.Init.Main_Store.Load (Path)
                   else To_Store.Load (Path));
   begin
      if not Success then
         raise Constraint_Error with "load failed";
      end if;
   end Load;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True)
      return Object_Store_Instance
   is
      Machine : constant Lith.Machine.Lith_Machine :=
                  Lith.Machine.Create (Core_Size);
   begin
      Lith.Init.Init_Store (Machine.all);

      if Import_Scheme_Base then
         Lith.Parser.Parse_File
           (Machine.all,
            Lith.Paths.Config_Path & "/interaction-environment.scm");
      end if;
      return Object_Store_Instance (Machine);
   end New_Instance;

   ----------------
   -- Start_Repl --
   ----------------

   procedure Start_Repl
     (With_Store : access Lith.Objects.Object_Store'Class := null)
   is
   begin
      if With_Store = null then
         Lith.Repl.Execute (Lith.Init.Main_Store);
      else
         Lith.Repl.Execute (With_Store);
      end if;
   end Start_Repl;

end Lith.Library;

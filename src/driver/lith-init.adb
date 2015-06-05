with Lith.Bytevectors;
with Lith.Machine;
with Lith.IO;
with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Vectors;

package body Lith.Init is

   Library_Machine : Lith.Machine.Lith_Machine;

   ----------------
   -- Main_Store --
   ----------------

   function Main_Store
     return access Lith.Objects.Object_Store'Class
   is
   begin
      return Library_Machine;
   end Main_Store;

   ----------------
   -- Start_Lith --
   ----------------

   procedure Start_Lith is
      Core_Size       : constant := 64 * 1024;
   begin

      Library_Machine := Lith.Machine.Create (Core_Size);

      Lith.Primitives.Add_Primitives;
      Lith.IO.Initialise_IO;
      Lith.Vectors.Register (Library_Machine.all);
      Lith.Bytevectors.Register (Library_Machine.all);

      Lith.Parser.Parse_File
        (Library_Machine.all,
         Lith.Paths.Config_Path & "/lith.l");
   end Start_Lith;

end Lith.Init;

with Ada.Wide_Wide_Text_IO;

with Lith.IO;
with Lith.Machine;

with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Repl;

with Lith.Vectors;
with Lith.Bytevectors;

procedure Lith.Driver is
   Core_Size : constant := 256 * 1024;
   Machine   : constant Lith.Machine.Lith_Machine :=
                 Lith.Machine.Create (Core_Size);
   Profile   : constant Boolean := False;
begin

   Lith.Primitives.Add_Primitives;
   Lith.IO.Initialise_IO;
   Lith.Vectors.Register (Machine.all);
   Lith.Bytevectors.Register (Machine.all);

   Lith.Parser.Parse_File
     (Machine.all,
      Lith.Paths.Config_Path & "/lith.l");

   if Profile then
      Machine.Start_Profile;
   end if;

   if True then
      Lith.Repl.Execute (Machine);
   end if;

   if Profile then
      Machine.Finish_Profile;
   end if;

   Machine.Report_State;
   Machine.Report_Memory;

   if Profile then
      Machine.Report_Profile (True, 100);
   end if;

exception
   when others =>
      Machine.Report_State;
      Machine.Report_Memory;
      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         "exiting because of unhandled exception");
      raise;
end Lith.Driver;

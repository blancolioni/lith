with Ada.Text_IO;

with Lith.IO;
with Lith.Machine;

with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Repl;

with Lith.Vectors;

procedure Lith.Driver is
   Core_Size : constant := 128 * 1024;
   Machine   : constant Lith.Machine.Lith_Machine :=
                 Lith.Machine.Create (Core_Size);
begin

   Lith.Primitives.Add_Primitives;
   Lith.IO.Initialise_IO;
   Lith.Vectors.Register (Machine.all);

   Lith.Parser.Parse_File
     (Machine.all,
      Lith.Paths.Config_Path & "/lith.l");

   if True then
      Lith.Repl.Execute (Machine);
   end if;

   Machine.Report_State;
   Machine.Report_Memory;

exception
   when others =>
      Machine.Report_State;
      Machine.Report_Memory;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "exiting because of unhandled exception");
      raise;
end Lith.Driver;

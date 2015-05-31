with Ada.Wide_Wide_Text_IO;

with Lith.IO;
with Lith.Machine;
with Lith.Machine.Profiling;

with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Repl;

with Lith.Vectors;
with Lith.Bytevectors;

procedure Lith.Driver is
   --  Core_Size       : constant := 4 * 1024 * 1024;
   Core_Size       : constant := 64 * 1024;
   Machine         : constant Lith.Machine.Lith_Machine :=
                       Lith.Machine.Create (Core_Size);
   Profile         : constant Boolean := False;
   Exit_Statistics : constant Boolean := True;
   Profiler        : Lith.Machine.Profiling.Profile_Type;
begin

   Lith.Primitives.Add_Primitives;
   Lith.IO.Initialise_IO;
   Lith.Vectors.Register (Machine.all);
   Lith.Bytevectors.Register (Machine.all);

   Lith.Parser.Parse_File
     (Machine.all,
      Lith.Paths.Config_Path & "/lith.l");

   if Profile then
      Profiler.Start (Machine);
      Machine.Start_Profile;
   end if;

   if True then
      Lith.Repl.Execute (Machine);
   end if;

   if Profile then
      Profiler.Finish;
      Machine.Finish_Profile;
   end if;

   if Exit_Statistics then
      Machine.Report_State;
      Machine.Report_Memory;
   end if;

   if Profile then
      Profiler.Report;
      Machine.Report_Profile (True);
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

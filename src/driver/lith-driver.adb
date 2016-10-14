with Ada.Text_IO;

with Lith.IO;
with Lith.Machine;
with Lith.Machine.Profiling;

with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Repl;

with Lith.Vectors;
with Lith.Bytevectors;

with Lith.Options;

procedure Lith.Driver is
   Machine : Lith.Machine.Lith_Machine;
   Profiler : Lith.Machine.Profiling.Profile_Type;
begin

   Lith.Options.Load_Options;

   Machine := Lith.Machine.Create (Lith.Options.Core_Size);

   Lith.Primitives.Add_Primitives;
   Lith.IO.Initialise_IO;
   Lith.Vectors.Register (Machine.all);
   Lith.Bytevectors.Register (Machine.all);

   Lith.Parser.Parse_File
     (Machine.all,
      Lith.Paths.Config_Path & "/lith.l");

   if Lith.Options.Profile then
      Profiler.Start (Machine);
      Machine.Start_Profile;
   end if;

   if Lith.Options.Self_Test then
      Lith.Parser.Parse_File
        (Machine.all,
         Lith.Paths.Config_Path & "/interaction-environment.scm");
      Lith.Parser.Parse_File
        (Machine.all,
         Lith.Paths.Config_Path & "/self-test.scm");
   end if;

   if Lith.Options.Interactive then
      Lith.Repl.Execute (Machine);
   end if;

   if Lith.Options.Profile then
      Profiler.Finish;
      Machine.Finish_Profile;
   end if;

   if Lith.Options.Exit_Statistics then
      Machine.Report_State;
      Machine.Report_Memory;
   end if;

   if Lith.Options.Profile then
      Profiler.Report;
      Machine.Report_Profile (True);
   end if;

exception
   when others =>
      Machine.Report_State;
      Machine.Report_Memory;
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "exiting because of unhandled exception");
      raise;
end Lith.Driver;

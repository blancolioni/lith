with Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Text_IO;

with Lith.Machine;

with Lith.Parser;
with Lith.Paths;
with Lith.Primitives;
with Lith.Repl;

procedure Lith.Driver is
   Core_Size : constant := 20_000;
   Machine   : constant Lith.Machine.Lith_Machine :=
                 Lith.Machine.Create (Core_Size);
begin

   Lith.Primitives.Add_Primitives;

   Lith.Parser.Parse_File
     (Machine,
      Ada.Characters.Conversions.To_Wide_Wide_String
        (Lith.Paths.Config_Path)
      & "/lith.l");

   if True then
      if Ada.Directories.Exists ("auto.l") then
         Lith.Parser.Parse_File
           (Machine, "auto.l");
      end if;
   end if;

   if False then
      Lith.Repl.Execute (Machine);
   end if;

   Machine.Report;

exception
   when others =>
      Machine.Report;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "exiting because of unhandled exception");
      raise;
end Lith.Driver;

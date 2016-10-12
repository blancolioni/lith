with Lith.Objects;

package Lith.Library is

   procedure Initialise
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True);

   function Store return access Lith.Objects.Object_Store'Class;

   procedure Start_Repl;

   procedure Load (Path : String);

end Lith.Library;

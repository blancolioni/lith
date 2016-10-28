with Lith.Objects;

package Lith.Library is

   procedure Initialise
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True);

   type Object_Store_Instance is access all Lith.Objects.Object_Store'Class;

   function New_Instance
     (Core_Size          : Natural := 64 * 1024)
      return Object_Store_Instance;

   procedure Close_Instance (Instance : in out Object_Store_Instance);

   function Store return access Lith.Objects.Object_Store'Class;

   procedure Start_Repl;

   procedure Load (Path : String);

end Lith.Library;

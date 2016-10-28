with Lith.Objects;

package Lith.Library is

   procedure Initialise
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True);

   type Object_Store_Instance is access all Lith.Objects.Object_Store'Class;

   function New_Instance
     (Core_Size          : Natural := 64 * 1024;
      Import_Scheme_Base : Boolean := True)
      return Object_Store_Instance;

   procedure Close_Instance (Instance : in out Object_Store_Instance);

   function Library_Store return access Lith.Objects.Object_Store'Class;

   procedure Start_Repl
     (With_Store : access Lith.Objects.Object_Store'Class := null);

   procedure Load
     (Path : String;
      To_Store : access Lith.Objects.Object_Store'Class := null);

end Lith.Library;

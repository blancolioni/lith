with Lith.Objects;

private package Lith.Init is

   procedure Start_Lith
     (Core_Size : Natural := 64 * 1024);

   function Main_Store
     return access Lith.Objects.Object_Store'Class;

end Lith.Init;

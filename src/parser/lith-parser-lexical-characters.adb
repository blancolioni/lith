with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Characters.Latin_9;
with Ada.Strings.Fixed.Hash;

package body Lith.Parser.Lexical.Characters is

   package Character_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Character,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   Name_Map : Character_Maps.Map;

   procedure Create_Name_Map;

   ---------------------
   -- Create_Name_Map --
   ---------------------

   procedure Create_Name_Map is
      use Ada.Characters.Latin_9;
   begin
      --  standard character names
      Name_Map.Insert ("alarm", BEL);
      Name_Map.Insert ("backspace", BS);
      Name_Map.Insert ("delete", DEL);
      Name_Map.Insert ("escape", ESC);
      Name_Map.Insert ("newline", LF);
      Name_Map.Insert ("null", NUL);
      Name_Map.Insert ("return", CR);
      Name_Map.Insert ("space", ' ');
      Name_Map.Insert ("tab", HT);

      --  extra character names
      Name_Map.Insert ("euro", Euro_Sign);
   end Create_Name_Map;

   -----------------------
   -- Name_To_Character --
   -----------------------

   function Name_To_Character
     (Name : String)
      return Character
   is
   begin
      if Name_Map.Is_Empty then
         Create_Name_Map;
      end if;
      if Name'Length = 1 then
         return Name (Name'First);
      elsif Name_Map.Contains (Name) then
         return Name_Map.Element (Name);
      else
         raise Constraint_Error with
           "undefined character name: " & Name;
      end if;
   end Name_To_Character;

end Lith.Parser.Lexical.Characters;

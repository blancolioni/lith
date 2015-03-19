with Ada.Characters.Conversions;
with Ada.Strings.Fixed;
with Ada.Wide_Wide_Characters.Handling;

with Lith.Objects.Numbers;

package body Lith.Parser.Lexical.Identifiers is

   type State_Type is (Start, Scanning_Integer, Base_Prefix,
                       End_State_Identifier,
                       End_State_Integer,
                       End_State_Float);

   subtype End_State is State_Type range
     End_State_Identifier .. End_State_Float;

   type Integer_Base is range 2 .. 16;

   function Is_Base_Digit
     (Ch : Wide_Wide_Character;
      Base : Integer_Base)
      return Boolean;

   function Base_Digit_Value
     (Ch : Wide_Wide_Character;
      Base : Integer_Base)
      return Natural;

   procedure Scan_Identifier
     (Text : Wide_Wide_String;
      Call_Back : access
        procedure (State : State_Type;
                   Ch    : Wide_Wide_Character));

   ----------------------
   -- Base_Digit_Value --
   ----------------------

   function Base_Digit_Value
     (Ch : Wide_Wide_Character;
      Base : Integer_Base)
      return Natural
   is
      use Ada.Wide_Wide_Characters.Handling;
      use Ada.Characters.Conversions;
      use Ada.Strings.Fixed;
   begin
      if Is_Digit (Ch) then
         return Index ("0123456789", (1 => To_Character (Ch))) - 1;
      elsif Is_Upper (Ch) then
         return Index ("ABCDEF", (1 => To_Character (Ch))) + 9;
      elsif Is_Lower (Ch) then
         return Index ("abcdef", (1 => To_Character (Ch))) + 9;
      else
         raise Program_Error with
           "bad digit for base"
           & Integer_Base'Image (Base)
           & " number: " & To_Character (Ch);
      end if;
   end Base_Digit_Value;

   -------------------------
   -- Classify_Identifier --
   -------------------------

   function Classify_Identifier
     (Text : Wide_Wide_String)
      return Lith.Parser.Tokens.Token
   is
      use Ada.Wide_Wide_Characters.Handling;

      Base   : Integer_Base := 10;
      State  : State_Type := Start;
      Result : Token := Tok_Identifier;

   begin
      for I in Text'Range loop
         exit when State in End_State;
         declare
            Ch : constant Wide_Wide_Character := Text (I);
            Last : constant Boolean := I = Text'Last;
--              Next_Ch : constant Wide_Wide_Character :=
--                          (if not Last
--                           then Text (I + 1)
--                           else Wide_Wide_Character'Val (26));
         begin
            case State is
            when Start =>
               if Ch = '#' then
                  State := Base_Prefix;
               elsif Is_Digit (Ch) then
                  if Last then
                     State := End_State_Integer;
                  else
                     State := Scanning_Integer;
                  end if;
               else
                  State := End_State_Identifier;
               end if;
            when Scanning_Integer =>
               if not Is_Base_Digit (Ch, Base) then
                  State := End_State_Identifier;
               elsif Last then
                  State := End_State_Integer;
               end if;
            when Base_Prefix =>
               if Ch = 'o' then
                  Base := 8;
                  State := Scanning_Integer;
               elsif Ch = 'b' then
                  Base := 2;
                  State := Scanning_Integer;
               elsif Ch = 'd' then
                  Base := 10;
                  State := Scanning_Integer;
               elsif Ch = 'x' then
                  Base := 16;
                  State := Scanning_Integer;
               else
                  State := End_State_Identifier;
               end if;
            when End_State =>
               null;
            end case;
         end;
      end loop;

      if State in End_State then
         case End_State (State) is
            when End_State_Identifier =>
               Result := Tok_Identifier;
            when End_State_Integer =>
               Result := Tok_Integer;
            when End_State_Float =>
               Result := Tok_Float;
         end case;
      end if;

      return Result;
   end Classify_Identifier;

   -------------------
   -- Is_Base_Digit --
   -------------------

   function Is_Base_Digit
     (Ch : Wide_Wide_Character;
      Base : Integer_Base)
      return Boolean
   is
      use Ada.Wide_Wide_Characters.Handling;
   begin
      case Base is
         when 2 =>
            return Ch = '0' or else Ch = '1';
         when 8 =>
            return Ch in '0' .. '7';
         when 10 =>
            return Is_Digit (Ch);
         when 16 =>
            return Is_Digit (Ch)
              or else Ch in 'A' .. 'F'
              or else Ch in 'a' .. 'f';
         when others =>
            raise Program_Error with
              "bad base";
      end case;
   end Is_Base_Digit;

   ------------------
   -- Push_Integer --
   ------------------

   procedure Push_Integer
     (Store : in out Lith.Objects.Object_Store'Class;
      Text  : in     Wide_Wide_String)
   is
      Base  : Integer_Base := 10;

      procedure On_Character
        (State : State_Type;
         Ch    : Wide_Wide_Character);

      ------------------
      -- On_Character --
      ------------------

      procedure On_Character
        (State : State_Type;
         Ch    : Wide_Wide_Character)
      is
      begin
         case State is
            when Base_Prefix =>
               if Ch = 'o' then
                  Base := 8;
               elsif Ch = 'b' then
                  Base := 2;
               elsif Ch = 'd' then
                  Base := 10;
               elsif Ch = 'x' then
                  Base := 16;
               end if;
            when Start | Scanning_Integer =>
               if Is_Base_Digit (Ch, Base) then
                  Store.Push (Lith.Objects.To_Object (Integer (Base)));
                  Lith.Objects.Numbers.Exact_Multiply (Store);
                  Store.Push (Lith.Objects.To_Object
                              (Base_Digit_Value (Ch,
                                 Base)));
                  Lith.Objects.Numbers.Exact_Add (Store);
               end if;
            when others =>
               null;
         end case;
      end On_Character;

   begin
      Store.Push (Lith.Objects.To_Object (Integer'(0)));
      Scan_Identifier (Text, On_Character'Access);
   end Push_Integer;

   ---------------------
   -- Scan_Identifier --
   ---------------------

   procedure Scan_Identifier
     (Text : Wide_Wide_String;
      Call_Back : access
        procedure (State : State_Type;
                   Ch    : Wide_Wide_Character))
   is
      use Ada.Wide_Wide_Characters.Handling;

      Base   : Integer_Base := 10;
      State  : State_Type := Start;

   begin
      for I in Text'Range loop
         if Call_Back /= null then
            Call_Back (State, Text (I));
         end if;
         exit when State in End_State;
         declare
            Ch : constant Wide_Wide_Character := Text (I);
            Last : constant Boolean := I = Text'Last;
         begin
            case State is
            when Start =>
               if Ch = '#' then
                  State := Base_Prefix;
               elsif Is_Digit (Ch) then
                  State := Scanning_Integer;
               else
                  State := End_State_Identifier;
               end if;
            when Scanning_Integer =>
               if not Is_Base_Digit (Ch, Base) then
                  State := End_State_Identifier;
               elsif Last then
                  State := End_State_Integer;
               end if;
            when Base_Prefix =>
               if Ch = 'o' then
                  Base := 8;
                  State := Scanning_Integer;
               elsif Ch = 'b' then
                  Base := 2;
                  State := Scanning_Integer;
               elsif Ch = 'd' then
                  Base := 10;
                  State := Scanning_Integer;
               elsif Ch = 'x' then
                  Base := 16;
                  State := Scanning_Integer;
               else
                  State := End_State_Identifier;
               end if;
            when End_State =>
               null;
            end case;
         end;
      end loop;

   end Scan_Identifier;

end Lith.Parser.Lexical.Identifiers;

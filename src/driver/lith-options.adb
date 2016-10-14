with Ada.Command_Line;
with Ada.Strings.Fixed;

package body Lith.Options is

   function Boolean_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Boolean := False)
      return Boolean;

   function Integer_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Integer := 0)
      return Integer;

   function String_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : String := "")
      return String;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Boolean := False)
      return Boolean
   is
      use Ada.Strings.Fixed;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
                         Ada.Command_Line.Argument (I);
         begin
            if Argument'Length > 2
              and then Argument (1 .. 2) = "--"
              and then Index (Argument, "=") = 0
              and then Argument (3 .. Argument'Last) = Long_Name
            then
               return True;
            elsif Argument'Length > 5
              and then Argument (1 .. 5) = "--no-"
              and then Index (Argument, "=") = 0
              and then Argument (6 .. Argument'Last) = Long_Name
            then
               return False;
            elsif Short_Name /= Character'Val (0)
              and then Argument (1) = '-'
              and then Argument'Length > 1
              and then Argument (2) /= '-'
            then
               for J in 2 .. Argument'Last loop
                  if Argument (J) = Short_Name then
                     return not Default;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      return Default;

   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : Integer := 0)
      return Integer
   is
   begin
      return Integer'Value (String_Value (Long_Name, Short_Name,
                            Integer'Image (Default)));
   exception
      when Constraint_Error =>
         return Default;
   end Integer_Value;

   ------------------
   -- Load_Options --
   ------------------

   procedure Load_Options is
   begin
      Exit_Statistics := Boolean_Value ("exit-statistics");
      Profile := Boolean_Value ("profile", 'p');
      Self_Test := Boolean_Value ("self-test");
      Trace_Definitions := Boolean_Value ("trace-definitions");
      Trace_Evaluation  := Boolean_Value ("trace-evaluation");
      Trace_GC          := Boolean_Value ("trace-gc");
      Trace_Patterns    := Boolean_Value ("trace-patterns");
      Trace_Stack       := Boolean_Value ("trace-static");

      Core_Size := Integer_Value ("core-size", 'm', 64 * 1024);

   end Load_Options;

   ------------------
   -- String_Value --
   ------------------

   function String_Value
     (Long_Name  : String;
      Short_Name : Character := Character'Val (0);
      Default    : String := "")
      return String
   is
      use Ada.Strings.Fixed;
   begin
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String :=
                         Ada.Command_Line.Argument (I);
         begin
            if Argument'Length > 2
              and then Argument (1 .. 2) = "--"
              and then Index (Argument, "=") > 0
            then
               declare
                  Name : constant String :=
                           Argument (3 .. Index (Argument, "=") - 1);
               begin
                  if Name = Long_Name then
                     return Argument (Index (Argument, "=") + 1
                                      .. Argument'Last);
                  end if;
               end;
            elsif Short_Name /= Character'Val (0)
              and then Argument (1) = '-'
              and then Argument'Length = 2
              and then Argument (2) = Short_Name
              and then I < Ada.Command_Line.Argument_Count
            then
               return Ada.Command_Line.Argument (I + 1);
            end if;
         end;
      end loop;

      return Default;

   end String_Value;

end Lith.Options;

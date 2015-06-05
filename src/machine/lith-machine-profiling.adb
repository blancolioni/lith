with Lith.Objects.Symbols;
with Ada.Text_IO;

package body Lith.Machine.Profiling is

   ----------
   -- Call --
   ----------

   overriding function Call
     (Profile   : in out Profile_Type;
      Arguments : Lith.Objects.Object)
      return Lith.Objects.Object
   is
      use Lith.Objects, Lith.Objects.Symbols;
      Cost_Centre : constant Symbol_Type :=
                      To_Symbol (Profile.Machine.Cadr (Arguments));
   begin
      if Get_Name (To_Symbol (Profile.Machine.Car (Arguments))) = "start" then
         Profile.Start_Cost_Centre (Cost_Centre);
      else
         Profile.End_Cost_Centre (Cost_Centre);
      end if;
      return No_Value;
   end Call;

   ---------------------
   -- End_Cost_Centre --
   ---------------------

   procedure End_Cost_Centre
     (Profile : in out Profile_Type;
      Name    : Lith.Objects.Symbol_Type)
   is
      use Cost_Centre_Stacks;
      use Lith.Objects;
      Position : Cursor := Profile.Stack.First;
   begin
      while Has_Element (Position)
        and then Element (Position).Name /= Name
      loop
         Next (Position);
      end loop;

      if Has_Element (Position) then
         declare
            use Ada.Calendar;
            Running : constant Running_Cost_Centre :=
                        Element (Position);
         begin
            Profile.Stack.Delete (Position);
            if Profile.Cost_Centres.Contains (Name) then
               declare
                  Acc : Cost_Centre :=
                          Profile.Cost_Centres.Element (Name);
               begin
                  Acc.Total_Time := Acc.Total_Time
                    + Clock - Running.Start;
                  Acc.Ex_Children := Acc.Ex_Children + Running.Cum;
                  Profile.Cost_Centres.Replace (Name, Acc);
               end;
            else
               Profile.Cost_Centres.Insert
                 (Name,
                  (Name, Clock - Running.Start, Running.Cum));
            end if;
         end;
      end if;
   end End_Cost_Centre;

   ------------
   -- Finish --
   ------------

   procedure Finish (Profile : in out Profile_Type) is

      procedure Show (Centre : Cost_Centre);

      ----------
      -- Show --
      ----------

      procedure Show (Centre : Cost_Centre) is
      begin
         Ada.Text_IO.Put
           (Duration'Image (Centre.Total_Time));
         Ada.Text_IO.Set_Col (16);
         Ada.Text_IO.Put
           (Duration'Image (Centre.Ex_Children));
         Ada.Text_IO.Set_Col (32);
         Ada.Text_IO.Put_Line
           (Lith.Objects.Symbols.Get_Name (Centre.Name));
      end Show;

   begin
      Profile.Cost_Centres.Iterate (Show'Access);
   end Finish;

   ------------
   -- Report --
   ------------

   procedure Report (Profile : Profile_Type) is
   begin
      null;
   end Report;

   -----------
   -- Start --
   -----------

   procedure Start
     (Profile : in out Profile_Type;
      Machine : Lith_Machine)
   is
   begin
      Profile.Machine := Machine;
      Machine.Add_Hook ("cost-centre", Profile'Unchecked_Access);
   end Start;

   -----------------------
   -- Start_Cost_Centre --
   -----------------------

   procedure Start_Cost_Centre
     (Profile : in out Profile_Type;
      Name    : Lith.Objects.Symbol_Type)
   is
      use Ada.Calendar;
   begin
      if not Profile.Stack.Is_Empty then
         declare
            Head : Running_Cost_Centre := Profile.Stack.First_Element;
         begin
            Head.Cum := Head.Cum + Ada.Calendar.Clock - Head.Start;
            Profile.Stack.Replace_Element (Profile.Stack.First, Head);
         end;
      end if;

      Profile.Stack.Insert (Profile.Stack.First,
                            (Name, Ada.Calendar.Clock, 0.0));

   end Start_Cost_Centre;

end Lith.Machine.Profiling;

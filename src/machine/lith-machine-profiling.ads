private with Ada.Containers.Doubly_Linked_Lists;

package Lith.Machine.Profiling is

   type Profile_Type is
     new Lith.Objects.Evaluation_Hook_Interface with private;

   procedure Start (Profile : in out Profile_Type;
                    Machine : Lith_Machine);

   procedure Finish (Profile : in out Profile_Type);

   procedure Start_Cost_Centre
     (Profile : in out Profile_Type;
      Name    : Lith.Objects.Symbol_Type);

   procedure End_Cost_Centre
     (Profile : in out Profile_Type;
      Name    : Lith.Objects.Symbol_Type);

   procedure Report (Profile : Profile_Type);

private

   type Cost_Centre is
      record
         Name        : Lith.Objects.Symbol_Type;
         Total_Time  : Duration;
         Ex_Children : Duration;
      end record;

   package Cost_Centre_Maps is
     new Lith.Objects.Symbol_Maps
       (Element_Type => Cost_Centre);

   type Running_Cost_Centre is
      record
         Name  : Lith.Objects.Symbol_Type;
         Start : Ada.Calendar.Time;
         Cum   : Duration;
      end record;

   package Cost_Centre_Stacks is
     new Ada.Containers.Doubly_Linked_Lists (Running_Cost_Centre);

   type Profile_Type is new Lith.Objects.Evaluation_Hook_Interface with
      record
         Cost_Centres : Cost_Centre_Maps.Map;
         Stack        : Cost_Centre_Stacks.List;
         Machine      : Lith_Machine;
      end record;

   overriding function Call
     (Profile   : in out Profile_Type;
      Arguments : Lith.Objects.Object)
      return Lith.Objects.Object;

end Lith.Machine.Profiling;

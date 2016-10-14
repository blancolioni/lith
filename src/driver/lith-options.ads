package Lith.Options is

   Core_Size : Positive := 64 * 1024;

   Interactive       : Boolean := False;

   Exit_Statistics   : Boolean := False;
   Profile           : Boolean := False;
   Self_Test         : Boolean := False;
   Trace_Definitions : Boolean := False;
   Trace_Evaluation  : Boolean := False;
   Trace_GC          : Boolean := False;
   Trace_Patterns    : Boolean := False;
   Trace_Stack       : Boolean := False;

   procedure Load_Options;

end Lith.Options;

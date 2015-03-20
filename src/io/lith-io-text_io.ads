with Lith.Objects;

package Lith.IO.Text_IO is

   function Evaluate_Open_Output_File
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Evaluate_Open_Input_File
     (Store       : in out Lith.Objects.Object_Store'Class;
      Arguments   : Lith.Objects.Array_Of_Objects)
      return Lith.Objects.Object;

   function Is_Text_Port (Port : Port_Type'Class) return Boolean;

end Lith.IO.Text_IO;

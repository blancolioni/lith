with Lith.Objects;

package Lith.Environment is

   procedure Define (Name  : Lith.Objects.Symbol_Type;
                     Value : Lith.Objects.Object);

   procedure Get (Name  : Lith.Objects.Symbol_Type;
                  Value : out Lith.Objects.Object;
                  Found : out Boolean);

   function Get (Name    : Lith.Objects.Symbol_Type;
                 Default : Lith.Objects.Object := Lith.Objects.Nil)
                 return Lith.Objects.Object;

   procedure Replace (Name  : Lith.Objects.Symbol_Type;
                      Value : Lith.Objects.Object);

   procedure Mark (Store : in out Lith.Objects.Object_Store'Class);

   procedure Define
     (Name  : String;
      Value : Lith.Objects.Object);

   function Get
     (Name    : String;
      Default : Lith.Objects.Object := Lith.Objects.Nil)
      return Lith.Objects.Object;

end Lith.Environment;

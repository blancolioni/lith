private with Ada.Containers.Ordered_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Lith.Objects.Symbol_Maps is

   type Map is tagged private;

   function Contains (Container : Map;
                      Key       : Symbol_Type)
                      return Boolean;

   function Element (Container : Map;
                     Key       : Symbol_Type)
                     return Element_Type;

   procedure Insert (Container : in out Map;
                     Key       : Symbol_Type;
                     Value     : Element_Type);

   procedure Replace (Container : in out Map;
                      Key       : Symbol_Type;
                      Value     : Element_Type);

   procedure Clear (Container : in out Map);

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Item : Element_Type));

   procedure Update
     (Container : Map;
      Process   : not null access
        procedure (Item : in out Element_Type));

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Key : Symbol_Type;
                   Item : Element_Type));

private

   package Internal_Symbol_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Symbol_Type,
        Element_Type => Element_Type);

   type Map is tagged
      record
         Internal : Internal_Symbol_Maps.Map;
      end record;

end Lith.Objects.Symbol_Maps;

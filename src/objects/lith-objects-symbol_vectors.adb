package body Lith.Objects.Symbol_Vectors is

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Symbol_Type)
      return Element_Type
   is
   begin
      return Container.Internal.Element (Positive (Index));
   end Element;

   -----------------
   -- New_Element --
   -----------------

   procedure New_Element
     (Container  : in out Vector;
      Value      : Element_Type;
      New_Symbol : out Symbol_Type)
   is
   begin
      Container.Internal.Append (Value);
      New_Symbol := Symbol_Type (Container.Internal.Last_Index);
   end New_Element;

end Lith.Objects.Symbol_Vectors;

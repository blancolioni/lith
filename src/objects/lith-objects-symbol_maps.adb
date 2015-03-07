package body Lith.Objects.Symbol_Maps is

   function Contains
     (Container : Map;
      Key       : Symbol_Type)
      return Boolean
   is
   begin
      return Container.Internal.Contains (Key);
   end Contains;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      Key       : Symbol_Type)
      return Element_Type
   is
   begin
      return Container.Internal.Element (Key);
   end Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Symbol_Type;
      Value     : Element_Type)
   is
   begin
      Container.Internal.Insert (Key, Value);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Item : Element_Type))
   is
   begin
      for Item of Container.Internal loop
         Process (Item);
      end loop;
   end Iterate;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Symbol_Type;
      Value     : Element_Type)
   is
   begin
      Container.Internal.Replace (Key, Value);
   end Replace;

end Lith.Objects.Symbol_Maps;

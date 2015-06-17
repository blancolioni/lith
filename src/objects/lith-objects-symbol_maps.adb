package body Lith.Objects.Symbol_Maps is

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Map) is
   begin
      Container.Internal.Clear;
   end Clear;

   --------------
   -- Contains --
   --------------

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
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Key : Symbol_Type;
                   Item : Element_Type))
   is
   begin
      for Position in Container.Internal.Iterate loop
         Process (Internal_Symbol_Maps.Key (Position),
                  Internal_Symbol_Maps.Element (Position));
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

   ------------
   -- Update --
   ------------

   procedure Update
     (Container : in out Map;
      Process   : not null access
        procedure (Item : in out Element_Type))
   is
   begin
      for Position in Container.Internal.Iterate loop
         declare
            Item : Element_Type := Internal_Symbol_Maps.Element (Position);
         begin
            Process (Item);
            Container.Internal.Replace_Element (Position, Item);
         end;
      end loop;
   end Update;

end Lith.Objects.Symbol_Maps;

with Lith.Objects.Symbol_Maps;

with Lith.Objects.Symbols;

package body Lith.Environment is

   package Environment_Maps is
     new Lith.Objects.Symbol_Maps (Lith.Objects.Object,
                                   Lith.Objects."=");

   Top : Environment_Maps.Map;

   ------------
   -- Define --
   ------------

   procedure Define
     (Name  : Lith.Objects.Symbol_Type;
      Value : Lith.Objects.Object)
   is
   begin
      Top.Insert (Name, Value);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
         Lith.Objects.Symbols.Get_Name (Name)
           & ": already defined";

   end Define;

   ------------
   -- Define --
   ------------

   procedure Define
     (Name  : String;
      Value : Lith.Objects.Object)
   is
   begin
      Define (Lith.Objects.Symbols.Get_Symbol (Name), Value);
   end Define;

   ---------
   -- Get --
   ---------

   procedure Get
     (Name  : Lith.Objects.Symbol_Type;
      Value : out Lith.Objects.Object;
      Found : out Boolean)
   is
   begin
      if Top.Contains (Name) then
         Value := Top.Element (Name);
         Found := True;
      else
         Found := False;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Name    : Lith.Objects.Symbol_Type;
      Default : Lith.Objects.Object := Lith.Objects.Nil)
      return Lith.Objects.Object
   is
   begin
      if Top.Contains (Name) then
         return Top.Element (Name);
      else
         return Default;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Name    : String;
      Default : Lith.Objects.Object := Lith.Objects.Nil)
      return Lith.Objects.Object
   is
   begin
      return Get (Lith.Objects.Symbols.Get_Symbol (Name), Default);
   end Get;

   ----------
   -- Mark --
   ----------

   procedure Mark (Store : in out Lith.Objects.Object_Store'Class) is
      procedure Set_Mark (Item : in out Lith.Objects.Object);

      --------------
      -- Set_Mark --
      --------------

      procedure Set_Mark (Item : in out Lith.Objects.Object) is
      begin
         Store.Mark (Item);
      end Set_Mark;

   begin
      Top.Update (Set_Mark'Access);
   end Mark;

   -------------
   -- Replace --
   -------------

   procedure Replace (Name  : Lith.Objects.Symbol_Type;
                      Value : Lith.Objects.Object)
   is
   begin
      if Top.Contains (Name) then
         Top.Replace (Name, Value);
      else
         Top.Insert (Name, Value);
      end if;
   end Replace;

end Lith.Environment;

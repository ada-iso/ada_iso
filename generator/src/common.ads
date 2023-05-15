with DOM.Core;                        use DOM.Core;
with DOM.Core.Nodes;                  use DOM.Core.Nodes;
with Ada.Strings.Fixed;               use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Fixed;     use Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;


package Common is
    --  Issue with file
   Invalid_File : exception;

   --  Returns a substring of the CSV based on comma, supports quotes.
   function Get_Next_Field (S : Wide_Wide_String; Next : out Natural)
      return Unbounded_Wide_Wide_String;
  
   --  Returns string of an XML node between <colmunx></columnx>
   function Get_Node_String (N : Node) return Unbounded_Wide_Wide_String;
   function Get_Node_String (N : Node) return Unbounded_String;

   function Sanatize (S : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String;

   function TWS (S : Unbounded_String) return Wide_Wide_String is
      (To_Wide_Wide_String (To_String (S)));
   function TWS (S : Unbounded_Wide_Wide_String) return Wide_Wide_String renames
      To_Wide_Wide_String;
   function TWS (S : String) return Wide_Wide_String renames
      To_Wide_Wide_String;
   function TS (Item : Wide_Wide_String; Substitute : Character := ' ')
      return String renames To_String;
   function TS (S : Unbounded_Wide_Wide_String) return String is
      ( TS (TWS (S)));
   function TUB (S : Unbounded_Wide_Wide_String) return Unbounded_String is
      (To_Unbounded_String (TS (S)));
   --  String stuff
   function EQ (Left, Right : Unbounded_Wide_Wide_String) return Boolean is
      (To_Unbounded_Wide_Wide_String(To_Upper(To_Wide_Wide_String (Left))) =
       To_Unbounded_Wide_Wide_String(To_Upper(To_Wide_Wide_String (Right))) );

   function Is_Natural (Item : String) return Boolean is
      (if Item'Length > 0 then
         (for all Ch of Item => Ch in '0' .. '9') else False);
   function Is_Natural (Item : Unbounded_String) return Boolean is
   ( Is_Natural (To_String (Item)));
   function To_Natural (Item : String) return Natural is
      (Natural'Value (Item));
   function To_Natural (Item : Wide_Wide_String) return Natural is
      (To_Natural (TS (Item)));
   function To_Natural (Item : Unbounded_String) return Natural is
      (To_Natural (To_String (Item)));
   function To_Natural (Item : Unbounded_Wide_Wide_String) return Natural is
      (To_Natural (TS (Item)));

end Common;
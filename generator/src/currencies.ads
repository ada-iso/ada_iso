with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Countries;
with Common; use Common;
package Currencies is

   --  Our File Names.
   XML_List_1  : constant String := "files/currencies.xml";
   XML_List_3  : constant String := "files/currencies-historic.xml";
   Symbol_List : constant String := "files/currencies-symbols.csv";

   package Unbound_List is new
      Ada.Containers.Vectors
         (Index_Type => Positive,
          Element_Type => Unbounded_String);

   package Wide_Unbound_List is new
      Ada.Containers.Vectors
         (Index_Type => Positive,
          Element_Type => Unbounded_Wide_Wide_String, "=" => EQ);

   type ISO_4217 is record
      Name               : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      Code               : Unbounded_String           := Null_Unbounded_String;
      Numeric            : Unbounded_String           := Null_Unbounded_String;
      Unit               : Unbounded_String           := Null_Unbounded_String;
      Withdraw_Date      : Unbounded_String           := Null_Unbounded_String;
      Fund               : Boolean                    := False;
      Historic           : Boolean                    := False;
      Only_Historic      : Boolean                    := False;
      Withdraw_Dates     : Unbounded_String           := Null_Unbounded_String;
      Historic_Records   : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      Symbol             : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      Historic_Countries : Wide_Unbound_List.Vector;
      Historic_Numbers   : Unbound_List.Vector;
      Historic_Names     : Wide_Unbound_List.Vector;
      Countries          : Wide_Unbound_List.Vector;
   end record;

   --  The lookup table that will hold our stuff.
   package ISO_4217_Table is new
      Ada.Containers.Hashed_Maps
         (Key_Type => Unbounded_String,
          Element_Type => ISO_4217,
          Hash => Ada.Strings.Unbounded.Hash,
          Equivalent_Keys => "=");

   function Load_Currencies return ISO_4217_Table.Map;

   procedure Generate_Currencies (Table : ISO_4217_Table.Map; C : Countries.ISO_3166_1_Table.Vector);
end Currencies;
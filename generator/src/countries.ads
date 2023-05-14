with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;
package Countries is

   Countries_CSV : constant String := "files/countries.csv";

   --  The CSV goes in a vector of these.
   type ISO_3166_1_Row is record
      Name    : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      Alpha_2 : Unbounded_String           := Null_Unbounded_String;
      Alpha_3 : Unbounded_String           := Null_Unbounded_String;
      Numeric : Unbounded_String           := Null_Unbounded_String;
   end record;

   --  The vector that will hold our stuff.
   package ISO_3166_1_Table is new
               Ada.Containers.Vectors
                  (Index_Type => Positive,
                  Element_Type => ISO_3166_1_Row);

   function Search_Country_Name (This : ISO_3166_1_Table.Vector; Name : Unbounded_Wide_Wide_String) return Natural;

   function Load_Countries_CSV return ISO_3166_1_Table.Vector;

   procedure Generate_Countries (Table : ISO_3166_1_Table.Vector);
end Countries;
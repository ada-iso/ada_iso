with Countries; use Countries;
with Currencies; use Currencies;
with Ada.Text_IO; use Ada.Text_IO; 
--  Generates into "<working directory>/output"

procedure Generator is
   Country_Table  : constant ISO_3166_1_Table.Vector := Load_Countries_CSV;
   Currency_Table : constant ISO_4217_Table.Map      := Load_Currencies;
begin
   Generate_Countries (Country_Table);
   Generate_Currencies (Currency_Table, Country_Table);
end Generator;
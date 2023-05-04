with Ada.Directories; use Ada.Directories;
with Countries; use Countries;
--  Generates into "<working directory>/output"

procedure Generator is
begin

   if Exists ("files/countries.csv") then
      Generate_Countries;
   end if;

end Generator;
with ISO_3166;       use ISO_3166;
with Ada.Locales;    use Ada.Locales;
with Ada.Assertions; use Ada.Assertions;
procedure Iso_3166_Tests is

   --  Create some countries.
   US_1 : constant ISO_3166_1 := From_Alpha2 ("US");
   US_2 : constant ISO_3166_1 := From_Alpha3 ("USA");

   --  Create some countries based on numeric.
   Bosnia_1 : constant ISO_3166_1 := From_Numeric (070);
   Bosnia_2 : constant ISO_3166_1 := From_Numeric ("070");

   --  Create a country based on ada-spec country code
   AU_Code_1 : constant Country_Code := "AU";
   AU        : constant ISO_3166_1   := From_Country_Code (AU_Code_1);
   AU_Code_2 : constant Country_Code := To_Country_Code(AU);

   --  For Testing all countries
   V : constant Countries := All_Countries;

begin
   --  Validate USA
   Assert (US_1 = US_2, "Country Failed");
   Assert (US_1.Name = "United States of America (the)", "US Name failed.");
   Assert (US_1.Alpha_2 = "US", "US Alpha2 failed");
   Assert (US_1.Alpha_3 = "USA", "US Alpha3 failed");
   Assert (US_1.Numeric = 840, "US Numeric failed");

   --  Validate Bosnia
   Assert (Bosnia_1 = Bosnia_2, "Country Failed");
   Assert (Bosnia_1.Name = "Bosnia and Herzegovina", "Bosnia name failed");
   Assert (Bosnia_1.Alpha_2 = "BA", "Bosnia Alpha2 failed");
   Assert (Bosnia_1.Alpha_3 = "BIH", "Bosnia Alpha3 failed");
   Assert (Bosnia_1.Numeric = 070, "Bosnia Numeric failed");

   --  Validate AU
   Assert (AU_Code_1 = AU_Code_2, "Country Code failed");

   --  Validate array.
   for X of V loop
      Assert (X.Name = From_Alpha2  (X.Alpha_2).Name, "Vector Failed.");
      Assert (X.Name = From_Alpha3  (X.Alpha_3).Name, "Vector Failed.");
      Assert (X.Name = From_Numeric (X.Numeric).Name, "Vector Failed.");
      Assert (X.Name = From_Numeric (X.Numeric'Image).Name, "Vector Failed.");
   end loop;

   --  All tests have passed.
end Iso_3166_Tests;

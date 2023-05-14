with Ada.Assertions; use Ada.Assertions;
with ISO.Countries; use ISO.Countries;
with Ada.Locales;
package body ISO_Countries_Tests is
   procedure Run_Tests is

      --  Create some countries.
      US_1 : constant Country := From_Alpha2 ("US");
      US_2 : constant Country := From_Alpha3 ("USA");

      --  Create some countries based on numeric.
      Bosnia_1 : constant Country := From_Numeric (070);
      Bosnia_2 : constant Country := From_Numeric ("070");

      --  Create a country based on ada-spec country code
      AU_Code_1 : constant Ada.Locales.Country_Code := "AU";
      AU        : constant Country := ISO.Countries.From_Country_Code (AU_Code_1);
      AU_Code_2 : constant Ada.Locales.Country_Code := AU.Country_Code;

      --  For Testing all countries
      A : constant All_Countries := Init_Countries;

      --  For testing undefined` country
      Undef : Country;

   begin

      --  Validate default initialization
      Assert (Undef.Name = "Undefined", "Undef failed.");
      Assert (Undef.Alpha2 = "ZZ", "Undef failed.");
      Assert (Undef.Alpha3 = "ZZZ", "Undef failed.");
      Assert (Undef.Numeric = 0, "Undef failed.");

      --  Validate USA
      Assert (US_1 = US_2, "Country Failed");
      Assert (US_1.Name = "United States of America (the)", "US Name failed.");
      Assert (US_1.Alpha2 = "US", "US Alpha2 failed");
      Assert (US_1.Alpha3 = "USA", "US Alpha3 failed");
      Assert (US_1.Numeric = 840, "US Numeric failed");

      --  Validate Bosnia
      Assert (Bosnia_1 = Bosnia_2, "Country Failed");
      Assert (Bosnia_1.Name = "Bosnia and Herzegovina", "Bosnia name failed");
      Assert (Bosnia_1.Alpha2 = "BA", "Bosnia Alpha2 failed");
      Assert (Bosnia_1.Alpha3 = "BIH", "Bosnia Alpha3 failed");
      Assert (Bosnia_1.Numeric = 070, "Bosnia Numeric failed");

      --  Validate Country Codes
      declare
         use Ada.Locales;
      begin
         Assert (AU_Code_1 = AU_Code_2, "Country Code failed");
         Assert (Undef.Country_Code = "ZZ", "Undef failed.");
      end;

      --  Validate array.
      Assert (A (C_US).Name = "United States of America (the)", "Index Validation Failed.");
      for X of A loop
         Assert (X.Name = From_Alpha2  (X.Alpha2).Name, "All Countries Failed.");
         Assert (X.Name = From_Alpha3  (X.Alpha3).Name, "All Countries Failed.");
         Assert (X.Name = From_Numeric (X.Numeric).Name, "All Countries Failed.");
         Assert (X.Name = From_Numeric (X.Numeric'Image).Name, "All Countries Failed.");
      end loop;

      --  Check if all country codes are valid alpha2s.
      for C in Country_Key'Range loop
         declare
            Test_String : constant String := C'Image;
            Test_Code : constant Alpha2_Code := Test_String (Test_String'Last - 1 .. Test_String'Last);
         begin
            null;
         end;
      end loop;

      --  All tests have passed.
   end Run_Tests;
end ISO_Countries_Tests;
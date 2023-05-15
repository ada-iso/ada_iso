with Ada.Assertions; use Ada.Assertions;
with ISO.Currencies; use ISO.Currencies;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with ISO.Countries;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Locales;
package body ISO_Currencies_Tests is
   procedure Run_Tests is
   begin

      declare
         --  Create a currency based on ada-spec country
         Test_Country    : constant ISO.Countries.Country := (Key => ISO.Countries.C_SV);
         Country_Result  : constant Currency_List := From_Country (Test_Country);
      begin
         Assert (for some C of Country_Result => C.Key in C_USD);
      end;

      declare
         --  Create some currencies based on numeric.
         EUR_1 : constant Currency := From_Numeric (978);
         EUR_2 : constant Currency := From_Numeric ("978");
         EUR_3 : constant Currency := (Key => C_EUR);
         EUR_Symbol : constant Wide_Wide_String := "€";
      begin
         Assert (EUR_1 = EUR_2 and then EUR_1 = EUR_3, "From_Numeric failed");
         --  Validate EUR
         Assert (EUR_1.Name = "Euro", "EUR Name failed.");
         Assert (EUR_1.Code = "EUR", "EUR Code failed");
         Assert (EUR_1.Numeric = 978, "EUR Numeric failed");
         Assert (EUR_1.Symbol = EUR_Symbol, "EUR Symbol failed");
         Assert (EUR_1.Unit = 2, "EUR Unit failed");
         for X of EUR_1.Entities loop
            Assert ((for some C of From_Country (X) => C = EUR_1 ), "Country not in currency");
         end loop;
      end;

      declare
         --  For testing undefined country
         Undef : Currency;
         Unknown_Symbol : constant Wide_Wide_String := "¤";
      begin
         --  Validate default initialization
         Assert (Undef.Name = "Unknown", "Undef failed.");
         Assert (Undef.Code = "ZZZ", "Undef failed.");
         Assert (Undef.Numeric = 0, "Undef failed.");
         Assert (Undef.Symbol = Unknown_Symbol, "Undef failed.");
         Assert (Undef.Unit = 0, "Undef failed");
      end;

      declare
         Fund_Currency : constant Currency := From_Code("USN");
      begin
         Assert (Fund_Currency.Is_Fund, "Fund failed");
      end;

      declare
         Historic_Currency : constant Currency := From_Code ("AFA");
      begin
         Assert (Historic_Currency.Is_Historic, "Historic Failed");
         Assert (Historic_Currency.Name = "Afghani", "Name failed");
         Assert (Historic_Currency.Withdraw_Date = "2003-01", "Historic failed" );
      end;

      declare
         --  For Testing all countries
         A : constant All_Currencies := Init_Currencies;
      begin
         --  Validate array.
         Assert (A (C_USD).Name = "US Dollar", "Index Validation Failed: """ & A (C_USD).Name & """");
         for X of A loop
            --  Check creation functions
            Assert (X.Name   = From_Code  (X.Code).Name, "All Countries Failed.");
            if not X.Is_Historic then
               Assert (X.Name   = From_Numeric (X.Numeric).Name, "All Countries Failed." & X.Name & "/=" & From_Numeric (X.Numeric).Name);
               Assert (X.Name   = From_Numeric (X.Numeric'Image).Name, "All Countries Failed.");
            end if;
            --  Rest of the member functions.
            declare
               C : constant Currency := From_Code (X.Code);
               Symbol : constant Wide_Wide_String := C.Symbol;
            begin
               Assert (X = C, "All countries failed");
               Assert (X.Code = C.Code, "All countries failed");
               Assert (X.Numeric = C.Numeric, "All Countries failed");
               Assert (X.Unit   = C.Unit, "All Countries Failed.");
               Assert (X.Symbol = Symbol, "All Countries Failed.");
               Assert (X.Is_Fund = C.Is_Fund, "All Countries failed");
               if X.Is_Historic then
                  Assert (C.Is_Historic, "All Countries failed");
                  Assert (X.Withdraw_Date = C.Withdraw_Date, "All Countries failed");
                  Assert (X.Withdraw_Dates = C.Withdraw_Dates, "All Countries failed");
                  Assert (X.Historic_Entities = C.Historic_Entities, "All Countries failed");
                  Assert (X.Historic_Records = C.Historic_Records, "All Countries failed");
               end if;
            end;
            --  Check countries
            for Y of X.Entities loop
               if Y.Alpha2 /= "ZZ" then
                  Assert ((for some C of From_Country(Y) => C = X ), "Country not in currency: for " & X.Code & " to " & Y.Alpha2);
               end if;
            end loop;
         end loop;
      end;

      --  Check if all country codes are valid
      for C in Currency_Key'Range loop
         declare
            Test_String : constant String := C'Image;
            Test_Code   : constant Alphabetic_Code := Test_String (Test_String'Last - 2 .. Test_String'Last);
         begin
            null;
         end;
      end loop;
      --  All tests have passed.

   end Run_Tests;
end ISO_Currencies_Tests;
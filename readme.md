# The Ada ISO Library

This is my attempt to implement various [ISO standards](https://www.iso.org/) in Ada.

This is *not* the Ada Standard Library (which is defined by [Ada Auth](http://www.ada-auth.org/standards/ada22.html)) but rather things like ISO 3166 (Country Codes), ISO 4217 (Currency Codes), or ISO 639 (Language Codes).

Current standards implemented:
- [ISO.Countries (ISO 3166-1)](https://ada-iso.github.io/docs/iso-countries_ads.html)
- [ISO.Currencies (ISO 4217)](https://ada-iso.github.io/docs/iso-currencies_ads.html)

Coming Soon:
- ISO.Languages (ISO 639)

## Installation

### With [Alire](https://alire.ada.dev/)

Be sure that you're using the latest community index:
```sh
alr index --update-all
```

To download and build:
```sh
alr get --build iso
```

To include it as a dependency in your Alire project:
```sh
alr with iso
```

### Without Alire

If you don't use Alire, you can just download the `ads` and `adb` files under `/src` and include them in your project.

## Usage
You can also read the [full API documentation](https://ada-iso.github.io/docs/toc_index.html) which has been generated with [ROBODoc](https://github.com/gumpu/ROBODoc).

### ISO.Countries

This is a full lookup table for the ISO 3166-1 country records in Ada.  In the future, I would like to include iso-3166-2 and iso-3166-3 in this. If that would be useful to you, then read the section below under "Contribute". [Full API Here](https://ada-iso.github.io/docs/iso-countries_ads.html).

Provided is an enumerated data type called `Country_Keys` comprised of `C_` followed by a Country Code.  That country key is stored in a `Country` object:
```Ada
type Country_Key is (
   C_AF, --  Afghanistan
   C_AL, --  Albania
   C_DZ, --  Algeria
--    . . .
   C_ZZ  -- Undefined
);
--  The ISO_3166_1 country to be referenced.
type Country is tagged record
   Key : Country_Key := C_ZZ;
end record;
```

There's also an array type with a positive index for this object:
```Ada
type Country_List is array (Positive range <>) of Country;
```

The `Country` object has the following member functions / methods:
```Ada
function Name         (This : Country) return String;
function Alpha2       (This : Country) return Alpha2_Code;
function Alpha3       (This : Country) return Alpha3_Code;
function Numeric      (This : Country) return Numeric_Code;
function Country_Code (This : Country) return Ada.Locales.Country_Code;
```
Since these are member functions, you can simply access them via the dot-operator:
```Ada
My_Country : Country := (C_US);
My_Name    : String  := My_Country.Name; -- "United States of America (The)"
```

`Alpha2_Code` and `Alpha3_Code` are subtypes of `String`, and `Numeric_Code` is a subtype of `Natural`.  Each are restricted to the only possible values defined  by the standard and `"ZZ"`, `"ZZZ"`, and `0` respectively for an Undefined country:   
```Ada
subtype Alpha2_Code is String (1 .. 2)
   with Dynamic_Predicate => Alpha2_Code in
      "AF" | "AL" | "DZ" | ... | "ZZ";
subtype Alpha3_Code is String (1 .. 3)
   with Dynamic_Predicate => Alpha3_Code in
      "AFG" | "ALB" | "DZA" | ... | "ZZZ";
subtype Numeric_Code is Natural
   with Dynamic_Predicate => Numeric_Code in
      004 | 008 | 012 | ... | 0; 
```

There are several functions that return this object, depending on what item you're using to look it up:
```Ada
with ISO.Countries; use ISO.Countries;
with Ada.Locales;
declare
   --  All of these will return the USA country.
   USA : constant Country := From_Alpha2 ("US");   --  Country Code
   AUS : constant Country := From_Alpha3 ("AUS");  --  Country Code3
   BIH : constant Country := From_Numeric (070);   --  Numeric
   ATA : constant Country := From_Numeric ("010"); --  Numeric String
   --  Get more information from Ada.Locales country code.
   Local_Country : Country := From_Country_Code (Ada.Locales.Country¨);
begin
   Assert (USA.Name = "United States of America (the)");
   Assert (AUS.Name = "Australia");
   Assert (BIH.Name = "Bosnia and Herzegovina");
   Assert (ATA.Name = "Antarctica");
end;
```

There's also an array that can contain all of the countries.  It must be initiated with the function `Init_Countries`.  The index is an item of `Country_Key`: 
```Ada
with ISO.Countries; use ISO.Countries;
with Ada.Text_IO;   use Ada.Text_IO;
declare
   A : constant All_Countries := Init_Countries;
begin
   Put_Line (A (C_US).Name);
   for X of A loop
      Put_Line (X.Name);
      Put_Line (X.Alpha2);
   end loop;
end;
```

### ISO.Currencies
This is a full lookup table for ISO 4217, both list one (current Currencies) and list three (previous currencies).  [Full API Here](https://ada-iso.github.io/docs/iso-currencies_ads.html).

Provided is an enumerated data type called `Currency_Keys` comprised of `C_` followed by a Currency Code.  That code key is stored in a `Currency` object:
```Ada
type Currency_Key is (
   C_NLG, --  Netherlands Guilder
   C_BMD, --  Bermudian Dollar
---   . . . 
   C_ZZZ --  Unknown
);

--  The ISO 4217 currency to be referenced.
type Currency is tagged record
   Key : Currency_Key := C_ZZZ;
end record;
```

There's also an array type with a positive index for this object:
```Ada
type Currency_List is array (Positive range <>) of Currency;
```

The `Currency` object has the following member functions / methods:
```Ada
function Name              (This : Currency) return String;
function Code              (This : Currency) return Alphabetic_Code;
function Numeric           (This : Currency) return Numeric_Code;
function Unit              (This : Currency) return Minor_Unit;
function Symbol            (This : Currency) return Wide_Wide_String;
function Symbol            (This : Currency) return String;
function Is_Fund           (This : Currency) return Boolean;
function Is_Historic       (This : Currency) return Boolean;
function Withdraw_Date     (This : Currency) return String;
function Withdraw_Dates    (This : Currency) return String;
function Historic_Names    (This : Currency) return String;
function Historic_Numerics (This : Currency) return String;
function Historic_Entities (This : Currency) return String;
function Historic_Records  (This : Currency) return String;
function Entities          (This : Currency) return Countries.Country_List;
```

Since these are member functions, you can simply access them via the dot-operator:
```Ada
My_Currency : Currency := (C_USD);
My_Name     : String   := My_Country.Name; -- "US Dollar"
```

`Alphabetic_Code` is a subtype of `String`, `Numeric_Code` and `Minor_Unit` are subtypes of Natural.  Each are restricted to the only possible values defined by the standard and `"ZZZ"`, and `0` respectively for an Undefined country:
```Ada
subtype Alphabetic_Code is String (1 .. 3)
   with Dynamic_Predicate => Alphabetic_Code in
      "NLG" | "BMD" | "VES" | ... | "ZZZ";
subtype Numeric_Code is Natural
   with Dynamic_Predicate => Numeric_Code in
      528 | 060 | 928 | ... | 0; 
subtype Minor_Unit is Natural range 0 ..  4;
```

There are several functions that return this object, depending on what item you're using to look it up:
```Ada
with ISO.Currencies; use ISO.Currencies;
with ISO.Countries;
declare
   --  All of these will return the USA country.
   USD : constant Currency := From_Code ("USD");   --  Currency Code
   AUD : constant Currency := From_Numeric (036);  --  Numeric Code
   GBP : constant Currency := From_Numeric ("826"); --  Numeric String
begin
   Assert (USD.Name = "US Dollar");
   Assert (AUD.Name = "Australian Dollar");
   Assert (GBP.Name = "Pound Sterling");
end;
```

You can retrieve every currency associated with a country using the `From_Countries` function:
```Ada
with ISO.Currencies; use ISO.Currencies;
with ISO.Countries;
SLV           : constant Countries.Country := (C_SV); -- El Salvador
My_Currencies : constant Currency_List := From_Country(SLV); -- This returns both USD and SVC
```

There's also an array that can contain all of the currencies.  It must be initiated with the function `Init_Currencies`.  The index is an item of `Currency_Key`: 
```Ada
with ISO.Currencies; use ISO.Currencies;
with Ada.Text_IO;   use Ada.Text_IO;
declare
   A : constant All_Currencies := Init_Currencies;
begin
   Put_Line (A (C_USD).Name);
   for X of A loop
      Put_Line (X.Name);
      Put_Line (X.Code);
   end loop;
end;
```

You can retrieve the symbol for each currency by calling the `Symbol` function.  If a symbol is known, it will return that symbol either as a string or wide_wide_string; if it's unknown, then it will return the universal currency symbol, `¤`:
```Ada
with ISO.Currencies; use ISO.Currencies;
with Ada.Text_IO;   use Ada.Text_IO;
declare
   Value : Natural := 1337;
   My_Currency : Currency := (C_USD); -- This is retrieved at some point.
begin
   Put_Line ("The price is: " & My_Currency.Symbol & Value'Image);
end;
```

You can also retrieve historic countries by country code, but not by numeric.
There are several functions to reference the historic information, usually as a semi-colon separated list.  If I even get access to ISO 3166-2 and ISO 3166-3, I can handle these lists more gracefully.

## Generating an update

The source files are currently being generated by another app written in Ada, but you don't need to run it in order to use this library, as I've already generated it.

If you want to generate the source files yourself, this is how:

1. Consolidating the reference files
   - For Countries:
      1. Visit the [Country Code Search](https://www.iso.org/obp/ui/#search/code/)
      2. Be sure to set the page length to 300 to catch everything.
      3. Paste the table into a CSV
      4. **REMOVE** the French Short Name, column B.
      5. Verify the CSV Has the following headers in this exact order: `English short name,Alpha-2 code,Alpha-3 code,Numeric`
      6. Save the CSV in the `generator/files` folder as `countries.csv`.
   - For Currencies:
      1. Visit the [ISO 4217 Standards Website](https://www.iso.org/iso-4217-currency-codes.html)
      2. Download the ISO standard in XML format.  Specifically `list-one.xml` and `list-three.xml`
         1. Rename `list-one.xml` as `currencies.xml`
         2. Rename `list-three.xml` as `currencies-historic.xml`
      3. Save these two _renamed_ xml files in `generator/files`
      4. If you have an update for currency symbols, I am manually maintaining this in `currencies-symbols.csv`.  So just edit that file.
      - NOTE: Ensure that country codes have already been downloaded. The generator relies on them.
2. Build the generate app
   1. `cd generate`
   2. `alr build`
3. Run the generate app
   1. `bin/generate`
   2. The source files will be stored in the `generate/output` directory.
4. Copy the source files to `ada_iso/src` (not `generate/src`!) folder.

## Sources
- Country Codes:
   - https://www.iso.org/obp/ui/#search/code/
- Currency Codes:
   - https://www.iso.org/iso-4217-currency-codes.html
   - https://www.six-group.com/en/products-services/financial-information/data-standards.html
- Currency Symbols:
   - https://en.wikipedia.org/wiki/Currency_symbol

## Contribute
If someone would like to give me 300 CHF (however much that is in USD) every time a new version of the [Country Codes Collection](https://www.iso.org/publication/PUB500001.html) is made available, I'll be happy to start using XML dumps like in the iso4217 library.

If the Country Codes Collection contains iso-3166-2 and iso-3166-3, I can start including those to build a more complete list.
NOTE: I probably won't be able to share the XML files if this happens, but I can still provide the generator once written.
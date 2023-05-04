with Common;                use Common;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package body Countries is
   procedure Generate_Countries is
      --  The CSV goes in a vector of these.
      type ISO_3166_1_Row is record
         Name    : Unbounded_String;
         Alpha_2 : Unbounded_String;
         Alpha_3 : Unbounded_String;
         Numeric : Unbounded_String;
      end record;

      --  The vector that will hold our stuff.
      package ISO_3166_1_Table is new
                  Ada.Containers.Vectors
                     (Index_Type => Natural,
                     Element_Type => ISO_3166_1_Row);

      --  Our CSV File Name.
      CSV_Name : constant String := "files/countries.csv";
      F : File_Type;

      --  Returns a substring of the CSV based on comma, supports quotes.
      function Get_Next_Field (S : String; Next : out Natural)
         return Unbounded_String
      is
         Has_Comma : constant Boolean := S (S'First) = '"';
         --  Set it to the start, or start+1 if it has a quote.
         First : constant Natural := (if S'First = S'Last then raise Invalid_File
                                 else (if Has_Comma then
                                             S'First + 1 else
                                             S'First));
         --  Set it to the next comma, or the quote if it has a quote.
         Next_Sep : constant Natural := Index (S (First .. S'Last),
                                             (if Has_Comma then
                                                   """" else
                                                   ","));
         Sep_Found : constant Boolean := Next_Sep > 0;
         --  Actual end of string we're returning.
         Last : constant Natural := (if Sep_Found then
                                       Next_Sep - 1 --  Right before , or "
                                 else
                                       S'Last);
      begin
         --  Next is set to the comma/quote +1 (or end)
         Next := (if Sep_Found then
                     (if Has_Comma then -- we need to look for ", or " as eol
                     (if Next_Sep /= S'Last and then
                           (Next_Sep + 2) > S'Last
                     then
                           S'Last --  " is eol
                     else --  Next_Sep check
                           Next_Sep + 2) --  It's probably ",
                     else --  Has_Comma
                     Next_Sep + 1) --  It has no comma
                  else --  Sep_Found
                     S'Last); --  No seperator found
         --  Return the result
         return To_Unbounded_String (S (First .. Last));
      end Get_Next_Field;

      --  Our vector.
      Table : ISO_3166_1_Table.Vector;

   begin
      --  Load the CSV
      Open (F, In_File, CSV_Name);
      --  Validate the header.
      if Get_Line (F) /= "English short name,Alpha-2 code,Alpha-3 code,Numeric" then
         raise Invalid_File with "Invalid columns";
      end if;
      while not End_Of_File (F) loop
         declare
         --  Read next row.
         Next_Line : constant String := Get_Line (F);
         Next_Row  : ISO_3166_1_Row;

         --  To keep track of where we are in the row.
         Next : Natural;
         begin
         --  Break up each field; don't forget to handle commas!
         --  This really is quick and dirty, there's plenty of better ways
         --  to do this more generically, but I'm hoping someday to migrate
         --  to XML.
         Next_Row.Name := Get_Next_Field (Next_Line, Next);
         Next_Row.Alpha_2 := Get_Next_Field
                                 (Next_Line (Next .. Next_Line'Last), Next);
         Next_Row.Alpha_3 := Get_Next_Field
                                 (Next_Line (Next .. Next_Line'Last), Next);
         Next_Row.Numeric := Get_Next_Field
                                 (Next_Line (Next .. Next_Line'Last), Next);
         --  Insert the item.
         Table.Append (Next_Row);
         end;
      end loop;
      Close (F);

      --  Create the output directory if it doesn't exist.
      Create_Path ("output");

      --  Delete iso-countries.ads if it exists, we're regenerating.
      if Exists ("output/iso-countries.ads") then
         Open (F, Out_File, "output/iso-countries.ads");
         Delete (F);
      end if;
      --  Create iso-countries.ads
      Create (F, Out_File, "output/iso-countries.ads");
      Put_Line (F, "with Ada.Locales;");
      Put_Line (F, "--  ****h* ISO/Countries");
      Put_Line (F, "--  DESCRIPTION");
      Put_Line (F, "--    Implimentation of ISO 3166-1 (Country Codes).");
      Put_Line (F, "--  SOURCE");
      Put_Line (F, "package ISO.Countries is");
      Put_Line (F, "--  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.Alpha2_Code");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The two-letter country code defined by ISO 3166-1.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    Ada.Locales.Country_Code");
      Put_Line (F, "   --  SOURCE");
      --  Alpha2 code with dynamic predicate of every possible options.
      Put_Line (F, "   subtype Alpha2_Code is String (1 .. 2)");
      Put_Line (F, "      with Dynamic_Predicate => Alpha2_Code in");
      Put (F, "         ");
      for I of Table loop
         Put (F, """" & To_String (I.Alpha_2) & """ | ");
      end loop;
      Put_Line (F, """ZZ"";");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.Alpha3_Code");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The three-letter country code defined by ISO 3166-1.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    String");
      Put_Line (F, "   --  SOURCE");
      --  Alpha3 code with dynamic predicate of every possible options.
      Put_Line (F, "   subtype Alpha3_Code is String (1 .. 3)");
      Put_Line (F, "      with Dynamic_Predicate => Alpha3_Code in");
      Put (F, "         ");
      for I of Table loop
         Put (F, """" & To_String (I.Alpha_3) & """ | ");
      end loop;
      Put_Line (F, """ZZZ"";");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.Numeric_Code");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The numeric country code defined by ISO 3166-1.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    Natural");
      Put_Line (F, "   --  SOURCE");
      --  Numeric code with dynamic predicate of every possible options.
      Put_Line (F, "   subtype Numeric_Code is Natural");
      Put_Line (F, "      with Dynamic_Predicate => Numeric_Code in");
      Put (F, "         ");
      for I of Table loop
         Put (F, To_String (I.Numeric) & " | ");
      end loop;
      Put_Line (F, "0;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.Country_Key");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    An enumeration of every iso 3166 alpha2 country code.");
      Put_Line (F, "   --    Each code starts with ""C_"", to prevent any enumeration conlficting");
      Put_Line (F, "   --    with a reserved word.  So you can cast any alpha2 to Country_Key by");
      Put_Line (F, "   --    doing ""Country_Key'Value (""C_"" & Alpha2)""");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Key : Country_Key := C_US;");
      Put_Line (F, "   --  SOURCE");
      --  All country keys.
      Put_Line (F, "   type Country_Key is (");
      for I of Table loop
         Put_Line (F, "      C_" & To_String (I.Alpha_2) & ", --  " & To_String (I.Name));
      end loop;
      Put_Line (F, "      C_ZZ  --  Undefined Country");
      Put_Line (F, "   );");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.Country");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The ISO_3166-1 country to be referenced.  When initializing, you can");
      Put_Line (F, "   --    set the key to Country_Key that will be utilizeed one of the functions");
      Put_Line (F, "   --    to access the Country.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Country : Country := (C_US); --  This is the USA.");
      Put_Line (F, "   --  METHODS");
      Put_Line (F, "   --  * Countries/ISO.Countries.Country/Name");
      Put_Line (F, "   --  * Countries/ISO.Countries.Country/Alpha2");
      Put_Line (F, "   --  * Countries/ISO.Countries.Country/Alpha3");
      Put_Line (F, "   --  * Countries/ISO.Countries.Country/Numeric");
      Put_Line (F, "   --  * Countries/ISO.Countries.Country/Country_Code");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    --  To create a country and initalize it to the USA,");
      Put_Line (F, "   --    --  then reference it like so:");
      Put_Line (F, "   --    My_Country : Country := (C_US); --  This is the USA.");
      Put_Line (F, "   --    To access the country's name, do so like so:");
      Put_Line (F, "   --    My_Country.Name --  ""United States of America (The)""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type Country is tagged record");
      Put_Line (F, "      Key : Country_Key := C_ZZ;");
      Put_Line (F, "   end record;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Countries.Country/Name");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the name of the provided country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: The ISO 3166 name of the current country.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := (C_AU);");
      Put_Line (F, "   --    The_Name : String := My_Country.Name; --  Will return ""Australia""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Name (This : Country) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Countries.Country/Alpha2");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the two-letter country code of the provided country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Alpha2_Code: ISO 3166 Alpha2 of the current country.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := (C_AU);");
      Put_Line (F, "   --    Code : Alhpa2_Code := My_Country.Alpha2; --  Will return ""AU""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Alpha2 (This : Country) return Alpha2_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Countries.Country/Alpha3");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the three-letter country code of the provided country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Alpha3_Code: ISO 3166 Alpha3 of the current country.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := (C_AU);");
      Put_Line (F, "   --    Code : Alpha3_Code := My_Country.Alpha3; --  Will return ""AUS""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Alpha3 (This : Country) return Alpha3_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Countries.Country/Numeric");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the numerical country code of the provided country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Numeric_Code: ISO 3166 number of the current country.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := (C_AU);");
      Put_Line (F, "   --    Number : Numeric_Code := My_Country.Numeric; --  Will return 40");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Numeric (This : Country) return Numeric_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Countries.Country/Country_Code");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the Ada.Locale.Country_Code of the provided country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    Ada.Locale.Country_Code: Ada's Country_Code of the current country.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := (C_AU);");
      Put_Line (F, "   --    Number : Ada.Locale.Country_Code := My_Country.Country_Code; -- ""AU""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Country_Code");
      Put_Line (F, "      (This : Country) return Ada.Locales.Country_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  Return all countries");
      Put_Line (F, "   --  function All_Countries return All_Countries;");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Countries/ISO.Countries.From_Alpha2");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a country from a provided Alpha2 string.");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Code - An alpha2 code, such as ""AU"" or ""US"".");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Country: Country corresponding to that alpha2 code.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := From_Alpha2(""AU"");");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Alpha2 (Code : Alpha2_Code) return Country;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Countries/ISO.Countries.From_Alpha3");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a country from a provided Alpha3 string.");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Code - An alpha3 code, such as ""AUS"" or ""USA"".");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Country: Country corresponding to that alpha3 code.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : Country := From_Alpha3(""AUS"");");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Alpha3 (Code : Alpha3_Code) return Country;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Countries/ISO.Countries.From_Numeric");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a country from a provided numerical code string.");
      Put_Line (F, "   --  EXAMPLES");
      Put_Line (F, "   --    My_Country_1 : Country := From_Alpha3(40);");
      Put_Line (F, "   --    My_Country_2 : Country := From_Alpha3(040);");
      Put_Line (F, "   --    My_Country_3 : Country := From_Alpha3(""040"");");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Number - A Numeric Code, either as a string or integer.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Country: Country corresponding to that numerical code.");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Numeric (Number : Numeric_Code) return Country;");
      Put_Line (F, "   function From_Numeric (Number : String) return Country;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Countries/ISO.Countries.From_Country_Code");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a country from a provided numerical code string.");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Code - A country code as defined in Ada.Locales.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Country: Country corresponding to that numerical code.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Country : Country := From_Alpha3(Ada.Locales.Country);");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Country_Code (Code : Ada.Locales.Country_Code) return Country;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Countries/ISO.Countries.All_Countries");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    All of the countries, utilizing the Country_Key as an index.");
      Put_Line (F, "   --    To conserve the stack");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Countries : constant All_Countries := Init_Countries;");
      Put_Line (F, "   --    US_Name : String := My_Countries (C_US).Name;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type All_Countries is array (Country_Key'Range) of Country;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Countries/ISO.Countries.Init_Countries");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Initialize all of the countries in an array.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.All_Countries: Array containing all countries.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Countries : constant All_Countries := Init_Countries;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Init_Countries return All_Countries;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "private");
      Put_Line (F, "   function Alpha3_To_Key (Alpha_3 : Alpha3_Code) return Country_Key;");
      Put_Line (F, "   function Numeric_To_Key (Numeric : Numeric_Code) return Country_Key;");
      Put_Line (F, "end ISO.Countries;");
      Close (F);

      --  Now generate iso-countries.adb

      --  Delete iso-countries.adb if it exists, we're regenerating.
      if Exists ("output/iso-countries.adb") then
         Open (F, Out_File, "output/iso-countries.adb");
         Delete (F);
      end if;
      --  Create iso-countries.adb
      Create (F, Out_File, "output/iso-countries.adb");
      Put_Line (F, "package body ISO.Countries is");
      Put_Line (F, "");
      Put_Line (F, "   --  *** Begin Methods/Member Functions ***");
      Put_Line (F, "   function Name    (This : Country) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for I of Table loop
         Put_Line (F, "         when C_" & To_String (I.Alpha_2) & " => return """ &  To_String (I.Name) &""";");
      end loop;
      Put_Line (F, "         when C_ZZ => return ""Undefined"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Name;");
      Put_Line (F, "   function Alpha2  (This : Country) return Alpha2_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for I of Table loop
         Put_Line (F, "         when C_" & To_String (I.Alpha_2) & " => return """ &  To_String (I.Alpha_2) &""";");
      end loop;
      Put_Line (F, "         when C_ZZ => return ""ZZ"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Alpha2;");
      Put_Line (F, "   function Alpha3  (This : Country) return Alpha3_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for I of Table loop
         Put_Line (F, "         when C_" & To_String (I.Alpha_2) & " => return """ &  To_String (I.Alpha_3) &""";");
      end loop;
      Put_Line (F, "         when C_ZZ => return ""ZZZ"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Alpha3;");
      Put_Line (F, "   function Numeric (This : Country) return Numeric_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for I of Table loop
         Put_Line (F, "         when C_" & To_String (I.Alpha_2) & " => return " &  To_String (I.Numeric) &";");
      end loop;
      Put_Line (F, "         when C_ZZ => return 0;");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Numeric;");
      Put_Line (F, "   function Country_Code");
      Put_Line (F, "      (This : Country) return Ada.Locales.Country_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return  Ada.Locales.Country_Code (Alpha2 (This));");
      Put_Line (F, "   end Country_Code;");
      Put_Line (F, "   --  *** End Methods/Member Functions ***");
      Put_Line (F, "");
      Put_Line (F, "   --  *** Creation Functions ***");
      Put_Line (F, "   function From_Alpha2 (Code : Alpha2_Code) return Country is");
      Put_Line (F, "      Result : constant Country := (Key => Country_Key'Value");
      Put_Line (F, "                                             (""C_"" & Code));");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result;");
      Put_Line (F, "   end From_Alpha2;");
      Put_Line (F, "   function From_Alpha3 (Code : Alpha3_Code) return Country is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : Country do");
      Put_Line (F, "         Result.Key := Alpha3_To_Key (Code);");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end From_Alpha3;");
      Put_Line (F, "   function From_Numeric (Number : Numeric_Code) return Country is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : Country do");
      Put_Line (F, "         Result.Key := Numeric_To_Key (Number);");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end From_Numeric;");
      Put_Line (F, "   function From_Numeric (Number : String) return Country is");
      Put_Line (F, "      Real_Number : constant Numeric_Code := Numeric_Code'Value (Number);");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : Country do");
      Put_Line (F, "         Result.Key := Numeric_To_Key (Real_Number);");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end From_Numeric;");
      Put_Line (F, "   function From_Country_Code (Code : Ada.Locales.Country_Code) return Country");
      Put_Line (F, "   is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return From_Alpha2 (Alpha2_Code (Code));");
      Put_Line (F, "   end From_Country_Code;");
      Put_Line (F, "   function Init_Countries return All_Countries is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : All_Countries do");
      Put_Line (F, "         for X in Country_Key'Range loop");
      Put_Line (F, "            Result (X) := (Key => X);");
      Put_Line (F, "         end loop;");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end Init_Countries;");
      Put_Line (F, "   --  *** End Creation Functions ***");
      Put_Line (F, "");
      Put_Line (F, "   --  *** Begin Lookup table functions ***");
      Put_Line (F, "   --  Lookup table to convert numeric to country key.");
      Put_Line (F, "   function Numeric_To_Key (Numeric : Numeric_Code) return Country_Key is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case Numeric is");
      for I of Table loop
         Put_Line (F, "         when " & To_String (I.Numeric) & " => return C_" &  To_String (I.Alpha_2) & ";");
      end loop;
      Put_Line (F, "         when 0 => return C_ZZ;");
      Put_Line (F, "         when others => return C_ZZ;");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Numeric_To_Key;");
      Put_Line (F, "   --  Lookup table to convert alpha_3 string to country key.");
      Put_Line (F, "   function Alpha3_To_Key (Alpha_3 : Alpha3_Code) return Country_Key is");
      Put_Line (F, "      --  Match the alpha 3 with a lookup table similar to Country_Key");
      Put (F, "      type Alpha3_Key is (");
      for I of Table loop
         Put (F, "C_" & To_String (I.Alpha_3) & ", ");
      end loop;
      Put_Line (F, "C_ZZZ);");
      Put_Line (F, "      Key : constant Alpha3_Key := Alpha3_Key'Value (""C_"" & Alpha_3);");
      Put_Line (F, "   begin");
      Put_Line (F, "      case Key is");
      for I of Table loop
         Put_Line (F, "         when C_" & To_String (I.Alpha_3) & " => return C_" &  To_String (I.Alpha_2) & ";");
      end loop;
      Put_Line (F, "         when C_ZZZ => return C_ZZ;");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Alpha3_To_Key;");
      Put_Line (F, "   --  *** End Lookup table functions ***");
      Put_Line (F, "");
      Put_Line (F, "end ISO.Countries;");
      Close (F);

      Put_Line ("Files output/iso-countries.ads and output/iso-countries.ads have been regenerated.");
   end Generate_Countries;
end Countries;
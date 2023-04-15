--  Generates into "<working directory>/output"
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

procedure Generator is
   --  Issue with file
   Invalid_File,
   Invalid_Args : exception;

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
   CSV_Name : constant String :=
             (if Argument_Count = 1 then Argument (1) else
               raise Invalid_Args with "Usage: ./generate ""countries.csv""");
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

   --  Delete iso_3166.ads if it exists, we're regenerating.
   if Exists ("output/iso_3166.ads") then
      Open (F, Out_File, "output/iso_3166.ads");
      Delete (F);
   end if;
   --  Create iso_3166.ads
   Create (F, Out_File, "output/iso_3166.ads");
   Put_Line (F, "with Ada.Locales;");
   Put_Line (F, "with Ada.Containers.Indefinite_Vectors;");
   Put_Line (F, "package ISO_3166 is");
   Put_Line (F, "");
   Put_Line (F, "   --  Raised if a country is not found.");
   Put_Line (F, "   Country_Not_Found : exception;");
   Put_Line (F, "   Invalid_Number    : exception;");
   Put_Line (F, "");
   Put_Line (F, "   --  Alpha-2 Code.");
   Put_Line (F, "   type Alpha_2_Code is new String (1 .. 2)");
   Put_Line (F, "      with Dynamic_Predicate =>");
   Put_Line (F, "         (for all E of Alpha_2_Code => E in 'A' .. 'Z');");
   Put_Line (F, "");
   Put_Line (F, "   --  Alpha-3 Code.");
   Put_Line (F, "   type Alpha_3_Code is new String (1 .. 3)");
   Put_Line (F, "      with Dynamic_Predicate =>");
   Put_Line (F, "         (for all E of Alpha_3_Code => E in 'A' .. 'Z');");
   Put_Line (F, "");
   Put_Line (F, "   --  The ISO_3166_1 country to be referenced.");
   Put_Line (F, "   type ISO_3166_1 (Name_Length : Natural) is record");
   Put_Line (F, "      Name    : String (1 .. Name_Length);");
   Put_Line (F, "      Alpha_2 : Alpha_2_Code;");
   Put_Line (F, "      Alpha_3 : Alpha_3_code;");
   Put_Line (F, "      Numeric : Natural;");
   Put_Line (F, "   end record;");
   Put_Line (F, "");
   Put_Line (F, "   --  Iteratable vector of all countries.");
   Put_Line (F, "   package Countries_Vector is new");
   Put_Line (F, "      Ada.Containers.Indefinite_Vectors");
   Put_Line (F, "         (Index_Type => Natural, Element_Type => ISO_3166_1);");
   Put_Line (F, "");
   Put_Line (F, "   --  Vector containing all countries.");
   Put_Line (F, "   subtype Countries is Countries_Vector.Vector;");
   Put_Line (F, "");
   Put_Line (F, "   --  Return a vector of all countries");
   Put_Line (F, "   function All_Countries return Countries;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Alpha_2 code.");
   Put_Line (F, "   function From_Alpha2 (Alpha_2 : Alpha_2_Code) return ISO_3166_1;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Alpha_3 code.");
   Put_Line (F, "   function From_Alpha3 (Alpha_3 : Alpha_3_Code) return ISO_3166_1;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Numerical code.");
   Put_Line (F, "   function From_Numeric (Numeric : Natural) return ISO_3166_1;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Numerical string.");
   Put_Line (F, "   function From_Numeric (Numeric : String) return ISO_3166_1;");
   Put_Line (F, "");
   Put_Line (F, "   --  From a Ada.Locales country");
   Put_Line (F, "   function From_Country_Code (Item : Ada.Locales.Country_Code) return ISO_3166_1;");
   Put_Line (F, "");
   Put_Line (F, "   --  Country to Ada.Locales Country_Code");
   Put_Line (F, "   function To_Country_Code (Item : ISO_3166_1) return Ada.Locales.Country_Code;");
   Put_Line (F, "");
   Put_Line (F, "private");
   Put_Line (F, "");
   --   Country Code keys, alpha2 and alpha3
   Put_Line (F, "   --  Country keys as alpha2 and alpha3");
   Put_Line (F, "   type Alpha_2_Key is (");
   for I of Table loop
      Put_Line (F, "    Key_" & To_String (I.Alpha_2) & ", ");
   end loop;
   Put_Line (F, "    NA);"); -- Top off the alpha_2 key with an NA
   Put_Line (F, "   type Alpha_3_Key is (");
   for I of Table loop
      Put_Line (F, "    Key_" & To_String (I.Alpha_3) & ", ");
   end loop;
   Put_Line (F, "    NA);"); -- Top off the alpha_3 key with an NA
   --  More built in functions.
   Put_Line (F, "");
   Put_Line (F, "   --  Lookup table to convert alpha_3 string to alpha_2 key.");
   Put_Line (F, "   function Alpha_3_to_Alpha_2 (Alpha_3 : Alpha_3_Code) return Alpha_2_Key;");
   Put_Line (F, "");
   Put_Line (F, "   --  Lookup table to convert numeric to alpha_2 key.");
   Put_Line (F, "   function Numeric_To_Alpha_2 (Numeric : Natural) return Alpha_2_Key;");
   Put_Line (F, "");
   Put_Line (F, "   --  Lookup table to match alpha_2 key to country.");
   Put_Line (F, "   function Alpha_2_Key_To_Country (Alpha_2 : Alpha_2_Key) return ISO_3166_1;");
   Put_Line (F, "");
   --  The country lookup table... that's the big one.
   Put_Line (F, "   --  Country lookup table");
   for I of Table loop
      Put_Line (F, "   Country_" & To_String (I.Alpha_2) & " : constant ISO_3166_1 := (");
      Put_Line (F, "      Name_Length => String'(""" & To_String (I.Name) & """)'Length,");
      Put_Line (F, "      Name        => """ & To_String (I.Name) & """,");
      Put_Line (F, "      Alpha_2     => """ & To_String (I.Alpha_2) & """,");
      Put_Line (F, "      Alpha_3     => """ & To_String (I.Alpha_3) & """,");
      Put_Line (F, "      Numeric     => " & To_String (I.Numeric) & ");");
   end loop;
   Put_Line (F, "end ISO_3166;");

   Close (F);

   --  Now generate iso_3166.adb

   --  Delete iso_3166.adb if it exists, we're regenerating.
   if Exists ("output/iso_3166.adb") then
      Open (F, Out_File, "output/iso_3166.adb");
      Delete (F);
   end if;
   --  Create iso_3166.adb
   Create (F, Out_File, "output/iso_3166.adb");
   Put_Line (F, "package body ISO_3166 is");
   Put_Line (F, "");
   Put_Line (F, "   --  *** Begin Public Functions ***");
   Put_Line (F, "   --   Retrive a country with a provided Alpha_2 code.");
   Put_Line (F, "   function From_Alpha2 (Alpha_2 : Alpha_2_Code) return ISO_3166_1");
   Put_Line (F, "   is");
   Put_Line (F, "      Country_Key : constant Alpha_2_Key := Alpha_2_Key' Value");
   Put_Line (F, "                                             (""Key_"" & String(Alpha_2));");
   Put_Line (F, "   begin");
   Put_Line (F, "      return Alpha_2_Key_To_Country (Country_Key);");
   Put_Line (F, "   exception");
   Put_Line (F, "      when CONSTRAINT_ERROR => raise Country_Not_Found;");
   Put_Line (F, "   end From_Alpha2;");
   Put_Line (F, "");
   Put_Line (F, "   -- Retrive a country with a provided Alpha_3 code.");
   Put_Line (F, "   function From_Alpha3 (Alpha_3 : Alpha_3_Code) return ISO_3166_1 is");
   Put_Line (F, "      --  Retrive alpha_2 key from alpha_3 key.");
   Put_Line (F, "      Country_Key : constant Alpha_2_Key := Alpha_3_to_Alpha_2 (Alpha_3);");
   Put_Line (F, "   begin");
   Put_Line (F, "      --  Retrieve country from alpha_2 key.");
   Put_Line (F, "      return Alpha_2_Key_To_Country (Country_Key);");
   Put_Line (F, "   exception");
   Put_Line (F, "      when CONSTRAINT_ERROR => raise Country_Not_Found;");
   Put_Line (F, "   end From_Alpha3;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Numerical code.");
   Put_Line (F, "   function From_Numeric (Numeric : Natural) return ISO_3166_1");
   Put_Line (F, "   is");
   Put_Line (F, "      Country_Key : constant Alpha_2_Key := Numeric_To_Alpha_2 (Numeric);");
   Put_Line (F, "   begin");
   Put_Line (F, "      return Alpha_2_Key_To_Country (Country_Key);");
   Put_Line (F, "   end From_Numeric;");
   Put_Line (F, "");
   Put_Line (F, "   --  Retrive a country with a provided Numerical string.");
   Put_Line (F, "   function From_Numeric (Numeric : String) return ISO_3166_1");
   Put_Line (F, "   is");
   Put_Line (F, "      Numeric_Number : constant Natural := Natural'Value (Numeric);");
   Put_Line (F, "   begin");
   Put_Line (F, "      return From_Numeric (Numeric_Number);");
   Put_Line (F, "   end From_Numeric;");
   Put_Line (F, "");
   Put_Line (F, "   --  From a Ada.Locales country");
   Put_Line (F, "   function From_Country_Code (Item : Ada.Locales.Country_Code) return ISO_3166_1");
   Put_Line (F, "   is");
   Put_Line (F, "      use Ada.Locales;");
   Put_Line (F, "   begin");
   Put_Line (F, "      return From_Alpha2 (Alpha_2_Code (Item));");
   Put_Line (F, "   exception");
   Put_Line (F, "      when CONSTRAINT_ERROR => raise Country_Not_Found;");
   Put_Line (F, "   end From_Country_Code;");
   Put_Line (F, "");
   Put_Line (F, "   --  Country to Ada.Locales Country_Code");
   Put_Line (F, "   function To_Country_Code (Item : ISO_3166_1) return Ada.Locales.Country_Code");
   Put_Line (F, "   is");
   Put_Line (F, "      use Ada.Locales;");
   Put_Line (F, "   begin");
   Put_Line (F, "      return Country_Code (Item.Alpha_2);");
   Put_Line (F, "   end To_Country_Code;");
   Put_Line (F, "");
   Put_Line (F, "   --  Return a vector of all countries");
   Put_Line (F, "   function All_Countries return Countries is");
   Put_Line (F, "      Result : Countries;");
   Put_Line (F, "   begin");
   for I of Table loop
      Put_Line (F, "      Result.Append(Country_" & To_String(I.Alpha_2) & ");");
   end loop;
   Put_Line (F, "      return Result;");
   Put_Line (F, "   end All_Countries;");
   Put_Line (F, "");
   Put_Line (F, "   --  *** End Public Functions ***");
   Put_Line (F, "");
   Put_Line (F, "   --  *** Begin Lookup table functions ***");
   Put_Line (F, "   --  Lookup table to convert alpha_3 string to alpha_2 key.");
   Put_Line (F, "   function Alpha_3_to_Alpha_2 (Alpha_3 : Alpha_3_Code) return Alpha_2_Key is");
   Put_Line (F, "      Country_Key : constant Alpha_3_Key := Alpha_3_Key'Value");
   Put_Line (F, "                                                (""Key_"" & String(Alpha_3));");
   Put_Line (F, "   begin");
   Put_Line (F, "      case Country_Key is");
   for I of Table loop
      Put_Line (F, "         when Key_" & To_String (I.Alpha_3) & " => return Key_" & To_String (I.Alpha_2) & ";");
   end loop;
   Put_Line (F, "         when others => raise Country_Not_Found;");
   Put_Line (F, "      end case;");
   Put_Line (F, "   end Alpha_3_to_Alpha_2;");
   Put_Line (F, "");
   Put_Line (F, "   --  Lookup table to convert numeric to alpha_2 key.");
   Put_Line (F, "   function Numeric_To_Alpha_2 (Numeric : Natural) return Alpha_2_Key is");
   Put_Line (F, "   begin");
   Put_Line (F, "      case Numeric is");
   for I of Table loop
      Put_Line (F, "         when " & To_String (I.Numeric) & " => return Key_" & To_String (I.Alpha_2) & ";");
   end loop;
   Put_Line (F, "         when others => raise Country_Not_Found;");
   Put_Line (F, "      end case;");
   Put_Line (F, "   end Numeric_To_Alpha_2;");
   Put_Line (F, "");
   Put_Line (F, "   --  Lookup table as function to match country table with key.");
   Put_Line (F, "   function Alpha_2_Key_To_Country (Alpha_2 : Alpha_2_Key) return ISO_3166_1 is");
   Put_Line (F, "   begin");
   Put_Line (F, "      -- Alpha_2 lookup table.");
   Put_Line (F, "      case Alpha_2 is");
   for I of Table loop
      Put_Line (F, "         when Key_" & To_String (I.Alpha_2) & " => return Country_" & To_String (I.Alpha_2) & ";");
   end loop;
   Put_Line (F, "         when others => raise Country_Not_Found;");
   Put_Line (F, "      end case;");
   Put_Line (F, "   end Alpha_2_Key_To_Country;");
   Put_Line (F, "");
   Put_Line (F, "   --  *** End Lookup table functions ***");
   Put_Line (F, "");
   Put_Line (F, "end ISO_3166;");

   Close (F);

   Put_Line ("Files output/iso_3166.ads and output/iso_3166.ads have been regenerated.");

end Generator;

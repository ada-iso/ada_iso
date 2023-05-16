pragma Ada_2022;
with DOM.Core;                    use DOM.Core;
with DOM.Readers;                 use DOM.Readers;
with DOM.Core.Nodes;              use DOM.Core.Nodes;
with DOM.Core.Attrs;              use DOM.Core.Attrs;
with DOM.Core.Documents;          use DOM.Core.Documents;
with Input_Sources.File;          use Input_Sources.File;
with Ada.Directories;             use Ada.Directories;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;
with Ada.Characters.Conversions;  use Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;
package body Currencies is
   function Load_Currencies
      return ISO_4217_Table.Map
   is
      Input  : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
      List   : Node_List;

      F : Ada.Wide_Wide_Text_IO.File_Type;

      --  Our map.  The currency code will be the index.
      Table : ISO_4217_Table.Map;
   begin
      --  Load our List1 XML file.
      Open (XML_List_1, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      --  Begin reading every country record
      List := Get_Elements_By_Tag_Name (Doc, "CcyNtry");
      for I in 0 .. Length (List) - 1 loop
         declare
            Next_Node : constant Node := Item (List, I);
            --  Read each item of country
            Next_List : constant Node_List := Child_Nodes (Next_Node);
            Next_Row : ISO_4217;
            Current_Country : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
         begin
            --  Loop through each child element (except for raw text)
            for J in 0 .. Length (Next_List) - 1
               -- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=109798
               -- when Node_Type (Item (Next_List, J)) /= Text_Node -- 
            loop
               if Node_Type (Item (Next_List, J)) /= Text_Node then
                  declare
                     Current_Node : constant Node := Item (Next_List, J);
                     Element_Name : constant String := Local_Name (Current_Node);
                     Element_Value : constant Unbounded_Wide_Wide_String := Get_Node_String (Current_Node);
                  begin
                     if Element_Name = "CtryNm" then
                        Current_Country := Element_Value;
                     elsif Element_Name = "CcyNm" then
                        Next_Row.Name := Element_Value;
                        --  Also check if this is a fund or not.
                        if Get_Named_Item (Attributes (Current_Node), "IsFund") /= null and then
                           Value (Get_Named_Item (Attributes (Current_Node), "IsFund")) /= "false"
                        then
                           Next_Row.Fund := True;
                        end if;
                     elsif Element_Name = "Ccy" then
                        Next_Row.Code := TUB (Element_Value);
                     elsif Element_Name = "CcyNbr" then
                        Next_Row.Numeric := TUB (Element_Value);
                     elsif Element_Name = "CcyMnrUnts" then
                        Next_Row.Unit := TUB (Element_Value);
                     end if;
                  end;
               end if;
            end loop;
            --  Add, but only if there's a valid code.
            if Next_Row.Code /= Null_Unbounded_String then
               if Table.Contains (Next_Row.Code) then
                  Table (Next_Row.Code).Countries.Append (Current_Country);
               else
                  Next_Row.Countries.Append (Current_Country);
                  Table.Include (Next_Row.Code, Next_Row);
               end if;
            end if;
         end;

      end loop;
      Free (List);
      Free (Reader);

      --  Load our List2 XML file.
      Open (XML_List_3, Input);
      Parse (Reader, Input);
      Close (Input);
      Doc := Get_Tree (Reader);
      List := Get_Elements_By_Tag_Name (Doc, "HstrcCcyNtry");
      for I in 0 .. Length (List) - 1 loop
         declare
            Next_Node : constant Node := Item (List, I);
            --  Read each item of country
            Next_List : constant Node_List := Child_Nodes (Next_Node);
            Next_Row : ISO_4217;
            Current_Country : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
         begin
            Next_Row.Historic := True;
            --  Loop through each child element (except for raw text)
            for J in 0 .. Length (Next_List) - 1
               -- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=109798
               -- when Node_Type (Item (Next_List, J)) /= Text_Node -- 
            loop
               if Node_Type (Item (Next_List, J)) /= Text_Node then
                  declare
                     Current_Node : constant Node := Item (Next_List, J);
                     Element_Name : constant String := Local_Name (Current_Node);
                     Element_Value : constant Unbounded_Wide_Wide_String := Get_Node_String (Current_Node);
                  begin
                     if Element_Name = "CtryNm" then
                        Current_Country := Element_Value;
                     elsif Element_Name = "CcyNm" then
                        Next_Row.Name := Element_Value;
                        --  Also check if this is a fund or not.
                        if Get_Named_Item (Attributes (Current_Node), "IsFund") /= null and then
                           Value (Get_Named_Item (Attributes (Current_Node), "IsFund")) /= "false"
                        then
                           Next_Row.Fund := True;
                        end if;
                     elsif Element_Name = "Ccy" then
                        Next_Row.Code := TUB (Element_Value);
                     elsif Element_Name = "CcyNbr" then
                        Next_Row.Numeric := TUB (Element_Value);
                     elsif Element_Name = "WthdrwlDt" then
                        Next_Row.Withdraw_Date := TUB (Element_Value);
                     end if;
                  end;
               end if;
            end loop;
            --  Add, but only if there's a valid code.
            if Next_Row.Code /= Null_Unbounded_String then

               if Table.Contains (Next_Row.Code) then

                  --  The code already exists.  Set it to historic.
                  if not Table (Next_Row.Code).Historic then
                     Table (Next_Row.Code).Historic := True;
                  end if;

                  --  This will end up returning the latest withdraw date
                  Table (Next_Row.Code).Withdraw_Date := Next_Row.Withdraw_Date;

                  --  If this row specifies it is a fund, then and it.
                  if Next_Row.Fund and then not Table (Next_Row.Code).Fund then
                     Table (Next_Row.Code).Fund := True;
                  end if;

                  --  Only overwrite the name and numeric if this is only historic
                  if Table (Next_Row.Code).Only_Historic then
                     Set_Unbounded_Wide_Wide_String (Table (Next_Row.Code).Name, TWS (Next_Row.Name));
                     Set_Unbounded_String (Table (Next_Row.Code).Numeric, To_String (Next_Row.Numeric));
                  end if;

                  --  Handle historical names.
                  if not Table (Next_Row.Code).Historic_Names.Contains (Next_Row.Name) then
                     Table (Next_Row.Code).Historic_Names.Append (Next_Row.Name);
                  end if;
                  --  Handle historical numerics.  We're not overriting the number, just adding.
                  if not Table (Next_Row.Code).Historic_Numbers.Contains (Next_Row.Numeric) then
                     Table (Next_Row.Code).Historic_Numbers.Append (Next_Row.Numeric);
                  end if;
                  --  Handle country names.
                  if not Table (Next_Row.Code).Historic_Countries.Contains (Current_Country) then
                     Table (Next_Row.Code).Historic_Countries.Append (Current_Country);
                  end if;
                  --  Historcial withdraw dates in format "Year1:Year2"
                  if Table (Next_Row.Code).Withdraw_Dates /= Null_Unbounded_String then
                     Append (Table (Next_Row.Code).Withdraw_Dates, ";");
                  end if;
                  Append (Table (Next_Row.Code).Withdraw_Dates, Next_Row.Withdraw_Date);
                  --  Handle historical record format in format
                  if Table (Next_Row.Code).Historic_Records /= Null_Unbounded_Wide_Wide_String then
                     Append (Table (Next_Row.Code).Historic_Records, ";");
                  end if;
                  Append (Table (Next_Row.Code).Historic_Records, Current_Country);
                  Append (Table (Next_Row.Code).Historic_Records, ":");
                  Append (Table (Next_Row.Code).Historic_Records, Next_Row.Name);
                  Append (Table (Next_Row.Code).Historic_Records, ":");
                  Append (Table (Next_Row.Code).Historic_Records, TWS (Next_Row.Numeric));
                  Append (Table (Next_Row.Code).Historic_Records, ":");
                  Append (Table (Next_Row.Code).Historic_Records, TWS (Next_Row.Withdraw_Date));
               else
                  Next_Row.Only_Historic := True; -- No non-historic record exists
                  --  Add initial reference vector stuff
                  Next_Row.Historic_Countries.Append (Current_Country);
                  Next_Row.Historic_Names.Append (Next_Row.Name);
                  Next_Row.Historic_Numbers.Append (Next_Row.Numeric);
                  Next_Row.Withdraw_Dates := Next_Row.Withdraw_Date;
                  --  Add initial historic record
                  Append (Next_Row.Historic_Records, Current_Country);
                  Append (Next_Row.Historic_Records, ":");
                  Append (Next_Row.Historic_Records, Next_Row.Name);
                  Append (Next_Row.Historic_Records, ":");
                  Append (Next_Row.Historic_Records, TWS (Next_Row.Numeric));
                  Append (Next_Row.Historic_Records, ":");
                  Append (Next_Row.Historic_Records, TWS (Next_Row.Withdraw_Date));
                  --  Add to the table.
                  Table.Include (Next_Row.Code, Next_Row);
               end if;
            end if;
         end;
      end loop;
      Free (List);
      Free (Reader);

      --  Begin reading in the symbols.
      Ada.Wide_Wide_Text_IO.Open (F, Ada.Wide_Wide_Text_IO.In_File, Symbol_List);
      --  Validate the header.
      if Ada.Wide_Wide_Text_IO.Get_Line (F) /= "Code,Symbol" then
         Ada.Wide_Wide_Text_IO.Close (F);
         raise Invalid_File with "Invalid columns";
      end if;
      while not Ada.Wide_Wide_Text_IO.End_Of_File (F) loop
         declare
            Next_Line : constant Wide_Wide_String := Ada.Wide_Wide_Text_IO.Get_Line (F);
            type Symbol_Row is record
               Code   : Unbounded_String;
               Symbol : Unbounded_Wide_Wide_String;
            end record;
            Comma    : constant Natural := Index (Next_Line, ",");
            Next_Row : constant Symbol_Row :=
                        (Code => To_Unbounded_String ( To_String (Next_Line (Next_Line'First .. Comma-1))),
                         Symbol => To_Unbounded_Wide_Wide_String (Next_Line (Comma+1 .. Next_Line'Last)));
         begin
            if Table.Contains(Next_Row.Code) then
               Table (Next_Row.Code).Symbol := Next_Row.Symbol;
            end if;
         end;
      end loop;
      Ada.Wide_Wide_Text_IO.Close (F);
      --  Sanatize the data
      for X of Table loop
         X.Name := Sanatize (X.Name);
         X.Historic_Records := Sanatize (X.Historic_Records);
         for Y of X.Historic_Countries loop
            Y := Sanatize (Y);
         end loop;
         for Y of X.Historic_Names loop
            Y := Sanatize (Y);
         end loop;
      end loop;
      return Table;
   end Load_Currencies;

   procedure Generate_Currencies (Table : ISO_4217_Table.Map; C : Countries.ISO_3166_1_Table.Vector) is
      use Ada.Wide_Wide_Text_IO; 

      F : File_Type;

      Max_Unit : Natural := 0;
   begin
      --  Calculate the max size of the minor unit.  This will be useful later.
      for X of Table
         when Is_Natural (X.Unit)
      loop
         declare
            N : constant Natural := To_Natural (X.Unit); 
         begin
            if N > Max_Unit then
               Max_Unit := N;
            end if;
         end;
         --Put_Line ("End of loop");
      end loop;
      --  Create the output directory if it doesn't exist.
      Create_Path ("output");

      --  Delete iso-currencies.ads if it exists, we're regenerating.
      if Exists ("output/iso-currencies.ads") then
         Open (F, Out_File, "output/iso-currencies.ads");
         Delete (F);
      end if;
      --  Create iso-currencies.ads
      Create (F, Out_File, "output/iso-currencies.ads");
      Put_Line (F, "with ISO.Countries;");
      Put_Line (F, "--  ****h* ISO/Currencies");
      Put_Line (F, "--  DESCRIPTION");
      Put_Line (F, "--    Implimentation of ISO 4217 (Currency Codes).");
      Put_Line (F, "--  SOURCE");
      Put_Line (F, "package ISO.Currencies is");
      Put_Line (F, "--  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Alphabetic_Code");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The three-letter currency code defined by ISO 4217.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    String");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   subtype Alphabetic_Code is String (1 .. 3)");
      Put_Line (F, "      with Dynamic_Predicate => Alphabetic_Code in");
      Put (F, "         ");
      for X of Table loop
         Put (F, """" & TWS (X.Code) & """ | ");
      end loop;
      Put_Line (F, """ZZZ"";");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Numeric_Code");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The numeric currency code defined by ISO 4217.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    Natural");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   subtype Numeric_Code is Natural");
      Put_Line (F, "      with Dynamic_Predicate => Numeric_Code in");
      Put (F, "         ");
      declare
         TmpV : Unbound_List.Vector;
      begin
         for X of Table
            when Is_Natural (X.Numeric) and then
                  not TmpV.Contains (X.Numeric)
         loop
            Put (F, TWS (X.Numeric) & " | ");
            TmpV.Append (X.Numeric);
         end loop;
      end;
      Put_Line (F, "0;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Currency_Key");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    An enumeration of every iso 4217 currency code.");
      Put_Line (F, "   --    Each code starts with ""C_"", to prevent any enumeration conlficting");
      Put_Line (F, "   --    with a reserved word.  So you can cast any currency to currency code by");
      Put_Line (F, "   --    doing ""Currency_Key'Value (""C_"" & currency_code)""");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Key : Currency_Key := C_USD;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type Currency_Key is (");
      for X of Table loop
         Put_Line (F, "      C_" & TWS (X.Code) & ", --  " & Trim (TWS (X.Name), Ada.Strings.Right));
      end loop;
      Put_Line (F, "      C_ZZZ --  Unknown");
      Put_Line (F, "   );");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Minor_Unit");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The unit of the currency defined by ISO 4217.");
      Put_Line (F, "   --    A minor unit of X where X > 0 would be a delta of 10 ** (-X).");
      Put_Line (F, "   --  NOTES");
      Put_Line (F, "   --    A minor unit of 0 may indicate ""N.A."" rather than 0.");
      Put_Line (F, "   --  DERIVED FROM");
      Put_Line (F, "   --    Natural");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   subtype Minor_Unit is Natural range 0 .. " & TWS(Max_Unit'Image) & ";");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Currency");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    The ISO 4217 currency to be referenced.  When initializing, you can");
      Put_Line (F, "   --    set the key to Currency_Key that will be utilizeed one of the functions");
      Put_Line (F, "   --    to access the Currency");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD); --  This is the US Doller.");
      Put_Line (F, "   --  METHODS");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Name");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Code");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Numeric");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Unit");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Symbol");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Is_Fund");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Is_Historic");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Historic_Name");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Historic_Numerics");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Withdraw_Date");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Withdraw_Dates");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Historic_Entities");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Historic_Records");
      Put_Line (F, "   --  * ISO.Currencies.Currency/Entities");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    --  To create a currency and initalize it to the US Doller,");
      Put_Line (F, "   --    --  then reference it like so:");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD); --  This is the USA.");
      Put_Line (F, "   --    --  To access the currency's name, do so like so:");
      Put_Line (F, "   --    My_Currency.Name --  ""US Doller""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type Currency is tagged record");
      Put_Line (F, "      Key : Currency_Key := C_ZZZ;");
      Put_Line (F, "   end record;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Name");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the name of the provided currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: The ISO 4217 name of the current currency.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD);");
      Put_Line (F, "   --    The_Name : String := My_Country.Name; --  ""US Doller""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Name (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Code");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the three-letter currency code of the provided currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Alphabetic_Code: ISO 4217 code of the current currency.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD);");
      Put_Line (F, "   --    Code : Alphabetic_Code := My_Currency.Code; --  Will return ""USD""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Code (This : Currency) return Alphabetic_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Numeric");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the numerical currency code of the provided currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Numeric_Code: ISO 4217 number of the current currency.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD);");
      Put_Line (F, "   --    Number : Numeric_Code := My_Currency.Numeric; --  Will return 840");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Numeric (This : Currency) return Numeric_Code;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Unit");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the minor unit of the provided currency.");
      Put_Line (F, "   --    A minor unit of X would be a delta of 10 ** (-X).");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Minor_Unit: ISO 4217 minor unit of the currency.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_USD);");
      Put_Line (F, "   --    Number : Minor_Unit := My_Currency.Unit; --  Will return 2");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Unit (This : Currency) return Minor_Unit;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Symbol");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    If applicable, return the symbol associated with a given currency.");
      Put_Line (F, "   --    This is not part of the ISO 4217 standard, but nice to have.");
      Put_Line (F, "   --    It may return a unicode-encoded string, so more than one character.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String or Wide_Wide_String: Current symbol captured by the currency or empty string if no symbol was found.");
      Put_Line (F, "   --  NOTES");
      Put_Line (F, "   --    Some symbols may be unicode; if that's the case, you may want to use a wide_wide_string.  Otherwise it will be converted into a String.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency1 : Currency := (C_USD);");
      Put_Line (F, "   --    My_Currency2 : Currency := (C_AZN);");
      Put_Line (F, "   --    Symb  : String := My_Currency1.Symbol; --  ""$""");
      Put_Line (F, "   --    Symb2 : Wide_Wide_String := My_Currency2.Symbol; -- ""₼""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Symbol (This : Currency) return Wide_Wide_String;");
      Put_Line (F, "   function Symbol (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Is_Fund");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Indicates whether or not the currency code is a fund.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --   * True is the currency is a fund.");
      Put_Line (F, "   --   * False if the currency is not fund.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    declare");
      Put_Line (F, "   --      My_Currency : Currency := (C_CLF);");
      Put_Line (F, "   --    begin");
      Put_Line (F, "   --      if My_Currency.Is_Fund then");
      Put_Line (F, "   --        Put_Line (""Is a fund."");");
      Put_Line (F, "   --      end if;");
      Put_Line (F, "   --    end;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Is_Fund (This : Currency) return Boolean;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Is_Historic");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Indicates whether or not the currency code is a historc record");
      Put_Line (F, "   --    contained in ISO 4217 List-3.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --   * True is the currency is historic (in ISO 4217-3).");
      Put_Line (F, "   --   * False if the currency is not historic.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    declare");
      Put_Line (F, "   --      My_Currency : Currency := (C_AFA);");
      Put_Line (F, "   --    begin");
      Put_Line (F, "   --      if My_Currency.Is_Historic then");
      Put_Line (F, "   --        Put_Line (""Is a historic code."");");
      Put_Line (F, "   --      end if;");
      Put_Line (F, "   --    end;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Is_Historic (This : Currency) return Boolean;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Withdraw_Date");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the MOST RECENT withdraw date for the historical record of the conutry code.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --   String: Contains the historical withdraw date of the code specificed in ISO 4217.  This may be a year and a month or a range, or a null string if the country is not historic.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    Currency1 : Currency := (C_AFA);");
      Put_Line (F, "   --    Currency2 : Currency := (C_CSJ);");
      Put_Line (F, "   --    Date1     : String   := Currency1.Withdraw_Date; -- ""2003-01""");
      Put_Line (F, "   --    Date2     : String   := Currency2.Withdraw_Date; -- ""1989 to 1990""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Withdraw_Date (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Withdraw_Dates");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve a semi colon separated list of historic withdraw dates associated with the iso 4217-3 standard as a string.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --   String: Contains the historical withdraw date of the code specificed in ISO 4217.  This may be a year and a month or a range, or a null string if the country is not historic.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    Currency : Currency := (VEF);");
      Put_Line (F, "   --    Dates    : String   := Currency.Withdraw_Dates; -- ""2011-12;2016-02;2018-08""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Withdraw_Dates (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Historic_Name");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the historic names of the provided currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: A semi colon separated list of historic names. Empty string if not historic.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_VEF);");
      Put_Line (F, "   --    The_Names : String := My_Country.Historic_Names; --  ""Bolivar Fuerte;Bolivar;Bolívar""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Historic_Names (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Historic_Numerics");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve the historic numermic values of the provided currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: A semi colon separated list of historic Numeric values.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_VEF);");
      Put_Line (F, "   --    The_Names : String := My_Country.Historic_Numeric; --  ""937""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Historic_Numerics (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Historic_Entities");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve a semi colon separated list of historic countries associated with the iso 4217-3 standard as a string.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: List of historical countries associated with a record.");
      Put_Line (F, "   --  TODO");
      Put_Line (F, "   --    Handle historic country records utilizing the iso standard for historic countries.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_VEF);");
      Put_Line (F, "   --    My_Counties : String := My_Currency.Historic_Entities; -- ""VENEZUELA;VENEZUELA (BOLIVARIAN REPUBLIC OF)""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Historic_Entities (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Historic_Records");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve a colon-and-semi colon separated list of the full iso 4217-3 records as a string.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    String: List of historical record for a currency.");
      Put_Line (F, "   --  TODO");
      Put_Line (F, "   --    Handle historic country records utilizing the iso standard for historic countries.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_VEF);");
      Put_Line (F, "   --    My_Records : String := My_Currency.Historic_Records; -- ""VENEZUELA:Bolivar Fuerte:937:2011-12;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolivar:937:2016-02;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolívar:937:2018-08""");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Historic_Records (This : Currency) return String;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****m* ISO.Currencies.Currency/Entities");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve all countries currently associated with a currency.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Countries.Country_List: Array containing the countries.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := (C_GBP);");
      Put_Line (F, "   --    My_Counties : ISO.Countries.Country_List := My_Currency.Entities; -- returns the United Kingdom, Jersey, Isle of Man, and Guernsey.");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Entities (This : Currency) return Countries.Country_List;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Currencies/ISO.Currencies.From_Code");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a currency from a provided alphabetic code string.");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Code - An alphabetic code, such as ""EUR"" or ""USD"".");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Currency: Currency corresponding to that code.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Currency : Currency := From_Code(""USD"");");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Code (Code : Alphabetic_Code) return Currency;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Currencies/ISO.Currencies.From_Numeric");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Create a non-historical currency from a provided numerical code string.");
      Put_Line (F, "   --  NOTES");
      Put_Line (F, "   --    This will only create non-historical currencies, since some historical curriences have the same number but different symbol.");
      Put_Line (F, "   --  TODO");
      Put_Line (F, "   --    Create ""function From_Numeric (Number : Numeric_Code) return Currency_List"" with historical support");
      Put_Line (F, "   --  EXAMPLES");
      Put_Line (F, "   --    My_Currency_1 : Currency := From_Numeric(36);");
      Put_Line (F, "   --    My_Currency_2 : Currency := From_Numeric(036);");
      Put_Line (F, "   --    My_Currency_3 : Currency := From_Numeric(""036"");");
      Put_Line (F, "   --  PARAMETERS");
      Put_Line (F, "   --    Number - A Numeric Code, either as a string or integer.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Currency: Currency corresponding to that numerical code.");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Numeric (Number : Numeric_Code) return Currency;");
      Put_Line (F, "   function From_Numeric (Number : String) return Currency;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.Currency_List");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    An arbitrary-sized array of currencies.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    declare");
      Put_Line (F, "   --       My_Currencies : Currency_List (1 .. 2);");
      Put_Line (F, "   --    begin");
      Put_Line (F, "   --       My_Currencies (1) := (Key => C_USD);");
      Put_Line (F, "   --       My_Currencies (2) := (Key => C_EUR);");
      Put_Line (F, "   --    end;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type Currency_List is array (Positive range <>) of Currency;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****t* Currencies/ISO.Currencies.All_Currencies");
      Put_Line (F, "   --  DESCRIPTION");
      Put_Line (F, "   --    All of the currencies, utilizing the Currency_Key as an index to");
      Put_Line (F, "   --    conserve the stack.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Currencies : constant All_Currencies := Init_Currencies;");
      Put_Line (F, "   --    USD_Name : String := My_Currencies (C_USD).Name;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   type All_Currencies is array (Currency_Key'Range) of Currency;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Currencies/ISO.Currencies.From_Country");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Retrieve all currencies currently associated with a country.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.Currency_List: Array containing the currencies.");
      Put_Line (F, "   --  EXAMPLE");
      Put_Line (F, "   --    My_Country : ISO.Countries.Country := (C_BO); --  Bolivia");
      Put_Line (F, "   --    My_Currencies : constant Currency_List := From_Country(My_Country); --  Returns Boliviano and Mvdol.");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function From_Country (Item : Countries.Country) return Currency_List;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "");
      Put_Line (F, "   --  ****f* Currencies/ISO.Currencies.Init_Currencies");
      Put_Line (F, "   --  FUNCTION");
      Put_Line (F, "   --    Initialize all of the currencies in an array.");
      Put_Line (F, "   --  RETURN VALUE");
      Put_Line (F, "   --    ISO.Currencies.All_Currencies: Array containing all countries.");
      Put_Line (F, "   --  USAGE");
      Put_Line (F, "   --    My_Currencies : constant All_Currencies := Init_Currencies;");
      Put_Line (F, "   --  SOURCE");
      Put_Line (F, "   function Init_Currencies return All_Currencies;");
      Put_Line (F, "   --  ****");
      Put_Line (F, "private");
      Put_Line (F, "   function Numeric_To_Key (Numeric : Numeric_Code) return Currency_Key;");
      Put_Line (F, "end ISO.Currencies;");
      Close (F);

      --  Delete iso-currencies.adb if it exists, we're regenerating.
      if Exists ("output/iso-currencies.adb") then
         Open (F, Out_File, "output/iso-currencies.adb");
         Delete (F);
      end if;

      Create (F, Out_File, "output/iso-currencies.adb");
      Put_Line (F, "with Ada.Characters.Conversions;");
      Put_Line (F, "package body ISO.Currencies is");
      Put_Line (F, "");
      Put_Line (F, "   function Name (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when X.Name /= Null_Unbounded_Wide_Wide_String
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Name) & """;");
      end loop;
      Put_Line (F, "         when others => return ""Unknown"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Name;");
      Put_Line (F, "");
      Put_Line (F, "   function Code (This : Currency) return Alphabetic_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Code) & """;");
      end loop;
      Put_Line (F, "         when C_ZZZ => return ""ZZZ"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Code;");
      Put_Line (F, "");
      Put_Line (F, "   function Numeric (This : Currency) return Numeric_Code is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when Is_Natural (X.Numeric)
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return " & TWS (X.Numeric) & ";");
      end loop;
      Put_Line (F, "         when others => return 0;");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Numeric;");
      Put_Line (F, "");
      Put_Line (F, "   function Unit (This : Currency) return Minor_Unit is");
      Put_Line (F, "   begin");
      if Max_Unit > 0 then
         Put_Line (F, "      case This.Key is");
         for I in 1 .. Max_Unit loop
            declare
               --  Just to verify if there's at least one entry.
               Added_When : Boolean := False;
            begin
               for X of Table
                  when Is_Natural (X.Unit) and then To_Natural (X.Unit) = I
               loop
                  if not Added_When then
                     Put (F, "         when C_" & TWS (X.Code));
                     Added_When := True;
                  else
                     Put (F, " | C_" & TWS (X.Code));
                  end if;
               end loop;
               if Added_When then
                  Put_Line (F, " => return" & TWS (I'Image) & ";");
               end if;
            end;
         end loop;
         Put_Line (F, "         when others => return 0;");
         Put_Line (F, "      end case;");
      else
         Put_Line (F, "      return 0;");
      end if;
      Put_Line (F, "   end Unit;");
      Put_Line (F, "");
      Put_Line (F, "   function Symbol (This : Currency) return Wide_Wide_String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when X.Symbol /= Null_Unbounded_Wide_Wide_String
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Symbol) & """;");
      end loop;               --  "¤" is the "unspecified currency" symbol
      Put_Line (F, "         when others => return ""¤"";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Symbol;");
      Put_Line (F, "");
      Put_Line (F, "   function Symbol (This : Currency) return String is");
      Put_Line (F, "      use Ada.Characters.Conversions;");
      Put_Line (F, "   begin");
      Put_Line (F, "      return To_String (This.Symbol);");
      Put_Line (F, "   end Symbol;");
      Put_Line (F, "");
      Put_Line (F, "   function Is_Fund (This : Currency) return Boolean is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      declare
         Added_When : Boolean := False;
      begin
         for X of Table
            when X.Fund
         loop
            if not Added_When then
               Put (F, "         when C_" & TWS (X.Code));
               Added_When := True;
            else
               Put (F, " | C_" & TWS (X.Code));
            end if;
         end loop;
         if Added_When then
            Put_Line (F, " => return True;");
         end if;
         Put_Line (F, "         when others => return False;");
      end;
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Is_Fund;");
      Put_Line (F, "");
      Put_Line (F, "   function Is_Historic (This : Currency) return Boolean is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      declare
         Added_When : Boolean := False;
      begin
         for X of Table
            when X.Historic
         loop
            if not Added_When then
               Put (F, "         when C_" & TWS (X.Code));
               Added_When := True;
            else
               Put (F, " | C_" & TWS (X.Code));
            end if;
         end loop;
         if Added_When then
            Put_Line (F, " => return True;");
         end if;
         Put_Line (F, "         when others => return False;");
      end;
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Is_Historic;");
      Put_Line (F, "");
      Put_Line (F, "   function Withdraw_Date (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when X.Withdraw_Date /= Null_Unbounded_String
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Withdraw_Date) & """;");
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Withdraw_Date;");
      Put_Line (F, "");
      Put_Line (F, "   function Withdraw_Dates (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when X.Withdraw_Dates /= Null_Unbounded_String
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Withdraw_Dates) & """;");
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Withdraw_Dates;");
      Put_Line (F, "");
      Put_Line (F, "   function Historic_Names (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when not X.Historic_Names.Is_Empty
      loop
         declare
            Added_When : Boolean := False;
         begin
            Put (F, "         when C_" & TWS (X.Code) & " => return """);
            for Y of X.Historic_Names loop
               if not Added_When then
                  Put (F, TWS (Y));
                  Added_When := True;
               else
                  Put (F, ";");
                  Put (F, TWS (Y));
               end if;
            end loop;
            Put_Line (F, """;");
         end;
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Historic_Names;");
      Put_Line (F, "");
      Put_Line (F, "   function Historic_Numerics (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when not X.Historic_Numbers.Is_Empty
      loop
         declare
            Added_When : Boolean := False;
         begin
            for Y of X.Historic_Numbers
               when Is_Natural (Y)
            loop
               if not Added_When then
                  Put (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (Y));
                  Added_When := True;
               else
                  Put (F, ";");
                  Put (F, TWS (Y));
               end if;
            end loop;
            if Added_When then
               Put_Line (F, """;");
            end if;
         end;
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Historic_Numerics;");
      Put_Line (F, "");
      Put_Line (F, "   function Historic_Entities (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when not X.Historic_Countries.Is_Empty
      loop
         declare
            Added_When : Boolean := False;
         begin
            Put (F, "         when C_" & TWS (X.Code) & " => return """);
            for Y of X.Historic_Countries loop
               if not Added_When then
                  Put (F, TWS (Y));
                  Added_When := True;
               else
                  Put (F, ";");
                  Put (F, TWS (Y));
               end if;
            end loop;
            Put_Line (F, """;");
         end;
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Historic_Entities;");
      Put_Line (F, "");
      Put_Line (F, "   function Historic_Records (This : Currency) return String is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when X.Historic_Records /= Null_Unbounded_Wide_Wide_String
      loop
         Put_Line (F, "         when C_" & TWS (X.Code) & " => return """ & TWS (X.Historic_Records) & """;");
      end loop;
      Put_Line (F, "         when others => return """";");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Historic_Records;");
      Put_Line (F, "");
      Put_Line (F, "   function Entities (This : Currency) return Countries.Country_List is");
      Put_Line (F, "      use Countries;");
      Put_Line (F, "   begin");
      Put_Line (F, "      case This.Key is");
      for X of Table
         when not X.Countries.Is_Empty
       loop
         declare
            Idx : Positive := 1;
         begin
            for Y of X.Countries
               when Countries.Search_Country_Name (C, Y) > 0
            loop
               declare
                  Our_Code : constant Unbounded_String := C (Countries.Search_Country_Name (C, Y)).Alpha_2;
               begin
                  if Idx = 1 then
                     Put_Line (F, "         when C_" & TWS (X.Code) & " => return ("); 
                  else
                     Put_Line (F, ",");
                  end if;
                  Put (F, "            " & TWS (Idx'Image) & " => (Key => C_" & TWS (Our_Code) & ")");
                  Idx := Idx + 1;
               end;
            end loop;
            if Idx > 1 then
               Put_Line (F, "");
               Put_Line (F, "            );");
            end if;
         end;
      end loop;
      Put_Line (F, "         when others => return (");
      Put_Line (F, "            1 => (Key => C_ZZ)");
      Put_Line (F, "            );");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Entities;");
      Put_Line (F, "");
      Put_Line (F, "   function From_Code (Code : Alphabetic_Code) return Currency is");
      Put_Line (F, "      Result : constant Currency := (Key => Currency_Key'Value");
      Put_Line (F, "                                             (""C_"" & Code));");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result;");
      Put_Line (F, "   end From_Code;");
      Put_Line (F, "");
      Put_Line (F, "   function From_Numeric (Number : Numeric_Code) return Currency is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : Currency do");
      Put_Line (F, "         Result.Key := Numeric_To_Key (Number);");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end From_Numeric;");
      Put_Line (F, "   function From_Numeric (Number : String) return Currency is");
      Put_Line (F, "      Real_Number : constant Numeric_Code := Numeric_Code'Value (Number);");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : Currency do");
      Put_Line (F, "         Result.Key := Numeric_To_Key (Real_Number);");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end From_Numeric;");
      Put_Line (F, "");
      Put_Line (F, "   function From_Country (Item : Countries.Country) return Currency_List is");
      Put_Line (F, "      use Countries;");
      Put_Line (F, "   begin");
      Put_Line (F, "      case Item.Key is");
      for X of C loop
         declare
            Idx : Natural := 1;
         begin
            for Y of Table
               when Y.Countries.Contains (X.Name)
            loop
               --  We have at least one result
               if Idx = 1 then
                  Put_Line (F, "         when C_" & TWS (X.Alpha_2) & " => return (");
               else
                  Put_Line (F, ",");
               end if;
               Put (F, "            " & TWS (Idx'Image) & " => (Key => C_" & TWS (Y.Code) & ")");
               Idx := Idx + 1;
            end loop;
            if Idx > 1 then
               Put_Line (F, "");
               Put_Line (F, "            );");
            end if;
         end;
      end loop;
      Put_Line (F, "         when others => return (");
      Put_Line (F, "            1 => (Key => C_ZZZ)");
      Put_Line (F, "            );");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end From_Country;");
      Put_Line (F, "");
      Put_Line (F, "   function Init_Currencies return All_Currencies is");
      Put_Line (F, "   begin");
      Put_Line (F, "      return Result : All_Currencies do");
      Put_Line (F, "         for X in Currency_Key'Range loop");
      Put_Line (F, "            Result (X) := (Key => X);");
      Put_Line (F, "         end loop;");
      Put_Line (F, "      end return;");
      Put_Line (F, "   end Init_Currencies;");
      Put_Line (F, "");
      Put_Line (F, "   function Numeric_To_Key (Numeric : Numeric_Code) return Currency_Key is");
      Put_Line (F, "   begin");
      Put_Line (F, "      case Numeric is");
      declare
         --  This is to hold a record of numerics already here.
         TmpV : Unbound_List.Vector;
      begin
         --  First search non-historic numerics
         for X of Table
            when not X.Only_Historic and then
                  Is_Natural (X.Numeric) and then
                  not TmpV.Contains (X.Numeric)
         loop
            TmpV.Append (X.Numeric);
            Put_Line (F, "         when " & TWS (X.Numeric) & " => return C_" & TWS (X.Code) & ";");
         end loop;
      end;
      Put_Line (F, "         when others => return C_ZZZ;");
      Put_Line (F, "      end case;");
      Put_Line (F, "   end Numeric_To_Key;");
      Put_Line (F, "end ISO.Currencies;");
      Close (F);
      Put_Line ("Files output/iso-currencies.ads and output/iso-currencies.adb have been regenerated.");

   end Generate_Currencies;
end Currencies;
with Ada.Locales;
--  ****h* ISO/Countries
--  DESCRIPTION
--    Implimentation of ISO 3166-1 (Country Codes).
--  SOURCE
package ISO.Countries is
--  ****

   --  ****t* Countries/ISO.Countries.Alpha2_Code
   --  DESCRIPTION
   --    The two-letter country code defined by ISO 3166-1.
   --  DERIVED FROM
   --    Ada.Locales.Country_Code
   --  SOURCE
   subtype Alpha2_Code is String (1 .. 2)
      with Dynamic_Predicate => Alpha2_Code in
         "AF" | "AL" | "DZ" | "AS" | "AD" | "AO" | "AI" | "AQ" | "AG" | "AR" | "AM" | "AW" | "AU" | "AT" | "AZ" | "BS" | "BH" | "BD" | "BB" | "BY" | "BE" | "BZ" | "BJ" | "BM" | "BT" | "BO" | "BQ" | "BA" | "BW" | "BV" | "BR" | "IO" | "BN" | "BG" | "BF" | "BI" | "CV" | "KH" | "CM" | "CA" | "KY" | "CF" | "TD" | "CL" | "CN" | "CX" | "CC" | "CO" | "KM" | "CD" | "CG" | "CK" | "CR" | "HR" | "CU" | "CW" | "CY" | "CZ" | "CI" | "DK" | "DJ" | "DM" | "DO" | "EC" | "EG" | "SV" | "GQ" | "ER" | "EE" | "SZ" | "ET" | "FK" | "FO" | "FJ" | "FI" | "FR" | "GF" | "PF" | "TF" | "GA" | "GM" | "GE" | "DE" | "GH" | "GI" | "GR" | "GL" | "GD" | "GP" | "GU" | "GT" | "GG" | "GN" | "GW" | "GY" | "HT" | "HM" | "VA" | "HN" | "HK" | "HU" | "IS" | "IN" | "ID" | "IR" | "IQ" | "IE" | "IM" | "IL" | "IT" | "JM" | "JP" | "JE" | "JO" | "KZ" | "KE" | "KI" | "KP" | "KR" | "KW" | "KG" | "LA" | "LV" | "LB" | "LS" | "LR" | "LY" | "LI" | "LT" | "LU" | "MO" | "MG" | "MW" | "MY" | "MV" | "ML" | "MT" | "MH" | "MQ" | "MR" | "MU" | "YT" | "MX" | "FM" | "MD" | "MC" | "MN" | "ME" | "MS" | "MA" | "MZ" | "MM" | "NA" | "NR" | "NP" | "NL" | "NC" | "NZ" | "NI" | "NE" | "NG" | "NU" | "NF" | "MK" | "MP" | "NO" | "OM" | "PK" | "PW" | "PS" | "PA" | "PG" | "PY" | "PE" | "PH" | "PN" | "PL" | "PT" | "PR" | "QA" | "RO" | "RU" | "RW" | "RE" | "BL" | "SH" | "KN" | "LC" | "MF" | "PM" | "VC" | "WS" | "SM" | "ST" | "SA" | "SN" | "RS" | "SC" | "SL" | "SG" | "SX" | "SK" | "SI" | "SB" | "SO" | "ZA" | "GS" | "SS" | "ES" | "LK" | "SD" | "SR" | "SJ" | "SE" | "CH" | "SY" | "TW" | "TJ" | "TZ" | "TH" | "TL" | "TG" | "TK" | "TO" | "TT" | "TN" | "TM" | "TC" | "TV" | "TR" | "UG" | "UA" | "AE" | "GB" | "UM" | "US" | "UY" | "UZ" | "VU" | "VE" | "VN" | "VG" | "VI" | "WF" | "EH" | "YE" | "ZM" | "ZW" | "AX" | "ZZ";
   --  ****

   --  ****t* Countries/ISO.Countries.Alpha3_Code
   --  DESCRIPTION
   --    The three-letter country code defined by ISO 3166-1.
   --  DERIVED FROM
   --    String
   --  SOURCE
   subtype Alpha3_Code is String (1 .. 3)
      with Dynamic_Predicate => Alpha3_Code in
         "AFG" | "ALB" | "DZA" | "ASM" | "AND" | "AGO" | "AIA" | "ATA" | "ATG" | "ARG" | "ARM" | "ABW" | "AUS" | "AUT" | "AZE" | "BHS" | "BHR" | "BGD" | "BRB" | "BLR" | "BEL" | "BLZ" | "BEN" | "BMU" | "BTN" | "BOL" | "BES" | "BIH" | "BWA" | "BVT" | "BRA" | "IOT" | "BRN" | "BGR" | "BFA" | "BDI" | "CPV" | "KHM" | "CMR" | "CAN" | "CYM" | "CAF" | "TCD" | "CHL" | "CHN" | "CXR" | "CCK" | "COL" | "COM" | "COD" | "COG" | "COK" | "CRI" | "HRV" | "CUB" | "CUW" | "CYP" | "CZE" | "CIV" | "DNK" | "DJI" | "DMA" | "DOM" | "ECU" | "EGY" | "SLV" | "GNQ" | "ERI" | "EST" | "SWZ" | "ETH" | "FLK" | "FRO" | "FJI" | "FIN" | "FRA" | "GUF" | "PYF" | "ATF" | "GAB" | "GMB" | "GEO" | "DEU" | "GHA" | "GIB" | "GRC" | "GRL" | "GRD" | "GLP" | "GUM" | "GTM" | "GGY" | "GIN" | "GNB" | "GUY" | "HTI" | "HMD" | "VAT" | "HND" | "HKG" | "HUN" | "ISL" | "IND" | "IDN" | "IRN" | "IRQ" | "IRL" | "IMN" | "ISR" | "ITA" | "JAM" | "JPN" | "JEY" | "JOR" | "KAZ" | "KEN" | "KIR" | "PRK" | "KOR" | "KWT" | "KGZ" | "LAO" | "LVA" | "LBN" | "LSO" | "LBR" | "LBY" | "LIE" | "LTU" | "LUX" | "MAC" | "MDG" | "MWI" | "MYS" | "MDV" | "MLI" | "MLT" | "MHL" | "MTQ" | "MRT" | "MUS" | "MYT" | "MEX" | "FSM" | "MDA" | "MCO" | "MNG" | "MNE" | "MSR" | "MAR" | "MOZ" | "MMR" | "NAM" | "NRU" | "NPL" | "NLD" | "NCL" | "NZL" | "NIC" | "NER" | "NGA" | "NIU" | "NFK" | "MKD" | "MNP" | "NOR" | "OMN" | "PAK" | "PLW" | "PSE" | "PAN" | "PNG" | "PRY" | "PER" | "PHL" | "PCN" | "POL" | "PRT" | "PRI" | "QAT" | "ROU" | "RUS" | "RWA" | "REU" | "BLM" | "SHN" | "KNA" | "LCA" | "MAF" | "SPM" | "VCT" | "WSM" | "SMR" | "STP" | "SAU" | "SEN" | "SRB" | "SYC" | "SLE" | "SGP" | "SXM" | "SVK" | "SVN" | "SLB" | "SOM" | "ZAF" | "SGS" | "SSD" | "ESP" | "LKA" | "SDN" | "SUR" | "SJM" | "SWE" | "CHE" | "SYR" | "TWN" | "TJK" | "TZA" | "THA" | "TLS" | "TGO" | "TKL" | "TON" | "TTO" | "TUN" | "TKM" | "TCA" | "TUV" | "TUR" | "UGA" | "UKR" | "ARE" | "GBR" | "UMI" | "USA" | "URY" | "UZB" | "VUT" | "VEN" | "VNM" | "VGB" | "VIR" | "WLF" | "ESH" | "YEM" | "ZMB" | "ZWE" | "ALA" | "ZZZ";
   --  ****

   --  ****t* Countries/ISO.Countries.Numeric_Code
   --  DESCRIPTION
   --    The numeric country code defined by ISO 3166-1.
   --  DERIVED FROM
   --    Natural
   --  SOURCE
   subtype Numeric_Code is Natural
      with Dynamic_Predicate => Numeric_Code in
         004 | 008 | 012 | 016 | 020 | 024 | 660 | 010 | 028 | 032 | 051 | 533 | 036 | 040 | 031 | 044 | 048 | 050 | 052 | 112 | 056 | 084 | 204 | 060 | 064 | 068 | 535 | 070 | 072 | 074 | 076 | 086 | 096 | 100 | 854 | 108 | 132 | 116 | 120 | 124 | 136 | 140 | 148 | 152 | 156 | 162 | 166 | 170 | 174 | 180 | 178 | 184 | 188 | 191 | 192 | 531 | 196 | 203 | 384 | 208 | 262 | 212 | 214 | 218 | 818 | 222 | 226 | 232 | 233 | 748 | 231 | 238 | 234 | 242 | 246 | 250 | 254 | 258 | 260 | 266 | 270 | 268 | 276 | 288 | 292 | 300 | 304 | 308 | 312 | 316 | 320 | 831 | 324 | 624 | 328 | 332 | 334 | 336 | 340 | 344 | 348 | 352 | 356 | 360 | 364 | 368 | 372 | 833 | 376 | 380 | 388 | 392 | 832 | 400 | 398 | 404 | 296 | 408 | 410 | 414 | 417 | 418 | 428 | 422 | 426 | 430 | 434 | 438 | 440 | 442 | 446 | 450 | 454 | 458 | 462 | 466 | 470 | 584 | 474 | 478 | 480 | 175 | 484 | 583 | 498 | 492 | 496 | 499 | 500 | 504 | 508 | 104 | 516 | 520 | 524 | 528 | 540 | 554 | 558 | 562 | 566 | 570 | 574 | 807 | 580 | 578 | 512 | 586 | 585 | 275 | 591 | 598 | 600 | 604 | 608 | 612 | 616 | 620 | 630 | 634 | 642 | 643 | 646 | 638 | 652 | 654 | 659 | 662 | 663 | 666 | 670 | 882 | 674 | 678 | 682 | 686 | 688 | 690 | 694 | 702 | 534 | 703 | 705 | 090 | 706 | 710 | 239 | 728 | 724 | 144 | 729 | 740 | 744 | 752 | 756 | 760 | 158 | 762 | 834 | 764 | 626 | 768 | 772 | 776 | 780 | 788 | 795 | 796 | 798 | 792 | 800 | 804 | 784 | 826 | 581 | 840 | 858 | 860 | 548 | 862 | 704 | 092 | 850 | 876 | 732 | 887 | 894 | 716 | 248 | 0;
   --  ****

   --  ****t* Countries/ISO.Countries.Country_Key
   --  DESCRIPTION
   --    An enumeration of every iso 3166 alpha2 country code.
   --    Each code starts with "C_", to prevent any enumeration conlficting
   --    with a reserved word.  So you can cast any alpha2 to Country_Key by
   --    doing "Country_Key'Value ("C_" & Alpha2)"
   --  EXAMPLE
   --    My_Key : Country_Key := C_US;
   --  SOURCE
   type Country_Key is (
      C_AF, --  Afghanistan
      C_AL, --  Albania
      C_DZ, --  Algeria
      C_AS, --  American Samoa
      C_AD, --  Andorra
      C_AO, --  Angola
      C_AI, --  Anguilla
      C_AQ, --  Antarctica
      C_AG, --  Antigua and Barbuda
      C_AR, --  Argentina
      C_AM, --  Armenia
      C_AW, --  Aruba
      C_AU, --  Australia
      C_AT, --  Austria
      C_AZ, --  Azerbaijan
      C_BS, --  Bahamas (the)
      C_BH, --  Bahrain
      C_BD, --  Bangladesh
      C_BB, --  Barbados
      C_BY, --  Belarus
      C_BE, --  Belgium
      C_BZ, --  Belize
      C_BJ, --  Benin
      C_BM, --  Bermuda
      C_BT, --  Bhutan
      C_BO, --  Bolivia (Plurinational State of)
      C_BQ, --  Bonaire, Sint Eustatius and Saba
      C_BA, --  Bosnia and Herzegovina
      C_BW, --  Botswana
      C_BV, --  Bouvet Island
      C_BR, --  Brazil
      C_IO, --  British Indian Ocean Territory (the)
      C_BN, --  Brunei Darussalam
      C_BG, --  Bulgaria
      C_BF, --  Burkina Faso
      C_BI, --  Burundi
      C_CV, --  Cabo Verde
      C_KH, --  Cambodia
      C_CM, --  Cameroon
      C_CA, --  Canada
      C_KY, --  Cayman Islands (the)
      C_CF, --  Central African Republic (the)
      C_TD, --  Chad
      C_CL, --  Chile
      C_CN, --  China
      C_CX, --  Christmas Island
      C_CC, --  Cocos (Keeling) Islands (the)
      C_CO, --  Colombia
      C_KM, --  Comoros (the)
      C_CD, --  Congo (the Democratic Republic of the)
      C_CG, --  Congo (the)
      C_CK, --  Cook Islands (the)
      C_CR, --  Costa Rica
      C_HR, --  Croatia
      C_CU, --  Cuba
      C_CW, --  Curaçao
      C_CY, --  Cyprus
      C_CZ, --  Czechia
      C_CI, --  Côte d'Ivoire
      C_DK, --  Denmark
      C_DJ, --  Djibouti
      C_DM, --  Dominica
      C_DO, --  Dominican Republic (the)
      C_EC, --  Ecuador
      C_EG, --  Egypt
      C_SV, --  El Salvador
      C_GQ, --  Equatorial Guinea
      C_ER, --  Eritrea
      C_EE, --  Estonia
      C_SZ, --  Eswatini
      C_ET, --  Ethiopia
      C_FK, --  Falkland Islands (the) [Malvinas]
      C_FO, --  Faroe Islands (the)
      C_FJ, --  Fiji
      C_FI, --  Finland
      C_FR, --  France
      C_GF, --  French Guiana
      C_PF, --  French Polynesia
      C_TF, --  French Southern Territories (the)
      C_GA, --  Gabon
      C_GM, --  Gambia (the)
      C_GE, --  Georgia
      C_DE, --  Germany
      C_GH, --  Ghana
      C_GI, --  Gibraltar
      C_GR, --  Greece
      C_GL, --  Greenland
      C_GD, --  Grenada
      C_GP, --  Guadeloupe
      C_GU, --  Guam
      C_GT, --  Guatemala
      C_GG, --  Guernsey
      C_GN, --  Guinea
      C_GW, --  Guinea-Bissau
      C_GY, --  Guyana
      C_HT, --  Haiti
      C_HM, --  Heard Island and McDonald Islands
      C_VA, --  Holy See (the)
      C_HN, --  Honduras
      C_HK, --  Hong Kong
      C_HU, --  Hungary
      C_IS, --  Iceland
      C_IN, --  India
      C_ID, --  Indonesia
      C_IR, --  Iran (Islamic Republic of)
      C_IQ, --  Iraq
      C_IE, --  Ireland
      C_IM, --  Isle of Man
      C_IL, --  Israel
      C_IT, --  Italy
      C_JM, --  Jamaica
      C_JP, --  Japan
      C_JE, --  Jersey
      C_JO, --  Jordan
      C_KZ, --  Kazakhstan
      C_KE, --  Kenya
      C_KI, --  Kiribati
      C_KP, --  Korea (the Democratic People's Republic of)
      C_KR, --  Korea (the Republic of)
      C_KW, --  Kuwait
      C_KG, --  Kyrgyzstan
      C_LA, --  Lao People's Democratic Republic (the)
      C_LV, --  Latvia
      C_LB, --  Lebanon
      C_LS, --  Lesotho
      C_LR, --  Liberia
      C_LY, --  Libya
      C_LI, --  Liechtenstein
      C_LT, --  Lithuania
      C_LU, --  Luxembourg
      C_MO, --  Macao
      C_MG, --  Madagascar
      C_MW, --  Malawi
      C_MY, --  Malaysia
      C_MV, --  Maldives
      C_ML, --  Mali
      C_MT, --  Malta
      C_MH, --  Marshall Islands (the)
      C_MQ, --  Martinique
      C_MR, --  Mauritania
      C_MU, --  Mauritius
      C_YT, --  Mayotte
      C_MX, --  Mexico
      C_FM, --  Micronesia (Federated States of)
      C_MD, --  Moldova (the Republic of)
      C_MC, --  Monaco
      C_MN, --  Mongolia
      C_ME, --  Montenegro
      C_MS, --  Montserrat
      C_MA, --  Morocco
      C_MZ, --  Mozambique
      C_MM, --  Myanmar
      C_NA, --  Namibia
      C_NR, --  Nauru
      C_NP, --  Nepal
      C_NL, --  Netherlands (Kingdom of the)
      C_NC, --  New Caledonia
      C_NZ, --  New Zealand
      C_NI, --  Nicaragua
      C_NE, --  Niger (the)
      C_NG, --  Nigeria
      C_NU, --  Niue
      C_NF, --  Norfolk Island
      C_MK, --  North Macedonia
      C_MP, --  Northern Mariana Islands (the)
      C_NO, --  Norway
      C_OM, --  Oman
      C_PK, --  Pakistan
      C_PW, --  Palau
      C_PS, --  Palestine, State of
      C_PA, --  Panama
      C_PG, --  Papua New Guinea
      C_PY, --  Paraguay
      C_PE, --  Peru
      C_PH, --  Philippines (the)
      C_PN, --  Pitcairn
      C_PL, --  Poland
      C_PT, --  Portugal
      C_PR, --  Puerto Rico
      C_QA, --  Qatar
      C_RO, --  Romania
      C_RU, --  Russian Federation (the)
      C_RW, --  Rwanda
      C_RE, --  Réunion
      C_BL, --  Saint Barthélemy
      C_SH, --  Saint Helena, Ascension and Tristan da Cunha
      C_KN, --  Saint Kitts and Nevis
      C_LC, --  Saint Lucia
      C_MF, --  Saint Martin (French part)
      C_PM, --  Saint Pierre and Miquelon
      C_VC, --  Saint Vincent and the Grenadines
      C_WS, --  Samoa
      C_SM, --  San Marino
      C_ST, --  Sao Tome and Principe
      C_SA, --  Saudi Arabia
      C_SN, --  Senegal
      C_RS, --  Serbia
      C_SC, --  Seychelles
      C_SL, --  Sierra Leone
      C_SG, --  Singapore
      C_SX, --  Sint Maarten (Dutch part)
      C_SK, --  Slovakia
      C_SI, --  Slovenia
      C_SB, --  Solomon Islands
      C_SO, --  Somalia
      C_ZA, --  South Africa
      C_GS, --  South Georgia and the South Sandwich Islands
      C_SS, --  South Sudan
      C_ES, --  Spain
      C_LK, --  Sri Lanka
      C_SD, --  Sudan (the)
      C_SR, --  Suriname
      C_SJ, --  Svalbard and Jan Mayen
      C_SE, --  Sweden
      C_CH, --  Switzerland
      C_SY, --  Syrian Arab Republic (the)
      C_TW, --  Taiwan (Province of China)
      C_TJ, --  Tajikistan
      C_TZ, --  Tanzania, the United Republic of
      C_TH, --  Thailand
      C_TL, --  Timor-Leste
      C_TG, --  Togo
      C_TK, --  Tokelau
      C_TO, --  Tonga
      C_TT, --  Trinidad and Tobago
      C_TN, --  Tunisia
      C_TM, --  Turkmenistan
      C_TC, --  Turks and Caicos Islands (the)
      C_TV, --  Tuvalu
      C_TR, --  Türkiye
      C_UG, --  Uganda
      C_UA, --  Ukraine
      C_AE, --  United Arab Emirates (the)
      C_GB, --  United Kingdom of Great Britain and Northern Ireland (the)
      C_UM, --  United States Minor Outlying Islands (the)
      C_US, --  United States of America (the)
      C_UY, --  Uruguay
      C_UZ, --  Uzbekistan
      C_VU, --  Vanuatu
      C_VE, --  Venezuela (Bolivarian Republic of)
      C_VN, --  Viet Nam
      C_VG, --  Virgin Islands (British)
      C_VI, --  Virgin Islands (U.S.)
      C_WF, --  Wallis and Futuna
      C_EH, --  Western Sahara*
      C_YE, --  Yemen
      C_ZM, --  Zambia
      C_ZW, --  Zimbabwe
      C_AX, --  Åland Islands
      C_ZZ  --  Undefined Country
   );
   --  ****

   --  ****t* Countries/ISO.Countries.Country
   --  DESCRIPTION
   --    The ISO_3166-1 country to be referenced.  When initializing, you can
   --    set the key to Country_Key that will be utilizeed one of the functions
   --    to access the Country.
   --  USAGE
   --    My_Country : Country := (C_US); --  This is the USA.
   --  METHODS
   --  * ISO.Countries.Country/Name
   --  * ISO.Countries.Country/Alpha2
   --  * ISO.Countries.Country/Alpha3
   --  * ISO.Countries.Country/Numeric
   --  * ISO.Countries.Country/Country_Code
   --  EXAMPLE
   --    --  To create a country and initalize it to the USA,
   --    --  then reference it like so:
   --    My_Country : Country := (C_US); --  This is the USA.
   --    To access the country's name, do so like so:
   --    My_Country.Name --  "United States of America (The)"
   --  SOURCE
   type Country is tagged record
      Key : Country_Key := C_ZZ;
   end record;
   --  ****

   --  ****m* ISO.Countries.Country/Name
   --  FUNCTION
   --    Retrieve the name of the provided country.
   --  RETURN VALUE
   --    String: The ISO 3166 name of the current country.
   --  EXAMPLE
   --    My_Country : Country := (C_AU);
   --    The_Name : String := My_Country.Name; --  Will return "Australia"
   --  SOURCE
   function Name (This : Country) return String;
   --  ****

   --  ****m* ISO.Countries.Country/Alpha2
   --  FUNCTION
   --    Retrieve the two-letter country code of the provided country.
   --  RETURN VALUE
   --    ISO.Countries.Alpha2_Code: ISO 3166 Alpha2 of the current country.
   --  EXAMPLE
   --    My_Country : Country := (C_AU);
   --    Code : Alhpa2_Code := My_Country.Alpha2; --  Will return "AU"
   --  SOURCE
   function Alpha2 (This : Country) return Alpha2_Code;
   --  ****

   --  ****m* ISO.Countries.Country/Alpha3
   --  FUNCTION
   --    Retrieve the three-letter country code of the provided country.
   --  RETURN VALUE
   --    ISO.Countries.Alpha3_Code: ISO 3166 Alpha3 of the current country.
   --  EXAMPLE
   --    My_Country : Country := (C_AU);
   --    Code : Alpha3_Code := My_Country.Alpha3; --  Will return "AUS"
   --  SOURCE
   function Alpha3 (This : Country) return Alpha3_Code;
   --  ****

   --  ****m* ISO.Countries.Country/Numeric
   --  FUNCTION
   --    Retrieve the numerical country code of the provided country.
   --  RETURN VALUE
   --    ISO.Countries.Numeric_Code: ISO 3166 number of the current country.
   --  EXAMPLE
   --    My_Country : Country := (C_AU);
   --    Number : Numeric_Code := My_Country.Numeric; --  Will return 40
   --  SOURCE
   function Numeric (This : Country) return Numeric_Code;
   --  ****

   --  ****m* ISO.Countries.Country/Country_Code
   --  FUNCTION
   --    Retrieve the Ada.Locale.Country_Code of the provided country.
   --  RETURN VALUE
   --    Ada.Locale.Country_Code: Ada's Country_Code of the current country.
   --  EXAMPLE
   --    My_Country : Country := (C_AU);
   --    Number : Ada.Locale.Country_Code := My_Country.Country_Code; -- "AU"
   --  SOURCE
   function Country_Code
      (This : Country) return Ada.Locales.Country_Code;
   --  ****

   --  Return all countries
   --  function All_Countries return All_Countries;

   --  ****f* Countries/ISO.Countries.From_Alpha2
   --  FUNCTION
   --    Create a country from a provided Alpha2 string.
   --  PARAMETERS
   --    Code - An alpha2 code, such as "AU" or "US".
   --  RETURN VALUE
   --    ISO.Countries.Country: Country corresponding to that alpha2 code.
   --  EXAMPLE
   --    My_Country : Country := From_Alpha2("AU");
   --  SOURCE
   function From_Alpha2 (Code : Alpha2_Code) return Country;
   --  ****

   --  ****f* Countries/ISO.Countries.From_Alpha3
   --  FUNCTION
   --    Create a country from a provided Alpha3 string.
   --  PARAMETERS
   --    Code - An alpha3 code, such as "AUS" or "USA".
   --  RETURN VALUE
   --    ISO.Countries.Country: Country corresponding to that alpha3 code.
   --  EXAMPLE
   --    My_Country : Country := From_Alpha3("AUS");
   --  SOURCE
   function From_Alpha3 (Code : Alpha3_Code) return Country;
   --  ****

   --  ****f* Countries/ISO.Countries.From_Numeric
   --  FUNCTION
   --    Create a country from a provided numerical code string.
   --  EXAMPLES
   --    My_Country_1 : Country := From_Numeric(40);
   --    My_Country_2 : Country := From_Numeric(040);
   --    My_Country_3 : Country := From_Numeric("040");
   --  PARAMETERS
   --    Number - A Numeric Code, either as a string or integer.
   --  RETURN VALUE
   --    ISO.Countries.Country: Country corresponding to that numerical code.
   --  SOURCE
   function From_Numeric (Number : Numeric_Code) return Country;
   function From_Numeric (Number : String) return Country;
   --  ****

   --  ****f* Countries/ISO.Countries.From_Country_Code
   --  FUNCTION
   --    Create a country from a provided numerical code string.
   --  PARAMETERS
   --    Code - A country code as defined in Ada.Locales.
   --  RETURN VALUE
   --    ISO.Countries.Country: Country corresponding to that numerical code.
   --  USAGE
   --    My_Country : Country := From_Country_Code(Ada.Locales.Country);
   --  SOURCE
   function From_Country_Code (Code : Ada.Locales.Country_Code) return Country;
   --  ****

   --  ****t* Countries/ISO.Countries.Country_List
   --  DESCRIPTION
   --    An arbitrary-sized array of countries.
   --  USAGE
   --    declare
   --       My_Countries : Country_List (1 .. 2);
   --    begin
   --       My_Countries (1) := (Key => C_AU);
   --       My_Countries (2) := (Key => C_US);
   --    end;
   --  SOURCE
   type Country_List is array (Positive range <>) of Country;
   --  ****

   --  ****t* Countries/ISO.Countries.All_Countries
   --  DESCRIPTION
   --    All of the countries, utilizing the Country_Key as an index.
   --    To conserve the stack
   --  USAGE
   --    My_Countries : constant All_Countries := Init_Countries;
   --    US_Name : String := My_Countries (C_US).Name;
   --  SOURCE
   type All_Countries is array (Country_Key'Range) of Country;
   --  ****

   --  ****f* Countries/ISO.Countries.Init_Countries
   --  FUNCTION
   --    Initialize all of the countries in an array.
   --  RETURN VALUE
   --    ISO.Countries.All_Countries: Array containing all countries.
   --  USAGE
   --    My_Countries : constant All_Countries := Init_Countries;
   --  SOURCE
   function Init_Countries return All_Countries;
   --  ****
private
   function Alpha3_To_Key (Alpha_3 : Alpha3_Code) return Country_Key;
   function Numeric_To_Key (Numeric : Numeric_Code) return Country_Key;
end ISO.Countries;

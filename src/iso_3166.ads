with Ada.Locales;
with Ada.Containers.Indefinite_Vectors;
package ISO_3166 is

   --  Raised if a country is not found.
   Country_Not_Found : exception;
   Invalid_Number    : exception;

   --  Alpha-2 Code.
   type Alpha_2_Code is new String (1 .. 2)
      with Dynamic_Predicate =>
         (for all E of Alpha_2_Code => E in 'A' .. 'Z');

   --  Alpha-3 Code.
   type Alpha_3_Code is new String (1 .. 3)
      with Dynamic_Predicate =>
         (for all E of Alpha_3_Code => E in 'A' .. 'Z');

   --  The ISO_3166_1 country to be referenced.
   type ISO_3166_1 (Name_Length : Natural) is record
      Name    : String (1 .. Name_Length);
      Alpha_2 : Alpha_2_Code;
      Alpha_3 : Alpha_3_code;
      Numeric : Natural;
   end record;

   --  Iteratable vector of all countries.
   package Countries_Vector is new
      Ada.Containers.Indefinite_Vectors
         (Index_Type => Natural, Element_Type => ISO_3166_1);

   --  Vector containing all countries.
   subtype Countries is Countries_Vector.Vector;

   --  Return a vector of all countries
   function All_Countries return Countries;

   --  Retrive a country with a provided Alpha_2 code.
   function From_Alpha2 (Alpha_2 : Alpha_2_Code) return ISO_3166_1;

   --  Retrive a country with a provided Alpha_3 code.
   function From_Alpha3 (Alpha_3 : Alpha_3_Code) return ISO_3166_1;

   --  Retrive a country with a provided Numerical code.
   function From_Numeric (Numeric : Natural) return ISO_3166_1;

   --  Retrive a country with a provided Numerical string.
   function From_Numeric (Numeric : String) return ISO_3166_1;

   --  From a Ada.Locales country
   function From_Country_Code (Item : Ada.Locales.Country_Code) return ISO_3166_1;

   --  Country to Ada.Locales Country_Code
   function To_Country_Code (Item : ISO_3166_1) return Ada.Locales.Country_Code;

private

   --  Country keys as alpha2 and alpha3
   type Alpha_2_Key is (
    Key_AF, 
    Key_AL, 
    Key_DZ, 
    Key_AS, 
    Key_AD, 
    Key_AO, 
    Key_AI, 
    Key_AQ, 
    Key_AG, 
    Key_AR, 
    Key_AM, 
    Key_AW, 
    Key_AU, 
    Key_AT, 
    Key_AZ, 
    Key_BS, 
    Key_BH, 
    Key_BD, 
    Key_BB, 
    Key_BY, 
    Key_BE, 
    Key_BZ, 
    Key_BJ, 
    Key_BM, 
    Key_BT, 
    Key_BO, 
    Key_BQ, 
    Key_BA, 
    Key_BW, 
    Key_BV, 
    Key_BR, 
    Key_IO, 
    Key_BN, 
    Key_BG, 
    Key_BF, 
    Key_BI, 
    Key_CV, 
    Key_KH, 
    Key_CM, 
    Key_CA, 
    Key_KY, 
    Key_CF, 
    Key_TD, 
    Key_CL, 
    Key_CN, 
    Key_CX, 
    Key_CC, 
    Key_CO, 
    Key_KM, 
    Key_CD, 
    Key_CG, 
    Key_CK, 
    Key_CR, 
    Key_HR, 
    Key_CU, 
    Key_CW, 
    Key_CY, 
    Key_CZ, 
    Key_CI, 
    Key_DK, 
    Key_DJ, 
    Key_DM, 
    Key_DO, 
    Key_EC, 
    Key_EG, 
    Key_SV, 
    Key_GQ, 
    Key_ER, 
    Key_EE, 
    Key_SZ, 
    Key_ET, 
    Key_FK, 
    Key_FO, 
    Key_FJ, 
    Key_FI, 
    Key_FR, 
    Key_GF, 
    Key_PF, 
    Key_TF, 
    Key_GA, 
    Key_GM, 
    Key_GE, 
    Key_DE, 
    Key_GH, 
    Key_GI, 
    Key_GR, 
    Key_GL, 
    Key_GD, 
    Key_GP, 
    Key_GU, 
    Key_GT, 
    Key_GG, 
    Key_GN, 
    Key_GW, 
    Key_GY, 
    Key_HT, 
    Key_HM, 
    Key_VA, 
    Key_HN, 
    Key_HK, 
    Key_HU, 
    Key_IS, 
    Key_IN, 
    Key_ID, 
    Key_IR, 
    Key_IQ, 
    Key_IE, 
    Key_IM, 
    Key_IL, 
    Key_IT, 
    Key_JM, 
    Key_JP, 
    Key_JE, 
    Key_JO, 
    Key_KZ, 
    Key_KE, 
    Key_KI, 
    Key_KP, 
    Key_KR, 
    Key_KW, 
    Key_KG, 
    Key_LA, 
    Key_LV, 
    Key_LB, 
    Key_LS, 
    Key_LR, 
    Key_LY, 
    Key_LI, 
    Key_LT, 
    Key_LU, 
    Key_MO, 
    Key_MG, 
    Key_MW, 
    Key_MY, 
    Key_MV, 
    Key_ML, 
    Key_MT, 
    Key_MH, 
    Key_MQ, 
    Key_MR, 
    Key_MU, 
    Key_YT, 
    Key_MX, 
    Key_FM, 
    Key_MD, 
    Key_MC, 
    Key_MN, 
    Key_ME, 
    Key_MS, 
    Key_MA, 
    Key_MZ, 
    Key_MM, 
    Key_NA, 
    Key_NR, 
    Key_NP, 
    Key_NL, 
    Key_NC, 
    Key_NZ, 
    Key_NI, 
    Key_NE, 
    Key_NG, 
    Key_NU, 
    Key_NF, 
    Key_MK, 
    Key_MP, 
    Key_NO, 
    Key_OM, 
    Key_PK, 
    Key_PW, 
    Key_PS, 
    Key_PA, 
    Key_PG, 
    Key_PY, 
    Key_PE, 
    Key_PH, 
    Key_PN, 
    Key_PL, 
    Key_PT, 
    Key_PR, 
    Key_QA, 
    Key_RO, 
    Key_RU, 
    Key_RW, 
    Key_RE, 
    Key_BL, 
    Key_SH, 
    Key_KN, 
    Key_LC, 
    Key_MF, 
    Key_PM, 
    Key_VC, 
    Key_WS, 
    Key_SM, 
    Key_ST, 
    Key_SA, 
    Key_SN, 
    Key_RS, 
    Key_SC, 
    Key_SL, 
    Key_SG, 
    Key_SX, 
    Key_SK, 
    Key_SI, 
    Key_SB, 
    Key_SO, 
    Key_ZA, 
    Key_GS, 
    Key_SS, 
    Key_ES, 
    Key_LK, 
    Key_SD, 
    Key_SR, 
    Key_SJ, 
    Key_SE, 
    Key_CH, 
    Key_SY, 
    Key_TW, 
    Key_TJ, 
    Key_TZ, 
    Key_TH, 
    Key_TL, 
    Key_TG, 
    Key_TK, 
    Key_TO, 
    Key_TT, 
    Key_TN, 
    Key_TM, 
    Key_TC, 
    Key_TV, 
    Key_TR, 
    Key_UG, 
    Key_UA, 
    Key_AE, 
    Key_GB, 
    Key_UM, 
    Key_US, 
    Key_UY, 
    Key_UZ, 
    Key_VU, 
    Key_VE, 
    Key_VN, 
    Key_VG, 
    Key_VI, 
    Key_WF, 
    Key_EH, 
    Key_YE, 
    Key_ZM, 
    Key_ZW, 
    Key_AX, 
    NA);
   type Alpha_3_Key is (
    Key_AFG, 
    Key_ALB, 
    Key_DZA, 
    Key_ASM, 
    Key_AND, 
    Key_AGO, 
    Key_AIA, 
    Key_ATA, 
    Key_ATG, 
    Key_ARG, 
    Key_ARM, 
    Key_ABW, 
    Key_AUS, 
    Key_AUT, 
    Key_AZE, 
    Key_BHS, 
    Key_BHR, 
    Key_BGD, 
    Key_BRB, 
    Key_BLR, 
    Key_BEL, 
    Key_BLZ, 
    Key_BEN, 
    Key_BMU, 
    Key_BTN, 
    Key_BOL, 
    Key_BES, 
    Key_BIH, 
    Key_BWA, 
    Key_BVT, 
    Key_BRA, 
    Key_IOT, 
    Key_BRN, 
    Key_BGR, 
    Key_BFA, 
    Key_BDI, 
    Key_CPV, 
    Key_KHM, 
    Key_CMR, 
    Key_CAN, 
    Key_CYM, 
    Key_CAF, 
    Key_TCD, 
    Key_CHL, 
    Key_CHN, 
    Key_CXR, 
    Key_CCK, 
    Key_COL, 
    Key_COM, 
    Key_COD, 
    Key_COG, 
    Key_COK, 
    Key_CRI, 
    Key_HRV, 
    Key_CUB, 
    Key_CUW, 
    Key_CYP, 
    Key_CZE, 
    Key_CIV, 
    Key_DNK, 
    Key_DJI, 
    Key_DMA, 
    Key_DOM, 
    Key_ECU, 
    Key_EGY, 
    Key_SLV, 
    Key_GNQ, 
    Key_ERI, 
    Key_EST, 
    Key_SWZ, 
    Key_ETH, 
    Key_FLK, 
    Key_FRO, 
    Key_FJI, 
    Key_FIN, 
    Key_FRA, 
    Key_GUF, 
    Key_PYF, 
    Key_ATF, 
    Key_GAB, 
    Key_GMB, 
    Key_GEO, 
    Key_DEU, 
    Key_GHA, 
    Key_GIB, 
    Key_GRC, 
    Key_GRL, 
    Key_GRD, 
    Key_GLP, 
    Key_GUM, 
    Key_GTM, 
    Key_GGY, 
    Key_GIN, 
    Key_GNB, 
    Key_GUY, 
    Key_HTI, 
    Key_HMD, 
    Key_VAT, 
    Key_HND, 
    Key_HKG, 
    Key_HUN, 
    Key_ISL, 
    Key_IND, 
    Key_IDN, 
    Key_IRN, 
    Key_IRQ, 
    Key_IRL, 
    Key_IMN, 
    Key_ISR, 
    Key_ITA, 
    Key_JAM, 
    Key_JPN, 
    Key_JEY, 
    Key_JOR, 
    Key_KAZ, 
    Key_KEN, 
    Key_KIR, 
    Key_PRK, 
    Key_KOR, 
    Key_KWT, 
    Key_KGZ, 
    Key_LAO, 
    Key_LVA, 
    Key_LBN, 
    Key_LSO, 
    Key_LBR, 
    Key_LBY, 
    Key_LIE, 
    Key_LTU, 
    Key_LUX, 
    Key_MAC, 
    Key_MDG, 
    Key_MWI, 
    Key_MYS, 
    Key_MDV, 
    Key_MLI, 
    Key_MLT, 
    Key_MHL, 
    Key_MTQ, 
    Key_MRT, 
    Key_MUS, 
    Key_MYT, 
    Key_MEX, 
    Key_FSM, 
    Key_MDA, 
    Key_MCO, 
    Key_MNG, 
    Key_MNE, 
    Key_MSR, 
    Key_MAR, 
    Key_MOZ, 
    Key_MMR, 
    Key_NAM, 
    Key_NRU, 
    Key_NPL, 
    Key_NLD, 
    Key_NCL, 
    Key_NZL, 
    Key_NIC, 
    Key_NER, 
    Key_NGA, 
    Key_NIU, 
    Key_NFK, 
    Key_MKD, 
    Key_MNP, 
    Key_NOR, 
    Key_OMN, 
    Key_PAK, 
    Key_PLW, 
    Key_PSE, 
    Key_PAN, 
    Key_PNG, 
    Key_PRY, 
    Key_PER, 
    Key_PHL, 
    Key_PCN, 
    Key_POL, 
    Key_PRT, 
    Key_PRI, 
    Key_QAT, 
    Key_ROU, 
    Key_RUS, 
    Key_RWA, 
    Key_REU, 
    Key_BLM, 
    Key_SHN, 
    Key_KNA, 
    Key_LCA, 
    Key_MAF, 
    Key_SPM, 
    Key_VCT, 
    Key_WSM, 
    Key_SMR, 
    Key_STP, 
    Key_SAU, 
    Key_SEN, 
    Key_SRB, 
    Key_SYC, 
    Key_SLE, 
    Key_SGP, 
    Key_SXM, 
    Key_SVK, 
    Key_SVN, 
    Key_SLB, 
    Key_SOM, 
    Key_ZAF, 
    Key_SGS, 
    Key_SSD, 
    Key_ESP, 
    Key_LKA, 
    Key_SDN, 
    Key_SUR, 
    Key_SJM, 
    Key_SWE, 
    Key_CHE, 
    Key_SYR, 
    Key_TWN, 
    Key_TJK, 
    Key_TZA, 
    Key_THA, 
    Key_TLS, 
    Key_TGO, 
    Key_TKL, 
    Key_TON, 
    Key_TTO, 
    Key_TUN, 
    Key_TKM, 
    Key_TCA, 
    Key_TUV, 
    Key_TUR, 
    Key_UGA, 
    Key_UKR, 
    Key_ARE, 
    Key_GBR, 
    Key_UMI, 
    Key_USA, 
    Key_URY, 
    Key_UZB, 
    Key_VUT, 
    Key_VEN, 
    Key_VNM, 
    Key_VGB, 
    Key_VIR, 
    Key_WLF, 
    Key_ESH, 
    Key_YEM, 
    Key_ZMB, 
    Key_ZWE, 
    Key_ALA, 
    NA);

   --  Lookup table to convert alpha_3 string to alpha_2 key.
   function Alpha_3_to_Alpha_2 (Alpha_3 : Alpha_3_Code) return Alpha_2_Key;

   --  Lookup table to convert numeric to alpha_2 key.
   function Numeric_To_Alpha_2 (Numeric : Natural) return Alpha_2_Key;

   --  Lookup table to match alpha_2 key to country.
   function Alpha_2_Key_To_Country (Alpha_2 : Alpha_2_Key) return ISO_3166_1;

   --  Country lookup table
   Country_AF : constant ISO_3166_1 := (
      Name_Length => String'("Afghanistan")'Length,
      Name        => "Afghanistan",
      Alpha_2     => "AF",
      Alpha_3     => "AFG",
      Numeric     => 004);
   Country_AL : constant ISO_3166_1 := (
      Name_Length => String'("Albania")'Length,
      Name        => "Albania",
      Alpha_2     => "AL",
      Alpha_3     => "ALB",
      Numeric     => 008);
   Country_DZ : constant ISO_3166_1 := (
      Name_Length => String'("Algeria")'Length,
      Name        => "Algeria",
      Alpha_2     => "DZ",
      Alpha_3     => "DZA",
      Numeric     => 012);
   Country_AS : constant ISO_3166_1 := (
      Name_Length => String'("American Samoa")'Length,
      Name        => "American Samoa",
      Alpha_2     => "AS",
      Alpha_3     => "ASM",
      Numeric     => 016);
   Country_AD : constant ISO_3166_1 := (
      Name_Length => String'("Andorra")'Length,
      Name        => "Andorra",
      Alpha_2     => "AD",
      Alpha_3     => "AND",
      Numeric     => 020);
   Country_AO : constant ISO_3166_1 := (
      Name_Length => String'("Angola")'Length,
      Name        => "Angola",
      Alpha_2     => "AO",
      Alpha_3     => "AGO",
      Numeric     => 024);
   Country_AI : constant ISO_3166_1 := (
      Name_Length => String'("Anguilla")'Length,
      Name        => "Anguilla",
      Alpha_2     => "AI",
      Alpha_3     => "AIA",
      Numeric     => 660);
   Country_AQ : constant ISO_3166_1 := (
      Name_Length => String'("Antarctica")'Length,
      Name        => "Antarctica",
      Alpha_2     => "AQ",
      Alpha_3     => "ATA",
      Numeric     => 010);
   Country_AG : constant ISO_3166_1 := (
      Name_Length => String'("Antigua and Barbuda")'Length,
      Name        => "Antigua and Barbuda",
      Alpha_2     => "AG",
      Alpha_3     => "ATG",
      Numeric     => 028);
   Country_AR : constant ISO_3166_1 := (
      Name_Length => String'("Argentina")'Length,
      Name        => "Argentina",
      Alpha_2     => "AR",
      Alpha_3     => "ARG",
      Numeric     => 032);
   Country_AM : constant ISO_3166_1 := (
      Name_Length => String'("Armenia")'Length,
      Name        => "Armenia",
      Alpha_2     => "AM",
      Alpha_3     => "ARM",
      Numeric     => 051);
   Country_AW : constant ISO_3166_1 := (
      Name_Length => String'("Aruba")'Length,
      Name        => "Aruba",
      Alpha_2     => "AW",
      Alpha_3     => "ABW",
      Numeric     => 533);
   Country_AU : constant ISO_3166_1 := (
      Name_Length => String'("Australia")'Length,
      Name        => "Australia",
      Alpha_2     => "AU",
      Alpha_3     => "AUS",
      Numeric     => 036);
   Country_AT : constant ISO_3166_1 := (
      Name_Length => String'("Austria")'Length,
      Name        => "Austria",
      Alpha_2     => "AT",
      Alpha_3     => "AUT",
      Numeric     => 040);
   Country_AZ : constant ISO_3166_1 := (
      Name_Length => String'("Azerbaijan")'Length,
      Name        => "Azerbaijan",
      Alpha_2     => "AZ",
      Alpha_3     => "AZE",
      Numeric     => 031);
   Country_BS : constant ISO_3166_1 := (
      Name_Length => String'("Bahamas (the)")'Length,
      Name        => "Bahamas (the)",
      Alpha_2     => "BS",
      Alpha_3     => "BHS",
      Numeric     => 044);
   Country_BH : constant ISO_3166_1 := (
      Name_Length => String'("Bahrain")'Length,
      Name        => "Bahrain",
      Alpha_2     => "BH",
      Alpha_3     => "BHR",
      Numeric     => 048);
   Country_BD : constant ISO_3166_1 := (
      Name_Length => String'("Bangladesh")'Length,
      Name        => "Bangladesh",
      Alpha_2     => "BD",
      Alpha_3     => "BGD",
      Numeric     => 050);
   Country_BB : constant ISO_3166_1 := (
      Name_Length => String'("Barbados")'Length,
      Name        => "Barbados",
      Alpha_2     => "BB",
      Alpha_3     => "BRB",
      Numeric     => 052);
   Country_BY : constant ISO_3166_1 := (
      Name_Length => String'("Belarus")'Length,
      Name        => "Belarus",
      Alpha_2     => "BY",
      Alpha_3     => "BLR",
      Numeric     => 112);
   Country_BE : constant ISO_3166_1 := (
      Name_Length => String'("Belgium")'Length,
      Name        => "Belgium",
      Alpha_2     => "BE",
      Alpha_3     => "BEL",
      Numeric     => 056);
   Country_BZ : constant ISO_3166_1 := (
      Name_Length => String'("Belize")'Length,
      Name        => "Belize",
      Alpha_2     => "BZ",
      Alpha_3     => "BLZ",
      Numeric     => 084);
   Country_BJ : constant ISO_3166_1 := (
      Name_Length => String'("Benin")'Length,
      Name        => "Benin",
      Alpha_2     => "BJ",
      Alpha_3     => "BEN",
      Numeric     => 204);
   Country_BM : constant ISO_3166_1 := (
      Name_Length => String'("Bermuda")'Length,
      Name        => "Bermuda",
      Alpha_2     => "BM",
      Alpha_3     => "BMU",
      Numeric     => 060);
   Country_BT : constant ISO_3166_1 := (
      Name_Length => String'("Bhutan")'Length,
      Name        => "Bhutan",
      Alpha_2     => "BT",
      Alpha_3     => "BTN",
      Numeric     => 064);
   Country_BO : constant ISO_3166_1 := (
      Name_Length => String'("Bolivia (Plurinational State of)")'Length,
      Name        => "Bolivia (Plurinational State of)",
      Alpha_2     => "BO",
      Alpha_3     => "BOL",
      Numeric     => 068);
   Country_BQ : constant ISO_3166_1 := (
      Name_Length => String'("Bonaire, Sint Eustatius and Saba")'Length,
      Name        => "Bonaire, Sint Eustatius and Saba",
      Alpha_2     => "BQ",
      Alpha_3     => "BES",
      Numeric     => 535);
   Country_BA : constant ISO_3166_1 := (
      Name_Length => String'("Bosnia and Herzegovina")'Length,
      Name        => "Bosnia and Herzegovina",
      Alpha_2     => "BA",
      Alpha_3     => "BIH",
      Numeric     => 070);
   Country_BW : constant ISO_3166_1 := (
      Name_Length => String'("Botswana")'Length,
      Name        => "Botswana",
      Alpha_2     => "BW",
      Alpha_3     => "BWA",
      Numeric     => 072);
   Country_BV : constant ISO_3166_1 := (
      Name_Length => String'("Bouvet Island")'Length,
      Name        => "Bouvet Island",
      Alpha_2     => "BV",
      Alpha_3     => "BVT",
      Numeric     => 074);
   Country_BR : constant ISO_3166_1 := (
      Name_Length => String'("Brazil")'Length,
      Name        => "Brazil",
      Alpha_2     => "BR",
      Alpha_3     => "BRA",
      Numeric     => 076);
   Country_IO : constant ISO_3166_1 := (
      Name_Length => String'("British Indian Ocean Territory (the)")'Length,
      Name        => "British Indian Ocean Territory (the)",
      Alpha_2     => "IO",
      Alpha_3     => "IOT",
      Numeric     => 086);
   Country_BN : constant ISO_3166_1 := (
      Name_Length => String'("Brunei Darussalam")'Length,
      Name        => "Brunei Darussalam",
      Alpha_2     => "BN",
      Alpha_3     => "BRN",
      Numeric     => 096);
   Country_BG : constant ISO_3166_1 := (
      Name_Length => String'("Bulgaria")'Length,
      Name        => "Bulgaria",
      Alpha_2     => "BG",
      Alpha_3     => "BGR",
      Numeric     => 100);
   Country_BF : constant ISO_3166_1 := (
      Name_Length => String'("Burkina Faso")'Length,
      Name        => "Burkina Faso",
      Alpha_2     => "BF",
      Alpha_3     => "BFA",
      Numeric     => 854);
   Country_BI : constant ISO_3166_1 := (
      Name_Length => String'("Burundi")'Length,
      Name        => "Burundi",
      Alpha_2     => "BI",
      Alpha_3     => "BDI",
      Numeric     => 108);
   Country_CV : constant ISO_3166_1 := (
      Name_Length => String'("Cabo Verde")'Length,
      Name        => "Cabo Verde",
      Alpha_2     => "CV",
      Alpha_3     => "CPV",
      Numeric     => 132);
   Country_KH : constant ISO_3166_1 := (
      Name_Length => String'("Cambodia")'Length,
      Name        => "Cambodia",
      Alpha_2     => "KH",
      Alpha_3     => "KHM",
      Numeric     => 116);
   Country_CM : constant ISO_3166_1 := (
      Name_Length => String'("Cameroon")'Length,
      Name        => "Cameroon",
      Alpha_2     => "CM",
      Alpha_3     => "CMR",
      Numeric     => 120);
   Country_CA : constant ISO_3166_1 := (
      Name_Length => String'("Canada")'Length,
      Name        => "Canada",
      Alpha_2     => "CA",
      Alpha_3     => "CAN",
      Numeric     => 124);
   Country_KY : constant ISO_3166_1 := (
      Name_Length => String'("Cayman Islands (the)")'Length,
      Name        => "Cayman Islands (the)",
      Alpha_2     => "KY",
      Alpha_3     => "CYM",
      Numeric     => 136);
   Country_CF : constant ISO_3166_1 := (
      Name_Length => String'("Central African Republic (the)")'Length,
      Name        => "Central African Republic (the)",
      Alpha_2     => "CF",
      Alpha_3     => "CAF",
      Numeric     => 140);
   Country_TD : constant ISO_3166_1 := (
      Name_Length => String'("Chad")'Length,
      Name        => "Chad",
      Alpha_2     => "TD",
      Alpha_3     => "TCD",
      Numeric     => 148);
   Country_CL : constant ISO_3166_1 := (
      Name_Length => String'("Chile")'Length,
      Name        => "Chile",
      Alpha_2     => "CL",
      Alpha_3     => "CHL",
      Numeric     => 152);
   Country_CN : constant ISO_3166_1 := (
      Name_Length => String'("China")'Length,
      Name        => "China",
      Alpha_2     => "CN",
      Alpha_3     => "CHN",
      Numeric     => 156);
   Country_CX : constant ISO_3166_1 := (
      Name_Length => String'("Christmas Island")'Length,
      Name        => "Christmas Island",
      Alpha_2     => "CX",
      Alpha_3     => "CXR",
      Numeric     => 162);
   Country_CC : constant ISO_3166_1 := (
      Name_Length => String'("Cocos (Keeling) Islands (the)")'Length,
      Name        => "Cocos (Keeling) Islands (the)",
      Alpha_2     => "CC",
      Alpha_3     => "CCK",
      Numeric     => 166);
   Country_CO : constant ISO_3166_1 := (
      Name_Length => String'("Colombia")'Length,
      Name        => "Colombia",
      Alpha_2     => "CO",
      Alpha_3     => "COL",
      Numeric     => 170);
   Country_KM : constant ISO_3166_1 := (
      Name_Length => String'("Comoros (the)")'Length,
      Name        => "Comoros (the)",
      Alpha_2     => "KM",
      Alpha_3     => "COM",
      Numeric     => 174);
   Country_CD : constant ISO_3166_1 := (
      Name_Length => String'("Congo (the Democratic Republic of the)")'Length,
      Name        => "Congo (the Democratic Republic of the)",
      Alpha_2     => "CD",
      Alpha_3     => "COD",
      Numeric     => 180);
   Country_CG : constant ISO_3166_1 := (
      Name_Length => String'("Congo (the)")'Length,
      Name        => "Congo (the)",
      Alpha_2     => "CG",
      Alpha_3     => "COG",
      Numeric     => 178);
   Country_CK : constant ISO_3166_1 := (
      Name_Length => String'("Cook Islands (the)")'Length,
      Name        => "Cook Islands (the)",
      Alpha_2     => "CK",
      Alpha_3     => "COK",
      Numeric     => 184);
   Country_CR : constant ISO_3166_1 := (
      Name_Length => String'("Costa Rica")'Length,
      Name        => "Costa Rica",
      Alpha_2     => "CR",
      Alpha_3     => "CRI",
      Numeric     => 188);
   Country_HR : constant ISO_3166_1 := (
      Name_Length => String'("Croatia")'Length,
      Name        => "Croatia",
      Alpha_2     => "HR",
      Alpha_3     => "HRV",
      Numeric     => 191);
   Country_CU : constant ISO_3166_1 := (
      Name_Length => String'("Cuba")'Length,
      Name        => "Cuba",
      Alpha_2     => "CU",
      Alpha_3     => "CUB",
      Numeric     => 192);
   Country_CW : constant ISO_3166_1 := (
      Name_Length => String'("Curaçao")'Length,
      Name        => "Curaçao",
      Alpha_2     => "CW",
      Alpha_3     => "CUW",
      Numeric     => 531);
   Country_CY : constant ISO_3166_1 := (
      Name_Length => String'("Cyprus")'Length,
      Name        => "Cyprus",
      Alpha_2     => "CY",
      Alpha_3     => "CYP",
      Numeric     => 196);
   Country_CZ : constant ISO_3166_1 := (
      Name_Length => String'("Czechia")'Length,
      Name        => "Czechia",
      Alpha_2     => "CZ",
      Alpha_3     => "CZE",
      Numeric     => 203);
   Country_CI : constant ISO_3166_1 := (
      Name_Length => String'("Côte d'Ivoire")'Length,
      Name        => "Côte d'Ivoire",
      Alpha_2     => "CI",
      Alpha_3     => "CIV",
      Numeric     => 384);
   Country_DK : constant ISO_3166_1 := (
      Name_Length => String'("Denmark")'Length,
      Name        => "Denmark",
      Alpha_2     => "DK",
      Alpha_3     => "DNK",
      Numeric     => 208);
   Country_DJ : constant ISO_3166_1 := (
      Name_Length => String'("Djibouti")'Length,
      Name        => "Djibouti",
      Alpha_2     => "DJ",
      Alpha_3     => "DJI",
      Numeric     => 262);
   Country_DM : constant ISO_3166_1 := (
      Name_Length => String'("Dominica")'Length,
      Name        => "Dominica",
      Alpha_2     => "DM",
      Alpha_3     => "DMA",
      Numeric     => 212);
   Country_DO : constant ISO_3166_1 := (
      Name_Length => String'("Dominican Republic (the)")'Length,
      Name        => "Dominican Republic (the)",
      Alpha_2     => "DO",
      Alpha_3     => "DOM",
      Numeric     => 214);
   Country_EC : constant ISO_3166_1 := (
      Name_Length => String'("Ecuador")'Length,
      Name        => "Ecuador",
      Alpha_2     => "EC",
      Alpha_3     => "ECU",
      Numeric     => 218);
   Country_EG : constant ISO_3166_1 := (
      Name_Length => String'("Egypt")'Length,
      Name        => "Egypt",
      Alpha_2     => "EG",
      Alpha_3     => "EGY",
      Numeric     => 818);
   Country_SV : constant ISO_3166_1 := (
      Name_Length => String'("El Salvador")'Length,
      Name        => "El Salvador",
      Alpha_2     => "SV",
      Alpha_3     => "SLV",
      Numeric     => 222);
   Country_GQ : constant ISO_3166_1 := (
      Name_Length => String'("Equatorial Guinea")'Length,
      Name        => "Equatorial Guinea",
      Alpha_2     => "GQ",
      Alpha_3     => "GNQ",
      Numeric     => 226);
   Country_ER : constant ISO_3166_1 := (
      Name_Length => String'("Eritrea")'Length,
      Name        => "Eritrea",
      Alpha_2     => "ER",
      Alpha_3     => "ERI",
      Numeric     => 232);
   Country_EE : constant ISO_3166_1 := (
      Name_Length => String'("Estonia")'Length,
      Name        => "Estonia",
      Alpha_2     => "EE",
      Alpha_3     => "EST",
      Numeric     => 233);
   Country_SZ : constant ISO_3166_1 := (
      Name_Length => String'("Eswatini")'Length,
      Name        => "Eswatini",
      Alpha_2     => "SZ",
      Alpha_3     => "SWZ",
      Numeric     => 748);
   Country_ET : constant ISO_3166_1 := (
      Name_Length => String'("Ethiopia")'Length,
      Name        => "Ethiopia",
      Alpha_2     => "ET",
      Alpha_3     => "ETH",
      Numeric     => 231);
   Country_FK : constant ISO_3166_1 := (
      Name_Length => String'("Falkland Islands (the) [Malvinas]")'Length,
      Name        => "Falkland Islands (the) [Malvinas]",
      Alpha_2     => "FK",
      Alpha_3     => "FLK",
      Numeric     => 238);
   Country_FO : constant ISO_3166_1 := (
      Name_Length => String'("Faroe Islands (the)")'Length,
      Name        => "Faroe Islands (the)",
      Alpha_2     => "FO",
      Alpha_3     => "FRO",
      Numeric     => 234);
   Country_FJ : constant ISO_3166_1 := (
      Name_Length => String'("Fiji")'Length,
      Name        => "Fiji",
      Alpha_2     => "FJ",
      Alpha_3     => "FJI",
      Numeric     => 242);
   Country_FI : constant ISO_3166_1 := (
      Name_Length => String'("Finland")'Length,
      Name        => "Finland",
      Alpha_2     => "FI",
      Alpha_3     => "FIN",
      Numeric     => 246);
   Country_FR : constant ISO_3166_1 := (
      Name_Length => String'("France")'Length,
      Name        => "France",
      Alpha_2     => "FR",
      Alpha_3     => "FRA",
      Numeric     => 250);
   Country_GF : constant ISO_3166_1 := (
      Name_Length => String'("French Guiana")'Length,
      Name        => "French Guiana",
      Alpha_2     => "GF",
      Alpha_3     => "GUF",
      Numeric     => 254);
   Country_PF : constant ISO_3166_1 := (
      Name_Length => String'("French Polynesia")'Length,
      Name        => "French Polynesia",
      Alpha_2     => "PF",
      Alpha_3     => "PYF",
      Numeric     => 258);
   Country_TF : constant ISO_3166_1 := (
      Name_Length => String'("French Southern Territories (the)")'Length,
      Name        => "French Southern Territories (the)",
      Alpha_2     => "TF",
      Alpha_3     => "ATF",
      Numeric     => 260);
   Country_GA : constant ISO_3166_1 := (
      Name_Length => String'("Gabon")'Length,
      Name        => "Gabon",
      Alpha_2     => "GA",
      Alpha_3     => "GAB",
      Numeric     => 266);
   Country_GM : constant ISO_3166_1 := (
      Name_Length => String'("Gambia (the)")'Length,
      Name        => "Gambia (the)",
      Alpha_2     => "GM",
      Alpha_3     => "GMB",
      Numeric     => 270);
   Country_GE : constant ISO_3166_1 := (
      Name_Length => String'("Georgia")'Length,
      Name        => "Georgia",
      Alpha_2     => "GE",
      Alpha_3     => "GEO",
      Numeric     => 268);
   Country_DE : constant ISO_3166_1 := (
      Name_Length => String'("Germany")'Length,
      Name        => "Germany",
      Alpha_2     => "DE",
      Alpha_3     => "DEU",
      Numeric     => 276);
   Country_GH : constant ISO_3166_1 := (
      Name_Length => String'("Ghana")'Length,
      Name        => "Ghana",
      Alpha_2     => "GH",
      Alpha_3     => "GHA",
      Numeric     => 288);
   Country_GI : constant ISO_3166_1 := (
      Name_Length => String'("Gibraltar")'Length,
      Name        => "Gibraltar",
      Alpha_2     => "GI",
      Alpha_3     => "GIB",
      Numeric     => 292);
   Country_GR : constant ISO_3166_1 := (
      Name_Length => String'("Greece")'Length,
      Name        => "Greece",
      Alpha_2     => "GR",
      Alpha_3     => "GRC",
      Numeric     => 300);
   Country_GL : constant ISO_3166_1 := (
      Name_Length => String'("Greenland")'Length,
      Name        => "Greenland",
      Alpha_2     => "GL",
      Alpha_3     => "GRL",
      Numeric     => 304);
   Country_GD : constant ISO_3166_1 := (
      Name_Length => String'("Grenada")'Length,
      Name        => "Grenada",
      Alpha_2     => "GD",
      Alpha_3     => "GRD",
      Numeric     => 308);
   Country_GP : constant ISO_3166_1 := (
      Name_Length => String'("Guadeloupe")'Length,
      Name        => "Guadeloupe",
      Alpha_2     => "GP",
      Alpha_3     => "GLP",
      Numeric     => 312);
   Country_GU : constant ISO_3166_1 := (
      Name_Length => String'("Guam")'Length,
      Name        => "Guam",
      Alpha_2     => "GU",
      Alpha_3     => "GUM",
      Numeric     => 316);
   Country_GT : constant ISO_3166_1 := (
      Name_Length => String'("Guatemala")'Length,
      Name        => "Guatemala",
      Alpha_2     => "GT",
      Alpha_3     => "GTM",
      Numeric     => 320);
   Country_GG : constant ISO_3166_1 := (
      Name_Length => String'("Guernsey")'Length,
      Name        => "Guernsey",
      Alpha_2     => "GG",
      Alpha_3     => "GGY",
      Numeric     => 831);
   Country_GN : constant ISO_3166_1 := (
      Name_Length => String'("Guinea")'Length,
      Name        => "Guinea",
      Alpha_2     => "GN",
      Alpha_3     => "GIN",
      Numeric     => 324);
   Country_GW : constant ISO_3166_1 := (
      Name_Length => String'("Guinea-Bissau")'Length,
      Name        => "Guinea-Bissau",
      Alpha_2     => "GW",
      Alpha_3     => "GNB",
      Numeric     => 624);
   Country_GY : constant ISO_3166_1 := (
      Name_Length => String'("Guyana")'Length,
      Name        => "Guyana",
      Alpha_2     => "GY",
      Alpha_3     => "GUY",
      Numeric     => 328);
   Country_HT : constant ISO_3166_1 := (
      Name_Length => String'("Haiti")'Length,
      Name        => "Haiti",
      Alpha_2     => "HT",
      Alpha_3     => "HTI",
      Numeric     => 332);
   Country_HM : constant ISO_3166_1 := (
      Name_Length => String'("Heard Island and McDonald Islands")'Length,
      Name        => "Heard Island and McDonald Islands",
      Alpha_2     => "HM",
      Alpha_3     => "HMD",
      Numeric     => 334);
   Country_VA : constant ISO_3166_1 := (
      Name_Length => String'("Holy See (the)")'Length,
      Name        => "Holy See (the)",
      Alpha_2     => "VA",
      Alpha_3     => "VAT",
      Numeric     => 336);
   Country_HN : constant ISO_3166_1 := (
      Name_Length => String'("Honduras")'Length,
      Name        => "Honduras",
      Alpha_2     => "HN",
      Alpha_3     => "HND",
      Numeric     => 340);
   Country_HK : constant ISO_3166_1 := (
      Name_Length => String'("Hong Kong")'Length,
      Name        => "Hong Kong",
      Alpha_2     => "HK",
      Alpha_3     => "HKG",
      Numeric     => 344);
   Country_HU : constant ISO_3166_1 := (
      Name_Length => String'("Hungary")'Length,
      Name        => "Hungary",
      Alpha_2     => "HU",
      Alpha_3     => "HUN",
      Numeric     => 348);
   Country_IS : constant ISO_3166_1 := (
      Name_Length => String'("Iceland")'Length,
      Name        => "Iceland",
      Alpha_2     => "IS",
      Alpha_3     => "ISL",
      Numeric     => 352);
   Country_IN : constant ISO_3166_1 := (
      Name_Length => String'("India")'Length,
      Name        => "India",
      Alpha_2     => "IN",
      Alpha_3     => "IND",
      Numeric     => 356);
   Country_ID : constant ISO_3166_1 := (
      Name_Length => String'("Indonesia")'Length,
      Name        => "Indonesia",
      Alpha_2     => "ID",
      Alpha_3     => "IDN",
      Numeric     => 360);
   Country_IR : constant ISO_3166_1 := (
      Name_Length => String'("Iran (Islamic Republic of)")'Length,
      Name        => "Iran (Islamic Republic of)",
      Alpha_2     => "IR",
      Alpha_3     => "IRN",
      Numeric     => 364);
   Country_IQ : constant ISO_3166_1 := (
      Name_Length => String'("Iraq")'Length,
      Name        => "Iraq",
      Alpha_2     => "IQ",
      Alpha_3     => "IRQ",
      Numeric     => 368);
   Country_IE : constant ISO_3166_1 := (
      Name_Length => String'("Ireland")'Length,
      Name        => "Ireland",
      Alpha_2     => "IE",
      Alpha_3     => "IRL",
      Numeric     => 372);
   Country_IM : constant ISO_3166_1 := (
      Name_Length => String'("Isle of Man")'Length,
      Name        => "Isle of Man",
      Alpha_2     => "IM",
      Alpha_3     => "IMN",
      Numeric     => 833);
   Country_IL : constant ISO_3166_1 := (
      Name_Length => String'("Israel")'Length,
      Name        => "Israel",
      Alpha_2     => "IL",
      Alpha_3     => "ISR",
      Numeric     => 376);
   Country_IT : constant ISO_3166_1 := (
      Name_Length => String'("Italy")'Length,
      Name        => "Italy",
      Alpha_2     => "IT",
      Alpha_3     => "ITA",
      Numeric     => 380);
   Country_JM : constant ISO_3166_1 := (
      Name_Length => String'("Jamaica")'Length,
      Name        => "Jamaica",
      Alpha_2     => "JM",
      Alpha_3     => "JAM",
      Numeric     => 388);
   Country_JP : constant ISO_3166_1 := (
      Name_Length => String'("Japan")'Length,
      Name        => "Japan",
      Alpha_2     => "JP",
      Alpha_3     => "JPN",
      Numeric     => 392);
   Country_JE : constant ISO_3166_1 := (
      Name_Length => String'("Jersey")'Length,
      Name        => "Jersey",
      Alpha_2     => "JE",
      Alpha_3     => "JEY",
      Numeric     => 832);
   Country_JO : constant ISO_3166_1 := (
      Name_Length => String'("Jordan")'Length,
      Name        => "Jordan",
      Alpha_2     => "JO",
      Alpha_3     => "JOR",
      Numeric     => 400);
   Country_KZ : constant ISO_3166_1 := (
      Name_Length => String'("Kazakhstan")'Length,
      Name        => "Kazakhstan",
      Alpha_2     => "KZ",
      Alpha_3     => "KAZ",
      Numeric     => 398);
   Country_KE : constant ISO_3166_1 := (
      Name_Length => String'("Kenya")'Length,
      Name        => "Kenya",
      Alpha_2     => "KE",
      Alpha_3     => "KEN",
      Numeric     => 404);
   Country_KI : constant ISO_3166_1 := (
      Name_Length => String'("Kiribati")'Length,
      Name        => "Kiribati",
      Alpha_2     => "KI",
      Alpha_3     => "KIR",
      Numeric     => 296);
   Country_KP : constant ISO_3166_1 := (
      Name_Length => String'("Korea (the Democratic People's Republic of)")'Length,
      Name        => "Korea (the Democratic People's Republic of)",
      Alpha_2     => "KP",
      Alpha_3     => "PRK",
      Numeric     => 408);
   Country_KR : constant ISO_3166_1 := (
      Name_Length => String'("Korea (the Republic of)")'Length,
      Name        => "Korea (the Republic of)",
      Alpha_2     => "KR",
      Alpha_3     => "KOR",
      Numeric     => 410);
   Country_KW : constant ISO_3166_1 := (
      Name_Length => String'("Kuwait")'Length,
      Name        => "Kuwait",
      Alpha_2     => "KW",
      Alpha_3     => "KWT",
      Numeric     => 414);
   Country_KG : constant ISO_3166_1 := (
      Name_Length => String'("Kyrgyzstan")'Length,
      Name        => "Kyrgyzstan",
      Alpha_2     => "KG",
      Alpha_3     => "KGZ",
      Numeric     => 417);
   Country_LA : constant ISO_3166_1 := (
      Name_Length => String'("Lao People's Democratic Republic (the)")'Length,
      Name        => "Lao People's Democratic Republic (the)",
      Alpha_2     => "LA",
      Alpha_3     => "LAO",
      Numeric     => 418);
   Country_LV : constant ISO_3166_1 := (
      Name_Length => String'("Latvia")'Length,
      Name        => "Latvia",
      Alpha_2     => "LV",
      Alpha_3     => "LVA",
      Numeric     => 428);
   Country_LB : constant ISO_3166_1 := (
      Name_Length => String'("Lebanon")'Length,
      Name        => "Lebanon",
      Alpha_2     => "LB",
      Alpha_3     => "LBN",
      Numeric     => 422);
   Country_LS : constant ISO_3166_1 := (
      Name_Length => String'("Lesotho")'Length,
      Name        => "Lesotho",
      Alpha_2     => "LS",
      Alpha_3     => "LSO",
      Numeric     => 426);
   Country_LR : constant ISO_3166_1 := (
      Name_Length => String'("Liberia")'Length,
      Name        => "Liberia",
      Alpha_2     => "LR",
      Alpha_3     => "LBR",
      Numeric     => 430);
   Country_LY : constant ISO_3166_1 := (
      Name_Length => String'("Libya")'Length,
      Name        => "Libya",
      Alpha_2     => "LY",
      Alpha_3     => "LBY",
      Numeric     => 434);
   Country_LI : constant ISO_3166_1 := (
      Name_Length => String'("Liechtenstein")'Length,
      Name        => "Liechtenstein",
      Alpha_2     => "LI",
      Alpha_3     => "LIE",
      Numeric     => 438);
   Country_LT : constant ISO_3166_1 := (
      Name_Length => String'("Lithuania")'Length,
      Name        => "Lithuania",
      Alpha_2     => "LT",
      Alpha_3     => "LTU",
      Numeric     => 440);
   Country_LU : constant ISO_3166_1 := (
      Name_Length => String'("Luxembourg")'Length,
      Name        => "Luxembourg",
      Alpha_2     => "LU",
      Alpha_3     => "LUX",
      Numeric     => 442);
   Country_MO : constant ISO_3166_1 := (
      Name_Length => String'("Macao")'Length,
      Name        => "Macao",
      Alpha_2     => "MO",
      Alpha_3     => "MAC",
      Numeric     => 446);
   Country_MG : constant ISO_3166_1 := (
      Name_Length => String'("Madagascar")'Length,
      Name        => "Madagascar",
      Alpha_2     => "MG",
      Alpha_3     => "MDG",
      Numeric     => 450);
   Country_MW : constant ISO_3166_1 := (
      Name_Length => String'("Malawi")'Length,
      Name        => "Malawi",
      Alpha_2     => "MW",
      Alpha_3     => "MWI",
      Numeric     => 454);
   Country_MY : constant ISO_3166_1 := (
      Name_Length => String'("Malaysia")'Length,
      Name        => "Malaysia",
      Alpha_2     => "MY",
      Alpha_3     => "MYS",
      Numeric     => 458);
   Country_MV : constant ISO_3166_1 := (
      Name_Length => String'("Maldives")'Length,
      Name        => "Maldives",
      Alpha_2     => "MV",
      Alpha_3     => "MDV",
      Numeric     => 462);
   Country_ML : constant ISO_3166_1 := (
      Name_Length => String'("Mali")'Length,
      Name        => "Mali",
      Alpha_2     => "ML",
      Alpha_3     => "MLI",
      Numeric     => 466);
   Country_MT : constant ISO_3166_1 := (
      Name_Length => String'("Malta")'Length,
      Name        => "Malta",
      Alpha_2     => "MT",
      Alpha_3     => "MLT",
      Numeric     => 470);
   Country_MH : constant ISO_3166_1 := (
      Name_Length => String'("Marshall Islands (the)")'Length,
      Name        => "Marshall Islands (the)",
      Alpha_2     => "MH",
      Alpha_3     => "MHL",
      Numeric     => 584);
   Country_MQ : constant ISO_3166_1 := (
      Name_Length => String'("Martinique")'Length,
      Name        => "Martinique",
      Alpha_2     => "MQ",
      Alpha_3     => "MTQ",
      Numeric     => 474);
   Country_MR : constant ISO_3166_1 := (
      Name_Length => String'("Mauritania")'Length,
      Name        => "Mauritania",
      Alpha_2     => "MR",
      Alpha_3     => "MRT",
      Numeric     => 478);
   Country_MU : constant ISO_3166_1 := (
      Name_Length => String'("Mauritius")'Length,
      Name        => "Mauritius",
      Alpha_2     => "MU",
      Alpha_3     => "MUS",
      Numeric     => 480);
   Country_YT : constant ISO_3166_1 := (
      Name_Length => String'("Mayotte")'Length,
      Name        => "Mayotte",
      Alpha_2     => "YT",
      Alpha_3     => "MYT",
      Numeric     => 175);
   Country_MX : constant ISO_3166_1 := (
      Name_Length => String'("Mexico")'Length,
      Name        => "Mexico",
      Alpha_2     => "MX",
      Alpha_3     => "MEX",
      Numeric     => 484);
   Country_FM : constant ISO_3166_1 := (
      Name_Length => String'("Micronesia (Federated States of)")'Length,
      Name        => "Micronesia (Federated States of)",
      Alpha_2     => "FM",
      Alpha_3     => "FSM",
      Numeric     => 583);
   Country_MD : constant ISO_3166_1 := (
      Name_Length => String'("Moldova (the Republic of)")'Length,
      Name        => "Moldova (the Republic of)",
      Alpha_2     => "MD",
      Alpha_3     => "MDA",
      Numeric     => 498);
   Country_MC : constant ISO_3166_1 := (
      Name_Length => String'("Monaco")'Length,
      Name        => "Monaco",
      Alpha_2     => "MC",
      Alpha_3     => "MCO",
      Numeric     => 492);
   Country_MN : constant ISO_3166_1 := (
      Name_Length => String'("Mongolia")'Length,
      Name        => "Mongolia",
      Alpha_2     => "MN",
      Alpha_3     => "MNG",
      Numeric     => 496);
   Country_ME : constant ISO_3166_1 := (
      Name_Length => String'("Montenegro")'Length,
      Name        => "Montenegro",
      Alpha_2     => "ME",
      Alpha_3     => "MNE",
      Numeric     => 499);
   Country_MS : constant ISO_3166_1 := (
      Name_Length => String'("Montserrat")'Length,
      Name        => "Montserrat",
      Alpha_2     => "MS",
      Alpha_3     => "MSR",
      Numeric     => 500);
   Country_MA : constant ISO_3166_1 := (
      Name_Length => String'("Morocco")'Length,
      Name        => "Morocco",
      Alpha_2     => "MA",
      Alpha_3     => "MAR",
      Numeric     => 504);
   Country_MZ : constant ISO_3166_1 := (
      Name_Length => String'("Mozambique")'Length,
      Name        => "Mozambique",
      Alpha_2     => "MZ",
      Alpha_3     => "MOZ",
      Numeric     => 508);
   Country_MM : constant ISO_3166_1 := (
      Name_Length => String'("Myanmar")'Length,
      Name        => "Myanmar",
      Alpha_2     => "MM",
      Alpha_3     => "MMR",
      Numeric     => 104);
   Country_NA : constant ISO_3166_1 := (
      Name_Length => String'("Namibia")'Length,
      Name        => "Namibia",
      Alpha_2     => "NA",
      Alpha_3     => "NAM",
      Numeric     => 516);
   Country_NR : constant ISO_3166_1 := (
      Name_Length => String'("Nauru")'Length,
      Name        => "Nauru",
      Alpha_2     => "NR",
      Alpha_3     => "NRU",
      Numeric     => 520);
   Country_NP : constant ISO_3166_1 := (
      Name_Length => String'("Nepal")'Length,
      Name        => "Nepal",
      Alpha_2     => "NP",
      Alpha_3     => "NPL",
      Numeric     => 524);
   Country_NL : constant ISO_3166_1 := (
      Name_Length => String'("Netherlands (Kingdom of the)")'Length,
      Name        => "Netherlands (Kingdom of the)",
      Alpha_2     => "NL",
      Alpha_3     => "NLD",
      Numeric     => 528);
   Country_NC : constant ISO_3166_1 := (
      Name_Length => String'("New Caledonia")'Length,
      Name        => "New Caledonia",
      Alpha_2     => "NC",
      Alpha_3     => "NCL",
      Numeric     => 540);
   Country_NZ : constant ISO_3166_1 := (
      Name_Length => String'("New Zealand")'Length,
      Name        => "New Zealand",
      Alpha_2     => "NZ",
      Alpha_3     => "NZL",
      Numeric     => 554);
   Country_NI : constant ISO_3166_1 := (
      Name_Length => String'("Nicaragua")'Length,
      Name        => "Nicaragua",
      Alpha_2     => "NI",
      Alpha_3     => "NIC",
      Numeric     => 558);
   Country_NE : constant ISO_3166_1 := (
      Name_Length => String'("Niger (the)")'Length,
      Name        => "Niger (the)",
      Alpha_2     => "NE",
      Alpha_3     => "NER",
      Numeric     => 562);
   Country_NG : constant ISO_3166_1 := (
      Name_Length => String'("Nigeria")'Length,
      Name        => "Nigeria",
      Alpha_2     => "NG",
      Alpha_3     => "NGA",
      Numeric     => 566);
   Country_NU : constant ISO_3166_1 := (
      Name_Length => String'("Niue")'Length,
      Name        => "Niue",
      Alpha_2     => "NU",
      Alpha_3     => "NIU",
      Numeric     => 570);
   Country_NF : constant ISO_3166_1 := (
      Name_Length => String'("Norfolk Island")'Length,
      Name        => "Norfolk Island",
      Alpha_2     => "NF",
      Alpha_3     => "NFK",
      Numeric     => 574);
   Country_MK : constant ISO_3166_1 := (
      Name_Length => String'("North Macedonia")'Length,
      Name        => "North Macedonia",
      Alpha_2     => "MK",
      Alpha_3     => "MKD",
      Numeric     => 807);
   Country_MP : constant ISO_3166_1 := (
      Name_Length => String'("Northern Mariana Islands (the)")'Length,
      Name        => "Northern Mariana Islands (the)",
      Alpha_2     => "MP",
      Alpha_3     => "MNP",
      Numeric     => 580);
   Country_NO : constant ISO_3166_1 := (
      Name_Length => String'("Norway")'Length,
      Name        => "Norway",
      Alpha_2     => "NO",
      Alpha_3     => "NOR",
      Numeric     => 578);
   Country_OM : constant ISO_3166_1 := (
      Name_Length => String'("Oman")'Length,
      Name        => "Oman",
      Alpha_2     => "OM",
      Alpha_3     => "OMN",
      Numeric     => 512);
   Country_PK : constant ISO_3166_1 := (
      Name_Length => String'("Pakistan")'Length,
      Name        => "Pakistan",
      Alpha_2     => "PK",
      Alpha_3     => "PAK",
      Numeric     => 586);
   Country_PW : constant ISO_3166_1 := (
      Name_Length => String'("Palau")'Length,
      Name        => "Palau",
      Alpha_2     => "PW",
      Alpha_3     => "PLW",
      Numeric     => 585);
   Country_PS : constant ISO_3166_1 := (
      Name_Length => String'("Palestine, State of")'Length,
      Name        => "Palestine, State of",
      Alpha_2     => "PS",
      Alpha_3     => "PSE",
      Numeric     => 275);
   Country_PA : constant ISO_3166_1 := (
      Name_Length => String'("Panama")'Length,
      Name        => "Panama",
      Alpha_2     => "PA",
      Alpha_3     => "PAN",
      Numeric     => 591);
   Country_PG : constant ISO_3166_1 := (
      Name_Length => String'("Papua New Guinea")'Length,
      Name        => "Papua New Guinea",
      Alpha_2     => "PG",
      Alpha_3     => "PNG",
      Numeric     => 598);
   Country_PY : constant ISO_3166_1 := (
      Name_Length => String'("Paraguay")'Length,
      Name        => "Paraguay",
      Alpha_2     => "PY",
      Alpha_3     => "PRY",
      Numeric     => 600);
   Country_PE : constant ISO_3166_1 := (
      Name_Length => String'("Peru")'Length,
      Name        => "Peru",
      Alpha_2     => "PE",
      Alpha_3     => "PER",
      Numeric     => 604);
   Country_PH : constant ISO_3166_1 := (
      Name_Length => String'("Philippines (the)")'Length,
      Name        => "Philippines (the)",
      Alpha_2     => "PH",
      Alpha_3     => "PHL",
      Numeric     => 608);
   Country_PN : constant ISO_3166_1 := (
      Name_Length => String'("Pitcairn")'Length,
      Name        => "Pitcairn",
      Alpha_2     => "PN",
      Alpha_3     => "PCN",
      Numeric     => 612);
   Country_PL : constant ISO_3166_1 := (
      Name_Length => String'("Poland")'Length,
      Name        => "Poland",
      Alpha_2     => "PL",
      Alpha_3     => "POL",
      Numeric     => 616);
   Country_PT : constant ISO_3166_1 := (
      Name_Length => String'("Portugal")'Length,
      Name        => "Portugal",
      Alpha_2     => "PT",
      Alpha_3     => "PRT",
      Numeric     => 620);
   Country_PR : constant ISO_3166_1 := (
      Name_Length => String'("Puerto Rico")'Length,
      Name        => "Puerto Rico",
      Alpha_2     => "PR",
      Alpha_3     => "PRI",
      Numeric     => 630);
   Country_QA : constant ISO_3166_1 := (
      Name_Length => String'("Qatar")'Length,
      Name        => "Qatar",
      Alpha_2     => "QA",
      Alpha_3     => "QAT",
      Numeric     => 634);
   Country_RO : constant ISO_3166_1 := (
      Name_Length => String'("Romania")'Length,
      Name        => "Romania",
      Alpha_2     => "RO",
      Alpha_3     => "ROU",
      Numeric     => 642);
   Country_RU : constant ISO_3166_1 := (
      Name_Length => String'("Russian Federation (the)")'Length,
      Name        => "Russian Federation (the)",
      Alpha_2     => "RU",
      Alpha_3     => "RUS",
      Numeric     => 643);
   Country_RW : constant ISO_3166_1 := (
      Name_Length => String'("Rwanda")'Length,
      Name        => "Rwanda",
      Alpha_2     => "RW",
      Alpha_3     => "RWA",
      Numeric     => 646);
   Country_RE : constant ISO_3166_1 := (
      Name_Length => String'("Réunion")'Length,
      Name        => "Réunion",
      Alpha_2     => "RE",
      Alpha_3     => "REU",
      Numeric     => 638);
   Country_BL : constant ISO_3166_1 := (
      Name_Length => String'("Saint Barthélemy")'Length,
      Name        => "Saint Barthélemy",
      Alpha_2     => "BL",
      Alpha_3     => "BLM",
      Numeric     => 652);
   Country_SH : constant ISO_3166_1 := (
      Name_Length => String'("Saint Helena, Ascension and Tristan da Cunha")'Length,
      Name        => "Saint Helena, Ascension and Tristan da Cunha",
      Alpha_2     => "SH",
      Alpha_3     => "SHN",
      Numeric     => 654);
   Country_KN : constant ISO_3166_1 := (
      Name_Length => String'("Saint Kitts and Nevis")'Length,
      Name        => "Saint Kitts and Nevis",
      Alpha_2     => "KN",
      Alpha_3     => "KNA",
      Numeric     => 659);
   Country_LC : constant ISO_3166_1 := (
      Name_Length => String'("Saint Lucia")'Length,
      Name        => "Saint Lucia",
      Alpha_2     => "LC",
      Alpha_3     => "LCA",
      Numeric     => 662);
   Country_MF : constant ISO_3166_1 := (
      Name_Length => String'("Saint Martin (French part)")'Length,
      Name        => "Saint Martin (French part)",
      Alpha_2     => "MF",
      Alpha_3     => "MAF",
      Numeric     => 663);
   Country_PM : constant ISO_3166_1 := (
      Name_Length => String'("Saint Pierre and Miquelon")'Length,
      Name        => "Saint Pierre and Miquelon",
      Alpha_2     => "PM",
      Alpha_3     => "SPM",
      Numeric     => 666);
   Country_VC : constant ISO_3166_1 := (
      Name_Length => String'("Saint Vincent and the Grenadines")'Length,
      Name        => "Saint Vincent and the Grenadines",
      Alpha_2     => "VC",
      Alpha_3     => "VCT",
      Numeric     => 670);
   Country_WS : constant ISO_3166_1 := (
      Name_Length => String'("Samoa")'Length,
      Name        => "Samoa",
      Alpha_2     => "WS",
      Alpha_3     => "WSM",
      Numeric     => 882);
   Country_SM : constant ISO_3166_1 := (
      Name_Length => String'("San Marino")'Length,
      Name        => "San Marino",
      Alpha_2     => "SM",
      Alpha_3     => "SMR",
      Numeric     => 674);
   Country_ST : constant ISO_3166_1 := (
      Name_Length => String'("Sao Tome and Principe")'Length,
      Name        => "Sao Tome and Principe",
      Alpha_2     => "ST",
      Alpha_3     => "STP",
      Numeric     => 678);
   Country_SA : constant ISO_3166_1 := (
      Name_Length => String'("Saudi Arabia")'Length,
      Name        => "Saudi Arabia",
      Alpha_2     => "SA",
      Alpha_3     => "SAU",
      Numeric     => 682);
   Country_SN : constant ISO_3166_1 := (
      Name_Length => String'("Senegal")'Length,
      Name        => "Senegal",
      Alpha_2     => "SN",
      Alpha_3     => "SEN",
      Numeric     => 686);
   Country_RS : constant ISO_3166_1 := (
      Name_Length => String'("Serbia")'Length,
      Name        => "Serbia",
      Alpha_2     => "RS",
      Alpha_3     => "SRB",
      Numeric     => 688);
   Country_SC : constant ISO_3166_1 := (
      Name_Length => String'("Seychelles")'Length,
      Name        => "Seychelles",
      Alpha_2     => "SC",
      Alpha_3     => "SYC",
      Numeric     => 690);
   Country_SL : constant ISO_3166_1 := (
      Name_Length => String'("Sierra Leone")'Length,
      Name        => "Sierra Leone",
      Alpha_2     => "SL",
      Alpha_3     => "SLE",
      Numeric     => 694);
   Country_SG : constant ISO_3166_1 := (
      Name_Length => String'("Singapore")'Length,
      Name        => "Singapore",
      Alpha_2     => "SG",
      Alpha_3     => "SGP",
      Numeric     => 702);
   Country_SX : constant ISO_3166_1 := (
      Name_Length => String'("Sint Maarten (Dutch part)")'Length,
      Name        => "Sint Maarten (Dutch part)",
      Alpha_2     => "SX",
      Alpha_3     => "SXM",
      Numeric     => 534);
   Country_SK : constant ISO_3166_1 := (
      Name_Length => String'("Slovakia")'Length,
      Name        => "Slovakia",
      Alpha_2     => "SK",
      Alpha_3     => "SVK",
      Numeric     => 703);
   Country_SI : constant ISO_3166_1 := (
      Name_Length => String'("Slovenia")'Length,
      Name        => "Slovenia",
      Alpha_2     => "SI",
      Alpha_3     => "SVN",
      Numeric     => 705);
   Country_SB : constant ISO_3166_1 := (
      Name_Length => String'("Solomon Islands")'Length,
      Name        => "Solomon Islands",
      Alpha_2     => "SB",
      Alpha_3     => "SLB",
      Numeric     => 090);
   Country_SO : constant ISO_3166_1 := (
      Name_Length => String'("Somalia")'Length,
      Name        => "Somalia",
      Alpha_2     => "SO",
      Alpha_3     => "SOM",
      Numeric     => 706);
   Country_ZA : constant ISO_3166_1 := (
      Name_Length => String'("South Africa")'Length,
      Name        => "South Africa",
      Alpha_2     => "ZA",
      Alpha_3     => "ZAF",
      Numeric     => 710);
   Country_GS : constant ISO_3166_1 := (
      Name_Length => String'("South Georgia and the South Sandwich Islands")'Length,
      Name        => "South Georgia and the South Sandwich Islands",
      Alpha_2     => "GS",
      Alpha_3     => "SGS",
      Numeric     => 239);
   Country_SS : constant ISO_3166_1 := (
      Name_Length => String'("South Sudan")'Length,
      Name        => "South Sudan",
      Alpha_2     => "SS",
      Alpha_3     => "SSD",
      Numeric     => 728);
   Country_ES : constant ISO_3166_1 := (
      Name_Length => String'("Spain")'Length,
      Name        => "Spain",
      Alpha_2     => "ES",
      Alpha_3     => "ESP",
      Numeric     => 724);
   Country_LK : constant ISO_3166_1 := (
      Name_Length => String'("Sri Lanka")'Length,
      Name        => "Sri Lanka",
      Alpha_2     => "LK",
      Alpha_3     => "LKA",
      Numeric     => 144);
   Country_SD : constant ISO_3166_1 := (
      Name_Length => String'("Sudan (the)")'Length,
      Name        => "Sudan (the)",
      Alpha_2     => "SD",
      Alpha_3     => "SDN",
      Numeric     => 729);
   Country_SR : constant ISO_3166_1 := (
      Name_Length => String'("Suriname")'Length,
      Name        => "Suriname",
      Alpha_2     => "SR",
      Alpha_3     => "SUR",
      Numeric     => 740);
   Country_SJ : constant ISO_3166_1 := (
      Name_Length => String'("Svalbard and Jan Mayen")'Length,
      Name        => "Svalbard and Jan Mayen",
      Alpha_2     => "SJ",
      Alpha_3     => "SJM",
      Numeric     => 744);
   Country_SE : constant ISO_3166_1 := (
      Name_Length => String'("Sweden")'Length,
      Name        => "Sweden",
      Alpha_2     => "SE",
      Alpha_3     => "SWE",
      Numeric     => 752);
   Country_CH : constant ISO_3166_1 := (
      Name_Length => String'("Switzerland")'Length,
      Name        => "Switzerland",
      Alpha_2     => "CH",
      Alpha_3     => "CHE",
      Numeric     => 756);
   Country_SY : constant ISO_3166_1 := (
      Name_Length => String'("Syrian Arab Republic (the)")'Length,
      Name        => "Syrian Arab Republic (the)",
      Alpha_2     => "SY",
      Alpha_3     => "SYR",
      Numeric     => 760);
   Country_TW : constant ISO_3166_1 := (
      Name_Length => String'("Taiwan (Province of China)")'Length,
      Name        => "Taiwan (Province of China)",
      Alpha_2     => "TW",
      Alpha_3     => "TWN",
      Numeric     => 158);
   Country_TJ : constant ISO_3166_1 := (
      Name_Length => String'("Tajikistan")'Length,
      Name        => "Tajikistan",
      Alpha_2     => "TJ",
      Alpha_3     => "TJK",
      Numeric     => 762);
   Country_TZ : constant ISO_3166_1 := (
      Name_Length => String'("Tanzania, the United Republic of")'Length,
      Name        => "Tanzania, the United Republic of",
      Alpha_2     => "TZ",
      Alpha_3     => "TZA",
      Numeric     => 834);
   Country_TH : constant ISO_3166_1 := (
      Name_Length => String'("Thailand")'Length,
      Name        => "Thailand",
      Alpha_2     => "TH",
      Alpha_3     => "THA",
      Numeric     => 764);
   Country_TL : constant ISO_3166_1 := (
      Name_Length => String'("Timor-Leste")'Length,
      Name        => "Timor-Leste",
      Alpha_2     => "TL",
      Alpha_3     => "TLS",
      Numeric     => 626);
   Country_TG : constant ISO_3166_1 := (
      Name_Length => String'("Togo")'Length,
      Name        => "Togo",
      Alpha_2     => "TG",
      Alpha_3     => "TGO",
      Numeric     => 768);
   Country_TK : constant ISO_3166_1 := (
      Name_Length => String'("Tokelau")'Length,
      Name        => "Tokelau",
      Alpha_2     => "TK",
      Alpha_3     => "TKL",
      Numeric     => 772);
   Country_TO : constant ISO_3166_1 := (
      Name_Length => String'("Tonga")'Length,
      Name        => "Tonga",
      Alpha_2     => "TO",
      Alpha_3     => "TON",
      Numeric     => 776);
   Country_TT : constant ISO_3166_1 := (
      Name_Length => String'("Trinidad and Tobago")'Length,
      Name        => "Trinidad and Tobago",
      Alpha_2     => "TT",
      Alpha_3     => "TTO",
      Numeric     => 780);
   Country_TN : constant ISO_3166_1 := (
      Name_Length => String'("Tunisia")'Length,
      Name        => "Tunisia",
      Alpha_2     => "TN",
      Alpha_3     => "TUN",
      Numeric     => 788);
   Country_TM : constant ISO_3166_1 := (
      Name_Length => String'("Turkmenistan")'Length,
      Name        => "Turkmenistan",
      Alpha_2     => "TM",
      Alpha_3     => "TKM",
      Numeric     => 795);
   Country_TC : constant ISO_3166_1 := (
      Name_Length => String'("Turks and Caicos Islands (the)")'Length,
      Name        => "Turks and Caicos Islands (the)",
      Alpha_2     => "TC",
      Alpha_3     => "TCA",
      Numeric     => 796);
   Country_TV : constant ISO_3166_1 := (
      Name_Length => String'("Tuvalu")'Length,
      Name        => "Tuvalu",
      Alpha_2     => "TV",
      Alpha_3     => "TUV",
      Numeric     => 798);
   Country_TR : constant ISO_3166_1 := (
      Name_Length => String'("Türkiye")'Length,
      Name        => "Türkiye",
      Alpha_2     => "TR",
      Alpha_3     => "TUR",
      Numeric     => 792);
   Country_UG : constant ISO_3166_1 := (
      Name_Length => String'("Uganda")'Length,
      Name        => "Uganda",
      Alpha_2     => "UG",
      Alpha_3     => "UGA",
      Numeric     => 800);
   Country_UA : constant ISO_3166_1 := (
      Name_Length => String'("Ukraine")'Length,
      Name        => "Ukraine",
      Alpha_2     => "UA",
      Alpha_3     => "UKR",
      Numeric     => 804);
   Country_AE : constant ISO_3166_1 := (
      Name_Length => String'("United Arab Emirates (the)")'Length,
      Name        => "United Arab Emirates (the)",
      Alpha_2     => "AE",
      Alpha_3     => "ARE",
      Numeric     => 784);
   Country_GB : constant ISO_3166_1 := (
      Name_Length => String'("United Kingdom of Great Britain and Northern Ireland (the)")'Length,
      Name        => "United Kingdom of Great Britain and Northern Ireland (the)",
      Alpha_2     => "GB",
      Alpha_3     => "GBR",
      Numeric     => 826);
   Country_UM : constant ISO_3166_1 := (
      Name_Length => String'("United States Minor Outlying Islands (the)")'Length,
      Name        => "United States Minor Outlying Islands (the)",
      Alpha_2     => "UM",
      Alpha_3     => "UMI",
      Numeric     => 581);
   Country_US : constant ISO_3166_1 := (
      Name_Length => String'("United States of America (the)")'Length,
      Name        => "United States of America (the)",
      Alpha_2     => "US",
      Alpha_3     => "USA",
      Numeric     => 840);
   Country_UY : constant ISO_3166_1 := (
      Name_Length => String'("Uruguay")'Length,
      Name        => "Uruguay",
      Alpha_2     => "UY",
      Alpha_3     => "URY",
      Numeric     => 858);
   Country_UZ : constant ISO_3166_1 := (
      Name_Length => String'("Uzbekistan")'Length,
      Name        => "Uzbekistan",
      Alpha_2     => "UZ",
      Alpha_3     => "UZB",
      Numeric     => 860);
   Country_VU : constant ISO_3166_1 := (
      Name_Length => String'("Vanuatu")'Length,
      Name        => "Vanuatu",
      Alpha_2     => "VU",
      Alpha_3     => "VUT",
      Numeric     => 548);
   Country_VE : constant ISO_3166_1 := (
      Name_Length => String'("Venezuela (Bolivarian Republic of)")'Length,
      Name        => "Venezuela (Bolivarian Republic of)",
      Alpha_2     => "VE",
      Alpha_3     => "VEN",
      Numeric     => 862);
   Country_VN : constant ISO_3166_1 := (
      Name_Length => String'("Viet Nam")'Length,
      Name        => "Viet Nam",
      Alpha_2     => "VN",
      Alpha_3     => "VNM",
      Numeric     => 704);
   Country_VG : constant ISO_3166_1 := (
      Name_Length => String'("Virgin Islands (British)")'Length,
      Name        => "Virgin Islands (British)",
      Alpha_2     => "VG",
      Alpha_3     => "VGB",
      Numeric     => 092);
   Country_VI : constant ISO_3166_1 := (
      Name_Length => String'("Virgin Islands (U.S.)")'Length,
      Name        => "Virgin Islands (U.S.)",
      Alpha_2     => "VI",
      Alpha_3     => "VIR",
      Numeric     => 850);
   Country_WF : constant ISO_3166_1 := (
      Name_Length => String'("Wallis and Futuna")'Length,
      Name        => "Wallis and Futuna",
      Alpha_2     => "WF",
      Alpha_3     => "WLF",
      Numeric     => 876);
   Country_EH : constant ISO_3166_1 := (
      Name_Length => String'("Western Sahara*")'Length,
      Name        => "Western Sahara*",
      Alpha_2     => "EH",
      Alpha_3     => "ESH",
      Numeric     => 732);
   Country_YE : constant ISO_3166_1 := (
      Name_Length => String'("Yemen")'Length,
      Name        => "Yemen",
      Alpha_2     => "YE",
      Alpha_3     => "YEM",
      Numeric     => 887);
   Country_ZM : constant ISO_3166_1 := (
      Name_Length => String'("Zambia")'Length,
      Name        => "Zambia",
      Alpha_2     => "ZM",
      Alpha_3     => "ZMB",
      Numeric     => 894);
   Country_ZW : constant ISO_3166_1 := (
      Name_Length => String'("Zimbabwe")'Length,
      Name        => "Zimbabwe",
      Alpha_2     => "ZW",
      Alpha_3     => "ZWE",
      Numeric     => 716);
   Country_AX : constant ISO_3166_1 := (
      Name_Length => String'("Åland Islands")'Length,
      Name        => "Åland Islands",
      Alpha_2     => "AX",
      Alpha_3     => "ALA",
      Numeric     => 248);
end ISO_3166;

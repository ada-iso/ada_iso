with ISO.Countries;
--  ****h* ISO/Currencies
--  DESCRIPTION
--    Implimentation of ISO 4217 (Currency Codes).
--  SOURCE
package ISO.Currencies is
--  ****

   --  ****t* Currencies/ISO.Currencies.Alphabetic_Code
   --  DESCRIPTION
   --    The three-letter currency code defined by ISO 4217.
   --  DERIVED FROM
   --    String
   --  SOURCE
   subtype Alphabetic_Code is String (1 .. 3)
      with Dynamic_Predicate => Alphabetic_Code in
         "NLG" | "BMD" | "VES" | "SLE" | "HRK" | "NAD" | "MGA" | "ILP" | "GMD" | "ILR" | "BBD" | "MGF" | "ILS" | "SLL" | "MRO" | "XAF" | "XAG" | "MRU" | "USD" | "SAR" | "GBP" | "LBP" | "XAU" | "TND" | "CZK" | "USN" | "STD" | "MZE" | "ITL" | "KHR" | "NIC" | "PHP" | "USS" | "CDF" | "ECS" | "LUC" | "BUK" | "MZM" | "ECV" | "MZN" | "COP" | "LUF" | "STN" | "NIO" | "COU" | "AED" | "ROK" | "LUL" | "ROL" | "MOP" | "MDL" | "RON" | "XTS" | "SIT" | "IQD" | "PEH" | "PEI" | "VUV" | "BRB" | "BRC" | "CLF" | "KPW" | "PEN" | "BRE" | "KES" | "CAD" | "GRD" | "PES" | "RWF" | "MWK" | "MLF" | "LRD" | "BRL" | "CLP" | "AMD" | "BRN" | "MAD" | "BGJ" | "ESA" | "BGK" | "ESB" | "BGL" | "BRR" | "BGN" | "KMF" | "XFO" | "XFU" | "ESP" | "BZD" | "THB" | "BOB" | "HTG" | "ZMK" | "AUD" | "SYP" | "INR" | "MTL" | "XCD" | "MTP" | "ZMW" | "BOP" | "FJD" | "BOV" | "SCR" | "BDT" | "TPE" | "EEK" | "SVC" | "DKK" | "GWE" | "ARA" | "JPY" | "BAD" | "BWP" | "SKK" | "FRF" | "GWP" | "ARP" | "YER" | "BAM" | "ARS" | "LAJ" | "JMD" | "LAK" | "ARY" | "PGK" | "ISJ" | "KRW" | "ISK" | "KGS" | "QAR" | "ZRN" | "CYP" | "TMM" | "AOA" | "OMR" | "BIF" | "UGS" | "HNL" | "BTN" | "AZM" | "SSP" | "UGW" | "TMT" | "AZN" | "MYR" | "ZRZ" | "UGX" | "LTL" | "AOK" | "GTQ" | "SHP" | "CNY" | "AON" | "MNT" | "XSU" | "GIP" | "AOR" | "LTT" | "ADP" | "KZT" | "CVE" | "EUR" | "YUD" | "UZS" | "HKD" | "DEM" | "YUM" | "XPD" | "YUN" | "GQE" | "MKD" | "XPF" | "AWG" | "IEP" | "NPR" | "TJR" | "KWD" | "MVQ" | "TJS" | "MVR" | "SEK" | "ALK" | "ALL" | "XPT" | "ZWC" | "ZWD" | "PAB" | "ERN" | "XEU" | "CSD" | "BYB" | "ZWL" | "PLN" | "ZWN" | "CHC" | "CSJ" | "EGP" | "CSK" | "UAH" | "GYD" | "CHE" | "TRL" | "CHF" | "ZWR" | "BND" | "UAK" | "LYD" | "RSD" | "BYN" | "GNE" | "PLZ" | "ZAL" | "GNF" | "SBD" | "XBA" | "BYR" | "XBB" | "RHD" | "XBC" | "XBD" | "ZAR" | "TRY" | "VNC" | "PTE" | "VND" | "CHW" | "XXX" | "ATS" | "GNS" | "JOD" | "FIM" | "WST" | "DJF" | "XUA" | "TZS" | "YDD" | "TOP" | "AFA" | "SUR" | "LVL" | "LVR" | "AFN" | "TWD" | "LKR" | "SRD" | "BSD" | "SRG" | "XRE" | "SGD" | "IRR" | "BHD" | "GHC" | "MXN" | "MXP" | "KYD" | "AYM" | "MMK" | "NGN" | "ANG" | "ETB" | "LSL" | "LSM" | "MXV" | "PYG" | "DZD" | "GHP" | "UYI" | "GHS" | "CUC" | "NZD" | "TTD" | "UYN" | "UYP" | "HUF" | "UYU" | "DOP" | "RUB" | "UYW" | "SZL" | "DDM" | "CUP" | "NOK" | "BEC" | "SDD" | "XOF" | "BEF" | "SDG" | "IDR" | "MUR" | "BEL" | "SOS" | "GEK" | "RUR" | "SDP" | "GEL" | "VEB" | "VED" | "VEF" | "XDR" | "FKP" | "CRC" | "HRD" | "PKR" | "ZZZ";
   --  ****

   --  ****t* Currencies/ISO.Currencies.Numeric_Code
   --  DESCRIPTION
   --    The numeric currency code defined by ISO 4217.
   --  DERIVED FROM
   --    Natural
   --  SOURCE
   subtype Numeric_Code is Natural
      with Dynamic_Predicate => Numeric_Code in
         528 | 060 | 928 | 925 | 191 | 516 | 969 | 376 | 270 | 052 | 450 | 694 | 478 | 950 | 961 | 929 | 840 | 682 | 826 | 422 | 959 | 788 | 203 | 997 | 678 | 508 | 380 | 116 | 558 | 608 | 998 | 976 | 218 | 989 | 104 | 983 | 943 | 170 | 442 | 930 | 970 | 784 | 642 | 988 | 446 | 498 | 946 | 963 | 705 | 368 | 604 | 548 | 076 | 990 | 408 | 404 | 124 | 300 | 646 | 454 | 466 | 430 | 986 | 152 | 051 | 504 | 100 | 996 | 995 | 987 | 975 | 174 | 724 | 084 | 764 | 068 | 332 | 894 | 036 | 760 | 356 | 470 | 951 | 967 | 242 | 984 | 690 | 050 | 626 | 233 | 222 | 208 | 624 | 032 | 392 | 070 | 072 | 703 | 250 | 886 | 977 | 418 | 388 | 598 | 352 | 410 | 417 | 634 | 180 | 196 | 795 | 973 | 512 | 108 | 800 | 340 | 064 | 031 | 728 | 934 | 944 | 458 | 440 | 024 | 320 | 654 | 156 | 496 | 994 | 292 | 982 | 020 | 398 | 132 | 978 | 890 | 860 | 344 | 276 | 891 | 964 | 226 | 807 | 953 | 533 | 372 | 524 | 762 | 414 | 462 | 972 | 752 | 008 | 962 | 716 | 590 | 232 | 954 | 112 | 932 | 985 | 942 | 948 | 818 | 200 | 980 | 328 | 947 | 792 | 756 | 935 | 096 | 804 | 434 | 941 | 933 | 324 | 616 | 991 | 090 | 955 | 974 | 956 | 957 | 958 | 710 | 949 | 704 | 620 | 999 | 040 | 400 | 246 | 882 | 262 | 965 | 834 | 720 | 776 | 004 | 810 | 428 | 971 | 901 | 144 | 968 | 044 | 740 | 702 | 364 | 048 | 288 | 484 | 136 | 945 | 566 | 532 | 230 | 426 | 979 | 600 | 012 | 939 | 940 | 936 | 931 | 554 | 780 | 858 | 348 | 214 | 643 | 927 | 748 | 278 | 192 | 578 | 993 | 736 | 952 | 056 | 938 | 360 | 480 | 992 | 706 | 268 | 981 | 862 | 926 | 937 | 960 | 238 | 188 | 586 | 0;
   --  ****

   --  ****t* Currencies/ISO.Currencies.Currency_Key
   --  DESCRIPTION
   --    An enumeration of every iso 4217 currency code.
   --    Each code starts with "C_", to prevent any enumeration conlficting
   --    with a reserved word.  So you can cast any currency to currency code by
   --    doing "Currency_Key'Value ("C_" & currency_code)"
   --  EXAMPLE
   --    My_Key : Currency_Key := C_USD;
   --  SOURCE
   type Currency_Key is (
      C_NLG, --  Netherlands Guilder
      C_BMD, --  Bermudian Dollar
      C_VES, --  Bolívar Soberano
      C_SLE, --  Leone
      C_HRK, --  Kuna
      C_NAD, --  Namibia Dollar
      C_MGA, --  Malagasy Ariary
      C_ILP, --  Pound
      C_GMD, --  Dalasi
      C_ILR, --  Old Shekel
      C_BBD, --  Barbados Dollar
      C_MGF, --  Malagasy Franc
      C_ILS, --  New Israeli Sheqel
      C_SLL, --  Leone
      C_MRO, --  Ouguiya
      C_XAF, --  CFA Franc BEAC
      C_XAG, --  Silver
      C_MRU, --  Ouguiya
      C_USD, --  US Dollar
      C_SAR, --  Saudi Riyal
      C_GBP, --  Pound Sterling
      C_LBP, --  Lebanese Pound
      C_XAU, --  Gold
      C_TND, --  Tunisian Dinar
      C_CZK, --  Czech Koruna
      C_USN, --  US Dollar (Next day)
      C_STD, --  Dobra
      C_MZE, --  Mozambique Escudo
      C_ITL, --  Italian Lira
      C_KHR, --  Riel
      C_NIC, --  Cordoba
      C_PHP, --  Philippine Peso
      C_USS, --  US Dollar (Same day)
      C_CDF, --  Congolese Franc
      C_ECS, --  Sucre
      C_LUC, --  Luxembourg Convertible Franc
      C_BUK, --  Kyat
      C_MZM, --  Mozambique Metical
      C_ECV, --  Unidad de Valor Constante (UVC)
      C_MZN, --  Mozambique Metical
      C_COP, --  Colombian Peso
      C_LUF, --  Luxembourg Franc
      C_STN, --  Dobra
      C_NIO, --  Cordoba Oro
      C_COU, --  Unidad de Valor Real
      C_AED, --  UAE Dirham
      C_ROK, --  Leu A/52
      C_LUL, --  Luxembourg Financial Franc
      C_ROL, --  Old Leu
      C_MOP, --  Pataca
      C_MDL, --  Moldovan Leu
      C_RON, --  Romanian Leu
      C_XTS, --  Codes specifically reserved for testing purposes
      C_SIT, --  Tolar
      C_IQD, --  Iraqi Dinar
      C_PEH, --  Sol
      C_PEI, --  Inti
      C_VUV, --  Vatu
      C_BRB, --  Cruzeiro
      C_BRC, --  Cruzado
      C_CLF, --  Unidad de Fomento
      C_KPW, --  North Korean Won
      C_PEN, --  Sol
      C_BRE, --  Cruzeiro
      C_KES, --  Kenyan Shilling
      C_CAD, --  Canadian Dollar
      C_GRD, --  Drachma
      C_PES, --  Sol
      C_RWF, --  Rwanda Franc
      C_MWK, --  Malawi Kwacha
      C_MLF, --  Mali Franc
      C_LRD, --  Liberian Dollar
      C_BRL, --  Brazilian Real
      C_CLP, --  Chilean Peso
      C_AMD, --  Armenian Dram
      C_BRN, --  New Cruzado
      C_MAD, --  Moroccan Dirham
      C_BGJ, --  Lev A/52
      C_ESA, --  Spanish Peseta
      C_BGK, --  Lev A/62
      C_ESB, --  ""A"" Account (convertible Peseta Account)
      C_BGL, --  Lev
      C_BRR, --  Cruzeiro Real
      C_BGN, --  Bulgarian Lev
      C_KMF, --  Comorian Franc
      C_XFO, --  Gold-Franc
      C_XFU, --  UIC-Franc
      C_ESP, --  Spanish Peseta
      C_BZD, --  Belize Dollar
      C_THB, --  Baht
      C_BOB, --  Boliviano
      C_HTG, --  Gourde
      C_ZMK, --  Zambian Kwacha
      C_AUD, --  Australian Dollar
      C_SYP, --  Syrian Pound
      C_INR, --  Indian Rupee
      C_MTL, --  Maltese Lira
      C_XCD, --  East Caribbean Dollar
      C_MTP, --  Maltese Pound
      C_ZMW, --  Zambian Kwacha
      C_BOP, --  Peso boliviano
      C_FJD, --  Fiji Dollar
      C_BOV, --  Mvdol
      C_SCR, --  Seychelles Rupee
      C_BDT, --  Taka
      C_TPE, --  Timor Escudo
      C_EEK, --  Kroon
      C_SVC, --  El Salvador Colon
      C_DKK, --  Danish Krone
      C_GWE, --  Guinea Escudo
      C_ARA, --  Austral
      C_JPY, --  Yen
      C_BAD, --  Dinar
      C_BWP, --  Pula
      C_SKK, --  Slovak Koruna
      C_FRF, --  French Franc
      C_GWP, --  Guinea-Bissau Peso
      C_ARP, --  Peso Argentino
      C_YER, --  Yemeni Rial
      C_BAM, --  Convertible Mark
      C_ARS, --  Argentine Peso
      C_LAJ, --  Pathet Lao Kip
      C_JMD, --  Jamaican Dollar
      C_LAK, --  Lao Kip
      C_ARY, --  Peso
      C_PGK, --  Kina
      C_ISJ, --  Old Krona
      C_KRW, --  Won
      C_ISK, --  Iceland Krona
      C_KGS, --  Som
      C_QAR, --  Qatari Rial
      C_ZRN, --  New Zaire
      C_CYP, --  Cyprus Pound
      C_TMM, --  Turkmenistan Manat
      C_AOA, --  Kwanza
      C_OMR, --  Rial Omani
      C_BIF, --  Burundi Franc
      C_UGS, --  Uganda Shilling
      C_HNL, --  Lempira
      C_BTN, --  Ngultrum
      C_AZM, --  Azerbaijanian Manat
      C_SSP, --  South Sudanese Pound
      C_UGW, --  Old Shilling
      C_TMT, --  Turkmenistan New Manat
      C_AZN, --  Azerbaijan Manat
      C_MYR, --  Malaysian Ringgit
      C_ZRZ, --  Zaire
      C_UGX, --  Uganda Shilling
      C_LTL, --  Lithuanian Litas
      C_AOK, --  Kwanza
      C_GTQ, --  Quetzal
      C_SHP, --  Saint Helena Pound
      C_CNY, --  Yuan Renminbi
      C_AON, --  New Kwanza
      C_MNT, --  Tugrik
      C_XSU, --  Sucre
      C_GIP, --  Gibraltar Pound
      C_AOR, --  Kwanza Reajustado
      C_LTT, --  Talonas
      C_ADP, --  Andorran Peseta
      C_KZT, --  Tenge
      C_CVE, --  Cabo Verde Escudo
      C_EUR, --  Euro
      C_YUD, --  New Yugoslavian Dinar
      C_UZS, --  Uzbekistan Sum
      C_HKD, --  Hong Kong Dollar
      C_DEM, --  Deutsche Mark
      C_YUM, --  New Dinar
      C_XPD, --  Palladium
      C_YUN, --  Yugoslavian Dinar
      C_GQE, --  Ekwele
      C_MKD, --  Denar
      C_XPF, --  CFP Franc
      C_AWG, --  Aruban Florin
      C_IEP, --  Irish Pound
      C_NPR, --  Nepalese Rupee
      C_TJR, --  Tajik Ruble
      C_KWD, --  Kuwaiti Dinar
      C_MVQ, --  Maldive Rupee
      C_TJS, --  Somoni
      C_MVR, --  Rufiyaa
      C_SEK, --  Swedish Krona
      C_ALK, --  Old Lek
      C_ALL, --  Lek
      C_XPT, --  Platinum
      C_ZWC, --  Rhodesian Dollar
      C_ZWD, --  Zimbabwe Dollar
      C_PAB, --  Balboa
      C_ERN, --  Nakfa
      C_XEU, --  European Currency Unit (E.C.U)
      C_CSD, --  Serbian Dinar
      C_BYB, --  Belarusian Ruble
      C_ZWL, --  Zimbabwe Dollar
      C_PLN, --  Zloty
      C_ZWN, --  Zimbabwe Dollar (new)
      C_CHC, --  WIR Franc (for electronic)
      C_CSJ, --  Krona A/53
      C_EGP, --  Egyptian Pound
      C_CSK, --  Koruna
      C_UAH, --  Hryvnia
      C_GYD, --  Guyana Dollar
      C_CHE, --  WIR Euro
      C_TRL, --  Old Turkish Lira
      C_CHF, --  Swiss Franc
      C_ZWR, --  Zimbabwe Dollar
      C_BND, --  Brunei Dollar
      C_UAK, --  Karbovanet
      C_LYD, --  Libyan Dinar
      C_RSD, --  Serbian Dinar
      C_BYN, --  Belarusian Ruble
      C_GNE, --  Syli
      C_PLZ, --  Zloty
      C_ZAL, --  Financial Rand
      C_GNF, --  Guinean Franc
      C_SBD, --  Solomon Islands Dollar
      C_XBA, --  Bond Markets Unit European Composite Unit (EURCO)
      C_BYR, --  Belarusian Ruble
      C_XBB, --  Bond Markets Unit European Monetary Unit (E.M.U.-6)
      C_RHD, --  Rhodesian Dollar
      C_XBC, --  Bond Markets Unit European Unit of Account 9 (E.U.A.-9)
      C_XBD, --  Bond Markets Unit European Unit of Account 17 (E.U.A.-17)
      C_ZAR, --  Rand
      C_TRY, --  Turkish Lira
      C_VNC, --  Old Dong
      C_PTE, --  Portuguese Escudo
      C_VND, --  Dong
      C_CHW, --  WIR Franc
      C_XXX, --  The codes assigned for transactions where no currency is involved
      C_ATS, --  Schilling
      C_GNS, --  Syli
      C_JOD, --  Jordanian Dinar
      C_FIM, --  Markka
      C_WST, --  Tala
      C_DJF, --  Djibouti Franc
      C_XUA, --  ADB Unit of Account
      C_TZS, --  Tanzanian Shilling
      C_YDD, --  Yemeni Dinar
      C_TOP, --  Pa’anga
      C_AFA, --  Afghani
      C_SUR, --  Rouble
      C_LVL, --  Latvian Lats
      C_LVR, --  Latvian Ruble
      C_AFN, --  Afghani
      C_TWD, --  New Taiwan Dollar
      C_LKR, --  Sri Lanka Rupee
      C_SRD, --  Surinam Dollar
      C_BSD, --  Bahamian Dollar
      C_SRG, --  Surinam Guilder
      C_XRE, --  RINET Funds Code
      C_SGD, --  Singapore Dollar
      C_IRR, --  Iranian Rial
      C_BHD, --  Bahraini Dinar
      C_GHC, --  Cedi
      C_MXN, --  Mexican Peso
      C_MXP, --  Mexican Peso
      C_KYD, --  Cayman Islands Dollar
      C_AYM, --  Azerbaijan Manat
      C_MMK, --  Kyat
      C_NGN, --  Naira
      C_ANG, --  Netherlands Antillean Guilder
      C_ETB, --  Ethiopian Birr
      C_LSL, --  Loti
      C_LSM, --  Loti
      C_MXV, --  Mexican Unidad de Inversion (UDI)
      C_PYG, --  Guarani
      C_DZD, --  Algerian Dinar
      C_GHP, --  Ghana Cedi
      C_UYI, --  Uruguay Peso en Unidades Indexadas (UI)
      C_GHS, --  Ghana Cedi
      C_CUC, --  Peso Convertible
      C_NZD, --  New Zealand Dollar
      C_TTD, --  Trinidad and Tobago Dollar
      C_UYN, --  Old Uruguay Peso
      C_UYP, --  Uruguayan Peso
      C_HUF, --  Forint
      C_UYU, --  Peso Uruguayo
      C_DOP, --  Dominican Peso
      C_RUB, --  Russian Ruble
      C_UYW, --  Unidad Previsional
      C_SZL, --  Lilangeni
      C_DDM, --  Mark der DDR
      C_CUP, --  Cuban Peso
      C_NOK, --  Norwegian Krone
      C_BEC, --  Convertible Franc
      C_SDD, --  Sudanese Dinar
      C_XOF, --  CFA Franc BCEAO
      C_BEF, --  Belgian Franc
      C_SDG, --  Sudanese Pound
      C_IDR, --  Rupiah
      C_MUR, --  Mauritius Rupee
      C_BEL, --  Financial Franc
      C_SOS, --  Somali Shilling
      C_GEK, --  Georgian Coupon
      C_RUR, --  Russian Ruble
      C_SDP, --  Sudanese Pound
      C_GEL, --  Lari
      C_VEB, --  Bolivar
      C_VED, --  Bolívar Soberano
      C_VEF, --  Bolívar
      C_XDR, --  SDR (Special Drawing Right)
      C_FKP, --  Falkland Islands Pound
      C_CRC, --  Costa Rican Colon
      C_HRD, --  Croatian Dinar
      C_PKR, --  Pakistan Rupee
      C_ZZZ --  Unknown
   );
   --  ****

   --  ****t* Currencies/ISO.Currencies.Minor_Unit
   --  DESCRIPTION
   --    The unit of the currency defined by ISO 4217.
   --    A minor unit of X where X > 0 would be a delta of 10 ** (-X).
   --  NOTES
   --    A minor unit of 0 may indicate "N.A." rather than 0.
   --  DERIVED FROM
   --    Natural
   --  SOURCE
   subtype Minor_Unit is Natural range 0 ..  4;
   --  ****

   --  ****t* Currencies/ISO.Currencies.Currency
   --  DESCRIPTION
   --    The ISO 4217 currency to be referenced.  When initializing, you can
   --    set the key to Currency_Key that will be utilizeed one of the functions
   --    to access the Currency
   --  USAGE
   --    My_Currency : Currency := (C_USD); --  This is the US Doller.
   --  METHODS
   --  * ISO.Currencies.Currency/Name
   --  * ISO.Currencies.Currency/Code
   --  * ISO.Currencies.Currency/Numeric
   --  * ISO.Currencies.Currency/Unit
   --  * ISO.Currencies.Currency/Symbol
   --  * ISO.Currencies.Currency/Is_Fund
   --  * ISO.Currencies.Currency/Is_Historic
   --  * ISO.Currencies.Currency/Historic_Name
   --  * ISO.Currencies.Currency/Historic_Numerics
   --  * ISO.Currencies.Currency/Withdraw_Date
   --  * ISO.Currencies.Currency/Withdraw_Dates
   --  * ISO.Currencies.Currency/Historic_Entities
   --  * ISO.Currencies.Currency/Historic_Records
   --  * ISO.Currencies.Currency/Entities
   --  EXAMPLE
   --    --  To create a currency and initalize it to the US Doller,
   --    --  then reference it like so:
   --    My_Currency : Currency := (C_USD); --  This is the USA.
   --    --  To access the currency's name, do so like so:
   --    My_Currency.Name --  "US Doller"
   --  SOURCE
   type Currency is tagged record
      Key : Currency_Key := C_ZZZ;
   end record;
   --  ****

   --  ****m* ISO.Currencies.Currency/Name
   --  FUNCTION
   --    Retrieve the name of the provided currency.
   --  RETURN VALUE
   --    String: The ISO 4217 name of the current currency.
   --  EXAMPLE
   --    My_Currency : Currency := (C_USD);
   --    The_Name : String := My_Country.Name; --  "US Doller"
   --  SOURCE
   function Name (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Code
   --  FUNCTION
   --    Retrieve the three-letter currency code of the provided currency.
   --  RETURN VALUE
   --    ISO.Currencies.Alphabetic_Code: ISO 4217 code of the current currency.
   --  EXAMPLE
   --    My_Currency : Currency := (C_USD);
   --    Code : Alphabetic_Code := My_Currency.Code; --  Will return "USD"
   --  SOURCE
   function Code (This : Currency) return Alphabetic_Code;
   --  ****

   --  ****m* ISO.Currencies.Currency/Numeric
   --  FUNCTION
   --    Retrieve the numerical currency code of the provided currency.
   --  RETURN VALUE
   --    ISO.Currencies.Numeric_Code: ISO 4217 number of the current currency.
   --  EXAMPLE
   --    My_Currency : Currency := (C_USD);
   --    Number : Numeric_Code := My_Currency.Numeric; --  Will return 840
   --  SOURCE
   function Numeric (This : Currency) return Numeric_Code;
   --  ****

   --  ****m* ISO.Currencies.Currency/Unit
   --  FUNCTION
   --    Retrieve the minor unit of the provided currency.
   --    A minor unit of X would be a delta of 10 ** (-X).
   --  RETURN VALUE
   --    ISO.Currencies.Minor_Unit: ISO 4217 minor unit of the currency.
   --  EXAMPLE
   --    My_Currency : Currency := (C_USD);
   --    Number : Minor_Unit := My_Currency.Unit; --  Will return 2
   --  SOURCE
   function Unit (This : Currency) return Minor_Unit;
   --  ****

   --  ****m* ISO.Currencies.Currency/Symbol
   --  FUNCTION
   --    If applicable, return the symbol associated with a given currency.
   --    This is not part of the ISO 4217 standard, but nice to have.
   --    It may return a unicode-encoded string, so more than one character.
   --  RETURN VALUE
   --    String or Wide_Wide_String: Current symbol captured by the currency or empty string if no symbol was found.
   --  NOTES
   --    Some symbols may be unicode; if that's the case, you may want to use a wide_wide_string.  Otherwise it will be converted into a String.
   --  EXAMPLE
   --    My_Currency1 : Currency := (C_USD);
   --    My_Currency2 : Currency := (C_AZN);
   --    Symb  : String := My_Currency1.Symbol; --  "$"
   --    Symb2 : Wide_Wide_String := My_Currency2.Symbol; -- "₼"
   --  SOURCE
   function Symbol (This : Currency) return Wide_Wide_String;
   function Symbol (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Is_Fund
   --  FUNCTION
   --    Indicates whether or not the currency code is a fund.
   --  RETURN VALUE
   --   * True is the currency is a fund.
   --   * False if the currency is not fund.
   --  EXAMPLE
   --    declare
   --      My_Currency : Currency := (C_CLF);
   --    begin
   --      if My_Currency.Is_Fund then
   --        Put_Line ("Is a fund.");
   --      end if;
   --    end;
   --  SOURCE
   function Is_Fund (This : Currency) return Boolean;
   --  ****

   --  ****m* ISO.Currencies.Currency/Is_Historic
   --  FUNCTION
   --    Indicates whether or not the currency code is a historc record
   --    contained in ISO 4217 List-3.
   --  RETURN VALUE
   --   * True is the currency is historic (in ISO 4217-3).
   --   * False if the currency is not historic.
   --  EXAMPLE
   --    declare
   --      My_Currency : Currency := (C_AFA);
   --    begin
   --      if My_Currency.Is_Historic then
   --        Put_Line ("Is a historic code.");
   --      end if;
   --    end;
   --  SOURCE
   function Is_Historic (This : Currency) return Boolean;
   --  ****

   --  ****m* ISO.Currencies.Currency/Withdraw_Date
   --  FUNCTION
   --    Retrieve the MOST RECENT withdraw date for the historical record of the conutry code.
   --  RETURN VALUE
   --   String: Contains the historical withdraw date of the code specificed in ISO 4217.  This may be a year and a month or a range, or a null string if the country is not historic.
   --  EXAMPLE
   --    Currency1 : Currency := (C_AFA);
   --    Currency2 : Currency := (C_CSJ);
   --    Date1     : String   := Currency1.Withdraw_Date; -- "2003-01"
   --    Date2     : String   := Currency2.Withdraw_Date; -- "1989 to 1990"
   --  SOURCE
   function Withdraw_Date (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Withdraw_Dates
   --  FUNCTION
   --    Retrieve a semi colon separated list of historic withdraw dates associated with the iso 4217-3 standard as a string.
   --  RETURN VALUE
   --   String: Contains the historical withdraw date of the code specificed in ISO 4217.  This may be a year and a month or a range, or a null string if the country is not historic.
   --  EXAMPLE
   --    Currency : Currency := (VEF);
   --    Dates    : String   := Currency.Withdraw_Dates; -- "2011-12;2016-02;2018-08"
   --  SOURCE
   function Withdraw_Dates (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Historic_Name
   --  FUNCTION
   --    Retrieve the historic names of the provided currency.
   --  RETURN VALUE
   --    String: A semi colon separated list of historic names. Empty string if not historic.
   --  EXAMPLE
   --    My_Currency : Currency := (C_VEF);
   --    The_Names : String := My_Country.Historic_Names; --  "Bolivar Fuerte;Bolivar;Bolívar"
   --  SOURCE
   function Historic_Names (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Historic_Numerics
   --  FUNCTION
   --    Retrieve the historic numermic values of the provided currency.
   --  RETURN VALUE
   --    String: A semi colon separated list of historic Numeric values.
   --  EXAMPLE
   --    My_Currency : Currency := (C_VEF);
   --    The_Names : String := My_Country.Historic_Numeric; --  "937"
   --  SOURCE
   function Historic_Numerics (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Historic_Entities
   --  FUNCTION
   --    Retrieve a semi colon separated list of historic countries associated with the iso 4217-3 standard as a string.
   --  RETURN VALUE
   --    String: List of historical countries associated with a record.
   --  TODO
   --    Handle historic country records utilizing the iso standard for historic countries.
   --  EXAMPLE
   --    My_Currency : Currency := (C_VEF);
   --    My_Counties : String := My_Currency.Historic_Entities; -- "VENEZUELA;VENEZUELA (BOLIVARIAN REPUBLIC OF)"
   --  SOURCE
   function Historic_Entities (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Historic_Records
   --  FUNCTION
   --    Retrieve a colon-and-semi colon separated list of the full iso 4217-3 records as a string.
   --  RETURN VALUE
   --    String: List of historical record for a currency.
   --  TODO
   --    Handle historic country records utilizing the iso standard for historic countries.
   --  EXAMPLE
   --    My_Currency : Currency := (C_VEF);
   --    My_Records : String := My_Currency.Historic_Records; -- "VENEZUELA:Bolivar Fuerte:937:2011-12;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolivar:937:2016-02;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolívar:937:2018-08"
   --  SOURCE
   function Historic_Records (This : Currency) return String;
   --  ****

   --  ****m* ISO.Currencies.Currency/Entities
   --  FUNCTION
   --    Retrieve all countries currently associated with a currency.
   --  RETURN VALUE
   --    ISO.Countries.Country_List: Array containing the countries.
   --  EXAMPLE
   --    My_Currency : Currency := (C_GBP);
   --    My_Counties : ISO.Countries.Country_List := My_Currency.Entities; -- returns the United Kingdom, Jersey, Isle of Man, and Guernsey.
   --  SOURCE
   function Entities (This : Currency) return Countries.Country_List;
   --  ****

   --  ****f* Currencies/ISO.Currencies.From_Code
   --  FUNCTION
   --    Create a currency from a provided alphabetic code string.
   --  PARAMETERS
   --    Code - An alphabetic code, such as "EUR" or "USD".
   --  RETURN VALUE
   --    ISO.Currencies.Currency: Currency corresponding to that code.
   --  EXAMPLE
   --    My_Currency : Currency := From_Code("USD");
   --  SOURCE
   function From_Code (Code : Alphabetic_Code) return Currency;
   --  ****

   --  ****f* Currencies/ISO.Currencies.From_Numeric
   --  FUNCTION
   --    Create a non-historical currency from a provided numerical code string.
   --  NOTES
   --    This will only create non-historical currencies, since some historical curriences have the same number but different symbol.
   --  TODO
   --    Create "function From_Numeric (Number : Numeric_Code) return Currency_List" with historical support
   --  EXAMPLES
   --    My_Currency_1 : Currency := From_Numeric(36);
   --    My_Currency_2 : Currency := From_Numeric(036);
   --    My_Currency_3 : Currency := From_Numeric("036");
   --  PARAMETERS
   --    Number - A Numeric Code, either as a string or integer.
   --  RETURN VALUE
   --    ISO.Currencies.Currency: Currency corresponding to that numerical code.
   --  SOURCE
   function From_Numeric (Number : Numeric_Code) return Currency;
   function From_Numeric (Number : String) return Currency;
   --  ****

   --  ****t* Currencies/ISO.Currencies.Currency_List
   --  DESCRIPTION
   --    An arbitrary-sized array of currencies.
   --  USAGE
   --    declare
   --       My_Currencies : Currency_List (1 .. 2);
   --    begin
   --       My_Currencies (1) := (Key => C_USD);
   --       My_Currencies (2) := (Key => C_EUR);
   --    end;
   --  SOURCE
   type Currency_List is array (Positive range <>) of Currency;
   --  ****

   --  ****t* Currencies/ISO.Currencies.All_Currencies
   --  DESCRIPTION
   --    All of the currencies, utilizing the Currency_Key as an index to
   --    conserve the stack.
   --  USAGE
   --    My_Currencies : constant All_Currencies := Init_Currencies;
   --    USD_Name : String := My_Currencies (C_USD).Name;
   --  SOURCE
   type All_Currencies is array (Currency_Key'Range) of Currency;
   --  ****

   --  ****f* Currencies/ISO.Currencies.From_Country
   --  FUNCTION
   --    Retrieve all currencies currently associated with a country.
   --  RETURN VALUE
   --    ISO.Currencies.Currency_List: Array containing the currencies.
   --  EXAMPLE
   --    My_Country : ISO.Countries.Country := (C_BO); --  Bolivia
   --    My_Currencies : constant Currency_List := From_Country(My_Country); --  Returns Boliviano and Mvdol.
   --  SOURCE
   function From_Country (Item : Countries.Country) return Currency_List;
   --  ****

   --  ****f* Currencies/ISO.Currencies.Init_Currencies
   --  FUNCTION
   --    Initialize all of the currencies in an array.
   --  RETURN VALUE
   --    ISO.Currencies.All_Currencies: Array containing all countries.
   --  USAGE
   --    My_Currencies : constant All_Currencies := Init_Currencies;
   --  SOURCE
   function Init_Currencies return All_Currencies;
   --  ****
private
   function Numeric_To_Key (Numeric : Numeric_Code) return Currency_Key;
end ISO.Currencies;

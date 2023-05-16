with Ada.Characters.Conversions;
package body ISO.Currencies is

   function Name (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "Netherlands Guilder";
         when C_BMD => return "Bermudian Dollar";
         when C_VES => return "Bolívar Soberano";
         when C_SLE => return "Leone";
         when C_HRK => return "Kuna";
         when C_NAD => return "Namibia Dollar";
         when C_MGA => return "Malagasy Ariary";
         when C_ILP => return "Pound";
         when C_GMD => return "Dalasi";
         when C_ILR => return "Old Shekel";
         when C_BBD => return "Barbados Dollar";
         when C_MGF => return "Malagasy Franc";
         when C_ILS => return "New Israeli Sheqel";
         when C_SLL => return "Leone";
         when C_MRO => return "Ouguiya";
         when C_XAF => return "CFA Franc BEAC";
         when C_XAG => return "Silver";
         when C_MRU => return "Ouguiya";
         when C_USD => return "US Dollar";
         when C_SAR => return "Saudi Riyal";
         when C_GBP => return "Pound Sterling";
         when C_LBP => return "Lebanese Pound";
         when C_XAU => return "Gold";
         when C_TND => return "Tunisian Dinar";
         when C_CZK => return "Czech Koruna";
         when C_USN => return "US Dollar (Next day)";
         when C_STD => return "Dobra";
         when C_MZE => return "Mozambique Escudo";
         when C_ITL => return "Italian Lira";
         when C_KHR => return "Riel";
         when C_NIC => return "Cordoba";
         when C_PHP => return "Philippine Peso";
         when C_USS => return "US Dollar (Same day)";
         when C_CDF => return "Congolese Franc";
         when C_ECS => return "Sucre";
         when C_LUC => return "Luxembourg Convertible Franc";
         when C_BUK => return "Kyat";
         when C_MZM => return "Mozambique Metical";
         when C_ECV => return "Unidad de Valor Constante (UVC)";
         when C_MZN => return "Mozambique Metical";
         when C_COP => return "Colombian Peso";
         when C_LUF => return "Luxembourg Franc";
         when C_STN => return "Dobra";
         when C_NIO => return "Cordoba Oro";
         when C_COU => return "Unidad de Valor Real";
         when C_AED => return "UAE Dirham";
         when C_ROK => return "Leu A/52";
         when C_LUL => return "Luxembourg Financial Franc";
         when C_ROL => return "Old Leu";
         when C_MOP => return "Pataca";
         when C_MDL => return "Moldovan Leu";
         when C_RON => return "Romanian Leu";
         when C_XTS => return "Codes specifically reserved for testing purposes";
         when C_SIT => return "Tolar";
         when C_IQD => return "Iraqi Dinar";
         when C_PEH => return "Sol";
         when C_PEI => return "Inti";
         when C_VUV => return "Vatu";
         when C_BRB => return "Cruzeiro";
         when C_BRC => return "Cruzado";
         when C_CLF => return "Unidad de Fomento";
         when C_KPW => return "North Korean Won";
         when C_PEN => return "Sol";
         when C_BRE => return "Cruzeiro";
         when C_KES => return "Kenyan Shilling";
         when C_CAD => return "Canadian Dollar";
         when C_GRD => return "Drachma";
         when C_PES => return "Sol";
         when C_RWF => return "Rwanda Franc";
         when C_MWK => return "Malawi Kwacha";
         when C_MLF => return "Mali Franc";
         when C_LRD => return "Liberian Dollar";
         when C_BRL => return "Brazilian Real";
         when C_CLP => return "Chilean Peso";
         when C_AMD => return "Armenian Dram";
         when C_BRN => return "New Cruzado";
         when C_MAD => return "Moroccan Dirham";
         when C_BGJ => return "Lev A/52";
         when C_ESA => return "Spanish Peseta";
         when C_BGK => return "Lev A/62";
         when C_ESB => return """A"" Account (convertible Peseta Account)";
         when C_BGL => return "Lev";
         when C_BRR => return "Cruzeiro Real";
         when C_BGN => return "Bulgarian Lev";
         when C_KMF => return "Comorian Franc";
         when C_XFO => return "Gold-Franc";
         when C_XFU => return "UIC-Franc";
         when C_ESP => return "Spanish Peseta";
         when C_BZD => return "Belize Dollar";
         when C_THB => return "Baht";
         when C_BOB => return "Boliviano";
         when C_HTG => return "Gourde";
         when C_ZMK => return "Zambian Kwacha";
         when C_AUD => return "Australian Dollar";
         when C_SYP => return "Syrian Pound";
         when C_INR => return "Indian Rupee";
         when C_MTL => return "Maltese Lira";
         when C_XCD => return "East Caribbean Dollar";
         when C_MTP => return "Maltese Pound";
         when C_ZMW => return "Zambian Kwacha";
         when C_BOP => return "Peso boliviano";
         when C_FJD => return "Fiji Dollar";
         when C_BOV => return "Mvdol";
         when C_SCR => return "Seychelles Rupee";
         when C_BDT => return "Taka";
         when C_TPE => return "Timor Escudo";
         when C_EEK => return "Kroon";
         when C_SVC => return "El Salvador Colon";
         when C_DKK => return "Danish Krone";
         when C_GWE => return "Guinea Escudo";
         when C_ARA => return "Austral";
         when C_JPY => return "Yen";
         when C_BAD => return "Dinar";
         when C_BWP => return "Pula";
         when C_SKK => return "Slovak Koruna";
         when C_FRF => return "French Franc";
         when C_GWP => return "Guinea-Bissau Peso";
         when C_ARP => return "Peso Argentino";
         when C_YER => return "Yemeni Rial";
         when C_BAM => return "Convertible Mark";
         when C_ARS => return "Argentine Peso";
         when C_LAJ => return "Pathet Lao Kip";
         when C_JMD => return "Jamaican Dollar";
         when C_LAK => return "Lao Kip";
         when C_ARY => return "Peso";
         when C_PGK => return "Kina";
         when C_ISJ => return "Old Krona";
         when C_KRW => return "Won";
         when C_ISK => return "Iceland Krona";
         when C_KGS => return "Som";
         when C_QAR => return "Qatari Rial";
         when C_ZRN => return "New Zaire";
         when C_CYP => return "Cyprus Pound";
         when C_TMM => return "Turkmenistan Manat";
         when C_AOA => return "Kwanza";
         when C_OMR => return "Rial Omani";
         when C_BIF => return "Burundi Franc";
         when C_UGS => return "Uganda Shilling";
         when C_HNL => return "Lempira";
         when C_BTN => return "Ngultrum";
         when C_AZM => return "Azerbaijanian Manat";
         when C_SSP => return "South Sudanese Pound";
         when C_UGW => return "Old Shilling";
         when C_TMT => return "Turkmenistan New Manat";
         when C_AZN => return "Azerbaijan Manat";
         when C_MYR => return "Malaysian Ringgit";
         when C_ZRZ => return "Zaire";
         when C_UGX => return "Uganda Shilling";
         when C_LTL => return "Lithuanian Litas";
         when C_AOK => return "Kwanza";
         when C_GTQ => return "Quetzal";
         when C_SHP => return "Saint Helena Pound";
         when C_CNY => return "Yuan Renminbi";
         when C_AON => return "New Kwanza";
         when C_MNT => return "Tugrik";
         when C_XSU => return "Sucre";
         when C_GIP => return "Gibraltar Pound";
         when C_AOR => return "Kwanza Reajustado";
         when C_LTT => return "Talonas";
         when C_ADP => return "Andorran Peseta";
         when C_KZT => return "Tenge";
         when C_CVE => return "Cabo Verde Escudo";
         when C_EUR => return "Euro";
         when C_YUD => return "New Yugoslavian Dinar";
         when C_UZS => return "Uzbekistan Sum";
         when C_HKD => return "Hong Kong Dollar";
         when C_DEM => return "Deutsche Mark";
         when C_YUM => return "New Dinar";
         when C_XPD => return "Palladium";
         when C_YUN => return "Yugoslavian Dinar";
         when C_GQE => return "Ekwele";
         when C_MKD => return "Denar";
         when C_XPF => return "CFP Franc";
         when C_AWG => return "Aruban Florin";
         when C_IEP => return "Irish Pound";
         when C_NPR => return "Nepalese Rupee";
         when C_TJR => return "Tajik Ruble";
         when C_KWD => return "Kuwaiti Dinar";
         when C_MVQ => return "Maldive Rupee";
         when C_TJS => return "Somoni";
         when C_MVR => return "Rufiyaa";
         when C_SEK => return "Swedish Krona";
         when C_ALK => return "Old Lek";
         when C_ALL => return "Lek";
         when C_XPT => return "Platinum";
         when C_ZWC => return "Rhodesian Dollar";
         when C_ZWD => return "Zimbabwe Dollar";
         when C_PAB => return "Balboa";
         when C_ERN => return "Nakfa";
         when C_XEU => return "European Currency Unit (E.C.U)";
         when C_CSD => return "Serbian Dinar";
         when C_BYB => return "Belarusian Ruble";
         when C_ZWL => return "Zimbabwe Dollar";
         when C_PLN => return "Zloty";
         when C_ZWN => return "Zimbabwe Dollar (new)";
         when C_CHC => return "WIR Franc (for electronic)";
         when C_CSJ => return "Krona A/53";
         when C_EGP => return "Egyptian Pound";
         when C_CSK => return "Koruna";
         when C_UAH => return "Hryvnia";
         when C_GYD => return "Guyana Dollar";
         when C_CHE => return "WIR Euro";
         when C_TRL => return "Old Turkish Lira";
         when C_CHF => return "Swiss Franc";
         when C_ZWR => return "Zimbabwe Dollar";
         when C_BND => return "Brunei Dollar";
         when C_UAK => return "Karbovanet";
         when C_LYD => return "Libyan Dinar";
         when C_RSD => return "Serbian Dinar";
         when C_BYN => return "Belarusian Ruble";
         when C_GNE => return "Syli";
         when C_PLZ => return "Zloty";
         when C_ZAL => return "Financial Rand";
         when C_GNF => return "Guinean Franc";
         when C_SBD => return "Solomon Islands Dollar";
         when C_XBA => return "Bond Markets Unit European Composite Unit (EURCO)";
         when C_BYR => return "Belarusian Ruble";
         when C_XBB => return "Bond Markets Unit European Monetary Unit (E.M.U.-6)";
         when C_RHD => return "Rhodesian Dollar";
         when C_XBC => return "Bond Markets Unit European Unit of Account 9 (E.U.A.-9)";
         when C_XBD => return "Bond Markets Unit European Unit of Account 17 (E.U.A.-17)";
         when C_ZAR => return "Rand";
         when C_TRY => return "Turkish Lira";
         when C_VNC => return "Old Dong";
         when C_PTE => return "Portuguese Escudo";
         when C_VND => return "Dong";
         when C_CHW => return "WIR Franc";
         when C_XXX => return "The codes assigned for transactions where no currency is involved";
         when C_ATS => return "Schilling";
         when C_GNS => return "Syli";
         when C_JOD => return "Jordanian Dinar";
         when C_FIM => return "Markka";
         when C_WST => return "Tala";
         when C_DJF => return "Djibouti Franc";
         when C_XUA => return "ADB Unit of Account";
         when C_TZS => return "Tanzanian Shilling";
         when C_YDD => return "Yemeni Dinar";
         when C_TOP => return "Pa’anga";
         when C_AFA => return "Afghani";
         when C_SUR => return "Rouble";
         when C_LVL => return "Latvian Lats";
         when C_LVR => return "Latvian Ruble";
         when C_AFN => return "Afghani";
         when C_TWD => return "New Taiwan Dollar";
         when C_LKR => return "Sri Lanka Rupee";
         when C_SRD => return "Surinam Dollar";
         when C_BSD => return "Bahamian Dollar";
         when C_SRG => return "Surinam Guilder";
         when C_XRE => return "RINET Funds Code";
         when C_SGD => return "Singapore Dollar";
         when C_IRR => return "Iranian Rial";
         when C_BHD => return "Bahraini Dinar";
         when C_GHC => return "Cedi";
         when C_MXN => return "Mexican Peso";
         when C_MXP => return "Mexican Peso";
         when C_KYD => return "Cayman Islands Dollar";
         when C_AYM => return "Azerbaijan Manat";
         when C_MMK => return "Kyat";
         when C_NGN => return "Naira";
         when C_ANG => return "Netherlands Antillean Guilder";
         when C_ETB => return "Ethiopian Birr";
         when C_LSL => return "Loti";
         when C_LSM => return "Loti";
         when C_MXV => return "Mexican Unidad de Inversion (UDI)";
         when C_PYG => return "Guarani";
         when C_DZD => return "Algerian Dinar";
         when C_GHP => return "Ghana Cedi";
         when C_UYI => return "Uruguay Peso en Unidades Indexadas (UI)";
         when C_GHS => return "Ghana Cedi";
         when C_CUC => return "Peso Convertible";
         when C_NZD => return "New Zealand Dollar";
         when C_TTD => return "Trinidad and Tobago Dollar";
         when C_UYN => return "Old Uruguay Peso";
         when C_UYP => return "Uruguayan Peso";
         when C_HUF => return "Forint";
         when C_UYU => return "Peso Uruguayo";
         when C_DOP => return "Dominican Peso";
         when C_RUB => return "Russian Ruble";
         when C_UYW => return "Unidad Previsional";
         when C_SZL => return "Lilangeni";
         when C_DDM => return "Mark der DDR";
         when C_CUP => return "Cuban Peso";
         when C_NOK => return "Norwegian Krone";
         when C_BEC => return "Convertible Franc";
         when C_SDD => return "Sudanese Dinar";
         when C_XOF => return "CFA Franc BCEAO";
         when C_BEF => return "Belgian Franc";
         when C_SDG => return "Sudanese Pound";
         when C_IDR => return "Rupiah";
         when C_MUR => return "Mauritius Rupee";
         when C_BEL => return "Financial Franc";
         when C_SOS => return "Somali Shilling";
         when C_GEK => return "Georgian Coupon";
         when C_RUR => return "Russian Ruble";
         when C_SDP => return "Sudanese Pound";
         when C_GEL => return "Lari";
         when C_VEB => return "Bolivar";
         when C_VED => return "Bolívar Soberano";
         when C_VEF => return "Bolívar";
         when C_XDR => return "SDR (Special Drawing Right)";
         when C_FKP => return "Falkland Islands Pound";
         when C_CRC => return "Costa Rican Colon";
         when C_HRD => return "Croatian Dinar";
         when C_PKR => return "Pakistan Rupee";
         when others => return "Unknown";
      end case;
   end Name;

   function Code (This : Currency) return Alphabetic_Code is
   begin
      case This.Key is
         when C_NLG => return "NLG";
         when C_BMD => return "BMD";
         when C_VES => return "VES";
         when C_SLE => return "SLE";
         when C_HRK => return "HRK";
         when C_NAD => return "NAD";
         when C_MGA => return "MGA";
         when C_ILP => return "ILP";
         when C_GMD => return "GMD";
         when C_ILR => return "ILR";
         when C_BBD => return "BBD";
         when C_MGF => return "MGF";
         when C_ILS => return "ILS";
         when C_SLL => return "SLL";
         when C_MRO => return "MRO";
         when C_XAF => return "XAF";
         when C_XAG => return "XAG";
         when C_MRU => return "MRU";
         when C_USD => return "USD";
         when C_SAR => return "SAR";
         when C_GBP => return "GBP";
         when C_LBP => return "LBP";
         when C_XAU => return "XAU";
         when C_TND => return "TND";
         when C_CZK => return "CZK";
         when C_USN => return "USN";
         when C_STD => return "STD";
         when C_MZE => return "MZE";
         when C_ITL => return "ITL";
         when C_KHR => return "KHR";
         when C_NIC => return "NIC";
         when C_PHP => return "PHP";
         when C_USS => return "USS";
         when C_CDF => return "CDF";
         when C_ECS => return "ECS";
         when C_LUC => return "LUC";
         when C_BUK => return "BUK";
         when C_MZM => return "MZM";
         when C_ECV => return "ECV";
         when C_MZN => return "MZN";
         when C_COP => return "COP";
         when C_LUF => return "LUF";
         when C_STN => return "STN";
         when C_NIO => return "NIO";
         when C_COU => return "COU";
         when C_AED => return "AED";
         when C_ROK => return "ROK";
         when C_LUL => return "LUL";
         when C_ROL => return "ROL";
         when C_MOP => return "MOP";
         when C_MDL => return "MDL";
         when C_RON => return "RON";
         when C_XTS => return "XTS";
         when C_SIT => return "SIT";
         when C_IQD => return "IQD";
         when C_PEH => return "PEH";
         when C_PEI => return "PEI";
         when C_VUV => return "VUV";
         when C_BRB => return "BRB";
         when C_BRC => return "BRC";
         when C_CLF => return "CLF";
         when C_KPW => return "KPW";
         when C_PEN => return "PEN";
         when C_BRE => return "BRE";
         when C_KES => return "KES";
         when C_CAD => return "CAD";
         when C_GRD => return "GRD";
         when C_PES => return "PES";
         when C_RWF => return "RWF";
         when C_MWK => return "MWK";
         when C_MLF => return "MLF";
         when C_LRD => return "LRD";
         when C_BRL => return "BRL";
         when C_CLP => return "CLP";
         when C_AMD => return "AMD";
         when C_BRN => return "BRN";
         when C_MAD => return "MAD";
         when C_BGJ => return "BGJ";
         when C_ESA => return "ESA";
         when C_BGK => return "BGK";
         when C_ESB => return "ESB";
         when C_BGL => return "BGL";
         when C_BRR => return "BRR";
         when C_BGN => return "BGN";
         when C_KMF => return "KMF";
         when C_XFO => return "XFO";
         when C_XFU => return "XFU";
         when C_ESP => return "ESP";
         when C_BZD => return "BZD";
         when C_THB => return "THB";
         when C_BOB => return "BOB";
         when C_HTG => return "HTG";
         when C_ZMK => return "ZMK";
         when C_AUD => return "AUD";
         when C_SYP => return "SYP";
         when C_INR => return "INR";
         when C_MTL => return "MTL";
         when C_XCD => return "XCD";
         when C_MTP => return "MTP";
         when C_ZMW => return "ZMW";
         when C_BOP => return "BOP";
         when C_FJD => return "FJD";
         when C_BOV => return "BOV";
         when C_SCR => return "SCR";
         when C_BDT => return "BDT";
         when C_TPE => return "TPE";
         when C_EEK => return "EEK";
         when C_SVC => return "SVC";
         when C_DKK => return "DKK";
         when C_GWE => return "GWE";
         when C_ARA => return "ARA";
         when C_JPY => return "JPY";
         when C_BAD => return "BAD";
         when C_BWP => return "BWP";
         when C_SKK => return "SKK";
         when C_FRF => return "FRF";
         when C_GWP => return "GWP";
         when C_ARP => return "ARP";
         when C_YER => return "YER";
         when C_BAM => return "BAM";
         when C_ARS => return "ARS";
         when C_LAJ => return "LAJ";
         when C_JMD => return "JMD";
         when C_LAK => return "LAK";
         when C_ARY => return "ARY";
         when C_PGK => return "PGK";
         when C_ISJ => return "ISJ";
         when C_KRW => return "KRW";
         when C_ISK => return "ISK";
         when C_KGS => return "KGS";
         when C_QAR => return "QAR";
         when C_ZRN => return "ZRN";
         when C_CYP => return "CYP";
         when C_TMM => return "TMM";
         when C_AOA => return "AOA";
         when C_OMR => return "OMR";
         when C_BIF => return "BIF";
         when C_UGS => return "UGS";
         when C_HNL => return "HNL";
         when C_BTN => return "BTN";
         when C_AZM => return "AZM";
         when C_SSP => return "SSP";
         when C_UGW => return "UGW";
         when C_TMT => return "TMT";
         when C_AZN => return "AZN";
         when C_MYR => return "MYR";
         when C_ZRZ => return "ZRZ";
         when C_UGX => return "UGX";
         when C_LTL => return "LTL";
         when C_AOK => return "AOK";
         when C_GTQ => return "GTQ";
         when C_SHP => return "SHP";
         when C_CNY => return "CNY";
         when C_AON => return "AON";
         when C_MNT => return "MNT";
         when C_XSU => return "XSU";
         when C_GIP => return "GIP";
         when C_AOR => return "AOR";
         when C_LTT => return "LTT";
         when C_ADP => return "ADP";
         when C_KZT => return "KZT";
         when C_CVE => return "CVE";
         when C_EUR => return "EUR";
         when C_YUD => return "YUD";
         when C_UZS => return "UZS";
         when C_HKD => return "HKD";
         when C_DEM => return "DEM";
         when C_YUM => return "YUM";
         when C_XPD => return "XPD";
         when C_YUN => return "YUN";
         when C_GQE => return "GQE";
         when C_MKD => return "MKD";
         when C_XPF => return "XPF";
         when C_AWG => return "AWG";
         when C_IEP => return "IEP";
         when C_NPR => return "NPR";
         when C_TJR => return "TJR";
         when C_KWD => return "KWD";
         when C_MVQ => return "MVQ";
         when C_TJS => return "TJS";
         when C_MVR => return "MVR";
         when C_SEK => return "SEK";
         when C_ALK => return "ALK";
         when C_ALL => return "ALL";
         when C_XPT => return "XPT";
         when C_ZWC => return "ZWC";
         when C_ZWD => return "ZWD";
         when C_PAB => return "PAB";
         when C_ERN => return "ERN";
         when C_XEU => return "XEU";
         when C_CSD => return "CSD";
         when C_BYB => return "BYB";
         when C_ZWL => return "ZWL";
         when C_PLN => return "PLN";
         when C_ZWN => return "ZWN";
         when C_CHC => return "CHC";
         when C_CSJ => return "CSJ";
         when C_EGP => return "EGP";
         when C_CSK => return "CSK";
         when C_UAH => return "UAH";
         when C_GYD => return "GYD";
         when C_CHE => return "CHE";
         when C_TRL => return "TRL";
         when C_CHF => return "CHF";
         when C_ZWR => return "ZWR";
         when C_BND => return "BND";
         when C_UAK => return "UAK";
         when C_LYD => return "LYD";
         when C_RSD => return "RSD";
         when C_BYN => return "BYN";
         when C_GNE => return "GNE";
         when C_PLZ => return "PLZ";
         when C_ZAL => return "ZAL";
         when C_GNF => return "GNF";
         when C_SBD => return "SBD";
         when C_XBA => return "XBA";
         when C_BYR => return "BYR";
         when C_XBB => return "XBB";
         when C_RHD => return "RHD";
         when C_XBC => return "XBC";
         when C_XBD => return "XBD";
         when C_ZAR => return "ZAR";
         when C_TRY => return "TRY";
         when C_VNC => return "VNC";
         when C_PTE => return "PTE";
         when C_VND => return "VND";
         when C_CHW => return "CHW";
         when C_XXX => return "XXX";
         when C_ATS => return "ATS";
         when C_GNS => return "GNS";
         when C_JOD => return "JOD";
         when C_FIM => return "FIM";
         when C_WST => return "WST";
         when C_DJF => return "DJF";
         when C_XUA => return "XUA";
         when C_TZS => return "TZS";
         when C_YDD => return "YDD";
         when C_TOP => return "TOP";
         when C_AFA => return "AFA";
         when C_SUR => return "SUR";
         when C_LVL => return "LVL";
         when C_LVR => return "LVR";
         when C_AFN => return "AFN";
         when C_TWD => return "TWD";
         when C_LKR => return "LKR";
         when C_SRD => return "SRD";
         when C_BSD => return "BSD";
         when C_SRG => return "SRG";
         when C_XRE => return "XRE";
         when C_SGD => return "SGD";
         when C_IRR => return "IRR";
         when C_BHD => return "BHD";
         when C_GHC => return "GHC";
         when C_MXN => return "MXN";
         when C_MXP => return "MXP";
         when C_KYD => return "KYD";
         when C_AYM => return "AYM";
         when C_MMK => return "MMK";
         when C_NGN => return "NGN";
         when C_ANG => return "ANG";
         when C_ETB => return "ETB";
         when C_LSL => return "LSL";
         when C_LSM => return "LSM";
         when C_MXV => return "MXV";
         when C_PYG => return "PYG";
         when C_DZD => return "DZD";
         when C_GHP => return "GHP";
         when C_UYI => return "UYI";
         when C_GHS => return "GHS";
         when C_CUC => return "CUC";
         when C_NZD => return "NZD";
         when C_TTD => return "TTD";
         when C_UYN => return "UYN";
         when C_UYP => return "UYP";
         when C_HUF => return "HUF";
         when C_UYU => return "UYU";
         when C_DOP => return "DOP";
         when C_RUB => return "RUB";
         when C_UYW => return "UYW";
         when C_SZL => return "SZL";
         when C_DDM => return "DDM";
         when C_CUP => return "CUP";
         when C_NOK => return "NOK";
         when C_BEC => return "BEC";
         when C_SDD => return "SDD";
         when C_XOF => return "XOF";
         when C_BEF => return "BEF";
         when C_SDG => return "SDG";
         when C_IDR => return "IDR";
         when C_MUR => return "MUR";
         when C_BEL => return "BEL";
         when C_SOS => return "SOS";
         when C_GEK => return "GEK";
         when C_RUR => return "RUR";
         when C_SDP => return "SDP";
         when C_GEL => return "GEL";
         when C_VEB => return "VEB";
         when C_VED => return "VED";
         when C_VEF => return "VEF";
         when C_XDR => return "XDR";
         when C_FKP => return "FKP";
         when C_CRC => return "CRC";
         when C_HRD => return "HRD";
         when C_PKR => return "PKR";
         when C_ZZZ => return "ZZZ";
      end case;
   end Code;

   function Numeric (This : Currency) return Numeric_Code is
   begin
      case This.Key is
         when C_NLG => return 528;
         when C_BMD => return 060;
         when C_VES => return 928;
         when C_SLE => return 925;
         when C_HRK => return 191;
         when C_NAD => return 516;
         when C_MGA => return 969;
         when C_ILP => return 376;
         when C_GMD => return 270;
         when C_ILR => return 376;
         when C_BBD => return 052;
         when C_MGF => return 450;
         when C_ILS => return 376;
         when C_SLL => return 694;
         when C_MRO => return 478;
         when C_XAF => return 950;
         when C_XAG => return 961;
         when C_MRU => return 929;
         when C_USD => return 840;
         when C_SAR => return 682;
         when C_GBP => return 826;
         when C_LBP => return 422;
         when C_XAU => return 959;
         when C_TND => return 788;
         when C_CZK => return 203;
         when C_USN => return 997;
         when C_STD => return 678;
         when C_MZE => return 508;
         when C_ITL => return 380;
         when C_KHR => return 116;
         when C_NIC => return 558;
         when C_PHP => return 608;
         when C_USS => return 998;
         when C_CDF => return 976;
         when C_ECS => return 218;
         when C_LUC => return 989;
         when C_BUK => return 104;
         when C_MZM => return 508;
         when C_ECV => return 983;
         when C_MZN => return 943;
         when C_COP => return 170;
         when C_LUF => return 442;
         when C_STN => return 930;
         when C_NIO => return 558;
         when C_COU => return 970;
         when C_AED => return 784;
         when C_ROK => return 642;
         when C_LUL => return 988;
         when C_ROL => return 642;
         when C_MOP => return 446;
         when C_MDL => return 498;
         when C_RON => return 946;
         when C_XTS => return 963;
         when C_SIT => return 705;
         when C_IQD => return 368;
         when C_PEH => return 604;
         when C_PEI => return 604;
         when C_VUV => return 548;
         when C_BRB => return 076;
         when C_BRC => return 076;
         when C_CLF => return 990;
         when C_KPW => return 408;
         when C_PEN => return 604;
         when C_BRE => return 076;
         when C_KES => return 404;
         when C_CAD => return 124;
         when C_GRD => return 300;
         when C_PES => return 604;
         when C_RWF => return 646;
         when C_MWK => return 454;
         when C_MLF => return 466;
         when C_LRD => return 430;
         when C_BRL => return 986;
         when C_CLP => return 152;
         when C_AMD => return 051;
         when C_BRN => return 076;
         when C_MAD => return 504;
         when C_BGJ => return 100;
         when C_ESA => return 996;
         when C_BGK => return 100;
         when C_ESB => return 995;
         when C_BGL => return 100;
         when C_BRR => return 987;
         when C_BGN => return 975;
         when C_KMF => return 174;
         when C_ESP => return 724;
         when C_BZD => return 084;
         when C_THB => return 764;
         when C_BOB => return 068;
         when C_HTG => return 332;
         when C_ZMK => return 894;
         when C_AUD => return 036;
         when C_SYP => return 760;
         when C_INR => return 356;
         when C_MTL => return 470;
         when C_XCD => return 951;
         when C_MTP => return 470;
         when C_ZMW => return 967;
         when C_BOP => return 068;
         when C_FJD => return 242;
         when C_BOV => return 984;
         when C_SCR => return 690;
         when C_BDT => return 050;
         when C_TPE => return 626;
         when C_EEK => return 233;
         when C_SVC => return 222;
         when C_DKK => return 208;
         when C_GWE => return 624;
         when C_ARA => return 032;
         when C_JPY => return 392;
         when C_BAD => return 070;
         when C_BWP => return 072;
         when C_SKK => return 703;
         when C_FRF => return 250;
         when C_GWP => return 624;
         when C_ARP => return 032;
         when C_YER => return 886;
         when C_BAM => return 977;
         when C_ARS => return 032;
         when C_LAJ => return 418;
         when C_JMD => return 388;
         when C_LAK => return 418;
         when C_ARY => return 032;
         when C_PGK => return 598;
         when C_ISJ => return 352;
         when C_KRW => return 410;
         when C_ISK => return 352;
         when C_KGS => return 417;
         when C_QAR => return 634;
         when C_ZRN => return 180;
         when C_CYP => return 196;
         when C_TMM => return 795;
         when C_AOA => return 973;
         when C_OMR => return 512;
         when C_BIF => return 108;
         when C_UGS => return 800;
         when C_HNL => return 340;
         when C_BTN => return 064;
         when C_AZM => return 031;
         when C_SSP => return 728;
         when C_UGW => return 800;
         when C_TMT => return 934;
         when C_AZN => return 944;
         when C_MYR => return 458;
         when C_ZRZ => return 180;
         when C_UGX => return 800;
         when C_LTL => return 440;
         when C_AOK => return 024;
         when C_GTQ => return 320;
         when C_SHP => return 654;
         when C_CNY => return 156;
         when C_AON => return 024;
         when C_MNT => return 496;
         when C_XSU => return 994;
         when C_GIP => return 292;
         when C_AOR => return 982;
         when C_LTT => return 440;
         when C_ADP => return 020;
         when C_KZT => return 398;
         when C_CVE => return 132;
         when C_EUR => return 978;
         when C_YUD => return 890;
         when C_UZS => return 860;
         when C_HKD => return 344;
         when C_DEM => return 276;
         when C_YUM => return 891;
         when C_XPD => return 964;
         when C_YUN => return 890;
         when C_GQE => return 226;
         when C_MKD => return 807;
         when C_XPF => return 953;
         when C_AWG => return 533;
         when C_IEP => return 372;
         when C_NPR => return 524;
         when C_TJR => return 762;
         when C_KWD => return 414;
         when C_MVQ => return 462;
         when C_TJS => return 972;
         when C_MVR => return 462;
         when C_SEK => return 752;
         when C_ALK => return 008;
         when C_ALL => return 008;
         when C_XPT => return 962;
         when C_ZWC => return 716;
         when C_ZWD => return 716;
         when C_PAB => return 590;
         when C_ERN => return 232;
         when C_XEU => return 954;
         when C_CSD => return 891;
         when C_BYB => return 112;
         when C_ZWL => return 932;
         when C_PLN => return 985;
         when C_ZWN => return 942;
         when C_CHC => return 948;
         when C_CSJ => return 203;
         when C_EGP => return 818;
         when C_CSK => return 200;
         when C_UAH => return 980;
         when C_GYD => return 328;
         when C_CHE => return 947;
         when C_TRL => return 792;
         when C_CHF => return 756;
         when C_ZWR => return 935;
         when C_BND => return 096;
         when C_UAK => return 804;
         when C_LYD => return 434;
         when C_RSD => return 941;
         when C_BYN => return 933;
         when C_GNE => return 324;
         when C_PLZ => return 616;
         when C_ZAL => return 991;
         when C_GNF => return 324;
         when C_SBD => return 090;
         when C_XBA => return 955;
         when C_BYR => return 974;
         when C_XBB => return 956;
         when C_RHD => return 716;
         when C_XBC => return 957;
         when C_XBD => return 958;
         when C_ZAR => return 710;
         when C_TRY => return 949;
         when C_VNC => return 704;
         when C_PTE => return 620;
         when C_VND => return 704;
         when C_CHW => return 948;
         when C_XXX => return 999;
         when C_ATS => return 040;
         when C_GNS => return 324;
         when C_JOD => return 400;
         when C_FIM => return 246;
         when C_WST => return 882;
         when C_DJF => return 262;
         when C_XUA => return 965;
         when C_TZS => return 834;
         when C_YDD => return 720;
         when C_TOP => return 776;
         when C_AFA => return 004;
         when C_SUR => return 810;
         when C_LVL => return 428;
         when C_LVR => return 428;
         when C_AFN => return 971;
         when C_TWD => return 901;
         when C_LKR => return 144;
         when C_SRD => return 968;
         when C_BSD => return 044;
         when C_SRG => return 740;
         when C_SGD => return 702;
         when C_IRR => return 364;
         when C_BHD => return 048;
         when C_GHC => return 288;
         when C_MXN => return 484;
         when C_MXP => return 484;
         when C_KYD => return 136;
         when C_AYM => return 945;
         when C_MMK => return 104;
         when C_NGN => return 566;
         when C_ANG => return 532;
         when C_ETB => return 230;
         when C_LSL => return 426;
         when C_LSM => return 426;
         when C_MXV => return 979;
         when C_PYG => return 600;
         when C_DZD => return 012;
         when C_GHP => return 939;
         when C_UYI => return 940;
         when C_GHS => return 936;
         when C_CUC => return 931;
         when C_NZD => return 554;
         when C_TTD => return 780;
         when C_UYN => return 858;
         when C_UYP => return 858;
         when C_HUF => return 348;
         when C_UYU => return 858;
         when C_DOP => return 214;
         when C_RUB => return 643;
         when C_UYW => return 927;
         when C_SZL => return 748;
         when C_DDM => return 278;
         when C_CUP => return 192;
         when C_NOK => return 578;
         when C_BEC => return 993;
         when C_SDD => return 736;
         when C_XOF => return 952;
         when C_BEF => return 056;
         when C_SDG => return 938;
         when C_IDR => return 360;
         when C_MUR => return 480;
         when C_BEL => return 992;
         when C_SOS => return 706;
         when C_GEK => return 268;
         when C_RUR => return 810;
         when C_SDP => return 736;
         when C_GEL => return 981;
         when C_VEB => return 862;
         when C_VED => return 926;
         when C_VEF => return 937;
         when C_XDR => return 960;
         when C_FKP => return 238;
         when C_CRC => return 188;
         when C_HRD => return 191;
         when C_PKR => return 586;
         when others => return 0;
      end case;
   end Numeric;

   function Unit (This : Currency) return Minor_Unit is
   begin
      case This.Key is
         when C_BMD | C_VES | C_SLE | C_NAD | C_MGA | C_GMD | C_BBD | C_ILS | C_SLL | C_MRU | C_USD | C_SAR | C_GBP | C_LBP | C_CZK | C_USN | C_KHR | C_PHP | C_CDF | C_MZN | C_COP | C_STN | C_NIO | C_COU | C_AED | C_MOP | C_MDL | C_RON | C_KPW | C_PEN | C_KES | C_CAD | C_MWK | C_LRD | C_BRL | C_AMD | C_MAD | C_BGN | C_BZD | C_THB | C_BOB | C_HTG | C_AUD | C_SYP | C_INR | C_XCD | C_ZMW | C_FJD | C_BOV | C_SCR | C_BDT | C_SVC | C_DKK | C_BWP | C_YER | C_BAM | C_ARS | C_JMD | C_LAK | C_PGK | C_KGS | C_QAR | C_AOA | C_HNL | C_BTN | C_SSP | C_TMT | C_AZN | C_MYR | C_GTQ | C_SHP | C_CNY | C_MNT | C_GIP | C_KZT | C_CVE | C_EUR | C_UZS | C_HKD | C_MKD | C_AWG | C_NPR | C_TJS | C_MVR | C_SEK | C_ALL | C_PAB | C_ERN | C_ZWL | C_PLN | C_EGP | C_UAH | C_GYD | C_CHE | C_CHF | C_BND | C_RSD | C_BYN | C_SBD | C_ZAR | C_TRY | C_CHW | C_WST | C_TZS | C_TOP | C_AFN | C_TWD | C_LKR | C_SRD | C_BSD | C_SGD | C_IRR | C_MXN | C_KYD | C_MMK | C_NGN | C_ANG | C_ETB | C_LSL | C_MXV | C_DZD | C_GHS | C_CUC | C_NZD | C_TTD | C_HUF | C_UYU | C_DOP | C_RUB | C_SZL | C_CUP | C_NOK | C_SDG | C_IDR | C_MUR | C_SOS | C_GEL | C_VED | C_FKP | C_CRC | C_PKR => return 2;
         when C_TND | C_IQD | C_OMR | C_KWD | C_LYD | C_JOD | C_BHD => return 3;
         when C_CLF | C_UYW => return 4;
         when others => return 0;
      end case;
   end Unit;

   function Symbol (This : Currency) return Wide_Wide_String is
   begin
      case This.Key is
         when C_BMD => return "$";
         when C_HRK => return "kn";
         when C_NAD => return "$";
         when C_BBD => return "$";
         when C_ILS => return "₪";
         when C_USD => return "$";
         when C_SAR => return "﷼";
         when C_GBP => return "£";
         when C_LBP => return "£";
         when C_CZK => return "Kč";
         when C_KHR => return "៛";
         when C_PHP => return "₱";
         when C_MZN => return "MT";
         when C_COP => return "$";
         when C_NIO => return "C$";
         when C_AED => return "د.إ";
         when C_RON => return "lei";
         when C_KPW => return "₩";
         when C_PEN => return "S/.";
         when C_CAD => return "$";
         when C_LRD => return "$";
         when C_BRL => return "R$";
         when C_CLP => return "$";
         when C_BGN => return "лв";
         when C_BZD => return "BZ$";
         when C_THB => return "฿";
         when C_BOB => return "$b";
         when C_AUD => return "$";
         when C_SYP => return "£";
         when C_INR => return "₹";
         when C_XCD => return "$";
         when C_FJD => return "$";
         when C_SCR => return "₨";
         when C_SVC => return "$";
         when C_DKK => return "kr";
         when C_JPY => return "¥";
         when C_BWP => return "P";
         when C_YER => return "﷼";
         when C_BAM => return "KM";
         when C_ARS => return "$";
         when C_JMD => return "J$";
         when C_LAK => return "₭";
         when C_KRW => return "₩";
         when C_ISK => return "kr";
         when C_KGS => return "лв";
         when C_QAR => return "﷼";
         when C_OMR => return "﷼";
         when C_HNL => return "L";
         when C_AZN => return "₼";
         when C_MYR => return "RM";
         when C_GTQ => return "Q";
         when C_SHP => return "£";
         when C_CNY => return "¥";
         when C_MNT => return "د.إ";
         when C_GIP => return "£";
         when C_KZT => return "лв";
         when C_EUR => return "€";
         when C_UZS => return "лв";
         when C_HKD => return "$";
         when C_MKD => return "ден";
         when C_AWG => return "ƒ";
         when C_NPR => return "₨";
         when C_SEK => return "kr";
         when C_ALL => return "Lek";
         when C_ZWD => return "Z$";
         when C_PAB => return "B/.";
         when C_PLN => return "zł";
         when C_EGP => return "£";
         when C_UAH => return "₴";
         when C_GYD => return "$";
         when C_CHF => return "CHF";
         when C_BND => return "$";
         when C_RSD => return "Дин.";
         when C_BYN => return "Br";
         when C_SBD => return "$";
         when C_ZAR => return "R";
         when C_TRY => return "₺";
         when C_VND => return "₫";
         when C_AFN => return "؋";
         when C_TWD => return "NT$";
         when C_LKR => return "₨";
         when C_SRD => return "$";
         when C_BSD => return "$";
         when C_SGD => return "$";
         when C_IRR => return "﷼";
         when C_MXN => return "$";
         when C_KYD => return "$";
         when C_NGN => return "₦";
         when C_ANG => return "ƒ";
         when C_PYG => return "Gs";
         when C_GHS => return "¢";
         when C_NZD => return "$";
         when C_TTD => return "TT$";
         when C_HUF => return "Ft";
         when C_UYU => return "$U";
         when C_DOP => return "RD$";
         when C_RUB => return "₽";
         when C_CUP => return "₱";
         when C_NOK => return "kr";
         when C_IDR => return "Rp";
         when C_MUR => return "₨";
         when C_SOS => return "S";
         when C_VEF => return "Bs";
         when C_FKP => return "£";
         when C_CRC => return "₡";
         when C_PKR => return "₨";
         when others => return "¤";
      end case;
   end Symbol;

   function Symbol (This : Currency) return String is
      use Ada.Characters.Conversions;
   begin
      return To_String (This.Symbol);
   end Symbol;

   function Is_Fund (This : Currency) return Boolean is
   begin
      case This.Key is
         when C_USN | C_COU | C_CLF | C_XFU | C_BOV | C_CHE | C_CHW | C_XRE | C_MXV | C_UYI => return True;
         when others => return False;
      end case;
   end Is_Fund;

   function Is_Historic (This : Currency) return Boolean is
   begin
      case This.Key is
         when C_NLG | C_HRK | C_ILP | C_ILR | C_MGF | C_MRO | C_STD | C_MZE | C_ITL | C_NIC | C_USS | C_ECS | C_LUC | C_BUK | C_MZM | C_ECV | C_LUF | C_ROK | C_LUL | C_ROL | C_RON | C_SIT | C_PEH | C_PEI | C_BRB | C_BRC | C_PEN | C_BRE | C_GRD | C_PES | C_MWK | C_MLF | C_BRN | C_BGJ | C_ESA | C_BGK | C_ESB | C_BGL | C_BRR | C_XFO | C_XFU | C_ESP | C_ZMK | C_MTL | C_MTP | C_BOP | C_TPE | C_EEK | C_GWE | C_ARA | C_BAD | C_SKK | C_FRF | C_GWP | C_ARP | C_LAJ | C_ARY | C_ISJ | C_ZRN | C_CYP | C_TMM | C_UGS | C_AZM | C_UGW | C_ZRZ | C_LTL | C_AOK | C_AON | C_AOR | C_LTT | C_ADP | C_EUR | C_YUD | C_DEM | C_YUM | C_YUN | C_GQE | C_IEP | C_TJR | C_MVQ | C_ALK | C_ZWC | C_ZWD | C_XEU | C_CSD | C_BYB | C_ZWN | C_CHC | C_CSJ | C_CSK | C_TRL | C_ZWR | C_UAK | C_GNE | C_PLZ | C_ZAL | C_BYR | C_RHD | C_TRY | C_VNC | C_PTE | C_ATS | C_GNS | C_FIM | C_YDD | C_AFA | C_SUR | C_LVL | C_LVR | C_SRG | C_XRE | C_GHC | C_MXP | C_AYM | C_ANG | C_LSM | C_GHP | C_UYN | C_UYP | C_SZL | C_DDM | C_BEC | C_SDD | C_BEF | C_SDG | C_IDR | C_BEL | C_GEK | C_RUR | C_SDP | C_VEB | C_VEF | C_HRD => return True;
         when others => return False;
      end case;
   end Is_Historic;

   function Withdraw_Date (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "2002-03";
         when C_HRK => return "2023-01";
         when C_ILP => return "1978 to 1981";
         when C_ILR => return "1989 to 1990";
         when C_MGF => return "2004-12";
         when C_MRO => return "2017-12";
         when C_STD => return "2017-12";
         when C_MZE => return "1978 to 1981";
         when C_ITL => return "2002-03";
         when C_NIC => return "1990-10";
         when C_USS => return "2014-03";
         when C_ECS => return "2000-09";
         when C_LUC => return "1990-03";
         when C_BUK => return "1990-02";
         when C_MZM => return "2006-06";
         when C_ECV => return "2000-09";
         when C_LUF => return "2002-03";
         when C_ROK => return "1989 to 1990";
         when C_LUL => return "1990-03";
         when C_ROL => return "2005-06";
         when C_RON => return "2015-06";
         when C_SIT => return "2007-01";
         when C_PEH => return "1989 to 1990";
         when C_PEI => return "1991-07";
         when C_BRB => return "1986-03";
         when C_BRC => return "1989-02";
         when C_PEN => return "2015-12";
         when C_BRE => return "1993-03";
         when C_GRD => return "2002-03";
         when C_PES => return "1986-02";
         when C_MWK => return "2016-02";
         when C_MLF => return "1984-11";
         when C_BRN => return "1990-03";
         when C_BGJ => return "1989 to 1990";
         when C_ESA => return "1978 to 1981";
         when C_BGK => return "1989 to 1990";
         when C_ESB => return "1994-12";
         when C_BGL => return "2003-11";
         when C_BRR => return "1994-07";
         when C_XFO => return "2006-10";
         when C_XFU => return "2013-11";
         when C_ESP => return "2002-03";
         when C_ZMK => return "2012-12";
         when C_MTL => return "2008-01";
         when C_MTP => return "1983-06";
         when C_BOP => return "1987-02";
         when C_TPE => return "2002-11";
         when C_EEK => return "2011-01";
         when C_GWE => return "1978 to 1981";
         when C_ARA => return "1992-01";
         when C_BAD => return "1998-07";
         when C_SKK => return "2009-01";
         when C_FRF => return "1999-01";
         when C_GWP => return "1997-05";
         when C_ARP => return "1985-07";
         when C_LAJ => return "1979-12";
         when C_ARY => return "1989 to 1990";
         when C_ISJ => return "1989 to 1990";
         when C_ZRN => return "1999-06";
         when C_CYP => return "2008-01";
         when C_TMM => return "2009-01";
         when C_UGS => return "1987-05";
         when C_AZM => return "2005-12";
         when C_UGW => return "1989 to 1990";
         when C_ZRZ => return "1994-02";
         when C_LTL => return "2014-12";
         when C_AOK => return "1991-03";
         when C_AON => return "2000-02";
         when C_AOR => return "2000-02";
         when C_LTT => return "1993-07";
         when C_ADP => return "2003-07";
         when C_EUR => return "2006-10";
         when C_YUD => return "1990-01";
         when C_DEM => return "2002-03";
         when C_YUM => return "2003-07";
         when C_YUN => return "1995-11";
         when C_GQE => return "1986-06";
         when C_IEP => return "2002-03";
         when C_TJR => return "2001-04";
         when C_MVQ => return "1989-12";
         when C_ALK => return "1989-12";
         when C_ZWC => return "1989-12";
         when C_ZWD => return "2008-08";
         when C_XEU => return "1999-01";
         when C_CSD => return "2006-10";
         when C_BYB => return "2001-01";
         when C_ZWN => return "2006-09";
         when C_CHC => return "2004-11";
         when C_CSJ => return "1989 to 1990";
         when C_CSK => return "1993-03";
         when C_TRL => return "2005-12";
         when C_ZWR => return "2009-06";
         when C_UAK => return "1996-09";
         when C_GNE => return "1989-12";
         when C_PLZ => return "1997-01";
         when C_ZAL => return "1995-03";
         when C_BYR => return "2017-01";
         when C_RHD => return "1978 to 1981";
         when C_TRY => return "2009-01";
         when C_VNC => return "1989-1990";
         when C_PTE => return "2002-03";
         when C_ATS => return "2002-03";
         when C_GNS => return "1986-02";
         when C_FIM => return "2002-03";
         when C_YDD => return "1991-09";
         when C_AFA => return "2003-01";
         when C_SUR => return "1990-12";
         when C_LVL => return "2014-01";
         when C_LVR => return "1994-12";
         when C_SRG => return "2003-12";
         when C_XRE => return "1999-11";
         when C_GHC => return "2008-01";
         when C_MXP => return "1993-01";
         when C_AYM => return "2005-10";
         when C_ANG => return "2010-10";
         when C_LSM => return "1985-05";
         when C_GHP => return "2007-06";
         when C_UYN => return "1989-12";
         when C_UYP => return "1993-03";
         when C_SZL => return "2018-08";
         when C_DDM => return "1990-07 to 1990-09";
         when C_BEC => return "1990-03";
         when C_SDD => return "2007-07";
         when C_BEF => return "2002-03";
         when C_SDG => return "2012-09";
         when C_IDR => return "2002-07";
         when C_BEL => return "1990-03";
         when C_GEK => return "1995-10";
         when C_RUR => return "1994-07";
         when C_SDP => return "1998-06";
         when C_VEB => return "2008-01";
         when C_VEF => return "2018-08";
         when C_HRD => return "1995-01";
         when others => return "";
      end case;
   end Withdraw_Date;

   function Withdraw_Dates (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "2002-03";
         when C_HRK => return "2015-06;2023-01";
         when C_ILP => return "1978 to 1981";
         when C_ILR => return "1989 to 1990";
         when C_MGF => return "2004-12";
         when C_MRO => return "2017-12";
         when C_STD => return "2017-12";
         when C_MZE => return "1978 to 1981";
         when C_ITL => return "2002-03;2002-03;2002-03";
         when C_NIC => return "1990-10";
         when C_USS => return "2014-03";
         when C_ECS => return "2000-09";
         when C_LUC => return "1990-03";
         when C_BUK => return "1990-02";
         when C_MZM => return "2006-06";
         when C_ECV => return "2000-09";
         when C_LUF => return "2002-03";
         when C_ROK => return "1989 to 1990";
         when C_LUL => return "1990-03";
         when C_ROL => return "2005-06";
         when C_RON => return "2015-06";
         when C_SIT => return "2007-01";
         when C_PEH => return "1989 to 1990";
         when C_PEI => return "1991-07";
         when C_BRB => return "1986-03";
         when C_BRC => return "1989-02";
         when C_PEN => return "2015-12";
         when C_BRE => return "1993-03";
         when C_GRD => return "2002-03";
         when C_PES => return "1986-02";
         when C_MWK => return "2016-02";
         when C_MLF => return "1984-11";
         when C_BRN => return "1990-03";
         when C_BGJ => return "1989 to 1990";
         when C_ESA => return "1978 to 1981";
         when C_BGK => return "1989 to 1990";
         when C_ESB => return "1994-12";
         when C_BGL => return "2003-11";
         when C_BRR => return "1994-07";
         when C_XFO => return "2006-10";
         when C_XFU => return "2013-11";
         when C_ESP => return "2002-03;2002-03";
         when C_ZMK => return "2012-12";
         when C_MTL => return "2008-01";
         when C_MTP => return "1983-06";
         when C_BOP => return "1987-02";
         when C_TPE => return "2002-11";
         when C_EEK => return "2011-01";
         when C_GWE => return "1978 to 1981";
         when C_ARA => return "1992-01";
         when C_BAD => return "1998-07";
         when C_SKK => return "2009-01";
         when C_FRF => return "2002-03;2002-03;2002-03;2002-03;2002-03;2002-03;2002-03;2002-03;2002-03;1999-01;2002-03;1999-01";
         when C_GWP => return "1997-05";
         when C_ARP => return "1985-07";
         when C_LAJ => return "1979-12";
         when C_ARY => return "1989 to 1990";
         when C_ISJ => return "1989 to 1990";
         when C_ZRN => return "1999-06";
         when C_CYP => return "2008-01";
         when C_TMM => return "2009-01";
         when C_UGS => return "1987-05";
         when C_AZM => return "2005-12";
         when C_UGW => return "1989 to 1990";
         when C_ZRZ => return "1994-02";
         when C_LTL => return "2014-12";
         when C_AOK => return "1991-03";
         when C_AON => return "2000-02";
         when C_AOR => return "2000-02";
         when C_LTT => return "1993-07";
         when C_ADP => return "2003-07";
         when C_EUR => return "2006-10";
         when C_YUD => return "1990-01";
         when C_DEM => return "2002-03";
         when C_YUM => return "2003-07";
         when C_YUN => return "1995-11";
         when C_GQE => return "1986-06";
         when C_IEP => return "2002-03";
         when C_TJR => return "2001-04";
         when C_MVQ => return "1989-12";
         when C_ALK => return "1989-12";
         when C_ZWC => return "1989-12";
         when C_ZWD => return "2006-08;2008-08";
         when C_XEU => return "1999-01";
         when C_CSD => return "2006-10";
         when C_BYB => return "2001-01";
         when C_ZWN => return "2006-09";
         when C_CHC => return "2004-11";
         when C_CSJ => return "1989 to 1990";
         when C_CSK => return "1993-03";
         when C_TRL => return "2005-12";
         when C_ZWR => return "2009-06";
         when C_UAK => return "1996-09";
         when C_GNE => return "1989-12";
         when C_PLZ => return "1997-01";
         when C_ZAL => return "1995-03;1995-03";
         when C_BYR => return "2017-01";
         when C_RHD => return "1978 to 1981";
         when C_TRY => return "2009-01";
         when C_VNC => return "1989-1990";
         when C_PTE => return "2002-03";
         when C_ATS => return "2002-03";
         when C_GNS => return "1986-02";
         when C_FIM => return "2002-03;2002-03";
         when C_YDD => return "1991-09";
         when C_AFA => return "2003-01";
         when C_SUR => return "1990-12";
         when C_LVL => return "2014-01";
         when C_LVR => return "1994-12";
         when C_SRG => return "2003-12";
         when C_XRE => return "1999-11";
         when C_GHC => return "2008-01";
         when C_MXP => return "1993-01";
         when C_AYM => return "2005-10";
         when C_ANG => return "2010-10";
         when C_LSM => return "1985-05";
         when C_GHP => return "2007-06";
         when C_UYN => return "1989-12";
         when C_UYP => return "1993-03";
         when C_SZL => return "2018-08";
         when C_DDM => return "1990-07 to 1990-09";
         when C_BEC => return "1990-03";
         when C_SDD => return "2007-07";
         when C_BEF => return "2002-03";
         when C_SDG => return "2012-09";
         when C_IDR => return "2002-07";
         when C_BEL => return "1990-03";
         when C_GEK => return "1995-10";
         when C_RUR => return "1994-08;1994-08;1994-06;1994-04;1994-05;1993-01;1993-12;2004-01;1995-05;1993-10;1994-07";
         when C_SDP => return "1998-06";
         when C_VEB => return "2008-01";
         when C_VEF => return "2011-12;2016-02;2018-08";
         when C_HRD => return "1995-01";
         when others => return "";
      end case;
   end Withdraw_Dates;

   function Historic_Names (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "Netherlands Guilder";
         when C_HRK => return "Croatian Kuna;Kuna";
         when C_ILP => return "Pound";
         when C_ILR => return "Old Shekel";
         when C_MGF => return "Malagasy Franc";
         when C_MRO => return "Ouguiya";
         when C_STD => return "Dobra";
         when C_MZE => return "Mozambique Escudo";
         when C_ITL => return "Italian Lira";
         when C_NIC => return "Cordoba";
         when C_USS => return "US Dollar (Same day)";
         when C_ECS => return "Sucre";
         when C_LUC => return "Luxembourg Convertible Franc";
         when C_BUK => return "Kyat";
         when C_MZM => return "Mozambique Metical";
         when C_ECV => return "Unidad de Valor Constante (UVC)";
         when C_LUF => return "Luxembourg Franc";
         when C_ROK => return "Leu A/52";
         when C_LUL => return "Luxembourg Financial Franc";
         when C_ROL => return "Old Leu";
         when C_RON => return "New Romanian Leu ";
         when C_SIT => return "Tolar";
         when C_PEH => return "Sol";
         when C_PEI => return "Inti";
         when C_BRB => return "Cruzeiro";
         when C_BRC => return "Cruzado";
         when C_PEN => return "Nuevo Sol ";
         when C_BRE => return "Cruzeiro";
         when C_GRD => return "Drachma";
         when C_PES => return "Sol";
         when C_MWK => return "Kwacha";
         when C_MLF => return "Mali Franc";
         when C_BRN => return "New Cruzado";
         when C_BGJ => return "Lev A/52";
         when C_ESA => return "Spanish Peseta";
         when C_BGK => return "Lev A/62";
         when C_ESB => return """A"" Account (convertible Peseta Account)";
         when C_BGL => return "Lev";
         when C_BRR => return "Cruzeiro Real";
         when C_XFO => return "Gold-Franc";
         when C_XFU => return "UIC-Franc";
         when C_ESP => return "Spanish Peseta";
         when C_ZMK => return "Zambian Kwacha";
         when C_MTL => return "Maltese Lira";
         when C_MTP => return "Maltese Pound";
         when C_BOP => return "Peso boliviano";
         when C_TPE => return "Timor Escudo";
         when C_EEK => return "Kroon";
         when C_GWE => return "Guinea Escudo";
         when C_ARA => return "Austral";
         when C_BAD => return "Dinar";
         when C_SKK => return "Slovak Koruna";
         when C_FRF => return "French Franc";
         when C_GWP => return "Guinea-Bissau Peso";
         when C_ARP => return "Peso Argentino";
         when C_LAJ => return "Pathet Lao Kip";
         when C_ARY => return "Peso";
         when C_ISJ => return "Old Krona";
         when C_ZRN => return "New Zaire";
         when C_CYP => return "Cyprus Pound";
         when C_TMM => return "Turkmenistan Manat";
         when C_UGS => return "Uganda Shilling";
         when C_AZM => return "Azerbaijanian Manat";
         when C_UGW => return "Old Shilling";
         when C_ZRZ => return "Zaire";
         when C_LTL => return "Lithuanian Litas";
         when C_AOK => return "Kwanza";
         when C_AON => return "New Kwanza";
         when C_AOR => return "Kwanza Reajustado";
         when C_LTT => return "Talonas";
         when C_ADP => return "Andorran Peseta";
         when C_EUR => return "Euro";
         when C_YUD => return "New Yugoslavian Dinar";
         when C_DEM => return "Deutsche Mark";
         when C_YUM => return "New Dinar";
         when C_YUN => return "Yugoslavian Dinar";
         when C_GQE => return "Ekwele";
         when C_IEP => return "Irish Pound";
         when C_TJR => return "Tajik Ruble";
         when C_MVQ => return "Maldive Rupee";
         when C_ALK => return "Old Lek";
         when C_ZWC => return "Rhodesian Dollar";
         when C_ZWD => return "Zimbabwe Dollar (old);Zimbabwe Dollar";
         when C_XEU => return "European Currency Unit (E.C.U)";
         when C_CSD => return "Serbian Dinar";
         when C_BYB => return "Belarusian Ruble";
         when C_ZWN => return "Zimbabwe Dollar (new)";
         when C_CHC => return "WIR Franc (for electronic)";
         when C_CSJ => return "Krona A/53";
         when C_CSK => return "Koruna";
         when C_TRL => return "Old Turkish Lira";
         when C_ZWR => return "Zimbabwe Dollar";
         when C_UAK => return "Karbovanet";
         when C_GNE => return "Syli";
         when C_PLZ => return "Zloty";
         when C_ZAL => return "Financial Rand";
         when C_BYR => return "Belarusian Ruble";
         when C_RHD => return "Rhodesian Dollar";
         when C_TRY => return "New Turkish Lira";
         when C_VNC => return "Old Dong";
         when C_PTE => return "Portuguese Escudo";
         when C_ATS => return "Schilling";
         when C_GNS => return "Syli";
         when C_FIM => return "Markka";
         when C_YDD => return "Yemeni Dinar";
         when C_AFA => return "Afghani";
         when C_SUR => return "Rouble";
         when C_LVL => return "Latvian Lats";
         when C_LVR => return "Latvian Ruble";
         when C_SRG => return "Surinam Guilder";
         when C_XRE => return "RINET Funds Code";
         when C_GHC => return "Cedi";
         when C_MXP => return "Mexican Peso";
         when C_AYM => return "Azerbaijan Manat";
         when C_ANG => return "Netherlands Antillean Guilder";
         when C_LSM => return "Loti";
         when C_GHP => return "Ghana Cedi";
         when C_UYN => return "Old Uruguay Peso";
         when C_UYP => return "Uruguayan Peso";
         when C_SZL => return "Lilangeni";
         when C_DDM => return "Mark der DDR";
         when C_BEC => return "Convertible Franc";
         when C_SDD => return "Sudanese Dinar";
         when C_BEF => return "Belgian Franc";
         when C_SDG => return "Sudanese Pound";
         when C_IDR => return "Rupiah";
         when C_BEL => return "Financial Franc";
         when C_GEK => return "Georgian Coupon";
         when C_RUR => return "Russian Ruble";
         when C_SDP => return "Sudanese Pound";
         when C_VEB => return "Bolivar";
         when C_VEF => return "Bolivar Fuerte;Bolivar;Bolívar";
         when C_HRD => return "Croatian Dinar";
         when others => return "";
      end case;
   end Historic_Names;

   function Historic_Numerics (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "528";
         when C_HRK => return "191";
         when C_ILP => return "376";
         when C_ILR => return "376";
         when C_MGF => return "450";
         when C_MRO => return "478";
         when C_STD => return "678";
         when C_MZE => return "508";
         when C_ITL => return "380";
         when C_NIC => return "558";
         when C_USS => return "998";
         when C_ECS => return "218";
         when C_LUC => return "989";
         when C_BUK => return "104";
         when C_MZM => return "508";
         when C_ECV => return "983";
         when C_LUF => return "442";
         when C_ROK => return "642";
         when C_LUL => return "988";
         when C_ROL => return "642";
         when C_RON => return "946";
         when C_SIT => return "705";
         when C_PEH => return "604";
         when C_PEI => return "604";
         when C_BRB => return "076";
         when C_BRC => return "076";
         when C_PEN => return "604";
         when C_BRE => return "076";
         when C_GRD => return "300";
         when C_PES => return "604";
         when C_MWK => return "454";
         when C_MLF => return "466";
         when C_BRN => return "076";
         when C_BGJ => return "100";
         when C_ESA => return "996";
         when C_BGK => return "100";
         when C_ESB => return "995";
         when C_BGL => return "100";
         when C_BRR => return "987";
         when C_ESP => return "724";
         when C_ZMK => return "894";
         when C_MTL => return "470";
         when C_MTP => return "470";
         when C_BOP => return "068";
         when C_TPE => return "626";
         when C_EEK => return "233";
         when C_GWE => return "624";
         when C_ARA => return "032";
         when C_BAD => return "070";
         when C_SKK => return "703";
         when C_FRF => return "250";
         when C_GWP => return "624";
         when C_ARP => return "032";
         when C_LAJ => return "418";
         when C_ARY => return "032";
         when C_ISJ => return "352";
         when C_ZRN => return "180";
         when C_CYP => return "196";
         when C_TMM => return "795";
         when C_UGS => return "800";
         when C_AZM => return "031";
         when C_UGW => return "800";
         when C_ZRZ => return "180";
         when C_LTL => return "440";
         when C_AOK => return "024";
         when C_AON => return "024";
         when C_AOR => return "982";
         when C_LTT => return "440";
         when C_ADP => return "020";
         when C_EUR => return "978";
         when C_YUD => return "890";
         when C_DEM => return "276";
         when C_YUM => return "891";
         when C_YUN => return "890";
         when C_GQE => return "226";
         when C_IEP => return "372";
         when C_TJR => return "762";
         when C_MVQ => return "462";
         when C_ALK => return "008";
         when C_ZWC => return "716";
         when C_ZWD => return "716";
         when C_XEU => return "954";
         when C_CSD => return "891";
         when C_BYB => return "112";
         when C_ZWN => return "942";
         when C_CHC => return "948";
         when C_CSJ => return "203";
         when C_CSK => return "200";
         when C_TRL => return "792";
         when C_ZWR => return "935";
         when C_UAK => return "804";
         when C_GNE => return "324";
         when C_PLZ => return "616";
         when C_ZAL => return "991";
         when C_BYR => return "974";
         when C_RHD => return "716";
         when C_TRY => return "949";
         when C_VNC => return "704";
         when C_PTE => return "620";
         when C_ATS => return "040";
         when C_GNS => return "324";
         when C_FIM => return "246";
         when C_YDD => return "720";
         when C_AFA => return "004";
         when C_SUR => return "810";
         when C_LVL => return "428";
         when C_LVR => return "428";
         when C_SRG => return "740";
         when C_GHC => return "288";
         when C_MXP => return "484";
         when C_AYM => return "945";
         when C_ANG => return "532";
         when C_LSM => return "426";
         when C_GHP => return "939";
         when C_UYN => return "858";
         when C_UYP => return "858";
         when C_SZL => return "748";
         when C_DDM => return "278";
         when C_BEC => return "993";
         when C_SDD => return "736";
         when C_BEF => return "056";
         when C_SDG => return "938";
         when C_IDR => return "360";
         when C_BEL => return "992";
         when C_GEK => return "268";
         when C_RUR => return "810";
         when C_SDP => return "736";
         when C_VEB => return "862";
         when C_VEF => return "937";
         when C_HRD => return "191";
         when others => return "";
      end case;
   end Historic_Numerics;

   function Historic_Entities (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "NETHERLANDS";
         when C_HRK => return "CROATIA";
         when C_ILP => return "ISRAEL";
         when C_ILR => return "ISRAEL";
         when C_MGF => return "MADAGASCAR";
         when C_MRO => return "MAURITANIA";
         when C_STD => return "SAO TOME AND PRINCIPE";
         when C_MZE => return "MOZAMBIQUE";
         when C_ITL => return "HOLY SEE (VATICAN CITY STATE);ITALY;SAN MARINO";
         when C_NIC => return "NICARAGUA";
         when C_USS => return "UNITED STATES";
         when C_ECS => return "ECUADOR";
         when C_LUC => return "LUXEMBOURG";
         when C_BUK => return "BURMA ";
         when C_MZM => return "MOZAMBIQUE";
         when C_ECV => return "ECUADOR";
         when C_LUF => return "LUXEMBOURG";
         when C_ROK => return "ROMANIA";
         when C_LUL => return "LUXEMBOURG";
         when C_ROL => return "ROMANIA";
         when C_RON => return "ROMANIA";
         when C_SIT => return "SLOVENIA";
         when C_PEH => return "PERU";
         when C_PEI => return "PERU";
         when C_BRB => return "BRAZIL";
         when C_BRC => return "BRAZIL";
         when C_PEN => return "PERU";
         when C_BRE => return "BRAZIL";
         when C_GRD => return "GREECE";
         when C_PES => return "PERU";
         when C_MWK => return "MALAWI";
         when C_MLF => return "MALI";
         when C_BRN => return "BRAZIL";
         when C_BGJ => return "BULGARIA";
         when C_ESA => return "SPAIN";
         when C_BGK => return "BULGARIA";
         when C_ESB => return "SPAIN";
         when C_BGL => return "BULGARIA";
         when C_BRR => return "BRAZIL";
         when C_XFO => return "ZZ01_Gold-Franc";
         when C_XFU => return "ZZ05_UIC-Franc";
         when C_ESP => return "ANDORRA;SPAIN";
         when C_ZMK => return "ZAMBIA";
         when C_MTL => return "MALTA";
         when C_MTP => return "MALTA";
         when C_BOP => return "BOLIVIA";
         when C_TPE => return "TIMOR-LESTE";
         when C_EEK => return "ESTONIA";
         when C_GWE => return "GUINEA-BISSAU";
         when C_ARA => return "ARGENTINA";
         when C_BAD => return "BOSNIA AND HERZEGOVINA";
         when C_SKK => return "SLOVAKIA";
         when C_FRF => return "ANDORRA;FRANCE;FRENCH  GUIANA;FRENCH SOUTHERN TERRITORIES;GUADELOUPE;MARTINIQUE;MAYOTTE;MONACO;RÉUNION;SAINT MARTIN;SAINT PIERRE AND MIQUELON;SAINT-BARTHÉLEMY";
         when C_GWP => return "GUINEA-BISSAU";
         when C_ARP => return "ARGENTINA";
         when C_LAJ => return "LAO";
         when C_ARY => return "ARGENTINA";
         when C_ISJ => return "ICELAND";
         when C_ZRN => return "ZAIRE";
         when C_CYP => return "CYPRUS";
         when C_TMM => return "TURKMENISTAN";
         when C_UGS => return "UGANDA";
         when C_AZM => return "AZERBAIJAN";
         when C_UGW => return "UGANDA";
         when C_ZRZ => return "ZAIRE";
         when C_LTL => return "LITHUANIA";
         when C_AOK => return "ANGOLA";
         when C_AON => return "ANGOLA";
         when C_AOR => return "ANGOLA";
         when C_LTT => return "LITHUANIA";
         when C_ADP => return "ANDORRA";
         when C_EUR => return "SERBIA AND MONTENEGRO";
         when C_YUD => return "YUGOSLAVIA";
         when C_DEM => return "GERMANY";
         when C_YUM => return "YUGOSLAVIA";
         when C_YUN => return "YUGOSLAVIA";
         when C_GQE => return "EQUATORIAL GUINEA";
         when C_IEP => return "IRELAND";
         when C_TJR => return "TAJIKISTAN";
         when C_MVQ => return "MALDIVES";
         when C_ALK => return "ALBANIA";
         when C_ZWC => return "ZIMBABWE";
         when C_ZWD => return "ZIMBABWE";
         when C_XEU => return "EUROPEAN MONETARY CO-OPERATION FUND (EMCF)";
         when C_CSD => return "SERBIA AND MONTENEGRO";
         when C_BYB => return "BELARUS";
         when C_ZWN => return "ZIMBABWE";
         when C_CHC => return "SWITZERLAND";
         when C_CSJ => return "CZECHOSLOVAKIA";
         when C_CSK => return "CZECHOSLOVAKIA";
         when C_TRL => return "TURKEY";
         when C_ZWR => return "ZIMBABWE";
         when C_UAK => return "UKRAINE";
         when C_GNE => return "GUINEA";
         when C_PLZ => return "POLAND";
         when C_ZAL => return "LESOTHO;SOUTH AFRICA";
         when C_BYR => return "BELARUS";
         when C_RHD => return "SOUTHERN RHODESIA ";
         when C_TRY => return "TURKEY";
         when C_VNC => return "VIETNAM";
         when C_PTE => return "PORTUGAL";
         when C_ATS => return "AUSTRIA";
         when C_GNS => return "GUINEA";
         when C_FIM => return "ÅLAND ISLANDS;FINLAND";
         when C_YDD => return "YEMEN, DEMOCRATIC";
         when C_AFA => return "AFGHANISTAN";
         when C_SUR => return "UNION OF SOVIET SOCIALIST REPUBLICS";
         when C_LVL => return "LATVIA";
         when C_LVR => return "LATVIA";
         when C_SRG => return "SURINAME";
         when C_XRE => return "ZZ02_RINET Funds Code";
         when C_GHC => return "GHANA";
         when C_MXP => return "MEXICO";
         when C_AYM => return "AZERBAIJAN";
         when C_ANG => return "NETHERLANDS ANTILLES";
         when C_LSM => return "LESOTHO";
         when C_GHP => return "GHANA";
         when C_UYN => return "URUGUAY";
         when C_UYP => return "URUGUAY";
         when C_SZL => return "SWAZILAND";
         when C_DDM => return "GERMAN DEMOCRATIC REPUBLIC";
         when C_BEC => return "BELGIUM";
         when C_SDD => return "SUDAN";
         when C_BEF => return "BELGIUM";
         when C_SDG => return "SOUTH SUDAN";
         when C_IDR => return "TIMOR-LESTE";
         when C_BEL => return "BELGIUM";
         when C_GEK => return "GEORGIA";
         when C_RUR => return "ARMENIA;AZERBAIJAN;BELARUS;GEORGIA;KAZAKHSTAN;KYRGYZSTAN;MOLDOVA, REPUBLIC OF;RUSSIAN FEDERATION;TAJIKISTAN;TURKMENISTAN;UZBEKISTAN";
         when C_SDP => return "SUDAN";
         when C_VEB => return "VENEZUELA";
         when C_VEF => return "VENEZUELA;VENEZUELA (BOLIVARIAN REPUBLIC OF)";
         when C_HRD => return "CROATIA";
         when others => return "";
      end case;
   end Historic_Entities;

   function Historic_Records (This : Currency) return String is
   begin
      case This.Key is
         when C_NLG => return "NETHERLANDS:Netherlands Guilder:528:2002-03";
         when C_HRK => return "CROATIA:Croatian Kuna:191:2015-06;CROATIA:Kuna:191:2023-01";
         when C_ILP => return "ISRAEL:Pound:376:1978 to 1981";
         when C_ILR => return "ISRAEL:Old Shekel:376:1989 to 1990";
         when C_MGF => return "MADAGASCAR:Malagasy Franc:450:2004-12";
         when C_MRO => return "MAURITANIA:Ouguiya:478:2017-12";
         when C_STD => return "SAO TOME AND PRINCIPE:Dobra:678:2017-12";
         when C_MZE => return "MOZAMBIQUE:Mozambique Escudo:508:1978 to 1981";
         when C_ITL => return "HOLY SEE (VATICAN CITY STATE):Italian Lira:380:2002-03;ITALY:Italian Lira:380:2002-03;SAN MARINO:Italian Lira:380:2002-03";
         when C_NIC => return "NICARAGUA:Cordoba:558:1990-10";
         when C_USS => return "UNITED STATES:US Dollar (Same day):998:2014-03";
         when C_ECS => return "ECUADOR:Sucre:218:2000-09";
         when C_LUC => return "LUXEMBOURG:Luxembourg Convertible Franc:989:1990-03";
         when C_BUK => return "BURMA :Kyat:104:1990-02";
         when C_MZM => return "MOZAMBIQUE:Mozambique Metical:508:2006-06";
         when C_ECV => return "ECUADOR:Unidad de Valor Constante (UVC):983:2000-09";
         when C_LUF => return "LUXEMBOURG:Luxembourg Franc:442:2002-03";
         when C_ROK => return "ROMANIA:Leu A/52:642:1989 to 1990";
         when C_LUL => return "LUXEMBOURG:Luxembourg Financial Franc:988:1990-03";
         when C_ROL => return "ROMANIA:Old Leu:642:2005-06";
         when C_RON => return "ROMANIA:New Romanian Leu :946:2015-06";
         when C_SIT => return "SLOVENIA:Tolar:705:2007-01";
         when C_PEH => return "PERU:Sol:604:1989 to 1990";
         when C_PEI => return "PERU:Inti:604:1991-07";
         when C_BRB => return "BRAZIL:Cruzeiro:076:1986-03";
         when C_BRC => return "BRAZIL:Cruzado:076:1989-02";
         when C_PEN => return "PERU:Nuevo Sol :604:2015-12";
         when C_BRE => return "BRAZIL:Cruzeiro:076:1993-03";
         when C_GRD => return "GREECE:Drachma:300:2002-03";
         when C_PES => return "PERU:Sol:604:1986-02";
         when C_MWK => return "MALAWI:Kwacha:454:2016-02";
         when C_MLF => return "MALI:Mali Franc:466:1984-11";
         when C_BRN => return "BRAZIL:New Cruzado:076:1990-03";
         when C_BGJ => return "BULGARIA:Lev A/52:100:1989 to 1990";
         when C_ESA => return "SPAIN:Spanish Peseta:996:1978 to 1981";
         when C_BGK => return "BULGARIA:Lev A/62:100:1989 to 1990";
         when C_ESB => return "SPAIN:""A"" Account (convertible Peseta Account):995:1994-12";
         when C_BGL => return "BULGARIA:Lev:100:2003-11";
         when C_BRR => return "BRAZIL:Cruzeiro Real:987:1994-07";
         when C_XFO => return "ZZ01_Gold-Franc:Gold-Franc::2006-10";
         when C_XFU => return "ZZ05_UIC-Franc:UIC-Franc::2013-11";
         when C_ESP => return "ANDORRA:Spanish Peseta:724:2002-03;SPAIN:Spanish Peseta:724:2002-03";
         when C_ZMK => return "ZAMBIA:Zambian Kwacha:894:2012-12";
         when C_MTL => return "MALTA:Maltese Lira:470:2008-01";
         when C_MTP => return "MALTA:Maltese Pound:470:1983-06";
         when C_BOP => return "BOLIVIA:Peso boliviano:068:1987-02";
         when C_TPE => return "TIMOR-LESTE:Timor Escudo:626:2002-11";
         when C_EEK => return "ESTONIA:Kroon:233:2011-01";
         when C_GWE => return "GUINEA-BISSAU:Guinea Escudo:624:1978 to 1981";
         when C_ARA => return "ARGENTINA:Austral:032:1992-01";
         when C_BAD => return "BOSNIA AND HERZEGOVINA:Dinar:070:1998-07";
         when C_SKK => return "SLOVAKIA:Slovak Koruna:703:2009-01";
         when C_FRF => return "ANDORRA:French Franc:250:2002-03;FRANCE:French Franc:250:2002-03;FRENCH  GUIANA:French Franc:250:2002-03;FRENCH SOUTHERN TERRITORIES:French Franc:250:2002-03;GUADELOUPE:French Franc:250:2002-03;MARTINIQUE:French Franc:250:2002-03;MAYOTTE:French Franc:250:2002-03;MONACO:French Franc:250:2002-03;RÉUNION:French Franc:250:2002-03;SAINT MARTIN:French Franc:250:1999-01;SAINT PIERRE AND MIQUELON:French Franc:250:2002-03;SAINT-BARTHÉLEMY:French Franc:250:1999-01";
         when C_GWP => return "GUINEA-BISSAU:Guinea-Bissau Peso:624:1997-05";
         when C_ARP => return "ARGENTINA:Peso Argentino:032:1985-07";
         when C_LAJ => return "LAO:Pathet Lao Kip:418:1979-12";
         when C_ARY => return "ARGENTINA:Peso:032:1989 to 1990";
         when C_ISJ => return "ICELAND:Old Krona:352:1989 to 1990";
         when C_ZRN => return "ZAIRE:New Zaire:180:1999-06";
         when C_CYP => return "CYPRUS:Cyprus Pound:196:2008-01";
         when C_TMM => return "TURKMENISTAN:Turkmenistan Manat:795:2009-01";
         when C_UGS => return "UGANDA:Uganda Shilling:800:1987-05";
         when C_AZM => return "AZERBAIJAN:Azerbaijanian Manat:031:2005-12";
         when C_UGW => return "UGANDA:Old Shilling:800:1989 to 1990";
         when C_ZRZ => return "ZAIRE:Zaire:180:1994-02";
         when C_LTL => return "LITHUANIA:Lithuanian Litas:440:2014-12";
         when C_AOK => return "ANGOLA:Kwanza:024:1991-03";
         when C_AON => return "ANGOLA:New Kwanza:024:2000-02";
         when C_AOR => return "ANGOLA:Kwanza Reajustado:982:2000-02";
         when C_LTT => return "LITHUANIA:Talonas:440:1993-07";
         when C_ADP => return "ANDORRA:Andorran Peseta:020:2003-07";
         when C_EUR => return "SERBIA AND MONTENEGRO:Euro:978:2006-10";
         when C_YUD => return "YUGOSLAVIA:New Yugoslavian Dinar:890:1990-01";
         when C_DEM => return "GERMANY:Deutsche Mark:276:2002-03";
         when C_YUM => return "YUGOSLAVIA:New Dinar:891:2003-07";
         when C_YUN => return "YUGOSLAVIA:Yugoslavian Dinar:890:1995-11";
         when C_GQE => return "EQUATORIAL GUINEA:Ekwele:226:1986-06";
         when C_IEP => return "IRELAND:Irish Pound:372:2002-03";
         when C_TJR => return "TAJIKISTAN:Tajik Ruble:762:2001-04";
         when C_MVQ => return "MALDIVES:Maldive Rupee:462:1989-12";
         when C_ALK => return "ALBANIA:Old Lek:008:1989-12";
         when C_ZWC => return "ZIMBABWE:Rhodesian Dollar:716:1989-12";
         when C_ZWD => return "ZIMBABWE:Zimbabwe Dollar (old):716:2006-08;ZIMBABWE:Zimbabwe Dollar:716:2008-08";
         when C_XEU => return "EUROPEAN MONETARY CO-OPERATION FUND (EMCF):European Currency Unit (E.C.U):954:1999-01";
         when C_CSD => return "SERBIA AND MONTENEGRO:Serbian Dinar:891:2006-10";
         when C_BYB => return "BELARUS:Belarusian Ruble:112:2001-01";
         when C_ZWN => return "ZIMBABWE:Zimbabwe Dollar (new):942:2006-09";
         when C_CHC => return "SWITZERLAND:WIR Franc (for electronic):948:2004-11";
         when C_CSJ => return "CZECHOSLOVAKIA:Krona A/53:203:1989 to 1990";
         when C_CSK => return "CZECHOSLOVAKIA:Koruna:200:1993-03";
         when C_TRL => return "TURKEY:Old Turkish Lira:792:2005-12";
         when C_ZWR => return "ZIMBABWE:Zimbabwe Dollar:935:2009-06";
         when C_UAK => return "UKRAINE:Karbovanet:804:1996-09";
         when C_GNE => return "GUINEA:Syli:324:1989-12";
         when C_PLZ => return "POLAND:Zloty:616:1997-01";
         when C_ZAL => return "LESOTHO:Financial Rand:991:1995-03;SOUTH AFRICA:Financial Rand:991:1995-03";
         when C_BYR => return "BELARUS:Belarusian Ruble:974:2017-01";
         when C_RHD => return "SOUTHERN RHODESIA :Rhodesian Dollar:716:1978 to 1981";
         when C_TRY => return "TURKEY:New Turkish Lira:949:2009-01";
         when C_VNC => return "VIETNAM:Old Dong:704:1989-1990";
         when C_PTE => return "PORTUGAL:Portuguese Escudo:620:2002-03";
         when C_ATS => return "AUSTRIA:Schilling:040:2002-03";
         when C_GNS => return "GUINEA:Syli:324:1986-02";
         when C_FIM => return "ÅLAND ISLANDS:Markka:246:2002-03;FINLAND:Markka:246:2002-03";
         when C_YDD => return "YEMEN, DEMOCRATIC:Yemeni Dinar:720:1991-09";
         when C_AFA => return "AFGHANISTAN:Afghani:004:2003-01";
         when C_SUR => return "UNION OF SOVIET SOCIALIST REPUBLICS:Rouble:810:1990-12";
         when C_LVL => return "LATVIA:Latvian Lats:428:2014-01";
         when C_LVR => return "LATVIA:Latvian Ruble:428:1994-12";
         when C_SRG => return "SURINAME:Surinam Guilder:740:2003-12";
         when C_XRE => return "ZZ02_RINET Funds Code:RINET Funds Code::1999-11";
         when C_GHC => return "GHANA:Cedi:288:2008-01";
         when C_MXP => return "MEXICO:Mexican Peso:484:1993-01";
         when C_AYM => return "AZERBAIJAN:Azerbaijan Manat:945:2005-10";
         when C_ANG => return "NETHERLANDS ANTILLES:Netherlands Antillean Guilder:532:2010-10";
         when C_LSM => return "LESOTHO:Loti:426:1985-05";
         when C_GHP => return "GHANA:Ghana Cedi:939:2007-06";
         when C_UYN => return "URUGUAY:Old Uruguay Peso:858:1989-12";
         when C_UYP => return "URUGUAY:Uruguayan Peso:858:1993-03";
         when C_SZL => return "SWAZILAND:Lilangeni:748:2018-08";
         when C_DDM => return "GERMAN DEMOCRATIC REPUBLIC:Mark der DDR:278:1990-07 to 1990-09";
         when C_BEC => return "BELGIUM:Convertible Franc:993:1990-03";
         when C_SDD => return "SUDAN:Sudanese Dinar:736:2007-07";
         when C_BEF => return "BELGIUM:Belgian Franc:056:2002-03";
         when C_SDG => return "SOUTH SUDAN:Sudanese Pound:938:2012-09";
         when C_IDR => return "TIMOR-LESTE:Rupiah:360:2002-07";
         when C_BEL => return "BELGIUM:Financial Franc:992:1990-03";
         when C_GEK => return "GEORGIA:Georgian Coupon:268:1995-10";
         when C_RUR => return "ARMENIA:Russian Ruble:810:1994-08;AZERBAIJAN:Russian Ruble:810:1994-08;BELARUS:Russian Ruble:810:1994-06;GEORGIA:Russian Ruble:810:1994-04;KAZAKHSTAN:Russian Ruble:810:1994-05;KYRGYZSTAN:Russian Ruble:810:1993-01;MOLDOVA, REPUBLIC OF:Russian Ruble:810:1993-12;RUSSIAN FEDERATION:Russian Ruble:810:2004-01;TAJIKISTAN:Russian Ruble:810:1995-05;TURKMENISTAN:Russian Ruble:810:1993-10;UZBEKISTAN:Russian Ruble:810:1994-07";
         when C_SDP => return "SUDAN:Sudanese Pound:736:1998-06";
         when C_VEB => return "VENEZUELA:Bolivar:862:2008-01";
         when C_VEF => return "VENEZUELA:Bolivar Fuerte:937:2011-12;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolivar:937:2016-02;VENEZUELA (BOLIVARIAN REPUBLIC OF):Bolívar:937:2018-08";
         when C_HRD => return "CROATIA:Croatian Dinar:191:1995-01";
         when others => return "";
      end case;
   end Historic_Records;

   function Entities (This : Currency) return Countries.Country_List is
      use Countries;
   begin
      case This.Key is
         when C_BMD => return (
             1 => (Key => C_BM)
            );
         when C_VES => return (
             1 => (Key => C_VE)
            );
         when C_SLE => return (
             1 => (Key => C_SL)
            );
         when C_NAD => return (
             1 => (Key => C_NA)
            );
         when C_MGA => return (
             1 => (Key => C_MG)
            );
         when C_GMD => return (
             1 => (Key => C_GM)
            );
         when C_BBD => return (
             1 => (Key => C_BB)
            );
         when C_ILS => return (
             1 => (Key => C_IL)
            );
         when C_SLL => return (
             1 => (Key => C_SL)
            );
         when C_XAF => return (
             1 => (Key => C_CM),
             2 => (Key => C_CF),
             3 => (Key => C_TD),
             4 => (Key => C_CG),
             5 => (Key => C_GQ),
             6 => (Key => C_GA)
            );
         when C_MRU => return (
             1 => (Key => C_MR)
            );
         when C_USD => return (
             1 => (Key => C_AS),
             2 => (Key => C_BQ),
             3 => (Key => C_IO),
             4 => (Key => C_EC),
             5 => (Key => C_SV),
             6 => (Key => C_GU),
             7 => (Key => C_HT),
             8 => (Key => C_MH),
             9 => (Key => C_FM),
             10 => (Key => C_MP),
             11 => (Key => C_PW),
             12 => (Key => C_PA),
             13 => (Key => C_PR),
             14 => (Key => C_TL),
             15 => (Key => C_TC),
             16 => (Key => C_UM),
             17 => (Key => C_US),
             18 => (Key => C_VG),
             19 => (Key => C_VI)
            );
         when C_SAR => return (
             1 => (Key => C_SA)
            );
         when C_GBP => return (
             1 => (Key => C_GG),
             2 => (Key => C_IM),
             3 => (Key => C_JE),
             4 => (Key => C_GB)
            );
         when C_LBP => return (
             1 => (Key => C_LB)
            );
         when C_TND => return (
             1 => (Key => C_TN)
            );
         when C_CZK => return (
             1 => (Key => C_CZ)
            );
         when C_USN => return (
             1 => (Key => C_US)
            );
         when C_KHR => return (
             1 => (Key => C_KH)
            );
         when C_PHP => return (
             1 => (Key => C_PH)
            );
         when C_CDF => return (
             1 => (Key => C_CD)
            );
         when C_MZN => return (
             1 => (Key => C_MZ)
            );
         when C_COP => return (
             1 => (Key => C_CO)
            );
         when C_STN => return (
             1 => (Key => C_ST)
            );
         when C_NIO => return (
             1 => (Key => C_NI)
            );
         when C_COU => return (
             1 => (Key => C_CO)
            );
         when C_AED => return (
             1 => (Key => C_AE)
            );
         when C_MOP => return (
             1 => (Key => C_MO)
            );
         when C_MDL => return (
             1 => (Key => C_MD)
            );
         when C_RON => return (
             1 => (Key => C_RO)
            );
         when C_IQD => return (
             1 => (Key => C_IQ)
            );
         when C_VUV => return (
             1 => (Key => C_VU)
            );
         when C_CLF => return (
             1 => (Key => C_CL)
            );
         when C_PEN => return (
             1 => (Key => C_PE)
            );
         when C_KES => return (
             1 => (Key => C_KE)
            );
         when C_CAD => return (
             1 => (Key => C_CA)
            );
         when C_RWF => return (
             1 => (Key => C_RW)
            );
         when C_MWK => return (
             1 => (Key => C_MW)
            );
         when C_LRD => return (
             1 => (Key => C_LR)
            );
         when C_BRL => return (
             1 => (Key => C_BR)
            );
         when C_CLP => return (
             1 => (Key => C_CL)
            );
         when C_AMD => return (
             1 => (Key => C_AM)
            );
         when C_MAD => return (
             1 => (Key => C_MA)
            );
         when C_BGN => return (
             1 => (Key => C_BG)
            );
         when C_KMF => return (
             1 => (Key => C_KM)
            );
         when C_BZD => return (
             1 => (Key => C_BZ)
            );
         when C_THB => return (
             1 => (Key => C_TH)
            );
         when C_BOB => return (
             1 => (Key => C_BO)
            );
         when C_HTG => return (
             1 => (Key => C_HT)
            );
         when C_AUD => return (
             1 => (Key => C_AU),
             2 => (Key => C_CX),
             3 => (Key => C_CC),
             4 => (Key => C_HM),
             5 => (Key => C_KI),
             6 => (Key => C_NR),
             7 => (Key => C_NF),
             8 => (Key => C_TV)
            );
         when C_INR => return (
             1 => (Key => C_BT),
             2 => (Key => C_IN)
            );
         when C_XCD => return (
             1 => (Key => C_AI),
             2 => (Key => C_AG),
             3 => (Key => C_DM),
             4 => (Key => C_GD),
             5 => (Key => C_MS),
             6 => (Key => C_KN),
             7 => (Key => C_LC),
             8 => (Key => C_VC)
            );
         when C_ZMW => return (
             1 => (Key => C_ZM)
            );
         when C_FJD => return (
             1 => (Key => C_FJ)
            );
         when C_BOV => return (
             1 => (Key => C_BO)
            );
         when C_SCR => return (
             1 => (Key => C_SC)
            );
         when C_BDT => return (
             1 => (Key => C_BD)
            );
         when C_SVC => return (
             1 => (Key => C_SV)
            );
         when C_DKK => return (
             1 => (Key => C_DK),
             2 => (Key => C_FO),
             3 => (Key => C_GL)
            );
         when C_JPY => return (
             1 => (Key => C_JP)
            );
         when C_BWP => return (
             1 => (Key => C_BW)
            );
         when C_YER => return (
             1 => (Key => C_YE)
            );
         when C_BAM => return (
             1 => (Key => C_BA)
            );
         when C_ARS => return (
             1 => (Key => C_AR)
            );
         when C_JMD => return (
             1 => (Key => C_JM)
            );
         when C_PGK => return (
             1 => (Key => C_PG)
            );
         when C_KRW => return (
             1 => (Key => C_KR)
            );
         when C_ISK => return (
             1 => (Key => C_IS)
            );
         when C_KGS => return (
             1 => (Key => C_KG)
            );
         when C_QAR => return (
             1 => (Key => C_QA)
            );
         when C_AOA => return (
             1 => (Key => C_AO)
            );
         when C_OMR => return (
             1 => (Key => C_OM)
            );
         when C_BIF => return (
             1 => (Key => C_BI)
            );
         when C_HNL => return (
             1 => (Key => C_HN)
            );
         when C_BTN => return (
             1 => (Key => C_BT)
            );
         when C_SSP => return (
             1 => (Key => C_SS)
            );
         when C_TMT => return (
             1 => (Key => C_TM)
            );
         when C_AZN => return (
             1 => (Key => C_AZ)
            );
         when C_MYR => return (
             1 => (Key => C_MY)
            );
         when C_UGX => return (
             1 => (Key => C_UG)
            );
         when C_GTQ => return (
             1 => (Key => C_GT)
            );
         when C_SHP => return (
             1 => (Key => C_SH)
            );
         when C_CNY => return (
             1 => (Key => C_CN)
            );
         when C_MNT => return (
             1 => (Key => C_MN)
            );
         when C_GIP => return (
             1 => (Key => C_GI)
            );
         when C_KZT => return (
             1 => (Key => C_KZ)
            );
         when C_CVE => return (
             1 => (Key => C_CV)
            );
         when C_EUR => return (
             1 => (Key => C_AX),
             2 => (Key => C_AD),
             3 => (Key => C_AT),
             4 => (Key => C_BE),
             5 => (Key => C_HR),
             6 => (Key => C_CY),
             7 => (Key => C_EE),
             8 => (Key => C_FI),
             9 => (Key => C_FR),
             10 => (Key => C_GF),
             11 => (Key => C_TF),
             12 => (Key => C_DE),
             13 => (Key => C_GR),
             14 => (Key => C_GP),
             15 => (Key => C_VA),
             16 => (Key => C_IE),
             17 => (Key => C_IT),
             18 => (Key => C_LV),
             19 => (Key => C_LT),
             20 => (Key => C_LU),
             21 => (Key => C_MT),
             22 => (Key => C_MQ),
             23 => (Key => C_YT),
             24 => (Key => C_MC),
             25 => (Key => C_ME),
             26 => (Key => C_PT),
             27 => (Key => C_MF),
             28 => (Key => C_PM),
             29 => (Key => C_SM),
             30 => (Key => C_SK),
             31 => (Key => C_SI),
             32 => (Key => C_ES)
            );
         when C_UZS => return (
             1 => (Key => C_UZ)
            );
         when C_HKD => return (
             1 => (Key => C_HK)
            );
         when C_MKD => return (
             1 => (Key => C_MK)
            );
         when C_XPF => return (
             1 => (Key => C_PF),
             2 => (Key => C_NC),
             3 => (Key => C_WF)
            );
         when C_AWG => return (
             1 => (Key => C_AW)
            );
         when C_NPR => return (
             1 => (Key => C_NP)
            );
         when C_KWD => return (
             1 => (Key => C_KW)
            );
         when C_TJS => return (
             1 => (Key => C_TJ)
            );
         when C_MVR => return (
             1 => (Key => C_MV)
            );
         when C_SEK => return (
             1 => (Key => C_SE)
            );
         when C_ALL => return (
             1 => (Key => C_AL)
            );
         when C_PAB => return (
             1 => (Key => C_PA)
            );
         when C_ERN => return (
             1 => (Key => C_ER)
            );
         when C_ZWL => return (
             1 => (Key => C_ZW)
            );
         when C_PLN => return (
             1 => (Key => C_PL)
            );
         when C_EGP => return (
             1 => (Key => C_EG)
            );
         when C_UAH => return (
             1 => (Key => C_UA)
            );
         when C_GYD => return (
             1 => (Key => C_GY)
            );
         when C_CHE => return (
             1 => (Key => C_CH)
            );
         when C_CHF => return (
             1 => (Key => C_LI),
             2 => (Key => C_CH)
            );
         when C_BND => return (
             1 => (Key => C_BN)
            );
         when C_LYD => return (
             1 => (Key => C_LY)
            );
         when C_RSD => return (
             1 => (Key => C_RS)
            );
         when C_BYN => return (
             1 => (Key => C_BY)
            );
         when C_GNF => return (
             1 => (Key => C_GN)
            );
         when C_SBD => return (
             1 => (Key => C_SB)
            );
         when C_ZAR => return (
             1 => (Key => C_LS),
             2 => (Key => C_NA),
             3 => (Key => C_ZA)
            );
         when C_VND => return (
             1 => (Key => C_VN)
            );
         when C_CHW => return (
             1 => (Key => C_CH)
            );
         when C_JOD => return (
             1 => (Key => C_JO)
            );
         when C_WST => return (
             1 => (Key => C_WS)
            );
         when C_DJF => return (
             1 => (Key => C_DJ)
            );
         when C_TOP => return (
             1 => (Key => C_TO)
            );
         when C_AFN => return (
             1 => (Key => C_AF)
            );
         when C_TWD => return (
             1 => (Key => C_TW)
            );
         when C_LKR => return (
             1 => (Key => C_LK)
            );
         when C_SRD => return (
             1 => (Key => C_SR)
            );
         when C_BSD => return (
             1 => (Key => C_BS)
            );
         when C_SGD => return (
             1 => (Key => C_SG)
            );
         when C_IRR => return (
             1 => (Key => C_IR)
            );
         when C_BHD => return (
             1 => (Key => C_BH)
            );
         when C_MXN => return (
             1 => (Key => C_MX)
            );
         when C_KYD => return (
             1 => (Key => C_KY)
            );
         when C_MMK => return (
             1 => (Key => C_MM)
            );
         when C_NGN => return (
             1 => (Key => C_NG)
            );
         when C_ANG => return (
             1 => (Key => C_SX)
            );
         when C_ETB => return (
             1 => (Key => C_ET)
            );
         when C_LSL => return (
             1 => (Key => C_LS)
            );
         when C_MXV => return (
             1 => (Key => C_MX)
            );
         when C_PYG => return (
             1 => (Key => C_PY)
            );
         when C_DZD => return (
             1 => (Key => C_DZ)
            );
         when C_UYI => return (
             1 => (Key => C_UY)
            );
         when C_GHS => return (
             1 => (Key => C_GH)
            );
         when C_CUC => return (
             1 => (Key => C_CU)
            );
         when C_NZD => return (
             1 => (Key => C_CK),
             2 => (Key => C_NZ),
             3 => (Key => C_NU),
             4 => (Key => C_PN),
             5 => (Key => C_TK)
            );
         when C_TTD => return (
             1 => (Key => C_TT)
            );
         when C_HUF => return (
             1 => (Key => C_HU)
            );
         when C_UYU => return (
             1 => (Key => C_UY)
            );
         when C_DOP => return (
             1 => (Key => C_DO)
            );
         when C_RUB => return (
             1 => (Key => C_RU)
            );
         when C_UYW => return (
             1 => (Key => C_UY)
            );
         when C_SZL => return (
             1 => (Key => C_SZ)
            );
         when C_CUP => return (
             1 => (Key => C_CU)
            );
         when C_NOK => return (
             1 => (Key => C_BV),
             2 => (Key => C_NO),
             3 => (Key => C_SJ)
            );
         when C_XOF => return (
             1 => (Key => C_BJ),
             2 => (Key => C_BF),
             3 => (Key => C_GW),
             4 => (Key => C_ML),
             5 => (Key => C_NE),
             6 => (Key => C_SN),
             7 => (Key => C_TG)
            );
         when C_SDG => return (
             1 => (Key => C_SD)
            );
         when C_IDR => return (
             1 => (Key => C_ID)
            );
         when C_MUR => return (
             1 => (Key => C_MU)
            );
         when C_SOS => return (
             1 => (Key => C_SO)
            );
         when C_GEL => return (
             1 => (Key => C_GE)
            );
         when C_VED => return (
             1 => (Key => C_VE)
            );
         when C_FKP => return (
             1 => (Key => C_FK)
            );
         when C_CRC => return (
             1 => (Key => C_CR)
            );
         when C_PKR => return (
             1 => (Key => C_PK)
            );
         when others => return (
            1 => (Key => C_ZZ)
            );
      end case;
   end Entities;

   function From_Code (Code : Alphabetic_Code) return Currency is
      Result : constant Currency := (Key => Currency_Key'Value
                                             ("C_" & Code));
   begin
      return Result;
   end From_Code;

   function From_Numeric (Number : Numeric_Code) return Currency is
   begin
      return Result : Currency do
         Result.Key := Numeric_To_Key (Number);
      end return;
   end From_Numeric;
   function From_Numeric (Number : String) return Currency is
      Real_Number : constant Numeric_Code := Numeric_Code'Value (Number);
   begin
      return Result : Currency do
         Result.Key := Numeric_To_Key (Real_Number);
      end return;
   end From_Numeric;

   function From_Country (Item : Countries.Country) return Currency_List is
      use Countries;
   begin
      case Item.Key is
         when C_AF => return (
             1 => (Key => C_AFN)
            );
         when C_AL => return (
             1 => (Key => C_ALL)
            );
         when C_DZ => return (
             1 => (Key => C_DZD)
            );
         when C_AS => return (
             1 => (Key => C_USD)
            );
         when C_AD => return (
             1 => (Key => C_EUR)
            );
         when C_AO => return (
             1 => (Key => C_AOA)
            );
         when C_AI => return (
             1 => (Key => C_XCD)
            );
         when C_AG => return (
             1 => (Key => C_XCD)
            );
         when C_AR => return (
             1 => (Key => C_ARS)
            );
         when C_AM => return (
             1 => (Key => C_AMD)
            );
         when C_AW => return (
             1 => (Key => C_AWG)
            );
         when C_AU => return (
             1 => (Key => C_AUD)
            );
         when C_AT => return (
             1 => (Key => C_EUR)
            );
         when C_AZ => return (
             1 => (Key => C_AZN)
            );
         when C_BS => return (
             1 => (Key => C_BSD)
            );
         when C_BH => return (
             1 => (Key => C_BHD)
            );
         when C_BD => return (
             1 => (Key => C_BDT)
            );
         when C_BB => return (
             1 => (Key => C_BBD)
            );
         when C_BY => return (
             1 => (Key => C_BYN)
            );
         when C_BE => return (
             1 => (Key => C_EUR)
            );
         when C_BZ => return (
             1 => (Key => C_BZD)
            );
         when C_BJ => return (
             1 => (Key => C_XOF)
            );
         when C_BM => return (
             1 => (Key => C_BMD)
            );
         when C_BT => return (
             1 => (Key => C_INR),
             2 => (Key => C_BTN)
            );
         when C_BO => return (
             1 => (Key => C_BOB),
             2 => (Key => C_BOV)
            );
         when C_BQ => return (
             1 => (Key => C_USD)
            );
         when C_BA => return (
             1 => (Key => C_BAM)
            );
         when C_BW => return (
             1 => (Key => C_BWP)
            );
         when C_BV => return (
             1 => (Key => C_NOK)
            );
         when C_BR => return (
             1 => (Key => C_BRL)
            );
         when C_IO => return (
             1 => (Key => C_USD)
            );
         when C_BN => return (
             1 => (Key => C_BND)
            );
         when C_BG => return (
             1 => (Key => C_BGN)
            );
         when C_BF => return (
             1 => (Key => C_XOF)
            );
         when C_BI => return (
             1 => (Key => C_BIF)
            );
         when C_CV => return (
             1 => (Key => C_CVE)
            );
         when C_KH => return (
             1 => (Key => C_KHR)
            );
         when C_CM => return (
             1 => (Key => C_XAF)
            );
         when C_CA => return (
             1 => (Key => C_CAD)
            );
         when C_KY => return (
             1 => (Key => C_KYD)
            );
         when C_CF => return (
             1 => (Key => C_XAF)
            );
         when C_TD => return (
             1 => (Key => C_XAF)
            );
         when C_CL => return (
             1 => (Key => C_CLF),
             2 => (Key => C_CLP)
            );
         when C_CN => return (
             1 => (Key => C_CNY)
            );
         when C_CX => return (
             1 => (Key => C_AUD)
            );
         when C_CC => return (
             1 => (Key => C_AUD)
            );
         when C_CO => return (
             1 => (Key => C_COP),
             2 => (Key => C_COU)
            );
         when C_KM => return (
             1 => (Key => C_KMF)
            );
         when C_CD => return (
             1 => (Key => C_CDF)
            );
         when C_CG => return (
             1 => (Key => C_XAF)
            );
         when C_CK => return (
             1 => (Key => C_NZD)
            );
         when C_CR => return (
             1 => (Key => C_CRC)
            );
         when C_HR => return (
             1 => (Key => C_EUR)
            );
         when C_CU => return (
             1 => (Key => C_CUC),
             2 => (Key => C_CUP)
            );
         when C_CY => return (
             1 => (Key => C_EUR)
            );
         when C_CZ => return (
             1 => (Key => C_CZK)
            );
         when C_DK => return (
             1 => (Key => C_DKK)
            );
         when C_DJ => return (
             1 => (Key => C_DJF)
            );
         when C_DM => return (
             1 => (Key => C_XCD)
            );
         when C_DO => return (
             1 => (Key => C_DOP)
            );
         when C_EC => return (
             1 => (Key => C_USD)
            );
         when C_EG => return (
             1 => (Key => C_EGP)
            );
         when C_SV => return (
             1 => (Key => C_USD),
             2 => (Key => C_SVC)
            );
         when C_GQ => return (
             1 => (Key => C_XAF)
            );
         when C_ER => return (
             1 => (Key => C_ERN)
            );
         when C_EE => return (
             1 => (Key => C_EUR)
            );
         when C_SZ => return (
             1 => (Key => C_SZL)
            );
         when C_ET => return (
             1 => (Key => C_ETB)
            );
         when C_FK => return (
             1 => (Key => C_FKP)
            );
         when C_FO => return (
             1 => (Key => C_DKK)
            );
         when C_FJ => return (
             1 => (Key => C_FJD)
            );
         when C_FI => return (
             1 => (Key => C_EUR)
            );
         when C_FR => return (
             1 => (Key => C_EUR)
            );
         when C_GF => return (
             1 => (Key => C_EUR)
            );
         when C_PF => return (
             1 => (Key => C_XPF)
            );
         when C_TF => return (
             1 => (Key => C_EUR)
            );
         when C_GA => return (
             1 => (Key => C_XAF)
            );
         when C_GM => return (
             1 => (Key => C_GMD)
            );
         when C_GE => return (
             1 => (Key => C_GEL)
            );
         when C_DE => return (
             1 => (Key => C_EUR)
            );
         when C_GH => return (
             1 => (Key => C_GHS)
            );
         when C_GI => return (
             1 => (Key => C_GIP)
            );
         when C_GR => return (
             1 => (Key => C_EUR)
            );
         when C_GL => return (
             1 => (Key => C_DKK)
            );
         when C_GD => return (
             1 => (Key => C_XCD)
            );
         when C_GP => return (
             1 => (Key => C_EUR)
            );
         when C_GU => return (
             1 => (Key => C_USD)
            );
         when C_GT => return (
             1 => (Key => C_GTQ)
            );
         when C_GG => return (
             1 => (Key => C_GBP)
            );
         when C_GN => return (
             1 => (Key => C_GNF)
            );
         when C_GW => return (
             1 => (Key => C_XOF)
            );
         when C_GY => return (
             1 => (Key => C_GYD)
            );
         when C_HT => return (
             1 => (Key => C_USD),
             2 => (Key => C_HTG)
            );
         when C_HM => return (
             1 => (Key => C_AUD)
            );
         when C_VA => return (
             1 => (Key => C_EUR)
            );
         when C_HN => return (
             1 => (Key => C_HNL)
            );
         when C_HK => return (
             1 => (Key => C_HKD)
            );
         when C_HU => return (
             1 => (Key => C_HUF)
            );
         when C_IS => return (
             1 => (Key => C_ISK)
            );
         when C_IN => return (
             1 => (Key => C_INR)
            );
         when C_ID => return (
             1 => (Key => C_IDR)
            );
         when C_IR => return (
             1 => (Key => C_IRR)
            );
         when C_IQ => return (
             1 => (Key => C_IQD)
            );
         when C_IE => return (
             1 => (Key => C_EUR)
            );
         when C_IM => return (
             1 => (Key => C_GBP)
            );
         when C_IL => return (
             1 => (Key => C_ILS)
            );
         when C_IT => return (
             1 => (Key => C_EUR)
            );
         when C_JM => return (
             1 => (Key => C_JMD)
            );
         when C_JP => return (
             1 => (Key => C_JPY)
            );
         when C_JE => return (
             1 => (Key => C_GBP)
            );
         when C_JO => return (
             1 => (Key => C_JOD)
            );
         when C_KZ => return (
             1 => (Key => C_KZT)
            );
         when C_KE => return (
             1 => (Key => C_KES)
            );
         when C_KI => return (
             1 => (Key => C_AUD)
            );
         when C_KR => return (
             1 => (Key => C_KRW)
            );
         when C_KW => return (
             1 => (Key => C_KWD)
            );
         when C_KG => return (
             1 => (Key => C_KGS)
            );
         when C_LV => return (
             1 => (Key => C_EUR)
            );
         when C_LB => return (
             1 => (Key => C_LBP)
            );
         when C_LS => return (
             1 => (Key => C_ZAR),
             2 => (Key => C_LSL)
            );
         when C_LR => return (
             1 => (Key => C_LRD)
            );
         when C_LY => return (
             1 => (Key => C_LYD)
            );
         when C_LI => return (
             1 => (Key => C_CHF)
            );
         when C_LT => return (
             1 => (Key => C_EUR)
            );
         when C_LU => return (
             1 => (Key => C_EUR)
            );
         when C_MO => return (
             1 => (Key => C_MOP)
            );
         when C_MG => return (
             1 => (Key => C_MGA)
            );
         when C_MW => return (
             1 => (Key => C_MWK)
            );
         when C_MY => return (
             1 => (Key => C_MYR)
            );
         when C_MV => return (
             1 => (Key => C_MVR)
            );
         when C_ML => return (
             1 => (Key => C_XOF)
            );
         when C_MT => return (
             1 => (Key => C_EUR)
            );
         when C_MH => return (
             1 => (Key => C_USD)
            );
         when C_MQ => return (
             1 => (Key => C_EUR)
            );
         when C_MR => return (
             1 => (Key => C_MRU)
            );
         when C_MU => return (
             1 => (Key => C_MUR)
            );
         when C_YT => return (
             1 => (Key => C_EUR)
            );
         when C_MX => return (
             1 => (Key => C_MXN),
             2 => (Key => C_MXV)
            );
         when C_FM => return (
             1 => (Key => C_USD)
            );
         when C_MD => return (
             1 => (Key => C_MDL)
            );
         when C_MC => return (
             1 => (Key => C_EUR)
            );
         when C_MN => return (
             1 => (Key => C_MNT)
            );
         when C_ME => return (
             1 => (Key => C_EUR)
            );
         when C_MS => return (
             1 => (Key => C_XCD)
            );
         when C_MA => return (
             1 => (Key => C_MAD)
            );
         when C_MZ => return (
             1 => (Key => C_MZN)
            );
         when C_MM => return (
             1 => (Key => C_MMK)
            );
         when C_NA => return (
             1 => (Key => C_NAD),
             2 => (Key => C_ZAR)
            );
         when C_NR => return (
             1 => (Key => C_AUD)
            );
         when C_NP => return (
             1 => (Key => C_NPR)
            );
         when C_NC => return (
             1 => (Key => C_XPF)
            );
         when C_NZ => return (
             1 => (Key => C_NZD)
            );
         when C_NI => return (
             1 => (Key => C_NIO)
            );
         when C_NE => return (
             1 => (Key => C_XOF)
            );
         when C_NG => return (
             1 => (Key => C_NGN)
            );
         when C_NU => return (
             1 => (Key => C_NZD)
            );
         when C_NF => return (
             1 => (Key => C_AUD)
            );
         when C_MK => return (
             1 => (Key => C_MKD)
            );
         when C_MP => return (
             1 => (Key => C_USD)
            );
         when C_NO => return (
             1 => (Key => C_NOK)
            );
         when C_OM => return (
             1 => (Key => C_OMR)
            );
         when C_PK => return (
             1 => (Key => C_PKR)
            );
         when C_PW => return (
             1 => (Key => C_USD)
            );
         when C_PA => return (
             1 => (Key => C_USD),
             2 => (Key => C_PAB)
            );
         when C_PG => return (
             1 => (Key => C_PGK)
            );
         when C_PY => return (
             1 => (Key => C_PYG)
            );
         when C_PE => return (
             1 => (Key => C_PEN)
            );
         when C_PH => return (
             1 => (Key => C_PHP)
            );
         when C_PN => return (
             1 => (Key => C_NZD)
            );
         when C_PL => return (
             1 => (Key => C_PLN)
            );
         when C_PT => return (
             1 => (Key => C_EUR)
            );
         when C_PR => return (
             1 => (Key => C_USD)
            );
         when C_QA => return (
             1 => (Key => C_QAR)
            );
         when C_RO => return (
             1 => (Key => C_RON)
            );
         when C_RU => return (
             1 => (Key => C_RUB)
            );
         when C_RW => return (
             1 => (Key => C_RWF)
            );
         when C_SH => return (
             1 => (Key => C_SHP)
            );
         when C_KN => return (
             1 => (Key => C_XCD)
            );
         when C_LC => return (
             1 => (Key => C_XCD)
            );
         when C_MF => return (
             1 => (Key => C_EUR)
            );
         when C_PM => return (
             1 => (Key => C_EUR)
            );
         when C_VC => return (
             1 => (Key => C_XCD)
            );
         when C_WS => return (
             1 => (Key => C_WST)
            );
         when C_SM => return (
             1 => (Key => C_EUR)
            );
         when C_ST => return (
             1 => (Key => C_STN)
            );
         when C_SA => return (
             1 => (Key => C_SAR)
            );
         when C_SN => return (
             1 => (Key => C_XOF)
            );
         when C_RS => return (
             1 => (Key => C_RSD)
            );
         when C_SC => return (
             1 => (Key => C_SCR)
            );
         when C_SL => return (
             1 => (Key => C_SLE),
             2 => (Key => C_SLL)
            );
         when C_SG => return (
             1 => (Key => C_SGD)
            );
         when C_SX => return (
             1 => (Key => C_ANG)
            );
         when C_SK => return (
             1 => (Key => C_EUR)
            );
         when C_SI => return (
             1 => (Key => C_EUR)
            );
         when C_SB => return (
             1 => (Key => C_SBD)
            );
         when C_SO => return (
             1 => (Key => C_SOS)
            );
         when C_ZA => return (
             1 => (Key => C_ZAR)
            );
         when C_SS => return (
             1 => (Key => C_SSP)
            );
         when C_ES => return (
             1 => (Key => C_EUR)
            );
         when C_LK => return (
             1 => (Key => C_LKR)
            );
         when C_SD => return (
             1 => (Key => C_SDG)
            );
         when C_SR => return (
             1 => (Key => C_SRD)
            );
         when C_SJ => return (
             1 => (Key => C_NOK)
            );
         when C_SE => return (
             1 => (Key => C_SEK)
            );
         when C_CH => return (
             1 => (Key => C_CHE),
             2 => (Key => C_CHF),
             3 => (Key => C_CHW)
            );
         when C_TW => return (
             1 => (Key => C_TWD)
            );
         when C_TJ => return (
             1 => (Key => C_TJS)
            );
         when C_TH => return (
             1 => (Key => C_THB)
            );
         when C_TL => return (
             1 => (Key => C_USD)
            );
         when C_TG => return (
             1 => (Key => C_XOF)
            );
         when C_TK => return (
             1 => (Key => C_NZD)
            );
         when C_TO => return (
             1 => (Key => C_TOP)
            );
         when C_TT => return (
             1 => (Key => C_TTD)
            );
         when C_TN => return (
             1 => (Key => C_TND)
            );
         when C_TM => return (
             1 => (Key => C_TMT)
            );
         when C_TC => return (
             1 => (Key => C_USD)
            );
         when C_TV => return (
             1 => (Key => C_AUD)
            );
         when C_UG => return (
             1 => (Key => C_UGX)
            );
         when C_UA => return (
             1 => (Key => C_UAH)
            );
         when C_AE => return (
             1 => (Key => C_AED)
            );
         when C_GB => return (
             1 => (Key => C_GBP)
            );
         when C_UM => return (
             1 => (Key => C_USD)
            );
         when C_US => return (
             1 => (Key => C_USD),
             2 => (Key => C_USN)
            );
         when C_UY => return (
             1 => (Key => C_UYI),
             2 => (Key => C_UYU),
             3 => (Key => C_UYW)
            );
         when C_UZ => return (
             1 => (Key => C_UZS)
            );
         when C_VU => return (
             1 => (Key => C_VUV)
            );
         when C_VE => return (
             1 => (Key => C_VES),
             2 => (Key => C_VED)
            );
         when C_VN => return (
             1 => (Key => C_VND)
            );
         when C_VG => return (
             1 => (Key => C_USD)
            );
         when C_VI => return (
             1 => (Key => C_USD)
            );
         when C_WF => return (
             1 => (Key => C_XPF)
            );
         when C_YE => return (
             1 => (Key => C_YER)
            );
         when C_ZM => return (
             1 => (Key => C_ZMW)
            );
         when C_ZW => return (
             1 => (Key => C_ZWL)
            );
         when C_AX => return (
             1 => (Key => C_EUR)
            );
         when others => return (
            1 => (Key => C_ZZZ)
            );
      end case;
   end From_Country;

   function Init_Currencies return All_Currencies is
   begin
      return Result : All_Currencies do
         for X in Currency_Key'Range loop
            Result (X) := (Key => X);
         end loop;
      end return;
   end Init_Currencies;

   function Numeric_To_Key (Numeric : Numeric_Code) return Currency_Key is
   begin
      case Numeric is
         when 060 => return C_BMD;
         when 928 => return C_VES;
         when 925 => return C_SLE;
         when 516 => return C_NAD;
         when 969 => return C_MGA;
         when 270 => return C_GMD;
         when 052 => return C_BBD;
         when 376 => return C_ILS;
         when 694 => return C_SLL;
         when 950 => return C_XAF;
         when 961 => return C_XAG;
         when 929 => return C_MRU;
         when 840 => return C_USD;
         when 682 => return C_SAR;
         when 826 => return C_GBP;
         when 422 => return C_LBP;
         when 959 => return C_XAU;
         when 788 => return C_TND;
         when 203 => return C_CZK;
         when 997 => return C_USN;
         when 116 => return C_KHR;
         when 608 => return C_PHP;
         when 976 => return C_CDF;
         when 943 => return C_MZN;
         when 170 => return C_COP;
         when 930 => return C_STN;
         when 558 => return C_NIO;
         when 970 => return C_COU;
         when 784 => return C_AED;
         when 446 => return C_MOP;
         when 498 => return C_MDL;
         when 946 => return C_RON;
         when 963 => return C_XTS;
         when 368 => return C_IQD;
         when 548 => return C_VUV;
         when 990 => return C_CLF;
         when 408 => return C_KPW;
         when 604 => return C_PEN;
         when 404 => return C_KES;
         when 124 => return C_CAD;
         when 646 => return C_RWF;
         when 454 => return C_MWK;
         when 430 => return C_LRD;
         when 986 => return C_BRL;
         when 152 => return C_CLP;
         when 051 => return C_AMD;
         when 504 => return C_MAD;
         when 975 => return C_BGN;
         when 174 => return C_KMF;
         when 084 => return C_BZD;
         when 764 => return C_THB;
         when 068 => return C_BOB;
         when 332 => return C_HTG;
         when 036 => return C_AUD;
         when 760 => return C_SYP;
         when 356 => return C_INR;
         when 951 => return C_XCD;
         when 967 => return C_ZMW;
         when 242 => return C_FJD;
         when 984 => return C_BOV;
         when 690 => return C_SCR;
         when 050 => return C_BDT;
         when 222 => return C_SVC;
         when 208 => return C_DKK;
         when 392 => return C_JPY;
         when 072 => return C_BWP;
         when 886 => return C_YER;
         when 977 => return C_BAM;
         when 032 => return C_ARS;
         when 388 => return C_JMD;
         when 418 => return C_LAK;
         when 598 => return C_PGK;
         when 410 => return C_KRW;
         when 352 => return C_ISK;
         when 417 => return C_KGS;
         when 634 => return C_QAR;
         when 973 => return C_AOA;
         when 512 => return C_OMR;
         when 108 => return C_BIF;
         when 340 => return C_HNL;
         when 064 => return C_BTN;
         when 728 => return C_SSP;
         when 934 => return C_TMT;
         when 944 => return C_AZN;
         when 458 => return C_MYR;
         when 800 => return C_UGX;
         when 320 => return C_GTQ;
         when 654 => return C_SHP;
         when 156 => return C_CNY;
         when 496 => return C_MNT;
         when 994 => return C_XSU;
         when 292 => return C_GIP;
         when 398 => return C_KZT;
         when 132 => return C_CVE;
         when 978 => return C_EUR;
         when 860 => return C_UZS;
         when 344 => return C_HKD;
         when 964 => return C_XPD;
         when 807 => return C_MKD;
         when 953 => return C_XPF;
         when 533 => return C_AWG;
         when 524 => return C_NPR;
         when 414 => return C_KWD;
         when 972 => return C_TJS;
         when 462 => return C_MVR;
         when 752 => return C_SEK;
         when 008 => return C_ALL;
         when 962 => return C_XPT;
         when 590 => return C_PAB;
         when 232 => return C_ERN;
         when 932 => return C_ZWL;
         when 985 => return C_PLN;
         when 818 => return C_EGP;
         when 980 => return C_UAH;
         when 328 => return C_GYD;
         when 947 => return C_CHE;
         when 756 => return C_CHF;
         when 096 => return C_BND;
         when 434 => return C_LYD;
         when 941 => return C_RSD;
         when 933 => return C_BYN;
         when 324 => return C_GNF;
         when 090 => return C_SBD;
         when 955 => return C_XBA;
         when 956 => return C_XBB;
         when 957 => return C_XBC;
         when 958 => return C_XBD;
         when 710 => return C_ZAR;
         when 949 => return C_TRY;
         when 704 => return C_VND;
         when 948 => return C_CHW;
         when 999 => return C_XXX;
         when 400 => return C_JOD;
         when 882 => return C_WST;
         when 262 => return C_DJF;
         when 965 => return C_XUA;
         when 834 => return C_TZS;
         when 776 => return C_TOP;
         when 971 => return C_AFN;
         when 901 => return C_TWD;
         when 144 => return C_LKR;
         when 968 => return C_SRD;
         when 044 => return C_BSD;
         when 702 => return C_SGD;
         when 364 => return C_IRR;
         when 048 => return C_BHD;
         when 484 => return C_MXN;
         when 136 => return C_KYD;
         when 104 => return C_MMK;
         when 566 => return C_NGN;
         when 532 => return C_ANG;
         when 230 => return C_ETB;
         when 426 => return C_LSL;
         when 979 => return C_MXV;
         when 600 => return C_PYG;
         when 012 => return C_DZD;
         when 940 => return C_UYI;
         when 936 => return C_GHS;
         when 931 => return C_CUC;
         when 554 => return C_NZD;
         when 780 => return C_TTD;
         when 348 => return C_HUF;
         when 858 => return C_UYU;
         when 214 => return C_DOP;
         when 643 => return C_RUB;
         when 927 => return C_UYW;
         when 748 => return C_SZL;
         when 192 => return C_CUP;
         when 578 => return C_NOK;
         when 952 => return C_XOF;
         when 938 => return C_SDG;
         when 360 => return C_IDR;
         when 480 => return C_MUR;
         when 706 => return C_SOS;
         when 981 => return C_GEL;
         when 926 => return C_VED;
         when 960 => return C_XDR;
         when 238 => return C_FKP;
         when 188 => return C_CRC;
         when 586 => return C_PKR;
         when others => return C_ZZZ;
      end case;
   end Numeric_To_Key;
end ISO.Currencies;

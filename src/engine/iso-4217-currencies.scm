;; currency descriptions for ISO4217 currencies.
;;
;; This file is not currently used at runtime.  It's used to generate
;; the contents of iso-4217-currencies.c.
;;
;; You can find Currency Information from the following sites:
;;   http://www.evertype.com/standards/iso4217/iso4217-en.html
;;   http://www.xe.com/iso4217.htm
;;   http://www.thefinancials.com/vortex/CurrencyFormats.html
;; But currently used was
;;   http://en.wikipedia.org/wiki/ISO4217 and relatives and the official at
;;   http://www.iso.org/iso/support/faqs/faqs_widely_used_standards/widely_used_standards_other/currency_codes/currency_codes_list-1.htm
;;
;; Learned from some bugs (543061, 564450), please keep in mind:
;; If there are no coins for subunits, subunits might still be in use on the paper
;;
;; Format:
;; (fullname, unitname*, partname*, namespace, mnemonic, exchange-code, parts-per-unit*, smallest-fraction,  local-symbol)
;; planed extensions: replace-date, by-ISO, frozen-rate [x old : 1 new]
;;
;; where * means currently unused;
;; fullname should be the same as in /usr/share/xml/iso-codes/iso_4217.xml from package iso-codes
;; exchange-code is stored in ISIN/CUSIP;
;; see iso-currencies-to-c for details and recent changes.
;; Sort order by ISO codes for simpler maintainance
;;
( "Andorran Franc" "franc" "centime" "ISO4217" "ADF" "950" 100 100 "₣" ) ;; 2002-01-01 "FRF" 1  ;; = 1/1 French Franc replaced by EUR
( "Andorran Peseta" "peseta" "centimo"  "ISO4217" "ADP" "724" 100 100 "₧" ) ;; 2002-01-01 "ESP" 1 ;; = 1/1 Spanish Peseta replaced by EUR
( "UAE Dirham" "dirham" "fil" "ISO4217" "AED" "784" 100 100 "Dhs" ) ;; There is also an arabic symbol for "Dhs"
( "Afghani" "afghani" "pul" "ISO4217" "AFA" "004" 100 100 "" ) ;; through 2003-01-02 "AFN" 1000
( "Afghani" "afghani" "pul" "ISO4217" "AFN" "971" 1 1 "" )
( "Lek" "lek" "qindarka" "ISO4217" "ALL" "008" 100 100 "" )
( "Armenian Dram" "dram" "Luma" "ISO4217" "AMD" "051" 100 100 "դր." )
( "Netherlands Antillian Guilder" "guilder" "cent" "ISO4217" "ANG" "532" 100 100 "NAƒ" ) ;; through 2010-10 ???
( "Kwanza" "kwanza" "cêntimos" "ISO4217" "AOA" "973" 100 100 "Kz" )
( "Angola New Kwanza" "new kwanza" "lwei" "ISO4217" "AON" "024" 100 100 "" ) ;; 1995-07-01 "AOR" 1000
( "Angola Kwanza Reajustado" "reajustado kwanza" "centimos" "ISO4217" "AOR" "982" 100 100 "" ) ;; 2000-02-01 "AOA" 1000000
( "Argentine Austral" "austral" "centavo" "ISO4217" "ARA" "XXX" 100 100 "" ) ;; 1992-01-01 "ARS" 10000
( "Argentine Peso" "peso" "centavo"  "ISO4217" "ARS" "032" 100 100 "" )
( "Austrian Schilling" "shilling" "groschen"  "ISO4217" "ATS" "040" 100 100 "öS" ) ;; 2002-01-01 "EUR" 13.7603
( "Australian Dollar" "dollar" "cent" "ISO4217" "AUD" "036" 100 100 "$" )
( "Aruban Guilder" "florin" "cent" "ISO4217" "AWG" "533" 100 100 "Afl." ) ;; ""
( "Azerbaijanian Manat" "manat" "qəpik" "ISO4217" "AZM" "031" 100 100 "" ) ;; 2006-01-01 "AZN" 5000
( "Azerbaijanian Manat" "manat" "qəpik" "ISO4217" "AZN" "944" 100 100 "m" ) ;; "m" should be  a 90° rotated € sign, but currently not encoded in Unicode
( "Bosnia and Herzegovina Dinar" "B.H. dinar" "para" "ISO4217" "BAD" "070" 100 100 "" ) ;; 1998-06-22 "BAM" 100 ;; there was a revaluation 1994-08: 10000:1
( "Convertible Marks" "B.H. mark" "fening" "ISO4217" "BAM" "977" 100 100 "KM" )
( "Barbados Dollar" "dollar" "cent"  "ISO4217" "BBD" "052" 100 100 "Bds$" )
( "Taka" "taka" "paisa"  "ISO4217" "BDT" "050" 100 100 "৳" )
( "Belgian Franc" "franc" "centime" "ISO4217" "BEF" "056" 100 100 "fr." ) ;; 2002-01-01 "EUR" 40.3399
( "Bulgarian Lev A/99" "lev" "stotinki" "ISO4217" "BGL" "100" 100 100 "" ) ;; 1999-07-05 "BGN" 1000
( "Bulgarian Lev" "lev" "stotinki" "ISO4217" "BGN" "975" 100 100 "лв" ) ;; scheduled replacement: 2012-01-01 "EUR" 1.95583 (=DEM)
( "Bahraini Dinar" "dinar" "fil"  "ISO4217" "BHD" "048" 1000 1000 "BD" ) ;; ar:.د.ب
( "Burundi Franc" "franc" "centime" "ISO4217" "BIF" "108" 100 100 "FBu" )
( "Bermudian Dollar" "dollar" "cent" "ISO4217" "BMD" "060" 100 100 "BD$" )
( "Brunei Dollar" "dollar" "sen" "ISO4217" "BND" "096" 100 100 "B$" )
( "Boliviano" "boliviano"  "centavo" "ISO4217" "BOB" "068" 100 100 "Bs." )
( "Mvdol" "mvdol"  "centavo" "ISO4217" "BOV" "984" 100 100 "" ) ;; Mantenimiento de Valor respecto al Dólar
( "Brazilian Cruzeiro" "cruzeiro" "centavo" "ISO4217" "BRE" "076" 100 100 "" ) ;; 1993-08-01 "BRR" 1000
( "Brazilian Real" "real" "centavo"  "ISO4217" "BRL" "986" 100 100 "R$" )
( "Brazilian Cruzeiro Real" "cruzeiro" "centavo" "ISO4217" "BRR" "987" 100 100 "" ) ;; 1994-07-01 "BRL" 2750
( "Bahamian Dollar" "dollar" "cent"  "ISO4217" "BSD" "044" 100 100 "B$" )
( "Ngultrum" "ngultrum" "chetrum" "ISO4217" "BTN" "064" 100 100 "Nu." )
( "Pula" "pula" "thebe" "ISO4217" "BWP" "072" 100 100 "P" )
( "Belarussian Rouble" "ruble" "kapeyka" "ISO4217" "BYB" "" 100 1 "" ) ;; 2000-01-01 "BYR" 1000
( "Belarussian Ruble" "ruble" "ruble" "ISO4217" "BYR" "974" 1 100 "Br" )
( "Belize Dollar" "dollar" "cent" "ISO4217" "BZD" "084" 100 100 "BZ$" )
( "Canadian Dollar" "dollar" "cent" "ISO4217" "CAD" "124" 100 100 "C$" )
( "Franc Congolais" "franc" "centime" "ISO4217" "CDF" "976" 100 100 "FC" )
( "WIR Euro" "euro" "cent" "ISO4217" "CHE" "974" 100 100 "" ) ;; complementary currency by WIR Wirtschaftsring-Genossenschaft
( "Swiss Franc" "franc" "centime" "ISO4217" "CHF" "756" 100 100 "SFr." )
( "WIR Franc" "franc" "centime" "ISO4217" "CHW" "948" 100 100 "" ) ;; complementary currency by WIR Wirtschaftsring-Genossenschaft
( "Unidades de fomento" "UF" "" "ISO4217" "CLF" "990" 1 1 "" ) ;; international secured loans
( "Chilean Peso" "peso" "centavo" "ISO4217" "CLP" "152" 100 1 "$" ) ;; "$" should have 2 strokes
( "Yuan Renminbi" "renminbi" "fen" "ISO4217" "CNY" "156" 100 100 "元" )
( "Colombian Peso" "peso" "centavo" "ISO4217" "COP" "170" 100 100 "$" )
( "Unidad de Valor Real" "UVR" "???" "ISO4217" "COU" "970" 100 100 "" ) ;;
( "Costa Rican Colon" "colon" "centimo" "ISO4217" "CRC" "188" 100 100 "₡" )
( "Cuban Peso" "peso" "centavo" "ISO4217" "CUP" "192" 100 100 "$MN" )
( "Cuban Convertible Peso" "peso" "centavo" "ISO4217" "CUC" "931" 100 100 "CUC$" )
( "Cape Verde Escudo" "escudo" "centavo" "ISO4217" "CVE" "132" 100 100 "Esc" )
( "Cyprus Pound" "pound" "pence"  "ISO4217" "CYP" "196" 100 100 "£" ) ;; 2008-01-01 "EUR" 0.585274
( "Czech Koruna" "koruna" "haleru" "ISO4217" "CZK" "203" 100 100  "Kč" )
( "Deutsche Mark" "deutschemark" "pfennig" "ISO4217" "DEM" "280" 100 100 "DM" ) ;; 2002-01-01 "EUR" 1.95583
( "Djibouti Franc" "franc" "centime" "ISO4217" "DJF" "262" 100 1 "Fdj" )
( "Danish Krone" "krone" "øre" "ISO4217" "DKK" "208" 100 100 "kr" )
( "Dominican Peso" "peso" "centavo"  "ISO4217" "DOP" "214" 100 100 "RD$" )
( "Algerian Dinar" "dinar" "santeem"  "ISO4217" "DZD" "012" 100 100 "DA" ) ;; ar.: 	دج
( "Ecuador Sucre" "sucre" "centavo" "ISO4217" "ECS" "218" 100 100 "S/." ) ;; 2000-09-15 "USD" 25000
( "Kroon" "kroon" "sent" "ISO4217" "EEK" "233" 100 100 "kr" )
( "Egyptian Pound" "pound" "qirsh"  "ISO4217" "EGP" "818" 100 100 "£E" ) ;; ar.:ج.م maleem=0.001 £E for accounting?
( "Nakfa" "nakfa" "cent" "ISO4217" "ERN" "232" 100 100 "Nfa" )
( "Spanish Peseta" "peseta" "centimo"  "ISO4217" "ESP" "724" 100 100 "₧" ) ;; 2002-01-01 "EUR" 166.386
( "Ethiopian Birr" "birr" "santim" "ISO4217" "ETB" "230" 100 100 "Br" )
( "Euro" "euro" "euro-cent" "ISO4217" "EUR" "978" 100 100 "€" )
( "Finnish Markka" "markka" "penni"  "ISO4217" "FIM" "246" 100 100 "mk" ) ;; 2002-01-01 "EUR" 5.94573
( "Fiji Dollar" "dollar" "cent" "ISO4217" "FJD" "242" 100 100 "FJ$" )
( "Falkland Islands Pound" "pound" "pence" "ISO4217" "FKP" "238" 100 100 "FK£" )
( "French Franc" "franc" "centime" "ISO4217" "FRF" "250" 100 100 "₣" ) ;; 2002-01-01 "EUR" 6.55957
( "Pound Sterling" "pound" "pence" "ISO4217" "GBP" "826" 100 100 "£" )
( "Lari" "lari" "tetri" "ISO4217" "GEL" "981" 100 100 "" )
( "Cedi" "cedi" "pesewa" "ISO4217" "GHC" "288" 100 100 "" ) ;; 2007-07-01 "GHS" 10000
( "Ghana Cedi" "cedi" "pesewa" "ISO4217" "GHS" "936" 100 100 "GH₵" )
( "Gibraltar Pound" "pound" "pence"  "ISO4217" "GIP" "292" 100 100 "£" )
( "Dalasi" "dalasi" "butut" "ISO4217" "GMD" "270" 100 100 "D" )
( "Guinea Franc" "franc" "centime" "ISO4217" "GNF" "324" 100 100 "FG" )
( "Greek Drachma" "drachma" "lepta" "ISO4217" "GRD" "200" 100 100 "Δρ." ) ;; 2002-01-01 "EUR" 340.750
( "Quetzal" "quetzal" "centavo" "ISO4217" "GTQ" "320" 100 100 "Q" )
( "Guinea-Bissau Peso" "peso" "centavo" "ISO4217" "GWP" "624" 100 100"" ) ;; 1997-01-01 "XOF" 65
( "Guyana Dollar" "dollar" "cent" "ISO4217" "GYD" "328" 100 100 "G$" )
( "Hong Kong Dollar" "dollar" "cent"  "ISO4217" "HKD" "344" 100 100 "HK$" )
( "Lempira" "lempira" "centavo"  "ISO4217" "HNL" "340" 100 100 "L" )
( "Croatian Kuna" "kuna" "lipa" "ISO4217" "HRK" "191" 100 100 "kn" )
( "Gourde" "gourde" "centime"  "ISO4217" "HTG" "332" 100 100 "G" )
( "Forint" "forint" "fillér" "ISO4217" "HUF" "348" 1 100 "Ft" )
( "Rupiah" "rupiah" "sen" "ISO4217" "IDR" "360" 1 100 "Rp" )
( "Irish Pound" "punt" "pingin" "ISO4217" "IEP" "372" 100 100 "£" ) ;; 2002-01-01 "EUR" 0.787564
( "New Israeli Sheqel" "new shekel" "agora"  "ISO4217" "ILS" "376" 100 100 "₪" )
( "Indian Rupee" "rupee" "paisa" "ISO4217" "INR" "356" 100 100 "रू" )
( "Iraqi Dinar" "dinar" "fil"  "ISO4217" "IQD" "368" 1000 1000 "ع.د" )
( "Iranian Rial" "rial" "dinar" "ISO4217" "IRR" "364" 1 1 "﷼﷼" )
( "Iceland Krona" "krona" "aur" "ISO4217" "ISK" "352" 1 100 "kr" )
( "Italian Lira" "lira" "lira" "ISO4217" "ITL" "380" 1 1 "₤" ) ;; 2002-01-01 "EUR" 1936.27
( "Jamaican Dollar" "dollar" "cent" "ISO4217" "JMD" "388" 100 100 "J$" )
( "Jordanian Dinar" "dinar" "fil"  "ISO4217" "JOD" "400" 1000 1000 "JD" )
( "Yen" "yen" "sen"  "ISO4217" "JPY" "392" 100 1 "¥" )
( "Kenyan Shilling" "shilling" "cent" "ISO4217" "KES" "404" 100 100 "Ksh" )
( "Som" "som" "tyiyn" "ISO4217" "KGS" "417" 100 100 "" )
( "Riel" "riel" "sen" "ISO4217" "KHR" "116" 100 100 "" )
( "Comoro Franc" "franc" "centime" "ISO4217" "KMF" "174" 100 1  "FC" )
( "North Korean Won" "won" "chon" "ISO4217" "KPW" "408" 100 100 "₩" )
( "Won" "won" "chon"  "ISO4217" "KRW" "410" 1 100 "₩" )
( "Kuwaiti Dinar" "dinar" "fils"  "ISO4217" "KWD" "414" 1000 1000 "د.ك" )
( "Cayman Islands Dollar" "dollar" "cent"  "ISO4217" "KYD" "136" 100 100 "CI$" )
( "Tenge" "tenge" "tiyn" "ISO4217" "KZT" "398" 100 100 "₸" )
( "Kip" "kip" "att" "ISO4217" "LAK" "418" 100 100 "₭" )
( "Lebanese Pound" "pound" "piastre"  "ISO4217" "LBP" "422" 100 100 "ل.ل" )
( "Sri Lanka Rupee" "rupee" "cent"  "ISO4217" "LKR" "144" 100 100 "₨" )
( "Liberian Dollar" "dollar" "cent" "ISO4217" "LRD" "430" 100 100 "L$" )
( "Loti" "loti" "sente" "ISO4217" "LSL" "426" 100 100 "M" )
( "Lithuanian Litas" "litas" "centas" "ISO4217" "LTL" "440" 100 100 "Lt" )
( "Luxembourg Franc" "frang" "centime" "ISO4217" "LUF" "442" 100 100  "Flux" ) ;; 2002-01-01 "EUR" 40.3399
( "Latvian Lats" "lats" "santīms" "ISO4217" "LVL" "428" 100 100 "Ls" )
( "Libyan Dinar" "dinar" "dirham" "ISO4217" "LYD" "434" 1000 1000 "ل.د" )
( "Moroccan Dirham" "dirham" "centime"  "ISO4217" "MAD" "504" 100 100 "د.م" )
( "Moldovan Leu" "leu" "ban" "ISO4217" "MDL" "498" 100 100 "" )
( "Malagasy Ariary" "ariary" "iraimbilanja" "ISO4217" "MGA" "969" 5 5 "" )
( "Malagasy Franc" "franc" "centime" "ISO4217" "MGF" "450" 500 500 "" ) ;; 2003-07-31 "MGA" 5
( "Denar" "denar" "deni" "ISO4217" "MKD" "807" 100 100 "ден" )
( "Mali Franc" "franc" "centime" "ISO4217" "MLF" "466" 100 100 "" ) ;; 1984-07-01 "XOF" 2
( "Kyat" "kyat" "pya" "ISO4217" "MMK" "104" 100 100 "K" )
( "Tugrik" "tugrik" "mongo" "ISO4217" "MNT" "496" 100 100 "₮" )
( "Pataca" "pataca" "avo"  "ISO4217" "MOP" "446" 100 100 "MOP$" )
( "Ouguiya" "ouguiya" "khoum"  "ISO4217" "MRO" "478" 5 5 "UM" )
( "Maltese Lira" "lira" "cent"  "ISO4217" "MTL" "470" 100 100 "Lm" ) ;; 2008-01-01 "EUR" 0.4293
( "Mauritius Rupee" "rupee" "cent"  "ISO4217" "MUR" "480" 100 100 "R" )
( "Rufiyaa" "rufiyaa" "laari" "ISO4217" "MVR" "462" 100 100 ".ރ" )
( "Kwacha" "kwacha" "tambala"  "ISO4217" "MWK" "454" 100 100 "MK" )
( "Mexican Peso" "peso" "centavo" "ISO4217" "MXN" "484" 100 100 "Mex$" ) ;;since Jan 1993 (1000 MXP = 1 MXN)
( "Mexican Unidad de Inversion (UDI)" "UDI" "centavo" "ISO4217" "MXV" "979" 100 100 "" ) ;;fund index based; used for credits, not subject to inflation
( "Malaysian Ringgit" "ringgit" "sen"  "ISO4217" "MYR" "458" 100 100 "RM" )
( "Mozambique Metical" "metical" "centavo" "ISO4217" "MZM" "508" 100 100 "" ) ;; 2006-07-01 "MZN" 1000
( "Metical" "metical" "centavo" "ISO4217" "MZN" "943" 100 100 "MTn" )
( "Namibia Dollar" "dollar" "cent" "ISO4217" "NAD" "516" 100 100 "N$" )
( "Naira" "naira" "kobo"  "ISO4217" "NGN" "566" 100 100 "₦" )
( "Nicaraguan Cordoba" "cordoba" "centavo" "ISO4217" "NIC" "558" 100 100 "" ) ;; 1990-10-13 "NIO" 5000000
( "Cordoba Oro" "cordoba" "centavo" "ISO4217" "NIO" "558" 100 100 "C$" )
( "Netherlands Guilder" "guilder" "cent" "ISO4217" "NLG" "528" 100 100 "" ) ;; 2002-01-01 "EUR" 2.20371
( "Norwegian Krone" "krone" "ore"  "ISO4217" "NOK" "578" 100 100 "kr" )
( "Nepalese Rupee" "rupee" "paise" "ISO4217" "NPR" "524" 100 100 "₨" )
( "New Zealand Dollar" "dollar" "cent" "ISO4217" "NZD" "554" 100 100 "NZ$" )
( "Rial Omani" "rial" "baisa" "ISO4217" "OMR" "512" 1000 1000 "ر.ع." )
( "Balboa" "balboa" "centésimo" "ISO4217" "PAB" "590" 100 100 "฿" )
( "Nuevo Sol" "nuevo sol" "centimo"  "ISO4217" "PEN" "604" 100 100 "S/." )
( "Kina" "kina" "toea" "ISO4217" "PGK" "598" 100 100 "K" )
( "Philippine Peso" "peso" "centavo" "ISO4217" "PHP" "608" 100 100 "₱" )
( "Pakistan Rupee" "rupee" "paisa" "ISO4217" "PKR" "586" 100 100 "Rs" )
( "Zloty" "zloty" "grosz" "ISO4217" "PLN" "985" 100 100 "zł" )
( "Portuguese Escudo" "escudo" "centavo" "ISO4217" "PTE" "620" 100 100 "$" ) ;; 2002-01-01 "EUR" 200.482
( "Guarani" "guarani" "centimo" "ISO4217" "PYG" "600" 100 100 "₲" ) ;; scheduled revaluation 2011
( "Qatari Rial" "rial" "dirham" "ISO4217" "QAR" "634" 100 100 "ر.ق" )
( "Romanian Old Leu" "leu" "ban"  "ISO4217" "ROL" "642" 100 100 "" ) ;; 2005-07-01 "RON" 10000
( "New Leu" "leu" "ban"  "ISO4217" "RON" "946" 100 100 "" ) ;; scheduled 2012..14: EUR
( "Serbian Dinar" "dinar" "para"  "ISO4217" "RSD"  "941" 100 100 "" )
( "Russian Rouble" "rouble" "kopek" "ISO4217" "RUB" "643" 100 100 "руб" ) ;; RUR: 1998-01-1 "RUB" 1000; see bug #393185
( "Rwanda Franc" "franc" "centime" "ISO4217" "RWF" "646" 100 100 "RF" )
( "Saudi Riyal" "riyal" "halala"  "ISO4217" "SAR" "682" 100 100 "ر.س" )
( "Solomon Islands Dollar" "dollar" "cent"  "ISO4217" "SBD" "090" 100 100 "SI$" )
( "Seychelles Rupee" "rupee" "cent" "ISO4217" "SCR" "690" 100 100 "SR" )
( "Sudanese Dinar" "dinar" "piastre"  "ISO4217" "SDD" "736" 100 100 "" ) ;; 2007-07-01 "SDG" 100, reunite with South
( "Sudanese Pound" "pound" "qirsh"  "ISO4217" "SDG" "938" 100 100 "" )
( "Sudanese Pound" "pound" "piastre"  "ISO4217" "SDP" "736" 100 100 "" ) ;; 1992-01-01 "SDD" 10, but only in the North
( "Swedish Krona" "krona" "ore"  "ISO4217" "SEK" "752" 100 100 "kr" )
( "Singapore Dollar" "dollar" "cent" "ISO4217" "SGD" "702" 100 100 "S$" )
( "Saint Helena Pound" "pound" "penny"  "ISO4217" "SHP"  "654" 100 100 "£" )
( "Slovenian Tolar" "tolar" "stotin"  "ISO4217" "SIT" "705" 100 100 "" ) ;; 2007-01-01 "EUR" 239.640
( "Slovak Koruna" "koruna" "halier"  "ISO4217" "SKK" "703" 100 100 "" ) ;; 2009-01-01 "EUR" 30.126
( "Leone" "leone" "cent"  "ISO4217" "SLL" "694" 100 100 "Le" )
( "Somali Shilling" "shilling" "centisimi" "ISO4217" "SOS" "706" 100 100 "SoSh" )
( "Surinam Dollar" "dollar" "cent"  "ISO4217" "SRD" "968" 100 100 "$" )
( "Suriname Guilder" "guilder" "cent"  "ISO4217" "SRG" "740" 100 100 "" ) ;; 2004-01-01 "SRD" 1000
( "Dobra" "dobra" "centimo" "ISO4217" "STD" "678" 100 100 "Db" )
( "El Salvador Colon" "colon" "centavo" "ISO4217" "SVC" "222" 100 100 "" ) ;; 2001-01-01 "USD" 8.75
( "Syrian Pound" "pound" "qirsh"  "ISO4217" "SYP" "760" 100 100 "" )
( "Lilangeni" "lilangeni" "cent"  "ISO4217" "SZL" "748" 100 100 "L" )
( "Baht" "baht" "satang" "ISO4217" "THB" "764" 100 100 "฿" )
( "Tajik Rouble" "ruble" "ruble" "ISO4217" "TJR" "762" 1 1 "" ) ;; 2002-11-01 "TJS" 1000
( "Somoni" "somoni" "diram" "ISO4217" "TJS" "972" 100 100 "" )
( "Manat" "manat" "tenga" "ISO4217" "TMM" "795" 100 100 "" ) ;; 2009-01-01 "TMT" 5000
( "Manat" "manat" "teňňe" "ISO4217" "TMT" "934" 100 100 "m" )
( "Tunisian Dinar" "dinar" "milim" "ISO4217" "TND" "788" 1000 1000 "د.ت" )
( "Pa'anga" "Pa'anga" "seniti" "ISO4217" "TOP" "776" 100 100 "T$" )
( "Turkish Lira" "lira" "kuruş" "ISO4217" "TRY" "949" 100 100 "₤" )
( "Trinidad and Tobago Dollar" "dollar" "cent" "ISO4217" "TTD" "780" 100 100 "TT$" )
( "New Taiwan Dollar" "dollar" "cent" "ISO4217" "TWD" "901" 100 100 "NT$" )
( "Tanzanian Shilling" "shilling" "senti"  "ISO4217" "TZS" "834" 100 100 "/" )
( "Hryvnia" "hryvnia" "kopiyka"  "ISO4217" "UAH" "980" 100 100 "₴" )
( "Uganda Shilling" "shilling" "cent"  "ISO4217" "UGX" "800" 100 100  "USh" )
( "US Dollar" "dollar" "cent" "ISO4217" "USD" "840" 100 100 "$" )
( "US Dollar (Next day)" "dollar" "cent" "ISO4217" "USN" "997" 100 100 "$n" ) ;; funds code
( "US Dollar (Same day)" "dollar" "cent" "ISO4217" "USS" "998" 100 100 "$s" ) ;; funds code
( "Uruguay Peso en Unidades Indexadas" "UI" "centesimo" "ISO4217" "UYI" "940" 100 100 "UI" )
( "Peso Uruguayo" "peso" "centesimo" "ISO4217" "UYU" "858" 100 100 "$U" )
( "Uzbekistan Sum" "so‘m" "tiyin" "ISO4217" "UZS" "860" 100 100 "som" )
( "Venezuela Bolívar" "bolivar" "centimo" "ISO4217" "VEB" "862" 100 100 "" ) ;; 2008-01-01 "VEF" 1000
( "Bolivar Fuerte" "bolivar" "centimo" "ISO4217" "VEF" "937" 100 100 "Bs." )
( "Dong" "đồng" "xu" "ISO4217" "VND" "704" 100 100 "₫" )
( "Vatu" "vatu" "centime" "ISO4217" "VUV" "548" 1 1 "Vt" )
( "Tala" "tala" "sene" "ISO4217" "WST" "882" 100 100 "WS$" )
( "Yemeni Rial" "riyal" "fils" "ISO4217" "YER" "886" 100 100 "Rl" )
( "Yugoslavian Dinar" "dinar" "para"  "ISO4217" "YUM"  "890" 100 100 "" ) ;; 2003 replaced by RSD 1
( "Rand" "rand" "cent" "ISO4217" "ZAR" "710" 100 100 "R" )
( "Kwacha" "kwacha" "ngwee"  "ISO4217" "ZMK" "894" 100 100 "ZK" )
( "Zimbabwe Dollar" "dollar" "cent" "ISO4217" "ZWD" "716" 100 100 "" ) ;; 2006-08-01 "ZWN" 1000, 2008-08-01 "ZWR" 10000000000, 2009-02-01 "ZWD" 1000000000000
( "Zimbabwe Dollar" "dollar" "cent" "ISO4217" "ZWL" "716" 100 100 "Z.$" ) ;; 2009-04 suspended for at least 1 year

;; multinational
( "CFA Franc BEAC" "franc" "centime" "ISO4217" "XAF" "950" 1 100 "" ) ;; Banque des États de l'Afrique Centrale
;; XB* should be replaced by EUR, if I am right
( "East Caribbean Dollar" "dollar" "cent" "ISO4217" "XCD" "951" 100 100 "EC$" ) ;; Organisation of Eastern Caribbean States
( "SDR" "SDR" "SDR" "ISO4217" "XDR" "960" 1 1 "" ) ;; International Monetary Funds Special Drawing Rights
( "Gold-Franc" "franc" "centime" "ISO4217" "XFO" "nil" 1 100 "" ) ;; Bank for International Settlements
( "UIC-Franc" "franc" "centime" "ISO4217" "XFU" "nil" 1 100 "" ) ;; Union Internationale des Chemins de fer
( "CFA Franc BCEAO" "franc" "centime" "ISO4217" "XOF" "952" 1 100 "" ) ;; Banque Centrale des États de l'Afrique de l'Ouest
( "CFP Franc" "franc" "centime" "ISO4217" "XPF" "953" 1 100 "" ) ;; Communauté Financière du Pacifique
( "Code for testing purposes" "TEST" "test" "ISO4217" "XTS" "963" 1 1000000 "" ) ;; Code reserved for testing purposes
( "No currency" "" "" "ISO4217" "XXX" "999" 1 1000000 "" )

;; precious metals
( "Silver" "ounce" "ounce" "ISO4217" "XAG" "961" 1 1000000 "" )
( "Gold" "ounce" "ounce" "ISO4217" "XAU" "959" 1 1000000 "" )
( "Palladium" "ounce" "ounce" "ISO4217" "XPD" "964" 1 1000000 "" )
( "Platinum" "ounce" "ounce" "ISO4217" "XPT" "962" 1 1000000 "" )

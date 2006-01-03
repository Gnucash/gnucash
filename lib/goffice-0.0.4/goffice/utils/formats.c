/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * formats.c: The default formats supported in Gnumeric
 *
 * For information on how to translate these format strings properly,
 * refer to the doc/translating.sgml file in the Gnumeric distribution.
 *
 * Author:
 *    Miguel de Icaza (miguel@kernel.org)
 */
#include <goffice/goffice-config.h>
#include "go-format.h"
#include <goffice/cut-n-paste/pcre/pcreposix.h>
#include <glib/gi18n.h>
#include <string.h>

/* The various formats */
static char const * const
fmts_general [] = {
	"General",
	NULL
};

static char const * const
fmts_number [] = {
	"0",
	"0.00",
	"#,##0",
	"#,##0.00",
	"#,##0_);(#,##0)",
	"#,##0_);[Red](#,##0)",
	"#,##0.00_);(#,##0.00)",
	"#,##0.00_);[Red](#,##0.00)",
	"0.0",
	NULL
};

/* Some are generated */
static char const *
fmts_currency [] = {
	NULL, /* "$#,##0", */
	NULL, /* "$#,##0_);($#,##0)", */
	NULL, /* "$#,##0_);[Red]($#,##0)", */
	NULL, /* "$#,##0.00", */
	NULL, /* "$#,##0.00_);($#,##0.00)", */
	NULL, /* "$#,##0.00_);[Red]($#,##0.00)", */
	NULL,
};

/* Some are generated */
static char const *
fmts_accounting [] = {
	NULL, /* "_($* #,##0_);_($* (#,##0);_($* \"-\"_);_(@_)", */
	"_(* #,##0_);_(* (#,##0);_(* \"-\"_);_(@_)",
	NULL, /* "_($* #,##0.00_);_($* (#,##0.00);_($* \"-\"??_);_(@_)", */
	"_(* #,##0.00_);_(* (#,##0.00);_(* \"-\"??_);_(@_)",
	NULL
};

/*****************************************************************/
/* Some are generated						 */
/* WARNING WARNING WARNING : do not reorder these !! 		 */
/* the generated versions and the excel plugin assume this order */
static char const *
fmts_date [] = {
	"m/d/yy",		/* 0 */
	"m/d/yyyy",		/* 1 */
	"d-mmm-yy",		/* 2 */
	"d-mmm-yyyy",		/* 3 */
	"d-mmm",		/* 4 */
	"d-mm",			/* 5 */
	"mmm/d",		/* 6 */
	"mm/d",			/* 7 */
	"mm/dd/yy",		/* 8 */
	"mm/dd/yyyy",		/* 9 */
	"mmm/dd/yy",		/* 10 */
	"mmm/dd/yyyy",		/* 11 */
	"mmm/ddd/yy",		/* 12 */
	"mmm/ddd/yyyy",		/* 13 */
	"mm/ddd/yy",		/* 14 */
	"mm/ddd/yyyy",		/* 15 */
	"mmm-yy",		/* 16 */
	"mmm-yyyy",		/* 17 */
	"mmmm-yy",		/* 18 */
	"mmmm-yyyy",		/* 19 */
	"m/d/yy h:mm",		/* 20 */
	"m/d/yyyy h:mm",	/* 21 */
	"yyyy/mm/d",		/* 22 */
	"yyyy/mmm/d",		/* 23 */
	"yyyy/mm/dd",		/* 24 */
	"yyyy/mmm/dd",		/* 25 */
	"yyyy-mm-d",		/* 26 */
	"yyyy-mmm-d",		/* 27 */
	"yyyy-mm-dd",		/* 28 */
	"yyyy-mmm-dd",		/* 29 */
	"yy",			/* 30 */
	"yyyy",			/* 31 */
	"mmmmm",		/* 32 stored as custom in xls */
	"mmmmm-yy",		/* 33 stored as custom in xls */
	NULL
};

/*****************************************************/

/* Some are generated */
static char const *
fmts_time [] = {
	"h:mm AM/PM",
	"h:mm:ss AM/PM",
	"h:mm",
	"h:mm:ss",
	"m/d/yy h:mm",
	"[h]:mm",		/* keep this before mm:ss so that 24:00 will */
	"[h]:mm:ss",		/* match an hour based format (bug #86338)   */
	"mm:ss",
	"mm:ss.0",
	"[mm]:ss",
	"[ss]",
	NULL
};

static char const * const
fmts_percentage [] = {
	"0%",
	"0.00%",
	NULL,
};

static char const * const
fmts_fraction [] = {
	"# ?/?",
	"# ?" "?/?" "?",  /* Don't accidentally use trigraph.  */
	"# ?" "?" "?/?" "?" "?",
	"# ?/2",
	"# ?/4",
	"# ?/8",
	"# ?/16",
	"# ?/10",
	"# ?/100",
	NULL
};

static char const * const
fmts_science [] = {
	"0.00E+00",
	"##0.0E+0",
	NULL
};

static char const *
fmts_text [] = {
	"@",
	NULL,
};

char const * const * const
go_format_builtins [] = {
	fmts_general,
	fmts_number,
	fmts_currency,
	fmts_accounting,
	fmts_date,
	fmts_time,
	fmts_percentage,
	fmts_fraction,
	fmts_science,
	fmts_text,
	NULL
};

/* The compiled regexp for go_format_classify */
static GORegexp re_simple_number;
static GORegexp re_red_number;
static GORegexp re_brackets_number;
static GORegexp re_percent_science;
static GORegexp re_account;
static GORegexp re_fraction;

static const char *
my_regerror (int err, const GORegexp *preg)
{
	static char buffer[1024];
	go_regerror (err, preg, buffer, sizeof (buffer));
	return buffer;
}

void
currency_date_format_init (void)
{
	gboolean precedes, space_sep;
	char const *curr = format_get_currency (&precedes, &space_sep)->str;
	char *pre, *post, *pre_rep, *post_rep;
	int err;

	/* Compile the regexps for format classification */

	/* This one is for simple numbers - it is an extended regexp */
	char const * const simple_number_pattern = "^(\"?(\\$|£|¥|€|\\[\\$.{1,3}-?[0-9]{0,3}\\])\"? ?)?(#,##)?0(\\.0{1,30})?( ?(\\$|£|¥|€|\\[\\$.{1,3}-?[0-9]{0,3}\\]))?$";

	/* This one is for matching formats like 0.00;[Red]0.00 */
	char const * const red_number_pattern = "^(.*);\\[[Rr][Ee][Dd]\\]\\1$";

	/* This one is for matching formats like 0.00_);(0.00) */
	char const * const brackets_number_pattern = "^(.*)_\\);(\\[[Rr][Ee][Dd]\\])?\\(\\1\\)$";

	/* This one is for GO_FORMAT_PERCENTAGE and GO_FORMAT_SCIENTIFIC, extended regexp */
	char const *pattern_percent_science = "^0(.0{1,30})?(%|E+00)$";

	char const *pattern_fraction = "^#\\\\? (\\?+)/(\\?+|[1-9]\\d*)$";

	/* This one is for GO_FORMAT_ACCOUNTING */

	/*
	 *  1. "$*  "	Yes, it is needed (because we use match[])
	 *  2. "$*  "	This one is like \1, but doesn't have the \{0,1\}
	 *  3. "$"
	 *  4. "#,##0.00"
	 *  5. ".00"
	 *  6. "*  $"	Same here.
	 *  7. "*  $"
	 *  8. "$"
	 */

	char const *pattern_account = "^_\\((((.*)\\*  ?)?)(#,##0(\\.0{1,30})?)((\\*  ?(.*))?)_\\);_\\(\\1\\(\\4\\)\\6;_\\(\\1\"-\"\\?{0,30}\\6_\\);_\\(@_\\)$";


	err = go_regcomp (&re_simple_number, simple_number_pattern, 0);
	if (err)
		g_warning ("Error in regcomp() for simple number, please report the bug [%s] [%s]",
			   my_regerror (err, &re_simple_number), simple_number_pattern);

	err = go_regcomp (&re_red_number, red_number_pattern, 0);
	if (err)
		g_warning ("Error in regcomp() for red number, please report the bug [%s] [%s]",
			   my_regerror (err, &re_red_number), red_number_pattern);

	err = go_regcomp (&re_brackets_number, brackets_number_pattern, 0);
	if (err)
		g_warning ("Error in regcomp() for brackets number, please report the bug [%s] [%s]",
			   my_regerror (err, &re_brackets_number), brackets_number_pattern);

	err = go_regcomp (&re_percent_science, pattern_percent_science, 0);
	if (err)
		g_warning ("Error in regcomp() for percent and science, please report the bug [%s] [%s]",
			  my_regerror (err, &re_percent_science), pattern_percent_science);

	err = go_regcomp (&re_fraction, pattern_fraction, 0);
	if (err)
		g_warning ("Error in regcomp() for fraction, please report the bug [%s] [%s]",
			  my_regerror (err, &re_fraction), pattern_fraction);

	err = go_regcomp (&re_account, pattern_account, 0);
	if (err)
		g_warning ("Error in regcomp() for account, please report the bug [%s] [%s]",
			   my_regerror (err, &re_account), pattern_account);

	if (precedes) {
		post_rep = post = (char *)"";
		pre_rep = (char *)"* ";
		pre = g_strconcat ("\"", curr,
				   (space_sep ? "\" " : "\""), NULL);
	} else {
		pre_rep = pre = (char *)"";
		post_rep = (char *)"* ";
		post = g_strconcat ((space_sep ? " \"" : "\""),
				    curr, "\"", NULL);
	}

	fmts_currency [0] = g_strdup_printf (
		"%s#,##0%s",
		pre, post);
	fmts_currency [1] = g_strdup_printf (
		"%s#,##0%s_);(%s#,##0%s)",
		pre, post, pre, post);
	fmts_currency [2] = g_strdup_printf (
		"%s#,##0%s_);[Red](%s#,##0%s)",
		pre, post, pre, post);
	fmts_currency [3] = g_strdup_printf (
		"%s#,##0.00%s",
		pre, post);
	fmts_currency [4] = g_strdup_printf (
		"%s#,##0.00%s_);(%s#,##0.00%s)",
		pre, post, pre, post);
	fmts_currency [5] = g_strdup_printf (
		"%s#,##0.00%s_);[Red](%s#,##0.00%s)",
		pre, post, pre, post);

	fmts_accounting [0] = g_strdup_printf (
		"_(%s%s#,##0%s%s_);_(%s%s(#,##0)%s%s;_(%s%s\"-\"%s%s_);_(@_)",
		pre, pre_rep, post_rep, post,
		pre, pre_rep, post_rep, post,
		pre, pre_rep, post_rep, post);
	fmts_accounting [2] = g_strdup_printf (
		"_(%s%s#,##0.00%s%s_);_(%s%s(#,##0.00)%s%s;_(%s%s\"-\"??%s%s_);_(@_)",
		pre, pre_rep, post_rep, post,
		pre, pre_rep, post_rep, post,
		pre, pre_rep, post_rep, post);

	g_free (*pre ? pre : post);

	if (!format_month_before_day ()) {
		fmts_date [0]  = "d/m/yy";
		fmts_date [1]  = "d/m/yyyy";
		fmts_date [2]  = "mmm-d-yy";
		fmts_date [3]  = "mmm-d-yyyy";
		fmts_date [4]  = "mmm-d";
		fmts_date [5]  = "mm-d";
		fmts_date [6]  = "d/mmm";
		fmts_date [7]  = "d/mm";
		fmts_date [8]  = "dd/mm/yy";
		fmts_date [9]  = "dd/mm/yyyy";
		fmts_date [10] = "dd/mmm/yy";
		fmts_date [11] = "dd/mmm/yyyy";
		fmts_date [12] = "ddd/mmm/yy";
		fmts_date [13] = "ddd/mmm/yyyy";
		fmts_date [14] = "ddd/mm/yy";
		fmts_date [15] = "ddd/mm/yyyy";
		fmts_date [20] = "d/m/yy h:mm";
		fmts_date [21] = "d/m/yyyy h:mm";

		fmts_time [4]  = "d/m/yy h:mm";
	} else {
		fmts_date [0]  = "m/d/yy";
		fmts_date [1]  = "m/d/yyyy";
		fmts_date [2]  = "d-mmm-yy";
		fmts_date [3]  = "d-mmm-yyyy";
		fmts_date [4]  = "d-mmm";
		fmts_date [5]  = "d-mm";
		fmts_date [6]  = "mmm/d";
		fmts_date [7]  = "mm/d";
		fmts_date [8]  = "mm/dd/yy";
		fmts_date [9]  = "mm/dd/yyyy";
		fmts_date [10] = "mmm/dd/yy";
		fmts_date [11] = "mmm/dd/yyyy";
		fmts_date [12] = "mmm/ddd/yy";
		fmts_date [13] = "mmm/ddd/yyyy";
		fmts_date [14] = "mm/ddd/yy";
		fmts_date [15] = "mm/ddd/yyyy";
		fmts_date [20] = "m/d/yy h:mm";
		fmts_date [21] = "m/d/yyyy h:mm";

		fmts_time [4]  = "m/d/yy h:mm";
	}
}

void
currency_date_format_shutdown (void)
{
	/* We need to free allocated strings since */
	/* currency_date_format_init/shutdown may  */
	/* be called repeatedly by the format selector */
	/* when switching locales */

	int i;

	for (i = 0; i < 6; i++) {
		g_free ((char *)(fmts_currency [i]));
		fmts_currency [i] = NULL;
	}
	g_free ((char *)(fmts_accounting [0]));
	fmts_accounting [0] = NULL;
	g_free ((char *)(fmts_accounting [2]));
	fmts_accounting [3] = NULL;

	go_regfree (&re_simple_number);
	go_regfree (&re_red_number);
	go_regfree (&re_brackets_number);
	go_regfree (&re_percent_science);
	go_regfree (&re_account);
	go_regfree (&re_fraction);
}

#define EURO_FORM_1 4

GOFormatCurrency const go_format_currencies[] =
{
 	{ "", "None", TRUE, FALSE },	/* These first six elements */
 	{ "$", "$", TRUE, FALSE },	/* Must stay in this order */
 	{ "£", "£", TRUE, FALSE },	/* GBP */
 	{ "¥", "¥", TRUE, FALSE },	/* JPY */

 	/* Add yours to this list ! */
 	{ "[$€-1]", "€ Euro (100 €)", FALSE, TRUE},
	{ "[$€-2]", "€ Euro (€ 100)", TRUE, TRUE},

	/* The first column has three letter acronyms
	 * for each currency.  They MUST start with '[$'
	 * The second column has the long names of the currencies.
	 */

	/* 2002/08/04 Updated to match iso 4217 */
	{ "[$AED]",	N_("United Arab Emirates, Dirhams"), TRUE, TRUE },
	{ "[$AFA]",	N_("Afghanistan, Afghanis"), TRUE, TRUE },
	{ "[$ALL]",	N_("Albania, Leke"), TRUE, TRUE },
	{ "[$AMD]",	N_("Armenia, Drams"), TRUE, TRUE },
	{ "[$ANG]",	N_("Netherlands Antilles, Guilders"), TRUE, TRUE },
	{ "[$AOA]",	N_("Angola, Kwanza"), TRUE, TRUE },
	{ "[$ARS]",	N_("Argentina, Pesos"), TRUE, TRUE },
	{ "[$AUD]",	N_("Australia, Dollars"), TRUE, TRUE },
	{ "[$AWG]",	N_("Aruba, Guilders"), TRUE, TRUE },
	{ "[$AZM]",	N_("Azerbaijan, Manats"), TRUE, TRUE },
	{ "[$BAM]",	N_("Bosnia and Herzegovina, Convertible Marka"), TRUE, TRUE },
	{ "[$BBD]",	N_("Barbados, Dollars"), TRUE, TRUE },
	{ "[$BDT]",	N_("Bangladesh, Taka"), TRUE, TRUE },
	{ "[$BGL]",	N_("Bulgaria, Leva"), TRUE, TRUE },
	{ "[$BHD]",	N_("Bahrain, Dinars"), TRUE, TRUE },
	{ "[$BIF]",	N_("Burundi, Francs"), TRUE, TRUE },
	{ "[$BMD]",	N_("Bermuda, Dollars"), TRUE, TRUE },
	{ "[$BND]",	N_("Brunei Darussalam, Dollars"), TRUE, TRUE },
	{ "[$BOB]",	N_("Bolivia, Bolivianos"), TRUE, TRUE },
	{ "[$BRL]",	N_("Brazil, Brazil Real"), TRUE, TRUE },
	{ "[$BSD]",	N_("Bahamas, Dollars"), TRUE, TRUE },
	{ "[$BTN]",	N_("Bhutan, Ngultrum"), TRUE, TRUE },
	{ "[$BWP]",	N_("Botswana, Pulas"), TRUE, TRUE },
	{ "[$BYR]",	N_("Belarus, Rubles"), TRUE, TRUE },
	{ "[$BZD]",	N_("Belize, Dollars"), TRUE, TRUE },
	{ "[$CAD]",	N_("Canada, Dollars"), TRUE, TRUE },
	{ "[$CDF]",	N_("Congo/Kinshasa, Congolese Francs"), TRUE, TRUE },
	{ "[$CHF]",	N_("Switzerland, Francs"), TRUE, TRUE },
	{ "[$CLP]",	N_("Chile, Pesos"), TRUE, TRUE },
	{ "[$CNY]",	N_("China, Yuan Renminbi"), TRUE, TRUE },
	{ "[$COP]",	N_("Colombia, Pesos"), TRUE, TRUE },
	{ "[$CRC]",	N_("Costa Rica, Colones"), TRUE, TRUE },
	{ "[$CUP]",	N_("Cuba, Pesos"), TRUE, TRUE },
	{ "[$CVE]",	N_("Cape Verde, Escudos"), TRUE, TRUE },
	{ "[$CYP]",	N_("Cyprus, Pounds"), TRUE, TRUE },
	{ "[$CZK]",	N_("Czech Republic, Koruny"), TRUE, TRUE },
	{ "[$DJF]",	N_("Djibouti, Francs"), TRUE, TRUE },
	{ "[$DKK]",	N_("Denmark, Kroner"), TRUE, TRUE },
	{ "[$DOP]",	N_("Dominican Republic, Pesos"), TRUE, TRUE },
	{ "[$DZD]",	N_("Algeria, Algeria Dinars"), TRUE, TRUE },
	{ "[$EEK]",	N_("Estonia, Krooni"), TRUE, TRUE },
	{ "[$EGP]",	N_("Egypt, Pounds"), TRUE, TRUE },
	{ "[$ERN]",	N_("Eritrea, Nakfa"), TRUE, TRUE },
	{ "[$ETB]",	N_("Ethiopia, Birr"), TRUE, TRUE },
	{ "[$EUR]",	N_("Euro Member Countries, Euro"), TRUE, TRUE },
	{ "[$FJD]",	N_("Fiji, Dollars"), TRUE, TRUE },
	{ "[$FKP]",	N_("Falkland Islands (Malvinas), Pounds"), TRUE, TRUE },
	{ "[$GBP]",	N_("United Kingdom, Pounds"), TRUE, TRUE },
	{ "[$GEL]",	N_("Georgia, Lari"), TRUE, TRUE },
	{ "[$GGP]",	N_("Guernsey, Pounds"), TRUE, TRUE },
	{ "[$GHC]",	N_("Ghana, Cedis"), TRUE, TRUE },
	{ "[$GIP]",	N_("Gibraltar, Pounds"), TRUE, TRUE },
	{ "[$GMD]",	N_("Gambia, Dalasi"), TRUE, TRUE },
	{ "[$GNF]",	N_("Guinea, Francs"), TRUE, TRUE },
	{ "[$GTQ]",	N_("Guatemala, Quetzales"), TRUE, TRUE },
	{ "[$GYD]",	N_("Guyana, Dollars"), TRUE, TRUE },
	{ "[$HKD]",	N_("Hong Kong, Dollars"), TRUE, TRUE },
	{ "[$HNL]",	N_("Honduras, Lempiras"), TRUE, TRUE },
	{ "[$HRK]",	N_("Croatia, Kuna"), TRUE, TRUE },
	{ "[$HTG]",	N_("Haiti, Gourdes"), TRUE, TRUE },
	{ "[$HUF]",	N_("Hungary, Forint"), TRUE, TRUE },
	{ "[$IDR]",	N_("Indonesia, Rupiahs"), TRUE, TRUE },
	{ "[$ILS]",	N_("Israel, New Shekels"), TRUE, TRUE },
	{ "[$IMP]",	N_("Isle of Man, Pounds"), TRUE, TRUE },
	{ "[$INR]",	N_("India, Rupees"), TRUE, TRUE },
	{ "[$IQD]",	N_("Iraq, Dinars"), TRUE, TRUE },
	{ "[$IRR]",	N_("Iran, Rials"), TRUE, TRUE },
	{ "[$ISK]",	N_("Iceland, Kronur"), TRUE, TRUE },
	{ "[$JEP]",	N_("Jersey, Pounds"), TRUE, TRUE },
	{ "[$JMD]",	N_("Jamaica, Dollars"), TRUE, TRUE },
	{ "[$JOD]",	N_("Jordan, Dinars"), TRUE, TRUE },
	{ "[$JPY]",	N_("Japan, Yen"), TRUE, TRUE },
	{ "[$KES]",	N_("Kenya, Shillings"), TRUE, TRUE },
	{ "[$KGS]",	N_("Kyrgyzstan, Soms"), TRUE, TRUE },
	{ "[$KHR]",	N_("Cambodia, Riels"), TRUE, TRUE },
	{ "[$KMF]",	N_("Comoros, Francs"), TRUE, TRUE },
	{ "[$KPW]",	N_("Korea (North), Won"), TRUE, TRUE },
	{ "[$KRW]",	N_("Korea (South), Won"), TRUE, TRUE },
	{ "[$KWD]",	N_("Kuwait, Dinars"), TRUE, TRUE },
	{ "[$KYD]",	N_("Cayman Islands, Dollars"), TRUE, TRUE },
	{ "[$KZT]",	N_("Kazakstan, Tenge"), TRUE, TRUE },
	{ "[$LAK]",	N_("Laos, Kips"), TRUE, TRUE },
	{ "[$LBP]",	N_("Lebanon, Pounds"), TRUE, TRUE },
	{ "[$LKR]",	N_("Sri Lanka, Rupees"), TRUE, TRUE },
	{ "[$LRD]",	N_("Liberia, Dollars"), TRUE, TRUE },
	{ "[$LSL]",	N_("Lesotho, Maloti"), TRUE, TRUE },
	{ "[$LTL]",	N_("Lithuania, Litai"), TRUE, TRUE },
	{ "[$LVL]",	N_("Latvia, Lati"), TRUE, TRUE },
	{ "[$LYD]",	N_("Libya, Dinars"), TRUE, TRUE },
	{ "[$MAD]",	N_("Morocco, Dirhams"), TRUE, TRUE },
	{ "[$MDL]",	N_("Moldova, Lei"), TRUE, TRUE },
	{ "[$MGF]",	N_("Madagascar, Malagasy Francs"), TRUE, TRUE },
	{ "[$MKD]",	N_("Macedonia, Denars"), TRUE, TRUE },
	{ "[$MMK]",	N_("Myanmar (Burma), Kyats"), TRUE, TRUE },
	{ "[$MNT]",	N_("Mongolia, Tugriks"), TRUE, TRUE },
	{ "[$MOP]",	N_("Macau, Patacas"), TRUE, TRUE },
	{ "[$MRO]",	N_("Mauritania, Ouguiyas"), TRUE, TRUE },
	{ "[$MTL]",	N_("Malta, Liri"), TRUE, TRUE },
	{ "[$MUR]",	N_("Mauritius, Rupees"), TRUE, TRUE },
	{ "[$MVR]",	N_("Maldives (Maldive Islands), Rufiyaa"), TRUE, TRUE },
	{ "[$MWK]",	N_("Malawi, Kwachas"), TRUE, TRUE },
	{ "[$MXN]",	N_("Mexico, Pesos"), TRUE, TRUE },
	{ "[$MYR]",	N_("Malaysia, Ringgits"), TRUE, TRUE },
	{ "[$MZM]",	N_("Mozambique, Meticais"), TRUE, TRUE },
	{ "[$NAD]",	N_("Namibia, Dollars"), TRUE, TRUE },
	{ "[$NGN]",	N_("Nigeria, Nairas"), TRUE, TRUE },
	{ "[$NIO]",	N_("Nicaragua, Gold Cordobas"), TRUE, TRUE },
	{ "[$NOK]",	N_("Norway, Krone"), TRUE, TRUE },
	{ "[$NPR]",	N_("Nepal, Nepal Rupees"), TRUE, TRUE },
	{ "[$NZD]",	N_("New Zealand, Dollars"), TRUE, TRUE },
	{ "[$OMR]",	N_("Oman, Rials"), TRUE, TRUE },
	{ "[$PAB]",	N_("Panama, Balboa"), TRUE, TRUE },
	{ "[$PEN]",	N_("Peru, Nuevos Soles"), TRUE, TRUE },
	{ "[$PGK]",	N_("Papua New Guinea, Kina"), TRUE, TRUE },
	{ "[$PHP]",	N_("Philippines, Pesos"), TRUE, TRUE },
	{ "[$PKR]",	N_("Pakistan, Rupees"), TRUE, TRUE },
	{ "[$PLN]",	N_("Poland, Zlotys"), TRUE, TRUE },
	{ "[$PYG]",	N_("Paraguay, Guarani"), TRUE, TRUE },
	{ "[$QAR]",	N_("Qatar, Rials"), TRUE, TRUE },
	{ "[$ROL]",	N_("Romania, Lei"), TRUE, TRUE },
	{ "[$RUR]",	N_("Russia, Rubles"), TRUE, TRUE },
	{ "[$RWF]",	N_("Rwanda, Rwanda Francs"), TRUE, TRUE },
	{ "[$SAR]",	N_("Saudi Arabia, Riyals"), TRUE, TRUE },
	{ "[$SBD]",	N_("Solomon Islands, Dollars"), TRUE, TRUE },
	{ "[$SCR]",	N_("Seychelles, Rupees"), TRUE, TRUE },
	{ "[$SDD]",	N_("Sudan, Dinars"), TRUE, TRUE },
	{ "[$SEK]",	N_("Sweden, Kronor"), TRUE, TRUE },
	{ "[$SGD]",	N_("Singapore, Dollars"), TRUE, TRUE },
	{ "[$SHP]",	N_("Saint Helena, Pounds"), TRUE, TRUE },
	{ "[$SIT]",	N_("Slovenia, Tolars"), TRUE, TRUE },
	{ "[$SKK]",	N_("Slovakia, Koruny"), TRUE, TRUE },
	{ "[$SLL]",	N_("Sierra Leone, Leones"), TRUE, TRUE },
	{ "[$SOS]",	N_("Somalia, Shillings"), TRUE, TRUE },
	{ "[$SPL]",	N_("Seborga, Luigini"), TRUE, TRUE },
	{ "[$SRG]",	N_("Suriname, Guilders"), TRUE, TRUE },
	{ "[$STD]",	N_("Sao Tome and Principe, Dobras"), TRUE, TRUE },
	{ "[$SVC]",	N_("El Salvador, Colones"), TRUE, TRUE },
	{ "[$SYP]",	N_("Syria, Pounds"), TRUE, TRUE },
	{ "[$SZL]",	N_("Swaziland, Emalangeni"), TRUE, TRUE },
	{ "[$THB]",	N_("Thailand, Baht"), TRUE, TRUE },
	{ "[$TJR]",	N_("Tajikistan, Rubles"), TRUE, TRUE },
	{ "[$TMM]",	N_("Turkmenistan, Manats"), TRUE, TRUE },
	{ "[$TND]",	N_("Tunisia, Dinars"), TRUE, TRUE },
	{ "[$TOP]",	N_("Tonga, Pa'anga"), TRUE, TRUE },
	{ "[$TRL]",	N_("Turkey, Liras"), TRUE, TRUE },
	{ "[$TTD]",	N_("Trinidad and Tobago, Dollars"), TRUE, TRUE },
	{ "[$TVD]",	N_("Tuvalu, Tuvalu Dollars"), TRUE, TRUE },
	{ "[$TWD]",	N_("Taiwan, New Dollars"), TRUE, TRUE },
	{ "[$TZS]",	N_("Tanzania, Shillings"), TRUE, TRUE },
	{ "[$UAH]",	N_("Ukraine, Hryvnia"), TRUE, TRUE },
	{ "[$UGX]",	N_("Uganda, Shillings"), TRUE, TRUE },
	{ "[$USD]",	N_("United States of America, Dollars"), TRUE, TRUE },
	{ "[$UYU]",	N_("Uruguay, Pesos"), TRUE, TRUE },
	{ "[$UZS]",	N_("Uzbekistan, Sums"), TRUE, TRUE },
	{ "[$VEB]",	N_("Venezuela, Bolivares"), TRUE, TRUE },
	{ "[$VND]",	N_("Viet Nam, Dong"), TRUE, TRUE },
	{ "[$VUV]",	N_("Vanuatu, Vatu"), TRUE, TRUE },
	{ "[$WST]",	N_("Samoa, Tala"), TRUE, TRUE },
	{ "[$XAF]",	N_("Communaute Financiere Africaine BEAC, Francs"), TRUE, TRUE },
	{ "[$XAG]",	N_("Silver, Ounces"), TRUE, TRUE },
	{ "[$XAU]",	N_("Gold, Ounces"), TRUE, TRUE },
	{ "[$XCD]",	N_("East Caribbean Dollars"), TRUE, TRUE },
	{ "[$XDR]",	N_("International Monetary Fund (IMF) Special Drawing Rights"), TRUE, TRUE },
	{ "[$XOF]",	N_("Communaute Financiere Africaine BCEAO, Francs"), TRUE, TRUE },
	{ "[$XPD]",	N_("Palladium, Ounces"), TRUE, TRUE },
	{ "[$XPF]",	N_("Comptoirs Francais du Pacifique Francs"), TRUE, TRUE },
	{ "[$XPT]",	N_("Platinum, Ounces"), TRUE, TRUE },
	{ "[$YER]",	N_("Yemen, Rials"), TRUE, TRUE },
	{ "[$YUM]",	N_("Yugoslavia, New Dinars"), TRUE, TRUE },
	{ "[$ZAR]",	N_("South Africa, Rand"), TRUE, TRUE },
	{ "[$ZMK]",	N_("Zambia, Kwacha"), TRUE, TRUE },
	{ "[$ZWD]",	N_("Zimbabwe, Zimbabwe Dollars"), TRUE, TRUE },

	{ NULL, NULL, FALSE, FALSE }
};

/* Returns the index in go_format_currencies of the symbol in ptr */
static int
find_currency (char const *ptr, int len)
{
	int i;

	if (len == 5 && memcmp (ptr, "\"€\"", 5) == 0) {
		/* Accept quoted Euro character -- arbitrarity pick form 1.  */
		return EURO_FORM_1;
	} else if (len >= 2 && ptr[0] == '"' && ptr [len - 1] == '"') {
		/* Accept quotes around the currency.  */
		ptr++;
		len -= 2;
	}

	for (i = 0; go_format_currencies[i].symbol; i++)
		if (strncmp (go_format_currencies[i].symbol, ptr, len) == 0)
			return i;

	return -1;
}

static GOFormatFamily
cell_format_simple_number (char const * const fmt, GOFormatDetails *info)
{
	GOFormatFamily result = GO_FORMAT_NUMBER;
	int cur = -1;
	regmatch_t match[7];

	if (go_regexec (&re_simple_number, fmt, G_N_ELEMENTS (match), match, 0) == 0) {

		if (match[2].rm_eo == -1 && match[6].rm_eo == -1) {
			result = GO_FORMAT_NUMBER;
			info->currency_symbol_index = 0;
		} else {
			result = GO_FORMAT_CURRENCY;
			if (match[6].rm_eo == -1)
				cur = find_currency (fmt + match[2].rm_so,
						     match[2].rm_eo
						     - match[2].rm_so);
			else if (match[2].rm_eo == -1)
				cur = find_currency (fmt + match[6].rm_so,
						     match[6].rm_eo
						     - match[6].rm_so);
			if (cur == -1)
				return GO_FORMAT_UNKNOWN;
			info->currency_symbol_index = cur;
		}

		if (match[3].rm_eo != -1)
			info->thousands_sep = TRUE;

		info->num_decimals = 0;
		if (match[4].rm_eo != -1)
			info->num_decimals = match[4].rm_eo -
				match[4].rm_so - 1;

		return result;
	} else {
		return GO_FORMAT_UNKNOWN;
	}
}

static GOFormatFamily
cell_format_is_number (char const * const fmt, GOFormatDetails *info)
{
	GOFormatFamily result = GO_FORMAT_NUMBER;
	char const *ptr = fmt;
	int cur = -1;
	regmatch_t match[9];

	/* GO_FORMAT_CURRENCY or GO_FORMAT_NUMBER ? */
	if ((result = cell_format_simple_number (fmt, info)) != GO_FORMAT_UNKNOWN)
		return result;

	if (go_regexec (&re_red_number, fmt, G_N_ELEMENTS (match), match, 0) == 0) {
		char *tmp = g_strndup(fmt+match[1].rm_so,
				      match[1].rm_eo-match[1].rm_so);
		result = cell_format_simple_number (tmp, info);
		g_free(tmp);
		info->negative_fmt = 1;
		return result;
	}

	if (go_regexec (&re_brackets_number, fmt, G_N_ELEMENTS (match), match, 0) == 0) {
		char *tmp = g_strndup(fmt+match[1].rm_so,
				      match[1].rm_eo-match[1].rm_so);
		result = cell_format_simple_number (tmp, info);
		g_free(tmp);
		if (match[2].rm_eo != -1)
			info->negative_fmt = 3;
		else
			info->negative_fmt = 2;
		return result;
	}

	/* GO_FORMAT_PERCENTAGE or GO_FORMAT_SCIENTIFIC ? */
	if (go_regexec (&re_percent_science, fmt, G_N_ELEMENTS (match), match, 0) == 0) {
		info->num_decimals = 0;
		if (match[1].rm_eo != -1)
			info->num_decimals = match[1].rm_eo -
				match[1].rm_so - 1;

		if (ptr[match[2].rm_so] == '%')
			return GO_FORMAT_PERCENTAGE;
		else
			return GO_FORMAT_SCIENTIFIC;
	}

	/* GO_FORMAT_ACCOUNTING */
	if (go_regexec (&re_account, fmt, G_N_ELEMENTS (match), match, 0) == 0) {
		info->num_decimals = 0;
		if (match[5].rm_eo != -1)
			info->num_decimals = match[5].rm_eo -
				match[5].rm_so - 1;

		if (match[1].rm_eo == -1 && match[6].rm_eo == -1)
			return GO_FORMAT_UNKNOWN;
		else {
			if (match[8].rm_eo == -1)
				cur = find_currency (ptr + match[3].rm_so,
						    match[3].rm_eo
						    - match[3].rm_so);
			else if (match[3].rm_eo == -1)
				cur = find_currency (ptr + match[8].rm_so,
						    match[8].rm_eo
						    - match[8].rm_so);
			else
				return GO_FORMAT_UNKNOWN;

		}

		if (cur == -1)
			return GO_FORMAT_UNKNOWN;
		info->currency_symbol_index = cur;

		return GO_FORMAT_ACCOUNTING;
	}

	return GO_FORMAT_UNKNOWN;

}

static gboolean
cell_format_is_fraction (char const *fmt, GOFormatDetails *info)
{
	regmatch_t match[3];
	const char *denominator;

	if (go_regexec (&re_fraction, fmt, G_N_ELEMENTS (match), match, 0) != 0)
		return FALSE;

	denominator = fmt + match[2].rm_so;
	if (denominator[0] == '?') {
		info->num_decimals = match[1].rm_eo - match[1].rm_so;
		info->fraction_denominator = 0;
	} else {
		info->num_decimals = 0;
		info->fraction_denominator = atoi (denominator);
	}
	return TRUE;
}


GOFormatFamily
go_format_classify (GOFormat const *gf, GOFormatDetails *info)
{
	char const *fmt = gf->format;
	GOFormatFamily res;
	int i;

	g_return_val_if_fail (fmt != NULL, GO_FORMAT_GENERAL);
	g_return_val_if_fail (info != NULL, GO_FORMAT_GENERAL);

	/* Init the result to something sane */
	info->thousands_sep = FALSE;
	info->num_decimals = 2;
	info->negative_fmt = 0;
	info->list_element = 0;
	info->currency_symbol_index = 1; /* '$' */
	info->date_has_days = FALSE;
	info->date_has_months = FALSE;
	info->fraction_denominator = 0;

	if (*fmt == '\0')
		return GO_FORMAT_UNKNOWN;

	/* Note: ->family is not yet ready.  */
	if (g_ascii_strcasecmp (gf->format, fmts_general[0]) == 0)
		return GO_FORMAT_GENERAL;

	if (fmt[0] == '@' && fmt[1] == '[')
		return GO_FORMAT_MARKUP;

	/* Can we parse it ? */
	if ((res = cell_format_is_number (fmt, info)) != GO_FORMAT_UNKNOWN)
		return res;

	if (cell_format_is_fraction (fmt, info))
		return GO_FORMAT_FRACTION;

	/* Is it in the lists */
	for (i = 0; go_format_builtins[i] != NULL ; ++i) {
		int j = 0;
		char const * const * elem = go_format_builtins[i];
		for (; elem[j] ; ++j)
			if (g_ascii_strcasecmp (_(elem[j]), fmt) == 0) {
				info->list_element = j;
				switch (i) {
				case GO_FORMAT_DATE:
					info->date_has_days = 
						(NULL != g_utf8_strchr (elem[j],-1,'d'));
					info->date_has_months = 
						(NULL != g_utf8_strchr (elem[j],-1,'m'));
					break;
				default:
					break;
				}
				
				return i;
			}
	}
	return GO_FORMAT_UNKNOWN;
}

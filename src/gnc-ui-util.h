/********************************************************************\
 * gnc-ui-util.h -- utility functions for the GnuCash UI            *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef __GNC_UI_UTIL_H__
#define __GNC_UI_UTIL_H__

#include "config.h"

#include <glib.h>
#include <locale.h>

#include "gnc-commodity.h"
#include "gnc-numeric.h"
#include "Account.h"
#include "Transaction.h"


/* User Settings ****************************************************/
gboolean gnc_color_deficits (void);

char gnc_get_account_separator (void);

gboolean gnc_reverse_balance(Account *account);
gboolean gnc_reverse_balance_type(GNCAccountType type);

const char * gnc_register_default_font(void);
const char * gnc_register_default_hint_font(void);


/* Engine enhancements & i18n ***************************************/
typedef enum
{
  ACCOUNT_TYPE = 0,
  ACCOUNT_NAME,
  ACCOUNT_CODE,
  ACCOUNT_DESCRIPTION,
  ACCOUNT_NOTES,
  ACCOUNT_CURRENCY,
  ACCOUNT_SECURITY,
  ACCOUNT_BALANCE, /* with sign reversal */
  ACCOUNT_BALANCE_EURO,
  ACCOUNT_TOTAL,   /* balance + children's balance with sign reversal */
  ACCOUNT_TOTAL_EURO,
  ACCOUNT_TAX_INFO,
  NUM_ACCOUNT_FIELDS
} AccountFieldCode;

const char * gnc_ui_account_get_field_name (AccountFieldCode field);

/* Must g_free string when done */
char * gnc_ui_account_get_field_value_string (Account *account,
                                              AccountFieldCode field);

/* Must g_free string when done */
char * gnc_ui_account_get_tax_info_string (Account *account);

gnc_numeric gnc_ui_account_get_balance (Account *account,
                                        gboolean include_children);

const char * gnc_get_reconcile_str (char reconciled_flag);

typedef enum
{
  EQUITY_OPENING_BALANCE,
  EQUITY_RETAINED_EARNINGS,
  NUM_EQUITY_TYPES
} GNCEquityType;

Account * gnc_find_or_create_equity_account (GNCEquityType equity_type,
                                             gnc_commodity *currency);
gboolean gnc_account_create_opening_balance (Account *account,
                                             gnc_numeric balance,
                                             time_t date);


/* Price source functions *******************************************/

/* NOTE: If you modify PriceSourceCode, please update price-quotes.scm
   as well. */
typedef enum
{
  SOURCE_NONE = 0,
  SOURCE_YAHOO,
  SOURCE_YAHOO_EUROPE,
  SOURCE_FIDELITY,
  SOURCE_TROWEPRICE,
  SOURCE_VANGUARD,
  SOURCE_ASX,
  SOURCE_TIAA_CREF,
  NUM_SOURCES
} PriceSourceCode;
/* NOTE: If you modify PriceSourceCode, please update price-quotes.scm
   as well. */

const char * gnc_get_source_name (PriceSourceCode source);
const char * gnc_get_source_code_name (PriceSourceCode source);
PriceSourceCode gnc_get_source_code (const char * codename);


/* Locale functions *************************************************/

/* The gnc_localeconv() subroutine returns an lconv structure
 * containing locale information. If no locale is set, the structure
 * is given default (en_US) values.  */
struct lconv * gnc_localeconv (void);

/* Returns the default currency of the current locale. */
gnc_commodity * gnc_locale_default_currency (void);

/* Returns the number of decimal place to print in the current locale */
int gnc_locale_decimal_places (void);

/* Push and pop locales. Currently, this has no effect on gnc_localeconv.
 * i.e., after the first call to gnc_localeconv, subsequent calls will
 * return the same information. */
void gnc_push_locale (const char *locale);
void gnc_pop_locale (void);

/* Amount printing and parsing **************************************/

/*
 * The xaccPrintAmount() and xaccSPrintAmount() routines provide
 *    i18n'ed convenience routines for printing gnc_numerics.
 *    amounts. Both routines take a gnc_numeric argument and
 *    a printing information object.
 *
 * The xaccPrintAmount() routine returns a pointer to a statically
 *    allocated buffer, and is therefore not thread-safe.
 *
 * The xaccSPrintAmount() routine accepts a pointer to the buffer to be
 *    printed to.  It returns the length of the printed string.
 */

typedef struct _GNCPrintAmountInfo
{
  gnc_commodity *commodity;  /* may be NULL */

  guint8 max_decimal_places;
  guint8 min_decimal_places;

  unsigned int use_separators : 1; /* Print thousands separators */
  unsigned int use_symbol : 1;     /* Print currency symbol */
  unsigned int use_locale : 1;     /* Use locale for some positioning */
  unsigned int monetary : 1;       /* Is a monetary quantity */
} GNCPrintAmountInfo;


GNCPrintAmountInfo gnc_default_print_info (gboolean use_symbol);

GNCPrintAmountInfo gnc_commodity_print_info (gnc_commodity *commodity,
                                             gboolean use_symbol);

GNCPrintAmountInfo gnc_account_quantity_print_info (Account *account,
                                                    gboolean use_symbol);
GNCPrintAmountInfo gnc_account_value_print_info (Account *account,
                                                 gboolean use_symbol);

GNCPrintAmountInfo gnc_split_quantity_print_info (Split *split,
                                                  gboolean use_symbol);
GNCPrintAmountInfo gnc_split_value_print_info (Split *split,
                                               gboolean use_symbol);

GNCPrintAmountInfo gnc_default_share_print_info (void);
GNCPrintAmountInfo gnc_default_price_print_info (void);

GNCPrintAmountInfo gnc_integral_print_info (void);

const char * DxaccPrintAmount (double val, GNCPrintAmountInfo info);
int DxaccSPrintAmount (char *buf, double val, GNCPrintAmountInfo info);

const char * xaccPrintAmount (gnc_numeric val, GNCPrintAmountInfo info);
int xaccSPrintAmount (char *buf, gnc_numeric val, GNCPrintAmountInfo info);


/* xaccParseAmount parses in_str to obtain a numeric result. The
 *   routine will parse as much of in_str as it can to obtain a single
 *   number. The number is parsed using the current locale information
 *   and the 'monetary' flag. The routine will return TRUE if it
 *   successfully parsed a number and FALSE otherwise. If TRUE is
 *   returned and result is non-NULL, the value of the parsed number
 *   is stored in *result. If FALSE is returned, *result is
 *   unchanged. If TRUE is returned and endstr is non-NULL, the
 *   location of the first character in in_str not used by the parser
 *   will be returned in *endstr. If FALSE is returned and endstr is
 *   non-NULL, *endstr will point to in_str. */
gboolean xaccParseAmount (const char * in_str, gboolean monetary,
                          gnc_numeric *result, char **endstr);
gboolean DxaccParseAmount (const char * in_str, gboolean monetary,
                           double *result, char **endstr);


/* Automatic decimal place conversion *******************************/

/* enable/disable the auto decimal option */
void gnc_set_auto_decimal_enabled(gboolean enabled);

/* set how many auto decimal places to use */
void gnc_set_auto_decimal_places(int places);

#endif

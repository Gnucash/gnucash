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
  NUM_ACCOUNT_FIELDS
} AccountFieldCode;

const char * gnc_ui_get_account_field_name (AccountFieldCode field);

const char * gnc_ui_get_account_field_value_string (Account *account,
                                                    AccountFieldCode field);

gnc_numeric gnc_ui_account_get_balance (Account *account,
                                        gboolean include_children);

const char * gnc_get_reconcile_str (char reconciled_flag);


/* Price source functions *******************************************/
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

const char * gnc_get_source_name (PriceSourceCode source);
const char * gnc_get_source_code_name (PriceSourceCode source);
PriceSourceCode gnc_get_source_code (const char * codename);


/* Locale functions *************************************************/

/* The gnc_localeconv() subroutine returns an lconv structure
 * containing locale information. If no locale is set, the structure
 * is given default (en_US) values.  */
struct lconv * gnc_localeconv (void);

/* Returns the default currency of the current locale. */
const gnc_commodity * gnc_locale_default_currency (void);

/* Returns the number of decimal place to print in the current locale */
int gnc_locale_decimal_places (void);


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
  const gnc_commodity *commodity;  /* may be NULL */

  guint8 max_decimal_places;
  guint8 min_decimal_places;

  unsigned int use_separators : 1; /* Print thousands separators */
  unsigned int use_symbol : 1;     /* Print currency symbol */
  unsigned int use_locale : 1;     /* Use locale for some positioning */
  unsigned int monetary : 1;       /* Is a monetary quantity */
} GNCPrintAmountInfo;


GNCPrintAmountInfo gnc_default_print_info (gboolean use_symbol);

GNCPrintAmountInfo gnc_commodity_print_info (const gnc_commodity *commodity,
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


/** TEMPLATES ******************************************************/
/* 
 * There are several ideas going on in here.
 *  -- if an account is already being edited, and user clicks on "open", it
 *     should raise that dialog to the top or de-iconize it instead of 
 *     creating a new window. 
 *
 * -- association between windows and accounts is many-to-one & one-to-many.
 *    e.g.  if a "general ledger" dialog is open, then it might be displaying 
 *    four accounts all at once. Thus, an account may be visible in its 
 *    "main" dialog (of which there is only one), and possibly many "general 
 *    ledger" windows.
 *
 * -- I don't remember, but I think I might also use these to manage redraws
 *    when some entry is updated, and it is visible in multiple windows. 
 *    (again, visible in the account main window, and possibly some general 
 *    ledger windows).
 *
 * -- If user deletes an account, then any open windows associated with this
 *    account are auto-closed (and other windows possibly updated).
 *
 * -- the macros associate an xaccAccount struct with a gui-specific struct
 *    which contains things like widgets and other pieces the GUI needs 
 *    to "remember" about that dialog.
 */


#define FIND_IN_LIST(Type,list,id,member,found) 	\
{							\
  int i;						\
  Type *elt;						\
							\
  /* lets see if we have an */				\
  /* open window for this account */			\
  found = NULL;						\
  if (id && list) {					\
    i = 0;						\
    elt = list[i];					\
    while (elt) {					\
      if (id == elt->member) found = elt;		\
      i++;						\
      elt = list[i];					\
    } 							\
  } 							\
}

#define FETCH_FROM_LIST(Type,list,id,member,elt) 	\
{							\
  int i;						\
  Type **newlist;					\
  if (!(id)) return 0x0;				\
							\
  /* lets see if we already have an */			\
  /* open window for this account */			\
  i = 0;						\
  if (list) {						\
    elt = list[i];					\
    while (elt) {					\
      if ((id) == elt->member) return elt;		\
      i++;						\
      elt = list[i];					\
    }    						\
  } 							\
							\
  /* if we are here, we didn't find it */		\
  newlist = (Type **) malloc ((i+2) * sizeof (Type *));	\
  i = 0;						\
  if (list) {						\
    elt = list[i];					\
    while (elt) {					\
      newlist[i] = elt;					\
      i++;						\
      elt = list[i];					\
    }    						\
    free (list);					\
  }							\
  list = newlist;					\
							\
  /* malloc a new one, add it to the list */		\
  elt = (Type *) malloc (sizeof (Type));		\
  elt->member = (id);					\
  list [i] = elt;					\
  list [i+1] = NULL;					\
}

#define REMOVE_FROM_LIST(Type,list,id,member)	 	\
{							\
  int i,j;						\
  Type *elt;						\
							\
  /* find the item in the list, and remove it */	\
  if (list && id) {					\
    i = j = 0;						\
    elt = list[i];					\
    while (elt) {					\
      list[j] = elt;					\
      if (id == elt->member) j--; 			\
      i++; j++;						\
      elt = list[i];					\
    }    						\
    list[j] = NULL;					\
  }							\
}

#endif

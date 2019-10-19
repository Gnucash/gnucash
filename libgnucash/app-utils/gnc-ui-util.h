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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiUtility Utility functions for the GnuCash GUI
 * @{ */
/** @file gnc-ui-util.h
    @brief  utility functions for the GnuCash UI
    @author Copyright (C) 2000 Dave Peticolas <dave@krondo.com>
*/

#ifndef GNC_UI_UTIL_H
#define GNC_UI_UTIL_H

#include <glib.h>
#include <locale.h>

#include "Account.h"
#include "gncOwner.h"
#include "qof.h"


typedef QofSession * (*QofSessionCB) (void);


gchar *gnc_normalize_account_separator (const gchar* separator);
gboolean gnc_reverse_balance(const Account *account);

/* Backward compatibility *******************************************
 * Return book's UNREVERSED_BUDGET feature check. */
gboolean gnc_using_unreversed_budgets (QofBook* book);

/* Backward compatibility *******************************************
 * Compare book's UNREVERSED_BUDGET with unreverse_check. If they
 * match, return account reversal according to global pref. If they
 * don't match, return FALSE. */
gboolean gnc_reverse_budget_balance (const Account *account, gboolean unreversed);

/* Default directory sections ***************************************/
#define GNC_PREFS_GROUP_OPEN_SAVE    "dialogs.open-save"
#define GNC_PREFS_GROUP_EXPORT       "dialogs.export-accounts"
#define GNC_PREFS_GROUP_REPORT       "dialogs.report"
#define GNC_PREF_AUTO_DECIMAL_POINT  "auto-decimal-point"
#define GNC_PREF_AUTO_DECIMAL_PLACES "auto-decimal-places"

/* Default directories **********************************************/

gchar *gnc_get_default_directory (const gchar *section);
void gnc_set_default_directory (const gchar *section,
                                const gchar *directory);

/* Engine enhancements & i18n ***************************************/
QofBook * gnc_get_current_book (void);

/* If there is no current session, there is no book and we must be dealing
 * with a new book. When gnucash is started with --nofile, there is
 * initially no session (and no book), but by the time we check, one
 * could have been created (for example, if an empty account tree tab is
 * opened, a session is created which creates a new, but empty, book).
 * A session is created and a book is loaded from a backend when gnucash is
 * started with a file, but selecting 'new file' keeps a session open. So we
 * need to check as well for a book with no accounts (root with no children). */
gboolean gnc_is_new_book (void);

void gnc_set_current_book_tax_name_type (gboolean name_changed,
                                            const gchar *tax_name,
                                            gboolean type_changed,
                                            const gchar *tax_type);
const gchar * gnc_get_current_book_tax_name (void);
const gchar * gnc_get_current_book_tax_type (void);
/** Calls gnc_book_option_num_field_source_change to initiate registered
  * callbacks when num_field_source book option changes so that
  * registers/reports can update themselves; sets feature flag */
void gnc_book_option_num_field_source_change_cb (gboolean num_action);

/** Calls gnc_book_option_book_currency_selected to initiate registered
  * callbacks when currency accounting book option changes to book-currency so
  * that registers/reports can update themselves; sets feature flag */
void gnc_book_option_book_currency_selected_cb (gboolean use_book_currency);

/** Returns TRUE if both book-currency and default gain/loss policy KVPs exist
  * and are valid and trading accounts are not used */
gboolean gnc_book_use_book_currency (QofBook *book);

/** Returns pointer to Book Currency name for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
const gchar * gnc_book_get_book_currency_name (QofBook *book);

/** Returns pointer to Book Currency for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
gnc_commodity * gnc_book_get_book_currency (QofBook *book);

/** Returns pointer to default gain/loss policy for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
const gchar * gnc_book_get_default_gains_policy (QofBook *book);

/** Returns pointer to default gain/loss account for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
Account * gnc_book_get_default_gain_loss_acct (QofBook *book);

Account * gnc_get_current_root_account (void);
gnc_commodity_table * gnc_get_current_commodities (void);

/**
 * Get either the full name of the account or the simple name, depending on the
 * configuration parameter general/register/show_leaf_account_names.
 *
 * @param account The account to retrieve the name for.
 * @return A newly allocated string.
*/
gchar *gnc_get_account_name_for_register(const Account *account);

/**
 * Retrieve the account matching the given name starting from the descendants of
 * base_account.
 * @a name is either considered to be the name of the leaf in the account tree
 * or to be the full account path, depending on the configuration parameter
 * general.register/show_leaf_account_names.
 *
 * @param base_account The account to start the search at.
 * @param name The name to search for.
 * @return A pointer to the account, or NULL if the account was not found.
*/
Account *gnc_account_lookup_for_register(const Account *base_account, const
        gchar *name);

/**
 * Get either the full name of the account or the simple name, depending on the
 * show_leaf_accounts.
 *
 * @param account The account to retrieve the name for.
 * @param show_leaf_accounts Whether the full name will be returned.
 * @return A newly allocated string.
*/
gchar *gnc_get_account_name_for_split_register(const Account *account,
        gboolean show_leaf_accounts);

/*
 * This is a wrapper routine around an xaccGetBalanceInCurrency
 * function that handles additional needs of the gui.
 *
 * @param fn        The underlying function in Account.c to call to retrieve
 *                  a specific balance from the account.
 * @param account   The account to retrieve data about.
 * @param recurse   Include all sub-accounts of this account.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 * @param commodity The commodity in which the account balance should
 *                  be returned. If NULL, the value will be returned in
 *                  the commodity of the account. This is normally used
 *                  to specify a currency, which forces the conversion
 *                  of things like stock account values from share
 *                  values to an amount the requested currency.
 */
char *gnc_ui_account_get_tax_info_string (const Account *account);

char *gnc_ui_account_get_tax_info_sub_acct_string (const Account *account);

const char * gnc_get_reconcile_str (char reconciled_flag);
const char * gnc_get_reconcile_valid_flags (void);
const char * gnc_get_reconcile_flag_order (void);

typedef enum
{
    EQUITY_OPENING_BALANCE,
    EQUITY_RETAINED_EARNINGS,
    NUM_EQUITY_TYPES
} GNCEquityType;

Account * gnc_find_or_create_equity_account (Account *root,
        GNCEquityType equity_type,
        gnc_commodity *currency);
gboolean gnc_account_create_opening_balance (Account *account,
        gnc_numeric balance,
        time64 date,
        QofBook *book);

/* Locale functions *************************************************/


/* Returns the default currency of the current locale, or NULL if no
 * sensible currency could be identified from the locale. */
gnc_commodity * gnc_locale_default_currency_nodefault (void);

/* Returns the default currency of the current locale. WATCH OUT: If
 * no currency could be identified from the locale, this one returns
 * "USD", but this will have nothing to do with the actual locale. */
gnc_commodity * gnc_locale_default_currency (void);

/* Returns the default ISO currency string of the current locale. */
const char * gnc_locale_default_iso_currency_code (void);


/** Return the default currency set by the user.  If the user's
 *  preference is invalid, then this routine will return the default
 *  currency for the user's locale.
 *
 *  @return A pointer to a currency.
 */
gnc_commodity * gnc_default_currency (void);

/** Returns a gnc_commodity that is a currency, suitable for being a
Transaction's currency. The gnc_commodity is taken either from the current
account, or from the next parent account that has a gnc_commodity that is a
currency, or from gnc_default_currency().

If the given account or any of its parent account have a commodity that is a
currency, it is returned and the gboolean currency_from_account_found is set to
TRUE (if non-NULL). If neither this account nor any of its parent accounts have
such a commodity, gnc_default_currency() is returned and the gboolean
currency_from_account_found is set to FALSE (if non-NULL). This can be used to
show an appropriate warning message.

If account is NULL, gnc_default_currency() is returned and
currency_from_account_found is set to FALSE.

@param account The account where the currency should be looked up. May be NULL.

@param currency_from_account_found A gboolean pointer that takes the output
argument of whether the returned currency was found in the account. May be
NULL.

@return A currency pointer (and never NULL).
*/
gnc_commodity * gnc_account_or_default_currency(const Account* account, gboolean * currency_from_account_found);

/** Return the default currency for use in reports, as set by the
 *  user.  If the user's preference is invalid, then this routine will
 *  return the default currency for the user's locale.
 *
 *  @return A pointer to a currency.
 */
gnc_commodity * gnc_default_report_currency (void);

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
    unsigned int force_fit : 1;      /* Don't print more than max_dp places */
    unsigned int round : 1;          /* Round at max_dp instead of truncating */
} GNCPrintAmountInfo;


GNCPrintAmountInfo gnc_default_print_info (gboolean use_symbol);

GNCPrintAmountInfo gnc_commodity_print_info (const gnc_commodity *commodity,
        gboolean use_symbol);

GNCPrintAmountInfo gnc_account_print_info (const Account *account,
        gboolean use_symbol);

GNCPrintAmountInfo gnc_split_amount_print_info (Split *split,
        gboolean use_symbol);

GNCPrintAmountInfo gnc_share_print_info_places (int decplaces);
GNCPrintAmountInfo gnc_default_share_print_info (void);
GNCPrintAmountInfo gnc_default_price_print_info (const gnc_commodity *curr);

GNCPrintAmountInfo gnc_integral_print_info (void);

/* WARNING: Garbage in, garbage out.  You must check the validity of
   the supplied gnc_numeric.  If it's invalid, the returned string
   could point to ANYTHING. */
const char * xaccPrintAmount (gnc_numeric val, GNCPrintAmountInfo info);
int xaccSPrintAmount (char *buf, gnc_numeric val, GNCPrintAmountInfo info);

const gchar *printable_value(gdouble val, gint denom);
gchar *number_to_words(gdouble val, gint64 denom);
gchar *numeric_to_words(gnc_numeric val);

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

/*
 * xaccParseAmountPosSign is just like xaccParseAmount except the
 * caller can choose whether the locale's postive sign (or in absense
 * the '+') character is ignored. Setting skip to TRUE will cause
 * the function to ignore any positive sign. Setting it to FALSE,
 * and positive signs will be treated as unrecognized characters.
 * xaccParseAmount will run as if skip is FALSE for compatibility
 * reasons (gnc-expression-parser depends on this behaviour).
 */
gboolean
xaccParseAmountPosSign (const char * in_str, gboolean monetary, gnc_numeric *result,
                        char **endstr, gboolean skip);

/*
 * xaccParseAmountExtended is just like xaccParseAmount except the
 * caller must provide all the locale-specific information.
 *
 * Note: if group is NULL, no group-size verification will take place.
 * ignore_list is a list of characters that are completely ignored
 * while processing the input string.  If ignore_list is NULL, nothing
 * is ignored.
 */
gboolean
xaccParseAmountExtended (const char * in_str, gboolean monetary,
                         gunichar negative_sign, gunichar decimal_point,
                         gunichar group_separator, const char *group, const char *ignore_list,
                         gnc_numeric *result, char **endstr);

/* Initialization ***************************************************/

void gnc_ui_util_init (void);

#endif
/** @} */
/** @} */

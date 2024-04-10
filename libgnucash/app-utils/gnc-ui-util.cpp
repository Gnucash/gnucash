/********************************************************************\
 * gnc-ui-util.c -- utility functions for the GnuCash UI            *
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

#include <config.h>

#ifdef __MINGW32__
#define __USE_MINGW_ANSI_STDIO 1
#endif
#include "gnc-ui-util.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <gio/gio.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#if defined(G_OS_WIN32) && !defined(_MSC_VER)
# include <pow.h>
#endif
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <cinttypes>
#include <unicode/listformatter.h>

#include "qof.h"
#include "gnc-prefs.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-engine.h"
#include "gnc-features.h"
#include "gnc-hooks.h"
#include "gnc-session.h"
#include "engine-helpers.h"
#include "gnc-locale-utils.h"

#define GNC_PREF_CURRENCY_CHOICE_LOCALE "currency-choice-locale"
#define GNC_PREF_CURRENCY_CHOICE_OTHER  "currency-choice-other"
#define GNC_PREF_CURRENCY_OTHER         "currency-other"
#define GNC_PREF_REVERSED_ACCTS_NONE    "reversed-accounts-none"
#define GNC_PREF_REVERSED_ACCTS_CREDIT  "reversed-accounts-credit"
#define GNC_PREF_REVERSED_ACCTS_INC_EXP "reversed-accounts-incomeexpense"
#define GNC_PREF_PRICES_FORCE_DECIMAL   "force-price-decimal"

using UniStr = icu::UnicodeString;

static QofLogModule log_module = GNC_MOD_GUI;

static bool auto_decimal_enabled = false;
static int auto_decimal_places = 2;    /* default, can be changed */

static bool reverse_balance_inited = false;
static bool reverse_type[NUM_ACCOUNT_TYPES];

/* Cache currency ISO codes and only look them up in the settings when
 * absolutely necessary. Can't cache a pointer to the data structure
 * as that will change any time the book changes. */
static char* user_default_currency = nullptr;
static char* user_report_currency = nullptr;
constexpr int maximum_decimals = 15;
constexpr int64_t pow_10[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
                               10000000, 100000000, 1000000000, 10000000000,
                               100000000000, 1000000000000, 10000000000000,
                               100000000000000, 1000000000000000};

char*
gnc_normalize_account_separator (const gchar* separator)
{
    char* new_sep=nullptr;

    if (!separator || !*separator || g_strcmp0(separator, "colon") == 0)
            new_sep = g_strdup (":");
        else if (g_strcmp0(separator, "slash") == 0)
            new_sep = g_strdup ("/");
        else if (g_strcmp0(separator, "backslash") == 0)
            new_sep = g_strdup ("\\");
        else if (g_strcmp0(separator, "dash") == 0)
            new_sep = g_strdup ("-");
        else if (g_strcmp0(separator, "period") == 0)
            new_sep = g_strdup (".");
        else
            new_sep = g_strdup (separator);

    return new_sep;
}
/********************************************************************\
 * gnc_configure_account_separator                                  *
 *   updates the current account separator character                *
 *                                                                  *
 * Args: none                                                       *
 \*******************************************************************/
static void
gnc_configure_account_separator (void)
{
    auto string = gnc_prefs_get_string(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNT_SEPARATOR);
    auto separator = gnc_normalize_account_separator (string);

    gnc_set_account_separator(separator);

    g_free(string);
    g_free(separator);
}


static void
gnc_configure_reverse_balance (void)
{
    for (auto i = 0; i < NUM_ACCOUNT_TYPES; i++)
        reverse_type[i] = false;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_INC_EXP))
    {
        reverse_type[ACCT_TYPE_INCOME]  = true;
        reverse_type[ACCT_TYPE_EXPENSE] = true;
    }
    else if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_CREDIT))
    {
        reverse_type[ACCT_TYPE_LIABILITY] = true;
        reverse_type[ACCT_TYPE_PAYABLE]   = true;
        reverse_type[ACCT_TYPE_EQUITY]    = true;
        reverse_type[ACCT_TYPE_INCOME]    = true;
        reverse_type[ACCT_TYPE_CREDIT]    = true;
    }
    else if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_NONE))
        PWARN("no reversed account preference set, using none");

}

static void
gnc_reverse_balance_init (void)
{
    gnc_configure_reverse_balance ();
    reverse_balance_inited = TRUE;
}

gboolean
gnc_reverse_balance (const Account *account)
{
    if (account == nullptr)
        return FALSE;

    auto type = xaccAccountGetType (account);
    if ((type < 0) || (type >= NUM_ACCOUNT_TYPES))
        return FALSE;

    if (!reverse_balance_inited)
        gnc_reverse_balance_init ();

    return reverse_type[type];
}

gboolean gnc_using_equity_type_opening_balance_account (QofBook* book)
{
    return gnc_features_check_used (book, GNC_FEATURE_EQUITY_TYPE_OPENING_BALANCE);
}

void gnc_set_use_equity_type_opening_balance_account (QofBook* book)
{
    gnc_features_set_used (book, GNC_FEATURE_EQUITY_TYPE_OPENING_BALANCE);
}

char*
gnc_get_default_directory (const char* section)
{
    auto dir = gnc_prefs_get_string (section, GNC_PREF_LAST_PATH);
    if (!(dir && *dir))
    {
        g_free (dir); // if it's ""
#ifdef G_OS_WIN32
        dir = g_strdup (g_get_user_data_dir ()); /* equivalent of "My Documents" */
#else
        dir = g_strdup (g_get_home_dir ());
#endif
    }
    return dir;
}

void
gnc_set_default_directory (const char* section, const char* directory)
{
    gnc_prefs_set_string(section, GNC_PREF_LAST_PATH, directory);
}

QofBook *
gnc_get_current_book (void)
{
    return qof_session_get_book (gnc_get_current_session ());
}

/* If there is no current session, there is no book and we must be dealing
 * with a new book. When gnucash is started with --nofile, there is
 * initially no session (and no book), but by the time we check, one
 * could have been created (for example, if an empty account tree tab is
 * opened, a session is created which creates a new, but empty, book).
 * A session is created and a book is loaded from a backend when gnucash is
 * started with a file, but selecting 'new file' keeps a session open. So we
 * need to check as well for a book with no accounts (root with no children). */
gboolean
gnc_is_new_book (void)
{
    return (!gnc_current_session_exist() ||
            gnc_account_n_children (gnc_book_get_root_account (gnc_get_current_book ())) == 0);
}

#define OPTION_TAXUS_NAME "tax_US/name"
#define OPTION_TAXUS_TYPE "tax_US/type"
#define OLD_OPTION_TAXUS_NAME "book/tax_US/name"
#define OLD_OPTION_TAXUS_TYPE "book/tax_US/type"

void
gnc_set_current_book_tax_name_type (gboolean name_changed, const char* tax_name,
                                    gboolean type_changed, const char* tax_type)
{
    if (name_changed)
    {
        if (type_changed)
        {
            auto book = gnc_get_current_book();
            if ((g_strcmp0 (tax_name, "") == 0) ||
                (tax_name == nullptr))
            { /* change to no name */
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == nullptr))
                { /* need to delete both name and type and the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, nullptr);
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, nullptr);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* delete the name & change the type; keep the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, nullptr);
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, tax_type);
                }
            }
            else /* new name */
            {
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == nullptr))
                { /* delete the type & change the name; keep the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, nullptr);
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, tax_name);
                }
                else /* and new type */
                { /* change the name & change the type */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, tax_name);
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, tax_type);
                }
            }
        }
        else /* no type change but name changed */
        {
            auto book = gnc_get_current_book();
            if ((g_strcmp0 (tax_name, "") == 0) ||
                (tax_name == nullptr))
            { /* change to no name */
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == nullptr))
                { /* delete the name; there is no type; deleted the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, nullptr);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* need to delete the name and keep "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, nullptr);
                }
            }
            else
            { /* change the name & keep "tax_US" frame */
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, tax_name);
            }
        }
   }
   else /* no name change */
   {
        if (type_changed)
        {
            auto book = gnc_get_current_book();
            if ((g_strcmp0 (tax_type, "Other") == 0) ||
                (g_strcmp0 (tax_type, "") == 0) ||
                (tax_type == nullptr))
            {
                if ((g_strcmp0 (tax_name, "") == 0) ||
                    (tax_name == nullptr))
                {/* delete the type; there is no name; delete the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, nullptr);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* need to delete the type and keep "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, nullptr);
                }
            }
            else
            { /* change the type & keep "tax_US" frame */
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, tax_type);
            }
        } /*else no name and no type change - do nothing */
   }
}

const char*
gnc_get_current_book_tax_name (void)
{
    auto book = gnc_get_current_book();
    auto tax_name = qof_book_get_string_option(book, OPTION_TAXUS_NAME);
    if (tax_name)
    {
        return tax_name;
    }
    else
    {
        const char* old_option_taxus_name =
            qof_book_get_string_option(book, OLD_OPTION_TAXUS_NAME);
        if (old_option_taxus_name)
        {
            char* taxus_name = g_strdup(old_option_taxus_name);
            const char* old_option_taxus_type =
                qof_book_get_string_option(book, OLD_OPTION_TAXUS_TYPE);
            if (old_option_taxus_type)
            { /* switch both name and type and remove unused frames */
                char* taxus_type = g_strdup(old_option_taxus_type);
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, taxus_name);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, nullptr);
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, nullptr);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
                g_free (taxus_type);
            }
            else
            { /* switch just name and remove unused frames */
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, taxus_name);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, nullptr);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
            }
            g_free (taxus_name);
            return qof_book_get_string_option(book, OPTION_TAXUS_NAME);
        }
        return nullptr;
    }
}

const char*
gnc_get_current_book_tax_type (void)
{
    auto book = gnc_get_current_book();
    auto tax_type =
        qof_book_get_string_option(book, OPTION_TAXUS_TYPE);
    if (tax_type)
    {
        return tax_type;
    }
    else
    {
        auto old_option_taxus_type =
            qof_book_get_string_option(book, OLD_OPTION_TAXUS_TYPE);
        if (old_option_taxus_type)
        {
            auto taxus_type = g_strdup(old_option_taxus_type);
            auto old_option_taxus_name =
                qof_book_get_string_option(book, OLD_OPTION_TAXUS_NAME);
            if (old_option_taxus_name)
            { /* switch both name and type and remove unused frames */
                auto taxus_name = g_strdup(old_option_taxus_name);
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, taxus_name);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, nullptr);
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, nullptr);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
                g_free (taxus_name);
            }
            else
            { /* switch just type and remove unused frames */
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, nullptr);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
            }
            g_free (taxus_type);
            return qof_book_get_string_option(book, OPTION_TAXUS_TYPE);
        }
        return nullptr;
    }
}

Account *
gnc_get_current_root_account (void)
{
    return gnc_book_get_root_account (gnc_get_current_book ());
}

gnc_commodity_table *
gnc_get_current_commodities (void)
{
     if (gnc_current_session_exist())
          return gnc_commodity_table_get_table (gnc_get_current_book ());
     return nullptr;
}

char*
gnc_get_account_name_for_split_register(const Account *account,
                                        gboolean show_leaf_accounts)
{
    if (show_leaf_accounts)
        return g_strdup (xaccAccountGetName (account));
    else
        return gnc_account_get_full_name (account);
}

char*
gnc_get_account_name_for_register(const Account *account)
{
    auto show_leaf_accounts =
        gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_SHOW_LEAF_ACCT_NAMES);

    return gnc_get_account_name_for_split_register(account, show_leaf_accounts);
}

Account *
gnc_account_lookup_for_register(const Account *base_account, const char* name)
{
    auto show_leaf_accounts =
        gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_SHOW_LEAF_ACCT_NAMES);

    if (show_leaf_accounts)
        return gnc_account_lookup_by_name (base_account, name);
    else
        return gnc_account_lookup_by_full_name (base_account, name);
}

/********************************************************************\
 * gnc_get_reconcile_str                                            *
 *   return the i18n'd string for the given reconciled flag         *
 *                                                                  *
 * Args: reconciled_flag - the flag to convert into a string        *
 * Returns: the i18n'd reconciled string                            *
\********************************************************************/
const char*
gnc_get_reconcile_str (char reconciled_flag)
{
    switch (reconciled_flag)
    {
    case NREC:
        return C_("Reconciled flag 'not cleared'", "n");
    case CREC:
        return C_("Reconciled flag 'cleared'", "c");
    case YREC:
        return C_("Reconciled flag 'reconciled'", "y");
    case FREC:
        return C_("Reconciled flag 'frozen'", "f");
    case VREC:
        return C_("Reconciled flag 'void'", "v");
    default:
        PERR("Bad reconciled flag\n");
        return nullptr;
    }
}

/********************************************************************\
 * gnc_get_reconcile_valid_flags                                    *
 *   return a string containing the list of reconciled flags        *
 *                                                                  *
 * Returns: the i18n'd reconciled flags string                      *
\********************************************************************/
const char*
gnc_get_reconcile_valid_flags (void)
{
    static const char flags[] = { NREC, CREC, YREC, FREC, VREC, 0 };
    return flags;
}

/********************************************************************\
 * gnc_get_reconcile_flag_order                                     *
 *   return a string containing the reconciled-flag change order    *
 *                                                                  *
 * Args: reconciled_flag - the flag to convert into a string        *
 * Returns: the i18n'd reconciled string                            *
\********************************************************************/
const char*
gnc_get_reconcile_flag_order (void)
{
    static const char flags[] = { NREC, CREC, 0 };
    return flags;
}

const char*
gnc_get_doclink_str (char link_flag)
{
    switch (link_flag)
    {
    case WLINK:
        return C_("Document Link flag for 'web'", "w");
    case FLINK:
        return C_("Document Link flag for 'file'", "f");
    case ' ':
        return " ";
    default:
        PERR("Bad link flag");
        return nullptr;
    }
}

const char*
gnc_get_doclink_valid_flags (void)
{
    static const char flags[] = { FLINK, WLINK, ' ', 0 };
    return flags;
}

const char*
gnc_get_doclink_flag_order (void)
{
    static const char flags[] = { FLINK, WLINK, ' ', 0 };
    return flags;
}

static const char*
equity_base_name (GNCEquityType equity_type)
{
    switch (equity_type)
    {
    case EQUITY_OPENING_BALANCE:
        return N_("Opening Balances");

    case EQUITY_RETAINED_EARNINGS:
        return N_("Retained Earnings");

    default:
        return nullptr;
    }
}

Account *
gnc_find_or_create_equity_account (Account *root,
                                   GNCEquityType equity_type,
                                   gnc_commodity *currency)
{
    g_return_val_if_fail (equity_type >= 0, nullptr);
    g_return_val_if_fail (equity_type < NUM_EQUITY_TYPES, nullptr);
    g_return_val_if_fail (currency != nullptr, nullptr);
    g_return_val_if_fail (root != nullptr, nullptr);
    g_return_val_if_fail (gnc_commodity_is_currency(currency), nullptr);

    auto use_eq_op_feature = equity_type == EQUITY_OPENING_BALANCE && gnc_using_equity_type_opening_balance_account (gnc_get_current_book ());

    if (use_eq_op_feature)
    {
        auto account = gnc_account_lookup_by_opening_balance (root, currency);
        if (account)
            return account;
    }

    auto base_name = equity_base_name (equity_type);

    auto account = gnc_account_lookup_by_name(root, base_name);
    if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
        account = nullptr;

    if (!account)
    {
        base_name = base_name && *base_name ? _(base_name) : "";

        account = gnc_account_lookup_by_name(root, base_name);
        if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
            account = nullptr;
    }

    auto base_name_exists = (account != nullptr);

    if (account &&
            gnc_commodity_equiv (currency, xaccAccountGetCommodity (account)))
    {
        if (use_eq_op_feature)
            xaccAccountSetIsOpeningBalance (account, TRUE);
        return account;
    }

    auto name = g_strconcat (base_name, " - ",
                        gnc_commodity_get_mnemonic (currency), nullptr);
    account = gnc_account_lookup_by_name(root, name);
    if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
        account = nullptr;

    auto name_exists = (account != nullptr);

    if (account &&
            gnc_commodity_equiv (currency, xaccAccountGetCommodity (account)))
    {
        if (use_eq_op_feature)
            xaccAccountSetIsOpeningBalance (account, TRUE);
        return account;
    }

    /* Couldn't find one, so create it */
    if (name_exists && base_name_exists)
    {
        PWARN ("equity account with unexpected currency");
        g_free (name);
        return nullptr;
    }

    if (!base_name_exists &&
            gnc_commodity_equiv (currency, gnc_default_currency ()))
    {
        g_free (name);
        name = g_strdup (base_name);
    }

    auto parent = gnc_account_lookup_by_name(root, _("Equity"));
    if (!parent || xaccAccountGetType (parent) != ACCT_TYPE_EQUITY)
        parent = root;
    g_assert(parent);

    account = xaccMallocAccount (gnc_account_get_book(root));

    xaccAccountBeginEdit (account);

    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_EQUITY);
    xaccAccountSetCommodity (account, currency);

    if (use_eq_op_feature)
        xaccAccountSetIsOpeningBalance (account, TRUE);

    xaccAccountBeginEdit (parent);
    gnc_account_append_child (parent, account);
    xaccAccountCommitEdit (parent);

    xaccAccountCommitEdit (account);

    g_free (name);

    return account;
}

gboolean
gnc_account_create_opening_balance (Account *account,
                                    gnc_numeric balance,
                                    time64 date,
                                    QofBook *book)
{
    if (gnc_numeric_zero_p (balance))
        return TRUE;

    g_return_val_if_fail (account != nullptr, FALSE);
    auto commodity = xaccAccountGetCommodity (account);
    g_return_val_if_fail (gnc_commodity_is_currency (commodity), FALSE);

    auto equity_account =
        gnc_find_or_create_equity_account (gnc_account_get_root(account),
                                           EQUITY_OPENING_BALANCE,
                                           commodity);
    if (!equity_account)
        return FALSE;

    xaccAccountBeginEdit (account);
    xaccAccountBeginEdit (equity_account);

    auto trans = xaccMallocTransaction (book);

    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, gnc_account_or_default_currency (account, nullptr));
    xaccTransSetDatePostedSecsNormalized (trans, date);
    xaccTransSetDescription (trans, _("Opening Balance"));

    auto split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (account, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    balance = gnc_numeric_neg (balance);

    split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (equity_account, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    xaccTransCommitEdit (trans);
    xaccAccountCommitEdit (equity_account);
    xaccAccountCommitEdit (account);

    return TRUE;
}


gnc_commodity *
gnc_locale_default_currency_nodefault (void)
{
    auto table = gnc_get_current_commodities ();
    auto code = gnc_locale_default_iso_currency_code ();

    auto currency = gnc_commodity_table_lookup (table,
                                                GNC_COMMODITY_NS_CURRENCY,
                                                code);

    return (currency ? currency : nullptr);
}

gnc_commodity*
gnc_locale_default_currency (void)
{
    auto currency = gnc_locale_default_currency_nodefault ();

    return (currency ? currency :
            gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                        GNC_COMMODITY_NS_CURRENCY, "USD"));
}


static gnc_commodity*
gnc_default_currency_common (char* requested_currency,
                             const char* section)
{
    gnc_commodity *currency = nullptr;

    if (requested_currency)
        return gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                          GNC_COMMODITY_NS_CURRENCY,
                                          requested_currency);

    if (gnc_current_session_exist() &&
        gnc_prefs_get_bool (section, GNC_PREF_CURRENCY_CHOICE_OTHER))
    {
        auto mnemonic = gnc_prefs_get_string(section, GNC_PREF_CURRENCY_OTHER);
        currency = gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                              GNC_COMMODITY_NS_CURRENCY,
                                              mnemonic);
        DEBUG("mnemonic %s, result %p",
              mnemonic && *mnemonic ? mnemonic : "(null)", currency);
        g_free(mnemonic);
    }

    if (!currency)
        currency = gnc_locale_default_currency ();

    if (currency)
    {
        g_free (requested_currency);
    }

    return currency;
}

gnc_commodity*
gnc_default_currency (void)
{
    return gnc_default_currency_common (user_default_currency,
                                        GNC_PREFS_GROUP_GENERAL);
}

gnc_commodity*
gnc_account_or_default_currency(const Account* account,
                                gboolean * currency_from_account_found)
{
    gnc_commodity *currency;
    if (!account)
    {
        if (currency_from_account_found)
            *currency_from_account_found = FALSE;
        return gnc_default_currency();
    }

    currency = gnc_account_get_currency_or_parent(account);
    if (currency)
    {
        if (currency_from_account_found)
            *currency_from_account_found = TRUE;
    }
    else
    {
        if (currency_from_account_found)
            *currency_from_account_found = FALSE;
        currency = gnc_default_currency();
    }
    return currency;
}



gnc_commodity*
gnc_default_report_currency (void)
{
    return gnc_default_currency_common (user_report_currency,
                                        GNC_PREFS_GROUP_GENERAL_REPORT);
}

static void
gnc_currency_changed_cb (GSettings *settings, char* key, gpointer user_data)
{
    user_default_currency = nullptr;
    user_report_currency = nullptr;
    gnc_hook_run(HOOK_CURRENCY_CHANGED, nullptr);
}


GNCPrintAmountInfo
gnc_default_print_info (gboolean use_symbol)
{
    static GNCPrintAmountInfo info;
    static bool got_it = false;

    /* These must be updated each time. */
    info.use_symbol = use_symbol ? 1 : 0;
    info.commodity = gnc_default_currency ();

    if (got_it)
        return info;

    auto lc = gnc_localeconv ();

    info.max_decimal_places = lc->frac_digits;
    info.min_decimal_places = lc->frac_digits;

    info.use_separators = 1;
    info.use_locale = 1;
    info.monetary = 1;
    info.force_fit = 0;
    info.round = 0;

    got_it = TRUE;

    return info;
}

static bool
is_decimal_fraction (int fraction, uint8_t *max_decimal_places_p)
{
    uint8_t max_decimal_places = 0;

    if (fraction <= 0)
        return false;

    while (fraction != 1)
    {
        if (fraction % 10 != 0)
            return false;

        fraction = fraction / 10;
        max_decimal_places += 1;
    }

    if (max_decimal_places_p)
        *max_decimal_places_p = max_decimal_places;

    return true;
}

GNCPrintAmountInfo
gnc_commodity_print_info (const gnc_commodity *commodity,
                          gboolean use_symbol)
{
    GNCPrintAmountInfo info;

    if (commodity == nullptr)
        return gnc_default_print_info (use_symbol);

    info.commodity = commodity;

    auto is_iso = gnc_commodity_is_iso (commodity);

    if (is_decimal_fraction (gnc_commodity_get_fraction (commodity),
                             &info.max_decimal_places))
    {
        if (is_iso)
            info.min_decimal_places = info.max_decimal_places;
        else
            info.min_decimal_places = 0;
    }
    else
        info.max_decimal_places = info.min_decimal_places = 0;

    info.use_separators = 1;
    info.use_symbol = use_symbol ? 1 : 0;
    info.use_locale = is_iso ? 1 : 0;
    info.monetary = 1;
    info.force_fit = 0;
    info.round = 0;

    return info;
}

static GNCPrintAmountInfo
gnc_account_print_info_helper(const Account *account, gboolean use_symbol,
                              gnc_commodity * (*efffunc)(const Account *),
                              int (*scufunc)(const Account*))
{
    GNCPrintAmountInfo info;

    if (account == nullptr)
        return gnc_default_print_info (use_symbol);

    info.commodity = efffunc (account);

    auto is_iso = gnc_commodity_is_iso (info.commodity);

    auto scu = scufunc (account);

    if (is_decimal_fraction (scu, &info.max_decimal_places))
    {
        if (is_iso)
            info.min_decimal_places = info.max_decimal_places;
        else
            info.min_decimal_places = 0;
    }
    else
        info.max_decimal_places = info.min_decimal_places = 0;

    info.use_separators = 1;
    info.use_symbol = use_symbol ? 1 : 0;
    info.use_locale = is_iso ? 1 : 0;
    info.monetary = 1;
    info.force_fit = 0;
    info.round = 0;

    return info;
}

GNCPrintAmountInfo
gnc_account_print_info (const Account *account, gboolean use_symbol)
{
    return gnc_account_print_info_helper(account, use_symbol,
                                         xaccAccountGetCommodity,
                                         xaccAccountGetCommoditySCU);
}

GNCPrintAmountInfo
gnc_split_amount_print_info (Split *split, gboolean use_symbol)
{
    if (!split)
    {
        GNCPrintAmountInfo info = gnc_default_share_print_info ();
        info.use_symbol = use_symbol;
        return info;
    }

    return gnc_account_print_info (xaccSplitGetAccount (split), use_symbol);
}

static GNCPrintAmountInfo
gnc_default_print_info_helper (int decplaces)
{
    GNCPrintAmountInfo info;

    info.commodity = nullptr;

    info.max_decimal_places = decplaces;
    info.min_decimal_places = 0;

    info.use_separators = 1;
    info.use_symbol = 0;
    info.use_locale = 1;
    info.monetary = 1;
    info.force_fit = 0;
    info.round = 0;

    return info;
}

GNCPrintAmountInfo
gnc_default_share_print_info (void)
{
    static GNCPrintAmountInfo info;
    static bool got_it = false;

    if (!got_it)
    {
        info = gnc_default_print_info_helper (5);
        info.monetary = 0;
        got_it = TRUE;
    }

    return info;
}

GNCPrintAmountInfo
gnc_share_print_info_places (int decplaces)
{
    GNCPrintAmountInfo info;

    info = gnc_default_share_print_info ();
    info.max_decimal_places = decplaces;
    info.min_decimal_places = decplaces;
    info.force_fit = 1;
    info.round = 1;
    return info;
}

GNCPrintAmountInfo
gnc_price_print_info (const gnc_commodity *curr, gboolean use_symbol)
{
    GNCPrintAmountInfo info;
    auto force = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                     GNC_PREF_PRICES_FORCE_DECIMAL);
    info.commodity = curr;

    if (info.commodity)
    {
        int frac = gnc_commodity_get_fraction (curr);
        guint8 decplaces = 2;
        while (frac != 1 && (frac % 10) == 0 && (frac /= 10)) ++decplaces;
        info.max_decimal_places = decplaces;
        info.min_decimal_places = decplaces;
    }
    else
    {
        info.max_decimal_places = 6;
        info.min_decimal_places = 0;
    }

    info.use_separators = 1;
    info.use_symbol = use_symbol ? 1 : 0;
    info.use_locale = 1;
    info.monetary = 1;

    info.force_fit = force;
    info.round = force;
    return info;
}

GNCPrintAmountInfo
gnc_default_price_print_info (const gnc_commodity *curr)
{
    return gnc_price_print_info (curr, FALSE);
}


GNCPrintAmountInfo
gnc_integral_print_info (void)
{
    static GNCPrintAmountInfo info;
    static bool got_it = false;

    if (!got_it)
    {
        info = gnc_default_print_info_helper (0);
        got_it = TRUE;
    }

    return info;
}

/* Utility function for printing non-negative amounts */
static int
PrintAmountInternal(char* buf, gnc_numeric val, const GNCPrintAmountInfo *info)
{
    auto *lc = gnc_localeconv();
    constexpr size_t buf_size = 128;
    char temp_buf[buf_size];

    g_return_val_if_fail (info != nullptr, 0);

    if (gnc_numeric_check (val))
    {
        PWARN ("Bad numeric: %s.",
               gnc_numeric_errorCode_to_string(gnc_numeric_check (val)));
        *buf = '\0';
        return 0;
    }

    /* Print the absolute value, but remember sign */
    auto value_is_negative = gnc_numeric_negative_p (val);
    val = gnc_numeric_abs (val);

    /* Try to print as decimal. */
    auto value_is_decimal = gnc_numeric_to_decimal(&val, nullptr);
    if (!value_is_decimal && info->force_fit && info->round)
    {
        /* if there's a commodity use 100x the commodity's fraction. N.B. This
         * assumes that commodity fractions are multiples of 10, a reasonable
         * assumption in 2018. Otherwise, if there's a reasonable
         * max_decimal_places, use that.
         */
        const int64_t denom = info->commodity ?
             gnc_commodity_get_fraction(info->commodity) * 100 :
             (info->max_decimal_places &&
               info->max_decimal_places <= maximum_decimals) ?
             pow_10[info->max_decimal_places] : pow_10[maximum_decimals];
        val = gnc_numeric_convert(val, denom, GNC_HOW_RND_ROUND_HALF_UP);
        value_is_decimal = gnc_numeric_to_decimal(&val, nullptr);
    }
    auto min_dp = info->min_decimal_places;
    auto max_dp = info->max_decimal_places;

    /* Don to limit the number of decimal places _UNLESS_ force_fit is
     * true. */
    if (!info->force_fit)
        max_dp = 99;

    /* rounding? -- can only ROUND if force_fit is also true */
    if (value_is_decimal && info->round && info->force_fit)
    {
        /* Limit the denom to 10^13 ~= 2^44, leaving max at ~524288 */
        gnc_numeric rounding{5, pow_10[max_dp + 1]};

        val = gnc_numeric_add(val, rounding, val.denom,
                              GNC_HOW_DENOM_EXACT | GNC_HOW_RND_TRUNC);

        if (gnc_numeric_check(val))
        {
            PWARN ("Bad numeric from rounding: %s.",
                   gnc_numeric_errorCode_to_string(gnc_numeric_check (val)));
            *buf = '\0';
            return 0;
        }
    }

    /* calculate the integer part and the remainder */
    auto whole = gnc_numeric_convert(val, 1, GNC_HOW_RND_TRUNC);
    val = gnc_numeric_sub (val, whole, GNC_DENOM_AUTO, GNC_HOW_RND_NEVER);
    if (gnc_numeric_check (val))
    {
        PWARN ("Problem with remainder: %s.",
               gnc_numeric_errorCode_to_string(gnc_numeric_check (val)));
        *buf = '\0';
        return 0;
    }

    // Value may now be decimal, for example if the factional part is zero
    value_is_decimal = gnc_numeric_to_decimal(&val, nullptr);
    /* print the integer part without separators */
    snprintf(temp_buf, buf_size, "%" G_GINT64_FORMAT, whole.num);
    auto num_whole_digits = strlen (temp_buf);

    if (!info->use_separators)
        strcpy (buf, temp_buf);
    else
    {
        char* separator;
        char* group;

        if (info->monetary)
        {
            separator = lc->mon_thousands_sep;
            group = lc->mon_grouping;
        }
        else
        {
            separator = lc->thousands_sep;
            group = lc->grouping;
        }

        auto buf_ptr = buf;
        auto temp_ptr = &temp_buf[num_whole_digits - 1];
        auto group_count = 0;

        while (temp_ptr != temp_buf)
        {
            *buf_ptr++ = *temp_ptr--;

            if (*group != CHAR_MAX)
            {
                group_count++;

                if (group_count == *group)
                {
                    g_utf8_strncpy(buf_ptr, separator, 1);
                    buf_ptr = g_utf8_find_next_char(buf_ptr, nullptr);
                    group_count = 0;

                    /* Peek ahead at the next group code */
                    switch (group[1])
                    {
                        /* A null char means repeat the last group indefinitely */
                    case '\0':
                        break;
                        /* CHAR_MAX means no more grouping allowed */
                    case CHAR_MAX:
                        /* fall through */
                        /* Anything else means another group size */
                    default:
                        group++;
                        break;
                    }
                }
            }
        }

        /* We built the string backwards, now reverse */
        *buf_ptr++ = *temp_ptr;
        *buf_ptr = '\0';
        auto rev_buf = g_utf8_strreverse(buf, -1);
        strcpy (buf, rev_buf);
        g_free(rev_buf);
    } /* endif */

    /* at this point, buf contains the whole part of the number */

    /* If it's not decimal, print the fraction as an expression. */
    if (!value_is_decimal)
    {
        val = gnc_numeric_reduce (val);

        if (val.denom > 0)
            snprintf (temp_buf, buf_size, "%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
                     val.num, val.denom);
        else
            snprintf (temp_buf, buf_size, "%" G_GINT64_FORMAT " * %" G_GINT64_FORMAT,
                     val.num, -val.denom);

        if (whole.num == 0)
            *buf = '\0';
        else if (value_is_negative)
            strcat(buf, " - ");
        else
            strcat(buf, " + ");

        strcat (buf, temp_buf);
    }
    else
    {
        guint8 num_decimal_places = 0;
        char* temp_ptr = temp_buf;

        auto decimal_point = info->monetary
                             ? lc->mon_decimal_point
                             : lc->decimal_point;
        g_utf8_strncpy(temp_ptr, decimal_point, 1);
        temp_ptr = g_utf8_find_next_char(temp_ptr, nullptr);

        while (!gnc_numeric_zero_p (val)
                && (val.denom != 1)
                && (num_decimal_places < max_dp))
        {
            gint64 digit;

            val.denom = val.denom / 10;

            digit = val.num / val.denom;

            *temp_ptr++ = digit + '0';
            num_decimal_places++;

            val.num = val.num - (digit * val.denom);
        }

        while (num_decimal_places < min_dp)
        {
            *temp_ptr++ = '0';
            num_decimal_places++;
        }

        /* cap the end and move to the last character */
        *temp_ptr-- = '\0';

        /* Here we strip off trailing decimal zeros per the argument. */
        while (*temp_ptr == '0' && num_decimal_places > min_dp)
        {
            *temp_ptr-- = '\0';
            num_decimal_places--;
        }

        if (num_decimal_places > max_dp)
        {
            PWARN ("max_decimal_places too small; limit %d, value %s%s",
                   info->max_decimal_places, buf, temp_buf);
        }

        if (num_decimal_places > 0)
            strcat (buf, temp_buf);
    }

    return strlen(buf);
}

/**
 * @param bufp Should be at least 64 chars.
 **/
int
xaccSPrintAmount (char*  bufp, gnc_numeric val, GNCPrintAmountInfo info)
{
    auto orig_bufp = bufp;
    auto currency_symbol = "";
    const char* sign;

    char cs_precedes;
    char sep_by_space;
    char sign_posn;

    bool print_sign = true;
    bool print_absolute = false;

    if (!bufp)
        return 0;

    auto lc = gnc_localeconv();
    if (info.use_locale)
        if (gnc_numeric_negative_p (val))
        {
            cs_precedes  = lc->n_cs_precedes;
            sep_by_space = lc->n_sep_by_space;
        }
        else
        {
            cs_precedes  = lc->p_cs_precedes;
            sep_by_space = lc->p_sep_by_space;
        }
    else
    {
        cs_precedes = TRUE;
        sep_by_space = TRUE;
    }

    if (info.commodity && info.use_symbol)
    {
        currency_symbol = gnc_commodity_get_nice_symbol (info.commodity);
        if (!gnc_commodity_is_iso (info.commodity))
        {
            cs_precedes  = FALSE;
            sep_by_space = TRUE;
        }
    }

    if (gnc_numeric_negative_p (val))
    {
        sign = lc->negative_sign;
        sign_posn = lc->n_sign_posn;
    }
    else
    {
        sign = lc->positive_sign;
        sign_posn = lc->p_sign_posn;
    }

    if (gnc_numeric_zero_p (val) || (sign == nullptr) || (sign[0] == 0))
        print_sign = FALSE;

    /* See if we print sign now */
    if (print_sign && (sign_posn == 1))
        bufp = g_stpcpy(bufp, sign);

    /* Now see if we print currency */
    if (cs_precedes)
    {
        /* See if we print sign now */
        if (print_sign && (sign_posn == 3))
            bufp = g_stpcpy(bufp, sign);

        if (info.use_symbol)
        {
            bufp = g_stpcpy(bufp, currency_symbol);
            if (sep_by_space)
                bufp = g_stpcpy(bufp, " ");
        }

        /* See if we print sign now */
        if (print_sign && (sign_posn == 4))
            bufp = g_stpcpy(bufp, sign);
    }

    /* Now see if we print parentheses */
    if (print_sign && (sign_posn == 0))
    {
        bufp = g_stpcpy(bufp, "(");
        print_absolute = TRUE;
    }

    /* Now print the value */
    bufp += PrintAmountInternal(bufp,
                                print_absolute ? gnc_numeric_abs(val) : val,
                                &info);

    /* Now see if we print parentheses */
    if (print_sign && (sign_posn == 0))
        bufp = g_stpcpy(bufp, ")");

    /* Now see if we print currency */
    if (!cs_precedes)
    {
        /* See if we print sign now */
        if (print_sign && (sign_posn == 3))
            bufp = g_stpcpy(bufp, sign);

        if (info.use_symbol)
        {
            if (sep_by_space)
                bufp = g_stpcpy(bufp, " ");
            bufp = g_stpcpy(bufp, currency_symbol);
        }

        /* See if we print sign now */
        if (print_sign && (sign_posn == 4))
            bufp = g_stpcpy(bufp, sign);
    }

    /* See if we print sign now */
    if (print_sign && (sign_posn == 2))
        bufp = g_stpcpy(bufp, sign);

    /* return length of printed string */
    return (bufp - orig_bufp);
}

#define BUFLEN 1024

const char*
xaccPrintAmount (gnc_numeric val, GNCPrintAmountInfo info)
{
    /* hack alert -- this is not thread safe ... */
    static char buf[BUFLEN];

    if (!xaccSPrintAmount (buf, val, info))
        buf[0] = '\0';

    /* its OK to return buf, since we declared it static */
    return buf;
}

const char*
gnc_print_amount_with_bidi_ltr_isolate (gnc_numeric val, GNCPrintAmountInfo info)
{
    /* hack alert -- this is not thread safe ... */
    static char buf[BUFLEN];
    static const char ltr_isolate[] = { '\xe2', '\x81', '\xa6' };
    static const char ltr_pop_isolate[] = { '\xe2', '\x81', '\xa9' };
    auto offset = info.use_symbol ? 3 : 0;

    if (!gnc_commodity_is_currency (info.commodity))
        offset = 0;

    memset (buf, 0, BUFLEN);
    if (!xaccSPrintAmount (buf + offset, val, info))
    {
        buf[0] = '\0';
        return buf;
    };

    if (offset == 0)
        return buf;

    memcpy (buf, ltr_isolate, 3);

    if (buf[BUFLEN - 4] == '\0')
    {
        auto length = strlen (buf);
        memcpy (buf + length, ltr_pop_isolate, 3);
    }
    else
    {
        buf[BUFLEN - 1] = '\0';
        memcpy (buf + BUFLEN - 4, ltr_pop_isolate, 3);

        PWARN("buffer length %d exceeded, string truncated was %s", BUFLEN, buf);
    }
    /* its OK to return buf, since we declared it static
       and is immediately g_strdup'd */
    return buf;
}

char*
gnc_wrap_text_with_bidi_ltr_isolate (const char* text)
{
    static const char* ltr = "\u2066"; // ltr isolate
    static const char* pop = "\u2069"; // pop directional formatting

    if (!text)
        return nullptr;

    return g_strconcat (ltr, text, pop, nullptr);
}

/********************************************************************\
 ********************************************************************/

#define FUDGE .00001

/* This function is basically untranslatable. I'd
   guess out of the 29 translations we have, 20 will have their number
   wordings in a totally different way than English has (not to
   mention gender-dependent number endings). Which means this
   word-by-word translation will be useless or even plain
   wrong. For this reason, we don't even start to pretend a
   word-by-word translation would be of any use, so we don't mark any
   of these strings for translation. cstim, 2007-04-15. */
static const char* small_numbers[] =
{
    /* Translators: This section is for generating the "amount, in
       words" field when printing a check. This function gets the
       wording right for English, but unfortunately not for most other
       languages. Decide for yourself whether the check printing is
       actually needed in your language; if not, you can safely skip the
       translation of all of these strings.  */
    "Zero", "One", "Two", "Three", "Four",
    "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen",
    "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen",
    "Twenty"
};
static const char* medium_numbers[] =
{
    "Zero", "Ten", "Twenty", "Thirty", "Forty",
    "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"
};
static const char* big_numbers[] =
{
    /* Translators: This is the word for the number 10^2 */
    "Hundred",
    /* Translators: This is the word for the number 10^3 */
    "Thousand",
    /* Translators: This is the word for the number 10^6, one thousand
       thousands. */
    "Million",
    /* Translators: This is the word for the number 10^9, one thousand
       millions. WATCH OUT: In British English and many other languages
       this word is used for 10^12 which is one million millions! In
       contrast to this, here in GnuCash this is used in the American
       English meaning of 10^9.  */
    "Billion",
    /* Translators: This is the word for the number 10^12, one million
       millions. */
    "Trillion",
    /* Translators: This is the word for the number 10^15 */
    "Quadrillion",
    /* Translators: This is the word for the number 10^18 */
    "Quintillion"
};

static char*
integer_to_words(gint64 val)
{
    if (val == 0)
        return g_strdup("zero");
    if (val < 0)
        val = -val;

    auto result = g_string_sized_new(100);

    while (val >= 1000)
    {
        int log_val = log10(val) / 3 + FUDGE;
        int pow_val = exp(log_val * 3 * G_LN10) + FUDGE;
        int this_part = val / pow_val;
        val -= this_part * pow_val;
        auto tmp = integer_to_words(this_part);
        g_string_append_printf(result, "%s %s ", tmp, gettext(big_numbers[log_val]));
        g_free(tmp);
    }

    if (val >= 100)
    {
        int this_part = val / 100;
        val -= this_part * 100;
        g_string_append_printf(result, "%s %s ",
                               gettext(small_numbers[this_part]),
                               gettext(big_numbers[0]));
    }

    if (val > 20)
    {
        int this_part = val / 10;
        val -= this_part * 10;
        g_string_append(result, gettext(medium_numbers[this_part]));
        g_string_append_c(result, ' ');
    }

    if (val > 0)
    {
        int this_part = val;
        g_string_append(result, gettext(small_numbers[this_part]));
        g_string_append_c(result, ' ');
    }

    result = g_string_truncate(result, result->len - 1);
    return g_string_free(result, FALSE);
}

#ifdef _MSC_VER
static double round(double x)
{
    // A simple round() implementation because MSVC doesn't seem to have that
    return floor(x + 0.5);
}
#endif

char*
number_to_words(double val, int64_t denom)
{
    if (val < 0) val = -val;
    if (denom < 0) denom = -denom;

    auto int_part = floor(val);
    auto frac_part = static_cast<int64_t>(round((val - int_part) * denom));

    auto int_string = integer_to_words(int_part);
    /* Inside of the gettext macro _(...) we must not use any macros but
       only plain string literals. For this reason, convert the strings
       separately. */
    auto nomin_string = g_strdup_printf("%02" PRId64, frac_part);
    auto denom_string = g_strdup_printf("%" PRId64, denom);
    auto full_string =
        /* Translators: This is for the "amount, in words" field in check
           printing. The first %s is the integer amount of dollars (or
           whatever currency), the second and third %s the cent amount as
           a fraction, e.g. 47/100.  */
        g_strdup_printf("%s and %s/%s",
                        int_string, nomin_string, denom_string);
    g_free(int_string);
    g_free(nomin_string);
    g_free(denom_string);
    return full_string;
}

char*
numeric_to_words(gnc_numeric val)
{
    return number_to_words(gnc_numeric_to_double(val),
                           gnc_numeric_denom(val));
}

const char*
printable_value (double val, int denom)
{
    auto num = gnc_numeric_create(round(val * denom), denom);
    auto info = gnc_share_print_info_places(log10(denom));
    return xaccPrintAmount (num, info);
}

/********************************************************************\
 * xaccParseAmount                                                  *
 *   parses amount strings using locale data                        *
 *                                                                  *
 * Args: in_str   -- pointer to string rep of num                   *
 *       monetary -- boolean indicating whether value is monetary   *
 *       result   -- pointer to result location, may be nullptr        *
 *       endstr   -- used to store first digit not used in parsing  *
 * Return: gboolean -- TRUE if a number found and parsed            *
 *                     If FALSE, result is not changed              *
\********************************************************************/

/* Parsing state machine states */
enum ParseState
{
    START_ST,       /* Parsing initial whitespace */
    NEG_ST,         /* Parsed a negative sign or a left paren */
    NUMER_ST,   /* Parsing digits before grouping and decimal characters */
    FRAC_ST,        /* Parsing the fractional portion of a number */
    DONE_ST,        /* Finished, number is correct module grouping constraints */
    NO_NUM_ST       /* Finished, number was malformed */
};

#define done_state(state) (((state) == DONE_ST) || ((state) == NO_NUM_ST))

static inline int64_t
multiplier (int num_decimals)
{
    switch (num_decimals)
    {
    case 12:
        return 1000000000000;
    case 11:
        return 100000000000;
    case 10:
        return 10000000000;
    case 9:
        return 1000000000;
    case 8:
        return 100000000;
    case 7:
        return 10000000;
    case 6:
        return 1000000;
    case 5:
        return 100000;
    case 4:
        return 10000;
    case 3:
        return 1000;
    case 2:
        return 100;
    case 1:
        return 10;
    case 0:
         return 1;
    default:
        PERR("bad fraction length");
        g_assert_not_reached();
        break;
    }

    return 1;
}

static gboolean
xaccParseAmountInternal (const char*  in_str, gboolean monetary,
                         gunichar negative_sign, gunichar decimal_point,
                         gunichar group_separator, const char* ignore_list,
                         gboolean use_auto_decimal,
                         gnc_numeric *result, char** endstr)
{
    /* Initialize *endstr to in_str */
    if (endstr)
        *endstr = (char* ) in_str;

    if (!in_str)
        return FALSE;

    const char* in;
    if (!g_utf8_validate(in_str, -1, &in))
    {
        printf("Invalid utf8 string '%s'. Bad character at position %ld.\n",
               in_str, g_utf8_pointer_to_offset (in_str, in));
        return FALSE;
    }

    /* 'out_str' will be used to store digits for numeric conversion.
     * 'out' will be used to traverse out_str. */
    auto out_str = g_new(gchar, strlen(in_str) + 128);
    auto out = out_str;

    /* 'in' is used to traverse 'in_str'. */
    in = in_str;

    auto is_negative = false;
    auto got_decimal = false;
    auto need_paren = false;
    int64_t numer = 0;
    int64_t denom = 1;

    /* Initialize the state machine */
    auto state = START_ST;

    /* This while loop implements a state machine for parsing numbers. */
    while (TRUE)
    {
        auto next_state = state;
        auto uc = g_utf8_get_char(in);

        /* Ignore anything in the 'ignore list' */
        if (ignore_list && uc && g_utf8_strchr(ignore_list, -1, uc))
        {
            in = g_utf8_next_char(in);
            continue;
        }

        /* Note we never need to check for the end of 'in_str' explicitly.
         * The 'else' clauses on all the state transitions will handle that. */
        switch (state)
        {
            /* START_ST means we have parsed 0 or more whitespace characters */
            case START_ST:
                if (g_unichar_isdigit(uc))
                {
                    int count = g_unichar_to_utf8(uc, out);
                    out += count; /* we record the digits themselves in out_str
                    * for later conversion by libc routines */
                    next_state = NUMER_ST;
                }
                else if (uc == decimal_point)
                    next_state = FRAC_ST;
                else if (g_unichar_isspace(uc))
                    ;
                else if (uc == negative_sign)
                {
                    is_negative = TRUE;
                    next_state = NEG_ST;
                }
                else if (uc == '(')
                {
                    is_negative = TRUE;
                    need_paren = TRUE;
                    next_state = NEG_ST;
                }
                else
                    next_state = NO_NUM_ST;

                break;

            /* NEG_ST means we have just parsed a negative sign. For now,
             * we only recognize formats where the negative sign comes first. */
            case NEG_ST:
                if (g_unichar_isdigit(uc))
                {
                    int count = g_unichar_to_utf8(uc, out);
                    out += count;
                    next_state = NUMER_ST;
                }
                else if (uc == decimal_point)
                    next_state = FRAC_ST;
                else if (g_unichar_isspace(uc))
                    ;
                else
                    next_state = NO_NUM_ST;

                break;

            /* NUMER_ST means we have started parsing the number, but
             * have not encountered a decimal separator. */
            case NUMER_ST:
                if (g_unichar_isdigit(uc))
                {
                    int count = g_unichar_to_utf8(uc, out);
                    out += count;
                }
                else if (uc == decimal_point)
                    next_state = FRAC_ST;
                else if (uc == group_separator)
                    ; //ignore it
                else if (uc == ')' && need_paren)
                {
                    next_state = DONE_ST;
                    need_paren = FALSE;
                }
                else
                    next_state = DONE_ST;

                break;

            /* FRAC_ST means we are now parsing fractional digits. */
            case FRAC_ST:
                if (g_unichar_isdigit(uc))
                {
                    int count = g_unichar_to_utf8(uc, out);
                    out += count;
                }
                else if (uc == decimal_point)
                {
                    /* If a subsequent decimal point is also whitespace,
                        * assume it was intended as such and stop parsing.
                        * Otherwise, there is a problem. */
                    if (g_unichar_isspace(decimal_point))
                        next_state = DONE_ST;
                    else
                        next_state = NO_NUM_ST;
                }
                else if (uc == group_separator)
                {
                    /* If a subsequent group separator is also whitespace,
                        * assume it was intended as such and stop parsing.
                        * Otherwise ignore it. */
                    if (g_unichar_isspace(group_separator))
                        next_state = DONE_ST;
                }
                else if (uc == ')' && need_paren)
                {
                    next_state = DONE_ST;
                    need_paren = FALSE;
                }
                else
                    next_state = DONE_ST;

                break;

            default:
                PERR("bad state");
                g_assert_not_reached();
                break;
        }

        /* If we're moving into the FRAC_ST or out of the machine
         * without going through FRAC_ST, record the integral value. */
        if (((next_state == FRAC_ST) && (state != FRAC_ST)) ||
            ((next_state == DONE_ST) && !got_decimal))
        {
            *out = '\0';

            if (*out_str && sscanf(out_str, "%" SCNd64, &numer) < 1)
                next_state = NO_NUM_ST;
            else if (next_state == FRAC_ST)
            {
                /* reset the out pointer to record the fraction */
                out = out_str;
                *out = '\0';

                got_decimal = TRUE;
            }
        }

        state = next_state;
        if (done_state (state))
            break;

        in = g_utf8_next_char(in);
    }

    /* If there was an error, just quit */
    if (need_paren || (state == NO_NUM_ST))
    {
        g_free(out_str);
        return FALSE;
    }

    /* Cap the end of the fraction string, if any */
    *out = '\0';

    /* Add in fractional value */
    if (got_decimal && *out_str)
    {

        auto len = strlen(out_str);

        if (len > 12)
        {
            out_str[12] = '\0';
            len = 12;
        }

        int64_t fraction;
        if (sscanf (out_str, "%" SCNd64 , &fraction) < 1)
        {
            g_free(out_str);
            return FALSE;
        }

        denom = multiplier(len);
        numer *= denom;
        numer += fraction;
    }
    else if (monetary && use_auto_decimal && !got_decimal)
    {
        if ((auto_decimal_places > 0) && (auto_decimal_places <= 12))
        {
            denom = multiplier(auto_decimal_places);

            /* No need to multiply numer by denom at this point,
             * since by specifying the auto decimal places the
             * user has effectively determined the scaling factor
             * for the numerator they entered.
             */
        }
    }

    if (result)
    {
        *result = gnc_numeric_create (numer, denom);
        if (is_negative)
            *result = gnc_numeric_neg (*result);
    }

    if (endstr)
        *endstr = (char* ) in;

    g_free (out_str);

    return TRUE;
}


static gboolean
xaccParseAmountBasicInternal (const char*  in_str, gboolean monetary,
                              gboolean use_auto_decimal, gnc_numeric *result,
                              char** endstr, gboolean skip)
{
    struct lconv *lc = gnc_localeconv();
    gunichar negative_sign = g_utf8_get_char(lc->negative_sign);

    gunichar decimal_point;
    gunichar group_separator;
    if (monetary)
    {
        group_separator = g_utf8_get_char(lc->mon_thousands_sep);
        decimal_point = g_utf8_get_char(lc->mon_decimal_point);
    }
    else
    {
        group_separator = g_utf8_get_char(lc->thousands_sep);
        decimal_point = g_utf8_get_char(lc->decimal_point);
    }

    const char* ignore = nullptr;
    if (skip)
    {
        /* We want the locale's positive sign to be ignored.
         * If the locale doesn't specify one, we assume "+" as
         * an optional positive sign and ignore that */
        ignore = lc->positive_sign;
        if (!ignore || !*ignore)
            ignore = "+";
    }

    return xaccParseAmountInternal(in_str, monetary, negative_sign,
                                   decimal_point, group_separator,
                                   ignore, use_auto_decimal,
                                   result, endstr);
}


gboolean
xaccParseAmount (const char*  in_str, gboolean monetary, gnc_numeric *result,
                 char** endstr)
{
    return xaccParseAmountBasicInternal (in_str, monetary, auto_decimal_enabled,
                                         result, endstr, FALSE);
}

gboolean
xaccParseAmountImport (const char*  in_str, gboolean monetary,
                        gnc_numeric *result,
                        char** endstr, gboolean skip)
{
    return xaccParseAmountBasicInternal (in_str, monetary, FALSE,
                                         result, endstr, skip);
}


gboolean
xaccParseAmountExtended (const char*  in_str, gboolean monetary,
                         gunichar negative_sign, gunichar decimal_point,
                         gunichar group_separator, const char* ignore_list,
                         gnc_numeric *result, char** endstr)
{
    return xaccParseAmountInternal (in_str, monetary, negative_sign,
                                    decimal_point, group_separator,
                                    ignore_list, auto_decimal_enabled,
                                    result, endstr);
}

gboolean
xaccParseAmountExtImport (const char*  in_str, gboolean monetary,
                             gunichar negative_sign, gunichar decimal_point,
                             gunichar group_separator, const char* ignore_list,
                             gnc_numeric *result, char** endstr)
{
    return xaccParseAmountInternal (in_str, monetary, negative_sign,
                                    decimal_point, group_separator,
                                    ignore_list, FALSE,
                                    result, endstr);
}


/* enable/disable the auto_decimal_enabled option */
static void
gnc_set_auto_decimal_enabled (gpointer settings, char* key, gpointer user_data)
{
    auto_decimal_enabled =
            gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_POINT);
}

/* set the number of auto decimal places to use */
static void
gnc_set_auto_decimal_places (gpointer settings, char* key, gpointer user_data)
{
    auto_decimal_places =
            gnc_prefs_get_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_PLACES);
}

static void
gnc_auto_decimal_init (void)
{
    auto_decimal_enabled =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_POINT);
    auto_decimal_places =
        gnc_prefs_get_int (GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_PLACES);
}

void
gnc_ui_util_init (void)
{
    gnc_configure_account_separator ();
    gnc_auto_decimal_init();

    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNT_SEPARATOR,
                          (void*)gnc_configure_account_separator, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_NONE,
                          (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_CREDIT,
                          (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_INC_EXP,
                          (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_CHOICE_LOCALE,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_CHOICE_OTHER,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_OTHER,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_CHOICE_LOCALE,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_CHOICE_OTHER,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_OTHER,
                          (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_POINT,
                          (void*)gnc_set_auto_decimal_enabled, nullptr);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_PLACES,
                          (void*)gnc_set_auto_decimal_places, nullptr);

}

void
gnc_ui_util_remove_registered_prefs (void)
{
    // remove the registered pref call backs above
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_ACCOUNT_SEPARATOR,
                                 (void*)gnc_configure_account_separator, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_REVERSED_ACCTS_NONE,
                                 (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_REVERSED_ACCTS_CREDIT,
                                 (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_REVERSED_ACCTS_INC_EXP,
                                 (void*)gnc_configure_reverse_balance, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_CURRENCY_CHOICE_LOCALE,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_CURRENCY_CHOICE_OTHER,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_CURRENCY_OTHER,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
                                 GNC_PREF_CURRENCY_CHOICE_LOCALE,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
                                 GNC_PREF_CURRENCY_CHOICE_OTHER,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
                                 GNC_PREF_CURRENCY_OTHER,
                                 (void*)gnc_currency_changed_cb, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_AUTO_DECIMAL_POINT,
                                 (void*)gnc_set_auto_decimal_enabled, nullptr);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_AUTO_DECIMAL_PLACES,
                                 (void*)gnc_set_auto_decimal_places, nullptr);
}

static inline bool
unichar_is_cntrl (gunichar uc)
{
    return (uc < 0x20 || (uc > 0x7e && uc < 0xa0));
}

char*
gnc_filter_text_for_control_chars (const char* text)
{
    bool cntrl = false;
    bool text_found = false;

    if (!text)
        return nullptr;

    if (!g_utf8_validate (text, -1, nullptr))
        return nullptr;

    auto filtered = g_string_sized_new (strlen (text) + 1);

    auto ch = text;

    while (*ch)
    {
        auto uc = g_utf8_get_char (ch);

        // check for starting with control characters
        if (unichar_is_cntrl (uc) && !text_found)
        {
            ch = g_utf8_next_char (ch);
            continue;
        }
        // check for alpha, num and punctuation
        if (!unichar_is_cntrl (uc))
        {
            filtered = g_string_append_unichar (filtered, uc);
            text_found = true;
        }
        // check for control characters after text
        if (unichar_is_cntrl (uc))
            cntrl = true;

        ch = g_utf8_next_char (ch);

        if (cntrl) // if control characters in text replace with space
        {
            auto uc2 = g_utf8_get_char (ch);

            if (!unichar_is_cntrl (uc2))
                filtered = g_string_append_unichar (filtered, ' ');
        }
        cntrl = false;
    }
    return g_string_free (filtered, FALSE);
}

void
gnc_filter_text_set_cursor_position (const char* incoming_text,
                                     const char* symbol,
                                     gint *cursor_position)
{
    int num = 0;

    if (*cursor_position == 0)
        return;

    if (!incoming_text || !symbol)
        return;

    if (g_strrstr (incoming_text, symbol) == nullptr)
        return;

    auto text_len = g_utf8_strlen (incoming_text, -1);

    for (int x = 0; x < text_len; x++)
    {
        auto temp = g_utf8_offset_to_pointer (incoming_text, x);

        if (g_str_has_prefix (temp, symbol))
            num++;

        if (g_strrstr (temp, symbol) == nullptr)
            break;
    }
    *cursor_position = *cursor_position - (num * g_utf8_strlen (symbol, -1));
}

char*
gnc_filter_text_for_currency_symbol (const char* incoming_text,
                                     const char* symbol)
{
    if (!incoming_text)
        return nullptr;

    if (!symbol)
       return g_strdup (incoming_text);

    if (g_strrstr (incoming_text, symbol) == nullptr)
        return g_strdup (incoming_text);

    auto split = g_strsplit (incoming_text, symbol, -1);

    auto ret_text = g_strjoinv (nullptr, split);

    g_strfreev (split);
    return ret_text;
}

char*
gnc_filter_text_for_currency_commodity (const gnc_commodity *comm,
                                        const char* incoming_text,
                                        const char** symbol)
{
    if (!incoming_text)
    {
        *symbol = nullptr;
        return nullptr;
    }

    if (!gnc_commodity_is_currency (comm))
    {
        *symbol = nullptr;
        return g_strdup (incoming_text);
    }

    if (comm)
        *symbol = gnc_commodity_get_nice_symbol (comm);
    else
        *symbol = gnc_commodity_get_nice_symbol (gnc_default_currency ());

    return gnc_filter_text_for_currency_symbol (incoming_text, *symbol);
}

gchar*
gnc_list_formatter (GList *strings)
{
    g_return_val_if_fail (strings, nullptr);

    UErrorCode status = U_ZERO_ERROR;
    auto formatter = icu::ListFormatter::createInstance(status);
    std::vector<UniStr> strvec;
    UniStr result;
    std::string retval;

    for (auto n = strings; n; n = g_list_next (n))
    {
        auto utf8_str{static_cast<const char*>(n->data)};
        strvec.push_back (UniStr::fromUTF8(utf8_str));
    }

    formatter->format (strvec.data(), strvec.size(), result, status);

    if (U_FAILURE(status))
        PERR ("Unicode error");
    else
        result.toUTF8String(retval);

    delete formatter;
    return g_strdup (retval.c_str());
}

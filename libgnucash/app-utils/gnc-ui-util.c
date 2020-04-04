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
#include <libguile.h>
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


#include "qof.h"
#include "guile-mappings.h"
#include "gnc-prefs.h"
#include "gnc-module.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-engine.h"
#include "gnc-euro.h"
#include "gnc-hooks.h"
#include "gnc-session.h"
#include "engine-helpers.h"
#include "gnc-locale-utils.h"
#include "gnc-component-manager.h"
#include "gnc-features.h"
#include "gnc-guile-utils.h"

#define GNC_PREF_CURRENCY_CHOICE_LOCALE "currency-choice-locale"
#define GNC_PREF_CURRENCY_CHOICE_OTHER  "currency-choice-other"
#define GNC_PREF_CURRENCY_OTHER         "currency-other"
#define GNC_PREF_REVERSED_ACCTS_NONE    "reversed-accounts-none"
#define GNC_PREF_REVERSED_ACCTS_CREDIT  "reversed-accounts-credit"
#define GNC_PREF_REVERSED_ACCTS_INC_EXP "reversed-accounts-incomeexpense"
#define GNC_PREF_PRICES_FORCE_DECIMAL   "force-price-decimal"

static QofLogModule log_module = GNC_MOD_GUI;

static gboolean auto_decimal_enabled = FALSE;
static int auto_decimal_places = 2;    /* default, can be changed */

static gboolean reverse_balance_inited = FALSE;
static gboolean reverse_type[NUM_ACCOUNT_TYPES];

/* Cache currency ISO codes and only look them up in the settings when
 * absolutely necessary. Can't cache a pointer to the data structure
 * as that will change any time the book changes. */
static gchar *user_default_currency = NULL;
static gchar *user_report_currency = NULL;
const static int maximum_decimals = 15;
static const gint64 pow_10[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
                               10000000, 100000000, 1000000000, 10000000000,
                               100000000000, 1000000000000, 10000000000000,
                               100000000000000, 1000000000000000};

gchar *gnc_normalize_account_separator (const gchar* separator)
{
    gchar *new_sep=NULL;

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
    gchar *separator;
    char *string;

    string = gnc_prefs_get_string(GNC_PREFS_GROUP_GENERAL, GNC_PREF_ACCOUNT_SEPARATOR);
    separator = gnc_normalize_account_separator (string);

    gnc_set_account_separator(separator);

    g_free(string);
    g_free(separator);
}


static void
gnc_configure_reverse_balance (void)
{
    gint i;

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
        reverse_type[i] = FALSE;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_INC_EXP))
    {
        reverse_type[ACCT_TYPE_INCOME]  = TRUE;
        reverse_type[ACCT_TYPE_EXPENSE] = TRUE;
    }
    else if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_CREDIT))
    {
        reverse_type[ACCT_TYPE_LIABILITY] = TRUE;
        reverse_type[ACCT_TYPE_PAYABLE]   = TRUE;
        reverse_type[ACCT_TYPE_EQUITY]    = TRUE;
        reverse_type[ACCT_TYPE_INCOME]    = TRUE;
        reverse_type[ACCT_TYPE_CREDIT]    = TRUE;
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
    int type;

    if (account == NULL)
        return FALSE;

    type = xaccAccountGetType (account);
    if ((type < 0) || (type >= NUM_ACCOUNT_TYPES))
        return FALSE;

    if (!reverse_balance_inited)
        gnc_reverse_balance_init ();

    return reverse_type[type];
}

gboolean gnc_using_unreversed_budgets (QofBook* book)
{
    return gnc_features_check_used (book, GNC_FEATURE_BUDGET_UNREVERSED);
}

/* similar to gnc_reverse_balance but also accepts a gboolean
   unreversed which specifies the reversal strategy - FALSE = pre-4.x
   always-assume-credit-accounts, TRUE = all amounts unreversed */
gboolean
gnc_reverse_budget_balance (const Account *account, gboolean unreversed)
{
    if (unreversed == gnc_using_unreversed_budgets(gnc_account_get_book(account)))
        return gnc_reverse_balance (account);

    return FALSE;
}


gchar *
gnc_get_default_directory (const gchar *section)
{
    gchar *dir;

    dir = gnc_prefs_get_string (section, GNC_PREF_LAST_PATH);
    if (!dir)
#ifdef G_OS_WIN32
        dir = g_strdup (g_get_user_data_dir ()); /* equivalent of "My Documents" */
#else
        dir = g_strdup (g_get_home_dir ());
#endif

    return dir;
}

void
gnc_set_default_directory (const gchar *section, const gchar *directory)
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
    return ((!gnc_current_session_exist() ||
                            (gnc_current_session_exist() &&
                                !gnc_account_get_children(
                                    gnc_book_get_root_account(
                                        gnc_get_current_book()))))
                            ? TRUE : FALSE);
}

#define OPTION_TAXUS_NAME "tax_US/name"
#define OPTION_TAXUS_TYPE "tax_US/type"
#define OLD_OPTION_TAXUS_NAME "book/tax_US/name"
#define OLD_OPTION_TAXUS_TYPE "book/tax_US/type"

void
gnc_set_current_book_tax_name_type (gboolean name_changed, const gchar *tax_name,
                                    gboolean type_changed, const gchar *tax_type)
{
    if (name_changed)
    {
        if (type_changed)
        {
            QofBook* book = gnc_get_current_book();
            if ((g_strcmp0 (tax_name, "") == 0) ||
                (tax_name == NULL))
            { /* change to no name */
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == NULL))
                { /* need to delete both name and type and the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, NULL);
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, NULL);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* delete the name & change the type; keep the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, NULL);
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, tax_type);
                }
            }
            else /* new name */
            {
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == NULL))
                { /* delete the type & change the name; keep the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, NULL);
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
            QofBook* book = gnc_get_current_book();
            if ((g_strcmp0 (tax_name, "") == 0) ||
                (tax_name == NULL))
            { /* change to no name */
                if ((g_strcmp0 (tax_type, "Other") == 0) ||
                    (g_strcmp0 (tax_type, "") == 0) ||
                    (tax_type == NULL))
                { /* delete the name; there is no type; deleted the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, NULL);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* need to delete the name and keep "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_NAME, NULL);
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
            QofBook* book = gnc_get_current_book();
            if ((g_strcmp0 (tax_type, "Other") == 0) ||
                (g_strcmp0 (tax_type, "") == 0) ||
                (tax_type == NULL))
            {
                if ((g_strcmp0 (tax_name, "") == 0) ||
                    (tax_name == NULL))
                {/* delete the type; there is no name; delete the "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, NULL);
                    qof_book_option_frame_delete(book, "tax_US");
                }
                else
                { /* need to delete the type and keep "tax_US" frame */
                    qof_book_set_string_option(book, OPTION_TAXUS_TYPE, NULL);
                }
            }
            else
            { /* change the type & keep "tax_US" frame */
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, tax_type);
            }
        } /*else no name and no type change - do nothing */
   }
}

const gchar *
gnc_get_current_book_tax_name (void)
{
    QofBook* book = gnc_get_current_book();
    const char* tax_name =
        qof_book_get_string_option(book, OPTION_TAXUS_NAME);
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
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, NULL);
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, NULL);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
                g_free (taxus_type);
            }
            else
            { /* switch just name and remove unused frames */
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, taxus_name);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, NULL);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
            }
            g_free (taxus_name);
            return qof_book_get_string_option(book, OPTION_TAXUS_NAME);
        }
        return NULL;
    }
}

const gchar *
gnc_get_current_book_tax_type (void)
{
    QofBook* book = gnc_get_current_book();
    const char* tax_type =
        qof_book_get_string_option(book, OPTION_TAXUS_TYPE);
    if (tax_type)
    {
        return tax_type;
    }
    else
    {
        const char* old_option_taxus_type =
            qof_book_get_string_option(book, OLD_OPTION_TAXUS_TYPE);
        if (old_option_taxus_type)
        {
            char* taxus_type = g_strdup(old_option_taxus_type);
            const char* old_option_taxus_name =
                qof_book_get_string_option(book, OLD_OPTION_TAXUS_NAME);
            if (old_option_taxus_name)
            { /* switch both name and type and remove unused frames */
                char* taxus_name = g_strdup(old_option_taxus_name);
                qof_book_set_string_option(book, OPTION_TAXUS_NAME, taxus_name);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_NAME, NULL);
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, NULL);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
                g_free (taxus_name);
            }
            else
            { /* switch just type and remove unused frames */
                qof_book_set_string_option(book, OPTION_TAXUS_TYPE, taxus_type);
                qof_book_set_string_option(book, OLD_OPTION_TAXUS_TYPE, NULL);
                qof_book_option_frame_delete(book, "book/tax_US");
                qof_book_option_frame_delete(book, "book");
            }
            g_free (taxus_type);
            return qof_book_get_string_option(book, OPTION_TAXUS_TYPE);
        }
        return NULL;
    }
}

/** Calls gnc_book_option_num_field_source_change to initiate registered
  * callbacks when num_field_source book option changes so that
  * registers/reports can update themselves; sets feature flag */
void
gnc_book_option_num_field_source_change_cb (gboolean num_action)
{
    gnc_suspend_gui_refresh ();
    if (num_action)
    {
    /* Set a feature flag in the book for use of the split action field as number.
     * This will prevent older GnuCash versions that don't support this feature
     * from opening this file. */
        gnc_features_set_used (gnc_get_current_book(),
                                                GNC_FEATURE_NUM_FIELD_SOURCE);
    }
    gnc_book_option_num_field_source_change (num_action);
    gnc_resume_gui_refresh ();
}

/** Calls gnc_book_option_book_currency_selected to initiate registered
  * callbacks when currency accounting book option changes to book-currency so
  * that registers/reports can update themselves; sets feature flag */
void
gnc_book_option_book_currency_selected_cb (gboolean use_book_currency)
{
    gnc_suspend_gui_refresh ();
    if (use_book_currency)
    {
    /* Set a feature flag in the book for use of book currency. This will
     * prevent older GnuCash versions that don't support this feature from
     * opening this file. */
        gnc_features_set_used (gnc_get_current_book(),
                                GNC_FEATURE_BOOK_CURRENCY);
    }
    gnc_book_option_book_currency_selected (use_book_currency);
    gnc_resume_gui_refresh ();
}

/** Returns TRUE if both book-currency and default gain/loss policy KVPs exist
  * and are valid and trading accounts are not used. */
gboolean
gnc_book_use_book_currency (QofBook *book)
{
    const gchar *policy;
    const gchar *currency;

    if (!book) return FALSE;

    policy = qof_book_get_default_gains_policy (book);
    currency = qof_book_get_book_currency_name (book);

    /* If either a default gain/loss policy or a book-currency does not exist,
       book-currency accounting method not valid */
    if (!policy || !currency)
       return FALSE;

    /* If both exist, both must be valid */
    if (!gnc_valid_policy_name (policy) || !gnc_commodity_table_lookup
                                            (gnc_commodity_table_get_table
                                              (gnc_get_current_book()),
                                                GNC_COMMODITY_NS_CURRENCY,
                                                 currency))
       return FALSE;

    /* If both exist and are valid, there must be no trading accounts flag */
    if (qof_book_use_trading_accounts (book))
       return FALSE;

    return TRUE;
}

/** Returns pointer to Book Currency name for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
const gchar *
gnc_book_get_book_currency_name (QofBook *book)
{
    if (!book) return NULL;

    if (gnc_book_use_book_currency (book))
        return qof_book_get_book_currency_name (book);

    return NULL;
}

/** Returns pointer to Book Currency for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
gnc_commodity *
gnc_book_get_book_currency (QofBook *book)
{
    if (!book) return NULL;

    if (gnc_book_use_book_currency (book))
        return gnc_commodity_table_lookup
                    (gnc_commodity_table_get_table(book),
                     GNC_COMMODITY_NS_CURRENCY,
                     qof_book_get_book_currency_name (book));

    return NULL;
}

/** Returns pointer to default gain/loss policy for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. */
const gchar *
gnc_book_get_default_gains_policy (QofBook *book)
{
    if (!book) return NULL;

    if (gnc_book_use_book_currency (book))
        return qof_book_get_default_gains_policy (book);

    return NULL;
}

/** Returns pointer to default gain/loss account for book or NULL; determines
  * that both book-currency and default gain/loss policy KVPs exist and that
  * both are valid, a requirement for the 'book-currency' currency accounting
  * method to apply. Also, account must not be hidden or a placeholder, and
  * must be of same currency as book-currency and income or expense type */
Account *
gnc_book_get_default_gain_loss_acct (QofBook *book)
{
    Account *gains_account = NULL;

    if (!book) return NULL;

    if (gnc_book_use_book_currency (book))
    {
        GncGUID *guid = qof_book_get_default_gain_loss_acct_guid (book);
        gains_account = xaccAccountLookup (guid, book);
        guid_free (guid);
    }

    if (gains_account &&
        !xaccAccountGetPlaceholder(gains_account) &&
        !xaccAccountGetHidden(gains_account) &&
        (gnc_commodity_equal(xaccAccountGetCommodity(gains_account),
                                    gnc_book_get_book_currency(book))) &&
        ((xaccAccountGetType(gains_account) == ACCT_TYPE_INCOME) ||
            (xaccAccountGetType(gains_account) == ACCT_TYPE_EXPENSE)))
    {
        return gains_account;
    }
    else
    {
        return NULL;
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
    return gnc_commodity_table_get_table (gnc_get_current_book ());
}

gchar *
gnc_get_account_name_for_split_register(const Account *account, gboolean show_leaf_accounts)
{
    if (show_leaf_accounts)
        return g_strdup (xaccAccountGetName (account));
    else
        return gnc_account_get_full_name (account);
}

gchar *
gnc_get_account_name_for_register(const Account *account)
{
    gboolean show_leaf_accounts;
    show_leaf_accounts = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                            GNC_PREF_SHOW_LEAF_ACCT_NAMES);

    return gnc_get_account_name_for_split_register(account, show_leaf_accounts);
}

Account *
gnc_account_lookup_for_register(const Account *base_account, const char *name)
{
    gboolean show_leaf_accounts;
    show_leaf_accounts = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                            GNC_PREF_SHOW_LEAF_ACCT_NAMES);

    if (show_leaf_accounts)
        return gnc_account_lookup_by_name (base_account, name);
    else
        return gnc_account_lookup_by_full_name (base_account, name);
}


/* Caller is responsible for g_free'ing returned memory */
char *
gnc_ui_account_get_tax_info_string (const Account *account)
{
    static SCM get_form = SCM_UNDEFINED;
    static SCM get_desc = SCM_UNDEFINED;

    gboolean tax_related = FALSE;
    const char *code;

    if (!account)
        return NULL;

    tax_related = xaccAccountGetTaxRelated (account);
    code = xaccAccountGetTaxUSCode (account);

    if (!code)
    {
        if (!tax_related)
            return NULL;
        /* tax_related && !code */
        else
            /* Translators: This and the following strings appear on
             * the account tab if the Tax Info column is displayed,
             * i.e. if the user wants to record the tax form number
             * and location on that tax form which corresponds to this
             * gnucash account. For the US Income Tax support in
             * gnucash, each tax code that can be assigned to an
             * account generally corresponds to a specific line number
             * on a paper form and each form has a unique
             * identification (e.g., Form 1040, Schedule A). */
            return g_strdup (_("Tax-related but has no tax code"));
    }
    else  /* with tax code */
    {
        const gchar *tax_type;
        GNCAccountType atype;
        SCM tax_entity_type;
        SCM category;
        gchar *num_code = NULL;
        const gchar *prefix = "N";
        gchar *return_string = NULL;

        tax_type = gnc_get_current_book_tax_type ();
        if (tax_type == NULL || (g_strcmp0 (tax_type, "") == 0))
            return g_strdup (_("Tax entity type not specified"));

        atype = xaccAccountGetType (account);
        tax_entity_type = scm_from_utf8_string (tax_type);

        if (get_form == SCM_UNDEFINED)
        {
            GNCModule module;
            const gchar *tax_module;
            /* load the tax info */
            /* This is a very simple hack that loads the (new, special) German
               tax definition file in a German locale, or (default) loads the
               US tax file. */
# ifdef G_OS_WIN32
            gchar *thislocale = g_win32_getlocale();
            gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
            g_free(thislocale);
# else /* !G_OS_WIN32 */
            const char *thislocale = setlocale(LC_ALL, NULL);
            gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
# endif /* G_OS_WIN32 */
            tax_module = is_de_DE ?
                         "gnucash/tax/de_DE" :
                         "gnucash/tax/us";

            module = gnc_module_load ((char *)tax_module, 0);

            g_return_val_if_fail (module, NULL);

            get_form = scm_c_eval_string
                       ("(false-if-exception gnc:txf-get-form)");
            get_desc = scm_c_eval_string
                       ("(false-if-exception gnc:txf-get-description)");
        }

        g_return_val_if_fail (scm_is_procedure (get_form), NULL);
        g_return_val_if_fail (scm_is_procedure (get_desc), NULL);

        category = scm_c_eval_string (atype == ACCT_TYPE_INCOME ?
                                      "txf-income-categories" :
                                      (atype == ACCT_TYPE_EXPENSE ?
                                       "txf-expense-categories" :
                                       (((atype == ACCT_TYPE_BANK)      ||
                                         (atype == ACCT_TYPE_CASH)      ||
                                         (atype == ACCT_TYPE_ASSET)     ||
                                         (atype == ACCT_TYPE_STOCK)     ||
                                         (atype == ACCT_TYPE_MUTUAL)    ||
                                         (atype == ACCT_TYPE_RECEIVABLE)) ?
                                        "txf-asset-categories" :
                                        (((atype == ACCT_TYPE_CREDIT)    ||
                                          (atype == ACCT_TYPE_LIABILITY) ||
                                          (atype == ACCT_TYPE_EQUITY)    ||
                                          (atype == ACCT_TYPE_PAYABLE)) ?
                                         "txf-liab-eq-categories" : ""))));

        if (g_str_has_prefix (code, prefix))
        {
            const gchar *num_code_tmp;
            num_code_tmp = g_strdup (code);
            num_code_tmp++; /* to lose the leading N */
            num_code = g_strdup (num_code_tmp);
            num_code_tmp--;
            g_free ((gpointer *) num_code_tmp);
        }
        else
        {
            num_code = g_strdup (code);
        }

        if (category == SCM_UNDEFINED)
        {
            if (tax_related)
                return_string = g_strdup_printf
                                (_("Tax type %s: invalid code %s for account type"),
                                 tax_type, num_code);
            else
                return_string = g_strdup_printf
                                (_("Not tax-related; tax type %s: invalid code %s for account type"),
                                 tax_type, num_code);
        }
        else
        {
            SCM code_scm;
            SCM form_scm;
            code_scm = scm_from_locale_symbol (code);
            form_scm = scm_call_3 (get_form, category, code_scm, tax_entity_type);
            if (!scm_is_string (form_scm))
            {
                if (tax_related)
                    return_string =  g_strdup_printf
                                     (_("Invalid code %s for tax type %s"),
                                      num_code, tax_type);
                else
                    return_string =  g_strdup_printf
                                     (_("Not tax-related; invalid code %s for tax type %s"),
                                      num_code, tax_type);
            }
            else
            {
                gchar *form = NULL;

                /* Note: using scm_to_utf8_stringn directly here instead
                   of our wrapper gnc_scm_to_utf8_string. 'form' should
                   be freed with 'free' instead of 'g_free'. This will
                   be taken care of automatically during scm_dynwind_end,
                   because we inform guile of this memory allocation via
                   scm_dynwind_free a little further. */
                form = scm_to_utf8_stringn (form_scm, NULL);
                if (!form)
                {
                    if (tax_related)
                        return_string = g_strdup_printf
                                        (_("No form: code %s, tax type %s"), num_code,
                                         tax_type);
                    else
                        return_string = g_strdup_printf
                                        (_("Not tax-related; no form: code %s, tax type %s"),
                                         num_code, tax_type);
                }
                else
                {
                    SCM desc_scm;

                    /* Create a dynwind context because we will be calling (scm) functions
                       that potentially exit non-locally */
                    scm_dynwind_begin (0);
                    scm_dynwind_free (form);
                    desc_scm = scm_call_3 (get_desc, category, code_scm,
                                           tax_entity_type);
                    if (!scm_is_string (desc_scm))
                    {
                        if (tax_related)
                            return_string = g_strdup_printf
                                            (_("No description: form %s, code %s, tax type %s"),
                                             form, num_code, tax_type);
                        else
                            return_string = g_strdup_printf
                                            (_("Not tax-related; no description: form %s, code %s, tax type %s"),
                                             form, num_code, tax_type);
                    }
                    else
                    {
                        gchar *desc = NULL;
                        desc = gnc_scm_to_utf8_string (desc_scm);
                        if (!desc)
                        {
                            if (tax_related)
                                return_string = g_strdup_printf
                                                (_("No description: form %s, code %s, tax type %s"),
                                                 form, num_code, tax_type);
                            else
                                return_string = g_strdup_printf
                                                (_("Not tax-related; no description: form %s, code %s, tax type %s"),
                                                 form, num_code, tax_type);
                        }
                        else
                        {
                            gint64 copy_number;
                            gchar *copy_txt = NULL;
                            copy_number = xaccAccountGetTaxUSCopyNumber (account);
                            copy_txt = (copy_number == 1) ?
                                       g_strdup ("") :
                                       g_strdup_printf ("(%d)",
                                                        (gint) copy_number);
                            if (tax_related)
                            {
                                if (g_strcmp0 (form, "") == 0)
                                    return_string = g_strdup_printf ("%s", desc);
                                else
                                    return_string = g_strdup_printf ("%s%s: %s",
                                                                     form, copy_txt, desc);
                            }
                            else
                            {
                                return_string = g_strdup_printf
                                                (_("Not tax-related; %s%s: %s (code %s, tax type %s)"),
                                                 form, copy_txt, desc, num_code, tax_type);
                            }
                            g_free (copy_txt);
                        }
                        g_free (desc);
                    }
                    scm_dynwind_end ();
                }
            }
        }
        g_free (num_code);
        return return_string;
    }
}

/* Caller is responsible for g_free'ing returned memory */
char *
gnc_ui_account_get_tax_info_sub_acct_string (const Account *account)
{
    GList *descendant, *account_descendants;

    if (!account)
        return NULL;

    account_descendants = gnc_account_get_descendants (account);
    if (account_descendants)
    {
        gint sub_acct_tax_number = 0;
        for (descendant = account_descendants; descendant;
                descendant = g_list_next(descendant))
        {
            if (xaccAccountGetTaxRelated (descendant->data))
                sub_acct_tax_number++;
        }
        g_list_free (account_descendants);
        g_list_free (descendant);
        /* Translators: This and the following strings appear on
         * the account tab if the Tax Info column is displayed,
         * i.e. if the user wants to record the tax form number
         * and location on that tax form which corresponds to this
         * gnucash account. For the US Income Tax support in
         * gnucash, each tax code that can be assigned to an
         * account generally corresponds to a specific line number
         * on a paper form and each form has a unique
         * identification (e.g., Form 1040, Schedule A). */
        return (sub_acct_tax_number == 0) ? NULL :
               g_strdup_printf (_("(Tax-related subaccounts: %d)"),
                                sub_acct_tax_number);
    }
    else
        return NULL;
}

/********************************************************************\
 * gnc_get_reconcile_str                                            *
 *   return the i18n'd string for the given reconciled flag         *
 *                                                                  *
 * Args: reconciled_flag - the flag to convert into a string        *
 * Returns: the i18n'd reconciled string                            *
\********************************************************************/
const char *
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
        return NULL;
    }
}

/********************************************************************\
 * gnc_get_reconcile_valid_flags                                    *
 *   return a string containing the list of reconciled flags        *
 *                                                                  *
 * Returns: the i18n'd reconciled flags string                      *
\********************************************************************/
const char *
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
const char *
gnc_get_reconcile_flag_order (void)
{
    static const char flags[] = { NREC, CREC, 0 };
    return flags;
}


static const char *
equity_base_name (GNCEquityType equity_type)
{
    switch (equity_type)
    {
    case EQUITY_OPENING_BALANCE:
        return N_("Opening Balances");

    case EQUITY_RETAINED_EARNINGS:
        return N_("Retained Earnings");

    default:
        return NULL;
    }
}

Account *
gnc_find_or_create_equity_account (Account *root,
                                   GNCEquityType equity_type,
                                   gnc_commodity *currency)
{
    Account *parent;
    Account *account;
    gboolean name_exists;
    gboolean base_name_exists;
    const char *base_name;
    char *name;

    g_return_val_if_fail (equity_type >= 0, NULL);
    g_return_val_if_fail (equity_type < NUM_EQUITY_TYPES, NULL);
    g_return_val_if_fail (currency != NULL, NULL);
    g_return_val_if_fail (root != NULL, NULL);

    base_name = equity_base_name (equity_type);

    account = gnc_account_lookup_by_name(root, base_name);
    if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
        account = NULL;

    if (!account)
    {
        base_name = base_name && *base_name ? _(base_name) : "";

        account = gnc_account_lookup_by_name(root, base_name);
        if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
            account = NULL;
    }

    base_name_exists = (account != NULL);

    if (account &&
            gnc_commodity_equiv (currency, xaccAccountGetCommodity (account)))
        return account;

    name = g_strconcat (base_name, " - ",
                        gnc_commodity_get_mnemonic (currency), NULL);
    account = gnc_account_lookup_by_name(root, name);
    if (account && xaccAccountGetType (account) != ACCT_TYPE_EQUITY)
        account = NULL;

    name_exists = (account != NULL);

    if (account &&
            gnc_commodity_equiv (currency, xaccAccountGetCommodity (account)))
        return account;

    /* Couldn't find one, so create it */
    if (name_exists && base_name_exists)
    {
        PWARN ("equity account with unexpected currency");
        g_free (name);
        return NULL;
    }

    if (!base_name_exists &&
            gnc_commodity_equiv (currency, gnc_default_currency ()))
    {
        g_free (name);
        name = g_strdup (base_name);
    }

    parent = gnc_account_lookup_by_name(root, _("Equity"));
    if (!parent || xaccAccountGetType (parent) != ACCT_TYPE_EQUITY)
        parent = root;
    g_assert(parent);

    account = xaccMallocAccount (gnc_account_get_book(root));

    xaccAccountBeginEdit (account);

    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_EQUITY);
    xaccAccountSetCommodity (account, currency);

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
    Account *equity_account;
    Transaction *trans;
    Split *split;

    if (gnc_numeric_zero_p (balance))
        return TRUE;

    g_return_val_if_fail (account != NULL, FALSE);

    equity_account =
        gnc_find_or_create_equity_account (gnc_account_get_root(account),
                                           EQUITY_OPENING_BALANCE,
                                           xaccAccountGetCommodity (account));
    if (!equity_account)
        return FALSE;

    xaccAccountBeginEdit (account);
    xaccAccountBeginEdit (equity_account);

    trans = xaccMallocTransaction (book);

    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, gnc_account_or_default_currency (account, NULL));
    xaccTransSetDatePostedSecsNormalized (trans, date);
    xaccTransSetDescription (trans, _("Opening Balance"));

    split = xaccMallocSplit (book);

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

#if 0 /* Not Used */
static void
gnc_lconv_set_utf8 (char **p_value, char *default_value)
{
    char *value = *p_value;
    *p_value = NULL;

    if ((value == NULL) || (value[0] == 0))
        value = default_value;

#ifdef G_OS_WIN32
    {
        /* get number of resulting wide characters */
        size_t count = mbstowcs (NULL, value, 0);
        if (count > 0)
        {
            /* malloc and convert */
            wchar_t *wvalue = g_malloc ((count + 1) * sizeof(wchar_t));
            count = mbstowcs (wvalue, value, count + 1);
            if (count > 0)
            {
                *p_value = g_utf16_to_utf8 (wvalue, -1, NULL, NULL, NULL);
            }
            g_free (wvalue);
        }
    }
#else /* !G_OS_WIN32 */
    *p_value = g_locale_to_utf8 (value, -1, NULL, NULL, NULL);
#endif

    if (*p_value == NULL)
    {
        // The g_locale_to_utf8 conversion failed. FIXME: Should we rather
        // use an empty string instead of the default_value? Not sure.
        *p_value = default_value;
    }
}

static void
gnc_lconv_set_char (char *p_value, char default_value)
{
    if ((p_value != NULL) && (*p_value == CHAR_MAX))
        *p_value = default_value;
}
#endif /* Not Used */

gnc_commodity *
gnc_locale_default_currency_nodefault (void)
{
    gnc_commodity * currency;
    gnc_commodity_table *table;
    const char *code;

    table = gnc_get_current_commodities ();
    code = gnc_locale_default_iso_currency_code ();

    currency = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, code);

    /* Some very old locales (notably on win32) still announce a euro
       currency as default, although it has been replaced by EUR in
       2001. We use EUR as default in that case, but the user can always
       override from gsettings. */
    if (gnc_is_euro_currency (currency))
        currency = gnc_get_euro();

    return (currency ? currency : NULL);
}

gnc_commodity *
gnc_locale_default_currency (void)
{
    gnc_commodity * currency = gnc_locale_default_currency_nodefault ();

    return (currency ? currency :
            gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                        GNC_COMMODITY_NS_CURRENCY, "USD"));
}


static gnc_commodity *
gnc_default_currency_common (gchar *requested_currency,
                             const gchar *section)
{
    gnc_commodity *currency = NULL;
    gchar  *mnemonic;

    if (requested_currency)
        return gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                          GNC_COMMODITY_NS_CURRENCY,
                                          requested_currency);

    if (gnc_book_use_book_currency (gnc_get_current_book ()))
        return gnc_book_get_book_currency (gnc_get_current_book ());

    if (gnc_prefs_get_bool (section, GNC_PREF_CURRENCY_CHOICE_OTHER))
    {
        mnemonic = gnc_prefs_get_string(section, GNC_PREF_CURRENCY_OTHER);
        currency = gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                              GNC_COMMODITY_NS_CURRENCY, mnemonic);
        DEBUG("mnemonic %s, result %p", mnemonic ? mnemonic : "(null)", currency);
        g_free(mnemonic);
    }

    if (!currency)
        currency = gnc_locale_default_currency ();
    if (currency)
    {
        mnemonic = requested_currency;
// ??? Does anyone know what this is supposed to be doing?
//        requested_currency = g_strdup(gnc_commodity_get_mnemonic(currency));
        g_free(mnemonic);
    }
    return currency;
}

gnc_commodity *
gnc_default_currency (void)
{
    return gnc_default_currency_common (user_default_currency, GNC_PREFS_GROUP_GENERAL);
}

gnc_commodity * gnc_account_or_default_currency(const Account* account, gboolean * currency_from_account_found)
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



gnc_commodity *
gnc_default_report_currency (void)
{
    return gnc_default_currency_common (user_report_currency, GNC_PREFS_GROUP_GENERAL_REPORT);
}

static void
gnc_currency_changed_cb (GSettings *settings, gchar *key, gpointer user_data)
{
    user_default_currency = NULL;
    user_report_currency = NULL;
    gnc_hook_run(HOOK_CURRENCY_CHANGED, NULL);
}


GNCPrintAmountInfo
gnc_default_print_info (gboolean use_symbol)
{
    static GNCPrintAmountInfo info;
    static gboolean got_it = FALSE;
    struct lconv *lc;

    /* These must be updated each time. */
    info.use_symbol = use_symbol ? 1 : 0;
    info.commodity = gnc_default_currency ();

    if (got_it)
        return info;

    lc = gnc_localeconv ();

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

static gboolean
is_decimal_fraction (int fraction, guint8 *max_decimal_places_p)
{
    guint8 max_decimal_places = 0;

    if (fraction <= 0)
        return FALSE;

    while (fraction != 1)
    {
        if (fraction % 10 != 0)
            return FALSE;

        fraction = fraction / 10;
        max_decimal_places += 1;
    }

    if (max_decimal_places_p)
        *max_decimal_places_p = max_decimal_places;

    return TRUE;
}

GNCPrintAmountInfo
gnc_commodity_print_info (const gnc_commodity *commodity,
                          gboolean use_symbol)
{
    GNCPrintAmountInfo info;
    gboolean is_iso;

    if (commodity == NULL)
        return gnc_default_print_info (use_symbol);

    info.commodity = commodity;

    is_iso = gnc_commodity_is_iso (commodity);

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
    gboolean is_iso;
    int scu;

    if (account == NULL)
        return gnc_default_print_info (use_symbol);

    info.commodity = efffunc (account);

    is_iso = gnc_commodity_is_iso (info.commodity);

    scu = scufunc (account);

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

    info.commodity = NULL;

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
    static gboolean got_it = FALSE;

    if (!got_it)
    {
        info = gnc_default_print_info_helper (5);
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
gnc_default_price_print_info (const gnc_commodity *curr)
{
    GNCPrintAmountInfo info;
    gboolean force = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
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
    info.use_symbol = 0;
    info.use_locale = 1;
    info.monetary = 1;
    
    info.force_fit = force;
    info.round = force;
    return info;
}

GNCPrintAmountInfo
gnc_integral_print_info (void)
{
    static GNCPrintAmountInfo info;
    static gboolean got_it = FALSE;

    if (!got_it)
    {
        info = gnc_default_print_info_helper (0);
        got_it = TRUE;
    }

    return info;
}

/* Utility function for printing non-negative amounts */
static int
PrintAmountInternal(char *buf, gnc_numeric val, const GNCPrintAmountInfo *info)
{
    struct lconv *lc = gnc_localeconv();
    int num_whole_digits;
    char temp_buf[128];
    gnc_numeric whole, rounding;
    int min_dp, max_dp;
    gboolean value_is_negative, value_is_decimal;

    g_return_val_if_fail (info != NULL, 0);

    if (gnc_numeric_check (val))
    {
        PWARN ("Bad numeric: %s.",
               gnc_numeric_errorCode_to_string(gnc_numeric_check (val)));
        *buf = '\0';
        return 0;
    }

    /* Print the absolute value, but remember sign */
    value_is_negative = gnc_numeric_negative_p (val);
    val = gnc_numeric_abs (val);

    /* Try to print as decimal. */
    value_is_decimal = gnc_numeric_to_decimal(&val, NULL);
    if (!value_is_decimal && info->force_fit && info->round)
    {
        /* if there's a commodity use 100x the commodity's fraction. N.B. This
         * assumes that commodity fractions are multiples of 10, a reasonable
         * assumption in 2018. Otherwise, if there's a reasonable
         * max_decimal_places, use that.
         */
        const gint64 denom = info->commodity ?
             gnc_commodity_get_fraction(info->commodity) * 100 :
             (info->max_decimal_places &&
               info->max_decimal_places <= maximum_decimals) ?
             pow_10[info->max_decimal_places] : pow_10[maximum_decimals];
        val = gnc_numeric_convert(val, denom, GNC_HOW_RND_ROUND_HALF_UP);
        value_is_decimal = gnc_numeric_to_decimal(&val, NULL);
    }
    min_dp = info->min_decimal_places;
    max_dp = info->max_decimal_places;
    
    /* Don to limit the number of decimal places _UNLESS_ force_fit is
     * true. */
    if (!info->force_fit)
        max_dp = 99;

    /* rounding? -- can only ROUND if force_fit is also true */
    if (value_is_decimal && info->round && info->force_fit)
    {
        rounding.num = 5; /* Limit the denom to 10^13 ~= 2^44, leaving max at ~524288 */
        rounding.denom = pow(10, max_dp + 1);
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
    whole = gnc_numeric_convert(val, 1, GNC_HOW_RND_TRUNC);
    val = gnc_numeric_sub (val, whole, GNC_DENOM_AUTO, GNC_HOW_RND_NEVER);
    if (gnc_numeric_check (val))
    {
        PWARN ("Problem with remainder: %s.",
               gnc_numeric_errorCode_to_string(gnc_numeric_check (val)));
        *buf = '\0';
        return 0;
    }
    
    // Value may now be decimal, for example if the factional part is zero
    value_is_decimal = gnc_numeric_to_decimal(&val, NULL);
    /* print the integer part without separators */
    sprintf(temp_buf, "%" G_GINT64_FORMAT, whole.num);
    num_whole_digits = strlen (temp_buf);

    if (!info->use_separators)
        strcpy (buf, temp_buf);
    else
    {
        int group_count;
        char *separator;
        char *temp_ptr;
        char *buf_ptr;
        char *group;
        gchar *rev_buf;

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

        buf_ptr = buf;
        temp_ptr = &temp_buf[num_whole_digits - 1];
        group_count = 0;

        while (temp_ptr != temp_buf)
        {
            *buf_ptr++ = *temp_ptr--;

            if (*group != CHAR_MAX)
            {
                group_count++;

                if (group_count == *group)
                {
                    g_utf8_strncpy(buf_ptr, separator, 1);
                    buf_ptr = g_utf8_find_next_char(buf_ptr, NULL);
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
        rev_buf = g_utf8_strreverse(buf, -1);
        strcpy (buf, rev_buf);
        g_free(rev_buf);
    } /* endif */

    /* at this point, buf contains the whole part of the number */

    /* If it's not decimal, print the fraction as an expression. */
    if (!value_is_decimal)
    {
        val = gnc_numeric_reduce (val);

        if (val.denom > 0)
            sprintf (temp_buf, "%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
                     val.num, val.denom);
        else
            sprintf (temp_buf, "%" G_GINT64_FORMAT " * %" G_GINT64_FORMAT,
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
        char *decimal_point;
        guint8 num_decimal_places = 0;
        char *temp_ptr = temp_buf;

        decimal_point = info->monetary
                        ? lc->mon_decimal_point
                        : lc->decimal_point;
        g_utf8_strncpy(temp_ptr, decimal_point, 1);
        temp_ptr = g_utf8_find_next_char(temp_ptr, NULL);

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
xaccSPrintAmount (char * bufp, gnc_numeric val, GNCPrintAmountInfo info)
{
    struct lconv *lc;

    char *orig_bufp = bufp;
    const char *currency_symbol;
    const char *sign;

    char cs_precedes;
    char sep_by_space;
    char sign_posn;

    gboolean print_sign = TRUE;
    gboolean print_absolute = FALSE;

    if (!bufp)
        return 0;

    lc = gnc_localeconv();
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
    else /* !info.use_symbol || !info.commodity */
        currency_symbol = "";

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

    if (gnc_numeric_zero_p (val) || (sign == NULL) || (sign[0] == 0))
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

const char *
xaccPrintAmount (gnc_numeric val, GNCPrintAmountInfo info)
{
    /* hack alert -- this is not thread safe ... */
    static char buf[1024];

    if (!xaccSPrintAmount (buf, val, info))
        buf[0] = '\0';

    /* its OK to return buf, since we declared it static */
    return buf;
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
static gchar *small_numbers[] =
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
static gchar *medium_numbers[] =
{
    "Zero", "Ten", "Twenty", "Thirty", "Forty",
    "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"
};
static gchar *big_numbers[] =
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

static gchar *
integer_to_words(gint64 val)
{
    gint64 log_val, pow_val, this_part;
    GString *result;
    gchar *tmp;

    if (val == 0)
        return g_strdup("zero");
    if (val < 0)
        val = -val;

    result = g_string_sized_new(100);

    while (val >= 1000)
    {
        log_val = log10(val) / 3 + FUDGE;
        pow_val = exp(log_val * 3 * G_LN10) + FUDGE;
        this_part = val / pow_val;
        val -= this_part * pow_val;
        tmp = integer_to_words(this_part);
        g_string_append_printf(result, "%s %s ", tmp,
                               gettext(big_numbers[log_val]));
        g_free(tmp);
    }

    if (val >= 100)
    {
        this_part = val / 100;
        val -= this_part * 100;
        g_string_append_printf(result, "%s %s ",
                               gettext(small_numbers[this_part]),
                               gettext(big_numbers[0]));
    }

    if (val > 20)
    {
        this_part = val / 10;
        val -= this_part * 10;
        g_string_append(result, gettext(medium_numbers[this_part]));
        g_string_append_c(result, ' ');
    }

    if (val > 0)
    {
        this_part = val;
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

gchar *
number_to_words(gdouble val, gint64 denom)
{
    gint64 int_part, frac_part;
    gchar *int_string, *nomin_string, *denom_string, *full_string;

    if (val < 0) val = -val;
    if (denom < 0) denom = -denom;

    int_part = floor(val);
    frac_part = round((val - int_part) * denom);

    int_string = integer_to_words(int_part);
    /* Inside of the gettext macro _(...) we must not use any macros but
       only plain string literals. For this reason, convert the strings
       separately. */
    nomin_string = g_strdup_printf("%02" G_GINT64_FORMAT, frac_part);
    denom_string = g_strdup_printf("%" G_GINT64_FORMAT, denom);
    full_string =
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

gchar *
numeric_to_words(gnc_numeric val)
{
    return number_to_words(gnc_numeric_to_double(val),
                           gnc_numeric_denom(val));
}

const gchar *
printable_value (gdouble val, gint denom)
{
    GNCPrintAmountInfo info;
    gnc_numeric num;

    num = gnc_numeric_create(round(val * denom), denom);
    info = gnc_share_print_info_places(log10(denom));
    return xaccPrintAmount (num, info);
}

/********************************************************************\
 * xaccParseAmount                                                  *
 *   parses amount strings using locale data                        *
 *                                                                  *
 * Args: in_str   -- pointer to string rep of num                   *
 *       monetary -- boolean indicating whether value is monetary   *
 *       result   -- pointer to result location, may be NULL        *
 *       endstr   -- used to store first digit not used in parsing  *
 * Return: gboolean -- TRUE if a number found and parsed            *
 *                     If FALSE, result is not changed              *
\********************************************************************/

/* Parsing state machine states */
typedef enum
{
    START_ST,       /* Parsing initial whitespace */
    NEG_ST,         /* Parsed a negative sign or a left paren */
    PRE_GROUP_ST,   /* Parsing digits before grouping and decimal characters */
    START_GROUP_ST, /* Start of a digit group encountered (possibly) */
    IN_GROUP_ST,    /* Within a digit group */
    FRAC_ST,        /* Parsing the fractional portion of a number */
    DONE_ST,        /* Finished, number is correct module grouping constraints */
    NO_NUM_ST       /* Finished, number was malformed */
} ParseState;

#define done_state(state) (((state) == DONE_ST) || ((state) == NO_NUM_ST))

static inline long long int
multiplier (int num_decimals)
{
    switch (num_decimals)
    {
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
    default:
        PERR("bad fraction length");
        g_assert_not_reached();
        break;
    }

    return 1;
}

gboolean
xaccParseAmount (const char * in_str, gboolean monetary, gnc_numeric *result,
                 char **endstr)
{
    return xaccParseAmountPosSign (in_str, monetary, result, endstr, FALSE);
}

gboolean
xaccParseAmountPosSign (const char * in_str, gboolean monetary, gnc_numeric *result,
                        char **endstr, gboolean skip)
{
    struct lconv *lc = gnc_localeconv();

    gunichar negative_sign;
    gunichar decimal_point;
    gunichar group_separator;
    gchar *ignore = NULL;
    char *plus_sign = "+";
    char *group;

    negative_sign = g_utf8_get_char(lc->negative_sign);
    if (monetary)
    {
        group_separator = g_utf8_get_char(lc->mon_thousands_sep);
        decimal_point = g_utf8_get_char(lc->mon_decimal_point);
        group = lc->mon_grouping;
    }
    else
    {
        group_separator = g_utf8_get_char(lc->thousands_sep);
        decimal_point = g_utf8_get_char(lc->decimal_point);
        group = lc->grouping;
    }

    if (skip)
    {
        /* We want the locale's positive sign to be ignored.
        * If the locale doesn't specify one, we assume "+" as
        * an optional positive sign and ignore that */
        ignore = lc->positive_sign;
        if (!ignore || !*ignore)
            ignore = plus_sign;
    }

    return xaccParseAmountExtended(in_str, monetary, negative_sign, decimal_point,
                                   group_separator, group, ignore, result, endstr);
}

/* Note: xaccParseAmountExtended causes test-print-parse-amount
to fail if QOF_SCANF_LLD is simply replaced by G_GINT64_FORMAT. Why?
A: Because scanf and printf use different symbols for 64-bit numbers.
*/
gboolean
xaccParseAmountExtended (const char * in_str, gboolean monetary,
                         gunichar negative_sign, gunichar decimal_point,
                         gunichar group_separator, const char *group, const char *ignore_list,
                         gnc_numeric *result, char **endstr)
{
    gboolean is_negative;
    gboolean got_decimal;
    gboolean need_paren;
    GList * group_data;
    long long int numer;
    long long int denom;
    int count, group_count;

    ParseState state;

    const gchar *in;
    gunichar uc;
    gchar *out_str;
    gchar *out;

    /* Initialize *endstr to in_str */
    if (endstr != NULL)
        *endstr = (char *) in_str;

    if (in_str == NULL)
        return FALSE;

    if (!g_utf8_validate(in_str, -1, &in))
    {
        printf("Invalid utf8 string '%s'. Bad character at position %ld.\n",
               in_str, g_utf8_pointer_to_offset (in_str, in));
        return FALSE;
    }

    /* 'out_str' will be used to store digits for numeric conversion.
     * 'out' will be used to traverse out_str. */
    out = out_str = g_new(gchar, strlen(in_str) + 128);

    /* 'in' is used to traverse 'in_str'. */
    in = in_str;

    is_negative = FALSE;
    got_decimal = FALSE;
    need_paren = FALSE;
    group_data = NULL;
    group_count = 0;
    numer = 0;
    denom = 1;

    /* Initialize the state machine */
    state = START_ST;

    /* This while loop implements a state machine for parsing numbers. */
    while (TRUE)
    {
        ParseState next_state = state;

        uc = g_utf8_get_char(in);

        /* Ignore anything in the 'ignore list' */
        if (ignore_list && uc && g_utf8_strchr(ignore_list, -1, uc) != NULL)
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
                count = g_unichar_to_utf8(uc, out);
                out += count; /* we record the digits themselves in out_str
                         * for later conversion by libc routines */
                next_state = PRE_GROUP_ST;
            }
            else if (uc == decimal_point)
            {
                next_state = FRAC_ST;
            }
            else if (g_unichar_isspace(uc))
            {
            }
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
            {
                next_state = NO_NUM_ST;
            }

            break;

            /* NEG_ST means we have just parsed a negative sign. For now,
             * we only recognize formats where the negative sign comes first. */
        case NEG_ST:
            if (g_unichar_isdigit(uc))
            {
                count = g_unichar_to_utf8(uc, out);
                out += count;
                next_state = PRE_GROUP_ST;
            }
            else if (uc == decimal_point)
            {
                next_state = FRAC_ST;
            }
            else if (g_unichar_isspace(uc))
            {
            }
            else
            {
                next_state = NO_NUM_ST;
            }

            break;

            /* PRE_GROUP_ST means we have started parsing the number, but
             * have not encountered a decimal point or a grouping character. */
        case PRE_GROUP_ST:
            if (g_unichar_isdigit(uc))
            {
                count = g_unichar_to_utf8(uc, out);
                out += count;
            }
            else if (uc == decimal_point)
            {
                next_state = FRAC_ST;
            }
            else if (uc == group_separator)
            {
                next_state = START_GROUP_ST;
            }
            else if (uc == ')' && need_paren)
            {
                next_state = DONE_ST;
                need_paren = FALSE;
            }
            else
            {
                next_state = DONE_ST;
            }

            break;

            /* START_GROUP_ST means we have just parsed a group character.
             * Note that group characters might be whitespace!!! In general,
             * if a decimal point or a group character is whitespace, we
             * try to interpret it in the fashion that will allow parsing
             * of the current number to continue. */
        case START_GROUP_ST:
            if (g_unichar_isdigit(uc))
            {
                count = g_unichar_to_utf8(uc, out);
                out += count;
                group_count++; /* We record the number of digits
                          * in the group for later checking. */
                next_state = IN_GROUP_ST;
            }
            else if (uc == decimal_point)
            {
                /* If we now get a decimal point, and both the decimal
                 * and the group separator are also whitespace, assume
                 * the last group separator was actually whitespace and
                 * stop parsing. Otherwise, there's a problem. */
                if (g_unichar_isspace(group_separator) &&
                        g_unichar_isspace(decimal_point))
                    next_state = DONE_ST;
                else
                    next_state = NO_NUM_ST;
            }
            else if (uc == ')' && need_paren)
            {
                if (g_unichar_isspace(group_separator))
                {
                    next_state = DONE_ST;
                    need_paren = FALSE;
                }
                else
                    next_state = NO_NUM_ST;
            }
            else
            {
                /* If the last group separator is also whitespace,
                 * assume it was intended as such and stop parsing.
                 * Otherwise, there is a problem. */
                if (g_unichar_isspace(group_separator))
                    next_state = DONE_ST;
                else
                    next_state = NO_NUM_ST;
            }
            break;

            /* IN_GROUP_ST means we are in the middle of parsing
             * a group of digits. */
        case IN_GROUP_ST:
            if (g_unichar_isdigit(uc))
            {
                count = g_unichar_to_utf8(uc, out);
                out += count;
                group_count++; /* We record the number of digits
                          * in the group for later checking. */
            }
            else if (uc == decimal_point)
            {
                next_state = FRAC_ST;
            }
            else if (uc == group_separator)
            {
                next_state = START_GROUP_ST;
            }
            else if (uc == ')' && need_paren)
            {
                next_state = DONE_ST;
                need_paren = FALSE;
            }
            else
            {
                next_state = DONE_ST;
            }

            break;

            /* FRAC_ST means we are now parsing fractional digits. */
        case FRAC_ST:
            if (g_unichar_isdigit(uc))
            {
                count = g_unichar_to_utf8(uc, out);
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
                 * Otherwise, there is a problem. */
                if (g_unichar_isspace(group_separator))
                    next_state = DONE_ST;
                else
                    next_state = NO_NUM_ST;
            }
            else if (uc == ')' && need_paren)
            {
                next_state = DONE_ST;
                need_paren = FALSE;
            }
            else
            {
                next_state = DONE_ST;
            }

            break;

        default:
            PERR("bad state");
            g_assert_not_reached();
            break;
        }

        /* If we're moving out of the IN_GROUP_ST, record data for the group */
        if ((state == IN_GROUP_ST) && (next_state != IN_GROUP_ST))
        {
            group_data = g_list_prepend(group_data, GINT_TO_POINTER(group_count));
            group_count = 0;
        }

        /* If we're moving into the FRAC_ST or out of the machine
         * without going through FRAC_ST, record the integral value. */
        if (((next_state == FRAC_ST) && (state != FRAC_ST)) ||
                ((next_state == DONE_ST) && !got_decimal))
        {
            *out = '\0';

            if (*out_str != '\0' && sscanf(out_str, QOF_SCANF_LLD, &numer) < 1)
            {
                next_state = NO_NUM_ST;
            }
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
        g_list_free(group_data);
        return FALSE;
    }

    /* If there were groups, validate them */
    if (group_data != NULL)
    {
        gboolean good_grouping = TRUE;
        GList *node;

        /* The groups were built in reverse order. This
         * is the easiest order to verify them in. */
        for (node = group_data; group && node; node = node->next)
        {
            /* Verify group size */
            if (*group != GPOINTER_TO_INT(node->data))
            {
                good_grouping = FALSE;
                break;
            }

            /* Peek ahead at the next group code */
            switch (group[1])
            {
                /* A null char means repeat the last group indefinitely */
            case '\0':
                break;
                /* CHAR_MAX means no more grouping allowed */
            case CHAR_MAX:
                if (node->next != NULL)
                    good_grouping = FALSE;
                break;
                /* Anything else means another group size */
            default:
                group++;
                break;
            }

            if (!good_grouping)
                break;
        }

        g_list_free(group_data);

        if (!good_grouping)
        {
            g_free(out_str);
            return FALSE;
        }
    }

    /* Cap the end of the fraction string, if any */
    *out = '\0';

    /* Add in fractional value */
    if (got_decimal && (*out_str != '\0'))
    {
        size_t len;
        long long int fraction;

        len = strlen(out_str);

        if (len > 8)
        {
            out_str[8] = '\0';
            len = 8;
        }

        if (sscanf (out_str, QOF_SCANF_LLD, &fraction) < 1)
        {
            g_free(out_str);
            return FALSE;
        }

        denom = multiplier(len);
        numer *= denom;
        numer += fraction;
    }
    else if (monetary && auto_decimal_enabled && !got_decimal)
    {
        if ((auto_decimal_places > 0) && (auto_decimal_places < 9))
        {
            denom = multiplier(auto_decimal_places);

            /* No need to multiply numer by denom at this point,
             * since by specifying the auto decimal places the
             * user has effectively determined the scaling factor
             * for the numerator they entered.
             */
        }
    }

    if (result != NULL)
    {
        *result = gnc_numeric_create (numer, denom);
        if (is_negative)
            *result = gnc_numeric_neg (*result);
    }

    if (endstr != NULL)
        *endstr = (char *) in;

    g_free (out_str);

    return TRUE;
}

/* enable/disable the auto_decimal_enabled option */
static void
gnc_set_auto_decimal_enabled (gpointer settings, gchar *key, gpointer user_data)
{
    auto_decimal_enabled =
            gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_POINT);
}

/* set the number of auto decimal places to use */
static void
gnc_set_auto_decimal_places (gpointer settings, gchar *key, gpointer user_data)
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
                          gnc_configure_account_separator, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_NONE,
                          gnc_configure_reverse_balance, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_CREDIT,
                          gnc_configure_reverse_balance, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_REVERSED_ACCTS_INC_EXP,
                          gnc_configure_reverse_balance, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_CHOICE_LOCALE,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_CHOICE_OTHER,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_CURRENCY_OTHER,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_CHOICE_LOCALE,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_CHOICE_OTHER,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_CURRENCY_OTHER,
                          gnc_currency_changed_cb, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_POINT,
                          gnc_set_auto_decimal_enabled, NULL);
    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTO_DECIMAL_PLACES,
                          gnc_set_auto_decimal_places, NULL);

}

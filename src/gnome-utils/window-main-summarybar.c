/********************************************************************
 * window-main-summarybar.c -- summary of financial info            *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999,2000 Linas Vepstas                       *
 * Copyright (C) 2001 Bill Gribble                                  *
 * Copyright (C) 2005 Joshua Sled <jsled@asynchronous.org>          *
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
 ********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "gnc-accounting-period.h"
#include "gnc-component-manager.h"
#include "gnc-euro.h"
#include "gnc-event.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui-util.h"
#include "window-main-summarybar.h"

typedef struct
{
    GtkWidget * hbox;
    GtkWidget * totals_combo;
    GtkListStore *datamodel;
    int       component_id;
    int       cnxn_id;
} GNCMainSummary;

#define WINDOW_SUMMARYBAR_CM_CLASS "summary-bar"

#define GCONF_SECTION    "window/pages/account_tree/summary"
#define KEY_GRAND_TOTAL  "grand_total"
#define KEY_NON_CURRENCY "non_currency"

/**
 * An accumulator for a given currency.
 *
 * This is used during the update to the status bar to contain the
 * accumulation for a single currency. These are placed in a GList and
 * kept around for the duration of the calculation. There may, in fact
 * be better ways to do this, but none occurred.
 *
 * @todo This structure and the non-GUI code that computes it's values
 * should move into the engine.
 **/
typedef struct
{
    gnc_commodity * currency;
    gnc_numeric assets;
    gnc_numeric profits;
    gint total_mode;
} GNCCurrencyAcc;


/* defines for total_mode in GNCCurrencyAcc and GNCCurrencyItem */
#define TOTAL_SINGLE           0
#define TOTAL_CURR_TOTAL       1
#define TOTAL_NON_CURR_TOTAL   2
#define TOTAL_GRAND_TOTAL      3


/** options for summarybar **/
typedef struct
{
    gnc_commodity *default_currency;
    gboolean euro;
    gboolean grand_total;
    gboolean non_currency;
    time_t start_date;
    time_t end_date;
} GNCSummarybarOptions;

/**
 * Get the existing currency accumulator matching the given currency and
 * total-mode, or create a new one.
 **/
static GNCCurrencyAcc *
gnc_ui_get_currency_accumulator(GList **list, gnc_commodity * currency, gint total_mode)
{
    GList *current;
    GNCCurrencyAcc *found;

    for (current = g_list_first(*list); current; current = g_list_next(current))
    {
        found = current->data;
        if ((gnc_commodity_equiv(currency, found->currency))
                && (found->total_mode == total_mode))
        {
            return found;
        }
    }

    found = g_new0 (GNCCurrencyAcc, 1);
    found->currency = currency;
    found->assets = gnc_numeric_zero ();
    found->profits = gnc_numeric_zero ();
    found->total_mode = total_mode;
    *list = g_list_append (*list, found);

    return found;
}

/**
 * @fixme Move this non-GUI code into the engine.
 **/
static void
gnc_ui_accounts_recurse (Account *parent, GList **currency_list,
                         GNCSummarybarOptions options)
{
    gnc_numeric start_amount;
    gnc_numeric start_amount_default_currency;
    gnc_numeric end_amount;
    gnc_numeric end_amount_default_currency;
    GNCAccountType account_type;
    gnc_commodity * account_currency;
    gnc_commodity * euro_commodity;
    GNCCurrencyAcc *currency_accum = NULL;
    GNCCurrencyAcc *euro_accum = NULL;
    GNCCurrencyAcc *grand_total_accum = NULL;
    GNCCurrencyAcc *non_curr_accum = NULL;
    GList *children, *node;
    gboolean non_currency = FALSE;
    Timespec end_timespec;
    Timespec start_timespec;

    if (parent == NULL) return;

    children = gnc_account_get_children(parent);
    for (node = children; node; node = g_list_next(node))
    {
        Account *account = node->data;

        account_type = xaccAccountGetType(account);
        account_currency = xaccAccountGetCommodity(account);

        if (options.grand_total)
            grand_total_accum = gnc_ui_get_currency_accumulator(currency_list,
                                options.default_currency,
                                TOTAL_GRAND_TOTAL);

        if (options.euro)
        {
            euro_commodity = gnc_get_euro ();
            euro_accum = gnc_ui_get_currency_accumulator(currency_list,
                         euro_commodity,
                         TOTAL_CURR_TOTAL);
        }
        else
            euro_commodity = NULL;

        if (!gnc_commodity_is_currency(account_currency))
        {
            non_currency = TRUE;
            non_curr_accum = gnc_ui_get_currency_accumulator(currency_list,
                             options.default_currency,
                             TOTAL_NON_CURR_TOTAL);
        }

        if (!non_currency || options.non_currency)
        {
            currency_accum = gnc_ui_get_currency_accumulator(currency_list,
                             account_currency,
                             TOTAL_SINGLE);
        }

        switch (account_type)
        {
        case ACCT_TYPE_BANK:
        case ACCT_TYPE_CASH:
        case ACCT_TYPE_ASSET:
        case ACCT_TYPE_STOCK:
        case ACCT_TYPE_MUTUAL:
        case ACCT_TYPE_CREDIT:
        case ACCT_TYPE_LIABILITY:
        case ACCT_TYPE_PAYABLE:
        case ACCT_TYPE_RECEIVABLE:
            end_amount = xaccAccountGetBalanceAsOfDate(account, options.end_date);
            timespecFromTime_t(&end_timespec, options.end_date);
            end_amount_default_currency =
                xaccAccountConvertBalanceToCurrencyAsOfDate
                (account, end_amount, account_currency, options.default_currency,
                 timespecToTime_t(timespecCanonicalDayTime(end_timespec)));

            if (!non_currency || options.non_currency)
            {
                currency_accum->assets =
                    gnc_numeric_add (currency_accum->assets, end_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_RND_ROUND);
            }

            if (non_currency)
            {
                non_curr_accum->assets =
                    gnc_numeric_add (non_curr_accum->assets, end_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
            }

            if (options.grand_total)
            {
                grand_total_accum->assets =
                    gnc_numeric_add (grand_total_accum->assets, end_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
            }

            if (options.euro && (currency_accum != euro_accum))
            {
                euro_accum->assets =
                    gnc_numeric_add (euro_accum->assets,
                                     gnc_convert_to_euro(account_currency, end_amount),
                                     gnc_commodity_get_fraction (euro_commodity),
                                     GNC_RND_ROUND);
            }

            gnc_ui_accounts_recurse(account, currency_list, options);
            break;
        case ACCT_TYPE_INCOME:
        case ACCT_TYPE_EXPENSE:
            start_amount = xaccAccountGetBalanceAsOfDate(account, options.start_date);
            timespecFromTime_t(&start_timespec, options.start_date);
            start_amount_default_currency =
                xaccAccountConvertBalanceToCurrencyAsOfDate
                (account, start_amount, account_currency, options.default_currency,
                 timespecToTime_t(timespecCanonicalDayTime(start_timespec)));
            end_amount = xaccAccountGetBalanceAsOfDate(account, options.end_date);
            timespecFromTime_t(&end_timespec, options.end_date);
            end_amount_default_currency =
                xaccAccountConvertBalanceToCurrencyAsOfDate
                (account, end_amount, account_currency, options.default_currency,
                 timespecToTime_t(timespecCanonicalDayTime(end_timespec)));

            if (!non_currency || options.non_currency)
            {
                currency_accum->profits =
                    gnc_numeric_add (currency_accum->profits, start_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_RND_ROUND);
                currency_accum->profits =
                    gnc_numeric_sub (currency_accum->profits, end_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_RND_ROUND);
            }

            if (non_currency)
            {
                non_curr_accum->profits =
                    gnc_numeric_add (non_curr_accum->profits, start_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
                non_curr_accum->profits =
                    gnc_numeric_sub (non_curr_accum->profits, end_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
            }

            if (options.grand_total)
            {
                grand_total_accum->profits =
                    gnc_numeric_add (grand_total_accum->profits,
                                     start_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
                grand_total_accum->profits =
                    gnc_numeric_sub (grand_total_accum->profits,
                                     end_amount_default_currency,
                                     gnc_commodity_get_fraction (options.default_currency),
                                     GNC_RND_ROUND);
            }

            if (options.euro && (currency_accum != euro_accum))
            {
                euro_accum->profits =
                    gnc_numeric_add (euro_accum->profits,
                                     gnc_convert_to_euro(account_currency, start_amount),
                                     gnc_commodity_get_fraction (euro_commodity),
                                     GNC_RND_ROUND);
                euro_accum->profits =
                    gnc_numeric_sub (euro_accum->profits,
                                     gnc_convert_to_euro(account_currency, end_amount),
                                     gnc_commodity_get_fraction (euro_commodity),
                                     GNC_RND_ROUND);
            }

            gnc_ui_accounts_recurse(account, currency_list, options);
            break;
        case ACCT_TYPE_EQUITY:
            /* no-op, see comments at top about summing assets */
            break;
            /**
             * @fixme I don't know if this is right or if trading accounts should be
             *        treated like income and expense accounts.
             **/
        case ACCT_TYPE_TRADING:
            break;
        case ACCT_TYPE_CURRENCY:
        default:
            break;
        }
    }
    g_list_free(children);
}

static char*
get_total_mode_label(const char *mnemonic, int total_mode)
{
    char *label_str;
    // i.e., "$, grand total," [profits: $12,345.67, assets: $23,456.78]
    switch (total_mode)
    {
    case TOTAL_CURR_TOTAL:
        label_str = g_strdup_printf( _("%s, Total:"), mnemonic );
        break;
    case TOTAL_NON_CURR_TOTAL:
        label_str = g_strdup_printf( _("%s, Non Currency Commodities Total:"), mnemonic );
        break;
    case TOTAL_GRAND_TOTAL:
        label_str = g_strdup_printf( _("%s, Grand Total:"), mnemonic );
        break;
    case TOTAL_SINGLE:
    default:
        label_str = g_strdup_printf( _("%s:"), mnemonic );
        break;
    }
    return label_str;
}

enum
{
    COLUMN_MNEMONIC_TYPE,
    COLUMN_ASSETS,
    COLUMN_ASSETS_VALUE,
    COLUMN_PROFITS,
    COLUMN_PROFITS_VALUE,
    N_COLUMNS,
};

/* The gnc_main_window_summary_refresh() subroutine redraws summary
 * information. The statusbar includes two fields, titled 'profits'
 * and 'assets'. The total assets equal the sum of all of the
 * non-equity, non-income accounts.  In theory, assets also equals the
 * grand total value of the equity accounts, but that assumes that
 * folks are using the equity account type correctly (which is not
 * likely). Thus we show the sum of assets, rather than the sum of
 * equities.
 *
 * The EURO gets special treatment. There can be one line with
 * EUR amounts and a EUR (total) line which summs up all EURO
 * member currencies.
 *
 * There can be a 'grand total', too, which sums up all accounts
 * converted to one common currency and a total of all non
 * currency commodities (e.g. stock, funds).  */

static void
gnc_main_window_summary_refresh (GNCMainSummary * summary)
{
    Account *root;
    char asset_string[256];
    char profit_string[256];
    GNCCurrencyAcc *currency_accum;
    GList *currency_list;
    GList *current;
    GNCSummarybarOptions options;

    options.default_currency = gnc_default_report_currency ();

    options.euro = gnc_gconf_get_bool(GCONF_GENERAL, KEY_ENABLE_EURO, NULL);
    options.grand_total =
        gnc_gconf_get_bool(GCONF_SECTION, KEY_GRAND_TOTAL, NULL);
    options.non_currency =
        gnc_gconf_get_bool(GCONF_SECTION, KEY_NON_CURRENCY, NULL);
    options.start_date = gnc_accounting_period_fiscal_start();
    options.end_date = gnc_accounting_period_fiscal_end();

    currency_list = NULL;

    /* grand total should be first in the list */
    if (options.grand_total)
    {
        gnc_ui_get_currency_accumulator (&currency_list, options.default_currency,
                                         TOTAL_GRAND_TOTAL);
    }
    /* Make sure there's at least one accumulator in the list. */
    gnc_ui_get_currency_accumulator (&currency_list, options.default_currency,
                                     TOTAL_SINGLE);

    root = gnc_get_current_root_account ();
    gnc_ui_accounts_recurse(root, &currency_list, options);

    {
        GtkTreeIter iter;
        char asset_amount_string[256], profit_amount_string[256];
        struct lconv *lc;

        lc = gnc_localeconv();

        g_object_ref(summary->datamodel);
        gtk_combo_box_set_model(GTK_COMBO_BOX(summary->totals_combo), NULL);
        gtk_list_store_clear(summary->datamodel);
        for (current = g_list_first(currency_list); current; current = g_list_next(current))
        {
            const char *mnemonic;
            gchar *total_mode_label;

            currency_accum = current->data;

            if (gnc_commodity_equiv (currency_accum->currency, gnc_locale_default_currency ()))
                mnemonic = lc->currency_symbol;
            else
                mnemonic = gnc_commodity_get_mnemonic (currency_accum->currency);

            if (mnemonic == NULL)
                mnemonic = "";

            *asset_string = '\0';
            xaccSPrintAmount(asset_amount_string,
                             currency_accum->assets,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            *profit_string = '\0';
            xaccSPrintAmount(profit_amount_string,
                             currency_accum->profits,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            gtk_list_store_append(summary->datamodel, &iter);
            total_mode_label = get_total_mode_label(mnemonic, currency_accum->total_mode);
            gtk_list_store_set(summary->datamodel, &iter,
                               COLUMN_MNEMONIC_TYPE, total_mode_label,
                               COLUMN_ASSETS,        _("Assets:"),
                               COLUMN_ASSETS_VALUE,  asset_amount_string,
                               COLUMN_PROFITS,       _("Profits:"),
                               COLUMN_PROFITS_VALUE, profit_amount_string,
                               -1);
            g_free(total_mode_label);
        }
        gtk_combo_box_set_model(GTK_COMBO_BOX(summary->totals_combo),
                                GTK_TREE_MODEL(summary->datamodel));
        g_object_unref(summary->datamodel);

        gtk_combo_box_set_active(GTK_COMBO_BOX(summary->totals_combo), 0);
    }

    /* Free the list we created for this */
    for (current = g_list_first(currency_list);
            current;
            current = g_list_next(current))
    {
        g_free(current->data);
    }
    g_list_free(currency_list);
}

static void
gnc_main_window_summary_destroy_cb(GNCMainSummary *summary, gpointer data)
{
    gnc_gconf_remove_anon_notification(GCONF_SECTION, summary->cnxn_id);
    gnc_unregister_gui_component(summary->component_id);
    g_free(summary);
}

static void
summarybar_refresh_handler(GHashTable * changes, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}

static void
gconf_client_notify_cb (GConfClient *client,
                        guint cnxn_id,
                        GConfEntry *entry,
                        gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}

GtkWidget *
gnc_main_window_summary_new (void)
{
    GNCMainSummary  * retval = g_new0(GNCMainSummary, 1);
    GtkCellRenderer *textRenderer;
    int i;
    // These options lead to a better looking layout for the combo-box, where
    // the "Assets: $####.##" and "Profit: $####.##" values are visually next
    // to each other.
    gboolean expandOptions[] = { TRUE, FALSE, TRUE, FALSE, TRUE };

    retval->datamodel = gtk_list_store_new( N_COLUMNS,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING );

    retval->hbox         = gtk_hbox_new (FALSE, 5);
    retval->totals_combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (retval->datamodel));
    g_object_unref (retval->datamodel);

    retval->component_id = gnc_register_gui_component (WINDOW_SUMMARYBAR_CM_CLASS,
                           summarybar_refresh_handler,
                           NULL, retval);
    gnc_gui_component_watch_entity_type (retval->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_DESTROY
                                         | GNC_EVENT_ITEM_CHANGED);

    for ( i = 0; i < N_COLUMNS; i++ )
    {
        textRenderer = GTK_CELL_RENDERER(gtk_cell_renderer_text_new());
        gtk_cell_layout_pack_start( GTK_CELL_LAYOUT(retval->totals_combo), textRenderer, expandOptions[i] );
        gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(retval->totals_combo), textRenderer, "text", i );
    }

    gtk_container_set_border_width (GTK_CONTAINER (retval->hbox), 2);
    gtk_box_pack_start (GTK_BOX(retval->hbox), retval->totals_combo, TRUE, TRUE, 5);
    gtk_widget_show (retval->totals_combo);
    gtk_widget_show (retval->hbox);

    g_signal_connect_swapped (G_OBJECT (retval->hbox), "destroy",
                              G_CALLBACK (gnc_main_window_summary_destroy_cb),
                              retval);

    gnc_main_window_summary_refresh(retval);

    retval->cnxn_id =  gnc_gconf_add_anon_notification(GCONF_SECTION,
                       gconf_client_notify_cb,
                       retval);

    return retval->hbox;
}


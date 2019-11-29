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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "gnc-accounting-period.h"
#include "gnc-component-manager.h"
#include "gnc-euro.h"
#include "gnc-event.h"
#include "gnc-prefs.h"
#include "gnc-locale-utils.h"
#include "gnc-ui-util.h"
#include "window-main-summarybar.h"
#include "dialog-utils.h"

typedef struct
{
    GtkWidget    *hbox;
    GtkWidget    *totals_combo;
    GtkListStore *datamodel;
    int           component_id;
    int           cnxn_id;
    gboolean      combo_popped;
    gboolean      show_negative_color;
    gchar        *negative_color;
} GNCMainSummary;

#define WINDOW_SUMMARYBAR_CM_CLASS "summary-bar"

#define GNC_PREFS_GROUP       "window.pages.account-tree.summary"
#define GNC_PREF_GRAND_TOTAL  "grand-total"
#define GNC_PREF_NON_CURRENCY "non-currency"

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
    gboolean grand_total;
    gboolean non_currency;
    time64 start_date;
    time64 end_date;
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
    GNCCurrencyAcc *currency_accum = NULL;
    GNCCurrencyAcc *grand_total_accum = NULL;
    GNCCurrencyAcc *non_curr_accum = NULL;
    GList *children, *node;
    gboolean non_currency = FALSE;

    if (parent == NULL) return;

    children = gnc_account_get_children(parent);
    for (node = children; node; node = g_list_next(node))
    {
        Account *account = node->data;
        QofBook *book = gnc_account_get_book (account);
        GNCPriceDB *pricedb = gnc_pricedb_get_db (book);
        gnc_commodity *to_curr = options.default_currency;

        account_type = xaccAccountGetType(account);
        account_currency = xaccAccountGetCommodity(account);

        if (options.grand_total)
            grand_total_accum = gnc_ui_get_currency_accumulator(currency_list,
                                to_curr,
                                TOTAL_GRAND_TOTAL);

        if (!gnc_commodity_is_currency(account_currency))
        {
            non_currency = TRUE;
            non_curr_accum = gnc_ui_get_currency_accumulator(currency_list,
                             to_curr,
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
            end_amount_default_currency =
                gnc_pricedb_convert_balance_nearest_price_t64 (pricedb,
                                                               end_amount,
                                                               account_currency,
                                                               to_curr,
                                                               options.end_date);

            if (!non_currency || options.non_currency)
            {
                currency_accum->assets =
                    gnc_numeric_add (currency_accum->assets, end_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_HOW_RND_ROUND_HALF_UP);
            }

            if (non_currency)
            {
                non_curr_accum->assets =
                    gnc_numeric_add (non_curr_accum->assets, end_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
            }

            if (options.grand_total)
            {
                grand_total_accum->assets =
                    gnc_numeric_add (grand_total_accum->assets, end_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
            }

            gnc_ui_accounts_recurse(account, currency_list, options);
            break;
        case ACCT_TYPE_INCOME:
        case ACCT_TYPE_EXPENSE:
            start_amount = xaccAccountGetBalanceAsOfDate(account, options.start_date);
            start_amount_default_currency =
                gnc_pricedb_convert_balance_nearest_price_t64 (pricedb,
                                                              start_amount,
                                                              account_currency,
                                                              to_curr,
                                                              options.start_date);
            end_amount = xaccAccountGetBalanceAsOfDate(account, options.end_date);
            end_amount_default_currency =
                gnc_pricedb_convert_balance_nearest_price_t64 (pricedb,
                                                               end_amount,
                                                               account_currency,
                                                               to_curr,
                                                               options.end_date);

            if (!non_currency || options.non_currency)
            {
                currency_accum->profits =
                    gnc_numeric_add (currency_accum->profits, start_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_HOW_RND_ROUND_HALF_UP);
                currency_accum->profits =
                    gnc_numeric_sub (currency_accum->profits, end_amount,
                                     gnc_commodity_get_fraction (account_currency),
                                     GNC_HOW_RND_ROUND_HALF_UP);
            }

            if (non_currency)
            {
                non_curr_accum->profits =
                    gnc_numeric_add (non_curr_accum->profits, start_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
                non_curr_accum->profits =
                    gnc_numeric_sub (non_curr_accum->profits, end_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
            }

            if (options.grand_total)
            {
                grand_total_accum->profits =
                    gnc_numeric_add (grand_total_accum->profits,
                                     start_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
                grand_total_accum->profits =
                    gnc_numeric_sub (grand_total_accum->profits,
                                     end_amount_default_currency,
                                     gnc_commodity_get_fraction (to_curr),
                                     GNC_HOW_RND_ROUND_HALF_UP);
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
    COLUMN_ASSETS_NEG,
    COLUMN_PROFITS_NEG,
    N_COLUMNS
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
 * EUR amounts and a EUR (total) line which sums up all EURO
 * member currencies.
 *
 * There can be a 'grand total', too, which sums up all accounts
 * converted to one common currency and a total of all non
 * currency commodities (e.g. stock, funds).  */

static void
gnc_main_window_summary_refresh (GNCMainSummary * summary)
{
    Account *root;
    GNCCurrencyAcc *currency_accum;
    GList *currency_list;
    GList *current;
    GNCSummarybarOptions options;


    root = gnc_get_current_root_account ();
    options.default_currency = gnc_default_currency ();
    if (options.default_currency == NULL)
    {
        options.default_currency = xaccAccountGetCommodity(root);
    }

    options.grand_total =
        gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_GRAND_TOTAL);
    options.non_currency =
        gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_NON_CURRENCY);
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

    gnc_ui_accounts_recurse(root, &currency_list, options);

    {
        GtkTreeIter iter;
        char asset_amount_string[256], profit_amount_string[256];

        g_object_ref(summary->datamodel);
        gtk_combo_box_set_model(GTK_COMBO_BOX(summary->totals_combo), NULL);
        gtk_list_store_clear(summary->datamodel);
        for (current = g_list_first(currency_list); current; current = g_list_next(current))
        {
            const char *mnemonic;
            gchar *total_mode_label;

            currency_accum = current->data;

            mnemonic = gnc_commodity_get_nice_symbol (currency_accum->currency);
            if (mnemonic == NULL)
                mnemonic = "";

            xaccSPrintAmount(asset_amount_string,
                             currency_accum->assets,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            xaccSPrintAmount(profit_amount_string,
                             currency_accum->profits,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            gtk_list_store_append(summary->datamodel, &iter);
            total_mode_label = get_total_mode_label(mnemonic, currency_accum->total_mode);
            gtk_list_store_set(summary->datamodel, &iter,
                               COLUMN_MNEMONIC_TYPE, total_mode_label,
                               COLUMN_ASSETS,        _("Net Assets:"),
                               COLUMN_ASSETS_VALUE,  asset_amount_string,
                               COLUMN_ASSETS_NEG,    gnc_numeric_negative_p(currency_accum->assets),
                               COLUMN_PROFITS,       _("Profits:"),
                               COLUMN_PROFITS_VALUE, profit_amount_string,
                               COLUMN_PROFITS_NEG,   gnc_numeric_negative_p(currency_accum->profits),
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

static gchar*
get_negative_color_str (void)
{
    GdkRGBA color;
    GdkRGBA *rgba;
    gchar *color_str;
    GtkWidget *label = gtk_label_new ("Color");
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(label));
    gtk_style_context_add_class (context, "negative-numbers");
    gtk_style_context_get_color (context, GTK_STATE_FLAG_NORMAL, &color);
    rgba = gdk_rgba_copy (&color);

    color_str = g_strdup_printf ("#%02X%02X%02X",
                              (int)(0.5 + CLAMP (rgba->red, 0., 1.) * 255.),
                              (int)(0.5 + CLAMP (rgba->green, 0., 1.) * 255.),
                              (int)(0.5 + CLAMP (rgba->blue, 0., 1.) * 255.));
    gdk_rgba_free (rgba);
    return color_str;
}

static void
summarybar_update_color (gpointer gsettings, gchar *key, gpointer user_data)
{
    GNCMainSummary *summary = user_data;

    summary->negative_color = get_negative_color_str();
    summary->show_negative_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);

    gnc_main_window_summary_refresh (summary);
}

static void
gnc_main_window_summary_destroy_cb(GNCMainSummary *summary, gpointer data)
{
    gnc_prefs_remove_cb_by_id (GNC_PREFS_GROUP, summary->cnxn_id);
    gnc_unregister_gui_component(summary->component_id);

    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                                summarybar_update_color, summary);

    g_free (summary->negative_color);
    g_free (summary);
}

static void
summarybar_refresh_handler(GHashTable * changes, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}

static void
prefs_changed_cb (gpointer prefs, gchar *pref, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}

static gchar*
check_string_for_markup (gchar *string)
{
    gchar **strings;
    gchar *ret_string = g_strdup (string);

    if (g_strrstr (ret_string, "&") != NULL)
    {
        strings = g_strsplit (ret_string, "&", -1);
        g_free (ret_string);
        ret_string = g_strjoinv ("&amp;", strings);
        g_strfreev (strings);
    }
    if (g_strrstr (ret_string, "<") != NULL)
    {
        strings = g_strsplit (ret_string, "<", -1);
        g_free (ret_string);
        ret_string = g_strjoinv ("&lt;", strings);
        g_strfreev (strings);
    }
    if (g_strrstr (ret_string, ">") != NULL)
    {
        strings = g_strsplit (ret_string, ">", -1);
        g_free (ret_string);
        ret_string = g_strjoinv ("&gt;", strings);
        g_strfreev (strings);
    }
    if (g_strrstr (ret_string, "\"") != NULL)
    {
        strings = g_strsplit (ret_string, "\"", -1);
        g_free (ret_string);
        ret_string = g_strjoinv ("&quot;", strings);
        g_strfreev (strings);
    }
    if (g_strrstr (ret_string, "'") != NULL)
    {
        strings = g_strsplit (ret_string, "'", -1);
        g_free (ret_string);
        ret_string = g_strjoinv ("&apos;", strings);
        g_strfreev (strings);
    }
    return ret_string;
}

static void
cdf (GtkCellLayout *cell_layout, GtkCellRenderer *cell, GtkTreeModel *tree_model, GtkTreeIter *iter,
                          gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gchar *type, *assets, *assets_val, *profits, *profits_val;
    gboolean assets_neg, profits_neg;
    gint viewcol;

    viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "view_column"));

    if (summary->combo_popped)
        g_object_set (cell, "xalign", 0.0, NULL);
    else
        g_object_set (cell, "xalign", 0.5, NULL);

    gtk_tree_model_get (GTK_TREE_MODEL (tree_model), iter,
                            COLUMN_MNEMONIC_TYPE, &type,
                            COLUMN_ASSETS, &assets,
                            COLUMN_ASSETS_VALUE, &assets_val,
                            COLUMN_PROFITS, &profits,
                            COLUMN_PROFITS_VALUE, &profits_val,
                            COLUMN_ASSETS_NEG, &assets_neg,
                            COLUMN_PROFITS_NEG, &profits_neg, -1);

    if (viewcol == 0)
        g_object_set (cell, "text", type, NULL);

    if (viewcol == 2)
    {
        gchar *a_string, *checked_string = check_string_for_markup (assets_val);
        if ((summary->show_negative_color == TRUE) && (assets_neg == TRUE))
            a_string = g_strconcat (assets, " <span foreground='", summary->negative_color, "'>", checked_string, "</span>", NULL);
        else
            a_string = g_strconcat (assets, " ", checked_string, NULL);

        g_object_set (cell, "markup", a_string, NULL);
        g_free (a_string);
        g_free (checked_string);
    }

    if (viewcol == 4)
    {
        gchar *p_string, *checked_string = check_string_for_markup (profits_val);
        if ((summary->show_negative_color == TRUE) && (profits_neg == TRUE))
            p_string = g_strconcat (profits, " <span foreground='", summary->negative_color, "'>", checked_string, "</span>", NULL);
        else
            p_string = g_strconcat (profits, " ", checked_string, NULL);

        g_object_set (cell, "markup", p_string, NULL);
        g_free (p_string);
        g_free (checked_string);
    }

    g_free (type);
    g_free (assets);
    g_free (assets_val);
    g_free (profits);
    g_free (profits_val);
}

static void
summary_combo_popped (GObject *widget, GParamSpec *pspec, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    if (summary->combo_popped)
        summary->combo_popped = FALSE;
    else
        summary->combo_popped = TRUE;
}

GtkWidget *
gnc_main_window_summary_new (void)
{
    GNCMainSummary  * retval = g_new0(GNCMainSummary, 1);
    GtkCellRenderer *textRenderer;
    int i;

    retval->datamodel = gtk_list_store_new (N_COLUMNS,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_BOOLEAN,
                                            G_TYPE_BOOLEAN);

    retval->hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (retval->hbox), FALSE);

    // Set the style context for this widget so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(retval->hbox), "summary-bar");

    retval->totals_combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (retval->datamodel));
    g_object_unref (retval->datamodel);

    retval->negative_color = get_negative_color_str();
    retval->show_negative_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                          summarybar_update_color, retval);

    retval->component_id = gnc_register_gui_component (WINDOW_SUMMARYBAR_CM_CLASS,
                           summarybar_refresh_handler,
                           NULL, retval);
    gnc_gui_component_watch_entity_type (retval->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_DESTROY
                                         | GNC_EVENT_ITEM_CHANGED);

    // Allows you to get when the popup menu is present
    g_signal_connect (retval->totals_combo, "notify::popup-shown",G_CALLBACK (summary_combo_popped), retval);

    retval->combo_popped = FALSE;

    for (i = 0; i <= N_COLUMNS - 2; i += 2)
    {
        textRenderer = GTK_CELL_RENDERER(gtk_cell_renderer_text_new());

        gtk_cell_renderer_set_fixed_size (textRenderer, 50, -1);

        gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(retval->totals_combo), textRenderer, TRUE);

        g_object_set_data (G_OBJECT(textRenderer), "view_column", GINT_TO_POINTER (i));
        gtk_cell_layout_set_cell_data_func (GTK_CELL_LAYOUT(retval->totals_combo), textRenderer, cdf, retval, NULL);
    }

    gtk_container_set_border_width (GTK_CONTAINER (retval->hbox), 2);
    gtk_box_pack_start (GTK_BOX(retval->hbox), retval->totals_combo, TRUE, TRUE, 5);
    gtk_widget_show (retval->totals_combo);
    gtk_widget_show (retval->hbox);

    g_signal_connect_swapped (G_OBJECT (retval->hbox), "destroy",
                              G_CALLBACK (gnc_main_window_summary_destroy_cb),
                              retval);

    gnc_main_window_summary_refresh(retval);

    retval->cnxn_id =  gnc_prefs_register_cb (GNC_PREFS_GROUP, NULL,
                       prefs_changed_cb, retval);

    return retval->hbox;
}

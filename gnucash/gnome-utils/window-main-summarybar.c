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

/* The summary bar is a button showing the currently selected row of the
 * datamodel (mnemonic, net assets, profits). Clicking it pops up a
 * GtkMenu listing every row, anchored so that the menu's bottom edge
 * sits on the top edge of the bar (the bar lives at the bottom of the
 * main window, so there is never room below it), with the current row
 * pre-selected. */
typedef struct
{
    GtkWidget    *hbox;
    GtkWidget    *button;
    GtkLabel     *bar_labels[3];
    GtkListStore *datamodel;
    GtkWidget    *menu;
    gchar        *active_key;     /* COLUMN_KEY of the row shown in the bar */
    int           component_id;
    int           cnxn_id;
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

    if (parent == NULL) return;

    children = gnc_account_get_children(parent);
    for (node = children; node; node = g_list_next(node))
    {
        Account *account = node->data;
        QofBook *book = gnc_account_get_book (account);
        GNCPriceDB *pricedb = gnc_pricedb_get_db (book);
        gnc_commodity *to_curr = options.default_currency;
        gboolean non_currency = FALSE;

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
get_total_mode_label (GNCCurrencyAcc *currency_accum)
{
    const char *mnemonic = gnc_commodity_get_nice_symbol (currency_accum->currency);
    char *label_str;
    if (mnemonic == NULL)
        mnemonic = "";
    // i.e., "$, grand total," [profits: $12,345.67, assets: $23,456.78]
    switch (currency_accum->total_mode)
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
    COLUMN_KEY,    /* stable row id: "<total_mode>:<commodity unique name>" */
    COLUMN_MNEMONIC_TYPE,
    COLUMN_ASSETS,
    COLUMN_ASSETS_VALUE,
    COLUMN_PROFITS,
    COLUMN_PROFITS_VALUE,
    COLUMN_ASSETS_NEG,
    COLUMN_PROFITS_NEG,
    N_COLUMNS
};

static void summary_update_bar (GNCMainSummary *summary);

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

        gtk_list_store_clear(summary->datamodel);
        for (current = g_list_first(currency_list); current; current = g_list_next(current))
        {
            gchar *total_mode_label;
            gchar *bidi_total, *bidi_asset_amount, *bidi_profit_amount;

            currency_accum = current->data;

            xaccSPrintAmount(asset_amount_string,
                             currency_accum->assets,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            xaccSPrintAmount(profit_amount_string,
                             currency_accum->profits,
                             gnc_commodity_print_info(currency_accum->currency, TRUE));

            gtk_list_store_append(summary->datamodel, &iter);
            total_mode_label = get_total_mode_label (currency_accum);
            bidi_total = gnc_wrap_text_with_bidi_ltr_isolate(total_mode_label);
            bidi_asset_amount = gnc_wrap_text_with_bidi_ltr_isolate(asset_amount_string);
            bidi_profit_amount = gnc_wrap_text_with_bidi_ltr_isolate(profit_amount_string);
            /* Not the displayed label: two different commodities can
             * share a symbol (e.g. AUD and USD both show "$"). */
            gchar *row_key = g_strdup_printf
                ("%d:%s", currency_accum->total_mode,
                 gnc_commodity_get_unique_name (currency_accum->currency));
            gtk_list_store_set(summary->datamodel, &iter,
                               COLUMN_KEY,           row_key,
                               COLUMN_MNEMONIC_TYPE, bidi_total,
                               COLUMN_ASSETS,        _("Net Assets:"),
                               COLUMN_ASSETS_VALUE,  bidi_asset_amount,
                               COLUMN_ASSETS_NEG,    gnc_numeric_negative_p(currency_accum->assets),
                               COLUMN_PROFITS,       _("Profits:"),
                               COLUMN_PROFITS_VALUE, bidi_profit_amount,
                               COLUMN_PROFITS_NEG,   gnc_numeric_negative_p(currency_accum->profits),
                               -1);
            g_free(row_key);
            g_free(total_mode_label);
            g_free(bidi_total);
            g_free(bidi_asset_amount);
            g_free(bidi_profit_amount);
        }

        /* The selected row is kept across refreshes by key (falling
         * back to the first row in summary_update_bar if it is gone). */
        summary_update_bar (summary);
    }

    g_list_free_full (currency_list, g_free);
}

static gchar*
get_negative_color_str (void)
{
    GdkRGBA color;
    gchar *color_str;
    GtkWidget *label = gtk_label_new ("Color");
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(label));
    g_object_ref_sink (label);
    gtk_style_context_add_class (context, "gnc-class-negative-numbers");
    gtk_style_context_get_color (context, GTK_STATE_FLAG_NORMAL, &color);
    color_str = g_strdup_printf ("#%02X%02X%02X",
                                 (int)(0.5 + CLAMP (color.red, 0., 1.) * 255.),
                                 (int)(0.5 + CLAMP (color.green, 0., 1.) * 255.),
                                 (int)(0.5 + CLAMP (color.blue, 0., 1.) * 255.));
    g_object_unref (label);
    return color_str;
}

static void
summarybar_update_color (gpointer gsettings, gchar *key, gpointer user_data)
{
    GNCMainSummary *summary = user_data;

    g_free(summary->negative_color);
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

    if (summary->menu)
    {
        gtk_widget_destroy (summary->menu);
        g_object_unref (summary->menu);
    }
    g_object_unref (summary->datamodel);

    g_free (summary->active_key);
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


static gchar *
make_markup (GtkTreeModel *model, GtkTreeIter *iter,
             const GNCMainSummary *summary,
             gint label_col, gint value_col, gint neg_col)
{
    gchar *label, *value;
    gboolean neg;
    gtk_tree_model_get (model, iter, label_col, &label, value_col, &value, neg_col, &neg,
                        -1);
    gchar* markup = summary->show_negative_color && neg
        ? g_markup_printf_escaped ("%s <span foreground='%s'>%s</span>",
                                   label, summary->negative_color, value)
        : g_markup_printf_escaped ("%s %s", label, value);
    g_free (label);
    g_free (value);
    return markup;
}

/* Fill three labels (mnemonic/type, net assets, profits) from one row of
 * the datamodel. Used for both the bar and the popup menu items. */
static void
fill_row_labels (const GNCMainSummary *summary, GtkTreeModel *model,
                 GtkTreeIter *iter, GtkLabel *labels[3])
{
    gchar *type;
    gtk_tree_model_get (model, iter, COLUMN_MNEMONIC_TYPE, &type, -1);
    gtk_label_set_text (labels[0], type);
    g_free (type);

    gchar* markup = make_markup (model, iter, summary,
                                 COLUMN_ASSETS, COLUMN_ASSETS_VALUE, COLUMN_ASSETS_NEG);
    gtk_label_set_markup (labels[1], markup);
    g_free (markup);

    markup = make_markup (model, iter, summary,
                          COLUMN_PROFITS, COLUMN_PROFITS_VALUE, COLUMN_PROFITS_NEG);
    gtk_label_set_markup (labels[2], markup);
    g_free (markup);
}

/* Find the row whose COLUMN_KEY equals key. Returns TRUE and sets iter
 * if found. */
static gboolean
summary_find_row_by_key (GtkTreeModel *model, const gchar *key,
                         GtkTreeIter *iter)
{
    if (!key || !gtk_tree_model_get_iter_first (model, iter))
        return FALSE;

    do
    {
        gchar *row_key = NULL;
        gtk_tree_model_get (model, iter, COLUMN_KEY, &row_key, -1);
        gboolean match = row_key && g_strcmp0 (row_key, key) == 0;
        g_free (row_key);
        if (match)
            return TRUE;
    }
    while (gtk_tree_model_iter_next (model, iter));

    return FALSE;
}

/* Show the active row of the datamodel in the bar itself. The row is
 * identified by key, so it survives rows being added, removed or
 * reordered. If the remembered row has vanished (or none was chosen
 * yet), fall back to the first row and remember that instead. */
static void
summary_update_bar (GNCMainSummary *summary)
{
    GtkTreeModel *model = GTK_TREE_MODEL (summary->datamodel);
    GtkTreeIter iter;

    if (!summary_find_row_by_key (model, summary->active_key, &iter))
    {
        g_free (summary->active_key);
        summary->active_key = NULL;

        if (!gtk_tree_model_get_iter_first (model, &iter))
        {
            for (int i = 0; i < 3; i++)
                gtk_label_set_text (summary->bar_labels[i], "");
            return;
        }
        gtk_tree_model_get (model, &iter, COLUMN_KEY, &summary->active_key, -1);
    }

    fill_row_labels (summary, model, &iter, summary->bar_labels);
}


/* A row of the popup menu was chosen: make it the row shown in the bar. */
static void
summary_menu_item_activate_cb (GtkMenuItem *item, gpointer user_data)
{
    GNCMainSummary *summary = user_data;
    const gchar *key = g_object_get_data (G_OBJECT (item), "summary-key");

    if (!key)
        return;

    g_free (summary->active_key);
    summary->active_key = g_strdup (key);
    summary_update_bar (summary);
}

static void
summary_menu_remove_child_cb (GtkWidget *child, gpointer user_data)
{
    gtk_widget_destroy (child);
}

/* Rebuild the menu from the datamodel. Returns the menu item for the
 * active row (or NULL). */
static GtkWidget *
summary_menu_populate (GNCMainSummary *summary)
{
    GtkTreeModel *model = GTK_TREE_MODEL (summary->datamodel);
    GtkWidget *active_item = NULL;

    gtk_container_foreach (GTK_CONTAINER (summary->menu),
                           summary_menu_remove_child_cb, NULL);

    GtkTreeIter iter;
    if (!gtk_tree_model_get_iter_first (model, &iter))
        return NULL;

    do
    {
        GtkWidget *item = gtk_menu_item_new ();
        GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
        GtkLabel *labels[3];
        gchar *key = NULL;

        /* Homogeneous, like the bar, so the three columns line up. */
        gtk_box_set_homogeneous (GTK_BOX (box), TRUE);

        for (int i = 0; i < 3; i++)
        {
            labels[i] = GTK_LABEL (gtk_label_new (NULL));
            gtk_label_set_xalign (labels[i], 0.0);
            gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (labels[i]),
                                TRUE, TRUE, 0);
        }
        fill_row_labels (summary, model, &iter, labels);

        gtk_container_add (GTK_CONTAINER (item), box);
        gtk_tree_model_get (model, &iter, COLUMN_KEY, &key, -1);
        if (g_strcmp0 (key, summary->active_key) == 0)
            active_item = item;
        /* The item owns the key string and frees it when destroyed. */
        g_object_set_data_full (G_OBJECT (item), "summary-key", key, g_free);

        g_signal_connect (item, "activate",
                          G_CALLBACK (summary_menu_item_activate_cb), summary);
        gtk_menu_shell_append (GTK_MENU_SHELL (summary->menu), item);
    }
    while (gtk_tree_model_iter_next (model, &iter));

    return active_item;
}

static void
summary_popup_menu (GNCMainSummary *summary, const GdkEvent *trigger)
{
    GtkWidget *active_item = summary_menu_populate (summary);

    /* Make the popup as wide as the bar. */
    gtk_widget_set_size_request (summary->menu,
                                gtk_widget_get_allocated_width (summary->button),
                                -1);
    gtk_widget_show_all (summary->menu);

    /* Menu's bottom-left corner on the bar's top-left corner: the list
     * grows upward from the bar. GDK flips it downward only if there is
     * no room above. */
    gtk_menu_popup_at_widget (GTK_MENU (summary->menu), summary->button,
                              GDK_GRAVITY_NORTH_WEST,
                              GDK_GRAVITY_SOUTH_WEST,
                              trigger);

    if (active_item)
        gtk_menu_shell_select_item (GTK_MENU_SHELL (summary->menu), active_item);
}

/* Mouse: open on press of the primary button. Returning TRUE stops the
 * button's own handling, so no "clicked" follows. */
static gboolean
summary_button_press_cb (GtkWidget *widget, GdkEventButton *event,
                         gpointer user_data)
{
    if (event->type != GDK_BUTTON_PRESS || event->button != GDK_BUTTON_PRIMARY)
        return FALSE;

    summary_popup_menu (user_data, (const GdkEvent *) event);
    return TRUE;
}

/* Keyboard: Space/Enter on the focused bar emits "clicked". */
static void
summary_button_clicked_cb (GtkButton *button, gpointer user_data)
{
    GNCMainSummary *summary = user_data;

    if (!gtk_widget_get_visible (summary->menu))
        summary_popup_menu (summary, NULL);
}

typedef gboolean (*SummaryStepFn) (GtkTreeModel *, GtkTreeIter *);

static gboolean
summary_iter_last (GtkTreeModel *model, GtkTreeIter *iter)
{
    int n = gtk_tree_model_iter_n_children (model, NULL);
    return n > 0 && gtk_tree_model_iter_nth_child (model, iter, NULL, n - 1);
}

static gboolean
summary_button_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GNCMainSummary *summary = user_data;
    GtkTreeModel *model = GTK_TREE_MODEL (summary->datamodel);
    SummaryStepFn fn;

    if (event->state & gtk_accelerator_get_default_mod_mask ())
        return FALSE;

    switch (event->keyval)
    {
    case GDK_KEY_Up:
    case GDK_KEY_KP_Up:
        fn = gtk_tree_model_iter_previous;
        break;
    case GDK_KEY_Down:
    case GDK_KEY_KP_Down:
        fn = gtk_tree_model_iter_next;
        break;
    case GDK_KEY_Home:
    case GDK_KEY_KP_Home:
        fn = gtk_tree_model_get_iter_first;
        break;
    case GDK_KEY_End:
    case GDK_KEY_KP_End:
        fn = summary_iter_last;
        break;
    default:
        return FALSE;
    }

    GtkTreeIter iter;
    if (!summary_find_row_by_key (model, summary->active_key, &iter) || !fn (model, &iter))
        return TRUE;

    g_free (summary->active_key);
    gtk_tree_model_get (model, &iter, COLUMN_KEY, &summary->active_key, -1);
    summary_update_bar (summary);
    return TRUE;
}

GtkWidget *
gnc_main_window_summary_new (void)
{
    GNCMainSummary  * retval = g_new0(GNCMainSummary, 1);
    int i;

    retval->datamodel = gtk_list_store_new (N_COLUMNS,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_STRING,
                                            G_TYPE_BOOLEAN,
                                            G_TYPE_BOOLEAN);

    retval->hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (retval->hbox), FALSE);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(retval->hbox), "gnc-id-account-summary-bar");

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

    /* The bar: a button holding the three centred labels and an arrow. */
    retval->button = gtk_button_new ();
    GtkWidget *inner = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (inner), TRUE);

    for (i = 0; i < 3; i++)
    {
        retval->bar_labels[i] = GTK_LABEL (gtk_label_new (NULL));
        gtk_label_set_xalign (retval->bar_labels[i], 0.5);
        gtk_box_pack_start (GTK_BOX (inner), GTK_WIDGET (retval->bar_labels[i]),
                            TRUE, TRUE, 0);
    }

    GtkWidget *outer = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    GtkWidget *arrow = gtk_image_new_from_icon_name ("pan-up-symbolic",
                                                     GTK_ICON_SIZE_BUTTON);
    gtk_box_pack_start (GTK_BOX (outer), inner, TRUE, TRUE, 0);
    gtk_box_pack_end (GTK_BOX (outer), arrow, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (retval->button), outer);

    /* The popup list; contents are rebuilt each time it is opened. */
    retval->menu = g_object_ref_sink (gtk_menu_new ());
    g_signal_connect (retval->button, "button-press-event",
                      G_CALLBACK (summary_button_press_cb), retval);
    g_signal_connect (retval->button, "clicked",
                      G_CALLBACK (summary_button_clicked_cb), retval);
    g_signal_connect (retval->button, "key-press-event",
                      G_CALLBACK (summary_button_key_press_cb), retval);
    gtk_container_set_border_width (GTK_CONTAINER (retval->hbox), 2);
    gtk_box_pack_start (GTK_BOX(retval->hbox), retval->button, TRUE, TRUE, 5);
    gtk_widget_show_all (retval->button);
    gtk_widget_show (retval->hbox);

    g_signal_connect_swapped (G_OBJECT (retval->hbox), "destroy",
                              G_CALLBACK (gnc_main_window_summary_destroy_cb),
                              retval);

    gnc_main_window_summary_refresh(retval);

    retval->cnxn_id =  gnc_prefs_register_cb (GNC_PREFS_GROUP, NULL,
                       prefs_changed_cb, retval);

    return retval->hbox;
}

/********************************************************************
 * window-main-summarybar.c -- summary of financial info            *
 * Copyright (C) 1998,1999 Jeremy Collins                           *
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
    GtkWidget    *totals_dropdown;
    GListStore   *store;

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
    gnc_commodity *currency;
    gnc_numeric    assets;
    gnc_numeric    profits;
    gint           total_mode;
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
    gboolean       grand_total;
    gboolean       non_currency;
    time64         start_date;
    time64         end_date;
} GNCSummarybarOptions;

static void summarybar_refresh (GNCMainSummary *summary);

/***********************************************************************/

#define SUMMARYBAR_TYPE_ITEM (summarybar_item_get_type ())
G_DECLARE_FINAL_TYPE (SummarybarItem, summarybar_item, SUMMARYBAR, ITEM, GObject)

struct _SummarybarItem {
    GObject parent_instance;
    gchar *mnemonic_type;
    gchar *assets;
    gchar *assets_value;
    gchar *profits;
    gchar *profits_value;
    gboolean assets_neg;
    gboolean profits_neg;
};

G_DEFINE_TYPE (SummarybarItem, summarybar_item, G_TYPE_OBJECT);

static void
summarybar_item_init (SummarybarItem *item)
{
}

static void
summarybar_item_finalize (GObject *object)
{
    SummarybarItem *item = SUMMARYBAR_ITEM(object);

    g_free (item->mnemonic_type);
    g_free (item->assets);
    g_free (item->assets_value);
    g_free (item->profits);
    g_free (item->profits_value);

    G_OBJECT_CLASS(summarybar_item_parent_class)->finalize (object);
}

static void
summarybar_item_class_init (SummarybarItemClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    object_class->finalize = summarybar_item_finalize;
}

static void
summarybar_setup_item_single_line (GtkSignalListItemFactory *factory,
                                   GtkListItem *list_item)
{
    GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_set_homogeneous (GTK_BOX(box), TRUE);

    GtkWidget *type = gtk_label_new ("");
    GtkWidget *assets = gtk_label_new ("");
    GtkWidget *profits = gtk_label_new ("");

    g_object_set (type, "xalign", 0.5, NULL);
    g_object_set (assets, "xalign", 0.5, NULL);
    g_object_set (profits, "xalign", 0.5, NULL);

    gtk_box_append (GTK_BOX(box), type);
    gtk_box_append (GTK_BOX(box), assets);
    gtk_box_append (GTK_BOX(box), profits);

    gtk_widget_set_hexpand (GTK_WIDGET(box), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(type), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(assets), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(profits), TRUE);

    g_object_set_data (G_OBJECT(list_item), "type", type);
    g_object_set_data (G_OBJECT(list_item), "assets", assets);
    g_object_set_data (G_OBJECT(list_item), "profits", profits);

    gtk_list_item_set_child (list_item, box);
}

/***********************************************************************/

/**
 * Get the existing currency accumulator matching the given currency and
 * total-mode, or create a new one.
 **/
static GNCCurrencyAcc *
gnc_ui_get_currency_accumulator (GList **list, gnc_commodity *currency, gint total_mode)
{
    GList *current;
    GNCCurrencyAcc *found;

    for (current = g_list_first (*list); current; current = g_list_next (current))
    {
        found = current->data;
        if ((gnc_commodity_equiv (currency, found->currency))
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
    gnc_commodity *account_currency;
    GNCCurrencyAcc *currency_accum = NULL;
    GNCCurrencyAcc *grand_total_accum = NULL;
    GNCCurrencyAcc *non_curr_accum = NULL;
    GList *children, *node;
    gboolean non_currency = FALSE;

    if (parent == NULL) return;

    children = gnc_account_get_children (parent);
    for (node = children; node; node = g_list_next (node))
    {
        Account *account = node->data;
        QofBook *book = gnc_account_get_book (account);
        GNCPriceDB *pricedb = gnc_pricedb_get_db (book);
        gnc_commodity *to_curr = options.default_currency;

        account_type = xaccAccountGetType (account);
        account_currency = xaccAccountGetCommodity (account);

        if (options.grand_total)
            grand_total_accum = gnc_ui_get_currency_accumulator (currency_list,
                                to_curr,
                                TOTAL_GRAND_TOTAL);

        if (!gnc_commodity_is_currency (account_currency))
        {
            non_currency = TRUE;
            non_curr_accum = gnc_ui_get_currency_accumulator (currency_list,
                             to_curr,
                             TOTAL_NON_CURR_TOTAL);
        }

        if (!non_currency || options.non_currency)
        {
            currency_accum = gnc_ui_get_currency_accumulator (currency_list,
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
            end_amount = xaccAccountGetBalanceAsOfDate (account, options.end_date);
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

            gnc_ui_accounts_recurse (account, currency_list, options);
            break;
        case ACCT_TYPE_INCOME:
        case ACCT_TYPE_EXPENSE:
            start_amount = xaccAccountGetBalanceAsOfDate (account, options.start_date);
            start_amount_default_currency =
                gnc_pricedb_convert_balance_nearest_price_t64 (pricedb,
                                                               start_amount,
                                                               account_currency,
                                                               to_curr,
                                                               options.start_date);
            end_amount = xaccAccountGetBalanceAsOfDate (account, options.end_date);
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

            gnc_ui_accounts_recurse (account, currency_list, options);
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
        label_str = g_strdup_printf (_("%s, Total:"), mnemonic);
        break;
    case TOTAL_NON_CURR_TOTAL:
        label_str = g_strdup_printf (_("%s, Non Currency Commodities Total:"), mnemonic);
        break;
    case TOTAL_GRAND_TOTAL:
        label_str = g_strdup_printf (_("%s, Grand Total:"), mnemonic);
        break;
    case TOTAL_SINGLE:
    default:
        label_str = g_strdup_printf (_("%s:"), mnemonic);
        break;
    }
    return label_str;
}

static gchar*
get_negative_color_str (void)
{
    GdkRGBA color;
    GdkRGBA *rgba;
    gchar *color_str;
    GtkWidget *label = gtk_label_new ("Color");
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(label));
    gtk_style_context_add_class (context, "gnc-class-negative-numbers");
    gtk_style_context_get_color (context, &color);
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

    summarybar_refresh (summary);
}

static void
gnc_main_window_summary_destroy_cb (GNCMainSummary *summary, gpointer user_data)
{
    gnc_prefs_remove_cb_by_id (GNC_PREFS_GROUP, summary->cnxn_id);
    gnc_unregister_gui_component (summary->component_id);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                                 summarybar_update_color, summary);

    g_free (summary->negative_color);
    g_free (summary);
}

static void
summarybar_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCMainSummary *summary = user_data;
    summarybar_refresh (summary);
}

static void
prefs_changed_cb (gpointer prefs, gchar *pref, gpointer user_data)
{
    GNCMainSummary *summary = user_data;
    summarybar_refresh (summary);
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

/***********************************************************************/

static void
summarybar_selected_item_changed (GtkDropDown *dropdown,
                                  GParamSpec *pspec,
                                  GtkListItem *item)
{
    GtkWidget *type = g_object_get_data (G_OBJECT(item), "type");
    GtkWidget *assets = g_object_get_data (G_OBJECT(item), "assets");
    GtkWidget *profits = g_object_get_data (G_OBJECT(item), "profits");

    g_object_set (type, "xalign", 0.0, NULL);
    g_object_set (assets, "xalign", 0.0, NULL);
    g_object_set (profits, "xalign", 0.0, NULL);
}

static void
summarybar_bind_item (GtkSignalListItemFactory *factory,
                      GtkListItem *list_item,
                      gpointer user_data)
{
    GNCMainSummary *summary = user_data;
    GtkDropDown *dropdown = GTK_DROP_DOWN(summary->totals_dropdown);
    SummarybarItem *item = gtk_list_item_get_item (list_item);
    GtkWidget *type = g_object_get_data (G_OBJECT(list_item), "type");
    GtkWidget *assets = g_object_get_data (G_OBJECT(list_item), "assets");
    GtkWidget *profits = g_object_get_data (G_OBJECT(list_item), "profits");
    GtkWidget *popup;

    gtk_label_set_label (GTK_LABEL(type), item->mnemonic_type);

    gchar *a_string, *p_string, *checked_string;

    checked_string = check_string_for_markup (item->assets_value);
    if ((summary->show_negative_color == TRUE) && (item->assets_neg == TRUE))
        a_string = g_strconcat (item->assets, " <span foreground='",
                                summary->negative_color, "'>",
                                checked_string, "</span>", NULL);
    else
        a_string = g_strconcat (item->assets, " ", checked_string, NULL);

    gtk_label_set_markup (GTK_LABEL(assets), a_string);

    g_free (a_string);
    g_free (checked_string);

    checked_string = check_string_for_markup (item->profits_value);

    if ((summary->show_negative_color == TRUE) && (item->profits_neg == TRUE))
        p_string = g_strconcat (item->profits, " <span foreground='",
                                summary->negative_color, "'>",
                                checked_string, "</span>", NULL);
    else
        p_string = g_strconcat (item->profits, " ", checked_string, NULL);

    gtk_label_set_markup (GTK_LABEL(profits), p_string);

    g_free (p_string);
    g_free (checked_string);

    popup = gtk_widget_get_ancestor (type, GTK_TYPE_POPOVER);
    if (popup && gtk_widget_is_ancestor (popup, GTK_WIDGET(dropdown)))
    {
        g_signal_connect (G_OBJECT(dropdown), "notify::selected-item",
                          G_CALLBACK(summarybar_selected_item_changed), list_item);
        summarybar_selected_item_changed (dropdown, NULL, list_item);
    }
}

static void
summarybar_unbind_item (GtkSignalListItemFactory *factory,
                        GtkListItem *list_item,
                        gpointer user_data)
{
  GtkDropDown *dropdown = user_data;
  g_signal_handlers_disconnect_by_func (dropdown,
                                        summarybar_selected_item_changed,
                                        list_item);
}

/* The summarybar_refresh() subroutine redraws summary
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
summarybar_refresh (GNCMainSummary *summary)
{
    Account *root = gnc_get_current_root_account ();
    GNCCurrencyAcc *currency_accum;
    GList *currency_list;
    GList *current;
    GNCSummarybarOptions options;

    options.default_currency = gnc_default_currency ();
    if (options.default_currency == NULL)
    {
        options.default_currency = xaccAccountGetCommodity (root);
    }

    options.grand_total = gnc_prefs_get_bool (GNC_PREFS_GROUP,
                                              GNC_PREF_GRAND_TOTAL);
    options.non_currency = gnc_prefs_get_bool (GNC_PREFS_GROUP,
                                               GNC_PREF_NON_CURRENCY);
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

    gnc_ui_accounts_recurse (root, &currency_list, options);

    char asset_amount_string[256], profit_amount_string[256];

    g_list_store_remove_all (summary->store);

    for (current = g_list_first (currency_list); current; current = g_list_next(current))
    {
        gchar *total_mode_label;
        gchar *bidi_total, *bidi_asset_amount, *bidi_profit_amount;

        currency_accum = current->data;

        xaccSPrintAmount (asset_amount_string,
                          currency_accum->assets,
                          gnc_commodity_print_info (currency_accum->currency, TRUE));

        xaccSPrintAmount (profit_amount_string,
                          currency_accum->profits,
                          gnc_commodity_print_info (currency_accum->currency, TRUE));

        total_mode_label = get_total_mode_label (currency_accum);
        bidi_total = gnc_wrap_text_with_bidi_ltr_isolate (total_mode_label);
        bidi_asset_amount = gnc_wrap_text_with_bidi_ltr_isolate (asset_amount_string);
        bidi_profit_amount = gnc_wrap_text_with_bidi_ltr_isolate (profit_amount_string);

        SummarybarItem *item = g_object_new (SUMMARYBAR_TYPE_ITEM, NULL);

        item->mnemonic_type = g_strdup (bidi_total);
        item->assets = g_strdup (_("Net Assets:"));
        item->assets_value = g_strdup (bidi_asset_amount);
        item->assets_neg = gnc_numeric_negative_p (currency_accum->assets);
        item->profits = g_strdup (_("Profits:"));
        item->profits_value = g_strdup (bidi_profit_amount);
        item->profits_neg = gnc_numeric_negative_p (currency_accum->profits);

        g_free (total_mode_label);
        g_free (bidi_total);
        g_free (bidi_asset_amount);
        g_free (bidi_profit_amount);

        g_list_store_append (summary->store, item);
        g_object_unref (item);
    }
    g_list_free_full (currency_list, g_free);
}

static GtkWidget *
summarybar_dropdown_new (GNCMainSummary *summary)
{
    GtkWidget *dropdown;

    summary->store = g_list_store_new (SUMMARYBAR_TYPE_ITEM);

    summarybar_refresh (summary);

    dropdown = g_object_new (GTK_TYPE_DROP_DOWN, "model",
                             G_LIST_MODEL(summary->store), NULL);
    g_object_unref (summary->store);

    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();

    g_signal_connect (G_OBJECT(factory), "setup",
                      G_CALLBACK(summarybar_setup_item_single_line), dropdown);
    g_signal_connect (G_OBJECT(factory), "bind",
                      G_CALLBACK(summarybar_bind_item), summary);
    g_signal_connect (G_OBJECT(factory), "unbind",
                      G_CALLBACK(summarybar_unbind_item), dropdown);

    g_object_set (dropdown,
                  "factory", factory,
                  "list-factory", NULL,
                  NULL);

    g_object_unref (factory);

    return dropdown;
}

GtkWidget *
gnc_main_window_summary_new (void)
{
    GNCMainSummary *summary = g_new0 (GNCMainSummary, 1);

    summary->hbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX(summary->hbox), FALSE);

    summary->negative_color = get_negative_color_str();
    summary->show_negative_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                                       GNC_PREF_NEGATIVE_IN_RED);

    // Set the name for this wodget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(summary->hbox), "gnc-id-account-summary-bar");

    summary->totals_dropdown = summarybar_dropdown_new (summary);

    gtk_box_append (GTK_BOX(summary->hbox), GTK_WIDGET(summary->totals_dropdown));

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                           summarybar_update_color, summary);

    summary->component_id = gnc_register_gui_component (WINDOW_SUMMARYBAR_CM_CLASS,
                                                        summarybar_refresh_handler,
                                                        NULL, summary);

    gnc_gui_component_watch_entity_type (summary->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_DESTROY |
                                         GNC_EVENT_ITEM_CHANGED);

    g_signal_connect_swapped (G_OBJECT(summary->hbox), "destroy",
                              G_CALLBACK(gnc_main_window_summary_destroy_cb),
                              summary);

    summary->cnxn_id = gnc_prefs_register_cb (GNC_PREFS_GROUP, NULL,
                                             prefs_changed_cb, summary);

    return summary->hbox;
}

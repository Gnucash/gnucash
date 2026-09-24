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
#include "gnc-gtk-utils.h"

typedef struct
{
    GtkWidget      *hbox;
    GtkDropDown    *totals_combo;
    GListStore     *datamodel;
    int             component_id;
    gulong          cnxn_id;
    gulong          negative_color_cnxn_id;
    gboolean        show_negative_color;
} GNCMainSummary;

typedef struct _GncSummaryRow
{
    GObject parent_instance;
    gchar *mnemonic_type;
    gchar *assets_label;
    gchar *assets_value;
    gchar *profits_label;
    gchar *profits_value;
    gboolean assets_negative;
    gboolean profits_negative;
    gboolean show_negative_color;
} GncSummaryRow;

typedef struct _GncSummaryRowClass
{
    GObjectClass parent_class;
} GncSummaryRowClass;

GType gnc_summary_row_get_type (void);

#define GNC_TYPE_SUMMARY_ROW (gnc_summary_row_get_type ())
#define GNC_SUMMARY_ROW(object) \
    (G_TYPE_CHECK_INSTANCE_CAST ((object), GNC_TYPE_SUMMARY_ROW, GncSummaryRow))
#define GNC_IS_SUMMARY_ROW(object) \
    (G_TYPE_CHECK_INSTANCE_TYPE ((object), GNC_TYPE_SUMMARY_ROW))

G_DEFINE_TYPE (GncSummaryRow, gnc_summary_row, G_TYPE_OBJECT)

static void
gnc_summary_row_finalize (GObject *object)
{
    GncSummaryRow *row = GNC_SUMMARY_ROW (object);

    g_clear_pointer (&row->mnemonic_type, g_free);
    g_clear_pointer (&row->assets_label, g_free);
    g_clear_pointer (&row->assets_value, g_free);
    g_clear_pointer (&row->profits_label, g_free);
    g_clear_pointer (&row->profits_value, g_free);

    G_OBJECT_CLASS (gnc_summary_row_parent_class)->finalize (object);
}

static void
gnc_summary_row_class_init (GncSummaryRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_summary_row_finalize;
}

static void
gnc_summary_row_init (G_GNUC_UNUSED GncSummaryRow *row)
{
}

static GncSummaryRow *
gnc_summary_row_new (const gchar *mnemonic_type,
                     const gchar *assets_label,
                     const gchar *assets_value,
                     gboolean assets_negative,
                     const gchar *profits_label,
                     const gchar *profits_value,
                     gboolean profits_negative,
                     gboolean show_negative_color)
{
    GncSummaryRow *row = g_object_new (GNC_TYPE_SUMMARY_ROW, NULL);

    row->mnemonic_type = g_strdup (mnemonic_type);
    row->assets_label = g_strdup (assets_label);
    row->assets_value = g_strdup (assets_value);
    row->profits_label = g_strdup (profits_label);
    row->profits_value = g_strdup (profits_value);
    row->assets_negative = assets_negative;
    row->profits_negative = profits_negative;
    row->show_negative_color = show_negative_color;

    return row;
}

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
        char asset_amount_string[256], profit_amount_string[256];

        g_list_store_remove_all (summary->datamodel);
        for (current = g_list_first (currency_list); current; current = g_list_next (current))
        {
            GncSummaryRow *row;
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

            row = gnc_summary_row_new (bidi_total,
                                       _("Net Assets:"), bidi_asset_amount,
                                       gnc_numeric_negative_p (currency_accum->assets),
                                       _("Profits:"), bidi_profit_amount,
                                       gnc_numeric_negative_p (currency_accum->profits),
                                       summary->show_negative_color);
            g_list_store_append (summary->datamodel, row);
            g_object_unref (row);

            g_free (total_mode_label);
            g_free (bidi_total);
            g_free (bidi_asset_amount);
            g_free (bidi_profit_amount);
        }

        gtk_drop_down_set_selected (summary->totals_combo, 0);
    }

    g_list_free_full (currency_list, g_free);
}

static void
summarybar_update_color (G_GNUC_UNUSED gpointer gsettings,
                         G_GNUC_UNUSED gchar *key, gpointer user_data)
{
    GNCMainSummary *summary = user_data;

    summary->show_negative_color =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    gnc_main_window_summary_refresh (summary);
}

static void
gnc_main_window_summary_destroy_cb (GNCMainSummary *summary,
                                    G_GNUC_UNUSED gpointer data)
{
    gnc_prefs_remove_cb_by_id (GNC_PREFS_GROUP, summary->cnxn_id);
    gnc_prefs_remove_cb_by_id (GNC_PREFS_GROUP_GENERAL, summary->negative_color_cnxn_id);
    gnc_unregister_gui_component (summary->component_id);

    g_clear_object (&summary->datamodel);
    g_free (summary);
}

static void
summarybar_refresh_handler(G_GNUC_UNUSED GHashTable *changes, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}

static void
prefs_changed_cb (G_GNUC_UNUSED gpointer prefs,
                  G_GNUC_UNUSED gchar *pref, gpointer user_data)
{
    GNCMainSummary * summary = user_data;
    gnc_main_window_summary_refresh(summary);
}


typedef struct
{
    GtkLabel *mnemonic_type;
    GtkLabel *assets_label;
    GtkLabel *assets_value;
    GtkLabel *profits_label;
    GtkLabel *profits_value;
} SummarybarItemWidgets;

#define SUMMARYBAR_ITEM_WIDGETS_KEY "gnc-summarybar-item-widgets"

static GtkWidget *
summarybar_value_cell_new (GtkLabel **label, GtkLabel **value)
{
    GtkWidget *cell = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);

    *label = GTK_LABEL (gtk_label_new (NULL));
    *value = GTK_LABEL (gtk_label_new (NULL));

    gtk_label_set_xalign (*label, 0.0);
    gtk_label_set_xalign (*value, 0.0);
    gtk_label_set_ellipsize (*label, PANGO_ELLIPSIZE_END);
    gtk_label_set_ellipsize (*value, PANGO_ELLIPSIZE_END);
    gtk_box_append (GTK_BOX (cell), GTK_WIDGET (*label));
    gtk_box_append (GTK_BOX (cell), GTK_WIDGET (*value));

    return cell;
}

static void
summarybar_item_setup (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item,
                       gpointer user_data)
{
    gboolean popup = GPOINTER_TO_INT (user_data);
    SummarybarItemWidgets *widgets = g_new0 (SummarybarItemWidgets, 1);
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 12);
    GtkWidget *assets;
    GtkWidget *profits;

    widgets->mnemonic_type = GTK_LABEL (gtk_label_new (NULL));
    gtk_label_set_xalign (widgets->mnemonic_type, 0.0);
    gtk_label_set_ellipsize (widgets->mnemonic_type, PANGO_ELLIPSIZE_END);
    gtk_widget_set_size_request (GTK_WIDGET (widgets->mnemonic_type), 50, -1);

    assets = summarybar_value_cell_new (&widgets->assets_label, &widgets->assets_value);
    profits = summarybar_value_cell_new (&widgets->profits_label, &widgets->profits_value);

    gtk_box_append (GTK_BOX (content), GTK_WIDGET (widgets->mnemonic_type));
    gtk_box_append (GTK_BOX (content), assets);
    gtk_box_append (GTK_BOX (content), profits);
    gtk_widget_set_halign (content, popup ? GTK_ALIGN_START : GTK_ALIGN_CENTER);
    g_object_set_data_full (G_OBJECT (content), SUMMARYBAR_ITEM_WIDGETS_KEY,
                            widgets, g_free);
    gtk_list_item_set_child (item, content);
}

static void
summarybar_value_label_set (GtkLabel *label, const gchar *value, gboolean negative)
{
    gtk_label_set_text (label, value ? value : "");

    if (negative)
        gtk_widget_add_css_class (GTK_WIDGET (label), "gnc-class-negative-numbers");
    else
        gtk_widget_remove_css_class (GTK_WIDGET (label), "gnc-class-negative-numbers");
}

static void
summarybar_item_bind (G_GNUC_UNUSED GtkListItemFactory *factory, GtkListItem *item,
                      G_GNUC_UNUSED gpointer user_data)
{
    GtkWidget *content = gtk_list_item_get_child (item);
    SummarybarItemWidgets *widgets;
    GObject *object;
    GncSummaryRow *row;

    g_return_if_fail (content != NULL);
    widgets = g_object_get_data (G_OBJECT (content), SUMMARYBAR_ITEM_WIDGETS_KEY);
    g_return_if_fail (widgets != NULL);

    object = gtk_list_item_get_item (item);
    if (!GNC_IS_SUMMARY_ROW (object))
    {
        gtk_label_set_text (widgets->mnemonic_type, "");
        gtk_label_set_text (widgets->assets_label, "");
        summarybar_value_label_set (widgets->assets_value, "", FALSE);
        gtk_label_set_text (widgets->profits_label, "");
        summarybar_value_label_set (widgets->profits_value, "", FALSE);
        return;
    }

    row = GNC_SUMMARY_ROW (object);
    gtk_label_set_text (widgets->mnemonic_type, row->mnemonic_type);
    gtk_label_set_text (widgets->assets_label, row->assets_label);
    summarybar_value_label_set (widgets->assets_value, row->assets_value,
                                row->show_negative_color && row->assets_negative);
    gtk_label_set_text (widgets->profits_label, row->profits_label);
    summarybar_value_label_set (widgets->profits_value, row->profits_value,
                                row->show_negative_color && row->profits_negative);
}

GtkWidget *
gnc_main_window_summary_new (void)
{
    GNCMainSummary *retval = g_new0 (GNCMainSummary, 1);
    GtkListItemFactory *selected_factory;
    GtkListItemFactory *list_factory;

    retval->datamodel = g_list_store_new (GNC_TYPE_SUMMARY_ROW);

    retval->hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (retval->hbox), FALSE);

    // Set the name for this widget so it can be easily manipulated with CSS.
    gtk_widget_set_name (retval->hbox, "gnc-id-account-summary-bar");

    retval->totals_combo = GTK_DROP_DOWN (
        gnc_gtk_drop_down_new (G_LIST_MODEL (g_object_ref (retval->datamodel)), NULL));

    selected_factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (selected_factory, "setup", G_CALLBACK (summarybar_item_setup), NULL);
    g_signal_connect (selected_factory, "bind", G_CALLBACK (summarybar_item_bind), NULL);
    gtk_drop_down_set_factory (retval->totals_combo, selected_factory);
    g_object_unref (selected_factory);

    list_factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (list_factory, "setup", G_CALLBACK (summarybar_item_setup),
                      GINT_TO_POINTER (TRUE));
    g_signal_connect (list_factory, "bind", G_CALLBACK (summarybar_item_bind), NULL);
    gtk_drop_down_set_list_factory (retval->totals_combo, list_factory);
    g_object_unref (list_factory);

    retval->show_negative_color =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    retval->negative_color_cnxn_id =
        gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                               summarybar_update_color, retval);

    retval->component_id = gnc_register_gui_component (WINDOW_SUMMARYBAR_CM_CLASS,
                                                        summarybar_refresh_handler,
                                                        NULL, retval);
    gnc_gui_component_watch_entity_type (retval->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_DESTROY | GNC_EVENT_ITEM_CHANGED);

    gnc_widget_set_all_margins (retval->hbox, 2);
    gnc_box_append_full (GTK_BOX (retval->hbox), GTK_WIDGET (retval->totals_combo),
                         TRUE, TRUE, 5);
    gtk_widget_set_visible (GTK_WIDGET (retval->totals_combo), TRUE);
    gtk_widget_set_visible (retval->hbox, TRUE);

    g_signal_connect_swapped (retval->hbox, "destroy",
                              G_CALLBACK (gnc_main_window_summary_destroy_cb), retval);

    gnc_main_window_summary_refresh (retval);

    retval->cnxn_id = gnc_prefs_register_cb (GNC_PREFS_GROUP, NULL,
                                              prefs_changed_cb, retval);

    return retval->hbox;
}

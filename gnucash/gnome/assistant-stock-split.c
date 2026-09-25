/********************************************************************\
 * assistant-stock-split.c -- stock split assistant for GnuCash     *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (c) 2001 Dave Peticolas <dave@krondo.com>              *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2011 Robert Fewell                                 *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Transaction.h"
#include "engine-helpers.h"
#include "dialog-utils.h"
#include "assistant-stock-split.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-account-sel.h"
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"


#define ASSISTANT_STOCK_SPLIT_CM_CLASS "assistant-stock-split"

typedef struct _StockSplitRow StockSplitRow;
typedef struct _StockSplitRowClass StockSplitRowClass;

struct _StockSplitRow
{
    GObject parent_instance;
    Account *account;
    gchar *full_name;
    gchar *mnemonic;
    gchar *shares;
};

struct _StockSplitRowClass
{
    GObjectClass parent_class;
};

GType stock_split_row_get_type (void);

G_DEFINE_FINAL_TYPE (StockSplitRow, stock_split_row, G_TYPE_OBJECT)

static void
stock_split_row_finalize (GObject *object)
{
    StockSplitRow *row = (StockSplitRow *)object;

    g_free (row->full_name);
    g_free (row->mnemonic);
    g_free (row->shares);
    G_OBJECT_CLASS (stock_split_row_parent_class)->finalize (object);
}

static void
stock_split_row_class_init (StockSplitRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = stock_split_row_finalize;
}

static void
stock_split_row_init (StockSplitRow *row)
{
    (void)row;
}

static StockSplitRow *
stock_split_row_new (Account *account)
{
    StockSplitRow *row;
    GNCPrintAmountInfo print_info;

    row = g_object_new (stock_split_row_get_type (), NULL);
    row->account = account;
    row->full_name = gnc_account_get_full_name (account);
    row->mnemonic = g_strdup (gnc_commodity_get_mnemonic
                              (xaccAccountGetCommodity (account)));
    print_info = gnc_account_print_info (account, FALSE);
    row->shares = g_strdup (xaccPrintAmount (xaccAccountGetBalance (account),
                                           print_info));
    return row;
}

/** structures *********************************************************/
typedef struct
{
    GtkWindow *window;
    GtkStack *stack;
    GtkWidget *pages[5];
    GtkWidget *back_button;
    GtkWidget *next_button;
    GtkWidget *apply_button;
    guint current_page;

    /* account page data */
    GtkWidget *account_page;
    GtkColumnView *account_view;
    GListStore *account_rows;
    GtkSingleSelection *account_selection;
    Account *acct;
    GncGUID account_guid;
    gboolean has_account_guid;
    gboolean updating_account_rows;

    /* info page data */
    GtkWidget *date_edit;
    GtkWidget *distribution_edit;
    GtkWidget *description_entry;
    GtkWidget *price_edit;
    GtkWidget *price_currency_edit;

    /* cash in lieu page data */
    GtkWidget *cash_edit;
    GtkWidget *memo_entry;
    GNCAccountSel *income_account;
    GNCAccountSel *asset_account;
} StockSplitInfo;

/** declarations *******************************************************/
static void stock_split_update_navigation (StockSplitInfo *info);
static void stock_split_show_page (StockSplitInfo *info, guint page);
static gboolean stock_split_details_complete (StockSplitInfo *info);
static gboolean stock_split_cash_complete (StockSplitInfo *info);
static void stock_split_finish (StockSplitInfo *info);
static void stock_split_cancel (StockSplitInfo *info);

/******* implementations ***********************************************/
static void
gnc_stock_split_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    StockSplitInfo *info = user_data;

    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);

    g_signal_handlers_disconnect_by_data (info->account_selection, info);
    g_clear_object (&info->account_selection);
    g_clear_object (&info->account_rows);
    g_free (info);
}


static void
stock_split_set_selected_account (StockSplitInfo *info, Account *account)
{
    info->acct = account;
    info->has_account_guid = account != NULL;
    if (account)
        info->account_guid = *xaccAccountGetGUID (account);
}

static Account *
stock_split_get_selected_account (const StockSplitInfo *info)
{
    if (!info->has_account_guid)
        return NULL;

    return xaccAccountLookup (&info->account_guid, gnc_get_current_book ());
}

static gboolean
stock_split_account_is_eligible (Account *account)
{
    return xaccAccountIsPriced (account) &&
           !gnc_numeric_zero_p (xaccAccountGetBalance (account)) &&
           !xaccAccountGetPlaceholder (account);
}

static guint
fill_account_list (StockSplitInfo *info, Account *selected_account)
{
    GList *accounts;
    GList *node;
    guint rows = 0;
    guint selected_position = GTK_INVALID_LIST_POSITION;

    info->updating_account_rows = TRUE;
    gtk_single_selection_set_selected (info->account_selection,
                                       GTK_INVALID_LIST_POSITION);
    g_list_store_remove_all (info->account_rows);

    accounts = gnc_account_get_descendants_sorted (gnc_get_current_root_account ());
    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data;
        StockSplitRow *row;

        if (!stock_split_account_is_eligible (account))
            continue;

        row = stock_split_row_new (account);
        g_list_store_append (info->account_rows, row);
        g_object_unref (row);

        if (account == selected_account)
            selected_position = rows;
        rows++;
    }
    g_list_free (accounts);
    info->updating_account_rows = FALSE;

    if (rows == 0)
    {
        stock_split_set_selected_account (info, NULL);
        stock_split_update_navigation (info);
        return 0;
    }

    if (selected_position == GTK_INVALID_LIST_POSITION)
        selected_position = 0;

    gtk_single_selection_set_selected (info->account_selection, selected_position);
    gtk_column_view_scroll_to (info->account_view, selected_position, NULL,
                               GTK_LIST_SCROLL_FOCUS, NULL);
    return rows;
}

static void
stock_split_selection_changed_cb (GtkSelectionModel *selection,
                                  guint position, guint n_items,
                                  StockSplitInfo *info)
{
    GObject *object;
    StockSplitRow *row;

    if (info->updating_account_rows)
        return;

    object = gtk_single_selection_get_selected_item (info->account_selection);
    row = object ? (StockSplitRow *)object : NULL;
    stock_split_set_selected_account (info, row ? row->account : NULL);
    stock_split_update_navigation (info);
    (void)selection;
    (void)position;
    (void)n_items;
}

typedef enum
{
    STOCK_SPLIT_ACCOUNT_COLUMN_NAME,
    STOCK_SPLIT_ACCOUNT_COLUMN_SYMBOL,
    STOCK_SPLIT_ACCOUNT_COLUMN_SHARES
} StockSplitAccountColumn;

static void
stock_split_account_cell_setup (GtkListItemFactory *factory,
                                GtkListItem *list_item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_label_set_xalign (GTK_LABEL (label),
                          GPOINTER_TO_UINT (user_data) ==
                          STOCK_SPLIT_ACCOUNT_COLUMN_SHARES ? 1.0 : 0.0);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
}

static void
stock_split_account_cell_bind (GtkListItemFactory *factory,
                               GtkListItem *list_item, gpointer user_data)
{
    StockSplitRow *row = (StockSplitRow *)gtk_list_item_get_item (list_item);
    StockSplitAccountColumn column = GPOINTER_TO_UINT (user_data);
    const gchar *text = "";

    if (row)
    {
        switch (column)
        {
        case STOCK_SPLIT_ACCOUNT_COLUMN_NAME:
            text = row->full_name;
            break;
        case STOCK_SPLIT_ACCOUNT_COLUMN_SYMBOL:
            text = row->mnemonic;
            break;
        case STOCK_SPLIT_ACCOUNT_COLUMN_SHARES:
            text = row->shares;
            break;
        }
    }
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)), text);
    (void)factory;
}

static void
stock_split_add_account_column (StockSplitInfo *info, const gchar *title,
                                StockSplitAccountColumn column, gboolean expand)
{
    GtkListItemFactory *factory;
    GtkColumnViewColumn *view_column;

    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (stock_split_account_cell_setup),
                      GUINT_TO_POINTER (column));
    g_signal_connect (factory, "bind", G_CALLBACK (stock_split_account_cell_bind),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (info->account_view, view_column);
    g_object_unref (view_column);
}

static void
refresh_details_page (StockSplitInfo *info)
{
    GNCPrintAmountInfo print_info;
    gnc_commodity *commodity, *currency;
    Account *account;
    QofBook *book;
    GNCPriceDB *db;
    GList *prices;

    account = stock_split_get_selected_account (info);
    info->acct = account;

    g_return_if_fail (account != NULL);

    print_info = gnc_account_print_info (account, FALSE);

    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (info->distribution_edit),
                                    print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (info->distribution_edit),
                                  xaccAccountGetCommoditySCU (account));

    commodity = xaccAccountGetCommodity (account);
    book = gnc_account_get_book (account);
    db = gnc_pricedb_get_db(book);

    prices = gnc_pricedb_lookup_latest_any_currency(db, commodity);
    if (prices)
    {
        /* Use the first existing price */
        if (gnc_commodity_equiv (commodity, gnc_price_get_currency(prices->data)))
            currency = gnc_price_get_commodity(prices->data);
        else
            currency = gnc_price_get_currency(prices->data);
    }
    else
    {
        /* Take a wild guess. */
        currency = gnc_default_currency ();
    }
    gnc_price_list_destroy(prices);

    gnc_currency_edit_set_currency
    (GNC_CURRENCY_EDIT (info->price_currency_edit),
     currency);
}



static gboolean
stock_split_details_complete (StockSplitInfo *info)
{
    GNCPrintAmountInfo print_info;
    gnc_commodity *currency;
    gnc_numeric amount;
    gint result;

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->distribution_edit),
                                            &amount, TRUE, NULL);
    if ( result != 0)
        return FALSE; /* Parsing error or field is empty */

    if (gnc_numeric_zero_p (amount))
        return FALSE; /* field value is 0 */

    currency = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT(info->price_currency_edit));
    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (info->price_edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (info->price_edit), 0);

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(info->price_edit),
                                            &amount, TRUE, NULL);
    if (result == -1)
        return TRUE; /* Optional field is empty */
    else if ( result > 0)
        return FALSE; /* Parsing error */
    else if (gnc_numeric_negative_p (amount))
        return FALSE; /* Negative price is not allowed */
    else
        return TRUE; /* Valid positive price */
}


static gboolean
stock_split_cash_complete (StockSplitInfo *info)
{
    gnc_numeric amount;
    gint result;
    Account *account;

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->cash_edit), &amount, TRUE, NULL);
    if (result == -1)
        return TRUE; /* Optional field is empty */
    else if ( result > 0)
        return FALSE; /* Parsing error */
    else if (gnc_numeric_negative_p (amount))
        return FALSE; /* Negative cash amount is not allowed */

    /* We have a positive cash amount */
    account = gnc_account_sel_get_account (info->income_account);
    if (!account)
        return FALSE;

    account = gnc_account_sel_get_account (info->asset_account);
    if (!account)
        return FALSE;

    return TRUE;
}


static void
stock_split_finish (StockSplitInfo *info)
{
    GList *account_commits;
    GList *node;

    gnc_numeric amount;
    Transaction *trans;
    Account *account;
    Split *split;
    time64 date;
    if (!stock_split_get_selected_account (info) ||
        !stock_split_details_complete (info) ||
        !stock_split_cash_complete (info))
    {
        stock_split_update_navigation (info);
        return;
    }

    account = stock_split_get_selected_account (info);
    info->acct = account;
    g_return_if_fail (account != NULL);

    amount = gnc_amount_edit_get_amount
             (GNC_AMOUNT_EDIT (info->distribution_edit));
    g_return_if_fail (!gnc_numeric_zero_p (amount));

    gnc_suspend_gui_refresh ();

    trans = xaccMallocTransaction (gnc_get_current_book ());

    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, gnc_default_currency ());

    date = gnc_date_edit_get_date (GNC_DATE_EDIT (info->date_edit));
    xaccTransSetDatePostedSecsNormalized (trans, date);

    {
        const char *description;

        description = gnc_entry_get_text (GTK_ENTRY (info->description_entry));
        xaccTransSetDescription (trans, description);
    }

    split = xaccMallocSplit (gnc_get_current_book ());

    xaccAccountBeginEdit (account);
    account_commits = g_list_prepend (NULL, account);

    xaccTransAppendSplit (trans, split);

    xaccAccountInsertSplit (account, split);

    xaccSplitSetAmount (split, amount);
    xaccSplitMakeStockSplit (split);
    /* Set split-action with gnc_set_num_action which is the same as
     * xaccSplitSetAction with these arguments */

    gnc_set_num_action (NULL, split, NULL, C_("Action Column", "Split"));

    amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->price_edit));
    if (gnc_numeric_positive_p (amount))
    {
        QofBook *book;
        GNCPrice *price;
        GNCPriceDB *pdb;
        GNCCurrencyEdit *ce;

        ce = GNC_CURRENCY_EDIT (info->price_currency_edit);
        price = gnc_price_create (gnc_get_current_book ());

        gnc_price_begin_edit (price);
        gnc_price_set_commodity (price, xaccAccountGetCommodity (account));
        gnc_price_set_currency (price, gnc_currency_edit_get_currency (ce));
        gnc_price_set_time64 (price, date);
        gnc_price_set_source (price, PRICE_SOURCE_STOCK_SPLIT);
        gnc_price_set_typestr (price, PRICE_TYPE_UNK);
        gnc_price_set_value (price, amount);
        gnc_price_commit_edit (price);

        book = gnc_get_current_book ();
        pdb = gnc_pricedb_get_db (book);

        if (!gnc_pricedb_add_price (pdb, price))
            gnc_error_dialog (GTK_WINDOW (info->window), "%s", _("Error adding price."));

    }

    amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->cash_edit));
    if (gnc_numeric_positive_p (amount))
    {
        const char *memo;

        memo = gnc_entry_get_text (GTK_ENTRY (info->memo_entry));

        /* asset split */
        account = gnc_account_sel_get_account (info->asset_account);

        split = xaccMallocSplit (gnc_get_current_book ());

        xaccAccountBeginEdit (account);
        account_commits = g_list_prepend (account_commits, account);

        xaccAccountInsertSplit (account, split);

        xaccTransAppendSplit (trans, split);

        xaccSplitSetAmount (split, amount);
        xaccSplitSetValue (split, amount);

        xaccSplitSetMemo (split, memo);


        /* income split */
        account = gnc_account_sel_get_account (info->income_account);

        split = xaccMallocSplit (gnc_get_current_book ());

        xaccAccountBeginEdit (account);
        account_commits = g_list_prepend (account_commits, account);

        xaccAccountInsertSplit (account, split);

        xaccTransAppendSplit (trans, split);

        xaccSplitSetAmount (split, gnc_numeric_neg (amount));
        xaccSplitSetValue (split, gnc_numeric_neg (amount));

        xaccSplitSetMemo (split, memo);
    }

    xaccTransCommitEdit (trans);

    for (node = account_commits; node; node = node->next)
        xaccAccountCommitEdit (node->data);
    g_list_free (account_commits);

    gnc_resume_gui_refresh ();

    gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);
}


static void
stock_split_cancel (StockSplitInfo *info)
{
    gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);
}

static gboolean
stock_split_page_is_complete (StockSplitInfo *info, guint page)
{
    switch (page)
    {
    case 1:
        return stock_split_get_selected_account (info) != NULL;
    case 2:
        return stock_split_details_complete (info);
    case 3:
        return stock_split_cash_complete (info);
    default:
        return TRUE;
    }
}

static void
stock_split_update_navigation (StockSplitInfo *info)
{
    gboolean is_last = info->current_page == G_N_ELEMENTS (info->pages) - 1;
    GtkWidget *default_widget = NULL;

    gtk_widget_set_sensitive (info->back_button, info->current_page != 0);
    gtk_widget_set_visible (info->next_button, !is_last);
    gtk_widget_set_sensitive (info->next_button,
                              stock_split_page_is_complete (info,
                                                            info->current_page));
    gtk_widget_set_visible (info->apply_button, is_last);
    gtk_widget_set_sensitive (info->apply_button, is_last);

    if (is_last)
        default_widget = info->apply_button;
    else if (gtk_widget_get_sensitive (info->next_button))
        default_widget = info->next_button;
    gtk_window_set_default_widget (info->window, default_widget);
}

static void
stock_split_show_page (StockSplitInfo *info, guint page)
{
    g_return_if_fail (page < G_N_ELEMENTS (info->pages));

    if (page == 2)
        refresh_details_page (info);

    info->current_page = page;
    gtk_stack_set_visible_child (info->stack, info->pages[page]);
    stock_split_update_navigation (info);
}

static void
stock_split_back_clicked_cb (GtkButton *button, StockSplitInfo *info)
{
    if (info->current_page != 0)
        stock_split_show_page (info, info->current_page - 1);
    (void)button;
}

static void
stock_split_next_clicked_cb (GtkButton *button, StockSplitInfo *info)
{
    if (info->current_page + 1 < G_N_ELEMENTS (info->pages) &&
        stock_split_page_is_complete (info, info->current_page))
        stock_split_show_page (info, info->current_page + 1);
    (void)button;
}

static void
stock_split_apply_clicked_cb (GtkButton *button, StockSplitInfo *info)
{
    stock_split_finish (info);
    (void)button;
}

static void
stock_split_cancel_clicked_cb (GtkButton *button, StockSplitInfo *info)
{
    stock_split_cancel (info);
    (void)button;
}

static gboolean
stock_split_close_request_cb (GtkWindow *window, StockSplitInfo *info)
{
    stock_split_cancel (info);
    (void)window;
    return TRUE;
}



static void
gnc_stock_split_details_valid_cb (GtkWidget *widget, gpointer user_data)
{
    stock_split_update_navigation (user_data);
    (void)widget;
}

static void
gnc_stock_split_cash_valid_cb (GtkWidget *widget, gpointer user_data)
{
    stock_split_update_navigation (user_data);
    (void)widget;
}


static void
gnc_stock_split_cash_selection_changed_cb (GNCAccountSel *selector,
                                           StockSplitInfo *info)
{
    gnc_stock_split_cash_valid_cb (NULL, info);
    (void)selector;
}

static GtkWidget *
gnc_stock_split_assistant_create (StockSplitInfo *info)
{
    GtkBuilder *builder;
    GtkWindow *window;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-stock-split.glade", "stock_split_assistant");
    window = GTK_WINDOW (gtk_builder_get_object (builder, "stock_split_assistant"));
    info->window = window;

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(window), "gnc-id-assistant-stock-split");

    info->stack = GTK_STACK (gtk_builder_get_object (builder, "stock_split_stack"));
    info->pages[0] = GTK_WIDGET (gtk_builder_get_object (builder, "intro_page_label"));
    info->pages[1] = GTK_WIDGET (gtk_builder_get_object (builder, "stock_account_page"));
    info->pages[2] = GTK_WIDGET (gtk_builder_get_object (builder, "stock_details_page"));
    info->pages[3] = GTK_WIDGET (gtk_builder_get_object (builder, "stock_cash_page"));
    info->pages[4] = GTK_WIDGET (gtk_builder_get_object (builder, "finish_page_label"));
    info->back_button = GTK_WIDGET (gtk_builder_get_object (builder, "stock_split_back"));
    info->next_button = GTK_WIDGET (gtk_builder_get_object (builder, "stock_split_next"));
    info->apply_button = GTK_WIDGET (gtk_builder_get_object (builder, "stock_split_apply"));

    g_signal_connect (info->back_button, "clicked",
                      G_CALLBACK (stock_split_back_clicked_cb), info);
    g_signal_connect (info->next_button, "clicked",
                      G_CALLBACK (stock_split_next_clicked_cb), info);
    g_signal_connect (info->apply_button, "clicked",
                      G_CALLBACK (stock_split_apply_clicked_cb), info);
    g_signal_connect (gtk_builder_get_object (builder, "stock_split_cancel"),
                      "clicked", G_CALLBACK (stock_split_cancel_clicked_cb), info);
    g_signal_connect (window, "close-request",
                      G_CALLBACK (stock_split_close_request_cb), info);

    /* Account page Widgets */
    info->account_page = GTK_WIDGET (gtk_builder_get_object (builder,
                                                              "stock_account_page"));
    info->account_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder,
                                                                    "account_view"));
    info->account_rows = g_list_store_new (stock_split_row_get_type ());
    info->account_selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (info->account_rows)));
    gtk_single_selection_set_autoselect (info->account_selection, FALSE);
    gtk_column_view_set_model (info->account_view,
                               GTK_SELECTION_MODEL (info->account_selection));
    gtk_column_view_set_show_row_separators
        (info->account_view,
         gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                             GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators
        (info->account_view,
         gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                             GNC_PREF_GRID_LINES_VERTICAL));
    stock_split_add_account_column (info, _("Account"),
                                    STOCK_SPLIT_ACCOUNT_COLUMN_NAME, TRUE);
    stock_split_add_account_column (info, _("Symbol"),
                                    STOCK_SPLIT_ACCOUNT_COLUMN_SYMBOL, FALSE);
    stock_split_add_account_column (info, _("Shares"),
                                    STOCK_SPLIT_ACCOUNT_COLUMN_SHARES, FALSE);
    g_signal_connect (info->account_selection, "selection-changed",
                      G_CALLBACK (stock_split_selection_changed_cb), info);

    /* Details Page Widgets */
    {
        GtkWidget *table;
        GtkWidget *amount;
        GtkWidget *date;
        GtkWidget *label;

        table = GTK_WIDGET(gtk_builder_get_object(builder, "stock_details_table"));
        info->description_entry = GTK_WIDGET(gtk_builder_get_object(builder, "description_entry"));

        date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        gtk_grid_attach (GTK_GRID(table), date, 1, 0, 1, 1);
        gtk_widget_set_visible (GTK_WIDGET(date), TRUE);
        info->date_edit = date;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "date_label"));
        gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);

        amount = gnc_amount_edit_new ();
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_details_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_grid_attach (GTK_GRID(table), amount, 1, 1, 1, 1);
        gtk_widget_set_visible (GTK_WIDGET(amount), TRUE);
        info->distribution_edit = amount;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "distribution_label"));
        gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(amount), label);

        amount = gnc_amount_edit_new ();
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (amount),
                                        gnc_default_price_print_info (gnc_default_currency()));
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_details_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_grid_attach (GTK_GRID(table), amount, 1, 5, 1, 1);
        gtk_widget_set_visible (GTK_WIDGET(amount), TRUE);
        info->price_edit = amount;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "price_label"));
        gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(amount), label);

        info->price_currency_edit = gnc_currency_edit_new();
        gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(info->price_currency_edit), gnc_default_currency());
        gtk_widget_set_visible (GTK_WIDGET(info->price_currency_edit), TRUE);
        gtk_grid_attach (GTK_GRID(table), info->price_currency_edit, 1, 6, 1, 1);
        g_signal_connect (info->price_currency_edit, "changed",
                          G_CALLBACK (gnc_stock_split_details_valid_cb), info);
    }

    /* Cash page Widgets */
    {
        GtkWidget *box;
        GtkWidget *amount;
        GtkWidget *label;
        GtkWidget *scroll;
        GList *types = NULL;

        box = GTK_WIDGET (gtk_builder_get_object (builder, "cash_box"));
        amount = gnc_amount_edit_new ();
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_cash_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_box_append (GTK_BOX (box), amount);
        info->cash_edit = amount;

        label = GTK_WIDGET (gtk_builder_get_object (builder, "cash_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL (label), amount);
        info->memo_entry = GTK_WIDGET (gtk_builder_get_object (builder, "memo_entry"));

        info->income_account = GNC_ACCOUNT_SEL (gnc_account_sel_new ());
        types = g_list_append (types, GINT_TO_POINTER (ACCT_TYPE_INCOME));
        gnc_account_sel_set_acct_filters (info->income_account, types, NULL);
        g_list_free (types);
        g_signal_connect (info->income_account, "account_sel_changed",
                          G_CALLBACK (gnc_stock_split_cash_selection_changed_cb), info);
        label = GTK_WIDGET (gtk_builder_get_object (builder, "income_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL (label),
                                       GTK_WIDGET (info->income_account));
        scroll = GTK_WIDGET (gtk_builder_get_object (builder, "income_scroll"));
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroll),
                                       GTK_WIDGET (info->income_account));

        info->asset_account = GNC_ACCOUNT_SEL (gnc_account_sel_new ());
        types = g_list_append (types, GINT_TO_POINTER (ACCT_TYPE_BANK));
        types = g_list_append (types, GINT_TO_POINTER (ACCT_TYPE_CASH));
        types = g_list_append (types, GINT_TO_POINTER (ACCT_TYPE_ASSET));
        gnc_account_sel_set_acct_filters (info->asset_account, types, NULL);
        g_list_free (types);
        g_signal_connect (info->asset_account, "account_sel_changed",
                          G_CALLBACK (gnc_stock_split_cash_selection_changed_cb), info);
        label = GTK_WIDGET (gtk_builder_get_object (builder, "asset_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL (label),
                                       GTK_WIDGET (info->asset_account));
        scroll = GTK_WIDGET (gtk_builder_get_object (builder, "asset_scroll"));
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroll),
                                       GTK_WIDGET (info->asset_account));
    }

    g_signal_connect (window, "destroy",
                      G_CALLBACK (gnc_stock_split_assistant_window_destroy_cb), info);

    info->current_page = 0;
    stock_split_show_page (info, info->current_page);
    g_object_unref (builder);
    return GTK_WIDGET (window);

}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    StockSplitInfo *info = user_data;
    Account *selected_account = stock_split_get_selected_account (info);

    if (fill_account_list (info, selected_account) == 0)
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);

    (void)changes;
}

static void
close_handler (gpointer user_data)
{
    StockSplitInfo *info = user_data;

    gtk_window_destroy (GTK_WINDOW (info->window));
}

/********************************************************************\
 * gnc_stock_split_dialog                                           *
 *   opens up a window to record a stock split                      *
 *                                                                  *
 * Args:   parent  - the parent ofthis window                       *
 *         initial - the initial account to use                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_stock_split_dialog (GtkWidget *parent, Account * initial)
{
    StockSplitInfo *info;
    gint component_id;

    info = g_new0 (StockSplitInfo, 1);

    info->acct = NULL;

    gnc_stock_split_assistant_create (info);

    component_id = gnc_register_gui_component (ASSISTANT_STOCK_SPLIT_CM_CLASS,
                   refresh_handler, close_handler,
                   info);

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    if (fill_account_list (info, initial) == 0)
    {
        gnc_warning_dialog (GTK_WINDOW (parent), "%s", _("You don't have any stock accounts with balances!"));
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);
        return;
    }

    gtk_window_set_transient_for (GTK_WINDOW (info->window), GTK_WINDOW(parent));
    gtk_window_present (GTK_WINDOW (info->window));

    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

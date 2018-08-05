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
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"


#define ASSISTANT_STOCK_SPLIT_CM_CLASS "assistant-stock-split"

enum split_cols
{
    SPLIT_COL_ACCOUNT = 0,
    SPLIT_COL_FULLNAME,
    SPLIT_COL_MNEMONIC,
    SPLIT_COL_SHARES,
    NUM_SPLIT_COLS
};

/** structures *********************************************************/
typedef struct
{
    GtkWidget * window;
    GtkWidget * assistant;

    /* account page data */
    GtkWidget * account_view;
    Account   * acct;

    /* info page data */
    GtkWidget * date_edit;
    GtkWidget * distribution_edit;
    GtkWidget * description_entry;
    GtkWidget * price_edit;
    GtkWidget * price_currency_edit;

    /* cash in lieu page data */
    GtkWidget * cash_edit;
    GtkWidget * memo_entry;
    GtkWidget * income_tree;
    GtkWidget * asset_tree;
} StockSplitInfo;


/** declarations *******************************************************/
void     gnc_stock_split_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data);
void     gnc_stock_split_assistant_prepare           (GtkAssistant  *assistant,
        GtkWidget *page,
        gpointer user_data);
void     gnc_stock_split_assistant_details_prepare   (GtkAssistant *assistant,
        gpointer user_data);
gboolean gnc_stock_split_assistant_details_complete  (GtkAssistant *assistant,
        gpointer user_data);
gboolean gnc_stock_split_assistant_cash_complete     (GtkAssistant *assistant,
        gpointer user_data);
void     gnc_stock_split_assistant_finish            (GtkAssistant *assistant,
        gpointer user_data);
void     gnc_stock_split_assistant_cancel            (GtkAssistant *gtkassistant,
        gpointer user_data);

/******* implementations ***********************************************/
void
gnc_stock_split_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    StockSplitInfo *info = user_data;

    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);

    g_free (info);
}


static int
fill_account_list (StockSplitInfo *info, Account *selected_account)
{
    GtkTreeRowReference *reference = NULL;
    GtkTreeView *view;
    GtkListStore *list;
    GtkTreeIter iter;
    GtkTreePath *path;
    GList *accounts;
    GList *node;
    gint rows = 0;
    gchar *full_name;

    view = GTK_TREE_VIEW(info->account_view);
    list = GTK_LIST_STORE(gtk_tree_view_get_model(view));

    gtk_list_store_clear (list);

    accounts = gnc_account_get_descendants_sorted (gnc_get_current_root_account ());
    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data;
        GNCPrintAmountInfo print_info;
        const gnc_commodity *commodity;
        gnc_numeric balance;

        if (!xaccAccountIsPriced(account))
            continue;

        balance = xaccAccountGetBalance (account);
        if (gnc_numeric_zero_p (balance))
            continue;

        if (xaccAccountGetPlaceholder (account))
            continue;

        commodity = xaccAccountGetCommodity (account);

        full_name = gnc_account_get_full_name (account);
        print_info = gnc_account_print_info (account, FALSE);

        gtk_list_store_append(list, &iter);
        gtk_list_store_set(list, &iter,
                           SPLIT_COL_ACCOUNT,  account,
                           SPLIT_COL_FULLNAME, full_name,
                           SPLIT_COL_MNEMONIC, gnc_commodity_get_mnemonic(commodity),
                           SPLIT_COL_SHARES,   xaccPrintAmount(balance, print_info),
                           -1);

        if (account == selected_account)
        {
            path = gtk_tree_model_get_path(GTK_TREE_MODEL(list), &iter);
            reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(list), path);
            gtk_tree_path_free(path);
        }

        g_free (full_name);

        rows++;
    }
    g_list_free(accounts);

    if (reference)
    {
        GtkTreeSelection* selection = gtk_tree_view_get_selection(view);
        path = gtk_tree_row_reference_get_path(reference);
        gtk_tree_row_reference_free(reference);
        if (path)
        {
            gtk_tree_selection_select_path(selection, path);
            gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
            gtk_tree_path_free(path);
        }
    }

    return rows;
}


static void
selection_changed_cb (GtkTreeSelection *selection,
                      gpointer user_data)
{
    StockSplitInfo *info = user_data;
    GtkTreeModel *list;
    GtkTreeIter iter;

    if (!gtk_tree_selection_get_selected(selection, &list, &iter))
        return;
    gtk_tree_model_get(list, &iter,
                       SPLIT_COL_ACCOUNT, &info->acct,
                       -1);
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

    account = info->acct;

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


void gnc_stock_split_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page,
                                        gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page(assistant);

    if (currentpage == 2) /* Current page is details page */
        gnc_stock_split_assistant_details_prepare(assistant, user_data);
}


void
gnc_stock_split_assistant_details_prepare (GtkAssistant *assistant,
        gpointer user_data)
{
    StockSplitInfo *info = user_data;

    refresh_details_page(info);
}


gboolean
gnc_stock_split_assistant_details_complete (GtkAssistant *assistant,
        gpointer user_data)
{
    StockSplitInfo *info = user_data;
    gnc_numeric amount;
    gint result;

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->distribution_edit), &amount, TRUE);
    if ( result != 0)
        return FALSE; /* Parsing error or field is empty */

    if (gnc_numeric_zero_p (amount))
        return FALSE; /* field value is 0 */

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->price_edit), &amount, TRUE);
    if (result == -1)
        return TRUE; /* Optional field is empty */
    else if ( result > 0)
        return FALSE; /* Parsing error */
    else if (gnc_numeric_negative_p (amount))
        return FALSE; /* Negative price is not allowed */
    else
        return TRUE; /* Valid positive price */
}


gboolean
gnc_stock_split_assistant_cash_complete (GtkAssistant *assistant,
        gpointer user_data)
{
    StockSplitInfo *info = user_data;
    gnc_numeric amount;
    gint result;
    Account *account;

    result = gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->cash_edit), &amount, TRUE);
    if (result == -1)
        return TRUE; /* Optional field is empty */
    else if ( result > 0)
        return FALSE; /* Parsing error */
    else if (gnc_numeric_negative_p (amount))
        return FALSE; /* Negative cash amount is not allowed */

    /* We have a positive cash amount */
    account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->income_tree));
    if (!account)
        return FALSE;

    account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->asset_tree));
    if (!account)
        return FALSE;

    return TRUE;
}


void
gnc_stock_split_assistant_finish (GtkAssistant *assistant,
                                  gpointer user_data)
{
    StockSplitInfo *info = user_data;
    GList *account_commits;
    GList *node;

    gnc_numeric amount;
    Transaction *trans;
    Account *account;
    Split *split;
    time64 date;

    account = info->acct;
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

        description = gtk_entry_get_text (GTK_ENTRY (info->description_entry));
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
    /* Translators: This string has a disambiguation prefix */
    gnc_set_num_action (NULL, split, NULL, Q_("Action Column|Split"));

    amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->price_edit));
    if (gnc_numeric_positive_p (amount))
    {
        QofBook *book;
        GNCPrice *price;
        GNCPriceDB *pdb;
        GNCCurrencyEdit *ce;
        Timespec ts;

        ce = GNC_CURRENCY_EDIT (info->price_currency_edit);

        ts.tv_sec = date;
        ts.tv_nsec = 0;

        price = gnc_price_create (gnc_get_current_book ());

        gnc_price_begin_edit (price);
        gnc_price_set_commodity (price, xaccAccountGetCommodity (account));
        gnc_price_set_currency (price, gnc_currency_edit_get_currency (ce));
        gnc_price_set_time (price, ts);
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

        memo = gtk_entry_get_text (GTK_ENTRY (info->memo_entry));

        /* asset split */
        account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->asset_tree));

        split = xaccMallocSplit (gnc_get_current_book ());

        xaccAccountBeginEdit (account);
        account_commits = g_list_prepend (account_commits, account);

        xaccAccountInsertSplit (account, split);

        xaccTransAppendSplit (trans, split);

        xaccSplitSetAmount (split, amount);
        xaccSplitSetValue (split, amount);

        xaccSplitSetMemo (split, memo);


        /* income split */
        account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->income_tree));

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


void
gnc_stock_split_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    StockSplitInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);
}


static gboolean
gnc_stock_split_assistant_view_filter_income (Account  *account,
        gpointer  data)
{
    GNCAccountType type;

    type = xaccAccountGetType(account);
    return (type == ACCT_TYPE_INCOME);
}


static gboolean
gnc_stock_split_assistant_view_filter_asset (Account  *account,
        gpointer  data)
{
    GNCAccountType type;

    type = xaccAccountGetType(account);
    return ((type == ACCT_TYPE_BANK) || (type == ACCT_TYPE_CASH) ||
            (type == ACCT_TYPE_ASSET));
}


static void
gnc_stock_split_details_valid_cb (GtkWidget *widget, gpointer user_data)
{
    StockSplitInfo *info = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     gnc_stock_split_assistant_details_complete (assistant, user_data));
}


static void
gnc_stock_split_cash_valid_cb (GtkWidget *widget, gpointer user_data)
{
    StockSplitInfo *info = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    gtk_assistant_set_page_complete (assistant, page,
                                     gnc_stock_split_assistant_cash_complete (assistant, user_data));
}


static GtkWidget *
gnc_stock_split_assistant_create (StockSplitInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *window;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-stock-split.glade", "stock_split_assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "stock_split_assistant"));
    info->window = window;

    // Set the style context for this assistant so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(window), "GncAssistStockSplit");

    /* Enable buttons on first, second, fourth and last page. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "intro_page_label")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "stock_account_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "stock_cash_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "finish_page_label")),
                                     TRUE);

    /* Account page Widgets */
    {
        GtkTreeView *view;
        GtkListStore *store;
        GtkTreeSelection *selection;
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *column;

        info->account_view = GTK_WIDGET(gtk_builder_get_object(builder, "account_view"));

        view = GTK_TREE_VIEW(info->account_view);

        // Set grid lines option to preference
        gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());

        store = gtk_list_store_new(NUM_SPLIT_COLS, G_TYPE_POINTER, G_TYPE_STRING,
                                   G_TYPE_STRING, G_TYPE_STRING);
        gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
        g_object_unref(store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Account"), renderer,
                 "text", SPLIT_COL_FULLNAME,
                 NULL);
        gtk_tree_view_append_column(view, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Symbol"), renderer,
                 "text", SPLIT_COL_MNEMONIC,
                 NULL);
        gtk_tree_view_append_column(view, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Shares"), renderer,
                 "text", SPLIT_COL_SHARES,
                 NULL);
        gtk_tree_view_append_column(view, column);

        selection = gtk_tree_view_get_selection(view);
        gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);
        g_signal_connect (selection, "changed",
                          G_CALLBACK (selection_changed_cb), info);

    }

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
        gtk_widget_show (date);
        info->date_edit = date;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "date_label"));
        gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);

        amount = gnc_amount_edit_new ();
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_details_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_grid_attach (GTK_GRID(table), amount, 1, 1, 1, 1);
        gtk_widget_show (amount);
        info->distribution_edit = amount;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "distribution_label"));
        gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

        amount = gnc_amount_edit_new ();
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (amount),
                                        gnc_default_price_print_info ());
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_details_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_grid_attach (GTK_GRID(table), amount, 1, 5, 1, 1);
        gtk_widget_show (amount);
        info->price_edit = amount;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "price_label"));
        gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

        info->price_currency_edit = gnc_currency_edit_new();
        gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(info->price_currency_edit), gnc_default_currency());
        gtk_widget_show (info->price_currency_edit);
        gtk_grid_attach (GTK_GRID(table), info->price_currency_edit, 1, 6, 1, 1);
    }

    /* Cash page Widgets */
    {
        GtkWidget *box;
        GtkWidget *tree;
        GtkWidget *amount;
        GtkWidget *label;
        GtkWidget *scroll;
        GtkTreeSelection *selection;

        box = GTK_WIDGET(gtk_builder_get_object(builder, "cash_box"));
        amount = gnc_amount_edit_new ();
        g_signal_connect (amount, "changed",
                          G_CALLBACK (gnc_stock_split_cash_valid_cb), info);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
        gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
        info->cash_edit = amount;

        label = GTK_WIDGET(gtk_builder_get_object(builder, "cash_label"));
        gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

        info->memo_entry = GTK_WIDGET(gtk_builder_get_object(builder, "memo_entry"));

        /* income tree */
        tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
        info->income_tree = tree;
        gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (tree),
                                          gnc_stock_split_assistant_view_filter_income,
                                          NULL, /* user data */
                                          NULL  /* destroy callback */);

        gtk_widget_show (tree);

        gtk_tree_view_expand_all (GTK_TREE_VIEW(tree));
        selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
        gtk_tree_selection_unselect_all (selection);
        g_signal_connect (selection, "changed",
                          G_CALLBACK (gnc_stock_split_cash_valid_cb), info);

        label = GTK_WIDGET(gtk_builder_get_object(builder, "income_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL(label), tree);

        scroll = GTK_WIDGET(gtk_builder_get_object(builder, "income_scroll"));
        gtk_container_add (GTK_CONTAINER (scroll), tree);

        /* asset tree */
        tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
        info->asset_tree = tree;
        gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (tree),
                                          gnc_stock_split_assistant_view_filter_asset,
                                          NULL /* user data */,
                                          NULL /* destroy callback */);

        gtk_widget_show (tree);

        label = GTK_WIDGET(gtk_builder_get_object(builder, "asset_label"));
        gtk_label_set_mnemonic_widget (GTK_LABEL(label), tree);

        scroll = GTK_WIDGET(gtk_builder_get_object(builder, "asset_scroll"));
        gtk_container_add (GTK_CONTAINER (scroll), tree);

        gtk_tree_view_expand_all (GTK_TREE_VIEW(tree));
        selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
        gtk_tree_selection_unselect_all (selection);
        g_signal_connect (selection, "changed",
                          G_CALLBACK (gnc_stock_split_cash_valid_cb), info);
    }

    g_signal_connect (G_OBJECT(window), "destroy",
                      G_CALLBACK (gnc_stock_split_assistant_window_destroy_cb), info);

    gtk_builder_connect_signals(builder, info);
    g_object_unref(G_OBJECT(builder));
    return window;

}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    StockSplitInfo *info = user_data;
    Account *old_account;

    old_account = info->acct;

    if (fill_account_list (info, info->acct) == 0)
    {
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_SPLIT_CM_CLASS, info);
        return;
    }

    if (NULL == info->acct || old_account == info->acct) return;
}

static void
close_handler (gpointer user_data)
{
    StockSplitInfo *info = user_data;

    gtk_widget_destroy (info->window);
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

    gtk_widget_show_all (info->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

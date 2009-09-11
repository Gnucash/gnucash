/********************************************************************\
 * druid-stock-split.c -- stock split druid for GnuCash             *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (c) 2001 Dave Peticolas <dave@krondo.com>              *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>

#include "Transaction.h"
#include "dialog-utils.h"
#include "druid-stock-split.h"
#include "druid-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-book.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "qof.h"
#include "gnc-exp-parser.h"
#include "gnc-gui-query.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"


#define DRUID_STOCK_SPLIT_CM_CLASS "druid-stock-split"

enum split_cols {
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
  GtkWidget * druid;

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
void     gnc_stock_split_druid_window_destroy_cb (GtkObject *object, gpointer data);
gboolean gnc_stock_split_druid_account_next      (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
void     gnc_stock_split_druid_details_prepare   (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
gboolean gnc_stock_split_druid_details_next      (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
void     gnc_stock_split_druid_cash_prepare      (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
gboolean gnc_stock_split_druid_cash_next         (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
void     gnc_stock_split_druid_finish            (GnomeDruidPage *druidpage,
						  gpointer arg1,
						  gpointer user_data);
void     gnc_stock_split_druid_cancel_druid      (GnomeDruid *druid,
						  gpointer user_data);

/******* implementations ***********************************************/
void
gnc_stock_split_druid_window_destroy_cb (GtkObject *object, gpointer data)
{
  StockSplitInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);

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

    if (account == selected_account) {
      path = gtk_tree_model_get_path(GTK_TREE_MODEL(list), &iter);
      reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(list), path);
      gtk_tree_path_free(path);
    }

    g_free (full_name);

    rows++;
  }
  g_list_free(accounts);

  if (reference) {
    GtkTreeSelection* selection = gtk_tree_view_get_selection(view);
    path = gtk_tree_row_reference_get_path(reference);
    gtk_tree_row_reference_free(reference);
    if (path) {
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
      gtk_tree_path_free(path);
    }
  }

  return rows;
}

static void
selection_changed (GtkTreeSelection *selection,
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
  db = gnc_book_get_pricedb(book);

  prices = gnc_pricedb_lookup_latest_any_currency(db, commodity);
  if (prices) {
    /* Use the first existing price */
    currency = gnc_price_get_currency(prices->data);
  } else {
    /* Take a wild guess. */
    currency = gnc_default_currency ();
 }
  gnc_price_list_destroy(prices);

  gnc_currency_edit_set_currency
    (GNC_CURRENCY_EDIT (info->price_currency_edit),
     currency);
}

gboolean
gnc_stock_split_druid_account_next (GnomeDruidPage *druidpage,
				    gpointer arg1,
				    gpointer user_data)
{
  StockSplitInfo *info = user_data;

  g_return_val_if_fail (info->acct != NULL, TRUE);

  refresh_details_page (info);

  return FALSE;
}

static void
gnc_parse_error_dialog (StockSplitInfo *info, const char *error_string)
{
  const char * parse_error_string;

  parse_error_string = gnc_exp_parser_error_string ();
  if (parse_error_string == NULL)
    parse_error_string = "";

  if (error_string == NULL)
    error_string = "";

  gnc_error_dialog (info->window,
		    "%s.\n\n%s: %s.",
		    error_string, _("Error"),
		    parse_error_string);
}

void
gnc_stock_split_druid_details_prepare (GnomeDruidPage *druidpage,
		 gpointer arg1,
		 gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gtk_widget_grab_focus(info->distribution_edit);
}

gboolean
gnc_stock_split_druid_details_next (GnomeDruidPage *druidpage,
				    gpointer arg1,
				    gpointer user_data)
{
  StockSplitInfo *info = user_data;
  gnc_numeric amount;

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (info->distribution_edit)))
  {
    gnc_parse_error_dialog (info,
                            _("You must enter a valid distribution amount."));
    return TRUE;
  }

  amount = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (info->distribution_edit));

  if (gnc_numeric_zero_p (amount))
  {
    const char *message = _("You must enter a distribution amount.");
    gnc_error_dialog (info->window, "%s", message);
    return TRUE;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (info->price_edit)))
  {
    gnc_parse_error_dialog (info,
                            _("You must either enter a valid price "
                              "or leave it blank."));
    return TRUE;
  }

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->price_edit));

  if (gnc_numeric_negative_p (amount))
  {
    const char *message = _("The price must be positive.");
    gnc_error_dialog (info->window, "%s", message);
    return TRUE;
  }

  return FALSE;
}

void
gnc_stock_split_druid_cash_prepare (GnomeDruidPage *druidpage,
				    gpointer arg1,
				    gpointer user_data)
{
  StockSplitInfo *info = user_data;
  GtkTreeSelection *selection;

  gtk_tree_view_expand_all (GTK_TREE_VIEW(info->income_tree));
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(info->income_tree));
  gtk_tree_selection_unselect_all (selection);

  gtk_tree_view_expand_all (GTK_TREE_VIEW(info->asset_tree));
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(info->asset_tree));
  gtk_tree_selection_unselect_all (selection);

  gtk_widget_grab_focus(info->cash_edit);
}

gboolean
gnc_stock_split_druid_cash_next (GnomeDruidPage *druidpage,
				 gpointer arg1,
				 gpointer user_data)
{
  StockSplitInfo *info = user_data;
  gnc_numeric amount;

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (info->cash_edit)))
  {
    gnc_parse_error_dialog (info,
                            _("You must either enter a valid cash amount "
                              "or leave it blank."));
    return TRUE;
  }

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->cash_edit));

  if (gnc_numeric_negative_p (amount))
  {
    const char *message = _("The cash distribution must be positive.");
    gnc_error_dialog (info->window, "%s", message);
    return TRUE;
  }

  if (gnc_numeric_positive_p (amount))
  {
    Account *account;

    account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->income_tree));
    if (!account)
    {
      const char *message = _("You must select an income account "
                              "for the cash distribution.");
      gnc_error_dialog (info->window, "%s", message);
      return TRUE;
    }

    account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT(info->asset_tree));
    if (!account)
    {
      const char *message = _("You must select an asset account "
                              "for the cash distribution.");
      gnc_error_dialog (info->window, "%s", message);
      return TRUE;
    }
  }

  return FALSE;
}

void
gnc_stock_split_druid_finish (GnomeDruidPage *druidpage,
			      gpointer arg1,
			      gpointer user_data)
{
  StockSplitInfo *info = user_data;
  GList *account_commits;
  GList *node;

  gnc_numeric amount;
  Transaction *trans;
  Account *account;
  Split *split;
  time_t date;

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
  xaccTransSetDatePostedSecs (trans, date);

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
  xaccSplitSetAction (split, _("Split"));

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
    gnc_price_set_source (price, "user:stock-split");
    gnc_price_set_typestr (price, "unknown");
    gnc_price_set_value (price, amount);
    gnc_price_commit_edit (price);

    book = gnc_get_current_book ();
    pdb = gnc_book_get_pricedb (book);

    if (!gnc_pricedb_add_price (pdb, price))
      gnc_error_dialog (info->window, _("Error adding price."));

    gnc_price_unref (price);
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

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}

void
gnc_stock_split_druid_cancel_druid (GnomeDruid *druid, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}

static gboolean
gnc_stock_split_druid_view_filter_income (Account  *account,
					  gpointer  data)
{
  GNCAccountType type;

  type = xaccAccountGetType(account);
  return (type == ACCT_TYPE_INCOME);
}

static gboolean
gnc_stock_split_druid_view_filter_asset (Account  *account,
					 gpointer  data)
{
  GNCAccountType type;

  type = xaccAccountGetType(account);
  return ((type == ACCT_TYPE_BANK) || (type == ACCT_TYPE_CASH) ||
	  (type == ACCT_TYPE_ASSET));
}

static void
gnc_stock_split_druid_create (StockSplitInfo *info)
{
  GladeXML *xml;

  xml = gnc_glade_xml_new ("stocks.glade", "Stock Split Druid");
  info->window = glade_xml_get_widget (xml, "Stock Split Druid");
  info->druid = glade_xml_get_widget (xml, "stock_split_druid");

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, info);

  /* libglade2 is broken. It should read these from the glade file. */
  gnc_druid_set_colors (GNOME_DRUID(info->druid));
  gnc_druid_set_watermark_images (GNOME_DRUID(info->druid),
				  "stock_split_title.png",
				  "stock_split_watermark.png");


  /* account list */
  {
    GtkTreeView *view;
    GtkListStore *store;
    GtkTreeSelection *selection;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    info->account_view = glade_xml_get_widget (xml, "account_view");

    view = GTK_TREE_VIEW(info->account_view);

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
		      G_CALLBACK (selection_changed), info);
  }

  /* info widgets */
  {
    GtkWidget *box;
    GtkWidget *amount;
    GtkWidget *date;
    GtkWidget *ce;
    GtkWidget *label;

    info->description_entry = glade_xml_get_widget (xml, "description_entry");

    box = glade_xml_get_widget (xml, "date_box");
    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (box), date, TRUE, TRUE, 0);
    info->date_edit = date;
    label = glade_xml_get_widget (xml, "date_label");
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);

    box = glade_xml_get_widget (xml, "distribution_box");
    amount = gnc_amount_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->distribution_edit = amount;
    label = glade_xml_get_widget (xml, "distribution_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

    box = glade_xml_get_widget (xml, "price_box");
    amount = gnc_amount_edit_new ();
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (amount),
                                    gnc_default_price_print_info ());
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->price_edit = amount;
    label = glade_xml_get_widget (xml, "price_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

    box = glade_xml_get_widget (xml, "price_currency_box");
    ce = gnc_currency_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), ce, TRUE, TRUE, 0);
    info->price_currency_edit = ce;
    label = glade_xml_get_widget (xml, "currency_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), ce);
  }

  /* Cash in Lieu page */
  {
    GtkWidget *box;
    GtkWidget *tree;
    GtkWidget *amount;
    GtkWidget *label;
    GtkWidget *scroll;

    box = glade_xml_get_widget (xml, "cash_box");
    amount = gnc_amount_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->cash_edit = amount;
    label = glade_xml_get_widget (xml, "cash_label");
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), amount);

    info->memo_entry = glade_xml_get_widget (xml, "memo_entry");

    /* income tree */
    tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    info->income_tree = tree;
    gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (tree),
				      gnc_stock_split_druid_view_filter_income,
				      NULL, /* user data */
				      NULL  /* destroy callback */);

    gtk_widget_show (tree);

    label = glade_xml_get_widget (xml, "income_label");
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), tree);

    scroll = glade_xml_get_widget (xml, "income_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), tree);


    /* asset tree */
    tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    info->asset_tree = tree;
    gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT (tree),
				      gnc_stock_split_druid_view_filter_asset,
				      NULL /* user data */,
				      NULL /* destroy callback */);

    gtk_widget_show (tree);

    label = glade_xml_get_widget (xml, "asset_label");
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), tree);

    scroll = glade_xml_get_widget (xml, "asset_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), tree);
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  StockSplitInfo *info = user_data;
  Account *old_account;
  GtkWidget *page;
  GladeXML *xml;

  old_account = info->acct;

  if (fill_account_list (info, info->acct) == 0)
  {
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  if (NULL == info->acct || old_account == info->acct) return;

  xml = glade_get_widget_tree (info->window);
  page = glade_xml_get_widget (xml, "account_page");

  gnome_druid_set_page (GNOME_DRUID (info->druid), GNOME_DRUID_PAGE (page));
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

  gnc_stock_split_druid_create (info);

  component_id = gnc_register_gui_component (DRUID_STOCK_SPLIT_CM_CLASS,
                                             refresh_handler, close_handler,
                                             info);

  gnc_gui_component_watch_entity_type (component_id,
                                       GNC_ID_ACCOUNT,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  if (fill_account_list (info, initial) == 0)
  {
    gnc_warning_dialog (parent, _("You don't have any stock accounts with balances!"));
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

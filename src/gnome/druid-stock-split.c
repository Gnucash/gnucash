/********************************************************************\
 * druid-stock-split.c -- stock split druid for GnuCash             *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "FileDialog.h"
#include "Group.h"
#include "account-tree.h"
#include "dialog-utils.h"
#include "glade-gnc-dialogs.h"
#include "glade-support.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-dateedit.h"
#include "gnc-exp-parser.h"
#include "gnc-ui.h"
#include "messages.h"
#include "query-user.h"


#define DRUID_STOCK_SPLIT_CM_CLASS "druid-stock-split"


/** structures *********************************************************/
typedef struct
{
  GtkWidget * window;
  GtkWidget * druid;

  /* account page data */
  GtkWidget * account_list;
  GUID account;

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


/** implementations ****************************************************/
static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  StockSplitInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);

  g_free (info);
}

static int
fill_account_list (StockSplitInfo *info, Account *account)
{
  GtkCList *clist;
  GList *accounts;
  GList *node;
  gint rows = 0;

  clist = GTK_CLIST (info->account_list);

  gtk_clist_freeze (clist);

  gtk_clist_clear (clist);

  accounts = xaccGroupGetSubAccounts (gncGetCurrentGroup ());
  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;
    GNCPrintAmountInfo print_info;
    const gnc_commodity *security;
    GNCAccountType account_type;
    gnc_numeric balance;
    char *strings[4];
    gint row;

    account_type = xaccAccountGetType (account);
    if (account_type != STOCK &&
        account_type != MUTUAL)
      continue;

    balance = xaccAccountGetShareBalance (account);
    if (gnc_numeric_zero_p (balance))
      continue;

    security = xaccAccountGetSecurity (account);

    print_info = gnc_account_quantity_print_info (account, FALSE);

    strings[0] = xaccAccountGetFullName (account,
                                         gnc_get_account_separator ());
    strings[1] = (char *) gnc_commodity_get_mnemonic (security);
    strings[2] = (char *) xaccPrintAmount (balance, print_info);
    strings[3] = NULL;

    row = gtk_clist_append (clist, strings);

    gtk_clist_set_row_data (clist, row, account);

    g_free (strings[0]);

    rows++;
  }

  {
    gint row = 0;

    if (account)
      row = gtk_clist_find_row_from_data (clist, account);

    if (row < 0)
      row = 0;

    gtk_clist_select_row (GTK_CLIST (info->account_list), row, 0);
  }

  gtk_clist_columns_autosize (clist);

  gtk_clist_thaw (clist);

  return rows;
}

static void
clist_select_row (GtkCList *clist,
                  gint row,
                  gint column,
                  GdkEventButton *event,
                  gpointer user_data)
{
  StockSplitInfo *info = user_data;
  Account *account;

  account = gtk_clist_get_row_data (clist, row);

  info->account = *xaccAccountGetGUID (account);
}

static void
refresh_details_page (StockSplitInfo *info)
{
  GNCPrintAmountInfo print_info;
  Account *account;

  account = xaccAccountLookup (&info->account);

  g_return_if_fail (account != NULL);

  print_info = gnc_account_quantity_print_info (account, FALSE);

  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (info->distribution_edit),
                                  print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (info->distribution_edit),
                                xaccAccountGetSecuritySCU (account));

  gnc_currency_edit_set_currency
    (GNC_CURRENCY_EDIT (info->price_currency_edit),
     xaccAccountGetCurrency (account));
}

static gboolean
account_next (GnomeDruidPage *druidpage,
              gpointer arg1,
              gpointer user_data)
{
  StockSplitInfo *info = user_data;
  Account *account;

  account = xaccAccountLookup (&info->account);

  g_return_val_if_fail (account != NULL, TRUE);

  refresh_details_page (info);

  return FALSE;
}

static void
gnc_parse_error_dialog (StockSplitInfo *info, const char *error_string)
{
  const char * parse_error_string;
  char * error_phrase;

  parse_error_string = gnc_exp_parser_error_string ();
  if (error_string == NULL)
    error_string = "";

  error_phrase = g_strdup_printf ("%s.\n\n%s: %s.",
                                  error_string, _("Error"),
                                  parse_error_string);

  gnc_error_dialog_parented (GTK_WINDOW (info->window), error_phrase);

  g_free (error_phrase);
}

static gboolean
details_next (GnomeDruidPage *druidpage,
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
    gnc_error_dialog_parented (GTK_WINDOW (info->window), message);
    return TRUE;
  }

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (info->price_edit)))
  {
    gnc_parse_error_dialog (info,
                            _("You must either enter a valid price\n"
                              "or leave it blank."));
    return TRUE;
  }

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->price_edit));

  if (gnc_numeric_negative_p (amount))
  {
    const char *message = _("The price must be positive.");
    gnc_error_dialog_parented (GTK_WINDOW (info->window), message);
    return TRUE;
  }

  return FALSE;
}

static void
cash_prepare (GnomeDruidPage *druidpage,
              gpointer arg1,
              gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_account_tree_refresh (GNC_ACCOUNT_TREE (info->income_tree));
  gnc_account_tree_expand_all (GNC_ACCOUNT_TREE (info->income_tree));
  gtk_clist_select_row (GTK_CLIST (info->income_tree), 0, 0);

  gnc_account_tree_refresh (GNC_ACCOUNT_TREE (info->asset_tree));
  gnc_account_tree_expand_all (GNC_ACCOUNT_TREE (info->asset_tree));
  gtk_clist_select_row (GTK_CLIST (info->asset_tree), 0, 0);
}

static gboolean
cash_next (GnomeDruidPage *druidpage,
           gpointer arg1,
           gpointer user_data)
{
  StockSplitInfo *info = user_data;
  gnc_numeric amount;

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (info->cash_edit)))
  {
    gnc_parse_error_dialog (info,
                            _("You must either enter a valid cash amount\n"
                              "or leave it blank."));
    return TRUE;
  }

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->cash_edit));

  if (gnc_numeric_negative_p (amount))
  {
    const char *message = _("The cash distribution must be positive.");
    gnc_error_dialog_parented (GTK_WINDOW (info->window), message);
    return TRUE;
  }

  if (gnc_numeric_positive_p (amount))
  {
    Account *account;

    account = gnc_account_tree_get_current_account
      (GNC_ACCOUNT_TREE (info->income_tree));
    if (!account)
    {
      const char *message = _("You must select an income account\n"
                              "for the cash distribution.");
      gnc_error_dialog_parented (GTK_WINDOW (info->window), message);
      return TRUE;
    }

    account = gnc_account_tree_get_current_account
      (GNC_ACCOUNT_TREE (info->asset_tree));
    if (!account)
    {
      const char *message = _("You must select an asset account\n"
                              "for the cash distribution.");
      gnc_error_dialog_parented (GTK_WINDOW (info->window), message);
      return TRUE;
    }
  }

  return FALSE;
}

static void
stock_split_finish (GnomeDruidPage *druidpage,
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

  account = xaccAccountLookup (&info->account);
  g_return_if_fail (account != NULL);

  amount = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (info->distribution_edit));
  g_return_if_fail (!gnc_numeric_zero_p (amount));

  gnc_suspend_gui_refresh ();

  trans = xaccMallocTransaction ();

  xaccTransBeginEdit (trans);

  date = gnc_date_edit_get_date (GNC_DATE_EDIT (info->date_edit));
  xaccTransSetDateSecs (trans, date);

  {
    const char *description;

    description = gtk_entry_get_text (GTK_ENTRY (info->description_entry));
    xaccTransSetDescription (trans, description);
  }

  split = xaccMallocSplit ();

  xaccAccountBeginEdit (account);
  account_commits = g_list_prepend (NULL, account);

  xaccTransAppendSplit (trans, split);

  xaccAccountInsertSplit (account, split);

  xaccSplitSetShareAmount (split, amount);
  xaccSplitMakeStockSplit (split);
  xaccSplitSetAction (split, _("Split"));

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->price_edit));
  if (gnc_numeric_positive_p (amount))
  {
    GNCBook *book;
    GNCPrice *price;
    GNCPriceDB *pdb;
    GNCCurrencyEdit *ce;
    Timespec ts;

    ce = GNC_CURRENCY_EDIT (info->price_currency_edit);

    ts.tv_sec = date;
    ts.tv_nsec = 0;

    price = gnc_price_create ();

    gnc_price_set_commodity (price, xaccAccountGetSecurity (account));
    gnc_price_set_currency (price, gnc_currency_edit_get_currency (ce));
    gnc_price_set_time (price, ts);
    gnc_price_set_source (price, "user:stock-split");
    gnc_price_set_type (price, "unknown");
    gnc_price_set_value (price, amount);

    book = gncGetCurrentBook ();
    pdb = gnc_book_get_pricedb (book);

    if (!gnc_pricedb_add_price (pdb, price))
      gnc_error_dialog_parented (GTK_WINDOW (info->window),
                                 _("Error adding price."));

    gnc_price_unref (price);
  }

  amount = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (info->cash_edit));
  if (gnc_numeric_positive_p (amount))
  {
    const char *memo;

    memo = gtk_entry_get_text (GTK_ENTRY (info->memo_entry));

    /* asset split */
    account = gnc_account_tree_get_current_account
      (GNC_ACCOUNT_TREE (info->asset_tree));

    split = xaccMallocSplit ();

    xaccAccountBeginEdit (account);
    account_commits = g_list_prepend (account_commits, account);

    xaccAccountInsertSplit (account, split);

    xaccTransAppendSplit (trans, split);

    xaccSplitSetShareAmount (split, amount);
    xaccSplitSetValue (split, amount);

    xaccSplitSetMemo (split, memo);


    /* income split */
    account = gnc_account_tree_get_current_account
      (GNC_ACCOUNT_TREE (info->income_tree));

    split = xaccMallocSplit ();

    xaccAccountBeginEdit (account);
    account_commits = g_list_prepend (account_commits, account);

    xaccAccountInsertSplit (account, split);

    xaccTransAppendSplit (trans, split);

    xaccSplitSetShareAmount (split, gnc_numeric_neg (amount));
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

static void
druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}

static gboolean
account_currency_filter (Account *account, gpointer user_data)
{
  StockSplitInfo *info = user_data;
  Account *split_account = xaccAccountLookup (&info->account);

  if (!account)
    return FALSE;

  return gnc_commodity_equiv (xaccAccountGetCurrency (split_account),
                              xaccAccountGetCurrency (account));
}

static void
gnc_stock_split_druid_create (StockSplitInfo *info)
{
  GtkWidget *page;

  info->window = create_Stock_Split_Druid ();

  info->druid = lookup_widget (info->window, "stock_split_druid");

  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->druid), "cancel",
                      GTK_SIGNAL_FUNC (druid_cancel), info);

  /* account list */
  {
    GtkCList *clist;

    info->account_list = lookup_widget (info->window, "account_clist");

    clist = GTK_CLIST (info->account_list);

    gtk_clist_set_selection_mode (clist, GTK_SELECTION_BROWSE);

    gtk_signal_connect (GTK_OBJECT (clist), "select_row",
                        GTK_SIGNAL_FUNC (clist_select_row), info);

    page = lookup_widget (info->window, "account_page");

    gtk_signal_connect (GTK_OBJECT (page), "next",
                        GTK_SIGNAL_FUNC (account_next), info);
  }

  /* info widgets */
  {
    GtkWidget *box;
    GtkWidget *amount;
    GtkWidget *date;
    GtkWidget *ce;

    info->description_entry =
      lookup_widget (info->window, "description_entry");

    box = lookup_widget (info->window, "date_box");
    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (box), date, TRUE, TRUE, 0);
    info->date_edit = date;

    box = lookup_widget (info->window, "distribution_box");
    amount = gnc_amount_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->distribution_edit = amount;

    box = lookup_widget (info->window, "price_box");
    amount = gnc_amount_edit_new ();
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (amount),
                                    gnc_default_price_print_info ());
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->price_edit = amount;

    box = lookup_widget (info->window, "price_currency_box");
    ce = gnc_currency_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), ce, TRUE, TRUE, 0);
    info->price_currency_edit = ce;

    page = lookup_widget (info->window, "details_page");

    gtk_signal_connect (GTK_OBJECT (page), "next",
                        GTK_SIGNAL_FUNC (details_next), info);
  }

  /* Cash in Lieu page */
  {
    AccountViewInfo view_info;
    GNCAccountType type;
    GtkWidget *box;
    GtkWidget *tree;
    GtkWidget *amount;
    GtkWidget *scroll;

    box = lookup_widget (info->window, "cash_box");
    amount = gnc_amount_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->cash_edit = amount;

    info->memo_entry = lookup_widget (info->window, "memo_entry");

    /* income tree */
    tree = gnc_account_tree_new ();
    info->income_tree = tree;
    gtk_clist_column_titles_hide (GTK_CLIST (tree));
    gtk_clist_set_selection_mode (GTK_CLIST (tree), GTK_SELECTION_BROWSE);
    gnc_account_tree_hide_all_but_name (GNC_ACCOUNT_TREE (tree));

    gnc_account_tree_get_view_info (GNC_ACCOUNT_TREE (tree), &view_info);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      view_info.include_type[type] = (type == INCOME);

    gnc_account_tree_set_view_info (GNC_ACCOUNT_TREE (tree), &view_info);

    gnc_account_tree_set_selectable_filter (GNC_ACCOUNT_TREE (tree),
                                            account_currency_filter,
                                            info);

    gtk_widget_show (tree);

    scroll = lookup_widget (info->window, "income_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), tree);


    /* asset tree */
    tree = gnc_account_tree_new ();
    info->asset_tree = tree;
    gtk_clist_column_titles_hide (GTK_CLIST (tree));
    gtk_clist_set_selection_mode (GTK_CLIST (tree), GTK_SELECTION_BROWSE);
    gnc_account_tree_hide_all_but_name (GNC_ACCOUNT_TREE (tree));

    gnc_account_tree_get_view_info (GNC_ACCOUNT_TREE (tree), &view_info);

    for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
      view_info.include_type[type] =
        (type == BANK) || (type == CASH) || (type == ASSET);

    gnc_account_tree_set_view_info (GNC_ACCOUNT_TREE (tree), &view_info);

    gnc_account_tree_set_selectable_filter (GNC_ACCOUNT_TREE (tree),
                                            account_currency_filter,
                                            info);

    gtk_widget_show (tree);

    scroll = lookup_widget (info->window, "asset_scroll");
    gtk_container_add (GTK_CONTAINER (scroll), tree);

    page = lookup_widget (info->window, "cash_page");

    gtk_signal_connect (GTK_OBJECT (page), "prepare",
                        GTK_SIGNAL_FUNC (cash_prepare), info);

    gtk_signal_connect (GTK_OBJECT (page), "next",
                        GTK_SIGNAL_FUNC (cash_next), info);
  }

  page = lookup_widget (info->window, "finish_page");

  gtk_signal_connect (GTK_OBJECT (page), "finish",
                      GTK_SIGNAL_FUNC (stock_split_finish), info);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  StockSplitInfo *info = user_data;
  Account *old_account;
  Account *new_account;
  GNCIdType id_type;
  GtkWidget *page;

  id_type = xaccGUIDType (&info->account);
  old_account = xaccAccountLookup (&info->account);

  if (fill_account_list (info, old_account) == 0)
  {
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  new_account = xaccAccountLookup (&info->account);

  if (id_type == GNC_ID_NULL || old_account == new_account)
    return;

  page = lookup_widget (info->window, "account_page");

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
 * Args:   initial - the initial account to use                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_stock_split_dialog (Account * initial)
{
  StockSplitInfo *info;
  gint component_id;

  info = g_new0 (StockSplitInfo, 1);

  info->account = *xaccGUIDNULL ();

  gnc_stock_split_druid_create (info);

  component_id = gnc_register_gui_component (DRUID_STOCK_SPLIT_CM_CLASS,
                                             refresh_handler, close_handler,
                                             info);

  gnc_gui_component_watch_entity_type (component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  if (fill_account_list (info, initial) == 0)
  {
    gnc_warning_dialog (_("You don't have any stock accounts with balances!"));
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

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

  /* account page data */
  GtkWidget * account_list;
  GUID account;

  /* info page data */
  GtkWidget * date_edit;
  GtkWidget * distribution_edit;
  GtkWidget * price_edit;

  /* cash in lieu page data */
  GtkWidget * cash_edit;
  GtkWidget * description_entry;
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

  return FALSE;
}

static void
druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}


static void
gnc_stock_split_druid_create (StockSplitInfo *info)
{
  GtkWidget *druid;

  info->window = create_Stock_Split_Druid ();

  druid = lookup_widget (info->window, "stock_split_druid");

  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (druid), "cancel",
                      GTK_SIGNAL_FUNC (druid_cancel), info);

  /* account list */
  {
    GtkCList *clist;
    GtkWidget *page;

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
    GtkWidget *page;
    GtkWidget *date;

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
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->price_edit = amount;

    page = lookup_widget (info->window, "details_page");

    gtk_signal_connect (GTK_OBJECT (page), "next",
                        GTK_SIGNAL_FUNC (details_next), info);
  }

  /* Cash in Lieu page */
  {
    GtkWidget *box;
    GtkWidget *amount;

    box = lookup_widget (info->window, "cash_box");
    amount = gnc_amount_edit_new ();
    gtk_box_pack_start (GTK_BOX (box), amount, TRUE, TRUE, 0);
    info->cash_edit = amount;

    info->description_entry = lookup_widget (info->window,
                                             "description_entry");
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  if (fill_account_list (info, xaccAccountLookup (&info->account)) == 0)
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
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
    gnc_warning_dialog (_("You don't have any stock accounts!"));
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

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
  GtkWidget * account_entry;
  GtkWidget * numerator_spin;
  GtkWidget * denominator_spin;
  GtkWidget * starting_entry;
  GtkWidget * ending_edit;
} StockSplitInfo;


/** declarations *******************************************************/
static void set_ending_from_spinners (StockSplitInfo *info);


/** implementations ****************************************************/
static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  StockSplitInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);

  g_free (info);
}

static int
fill_account_list (StockSplitInfo *info)
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
    gnc_numeric balance;
    char *strings[4];
    gint row;

    if (xaccAccountGetType (account) != STOCK)
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
    Account *account = xaccAccountLookup (&info->account);
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
  gnc_numeric amount;
  Account *account;
  const char *amount_str;
  char *name;

  account = xaccAccountLookup (&info->account);

  g_return_if_fail (account != NULL);

  name = xaccAccountGetFullName (account, gnc_get_account_separator ());
  gtk_entry_set_text (GTK_ENTRY (info->account_entry), name);
  g_free (name);

  amount = xaccAccountGetShareBalance (account);
  print_info = gnc_account_quantity_print_info (account, FALSE);

  amount_str = xaccPrintAmount(amount, print_info);
  gtk_entry_set_text (GTK_ENTRY (info->starting_entry), amount_str);

  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (info->ending_edit),
                                  print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (info->ending_edit),
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

  set_ending_from_spinners (info);

  return FALSE;
}

static void
druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}

static void
set_ending_from_spinners (StockSplitInfo *info)
{
  GtkSpinButton *spin;
  gnc_numeric starting;
  gnc_numeric ending;
  gint numerator;
  gint denominator;
  Account *account;

  account = xaccAccountLookup (&info->account);

  starting = xaccAccountGetShareBalance (account);

  spin = GTK_SPIN_BUTTON (info->numerator_spin);
  numerator = gtk_spin_button_get_value_as_int (spin);

  spin = GTK_SPIN_BUTTON (info->denominator_spin);
  denominator = gtk_spin_button_get_value_as_int (spin);

  if (numerator > 0 && denominator > 0)
  {
    gnc_numeric ratio;
    int scu;

    ratio = gnc_numeric_create (numerator, denominator);

    scu = xaccAccountGetSecuritySCU (account);

    ending = gnc_numeric_mul (starting, ratio, scu, GNC_RND_ROUND);
  }
  else
    ending = starting;

  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (info->ending_edit), ending);
}

static void
spin_changed (GtkEditable *editable, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  set_ending_from_spinners (info);
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

    info->account_entry    = lookup_widget (info->window, "account_entry");
    info->numerator_spin   = lookup_widget (info->window, "numerator_spin");
    info->denominator_spin = lookup_widget (info->window, "denominator_spin");
    info->starting_entry   = lookup_widget (info->window, "starting_entry");

    box = lookup_widget (info->window, "ending_hbox");
    amount = gnc_amount_edit_new();
    gtk_box_pack_end (GTK_BOX(box), amount, TRUE, TRUE, 0);

    info->ending_edit = amount;

    page = lookup_widget (info->window, "details_page");

    gtk_signal_connect (GTK_OBJECT (info->numerator_spin), "changed",
                        GTK_SIGNAL_FUNC (spin_changed), info);
    gtk_signal_connect (GTK_OBJECT (info->denominator_spin), "changed",
                        GTK_SIGNAL_FUNC (spin_changed), info);
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  if (fill_account_list (info) == 0)
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

  if (fill_account_list (info) == 0)
  {
    gnc_warning_dialog (_("You don't have any stock accounts!"));
    gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
    return;
  }

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

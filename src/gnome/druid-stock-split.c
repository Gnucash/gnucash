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
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "messages.h"


#define DRUID_STOCK_SPLIT_CM_CLASS "druid-stock-split"


typedef struct
{
  GtkWidget * window;
  GtkWidget * account_list;

  /* account page data */
  GUID account;


} StockSplitInfo;


static int
window_destroy_cb (GtkObject *object, gpointer data)
{
  StockSplitInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);

  g_free (info);

  return 1;
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
    char *strings[4];
    gint row;

    if (xaccAccountGetType (account) != STOCK)
      continue;

    security = xaccAccountGetSecurity (account);

    print_info = gnc_account_quantity_print_info (account, FALSE);

    strings[0] = xaccAccountGetFullName (account,
                                         gnc_get_account_separator ());
    strings[1] = (char *) gnc_commodity_get_mnemonic (security);
    strings[2] = (char *)
      xaccPrintAmount (xaccAccountGetShareBalance (account), print_info);
    strings[3] = NULL;

    row = gtk_clist_append (clist, strings);

    gtk_clist_set_row_data (clist, row, account);

    g_free (strings[0]);

    rows++;
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
druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  StockSplitInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);
}

static void
gnc_stock_split_druid_create (StockSplitInfo *info)
{
  info->window = create_Stock_Split_Druid ();

  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->window), "cancel",
                      GTK_SIGNAL_FUNC (druid_cancel), info);

  /* account list */
  {
    GtkCList *clist;

    info->account_list = lookup_widget (info->window, "account_clist");

    clist = GTK_CLIST (info->account_list);

    gtk_clist_set_selection_mode (clist, GTK_SELECTION_BROWSE);

    gtk_signal_connect (GTK_OBJECT (clist), "select_row",
                        GTK_SIGNAL_FUNC (clist_select_row), info);

    fill_account_list (info);
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

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

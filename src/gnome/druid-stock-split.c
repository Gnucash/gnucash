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

#include "Account.h"
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
} StockSplitInfo;


static int
window_destroy_cb (GtkObject *object, gpointer data)
{
  StockSplitInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_STOCK_SPLIT_CM_CLASS, info);

  g_free (info);
}

static void
gnc_stock_split_druid_create (StockSplitInfo *info)
{
  info->window = create_Stock_Split_Druid ();

  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), info);

  /* account list */
  info->account_list = lookup_widget (info->window, "account_clist");
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

  info = g_new0 (StockSplitInfo, 1);

  gnc_stock_split_druid_create (info);

  gnc_register_gui_component (DRUID_STOCK_SPLIT_CM_CLASS,
                              NULL, close_handler, info);

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

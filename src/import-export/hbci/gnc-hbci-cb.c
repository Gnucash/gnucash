/********************************************************************\
 * gnc-hbci-cb.c -- hbci callback functions                         *
 * Copyright (C) 2002 Christian Stimming                            *
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

#include "gnc-hbci-cb.h"

#include "Account.h"
#include "gnc-ui.h"
#include "window-acct-tree.h"

#include "gnc-hbci-actions.h"

void
gnc_hbci_acct_tree_menu_getbalance_cb (GtkWidget * widget, 
                                       GnomeMDIChild * child)
{
  GNCMDIChildInfo * mc = gtk_object_get_user_data(GTK_OBJECT(child));
  GNCAcctTreeWin   * win = mc->user_data;
  Account        * account = gnc_acct_tree_window_get_current_account(win);
  
  gnc_hbci_getbalance (gnc_ui_get_toplevel (), account);
}

void
gnc_hbci_acct_tree_menu_gettrans_cb (GtkWidget * widget, 
				     GnomeMDIChild * child)
{
  /*GNCMDIChildInfo * mc = gtk_object_get_user_data(GTK_OBJECT(child));
  GNCAcctTreeWin   * win = mc->user_data;
  Account        * account = gnc_acct_tree_window_get_current_account(win);*/
  
  gnc_warning_dialog_parented(gnc_ui_get_toplevel (), 
			      "Sorry, Transaction retrieval not yet implemented.");
  
}


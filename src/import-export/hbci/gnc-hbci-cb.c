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
#include "window-register.h"

#include "gnc-hbci-actions.h"
#include "gnc-hbci-getbalance.h"
#include "gnc-hbci-gettrans.h"

void
gnc_hbci_acct_tree_menu_getbalance_cb (GtkWidget * widget, 
                                       GnomeMDIChild * child)
{
  GNCMDIChildInfo * mc = NULL;
  GNCAcctTreeWin   * win = NULL;
  Account        * account = NULL;

  g_assert (child);
  mc = gtk_object_get_user_data (GTK_OBJECT (child));
  g_assert (mc);
  win = mc->user_data;
  g_assert (win);
  account = gnc_acct_tree_window_get_current_account (win);
  g_assert (account);
    
  gnc_hbci_getbalance (gnc_acct_tree_window_get_widget (win),
		       account);
}

void
gnc_hbci_acct_tree_menu_gettrans_cb (GtkWidget * widget, 
				     GnomeMDIChild * child)
{
  GNCMDIChildInfo * mc = NULL;
  GNCAcctTreeWin   * win = NULL;
  Account        * account = NULL;

  g_assert (child);
  mc = gtk_object_get_user_data (GTK_OBJECT (child));
  g_assert (mc);
  win = mc->user_data;
  g_assert (win);
  account = gnc_acct_tree_window_get_current_account (win);
  g_assert (account);
    
  gnc_hbci_maketrans (gnc_acct_tree_window_get_widget (win),
		      account);
}

void
gnc_hbci_register_menu_getbalance_cb (GtkWidget * widget, 
				      gpointer data)
{
  RegWindow *regData = data;
  GNCLedgerDisplay *ledger = NULL;
  Account *account = NULL;

  g_assert (regData);
  ledger = gnc_RegWindow_ledger (regData);
  g_assert (ledger);
  account = gnc_ledger_display_leader (ledger);
  if (!account)
      return;
      
  gnc_hbci_getbalance (gnc_RegWindow_window (regData), account);
}

void
gnc_hbci_register_menu_gettrans_cb (GtkWidget * widget, 
				    gpointer data)
{
  RegWindow *regData = data;
  GNCLedgerDisplay *ledger = NULL;
  Account *account = NULL;

  g_assert (regData);
  ledger = gnc_RegWindow_ledger (regData);
  g_assert (ledger);
  account = gnc_ledger_display_leader (ledger);
  if (!account)
      return;
      
  gnc_hbci_gettrans (gnc_RegWindow_window (regData), account);
}

void
gnc_hbci_register_menu_maketrans_cb (GtkWidget * widget, 
				     gpointer data)
{
  RegWindow *regData = data;
  GNCLedgerDisplay *ledger = NULL;
  Account *account = NULL;

  g_assert (regData);
  ledger = gnc_RegWindow_ledger (regData);
  g_assert (ledger);
  account = gnc_ledger_display_leader (ledger);
  if (!account)
      return;
    
  gnc_hbci_maketrans (gnc_RegWindow_window (regData), account);
}

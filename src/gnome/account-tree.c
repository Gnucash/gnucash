/********************************************************************\
 * account-tree.c -- The tree of accounts in the main window, and   *
 *                   associated helper and callback functions for   *
 *                   GnuCash.                                       *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <gnome.h>

#include "config.h"

#include "top-level.h"
#include "gnucash.h"
#include "messages.h"
#include "AccWindow.h"
#include "FileDialog.h"
#include "account-tree.h"
#include "account-treeP.h"
#include "util.h"


/********************************************************************\
 * gnc_create_account_tree                                          *
 *   creates the account tree                                       *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/
GtkWidget *
gnc_create_account_tree()
{
  GtkWidget *ctree;
  gchar     *ctitles[] = {ACC_NAME_STR,
			  DESC_STR,
			  BALN_STR,
			  NULL};
  gint num_titles = 0;

  while (ctitles[num_titles] != NULL)
    num_titles++;

  /* Create ctree */
  ctree = gtk_ctree_new_with_titles(num_titles, 0, ctitles);

  gtk_clist_set_shadow_type (GTK_CLIST(ctree), GTK_SHADOW_IN);

  {
    GtkStyle *st = gtk_widget_get_style(GTK_WIDGET(ctree));
    GdkFont *font = NULL;
    gint width;
    gint i;

    if (st != NULL)
      font = st->font;

    if (font != NULL)
      for (i = 0; i < num_titles; i++)
      {
	width = gdk_string_width(font, ctitles[i]);
	gtk_clist_set_column_min_width(GTK_CLIST(ctree), i, width + 5);
      }
  }

  gtk_signal_connect (GTK_OBJECT(ctree),
                      "button_press_event",
                      GTK_SIGNAL_FUNC(gnc_ctree_button_press_cb),
                      NULL);

  gtk_signal_connect (GTK_OBJECT(ctree), 
		      "tree_select_row",
		      GTK_SIGNAL_FUNC(gnc_acct_ctree_select_cb),
		      NULL);

  gtk_signal_connect (GTK_OBJECT(ctree), 
		      "tree_unselect_row",
		      GTK_SIGNAL_FUNC(gnc_acct_ctree_unselect_cb),
		      NULL);

  gtk_object_set_data(GTK_OBJECT(gnc_get_ui_data()), "ctree", ctree);

  return ctree;
}

/********************************************************************\
 * gnc_ui_refresh_tree                                              *
 *   refreshes the account tree                                     *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_ui_refresh_tree() 
{
  GtkCTree     *ctree;
  AccountGroup *accts;
  
  ctree  = gtk_object_get_data(GTK_OBJECT(gnc_get_ui_data()),
			       "ctree");

  accts = gncGetCurrentGroup();

  gtk_clist_freeze(GTK_CLIST(ctree));

  gtk_clist_clear(GTK_CLIST(ctree));

  gnc_ui_acct_ctree_fill(ctree, NULL, accts);

  gtk_clist_thaw(GTK_CLIST(ctree));

  gtk_clist_columns_autosize(GTK_CLIST(ctree));  
}

static gint
gnc_ctree_button_press_cb(GtkWidget *widget, GdkEventButton *event)
{
  GtkCTree *ctree = GTK_CTREE(widget);
  GtkCList *clist = GTK_CLIST(widget);
  GtkCTreeNode *node;
  Account *account, *old_acct;
  gint x, y, row, column;
	
  if (event->window == clist->clist_window) {
    x = event->x;
    y = event->y;

    if (!gtk_clist_get_selection_info(clist, x, y, &row, &column))
      return FALSE;

    node = gtk_ctree_node_nth (ctree, row);
    account = gtk_ctree_node_get_row_data(ctree, node);
		
    if (event->type == GDK_2BUTTON_PRESS) {
      /* so the the button_release will leave it selected */
      gtk_ctree_unselect (ctree, node);
      /* this will stop the node from being collapsed/expanded */
      gtk_signal_emit_stop_by_name (GTK_OBJECT(widget), "button_press_event");
      regWindowSimple ( account );
      return TRUE;
    }
  }
  return FALSE;
}

static gint
gnc_acct_ctree_select_cb(GtkWidget *widget, GtkCTreeNode *row,
			 gint column, gpointer user_data)
{
  Account *account;
  
  account = (Account *)gtk_ctree_node_get_row_data(GTK_CTREE(widget),
						   GTK_CTREE_NODE(row));

  gtk_object_set_data(GTK_OBJECT(gnc_get_ui_data()),
		      "selected_account", account);
  
  return TRUE;
}

static gint
gnc_acct_ctree_unselect_cb(GtkWidget *widget, GtkCTreeNode *row,
			   gint column, gpointer user_data)
{
  gtk_object_set_data(GTK_OBJECT(gnc_get_ui_data()),
		      "selected_account", NULL);
  
  return TRUE; 
}

static void
gnc_ui_acct_ctree_fill(GtkCTree *ctree, GtkCTreeNode *parent,
		       AccountGroup *accts)
{
  Account *acc;
  AccountGroup *acc_children;
  GtkCTreeNode *sibling = NULL;
  gint totalAccounts = xaccGroupGetNumAccounts(accts);
  gint currentAccount;
  
  /* Add each account to the tree */  
  for ( currentAccount = 0;
        currentAccount < totalAccounts;
        currentAccount++ )
  {
    acc = xaccGroupGetAccount(accts, currentAccount);
    acc_children = xaccAccountGetChildren (acc);

    gnc_tree_insert_row(ctree, parent, &sibling, acc);

    /* If this account has children,
     * then we need to build a subtree and fill it.
     */
    if(acc_children)
    {
      /* Call gnc_ui_accWindow_tree_fill to fill this new subtree */
      gnc_ui_acct_ctree_fill(ctree, sibling, acc_children );  
    }
  }
}

static void
gnc_tree_insert_row (GtkCTree *ctree, GtkCTreeNode *parent,
		     GtkCTreeNode **sibling, Account *acc)
{
  AccountGroup *acc_children = xaccAccountGetChildren (acc);
  int acc_type = xaccAccountGetType (acc);
  double dbalance;
  gchar *text[3];

  /* fill in the balance column */
  dbalance = xaccAccountGetBalance (acc);

  /* if the account has children, add in thier balance */
  if (acc_children)
    dbalance += xaccGroupGetBalance (acc_children);

  /* the meaning of "balance" for income and expense
   * accounts is reversed, since a deposit of a paycheck in a
   * bank account will appear as a debit of the corresponding
   * amount in the income account */
  if ((EXPENSE == acc_type) || (INCOME  == acc_type))
    dbalance = -dbalance;

  text[0] = xaccAccountGetName(acc);
  text[1] = xaccAccountGetDescription(acc);
  text[2] = xaccPrintAmount (dbalance, PRTSYM | PRTSEP);
    
  *sibling = gtk_ctree_insert_node (ctree, parent, *sibling, text, 0,
				    NULL, NULL, NULL, NULL,
				    FALSE, FALSE);
				           
  /* Set the user_data for the tree item to the account it */
  /* represents.                                           */
  gtk_ctree_node_set_row_data(GTK_CTREE(ctree), *sibling, acc);
}

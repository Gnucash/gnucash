/*******************************************************************\
 * account-tree.h -- private GNOME account tree functions           *
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

#ifndef __ACCOUNT_TREEP_H__
#define __ACCOUNT_TREEP_H__

/** PROTOTYPES ******************************************************/
static gint gnc_ctree_button_press_cb(GtkWidget *widget,
				      GdkEventButton *event);

static gint gnc_acct_ctree_select_cb(GtkWidget *widget,
				     GtkCTreeNode *row,
				     gint column,
				     gpointer user_data);

static gint gnc_acct_ctree_unselect_cb(GtkWidget *widget,
				       GtkCTreeNode *row,
				       gint column,
				       gpointer user_data);

static void gnc_tree_insert_row (GtkCTree *ctree,
				 GtkCTreeNode *parent,
				 GtkCTreeNode **sibling,
				 Account *acc);

static void gnc_ui_acct_ctree_fill(GtkCTree *ctree,
				   GtkCTreeNode *parent,
				   AccountGroup *accts);

#endif

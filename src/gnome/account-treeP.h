/********************************************************************\
 * account-treeP.h -- private GNOME account tree functions          *
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

#include "account-tree.h"


enum
{
  SELECT_ACCOUNT,
  UNSELECT_ACCOUNT,
  DOUBLE_CLICK_ACCOUNT,
  LAST_SIGNAL
};


/** PROTOTYPES ******************************************************/
static void gnc_account_tree_init(GNCAccountTree *tree);

static void gnc_account_tree_class_init(GNCAccountTreeClass *klass);

static gint gnc_account_tree_button_press(GtkWidget *widget,
					  GdkEventButton *event);

static void gnc_account_tree_select_row(GtkCTree *ctree,
					GtkCTreeNode *row,
					gint column);

static void gnc_account_tree_unselect_row(GtkCTree *ctree,
					  GtkCTreeNode *row,
					  gint column);

static GtkCTreeNode * gnc_account_tree_insert_row(GNCAccountTree *tree,
						  GtkCTreeNode *parent,
						  GtkCTreeNode *sibling,
						  Account *acc);

static void gnc_account_tree_fill(GNCAccountTree *tree,
				  GtkCTreeNode *parent,
				  AccountGroup *accts);

static void gnc_account_tree_set_view_info_real(GNCAccountTree *tree);

static void gnc_account_tree_update_column_visibility (GNCAccountTree *tree);

static void gnc_account_tree_destroy(GtkObject *object);

#endif

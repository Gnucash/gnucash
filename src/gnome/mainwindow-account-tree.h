/********************************************************************\
 * mainwindow-account-tree.h -- composite account selection widget  *
 *                              wrapped up for the main window      * 
 *                  and callback functions for GnuCash              *
 * Copyright (C) 2000 Gnumatic, Inc.                                *
 * Written by Robert Merkel <rgmerk@mira.net>                       *
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

#ifndef GNC_MAINWIN_ACCOUNT_TREE_H
#define GNC_MAINWIN_ACCOUNT_TREE_H

#include <gnome.h>

#include "gnc-account-tree.h"
#include "Account.h"

#define GNC_TYPE_MAINWIN_ACCOUNT_TREE		(gnc_mainwin_account_tree_get_type ())
#define GNC_MAINWIN_ACCOUNT_TREE(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_MAINWIN_ACCOUNT_TREE, GNCMainWinAccountTree))
#define GNC_MAINWIN_ACCOUNT_TREE_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_MAINWIN_ACCOUNT_TREE, GNCMainWinAccountTreeClass))
#define GNC_IS_MAINWIN_ACCOUNT_TREE(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MAINWIN_ACCOUNT_TREE))

typedef struct  { 
	GtkVBox vbox;
	GtkScrolledWindow *scrolled_window;
	GNCAccountTree *acc_tree; 
} GNCMainWinAccountTree;

typedef struct {
	GtkVBoxClass parent_class;  

	void (*select_account)   (GNCMainWinAccountTree *tree,
		       		  Account        *account);

	void (*unselect_account) (GNCMainWinAccountTree *tree,
				  Account        *account);

	void (*activate_account) (GNCMainWinAccountTree *tree,
		       		  Account        *account);
} GNCMainWinAccountTreeClass;

GType          gnc_mainwin_account_tree_get_type (void);
GtkWidget*     gnc_mainwin_account_tree_new (void);

GtkWidget* 
gnc_mainwin_account_tree_attach_popup(GNCMainWinAccountTree *tree,
                                      GnomeUIInfo *popup_info,
                                      gpointer user_data);

void
gnc_mainwin_account_tree_set_view_info(GNCMainWinAccountTree *tree,
                                       AccountViewInfo new_info);
Account *
gnc_mainwin_account_tree_get_current_account(GNCMainWinAccountTree *tree);
GList *
gnc_mainwin_account_tree_get_current_accounts(GNCMainWinAccountTree *tree);

void
gnc_mainwin_account_tree_toggle_account_expansion (GNCMainWinAccountTree *tree,
                                                   Account *account);

#endif /* __GNC_MAINWINDOW_ACCOUNT_TREE_H */

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

#ifndef __GNC_MAINWIN_ACCOUNT_TREE_H
#define __GNC_MAINWIN_ACCOUNT_TREE_H

#include <gnome.h>
#include "account-tree.h"
#include "Account.h"

#define GNC_MAINWIN_ACCOUNT_TREE(obj)          GTK_CHECK_CAST (obj, gnc_mainwin_account_tree_get_type (), GNCMainWinAccountTree)
#define GNC_MAINWIN_ACCOUNT_TREE_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_mainwin_account_tree_get_type(), GNCMainWinAccountTreeClass)
#define IS_GNC_MAINWIN_ACCOUNT_TREE(obj)       GTK_CHECK_TYPE (obj, gnc_mainwin_account_tree_get_type ())


typedef struct _GNCMainWinAccountTree       GNCMainWinAccountTree;
typedef struct _GNCMainWinAccountTreeClass  GNCMainWinAccountTreeClass;

struct _GNCMainWinAccountTree
{ 
  GtkVBox vbox;
  GtkScrolledWindow *scrolled_window;
  GNCAccountTree *acc_tree; 
};

struct _GNCMainWinAccountTreeClass
{
  GtkVBoxClass parent_class;  

  void (*select_account)   (GNCMainWinAccountTree *tree,
                            Account        *account);

  void (*unselect_account) (GNCMainWinAccountTree *tree,
                            Account        *account);

  void (*activate_account) (GNCMainWinAccountTree *tree,
                            Account        *account);
};

guint          gnc_mainwin_account_tree_get_type(void);
GtkWidget*     gnc_mainwin_account_tree_new(void);

void 
gnc_mainwin_account_tree_attach_popup(GNCMainWinAccountTree *tree, GnomeUIInfo *popup_info);

void
gnc_mainwin_account_tree_set_view_info(GNCMainWinAccountTree *tree, AccountViewInfo new_info);
Account *
gnc_mainwin_account_tree_get_current_account(GNCMainWinAccountTree *tree);
GList *
gnc_mainwin_account_tree_get_current_accounts(GNCMainWinAccountTree *tree);

void gnc_mainwin_account_tree_toggle_account_expansion(GNCMainWinAccountTree *tree, Account *account);

#endif /* __GNC_MAINWINDOW_ACCOUNT_TREE_H */

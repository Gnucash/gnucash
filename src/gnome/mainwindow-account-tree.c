/********************************************************************\
 * mainwindow-account-list.c -- composite account selection widget  *
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

#include "config.h"

#include <gnome.h>
#include "mainwindow-account-tree.h"


static GList *mw_acc_trees = NULL;

enum {
      SELECT_ACCOUNT_SIGNAL,
      UNSELECT_ACCOUNT_SIGNAL,
      ACTIVATE_ACCOUNT_SIGNAL,
      LAST_SIGNAL
    };

static gint mainwinaccounttree_signals[LAST_SIGNAL] = { 0 };

static void
unselect_account_callback(GNCAccountTree *tree, Account *account, gpointer user_data)
{
  GNCMainWinAccountTree *mw_tree = (GNCMainWinAccountTree *) user_data;
  gtk_signal_emit(GTK_OBJECT(mw_tree),
		  mainwinaccounttree_signals[UNSELECT_ACCOUNT_SIGNAL],
		  account);
  return;
}

static void
activate_account_callback(GNCAccountTree *tree, Account *account, gpointer user_data)
{  
  GNCMainWinAccountTree *mw_tree = (GNCMainWinAccountTree *) user_data;
  gtk_signal_emit(GTK_OBJECT(mw_tree),
		  mainwinaccounttree_signals[ACTIVATE_ACCOUNT_SIGNAL],
		  account);
  return;
}

static void
select_account_callback(GNCAccountTree  *tree, Account *account, gpointer user_data)
{
  GNCMainWinAccountTree *clicked_window = (GNCMainWinAccountTree *) user_data;
  GList *list_iterator;
  GNCMainWinAccountTree *current_window;
  for(list_iterator = mw_acc_trees; list_iterator; list_iterator=g_list_next(list_iterator))
  {
    current_window = list_iterator->data;
    if(current_window != clicked_window)
    {
      gnc_account_tree_unselect_account(current_window->acc_tree,
				       gnc_account_tree_get_current_account(current_window->acc_tree));
    }
  }

  gtk_signal_emit(GTK_OBJECT(clicked_window),
		  mainwinaccounttree_signals[SELECT_ACCOUNT_SIGNAL],
		  account);

  return;
}

static void
gnc_mainwin_account_tree_class_init (GNCMainWinAccountTreeClass *klass)
{
  GtkObjectClass *object_class;
  
  object_class = (GtkObjectClass*) klass;
      
  
  
  mainwinaccounttree_signals[SELECT_ACCOUNT_SIGNAL] = 
    gtk_signal_new("select_account",
		   GTK_RUN_FIRST,
		   object_class->type,
		   GTK_SIGNAL_OFFSET(GNCMainWinAccountTreeClass,
				     select_account),
		   gtk_marshal_NONE__POINTER,
		   GTK_TYPE_NONE, 1,
		   GTK_TYPE_POINTER);

  mainwinaccounttree_signals[UNSELECT_ACCOUNT_SIGNAL] =
    gtk_signal_new("unselect_account",
		   GTK_RUN_FIRST,
		   object_class->type,
		   GTK_SIGNAL_OFFSET(GNCMainWinAccountTreeClass,
				     unselect_account),
		   gtk_marshal_NONE__POINTER,
		   GTK_TYPE_NONE, 1,
		   GTK_TYPE_POINTER);

  mainwinaccounttree_signals[ACTIVATE_ACCOUNT_SIGNAL] =
    gtk_signal_new("activate_account",
		   GTK_RUN_FIRST,
		   object_class->type,
		   GTK_SIGNAL_OFFSET(GNCMainWinAccountTreeClass,
				     activate_account),
		   gtk_marshal_NONE__POINTER,
		   GTK_TYPE_NONE, 1,
		   GTK_TYPE_POINTER);

  gtk_object_class_add_signals(object_class,
			       mainwinaccounttree_signals,
			       LAST_SIGNAL);

  klass->select_account   = NULL;
  klass->unselect_account = NULL;
  klass->activate_account = NULL;
}
static void
gnc_mainwin_account_tree_init(GNCMainWinAccountTree *mwac_tree)
{
  mwac_tree->acc_tree = GNC_ACCOUNT_TREE(gnc_account_tree_new());
  mwac_tree->scrolled_window = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new(NULL, NULL));

  gtk_scrolled_window_set_policy (mwac_tree->scrolled_window,
                                  GTK_POLICY_AUTOMATIC,
                                  GTK_POLICY_AUTOMATIC);

  gtk_signal_connect(GTK_OBJECT(mwac_tree->acc_tree), "activate_account",
		     GTK_SIGNAL_FUNC (activate_account_callback), mwac_tree);

  gtk_signal_connect(GTK_OBJECT(mwac_tree->acc_tree), "select_account",
                     GTK_SIGNAL_FUNC(select_account_callback), mwac_tree);

  gtk_signal_connect(GTK_OBJECT(mwac_tree->acc_tree), "unselect_account",
                     GTK_SIGNAL_FUNC(unselect_account_callback), mwac_tree);
 
  gtk_container_add(GTK_CONTAINER(mwac_tree->scrolled_window), GTK_WIDGET(mwac_tree->acc_tree));

  gtk_box_pack_start(GTK_BOX(mwac_tree), GTK_WIDGET(mwac_tree->scrolled_window), TRUE, TRUE, 0);
  gtk_widget_show(GTK_WIDGET(mwac_tree->acc_tree));
  gtk_widget_show(GTK_WIDGET(mwac_tree->scrolled_window));
}

guint
gnc_mainwin_account_tree_get_type ()
{
  static guint mwactree = 0;
  
  if (!mwactree)
    {
      static const GtkTypeInfo mwactree_info =
      {
	"GNCMainWinAccountTree",
	sizeof (GNCMainWinAccountTree),
	sizeof (GNCMainWinAccountTreeClass),
	(GtkClassInitFunc) gnc_mainwin_account_tree_class_init,
	(GtkObjectInitFunc) gnc_mainwin_account_tree_init,
	(GtkArgSetFunc) NULL,
	(GtkArgGetFunc) NULL,
	(GtkClassInitFunc) NULL
      };
      
      mwactree = gtk_type_unique (gtk_hbox_get_type (), &mwactree_info);
    }
  
  return mwactree;
}


/*
 * Note that the interface was chosen purely because it's the calls needed to
 * get this working with the existing window-main.c
 * This is all subject to change
 */

/*******************************************************************************\
 * gnc_mainwin_account_tree_attach_popup                                       *
 *   attaches a popup window                                                   *
 *                                                                             *
 * Args: mwac_trec - the mainwindow account tree to attach to                  *
 *       popup_info - the popup to attach                                      *
 * Returns: Nothing                                                            *
\*******************************************************************************/

void 
gnc_mainwin_account_tree_attach_popup(GNCMainWinAccountTree *mwac_tree, GnomeUIInfo *popup_info)
{
  GtkWidget *popup = gnome_popup_menu_new(popup_info);
  gnome_popup_menu_attach(popup, GTK_WIDGET(mwac_tree->acc_tree), NULL);
  return;
}

/*******************************************************************************\
 * gnc_mainwin_account_tree_set_view_info                                      *
 *   set which accounts get viewed                                             *
 *                                                                             *
 * Args: mwac_tree - the mainwindow account tree to attach to                  *
 *       new_info - the new view info (see account-tree.c for more details)    *
 * Returns: Nothing                                                            *
\*******************************************************************************/

void
gnc_mainwin_account_tree_set_view_info(GNCMainWinAccountTree *mwac_tree, AccountViewInfo new_info)
{
  AccountViewInfo old_info;
  gnc_account_tree_get_view_info(mwac_tree->acc_tree, &old_info);
  
  if(memcmp(&new_info, &old_info, sizeof(AccountViewInfo)) != 0)
  {
    gnc_account_tree_set_view_info(mwac_tree->acc_tree, &new_info);
  }
  
  return;
}

/*******************************************************************************\
 * gnc_mainwin_account_tree_new                                                *
 *   create a new mainwindow_account_tree                                      *
 *                                                                             *
 * Args: nothing                                                               *
 * Returns : a new fresh-baked mainwindow_account_tree                         *
\*******************************************************************************/

GtkWidget *
gnc_mainwin_account_tree_new()
{
  GtkWidget *tree;
  guint type_of;
  type_of = gnc_mainwin_account_tree_get_type();
  tree = gtk_widget_new(type_of, NULL);

  return tree;
}

/*******************************************************************************\
 * gnc_mainwin_account_tree_get_current_acount                                 *
 *   get the current account selected in a mainwindow_account_tree             *
 *                                                                             *
 * Args: the account tree                                                      *
 * Returns : a pointer to the selected account                                 *
\*******************************************************************************/
Account *
gnc_mainwin_account_tree_get_current_account(GNCMainWinAccountTree *list)
{
  return gnc_account_tree_get_current_account(list->acc_tree);
}
/*******************************************************************************\
 * gnc_mainwin_account_tree_get_current_accounts                               *
 *   get the current account(s) selected in a mainwindow_account_tree          *
 *                                                                             *
 * Args: the account tree                                                      *
 * Returns : a Glist of selected accounts                                      *
\*******************************************************************************/

GList *
gnc_mainwin_account_tree_get_current_accounts(GNCMainWinAccountTree *list)
{
  return gnc_account_tree_get_current_accounts(list->acc_tree);
}

/*******************************************************************************\
 * gnc_mainwin_account_tree_toggle_account_expansion                           *
 *   toggle expans the nominated account to show children (or not)             *
 *                                                                             *
 * Args: the account tree                                                      *
 *       the account whose children to show/not show                           *
 * Returns : nothing                                                           *
\*******************************************************************************/

void 
gnc_mainwin_account_tree_toggle_account_expansion(
  GNCMainWinAccountTree *mwac_tree, Account *account)
{
  GNCAccountTree *tree = mwac_tree->acc_tree;
  gnc_account_tree_toggle_account_expansion(tree, account);
  return;
}


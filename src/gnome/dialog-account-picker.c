/********************************************************************\
 * dialog-account-picker.c -- window for picking a Gnucash account  *
 *                           (GnuCash)                              *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
#include <stdio.h>

#include "dialog-account-picker.h"

#include <guile/gh.h>

#include "FileDialog.h"
#include "Group.h"
#include "Account.h"

#include "dialog-utils.h"
#include "query-user.h"


static void
build_acct_tree(AccountGroup * group, GtkWidget * tree, GtkWidget * picker) {
  GList        * accts;
  GList        * node;
  AccountGroup * children;
  GtkWidget    * tree_item;
  GtkWidget    * sub_tree;
  
  accts = xaccGroupGetSubAccounts(group);

  for (node = accts; node; node = node->next) {
    Account *account = node->data;

    if(group == xaccAccountGetParent(account)) {
      tree_item = gtk_tree_item_new_with_label(xaccAccountGetName(account));

      gtk_object_set_user_data(GTK_OBJECT(tree_item), account);

      gtk_tree_append(GTK_TREE(tree), tree_item);
      children = xaccAccountGetChildren(account);

      if(children && (xaccGroupGetNumAccounts(children) > 0)) {
        sub_tree = gtk_tree_new();
        gtk_signal_connect(GTK_OBJECT(sub_tree), "select_child",
                           GTK_SIGNAL_FUNC(gnc_ui_account_picker_select_cb),
                           picker);        
        build_acct_tree(children, sub_tree, picker);
        gtk_tree_item_set_subtree(GTK_TREE_ITEM(tree_item),
                                  sub_tree);
      }
      gtk_widget_show(tree_item);
    }
  }

  g_list_free (accts);
}

static gboolean
delete_event_cb(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
  gtk_main_quit ();
  return TRUE;
}

/****************************************************************\
 * accountPickerBox
 * select an account from the ones that the engine knows about. 
 * this is sort of like fileBox... it returns a string for the 
 * account name or NULL on cancel.  It's modal.
\****************************************************************/

SCM
accountPickerBox(char * initial_selection, int initial_type) {  

  QIFAccountPickerDialog * wind;

  AccountGroup * topgroup; 
  Account      * selected;
  GtkWidget    * treeitem  = gtk_tree_item_new_with_label(_("All Accounts"));
  GtkWidget    * subtree   = gtk_tree_new();
  SCM          infolist;

  wind = g_new0(QIFAccountPickerDialog, 1);

  wind->dialog     = create_QIF_Import_Account_Picker();
  wind->treeview   = 
    gtk_object_get_data(GTK_OBJECT(wind->dialog), "account_tree");
  wind->acct_entry = 
    gtk_object_get_data(GTK_OBJECT(wind->dialog), "acct_entry");
  wind->descript_entry  = 
    gtk_object_get_data(GTK_OBJECT(wind->dialog), "acct_description_entry");
  wind->type_picker = 
    gtk_object_get_data(GTK_OBJECT(wind->dialog), "acct_type_picker");

  gtk_object_set_data(GTK_OBJECT(wind->dialog), "account-picker-dialog",
                      wind);

  gtk_signal_connect(GTK_OBJECT(subtree), "select_child",
                     GTK_SIGNAL_FUNC(gnc_ui_account_picker_select_cb),
                     wind->dialog);

  gtk_signal_connect (GTK_OBJECT (wind->dialog), "delete_event",
                      GTK_SIGNAL_FUNC (delete_event_cb), NULL);

  /* do some setup */
  topgroup = gncGetCurrentGroup();
  gtk_tree_append(GTK_TREE(wind->treeview), treeitem);
  gtk_widget_show(treeitem);

  build_acct_tree(topgroup, subtree, wind->dialog);
  gtk_tree_item_set_subtree(GTK_TREE_ITEM(treeitem), subtree);

  gtk_tree_set_view_lines(GTK_TREE(wind->treeview), TRUE);
  gtk_tree_item_expand(GTK_TREE_ITEM(treeitem));

  gnc_option_menu_init(wind->type_picker);

  if(initial_selection) {
    selected = xaccGetAccountFromFullName(topgroup, initial_selection, ':');
    gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), initial_selection);

    if(selected) {
      if(xaccAccountGetDescription(selected)) {
        gtk_entry_set_text(GTK_ENTRY(wind->descript_entry), 
                           xaccAccountGetDescription(selected));
      }
      gtk_option_menu_set_history(GTK_OPTION_MENU(wind->type_picker),
                                  xaccAccountGetType(selected));
      infolist = SCM_LIST3(gh_str02scm(initial_selection),
                           gh_int2scm(xaccAccountGetType(selected)),
                           gh_str02scm(xaccAccountGetDescription(selected)));
    }
    else {
      gtk_entry_set_text(GTK_ENTRY(wind->descript_entry), "");
      gtk_option_menu_set_history(GTK_OPTION_MENU(wind->type_picker),
                                  initial_type);
      infolist = SCM_LIST3(gh_str02scm(initial_selection),
                           gh_int2scm(initial_type),
                           gh_str02scm(""));
    }

    scm_protect_object(infolist);
    wind->scm_acct_info = infolist;
  }

  /* make sure the window is modal, then wait on it */
  gtk_window_set_modal(GTK_WINDOW(wind->dialog), TRUE);
  gtk_widget_show(GTK_WIDGET(wind->treeview));
  gtk_widget_show(GTK_WIDGET(wind->dialog));
  gtk_main();

  infolist = wind->scm_acct_info;

  /* destroy the window */
  gtk_widget_destroy(wind->dialog);
  scm_unprotect_object(wind->scm_acct_info);
  g_free(wind);

  return infolist;
}

void
gnc_ui_account_picker_select_cb(GtkTree   * tree,
                                GtkWidget * widget,
                                gpointer  user_data) {
  QIFAccountPickerDialog * wind;
  Account      * gnc_acct;
  const char   * description;
  char         * name;
  int          acct_type;
  SCM          infolist;

  wind = gtk_object_get_data(GTK_OBJECT(user_data),
                             "account-picker-dialog");

  gnc_acct = gtk_object_get_user_data(GTK_OBJECT(widget));

  name = xaccAccountGetFullName (gnc_acct, ':');
  if (name == NULL)
    name = g_strdup ("");

  gtk_entry_set_text(GTK_ENTRY(wind->acct_entry), name);

  description = xaccAccountGetDescription(gnc_acct);
  if (description == NULL)
    description = "";

  acct_type = xaccAccountGetType(gnc_acct);

  gtk_entry_set_text(GTK_ENTRY(wind->descript_entry), 
                     description);
  gtk_option_menu_set_history(GTK_OPTION_MENU(wind->type_picker),
                              acct_type);
  infolist = SCM_LIST3(gh_str02scm(name),
                       gh_int2scm(acct_type),
                       gh_str02scm((char *) description));
  scm_protect_object(infolist);

  scm_unprotect_object(wind->scm_acct_info);
  wind->scm_acct_info = infolist;

  g_free (name);
}

void
gnc_ui_account_picker_ok_cb(GtkButton *button,
                            gpointer   user_data) {
  QIFAccountPickerDialog * wind;

  char         * selected_acct;
  char         * description;
  int          acct_type;
  SCM          infolist;

  wind = gtk_object_get_data(GTK_OBJECT(user_data),
                             "account-picker-dialog");

  selected_acct = gtk_entry_get_text(GTK_ENTRY(wind->acct_entry));
  description = gtk_entry_get_text(GTK_ENTRY(wind->descript_entry));

  acct_type = gnc_option_menu_get_active(wind->type_picker);
  gtk_entry_set_text(GTK_ENTRY(wind->descript_entry), 
                     description);
  infolist = SCM_LIST3(gh_str02scm(selected_acct),
                       gh_int2scm(acct_type),
                       gh_str02scm(description));
  scm_protect_object(infolist);

  scm_unprotect_object(wind->scm_acct_info);
  wind->scm_acct_info = infolist;

  gtk_main_quit();
}

void
gnc_ui_account_picker_cancel_cb(GtkButton * button,
                                gpointer    user_data) {
  QIFAccountPickerDialog * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data), "account-picker-dialog");

  scm_unprotect_object(wind->scm_acct_info);

  wind->scm_acct_info = SCM_BOOL_F;
  scm_protect_object(wind->scm_acct_info);

  gtk_main_quit();
}

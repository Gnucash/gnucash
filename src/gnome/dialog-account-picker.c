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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "top-level.h"

#include <gnome.h>
#include <stdio.h>

#include "glade-account-picker.h"
#include "glade-cb-account-picker.h"

#include <guile/gh.h>

#include "FileDialog.h"
#include "Group.h"
#include "Account.h"

#include "dialog-utils.h"
#include "query-user.h"
#include "util.h"


static void
build_acct_tree(AccountGroup * group, GtkWidget * tree, GtkWidget * picker) {
  Account      ** accts;
  AccountGroup * children;
  GtkWidget    * tree_item;
  GtkWidget    * sub_tree;
  int          num_accts;
  int          i;
  
  accts     = xaccGetAccounts(group);
  num_accts = xaccGetNumAccounts(group);

  for(i = 0; i < num_accts; i++) {
    if(group == xaccAccountGetParent(accts[i])) {
      tree_item = 
        gtk_tree_item_new_with_label(xaccAccountGetName(accts[i]));
      
      gtk_object_set_data(GTK_OBJECT(tree_item),
                          "acct_name",
                          xaccAccountGetFullName(accts[i], ':'));

      gtk_tree_append(GTK_TREE(tree), tree_item);
      children = xaccAccountGetChildren(accts[i]);
      
      if(children && (xaccGetNumAccounts(children) > 0)) {
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
}


/****************************************************************\
 * accountPickerBox
 * select an account from the ones that the engine knows about. 
 * this is sort of like fileBox... it returns a string for the 
 * account name or NULL on cancel.  It's modal.
\****************************************************************/

SCM
accountPickerBox(char * initial_selection, int initial_type) {  
  AccountGroup * topgroup; 
  Account      * selected;
  int          i;

  GtkWidget    * picker    = create_GNUcash_Account_Picker();
  GtkWidget    * treeview  = gtk_object_get_data(GTK_OBJECT(picker),
                                                "account_tree");
  GtkWidget    * entry     = gtk_object_get_data(GTK_OBJECT(picker),
                                                "acct_entry");
  GtkWidget    * descript  = gtk_object_get_data(GTK_OBJECT(picker),
                                                 "acct_description_entry");
  GtkWidget    * type_pick = gtk_object_get_data(GTK_OBJECT(picker),
                                                 "acct_type_picker");
  GtkWidget    * treeitem  = gtk_tree_item_new_with_label("All Accounts");
  GtkWidget    * subtree   = gtk_tree_new();

  char         * selected_account = NULL;
  SCM          infolist;

  GtkWidget    * active, * menu;

  gtk_object_set_data(GTK_OBJECT(picker), "string_return", 
                      &selected_account);

  gtk_signal_connect(GTK_OBJECT(subtree), "select_child",
                     GTK_SIGNAL_FUNC(gnc_ui_account_picker_select_cb),
                     picker);
                     
  /* do some setup */
  topgroup  = gncGetCurrentGroup();
  gtk_tree_append(GTK_TREE(treeview), treeitem);
  gtk_widget_show(treeitem);

  build_acct_tree(topgroup, subtree, picker);
  gtk_tree_item_set_subtree(GTK_TREE_ITEM(treeitem), subtree);

  gtk_tree_set_view_lines(GTK_TREE(treeview), TRUE);
  gtk_tree_item_expand(GTK_TREE_ITEM(treeitem));

  /* this is a pain in the butt but there's no other way to easily 
   * find out the index of the optionmeny selection */
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(type_pick));
  for(i = 0; i < 11; i++) {
    gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick), i);
    active = gtk_menu_get_active(GTK_MENU(menu));
    gtk_object_set_data(GTK_OBJECT(active), 
                        "option_index",
                        (gpointer)(i));
  }
  
  gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick), 0);

  if(initial_selection) {
    printf("setting up initial selection..\n");
    selected = xaccGetAccountFromFullName(topgroup, initial_selection, ':');
    gtk_entry_set_text(GTK_ENTRY(entry), initial_selection);
    
    if(selected) {
      if(xaccAccountGetDescription(selected)) {
        gtk_entry_set_text(GTK_ENTRY(descript), 
                           xaccAccountGetDescription(selected));
      }
      gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick),
                                  xaccAccountGetType(selected));
      infolist = SCM_LIST3(gh_str02scm(selected_account),
                           gh_int2scm(xaccAccountGetType(selected)),
                           gh_str02scm(xaccAccountGetDescription(selected)));
    }
    else {
      gtk_entry_set_text(GTK_ENTRY(descript), "");
      gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick),
                                  initial_type);
      infolist = SCM_LIST3(gh_str02scm(selected_account),
                           gh_int2scm(initial_type),
                           gh_str02scm(""));
    }

    scm_protect_object(infolist);
    gtk_object_set_data(GTK_OBJECT(picker), 
                        "scm_acct_info", (gpointer)infolist);    
  }
  
  /* make sure the window is modal, then wait on it */
  gtk_window_set_modal(GTK_WINDOW(picker), TRUE);
  gtk_widget_show(GTK_WIDGET(treeview));
  gtk_widget_show(GTK_WIDGET(picker));
  gtk_main();
  
  infolist = (SCM)gtk_object_get_data(GTK_OBJECT(picker),
                                 "scm_acct_info");

  /* murder it */
  gtk_widget_destroy(picker);
  
  return infolist;
}

void
gnc_ui_account_picker_select_cb(GtkTree   * tree,
                                GtkWidget * widget,
                                gpointer  user_data) {
  AccountGroup * topgroup = gncGetCurrentGroup();
  Account      * gnc_acct;
  GtkWidget    * acct_entry = gtk_object_get_data(GTK_OBJECT(user_data),
                                                  "acct_entry");
  GtkWidget    * descript = gtk_object_get_data(GTK_OBJECT(user_data),
                                                "acct_description_entry");
  GtkWidget    * type_pick = gtk_object_get_data(GTK_OBJECT(user_data),
                                                 "acct_type_picker");
  char         * selected_acct;
  char         * description;
  int          acct_type;
  SCM          infolist;

  printf("in select cb\n");
  selected_acct = gtk_object_get_data(GTK_OBJECT(widget), "acct_name");  
  gnc_acct      = xaccGetAccountFromFullName(topgroup, selected_acct, 
                                             ':');
  
  gtk_entry_set_text(GTK_ENTRY(acct_entry), selected_acct);
  description = xaccAccountGetDescription(gnc_acct);
  acct_type = xaccAccountGetType(gnc_acct);

  gtk_entry_set_text(GTK_ENTRY(descript), 
                     description);
  gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick),
                              acct_type);
  infolist = SCM_LIST3(gh_str02scm(selected_acct),
                       gh_int2scm(acct_type),
                       gh_str02scm(description));
  scm_protect_object(infolist);
  gtk_object_set_data(GTK_OBJECT(user_data), 
                      "scm_acct_info", (gpointer)infolist);
  printf("leaving select cb\n");
}


void
gnc_ui_account_picker_ok_cb(GtkButton *button,
                            gpointer   user_data) {
  GtkWidget    * acct_entry = gtk_object_get_data(GTK_OBJECT(user_data),
                                                  "acct_entry");
  GtkWidget    * descript = gtk_object_get_data(GTK_OBJECT(user_data),
                                                "acct_description_entry");
  GtkWidget    * type_pick = gtk_object_get_data(GTK_OBJECT(user_data),
                                                 "acct_type_picker");
  GtkWidget    * type_menu;
  GtkWidget    * menuitem;

  char         * selected_acct;
  char         * description;
  int          acct_type;
  SCM          infolist;
  
  selected_acct = gtk_entry_get_text(GTK_ENTRY(acct_entry));
  description = gtk_entry_get_text(GTK_ENTRY(descript));

  type_menu    = gtk_option_menu_get_menu(GTK_OPTION_MENU(type_pick));
  menuitem     = gtk_menu_get_active(GTK_MENU(type_menu));
  acct_type    = (int)(gtk_object_get_data(GTK_OBJECT(menuitem),
                                           "option_index"));
  
  gtk_entry_set_text(GTK_ENTRY(descript), 
                     description);
  gtk_option_menu_set_history(GTK_OPTION_MENU(type_pick),
                              acct_type);
  infolist = SCM_LIST3(gh_str02scm(selected_acct),
                       gh_int2scm(acct_type),
                       gh_str02scm(description));
  scm_protect_object(infolist);
  gtk_object_set_data(GTK_OBJECT(user_data), 
                      "scm_acct_info", (gpointer)infolist);
  
  gtk_main_quit();
}

void
gnc_ui_account_picker_cancel_cb(GtkButton * button,
                                gpointer         user_data) {
  gtk_object_set_data(GTK_OBJECT(user_data),
                      "scm_acct_info",
                      (gpointer)SCM_BOOL_F);
  gtk_main_quit();
}

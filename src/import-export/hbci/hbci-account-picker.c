/********************************************************************\
 * hbci-account-picker.c -- window for picking a Gnucash account    * 
 * Copyright (C) 2002 Christian <stimming@tuhh.de>                  *
 * Heavily copied from the QIF importer, which is                   *
 * Copyright (C) 2000-2001 Bill Gribble <grib@billgribble.com>      *
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
#include <guile/gh.h>

#include "hbci-account-picker.h"
#include "dialog-utils.h"
#include "druid-hbci-initial.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "Group.h"

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkWidget       * treeview;
  HBCIInitialInfo * hbci_info;
  Account         * select;  
  GHashTable *hash;
};

static void
acct_tree_add_accts(AccountGroup *accts, 
		    GtkCTree * tree, GtkCTreeNode * parent,
                    char * base_name, int *row, 
		    HBCIAccountPickerDialog *wind) {
  char         * acctinfo[2];
  char         * acctname;
  char         sep[2] = " ";
  GtkCTreeNode * node; 
  gboolean     leafnode;
  AccountList *current;
  AccountList *list, *lastlist;
  AccountGroup *children;
    
  sep[0] = gnc_get_account_separator();
  acctinfo[1] = "";
  list = xaccGroupGetAccountList (accts);
  current = g_list_first (list);
  lastlist = g_list_last (list);
  
  while(current && (current != lastlist)) {

    acctname = g_strdup (xaccAccountGetName (current->data));
    acctinfo[0] = acctname;
    
    children = xaccAccountGetChildren (current->data);
    
    leafnode = (children) ? FALSE : TRUE;
    
    node = gtk_ctree_insert_node(tree, parent, NULL, 
                                 acctinfo, 2,
                                 NULL, NULL, NULL, NULL,
                                 leafnode, TRUE);
    /* set some row data */ 
    gtk_ctree_node_set_row_data(tree, node, current->data);
    g_hash_table_insert (wind->hash, node, current->data);
    
    gnc_clist_set_check (GTK_CLIST (tree), (*row)++, 1,
                         FALSE);

    if(!leafnode) {
      acct_tree_add_accts(children, tree, node, acctname, row, wind);
    }
    
    current = g_list_next (current);
  }
}

static void
build_acct_tree(HBCIAccountPickerDialog * picker, HBCIInitialInfo * import) {
  GtkCTreeNode * new_sel;
  int row = 0;
  AccountGroup *acct_tree;
  
  acct_tree = gnc_book_get_group (gnc_get_current_book ());

  gtk_clist_freeze (GTK_CLIST(picker->treeview));
  gtk_clist_clear(GTK_CLIST(picker->treeview));
  gtk_clist_set_column_justification (GTK_CLIST(picker->treeview),
                                      1, GTK_JUSTIFY_CENTER);

  acct_tree_add_accts(acct_tree, GTK_CTREE(picker->treeview),
                      NULL, NULL, &row, picker);

  if(picker->select) {
    new_sel =
      gtk_ctree_find_by_row_data(GTK_CTREE(picker->treeview),
				 NULL,
				 picker->select);
    
    gtk_ctree_select(GTK_CTREE(picker->treeview), new_sel);
    gtk_ctree_node_moveto(GTK_CTREE(picker->treeview), new_sel, 0,
                          0.5, 0.0);
  }

  gtk_clist_columns_autosize (GTK_CLIST (picker->treeview));
  gtk_clist_column_titles_passive (GTK_CLIST (picker->treeview));
  gtk_clist_thaw (GTK_CLIST(picker->treeview));
}

/* static void */
/* new_child_string_cb(char * string, gpointer data) { */
/*   if(string) { */
/*     strncpy(data, string, 250); */
/*   } */
/*   else { */
/*     *(char *)data = 0; */
/*   } */
/* } */

/* static void */
/* gnc_ui_qif_account_picker_new_cb(GtkButton * w, gpointer user_data) { */
/*   HBCIAccountPickerDialog * wind = user_data; */
/*   //SCM name_setter = gh_eval_str("qif-map-entry:set-gnc-name!"); */
/*   char name[251] = ""; */
/*   char sep[2] = " "; */
/*   int  retval    = -1; */
/*   char * fullname = NULL; */

/*   GtkWidget * dlg = gnome_request_dialog(FALSE,  */
/*                                          _("Enter a name for the account"),  */
/*                                          "", 250, */
/*                                          &new_child_string_cb, &name[0], */
/*                                          NULL); */
/*   retval = gnome_dialog_run_and_close(GNOME_DIALOG(dlg)); */
/*   sep[0] = gnc_get_account_separator(); */

/*    retval is 0 if the 'ok' button was clicked */ 
/*   if(retval == 0) { */
/*     if(wind->select) { */
/*       fullname = g_strjoin(sep, xaccAccountGetName(wind->select), name, NULL); */
/*     } */
/*     else { */
/*       fullname = g_strdup(name); */
/*     } */
/*     //wind->selected_name = g_strdup(fullname); */
/*     //gh_call2(name_setter, wind->select, gh_str02scm(fullname)); */
/*     //g_free(fullname); */
/*   } */

/*   build_acct_tree(wind, wind->hbci_info); */

/* } */

static void
gnc_ui_qif_account_picker_select_cb(GtkCTree   * tree,
                                    GtkCTreeNode  * node,
                                    gint column,
                                    gpointer  user_data) {
  HBCIAccountPickerDialog * wind = user_data;

  wind->select = 
    g_hash_table_lookup (wind->hash, node);
}


static void
gnc_ui_qif_account_picker_unselect_cb(GtkCTree   * tree,
                                      GtkCTreeNode  * node,
                                      gint column,
                                      gpointer  user_data) {
  HBCIAccountPickerDialog * wind = user_data;

  wind->select = NULL;
}



/****************************************************************
 * hbci_account_picker_dialog
 * select an account from the ones that the engine knows about. 
 * it returns a pointer to the resulting account or 
 * NULL on cancel.  It's modal.
 ****************************************************************/

Account *
hbci_account_picker_dialog(HBCIInitialInfo *info, Account *initial_sel) {  
  HBCIAccountPickerDialog * wind;
  int retval = -1;
  Account *retaccount;
  //char * scmname;
  GladeXML *xml;

  wind = g_new0(HBCIAccountPickerDialog, 1);

  xml = gnc_glade_xml_new ("qif.glade", "QIF Import Account Picker");

  //glade_xml_signal_connect_data
  //  (xml, "gnc_ui_qif_account_picker_new_cb",
  //   GTK_SIGNAL_FUNC (gnc_ui_qif_account_picker_new_cb), wind);

  wind->dialog     = glade_xml_get_widget (xml, "QIF Import Account Picker");
  wind->treeview   = glade_xml_get_widget (xml, "account_tree");
  wind->hbci_info   = info;

  wind->select  = initial_sel;
  wind->hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);

  
  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_select_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_select_cb),
                     wind);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_unselect_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_unselect_cb),
                     wind);

  /* this is to get the checkmarks set up right.. it will get called 
   * again after the window is mapped. */
  build_acct_tree(wind, wind->hbci_info);

  retval = gnome_dialog_run_and_close(GNOME_DIALOG(wind->dialog));  

  retaccount = (retval == 0) ? wind->select : initial_sel;
  
  g_hash_table_destroy (wind->hash);
  g_free(wind);

  return retaccount;
}

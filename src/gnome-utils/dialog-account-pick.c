/********************************************************************\
 * dialog-account-pick.c -- window for picking a Gnucash account    * 
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
#include <libguile.h>

#include "dialog-account-pick.h"
#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "Group.h"

/* Note: the name is '-pick' simply because a 'dialog-account-picker'
   already exists in the src/import-export/qif-import subdirectory. */

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkWidget       * treeview;
  Account         * select;  
  GHashTable *hash;
};

static gpointer add_acc_cb (Account *a, gpointer user_data);
typedef struct 
{
  GtkCTree * tree; 
  GtkCTreeNode * parent;
  char *acctinfo[2];
  int *row;
  GNCAccountPickerDialog *wind;
} add_acc_data;

static void
acct_tree_add_accts(AccountGroup *accts, 
		    GtkCTree * tree, GtkCTreeNode * parent,
                    char * base_name, int *row, 
		    GNCAccountPickerDialog *wind) 
{
  add_acc_data user_data;
  
  user_data.acctinfo[1] = "";
  user_data.tree = tree;
  user_data.parent = parent;
  user_data.row = row;
  user_data.wind = wind;

  /* Let the list traversal be done by the Group's foreach function. */
  xaccGroupForEachAccount (accts, add_acc_cb, &user_data, FALSE);
}

static gpointer add_acc_cb (Account *current, gpointer user_data)
{
  char* acctname;
  GtkCTreeNode * node; 
  AccountGroup *children;
  gboolean     leafnode;
  add_acc_data *data = user_data;
  g_assert (current);
  g_assert (data);
  
  acctname = g_strdup (xaccAccountGetName (current));
  data->acctinfo[0] = acctname;
    
  children = xaccAccountGetChildren (current);
  leafnode = (children) ? FALSE : TRUE;
    
  node = gtk_ctree_insert_node(data->tree, data->parent, NULL, 
			       data->acctinfo, 2,
			       NULL, NULL, NULL, NULL,
			       leafnode, TRUE);
  /* set some row data */ 
  gtk_ctree_node_set_row_data(data->tree, node, current);
  g_hash_table_insert (data->wind->hash, node, current);
  gnc_clist_set_check (GTK_CLIST (data->tree), 
		       (*(data->row))++, 1, FALSE);

  if(!leafnode) 
    acct_tree_add_accts(children, data->tree, node, acctname, 
			data->row, data->wind);
  return NULL;
}

static void
build_acct_tree(GNCAccountPickerDialog * picker) {
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

static void
gnc_ui_qif_account_picker_select_cb(GtkCTree   * tree,
                                    GtkCTreeNode  * node,
                                    gint column,
                                    gpointer  user_data) {
  GNCAccountPickerDialog * wind = user_data;

  wind->select = 
    g_hash_table_lookup (wind->hash, node);
}


static void
gnc_ui_qif_account_picker_unselect_cb(GtkCTree   * tree,
                                      GtkCTreeNode  * node,
                                      gint column,
                                      gpointer  user_data) {
  GNCAccountPickerDialog * wind = user_data;

  wind->select = NULL;
}



/****************************************************************
 * gnc_account_picker_dialog
 * select an account from the ones that the engine knows about. 
 * it returns a pointer to the resulting account or 
 * NULL on cancel.  It's modal.
 ****************************************************************/

Account *
gnc_account_picker_dialog(Account *initial_sel) {  
  GNCAccountPickerDialog * wind;
  int retval = -1;
  Account *retaccount;
  //char * scmname;
  GladeXML *xml;
  GtkWidget *new_account_button;
  
  wind = g_new0(GNCAccountPickerDialog, 1);

  xml = gnc_glade_xml_new ("account.glade", "Account Picker");

  //glade_xml_signal_connect_data
  //  (xml, "gnc_ui_qif_account_picker_new_cb",
  //   GTK_SIGNAL_FUNC (gnc_ui_qif_account_picker_new_cb), wind);

  g_assert
    (wind->dialog = glade_xml_get_widget (xml, "Account Picker"));
  g_assert
    (wind->treeview   = glade_xml_get_widget (xml, "account_tree"));
  g_assert
    (new_account_button = glade_xml_get_widget (xml, "new_account_button"));

  wind->select  = initial_sel;
  wind->hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);

  /* Make this button insensitive since it's still unimplemented. */
  gtk_widget_set_sensitive (GTK_WIDGET (new_account_button), FALSE);
  
  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_select_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_select_cb),
                     wind);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_unselect_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_unselect_cb),
                     wind);

  /* this is to get the checkmarks set up right.. it will get called 
   * again after the window is mapped. */
  build_acct_tree(wind);

  retval = gnome_dialog_run_and_close(GNOME_DIALOG(wind->dialog));  

  retaccount = (retval == 0) ? wind->select : initial_sel;
  
  g_hash_table_destroy (wind->hash);
  g_free(wind);

  return retaccount;
}

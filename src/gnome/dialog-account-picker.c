/********************************************************************\
 * dialog-account-picker.c -- window for picking a Gnucash account  * 
 * from the QIF importer.                                           *
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

#include "dialog-account-picker.h"
#include "druid-qif-import.h"

#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "query-user.h"

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkWidget       * treeview;
  QIFImportWindow * qif_wind;
  SCM             map_entry;  
  gchar           * selected_name;
};

static void
row_data_destroy_cb(gpointer data) {
  g_free(data);
}

static void
acct_tree_add_accts(SCM accts, GtkCTree * tree, GtkCTreeNode * parent,
                    char * base_name, int *row) {
  char         * acctinfo[2];
  char         * acctname;
  char         sep[2] = " ";
  GtkCTreeNode * node; 
  gboolean     leafnode;
  SCM          current;
  
  sep[0] = gnc_get_account_separator();
  acctinfo[1] = "";

  while(!gh_null_p(accts)) {
    current = gh_car(accts);

    if(gh_null_p(current)) {
      printf(" ** BUG in acct tree .. grib fix me! (everybody else ignore)\n");
      accts = gh_cdr(accts);
      continue;
    }

    acctinfo[0] = gh_scm2newstr(gh_car(current), NULL);

    if(!gh_null_p(gh_caddr(current))) {
      leafnode = FALSE;
    }
    else {
      leafnode = TRUE;
    }
    
    node = gtk_ctree_insert_node(tree, parent, NULL, 
                                 acctinfo, 2,
                                 NULL, NULL, NULL, NULL,
                                 leafnode, TRUE);

    gnc_clist_set_check (GTK_CLIST (tree), (*row)++, 1,
                         gh_cadr (current) == SCM_BOOL_T);

    /* set some row data */ 
    if(base_name && (strlen(base_name) > 0)) {
      acctname =  g_strjoin(sep, base_name, acctinfo[0], NULL);
    }
    else {
      acctname = g_strdup(acctinfo[0]);
    }
    gtk_ctree_node_set_row_data_full(tree, node,
                                     acctname,
                                     row_data_destroy_cb);
    if(!leafnode) {
      acct_tree_add_accts(gh_caddr(current), tree, node, acctname, row);
    }
    
    accts = gh_cdr(accts);      
  }
}

static gint
test_str_cmp(gconstpointer a, gconstpointer b) {
  return strcmp(a, b);
}

static void
build_acct_tree(QIFAccountPickerDialog * picker, QIFImportWindow * import) {
  SCM  get_accts = gh_eval_str("qif-import:get-all-accts");
  SCM  acct_tree = gh_call1(get_accts, 
                            gnc_ui_qif_import_druid_get_mappings(import));
  GtkCTreeNode * new_sel;
  int row = 0;

  gtk_clist_clear(GTK_CLIST(picker->treeview));

  gtk_clist_set_column_justification (GTK_CLIST(picker->treeview),
                                      1, GTK_JUSTIFY_CENTER);

  acct_tree_add_accts(acct_tree, GTK_CTREE(picker->treeview),
                      NULL, NULL, &row);

  if(picker->selected_name) {
    new_sel =
      gtk_ctree_find_by_row_data_custom(GTK_CTREE(picker->treeview),
                                        NULL,
                                        picker->selected_name,
                                        &test_str_cmp);
    
    gtk_ctree_select(GTK_CTREE(picker->treeview), new_sel);
    gtk_ctree_node_moveto(GTK_CTREE(picker->treeview), new_sel, 0,
                          0.5, 0.0);
  }

  gtk_clist_columns_autosize (GTK_CLIST (picker->treeview));
  gtk_clist_column_titles_passive (GTK_CLIST (picker->treeview));
}

static void
new_child_string_cb(char * string, gpointer data) {
  if(string) {
    strncpy(data, string, 250);
  }
  else {
    *(char *)data = 0;
  }
}

void
gnc_ui_qif_account_picker_new_cb(GtkButton * w, gpointer user_data) {
  QIFAccountPickerDialog * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data),
                        "account_picker_struct");
  SCM name_setter = gh_eval_str("qif-map-entry:set-gnc-name!");
  char name[251] = "";
  char sep[2] = " ";
  int  retval    = -1;
  char * fullname = NULL;

  GtkWidget * dlg = gnome_request_dialog(FALSE, 
                                         _("Enter a name for the account"), 
                                         "", 250,
                                         &new_child_string_cb, &name[0],
                                         NULL);
  retval = gnome_dialog_run_and_close(GNOME_DIALOG(dlg));
  sep[0] = gnc_get_account_separator();

  /* retval is 0 if the 'ok' button was clicked */
  if(retval == 0) {
    if(wind->selected_name && (strlen(wind->selected_name) > 0)) {
      fullname = g_strjoin(sep, wind->selected_name, name, NULL);
    }
    else {
      fullname = g_strdup(name);
    }
    wind->selected_name = g_strdup(fullname);
    gh_call2(name_setter, wind->map_entry, gh_str02scm(fullname));
    g_free(fullname);
  }

  build_acct_tree(wind, wind->qif_wind);

}

static void
gnc_ui_qif_account_picker_select_cb(GtkCTree   * tree,
                                    GtkCTreeNode  * node,
                                    gint column,
                                    gpointer  user_data) {
  QIFAccountPickerDialog * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data),
                        "account_picker_struct");
  SCM name_setter = gh_eval_str("qif-map-entry:set-gnc-name!");

  g_free(wind->selected_name);
  wind->selected_name = 
    g_strdup(gtk_ctree_node_get_row_data(tree, node));

  gh_call2(name_setter, wind->map_entry, gh_str02scm(wind->selected_name));
}


static void
gnc_ui_qif_account_picker_unselect_cb(GtkCTree   * tree,
                                      GtkCTreeNode  * node,
                                      gint column,
                                      gpointer  user_data) {
  QIFAccountPickerDialog * wind = 
    gtk_object_get_data(GTK_OBJECT(user_data),
                        "account_picker_struct");
  g_free(wind->selected_name);
  wind->selected_name = NULL;
}

/****************************************************************
 * accountPickerBox 
 * select an account from the ones that the engine knows about, plus
 * the ones that will be created newly by the QIF import.  this is
 * sort of like fileBox... it returns a string for the account name or
 * NULL on cancel.  It's modal.
 ****************************************************************/

SCM
qif_account_picker_dialog(QIFImportWindow * qif_wind, SCM map_entry) {  
  QIFAccountPickerDialog * wind;
  SCM save_entry   = gh_eval_str("qif-map-entry:clone");
  SCM init_pick    = gh_eval_str("qif-map-entry:gnc-name");
  SCM saved_entry  = gh_call1(save_entry, map_entry);
  int retval = -1;
  char * scmname;
  wind = g_new0(QIFAccountPickerDialog, 1);

  wind->dialog     = create_QIF_Import_Account_Picker();
  wind->treeview   = 
    gtk_object_get_data(GTK_OBJECT(wind->dialog), "account_tree");
  wind->qif_wind   = qif_wind;

  wind->map_entry  = map_entry;
  
  scmname = gh_scm2newstr(gh_call1(init_pick, map_entry), NULL);
  wind->selected_name = g_strdup(scmname);
  free(scmname);

  scm_protect_object(wind->map_entry);

  gtk_object_set_data(GTK_OBJECT(wind->dialog), "account_picker_struct",
                      wind);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_select_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_select_cb),
                     wind->dialog);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_unselect_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_unselect_cb),
                     wind->dialog);

  /* update the tree display with all the existing accounts plus all
   * the ones the QIF importer thinks it will be creating.  this will
   * also select the map_entry line. */
  build_acct_tree(wind, qif_wind);
  
  retval = gnome_dialog_run_and_close(GNOME_DIALOG(wind->dialog));  
  
  scm_unprotect_object(wind->map_entry);
  g_free(wind->selected_name);
  g_free(wind);

  if(retval == 0) {
    return map_entry;
  }
  else {
    return saved_entry;
  }
}

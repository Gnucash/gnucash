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
#include <libguile.h>

#include "dialog-account-picker.h"
#include "dialog-utils.h"
#include "druid-qif-import.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"

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
                    char * base_name, int *row)
{
  char         * acctinfo[2];
  char         * acctname;
  char         sep[2] = " ";
  GtkCTreeNode * node; 
  gboolean     leafnode;
  SCM          current;
  
  sep[0] = gnc_get_account_separator();
  acctinfo[1] = "";

  while(!SCM_NULLP(accts)) {
    current = SCM_CAR(accts);

    if(SCM_NULLP(current)) {
      printf(" ** BUG in acct tree .. grib fix me! (everybody else ignore)\n");
      accts = SCM_CDR(accts);
      continue;
    }

    if (SCM_STRINGP(SCM_CAR(current)))
      acctinfo[0] = g_strdup(SCM_STRING_CHARS(SCM_CAR(current)));
    else
      acctinfo[0] = g_strdup("");

    if(!SCM_NULLP(SCM_CADDR(current))) {
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
                         SCM_CADR (current) == SCM_BOOL_T);

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
      acct_tree_add_accts(SCM_CADDR(current), tree, node, acctname, row);
    }
    
    accts = SCM_CDR(accts);      
  }
}

static gint
test_str_cmp(gconstpointer a, gconstpointer b)
{
  return strcmp(a, b);
}

static void
build_acct_tree(QIFAccountPickerDialog * picker, QIFImportWindow * import)
{
  SCM  get_accts = scm_c_eval_string("qif-import:get-all-accts");
  SCM  acct_tree = scm_call_1(get_accts, 
			      gnc_ui_qif_import_druid_get_mappings(import));
  GtkCTreeNode * new_sel;
  int row = 0;

  gtk_clist_freeze(GTK_CLIST(picker->treeview));
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
  gtk_clist_thaw(GTK_CLIST(picker->treeview));
}

static void
gnc_ui_qif_account_picker_new_cb(GtkButton * w, gpointer user_data)
{
  QIFAccountPickerDialog * wind = user_data;
  SCM name_setter = scm_c_eval_string("qif-map-entry:set-gnc-name!");
  const char *name;
  char sep[2] = " ";
  int  response;
  char * fullname;
  GtkWidget *dlg, *entry;

  dlg = gtk_message_dialog_new (GTK_WINDOW(wind->dialog),
				GTK_DIALOG_DESTROY_WITH_PARENT,
				GTK_MESSAGE_QUESTION,
				GTK_BUTTONS_OK_CANCEL,
				_("Enter a name for the account"));

  entry = gtk_entry_new_with_max_length (250);
  gtk_widget_show(entry);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dlg)->vbox), entry);

  response = gtk_dialog_run(GTK_DIALOG(dlg));
  if (response == GTK_RESPONSE_OK) {
    name = gtk_entry_get_text(GTK_ENTRY(entry));
    if(wind->selected_name && (strlen(wind->selected_name) > 0)) {
      sep[0] = gnc_get_account_separator();
      fullname = g_strjoin(sep, wind->selected_name, name, NULL);
    }
    else {
      fullname = g_strdup(name);
    }
    wind->selected_name = g_strdup(fullname);
    scm_call_2(name_setter, wind->map_entry, scm_makfrom0str(fullname));
    g_free(fullname);
  }
  gtk_widget_destroy(dlg);

  build_acct_tree(wind, wind->qif_wind);

}

static void
gnc_ui_qif_account_picker_select_cb(GtkCTree   * tree,
                                    GtkCTreeNode  * node,
                                    gint column,
                                    gpointer  user_data)
{
  QIFAccountPickerDialog * wind = user_data;
  SCM name_setter = scm_c_eval_string("qif-map-entry:set-gnc-name!");

  g_free(wind->selected_name);
  wind->selected_name = 
    g_strdup(gtk_ctree_node_get_row_data(tree, node));

  scm_call_2(name_setter, wind->map_entry, scm_makfrom0str(wind->selected_name));
}


static void
gnc_ui_qif_account_picker_unselect_cb(GtkCTree   * tree,
                                      GtkCTreeNode  * node,
                                      gint column,
                                      gpointer  user_data)
{
  QIFAccountPickerDialog * wind = user_data;

  g_free(wind->selected_name);
  wind->selected_name = NULL;
}


static int
gnc_ui_qif_account_picker_map_cb(GtkWidget * w, gpointer user_data)
{
  QIFAccountPickerDialog * wind = user_data;

  /* update the tree display with all the existing accounts plus all
   * the ones the QIF importer thinks it will be creating.  this will
   * also select the map_entry line. */
  build_acct_tree(wind, wind->qif_wind);
  return FALSE;
}

/****************************************************************
 * qif_account_picker_dialog
 * select an account from the ones that the engine knows about, plus
 * the ones that will be created newly by the QIF import.  this is
 * sort of like fileBox... it returns a string for the account name or
 * NULL on cancel.  It's modal.
 ****************************************************************/

SCM
qif_account_picker_dialog(QIFImportWindow * qif_wind, SCM map_entry)
{  
  QIFAccountPickerDialog * wind;
  SCM save_entry   = scm_c_eval_string("qif-map-entry:clone");
  SCM init_pick    = scm_c_eval_string("qif-map-entry:gnc-name");
  SCM saved_entry  = scm_call_1(save_entry, map_entry);
  int response;
  const gchar * scmname;
  GladeXML *xml;
  GtkWidget *button;

  wind = g_new0(QIFAccountPickerDialog, 1);

  xml = gnc_glade_xml_new ("qif.glade", "QIF Import Account Picker");

  glade_xml_signal_connect_data
    (xml, "gnc_ui_qif_account_picker_new_cb",
     GTK_SIGNAL_FUNC (gnc_ui_qif_account_picker_new_cb), wind);

  wind->dialog     = glade_xml_get_widget (xml, "QIF Import Account Picker");
  wind->treeview   = glade_xml_get_widget (xml, "account_tree");
  wind->qif_wind   = qif_wind;

  wind->map_entry  = map_entry;
  
  scmname = SCM_STRING_CHARS(scm_call_1(init_pick, map_entry));
  wind->selected_name = g_strdup(scmname);

  scm_gc_protect_object(wind->map_entry);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_select_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_select_cb),
                     wind);

  gtk_signal_connect(GTK_OBJECT(wind->treeview), "tree_unselect_row",
                     GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_unselect_cb),
                     wind);

  gtk_signal_connect_after(GTK_OBJECT(wind->dialog), "map",
                           GTK_SIGNAL_FUNC(gnc_ui_qif_account_picker_map_cb),
                           wind);
  
  button = glade_xml_get_widget (xml, "newbutton");
  gtk_button_set_use_stock(GTK_BUTTON(button), TRUE);

  /* this is to get the checkmarks set up right.. it will get called 
   * again after the window is mapped. */
  build_acct_tree(wind, wind->qif_wind);

  do {
    response = gtk_dialog_run(GTK_DIALOG(wind->dialog));
  } while (response == GNC_RESPONSE_NEW);
  gtk_widget_destroy(wind->dialog);

  scm_gc_unprotect_object(wind->map_entry);
  g_free(wind->selected_name);
  g_free(wind);

  if (response == GTK_RESPONSE_OK) {
    return map_entry;
  }
  else {
    return saved_entry;
  }
}

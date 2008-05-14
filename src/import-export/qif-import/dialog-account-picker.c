/********************************************************************\
 * dialog-account-picker.c -- window for picking a Gnucash account  *
 * from the QIF importer.                                           *
 * Copyright (C) 2000-2001 Bill Gribble <grib@billgribble.com>      *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <libguile.h>

#include "dialog-account-picker.h"
#include "dialog-utils.h"
#include "druid-qif-import.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"

enum account_cols {
  ACCOUNT_COL_NAME = 0,
  ACCOUNT_COL_FULLNAME,
  ACCOUNT_COL_CHECK,
  NUM_ACCOUNT_COLS
};

struct _accountpickerdialog {
  GtkWidget       * dialog;
  GtkTreeView     * treeview;
  QIFImportWindow * qif_wind;
  SCM             map_entry;
  gchar           * selected_name;
};

static void
acct_tree_add_accts(SCM accts,
                    GtkTreeStore *store,
                    GtkTreeIter *parent,
                    const char *base_name,
                    const char *selected_name,
                    GtkTreeRowReference **reference)
{
  GtkTreeIter  iter;
  char         * compname;
  char         * acctname;
  gboolean     leafnode;
  SCM          current;
  gboolean     checked;

  while(!SCM_NULLP(accts)) {
    current = SCM_CAR(accts);

    if(SCM_NULLP(current)) {
      g_critical("QIF import: BUG DETECTED in acct_tree_add_accts!");
      accts = SCM_CDR(accts);
      continue;
    }

    if (SCM_STRINGP(SCM_CAR(current)))
      compname = SCM_STRING_CHARS(SCM_CAR(current));
    else
      compname = "";

    if (!SCM_NULLP(SCM_CADDR(current))) {
      leafnode = FALSE;
    }
    else {
      leafnode = TRUE;
    }

    /* compute full name */
    if (base_name && *base_name) {
      acctname =  g_strjoin(gnc_get_account_separator_string(),
                            base_name, compname, (char *)NULL);
    }
    else {
      acctname = g_strdup(compname);
    }

    checked = (SCM_CADR(current) == SCM_BOOL_T);

    gtk_tree_store_append(store, &iter, parent);
    gtk_tree_store_set(store, &iter,
                       ACCOUNT_COL_NAME, compname,
                       ACCOUNT_COL_FULLNAME, acctname,
                       ACCOUNT_COL_CHECK, checked,
                       -1);

    if (reference && !*reference &&
        selected_name && (g_utf8_collate(selected_name, acctname) == 0)) {
      GtkTreePath *path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
      *reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
      gtk_tree_path_free(path);
    }

    if(!leafnode) {
      acct_tree_add_accts(SCM_CADDR(current), store, &iter, acctname,
                          selected_name, reference);
    }

    g_free(acctname);

    accts = SCM_CDR(accts);
  }
}

static void
build_acct_tree(QIFAccountPickerDialog * picker, QIFImportWindow * import)
{
  SCM  get_accts = scm_c_eval_string("qif-import:get-all-accts");
  SCM  acct_tree = scm_call_1(get_accts,
                              gnc_ui_qif_import_druid_get_mappings(import));
  GtkTreeStore *store;
  GtkTreePath *path;
  GtkTreeSelection* selection;
  GtkTreeRowReference *reference = NULL;

  store = GTK_TREE_STORE(gtk_tree_view_get_model(picker->treeview));
  gtk_tree_store_clear(store);

  acct_tree_add_accts(acct_tree, store, NULL, NULL,
                      picker->selected_name, &reference);

  if (reference) {
    selection = gtk_tree_view_get_selection(picker->treeview);
    path = gtk_tree_row_reference_get_path(reference);
    if (path) {
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_path_free(path);
    }
    gtk_tree_row_reference_free(reference);
  }
}

static void
gnc_ui_qif_account_picker_new_cb(GtkButton * w, gpointer user_data)
{
  QIFAccountPickerDialog * wind = user_data;
  SCM name_setter = scm_c_eval_string("qif-map-entry:set-gnc-name!");
  const char *name;
  int  response;
  char * fullname;
  GtkWidget *dlg, *entry;

  dlg = gtk_message_dialog_new(GTK_WINDOW(wind->dialog),
                                GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_QUESTION,
                                GTK_BUTTONS_OK_CANCEL,
                                "%s", _("Enter a name for the account"));

  entry = gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(entry), 250);
  gtk_widget_show(entry);
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dlg)->vbox), entry);

  response = gtk_dialog_run(GTK_DIALOG(dlg));
  if (response == GTK_RESPONSE_OK) {
    name = gtk_entry_get_text(GTK_ENTRY(entry));
    if(wind->selected_name && (strlen(wind->selected_name) > 0)) {
      fullname = g_strjoin(gnc_get_account_separator_string(),
                           wind->selected_name, name, (char *)NULL);
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
gnc_ui_qif_account_picker_changed_cb(GtkTreeSelection *selection,
                                     gpointer          user_data)
{
  QIFAccountPickerDialog * wind = user_data;
  SCM name_setter = scm_c_eval_string("qif-map-entry:set-gnc-name!");
  GtkTreeModel *model;
  GtkTreeIter iter;

  g_free(wind->selected_name);
  if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
    gtk_tree_model_get(model, &iter,
                       ACCOUNT_COL_FULLNAME, &wind->selected_name,
                       -1);
    scm_call_2(name_setter, wind->map_entry,
               scm_makfrom0str(wind->selected_name));
  } else {
    wind->selected_name = NULL;
  }
}

static void
gnc_ui_qif_account_picker_row_activated_cb(GtkTreeView *view,
                                           GtkTreePath *path,
                                           GtkTreeViewColumn *column,
                                           gpointer user_data)
{
  QIFAccountPickerDialog *wind = user_data;
  g_return_if_fail(wind);

  gtk_dialog_response(GTK_DIALOG(wind->dialog), GTK_RESPONSE_OK);
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
 *
 * Select an account from the ones that the engine knows about,
 * plus those that will be created by the QIF import.  Returns
 * a new Scheme map entry, or SCM_BOOL_F on cancel. Modal.
 ****************************************************************/

SCM
qif_account_picker_dialog(QIFImportWindow * qif_wind, SCM map_entry)
{
  QIFAccountPickerDialog * wind;
  SCM clone_entry  = scm_c_eval_string("qif-map-entry:clone");
  SCM init_pick    = scm_c_eval_string("qif-map-entry:gnc-name");
  SCM new_entry    = scm_call_1(clone_entry, map_entry);
  int response;
  const gchar * scmname;
  GladeXML *xml;
  GtkWidget *button;

  wind = g_new0(QIFAccountPickerDialog, 1);

  xml = gnc_glade_xml_new("qif.glade", "QIF Import Account Picker");

  glade_xml_signal_connect_data(xml,
                                "gnc_ui_qif_account_picker_new_cb",
                                G_CALLBACK(gnc_ui_qif_account_picker_new_cb),
                                wind);

  wind->dialog     = glade_xml_get_widget(xml, "QIF Import Account Picker");
  wind->treeview   = GTK_TREE_VIEW(glade_xml_get_widget(xml, "account_tree"));
  wind->qif_wind   = qif_wind;

  wind->map_entry  = new_entry;

  scmname = SCM_STRING_CHARS(scm_call_1(init_pick, new_entry));
  wind->selected_name = g_strdup(scmname);

  scm_gc_protect_object(wind->map_entry);

  {
    GtkTreeStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;

    store = gtk_tree_store_new(NUM_ACCOUNT_COLS, G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_BOOLEAN);
    gtk_tree_view_set_model(wind->treeview, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Account"),
                                                      renderer,
                                                      "text",
                                                      ACCOUNT_COL_NAME,
                                                      NULL);
    g_object_set(column, "expand", TRUE, NULL);
    gtk_tree_view_append_column(wind->treeview, column);

    renderer = gtk_cell_renderer_toggle_new();
    g_object_set(renderer, "activatable", FALSE, NULL);
    column = gtk_tree_view_column_new_with_attributes(_("New?"),
                                                      renderer,
                                                      "active",
                                                      ACCOUNT_COL_CHECK,
                                                      NULL);
    gtk_tree_view_append_column(wind->treeview, column);

    selection = gtk_tree_view_get_selection(wind->treeview);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(gnc_ui_qif_account_picker_changed_cb), wind);
    g_signal_connect(wind->treeview, "row-activated",
                     G_CALLBACK(gnc_ui_qif_account_picker_row_activated_cb),
                     wind);
  }

  g_signal_connect_after(wind->dialog, "map",
                         G_CALLBACK(gnc_ui_qif_account_picker_map_cb),
                         wind);

  button = glade_xml_get_widget(xml, "newbutton");
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

  if (response == GTK_RESPONSE_OK)
    return new_entry;

  return SCM_BOOL_F;
}

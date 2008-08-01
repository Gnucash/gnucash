/*
 * dialog-billterms.c -- Dialog to create and edit billing terms
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"

#include "gncBillTerm.h"
#include "dialog-billterms.h"

#define DIALOG_BILLTERMS_CM_CLASS "billterms-dialog"

enum term_cols {
  BILL_TERM_COL_NAME = 0,
  BILL_TERM_COL_TERM,
  NUM_BILL_TERM_COLS
};

void billterms_new_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_edit_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_window_close (GtkWidget *widget, gpointer data);
void billterms_window_destroy_cb (GtkWidget *widget, gpointer data);
void billterms_type_combobox_changed (GtkComboBox *cb, gpointer data);

typedef struct _billterm_notebook {
  GtkTooltips *		tooltips;
  GtkWidget *		notebook;

  /* "Days" widgets */
  GtkWidget *		days_due_days;
  GtkWidget *		days_disc_days;
  GtkWidget *		days_disc;

  /* "Proximo" widgets */
  GtkWidget *		prox_due_day;
  GtkWidget *		prox_disc_day;
  GtkWidget *		prox_disc;
  GtkWidget *		prox_cutoff;

  /* What kind of term is this? */
  GncBillTermType	type;
} BillTermNB;

struct _billterms_window {
  GtkWidget *	dialog;
  GtkWidget *	terms_view;
  GtkWidget *	desc_entry;
  GtkWidget *	type_label;
  GtkWidget *	term_vbox;
  BillTermNB	notebook;

  GncBillTerm *	current_term;
  GNCBook *	book;
  gint		component_id;
};

typedef struct _new_billterms {
  GtkWidget *	dialog;
  GtkWidget *	name_entry;
  GtkWidget *	desc_entry;
  BillTermNB	notebook;

  BillTermsWindow *	btw;
  GncBillTerm *	this_term;
} NewBillTerm;


static GtkWidget *
read_widget (GladeXML *xml, char *name, gboolean read_only)
{
  GtkWidget *widget = glade_xml_get_widget (xml, name);
  if (read_only) {
    GtkAdjustment *adj;
    gtk_editable_set_editable (GTK_EDITABLE (widget), FALSE);
    adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON (widget));
    adj->step_increment = 0.0;
    adj->page_increment = 0.0;
    gtk_adjustment_changed (adj);
  }

  return widget;
}

/* NOTE: The caller needs to unref once they attach */
static void
init_notebook_widgets (BillTermNB *notebook, gboolean read_only,
		       GtkDialog *dialog, gpointer user_data)
{
  GladeXML *xml;
  GtkWidget *parent;

  /* Initialize the tooltips */
  notebook->tooltips = gtk_tooltips_new ();

  /* Load the notebook from XML */
  xml = gnc_glade_xml_new ("billterms.glade", "Term Notebook");
  notebook->notebook = glade_xml_get_widget (xml, "term_notebook");
  parent = glade_xml_get_widget (xml, "Term Notebook");

  /* load the "days" widgets */
  notebook->days_due_days = read_widget (xml, "days:due_days", read_only);
  notebook->days_disc_days = read_widget (xml, "days:discount_days", read_only);
  notebook->days_disc = read_widget (xml, "days:discount", read_only);

  /* load the "proximo" widgets */
  notebook->prox_due_day = read_widget (xml, "prox:due_day", read_only);
  notebook->prox_disc_day = read_widget (xml, "prox:discount_day", read_only);
  notebook->prox_disc = read_widget (xml, "prox:discount", read_only);
  notebook->prox_cutoff = read_widget (xml, "prox:cutoff_day", read_only);

  /* Disconnect the notebook from the window */
  g_object_ref (notebook->notebook);
  gtk_container_remove (GTK_CONTAINER (parent), notebook->notebook);
  gtk_widget_destroy (parent);

  /* NOTE: The caller needs to unref once they attach */
}

static void
set_numeric (GtkWidget *widget, GncBillTerm *term,
	     void (*func)(GncBillTerm *, gnc_numeric))
{
  gnc_numeric val;
  gdouble fl = 0.0;

  fl = gtk_spin_button_get_value (GTK_SPIN_BUTTON (widget));
  val = double_to_gnc_numeric (fl, 100000, GNC_RND_ROUND);
  func (term, val);
}

static void
get_numeric (GtkWidget *widget, GncBillTerm *term,
	     gnc_numeric (*func)(const GncBillTerm *))
{
  gnc_numeric val;
  gdouble fl;

  val = func (term);
  fl = gnc_numeric_to_double (val);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (widget), fl);
}

static void
set_int (GtkWidget *widget, GncBillTerm *term,
	     void (*func)(GncBillTerm *, gint))
{
  gint val;

  val = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (widget));
  func (term, val);
}

static void
get_int (GtkWidget *widget, GncBillTerm *term,
	     gint (*func)(const GncBillTerm *))
{
  gint val;

  val = func (term);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (widget), (gfloat)val);
}

/* return TRUE if anything truly changed */
static gboolean
ui_to_billterm (NewBillTerm *nbt)
{
  BillTermNB *notebook;
  GncBillTerm *term;
  const char *text;

  term = nbt->this_term;
  notebook = &nbt->notebook;

  text = gtk_entry_get_text (GTK_ENTRY (nbt->desc_entry));
  if (text)
    gncBillTermSetDescription (term, text);

  gncBillTermSetType (nbt->this_term, nbt->notebook.type);

  switch (nbt->notebook.type) {
  case GNC_TERM_TYPE_DAYS:
    set_int (notebook->days_due_days, term, gncBillTermSetDueDays);
    set_int (notebook->days_disc_days, term, gncBillTermSetDiscountDays);
    set_numeric (notebook->days_disc, term, gncBillTermSetDiscount);
    break;

  case GNC_TERM_TYPE_PROXIMO:
    set_int (notebook->prox_due_day, term, gncBillTermSetDueDays);
    set_int (notebook->prox_disc_day, term, gncBillTermSetDiscountDays);
    set_numeric (notebook->prox_disc, term, gncBillTermSetDiscount);
    set_int (notebook->prox_cutoff, term, gncBillTermSetCutoff);
    break;
  }

  return gncBillTermIsDirty (term);
}

static void
billterm_to_ui (GncBillTerm *term, GtkWidget *desc, BillTermNB *notebook)
{
  gtk_entry_set_text (GTK_ENTRY (desc), gncBillTermGetDescription (term));
  notebook->type = gncBillTermGetType (term);

  switch (notebook->type) {
  case GNC_TERM_TYPE_DAYS:
    get_int (notebook->days_due_days, term, gncBillTermGetDueDays);
    get_int (notebook->days_disc_days, term, gncBillTermGetDiscountDays);
    get_numeric (notebook->days_disc, term, gncBillTermGetDiscount);
    break;

  case GNC_TERM_TYPE_PROXIMO:
    get_int (notebook->prox_due_day, term, gncBillTermGetDueDays);
    get_int (notebook->prox_disc_day, term, gncBillTermGetDiscountDays);
    get_numeric (notebook->prox_disc, term, gncBillTermGetDiscount);
    get_int (notebook->prox_cutoff, term, gncBillTermGetCutoff);
    break;
  }
}

static gboolean
verify_term_ok (NewBillTerm *nbt)
{
  char *message;
  gnc_numeric num;

  return TRUE;

  /* verify the discount day(s) is less than the due day(s) */
  num = gnc_numeric_zero ();
  if (gnc_numeric_negative_p (num)) {
    message = _("Negative amounts are not allowed.");
    gnc_error_dialog (nbt->dialog, "%s", message);
    return FALSE;
  }
  if (gnc_numeric_compare (num, gnc_numeric_create (100, 1)) > 0) {
    message = _("Percentage amount must be between 0 and 100.");
    gnc_error_dialog (nbt->dialog, "%s", message);
    return FALSE;
  }
  return TRUE;
}

static gboolean
new_billterm_ok_cb (NewBillTerm *nbt)
{
  BillTermsWindow *btw;
  const char *name = NULL;
  char *message;

  g_return_val_if_fail (nbt, FALSE);
  btw = nbt->btw;

  /* Verify that we've got real, valid data */

  /* verify the name, maybe */
  if (nbt->this_term == NULL) {
    name = gtk_entry_get_text (GTK_ENTRY (nbt->name_entry));
    if (name == NULL || *name == '\0') {
      message = _("You must provide a name for this Billing Term.");
      gnc_error_dialog (nbt->dialog, "%s", message);
      return FALSE;
    }
    if (gncBillTermLookupByName (btw->book, name)) {
      message = g_strdup_printf(_(
			 "You must provide a unique name for this Billing Term. "
			 "Your choice \"%s\" is already in use."), name);
      gnc_error_dialog (nbt->dialog, "%s", message);
      g_free (message);
      return FALSE;
    }
  }

  /* Verify the actual data */
  if (!verify_term_ok (nbt))
    return FALSE;

  gnc_suspend_gui_refresh ();

  /* Ok, it's all valid, now either change or add this thing */
  if (nbt->this_term == NULL) {
    nbt->this_term = gncBillTermCreate (btw->book);
    gncBillTermBeginEdit (nbt->this_term);
    gncBillTermSetName (nbt->this_term, name);
    /* Reset the current term */
    btw->current_term = nbt->this_term;
  } else
    gncBillTermBeginEdit (btw->current_term);

  /* Fill in the rest of the term */
  if (ui_to_billterm (nbt))
    gncBillTermChanged (btw->current_term);

  /* Mark the table as changed and commit it */
  gncBillTermCommitEdit (btw->current_term);

  gnc_resume_gui_refresh();
  return TRUE;
}

static void
show_notebook (BillTermNB *notebook)
{
  g_return_if_fail (notebook->type > 0);
  gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook->notebook),
				 notebook->type-1);
}

static void
maybe_set_type (NewBillTerm *nbt, GncBillTermType type)
{
  /* See if anything to do? */
  if (type == nbt->notebook.type)
    return;

  /* Yep.  Let's refresh */
  nbt->notebook.type = type;
  show_notebook (&nbt->notebook);
}

void
billterms_type_combobox_changed (GtkComboBox *cb, gpointer data)
{
  NewBillTerm *nbt = data;
  gint value;

  value = gtk_combo_box_get_active(cb);
  maybe_set_type (nbt, value + 1);
}

static GncBillTerm *
new_billterm_dialog (BillTermsWindow *btw, GncBillTerm *term,
		     const char *name)
{
  GncBillTerm *created_term = NULL;
  NewBillTerm *nbt;
  GladeXML *xml;
  GtkWidget *box, *combo_box;
  gint response;
  gboolean done;
  const gchar *dialog_name;

  if (!btw) return NULL;

  nbt = g_new0 (NewBillTerm, 1);
  nbt->btw = btw;
  nbt->this_term = term;

  /* Open and read the XML */
  dialog_name = term ? "Edit Term Dialog" : "New Term Dialog";
  xml = gnc_glade_xml_new ("billterms.glade", dialog_name);
  nbt->dialog = glade_xml_get_widget (xml, dialog_name);
  nbt->name_entry = glade_xml_get_widget (xml, "name_entry");
  nbt->desc_entry = glade_xml_get_widget (xml, "desc_entry");
  if (name)
    gtk_entry_set_text (GTK_ENTRY (nbt->name_entry), name);

  /* Initialize the notebook widgets */
  init_notebook_widgets (&nbt->notebook, FALSE,
			 GTK_DIALOG (nbt->dialog), nbt);

  /* Attach the notebook */
  box = glade_xml_get_widget (xml, "notebook_box");
  gtk_box_pack_start (GTK_BOX (box), nbt->notebook.notebook, TRUE, TRUE, 0);
  g_object_unref (nbt->notebook.notebook);

  /* Fill in the widgets appropriately */
  if (term)
    billterm_to_ui (term, nbt->desc_entry, &nbt->notebook);
  else
    nbt->notebook.type = GNC_TERM_TYPE_DAYS;

  /* Create the menu */
  combo_box = glade_xml_get_widget (xml, "type_combobox");
  gtk_combo_box_set_active(GTK_COMBO_BOX(combo_box), nbt->notebook.type - 1);

  /* Show the right notebook page */
  show_notebook (&nbt->notebook);

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     nbt);

  gtk_window_set_transient_for (GTK_WINDOW(nbt->dialog),
				GTK_WINDOW(btw->dialog));

  /* Show what we should */
  gtk_widget_show_all (nbt->dialog);
  if (term) {
    gtk_widget_grab_focus (nbt->desc_entry);
  } else
    gtk_widget_grab_focus (nbt->name_entry);

  done = FALSE;
  while (!done) {
    response = gtk_dialog_run (GTK_DIALOG (nbt->dialog));
    switch (response) {
     case GTK_RESPONSE_OK:
      if (new_billterm_ok_cb (nbt)) {
	created_term = nbt->this_term;
	done = TRUE;
      }
      break;
     default:
      done = TRUE;
      break;
    }
  }

  gtk_widget_destroy(nbt->dialog);
  g_free(nbt);

  return created_term;
}

/***********************************************************************/

static void
billterms_term_refresh (BillTermsWindow *btw)
{
  char *type_label;

  g_return_if_fail (btw);

  if (!btw->current_term) {
    gtk_widget_hide_all (btw->term_vbox);
    return;
  }

  gtk_widget_show_all (btw->term_vbox);
  billterm_to_ui (btw->current_term, btw->desc_entry, &btw->notebook);
  switch (gncBillTermGetType (btw->current_term)) {
  case GNC_TERM_TYPE_DAYS:
    type_label = _("Days");
    break;
  case GNC_TERM_TYPE_PROXIMO:
    type_label = _("Proximo");
    break;
  default:
    type_label = _("Unknown");
    break;
  }
  show_notebook (&btw->notebook);
  gtk_label_set_text (GTK_LABEL (btw->type_label), type_label);
}

static void
billterms_window_refresh (BillTermsWindow *btw)
{
  GList *list, *node;
  GncBillTerm *term;
  GtkTreeView *view;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeSelection *selection;
  GtkTreeRowReference *reference = NULL;

  g_return_if_fail (btw);
  view = GTK_TREE_VIEW (btw->terms_view);
  store = GTK_LIST_STORE(gtk_tree_view_get_model(view));

  /* Clear the list */
  gtk_list_store_clear (store);
  gnc_gui_component_clear_watches (btw->component_id);

  /* Add the items to the list */
  list = gncBillTermGetTerms (btw->book);

  /* If there are no terms, clear the term display */
  if (list == NULL) {
    btw->current_term = NULL;
    billterms_term_refresh (btw);
  } else {
    list = g_list_reverse (g_list_copy (list));
  }

  for ( node = list; node; node = node->next) {
    term = node->data;
    gnc_gui_component_watch_entity (btw->component_id,
				    gncBillTermGetGUID (term),
				    QOF_EVENT_MODIFY);

    gtk_list_store_prepend(store, &iter);
    gtk_list_store_set(store, &iter,
		       BILL_TERM_COL_NAME, gncBillTermGetName(term),
		       BILL_TERM_COL_TERM, term,
		       -1);
    if (term == btw->current_term) {
      path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
      reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
      gtk_tree_path_free(path);
    }
  }

  g_list_free (list);

  gnc_gui_component_watch_entity_type (btw->component_id,
				       GNC_BILLTERM_MODULE_NAME,
				       QOF_EVENT_CREATE | QOF_EVENT_DESTROY);

  if (reference) {
    path = gtk_tree_row_reference_get_path(reference);
    gtk_tree_row_reference_free(reference);
    if (path) {
      selection = gtk_tree_view_get_selection(view);
      gtk_tree_selection_select_path(selection, path);
      gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
      gtk_tree_path_free(path);
    }
  }
}

static void
billterm_selection_changed (GtkTreeSelection *selection,
			    BillTermsWindow  *btw) 
{
  GncBillTerm *term = NULL;
  GtkTreeModel *model;
  GtkTreeIter iter;

  g_return_if_fail (btw);

  if (gtk_tree_selection_get_selected(selection, &model, &iter))
    gtk_tree_model_get(model, &iter, BILL_TERM_COL_TERM, &term, -1);

  /* If we've changed, then reset the term list */
  if (term != btw->current_term)
    btw->current_term = term;

  /* And force a refresh of the entries */
  billterms_term_refresh (btw);
}

static void
billterm_selection_activated (GtkTreeView       *tree_view,
			      GtkTreePath       *path,
			      GtkTreeViewColumn *column,
			      BillTermsWindow   *btw)
{
  new_billterm_dialog (btw, btw->current_term, NULL);
}

void
billterms_new_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);
  new_billterm_dialog (btw, NULL, NULL);
}

void
billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);

  if (!btw->current_term)
    return;

  if (gncBillTermGetRefcount (btw->current_term) > 0) {
    gnc_error_dialog (btw->dialog,
		      _("Term \"%s\" is in use.  You cannot delete it."),
		      gncBillTermGetName (btw->current_term));
    return;
  }

  if (gnc_verify_dialog (btw->dialog, FALSE,
			 _("Are you sure you want to delete \"%s\"?"),
			 gncBillTermGetName (btw->current_term))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
    gncBillTermBeginEdit (btw->current_term);
    gncBillTermDestroy (btw->current_term);
    btw->current_term = NULL;
    gnc_resume_gui_refresh ();
  }
}

void
billterms_edit_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);
  if (!btw->current_term)
    return;
  new_billterm_dialog (btw, btw->current_term, NULL);
}

static void
billterms_window_refresh_handler (GHashTable *changes, gpointer data)
{
  BillTermsWindow *btw = data;

  g_return_if_fail (data);
  billterms_window_refresh (btw);
}

static void
billterms_window_close_handler (gpointer data)
{
  BillTermsWindow *btw = data;
  g_return_if_fail (btw);

  gtk_widget_destroy (btw->dialog);
}

void
billterms_window_close (GtkWidget *widget, gpointer data)
{
  BillTermsWindow *btw = data;

  gnc_ui_billterms_window_destroy (btw);
}

void
billterms_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  BillTermsWindow *btw = data;

  if (!btw) return;

  gnc_unregister_gui_component (btw->component_id);

  g_free (btw);
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
  BillTermsWindow *btw = user_data;
  GNCBook *book = find_data;

  return (btw != NULL && btw->book == book);
}

/* Create a billterms window */
BillTermsWindow *
gnc_ui_billterms_window_new (GNCBook *book)
{
  BillTermsWindow *btw;
  GladeXML *xml;
  GtkWidget *widget;
  GtkTreeView *view;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkListStore *store;
  GtkTreeSelection *selection;

  if (!book) return NULL;

  /*
   * Find an existing billterm window.  If found, bring it to
   * the front.  If we have an actual owner, then set it in
   * the window.
   */
  btw = gnc_find_first_gui_component (DIALOG_BILLTERMS_CM_CLASS,
				      find_handler, book);
  if (btw) {
    gtk_window_present (GTK_WINDOW(btw->dialog));
    return btw;
  }

  /* Didn't find one -- create a new window */
  btw = g_new0 (BillTermsWindow, 1);
  btw->book = book;

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("billterms.glade", "Terms Window");
  btw->dialog = glade_xml_get_widget (xml, "Terms Window");
  btw->terms_view = glade_xml_get_widget (xml, "terms_view");
  btw->desc_entry = glade_xml_get_widget (xml, "desc_entry");
  btw->type_label = glade_xml_get_widget (xml, "type_label");
  btw->term_vbox = glade_xml_get_widget (xml, "term_vbox");

  /* Initialize the view */
  view = GTK_TREE_VIEW(btw->terms_view);
  store = gtk_list_store_new (NUM_BILL_TERM_COLS, G_TYPE_STRING, G_TYPE_POINTER);
  gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
  g_object_unref(store);

  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("", renderer,
						    "text", BILL_TERM_COL_NAME,
						    NULL);
  gtk_tree_view_append_column(view, column);

  g_signal_connect(view, "row-activated",
		   G_CALLBACK(billterm_selection_activated), btw);
  selection = gtk_tree_view_get_selection(view);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(billterm_selection_changed), btw);

  /* Initialize the notebook widgets */
  init_notebook_widgets (&btw->notebook, TRUE,
			 GTK_DIALOG (btw->dialog), btw);

  /* Attach the notebook */
  widget = glade_xml_get_widget (xml, "notebook_box");
  gtk_box_pack_start (GTK_BOX (widget), btw->notebook.notebook,
		      TRUE, TRUE, 0);
  g_object_unref (btw->notebook.notebook);

  /* Setup signals */
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     btw);

  /* register with component manager */
  btw->component_id =
    gnc_register_gui_component (DIALOG_BILLTERMS_CM_CLASS,
				billterms_window_refresh_handler,
				billterms_window_close_handler,
				btw);

  gtk_widget_show_all (btw->dialog);
  billterms_window_refresh (btw);

  return btw;
}

/* Destroy a billterms window */
void
gnc_ui_billterms_window_destroy (BillTermsWindow *btw)
{
  if (!btw)
    return;

  gnc_close_gui_component (btw->component_id);
}

#if 0
/* Create a new billterms by name */
GncBillTerm *
gnc_ui_billterms_new_from_name (GNCBook *book, const char *name)
{
  BillTermsWindow *btw;

  if (!book) return NULL;

  btw = gnc_ui_billterms_window_new (book);
  if (!btw) return NULL;

  return new_billterm_dialog (btw, NULL, name);
}
#endif

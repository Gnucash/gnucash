/*
 * dialog-billterms.c -- Dialog to create and edit billing terms
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"

#include "gncBillTerm.h"
#include "dialog-billterms.h"

#define DIALOG_BILLTERMS_CM_CLASS "billterms-dialog"

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
  GtkWidget *	terms_clist;
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
		       GnomeDialog *dialog, gpointer user_data)
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
  gtk_object_ref (GTK_OBJECT (notebook->notebook));
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

  fl = gtk_spin_button_get_value_as_float (GTK_SPIN_BUTTON (widget));
  val = double_to_gnc_numeric (fl, 100000, GNC_RND_ROUND);
  func (term, val);
}

static void
get_numeric (GtkWidget *widget, GncBillTerm *term,
	     gnc_numeric (*func)(GncBillTerm *))
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
	     gint (*func)(GncBillTerm *))
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
  char *text;

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
    gnc_error_dialog_parented (GTK_WINDOW (nbt->dialog), message);
    return FALSE;
  }
  if (gnc_numeric_compare (num, gnc_numeric_create (100, 1)) > 0) {
    message = _("Percentage amount must be between 0 and 100.");
    gnc_error_dialog_parented (GTK_WINDOW (nbt->dialog), message);
    return FALSE;
  }
  return TRUE;
}

static void
new_billterm_ok_cb (GtkWidget *widget, gpointer data)
{
  NewBillTerm *nbt = data;
  BillTermsWindow *btw;
  char *name = NULL;
  char *message;

  g_return_if_fail (nbt);
  btw = nbt->btw;

  /* Verify that we've got real, valid data */

  /* verify the name, maybe */
  if (nbt->this_term == NULL) {
    name = gtk_entry_get_text (GTK_ENTRY (nbt->name_entry));
    if (name == NULL || *name == '\0') {
      message = _("You must provide a name for this Billing Term.");
      gnc_error_dialog_parented (GTK_WINDOW (nbt->dialog), message);
      return;
    }
    if (gncBillTermLookupByName (btw->book, name)) {
      message = g_strdup_printf(_(
			 "You must provide a unique name for this Billing Term.\n"
			 "Your choice \"%s\" is already in use."), name);
      gnc_error_dialog_parented (GTK_WINDOW (nbt->dialog), message);
      g_free (message);
      return;
    }
  }

  /* Verify the actual data */
  if (!verify_term_ok (nbt))
    return;

  gnc_suspend_gui_refresh ();

  /* Ok, it's all valid, now either change or add this thing */
  if (nbt->this_term == NULL) {
    nbt->this_term = gncBillTermCreate (btw->book);
    gncBillTermSetName (nbt->this_term, name);
    /* Reset the current term */
    btw->current_term = nbt->this_term;
  }

  /* Fill in the rest of the term */
  if (ui_to_billterm (nbt))
    gncBillTermChanged (btw->current_term);

  /* Mark the table as changed and commit it */
  gncBillTermCommitEdit (btw->current_term);

  gnc_resume_gui_refresh();

  /* Then close/refresh the dialog/window */
  gnome_dialog_close (GNOME_DIALOG (nbt->dialog));
}

static void
new_billterm_cancel_cb (GtkWidget *widget, gpointer data)
{
  NewBillTerm *nbt = data;
  g_return_if_fail (nbt);
  gnome_dialog_close (GNOME_DIALOG (nbt->dialog));
}

static void
new_billterm_dialog_destroy_cb (GtkWidget *widget, gpointer data)
{
  NewBillTerm *nbt = data;

  if (!nbt) return;
  g_free (nbt);
}

static void
show_notebook (BillTermNB *notebook)
{
  g_return_if_fail (notebook->type > 0);
  gtk_notebook_set_page (GTK_NOTEBOOK (notebook->notebook), notebook->type-1);
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

static void
on_days1_activate (GtkWidget *w, gpointer data)
{
  NewBillTerm *nbt = data;

  maybe_set_type (nbt, GNC_TERM_TYPE_DAYS);
}

static void
on_proximo1_activate (GtkWidget *w, gpointer data)
{
  NewBillTerm *nbt = data;

  maybe_set_type (nbt, GNC_TERM_TYPE_PROXIMO);
}

static void
make_menu (GtkWidget *omenu, NewBillTerm *nbt)
{
  GladeXML *xml;
  GtkWidget *popup;

  /* Open and read the Popup XML */
  xml = gnc_glade_xml_new ("billterms.glade", "Term Type Popup");
  popup = glade_xml_get_widget (xml, "Term Type Popup");

  /* Glade insists on making this a tearoff menu. */
  if (gnome_preferences_get_menus_have_tearoff ()) {
    GtkMenuShell *ms = GTK_MENU_SHELL (popup);
    GtkWidget *tearoff;

    tearoff = g_list_nth_data (ms->children, 0);
    ms->children = g_list_remove (ms->children, tearoff);
    gtk_widget_destroy (tearoff);
  }

  /* attach the signal handlers */
  glade_xml_signal_connect_data (xml, "on_days1_activate",
				 on_days1_activate, nbt);
  glade_xml_signal_connect_data (xml, "on_proximo1_activate",
				 on_proximo1_activate, nbt);

  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), popup);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu),
			       nbt->notebook.type - 1);
}

static NewBillTerm *
new_billterm_dialog (BillTermsWindow *btw, GncBillTerm *term)
{
  NewBillTerm *nbt;
  GladeXML *xml;
  GtkWidget *box, *widget;

  if (!btw) return NULL;

  nbt = g_new0 (NewBillTerm, 1);
  nbt->btw = btw;
  nbt->this_term = term;

  /* Open and read the XML */
  xml = gnc_glade_xml_new ("billterms.glade", "New Term Dialog");
  nbt->dialog = glade_xml_get_widget (xml, "New Term Dialog");
  nbt->name_entry = glade_xml_get_widget (xml, "name_entry");
  nbt->desc_entry = glade_xml_get_widget (xml, "desc_entry");

  /* Initialize the notebook widgets */
  init_notebook_widgets (&nbt->notebook, FALSE,
			 GNOME_DIALOG (nbt->dialog), nbt);

  /* Attach the notebook */
  box = glade_xml_get_widget (xml, "notebook_box");
  gtk_box_pack_start (GTK_BOX (box), nbt->notebook.notebook, TRUE, TRUE, 0);
  gtk_object_unref (GTK_OBJECT (nbt->notebook.notebook));

  /* Fill in the widgets appropriately */
  if (term)
    billterm_to_ui (term, nbt->desc_entry, &nbt->notebook);
  else
    nbt->notebook.type = GNC_TERM_TYPE_DAYS;

  /* Create the menu */
  make_menu (glade_xml_get_widget (xml, "type_menu"), nbt);

  /* Show the right notebook page */
  show_notebook (&nbt->notebook);

  /* Make 'enter' do the right thing */
  gnome_dialog_set_default (GNOME_DIALOG (nbt->dialog), 0);
  gnome_dialog_editable_enters (GNOME_DIALOG (nbt->dialog),
				GTK_EDITABLE (nbt->name_entry));
  gnome_dialog_editable_enters (GNOME_DIALOG (nbt->dialog),
				GTK_EDITABLE (nbt->desc_entry));

  /* Connect the dialog buttons */
  gnome_dialog_button_connect (GNOME_DIALOG (nbt->dialog), 0,
			       new_billterm_ok_cb, nbt);

  gnome_dialog_button_connect (GNOME_DIALOG (nbt->dialog), 1,
			       new_billterm_cancel_cb, nbt);

  /* Set our modality */
  gnome_dialog_set_parent (GNOME_DIALOG (nbt->dialog),
			   GTK_WINDOW (btw->dialog));
  gtk_window_set_modal (GTK_WINDOW (nbt->dialog), TRUE);

  gtk_signal_connect (GTK_OBJECT (nbt->dialog), "destroy",
		      new_billterm_dialog_destroy_cb, nbt);


  /* Show what we should */
  gtk_widget_show_all (nbt->dialog);
  if (term) {
    widget = glade_xml_get_widget (xml, "term_frame");
    gtk_widget_hide_all (widget);
    gtk_widget_grab_focus (nbt->desc_entry);
  } else
    gtk_widget_grab_focus (nbt->name_entry);

  return nbt;
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
  GList *list;
  GtkAdjustment *vadjustment;
  GtkCList *clist;
  gfloat save_value = 0.0;

  g_return_if_fail (btw);
  clist = GTK_CLIST (btw->terms_clist);

  vadjustment = gtk_clist_get_vadjustment (clist);
  if (vadjustment)
    save_value = vadjustment->value;

  /* Clear the list */
  gtk_clist_freeze (clist);
  gtk_clist_clear (clist);

  gnc_gui_component_clear_watches (btw->component_id);

  /* Add the items to the list */
  list = gncBillTermGetTerms (btw->book);

  /* If there are no erms, clear the term display */
  if (list == NULL) {
    btw->current_term = NULL;
    billterms_term_refresh (btw);
  }

  for ( ; list; list = list->next) {
    char *row_text[2];
    gint row;
    GncBillTerm *term = list->data;

    gnc_gui_component_watch_entity (btw->component_id,
				    gncBillTermGetGUID (term),
				    GNC_EVENT_MODIFY);

    row_text[0] = (char *)gncBillTermGetName (term);
    row_text[1] = NULL;

    row = gtk_clist_prepend (clist, row_text);
    gtk_clist_set_row_data (clist, row, term);
    gtk_clist_set_selectable (clist, row, TRUE);
  }

  gnc_gui_component_watch_entity_type (btw->component_id,
				       GNC_BILLTERM_MODULE_NAME,
				       GNC_EVENT_CREATE | GNC_EVENT_DESTROY);

  if (vadjustment) {
    save_value = CLAMP (save_value, vadjustment->lower,
			vadjustment->upper - vadjustment->page_size);
    gtk_adjustment_set_value (vadjustment, save_value);
  }

  gtk_clist_thaw (clist);

  {
    gint row = gtk_clist_find_row_from_data (clist, btw->current_term);

    if (row < 0)
      row = 0;

    gtk_clist_select_row (clist, row, 0);

    /* If this row isn't visible, move it to the center */
    if (gtk_clist_row_is_visible (clist, row) != GTK_VISIBILITY_FULL)
      gtk_clist_moveto (clist, row, 0, 0.5, 0);
  }
  /* select_row() above will refresh the term display */
}

static void
billterms_row_selected (GtkCList *clist, gint row, gint column,
			GdkEventButton *event, gpointer user_data)
{
  BillTermsWindow *btw = user_data;
  GncBillTerm *term = gtk_clist_get_row_data (clist, row);

  g_return_if_fail (btw);
  g_return_if_fail (term);

  /* If we've changed, then reset the term list */
  if (term != btw->current_term)
    btw->current_term = term;

  /* And force a refresh of the entries */
  billterms_term_refresh (btw);

  /* If the user double-clicked on the item, pop up the edit window */
  if (event && event->type == GDK_2BUTTON_PRESS)
    new_billterm_dialog (btw, term);
}

static void
billterms_new_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);
  new_billterm_dialog (btw, NULL);
}

static void
billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);

  if (!btw->current_term)
    return;

  if (gncBillTermGetRefcount (btw->current_term) > 0) {
    char *message =
      g_strdup_printf (_("Term \"%s\" is in use.  You cannot delete it."),
		       gncBillTermGetName (btw->current_term));
    gnc_error_dialog_parented (GTK_WINDOW (btw->dialog), message);
    g_free (message);
    return;
  }

  if (gnc_verify_dialog_parented (btw->dialog, FALSE,
				  _("Are you sure you want to delete \"%s\"?"),
				  gncBillTermGetName (btw->current_term))) {
    /* Ok, let's remove it */
    gnc_suspend_gui_refresh ();
    gncBillTermDestroy (btw->current_term);
    gncBillTermCommitEdit (btw->current_term);
    btw->current_term = NULL;
    gnc_resume_gui_refresh ();
  }
}

static void
billterms_edit_term_cb (GtkButton *button, BillTermsWindow *btw)
{
  g_return_if_fail (btw);
  if (!btw->current_term)
    return;
  new_billterm_dialog (btw, btw->current_term);
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
  gnome_dialog_close (GNOME_DIALOG (btw->dialog));
}

static void
billterms_window_close (GtkWidget *widget, gpointer data)
{
  BillTermsWindow *btw = data;

  gnc_ui_billterms_window_destroy (btw);
}

static void
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
  GtkWidget *button, *widget;

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
  btw->terms_clist = glade_xml_get_widget (xml, "terms_clist");
  btw->desc_entry = glade_xml_get_widget (xml, "desc_entry");
  btw->type_label = glade_xml_get_widget (xml, "type_label");
  btw->term_vbox = glade_xml_get_widget (xml, "term_vbox");

  gnome_dialog_editable_enters (GNOME_DIALOG (btw->dialog),
				GTK_EDITABLE (btw->desc_entry));

  /* Initialize the notebook widgets */
  init_notebook_widgets (&btw->notebook, TRUE,
			 GNOME_DIALOG (btw->dialog), btw);

  /* Attach the notebook */
  widget = glade_xml_get_widget (xml, "notebook_box");
  gtk_box_pack_start (GTK_BOX (widget), btw->notebook.notebook,
		      TRUE, TRUE, 0);
  gtk_object_unref (GTK_OBJECT (btw->notebook.notebook));

  /* Connect all the buttons */
  button = glade_xml_get_widget (xml, "new_term_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      billterms_new_term_cb, btw);
  button = glade_xml_get_widget (xml, "delete_term_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      billterms_delete_term_cb, btw);
  button = glade_xml_get_widget (xml, "edit_term_button");
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      billterms_edit_term_cb, btw);

  /* Set the row-select callbacks */
  gtk_signal_connect (GTK_OBJECT (btw->terms_clist), "select-row",
		      billterms_row_selected, btw);

  /* Connect the dialog buttons */
  gnome_dialog_button_connect (GNOME_DIALOG (btw->dialog), 0,
			       billterms_window_close, btw);

  gtk_signal_connect (GTK_OBJECT (btw->dialog), "destroy",
		      billterms_window_destroy_cb, btw);

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
static int
from_name_close_cb (GnomeDialog *dialog, gpointer data)
{
  NewBillTerm *nbt;
  GncBillTerm **created_term = data;

  nbt = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");

  *created_term = nbt->created_term;

  gtk_main_quit ();

  return FALSE;
}

/* Create a new billterms by name */
GncBillTerm *
gnc_ui_billterms_new_from_name (GNCBook *book, const char *name)
{
  GncBillTerm *created_term = NULL;
  BillTermsWindow *btw;
  NewBillTerm *nbt;

  if (!book) return NULL;

  btw = gnc_ui_billterms_window_new (book);
  if (!btw) return NULL;

  nbt = new_billterm_dialog (btw, NULL);
  if (!nbt) return NULL;

  gtk_object_set_data (GTK_OBJECT (nbt->dialog), "dialog_info", nbt);
  gtk_signal_connect (GTK_OBJECT (nbt->dialog), "close",
		      GTK_SIGNAL_FUNC (from_name_close_cb), &created_term);

  /* Preset the name in the new dialog */
  if (name)
    gtk_entry_set_text (GTK_ENTRY (nbt->name_entry), name);

  /* I know that NBT is already modal, no need to reset it here */

  /* Now run the dialog -- wait for it to close */
  gtk_main ();

  return created_term;
}
#endif

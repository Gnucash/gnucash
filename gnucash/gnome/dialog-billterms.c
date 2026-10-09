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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"

#include "gncBillTerm.h"
#include "dialog-billterms.h"

#define DIALOG_BILLTERMS_CM_CLASS "billterms-dialog"
#define GNC_PREFS_GROUP           "dialogs.bill-terms"

enum term_cols
{
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

typedef struct _billterm_notebook
{
    GtkWidget *notebook;

    /* "Days" widgets */
    GtkWidget *days_due_days;
    GtkWidget *days_disc_days;
    GtkWidget *days_disc;

    /* "Proximo" widgets */
    GtkWidget *prox_due_day;
    GtkWidget *prox_disc_day;
    GtkWidget *prox_disc;
    GtkWidget *prox_cutoff;

    /* What kind of term is this? */
    GncBillTermType type;
} BillTermNB;

struct _billterms_window
{
    GtkWidget *window;
    GtkWidget *terms_view;
    GtkWidget *desc_entry;
    GtkWidget *type_label;
    GtkWidget *term_vbox;
    BillTermNB notebook;

    GncBillTerm *current_term;
    QofBook     *book;
    gint         component_id;
    QofSession  *session;
    guint        ref_count;
    gboolean     closing;
};

typedef struct _new_billterms
{
    GtkWidget *dialog;
    GtkWidget *name_entry;
    GtkWidget *desc_entry;
    BillTermNB notebook;

    BillTermsWindow *btw;
    gboolean         editing;
    GncGUID          book_guid;
    GncGUID          term_guid;
    gboolean         response_active;
    gboolean         dialog_destroyed;
} NewBillTerm;

typedef struct
{
    char *description;
    GncBillTermType type;
    gint due_days;
    gint discount_days;
    gint cutoff;
    gnc_numeric discount;
} BillTermValues;

static void new_billterm_response_cb (GtkDialog *dialog, gint response,
                                     NewBillTerm *nbt);

static BillTermsWindow *
billterms_window_ref (BillTermsWindow *btw)
{
    ++btw->ref_count;
    return btw;
}

static void
billterms_window_unref (BillTermsWindow *btw)
{
    if (--btw->ref_count)
        return;
    if (btw->book)
        g_object_remove_weak_pointer (G_OBJECT(btw->book), (gpointer *)&btw->book);
    g_free (btw);
}


static GtkWidget *
read_widget (GtkBuilder *builder, char *name, gboolean read_only)
{
    GtkWidget *widget = GTK_WIDGET(gtk_builder_get_object (builder, name));
    if (read_only)
    {
        GtkAdjustment *adj;
        gtk_editable_set_editable (GTK_EDITABLE(widget), FALSE);
        adj = gtk_spin_button_get_adjustment (GTK_SPIN_BUTTON(widget));
        gtk_adjustment_set_step_increment (adj, 0.0);
        gtk_adjustment_set_page_increment (adj, 0.0);
    }

    return widget;
}

/* NOTE: The caller needs to unref once they attach */
static void
init_notebook_widgets (BillTermNB *notebook, gboolean read_only,
                       gpointer user_data)
{
    GtkBuilder *builder;
    GtkWidget *parent;

    /* Load the notebook from Glade File */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "discount_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "discount_days_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "due_days_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "pdiscount_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "pdiscount_day_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "pdue_day_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "pcutoff_day_adj");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "terms_notebook_window");
    notebook->notebook = GTK_WIDGET(gtk_builder_get_object (builder, "term_notebook"));
    parent = GTK_WIDGET(gtk_builder_get_object (builder, "terms_notebook_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(notebook->notebook), "gnc-id-bill-term");
    gnc_widget_style_context_add_class (GTK_WIDGET(notebook->notebook), "gnc-class-bill-terms");

    /* load the "days" widgets */
    notebook->days_due_days = read_widget (builder, "days:due_days", read_only);
    notebook->days_disc_days = read_widget (builder, "days:discount_days", read_only);
    notebook->days_disc = read_widget (builder, "days:discount", read_only);

    /* load the "proximo" widgets */
    notebook->prox_due_day = read_widget (builder, "prox:due_day", read_only);
    notebook->prox_disc_day = read_widget (builder, "prox:discount_day", read_only);
    notebook->prox_disc = read_widget (builder, "prox:discount", read_only);
    notebook->prox_cutoff = read_widget (builder, "prox:cutoff_day", read_only);

    /* Disconnect the notebook from the window */
    g_object_ref (notebook->notebook);
    gtk_container_remove (GTK_CONTAINER(parent), notebook->notebook);
    g_object_unref (G_OBJECT(builder));
    gtk_widget_destroy (parent);

    /* NOTE: The caller needs to unref once they attach */
}

static void
get_numeric (GtkWidget *widget, GncBillTerm *term,
             gnc_numeric (*func)(const GncBillTerm *))
{
    gnc_numeric val;
    gdouble fl;

    val = func (term);
    fl = gnc_numeric_to_double (val);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(widget), fl);
}

static void
get_int (GtkWidget *widget, GncBillTerm *term,
         gint (*func)(const GncBillTerm *))
{
    gint val;

    val = func (term);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(widget), (gfloat)val);
}

static gnc_numeric
numeric_from_widget (GtkWidget *widget)
{
    return double_to_gnc_numeric (gtk_spin_button_get_value (GTK_SPIN_BUTTON(widget)),
                                  100000, GNC_HOW_RND_ROUND_HALF_UP);
}

static BillTermValues
billterm_values_from_ui (NewBillTerm *nbt)
{
    BillTermNB *nb = &nbt->notebook;
    BillTermValues values = {0};
    values.description = g_strdup (gtk_entry_get_text (GTK_ENTRY(nbt->desc_entry)));
    values.type = nb->type;
    if (values.type == GNC_TERM_TYPE_PROXIMO)
    {
        values.due_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(nb->prox_due_day));
        values.discount_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(nb->prox_disc_day));
        values.discount = numeric_from_widget (nb->prox_disc);
        values.cutoff = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(nb->prox_cutoff));
    }
    else
    {
        values.due_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(nb->days_due_days));
        values.discount_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(nb->days_disc_days));
        values.discount = numeric_from_widget (nb->days_disc);
    }
    return values;
}

static void
billterm_to_ui (GncBillTerm *term, GtkWidget *desc, BillTermNB *notebook)
{
    gtk_entry_set_text (GTK_ENTRY(desc), gncBillTermGetDescription (term));
    notebook->type = gncBillTermGetType (term);

    switch (notebook->type)
    {
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
verify_term_ok (NewBillTerm *nbt, const BillTermValues *values)
{
    if (values->due_days >= values->discount_days)
        return TRUE;
    gnc_error_dialog_async (GTK_WINDOW(nbt->dialog), "%s",
                            _("Discount days cannot be more than due days."));
    return FALSE;
}

static QofBook *
billterms_valid_book (BillTermsWindow *btw, const GncGUID *book_guid)
{
    QofSession *session;
    QofBook *book;

    book = btw->book;
    if (btw->closing || !gnc_current_session_exist ())
        return NULL;
    session = gnc_get_current_session ();
    if (!book || !session || session != btw->session ||
        qof_session_get_book (session) != book || !qof_book_is_open (book) ||
        qof_book_shutting_down (book) || qof_book_is_readonly (book) ||
        !guid_equal (book_guid, qof_book_get_guid (book)))
        return NULL;
    return book;
}

static QofBook *
new_billterm_valid_book (NewBillTerm *nbt)
{
    return billterms_valid_book (nbt->btw, &nbt->book_guid);
}

static QofBook *
new_billterm_live_book (NewBillTerm *nbt)
{
    QofBook *book = nbt->btw->book;
    if (!book || !qof_book_is_open (book) || qof_book_shutting_down (book) ||
        !guid_equal (&nbt->book_guid, qof_book_get_guid (book)))
        return NULL;
    return book;
}

static void
new_billterm_apply_values (GncBillTerm *term, const BillTermValues *values)
{
    gncBillTermSetDescription (term, values->description);
    gncBillTermSetType (term, values->type);
    gncBillTermSetDueDays (term, values->due_days);
    gncBillTermSetDiscountDays (term, values->discount_days);
    gncBillTermSetDiscount (term, values->discount);

    if (values->type == GNC_TERM_TYPE_PROXIMO)
        gncBillTermSetCutoff (term, values->cutoff);
}

static gboolean
new_billterm_ok_cb (NewBillTerm *nbt, const BillTermValues *values,
                    const char *name)
{
    QofBook *book;
    GncBillTerm *term;

    gnc_suspend_gui_refresh ();

    book = new_billterm_valid_book (nbt);
    if (!book)
    {
        gnc_resume_gui_refresh ();
        return FALSE;
    }

    if (nbt->editing)
        term = gncBillTermLookup (book, &nbt->term_guid);
    else
        term = gncBillTermCreate (book);

    /* gncBillTermCreate emits QOF_EVENT_CREATE synchronously. Do not inspect
       its result until the weak book/session guards have been checked again. */
    book = new_billterm_live_book (nbt);
    if (!book || !term)
    {
        gnc_resume_gui_refresh ();
        return FALSE;
    }
    if (!nbt->editing)
        nbt->term_guid = *gncBillTermGetGUID (term);
    term = gncBillTermLookup (book, &nbt->term_guid);
    if (!term)
    {
        gnc_resume_gui_refresh ();
        return FALSE;
    }

    gncBillTermBeginEdit (term);
    if (!nbt->editing)
        gncBillTermSetName (term, name);
    if (!nbt->btw->closing)
        nbt->btw->current_term = term;

    /* The outer edit defers engine events until the whole snapshot is applied.
     * Keep the resolved object through this non-reentrant section; abandoning
     * it midway would leave the QOF edit level open and partial values stored. */
    new_billterm_apply_values (term, values);
    gncBillTermChanged (term);
    gncBillTermCommitEdit (term);

    gnc_resume_gui_refresh ();
    return TRUE;
}

static void
new_billterm_free_context (GtkWidget *dialog, NewBillTerm *nbt)
{
    BillTermsWindow *btw = nbt->btw;
    g_signal_handlers_disconnect_by_func (dialog,
        G_CALLBACK(new_billterm_response_cb), nbt);
    g_free (nbt);
    billterms_window_unref (btw);
    g_object_unref (dialog);
}

static void
new_billterm_destroy_cb (GtkWidget *dialog, NewBillTerm *nbt)
{
    nbt->dialog_destroyed = TRUE;
    if (!nbt->response_active)
        new_billterm_free_context (dialog, nbt);
}

static void
new_billterm_handle_response (GtkDialog *dialog, gint response,
                              NewBillTerm *nbt)
{
    BillTermsWindow *btw = nbt->btw;
    QofBook *book;
    GncBillTerm *term = NULL;
    char *name = NULL;
    BillTermValues values;

    if (response != GTK_RESPONSE_OK || btw->closing)
    {
        gtk_widget_destroy (GTK_WIDGET(dialog));
        return;
    }

    book = new_billterm_valid_book (nbt);
    if (!book)
    {
        gtk_widget_destroy (GTK_WIDGET(dialog));
        return;
    }

    if (nbt->editing)
    {
        term = gncBillTermLookup (book, &nbt->term_guid);
        if (!term)
        {
            if (btw->window && !btw->closing)
                gnc_error_dialog_async (GTK_WINDOW(btw->window), "%s",
                                        _("This billing term no longer exists."));
            gtk_widget_destroy (GTK_WIDGET(dialog));
            return;
        }
    }
    else
    {
        name = g_strdup (gtk_entry_get_text (GTK_ENTRY(nbt->name_entry)));
        if (!name || !*name)
        {
            gnc_error_dialog_async (GTK_WINDOW(dialog), "%s",
                                   _("You must provide a name for this Billing Term."));
            g_free (name);
            return;
        }
        if (gncBillTermLookupByName (book, name))
        {
            char *message = g_strdup_printf (_(
                "You must provide a unique name for this Billing Term. "
                "Your choice \"%s\" is already in use."), name);
            gnc_error_dialog_async (GTK_WINDOW(dialog), "%s", message);
            g_free (message);
            g_free (name);
            return;
        }
    }

    values = billterm_values_from_ui (nbt);
    if (!verify_term_ok (nbt, &values))
    {
        g_free (values.description);
        g_free (name);
        return;
    }

    if (new_billterm_valid_book (nbt) &&
        new_billterm_ok_cb (nbt, &values, name))
        gtk_widget_destroy (GTK_WIDGET(dialog));
    g_free (values.description);
    g_free (name);
}

static void
new_billterm_response_cb (GtkDialog *dialog, gint response, NewBillTerm *nbt)
{
    /* Engine notifications emitted while committing can synchronously cause
     * another response. The outer invocation owns this context until it exits. */
    if (nbt->response_active)
        return;
    nbt->response_active = TRUE;
    new_billterm_handle_response (dialog, response, nbt);
    nbt->response_active = FALSE;
    if (nbt->dialog_destroyed)
        new_billterm_free_context (GTK_WIDGET(dialog), nbt);
}

static void
show_notebook (BillTermNB *notebook)
{
    g_return_if_fail (notebook->type > 0);
    gtk_notebook_set_current_page (GTK_NOTEBOOK(notebook->notebook),
                                   notebook->type - 1);
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

    value = gtk_combo_box_get_active (cb);
    maybe_set_type (nbt, value + 1);
}

static void
new_billterm_dialog (BillTermsWindow *btw, GncBillTerm *term,
                     const char *name)
{
    NewBillTerm *nbt;
    GtkBuilder *builder;
    GtkWidget *box, *combo_box;
    const gchar *dialog_name;
    const gchar *dialog_desc;
    const gchar *dialog_combo;
    const gchar *dialog_nb;

    if (!btw || btw->closing || !btw->book) return;

    nbt = g_new0 (NewBillTerm, 1);
    nbt->btw = billterms_window_ref (btw);
    nbt->editing = term != NULL;
    nbt->book_guid = *qof_book_get_guid (btw->book);
    if (term)
        nbt->term_guid = *gncBillTermGetGUID (term);

    /* Open and read the Glade File */
    if (term == NULL)
    {
        dialog_name = "new_term_dialog";
        dialog_desc = "description_entry";
        dialog_combo = "type_combobox";
        dialog_nb = "note_book_hbox";
    }
    else
    {
        dialog_name = "edit_term_dialog";
        dialog_desc = "entry_desc";
        dialog_combo = "type_combo";
        dialog_nb = "notebook_hbox";
    }
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "type_liststore");
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", dialog_name);
    nbt->dialog = GTK_WIDGET(gtk_builder_get_object (builder, dialog_name));
    nbt->name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));
    nbt->desc_entry = GTK_WIDGET(gtk_builder_get_object (builder, dialog_desc));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(nbt->dialog), "gnc-id-new-bill-terms");
    gnc_widget_style_context_add_class (GTK_WIDGET(nbt->dialog), "gnc-class-bill-terms");

    if (name)
        gtk_entry_set_text (GTK_ENTRY(nbt->name_entry), name);

    /* Initialize the notebook widgets */
    init_notebook_widgets (&nbt->notebook, FALSE, nbt);

    /* Attach the notebook */
    box = GTK_WIDGET(gtk_builder_get_object (builder, dialog_nb));
    gtk_box_pack_start (GTK_BOX(box), nbt->notebook.notebook, TRUE, TRUE, 0);
    g_object_unref (nbt->notebook.notebook);

    /* Fill in the widgets appropriately */
    if (term)
        billterm_to_ui (term, nbt->desc_entry, &nbt->notebook);
    else
        nbt->notebook.type = GNC_TERM_TYPE_DAYS;

    /* Create the menu */
    combo_box = GTK_WIDGET(gtk_builder_get_object (builder, dialog_combo));
    gtk_combo_box_set_active (GTK_COMBO_BOX(combo_box), nbt->notebook.type - 1);

    /* Show the right notebook page */
    show_notebook (&nbt->notebook);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, nbt);

    gtk_window_set_transient_for (GTK_WINDOW(nbt->dialog),
                                  GTK_WINDOW(btw->window));
    gtk_window_set_destroy_with_parent (GTK_WINDOW(nbt->dialog), TRUE);
    g_signal_connect (nbt->dialog, "response",
                      G_CALLBACK(new_billterm_response_cb), nbt);
    g_signal_connect (nbt->dialog, "destroy",
                      G_CALLBACK(new_billterm_destroy_cb), nbt);
    g_object_ref (nbt->dialog);

    /* Show what we should */
    gtk_widget_show_all (nbt->dialog);
    if (term)
    {
        gtk_widget_grab_focus (nbt->desc_entry);
    }
    else
        gtk_widget_grab_focus (nbt->name_entry);

    g_object_unref (G_OBJECT(builder));
}

/***********************************************************************/

static void
billterms_term_refresh (BillTermsWindow *btw)
{
    char *type_label;

    g_return_if_fail (btw);

    if (!btw->current_term)
    {
        gtk_widget_hide (btw->term_vbox);
        return;
    }

    gtk_widget_show_all (btw->term_vbox);
    billterm_to_ui (btw->current_term, btw->desc_entry, &btw->notebook);
    switch (gncBillTermGetType (btw->current_term))
    {
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
    gtk_label_set_text (GTK_LABEL(btw->type_label), type_label);
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
    view = GTK_TREE_VIEW(btw->terms_view);
    store = GTK_LIST_STORE(gtk_tree_view_get_model (view));
    selection = gtk_tree_view_get_selection (view);

    /* Clear the list */
    gtk_list_store_clear (store);
    gnc_gui_component_clear_watches (btw->component_id);

    /* Add the items to the list */
    list = gncBillTermGetTerms (btw->book);

    /* If there are no terms, clear the term display */
    if (list == NULL)
    {
        btw->current_term = NULL;
        billterms_term_refresh (btw);
    }
    else
    {
        list = g_list_reverse (g_list_copy (list));
    }

    for ( node = list; node; node = node->next)
    {
        term = node->data;
        gnc_gui_component_watch_entity (btw->component_id,
                                        gncBillTermGetGUID (term),
                                        QOF_EVENT_MODIFY);

        gtk_list_store_prepend (store, &iter);
        gtk_list_store_set (store, &iter,
                            BILL_TERM_COL_NAME, gncBillTermGetName (term),
                            BILL_TERM_COL_TERM, term,
                            -1);
        if (term == btw->current_term)
        {
            path = gtk_tree_model_get_path (GTK_TREE_MODEL(store), &iter);
            reference = gtk_tree_row_reference_new (GTK_TREE_MODEL(store), path);
            gtk_tree_path_free (path);
        }
    }

    g_list_free (list);

    gnc_gui_component_watch_entity_type (btw->component_id,
                                         GNC_BILLTERM_MODULE_NAME,
                                         QOF_EVENT_CREATE | QOF_EVENT_DESTROY);

    if (reference)
    {
        path = gtk_tree_row_reference_get_path (reference);
        gtk_tree_row_reference_free (reference);
        if (path)
        {
            gtk_tree_selection_select_path (selection, path);
            gtk_tree_view_scroll_to_cell (view, path, NULL, TRUE, 0.5, 0.0);
            gtk_tree_path_free (path);
        }
    }
    else
    {
        GtkTreeIter iter;
        if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL(store), &iter))
            gtk_tree_selection_select_iter (selection, &iter);
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

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        gtk_tree_model_get (model, &iter, BILL_TERM_COL_TERM, &term, -1);

    /* If we've changed, then reset the term list */
    if (GNC_IS_BILLTERM(term) && (term != btw->current_term))
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

typedef struct
{
    BillTermsWindow *btw;
    GncGUID book_guid;
    GncGUID term_guid;
} DeleteBillTerm;

static void
billterms_delete_response ([[maybe_unused]] GtkWindow *parent,
                          gint response, gpointer user_data)
{
    DeleteBillTerm *request = user_data;
    BillTermsWindow *btw = request->btw;
    QofBook *book = billterms_valid_book (btw, &request->book_guid);
    GncBillTerm *term = book ? gncBillTermLookup (book, &request->term_guid) : NULL;

    /* Selection and use count can change while the question is open. Only
     * the originally confirmed term may be deleted, if it is still unused. */
    if (response == GTK_RESPONSE_YES && term &&
        gncBillTermGetRefcount (term) == 0)
    {
        if (btw->current_term == term)
            btw->current_term = NULL;
        gnc_suspend_gui_refresh ();
        gncBillTermBeginEdit (term);
        gncBillTermDestroy (term);
        gnc_resume_gui_refresh ();
    }
    billterms_window_unref (btw);
    g_free (request);
}

void
billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw)
{
    DeleteBillTerm *request;
    g_return_if_fail (btw);

    if (!btw->current_term)
        return;

    if (gncBillTermGetRefcount (btw->current_term) > 0)
    {
        gnc_error_dialog_async (GTK_WINDOW(btw->window),
                                _("Term \"%s\" is in use. You cannot delete it."),
                                gncBillTermGetName (btw->current_term));
        return;
    }

    request = g_new0 (DeleteBillTerm, 1);
    request->btw = billterms_window_ref (btw);
    request->book_guid = *qof_book_get_guid (btw->book);
    request->term_guid = *gncBillTermGetGUID (btw->current_term);
    gnc_verify_dialog_async (GTK_WINDOW(btw->window), FALSE,
                             billterms_delete_response, request,
                             _("Are you sure you want to delete \"%s\"?"),
                             gncBillTermGetName (btw->current_term));
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

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(btw->window));
    gtk_widget_destroy (btw->window);
}

void
billterms_window_close (GtkWidget *widget, gpointer data)
{
    BillTermsWindow *btw = data;

    gnc_close_gui_component (btw->component_id);
}

static gboolean
billterms_window_delete_event_cb (GtkWidget *widget,
                                  GdkEvent  *event,
                                  gpointer   data)
{
    BillTermsWindow *btw = data;

    if (!btw) return FALSE;

    // this cb allows the window size to be saved on closing with the X
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(btw->window));
    return FALSE;
}

void
billterms_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    BillTermsWindow *btw = data;

    if (!btw) return;

    gnc_unregister_gui_component (btw->component_id);

    btw->closing = TRUE;
    btw->window = NULL;
    billterms_window_unref (btw);
}

static gboolean
billterms_window_key_press_cb (GtkWidget *widget, GdkEventKey *event,
                               gpointer data)
{
    BillTermsWindow *btw = data;

    if (event->keyval == GDK_KEY_Escape)
    {
        billterms_window_close_handler (btw);
        return TRUE;
    }
    else
        return FALSE;
}

static gboolean
find_handler (gpointer find_data, gpointer data)
{
    BillTermsWindow *btw = data;
    QofBook *book = find_data;

    return (btw != NULL && btw->book == book);
}

/* Create a billterms window */
BillTermsWindow *
gnc_ui_billterms_window_new (GtkWindow *parent, QofBook *book)
{
    BillTermsWindow *btw;
    GtkBuilder *builder;
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
    if (btw)
    {
        gtk_window_present (GTK_WINDOW(btw->window));
        return btw;
    }

    /* Didn't find one -- create a new window */
    btw = g_new0 (BillTermsWindow, 1);
    btw->ref_count = 1;
    btw->book = book;
    g_object_add_weak_pointer (G_OBJECT(book), (gpointer *)&btw->book);
    btw->session = gnc_get_current_session ();

    /* Open and read the Glade File */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-billterms.glade", "terms_window");
    btw->window = GTK_WIDGET(gtk_builder_get_object (builder, "terms_window"));
    btw->terms_view = GTK_WIDGET(gtk_builder_get_object (builder, "terms_view"));
    btw->desc_entry = GTK_WIDGET(gtk_builder_get_object (builder, "desc_entry"));
    btw->type_label = GTK_WIDGET(gtk_builder_get_object (builder, "type_label"));
    btw->term_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "term_vbox"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(btw->window), "gnc-id-bill-terms");
    gnc_widget_style_context_add_class (GTK_WIDGET(btw->window), "gnc-class-bill-terms");

    g_signal_connect (btw->window, "key_press_event",
                      G_CALLBACK(billterms_window_key_press_cb), btw);

    /* Initialize the view */
    view = GTK_TREE_VIEW(btw->terms_view);
    store = gtk_list_store_new (NUM_BILL_TERM_COLS, G_TYPE_STRING, G_TYPE_POINTER);
    gtk_tree_view_set_model (view, GTK_TREE_MODEL(store));
    g_object_unref (store);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes ("", renderer,
             "text", BILL_TERM_COL_NAME,
             NULL);
    gtk_tree_view_append_column (view, column);

    g_signal_connect (view, "row-activated",
                      G_CALLBACK(billterm_selection_activated), btw);
    selection = gtk_tree_view_get_selection (view);
    g_signal_connect (selection, "changed",
                      G_CALLBACK(billterm_selection_changed), btw);

    /* Initialize the notebook widgets */
    init_notebook_widgets (&btw->notebook, TRUE, btw);

    /* Attach the notebook */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "notebook_box"));
    gtk_box_pack_start (GTK_BOX(widget), btw->notebook.notebook,
                        TRUE, TRUE, 0);
    g_object_unref (btw->notebook.notebook);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, btw);

    /* register with component manager */
    btw->component_id =
        gnc_register_gui_component (DIALOG_BILLTERMS_CM_CLASS,
                                    billterms_window_refresh_handler,
                                    billterms_window_close_handler,
                                    btw);

    gnc_gui_component_set_session (btw->component_id, btw->session);

    g_signal_connect (G_OBJECT(btw->window), "delete-event",
                      G_CALLBACK(billterms_window_delete_event_cb), btw);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(btw->window), GTK_WINDOW(parent));

    gtk_widget_show_all (btw->window);
    billterms_window_refresh (btw);

    g_object_unref (G_OBJECT(builder));

    return btw;
}

#if 0
/* Create a new billterms by name */
GncBillTerm *
gnc_ui_billterms_new_from_name (GtkWindow *parent, QofBook *book, const char *name)
{
    BillTermsWindow *btw;

    if (!book) return NULL;

    btw = gnc_ui_billterms_window_new (parent, book);
    if (!btw) return NULL;

    return new_billterm_dialog (btw, NULL, name);
}
#endif

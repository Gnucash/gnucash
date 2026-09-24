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

void billterms_new_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_edit_term_cb (GtkButton *button, BillTermsWindow *btw);
void billterms_window_close (GtkWidget *widget, gpointer data);
void billterms_window_destroy_cb (GtkWidget *widget, gpointer data);
void billterms_type_dropdown_changed (GtkDropDown *dropdown, GParamSpec *pspec,
                                     gpointer data);

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

typedef struct _new_billterms NewBillTerm;

struct _billterms_window
{
    GtkWidget *window;
    GtkColumnView *terms_view;
    GListStore *terms_model;
    GtkSingleSelection *term_selection;
    GtkWidget *desc_entry;
    GtkWidget *type_label;
    GtkWidget *term_vbox;
    BillTermNB notebook;

    GncBillTerm *current_term;
    GncGUID current_term_guid;
    gboolean has_current_term;
    QofBook     *book;
    gint         component_id;
    QofSession  *session;
    NewBillTerm *active_editor;
};

struct _new_billterms
{
    GtkWindow *dialog;
    GtkWidget *name_entry;
    GtkWidget *desc_entry;
    BillTermNB notebook;

    BillTermsWindow *btw;
    GncBillTerm     *this_term;
    QofBook         *book;
    GncGUID          term_guid;
    GWeakRef         window;
    gulong           destroy_handler;
    gboolean         editing;
    gboolean         completed;
};

typedef struct
{
    GncGUID guid;
    gchar *name;
} BillTermRow;

static GQuark billterm_row_quark = 0;

static void
billterm_row_free (gpointer data)
{
    BillTermRow *row = data;

    if (!row)
        return;

    g_free (row->name);
    g_free (row);
}

static GObject *
billterm_row_new (GncBillTerm *term)
{
    GObject *object;
    BillTermRow *row;

    if (G_UNLIKELY (!billterm_row_quark))
        billterm_row_quark = g_quark_from_static_string ("gnc-billterm-row");

    object = G_OBJECT (g_object_new (G_TYPE_OBJECT, NULL));
    row = g_new0 (BillTermRow, 1);
    row->guid = *gncBillTermGetGUID (term);
    row->name = g_strdup (gncBillTermGetName (term));
    g_object_set_qdata_full (object, billterm_row_quark, row, billterm_row_free);
    return object;
}

static BillTermRow *
billterm_row_get (gpointer object)
{
    return object ? g_object_get_qdata (G_OBJECT (object), billterm_row_quark)
                  : NULL;
}

static void
billterms_set_current_term (BillTermsWindow *btw, GncBillTerm *term)
{
    btw->current_term = term;
    btw->has_current_term = term != NULL;
    if (term)
        btw->current_term_guid = *gncBillTermGetGUID (term);
    else
        btw->current_term_guid = *guid_null ();
}

static GncBillTerm *
billterms_get_current_term (BillTermsWindow *btw)
{
    GncBillTerm *term;

    if (!btw || !btw->has_current_term || qof_book_shutting_down (btw->book))
        return NULL;

    term = gncBillTermLookup (btw->book, &btw->current_term_guid);
    if (!term)
        billterms_set_current_term (btw, NULL);
    else
        btw->current_term = term;
    return term;
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
    gtk_window_set_child (GTK_WINDOW (parent), NULL);
    gtk_window_destroy (GTK_WINDOW (parent));
    g_object_unref (G_OBJECT(builder));

    /* NOTE: The caller needs to unref once they attach */
}

static void
set_numeric (GtkWidget *widget, GncBillTerm *term,
             void (*func)(GncBillTerm *, gnc_numeric))
{
    gnc_numeric val;
    gdouble fl = 0.0;

    fl = gtk_spin_button_get_value (GTK_SPIN_BUTTON(widget));
    val = double_to_gnc_numeric (fl, 100000, GNC_HOW_RND_ROUND_HALF_UP);
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
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(widget), fl);
}

static void
set_int (GtkWidget *widget, GncBillTerm *term,
         void (*func)(GncBillTerm *, gint))
{
    gint val;

    val = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(widget));
    func (term, val);
}

static void
get_int (GtkWidget *widget, GncBillTerm *term,
         gint (*func)(const GncBillTerm *))
{
    gint val;

    val = func (term);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON(widget), (gfloat)val);
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

    text = gnc_entry_get_text (GTK_ENTRY(nbt->desc_entry));
    if (text)
        gncBillTermSetDescription (term, text);

    gncBillTermSetType (nbt->this_term, nbt->notebook.type);

    switch (nbt->notebook.type)
    {
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
    gnc_entry_set_text (GTK_ENTRY(desc), gncBillTermGetDescription (term));
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
verify_term_ok (NewBillTerm *nbt)
{
    char *message = _("Discount days cannot be more than due days.");
    gboolean result;
    BillTermNB *notebook;
    gint days_due_days, days_disc_days;
    gint prox_due_days, prox_disc_days;

    notebook = &nbt->notebook;
    result = TRUE;


    days_due_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(notebook->days_due_days));
    days_disc_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(notebook->days_disc_days));
    prox_due_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(notebook->prox_due_day));
    prox_disc_days = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(notebook->prox_disc_day));

    switch (nbt->notebook.type)
    {
    case GNC_TERM_TYPE_DAYS:
        if (days_due_days<days_disc_days)
        {
              gnc_error_dialog (GTK_WINDOW(nbt->dialog), "%s", message);
              result = FALSE;
        }
        break;
    case GNC_TERM_TYPE_PROXIMO:
    if (prox_due_days<prox_disc_days)
        {
            gnc_error_dialog (GTK_WINDOW(nbt->dialog), "%s", message);
            result = FALSE;
        }
        break;
    }

    return result;
}

static gboolean
new_billterm_ok_cb (NewBillTerm *nbt)
{
    BillTermsWindow *btw;
    GncBillTerm *term;
    const char *name = NULL;
    char *message;

    g_return_val_if_fail (nbt, FALSE);
    btw = nbt->btw;
    if (!btw)
        return FALSE;

    /* Verify that we've got real, valid data. */
    if (nbt->this_term == NULL)
    {
        name = gnc_entry_get_text (GTK_ENTRY (nbt->name_entry));
        if (name == NULL || *name == '\0')
        {
            message = _("You must provide a name for this Billing Term.");
            gnc_error_dialog (nbt->dialog, "%s", message);
            return FALSE;
        }
        if (gncBillTermLookupByName (btw->book, name))
        {
            message = g_strdup_printf (_(
                "You must provide a unique name for this Billing Term. "
                "Your choice \"%s\" is already in use."), name);
            gnc_error_dialog (nbt->dialog, "%s", message);
            g_free (message);
            return FALSE;
        }
    }

    if (!verify_term_ok (nbt))
        return FALSE;

    gnc_suspend_gui_refresh ();
    term = nbt->this_term;
    if (!term)
    {
        term = gncBillTermCreate (btw->book);
        nbt->this_term = term;
        gncBillTermBeginEdit (term);
        gncBillTermSetName (term, name);
        billterms_set_current_term (btw, term);
    }
    else
        gncBillTermBeginEdit (term);

    if (ui_to_billterm (nbt))
        gncBillTermChanged (term);
    gncBillTermCommitEdit (term);
    gnc_resume_gui_refresh ();
    return TRUE;
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
billterms_type_dropdown_changed (GtkDropDown *dropdown, GParamSpec *pspec,
                                 gpointer data)
{
    NewBillTerm *nbt = data;
    guint value;

    value = gtk_drop_down_get_selected (dropdown);
    if (value != GTK_INVALID_LIST_POSITION)
        maybe_set_type (nbt, value + 1);
    (void)pspec;
}

static void new_billterm_dialog_complete (NewBillTerm *nbt);

static void
new_billterm_dialog_request_destroyed (GtkWidget *widget, NewBillTerm *nbt)
{
    (void)widget;
    nbt->btw = NULL;
    nbt->destroy_handler = 0;
    new_billterm_dialog_complete (nbt);
}

static void
new_billterm_dialog_free (NewBillTerm *nbt)
{
    GtkWidget *window = g_weak_ref_get (&nbt->window);

    if (window && nbt->destroy_handler)
        g_signal_handler_disconnect (window, nbt->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&nbt->window);
    g_free (nbt);
}

static void
new_billterm_dialog_destroy (NewBillTerm *nbt)
{
    GtkWindow *dialog = g_steal_pointer (&nbt->dialog);

    if (!dialog)
        return;

    g_signal_handlers_disconnect_by_data (dialog, nbt);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
new_billterm_dialog_complete (NewBillTerm *nbt)
{
    if (!nbt || nbt->completed)
        return;

    nbt->completed = TRUE;
    if (nbt->btw && nbt->btw->active_editor == nbt)
        nbt->btw->active_editor = NULL;
    new_billterm_dialog_destroy (nbt);
    new_billterm_dialog_free (nbt);
}

static void
new_billterm_dialog_accept (NewBillTerm *nbt)
{
    if (!nbt->btw || qof_book_shutting_down (nbt->book))
    {
        new_billterm_dialog_complete (nbt);
        return;
    }
    if (nbt->editing)
    {
        nbt->this_term = gncBillTermLookup (nbt->book, &nbt->term_guid);
        if (!nbt->this_term)
        {
            new_billterm_dialog_complete (nbt);
            return;
        }
    }
    if (!new_billterm_ok_cb (nbt))
        return;

    new_billterm_dialog_complete (nbt);
}

static void
new_billterm_dialog_accept_clicked_cb (GtkButton *button, NewBillTerm *nbt)
{
    (void)button;
    new_billterm_dialog_accept (nbt);
}

static void
new_billterm_dialog_cancel_clicked_cb (GtkButton *button, NewBillTerm *nbt)
{
    (void)button;
    new_billterm_dialog_complete (nbt);
}

static gboolean
new_billterm_dialog_close_request_cb (GtkWindow *dialog, NewBillTerm *nbt)
{
    (void)dialog;
    new_billterm_dialog_complete (nbt);
    return TRUE;
}

static void
new_billterm_dialog_destroy_cb (GtkWidget *widget, NewBillTerm *nbt)
{
    (void)widget;
    if (!nbt->completed)
        g_clear_object (&nbt->dialog);
    new_billterm_dialog_complete (nbt);
}

static void
new_billterm_dialog_request (BillTermsWindow *btw, GncBillTerm *term,
                             const char *name)
{
    NewBillTerm *nbt;
    GtkBuilder *builder;
    GtkWidget *box;
    GtkDropDown *dropdown;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    const gchar *dialog_name;
    const gchar *dialog_desc;
    const gchar *dialog_combo;
    const gchar *dialog_nb;
    const gchar *ok_button_name;
    const gchar *cancel_button_name;

    if (!btw)
        return;

    if (btw->active_editor)
    {
        gtk_window_present (btw->active_editor->dialog);
        return;
    }

    nbt = g_new0 (NewBillTerm, 1);
    nbt->btw = btw;
    nbt->book = btw->book;
    nbt->this_term = term;
    nbt->editing = term != NULL;
    if (term)
        nbt->term_guid = *gncBillTermGetGUID (term);
    g_weak_ref_init (&nbt->window, btw->window);
    nbt->destroy_handler = g_signal_connect (
        btw->window, "destroy", G_CALLBACK (new_billterm_dialog_request_destroyed),
        nbt);

    if (!term)
    {
        dialog_name = "new_term_dialog";
        dialog_desc = "description_entry";
        dialog_combo = "type_combobox";
        dialog_nb = "note_book_hbox";
        ok_button_name = "ok_button";
        cancel_button_name = "cancel_button";
    }
    else
    {
        dialog_name = "edit_term_dialog";
        dialog_desc = "entry_desc";
        dialog_combo = "type_combo";
        dialog_nb = "notebook_hbox";
        ok_button_name = "ok_butt";
        cancel_button_name = "cancel_butt";
    }

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-billterms.glade",
                                    "type_model") ||
        !gnc_builder_add_from_file (builder, "dialog-billterms.glade",
                                    dialog_name))
    {
        g_object_unref (builder);
        new_billterm_dialog_free (nbt);
        return;
    }

    nbt->dialog = GTK_WINDOW (gtk_builder_get_object (builder, dialog_name));
    nbt->name_entry = GTK_WIDGET (gtk_builder_get_object (builder, "name_entry"));
    nbt->desc_entry = GTK_WIDGET (gtk_builder_get_object (builder, dialog_desc));
    box = GTK_WIDGET (gtk_builder_get_object (builder, dialog_nb));
    dropdown = GTK_DROP_DOWN (gtk_builder_get_object (builder, dialog_combo));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, ok_button_name));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                         cancel_button_name));
    if (!nbt->dialog || !nbt->desc_entry || !box || !dropdown || !ok_button ||
        !cancel_button || (!term && !nbt->name_entry))
    {
        nbt->dialog = NULL;
        g_object_unref (builder);
        new_billterm_dialog_free (nbt);
        return;
    }

    nbt->dialog = g_object_ref (nbt->dialog);
    gtk_widget_set_name (GTK_WIDGET (nbt->dialog), "gnc-id-new-bill-terms");
    gnc_widget_style_context_add_class (GTK_WIDGET (nbt->dialog),
                                        "gnc-class-bill-terms");
    if (name)
        gnc_entry_set_text (GTK_ENTRY (nbt->name_entry), name);

    init_notebook_widgets (&nbt->notebook, FALSE, nbt);
    gtk_box_append (GTK_BOX (box), nbt->notebook.notebook);
    g_object_unref (nbt->notebook.notebook);

    if (term)
        billterm_to_ui (term, nbt->desc_entry, &nbt->notebook);
    else
        nbt->notebook.type = GNC_TERM_TYPE_DAYS;
    gtk_drop_down_set_selected (dropdown, nbt->notebook.type - 1);
    show_notebook (&nbt->notebook);
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, nbt);

    gtk_window_set_transient_for (nbt->dialog, GTK_WINDOW (btw->window));
    gtk_window_set_modal (nbt->dialog, TRUE);
    gtk_window_set_default_widget (nbt->dialog, ok_button);
    if (term)
        gtk_widget_grab_focus (nbt->desc_entry);
    else
        gtk_widget_grab_focus (nbt->name_entry);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (new_billterm_dialog_accept_clicked_cb), nbt);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (new_billterm_dialog_cancel_clicked_cb), nbt);
    g_signal_connect (nbt->dialog, "close-request",
                      G_CALLBACK (new_billterm_dialog_close_request_cb), nbt);
    g_signal_connect (nbt->dialog, "destroy",
                      G_CALLBACK (new_billterm_dialog_destroy_cb), nbt);
    g_object_unref (builder);

    btw->active_editor = nbt;
    gtk_window_present (nbt->dialog);
}
/***********************************************************************/

static void
billterms_term_refresh (BillTermsWindow *btw)
{
    GncBillTerm *term;
    char *type_label;

    g_return_if_fail (btw);

    term = billterms_get_current_term (btw);
    if (!term)
    {
        gtk_widget_set_visible (btw->term_vbox, FALSE);
        return;
    }

    gtk_widget_set_visible (btw->term_vbox, TRUE);
    billterm_to_ui (term, btw->desc_entry, &btw->notebook);
    switch (gncBillTermGetType (term))
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
    gtk_label_set_text (GTK_LABEL (btw->type_label), type_label);
}

static void
billterm_text_setup (GtkListItemFactory *factory, GtkListItem *item,
                     gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)user_data;
}

static void
billterm_text_bind (GtkListItemFactory *factory, GtkListItem *item,
                    gpointer user_data)
{
    BillTermRow *row = billterm_row_get (gtk_list_item_get_item (item));

    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        row ? row->name : "");
    (void)factory;
    (void)user_data;
}

static void
billterms_window_refresh (BillTermsWindow *btw)
{
    const GList *terms;
    const GList *node;
    GncGUID selected_guid;
    gboolean had_selection;
    guint selected_position = GTK_INVALID_LIST_POSITION;
    guint position = 0;

    g_return_if_fail (btw);

    had_selection = btw->has_current_term;
    selected_guid = btw->current_term_guid;
    gtk_single_selection_set_selected (btw->term_selection,
                                       GTK_INVALID_LIST_POSITION);
    g_list_store_remove_all (btw->terms_model);
    gnc_gui_component_clear_watches (btw->component_id);

    terms = gncBillTermGetTerms (btw->book);
    for (node = terms; node; node = node->next)
    {
        GncBillTerm *term = node->data;
        GObject *row;

        gnc_gui_component_watch_entity (btw->component_id,
                                        gncBillTermGetGUID (term),
                                        QOF_EVENT_MODIFY);
        row = billterm_row_new (term);
        g_list_store_append (btw->terms_model, row);
        g_object_unref (row);
        if (had_selection && guid_equal (&selected_guid, gncBillTermGetGUID (term)))
            selected_position = position;
        position++;
    }

    gnc_gui_component_watch_entity_type (btw->component_id,
                                         GNC_BILLTERM_MODULE_NAME,
                                         QOF_EVENT_CREATE | QOF_EVENT_DESTROY);
    if (selected_position == GTK_INVALID_LIST_POSITION && position > 0)
        selected_position = 0;
    gtk_single_selection_set_selected (btw->term_selection, selected_position);
    if (selected_position == GTK_INVALID_LIST_POSITION)
        billterms_set_current_term (btw, NULL);
    else
        gtk_column_view_scroll_to (btw->terms_view, selected_position, NULL,
                                   GTK_LIST_SCROLL_NONE, NULL);
}

static void
billterm_selection_changed (GtkSelectionModel *selection, guint position,
                            guint n_items, BillTermsWindow *btw)
{
    GObject *object;
    BillTermRow *row;
    GncBillTerm *term = NULL;
    guint selected;

    (void)position;
    (void)n_items;
    g_return_if_fail (btw);

    selected = gtk_single_selection_get_selected (GTK_SINGLE_SELECTION (selection));
    object = selected == GTK_INVALID_LIST_POSITION ? NULL
        : g_list_model_get_item (G_LIST_MODEL (btw->terms_model), selected);
    row = billterm_row_get (object);
    if (row)
        term = gncBillTermLookup (btw->book, &row->guid);
    billterms_set_current_term (btw, term);
    g_clear_object (&object);
    billterms_term_refresh (btw);
}

static void
billterm_selection_activated (GtkColumnView *view, guint position,
                               BillTermsWindow *btw)
{
    GObject *object = g_list_model_get_item (G_LIST_MODEL (btw->terms_model),
                                             position);
    BillTermRow *row = billterm_row_get (object);
    GncBillTerm *term = row ? gncBillTermLookup (btw->book, &row->guid) : NULL;

    if (term)
    {
        billterms_set_current_term (btw, term);
        new_billterm_dialog_request (btw, term, NULL);
    }
    g_clear_object (&object);
    (void)view;
}

void
billterms_new_term_cb (GtkButton *button, BillTermsWindow *btw)
{
    g_return_if_fail (btw);
    new_billterm_dialog_request (btw, NULL, NULL);
    (void)button;
}
typedef struct
{
    BillTermsWindow *btw;
    GWeakRef window;
    gulong destroy_handler;
    GncGUID term_guid;
} BillTermDeleteRequest;

static void
billterm_delete_request_destroyed (GtkWidget *window,
                                   BillTermDeleteRequest *request)
{
    (void)window;
    request->btw = NULL;
    request->destroy_handler = 0;
}

static void
billterm_delete_request_free (BillTermDeleteRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static void
billterm_delete_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    BillTermDeleteRequest *request = user_data;
    GncBillTerm *term = NULL;

    (void)parent;
    if (response == GTK_RESPONSE_YES && request->btw &&
        !qof_book_shutting_down (request->btw->book))
        term = gncBillTermLookup (request->btw->book, &request->term_guid);

    if (term && gncBillTermGetRefcount (term) == 0)
    {
        gnc_suspend_gui_refresh ();
        gncBillTermBeginEdit (term);
        gncBillTermDestroy (term);
        if (request->btw->has_current_term &&
            guid_equal (&request->btw->current_term_guid, &request->term_guid))
            billterms_set_current_term (request->btw, NULL);
        gnc_resume_gui_refresh ();
    }

    billterm_delete_request_free (request);
}

static void
billterm_delete_request (BillTermsWindow *btw, GncBillTerm *term)
{
    BillTermDeleteRequest *request = g_new0 (BillTermDeleteRequest, 1);

    request->btw = btw;
    g_weak_ref_init (&request->window, btw->window);
    request->destroy_handler = g_signal_connect (
        btw->window, "destroy", G_CALLBACK (billterm_delete_request_destroyed),
        request);
    request->term_guid = *gncBillTermGetGUID (term);
    gnc_verify_dialog_async (GTK_WINDOW (btw->window), FALSE,
                             billterm_delete_finished, request,
                             _("Are you sure you want to delete \"%s\"?"),
                             gncBillTermGetName (term));
}

void
billterms_delete_term_cb (GtkButton *button, BillTermsWindow *btw)
{
    GncBillTerm *term;

    g_return_if_fail (btw);

    term = billterms_get_current_term (btw);
    if (!term)
        return;

    if (gncBillTermGetRefcount (term) > 0)
    {
        gnc_error_dialog (GTK_WINDOW (btw->window),
                          _("Term \"%s\" is in use. You cannot delete it."),
                          gncBillTermGetName (term));
        return;
    }

    billterm_delete_request (btw, term);
    (void)button;
}

void
billterms_edit_term_cb (GtkButton *button, BillTermsWindow *btw)
{
    GncBillTerm *term;

    g_return_if_fail (btw);

    term = billterms_get_current_term (btw);
    if (term)
        new_billterm_dialog_request (btw, term, NULL);
    (void)button;
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
    if (!btw->window)
        return;

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(btw->window));
    gtk_window_destroy (GTK_WINDOW (btw->window));
}

void
billterms_window_close (GtkWidget *widget, gpointer data)
{
    BillTermsWindow *btw = data;

    if (btw)
        gnc_close_gui_component (btw->component_id);
    (void)widget;
}

static gboolean
billterms_window_close_request_cb (GtkWindow *window, gpointer data)
{
    BillTermsWindow *btw = data;

    if (!btw) return FALSE;

    // This callback allows the window size to be saved on closing with the X.
    gnc_save_window_size (GNC_PREFS_GROUP, window);
    return FALSE;
}

void
billterms_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    BillTermsWindow *btw = data;

    if (!btw) return;

    /* The GtkWindow is already being destroyed. Prevent a reentrant close
     * handler from trying to destroy it again before unregistering it. */
    btw->window = NULL;
    gnc_unregister_gui_component (btw->component_id);
    g_clear_object (&btw->term_selection);
    g_clear_object (&btw->terms_model);
    g_free (btw);
}

static gboolean
billterms_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                               guint keycode, GdkModifierType state,
                               gpointer user_data)
{
    BillTermsWindow *btw = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        gnc_close_gui_component (btw->component_id);
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
    GtkListItemFactory *factory;
    GtkColumnViewColumn *column;
    GtkEventController *event_controller;

    if (!book)
        return NULL;

    btw = gnc_find_first_gui_component (DIALOG_BILLTERMS_CM_CLASS,
                                        find_handler, book);
    if (btw)
    {
        gtk_window_present (GTK_WINDOW (btw->window));
        return btw;
    }

    btw = g_new0 (BillTermsWindow, 1);
    btw->book = book;
    btw->session = gnc_get_current_session ();

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-billterms.glade",
                                    "terms_window"))
    {
        g_object_unref (builder);
        g_free (btw);
        return NULL;
    }
    btw->window = GTK_WIDGET (gtk_builder_get_object (builder, "terms_window"));
    btw->terms_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "terms_view"));
    btw->desc_entry = GTK_WIDGET (gtk_builder_get_object (builder, "desc_entry"));
    btw->type_label = GTK_WIDGET (gtk_builder_get_object (builder, "type_label"));
    btw->term_vbox = GTK_WIDGET (gtk_builder_get_object (builder, "term_vbox"));
    if (!btw->window || !btw->terms_view || !btw->desc_entry ||
        !btw->type_label || !btw->term_vbox)
    {
        g_object_unref (builder);
        g_free (btw);
        return NULL;
    }

    gtk_widget_set_name (btw->window, "gnc-id-bill-terms");
    gnc_widget_style_context_add_class (btw->window, "gnc-class-bill-terms");

    event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (btw->window, event_controller);
    g_signal_connect (event_controller, "key-pressed",
                      G_CALLBACK (billterms_window_key_press_cb), btw);

    btw->terms_model = g_list_store_new (G_TYPE_OBJECT);
    btw->term_selection = gtk_single_selection_new
        (G_LIST_MODEL (g_object_ref (btw->terms_model)));
    gtk_single_selection_set_autoselect (btw->term_selection, TRUE);
    gtk_column_view_set_model (btw->terms_view,
                               GTK_SELECTION_MODEL (btw->term_selection));
    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup", G_CALLBACK (billterm_text_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (billterm_text_bind), NULL);
    column = gtk_column_view_column_new (_("Terms"), factory);
    gtk_column_view_column_set_expand (column, TRUE);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_append_column (btw->terms_view, column);
    g_object_unref (column);
    g_signal_connect (btw->terms_view, "activate",
                      G_CALLBACK (billterm_selection_activated), btw);
    g_signal_connect (btw->term_selection, "selection-changed",
                      G_CALLBACK (billterm_selection_changed), btw);

    init_notebook_widgets (&btw->notebook, TRUE, btw);
    widget = GTK_WIDGET (gtk_builder_get_object (builder, "notebook_box"));
    gtk_box_append (GTK_BOX (widget), btw->notebook.notebook);
    g_object_unref (btw->notebook.notebook);
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, btw);

    btw->component_id = gnc_register_gui_component (
        DIALOG_BILLTERMS_CM_CLASS, billterms_window_refresh_handler,
        billterms_window_close_handler, btw);
    gnc_gui_component_set_session (btw->component_id, btw->session);
    g_signal_connect (btw->window, "close-request",
                      G_CALLBACK (billterms_window_close_request_cb), btw);
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (btw->window), parent);
    billterms_window_refresh (btw);
    g_object_unref (builder);

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

    return new_billterm_dialog_request (btw, NULL, name);
}
#endif

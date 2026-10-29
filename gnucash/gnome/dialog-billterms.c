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
    GObject parent;
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
    gboolean closing;
    GtkEventController *key_controller;
    GtkWidget *buttons[4];
};

struct _new_billterms
{
    GObject parent;
    GtkWindow *dialog;
    GtkWidget *name_entry;
    GtkWidget *desc_entry;
    BillTermNB notebook;

    BillTermsWindow *btw;
    GncGUID          term_guid;
    GncGUID          book_guid;
    GWeakRef         window;
    GtkWidget       *dropdown;
    GtkWidget       *ok_button;
    GtkWidget       *cancel_button;
    gboolean         editing;
    gboolean         completed;
    gboolean         active;
};

typedef struct
{
    char *description;
    GncBillTermType type;
    gint due_days;
    gint discount_days;
    gint cutoff;
    gnc_numeric discount;
} BillTermValues;


typedef struct
{
    GObjectClass parent_class;
} BillTermsWindowClass;

GType billterms_window_get_type (void);
#define BILLTERMS_WINDOW(obj) \
    (G_TYPE_CHECK_INSTANCE_CAST ((obj), billterms_window_get_type (), BillTermsWindow))

G_DEFINE_TYPE (BillTermsWindow, billterms_window, G_TYPE_OBJECT)

static void
billterms_window_dispose (GObject *object)
{
    BillTermsWindow *btw = BILLTERMS_WINDOW (object);

    if (btw->book)
        g_object_remove_weak_pointer (G_OBJECT(btw->book), (gpointer *)&btw->book);
    btw->book = NULL;
    G_OBJECT_CLASS (billterms_window_parent_class)->dispose (object);
}

static void
billterms_window_class_init (BillTermsWindowClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = billterms_window_dispose;
}

static void
billterms_window_init (BillTermsWindow *btw)
{
}


typedef struct
{
    GObjectClass parent_class;
} NewBillTermClass;

GType new_bill_term_get_type (void);
G_DEFINE_TYPE (NewBillTerm, new_bill_term, G_TYPE_OBJECT)

static void
new_bill_term_dispose (GObject *object)
{
    NewBillTerm *nbt = (NewBillTerm *) object;
    g_clear_object (&nbt->dialog);
    g_clear_object (&nbt->btw);
    G_OBJECT_CLASS (new_bill_term_parent_class)->dispose (object);
}

static void
new_bill_term_finalize (GObject *object)
{
    NewBillTerm *nbt = (NewBillTerm *) object;
    g_weak_ref_clear (&nbt->window);
    G_OBJECT_CLASS (new_bill_term_parent_class)->finalize (object);
}

static void
new_bill_term_class_init (NewBillTermClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    object_class->dispose = new_bill_term_dispose;
    object_class->finalize = new_bill_term_finalize;
}

static void
new_bill_term_init (NewBillTerm *nbt)
{
    g_weak_ref_init (&nbt->window, NULL);
}

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

    if (!btw || btw->closing || !btw->book || !btw->has_current_term ||
        qof_book_shutting_down (btw->book))
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
    values.description = g_strdup (gtk_editable_get_text (GTK_EDITABLE(nbt->desc_entry)));
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
        billterms_set_current_term (nbt->btw, term);

    /* Apply the snapshot within one edit, matching the existing save path. */
    new_billterm_apply_values (term, values);
    gncBillTermChanged (term);
    gncBillTermCommitEdit (term);

    gnc_resume_gui_refresh ();
    return TRUE;
}

static void
new_billterm_dialog_complete (NewBillTerm *nbt)
{
    GtkWidget *parent;
    if (nbt->completed)
        return;
    nbt->completed = TRUE;
    parent = g_weak_ref_get (&nbt->window);
    if (nbt->btw && nbt->btw->active_editor == nbt)
        nbt->btw->active_editor = NULL;
    g_signal_handlers_disconnect_by_data (nbt->dialog, nbt);
    g_signal_handlers_disconnect_by_data (nbt->dropdown, nbt);
    g_signal_handlers_disconnect_by_data (nbt->ok_button, nbt);
    g_signal_handlers_disconnect_by_data (nbt->cancel_button, nbt);
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, nbt);
        g_object_unref (parent);
    }
    /* Disconnect first: destruction can synchronously emit signals. */
    gtk_window_destroy (nbt->dialog);
    g_object_unref (nbt);
}

static void
new_billterm_dialog_unrealize_cb (GtkWidget *widget, NewBillTerm *nbt)
{
    (void)widget;
    new_billterm_dialog_complete (nbt);
}

static void
new_billterm_dialog_visible_cb (GtkWidget *widget, GParamSpec *pspec, NewBillTerm *nbt)
{
    (void)pspec;
    if (!gtk_widget_get_visible (widget))
        new_billterm_dialog_complete (nbt);
}

static void
new_billterm_dialog_accept_clicked_cb (GtkButton *button, NewBillTerm *nbt)
{
    gboolean accepted;
    (void)button;
    if (nbt->completed || nbt->active)
        return;
    g_object_ref (nbt);
    nbt->active = TRUE;
    BillTermValues values = billterm_values_from_ui (nbt);
    char *name = nbt->editing ? NULL :
        g_strdup (gtk_editable_get_text (GTK_EDITABLE (nbt->name_entry)));
    QofBook *book = new_billterm_valid_book (nbt);
    accepted = FALSE;
    if (!book)
        new_billterm_dialog_complete (nbt);
    else if (!nbt->editing && (!name || !*name))
        gnc_error_dialog_async (nbt->dialog, "%s",
                                _("You must provide a name for this Billing Term."));
    else if (!nbt->editing && gncBillTermLookupByName (book, name))
    {
        char *message = g_strdup_printf (_("You must provide a unique name for this Billing Term. "
                                          "Your choice \"%s\" is already in use."), name);
        gnc_error_dialog_async (nbt->dialog, "%s", message);
        g_free (message);
    }
    else if (verify_term_ok (nbt, &values))
        accepted = new_billterm_ok_cb (nbt, &values, name);
    g_free (values.description);
    g_free (name);
    if (accepted && !nbt->completed)
    {
        new_billterm_dialog_complete (nbt);
    }
    nbt->active = FALSE;
    g_object_unref (nbt);
}

static void
new_billterm_dialog_cancel_clicked_cb (GtkButton *button, NewBillTerm *nbt)
{
    (void)button;
    if (nbt->completed || nbt->active)
        return;
    g_object_ref (nbt);
    nbt->active = TRUE;
    new_billterm_dialog_complete (nbt);
    nbt->active = FALSE;
    g_object_unref (nbt);
}

static gboolean
new_billterm_dialog_close_request_cb (GtkWindow *dialog, NewBillTerm *nbt)
{
    (void)dialog;
    if (nbt->completed || nbt->active)
        return TRUE;
    g_object_ref (nbt);
    nbt->active = TRUE;
    new_billterm_dialog_complete (nbt);
    nbt->active = FALSE;
    g_object_unref (nbt);
    return TRUE;
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

    if (!btw || btw->closing || !btw->book)
        return;

    if (btw->active_editor)
    {
        gtk_window_present (btw->active_editor->dialog);
        return;
    }

    nbt = g_object_new (new_bill_term_get_type (), NULL);
    nbt->btw = g_object_ref (btw);
    nbt->book_guid = *qof_book_get_guid (btw->book);
    nbt->editing = term != NULL;
    if (term)
        nbt->term_guid = *gncBillTermGetGUID (term);
    g_weak_ref_set (&nbt->window, btw->window);


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
        g_object_unref (nbt);
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
        g_object_unref (nbt);
        return;
    }

    nbt->dropdown = GTK_WIDGET (dropdown);
    nbt->ok_button = ok_button;
    nbt->cancel_button = cancel_button;
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
    g_signal_connect (dropdown, "notify::selected",
                      G_CALLBACK (billterms_type_dropdown_changed), nbt);

    gtk_window_set_transient_for (nbt->dialog, GTK_WINDOW (btw->window));
    gtk_window_set_destroy_with_parent (nbt->dialog, TRUE);
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
    g_signal_connect (nbt->dialog, "unrealize",
                      G_CALLBACK (new_billterm_dialog_unrealize_cb), nbt);
    g_signal_connect (nbt->dialog, "notify::visible",
                      G_CALLBACK (new_billterm_dialog_visible_cb), nbt);
    g_signal_connect (btw->window, "unrealize",
                      G_CALLBACK (new_billterm_dialog_unrealize_cb), nbt);
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
    {
#if GTK_CHECK_VERSION(4, 12, 0)
        gtk_column_view_scroll_to (btw->terms_view, selected_position, NULL,
                                   GTK_LIST_SCROLL_NONE, NULL);
#endif
    }
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
        term = btw->book && !btw->closing
            ? gncBillTermLookup (btw->book, &row->guid) : NULL;
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
    GncBillTerm *term = row && btw->book && !btw->closing
        ? gncBillTermLookup (btw->book, &row->guid) : NULL;

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
        if (btw->has_current_term &&
            guid_equal (&btw->current_term_guid, &request->term_guid))
            billterms_set_current_term (btw, NULL);
        gnc_suspend_gui_refresh ();
        gncBillTermBeginEdit (term);
        gncBillTermDestroy (term);
        gnc_resume_gui_refresh ();
    }
    g_object_unref (btw);
    g_free (request);
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

    DeleteBillTerm *request = g_new0 (DeleteBillTerm, 1);
    request->btw = g_object_ref (btw);
    request->book_guid = *qof_book_get_guid (btw->book);
    request->term_guid = *gncBillTermGetGUID (term);
    gnc_verify_dialog_async (GTK_WINDOW (btw->window), FALSE,
                             billterms_delete_response, request,
                             _("Are you sure you want to delete \"%s\"?"),
                             gncBillTermGetName (term));
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

    if (!btw || btw->closing)
        return;
    btw->closing = TRUE;
    g_signal_handlers_disconnect_by_data (widget, btw);
    if (btw->key_controller)
        g_signal_handlers_disconnect_by_data (btw->key_controller, btw);
    if (btw->term_selection)
        g_signal_handlers_disconnect_by_data (btw->term_selection, btw);
    if (btw->terms_view)
        g_signal_handlers_disconnect_by_data (btw->terms_view, btw);
    for (guint i = 0; i < G_N_ELEMENTS (btw->buttons); ++i)
        if (btw->buttons[i])
            g_signal_handlers_disconnect_by_data (btw->buttons[i], btw);
    if (btw->active_editor)
        new_billterm_dialog_unrealize_cb (widget, btw->active_editor);

    btw->window = NULL;
    gnc_unregister_gui_component (btw->component_id);
    g_clear_object (&btw->term_selection);
    g_clear_object (&btw->terms_model);
    g_object_unref (btw);
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

    btw = g_object_new (billterms_window_get_type (), NULL);
    g_object_add_weak_pointer (G_OBJECT(book), (gpointer *)&btw->book);
    btw->book = book;
    btw->session = gnc_get_current_session ();

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-billterms.glade",
                                    "terms_window"))
    {
        g_object_unref (builder);
        g_object_unref (btw);
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
        g_object_unref (btw);
        return NULL;
    }

    gtk_widget_set_name (btw->window, "gnc-id-bill-terms");
    gnc_widget_style_context_add_class (btw->window, "gnc-class-bill-terms");

    event_controller = gtk_event_controller_key_new ();
    btw->key_controller = event_controller;
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
    g_signal_connect (btw->window, "unrealize",
                      G_CALLBACK (billterms_window_destroy_cb), btw);
    btw->buttons[0] = GTK_WIDGET (gtk_builder_get_object (builder, "new_term_button"));
    g_signal_connect_object (btw->buttons[0], "clicked",
                             G_CALLBACK (billterms_new_term_cb), btw, 0);
    btw->buttons[1] = GTK_WIDGET (gtk_builder_get_object (builder, "delete_term_button"));
    g_signal_connect_object (btw->buttons[1], "clicked",
                             G_CALLBACK (billterms_delete_term_cb), btw, 0);
    btw->buttons[2] = GTK_WIDGET (gtk_builder_get_object (builder, "edit_term_button"));
    g_signal_connect_object (btw->buttons[2], "clicked",
                             G_CALLBACK (billterms_edit_term_cb), btw, 0);
    btw->buttons[3] = GTK_WIDGET (gtk_builder_get_object (builder, "close_button"));
    g_signal_connect_object (btw->buttons[3], "clicked",
                             G_CALLBACK (billterms_window_close), btw, 0);

    btw->component_id = gnc_register_gui_component (
        DIALOG_BILLTERMS_CM_CLASS, billterms_window_refresh_handler,
        billterms_window_close_handler, btw);
    gnc_gui_component_set_session (btw->component_id, btw->session);
    g_signal_connect (btw->window, "close-request",
                      G_CALLBACK (billterms_window_close_request_cb), btw);
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (btw->window), parent);
    gtk_window_present (GTK_WINDOW (btw->window));
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

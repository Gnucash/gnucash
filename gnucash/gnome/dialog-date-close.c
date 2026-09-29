/*
 * dialog-date-close.c -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-session.h"
#include "gnc-date-edit.h"
#include "gnc-account-sel.h"
#include "gnc-component-manager.h"

#include "business-gnome-utils.h"
#include "dialog-date-close.h"

typedef struct _dialog_date_close_window
{
    GtkWidget *dialog;
    GtkWidget *date;
    GtkWidget *post_date;
    GtkWidget *acct_combo;
    GtkWidget *memo_entry;
    GtkWidget *question_check;
    GncBillTerm *terms;
    time64 *t, *t2;
    GList * acct_types;
    GList * acct_commodities;
    QofBook *book;
    Account *acct;
    char **memo;
    gboolean retval;
    gboolean answer;
    time64 async_date;
    GncDateCloseResponseCallback callback;
    gpointer callback_data;
    gboolean completed;
    gboolean parent_destroyed;
    GWeakRef parent;
    gboolean has_parent;
    time64 async_post_date;
    char *async_memo;
    GncDateCloseFormResponseCallback form_callback;
    GPtrArray *signal_objects;
    gint component_id;
} DialogDateClose;

static void
date_close_track_objects(DialogDateClose *ddc, GtkBuilder *builder)
{
    ddc->signal_objects = g_ptr_array_new_with_free_func(g_object_unref);
    GSList *objects = gtk_builder_get_objects(builder);
    for (GSList *item = objects; item; item = item->next)
        g_ptr_array_add(ddc->signal_objects, g_object_ref(item->data));
    g_slist_free(objects);
    if (ddc->post_date)
        g_ptr_array_add(ddc->signal_objects, g_object_ref(ddc->post_date));
}

static void
date_close_disconnect(DialogDateClose *ddc)
{
    if (ddc->component_id)
    {
        gnc_unregister_gui_component(ddc->component_id);
        ddc->component_id = 0;
    }
    for (guint i = 0; i < ddc->signal_objects->len; ++i)
        g_signal_handlers_disconnect_by_data(
            g_ptr_array_index(ddc->signal_objects, i), ddc);
}

static void
date_close_session_closed(gpointer data)
{
    DialogDateClose *ddc = data;
    gtk_widget_destroy(ddc->dialog);
}

void gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data);


void
gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data)
{
    DialogDateClose *ddc = user_data;

    if (ddc->completed || (ddc->form_callback &&
        (!ddc->book || !gnc_current_session_exist() ||
         ddc->book != gnc_get_current_book() || qof_book_shutting_down(ddc->book))))
        return;

    if (ddc->acct_combo)
    {
        Account *acc;

        acc = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(ddc->acct_combo) );

        if (!acc)
        {
            gnc_error_dialog (GTK_WINDOW (ddc->dialog), "%s",
                              _("No Account selected. Please try again."));
            return;
        }

        if (xaccAccountGetPlaceholder (acc))
        {
            gnc_error_dialog (GTK_WINDOW (ddc->dialog), "%s",
                              _("Placeholder account selected. Please try again."));
            return;
        }

        ddc->acct = acc;
    }

    if (ddc->post_date)
        *ddc->t2 = gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->post_date));

    if (ddc->date)
    {
        if (ddc->terms)
            *ddc->t = gncBillTermComputeDueDate (ddc->terms, *ddc->t2);
        else
            *ddc->t = gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->date));
    }

    if (ddc->memo_entry && ddc->memo)
        *(ddc->memo) = gtk_editable_get_chars (GTK_EDITABLE (ddc->memo_entry),
                                               0, -1);
    if (ddc->question_check)
        ddc->answer = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ddc->question_check));
    ddc->retval = TRUE;
}

static void
fill_in_acct_info (DialogDateClose *ddc, gboolean set_default_acct)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (ddc->acct_combo);

    /* How do I set the book? */
    gnc_account_sel_set_acct_filters( gas, ddc->acct_types, ddc->acct_commodities );
    gnc_account_sel_set_new_account_ability( gas, TRUE );
    gnc_account_sel_set_new_account_modal( gas, TRUE );
    gnc_account_sel_set_account( gas, ddc->acct, set_default_acct );
}

static void
gnc_dialog_date_close_capture_response (GtkDialog *dialog, gint response,
                                        DialogDateClose *ddc)
{
    if (response == GTK_RESPONSE_OK)
        gnc_dialog_date_close_ok_cb (GTK_WIDGET (dialog), ddc);
}

static DialogDateClose *
gnc_dialog_date_close_create (GtkWidget *parent, const char *message,
                              const char *label_message,
                              gboolean ok_is_default, time64 *t,
                              gboolean destroy_with_parent)
{
    DialogDateClose *ddc;
    GtkWidget *date_box;
    GtkLabel *label;
    GtkBuilder *builder;
    if (!message || !label_message || !t)
        return NULL;

    ddc = g_new0 (DialogDateClose, 1);
    ddc->t = t;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-date-close.glade", "date_close_dialog");
    ddc->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "date_close_dialog"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ddc->dialog), "gnc-id-date-close");

    date_box = GTK_WIDGET(gtk_builder_get_object (builder, "date_box"));
    ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), *t);

    if (parent && GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW(ddc->dialog), GTK_WINDOW(parent));
        if (destroy_with_parent)
            gtk_window_set_destroy_with_parent (GTK_WINDOW(ddc->dialog), TRUE);
    }

    /* Set the labels */
    label = GTK_LABEL (gtk_builder_get_object (builder, "msg_label"));
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "label"));
    gtk_label_set_text (label, label_message);

    /* Setup signals */
    date_close_track_objects(ddc, builder);
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ddc);
    /* Capture inputs before asynchronous completion. The dialog's action
     * widget emits response before subsequently connected clicked handlers. */
    g_signal_connect (ddc->dialog, "response",
                      G_CALLBACK (gnc_dialog_date_close_capture_response), ddc);
    gtk_dialog_set_default_response (GTK_DIALOG (ddc->dialog),
        ok_is_default ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);

    g_object_unref (G_OBJECT(builder));
    return ddc;
}

static void
gnc_dialog_date_close_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                        DialogDateClose *ddc)
{
    ddc->parent_destroyed = TRUE;
    if (!ddc->completed && ddc->dialog)
        gtk_widget_destroy (ddc->dialog);
}

static void post_date_changed_cb (GNCDateEdit *gde, gpointer d);

static void
gnc_dialog_date_close_async_complete (GtkWidget *dialog,
                                      DialogDateClose *ddc,
                                      gboolean accepted, gboolean destroying)
{
    GncDateCloseResponseCallback callback = ddc->callback;
    gpointer callback_data = ddc->callback_data;
    time64 selected_date = ddc->async_date;
    GtkWidget *parent = ddc->has_parent ?
        GTK_WIDGET (g_weak_ref_get (&ddc->parent)) : NULL;

    if (ddc->completed)
    {
        g_clear_object (&parent);
        return;
    }
    ddc->completed = TRUE;
    date_close_disconnect(ddc);
    if (!destroying)
        gtk_widget_destroy (dialog);
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, ddc);
        if (ddc->parent_destroyed || gtk_widget_in_destruction (parent))
            accepted = FALSE;
    }
    else if (ddc->has_parent)
        accepted = FALSE;
    accepted = accepted && !destroying;
    if (ddc->has_parent)
        g_weak_ref_clear (&ddc->parent);
    g_clear_object (&parent);
    g_ptr_array_unref(ddc->signal_objects);
    g_free (ddc);
    callback (accepted, selected_date, callback_data);
}

static void
gnc_dialog_date_close_async_response (GtkDialog *dialog, gint response,
                                      DialogDateClose *ddc)
{
    if (response == GTK_RESPONSE_OK && !ddc->retval)
        return;
    gnc_dialog_date_close_async_complete (
        GTK_WIDGET (dialog), ddc, response == GTK_RESPONSE_OK && ddc->retval,
        FALSE);
}

static void
gnc_dialog_date_close_async_destroy (GtkWidget *dialog, DialogDateClose *ddc)
{
    gnc_dialog_date_close_async_complete (dialog, ddc, FALSE, TRUE);
}

void
gnc_dialog_date_close_async_parented (
    GtkWidget *parent, const char *message, const char *label_message,
    gboolean ok_is_default, time64 initial_date,
    GncDateCloseResponseCallback callback, gpointer user_data)
{
    DialogDateClose *ddc;
    time64 date_value = initial_date;
    if (!callback)
    {
        return;
    }

    ddc = gnc_dialog_date_close_create (parent, message, label_message,
                                        ok_is_default, &date_value,
                                        TRUE);
    if (!ddc)
    {
        callback (FALSE, initial_date, user_data);
        return;
    }
    ddc->async_date = initial_date;
    ddc->t = &ddc->async_date;
    gtk_dialog_set_default_response (
        GTK_DIALOG (ddc->dialog),
        ok_is_default ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    ddc->callback = callback;
    ddc->callback_data = user_data;
    ddc->retval = FALSE;
    if (parent)
    {
        g_weak_ref_init (&ddc->parent, G_OBJECT (parent));
        ddc->has_parent = TRUE;
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (gnc_dialog_date_close_parent_destroyed), ddc);
    }
    g_signal_connect (ddc->dialog, "response",
                      G_CALLBACK (gnc_dialog_date_close_async_response), ddc);
    g_signal_connect (ddc->dialog, "destroy",
                      G_CALLBACK (gnc_dialog_date_close_async_destroy), ddc);
    gtk_widget_show_all (ddc->dialog);
}

static void
gnc_dialog_date_close_form_complete (GtkWidget *dialog,
                                    DialogDateClose *ddc,
                                    gboolean accepted, gboolean destroying)
{
    GncDateCloseFormResponseCallback callback = ddc->form_callback;
    gpointer callback_data = ddc->callback_data;
    time64 due_date = ddc->async_date;
    time64 post_date = ddc->async_post_date;
    char *memo = ddc->async_memo;
    Account *account = ddc->acct;
    gboolean answer = ddc->answer;
    QofBook *book = ddc->book;
    GtkWidget *parent = ddc->has_parent ?
        GTK_WIDGET (g_weak_ref_get (&ddc->parent)) : NULL;

    if (ddc->completed)
    {
        g_clear_object (&parent);
        return;
    }
    ddc->completed = TRUE;
    if (accepted && (!book || !gnc_current_session_exist () ||
                     gnc_get_current_book () != book ||
                     !qof_book_is_open (book) || qof_book_shutting_down (book)))
        accepted = FALSE;
    accepted = accepted && !destroying;
    date_close_disconnect(ddc);
    if (!destroying)
        gtk_widget_destroy (dialog);
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, ddc);
        if (ddc->parent_destroyed || gtk_widget_in_destruction (parent))
            accepted = FALSE;
    }
    else if (ddc->has_parent)
        accepted = FALSE;
    if (ddc->has_parent)
        g_weak_ref_clear (&ddc->parent);
    if (book)
        g_object_remove_weak_pointer (G_OBJECT (book), (gpointer *)&ddc->book);
    if (ddc->terms)
        g_object_remove_weak_pointer(G_OBJECT(ddc->terms), (gpointer *)&ddc->terms);
    g_clear_object (&parent);
    if (!accepted)
    {
        g_free (memo);
        memo = NULL;
        account = NULL;
        answer = FALSE;
    }
    g_list_free(ddc->acct_types);
    g_list_free(ddc->acct_commodities);
    g_ptr_array_unref(ddc->signal_objects);
    g_free (ddc);
    callback (accepted, due_date, post_date, memo, account, answer,
              callback_data);
}

static void
gnc_dialog_date_close_form_response (GtkDialog *dialog, gint response,
                                     DialogDateClose *ddc)
{
    if (response == GTK_RESPONSE_OK && !ddc->retval)
        return;
    gnc_dialog_date_close_form_complete (
        GTK_WIDGET (dialog), ddc, response == GTK_RESPONSE_OK && ddc->retval,
        FALSE);
}

static void
gnc_dialog_date_close_form_destroy (GtkWidget *dialog, DialogDateClose *ddc)
{
    gnc_dialog_date_close_form_complete (dialog, ddc, FALSE, TRUE);
}

typedef struct
{
    GncDateCloseFormResponseCallback callback;
    gpointer user_data;
    time64 due_date;
    time64 post_date;
} InvalidDateCloseFormRequest;

static gboolean
gnc_dialog_date_close_form_invalid_idle (gpointer user_data)
{
    InvalidDateCloseFormRequest *request = user_data;
    request->callback (FALSE, request->due_date, request->post_date,
                       NULL, NULL, FALSE, request->user_data);
    g_free (request);
    return G_SOURCE_REMOVE;
}

void
gnc_dialog_dates_acct_question_async_parented (
    GtkWidget *parent, const char *message, const char *ddue_label_message,
    const char *post_label_message, const char *acct_label_message,
    const char *question_check_message, gboolean ok_is_default,
    gboolean set_default_acct, GList *acct_types, GList *acct_commodities,
    QofBook *book, GncBillTerm *terms, time64 initial_due_date,
    time64 initial_post_date, Account *initial_account,
    gboolean initial_answer, GncDateCloseFormResponseCallback callback,
    gpointer user_data)
{
    DialogDateClose *ddc;
    GtkBuilder *builder;
    GtkLabel *label;
    GtkWidget *date_box, *acct_box;

    g_return_if_fail (callback != NULL);
    if (!message || !ddue_label_message || !post_label_message ||
        !acct_label_message || !acct_types || !book)
    {
        InvalidDateCloseFormRequest *request =
            g_new (InvalidDateCloseFormRequest, 1);
        request->callback = callback;
        request->user_data = user_data;
        request->due_date = initial_due_date;
        request->post_date = initial_post_date;
        g_list_free (acct_types);
        g_list_free (acct_commodities);
        g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                         gnc_dialog_date_close_form_invalid_idle, request,
                         NULL);
        return;
    }

    ddc = g_new0 (DialogDateClose, 1);
    ddc->async_date = initial_due_date;
    ddc->async_post_date = initial_post_date;
    ddc->t = &ddc->async_date;
    ddc->t2 = &ddc->async_post_date;
    ddc->book = book;
    g_object_add_weak_pointer (G_OBJECT (book), (gpointer *)&ddc->book);
    ddc->acct_types = acct_types;
    ddc->acct_commodities = acct_commodities;
    ddc->acct = initial_account;
    ddc->memo = &ddc->async_memo;
    ddc->terms = terms;
    if (terms)
        g_object_add_weak_pointer(G_OBJECT(terms), (gpointer *)&ddc->terms);
    ddc->answer = initial_answer;
    ddc->form_callback = callback;
    ddc->callback_data = user_data;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-date-close.glade",
                               "date_account_dialog");
    ddc->dialog = GTK_WIDGET (gtk_builder_get_object (
        builder, "date_account_dialog"));
    gtk_widget_set_name (ddc->dialog, "gnc-id-date-close");

    acct_box = GTK_WIDGET (gtk_builder_get_object (builder, "acct_hbox"));
    ddc->acct_combo = gnc_account_sel_new ();
    gtk_box_pack_start (GTK_BOX (acct_box), ddc->acct_combo, TRUE, TRUE, 0);
    date_box = GTK_WIDGET (gtk_builder_get_object (builder, "date_hbox"));
    ddc->date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (date_box), ddc->date, TRUE, TRUE, 0);
    date_box = GTK_WIDGET (gtk_builder_get_object (builder, "post_date_box"));
    ddc->post_date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (date_box), ddc->post_date, TRUE, TRUE, 0);
    ddc->memo_entry = GTK_WIDGET (gtk_builder_get_object (builder, "memo_entry"));
    ddc->question_check = GTK_WIDGET (gtk_builder_get_object (
        builder, "question_check"));

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (ddc->dialog),
                                      GTK_WINDOW (parent));
    label = GTK_LABEL (gtk_builder_get_object (builder, "top_msg_label"));
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "date_label"));
    gtk_label_set_text (label, ddue_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "postdate_label"));
    gtk_label_set_text (label, post_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "acct_label"));
    gtk_label_set_text (label, acct_label_message);
    if (question_check_message)
    {
        gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (
            GTK_BIN (ddc->question_check))), question_check_message);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ddc->question_check),
                                      initial_answer);
    }
    else
    {
        gtk_widget_hide (ddc->question_check);
        gtk_widget_hide (GTK_WIDGET (gtk_builder_get_object (builder, "hide1")));
    }

    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->post_date), initial_post_date);
    if (terms)
    {
        g_signal_connect (ddc->post_date, "date_changed",
                          G_CALLBACK (post_date_changed_cb), ddc);
        gtk_widget_set_sensitive (ddc->date, FALSE);
        post_date_changed_cb (GNC_DATE_EDIT (ddc->post_date), ddc);
    }
    else
        gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), initial_due_date);
    fill_in_acct_info (ddc, set_default_acct);
    date_close_track_objects(ddc, builder);
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      ddc);
    ddc->retval = FALSE;
    if (parent)
    {
        g_weak_ref_init (&ddc->parent, G_OBJECT (parent));
        ddc->has_parent = TRUE;
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (gnc_dialog_date_close_parent_destroyed),
                          ddc);
    }
    g_signal_connect (ddc->dialog, "response",
                      G_CALLBACK (gnc_dialog_date_close_form_response), ddc);
    g_signal_connect (ddc->dialog, "destroy",
                      G_CALLBACK (gnc_dialog_date_close_form_destroy), ddc);
    ddc->component_id = gnc_register_gui_component("date-account-question", NULL,
                                                   date_close_session_closed, ddc);
    gnc_gui_component_set_session(ddc->component_id, gnc_get_current_session());
    gtk_dialog_set_default_response (GTK_DIALOG (ddc->dialog),
        ok_is_default ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    g_object_unref (builder);
    gtk_widget_show_all (ddc->dialog);
    gnc_date_grab_focus (GNC_DATE_EDIT (ddc->post_date));
}

static void
post_date_changed_cb (GNCDateEdit *gde, gpointer d)
{
    DialogDateClose *ddc = d;
    time64 post_date;
    time64 due_date = 0;

    if (ddc->completed || !ddc->terms || !ddc->book ||
        !gnc_current_session_exist() || ddc->book != gnc_get_current_book() ||
        qof_book_shutting_down(ddc->book))
        return;

    post_date = gnc_date_edit_get_date (gde);
    due_date = gncBillTermComputeDueDate (ddc->terms, post_date);
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), due_date);
}

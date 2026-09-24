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
#include "gnc-date-edit.h"
#include "gnc-account-sel.h"

#include "business-gnome-utils.h"
#include "dialog-date-close.h"

typedef struct
{
    time64 date;
    time64 post_date;
    char *memo;
    Account *acct;
    gboolean answer;
} DialogDateCloseResult;

typedef struct
{
    gatomicrefcount ref_count;
    GtkWindow *dialog;
    GtkWidget *date;
    GtkWidget *post_date;
    GtkWidget *acct_combo;
    GtkWidget *memo_entry;
    GtkWidget *question_check;
    GncBillTerm *terms;
    time64 date_value;
    time64 post_date_value;
    GList *acct_types;
    GList *acct_commodities;
    Account *acct;
    char *memo;
    gboolean answer;
    gboolean completed;
    GTask *task;
    GWeakRef parent;
    gulong parent_destroy_handler;
    gulong cancellable_handler;
    GMainContext *context;
} DialogDateClose;

static void date_close_complete (DialogDateClose *ddc, gboolean success,
                                 gboolean cancelled, const char *message);
static DialogDateClose *date_close_ref (DialogDateClose *ddc);
static void date_close_unref (DialogDateClose *ddc);

static void
date_close_result_free (DialogDateCloseResult *result)
{
    if (!result)
        return;

    g_free (result->memo);
    g_free (result);
}

static DialogDateCloseResult *
date_close_result_new (const DialogDateClose *ddc)
{
    DialogDateCloseResult *result = g_new0 (DialogDateCloseResult, 1);

    result->date = ddc->date_value;
    result->post_date = ddc->post_date_value;
    result->memo = g_strdup (ddc->memo);
    result->acct = ddc->acct;
    result->answer = ddc->answer;
    return result;
}

static void
date_close_disconnect_cancellable (DialogDateClose *ddc)
{
    GCancellable *cancellable;

    if (!ddc->task || !ddc->cancellable_handler)
        return;

    cancellable = g_task_get_cancellable (ddc->task);
    if (cancellable)
        g_cancellable_disconnect (cancellable, ddc->cancellable_handler);
    ddc->cancellable_handler = 0;
}

static void
date_close_destroy (DialogDateClose *ddc)
{
    GtkWindow *dialog = g_steal_pointer (&ddc->dialog);

    if (!dialog)
        return;

    g_signal_handlers_disconnect_by_data (dialog, ddc);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static DialogDateClose *
date_close_ref (DialogDateClose *ddc)
{
    g_atomic_ref_count_inc (&ddc->ref_count);
    return ddc;
}

static void
date_close_unref (DialogDateClose *ddc)
{
    GtkWindow *parent;

    if (!ddc || !g_atomic_ref_count_dec (&ddc->ref_count))
        return;

    parent = g_weak_ref_get (&ddc->parent);
    if (parent && ddc->parent_destroy_handler)
        g_signal_handler_disconnect (parent, ddc->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&ddc->parent);
    g_main_context_unref (ddc->context);

    date_close_disconnect_cancellable (ddc);
    date_close_destroy (ddc);
    g_list_free (ddc->acct_types);
    g_list_free (ddc->acct_commodities);
    g_free (ddc->memo);
    g_clear_object (&ddc->task);
    g_free (ddc);
}

static void
date_close_complete (DialogDateClose *ddc, gboolean success,
                     gboolean cancelled, const char *message)
{
    GTask *task;
    DialogDateCloseResult *result = NULL;

    if (!ddc || ddc->completed)
        return;

    ddc->completed = TRUE;
    date_close_disconnect_cancellable (ddc);
    task = g_steal_pointer (&ddc->task);

    if (success)
    {
        result = date_close_result_new (ddc);
        g_task_return_pointer (task, result,
                               (GDestroyNotify)date_close_result_free);
    }
    else if (cancelled)
    {
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_CANCELLED,
                                 "%s", _("The dialog was cancelled."));
    }
    else
    {
        g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_FAILED,
                                 "%s", message ? message :
                                 _("The dialog could not be created."));
    }

    date_close_unref (ddc);
    g_object_unref (task);
}

static gboolean
date_close_collect_values (DialogDateClose *ddc)
{
    Account *acct;

    if (ddc->acct_combo)
    {
        acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL (ddc->acct_combo));

        if (!acct)
        {
            gnc_error_dialog (ddc->dialog, "%s",
                              _("No Account selected. Please try again."));
            return FALSE;
        }

        if (xaccAccountGetPlaceholder (acct))
        {
            gnc_error_dialog (ddc->dialog, "%s",
                              _("Placeholder account selected. Please try again."));
            return FALSE;
        }

        ddc->acct = acct;
    }

    if (ddc->post_date)
        ddc->post_date_value =
            gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->post_date));

    if (ddc->date)
    {
        if (ddc->terms)
            ddc->date_value =
                gncBillTermComputeDueDate (ddc->terms, ddc->post_date_value);
        else
            ddc->date_value = gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->date));
    }

    if (ddc->memo_entry)
    {
        g_free (ddc->memo);
        ddc->memo = gtk_editable_get_chars (GTK_EDITABLE (ddc->memo_entry),
                                             0, -1);
    }

    if (ddc->question_check)
        ddc->answer = gtk_check_button_get_active (
            GTK_CHECK_BUTTON (ddc->question_check));

    return TRUE;
}

static void
date_close_respond (DialogDateClose *ddc, gint response)
{
    if (response == GTK_RESPONSE_OK)
    {
        if (!date_close_collect_values (ddc))
            return;

        date_close_complete (ddc, TRUE, FALSE, NULL);
        return;
    }

    date_close_complete (ddc, FALSE, TRUE, NULL);
}

static void
date_close_cancel_clicked_cb (GtkButton *button, DialogDateClose *ddc)
{
    (void)button;
    date_close_respond (ddc, GTK_RESPONSE_CANCEL);
}

static void
date_close_ok_clicked_cb (GtkButton *button, DialogDateClose *ddc)
{
    (void)button;
    date_close_respond (ddc, GTK_RESPONSE_OK);
}

static gboolean
date_close_close_request_cb (GtkWindow *dialog, DialogDateClose *ddc)
{
    (void)dialog;
    date_close_respond (ddc, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static void
date_close_destroy_cb (GtkWidget *widget, DialogDateClose *ddc)
{
    (void)widget;
    if (!ddc->completed)
        g_clear_object (&ddc->dialog);
    date_close_complete (ddc, FALSE, TRUE, NULL);
}

static void
date_close_parent_destroyed_cb (GtkWidget *widget, DialogDateClose *ddc)
{
    (void)widget;
    ddc->parent_destroy_handler = 0;
    date_close_complete (ddc, FALSE, TRUE, NULL);
}

static gboolean
date_close_cancel_on_main (gpointer user_data)
{
    DialogDateClose *ddc = user_data;

    date_close_complete (ddc, FALSE, TRUE, NULL);
    return G_SOURCE_REMOVE;
}

static void
date_close_cancelled_cb (GCancellable *cancellable, DialogDateClose *ddc)
{
    (void)cancellable;
    g_main_context_invoke_full (
        ddc->context, G_PRIORITY_DEFAULT, date_close_cancel_on_main,
        date_close_ref (ddc), (GDestroyNotify)date_close_unref);
}

static void
date_close_report_error_async (GCancellable *cancellable,
                               GAsyncReadyCallback callback,
                               gpointer user_data, const char *message)
{
    GTask *task = g_task_new (NULL, cancellable, callback, user_data);

    g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT,
                             "%s", message);
    g_object_unref (task);
}

static void
date_close_report_cancelled_async (GCancellable *cancellable,
                                  GAsyncReadyCallback callback,
                                  gpointer user_data)
{
    GTask *task = g_task_new (NULL, cancellable, callback, user_data);

    g_task_return_new_error (task, G_IO_ERROR, G_IO_ERROR_CANCELLED,
                             "%s", _("The dialog was cancelled."));
    g_object_unref (task);
}

static DialogDateClose *
date_close_new (GtkWidget *parent, GCancellable *cancellable,
                GAsyncReadyCallback callback, gpointer user_data)
{
    DialogDateClose *ddc = g_new0 (DialogDateClose, 1);

    g_atomic_ref_count_init (&ddc->ref_count);
    ddc->context = g_main_context_ref_thread_default ();

    ddc->task = g_task_new (NULL, cancellable, callback, user_data);
    g_weak_ref_init (&ddc->parent, parent);
    if (parent)
        ddc->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (date_close_parent_destroyed_cb), ddc);
    return ddc;
}

static gboolean
date_close_connect_cancellable (DialogDateClose *ddc)
{
    GCancellable *cancellable = g_task_get_cancellable (ddc->task);
    gulong handler;

    if (!cancellable)
        return TRUE;

    if (g_cancellable_is_cancelled (cancellable))
    {
        date_close_complete (ddc, FALSE, TRUE, NULL);
        return FALSE;
    }

    handler = g_cancellable_connect (
        cancellable, G_CALLBACK (date_close_cancelled_cb), date_close_ref (ddc),
        (GDestroyNotify)date_close_unref);
    if (!handler)
        return FALSE;

    ddc->cancellable_handler = handler;
    return TRUE;
}

static gboolean
date_close_setup_dialog (DialogDateClose *ddc, GtkBuilder *builder,
                         GtkWidget *parent, const char *dialog_name,
                         const char *cancel_button_name,
                         const char *ok_button_name, gboolean ok_is_default)
{
    GtkWidget *dialog = GTK_WIDGET (gtk_builder_get_object (builder, dialog_name));
    GtkWidget *cancel_button = GTK_WIDGET (
        gtk_builder_get_object (builder, cancel_button_name));
    GtkWidget *ok_button = GTK_WIDGET (
        gtk_builder_get_object (builder, ok_button_name));

    if (!dialog || !cancel_button || !ok_button)
        return FALSE;

    ddc->dialog = g_object_ref (GTK_WINDOW (dialog));
    if (parent)
        gtk_window_set_transient_for (ddc->dialog, GTK_WINDOW (parent));
    gtk_window_set_modal (ddc->dialog, TRUE);
    gtk_widget_set_name (GTK_WIDGET (ddc->dialog), "gnc-id-date-close");
    if (ok_is_default)
        gtk_window_set_default_widget (ddc->dialog, ok_button);

    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (date_close_cancel_clicked_cb), ddc);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (date_close_ok_clicked_cb), ddc);
    g_signal_connect (ddc->dialog, "close-request",
                      G_CALLBACK (date_close_close_request_cb), ddc);
    g_signal_connect (ddc->dialog, "destroy",
                      G_CALLBACK (date_close_destroy_cb), ddc);
    return TRUE;
}

static void
date_close_start (DialogDateClose *ddc)
{
    if (!date_close_connect_cancellable (ddc))
        return;

    gtk_window_present (ddc->dialog);
}

static void
fill_in_acct_info (DialogDateClose *ddc, gboolean set_default_acct)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (ddc->acct_combo);

    gnc_account_sel_set_acct_filters (gas, ddc->acct_types,
                                      ddc->acct_commodities);
    gnc_account_sel_set_new_account_ability (gas, TRUE);
    gnc_account_sel_set_new_account_modal (gas, TRUE);
    gnc_account_sel_set_account (gas, ddc->acct, set_default_acct);
}

static void
post_date_changed_cb (GNCDateEdit *gde, DialogDateClose *ddc)
{
    time64 post_date = gnc_date_edit_get_date (gde);
    time64 due_date = gncBillTermComputeDueDate (ddc->terms, post_date);

    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), due_date);
}

static gboolean
date_close_create_date_dialog (DialogDateClose *ddc, GtkWidget *parent,
                               const char *message, const char *label_message,
                               gboolean ok_is_default)
{
    GtkBuilder *builder;
    GtkWidget *date_box;
    GtkLabel *label;
    gboolean complete;

    builder = gtk_builder_new ();
    complete = gnc_builder_add_from_file (builder, "dialog-date-close.glade",
                                          "date_close_dialog");
    if (!complete || !date_close_setup_dialog (ddc, builder, parent,
                                                "date_close_dialog", "cancelbutton",
                                                "okbutton", ok_is_default))
    {
        g_object_unref (builder);
        return FALSE;
    }

    date_box = GTK_WIDGET (gtk_builder_get_object (builder, "date_box"));
    label = GTK_LABEL (gtk_builder_get_object (builder, "msg_label"));
    if (!date_box || !label)
    {
        g_object_unref (builder);
        return FALSE;
    }

    ddc->date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_append (GTK_BOX (date_box), ddc->date);
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), ddc->date_value);
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "label"));
    if (!label)
    {
        g_object_unref (builder);
        return FALSE;
    }
    gtk_label_set_text (label, label_message);

    g_object_unref (builder);
    return TRUE;
}

static gboolean
date_close_create_date_account_dialog (
    DialogDateClose *ddc, GtkWidget *parent, const char *message,
    const char *date_label_message, const char *post_label_message,
    const char *acct_label_message, const char *question_check_message,
    gboolean ok_is_default, gboolean set_default_acct, gboolean has_post_date,
    gboolean has_memo, gboolean has_question)
{
    GtkBuilder *builder;
    GtkWidget *date_box;
    GtkWidget *acct_box;
    GtkLabel *label;
    gboolean complete;

    builder = gtk_builder_new ();
    complete = gnc_builder_add_from_file (builder, "dialog-date-close.glade",
                                          "date_account_dialog");
    if (!complete || !date_close_setup_dialog (ddc, builder, parent,
                                                "date_account_dialog", "cancelbutton1",
                                                "okbutton1", ok_is_default))
    {
        g_object_unref (builder);
        return FALSE;
    }

    ddc->memo_entry = GTK_WIDGET (gtk_builder_get_object (builder, "memo_entry"));
    ddc->question_check = GTK_WIDGET (
        gtk_builder_get_object (builder, "question_check"));
    acct_box = GTK_WIDGET (gtk_builder_get_object (builder, "acct_hbox"));
    date_box = GTK_WIDGET (gtk_builder_get_object (builder, "date_hbox"));
    if (!ddc->memo_entry || !ddc->question_check || !acct_box || !date_box)
    {
        g_object_unref (builder);
        return FALSE;
    }

    ddc->acct_combo = gnc_account_sel_new ();
    gtk_box_append (GTK_BOX (acct_box), ddc->acct_combo);
    ddc->date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_append (GTK_BOX (date_box), ddc->date);

    label = GTK_LABEL (gtk_builder_get_object (builder, "top_msg_label"));
    if (!label)
    {
        g_object_unref (builder);
        return FALSE;
    }
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "date_label"));
    if (!label)
    {
        g_object_unref (builder);
        return FALSE;
    }
    gtk_label_set_text (label, date_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "acct_label"));
    if (!label)
    {
        g_object_unref (builder);
        return FALSE;
    }
    gtk_label_set_text (label, acct_label_message);

    if (has_post_date)
    {
        ddc->post_date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
        date_box = GTK_WIDGET (gtk_builder_get_object (builder, "post_date_box"));
        label = GTK_LABEL (gtk_builder_get_object (builder, "postdate_label"));
        if (!date_box || !label)
        {
            g_object_unref (builder);
            return FALSE;
        }

        gtk_box_append (GTK_BOX (date_box), ddc->post_date);
        gtk_label_set_text (label, post_label_message);
        gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->post_date),
                                 ddc->post_date_value);
        if (ddc->terms)
        {
            g_signal_connect (ddc->post_date, "date_changed",
                              G_CALLBACK (post_date_changed_cb), ddc);
            gtk_widget_set_sensitive (ddc->date, FALSE);
            post_date_changed_cb (GNC_DATE_EDIT (ddc->post_date), ddc);
        }
        else
            gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), ddc->date_value);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET (
            gtk_builder_get_object (builder, "postdate_label")), FALSE);
        gtk_widget_set_visible (GTK_WIDGET (
            gtk_builder_get_object (builder, "post_date_box")), FALSE);
        gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), ddc->date_value);
    }

    if (has_memo)
        gtk_editable_set_text (GTK_EDITABLE (ddc->memo_entry),
                               ddc->memo ? ddc->memo : "");
    else
    {
        gtk_widget_set_visible (ddc->memo_entry, FALSE);
        gtk_widget_set_visible (GTK_WIDGET (
            gtk_builder_get_object (builder, "memo_label")), FALSE);
        ddc->memo_entry = NULL;
    }

    if (has_question)
    {
        gtk_check_button_set_label (GTK_CHECK_BUTTON (ddc->question_check),
                                    question_check_message);
        gtk_check_button_set_active (GTK_CHECK_BUTTON (ddc->question_check),
                                      ddc->answer);
    }
    else
    {
        gtk_widget_set_visible (ddc->question_check, FALSE);
        ddc->question_check = NULL;
    }

    fill_in_acct_info (ddc, set_default_acct);
    if (has_post_date)
        gnc_date_grab_focus (GNC_DATE_EDIT (ddc->post_date));
    g_object_unref (builder);
    return TRUE;
}

void
gnc_dialog_date_close_parented_async (GtkWidget *parent, const char *message,
                                      const char *label_message,
                                      gboolean ok_is_default, time64 date,
                                      GCancellable *cancellable,
                                      GAsyncReadyCallback callback,
                                      gpointer user_data)
{
    DialogDateClose *ddc;

    if (cancellable && g_cancellable_is_cancelled (cancellable))
    {
        date_close_report_cancelled_async (cancellable, callback, user_data);
        return;
    }

    if (!message || !label_message)
    {
        date_close_report_error_async (cancellable, callback, user_data,
                                       _("The date dialog is incomplete."));
        return;
    }

    ddc = date_close_new (parent, cancellable, callback, user_data);
    ddc->date_value = date;
    if (!date_close_create_date_dialog (ddc, parent, message, label_message,
                                        ok_is_default))
    {
        date_close_complete (ddc, FALSE, FALSE,
                             _("The date dialog could not be created."));
        return;
    }

    date_close_start (ddc);
}

gboolean
gnc_dialog_date_close_parented_finish (GAsyncResult *result, time64 *date,
                                       GError **error)
{
    DialogDateCloseResult *dialog_result;

    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    g_return_val_if_fail (date, FALSE);

    dialog_result = g_task_propagate_pointer (G_TASK (result), error);
    if (!dialog_result)
        return FALSE;

    *date = dialog_result->date;
    date_close_result_free (dialog_result);
    return TRUE;
}

void
gnc_dialog_dates_acct_question_parented_async (
    GtkWidget *parent, const char *message, const char *ddue_label_message,
    const char *post_label_message, const char *acct_label_message,
    const char *question_check_message, gboolean ok_is_default,
    gboolean set_default_acct, GList *acct_types, GList *acct_commodities,
    QofBook *book, GncBillTerm *terms, time64 ddue, time64 post,
    const char *memo, Account *acct, gboolean answer,
    GCancellable *cancellable, GAsyncReadyCallback callback,
    gpointer user_data)
{
    DialogDateClose *ddc;

    if (cancellable && g_cancellable_is_cancelled (cancellable))
    {
        date_close_report_cancelled_async (cancellable, callback, user_data);
        return;
    }

    if (!message || !ddue_label_message || !post_label_message ||
        !acct_label_message || !acct_types || !book ||
        (question_check_message && !question_check_message[0]))
    {
        g_list_free (acct_types);
        date_close_report_error_async (cancellable, callback, user_data,
                                       _("The posting dialog is incomplete."));
        return;
    }

    ddc = date_close_new (parent, cancellable, callback, user_data);
    ddc->date_value = ddue;
    ddc->post_date_value = post;
    ddc->acct_types = acct_types;
    ddc->acct_commodities = g_list_copy (acct_commodities);
    ddc->terms = terms;
    ddc->memo = g_strdup (memo);
    ddc->acct = acct;
    ddc->answer = answer;
    if (!date_close_create_date_account_dialog (
            ddc, parent, message, ddue_label_message, post_label_message,
            acct_label_message, question_check_message, ok_is_default,
            set_default_acct, TRUE, TRUE, question_check_message != NULL))
    {
        date_close_complete (ddc, FALSE, FALSE,
                             _("The posting dialog could not be created."));
        return;
    }

    date_close_start (ddc);
}

gboolean
gnc_dialog_dates_acct_question_parented_finish (
    GAsyncResult *result, time64 *ddue, time64 *post, char **memo,
    Account **acct, gboolean *answer, GError **error)
{
    DialogDateCloseResult *dialog_result;

    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    g_return_val_if_fail (ddue && post && acct, FALSE);

    dialog_result = g_task_propagate_pointer (G_TASK (result), error);
    if (!dialog_result)
        return FALSE;

    *ddue = dialog_result->date;
    *post = dialog_result->post_date;
    if (memo)
        *memo = g_steal_pointer (&dialog_result->memo);
    if (answer)
        *answer = dialog_result->answer;
    *acct = dialog_result->acct;
    date_close_result_free (dialog_result);
    return TRUE;
}

void
gnc_dialog_date_acct_parented_async (GtkWidget *parent, const char *message,
                                     const char *date_label_message,
                                     const char *acct_label_message,
                                     gboolean ok_is_default,
                                     GList *acct_types, QofBook *book,
                                     time64 date, Account *acct,
                                     GCancellable *cancellable,
                                     GAsyncReadyCallback callback,
                                     gpointer user_data)
{
    DialogDateClose *ddc;

    if (cancellable && g_cancellable_is_cancelled (cancellable))
    {
        date_close_report_cancelled_async (cancellable, callback, user_data);
        return;
    }

    if (!message || !date_label_message || !acct_label_message ||
        !acct_types || !book)
    {
        g_list_free (acct_types);
        date_close_report_error_async (cancellable, callback, user_data,
                                       _("The date and account dialog is incomplete."));
        return;
    }

    ddc = date_close_new (parent, cancellable, callback, user_data);
    ddc->date_value = date;
    ddc->acct_types = acct_types;
    ddc->acct = acct;
    if (!date_close_create_date_account_dialog (
            ddc, parent, message, date_label_message, NULL,
            acct_label_message, NULL, ok_is_default, FALSE, FALSE, FALSE,
            FALSE))
    {
        date_close_complete (ddc, FALSE, FALSE,
                             _("The date and account dialog could not be created."));
        return;
    }

    date_close_start (ddc);
}

gboolean
gnc_dialog_date_acct_parented_finish (GAsyncResult *result, time64 *date,
                                      Account **acct, GError **error)
{
    DialogDateCloseResult *dialog_result;

    g_return_val_if_fail (g_task_is_valid (result, NULL), FALSE);
    g_return_val_if_fail (date && acct, FALSE);

    dialog_result = g_task_propagate_pointer (G_TASK (result), error);
    if (!dialog_result)
        return FALSE;

    *date = dialog_result->date;
    *acct = dialog_result->acct;
    date_close_result_free (dialog_result);
    return TRUE;
}

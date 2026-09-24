/********************************************************************\
 * dialog-dup-trans.c -- duplicate transaction dialog               *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 *                                                                    *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <limits.h>
#include <stdlib.h>

#include "dialog-dup-trans.h"
#include "dialog-utils.h"
#include "gnc-date-edit.h"
#include "qof.h"

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    GtkWindow *window;
    GtkWidget *date_edit;
    GtkWidget *num_edit;
    GtkWidget *tnum_edit;
    GtkWidget *link_edit;
    GtkWidget *duplicate_title_label;
    GtkWidget *date_label;
    GtkWidget *num_label;
    GtkWidget *tnum_label;
    GtkWidget *link_label;
    GWeakRef parent;
    gulong parent_destroy_handler;
    gboolean completed;
    gboolean show_date;
    gchar *doclink;
    GncDupTransDialogCallback callback;
    gpointer user_data;
} DupTransDialog;

static gboolean
parse_num (const char *string, long int *num)
{
    long int number;

    if (!string || !gnc_strisnum (string))
        return FALSE;

    number = strtol (string, NULL, 10);
    if (number == LONG_MIN || number == LONG_MAX)
        return FALSE;

    if (num)
        *num = number;
    return TRUE;
}

static gboolean
inc_dec_number (GtkWidget *widget, const gchar *text, gint increment)
{
    long int number;
    gchar *format;
    gchar *output;

    if (!parse_num (text, &number))
        return FALSE;

    number += increment;
    if (number == -1)
        number = 0;

    format = g_str_has_prefix (text, "0")
        ? g_strdup_printf ("%%0%ud", (guint)g_utf8_strlen (text, -1))
        : g_strdup ("%ld");
    output = g_strdup_printf (format, number);
    gnc_entry_set_text (GTK_ENTRY (widget), output);
    g_free (output);
    g_free (format);
    return TRUE;
}

static gboolean
number_key_pressed (GtkEventControllerKey *controller, guint keyval,
                    guint keycode, GdkModifierType state, gpointer user_data)
{
    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    const gchar *text = gnc_entry_get_text (GTK_ENTRY (widget));
    GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask ();
    gint increment;

    (void)keycode;
    (void)user_data;
    if (!gnc_strisnum (text) || (state & modifiers) == GDK_CONTROL_MASK ||
        (state & modifiers) == GDK_ALT_MASK)
        return FALSE;

    /* Semicolon is accepted for keyboard layouts where it emits '+'. */
    if (keyval == GDK_KEY_plus || keyval == GDK_KEY_KP_Add ||
        keyval == GDK_KEY_semicolon)
        increment = 1;
    else if (keyval == GDK_KEY_minus || keyval == GDK_KEY_KP_Subtract)
        increment = -1;
    else
        return FALSE;

    return inc_dec_number (widget, text, increment);
}

void
gnc_dup_trans_result_free (GncDupTransResult *result)
{
    if (!result)
        return;
    g_free (result->num);
    g_free (result->tnum);
    g_free (result->doclink);
    g_free (result);
}

static void
dup_trans_dialog_free (DupTransDialog *dialog)
{
    GtkWindow *parent;

    if (!dialog)
        return;
    parent = GTK_WINDOW (g_weak_ref_get (&dialog->parent));
    if (parent && dialog->parent_destroy_handler)
        g_signal_handler_disconnect (parent, dialog->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&dialog->parent);
    g_free (dialog->doclink);
    g_free (dialog);
}

static GncDupTransResult *
dup_trans_dialog_result (DupTransDialog *dialog)
{
    GncDupTransResult *result = g_new0 (GncDupTransResult, 1);

    result->date = gnc_date_edit_get_date (GNC_DATE_EDIT (dialog->date_edit));
    gnc_date_edit_get_gdate (GNC_DATE_EDIT (dialog->date_edit), &result->gdate);
    if (gtk_widget_get_visible (dialog->num_edit))
        result->num = g_strdup (gnc_entry_get_text (GTK_ENTRY (dialog->num_edit)));
    if (gtk_widget_get_visible (dialog->tnum_edit))
        result->tnum = g_strdup (gnc_entry_get_text (GTK_ENTRY (dialog->tnum_edit)));
    if (dialog->doclink && gtk_check_button_get_active (GTK_CHECK_BUTTON (dialog->link_edit)))
        result->doclink = g_strdup (dialog->doclink);
    return result;
}

static void
dup_trans_dialog_complete (DupTransDialog *dialog, gboolean accepted)
{
    GncDupTransResult *result = NULL;
    GtkWindow *window;

    if (!dialog || dialog->completed)
        return;
    dialog->completed = TRUE;
    if (accepted)
        result = dup_trans_dialog_result (dialog);

    window = dialog->window;
    dialog->window = NULL;
    if (window)
    {
        gtk_window_destroy (window);
        g_object_unref (window);
    }
    dialog->callback (result, dialog->user_data);
    dup_trans_dialog_free (dialog);
}

static void
dup_trans_dialog_accept_clicked (GtkButton *button, DupTransDialog *dialog)
{
    (void)button;
    dup_trans_dialog_complete (dialog, TRUE);
}

static void
dup_trans_dialog_cancel_clicked (GtkButton *button, DupTransDialog *dialog)
{
    (void)button;
    dup_trans_dialog_complete (dialog, FALSE);
}

static gboolean
dup_trans_dialog_close_requested (GtkWindow *window, DupTransDialog *dialog)
{
    (void)window;
    dup_trans_dialog_complete (dialog, FALSE);
    return TRUE;
}

static void
dup_trans_dialog_parent_destroyed (GtkWidget *parent, DupTransDialog *dialog)
{
    (void)parent;
    dialog->parent_destroy_handler = 0;
    dup_trans_dialog_complete (dialog, FALSE);
}

static void
dup_trans_dialog_setup (DupTransDialog *dialog, GtkWindow *parent,
                        const gchar *window_title, const gchar *title,
                        gboolean show_date, time64 initial_date,
                        const gchar *num, const gchar *tnum,
                        const gchar *doclink)
{
    GtkBuilder *builder = gtk_builder_new ();
    GtkWidget *box;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    const gchar *tooltip = _("You can type '+' or '-' to increment or decrement the number.");

    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade",
                               "duplicate_transaction_dialog");
    dialog->window = GTK_WINDOW (g_object_ref (gtk_builder_get_object (
        builder, "duplicate_transaction_dialog")));
    dialog->date_label = GTK_WIDGET (gtk_builder_get_object (builder, "date_label"));
    dialog->num_label = GTK_WIDGET (gtk_builder_get_object (builder, "num_label"));
    dialog->tnum_label = GTK_WIDGET (gtk_builder_get_object (builder, "tnum_label"));
    dialog->link_label = GTK_WIDGET (gtk_builder_get_object (builder, "link_label"));
    dialog->duplicate_title_label = GTK_WIDGET (gtk_builder_get_object (
        builder, "duplicate_title_label"));
    dialog->num_edit = GTK_WIDGET (gtk_builder_get_object (builder, "num_entry"));
    dialog->tnum_edit = GTK_WIDGET (gtk_builder_get_object (builder, "tnum_entry"));
    dialog->link_edit = GTK_WIDGET (gtk_builder_get_object (builder, "link_check_button"));
    box = GTK_WIDGET (gtk_builder_get_object (builder, "date_hbox"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "button77"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "button76"));

    dialog->show_date = show_date;
    dialog->doclink = g_strdup (doclink);
    dialog->date_edit = gnc_date_edit_new (initial_date, FALSE, FALSE);
    gnc_date_activates_default (GNC_DATE_EDIT (dialog->date_edit), TRUE);
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT (dialog->date_edit), dialog->date_label);
    gtk_box_prepend (GTK_BOX (box), dialog->date_edit);

    gtk_widget_set_name (GTK_WIDGET (dialog->window), "gnc-id-duplicate-transaction");
    gnc_widget_style_context_add_class (GTK_WIDGET (dialog->window), "gnc-class-transactions");
    gtk_window_set_modal (dialog->window, TRUE);
    if (parent)
    {
        gtk_window_set_transient_for (dialog->window, parent);
        dialog->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (dup_trans_dialog_parent_destroyed), dialog);
    }
    if (window_title)
        gtk_window_set_title (dialog->window, window_title);
    if (title)
    {
        gchar *markup = g_strdup_printf ("<b>%s</b>", title);
        gtk_label_set_markup (GTK_LABEL (dialog->duplicate_title_label), markup);
        g_free (markup);
    }

    gnc_entry_set_text (GTK_ENTRY (dialog->num_edit), num ? num : "");
    gnc_entry_set_text (GTK_ENTRY (dialog->tnum_edit), tnum ? tnum : "");
    gtk_widget_set_visible (dialog->date_label, show_date);
    gtk_widget_set_visible (dialog->date_edit, show_date);
    gtk_widget_set_visible (dialog->num_label, num != NULL);
    gtk_widget_set_visible (dialog->num_edit, num != NULL);
    gtk_widget_set_visible (dialog->tnum_label, tnum != NULL);
    gtk_widget_set_visible (dialog->tnum_edit, tnum != NULL);
    gtk_widget_set_visible (dialog->link_label, doclink != NULL);
    gtk_widget_set_visible (dialog->link_edit, doclink != NULL);

    if (!show_date && !tnum)
        gtk_label_set_markup (GTK_LABEL (dialog->num_label), _("Action/Number"));
    if (tnum)
    {
        gtk_entry_set_activates_default (GTK_ENTRY (dialog->num_edit), FALSE);
        gtk_entry_set_activates_default (GTK_ENTRY (dialog->tnum_edit), TRUE);
    }

    for (GtkWidget *entry = dialog->num_edit; entry;
         entry = entry == dialog->num_edit ? dialog->tnum_edit : NULL)
    {
        GtkEventController *controller = gtk_event_controller_key_new ();
        gtk_widget_add_controller (entry, controller);
        g_signal_connect (controller, "key-pressed", G_CALLBACK (number_key_pressed), dialog);
    }
    if (gnc_strisnum (num))
    {
        gtk_widget_set_tooltip_text (dialog->num_edit, tooltip);
        inc_dec_number (dialog->num_edit, num, 1);
    }
    if (gnc_strisnum (tnum))
    {
        gtk_widget_set_tooltip_text (dialog->tnum_edit, tooltip);
        inc_dec_number (dialog->tnum_edit, tnum, 1);
    }

    g_signal_connect (ok_button, "clicked", G_CALLBACK (dup_trans_dialog_accept_clicked), dialog);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (dup_trans_dialog_cancel_clicked), dialog);
    g_signal_connect (dialog->window, "close-request",
                      G_CALLBACK (dup_trans_dialog_close_requested), dialog);
    gtk_window_set_default_widget (dialog->window, ok_button);
    g_object_unref (builder);
}

void
gnc_dup_trans_dialog_async (GtkWindow *parent, const gchar *window_title,
                            const gchar *title, gboolean show_date,
                            time64 initial_date, const gchar *num,
                            const gchar *tnum, const gchar *doclink,
                            GncDupTransDialogCallback completed,
                            gpointer user_data)
{
    DupTransDialog *dialog;

    g_return_if_fail (!parent || GTK_IS_WINDOW (parent));
    g_return_if_fail (completed != NULL);
    dialog = g_new0 (DupTransDialog, 1);
    dialog->callback = completed;
    dialog->user_data = user_data;
    g_weak_ref_init (&dialog->parent, parent);
    dup_trans_dialog_setup (dialog, parent, window_title, title, show_date,
                            initial_date, num, tnum, doclink);
    if (show_date)
        gtk_widget_grab_focus (GNC_DATE_EDIT (dialog->date_edit)->date_entry);
    else if (num)
        gtk_widget_grab_focus (dialog->num_edit);
    gtk_window_present (dialog->window);
}

void
gnc_dup_date_dialog_async (GtkWindow *parent, const gchar *title,
                           const GDate *initial_date,
                           GncDupTransDialogCallback completed,
                           gpointer user_data)
{
    g_return_if_fail (initial_date != NULL);
    gnc_dup_trans_dialog_async (parent, NULL, title, TRUE,
                                gdate_to_time64 (*initial_date), NULL, NULL, NULL,
                                completed, user_data);
}

void
gnc_dup_time64_dialog_async (GtkWindow *parent, const gchar *window_title,
                             const gchar *title, time64 initial_date,
                             GncDupTransDialogCallback completed,
                             gpointer user_data)
{
    gnc_dup_trans_dialog_async (parent, window_title, title, TRUE, initial_date,
                                NULL, NULL, NULL, completed, user_data);
}

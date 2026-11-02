/*
 * gnc-autosave.c -- Functions related to the auto-save feature.
 *
 * Copyright (C) 2007 Christian Stimming <stimming@tuhh.de>
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

#include "gnc-autosave.h"

#include <glib/gi18n.h>
#include "gnc-engine.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-file.h"
#include "gnc-window.h"
#include "gnc-prefs.h"
#include "gnc-main-window.h"
#include "gnc-gui-query.h"
#include "dialog-utils.h"
#include <qoflog.h>

#define GNC_PREF_AUTOSAVE_SHOW_EXPLANATION "autosave-show-explanation"
#define GNC_PREF_AUTOSAVE_INTERVAL         "autosave-interval-minutes"
#define AUTOSAVE_SOURCE_ID "autosave_source_id"
#define AUTOSAVE_CONFIRMATION "autosave_confirmation"

#ifdef G_LOG_DOMAIN
# undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "gnc.gui.autosave"
static const QofLogModule log_module = G_LOG_DOMAIN;

static void
autosave_remove_timer_cb(QofBook *book, gpointer key, gpointer user_data);
static void gnc_autosave_add_timer (QofBook *book);
static void autosave_confirmation_book_destroyed (QofBook *book,
                                                   gpointer key,
                                                   gpointer user_data);

typedef struct
{
    QofBook *book; /* weak: cleared by the book data finalizer */
    GWeakRef toplevel;
    GWeakRef dialog;
    gboolean responding;
    gboolean parent_destroyed;
} AutosaveConfirmation;

static gboolean
autosave_book_is_current (QofBook *book)
{
    return book && !qof_book_shutting_down (book) &&
           gnc_current_session_exist () &&
           qof_session_get_book (gnc_get_current_session ()) == book;
}

static void
autosave_confirmation_free (AutosaveConfirmation *confirmation)
{
    GtkWindow *parent = g_weak_ref_get (&confirmation->toplevel);
    if (parent)
    {
        g_signal_handlers_disconnect_by_data (parent, confirmation);
        g_object_unref (parent);
    }
    g_weak_ref_clear (&confirmation->toplevel);
    g_weak_ref_clear (&confirmation->dialog);
    g_free (confirmation);
}

static void
autosave_confirmation_detach (AutosaveConfirmation *confirmation)
{
    QofBook *book = confirmation->book;
    if (book &&
        qof_book_get_data (book, AUTOSAVE_CONFIRMATION) == confirmation)
        /* A book-destroy event can close the dialog before book finalizers
         * run. Clear its data without changing the finalizer iteration. */
        qof_book_set_data_fin (book, AUTOSAVE_CONFIRMATION, NULL,
                               NULL);
}

static void
autosave_confirmation_destroyed (GtkWidget *dialog, gpointer user_data)
{
    AutosaveConfirmation *confirmation = user_data;
    QofBook *book;

    if (confirmation->responding)
        return;
    confirmation->responding = TRUE;
    g_signal_handlers_disconnect_by_data (dialog, confirmation);
    gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL,
                        GNC_PREF_AUTOSAVE_SHOW_EXPLANATION, TRUE);
    book = confirmation->book;
    if (autosave_book_is_current (book) &&
        qof_book_get_data (book, AUTOSAVE_CONFIRMATION) == confirmation)
    {
        if (!qof_book_is_readonly (book) && qof_book_session_not_saved (book))
        {
            gnc_autosave_remove_timer (book);
            gnc_autosave_add_timer (book);
        }
    }
    autosave_confirmation_detach (confirmation);
    autosave_confirmation_free (confirmation);
}

static void
autosave_confirmation_book_destroyed ([[maybe_unused]] QofBook *book,
                                      [[maybe_unused]] gpointer key,
                                      gpointer user_data)
{
    AutosaveConfirmation *confirmation = user_data;
    GtkWidget *dialog;
    if (!confirmation)
        return;
    confirmation->book = NULL;
    if (confirmation->responding)
        return; /* The response/destroy callback owns its cleanup. */
    confirmation->responding = TRUE;
    dialog = g_weak_ref_get (&confirmation->dialog);
    if (dialog)
    {
        g_signal_handlers_disconnect_by_data (dialog, confirmation);
        gtk_widget_destroy (dialog);
        g_object_unref (dialog);
    }
    autosave_confirmation_free (confirmation);
}

static void
autosave_save_now (QofBook *book, GtkWindow *parent)
{
    if (!autosave_book_is_current (book) || qof_book_is_readonly (book) ||
        gnc_file_save_in_progress ())
        return;

    if (GNC_IS_MAIN_WINDOW (parent))
        gnc_main_window_set_progressbar_window (GNC_MAIN_WINDOW (parent));
    if (GNC_IS_WINDOW (parent))
        gnc_window_set_progressbar_window (GNC_WINDOW (parent));
    gnc_file_save (parent);
    gnc_main_window_set_progressbar_window (NULL);
}

static void
autosave_confirmation_parent_destroyed ([[maybe_unused]] GtkWidget *parent,
                                        AutosaveConfirmation *confirmation)
{
    confirmation->parent_destroyed = TRUE;
}

static void
autosave_confirmation_response (GtkDialog *dialog, gint response,
                                gpointer user_data)
{
    AutosaveConfirmation *confirmation = user_data;
    QofBook *book;
    gboolean save_now = FALSE, switch_off = FALSE;
    gboolean show_again = TRUE;
    GtkWindow *parent;

    parent = gtk_window_get_transient_for (GTK_WINDOW (dialog));
    g_weak_ref_set (&confirmation->toplevel, parent);
    if (parent)
        g_signal_connect (parent, "destroy",
                          G_CALLBACK (autosave_confirmation_parent_destroyed),
                          confirmation);

    /* Destroy before any save can re-enter. Keep the book finalizer installed
     * through destruction: a destroy handler may close the session. */
    confirmation->responding = TRUE;
    g_signal_handlers_disconnect_by_data (dialog, confirmation);
    gtk_widget_destroy (GTK_WIDGET (dialog));
    if (confirmation->parent_destroyed)
        response = GTK_RESPONSE_NONE;
    book = confirmation->book;
    if (!autosave_book_is_current (book) || qof_book_is_readonly (book))
    {
        if (autosave_book_is_current (book) && qof_book_session_not_saved (book))
        {
            gnc_autosave_remove_timer (book);
            gnc_autosave_add_timer (book);
        }
        autosave_confirmation_detach (confirmation);
        autosave_confirmation_free (confirmation);
        return;
    }

    switch (response)
    {
    case 1: /* Yes, this time */
        save_now = TRUE;
        break;
    case 2: /* Yes, always */
        save_now = TRUE;
        show_again = FALSE;
        break;
    case 3: /* No, never */
        switch_off = TRUE;
        show_again = FALSE;
        break;
    default: /* No, not this time, close, or parent destruction */
        break;
    }

    gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL,
                        GNC_PREF_AUTOSAVE_SHOW_EXPLANATION, show_again);
    if (switch_off)
        gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL,
                             GNC_PREF_AUTOSAVE_INTERVAL, 0);

    /* Preference callbacks may change or destroy the session. The book
     * finalizer stays installed until the original context is revalidated. */
    book = confirmation->book;
    if (!autosave_book_is_current (book) || qof_book_is_readonly (book))
    {
        autosave_confirmation_detach (confirmation);
        autosave_confirmation_free (confirmation);
        return;
    }
    autosave_confirmation_detach (confirmation);

    if (save_now && !confirmation->parent_destroyed)
    {
        parent = g_weak_ref_get (&confirmation->toplevel);
        autosave_save_now (book, parent);
        g_clear_object (&parent);
    }
    else if (!switch_off && qof_book_session_not_saved (book))
    {
        gnc_autosave_remove_timer (book);
        gnc_autosave_add_timer (book);
    }
    autosave_confirmation_free (confirmation);
}

static void
autosave_confirm_async (QofBook *book, GtkWindow *toplevel)
{
    guint interval_mins = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_AUTOSAVE_INTERVAL);
    AutosaveConfirmation *confirmation;
    GtkWidget *dialog;

    if (qof_book_get_data (book, AUTOSAVE_CONFIRMATION))
        return;
    confirmation = g_new0 (AutosaveConfirmation, 1);
    confirmation->book = book;
    g_weak_ref_init (&confirmation->toplevel, toplevel);
    g_weak_ref_init (&confirmation->dialog, NULL);
    qof_book_set_data_fin (book, AUTOSAVE_CONFIRMATION, confirmation,
                           autosave_confirmation_book_destroyed);

    dialog = gtk_message_dialog_new (toplevel,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s",
        _("Save file automatically?"));
    gtk_widget_set_name (dialog, "gnc-id-auto-save");
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
        ngettext ("Your data file needs to be saved to your hard disk to save your changes. "
                  "GnuCash has a feature to save the file automatically every %d minute, "
                  "just as if you had pressed the \"Save\" button each time.\n\n"
                  "You can change the time interval or turn off this feature under "
                  "Edit->Preferences->General->Auto-save time interval.\n\n"
                  "Should your file be saved automatically?",
                  "Your data file needs to be saved to your hard disk to save your changes. "
                  "GnuCash has a feature to save the file automatically every %d minutes, "
                  "just as if you had pressed the \"Save\" button each time.\n\n"
                  "You can change the time interval or turn off this feature under "
                  "Edit->Preferences->General->Auto-save time interval.\n\n"
                  "Should your file be saved automatically?", interval_mins),
        interval_mins);
    gtk_dialog_add_buttons (GTK_DIALOG (dialog), _("_Yes, this time"), 1,
                            _("Yes, _always"), 2, _("No, n_ever"), 3,
                            _("_No, not this time"), 4, NULL);
    gtk_dialog_set_default_response (GTK_DIALOG (dialog), 4);
    g_weak_ref_set (&confirmation->dialog, dialog);
    g_signal_connect (
        dialog, "destroy", G_CALLBACK (autosave_confirmation_destroyed),
        confirmation);
    g_signal_connect (dialog, "response",
                      G_CALLBACK (autosave_confirmation_response), confirmation);
    gtk_widget_show (dialog);
}

/* Here's how autosave works:
 *
 * Initially, the book is in state "undirty". Once the book changes
 * state to "dirty", hence calling
 * gnc_main_window_autosave_dirty(true), the auto-save timer is added
 * and started. Now one out of two state changes can occur (well,
 * three actually), depending on which event occurs first:
 *
 * - Either the book changes state to "undirty", hence calling
 * gnc_main_window_autosave_dirty(false). In this case the auto-save
 * timer is removed and all returns to the initial state with the book
 * "undirty".
 *
 * - Or the auto-save timer hits its timeout, hence calling
 * autosave_timeout_cb(). If the explanation preference is enabled, an
 * asynchronous dialog is shown and the timer is cleared while the response
 * is pending. A negative response installs a fresh timer for a still-dirty
 * book; an affirmative response saves the original current book. Otherwise
 * the save starts immediately and the timer is removed.
 *
 * - As a third possibility, the book can also change state to
 * "closing", in which case the autosave_remove_timer_cb is called
 * that removes the auto-save timer and all returns to the initial
 * state with the book "undirty".
 */

static gboolean autosave_timeout_cb(gpointer user_data)
{
    QofBook *book = user_data;
    GtkWindow *toplevel;

    DEBUG("autosave_timeout_cb called\n");

    /* This one-shot source is consumed even when saving is no longer valid. */
    qof_book_set_data_fin (book, AUTOSAVE_SOURCE_ID, GUINT_TO_POINTER (0),
                           autosave_remove_timer_cb);

    /* Is there already a save in progress? If yes, return FALSE so that
       the timeout is automatically destroyed and the function will not
       be called again. */
    if (!autosave_book_is_current (book) || gnc_file_save_in_progress () ||
        qof_book_is_readonly (book))
        return FALSE;

    toplevel = gnc_ui_get_main_window (NULL);
    if (toplevel)
        g_object_ref (toplevel);
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_AUTOSAVE_SHOW_EXPLANATION))
        autosave_confirm_async (book, toplevel);
    else
        autosave_save_now (book, toplevel);
    g_clear_object (&toplevel);
    return FALSE;
}

static void
autosave_remove_timer_cb(QofBook *book, gpointer key, gpointer user_data)
{
    guint autosave_source_id = GPOINTER_TO_UINT(user_data);
    gboolean res;
    /* Remove the timer that would have triggered the next autosave */
    if (autosave_source_id > 0)
    {
        res = g_source_remove (autosave_source_id);
        DEBUG("Removing auto save timer with id %d, result=%s\n",
                autosave_source_id, (res ? "TRUE" : "FALSE"));

        /* Set the event source id to zero. */
        qof_book_set_data_fin(book, AUTOSAVE_SOURCE_ID,
                              GUINT_TO_POINTER(0), autosave_remove_timer_cb);
    }
}

void gnc_autosave_remove_timer(QofBook *book)
{
    autosave_remove_timer_cb(book, AUTOSAVE_SOURCE_ID,
                             qof_book_get_data(book, AUTOSAVE_SOURCE_ID));
}

static void gnc_autosave_add_timer(QofBook *book)
{
    guint interval_mins =
        gnc_prefs_get_float(GNC_PREFS_GROUP_GENERAL, GNC_PREF_AUTOSAVE_INTERVAL);

    /* Interval zero means auto-save is turned off. */
    if ( interval_mins > 0
            && ( ! gnc_file_save_in_progress() )
            && gnc_current_session_exist() )
    {
        /* Add a new timer (timeout) that runs until the next autosave
           timeout. */
        guint autosave_source_id =
            g_timeout_add_seconds(interval_mins * 60,
                                  autosave_timeout_cb, book);
        DEBUG("Adding new auto-save timer with id %d\n", autosave_source_id);

        /* Save the event source id for a potential removal, and also
           set the callback upon book closing */
        qof_book_set_data_fin(book, AUTOSAVE_SOURCE_ID,
                              GUINT_TO_POINTER(autosave_source_id),
                              autosave_remove_timer_cb);
    }
}

void gnc_autosave_dirty_handler (QofBook *book, gboolean dirty)
{
    DEBUG("gnc_main_window_autosave_dirty(dirty = %s)\n",
            (dirty ? "TRUE" : "FALSE"));
    if (dirty)
    {
        if (qof_book_is_readonly(book))
        {
            //DEBUG("Book is read-only, ignoring dirty flag");
            return;
        }

        /* Book state changed from non-dirty to dirty. */
        if (!qof_book_shutting_down(book))
        {
            /* Start the autosave timer.
            	 First stop a potentially running old timer. */
            gnc_autosave_remove_timer(book);
            /* Add a new timer (timeout) that runs until the next autosave
            	 timeout. */
            gnc_autosave_add_timer(book);
        }
        else
        {
            DEBUG("Shutting down book, ignoring dirty book");
        }
    }
    else
    {
        /* Book state changed from dirty to non-dirty (probably due to
           saving). Delete the running autosave timer. */
        gnc_autosave_remove_timer(book);
    }
}

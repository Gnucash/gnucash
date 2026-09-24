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
 * autosave_timeout_cb(). In this case gnc_file_save() is invoked, the
 * auto-save timer is removed, and all returns to the initial state
 * with the book "undirty".  (As an exceptional addition to this, on
 * the very first call to autosave_timeout_cb, if the key
 * autosave_show_explanation is true, an explanation dialog of this
 * feature is shown to the user, and the key autosave_show_explanation
 * is set to false to not show this dialog again.)
 *
 * - As a third possibility, the book can also change state to
 * "closing", in which case the autosave_remove_timer_cb is called
 * that removes the auto-save timer and all returns to the initial
 * state with the book "undirty".
 */


typedef struct
{
    QofBook *book;
    GWeakRef toplevel;
    GCancellable *cancellable;
} AutosaveConfirmation;

static void gnc_autosave_add_timer (QofBook *book);

static gboolean
autosave_book_is_current (QofBook *book)
{
    return book && gnc_current_session_exist () &&
           qof_session_get_book (gnc_get_current_session ()) == book;
}

static void
autosave_confirmation_free (AutosaveConfirmation *confirmation)
{
    g_weak_ref_clear (&confirmation->toplevel);
    g_clear_object (&confirmation->cancellable);
    g_free (confirmation);
}

static void
autosave_confirmation_cancel (QofBook *book, gpointer key, gpointer user_data)
{
    AutosaveConfirmation *confirmation = user_data;

    (void)book;
    (void)key;
    confirmation->book = NULL;
    g_cancellable_cancel (confirmation->cancellable);
}

static void
autosave_save_now (QofBook *book, GtkWindow *parent)
{
    if (!autosave_book_is_current (book) || qof_book_is_readonly (book) ||
        gnc_file_save_in_progress ())
        return;

    DEBUG ("autosave_timeout_cb: Really trigger auto-save now.\n");

    if (GNC_IS_MAIN_WINDOW (parent))
        gnc_main_window_set_progressbar_window (GNC_MAIN_WINDOW (parent));
    else
        DEBUG ("autosave_timeout_cb: toplevel is not a GNC_MAIN_WINDOW\n");
    if (GNC_IS_WINDOW (parent))
        gnc_window_set_progressbar_window (GNC_WINDOW (parent));
    else
        DEBUG ("autosave_timeout_cb: toplevel is not a GNC_WINDOW\n");

    gnc_file_save (parent);
    gnc_main_window_set_progressbar_window (NULL);
}

static void
autosave_confirmation_finished (GObject *source, GAsyncResult *result,
                                gpointer user_data)
{
    AutosaveConfirmation *confirmation = user_data;
    GError *error = NULL;
    gint response;
    gboolean save_now = FALSE;
    gboolean switch_off_autosave = FALSE;
    gboolean show_expl_again = TRUE;
    GtkWindow *parent = NULL;

    response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result,
                                               &error);
    if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        g_warning ("Auto-save confirmation failed: %s", error->message);

    if (autosave_book_is_current (confirmation->book))
        qof_book_set_data_fin (confirmation->book, AUTOSAVE_CONFIRMATION,
                               NULL, NULL);

    if (!error && confirmation->book)
    {
        switch (response)
        {
        case 0: /* Yes, this time */
            save_now = TRUE;
            break;
        case 1: /* Yes, always */
            save_now = TRUE;
            show_expl_again = FALSE;
            break;
        case 2: /* No, never */
            switch_off_autosave = TRUE;
            show_expl_again = FALSE;
            break;
        default: /* No, not this time, cancel, or parent destruction */
            break;
        }

        gnc_prefs_set_bool (GNC_PREFS_GROUP_GENERAL,
                            GNC_PREF_AUTOSAVE_SHOW_EXPLANATION,
                            show_expl_again);
        DEBUG ("autosave_timeout_cb: Show explanation again=%s\n",
               show_expl_again ? "TRUE" : "FALSE");

        if (switch_off_autosave)
        {
            gnc_prefs_set_float (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_AUTOSAVE_INTERVAL, 0);
            DEBUG ("autosave_timeout_cb: User chose to disable auto-save.\n");
        }
    }

    if (autosave_book_is_current (confirmation->book))
    {
        parent = g_weak_ref_get (&confirmation->toplevel);
        if (save_now)
            autosave_save_now (confirmation->book, parent);
        else if (!switch_off_autosave && !qof_book_is_readonly (confirmation->book) &&
                 qof_book_session_not_saved (confirmation->book))
        {
            gnc_autosave_remove_timer (confirmation->book);
            gnc_autosave_add_timer (confirmation->book);
        }
        g_clear_object (&parent);
    }

    g_clear_error (&error);
    autosave_confirmation_free (confirmation);
}

static void
autosave_confirm_async (QofBook *book, GtkWindow *toplevel)
{
    const char *buttons[] =
    {
        _("Yes, this time"),
        _("Yes, always"),
        _("No, never"),
        _("No, not this time"),
        NULL
    };
    AutosaveConfirmation *confirmation;
    GtkAlertDialog *dialog;
    guint interval_mins;
    gchar *detail;

    interval_mins = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL,
                                         GNC_PREF_AUTOSAVE_INTERVAL);
    detail = g_strdup_printf (ngettext (
        "Your data file needs to be saved to your hard disk to save your changes. "
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
        "Should your file be saved automatically?", interval_mins), interval_mins);

    /* A native alert is asynchronous: do not replace the book finalizer for
     * an already visible explanation, otherwise book destruction could leave
     * its first callback with a stale book pointer. */
    if (qof_book_get_data (book, AUTOSAVE_CONFIRMATION))
    {
        g_free (detail);
        return;
    }

    confirmation = g_new0 (AutosaveConfirmation, 1);
    confirmation->book = book;
    g_weak_ref_init (&confirmation->toplevel, toplevel);
    confirmation->cancellable = g_cancellable_new ();
    qof_book_set_data_fin (book, AUTOSAVE_CONFIRMATION, confirmation,
                           autosave_confirmation_cancel);

    dialog = gtk_alert_dialog_new ("%s", _("Save file automatically?"));
    gtk_alert_dialog_set_detail (dialog, detail);
    gtk_alert_dialog_set_buttons (dialog, buttons);
    gtk_alert_dialog_set_default_button (dialog, 3);
    gtk_alert_dialog_set_cancel_button (dialog, 3);
    gtk_alert_dialog_choose (dialog, toplevel, confirmation->cancellable,
                             autosave_confirmation_finished, confirmation);
    g_object_unref (dialog);
    g_free (detail);
}

static gboolean
autosave_timeout_cb (gpointer user_data)
{
    QofBook *book = user_data;
    GtkWindow *toplevel;

    DEBUG ("autosave_timeout_cb called\n");

    /* The source is one-shot after its timeout. Clear the stored id before
     * opening a native alert so a later dirty transition can install exactly
     * one fresh timer. */
    if (!autosave_book_is_current (book) || qof_book_is_readonly (book) ||
        gnc_file_save_in_progress ())
        return FALSE;
    qof_book_set_data_fin (book, AUTOSAVE_SOURCE_ID, GUINT_TO_POINTER (0),
                           autosave_remove_timer_cb);

    toplevel = GTK_WINDOW (gnc_ui_get_main_window (NULL));
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

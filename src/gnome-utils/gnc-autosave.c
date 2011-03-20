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

#include "config.h"

#include "gnc-autosave.h"

#include <glib/gi18n.h>
#include "gnc-engine.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-file.h"
#include "gnc-window.h"
#include "gnc-gconf-utils.h"
#include "gnc-main-window.h"
#include "gnc-gui-query.h"

#define KEY_AUTOSAVE_SHOW_EXPLANATION "autosave_show_explanation"
#define KEY_AUTOSAVE_INTERVAL "autosave_interval_minutes"
#define AUTOSAVE_SOURCE_ID "autosave_source_id"

#ifdef G_LOG_DOMAIN
# undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "gnc.gui.autosave"

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

static gboolean autosave_confirm(GtkWidget *toplevel)
{
    GtkWidget *dialog, *label;
    guint interval_mins =
        gnc_gconf_get_float(GCONF_GENERAL, KEY_AUTOSAVE_INTERVAL, NULL);
    gboolean switch_off_autosave, show_expl_again, save_now;
    gchar *message;
    gint response;

#define YES_THIS_TIME 1
#define YES_ALWAYS 2
#define NO_NEVER 3
#define NO_NOT_THIS_TIME 4
    /* The autosave timeout has occurred, and we should show the
       explanation dialog. */
    dialog =
        gtk_message_dialog_new(GTK_WINDOW(toplevel),
                               GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                               GTK_MESSAGE_QUESTION,
                               GTK_BUTTONS_NONE,
                               "%s",
                               _("Save file automatically?"));
    gtk_message_dialog_format_secondary_text
    (GTK_MESSAGE_DIALOG(dialog),
     _("Your data file needs to be saved to your hard disk to save your changes.  GnuCash has a feature to save the file automatically every %d minutes, just as if you had pressed the \"Save\" button each time. \n\n"
       "You can change the time interval or turn off this feature under Edit -> Preferences -> General -> Auto-save time interval. \n\n"
       "Should your file be saved automatically?"),
     interval_mins);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                           _("_Yes, this time"), YES_THIS_TIME,
                           _("Yes, _always"), YES_ALWAYS,
                           _("No, n_ever"), NO_NEVER,
                           _("_No, not this time"), NO_NOT_THIS_TIME,
                           NULL);
    gtk_dialog_set_default_response( GTK_DIALOG(dialog), NO_NOT_THIS_TIME);

    /* Run the modal dialog */
    response = gtk_dialog_run( GTK_DIALOG( dialog ) );
    gtk_widget_destroy( dialog );

    /* Evaluate the response */
    switch (response)
    {
    case YES_THIS_TIME:
        switch_off_autosave = FALSE;
        show_expl_again = TRUE;
        save_now = TRUE;
        break;
    case YES_ALWAYS:
        switch_off_autosave = FALSE;
        show_expl_again = FALSE;
        save_now = TRUE;
        break;
    case NO_NEVER:
        switch_off_autosave = TRUE;
        show_expl_again = FALSE;
        save_now = FALSE;
        break;
    default:
    case NO_NOT_THIS_TIME:
        switch_off_autosave = FALSE;
        show_expl_again = TRUE;
        save_now = FALSE;
    };

    /* Should we show this explanation again? */
    gnc_gconf_set_bool(GCONF_GENERAL, KEY_AUTOSAVE_SHOW_EXPLANATION, show_expl_again, NULL);
    g_debug("autosave_timeout_cb: Show explanation again=%s\n",
            (show_expl_again ? "TRUE" : "FALSE"));

    /* Should we switch off autosave? */
    if (switch_off_autosave)
    {
        gnc_gconf_set_float(GCONF_GENERAL, KEY_AUTOSAVE_INTERVAL, 0, NULL);
        g_debug("autosave_timeout_cb: User chose to disable auto-save.\n");
    }

    return save_now;
}


static gboolean autosave_timeout_cb(gpointer user_data)
{
    QofBook *book = user_data;
    gboolean show_explanation;
    gboolean save_now = TRUE;
    GtkWidget *toplevel;

    g_debug("autosave_timeout_cb called\n");

    /* Is there already a save in progress? If yes, return FALSE so that
       the timeout is automatically destroyed and the function will not
       be called again. */
    if (gnc_file_save_in_progress() || !gnc_current_session_exist())
        return FALSE;

    /* Store the current toplevel window for later use. */
    toplevel = gnc_ui_get_toplevel();

    /* Lookup gconf key to show an explanatory dialog, if wanted. */
    show_explanation =
        gnc_gconf_get_bool(GCONF_GENERAL, KEY_AUTOSAVE_SHOW_EXPLANATION, NULL);
    if (show_explanation)
    {
        save_now = autosave_confirm(toplevel);
    }

    if (save_now)
    {
        g_debug("autosave_timeout_cb: Really trigger auto-save now.\n");

        /* Timeout has passed - save the file. */
        if (GNC_IS_MAIN_WINDOW(toplevel))
            gnc_main_window_set_progressbar_window( GNC_MAIN_WINDOW( toplevel ) );
        else
            g_debug("autosave_timeout_cb: toplevel is not a GNC_MAIN_WINDOW\n");
        if (GNC_IS_WINDOW(toplevel))
            gnc_window_set_progressbar_window( GNC_WINDOW( toplevel ) );
        else
            g_debug("autosave_timeout_cb: toplevel is not a GNC_WINDOW\n");

        gnc_file_save();

        gnc_main_window_set_progressbar_window(NULL);

        /* Return FALSE so that the timeout is automatically destroyed and
           the function will not be called again. However, at least in my
           glib-2.12.4 the timer event source still exists after returning
           FALSE?! */
        return FALSE;
    }
    else
    {
        g_debug("autosave_timeout_cb: No auto-save this time, let the timeout run again.\n");
        /* Return TRUE so that the timeout is not removed but will be
           triggered again after the next time interval. */
        return TRUE;
    }
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
        g_debug("Removing auto save timer with id %d, result=%s\n",
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
        gnc_gconf_get_float(GCONF_GENERAL, KEY_AUTOSAVE_INTERVAL, NULL);

    /* Interval zero means auto-save is turned off. */
    if ( interval_mins > 0
            && ( ! gnc_file_save_in_progress() )
            && gnc_current_session_exist() )
    {
        /* Add a new timer (timeout) that runs until the next autosave
           timeout. */
        guint autosave_source_id =
#if GLIB_CHECK_VERSION(2, 14, 0)
            /* g_timeout_add_seconds is much more suitable here, but is new in
            	 glib-2.14. */
            g_timeout_add_seconds(interval_mins * 60,
                                  autosave_timeout_cb, book);
#else
            g_timeout_add(interval_mins * 60 * 1000,
                          autosave_timeout_cb, book);
#endif
        g_debug("Adding new auto-save timer with id %d\n", autosave_source_id);

        /* Save the event source id for a potential removal, and also
           set the callback upon book closing */
        qof_book_set_data_fin(book, AUTOSAVE_SOURCE_ID,
                              GUINT_TO_POINTER(autosave_source_id),
                              autosave_remove_timer_cb);
    }
}

void gnc_autosave_dirty_handler (QofBook *book, gboolean dirty)
{
    g_debug("gnc_main_window_autosave_dirty(dirty = %s)\n",
            (dirty ? "TRUE" : "FALSE"));
    if (dirty)
    {
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
            g_debug("Shutting down book, ignoring dirty book");
        }
    }
    else
    {
        /* Book state changed from dirty to non-dirty (probably due to
           saving). Delete the running autosave timer. */
        gnc_autosave_remove_timer(book);
    }
}

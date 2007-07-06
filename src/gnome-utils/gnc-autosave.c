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

static gboolean autosave_timeout_cb(gpointer user_data)
{
  QofBook *book = user_data;
  gboolean show_explanation;
  GtkWidget *toplevel;

  g_debug("autosave_timeout_cb called\n");

  /* Is there already a save in progress? If yes, return FALSE so that
     the timeout is automatically destroyed and the function will not
     be called again. */
  if (gnc_file_save_in_progress() || !gnc_current_session_exist())
    return FALSE;

  /* Store the current toplevel window for later use. */
  toplevel = gnc_ui_get_toplevel();

  /* Lookup gconf key to show an explanatory dialog the very first
     time this becomes active. */
  show_explanation =
    gnc_gconf_get_bool(GCONF_GENERAL, KEY_AUTOSAVE_SHOW_EXPLANATION, NULL);
  if (show_explanation) {
    guint interval_mins =
      gnc_gconf_get_float(GCONF_GENERAL, KEY_AUTOSAVE_INTERVAL, NULL);
    /* The autosave timeout has occurred for the very first
       time. Explain this feature. */
    gnc_info_dialog(NULL,
		      _("Your data file needs to be saved to your harddisk to save your changes.  GnuCash has a feature to save the file automatically every %d minutes.  This feature is being activated the very first time right now. \n\n"
			"If you like to change the time interval, you can do so under Edit -> Preferences -> General -> Auto-save time interval.  If you like to switch off this feature, set the time interval to zero and no auto-save will occur anymore.\n\n"
			"Press \"Close\" now so that your file will be saved."),
		      interval_mins);
    /* Don't show this explanation again. */
    gnc_gconf_set_bool(GCONF_GENERAL, KEY_AUTOSAVE_SHOW_EXPLANATION, FALSE, NULL);
  }

  /* Timeout has passed - save the file. */
  g_debug("autosave_timeout_cb: Really trigger auto-save now.\n");
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
     the function will not be called again. */
  return FALSE;
}

static void 
autosave_remove_timer_cb(QofBook *book, gpointer key, gpointer user_data)
{
  guint autosave_source_id = GPOINTER_TO_UINT(user_data);
  gboolean res;
  /* Remove the timer that would have triggered the next autosave */
  if (autosave_source_id > 0) {
    res = g_source_remove (autosave_source_id);
    g_debug("Removing auto save timer with id %d, result=%s\n",
	    autosave_source_id, (res ? "TRUE" : "FALSE"));

    /* Set the event source id to zero. */
    qof_book_set_data_fin(book, AUTOSAVE_SOURCE_ID,
			  GUINT_TO_POINTER(0), autosave_remove_timer_cb);
  }
}

static void autosave_remove_timer(QofBook *book)
{
  autosave_remove_timer_cb(book, AUTOSAVE_SOURCE_ID,
			   qof_book_get_data(book, AUTOSAVE_SOURCE_ID));
}

static void autosave_add_timer(QofBook *book)
{
  guint interval_mins =
    gnc_gconf_get_float(GCONF_GENERAL, KEY_AUTOSAVE_INTERVAL, NULL);

  /* Interval zero means auto-save is turned off. */
  if ( interval_mins > 0
       && ( ! gnc_file_save_in_progress() )
       && gnc_current_session_exist() ) {
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

void gnc_main_window_autosave_dirty (QofBook *book, gboolean dirty)
{
  g_debug("gnc_main_window_autosave_dirty(dirty = %s)\n",
	  (dirty ? "TRUE" : "FALSE"));
  if (dirty) {
    /* Book state changed from non-dirty to dirty. Start the autosave
       timer. */
    /* First stop a potentially running old timer. */
    autosave_remove_timer(book);
    /* Add a new timer (timeout) that runs until the next autosave
       timeout. */
    autosave_add_timer(book);
  } else {
    /* Book state changed from dirty to non-dirty (probably due to
       saving). Delete the running autosave timer. */
    autosave_remove_timer(book);
  }
}

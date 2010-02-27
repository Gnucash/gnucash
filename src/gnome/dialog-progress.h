/********************************************************************\
 * dialog-progress.h -- public GnuCash progress dialog functions    *
 * Copyright (C) 2000 Dave Peticolas                                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup Dialogs
    @{ */
/** @addtogroup ProgDialog Progress Dialog
    @brief Dialog for displaying progress of long-running operations.
    @details These functions constitute an API for streamlining the creation
    and management of progress dialogs. Once registered with the API,
    the dialog's display and behavior can be controlled via simple
    calls that prevent the caller from needing to know anything about
    the underlying GUI.

    A pop-up progress dialog can be created, displayed, and registered
    with the API by calling gnc_progress_dialog_new().  Alternatively,
    existing widgets can be registered with the API by calling
    gnc_progress_dialog_custom(). This method allows custom-made dialogs
    to hand off the management of typical progress-related widgets, and
    allows long-running operations report progress in a standard way.
    @{ */
/** @file dialog-progress.h
    @brief API for displaying progress of long-running operations.
    @author Copyright (C) 2000 Dave Peticolas
    @author Copyright (C) 2008 Charles Day
*/


#ifndef DIALOG_PROGRESS_H
#define DIALOG_PROGRESS_H

#include <glib.h>


typedef struct _GNCProgressDialog GNCProgressDialog;

typedef gboolean (*GNCProgressCancelFunc) (gpointer user_data);


/** Displays a pop-up dialog for showing the progress of a
 *  long-running activity.
 *
 *  By default only a title and progress bar are shown, but
 *  additional visual elements such as a Cancel button, text
 *  log, and additional labels can be activated by following
 *  with calls to some of the other API functions.
 *
 *  @param parent The parent window for which the progress dialog
 *  becomes modal.
 *
 *  @param use_ok_button If @c TRUE, an OK button is shown and must be
 *  clicked when progress is completed.
 *
 *  @return A ::GNCProgressDialog that identifies the dialog and
 *  is needed when making subsequent API calls. */
GNCProgressDialog * gnc_progress_dialog_new(GtkWidget *parent,
        gboolean use_ok_button);

/** Creates a dialog for displaying the progress of an activity using
 *  existing widgets. This allows long-running operations to update the
 *  progress in a custom dialog instead of a new pop-up.
 *
 *  @param primary a @c GtkLabel widget to use for primary text
 *
 *  @param secondary a @c GtkLabel widget to use for secondary text
 *
 *  @param bar a @c GtkProgressBar widget for filling or pulsing
 *
 *  @param suboperation a @c GtkLabel widget to use for suboperation text
 *
 *  @param log a @c GtkTextView widget for logging progress textually
 *
 *  Any of the parameters may be passed as @c NULL if management of
 *  that visual element is not desired.
 *
 *  @return A ::GNCProgressDialog that identifies the dialog and
 *  is needed when making subsequent API calls. */
GNCProgressDialog * gnc_progress_dialog_custom(GtkLabel       *primary,
        GtkLabel       *secondary,
        GtkProgressBar *bar,
        GtkLabel       *suboperation,
        GtkTextView    *log);

/** Set the title of a pop-up progress dialog. This function has no effect
 *  on dialogs registered using gnc_progress_dialog_custom().
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param title the window title to display */
void gnc_progress_dialog_set_title(GNCProgressDialog *progress,
                                   const char *title);

/** Set the primary text of the progress dialog. The text will
 *  be displayed using the HIG-recommended style. If @a str is @c NULL
 *  or blank, the label is hidden (this is the default state).
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param str the text to be displayed */
void gnc_progress_dialog_set_primary(GNCProgressDialog *progress,
                                     const gchar *str);

/** Set the primary text of the progress dialog. If @a str is @c NULL
 *  or blank, the label is hidden (this is the default state).
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param heading the text to be displayed
 *
 * NOTE: For HIG-compliant dialogs, use gnc_progress_dialog_set_primary()
 * instead. */
void gnc_progress_dialog_set_heading(GNCProgressDialog *progress,
                                     const char *heading);

/** Set the secondary text of the progress dialog. The text will
 *  be displayed using the HIG-recommended style. If @a str is @c NULL
 *  or blank, the label is hidden (this is the default state).
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param str the text to be displayed */
void gnc_progress_dialog_set_secondary(GNCProgressDialog *progress,
                                       const gchar *str);

/** Set the suboperation text of the progress dialog. The text will
 *  be displayed using the HIG-recommended style. If @a str is @c NULL
 *  or blank, the label is hidden (this is the default state).
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param str the text to be displayed */
void gnc_progress_dialog_set_sub(GNCProgressDialog *progress,
                                 const gchar *str);

/** Show the progress log and delete any existing text. If the dialog was
 *  created via gnc_progress_dialog_new(), the log is not shown by default.
 *  Calling this function will make it appear.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_reset_log(GNCProgressDialog *progress);

/** Append @a str to the progress log.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param str the text to be appended */
void gnc_progress_dialog_append_log(GNCProgressDialog *progress,
                                    const gchar *str);

/** Show that progress has been paused by appending "(paused)" to the
 *  suboperation text, the window title, or the primary text. The first
 *  that is both known and currently shown will be the one used.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_pause(GNCProgressDialog *progress);

/** Remove any indication that progress has paused by removing any existing
 *  "(paused)" suffix from the suboperation text, the window title, and the
 *  primary text.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_resume(GNCProgressDialog *progress);

/** Show a Cancel button and set the C function which will be called when it
 *  is pressed by the user. The cancel function must return a boolean value.
 *  If the value is @c TRUE, the window is hidden.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param cancel_func the callback function
 *
 *  @param user_data user data to be passed to @a cancel_func */
void gnc_progress_dialog_set_cancel_func(GNCProgressDialog *progress,
        GNCProgressCancelFunc cancel_func,
        gpointer user_data);

/** Show a Cancel button and set the Guile procedure that will be called
 *  when it is pressed by the user. It will be called after any C function
 *  registered with gnc_progress_dialog_set_cancel_func(). The procedure
 *  must return @c \#t if the dialog should be hidden. If there is no C or
 *  Guile cancel callback (the default state), the Cancel button is hidden.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param cancel_scm_func the Guile callback procedure */
void gnc_progress_dialog_set_cancel_scm_func(GNCProgressDialog *progress,
        SCM cancel_scm_func);

/** Set the fraction of the progress bar to fill, where 0 is empty and
 *  1 is full. If @a value is over 1, the bar will pulse instead of fill.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param value the fraction of the bar to fill */
void gnc_progress_dialog_set_value(GNCProgressDialog *progress, gdouble value);

/** Create a new "virtual" progress bar that, as it becomes full, will fill
 *  the current bar by the fraction specified by @a weight. All calls to
 *  gnc_progress_dialog_set_value() will operate on the new bar until
 *  gnc_progress_dialog_pop() is called.
 *
 *  This can be used to split an operation into weighted sub-operations. For
 *  example, if a particular suboperation should fill 30% of the bar, call
 *  gnc_progress_dialog_push() with a @a weight of 0.3. Calls to
 *  gnc_progress_dialog_set_value() will fill the virtual bar, which
 *  in turn trickles up at the 0.3 rate.
 *
 *  Multiple calls to gnc_progress_dialog_push() can be used to create a
 *  stack of virtual bars, each subordinate to the last. This allows a task
 *  to be split into any number of levels of sub-tasks.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @param weight the requested fraction of the current bar that the new bar
 *  will represent (The fraction actually assigned will be the lesser of the
 *  requested amount and the amount of the bar that is unfilled.)
 *
 *  @return the number of times that gnc_progress_dialog_pop() would have to
 *  be called to return to the top level. */
guint gnc_progress_dialog_push(GNCProgressDialog *progress, gdouble weight);

/** Moves up one level in the stack of virtual bars. See
 *  gnc_progress_dialog_push() for an explanation of virtual bars.
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @return the number of times that gnc_progress_dialog_pop() would have to
 *  be called again to return to the top level. */
guint gnc_progress_dialog_pop(GNCProgressDialog *progress);

/** Fills the current progress bar, then calls gnc_progress_dialog_pop().
 *
 *  @param progress a ::GNCProgressDialog
 *
 *  @return the value returned by gnc_progress_dialog_pop() */
guint gnc_progress_dialog_pop_full(GNCProgressDialog *progress);

/** Pop up to the top level and clear the progress bar.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_reset_value(GNCProgressDialog *progress);

/** Update the GUI of the progress dialog, and call any pending cancel
 *  callbacks. This function will be called automatically by the other
 *  functions, including gnc_progress_dialog_set_value.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_update(GNCProgressDialog *progress);

/** Set the progress meter to fully complete, change the heading, if
 *  any, to "Complete", enable the 'OK' button, and make the dialog
 *  non-modal.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_finish(GNCProgressDialog *progress);

/** Destroy the dialog. If gnc_progress_dialog_finish has been called,
 *  the dialog will not be destroyed until the user dismisses the window.
 *  This function must be called in order to reclaim the dialog's memory.
 *
 *  @param progress a ::GNCProgressDialog */
void gnc_progress_dialog_destroy(GNCProgressDialog *progress);

#endif
/** @} */
/** @} */

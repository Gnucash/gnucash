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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __DIALOG_PROGRESS_H__
#define __DIALOG_PROGRESS_H__

#include <glib.h>


typedef struct _GNCProgressDialog GNCProgressDialog;

typedef gboolean (*GNCProgressCancelFunc) (gpointer user_data);


/* Create and return a dialog for displaying the progress of
 * an activity. Useful for long-running operations. */
GNCProgressDialog * gnc_progress_dialog_new (GtkWidget *parent,
                                             gboolean use_ok_button);

/* Set the title of the progress dialog. */
void gnc_progress_dialog_set_title (GNCProgressDialog *progress,
                                    const char *title);

/* Set the heading (the text above the progress meter) of
 * the progress dialog. If it is NULL or blank, the heading
 * is hidden (this is the default state). */
void gnc_progress_dialog_set_heading (GNCProgressDialog *progress,
                                      const char *heading);

/* Set the upper and lower bound of the progress meter. */
void gnc_progress_dialog_set_limits (GNCProgressDialog *progress,
                                     gfloat min, gfloat max);

/* Set the activity mode. If TRUE, the dialog just indicates
 * that stuff is happening, rather than a percentage complete. */
void gnc_progress_dialog_set_activity_mode (GNCProgressDialog *progress,
                                            gboolean activity_mode);

/* Set the C function which will be called if the user hits the
 * 'cancel' button. The cancel function returns a boolean value.
 * If the value is TRUE, the window is hidden. */
void gnc_progress_dialog_set_cancel_func (GNCProgressDialog *progress,
                                          GNCProgressCancelFunc cancel_func,
                                          gpointer user_data);

/* Set a guile function which will be called if the user hits cancel.
 * Will be called after the C function, if any. The function should
 * return #t if the dialog should be hidden. If there is no C or guile
 * cancel callback (the default state), the cancel button is inactive. */
void gnc_progress_dialog_set_cancel_scm_func (GNCProgressDialog *progress,
                                              SCM cancel_scm_func);

/* Set the value of the progress dialog. */
void gnc_progress_dialog_set_value (GNCProgressDialog *progress, gfloat value);

/* Update the GUI of the progress dialog, and call any pending cancel
 * callbacks. This function will be called automatically by the other
 * functions, including gnc_progress_dialog_set_value. */
void gnc_progress_dialog_update (GNCProgressDialog *progress);

/* Set the progress meter to fully complete, change the heading, if
 * any, to "Complete", enable the 'OK' button, and make the dialog
 * non-modal. */
void gnc_progress_dialog_finish (GNCProgressDialog *progress);

/* Destroy the dialog. If gnc_progress_dialog_finish has been called,
 * the dialog will not be destroyed until the user dismisses the window.
 * This function must be called in order to reclaim the dialog's memory. */
void gnc_progress_dialog_destroy (GNCProgressDialog *progress);

#endif

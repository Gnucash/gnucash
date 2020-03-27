/********************************************************************\
 * dialog-options.h -- GNOME option handling                        *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
\********************************************************************/

#ifndef OPTIONS_DIALOG_H
#define OPTIONS_DIALOG_H

#include <libguile.h>
#ifdef __cplusplus
class GncOption;
class GncOptionDB;
using GNCOption = GncOption;
using GNCOptionDB = GncOptionDB;
extern "C"
{
#else
#include "option-util.h"
typedef GNCOption GncOption;
typedef GNCOptionDB GncOptionDB;
#endif
#include <guile-mappings.h>
#include <gtk/gtk.h>


/**
 * Retrieve the GtkWidget* used for packing the option control.
 *
 * This is not ncessarily the widget that has the input or handles signals.
 * @param option The option
 * @return a GtkWidget* const
 */
GtkWidget* const gnc_option_get_gtk_widget (const GncOption* option);

typedef struct gnc_option_win GNCOptionWin;

typedef void (* GNCOptionWinCallback)(GNCOptionWin *, gpointer data);

GNCOptionWin * gnc_options_dialog_new_modal (gboolean modal, gchar *title,
                                             const char *component_class,
                                             GtkWindow *parent);
GNCOptionWin * gnc_options_dialog_new (gchar *title, GtkWindow *parent);
void gnc_options_dialog_destroy (GNCOptionWin * win);
void gnc_options_register_stocks (void);

GtkWidget * gnc_options_dialog_widget (GNCOptionWin * win);
GtkWidget * gnc_options_page_list (GNCOptionWin * win);
GtkWidget * gnc_options_dialog_notebook (GNCOptionWin * win);

void gnc_options_dialog_changed (GNCOptionWin *win);

void gnc_option_changed_widget_cb (GtkWidget *widget, GncOption *option);
void gnc_option_changed_option_cb (GtkWidget *dummy, GncOption *option);

void gnc_options_dialog_set_apply_cb (GNCOptionWin * win,
                                      GNCOptionWinCallback thunk,
                                      gpointer cb_data);
void gnc_options_dialog_set_help_cb (GNCOptionWin * win,
                                     GNCOptionWinCallback thunk,
                                     gpointer cb_data);
void gnc_options_dialog_set_close_cb (GNCOptionWin * win,
                                      GNCOptionWinCallback thunk,
                                      gpointer cb_data);

void gnc_options_dialog_set_global_help_cb (GNCOptionWinCallback thunk,
                                            gpointer cb_data);

void gnc_options_dialog_build_contents (GNCOptionWin *win,
                                        GNCOptionDB  *odb);
void gnc_options_dialog_build_contents_full (GNCOptionWin *win,
                                             GNCOptionDB  *odb,
                                             gboolean show_dialog);
void gnc_options_ui_initialize (void);

/** Set the help callback to 'gnc_book_options_help_cb' to open a help browser
 *  and point it to the Book Options link in the Help file.
 */
void gnc_options_dialog_set_book_options_help_cb (GNCOptionWin *win);

/** Set the initial values of new book options to values specified in user
 *  preferences.
 */
void gnc_options_dialog_set_new_book_option_values (GNCOptionDB *odb);
#ifdef __cplusplus
}
#endif
#endif /* OPTIONS_DIALOG_H */

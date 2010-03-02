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

#include "option-util.h"

typedef struct gnc_option_win GNCOptionWin;

typedef void (* GNCOptionWinCallback)(GNCOptionWin *, gpointer data);

GNCOptionWin * gnc_options_dialog_new(gchar *title);
GNCOptionWin * gnc_options_dialog_new_w_dialog(gchar *title, GtkWidget *dialog);
void gnc_options_dialog_destroy(GNCOptionWin * win);
void gnc_options_register_stocks (void);

GtkWidget * gnc_options_dialog_widget(GNCOptionWin * win);
GtkWidget * gnc_options_dialog_notebook(GNCOptionWin * win);

void gnc_options_dialog_changed (GNCOptionWin *win);
void gnc_option_changed_widget_cb(GtkWidget *widget, GNCOption *option);
void gnc_option_changed_option_cb(GtkWidget *dummy, GNCOption *option);

void gnc_options_dialog_set_apply_cb(GNCOptionWin * win,
                                     GNCOptionWinCallback thunk,
                                     gpointer cb_data);
void gnc_options_dialog_set_help_cb(GNCOptionWin * win,
                                    GNCOptionWinCallback thunk,
                                    gpointer cb_data);
void gnc_options_dialog_set_close_cb(GNCOptionWin * win,
                                     GNCOptionWinCallback thunk,
                                     gpointer cb_data);

void gnc_options_dialog_set_global_help_cb(GNCOptionWinCallback thunk,
        gpointer cb_data);

void gnc_options_dialog_build_contents(GNCOptionWin *win,
                                       GNCOptionDB  *odb);

/* Both apply_cb and close_cb should be scheme functions with 0 arguments.
 * References to these functions will be held until the close_cb is called
 */
void gnc_options_dialog_set_scm_callbacks (GNCOptionWin *win,
        SCM apply_cb,
        SCM close_cb);

/*****************************************************************/
/* Option Registration                                           */

/* Function to set the UI widget based upon the option */
typedef GtkWidget *
(*GNCOptionUISetWidget)	(GNCOption *option, GtkBox *page_box,
                         GtkTooltips *tooltips,
                         char *name, char *documentation,
                         /* Return values */
                         GtkWidget **enclosing, gboolean *packed);

/* Function to set the UI Value for a particular option */
typedef gboolean
(*GNCOptionUISetValue)	(GNCOption *option, gboolean use_default,
                         GtkWidget *widget, SCM value);

/* Function to get the UI Value for a particular option */
typedef SCM
(*GNCOptionUIGetValue)	(GNCOption *option, GtkWidget *widget);


typedef struct gnc_option_def
{
    const char *		option_name;
    GNCOptionUISetWidget	set_widget;
    GNCOptionUISetValue	set_value;
    GNCOptionUIGetValue	get_value;
} GNCOptionDef_t;


/* Register a new option type in the UI */
void gnc_options_ui_initialize (void);
void gnc_options_ui_register_option (GNCOptionDef_t *option);
GNCOptionDef_t * gnc_options_ui_get_option (const char *option_name);

#endif /* OPTIONS_DIALOG_H */

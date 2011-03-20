/********************************************************************\
 * dialog-utils.h -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
 * Copyright (C) 2005 David Hampton <hampton@employees.org>         *
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

#ifndef DIALOG_UTILS_H
#define DIALOG_UTILS_H

#include <glade/glade.h>
#include <gtk/gtk.h>
#include "qof.h"

/* option button callback function */
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
typedef void (*GNCOptionCallback) (GtkWidget *,
                                   gpointer user_data);
#else
typedef void (*GNCOptionCallback) (GtkWidget *, gint index,
                                   gpointer user_data);
#endif

/* Structure for building option buttons */
typedef struct _GNCOptionInfo GNCOptionInfo;
struct _GNCOptionInfo
{
    char *name;
    char *tip;
    GNCOptionCallback callback;
    gpointer user_data;
};


/**** PROTOTYPES *************************************************/

GtkWidget * gnc_build_option_menu (GNCOptionInfo *option_info,
                                   gint num_options);


GtkToolbarStyle gnc_get_toolbar_style (void);
void gnc_get_deficit_color (GdkColor *color);
void gnc_set_label_color (GtkWidget *label, gnc_numeric value);


/********************************************************************\
 * Returns the window size to use for the given option prefix,      *
 * if window sizes are being saved, otherwise returns 0 for both.   *
 *                                                                  *
 * Args: prefix - the option name prefix                            *
 *       width  - pointer to width                                  *
 *       height - pointer to height                                 *
 * Returns: nothing                                                 *
 \*******************************************************************/
void gnc_restore_window_size (const char *prefix, GtkWindow *window);

/********************************************************************\
 * Save the window size into options whose names are determined     *
 * by the string prefix.                                            *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       width  - width of the window to save                       *
 *       height - height of the window to save                      *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_save_window_size (const char *section, GtkWindow *window);


void gnc_option_menu_init (GtkWidget * option_menu);
void gnc_option_menu_init_w_signal(GtkWidget * w,
                                   GCallback f,
                                   gpointer cb_data);
int  gnc_option_menu_get_active (GtkWidget * option_menu);

/********************************************************************\
 * Adjust the window size if it is bigger than the screen size.     *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_window_adjust_for_screen (GtkWindow * window);


gboolean gnc_handle_date_accelerator (GdkEventKey *event,
                                      struct tm *tm,
                                      const char *date_str);


/* This function sets a pixmap of a set or cleared check mark in a
 * cell of a GtkCList row.
 *
 * There are some restrictions on using this function. If you mix
 * this function with gtk_clist_{insert, prepend, remove} before
 * the clist is realized, then the checks may appear in the wrong
 * place. Stick to gtk_clist_append, or use gnc_clist_set_check
 * after you have built the list. This only applies to unrealized
 * widgets. */
#ifdef __GTK_CLIST_H__
void gnc_clist_set_check (GtkCList *list, int row, int col,
                          gboolean checked);
#endif

GladeXML * gnc_glade_xml_new (const char *filename, const char *root);
GtkWidget * gnc_glade_lookup_widget (GtkWidget *widget, const char *name);
void gnc_glade_autoconnect_full_func(const gchar *handler_name,
                                     GObject *signal_object,
                                     const gchar *signal_name,
                                     const gchar *signal_data,
                                     GObject *connect_object,
                                     gboolean after,
                                     gpointer user_data);

/** This function generates a button with icon and adds it to a
 *  GtkDialog.  This is similar to just adding a stock button to the
 *  dialog, only you can add an arbitrary pairing of button and label,
 *  which the stock system doesn't provide.
 *
 *  @param dialog The dialog where the button should be added.
 *
 *  @param label The text of the button.
 *
 *  @param stock_id The name of the stock button to use.
 *
 *  @param response The response id to return if this button is
 *  clicked.*/
void gnc_gtk_dialog_add_button (GtkWidget *dialog,
                                const gchar *label,
                                const gchar *stock_id,
                                guint response);


/** Note: This dialog is modal!  (It calls gtk_dialog_run() which is modal.)
 */
gint
gnc_dialog_run(GtkDialog *dialog, const gchar *gconf_key);

#endif

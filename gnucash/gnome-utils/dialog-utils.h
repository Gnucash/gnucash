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

#include <gtk/gtk.h>
#include "qof.h"

#define GNC_PREF_GRID_LINES_HORIZONTAL "grid-lines-horizontal"
#define GNC_PREF_GRID_LINES_VERTICAL   "grid-lines-vertical"

void gnc_set_label_color (GtkWidget *label, gnc_numeric value);

/********************************************************************\
 * Returns the window size to use for the given option prefix,      *
 * if window sizes are being saved, otherwise returns 0 for both.   *
 *                                                                  *
 * Args: prefix - the option name prefix                            *
 *       window - the window being restored                         *
 *       parent - the parent window for first use alignment         *
 * Returns: nothing                                                 *
 \*******************************************************************/
void gnc_restore_window_size (const char *prefix, GtkWindow *window,
                              GtkWindow *parent);

/********************************************************************\
 * Save the window size into options whose names are determined     *
 * by the string prefix.                                            *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       window - the window being saved                            *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_save_window_size (const char *section, GtkWindow *window);

/********************************************************************\
 * Adjust the window size if it is bigger than the screen size.     *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_window_adjust_for_screen (GtkWindow * window);

/********************************************************************\
 * Sets the alignament of a Label Widget, GTK3 version specific.    *
 *                                                                  *
 * Args: widget - the label widget to set alignment on              *
 *       xalign - x alignment                                       *
 *       yalign - y alignment                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_label_set_alignment (GtkWidget *widget, gfloat xalign, gfloat yalign);

/********************************************************************\
 * Get the preference for showing tree view grid lines              *
 *                                                                  *
 * Args: none                                                       *
 * Returns:  GtkTreeViewGridLines setting                           *
\********************************************************************/
GtkTreeViewGridLines gnc_tree_view_get_grid_lines_pref (void);

/********************************************************************\
 * Add a style context to a Widget so it can be altered with css    *
 *                                                                  *
 * Args:    widget - widget to add css style too                    *
 *       gnc_class - character string for css class name            *
 * Returns:  nothing                                                *
\********************************************************************/
void gnc_widget_set_style_context (GtkWidget *widget, const char *gnc_class);
void gnc_widget_style_context_add_class (GtkWidget *widget, const char *gnc_class);

/********************************************************************\
 * Remove a style context class from a Widget                       *
 *                                                                  *
 * Args:    widget - widget to remove style class from              *
 *       gnc_class - character string for css class name            *
 * Returns:  nothing                                                *
\********************************************************************/
void gnc_widget_style_context_remove_class (GtkWidget *widget, const char *gnc_class);

/********************************************************************\
 * Draw an arrow on a Widget so it can be altered with css          *
 *                                                                  *
 * Args:     widget - widget to add arrow to in the draw callback   *
 *               cr - cairo context for the draw callback           *
 *        direction - 0 for up, 1 for down                          *
 * Returns:  TRUE, stop other handlers being invoked for the event  *
\********************************************************************/
gboolean gnc_draw_arrow_cb (GtkWidget *widget, cairo_t *cr, gpointer direction);

gboolean gnc_gdate_in_valid_range (GDate *test_date, gboolean warn);

gboolean gnc_handle_date_accelerator (GdkEventKey *event,
                                      struct tm *tm,
                                      const char *date_str);

gboolean gnc_builder_add_from_file (GtkBuilder *builder, const char *filename, const char *root);

void gnc_builder_connect_full_func (GtkBuilder *builder,
                                    GObject *signal_object,
                                    const gchar *signal_name,
                                    const gchar *handler_name,
                                    GObject *connect_object,
                                    GConnectFlags flags,
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
 *  @param icon_name The name of the icon button to use.
 *
 *  @param response The response id to return if this button is
 *  clicked.*/
void gnc_gtk_dialog_add_button (GtkWidget *dialog,
                                const gchar *label,
                                const gchar *icon_name,
                                guint response);

/** Note: This dialog is modal!  (It calls gtk_dialog_run() which is modal.)
 */
gint
gnc_dialog_run(GtkDialog *dialog, const gchar *pref_key);

/* If this is a new book, this function can be used to display book options
 * dialog so user can specify options, before any transactions can be
 * imported/entered, since the book options can affect how transactions are
 * created. Note: This dialog is modal! */
gboolean gnc_new_book_option_display (GtkWidget *parent);

/** This function returns a widget for selecting a cost policy
  */
GtkWidget *
gnc_cost_policy_select_new (void);

gchar* get_negative_color (void);

#endif /* DIALOG_UTILS_H */

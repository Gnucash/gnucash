/********************************************************************\
 * dialog-utils.h -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
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

#ifndef DIALOG_UTILS_H
#define DIALOG_UTILS_H

#include <glade/glade.h>
#include <gnome.h>

#include "Account.h"


/* option button callback function */
typedef void (*GNCOptionCallback) (GtkWidget *, gint index,
                                   gpointer user_data);

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


/********************************************************************\
 * Returns a GnomePixmap widget given a pixmap filename             *
 *                                                                  *
 * Args: Filename of pixmap file                                    *
 * Returns: GnomePixmap widget or NULL if there was a problem       *
 \*******************************************************************/
GtkWidget * gnc_get_pixmap (const char *name);

/********************************************************************\
 * Returns a GdkImlibImage object given a pixmap filename           *
 *                                                                  *
 * Args: Filename of pixmap file                                    *
 * Returns: GdkImlibImage or NULL if there was a problem            *
 \*******************************************************************/
GdkImlibImage * gnc_get_gdk_imlib_image (const char *name);


GtkToolbarStyle gnc_get_toolbar_style (void);
GnomeMDIMode    gnc_get_mdi_mode(void);

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
void gnc_get_window_size (const char *prefix, int *width, int *height);

/********************************************************************\
 * Save the window size into options whose names are determined     *
 * by the string prefix.                                            *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       width  - width of the window to save                       *
 *       height - height of the window to save                      *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_save_window_size (const char *prefix, int width, int height);


/********************************************************************\
 * Fill the user data values in the menu structure with the given   *
 * value. The filling is done recursively.                          *
 *                                                                  *
 * Args: info - the menu to fill                                    *
 *       data - the value to fill with                              *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_fill_menu_with_data (GnomeUIInfo *info, gpointer data);

void gnc_option_menu_init (GtkWidget * option_menu);
void gnc_option_menu_init_w_signal(GtkWidget * w,
				   GtkSignalFunc f,
				   gpointer cb_data);
int  gnc_option_menu_get_active (GtkWidget * option_menu);

/********************************************************************\
 * Adjust the window size if it is bigger than the screen size.     *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void gnc_window_adjust_for_screen (GtkWindow * window);

void gtk_window_present (GtkWindow * window); /* Remove me for GTK 2.0 */


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
void gnc_clist_set_check (GtkCList *list, int row, int col, 
			  gboolean checked);

/* This function is similar to gtk_clist_columns_autosize, but
 * also takes into account the column titles. */
void gnc_clist_columns_autosize (GtkCList *list);

GladeXML * gnc_glade_xml_new (const char *filename, const char *root);
GtkWidget * gnc_glade_lookup_widget (GtkWidget *widget, const char *name);
void gnc_glade_autoconnect_full_func(const gchar *handler_name,
				     GtkObject *signal_object,
				     const gchar *signal_name,
				     const gchar *signal_data,
				     GtkObject *connect_object,
				     gboolean after,
				     gpointer user_data);

/* Multibyte/wide char string helper functions. */

/** Allocate new wide char string in dest_p. Return number of
 * wide chars or < 0 if error.  When the string is no longer
 * needed, free it with g_free().
 */
gint         gnc_mbstowcs (GdkWChar **dest_p, const char *src);

/** Return new multibyte string or NULL if failure. 
 * XXX how are we supposed to free this?? 
 * with g_free or something else ??
 */
char *       gnc_wcstombs (const GdkWChar *src);

/** Len of wide char string in chars */
gint         gnc_wcslen   (const GdkWChar *src);

/** Duplicate wide char string. 
 *  When the string is no longer needed, free it with g_free().
 */
GdkWChar *   gnc_wcsdup   (const GdkWChar *src);

#endif

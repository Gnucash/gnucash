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

#ifndef __DIALOG_UTILS_H__
#define __DIALOG_UTILS_H__

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
GtkWidget * gnc_ui_source_menu_create (Account *account);

guint       gnc_find_timezone_menu_position(const gchar *timezone);
gchar *     gnc_timezone_menu_position_to_string(guint pos);
GtkWidget * gnc_ui_quote_tz_menu_create (Account *account);

GtkWidget * gnc_build_option_menu (GNCOptionInfo *option_info,
				   gint num_options);

GtkWidget * gnc_get_pixmap (const char *name);
GdkImlibImage * gnc_get_gdk_imlib_image (const char *name);

GtkToolbarStyle gnc_get_toolbar_style (void);
GnomeMDIMode    gnc_get_mdi_mode(void);

void gnc_get_deficit_color (GdkColor *color);
void gnc_set_label_color (GtkWidget *label, gnc_numeric value);

void gnc_get_window_size (const char *prefix, int *width, int *height);
void gnc_save_window_size (const char *prefix, int width, int height);

void gnc_fill_menu_with_data (GnomeUIInfo *info, gpointer data);

void gnc_option_menu_init (GtkWidget * option_menu);
int  gnc_option_menu_get_active (GtkWidget * option_menu);

void gnc_window_adjust_for_screen (GtkWindow * window);


/* This function sets or clears a check mark in a GtkCList row.
 * There are some restrictions on using this function. If you mix
 * this function with gtk_clist_{insert, prepend, remove} before
 * the clist is realized, then the checks may appear in the wrong
 * place. Stick to gtk_clist_append, or use gnc_clist_set_check
 * after you have built the list. This only applies to unrealized
 * widgets. */
void gnc_clist_set_check (GtkCList *list, int row, int col, gboolean checked);

/* This function is similar to gtk_clist_columns_autosize, but
 * also takes into account the column titles. */
void gnc_clist_columns_autosize (GtkCList *list);


#endif

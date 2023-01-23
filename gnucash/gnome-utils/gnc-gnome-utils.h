/********************************************************************\
 * gnc-gnome-utils.h -- utility functions for gnome for GnuCash     *
 * Copyright (C) 2001 Linux Developers Group                        *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiGnome Gnome-specific GUI handling.
    @{ */
/** @file gnc-gnome-utils.h
    @brief Gnome specific utility functions.
    @author Copyright (C) 2001 Linux Developers Group
    @author Copyright (C) 2003 David Hampton <hampton@employees.org>
*/

#ifndef GNC_GNOME_UTILS_H
#define GNC_GNOME_UTILS_H

#include <gnc-main-window.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize the gnome-utils library
 *  Should be run once before using any gnome-utils features.
 */
void gnc_gnome_utils_init (void);

/** Load a gtk resource configuration file to customize gtk
 *  appearance and behaviour.
 */
void gnc_add_css_file (void);

/** Launch the systems default help browser, gnome's yelp for linux,
 *  and open to a given link within a given file.
 *  This routine will display an error message
 *  if it can't find the help file or can't open the help browser.
 *
 *  @param parent The parent window for any dialogs.
 *
 *  @param file_name The name of the help file.
 *
 *  @param anchor The anchor the help browser should scroll to.
 */
void gnc_gnome_help (GtkWindow *parent, const char *file_name,
                     const char *anchor);
/** Launch the default browser and open the provided URI.
 */
void gnc_launch_doclink (GtkWindow *parent, const char *uri);

/** Given a file name, find and load the requested pixmap.  This
 *  routine will display an error message if it can't find the file or
 *  load the pixmap.
 *
 *  @param name The name of the pixmap file to load.
 *
 *  @return A pointer to the pixmap, or NULL of the file couldn't
 *  be found or loaded..
 */
GtkWidget * gnc_gnome_get_pixmap (const char *name);


/** Given a file name, find and load the requested pixbuf.  This
 *  routine will display an error message if it can't find the file or
 *  load the pixbuf.
 *
 *  @param name The name of the pixbuf file to load.
 *
 *  @return A pointer to the pixbuf, or NULL of the file couldn't
 *  be found or loaded..
 */
GdkPixbuf * gnc_gnome_get_gdkpixbuf (const char *name);


/** Shutdown gnucash.  This function will initiate an orderly
 *  shutdown, and when that has finished it will exit the program.
 *
 *  @param exit_status The exit status for the program.
 */
void gnc_shutdown (int exit_status);


/** Initialize the gnucash gui */
GncMainWindow *gnc_gui_init (void);
int gnc_ui_start_event_loop (void);
gboolean gnucash_ui_is_running (void);

#ifdef __cplusplus
}
#endif

#endif
/** @} */
/** @} */

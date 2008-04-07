/********************************************************************\
 * gnc-gtk-utils.c -- utility functions based on glib functions     *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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

/** @addtogroup Gtk Gtk Utilities

    The API in this file is designed to provide support functions that
    wrap the base gtk functions and make them easier to use.

    @{ */
/** @file gnc-gtk-utils.h
 *  @brief gtk helper routines.
 *  @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 */

#ifndef GNC_GTK_UTILS_H
#define GNC_GTK_UTILS_H

#include <gtk/gtk.h>

/** @name gtk Miscellaneous Functions
 @{ 
*/

void gnc_cbe_set_by_string(GtkComboBoxEntry *cbe, const gchar *text);
void gnc_cbe_add_completion (GtkComboBoxEntry *cbe);
void gnc_cbe_require_list_item (GtkComboBoxEntry *cbe);

/** @} */

#endif /* GNC_GTK_UTILS_H */
/** @} */

/********************************************************************\
 * gnc-glib-utils.c -- utility functions based on glib functions    *
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

/** @addtogroup GLib
    @{ */
/** @addtogroup GConf GLib Utilities

    The API in this file is designed to provide support functions that
    wrap the base glib functions and make them easier to use.

    @{ */
/** @file gnc-glib-utils.h
 *  @brief glib helper routines.
 *  @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 */

#ifndef GNC_GLIB_UTILS_H
#define GNC_GLIB_UTILS_H

#include <glib.h>

/** @name glib Miscellaneous Functions
 @{ 
*/

/** Collate two utf8 strings.  This function performs basic argument
 *  checking before calling g_utf8_collate.
 *
 *  @param str1 The first string.
 *
 *  @param str2 The first string.
 *
 *  @return Same return value as g_utf8_collate. The values are: < 0
 *  if str1 compares before str2, 0 if they compare equal, > 0 if str1
 *  compares after str2. */
int safe_utf8_collate (const char *str1, const char *str2);


/** This is a helper function for guile. C code should call
 *  g_utf8_validate directly.
 *
 *  @param str The string to be validated.
 *
 *  @return TRUE if this string is valid utf8. */
gboolean gnc_utf8_validate (const gchar *str);


/** Strip any non-utf8 characters from a string.  This function
 *  rewrites the string "in place" instead of allocating and returning
 *  a new string.  This allows it to operat on strings that are
 *  defined as character arrays in a larger data structure.
 *
 *  @param str A pointer to the string to strip of invalid
 *  characters. */
void gnc_utf8_strip_invalid (gchar *str);


/** @} */

#endif /* GNC_GLIB_UTILS_H */
/** @} */
/** @} */

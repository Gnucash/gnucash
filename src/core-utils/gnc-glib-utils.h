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


/** Strip any non-utf8 characters from a string.  This function
 *  rewrites the string "in place" instead of allocating and returning
 *  a new string.  This allows it to operate on strings that are
 *  defined as character arrays in a larger data structure.  Note that
 *  it also removes some subset of invalid XML characters, too.
 *  See http://www.w3.org/TR/REC-xml/#NT-Char linked from bug #346535
 *
 *  @param str A pointer to the string to strip of invalid
 *  characters. */
void gnc_utf8_strip_invalid (gchar *str);

/** Returns a newly allocated copy of the given string but with any
 * non-utf8 character stripped from it.
 *
 * Note that it also removes some subset of invalid XML characters,
 * too.  See http://www.w3.org/TR/REC-xml/#NT-Char linked from bug
 * #346535
 *
 * @param str A pointer to the string to be copied and stripped of
 * non-utf8 characters.
 *
 * @return A newly allocated string that has to be g_free'd by the
 * caller. */
gchar *gnc_utf8_strip_invalid_strdup (const gchar* str);

/** Converts a string from UTF-8 to the encoding used for strings
 * in the current locale.
 *
 * This essentially is a wrapper for g_locale_from_utf8 that can
 * be swigified for use with Scheme to avoid adding a dependency
 * for guile-glib.
 *
 * @param str A pointer to a UTF-8 encoded string to be converted.
 *
 * @return A newly allocated string that has to be g_free'd by the
 * caller. */
gchar *gnc_locale_from_utf8(const gchar* str);

typedef gpointer (*GncGMapFunc)(gpointer data, gpointer user_data);

/**
 * @return Caller-owned GList* of results of apply @a fn to @a list in order.
 **/
GList* gnc_g_list_map(GList* list, GncGMapFunc fn, gpointer user_data);

/**
 * Cut a GList into two parts; the @a cut_point is the beginning of the
 * new list; @a list may need to be modified, but will be the list
 * before the @a cut_point.
 **/
void gnc_g_list_cut(GList **list, GList *cut_point);

void gnc_scm_log_warn(const gchar *msg);
void gnc_scm_log_error(const gchar *msg);
void gnc_scm_log_msg(const gchar *msg);
void gnc_scm_log_debug(const gchar *msg);

/** Kill a process.  On UNIX send a SIGKILL, on Windows call TerminateProcess.
 *
 *  @param pid The process ID. */
void gnc_gpid_kill(GPid pid);

/** @} */

#endif /* GNC_GLIB_UTILS_H */
/** @} */
/** @} */

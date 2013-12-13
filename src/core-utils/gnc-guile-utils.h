/********************************************************************\
 * gnc-guile-utils.h -- basic guile extensions                      *
 * Copyright (C) 2012 Geert Janssens                                *
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

#ifndef GNC_GUILE_UTILS_H
#define GNC_GUILE_UTILS_H

#include <glib.h>
#include <libguile.h>

/** Helper function to get the string representation of
 *  a guile string.
 *
 *  Returns a newly allocated string that must be freed with g_free*/
gchar * gnc_scm_to_utf8_string(SCM scm_string);

/** Helper function to get the string representation of
 *  a guile string.
 *   The string will be encoded in the current locale's encoding.
 *   Note: this function should only be use to convert filenames or
 *   strings from the environment. Or other strings that are in the
 *   system locale.
 *
 *  Returns a newly allocated string that must be freed with g_free*/
gchar * gnc_scm_to_locale_string(SCM scm_string);

/** Helper function to get the string representation of
 *  a guile symbol.
 *
 *  Returns a newly allocated string that must be freed with g_free*/
gchar * gnc_scm_symbol_to_locale_string(SCM scm_string);

/* Helpful functions for calling functions that return
 * specific kinds of values. These functions do error
 * checking to verify the result is of the correct type. */
char * gnc_scm_call_1_to_string(SCM func, SCM arg);
char * gnc_scm_call_1_symbol_to_string(SCM func, SCM arg);
SCM    gnc_scm_call_1_to_procedure(SCM func, SCM arg);
SCM    gnc_scm_call_1_to_list(SCM func, SCM arg);
SCM    gnc_scm_call_1_to_vector(SCM func, SCM arg);

/* Deprectated functions, will be removed soon */
#define gnc_guile_call1_to_string gnc_scm_call_1_to_string
#define gnc_guile_call1_symbol_to_string gnc_scm_call_1_symbol_to_string
#define gnc_guile_call1_to_procedure gnc_scm_call_1_to_procedure
#define gnc_guile_call1_to_list gnc_scm_call_1_to_list
#define gnc_guile_call1_to_vector gnc_scm_call_1_to_vector

/** Clean up a scheme options string for use in a key/value file.
 *  This function removes all full line comments, removes all blank
 *  lines, and removes all leading/trailing white space.
 *
 *  @note: This function does not correctly handle comments that occur
 *  at the end of a line. Fortunately there aren't any such
 *  comments. */
gchar *gnc_scm_strip_comments (SCM scm_text);

#endif

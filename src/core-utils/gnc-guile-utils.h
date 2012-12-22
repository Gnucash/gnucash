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
 *  a guile symbol. */
gchar * gnc_scm_symbol_to_locale_string(SCM scm_string);

/* Helpful functions for calling functions that return
 * specific kinds of values. These functions do error
 * checking to verify the result is of the correct type. */
char * gnc_guile_call1_to_string(SCM func, SCM arg);
char * gnc_guile_call1_symbol_to_string(SCM func, SCM arg);
SCM    gnc_guile_call1_to_procedure(SCM func, SCM arg);
SCM    gnc_guile_call1_to_list(SCM func, SCM arg);
SCM    gnc_guile_call1_to_vector(SCM func, SCM arg);

#endif

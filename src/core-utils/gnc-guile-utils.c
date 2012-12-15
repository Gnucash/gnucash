/********************************************************************\
 * gnc-guile-utils.c -- basic guile extensions                      *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include "swig-runtime.h"
#include <libguile.h>

#include "qof.h"
#include "gnc-guile-utils.h"
#include "guile-mappings.h"

/* This static indicates the debugging module this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;


/********************************************************************\
 * gnc_guile_symbol_to_locale_string                                *
 *   returns the string representation of the scm symbol in         *
 *   a newly allocated gchar * or NULL if it can't be retrieved.    *
 *                                                                  *
 * Args: symbol_value - the scm symbol                              *
 * Returns: newly allocated gchar * or NULL, should be freed with   *
 *          g_free by the caller                                    *
\********************************************************************/
gchar *
gnc_scm_symbol_to_locale_string(SCM symbol_value)
{

    if (scm_is_symbol(symbol_value))
    {
        SCM string_value = scm_symbol_to_string (symbol_value);
        if (scm_is_string (string_value))
        {
            char  *tmp = scm_to_locale_string (string_value);
            gchar *str = g_strdup (tmp);
            free (tmp);
            return str;
        }
    }

    /* Unable to extract string from the symbol...*/
    PERR("bad value\n");
    return NULL;
}

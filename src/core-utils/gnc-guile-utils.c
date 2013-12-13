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
 * gnc_scm_to_utf8_string                                           *
 *   returns the string representation of the scm string in         *
 *   a newly allocated gchar * or NULL if it can't be retrieved.    *
 *                                                                  *
 * Args: symbol_value - the scm symbol                              *
 * Returns: newly allocated gchar * or NULL, should be freed with   *
 *          g_free by the caller                                    *
\********************************************************************/
gchar *gnc_scm_to_utf8_string(SCM scm_string)
{
    if (scm_is_string (scm_string))
    {
        gchar* s;
        char * str;

        str = scm_to_utf8_string(scm_string);
        s = g_strdup(str);
        free (str);
        return s;
    }

    /* Unable to extract string from the symbol...*/
    PERR("bad value\n");
    return NULL;
}


/********************************************************************\
 * gnc_scm_to_locale_string                                         *
 *   returns the string representation of the scm string in         *
 *   a newly allocated gchar * or NULL if it can't be retrieved.    *
 *   The string will be encoded in the current locale's encoding.   *
 *   Note: this function should only be use to convert filenames or *
 *   strings from the environment. Or other strings that are in the *
 *   system locale.                                                 *
 *                                                                  *
 * Args: symbol_value - the scm symbol                              *
 * Returns: newly allocated gchar * or NULL, should be freed with   *
 *          g_free by the caller                                    *
\********************************************************************/
gchar *gnc_scm_to_locale_string(SCM scm_string)
{
    if (scm_is_string (scm_string))
    {
        gchar* s;
        char * str;

        str = scm_to_locale_string(scm_string);
        s = g_strdup(str);
        free (str);
        return s;
    }

    /* Unable to extract string from the symbol...*/
    PERR("bad value\n");
    return NULL;
}


/********************************************************************\
 * gnc_scm_symbol_to_locale_string                                  *
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
            char  *tmp = scm_to_utf8_string (string_value);
            gchar *str = g_strdup (tmp);
            free (tmp);
            return str;
        }
    }

    /* Unable to extract string from the symbol...*/
    PERR("bad value\n");
    return NULL;
}


/********************************************************************\
 * gnc_scm_call_1_to_string                                         *
 *   returns the malloc'ed string returned by the guile function    *
 *   or NULL if it can't be retrieved                               *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: g_malloc'ed char * or NULL must be freed with g_free    *
\********************************************************************/
char *
gnc_scm_call_1_to_string(SCM func, SCM arg)
{
    SCM value;

    if (scm_is_procedure(func))
    {
        value = scm_call_1(func, arg);

        if (scm_is_string(value))
        {
            return gnc_scm_to_utf8_string(value);
        }
        else
        {
            PERR("bad value\n");
        }
    }
    else
    {
        PERR("not a procedure\n");
    }

    return NULL;
}


/********************************************************************\
 * gnc_scm_call_1_symbol_to_string                                  *
 *   returns the malloc'ed string returned by the guile function    *
 *   or NULL if it can't be retrieved. The return value of the      *
 *   function should be a symbol.                                   *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: malloc'ed char * or NULL                                *
\********************************************************************/
char *
gnc_scm_call_1_symbol_to_string(SCM func, SCM arg)
{
    SCM symbol_value;

    if (scm_is_procedure(func))
    {
        symbol_value = scm_call_1(func, arg);
        return gnc_scm_symbol_to_locale_string (symbol_value);
    }
    else
    {
        PERR("not a procedure\n");
    }

    return NULL;
}


/********************************************************************\
 * gnc_scm_call_1_to_procedure                                      *
 *   returns the SCM handle to the procedure returned by the guile  *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM function handle or SCM_UNDEFINED                    *
\********************************************************************/
SCM
gnc_scm_call_1_to_procedure(SCM func, SCM arg)
{
    SCM value;

    if (scm_is_procedure(func))
    {
        value = scm_call_1(func, arg);

        if (scm_is_procedure(value))
            return value;
        else
        {
            PERR("bad value\n");
        }
    }
    else
    {
        PERR("not a procedure\n");
    }

    return SCM_UNDEFINED;
}


/********************************************************************\
 * gnc_scm_call_1_to_list                                           *
 *   returns the SCM handle to the list returned by the guile       *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM list handle or SCM_UNDEFINED                        *
\********************************************************************/
SCM
gnc_scm_call_1_to_list(SCM func, SCM arg)
{
    SCM value;

    if (scm_is_procedure(func))
    {
        value = scm_call_1(func, arg);

        if (scm_is_list(value))
            return value;
        else
        {
            PERR("bad value\n");
        }
    }
    else
    {
        PERR("not a procedure\n");
    }

    return SCM_UNDEFINED;
}


/********************************************************************\
 * gnc_scm_call_1_to_vector                                         *
 *   returns the SCM handle to the vector returned by the guile     *
 *   function, or SCM_UNDEFINED if it couldn't be retrieved.        *
 *                                                                  *
 * Args: func - the guile function to call                          *
 *       arg  - the single function argument                        *
 * Returns: SCM vector handle or SCM_UNDEFINED                      *
\********************************************************************/
SCM
gnc_scm_call_1_to_vector(SCM func, SCM arg)
{
    SCM value;

    if (scm_is_procedure(func))
    {
        value = scm_call_1(func, arg);

        if (scm_is_vector(value))
            return value;
        else
        {
            PERR("bad value\n");
        }
    }
    else
    {
        PERR("not a procedure\n");
    }

    return SCM_UNDEFINED;
}


/*  Clean up a scheme options string for use in a key/value file.
 *  This function removes all full line comments, removes all blank
 *  lines, and removes all leading/trailing white space. */
gchar *gnc_scm_strip_comments (SCM scm_text)
{
    gchar *raw_text, *text, **splits;
    gint i, j;

    raw_text = gnc_scm_to_utf8_string (scm_text);
    splits = g_strsplit(raw_text, "\n", -1);
    for (i = j = 0; splits[i]; i++)
    {
        gchar *haystack, *needle;
        if ((splits[i][0] == ';') || (splits[i][0] == '\0'))
        {
            g_free(splits[i]);
            continue;
        }

        /* Work around a bug in guile 1.8 that escapes spaces
         * in a symbol printed on a string port. We don't
         * want this, because this string can't be properly
         * converted back into a symbol later on. */

        haystack = splits [i];
        needle = g_strstr_len (haystack, -1, "\\ ");
        while (needle)
        {
            gchar *new_haystack = NULL;
            gsize prefix_size = needle - haystack;
            gchar *prefix = g_strndup (haystack, prefix_size);
            needle++;
            new_haystack = g_strconcat (prefix, needle, NULL);
            g_free (prefix);
            g_free (haystack);
            haystack = new_haystack;
            needle = g_strstr_len (haystack, -1, "\\ ");
        }
        splits[j++] = haystack;
    }
    splits[j] = NULL;

    text = g_strjoinv(" ", splits);
    g_free (raw_text);
    g_strfreev(splits);
    return text;
}

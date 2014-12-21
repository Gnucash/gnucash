/*
 * gnc-environment.c:
 *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <glib.h>
#include <string.h>
#include "gnc-environment.h"
#include "gnc-path.h"

static gchar  *environment_expand(gchar *param)
{
    gchar *search_start;
    gchar *opening_brace;
    gchar *closing_brace;
    gchar *result;
    gchar *tmp;
    gchar *expanded = NULL;

    if (!param)
        return NULL;

    /* Set an initial return value, so we can always use g_strconcat below) */
    result = g_strdup ("x");

    /* Look for matching pairs of { and }. Anything in between should be expanded */
    search_start = param;
    opening_brace = g_strstr_len (search_start, -1, "{");
    closing_brace = g_strstr_len (search_start, -1, "}");

    /* Note: the test on valid braces is fairly simple:
     *       * if no pair of opening/closing braces is found, no expansion occurs
     *       * braces can't be nested, this will give unexpected results
     *       * the string should contain no other braces than those used to mark
     *         expandable variables, or unexpected results will be returned.
     */
    while ( opening_brace && closing_brace && (closing_brace > opening_brace) )
    {
        /* Found a first matching pair */
        gchar *to_expand;
        const gchar *env_val;

        /* If the string had characters before the opening {, copy them first */
        if (opening_brace > search_start)
        {
            gchar *prefix = g_strndup (search_start, opening_brace - search_start);

            tmp = g_strconcat (result, prefix, NULL);
            g_free (result);
            result = tmp;
            g_free (prefix);
        }

        /* Expand the variable  we found and append it to the result */
        to_expand = g_strndup (opening_brace + 1, closing_brace - opening_brace - 1);
        env_val = g_getenv (to_expand);
        tmp = g_strconcat (result, env_val, NULL);
        g_free (result);
        result = tmp;
        g_free (to_expand);

        /* Look for matching pairs of { and }. Anything in between should be expanded */
        search_start = closing_brace + 1;
        opening_brace = g_strstr_len (search_start, -1, "{");
        closing_brace = g_strstr_len (search_start, -1, "}");
    }

    /* No more braces found, append the remaining characters */
    tmp = g_strconcat (result, search_start, NULL);
    g_free (result);
    result = tmp;

    /* Remove the "x" from our result */
    if (g_strcmp0 (result, "x"))
        expanded = g_strdup (result + 1);
    g_free (result);

    return expanded;
}

void
gnc_environment_setup (void)
{
    gchar *config_path;
    gchar *env_file;
    GKeyFile    *keyfile = g_key_file_new();
    GError      *error;
    gchar **env_vars;
    gsize param_count;
    gint i;
    gboolean got_keyfile;
    gchar *env_parm;

    /* Export default parameters to the environment */
    env_parm = gnc_path_get_prefix();
    if (!g_setenv("GNC_HOME", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable GNC_HOME.");
    g_free (env_parm);
    env_parm = gnc_path_get_bindir();
    if (!g_setenv("GNC_BIN", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable GNC_BIN.");
    g_free (env_parm);
    env_parm = gnc_path_get_pkglibdir();
    if (!g_setenv("GNC_LIB", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable GNC_LIB.");
    g_free (env_parm);
    env_parm = gnc_path_get_pkgdatadir();
    if (!g_setenv("GNC_DATA", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable GNC_DATA.");
    g_free (env_parm);
    env_parm = gnc_path_get_pkgsysconfdir();
    if (!g_setenv("GNC_CONF", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable GNC_CONF.");
    g_free (env_parm);
    env_parm = gnc_path_get_libdir();
    if (!g_setenv("SYS_LIB", env_parm, FALSE))
        g_warning ("Couldn't set/override environment variable SYS_LIB.");
    g_free (env_parm);

    config_path = gnc_path_get_pkgsysconfdir();
#ifdef G_OS_WIN32
    {
        /* unhide files without extension */
        gchar *pathext = g_build_path(";", ".", g_getenv("PATHEXT"),
                                      (gchar*) NULL);
        g_setenv("PATHEXT", pathext, TRUE);
        g_free(pathext);
    }
#endif

    env_file = g_build_filename (config_path, "environment", NULL);
    got_keyfile = g_key_file_load_from_file (keyfile, env_file, G_KEY_FILE_NONE, &error);
    g_free (config_path);
    g_free (env_file);
    if ( !got_keyfile )
    {
        g_key_file_free(keyfile);
        return;
    }

    /* Read the environment overrides and apply them */
    env_vars = g_key_file_get_keys(keyfile, "Variables", &param_count, &error);
    for ( i = 0; i < param_count; i++ )
    {
        gchar **val_list;
        gsize val_count;
        gint j;
        gchar *new_val = NULL, *tmp_val;

        /* For each variable, read its new value, optionally expand it and set/unset it */
        val_list = g_key_file_get_string_list (keyfile, "Variables",
                                               env_vars[i], &val_count,
                                               &error );
        if ( val_count == 0 )
            g_unsetenv (env_vars[i]);
        else
        {
            /* Set an initial return value, so we can always use g_build_path below) */
            tmp_val = g_strdup ("x");
            for ( j = 0; j < val_count; j++ )
            {
                gchar *expanded = environment_expand (val_list[j]);
                if (expanded && strlen(expanded))
                {
                    new_val = g_build_path (G_SEARCHPATH_SEPARATOR_S, tmp_val, expanded, NULL);
                    g_free (tmp_val);
                    g_free(expanded);
                    tmp_val = new_val;
                }
            }
            g_strfreev (val_list);

            /* Remove the "x" from our result */
            if (g_strcmp0 (tmp_val, "x"))
                new_val = g_strdup (tmp_val + sizeof (G_SEARCHPATH_SEPARATOR_S));
            g_free (tmp_val);

            if (!g_setenv (env_vars[i], new_val, TRUE))
                g_warning ("Couldn't properly override environment variable \"%s\". "
                           "This may lead to unexpected results", env_vars[i]);
            g_free(new_val);
        }
    }

    g_strfreev(env_vars);
    g_key_file_free(keyfile);
}

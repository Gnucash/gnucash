/*
 * gnc-help-utils.c
 *
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#include "config.h"
#include <glib.h>

#ifdef HAVE_HTMLHELPW
#    include <windows.h>
#    include <htmlhelp.h>
#endif

#include "gnc-help-utils.h"

#ifdef HAVE_HTMLHELPW

static GHashTable *
parse_hhmap_file(const gchar *chmfile)
{
    gchar *mapfile = NULL, *dot;
    GKeyFile *keyfile = NULL;
    GError *error = NULL;
    gchar **keys = NULL, **key;
    gint value;
    GHashTable *ctxtmap = NULL;

    g_return_val_if_fail(chmfile, NULL);

    mapfile = g_new(gchar, strlen(chmfile) + 7);
    strcpy(mapfile, chmfile);
    dot = strrchr(mapfile, '.');
    if (dot)
        strcpy(dot, ".hhmap");
    else
        strcat(mapfile, ".hhmap");

    keyfile = g_key_file_new();
    if (!g_key_file_load_from_file(keyfile, mapfile, G_KEY_FILE_NONE, &error))
        goto cleanup_parse;

    if (NULL == (keys = g_key_file_get_keys(keyfile, "Map", NULL, &error)))
        goto cleanup_parse;

    ctxtmap = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
    for (key = keys; *key; key++)
    {
        value = g_key_file_get_integer(keyfile, "Map", *key, &error);
        if (error)
            goto cleanup_parse;
        else
            g_hash_table_insert(ctxtmap, g_strdup(*key), GINT_TO_POINTER(value));
    }

cleanup_parse:
    if (error)
    {
        g_warning("Could not load help map file: %s", error->message);
        g_error_free(error);
        if (ctxtmap)
            g_hash_table_destroy(ctxtmap);
        ctxtmap = NULL;
    }
    if (keys)
        g_strfreev(keys);
    if (keyfile)
        g_key_file_free (keyfile);
    if (mapfile)
        g_free(mapfile);

    return ctxtmap;
}

void
gnc_show_htmlhelp(const gchar *chmfile, const gchar *anchor)
{
    static GHashTable *chmfile_ctxtmap_map;
    G_LOCK_DEFINE_STATIC(chmfile_ctxtmap_map);
    GHashTable *ctxtmap;
    gboolean create_map = FALSE;
    wchar_t *wpath;
    gint id = 0;

    g_return_if_fail(chmfile);

    if (anchor)
    {
        G_LOCK(chmfile_ctxtmap_map);
        if (!chmfile_ctxtmap_map)
        {
            chmfile_ctxtmap_map = g_hash_table_new(g_str_hash, g_str_equal);
            create_map = TRUE;
        }
        else
        {
            create_map = !g_hash_table_lookup_extended(
                             chmfile_ctxtmap_map, chmfile, NULL, (gpointer) & ctxtmap);
        }

        if (create_map)
        {
            ctxtmap = parse_hhmap_file(chmfile);
            g_hash_table_insert(chmfile_ctxtmap_map, g_strdup(chmfile), ctxtmap);
        }

        if (ctxtmap)
        {
            gpointer ptr = g_hash_table_lookup(ctxtmap, anchor);
            if (ptr)
                id = GPOINTER_TO_INT(ptr);
        }
        G_UNLOCK(chmfile_ctxtmap_map);

        if (!id)
            g_warning("Could not find anchor '%s'", anchor);
    }

    wpath = g_utf8_to_utf16(chmfile, -1, NULL, NULL, NULL);
    HtmlHelpW(GetDesktopWindow(), wpath, HH_HELP_CONTEXT, id);
    g_free(wpath);
}

#else /* !HAVE_HTMLHELPW */
void
gnc_show_htmlhelp(const gchar *chmfile, const gchar *anchor)
{
    gchar *argv[3];

    g_return_if_fail(chmfile);

    argv[0] = "hh";
    argv[1] = g_strdup(chmfile);
    argv[2] = NULL;

    if (!g_spawn_async(NULL, argv, NULL, G_SPAWN_SEARCH_PATH,
                       NULL, NULL, NULL, NULL))
        if (g_file_test(chmfile, G_FILE_TEST_IS_REGULAR))
            g_warning("Found CHM help file, but could not spawn hh to open it");

    g_free(argv[1]);
}
#endif /* HAVE_HTMLHELPW */

/*
 * redirect.c --
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>
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

#include "config.h"
#include <glib.h>
#include <windows.h>

/* This program will search for an existing file in
 * .\bin\$i
 * ..\bin\$i
 * ..\..\bin\$i
 * and so on, where $i is the base name of argv[0].  Once found, that
 * program is spawned synchronously with the same parameters and its
 * exit status will be the one of this program as well.
 * To avoid recursion, do not use it in a directory ending on "bin".
 */

/* __wgetmainargs() is an undocumented function to get the program's
 * argv in wide char format.
 */
typedef struct {
    int newmode;
} _startupinfo;

extern void __wgetmainargs(int *argc,
                           wchar_t ***wargv,
                           wchar_t ***wenviron,
                           int expand_wildcards,
                           _startupinfo *startupinfo);

static gboolean
wcharv_to_utf8_charv(wchar_t **wcharv,
                     gchar ***utf8_charv,
                     gint *error_index)
{
    gchar **retval = NULL;

  *utf8_charv = NULL;
  if (wcharv != NULL) {
      int n = 0, i;

      while (wcharv[n])
          n++;
      retval = g_new(gchar *, n + 1);

      for (i = 0; i < n; i++) {
          retval[i] = g_utf16_to_utf8 (wcharv[i], -1, NULL, NULL, NULL);
          if (retval[i] == NULL) {
              if (error_index)
                  *error_index = i;
              while (i)
                  g_free(retval[--i]);
              g_free (retval);
              return FALSE;
          }
      }

      retval[n] = NULL;
  }
  *utf8_charv = retval;
  return TRUE;
}

static gboolean
redirect_program(gchar **argv)
{
    gchar *orig_path, *orig_base, *dir, *path;
    gchar *index;
    gboolean retval = FALSE;

    g_return_val_if_fail(*argv, FALSE);
    if (g_path_is_absolute(*argv)) {
        orig_path = g_strdup(*argv);
    } else {
        gchar *cd = g_get_current_dir();
        orig_path = g_build_filename(cd, *argv, (gchar*) NULL);
        g_free(cd);
    }

    orig_base = g_path_get_basename(orig_path);
    dir = g_strdup(orig_path);
    while (((index = strrchr(dir, '\\')) != NULL)
           || ((index = strrchr(dir, '/')) != NULL)) {
        *index = '\0';
        path = g_build_filename(dir, "bin", orig_base, (gchar*) NULL);
        g_debug("Testing %s", path);
        if (g_file_test(path, G_FILE_TEST_EXISTS)) {
            g_free(orig_path);
            *argv = path;
            retval = TRUE;
            break;
        }
        g_free(path);
    }

    g_free(dir);
    g_free(orig_base);
    g_free(orig_path);

    return retval;
}

#ifdef __GNUC__
#    ifndef _stdcall
#        define _stdcall  __attribute__((stdcall))
#    endif
#endif

int _stdcall
WinMain(struct HINSTANCE__ *hInstance,
        struct HINSTANCE__ *hPrevInstance,
        char *lpszCmdLine,
        int nCmdShow)
{
    int argc;
    wchar_t **wargv, **wenvp;
    _startupinfo si = { 0 };
    gint index, status;
    gchar **utf8_argv;
    GError *error = NULL;

    __wgetmainargs(&argc, &wargv, &wenvp, 0, &si);
    g_assert(argc == __argc);

    if (!wcharv_to_utf8_charv(wargv, &utf8_argv, &index)) {
        g_warning("Invalid argument at position %d", index);
        return -1;
    }

    if (!redirect_program(utf8_argv)) {
        g_warning("Could not find destination for %s", *utf8_argv);
        g_strfreev(utf8_argv);
        return -1;
    }

    if (!g_spawn_sync(NULL, utf8_argv, NULL,
                      G_SPAWN_LEAVE_DESCRIPTORS_OPEN
                      | G_SPAWN_CHILD_INHERITS_STDIN,
                      NULL, NULL, NULL, NULL, &status, &error)) {
        g_warning("Could not spawn program: %s", error->message);
        g_error_free(error);
        g_strfreev(utf8_argv);
        return -1;
    }

    return status;
}

/*
 * BinReloc - a library for creating relocatable executables
 * Written by: Hongli Lai <h.lai@chello.nl>
 * http://autopackage.org/
 *
 * This source code is public domain. You can relicense this code
 * under whatever license you want.
 *
 * See http://autopackage.org/docs/binreloc/ for
 * more information and how to use this.
 */
/********************************************************************\
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


#ifndef __BINRELOC_C__
#define __BINRELOC_C__
#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#ifdef ENABLE_BINRELOC
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef MAC_INTEGRATION
#include <gtkmacintegration/gtkosxapplication.h>
#endif
#endif /* ENABLE_BINRELOC */
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "binreloc.h"
#include "gnc-filepath-utils.h"
#include <glib.h>
#include "gncla-dir.h"

G_BEGIN_DECLS

/** @internal
 * Find the canonical filename of the executable. Returns the filename
 * (which must be freed) or NULL on error. If the parameter 'error' is
 * not NULL, the error code will be stored there, if an error occurred.
 */
static char *
_br_find_exe (Gnc_GbrInitError *error)
{
#ifndef ENABLE_BINRELOC
    if (error)
        *error = GNC_GBR_INIT_ERROR_DISABLED;
    return NULL;
#else
#ifdef G_OS_WIN32
    /* I *thought* this program code already included the
       relocation code for windows. Unfortunately this is not
       the case and we have to add this manually. This is only
       one possibility; other ways of looking up the full path
       of gnucash.exe probably exist.*/
    gchar *prefix;
    gchar *result;

    /* From the glib docs: When passed NULL, this function looks
       up installation the directory of the main executable of
       the current process */
    prefix = g_win32_get_package_installation_directory_of_module (NULL);
    result = g_build_filename (prefix,
                               BINDIR, "gnucash.exe",
                               (char*)NULL);
    g_free (prefix);
    return result;
#elif defined MAC_INTEGRATION
    gchar *path = gtkosx_application_get_executable_path();
    g_print ("Application Path %s\n", path);
    return path;
#else
    char *path, *path2, *line, *result;
    size_t buf_size;
    ssize_t size;
    struct stat stat_buf;
    FILE *f;

    /* Read from /proc/self/exe (symlink) */
    if (sizeof (path) > SSIZE_MAX)
        buf_size = SSIZE_MAX - 1;
    else
        buf_size = PATH_MAX - 1;
    path = (char *) g_try_malloc (buf_size);
    if (path == NULL)
    {
        /* Cannot allocate memory. */
        if (error)
            *error = GNC_GBR_INIT_ERROR_NOMEM;
        return NULL;
    }
    path2 = (char *) g_try_malloc (buf_size);
    if (path2 == NULL)
    {
        /* Cannot allocate memory. */
        if (error)
            *error = GNC_GBR_INIT_ERROR_NOMEM;
        g_free (path);
        return NULL;
    }

    strncpy (path2, "/proc/self/exe", buf_size - 1);

    while (1)
    {
        int i;

        size = readlink (path2, path, buf_size - 1);
        if (size == -1)
        {
            /* Error. */
            g_free (path2);
            break;
        }

        /* readlink() success. */
        path[size] = '\0';

        /* Check whether the symlink's target is also a symlink.
         * We want to get the final target. */
        i = stat (path, &stat_buf);
        if (i == -1)
        {
            /* Error. */
            g_free (path2);
            break;
        }

        /* stat() success. */
        if (!S_ISLNK (stat_buf.st_mode))
        {
            /* path is not a symlink. Done. */
            g_free (path2);
            return path;
        }

        /* path is a symlink. Continue loop and resolve this. */
        strncpy (path, path2, buf_size - 1);
    }


    /* readlink() or stat() failed; this can happen when the program is
     * running in Valgrind 2.2. Read from /proc/self/maps as fallback. */

    buf_size = PATH_MAX + 128;
    line = (char *) g_try_realloc (path, buf_size);
    if (line == NULL)
    {
        /* Cannot allocate memory. */
        g_free (path);
        if (error)
            *error = GNC_GBR_INIT_ERROR_NOMEM;
        return NULL;
    }

    f = fopen ("/proc/self/maps", "r");
    if (f == NULL)
    {
        g_free (line);
        if (error)
            *error = GNC_GBR_INIT_ERROR_OPEN_MAPS;
        return NULL;
    }

    /* The first entry should be the executable name. */
    result = fgets (line, (int) buf_size, f);
    if (result == NULL)
    {
        fclose (f);
        g_free (line);
        if (error)
            *error = GNC_GBR_INIT_ERROR_READ_MAPS;
        return NULL;
    }

    /* Get rid of newline character. */
    buf_size = strlen (line);
    if (buf_size <= 0)
    {
        /* Huh? An empty string? */
        fclose (f);
        g_free (line);
        if (error)
            *error = GNC_GBR_INIT_ERROR_INVALID_MAPS;
        return NULL;
    }
    if (line[buf_size - 1] == 10)
        line[buf_size - 1] = 0;

    /* Extract the filename; it is always an absolute path. */
    path = strchr (line, '/');

    /* Sanity check. */
    if (strstr (line, " r-xp ") == NULL || path == NULL)
    {
        fclose (f);
        g_free (line);
        if (error)
            *error = GNC_GBR_INIT_ERROR_INVALID_MAPS;
        return NULL;
    }

    path = g_strdup (path);
    g_free (line);
    fclose (f);
    return path;
#endif /* G_OS_WINDOWS */
#endif /* ENABLE_BINRELOC */
}



static gchar *exe = NULL;

static void set_gerror (GError **error, Gnc_GbrInitError errcode);


void gnc_gbr_set_exe (const gchar* default_exe)
{
    if (exe != NULL)
        g_free(exe);
    exe = NULL;

    if (default_exe != NULL)
        exe = g_strdup(default_exe);
}


/** Initialize the BinReloc library (for applications).
 *
 * This function must be called before using any other BinReloc functions.
 * It attempts to locate the application's canonical filename.
 *
 * @note If you want to use BinReloc for a library, then you should call
 *       gnc_gbr_init_lib() instead.
 *
 * @param error  If BinReloc failed to initialize, then the error report will
 *               be stored in this variable. Set to NULL if you don't want an
 *               error report. See the #Gnc_GbrInitError for a list of error
 *               codes.
 *
 * @returns TRUE on success, FALSE if BinReloc failed to initialize.
 */
gboolean
gnc_gbr_init (GError **error)
{
    Gnc_GbrInitError errcode = 0;

    /* Locate the application's filename. */
    exe = _br_find_exe (&errcode);
    if (exe != NULL)
        /* Success! */
        return TRUE;
    else
    {
        /* Failed :-( */
        set_gerror (error, errcode);
        return FALSE;
    }
}


static void
set_gerror (GError **error, Gnc_GbrInitError errcode)
{
    gchar *error_message;

    if (error == NULL)
        return;

    switch (errcode)
    {
    case GNC_GBR_INIT_ERROR_NOMEM:
        error_message = "Cannot allocate memory.";
        break;
    case GNC_GBR_INIT_ERROR_OPEN_MAPS:
        error_message = "Unable to open /proc/self/maps for reading.";
        break;
    case GNC_GBR_INIT_ERROR_READ_MAPS:
        error_message = "Unable to read from /proc/self/maps.";
        break;
    case GNC_GBR_INIT_ERROR_INVALID_MAPS:
        error_message = "The file format of /proc/self/maps is invalid.";
        break;
    case GNC_GBR_INIT_ERROR_DISABLED:
        error_message = "Binary relocation support is disabled.";
        break;
    default:
        error_message = "Unknown error.";
        break;
    };
    g_set_error (error, g_quark_from_static_string ("GBinReloc"),
                 errcode, "%s", error_message);
}


/** Find the canonical filename of the current application.
 *
 * @param default_exe  A default filename which will be used as fallback.
 * @returns A string containing the application's canonical filename,
 *          which must be freed when no longer necessary. If BinReloc is
 *          not initialized, or if the initialization function failed,
 *          then a copy of default_exe will be returned. If default_exe
 *          is NULL, then NULL will be returned.
 */
gchar *
gnc_gbr_find_exe (const gchar *default_exe)
{
    if (exe == NULL)
    {
        /* BinReloc is not initialized. */
        if (default_exe != NULL)
            return g_strdup (default_exe);
        else
            return NULL;
    }
    return g_strdup (exe);
}


/** Locate the directory in which the current application is installed.
 *
 * The prefix is generated by the following pseudo-code evaluation:
 * \code
 * dirname(exename)
 * \endcode
 *
 * @param default_dir  A default directory which will used as fallback.
 * @return A string containing the directory, which must be freed when no
 *         longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_dir
 *         will be returned. If default_dir is NULL, then NULL will be
 *         returned.
 */
gchar *
gnc_gbr_find_exe_dir (const gchar *default_dir)
{
    if (exe == NULL)
    {
        /* BinReloc not initialized. */
        if (default_dir != NULL)
            return g_strdup (default_dir);
        else
            return NULL;
    }

    return g_path_get_dirname (exe);
}


/** Locate the prefix in which the current application is installed.
 *
 * The prefix is generated by the following pseudo-code evaluation:
 * \code
 * dirname(dirname(exename))
 * \endcode
 *
 * @param default_prefix  A default prefix which will used as fallback.
 * @return A string containing the prefix, which must be freed when no
 *         longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_prefix
 *         will be returned. If default_prefix is NULL, then NULL will be
 *         returned.
 */

static inline gchar *
get_mac_bundle_prefix()
{
#if defined ENABLE_BINRELOC && defined MAC_INTEGRATION
    gchar *id = gtkosx_application_get_bundle_id ();
    gchar *path = gtkosx_application_get_resource_path ();
     if (id == NULL)
    {
        gchar *dirname = g_path_get_dirname (path);
        g_free (path);
        g_free (id);
        return dirname;
    }
    g_free (id);
    return path;
#endif
    return NULL;
}

static inline gchar*
get_builddir_prefix()
{
    if (g_getenv ("GNC_UNINSTALLED"))
        return g_strdup (g_getenv ("GNC_BUILDDIR"));
    return NULL;
}

gchar *
gnc_gbr_find_prefix (const gchar *default_prefix)
{
    gchar *dir1, *dir2;
    if ((dir2 = get_builddir_prefix()) || (dir2 = get_mac_bundle_prefix()))
        return dir2;
    if (exe == NULL)
    {
        /* BinReloc not initialized. */
        if (default_prefix != NULL)
            return g_strdup (default_prefix);
        else
            return NULL;
    }
    dir1 = g_path_get_dirname (exe);
    dir2 = g_path_get_dirname (dir1);
    g_free (dir1);
    return dir2;
}


/** Locate the application's binary folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/bin"
 * \endcode
 *
 * @param default_bin_dir  A default path which will used as fallback.
 * @return A string containing the bin folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_bin_dir will
 *         be returned. If default_bin_dir is NULL, then NULL will be returned.
 */
gchar *
gnc_gbr_find_bin_dir (const gchar *default_bin_dir)
{
    gchar *prefix, *dir, *bindir;
    prefix = gnc_gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_bin_dir != NULL)
            return g_strdup (default_bin_dir);
        else
            return NULL;
    }
    bindir = gnc_file_path_relative_part(PREFIX, BINDIR);
    dir = g_build_filename (prefix, bindir, NULL);
    g_free (bindir);
    g_free (prefix);
    return dir;
}

/** Locate the application's data folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/share"
 * \endcode
 *
 * @param default_data_dir  A default path which will used as fallback.
 * @return A string containing the data folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_data_dir
 *         will be returned. If default_data_dir is NULL, then NULL will be
 *         returned.
 */
gchar *
gnc_gbr_find_data_dir (const gchar *default_data_dir)
{
    gchar *prefix, *dir, *datadir;

    prefix = gnc_gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_data_dir != NULL)
            return g_strdup (default_data_dir);
        else
            return NULL;
    }

    datadir = gnc_file_path_relative_part(PREFIX, DATADIR);
    dir = g_build_filename (prefix, datadir, NULL);
    g_free (datadir);
    g_free (prefix);
    return dir;
}

/** Locate the application's library folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/lib"
 * \endcode
 *
 * @param default_lib_dir  A default path which will used as fallback.
 * @return A string containing the library folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_lib_dir will be returned.
 *         If default_lib_dir is NULL, then NULL will be returned.
 */
gchar *
gnc_gbr_find_lib_dir (const gchar *default_lib_dir)
{
    gchar *prefix, *dir, *libdir;

    prefix = gnc_gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_lib_dir != NULL)
            return g_strdup (default_lib_dir);
        else
            return NULL;
    }

    libdir = gnc_file_path_relative_part(PREFIX, LIBDIR);
    dir = g_build_filename (prefix, libdir, NULL);
    g_free (libdir);
    g_free (prefix);
    return dir;
}

/** Locate the application's configuration files folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/etc"
 * \endcode
 *
 * @param default_etc_dir  A default path which will used as fallback.
 * @return A string containing the etc folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the initialization
 *         function failed, then a copy of default_etc_dir will be returned.
 *         If default_etc_dir is NULL, then NULL will be returned.
 */
gchar *
gnc_gbr_find_etc_dir (const gchar *default_etc_dir)
{
    gchar *prefix, *dir, *sysconfdir;

    prefix = gnc_gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_etc_dir != NULL)
            return g_strdup (default_etc_dir);
        else
            return NULL;
    }

    if (g_path_is_absolute (SYSCONFDIR))
    {
        sysconfdir = gnc_file_path_relative_part("/", SYSCONFDIR);
        dir = g_build_filename (prefix, sysconfdir, NULL);
        g_free (sysconfdir);
    }
    else if ((g_strcmp0 (PREFIX, "/opt") == 0) ||
             (g_str_has_prefix (PREFIX, "/opt/")))
    {
        /* If the prefix is "/opt/..." the etc stuff will be installed in
         * "SYSCONFDIR/opt/...", while the rest will be in "/opt/..."
         * If this gets relocated after (make install), there will be another
         * prefix prepended to all of that:
         * "prefix2/opt/..."
         * "prefix2/SYSCONFDIR/opt/..."
         * Note: this most likely won't work on Windows. Don't try a /opt
         * prefix on that platform...
         */
        gchar *std_etc_dir = g_build_filename ("/", SYSCONFDIR, PREFIX, NULL);

        gchar *base_prefix_pos = g_strstr_len (prefix, -1, PREFIX);
        if (!base_prefix_pos || base_prefix_pos == prefix)
            dir = g_build_filename ("/", std_etc_dir, NULL);
        else
        {
            gchar *prefix2 = g_strndup (prefix, base_prefix_pos - prefix);
            dir = g_build_filename (prefix2, std_etc_dir, NULL);
        }
        g_free (std_etc_dir);

    }
    else
    {
        sysconfdir = gnc_file_path_relative_part(PREFIX, SYSCONFDIR);
        dir = g_build_filename (prefix, sysconfdir, NULL);
        g_free (sysconfdir);
    }
    g_free (prefix);
    return dir;
}


G_END_DECLS

#endif /* __BINRELOC_C__ */

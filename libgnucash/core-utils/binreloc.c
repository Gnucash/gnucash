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
#elif defined G_OS_WIN32
    /* N.B. g_win32_get_package_installation_directory_of_module returns the
     * parent if the last element of the directory is "bin" or "lib", but
     * otherwise the directory itself. We assume that gnucash.exe isn't in lib.
     */
    gchar *prefix = g_win32_get_package_installation_directory_of_module (NULL);
    gchar *result = g_build_filename (prefix, "bin", "gnucash.exe", NULL);
    if (prefix == NULL)
    {
        if (error)
            *error = GNC_GBR_INIT_WIN32_NO_EXE_DIR;
        return NULL;
    }
    if (!g_file_test (result, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_EXECUTABLE))
    {
        g_free (result);
        result = g_build_filename (prefix, "gnucash.exe", NULL);
        if (!g_file_test (result,
                           G_FILE_TEST_EXISTS | G_FILE_TEST_IS_EXECUTABLE))
        {
            g_free (result);
            result = NULL;
            if (error)
                *error = GNC_GBR_INIT_WIN32_NO_EXE_DIR;
        }
    }
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
    case GNC_GBR_INIT_ERROR_MAC_NOT_BUNDLE:
        error_message = "BinReloc determined that gnucash is not running from a bundle";
        break;
    case GNC_GBR_INIT_ERROR_MAC_NOT_APP_BUNDLE:
        error_message = "Binreloc determined that the bundle is not an app bundle";
        break;
    case GNC_GBR_INIT_WIN32_NO_EXE_DIR:
        error_message = "Binreloc was unable to determine the location of gnucash.exe.";
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

/* Locate a specified component directory.
 *
 * E.g., <prefix>/share

 * default_dir is passed in from the wrapper function, compiled_dir is the corresponding constant from gncla-dir.h.
 *
 * If compiled_dir exists and is an absolute path then we check the dynamic
 * prefix and if it's NULL fall back first on the passed-in default and then on
 * compiled_dir;
 * otherwise if the dynamic prefix turns out to be the compile time defined PREFIX
 * just use that
 * otherwise we pass the compiled PREFIX value as a default to
 * gnc_gbr_find_prefix, remove the PREFIX part (if any) from the compiled_dir
 * and append that to the retrieved prefix.
 */
static gchar*
find_component_directory (const gchar *default_dir, const gchar* compiled_dir)
{
    gchar *prefix = NULL, *dir = NULL;
    gchar *subdir = gnc_file_path_relative_part(PREFIX, compiled_dir);

    prefix = gnc_gbr_find_prefix (NULL);
    if (prefix == NULL)
        return g_strdup (default_dir ? default_dir : compiled_dir);
    if (!g_getenv("GNC_UNINSTALLED"))
    {
        if (!g_strcmp0 (prefix, PREFIX))
            return g_strdup (compiled_dir);

        if (g_strcmp0 (compiled_dir, subdir) == 0)
        {
            /* compiled_dir isn't a subdir of PREFIX. This isn't relocatable so
             * return compiled_dir.
             */
            g_free (subdir);
            g_free (prefix);
            return g_strdup (compiled_dir);
        }
    }
    dir = g_build_filename (prefix, subdir, NULL);
    g_free (subdir);
    g_free (prefix);
    return dir;
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
        return find_component_directory (default_bin_dir, BINDIR);
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
    return find_component_directory (default_data_dir, DATADIR);
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
    return find_component_directory (default_lib_dir, LIBDIR);
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
    return find_component_directory (default_etc_dir, SYSCONFDIR);
}


G_END_DECLS

#endif /* __BINRELOC_C__ */

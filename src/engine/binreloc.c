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

#ifndef __BINRELOC_C__
#define __BINRELOC_C__
#include "config.h"

#ifdef ENABLE_BINRELOC
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef MAC_INTEGRATION
#include <igemacintegration/ige-mac-bundle.h>
#endif
#endif /* ENABLE_BINRELOC */
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "binreloc.h"
#include <glib.h>

G_BEGIN_DECLS
#if defined ENABLE_BINRELOC && defined MAC_INTEGRATION
static IgeMacBundle *bundle = NULL;
#endif

/** @internal
 * Find the canonical filename of the executable. Returns the filename
 * (which must be freed) or NULL on error. If the parameter 'error' is
 * not NULL, the error code will be stored there, if an error occured.
 */
static char *
_br_find_exe (GbrInitError *error)
{
#ifndef ENABLE_BINRELOC
    if (error)
        *error = GBR_INIT_ERROR_DISABLED;
    return NULL;
#else
#ifdef G_OS_WIN32
    /* I *thought* this program code already included the
       relocation code for windows. Unfortunately this is not
       the case and we have to add this manually. This is only
       one possibility; other ways of looking up the full path
       of gnucash-bin.exe probably exist.*/
    gchar *prefix;
    gchar *result;

    /* From the glib docs: When passed NULL, this function looks
       up installation the directory of the main executable of
       the current process */
    prefix = g_win32_get_package_installation_directory_of_module (NULL);
    result = g_build_filename (prefix,
                               "bin", "gnucash-bin.exe",
                               (char*)NULL);
    g_free (prefix);
    return result;
#elif MAC_INTEGRATION
    gchar *prefix = NULL, *result = NULL;
    g_type_init();
    bundle = ige_mac_bundle_new();
    if (!bundle)
    {
        *error = GBR_INIT_ERROR_MAC_NOT_BUNDLE;
        return NULL;
    }
    if (!ige_mac_bundle_get_is_app_bundle (bundle))
    {
        g_object_unref(bundle);
        bundle = NULL;
        *error = GBR_INIT_ERROR_MAC_NOT_APP_BUNDLE;
        return NULL;
    }
    ige_mac_bundle_setup_environment(bundle);
    prefix = g_strdup(ige_mac_bundle_get_path(bundle));
    result = g_build_filename(prefix, "Contents/MacOS",
                              "gnucash-bin", NULL);
    g_free(prefix);
    return result;
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
            *error = GBR_INIT_ERROR_NOMEM;
        return NULL;
    }
    path2 = (char *) g_try_malloc (buf_size);
    if (path2 == NULL)
    {
        /* Cannot allocate memory. */
        if (error)
            *error = GBR_INIT_ERROR_NOMEM;
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
            *error = GBR_INIT_ERROR_NOMEM;
        return NULL;
    }

    f = fopen ("/proc/self/maps", "r");
    if (f == NULL)
    {
        g_free (line);
        if (error)
            *error = GBR_INIT_ERROR_OPEN_MAPS;
        return NULL;
    }

    /* The first entry should be the executable name. */
    result = fgets (line, (int) buf_size, f);
    if (result == NULL)
    {
        fclose (f);
        g_free (line);
        if (error)
            *error = GBR_INIT_ERROR_READ_MAPS;
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
            *error = GBR_INIT_ERROR_INVALID_MAPS;
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
            *error = GBR_INIT_ERROR_INVALID_MAPS;
        return NULL;
    }

    path = g_strdup (path);
    g_free (line);
    fclose (f);
    return path;
#endif /* G_OS_WINDOWS */
#endif /* ENABLE_BINRELOC */
}


/** @internal
 * Find the canonical filename of the executable which owns symbol.
 * Returns a filename which must be freed, or NULL on error.
 */
static char *
_br_find_exe_for_symbol (const void *symbol, GbrInitError *error)
{
#ifndef ENABLE_BINRELOC
    if (error)
        *error = GBR_INIT_ERROR_DISABLED;
    return (char *) NULL;
#else
#if defined G_OS_WIN32
    g_warning ("_br_find_exe_for_symbol not implemented on win32.");
    if (error)
        *error = GBR_INIT_ERROR_DISABLED;
    return (char *) NULL;
#else
#define SIZE PATH_MAX + 100
    FILE *f;
    size_t address_string_len;
    char *address_string, line[SIZE], *found;

    if (symbol == NULL)
        return (char *) NULL;

    f = fopen ("/proc/self/maps", "r");
    if (f == NULL)
        return (char *) NULL;

    address_string_len = 4;
    address_string = (char *) g_try_malloc (address_string_len);
    found = (char *) NULL;

    while (!feof (f))
    {
        char *start_addr, *end_addr, *end_addr_end, *file;
        void *start_addr_p, *end_addr_p;
        size_t len;

        if (fgets (line, SIZE, f) == NULL)
            break;

        /* Sanity check. */
        if (strstr (line, " r-xp ") == NULL || strchr (line, '/') == NULL)
            continue;

        /* Parse line. */
        start_addr = line;
        end_addr = strchr (line, '-');
        file = strchr (line, '/');

        /* More sanity check. */
        if (!(file > end_addr && end_addr != NULL && end_addr[0] == '-'))
            continue;

        end_addr[0] = '\0';
        end_addr++;
        end_addr_end = strchr (end_addr, ' ');
        if (end_addr_end == NULL)
            continue;

        end_addr_end[0] = '\0';
        len = strlen (file);
        if (len == 0)
            continue;
        if (file[len - 1] == '\n')
            file[len - 1] = '\0';

        /* Get rid of "(deleted)" from the filename. */
        len = strlen (file);
        if (len > 10 && strcmp (file + len - 10, " (deleted)") == 0)
            file[len - 10] = '\0';

        /* I don't know whether this can happen but better safe than sorry. */
        len = strlen (start_addr);
        if (len != strlen (end_addr))
            continue;


        /* Transform the addresses into a string in the form of 0xdeadbeef,
         * then transform that into a pointer. */
        if (address_string_len < len + 3)
        {
            address_string_len = len + 3;
            address_string = (char *) g_try_realloc (address_string, address_string_len);
        }

        memcpy (address_string, "0x", 2);
        memcpy (address_string + 2, start_addr, len);
        address_string[2 + len] = '\0';
        sscanf (address_string, "%p", &start_addr_p);

        memcpy (address_string, "0x", 2);
        memcpy (address_string + 2, end_addr, len);
        address_string[2 + len] = '\0';
        sscanf (address_string, "%p", &end_addr_p);


        if (symbol >= start_addr_p && symbol < end_addr_p)
        {
            found = file;
            break;
        }
    }

    g_free (address_string);
    fclose (f);

    if (found == NULL)
        return (char *) NULL;
    else
        return g_strdup (found);
#endif /* G_OS_WIN32 */
#endif /* ENABLE_BINRELOC */
}


static gchar *exe = NULL;

static void set_gerror (GError **error, GbrInitError errcode);


/** Initialize the BinReloc library (for applications).
 *
 * This function must be called before using any other BinReloc functions.
 * It attempts to locate the application's canonical filename.
 *
 * @note If you want to use BinReloc for a library, then you should call
 *       gbr_init_lib() instead.
 *
 * @param error  If BinReloc failed to initialize, then the error report will
 *               be stored in this variable. Set to NULL if you don't want an
 *               error report. See the #GbrInitError for a list of error
 *               codes.
 *
 * @returns TRUE on success, FALSE if BinReloc failed to initialize.
 */
gboolean
gbr_init (GError **error)
{
    GbrInitError errcode = 0;

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


/** Initialize the BinReloc library (for libraries).
 *
 * This function must be called before using any other BinReloc functions.
 * It attempts to locate the calling library's canonical filename.
 *
 * @note The BinReloc source code MUST be included in your library, or this
 *       function won't work correctly.
 *
 * @returns TRUE on success, FALSE if a filename cannot be found.
 */
gboolean
gbr_init_lib (GError **error)
{
    GbrInitError errcode = 0;

    exe = _br_find_exe_for_symbol ((const void *) "", &errcode);
    if (exe != NULL)
        /* Success! */
        return TRUE;
    else
    {
        /* Failed :-( */
        set_gerror (error, errcode);
        return exe != NULL;
    }
}


static void
set_gerror (GError **error, GbrInitError errcode)
{
    gchar *error_message;

    if (error == NULL)
        return;

    switch (errcode)
    {
    case GBR_INIT_ERROR_NOMEM:
        error_message = "Cannot allocate memory.";
        break;
    case GBR_INIT_ERROR_OPEN_MAPS:
        error_message = "Unable to open /proc/self/maps for reading.";
        break;
    case GBR_INIT_ERROR_READ_MAPS:
        error_message = "Unable to read from /proc/self/maps.";
        break;
    case GBR_INIT_ERROR_INVALID_MAPS:
        error_message = "The file format of /proc/self/maps is invalid.";
        break;
    case GBR_INIT_ERROR_MAC_NOT_BUNDLE:
        error_message = "Binreloc did not find a bundle";
        break;
    case GBR_INIT_ERROR_MAC_NOT_APP_BUNDLE:
        error_message = "Binreloc found that the bundle is not an app bundle";
        break;
    case GBR_INIT_ERROR_DISABLED:
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
gbr_find_exe (const gchar *default_exe)
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
gbr_find_exe_dir (const gchar *default_dir)
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
gchar *
gbr_find_prefix (const gchar *default_prefix)
{
    gchar *dir1, *dir2;

#if defined ENABLE_BINRELOC && defined MAC_INTEGRATION
    gchar *prefix = NULL, *result = NULL;
    if (bundle == NULL)
    {
        /* BinReloc not initialized. */
        if (default_prefix != NULL)
            return g_strdup (default_prefix);
        else
            return NULL;
    }
    prefix = g_strdup(ige_mac_bundle_get_path(bundle));
    result = g_build_filename(prefix, "Contents/Resources", NULL);
    g_free(prefix);
    return result;
#else

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
#endif //ENABLE_BINRELOC && MAC_INTEGRATION
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
gbr_find_bin_dir (const gchar *default_bin_dir)
{
    gchar *prefix, *dir;
#if defined ENABLE_BINRELOC && defined MAC_INTEGRATION
    if (bundle == NULL)
    {
        /* BinReloc not initialized. */
        if (default_bin_dir != NULL)
            return g_strdup (default_bin_dir);
        else
            return NULL;
    }
    prefix = g_strdup(ige_mac_bundle_get_path(bundle));
    dir = g_build_filename(prefix, "Contents/MacOS", NULL);
    g_free(prefix);
    return dir;
#else

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_bin_dir != NULL)
            return g_strdup (default_bin_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "bin", NULL);
    g_free (prefix);
    return dir;
#endif //ENABLE_BINRELOC && MAC_INTEGRATION
}


/** Locate the application's superuser binary folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/sbin"
 * \endcode
 *
 * @param default_sbin_dir  A default path which will used as fallback.
 * @return A string containing the sbin folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the
 *         initialization function failed, then a copy of default_sbin_dir will
 *         be returned. If default_bin_dir is NULL, then NULL will be returned.
 */
gchar *
gbr_find_sbin_dir (const gchar *default_sbin_dir)
{
    gchar *prefix, *dir;

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_sbin_dir != NULL)
            return g_strdup (default_sbin_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "sbin", NULL);
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
gbr_find_data_dir (const gchar *default_data_dir)
{
    gchar *prefix, *dir;

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_data_dir != NULL)
            return g_strdup (default_data_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "share", NULL);
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
gbr_find_lib_dir (const gchar *default_lib_dir)
{
    gchar *prefix, *dir;

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_lib_dir != NULL)
            return g_strdup (default_lib_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "lib", NULL);
    g_free (prefix);
    return dir;
}


/** Locate the application's libexec folder.
 *
 * The path is generated by the following pseudo-code evaluation:
 * \code
 * prefix + "/libexec"
 * \endcode
 *
 * @param default_libexec_dir  A default path which will used as fallback.
 * @return A string containing the libexec folder's path, which must be freed when
 *         no longer necessary. If BinReloc is not initialized, or if the initialization
 *         function failed, then a copy of default_libexec_dir will be returned.
 *         If default_libexec_dir is NULL, then NULL will be returned.
 */
gchar *
gbr_find_libexec_dir (const gchar *default_libexec_dir)
{
    gchar *prefix, *dir;

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_libexec_dir != NULL)
            return g_strdup (default_libexec_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "libexec", NULL);
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
gbr_find_etc_dir (const gchar *default_etc_dir)
{
    gchar *prefix, *dir;

    prefix = gbr_find_prefix (NULL);
    if (prefix == NULL)
    {
        /* BinReloc not initialized. */
        if (default_etc_dir != NULL)
            return g_strdup (default_etc_dir);
        else
            return NULL;
    }

    dir = g_build_filename (prefix, "etc", NULL);
    g_free (prefix);
    return dir;
}


G_END_DECLS

#endif /* __BINRELOC_C__ */

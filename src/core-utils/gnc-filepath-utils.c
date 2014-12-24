/********************************************************************\
 * gnc-filepath-utils.c -- file path resolutin utilitie             *
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
\********************************************************************/

/*
 * @file gnc-filepath-utils.c
 * @brief file path resolution utilities
 * @author Copyright (c) 1998-2004 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gprintf.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <errno.h>

#include "gnc-path.h"
#include "gnc-filepath-utils.h"
#include "libqof/qof/qof.h"

#ifdef _MSC_VER
#include <glib/gwin32.h>
#define PATH_MAX MAXPATHLEN
#endif

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;


/**
 * Scrubs a filename by changing "strange" chars (e.g. those that are not
 * valid in a win32 file name) to "_".
 *
 * @param filename File name - updated in place
 */
static void
scrub_filename(char* filename)
{
    char* p;

#define STRANGE_CHARS "/:"
    p = strpbrk(filename, STRANGE_CHARS);
    while (p)
    {
        *p = '_';
        p = strpbrk(filename, STRANGE_CHARS);
    }
}

/** Check if the path exists and is a regular file.
 *
 * \param path -- freed if the path doesn't exist or isn't a regular file
 *
 *  \return NULL or the path
 */
static gchar *
check_path_return_if_valid(gchar *path)
{
    if (g_file_test(path, G_FILE_TEST_IS_REGULAR))
    {
        return path;
    }
    g_free (path);
    return NULL;
}

/** @brief Create an absolute path when given a relative path;
 *  otherwise return the argument.
 *
 *  @warning filefrag should be a simple path fragment. It shouldn't
 *  contain xml:// or http:// or <whatever>:// other protocol specifiers.
 *
 *  If passed a string which g_path_is_absolute declares an absolute
 *  path, return the argument.
 *
 *  Otherwise, assume that filefrag is a well-formed relative path and
 *  try to find a file with its path relative to
 *  \li  the current working directory,
 *  \li the installed system-wide data directory (e.g., /usr/local/share/gnucash),
 *  \li the installed system configuration directory (e.g., /usr/local/etc/gnucash),
 *  \li or in the user's configuration directory (e.g., $HOME/.gnucash/data)
 *
 *  The paths are searched for in that order. If a matching file is
 *  found, return the absolute path to it.

 *  If one isn't found, return a absolute path relative to the
 *  user's configuration directory and note in the trace file that it
 *  needs to be created.
 *
 *  @param filefrag The file path to resolve
 *
 *  @return An absolute file path.
 */
gchar *
gnc_resolve_file_path (const gchar * filefrag)
{
    gchar *fullpath = NULL, *tmp_path = NULL;

    /* seriously invalid */
    if (!filefrag)
    {
        g_critical("filefrag is NULL");
        return NULL;
    }

    /* ---------------------------------------------------- */
    /* OK, now we try to find or build an absolute file path */

    /* check for an absolute file path */
    if (g_path_is_absolute(filefrag))
        return g_strdup (filefrag);

    /* Look in the current working directory */
    tmp_path = g_get_current_dir();
    fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
    g_free(tmp_path);
    fullpath = check_path_return_if_valid(fullpath);
    if (fullpath != NULL)
        return fullpath;

    /* Look in the data dir (e.g. $PREFIX/share/gnucash) */
    tmp_path = gnc_path_get_pkgdatadir();
    fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
    g_free(tmp_path);
    fullpath = check_path_return_if_valid(fullpath);
    if (fullpath != NULL)
        return fullpath;

    /* Look in the config dir (e.g. $PREFIX/share/gnucash/accounts) */
    tmp_path = gnc_path_get_accountsdir();
    fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
    g_free(tmp_path);
    fullpath = check_path_return_if_valid(fullpath);
    if (fullpath != NULL)
        return fullpath;

    /* Look in the users config dir (e.g. $HOME/.gnucash/data) */
    fullpath = gnc_build_data_path(filefrag);
    if (g_file_test(fullpath, G_FILE_TEST_IS_REGULAR))
        return fullpath;

    /* OK, it's not there. Note that it needs to be created and pass it
     * back anyway */
    g_warning("create new file %s", fullpath);
    return fullpath;

}

/* Searches for a file fragment paths set via GNC_DOC_PATH environment
 * variable. If this variable is not set, fall back to search in
 * - a html directory in the local user's gnucash settings directory
 *   (typically $HOME/.gnucash/html)
 * - the gnucash documentation directory
 *   (typically /usr/share/doc/gnucash)
 * - the gnucash data directory
 *   (typically /usr/share/gnucash)
 * It searches in this order.
 *
 * This is used by gnc_path_find_localized_file to search for
 * localized versions of files if they exist.
 */
static gchar *
gnc_path_find_localized_html_file_internal (const gchar * file_name)
{
    gchar *full_path = NULL;
    int i;
    const gchar *env_doc_path = g_getenv("GNC_DOC_PATH");
    const gchar *default_dirs[] =
        {
            gnc_build_dotgnucash_path ("html"),
            gnc_path_get_pkgdocdir (),
            gnc_path_get_pkgdatadir (),
            NULL
        };
    gchar **dirs;

    if (!file_name || *file_name == '\0')
        return NULL;

    /* Allow search path override via GNC_DOC_PATH environment variable */
    if (env_doc_path)
        dirs = g_strsplit (env_doc_path, G_SEARCHPATH_SEPARATOR_S, -1);
    else
        dirs = (gchar **)default_dirs;

    for (i = 0; dirs[i]; i++)
    {
        full_path = g_build_filename (dirs[i], file_name, (gchar *)NULL);
        DEBUG ("Checking for existence of %s", full_path);
        full_path = check_path_return_if_valid (full_path);
        if (full_path != NULL)
            return full_path;
    }

    return NULL;
}

/** @brief Find an absolute path to a localized version of a given
 *  relative path to a html or html related file.
 *  If no localized version exists, an absolute path to the file
 *  is searched for. If that file doesn't exist either, returns NULL.
 *
 *  @warning file_name should be a simple path fragment. It shouldn't
 *  contain xml:// or http:// or <whatever>:// other protocol specifiers.
 *
 *  If passed a string which g_path_is_absolute declares an absolute
 *  path, return the argument.
 *
 *  Otherwise, assume that file_name is a well-formed relative path and
 *  try to find a file with its path relative to
 *  \li a localized subdirectory in the html directory
 *      of the user's configuration directory
 *      (e.g. $HOME/.gnucash/html/de_DE, $HOME/.gnucash/html/en,...)
 *  \li a localized subdirectory in the gnucash documentation directory
 *      (e.g. /usr/share/doc/gnucash/C,...)
 *  \li the html directory of the user's configuration directory
 *      (e.g. $HOME/.gnucash/html)
 *  \li the gnucash documentation directory
 *      (e.g. /usr/share/doc/gnucash/)
 *  \li last resort option: the gnucash data directory
 *      (e.g. /usr/share/gnucash/)
 *
 *  The paths are searched for in that order. If a matching file is
 *  found, return the absolute path to it.

 *  If one isn't found, return NULL.
 *
 *  @param file_name The file path to resolve
 *
 *  @return An absolute file path or NULL if no file is found.
 */
gchar *
gnc_path_find_localized_html_file (const gchar *file_name)
{
    gchar *loc_file_name = NULL;
    gchar *full_path = NULL;
    const gchar * const *lang;
    int i;

    if (!file_name || *file_name == '\0')
        return NULL;

    /* An absolute path is returned unmodified. */
    if (g_path_is_absolute (file_name))
        return g_strdup (file_name);

    /* First try to find the file in any of the localized directories
     * the user has set up on his system
     */
    for (lang = g_get_language_names (); *lang; lang++)
    {
        loc_file_name = g_build_filename (*lang, file_name, (gchar *)NULL);
        full_path = gnc_path_find_localized_html_file_internal (loc_file_name);
        g_free (loc_file_name);
        if (full_path != NULL)
            return full_path;
    }

    /* If not found in a localized directory, try to find the file
     * in any of the base directories
     */
    return gnc_path_find_localized_html_file_internal (file_name);

}

/* ====================================================================== */

/** @brief Check that the supplied directory path exists, is a directory, and
 * that the user has adequate permissions to use it.
 *
 * @param dirname The path to check
 */
static gboolean
gnc_validate_directory (const gchar *dirname, gboolean create, gchar **msg)
{
    struct stat statbuf;
    gint rc;

    *msg = NULL;
    rc = g_stat (dirname, &statbuf);
    if (rc)
    {
        switch (errno)
        {
        case ENOENT:
            if (create)
            {
                rc = g_mkdir (dirname,
#ifdef G_OS_WIN32
                              0          /* The mode argument is ignored on windows */
#else
                              S_IRWXU    /* perms = S_IRWXU = 0700 */
#endif
                              );
                if (rc)
                {
                    *msg = g_strdup_printf(
                            _("An error occurred while creating the directory:\n"
                              "  %s\n"
                              "Please correct the problem and restart GnuCash.\n"
                              "The reported error was '%s' (errno %d).\n"),
                              dirname, g_strerror(errno) ? g_strerror(errno) : "", errno);
                    return FALSE;
                }
            }
            else
            {
                *msg = g_strdup_printf(
                        _("Note: the directory\n"
                          "  %s\n"
                          "doesn't exist. This is however not fatal.\n"),
                          dirname);
                return FALSE;
            }
            g_stat (dirname, &statbuf);
            break;

        case EACCES:
            *msg = g_strdup_printf(
                      _("The directory\n"
                        "  %s\n"
                        "exists but cannot be accessed. This program \n"
                        "must have full access (read/write/execute) to \n"
                        "the directory in order to function properly.\n"),
                      dirname);
            return FALSE;

        case ENOTDIR:
            *msg = g_strdup_printf(
                      _("The path\n"
                        "  %s\n"
                        "exists but it is not a directory. Please delete\n"
                        "the file and start GnuCash again.\n"),
                      dirname);
            return FALSE;

        default:
            *msg = g_strdup_printf(
                      _("An unknown error occurred when validating that the\n"
                        "  %s\n"
                        "directory exists and is usable. Please correct the\n"
                        "problem and restart GnuCash. The reported error \n"
                        "was '%s' (errno %d)."),
                      dirname, g_strerror(errno) ? g_strerror(errno) : "", errno);
            return FALSE;
        }
    }

    if ((statbuf.st_mode & S_IFDIR) != S_IFDIR)
    {
        *msg = g_strdup_printf(
                  _("The path\n"
                    "  %s\n"
                    "exists but it is not a directory. Please delete\n"
                    "the file and start GnuCash again.\n"),
                  dirname);
        return FALSE;
    }
#ifndef G_OS_WIN32
    /* The mode argument is ignored on windows anyway */
    if ((statbuf.st_mode & S_IRWXU) != S_IRWXU)
    {
        *msg = g_strdup_printf(
                  _("The permissions are wrong on the directory\n"
                    "  %s\n"
                    "They must be at least 'rwx' for the user.\n"),
                  dirname);
        return FALSE;
    }
#endif

    return TRUE;
}

/** @fn const gchar * gnc_dotgnucash_dir ()
 *  @brief Ensure that the user's configuration directory exists and is minimally populated.
 *
 *  Note that the default path is $HOME/.gnucash; This can be changed
 *  by the environment variable $GNC_DOT_DIR.
 *
 *  @return An absolute path to the configuration directory
 */
const gchar *
gnc_dotgnucash_dir (void)
{
    static gchar *dotgnucash = NULL;
    gchar *tmp_dir;
    gchar *errmsg = NULL;

    if (dotgnucash)
        return dotgnucash;

    dotgnucash = g_strdup(g_getenv("GNC_DOT_DIR"));

    if (!dotgnucash)
    {
        const gchar *home = g_get_home_dir();
        if (!home || !gnc_validate_directory(home, FALSE, &errmsg))
        {
            g_free(errmsg);
            g_warning("Cannot find suitable home directory. Using tmp directory instead.");
            home = g_get_tmp_dir();
        }
        g_assert(home);

        dotgnucash = g_build_filename(home, ".gnucash", (gchar *)NULL);
    }
    if (!gnc_validate_directory(dotgnucash, TRUE, &errmsg))
    {
        const gchar *tmp = g_get_tmp_dir();
        g_free(errmsg);
        g_free(dotgnucash);
        g_warning("Cannot find suitable .gnucash directory in home directory. Using tmp directory instead.");
        g_assert(tmp);

        dotgnucash = g_build_filename(tmp, ".gnucash", (gchar *)NULL);
        
        if (!gnc_validate_directory(dotgnucash, TRUE, &errmsg))
            exit(1);
    }

    /* Since we're in code that is only executed once.... */
    tmp_dir = g_build_filename(dotgnucash, "books", (gchar *)NULL);
    if (!gnc_validate_directory(tmp_dir, TRUE, &errmsg))
        exit(1);
    g_free(tmp_dir);
    tmp_dir = g_build_filename(dotgnucash, "checks", (gchar *)NULL);
    if (!gnc_validate_directory(tmp_dir, TRUE, &errmsg))
        exit(1);
    g_free(tmp_dir);
    tmp_dir = g_build_filename(tmp_dir, "translog", (gchar *)NULL);
    if (!gnc_validate_directory(dotgnucash, TRUE, &errmsg))
        exit(1);
    g_free(tmp_dir);

    return dotgnucash;
}

/** @fn gchar * gnc_build_dotgnucash_path (const gchar *filename)
 *  @brief Make a path to filename in the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_dotgnucash_path (const gchar *filename)
{
    return g_build_filename(gnc_dotgnucash_dir(), filename, (gchar *)NULL);
}

/** @fn gchar * gnc_build_book_path (const gchar *filename)
 *  @brief Make a path to filename in the book subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_book_path (const gchar *filename)
{
    gchar* filename_dup = g_strdup(filename);
    gchar* result = NULL;

    scrub_filename(filename_dup);
    result = g_build_filename(gnc_dotgnucash_dir(), "books",
                              filename_dup, (gchar *)NULL);
    g_free(filename_dup);
    return result;
}

/** @fn gchar * gnc_build_translog_path (const gchar *filename)
 *  @brief Make a path to filename in the translog subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_translog_path (const gchar *filename)
{
    gchar* filename_dup = g_strdup(filename);
    gchar* result = NULL;

    scrub_filename(filename_dup);
    result = g_build_filename(gnc_dotgnucash_dir(), "translog",
                              filename_dup, (gchar *)NULL);
    g_free(filename_dup);
    return result;
}

/** @fn gchar * gnc_build_data_path (const gchar *filename)
 *  @brief Make a path to filename in the data subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_data_path (const gchar *filename)
{
    gchar* filename_dup = g_strdup(filename);
    gchar* result;

    scrub_filename(filename_dup);
    result = g_build_filename(gnc_dotgnucash_dir(), "data", filename_dup, (gchar *)NULL);
    g_free(filename_dup);
    return result;
}

/** @fn gchar * gnc_build_report_path (const gchar *filename)
 *  @brief Make a path to filename in the report directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_report_path (const gchar *filename)
{
    gchar *result = g_build_filename(gnc_path_get_reportdir(), filename, (gchar *)NULL);
    return result;
}

/** @fn gchar * gnc_build_stdreports_path (const gchar *filename)
 *  @brief Make a path to filename in the standard reports directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path.
 */

gchar *
gnc_build_stdreports_path (const gchar *filename)
{
    gchar *result = g_build_filename(gnc_path_get_stdreportsdir(), filename, (gchar *)NULL);
    return result;
}

static gchar *
gnc_filepath_locate_file (const gchar *default_path, const gchar *name)
{
    gchar *fullname;

    g_return_val_if_fail (name != NULL, NULL);

    if (g_path_is_absolute (name))
        fullname = g_strdup (name);
    else if (default_path)
        fullname = g_build_filename (default_path, name, NULL);
    else
        fullname = gnc_resolve_file_path (name);

    if (!g_file_test (fullname, G_FILE_TEST_IS_REGULAR))
    {
        g_warning ("Could not locate file %s", name);
        g_free (fullname);
        return NULL;
    }

    return fullname;
}

gchar *
gnc_filepath_locate_data_file (const gchar *name)
{
    return gnc_filepath_locate_file (gnc_path_get_pkgdatadir(), name);
}

gchar *
gnc_filepath_locate_pixmap (const gchar *name)
{
    gchar *default_path;
    gchar *fullname;

    default_path = g_build_filename (gnc_path_get_pkgdatadir (), "pixmaps", NULL);
    fullname = gnc_filepath_locate_file (default_path, name);
    g_free(default_path);

    return fullname;
}

gchar *
gnc_filepath_locate_ui_file (const gchar *name)
{
    gchar *default_path;
    gchar *fullname;

    default_path = g_build_filename (gnc_path_get_pkgdatadir (), "ui", NULL);
    fullname = gnc_filepath_locate_file (default_path, name);
    g_free(default_path);

    return fullname;
}

gchar *
gnc_filepath_locate_doc_file (const gchar *name)
{
    return gnc_filepath_locate_file (gnc_path_get_pkgdocdir(), name);
}


/* =============================== END OF FILE ========================== */

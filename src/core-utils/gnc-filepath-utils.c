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

#ifdef _MSC_VER
#include <glib/gwin32.h>
#define PATH_MAX MAXPATHLEN
#endif


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
    int namelen;
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

    /* get conservative on the length so that sprintf(getpid()) works ... */
    /* strlen ("/.LCK") + sprintf (%x%d) */
    namelen = strlen (filefrag) + 25;

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

    /* Look in the config dir (e.g. $PREFIX/etc/gnucash) */
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

/* ====================================================================== */

/** @brief Check that the supplied directory path exists, is a directory, and 
 * that the user has adequate permissions to use it.
 *
 * @param dirname The path to check
 */
static void
gnc_validate_directory (const gchar *dirname)
{
    struct stat statbuf;
    gint rc;

    rc = g_stat (dirname, &statbuf);
    if (rc)
    {
        switch (errno)
        {
        case ENOENT:
            rc = g_mkdir (dirname,
#ifdef G_OS_WIN32
                          0          /* The mode argument is ignored on windows */
#else
                          S_IRWXU    /* perms = S_IRWXU = 0700 */
#endif
                         );
            if (rc)
            {
                g_fprintf(stderr,
                          _("An error occurred while creating the directory:\n"
                            "  %s\n"
                            "Please correct the problem and restart GnuCash.\n"
                            "The reported error was '%s' (errno %d).\n"),
                          dirname, g_strerror(errno) ? g_strerror(errno) : "", errno);
                exit(1);
            }
            g_stat (dirname, &statbuf);
            break;

        case EACCES:
            g_fprintf(stderr,
                      _("The directory\n"
                        "  %s\n"
                        "exists but cannot be accessed.  This program \n"
                        "must have full access (read/write/execute) to \n"
                        "the directory in order to function properly.\n"),
                      dirname);
            exit(1);

        case ENOTDIR:
            g_fprintf(stderr,
                      _("The path\n"
                        "  %s\n"
                        "exists but it is not a directory. Please delete\n"
                        "the file and start GnuCash again.\n"),
                      dirname);
            exit(1);

        default:
            g_fprintf(stderr,
                      _("An unknown error occurred when validating that the\n"
                        "  %s\n"
                        "directory exists and is usable. Please correct the\n"
                        "problem and restart GnuCash.  The reported error \n"
                        "was '%s' (errno %d)."),
                      dirname, g_strerror(errno) ? g_strerror(errno) : "", errno);
            exit(1);
        }
    }

    if ((statbuf.st_mode & S_IFDIR) != S_IFDIR)
    {
        g_fprintf(stderr,
                  _("The path\n"
                    "  %s\n"
                    "exists but it is not a directory. Please delete\n"
                    "the file and start GnuCash again.\n"),
                  dirname);
        exit(1);
    }
#ifndef G_OS_WIN32
    /* The mode argument is ignored on windows anyway */
    if ((statbuf.st_mode & S_IRWXU) != S_IRWXU)
    {
        g_fprintf(stderr,
                  _("The permissions are wrong on the directory\n"
                    "  %s\n"
                    "They must be at least 'rwx' for the user.\n"),
                  dirname);
        exit(1);
    }
#endif
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

    if (dotgnucash)
        return dotgnucash;

    dotgnucash = g_strdup(g_getenv("GNC_DOT_DIR"));

    if (!dotgnucash)
    {
        const gchar *home = g_get_home_dir();
        if (!home)
        {
            g_warning("Cannot find home directory. Using tmp directory instead.");
            home = g_get_tmp_dir();
        }
        g_assert(home);

        dotgnucash = g_build_filename(home, ".gnucash", (gchar *)NULL);
    }
    gnc_validate_directory(dotgnucash);

    /* Since we're in code that is only executed once.... */
    tmp_dir = g_build_filename(dotgnucash, "books", (gchar *)NULL);
    gnc_validate_directory(tmp_dir);
    g_free(tmp_dir);
    tmp_dir = g_build_filename(dotgnucash, "checks", (gchar *)NULL);
    gnc_validate_directory(tmp_dir);
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

/* =============================== END OF FILE ========================== */

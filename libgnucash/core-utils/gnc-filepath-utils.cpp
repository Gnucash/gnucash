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

extern "C" {
#include "config.h"

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

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

#if defined (_MSC_VER) || defined (G_OS_WIN32)
#include <glib/gwin32.h>
#define PATH_MAX MAXPATHLEN
#endif
}

#include <boost/filesystem.hpp>

namespace bfs = boost::filesystem;
namespace bst = boost::system;

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
 *  \li the current working directory,
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
    fullpath = g_strdup(gnc_build_data_path(filefrag));
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
            gnc_build_userdata_path ("html"),
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
        g_debug ("Checking for existence of %s", full_path);
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
static bool
gnc_validate_directory (const bfs::path &dirname, bool create)
{
    if (dirname.empty())
        return false;

    if (!bfs::exists(dirname) && (!create))
        return false;

    /* Optionally create directories if they don't exist yet
     * Note this will do nothing if the directory and its
     * parents already exist, but will fail if the path
     * points to a file or a softlink. So it serves as a test
     * for that as well.
     */
    bfs::create_directories(dirname);

    auto d = bfs::directory_entry (dirname);
    auto perms = d.status().permissions();

    /* On Windows only write permission will be checked.
     * So strictly speaking we'd need two error messages here depending
     * on the platform. For simplicity this detail is glossed over though. */
    if ((perms & bfs::owner_all) != bfs::owner_all)
        throw (bfs::filesystem_error(
            std::string(_("Insufficient permissions, at least write and access permissions required: "))
            + dirname.c_str(), dirname,
            bst::error_code(bst::errc::permission_denied, bst::generic_category())));

    return true;
}

static bool userdata_is_home = false;
static bool userdata_is_tmp = false;
static auto userdata_home = bfs::path();
static auto gnc_userdata_home = bfs::path();

/* Will attempt to copy all files and directories from src to dest
 * Returns true if successful or false if not */
static bool
copy_recursive(const bfs::path& src, const bfs::path& dest)
{
    if (!bfs::exists(src))
        return false;

    auto old_str = src.string();
    auto old_len = old_str.size();
    try
    {
        /* Note: the for(auto elem : iterator) construct fails
         * on travis (g++ 4.8.x, boost 1.54) so I'm using
         * a traditional for loop here */
        for(auto direntry = bfs::recursive_directory_iterator(src);
            direntry != bfs::recursive_directory_iterator(); ++direntry)
        {
            auto cur_str = direntry->path().string();
            auto cur_len = cur_str.size();
            auto rel_str = std::string(cur_str, old_len, cur_len - old_len);
            auto relpath = bfs::path(rel_str).relative_path();
            auto newpath = bfs::absolute (relpath, dest);
            bfs::copy(direntry->path(), newpath);
        }
    }
    catch(const bfs::filesystem_error& ex)
    {
        g_warning("An error occured while trying to migrate the user configation from\n%s to\n%s"
                  "The reported failure is\n%s",
                  src.c_str(), gnc_userdata_home.c_str(), ex.what());
        return false;
    }

    return true;
}

#ifdef G_OS_WIN32
/* g_get_user_data_dir defaults to CSIDL_LOCAL_APPDATA, but
 * the roaming profile makes more sense.
 * So this function is a copy of glib's internal get_special_folder
 * and minimally adjusted to fetch CSIDL_APPDATA
 */
static gchar *
win32_get_userdata_home (void)
{
    wchar_t path[MAX_PATH+1];
    HRESULT hr;
    LPITEMIDLIST pidl = NULL;
    BOOL b;
    gchar *retval = NULL;

    hr = SHGetSpecialFolderLocation (NULL, CSIDL_APPDATA, &pidl);
    if (hr == S_OK)
    {
        b = SHGetPathFromIDListW (pidl, path);
        if (b)
            retval = g_utf16_to_utf8 (path, -1, NULL, NULL, NULL);
        CoTaskMemFree (pidl);
    }
    return retval;
}
#elif defined MAC_INTEGRATION
static gchar*
quarz_get_userdata_home(void)
{
    gchar *retval = NULL;
    NSFileManager*fm = [NSFileManager defaultManager];
    NSArray* appSupportDir = [fm URLsForDirectory:NSApplicationSupportDirectory
    inDomains:NSUserDomainMask];
    if ([appSupportDir count] > 0)
    {
        NSURL* dirUrl = [appSupportDir objectAtIndex:0];
        NSString* dirPath = [dirUrl path];
        retval = g_strdup([dirPath UTF8String]);
    }
    return retval;
}
#endif

static bfs::path
get_userdata_home(bool create)
{
    auto try_home_dir = true;
    gchar *data_dir = NULL;

#ifdef G_OS_WIN32
    data_dir = win32_get_userdata_home ();
#elif defined MAC_INTEGRATION
    data_dir = quarz_get_userdata_home ();
#endif

    if (data_dir)
    {
        userdata_home = data_dir;
        g_free(data_dir);
    }
    else
        userdata_home = g_get_user_data_dir();

    if (!userdata_home.empty())
    {
        try
        {
            if (!gnc_validate_directory(userdata_home, create))
                throw (bfs::filesystem_error(
                    std::string(_("Directory doesn't exist: "))
                    + userdata_home.c_str(), userdata_home,
                    bst::error_code(bst::errc::permission_denied, bst::generic_category())));
            try_home_dir = false;
        }
        catch (const bfs::filesystem_error& ex)
        {
            g_warning("%s is not a suitable base directory for the user data."
            "Trying home directory instead.\nThe failure is\n%s",
            userdata_home.c_str(), ex.what());
        }
    }

    if (try_home_dir)
    {
        userdata_home = g_get_home_dir();
        try
        {
            /* Never attempt to create a home directory, hence the false below */
            if (!gnc_validate_directory(userdata_home, false))
                throw (bfs::filesystem_error(
                    std::string(_("Directory doesn't exist: "))
                    + userdata_home.c_str(), userdata_home,
                    bst::error_code(bst::errc::permission_denied, bst::generic_category())));
            userdata_is_home = true;
        }
        catch (const bfs::filesystem_error& ex)
        {
            g_warning("Cannot find suitable home directory. Using tmp directory instead.\n"
            "The failure is\n%s", ex.what());
            userdata_home = g_get_tmp_dir();
            userdata_is_tmp = true;
        }
    }
    g_assert(!userdata_home.empty());

    return userdata_home;
}

gboolean
gnc_filepath_init(gboolean create)
{
    auto gnc_userdata_home_exists = false;
    auto gnc_userdata_home_from_env = false;
    auto gnc_userdata_home_env = g_getenv("GNC_DATA_HOME");
    if (gnc_userdata_home_env)
    {
        gnc_userdata_home = bfs::path(gnc_userdata_home_env);
        try
        {
            gnc_userdata_home_exists = bfs::exists(gnc_userdata_home);
            if (!gnc_validate_directory(gnc_userdata_home, create))
                throw (bfs::filesystem_error(
                    std::string(_("Directory doesn't exist: "))
                    + userdata_home.c_str(), userdata_home,
                    bst::error_code(bst::errc::permission_denied, bst::generic_category())));
            gnc_userdata_home_from_env = true;
        }
        catch (const bfs::filesystem_error& ex)
        {
            g_warning("%s (from environment variable 'GNC_DATA_HOME') is not a suitable directory for the user data."
            "Trying the default instead.\nThe failure is\n%s",
            gnc_userdata_home.c_str(), ex.what());
        }
    }

    if (!gnc_userdata_home_from_env)
    {
        auto userdata_home = get_userdata_home(create);
        if (userdata_is_home)
        {
            /* If we get here that means the platform
            * dependent gnc_userdata_home is not available for
            * some reason. If legacy .gnucash directory still exists,
            * use it as first fallback, but never create it (we want
            * it to go away eventually).
            * If missing, fall back to tmp_dir instead */
            gnc_userdata_home = userdata_home / ".gnucash";
            if (!bfs::exists(gnc_userdata_home))
            {
                userdata_home = g_get_tmp_dir();
                userdata_is_home = false;
            }
        }

        if (!userdata_is_home)
            gnc_userdata_home = userdata_home / PACKAGE_NAME;
        gnc_userdata_home_exists = bfs::exists(gnc_userdata_home);
        /* This may throw and end the program! */
        gnc_validate_directory(gnc_userdata_home, create);
    }

    auto migrated = FALSE;
    if (!userdata_is_home && !gnc_userdata_home_exists && create)
        migrated = copy_recursive(bfs::path (g_get_home_dir()) / ".gnucash",
                                  gnc_userdata_home);

    /* Since we're in code that is only executed once....
     * Note these calls may throw and end the program! */
    gnc_validate_directory(gnc_userdata_home / "books", (create || userdata_is_tmp));
    gnc_validate_directory(gnc_userdata_home / "checks", (create || userdata_is_tmp));
    gnc_validate_directory(gnc_userdata_home / "translog", (create || userdata_is_tmp));

    return migrated;
}

/** @fn const gchar * gnc_userdata_dir ()
 *  @brief Ensure that the user's configuration directory exists and is minimally populated.
 *
 *  Note that the default path depends on the platform.
 *  - Windows: CSIDL_APPDATA/Gnucash
 *  - OS X: $HOME/Application Support/Gnucash
 *  - Linux: $XDG_DATA_HOME/Gnucash (or the default $HOME/.local/share/Gnucash)
 *
 *  If any of these paths doesn't exist and can't be created the function will
 *  fall back to
 *  - $HOME/.gnucash
 *  - <TMP_DIR>/Gnucash
 *  (in that order)
 *
 *  @return An absolute path to the configuration directory. This string is
 *  owned by the gnc_filepath_utils code and should not be freed by the user.
 */


/* Note Don't create missing directories automatically
 * here and in the next function except if the
 * target directory is the temporary directory. This
 * should be done properly at a higher level (in the gui
 * code most likely) very early in application startup.
 * This call is just a fallback to prevent the code from
 * crashing because no directories were configured. This
 * weird construct is set up because compiling our guile
 * scripts also triggers this code and that's not the
 * right moment to start creating the necessary directories.
 * FIXME A better approach would be to have the gnc_userdata_home
 * verification/creation be part of the application code instead
 * of libgnucash. If libgnucash needs access to this directory
 * libgnucash will need some kind of initialization routine
 * that the application can call to set (among others) the proper
 * gnc_uderdata_home for libgnucash. The only other aspect to
 * consider here is how to handle this in the bindings (if they
 * need it).
 */
const gchar *
gnc_userdata_dir (void)
{
    if (gnc_userdata_home.empty())
        gnc_filepath_init(false);

    return gnc_userdata_home.c_str();
}

static const bfs::path&
gnc_userdata_dir_as_path (void)
{
    if (gnc_userdata_home.empty())
        /* Don't create missing directories automatically except
         * if the target directory is the temporary directory. This
         * should be done properly at a higher level (in the gui
         * code most likely) very early in application startup.
         * This call is just a fallback to prevent the code from
         * crashing because no directories were configured. */
        gnc_filepath_init(false);

    return gnc_userdata_home;
}

/** @fn gchar * gnc_build_userdata_path (const gchar *filename)
 *  @brief Make a path to filename in the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
 */

gchar *
gnc_build_userdata_path (const gchar *filename)
{
    return g_strdup((gnc_userdata_dir_as_path() / filename).c_str());
}

static bfs::path
gnc_build_userdata_subdir_path (const gchar *subdir, const gchar *filename)
{
    gchar* filename_dup = g_strdup(filename);

    scrub_filename(filename_dup);
    auto result = (gnc_userdata_dir_as_path() / subdir) / filename_dup;
    g_free(filename_dup);
    return result;
}

/** @fn gchar * gnc_build_book_path (const gchar *filename)
 *  @brief Make a path to filename in the book subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
 */

gchar *
gnc_build_book_path (const gchar *filename)
{
    return g_strdup(gnc_build_userdata_subdir_path("books", filename).c_str());
}

/** @fn gchar * gnc_build_translog_path (const gchar *filename)
 *  @brief Make a path to filename in the translog subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
 */

gchar *
gnc_build_translog_path (const gchar *filename)
{
    return g_strdup(gnc_build_userdata_subdir_path("translog", filename).c_str());
}

/** @fn gchar * gnc_build_data_path (const gchar *filename)
 *  @brief Make a path to filename in the data subdirectory of the user's configuration directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
 */

gchar *
gnc_build_data_path (const gchar *filename)
{
    return g_strdup(gnc_build_userdata_subdir_path("data", filename).c_str());
}

/** @fn gchar * gnc_build_report_path (const gchar *filename)
 *  @brief Make a path to filename in the report directory.
 *
 * @param filename The name of the file
 *
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
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
 *  @return An absolute path. The returned string should be freed by the user
 *  using g_free().
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
    gchar* pkgdatadir = gnc_path_get_pkgdatadir ();

    default_path = g_build_filename (pkgdatadir, "pixmaps", NULL);
    g_free(pkgdatadir);
    fullname = gnc_filepath_locate_file (default_path, name);
    g_free(default_path);

    return fullname;
}

gchar *
gnc_filepath_locate_ui_file (const gchar *name)
{
    gchar *default_path;
    gchar *fullname;
    gchar* pkgdatadir = gnc_path_get_pkgdatadir ();

    default_path = g_build_filename (pkgdatadir, "ui", NULL);
    g_free(pkgdatadir);
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

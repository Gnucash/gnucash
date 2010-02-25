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
 *
 * XXX this file does not belong in the gnucash engine; it is here
 * for the moment only because both the file backend and the app-file
 * GUI code make use of it.
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

#include "gnc-engine.h"
#include "gnc-path.h"
#include "gnc-filepath-utils.h"


static QofLogModule log_module = GNC_MOD_BACKEND;


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
    while (p) {
      *p = '_';
      p = strpbrk(filename, STRANGE_CHARS);
    }
}

/** \fn gchar * check_file_return_if_true (path)
 *  \brief Check if the path exists and is a regular file
 *
 * \param path -- freed if the path doesn't exist or isn't a regular file
 *
 *  \return NULL or the path
 */

static gchar *
check_path_return_if_valid(gchar *path)
{
    ENTER("Path: %s", path);
    if (g_file_test(path, G_FILE_TEST_IS_REGULAR))
    {
	LEAVE("found %s", path);
	return path;
    }
    g_free (path);
    return NULL;
}

char *
xaccResolveFilePath (const char * filefrag)
{
  char pathbuf[PATH_MAX];
/*  pathGenerator gens[4];*/
  char *filefrag_dup;
  int namelen;
  int i;
  gchar *fullpath = NULL, *tmp_path = NULL;

  /* seriously invalid */
  if (!filefrag)
  {
      PERR("filefrag is NULL");
      return NULL;
  }

  ENTER ("filefrag=%s", filefrag);

  /* ---------------------------------------------------- */
  /* OK, now we try to find or build an absolute file path */

  /* check for an absolute file path */
  if (g_path_is_absolute(filefrag)) {
    LEAVE("filefrag is absolute path");
    return g_strdup (filefrag);
  }

  if (!g_ascii_strncasecmp(filefrag, "file:", 5))
  {
      LEAVE("filefrag is file uri");
      if (!g_ascii_strncasecmp(filefrag, "file://", 7))
        return g_strdup(filefrag + 7);
      else
        return g_strdup(filefrag + 5);
  }
  if( g_ascii_strncasecmp( filefrag, "xml:", 4 ) == 0 ) {
  	  LEAVE( "filefrag is xml file uri" );
      if( g_ascii_strncasecmp( filefrag, "xml://", 6 ) == 0 )
        return g_strdup( filefrag + 6);
      else
	    return g_strdup( filefrag + 4);
  }


  /* get conservative on the length so that sprintf(getpid()) works ... */
  /* strlen ("/.LCK") + sprintf (%x%d) */
  namelen = strlen (filefrag) + 25;

  /* Look in the current working directory */
  tmp_path = g_get_current_dir();
  fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
  g_free(tmp_path);
  fullpath = check_path_return_if_valid(fullpath);
  if (fullpath != NULL)
  {
      LEAVE("found %s", fullpath);
      return fullpath;
  }

  /* Look in the data dir (e.g. $PREFIX/share/gnucash) */
  tmp_path = gnc_path_get_pkgdatadir();
  fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
  g_free(tmp_path);
  fullpath = check_path_return_if_valid(fullpath);
  if (fullpath != NULL)
   {
      LEAVE("found %s", fullpath);
      return fullpath;
  }

  /* Look in the config dir (e.g. $PREFIX/etc/gnucash) */
  tmp_path = gnc_path_get_accountsdir();
  fullpath = g_build_filename(tmp_path, filefrag, (gchar *)NULL);
  g_free(tmp_path);
  fullpath = check_path_return_if_valid(fullpath);
  if (fullpath != NULL)
  {
      LEAVE("found %s", fullpath);
      return fullpath;
  }

  /* Look in the users config dir (e.g. $HOME/.gnucash/data) */
  fullpath = gnc_build_data_path(filefrag);
  if (g_file_test(fullpath, G_FILE_TEST_IS_REGULAR))
  {
      LEAVE("found %s", fullpath);
      return fullpath;
  }
  /* OK, it's not there. Note that it needs to be created and pass it
   * back anyway */
  LEAVE("create new file %s", fullpath);
  return fullpath;

}

/* ====================================================================== */

char *
xaccResolveURL (const char * pathfrag)
{
  GList* list;
  GList* node;

  /* seriously invalid */
  if (!pathfrag) return NULL;

  /* At this stage of checking, URL's are always, by definition,
   * resolved.  If there's an error connecting, we'll find out later.
   *
   * FIXME -- we should probably use  ghttp_uri_validate
   * to make sure the uri is in good form.
   */

  if (!g_ascii_strncasecmp (pathfrag, "http://", 7) ||
      !g_ascii_strncasecmp (pathfrag, "https://", 8))
  {
    return g_strdup(pathfrag);
  }

  /* Check the URL against the list of registered access methods */
  list = qof_backend_get_registered_access_method_list();
  for( node = list; node != NULL; node = node->next ) {
  	const gchar* access_method = node->data;
	if( strcmp( access_method, "file" ) != 0 &&
			strcmp( access_method, "xml" ) != 0 ) {
		gchar s[30];
		sprintf( s, "%s://", access_method );
		if( !g_ascii_strncasecmp( pathfrag, s, strlen(s) ) ) {
			g_list_free(list);
    		return g_strdup(pathfrag);
		}
	}
  }
  g_list_free(list);
  return (xaccResolveFilePath (pathfrag));
}

/* ====================================================================== */

static void
gnc_validate_directory (const gchar *dirname)
{
  struct stat statbuf;
  gint rc;

  rc = g_stat (dirname, &statbuf);
  if (rc) {
    switch (errno) {
    case ENOENT:
      rc = g_mkdir (dirname, S_IRWXU);   /* perms = S_IRWXU = 0700 */
      if (rc) {
	g_fprintf(stderr,
		  _("An error occurred while creating the directory:\n"
		    "  %s\n"
		    "Please correct the problem and restart GnuCash.\n"
		    "The reported error was '%s' (errno %d).\n"),
		  dirname, strerror(errno), errno);
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
		dirname, strerror(errno), errno);
      exit(1);
    }
  }

  if ((statbuf.st_mode & S_IFDIR) != S_IFDIR) {
    g_fprintf(stderr,
	      _("The path\n"
		"  %s\n"
		"exists but it is not a directory. Please delete\n"
		"the file and start GnuCash again.\n"),
	      dirname);
    exit(1);
  }
  if ((statbuf.st_mode & S_IRWXU) != S_IRWXU) {
    g_fprintf(stderr,
	      _("The permissions are wrong on the directory\n"
		"  %s\n"
		"They must be at least 'rwx' for the user.\n"),
	      dirname);
    exit(1);
  }
}

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

gchar *
gnc_build_dotgnucash_path (const gchar *filename)
{
  return g_build_filename(gnc_dotgnucash_dir(), filename, (gchar *)NULL);
}

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

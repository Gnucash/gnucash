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
#include "gnc-filepath-utils.h"

#ifdef _MSC_VER
#include <glib/gwin32.h>
#define PATH_MAX MAXPATHLEN
#endif

static QofLogModule log_module = GNC_MOD_BACKEND;


/* ====================================================================== */
/* 
 * If $HOME/.gnucash/data directory doesn't exist, then create it.
 */

static void 
MakeHomeDir (void) 
{
  const gchar *home;
  char *path;
  char *data;

  /* Punt. Can't figure out where home is. */
  home = g_get_home_dir();
  if (!home) return;

  path = g_build_filename(home, ".gnucash", (gchar *)NULL);

  if (!g_file_test(path, G_FILE_TEST_EXISTS))
  {
    /* Go ahead and make it. Don't bother much with checking mkdir 
     * for errors; seems pointless. */
    g_mkdir (path, S_IRWXU);   /* perms = S_IRWXU = 0700 */
  }

  data = g_build_filename (path, "data", (gchar *)NULL);
  if (!g_file_test(data, G_FILE_TEST_EXISTS))
    g_mkdir (data, S_IRWXU);

  g_free (path);
  g_free (data);
}

/* ====================================================================== */

/* XXX hack alert -- we should be yanking this out of some config file */
/* These are obviously meant to be hard-coded paths to the gnucash
   data file. That is insane. These should be thrown out
   altogether. On non-Unix systems (Windows) these paths would not
   only have different directory separator characters but these
   would certainly be completely different paths. I'd vote to
   throw this out completely. -- cstim, 2006-07-19 */
static char * searchpaths[] =
{
   "/usr/share/gnucash/data",
   "/usr/local/share/gnucash/data",
   "/usr/share/gnucash/accounts",
   "/usr/local/share/gnucash/accounts",
   NULL,
};

typedef gboolean (*pathGenerator)(char *pathbuf, int which);

static gboolean
xaccCwdPathGenerator(char *pathbuf, int which)
{
    if(which != 0)
    {
        return FALSE;
    }
    else
    {
        /* try to find a file by this name in the cwd ... */
        if (getcwd (pathbuf, PATH_MAX) == NULL)
            return FALSE;

        return TRUE;
    }
}

static gboolean
xaccDataPathGenerator(char *pathbuf, int which)
{
    if(which != 0)
    {
        return FALSE;
    }
    else
    {
        const gchar *home;
	gchar *tmppath;
    
        home = g_get_home_dir ();
        if (!home)
            return FALSE;

	tmppath = g_build_filename (home, ".gnucash", "data", (gchar *)NULL);
        if (strlen(tmppath) >= PATH_MAX)
	{
	    g_free (tmppath);
	    return FALSE;
	}

        g_strlcpy (pathbuf, tmppath, PATH_MAX);
	g_free (tmppath);
        return TRUE;
    }
}

static gboolean
xaccUserPathPathGenerator(char *pathbuf, int which)
{
    char *path = NULL;
    
    if(searchpaths[which] == NULL)
    {
        return FALSE;
    }
    else
    {
        path = searchpaths[which];
        
        if (PATH_MAX <= strlen(path))
            return FALSE;

        g_strlcpy (pathbuf, path, PATH_MAX);
        return TRUE;
    }
}

/* ====================================================================== */

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

char * 
xaccResolveFilePath (const char * filefrag)
{
  char pathbuf[PATH_MAX];
  pathGenerator gens[4];
  char *filefrag_dup;
  int namelen;
  int i;

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

  gens[0] = xaccCwdPathGenerator;
  gens[1] = xaccDataPathGenerator;
  gens[2] = xaccUserPathPathGenerator;
  gens[3] = NULL;

  for (i = 0; gens[i] != NULL; i++) 
  {
      int j;
      for(j = 0; gens[i](pathbuf, j) ; j++)
      {
	  gchar *fullpath = g_build_filename(pathbuf, filefrag, (gchar *)NULL);

	  if (g_file_test(fullpath, G_FILE_TEST_IS_REGULAR))
	  {
	      LEAVE("found %s", fullpath);
	      return fullpath;
          }
	  g_free (fullpath);
      }
  }
  /* OK, we didn't find the file. */

  /* make sure that the gnucash home dir exists. */
  MakeHomeDir();

  filefrag_dup = g_strdup (filefrag);

  /* Replace "strange" chars with "_" for non-file backends. */
  if (strstr (filefrag, "://"))
  {
	scrub_filename(filefrag_dup);
  }

  /* Lets try creating a new file in $HOME/.gnucash/data */
  if (xaccDataPathGenerator(pathbuf, 0))
  {
      gchar *result;
      result = g_build_filename(pathbuf, filefrag_dup, (gchar *)NULL);
      g_free (filefrag_dup);
      LEAVE("create new file %s", result);
      return result;
  } 

  /* OK, we still didn't find the file */
  /* Lets try creating a new file in the cwd */
  if (xaccCwdPathGenerator(pathbuf, 0))
  {
      gchar *result;
      result = g_build_filename(pathbuf, filefrag_dup, (gchar *)NULL);
      g_free (filefrag_dup);
      LEAVE("create new file %s", result);
      return result;
  }

  g_free (filefrag_dup);

  LEAVE("%s not found", filefrag);
  return NULL;
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

  if (!g_ascii_strncasecmp (pathfrag, "http://", 7)      ||
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

  /* "file:" and "xml:" are handled specially */
  if (!g_ascii_strncasecmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag));
  }
  if (!g_ascii_strncasecmp (pathfrag, "xml:", 4)) {
    return (g_strdup_printf( "xml:%s", xaccResolveFilePath (pathfrag)) );
  }

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
  static gchar *dotgnucash = NULL, *tmp_dir;
  const gchar *home;

  if (dotgnucash)
    return dotgnucash;

  dotgnucash = g_strdup(g_getenv("GNC_DOT_DIR"));
  if (!dotgnucash) {
    home = g_get_home_dir();
    if (!home) {
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
  char* filename_dup = g_strdup(filename);
  char* result;

  scrub_filename(filename_dup);
  result = g_build_filename(gnc_dotgnucash_dir(), "books", filename_dup, (gchar *)NULL);
  g_free(filename_dup);
  return result;
}

/* =============================== END OF FILE ========================== */

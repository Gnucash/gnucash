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
#ifdef HAVE_GLIB26
#include <glib/gstdio.h>
#else
#define g_mkdir(a,b) mkdir(a,b)
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "gnc-engine.h"
#include "gnc-filepath-utils.h"

static QofLogModule log_module = GNC_MOD_BACKEND;


/* ====================================================================== */
/* 
 * If $HOME/.gnucash/data directory doesn't exist, then create it.
 */

static void 
MakeHomeDir (void) 
{
  int rc;
  struct stat statbuf;
  char *home;
  char *path;
  char *data;

  /* Punt. Can't figure out where home is. */
  home = getenv ("HOME");
  if (!home) return;

  path = g_strconcat(home, "/.gnucash", NULL);

  rc = stat (path, &statbuf);
  if (rc)
  {
    /* assume that the stat failed only because the dir is absent,
     * and not because its read-protected or other error.
     * Go ahead and make it. Don't bother much with checking mkdir 
     * for errors; seems pointless. */
    g_mkdir (path, S_IRWXU);   /* perms = S_IRWXU = 0700 */
  }

  data = g_strconcat (path, "/data", NULL);
  rc = stat (data, &statbuf);
  if (rc)
    g_mkdir (data, S_IRWXU);

  g_free (path);
  g_free (data);
}

/* ====================================================================== */

/* XXX hack alert -- we should be yanking this out of some config file */
static char * searchpaths[] =
{
   "/usr/share/gnucash/data/",
   "/usr/local/share/gnucash/data/",
   "/usr/share/gnucash/accounts/",
   "/usr/local/share/gnucash/accounts/",
   NULL,
};

typedef gboolean (*pathGenerator)(char *pathbuf, int which);

static gboolean
xaccAddEndPath(char *pathbuf, const char *ending, int len)
{
    if(len + strlen(pathbuf) >= PATH_MAX)
        return FALSE;
          
    strcat (pathbuf, ending);
    return TRUE;
}

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

        strcat (pathbuf, "/");
        return TRUE;
    }
}

static gboolean
xaccDataPathGenerator(char *pathbuf, int which)
{
    char *path;
    
    if(which != 0)
    {
        return FALSE;
    }
    else
    {
        path = getenv ("HOME");
        if (!path)
            return FALSE;

        if (PATH_MAX <= (strlen (path) + 20))
            return FALSE;

        strcpy (pathbuf, path);
        strcat (pathbuf, "/.gnucash/data/");
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

        strcpy (pathbuf, path);
        return TRUE;
    }
}

/* ====================================================================== */

char * 
xaccResolveFilePath (const char * filefrag)
{
  struct stat statbuf;
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
  if (*filefrag == '/')
    return g_strdup (filefrag);

  if (!g_ascii_strncasecmp(filefrag, "file:", 5))
  {
      char *ret = g_new(char, strlen(filefrag) - 5 + 1);
      strcpy(ret, filefrag + 5);
      return ret;
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
          if(xaccAddEndPath(pathbuf, filefrag, namelen))
          {
              int rc = stat (pathbuf, &statbuf);
              if ((!rc) && (S_ISREG(statbuf.st_mode)))
              {
                  return (g_strdup (pathbuf));
              }
          }
      }
  }
  /* OK, we didn't find the file. */

  /* make sure that the gnucash home dir exists. */
  MakeHomeDir();

  filefrag_dup = g_strdup (filefrag);

  /* Replace '/' with ',' for non file backends */
  if (strstr (filefrag, "://"))
  {
    char *p;

    p = strchr (filefrag_dup, '/');
    while (p) {
      *p = ',';
      p = strchr (filefrag_dup, '/');
    }
  }

  /* Lets try creating a new file in $HOME/.gnucash/data */
  if (xaccDataPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag_dup, namelen))
      {
          g_free (filefrag_dup);
          return (g_strdup (pathbuf));
      }
  } 

  /* OK, we still didn't find the file */
  /* Lets try creating a new file in the cwd */
  if (xaccCwdPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag_dup, namelen))
      {
          g_free (filefrag_dup);
          return (g_strdup (pathbuf));
      }
  }

  g_free (filefrag_dup);

  return NULL;
}

/* ====================================================================== */

char * 
xaccResolveURL (const char * pathfrag)
{
  /* seriously invalid */
  if (!pathfrag) return NULL;

  /* At this stage of checking, URL's are always, by definition,
   * resolved.  If there's an error connecting, we'll find out later.
   *
   * FIXME -- we should probably use  ghttp_uri_validate
   * to make sure the uri is in good form.
   */

  if (!g_ascii_strncasecmp (pathfrag, "http://", 7)      ||
      !g_ascii_strncasecmp (pathfrag, "https://", 8)     ||
      !g_ascii_strncasecmp (pathfrag, "postgres://", 11))
  {
    return g_strdup(pathfrag);
  }

  if (!g_ascii_strncasecmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag));
  }

  return (xaccResolveFilePath (pathfrag));
}

/* ====================================================================== */

static void
gnc_validate_directory (const gchar *dirname)
{
  struct stat statbuf;
  gint rc;

  rc = stat (dirname, &statbuf);
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
      stat (dirname, &statbuf);
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
  static gchar *dotgnucash = NULL, *books_dir;
  const gchar *home;

  if (dotgnucash)
    return dotgnucash;

  home = g_get_home_dir();
  if (!home) {
    g_warning("Cannot find home directory. Using tmp directory instead.");
    home = g_get_tmp_dir();
  }
  g_assert(home);

  dotgnucash = g_build_filename(home, ".gnucash", (gchar *)NULL);
  gnc_validate_directory(dotgnucash);

  /* Since we're in code that is only executed once.... */
  books_dir = g_build_filename(dotgnucash, "books", (gchar *)NULL);
  gnc_validate_directory(books_dir);
  g_free(books_dir);

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
  return g_build_filename(gnc_dotgnucash_dir(), "books", filename, (gchar *)NULL);
}

/* =============================== END OF FILE ========================== */

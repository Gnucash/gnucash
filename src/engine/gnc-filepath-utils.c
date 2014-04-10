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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
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
    mkdir (path, S_IRWXU);   /* perms = S_IRWXU = 0700 */
  }

  data = g_strconcat (path, "/data", NULL);
  rc = stat (data, &statbuf);
  if (rc)
    mkdir (data, S_IRWXU);

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

  if (!g_strncasecmp(filefrag, "file:", 5))
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

  if (!g_strncasecmp (pathfrag, "http://", 7)      ||
      !g_strncasecmp (pathfrag, "https://", 8)     ||
      !g_strncasecmp (pathfrag, "postgres://", 11) ||
      !g_strncasecmp (pathfrag, "rpc://", 6))
  {
    return g_strdup(pathfrag);
  }

  if (!g_strncasecmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag));
  }

  return (xaccResolveFilePath (pathfrag));
}

/* =============================== END OF FILE ========================== */

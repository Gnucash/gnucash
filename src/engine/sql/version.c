/********************************************************************\
 * version.c -- handle backward compatibility issues                *
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
handle versions
*/

#define _GNU_SOURCE

#include "config.h"

#include <libpq-fe.h> 

#include "Backend.h"
#include "BackendP.h"

#include "putil.h"

static short module = MOD_BACKEND; 

/* ============================================================= */

#define PGEND_CURRENT_MAJOR_VERSION 1

/* ============================================================= */

static gpointer
version_table_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   return TRUE;
}


static void
pgendVersionTable (PGBackend *be)
{
   char *p;
   gboolean table_exists = FALSE;
   
   /* First, see if we can find the table that stores 
    * the version info. */

   p = "SELECT tablename FROM pg_tables WHERE tablename='gncVersion';";
   SEND_QUERY (be,p, );
   table_exists = pgendGetResults (be, version_table_cb, FALSE);
   
   if (table_exists) return;

   /* create the table if it doesn't exist */
   p = "CREATE TABLE gncVersion (\n"
       "  major    INT NOT NULL,\n"
       "  minor    INT NOT NULL,\n"
       "  rev      INT DEFAULT '0',\n"
       "  name     TEXT UNIQUE NOT NULL CHECK (name <> '')\n"
       ");\n"
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,0,0,'Version Table');";
   SEND_QUERY (be,p, );
}

/* ============================================================= */

typedef struct {
   int major;
   int minor;
   int rev;
} pgendVersion;

static gpointer
version_version_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   pgendVersion *v = (pgendVersion *) data;
   v->major = atoi(DB_GET_VAL ("major", j));
   v->minor = atoi(DB_GET_VAL ("minor", j));
   v->rev = atoi(DB_GET_VAL ("rev", j));
   return NULL;
}


static pgendVersion
pgendGetVersion (PGBackend *be)
{
   pgendVersion vers;
   
   p = "SELECT major,minor,rev FROM gncVersion ORDER BY "
       " major DESC, minor DESC, rev DESC LIMIT 1;";
   SEND_QUERY (be,p, );
   vers.major = 0;
   pgendGetResults (be, version_version_cb, &vers);
   return vers;
}

/* ============================================================= */

void
pgendForwardVersion (PGBackend *be)
{
   pgendVersion vers;
   
   pgendVersionTable(be);
   
   vers = pgendGetVersion(be);

   if (1 > vers.major)
   {
      PERR ("something in the database is broken\n");
      return;
   }
   
   /* check to see if this client can connect */
   if (PGEND_CURRENT_MAJOR_VERSION < vers.major)
   {
      PINFO ("you need a newer gnucash client to connect "
	     "to this database");
      return;
   }

   /* start adding features to bring database up to date */
   if (1 == vers.major)
   {
      /* version 1.1.0 add iguids to transaction and entry tables */
      if (1> vers.minor)
      {
      }
   }

}

/* ======================== END OF FILE ======================== */

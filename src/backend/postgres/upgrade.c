/********************************************************************\
 * upgrade.c -- handle back-ward compatible database table upgrades *
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>               *
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
		  

#define _GNU_SOURCE

#include "config.h"

#include <libpq-fe.h> 

#include "PostgresBackend.h"
#include "upgrade.h"

#include "putil.h"

static short module = MOD_BACKEND; 

/* ============================================================= */

#define PGEND_CURRENT_MAJOR_VERSION  1
#define PGEND_CURRENT_MINOR_VERSION  4
#define PGEND_CURRENT_REV_VERSION    1

/* ============================================================= */
/* see if the version table exists, if not, create it */

static gpointer
version_table_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   return (gpointer) TRUE;
}

static void
pgendVersionTable (PGBackend *be)
{
   char *p;
   gboolean table_exists = FALSE;
   
   /* First, see if we can find the table that stores 
    * the version info. */

   p = "SELECT tablename FROM pg_tables WHERE tablename='gncversion';";
   SEND_QUERY (be,p, );
   table_exists = (gboolean) pgendGetResults (be, version_table_cb, FALSE);
   
   if (table_exists) return;

   /* create the table if it doesn't exist */
   p = "CREATE TABLE gncVersion (\n"
       "  major    INT NOT NULL,\n"
       "  minor    INT NOT NULL,\n"
       "  rev      INT DEFAULT '0',\n"
       "  name     TEXT UNIQUE NOT NULL CHECK (name <> ''),\n"
       "  date     DATETIME DEFAULT 'NOW' \n"
       ");\n"
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,0,0,'Version Table');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */
/* get the highest installed version number */

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
   char * p;
   pgendVersion vers;
   
   vers.major = 0;
   vers.minor = 0;
   vers.rev = 0;
   p = "SELECT major,minor,rev FROM gncVersion ORDER BY "
       " major DESC, minor DESC, rev DESC LIMIT 1;";
   SEND_QUERY (be,p, vers);
   pgendGetResults (be, version_version_cb, &vers);
   return vers;
}

/* ============================================================= */
/* move iguids to the tables */

static gpointer
get_iguid_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   int fin = atoi(DB_GET_VAL ("iguid", j));
   return (gpointer) fin;
}



static void 
put_iguid_in_tables (PGBackend *be)
{
   char *p, buff[200];
   guint iguid;
	
   p = "LOCK TABLE gncAccount IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncEntry IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncTransaction IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncKVPValue IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n"
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,1,0,'Start Put iGUID in Main Tables');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   p = "SELECT iguid FROM gncGUIDCache ORDER BY iguid DESC LIMIT 1;";
   SEND_QUERY (be,p, );
   iguid = (guint32) pgendGetResults (be, get_iguid_cb, 0);
   iguid ++;

   sprintf(buff, "CREATE SEQUENCE gnc_iguid_seq START %d;", iguid);
   SEND_QUERY (be,buff, );
   FINISH_QUERY(be->connection);

   p = "ALTER TABLE gncEntry ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncEntry SET iguid = 0;\n" 
       
       "UPDATE gncEntry SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncEntry.entryGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

       "ALTER TABLE gncEntryTrail ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncEntryTrail SET iguid = 0;\n" 
       
       "UPDATE gncEntryTrail SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncEntryTrail.entryGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
   
   p = "ALTER TABLE gncTransaction ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncTransaction SET iguid = 0;\n" 
       
       "UPDATE gncTransaction SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncTransaction.transGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

       "ALTER TABLE gncTransactionTrail ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncTransactionTrail SET iguid = 0;\n" 
       
       "UPDATE gncTransactionTrail SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncTransactionTrail.transGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
	   
   p = "ALTER TABLE gncAccount ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncAccount SET iguid = 0;\n" 
       
       "UPDATE gncAccount SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncAccount.accountGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

       "ALTER TABLE gncAccountTrail ADD COLUMN iguid INT4 DEFAULT 0;\n"
       "UPDATE gncAccountTrail SET iguid = 0;\n" 
       
       "UPDATE gncAccountTrail SET iguid = gncGUIDCache.iguid "
       " FROM gncGUIDCache, gncKVPValue "
       " WHERE gncGUIDCache.guid = gncAccountTrail.accountGUID "
       " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   p = "DROP TABLE gncGUIDCache; \n"
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,1,1,'End Put iGUID in Main Tables');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */

static void 
fix_reconciled_balance_func (PGBackend *be)
{
   char *p;

   p = "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n "
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,2,0,'Start Fix gncSubtotalReconedBalance');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   p = "DROP FUNCTION "
       "gncSubtotalReconedBalance (CHAR(32), DATETIME, DATETIME);";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   p = "CREATE FUNCTION "
       "gncSubtotalReconedBalance (CHAR(32), DATETIME, DATETIME)"
         "RETURNS INT8 "
         "AS 'SELECT INT8(sum(gncEntry.amount)) "
           "FROM gncEntry, gncTransaction "
           "WHERE "
           "gncEntry.accountGuid = $1 AND "
           "gncEntry.transGuid = gncTransaction.transGuid AND "
           "gncTransaction.date_posted BETWEEN $2 AND $3 AND "
           "(gncEntry.reconciled = \\'y\\' OR "
           " gncEntry.reconciled = \\'f\\')' "
         "LANGUAGE 'sql';";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,2,1,'End Fix gncSubtotalReconedBalance');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */

static void
add_kvp_timespec_tables (PGBackend *be)
{
  char *p;

  p = "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n "
      "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
      " (1,3,0,'Start Add kvp_timespec tables');";
  SEND_QUERY (be,p, );
  FINISH_QUERY(be->connection);

  p = "CREATE TABLE gncKVPvalue_timespec ( "
      "  data		DATETIME "
      ") INHERITS (gncKVPvalue);";
  SEND_QUERY (be,p, );
  FINISH_QUERY(be->connection);

  p = "CREATE TABLE gncKVPvalue_timespecTrail ( "
      "  iguid		INT4, "
      "  ipath		INT4, "
      "  type		char(4), "
      "  data		DATETIME "
      ") INHERITS (gncAuditTrail);";
  SEND_QUERY (be,p, );
  FINISH_QUERY(be->connection);

  p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
      " (1,3,1,'End Add kvp_timespec tables');";
  SEND_QUERY (be,p, );
  FINISH_QUERY(be->connection);
}

/* ============================================================= */

static void
add_multiple_book_support (PGBackend *be)
{
   char buff[4000];
   char *p;
 
   p = "LOCK TABLE gncAccount IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncAccountTrail IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncPrice IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncPriceTrail IN ACCESS EXCLUSIVE MODE;\n"
       "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n"
       "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,4,0,'Start Add multiple book support');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
 
   p = "CREATE TABLE gncBook (  \n"
       " bookGuid        CHAR(32) PRIMARY KEY, \n"
       " book_open       CHAR DEFAULT 'n', \n"
       " version         INT4 NOT NULL, \n"
       " iguid           INT4 DEFAULT 0 \n"
       ");";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
 
   p = "CREATE TABLE gncBookTrail ( \n"
       " bookGuid        CHAR(32) NOT NULL, \n"
       " book_open       CHAR DEFAULT 'n', \n"
       " version         INT4 NOT NULL, \n"
       " iguid           INT4 DEFAULT 0 \n"
       ") INHERITS (gncAuditTrail); \n\n"
       "CREATE INDEX gncBookTrail_book_idx ON gncBookTrail (bookGuid);" ;
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
 
   p = "ALTER TABLE gncAccount ADD COLUMN bookGuid CHAR(32) NOT NULL;\n"
       "ALTER TABLE gncAccountTrail ADD COLUMN bookGuid CHAR(32) NOT NULL;\n"
       "ALTER TABLE gncPrice ADD COLUMN bookGuid CHAR(32) NOT NULL;\n"
       "ALTER TABLE gncPriceTrail ADD COLUMN bookGuid CHAR(32) NOT NULL;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
 
   p = buff;
   p = stpcpy (p, "UPDATE gncAccount SET bookGuid = '");
   p = guid_to_string_buff (gnc_book_get_guid (be->book), p);
   p = stpcpy (p, "';\n");
   p = stpcpy (p, "UPDATE gncAccountTrail SET bookGuid = '");
   p = guid_to_string_buff (gnc_book_get_guid (be->book), p);
   p = stpcpy (p, "';\n");
   SEND_QUERY (be,buff, );
   FINISH_QUERY(be->connection);

   p = buff;
   p = stpcpy (p, "UPDATE gncPrice SET bookGuid = '");
   p = guid_to_string_buff (gnc_book_get_guid (be->book), p);
   p = stpcpy (p, "';\n");
   p = stpcpy (p, "UPDATE gncPriceTrail SET bookGuid = '");
   p = guid_to_string_buff (gnc_book_get_guid (be->book), p);
   p = stpcpy (p, "';\n");
   SEND_QUERY (be,buff, );
   FINISH_QUERY(be->connection);

   p = buff;
   p = stpcpy (p, "INSERT INTO gncBook (bookGuid, book_open, version, iguid) "
                  "VALUES ('");
   p = guid_to_string_buff (gnc_book_get_guid (be->book), p);
   p = stpcpy (p, "', 'y', 1, 0);");
   SEND_QUERY (be,buff, );
   FINISH_QUERY(be->connection);

   p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
       " (1,4,1,'End Add multiple book support');";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */
/* Are we up to date ? */
/* Return 0 if we are at db version. Return +1 if we are newer.
 * Return -1 if we are older and so we can't run.
 */

int
pgendDBVersionIsCurrent (PGBackend *be)
{
   pgendVersion vers;

   pgendVersionTable(be);
   vers = pgendGetVersion(be);

   if (1 > vers.major)
   {
      xaccBackendSetError (&be->be, ERR_BACKEND_DATA_CORRUPT);
      return -1;
   }

   if ((PGEND_CURRENT_MAJOR_VERSION == vers.major) &&
       (PGEND_CURRENT_MINOR_VERSION <= vers.minor)) return 0;

   /* check to see if this client can connect */
   if (PGEND_CURRENT_MAJOR_VERSION < vers.major)
   {
      xaccBackendSetError (&be->be, ERR_BACKEND_TOO_NEW);
      return -1;
   }

   return +1;
}

/* ============================================================= */

void
pgendUpgradeDB (PGBackend *be)
{
   pgendVersion vers;

   vers = pgendGetVersion(be);

   /* start adding features to bring database up to date */
   if (1 == vers.major)
   {
      /* version 1.1.0 add iguids to transaction and entry tables */
      if (1 > vers.minor)
      {
         put_iguid_in_tables(be);
      }
      if (2 > vers.minor)
      {
        fix_reconciled_balance_func (be);
      }
      if (3 > vers.minor)
      {
        add_kvp_timespec_tables (be);
      }
      if (4 > vers.minor)
      {
        add_multiple_book_support (be);
      }
   }
}

/* ======================== END OF FILE ======================== */

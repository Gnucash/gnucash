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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <libpq-fe.h>
#include <stdlib.h>

#include "PostgresBackend.h"
#include "qof.h"
#include "upgrade.h"

#include "putil.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

/* ============================================================= */

#define PGEND_CURRENT_MAJOR_VERSION  1
#define PGEND_CURRENT_MINOR_VERSION  5
#define PGEND_CURRENT_REV_VERSION    1

/* ============================================================= */
/* see if the version table exists, if not, create it */

static gpointer
version_table_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    return GINT_TO_POINTER (TRUE);
}

static void
pgendVersionTable (PGBackend *be)
{
    char *p;
    gboolean table_exists = FALSE;

    /* First, see if we can find the table that stores
     * the version info. */

    p = "SELECT tablename FROM pg_tables WHERE tablename='gncversion';";
    SEND_QUERY (be, p, );
    table_exists = GPOINTER_TO_INT (pgendGetResults (be, version_table_cb,
                                    FALSE));

    if (table_exists) return;

    /* create the table if it doesn't exist */
    p = "CREATE TABLE gncVersion (\n"
        "  major    INT NOT NULL,\n"
        "  minor    INT NOT NULL,\n"
        "  rev      INT DEFAULT '0',\n"
        "  name     TEXT UNIQUE NOT NULL CHECK (name <> ''),\n"
        "  date     TIMESTAMP DEFAULT 'NOW' \n"
        ");\n"
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,0,0,'Version Table');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
}

/* ============================================================= */
/* get the highest installed version number */

typedef struct
{
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
    SEND_QUERY (be, p, vers);
    pgendGetResults (be, version_version_cb, &vers);
    return vers;
}

/* ============================================================= */
/* move iguids to the tables */

static gpointer
get_iguid_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    int fin = atoi(DB_GET_VAL ("iguid", j));
    return GINT_TO_POINTER(fin);
}



static void
put_iguid_in_tables (PGBackend *be)
{
    char *p, buff[200];
    guint iguid;

    execQuery(be, "BEGIN");
    p = "LOCK TABLE gncAccount IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncEntry IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncTransaction IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncKVPValue IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n"
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,1,0,'Start Put iGUID in Main Tables');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "SELECT iguid FROM gncGUIDCache ORDER BY iguid DESC LIMIT 1;";
    SEND_QUERY (be, p, );
    iguid = (guint32) GPOINTER_TO_UINT(pgendGetResults (be, get_iguid_cb, 0));
    iguid ++;

    sprintf(buff, "CREATE SEQUENCE gnc_iguid_seq START %d;", iguid);
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    p = "ALTER TABLE gncEntry ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncEntry ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncEntry SET iguid = 0;\n"

        "UPDATE gncEntry SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncEntry.entryGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

        "ALTER TABLE gncEntryTrail ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncEntryTrail ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncEntryTrail SET iguid = 0;\n"

        "UPDATE gncEntryTrail SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncEntryTrail.entryGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "ALTER TABLE gncTransaction ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncTransaction ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncTransaction SET iguid = 0;\n"

        "UPDATE gncTransaction SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncTransaction.transGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

        "ALTER TABLE gncTransactionTrail ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncTransactionTrail ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncTransactionTrail SET iguid = 0;\n"

        "UPDATE gncTransactionTrail SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncTransactionTrail.transGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "ALTER TABLE gncAccount ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncAccount ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncAccount SET iguid = 0;\n"

        "UPDATE gncAccount SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncAccount.accountGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n"

        "ALTER TABLE gncAccountTrail ADD COLUMN iguid INT4;\n"
        "ALTER TABLE gncAccountTrail ALTER COLUMN iguid set DEFAULT 0;\n"
        "UPDATE gncAccountTrail SET iguid = 0;\n"

        "UPDATE gncAccountTrail SET iguid = gncGUIDCache.iguid "
        " FROM gncGUIDCache, gncKVPValue "
        " WHERE gncGUIDCache.guid = gncAccountTrail.accountGUID "
        " AND gncGUIDCache.iguid = gncKVPValue.iguid;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "DROP TABLE gncGUIDCache; \n"
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,1,1,'End Put iGUID in Main Tables');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    execQuery(be, "COMMIT");
}

/* ============================================================= */

static void
fix_reconciled_balance_func (PGBackend *be)
{
    char *p;

    execQuery(be, "BEGIN");

    p = "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n "
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,2,0,'Start Fix gncSubtotalReconedBalance');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "DROP FUNCTION "
        "gncSubtotalReconedBalance (CHAR(32), TIMESTAMP, TIMESTAMP);";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "CREATE FUNCTION "
        "gncSubtotalReconedBalance (CHAR(32), TIMESTAMP, TIMESTAMP)"
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
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,2,1,'End Fix gncSubtotalReconedBalance');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
    execQuery(be, "COMMIT");
}

/* ============================================================= */

static void
add_kvp_timespec_tables (PGBackend *be)
{
    char *p;

    execQuery(be, "BEGIN");

    p = "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n "
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,3,0,'Start Add kvp_timespec tables');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "CREATE TABLE gncKVPvalue_timespec ( "
        "  data		TIMESTAMP "
        ") INHERITS (gncKVPvalue);";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "CREATE TABLE gncKVPvalue_timespecTrail ( "
        "  iguid		INT4, "
        "  ipath		INT4, "
        "  type		char(4), "
        "  data		TIMESTAMP "
        ") INHERITS (gncAuditTrail);";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,3,1,'End Add kvp_timespec tables');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    execQuery(be, "COMMIT");
}

/* ============================================================= */

static void
add_multiple_book_support (PGBackend *be)
{
    gchar *buff;
    gchar *p;
    const gchar *guid;

    execQuery(be, "BEGIN");

    p = "LOCK TABLE gncAccount IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncAccountTrail IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncPrice IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncPriceTrail IN ACCESS EXCLUSIVE MODE;\n"
        "LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n"
        "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,4,0,'Start Add multiple book support');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "CREATE TABLE gncBook (  \n"
        " bookGuid        CHAR(32) PRIMARY KEY, \n"
        " book_open       CHAR DEFAULT 'n', \n"
        " version         INT4 NOT NULL, \n"
        " iguid           INT4 DEFAULT 0 \n"
        ");";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "CREATE TABLE gncBookTrail ( \n"
        " bookGuid        CHAR(32) NOT NULL, \n"
        " book_open       CHAR DEFAULT 'n', \n"
        " version         INT4 NOT NULL, \n"
        " iguid           INT4 DEFAULT 0 \n"
        ") INHERITS (gncAuditTrail); \n\n"
        "CREATE INDEX gncBookTrail_book_idx ON gncBookTrail (bookGuid);" ;
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "ALTER TABLE gncAccount ADD COLUMN bookGuid CHAR(32);\n"
        "ALTER TABLE gncAccountTrail ADD COLUMN bookGuid CHAR(32);\n"
        "ALTER TABLE gncPrice ADD COLUMN bookGuid CHAR(32);\n"
        "ALTER TABLE gncPriceTrail ADD COLUMN bookGuid CHAR(32);\n";

    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    guid = guid_to_string(qof_book_get_guid(pgendGetBook(be)));
    PINFO("guid = %s", guid);

    buff = g_strdup_printf("UPDATE gncAccount SET bookGuid = '%s';\n"
                           "UPDATE gncAccountTrail SET bookGuid = '%s';\n"
                           "UPDATE gncPrice SET bookGuid = '%s';\n"
                           "UPDATE gncPriceTrail SET bookGuid = '%s';\n",
                           guid, guid, guid, guid);

    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    g_free(buff);

    buff = g_strdup_printf("INSERT INTO gncBook (bookGuid, book_open, version, iguid) "
                           "VALUES ('%s', 'y', 1, 0);", guid);
    SEND_QUERY (be, buff, );
    FINISH_QUERY(be->connection);

    g_free(buff);

    p = "ALTER TABLE gncAccount ALTER COLUMN bookGuid SET NOT NULL;\n"
        "ALTER TABLE gncAccountTrail ALTER COLUMN bookGuid SET NOT NULL;\n"
        "ALTER TABLE gncPrice ALTER COLUMN bookGuid SET NOT NULL;\n"
        "ALTER TABLE gncPriceTrail ALTER COLUMN bookGuid SET NOT NULL;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    p = "INSERT INTO gncVersion (major,minor,rev,name) VALUES \n"
        " (1,4,1,'End Add multiple book support');";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);

    execQuery(be, "COMMIT");
}

static void
add_timezone_support(PGBackend *be)
{
#include "newtables.h"

    ENTER(" ");

    if (!be)
    {
        LEAVE("Backend is (null)");
        return;
    }
    if (!be->connection)
    {
        qof_backend_set_message(&be->be, _("Backend connection is not available"));
        qof_backend_set_error(&be->be, ERR_BACKEND_CONN_LOST);
        return;
    }

    if (execQuery(be, "BEGIN WORK;\n") != PGRES_COMMAND_OK)
    {
        LEAVE("Failed at BEGIN WORK (1)");
        return;
    }

    /* Drop the _old tables if the exist already from a previous
     * upgrage attempt, although
     * the ROLLBACKs ought to ensure that they don't
     */
    execQuery(be, drop_old_tables);
    execQuery(be, "COMMIT");

    /* Clear backend error messages.  Use _get_error, because
     * _set_error won't clear the backend error message if it
     * is already set.
     */
    qof_backend_set_message(&be->be, NULL);
    qof_backend_get_error(&be->be);

    /* execQuery sets the backend error message if one occurs */
    if (execQuery(be, "BEGIN WORK") != PGRES_COMMAND_OK)
    {
        LEAVE("Failed at BEGIN WORK (2)");
        return;
    }

    if (execQuery(be, lock_tables) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at lock tables");
        return;
    }
    if (execQuery(be, lock_entry) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at lock entry tables");
        return;
    }

    if (execQuery(be, drop_index) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at drop indexes");
        return;
    }
    if (execQuery(be, drop_functions) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at drop functions");
        return;
    }
    if (execQuery(be, alter_tables) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at alter tables");
        return;
    }

    if (execQuery(be, create_new_tables) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at create new tables");
        return;
    }
    if (execQuery(be, create_audits) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at create audit tables");
        return;
    }
    if (execQuery(be, create_indexes) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at create indexes");
        return;
    }
    if (execQuery(be, create_functions) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at create functions");
        return;
    }

    if (execQuery(be, lock_tables) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at lock tables (2)");
        return;
    }
    if (execQuery(be, lock_split) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at lock split");
        return;
    }

    if (execQuery(be, insert_new_data) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at insert new data");
        return;
    }
    if (execQuery(be, version_sql) != PGRES_COMMAND_OK)
    {
        execQuery(be, "ROLLBACK");
        LEAVE("Failed at insert version row");
        return;
    }

    /* Everything worked thus far, commit it now! */
    execQuery(be, "COMMIT WORK");

    /* Clean up crud, Drop the _old tables, vacuum to
     * bring the indexes into use.
     */
    execQuery(be, "BEGIN WORK");
    execQuery(be, drop_old_tables);
    execQuery(be, "COMMIT WORK");
    execQuery(be, "VACUUM FULL ANALYZE");

    /* Clear backend error messages.  Use _get_error, because
     * _set_error won't clear the backend error message if it
     * is already set.
     *
     * Cleanup failures don't necessarily mean we should error
     * out, but it would be handy to have an informational
     * message to pass back to the user...
     */
    qof_backend_set_message(&be->be, NULL);
    qof_backend_get_error(&be->be);

    LEAVE("Success");

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
        qof_backend_set_error (&be->be, ERR_BACKEND_DATA_CORRUPT);
        return -1;
    }

    if ((PGEND_CURRENT_MAJOR_VERSION == vers.major) &&
            (PGEND_CURRENT_MINOR_VERSION <= vers.minor)) return 0;

    /* check to see if this client can connect */
    if (PGEND_CURRENT_MAJOR_VERSION < vers.major)
    {
        qof_backend_set_error (&be->be, ERR_BACKEND_TOO_NEW);
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
    if (vers.major == 1)
    {
        /* version 1.1.0 add iguids to transaction and entry tables */
        if (vers.minor < 1)
        {
            put_iguid_in_tables(be);
        }
        if (vers.minor < 2)
        {
            fix_reconciled_balance_func (be);
        }
        if (vers.minor < 3)
        {
            add_kvp_timespec_tables (be);
        }
        if (vers.minor < 4)
        {
            add_multiple_book_support (be);
        }
        if (vers.minor < 5)
        {
            add_timezone_support(be);
        }
    }
}

/* ======================== END OF FILE ======================== */

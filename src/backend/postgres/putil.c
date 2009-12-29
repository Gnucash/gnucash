/********************************************************************\
 * putil.c -- utility macros for the postgres backend               *
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
 * FILE:
 * putil.c
 *
 * FUNCTION:
 * Postgres backend utility macros
 *
 * HISTORY:
 * Copyright (c) 2002 Matthew Vanecek <mevanecek@yahoo.com>
 *
 */

#include "config.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <libpq-fe.h>
#include <stdlib.h>
#include <string.h>

#include "qof.h"
#include "PostgresBackend.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#include "putil.h"

ExecStatusType execQuery(PGBackend *be, const char * q)
{
    PGresult * result;
    ExecStatusType status;
    gchar * msg;

    ENTER(" ");

    if (!be || !be->connection)
    {
        LEAVE("Backend or connection is not available");
        qof_backend_set_message(&be->be, _("Backend connection is not available"));
        qof_backend_set_error(&be->be, ERR_BACKEND_CONN_LOST);
        return -1;
    }

    result = PQexec(be->connection, q);

    if (!result)
    {
        PINFO("Query could not be executed");
        qof_backend_set_message(&be->be, _("Query could not be executed"));
        qof_backend_set_error(&be->be, ERR_BACKEND_SERVER_ERR);
        return -1;
    }

    status = PQresultStatus(result);
    msg = (gchar *)PQresultErrorMessage(result);
    PINFO("Result status: %s/%s",
          PQresStatus(status), (strlen(msg)) > 0 ? msg : "(No Message)");
    PINFO("Number of rows affected: %d", atoi(PQcmdTuples(result)));

    if (status != PGRES_COMMAND_OK)
    {
        PINFO("Query causing error: %s", q);
        qof_backend_set_message(&be->be, _("From the Postgresql Server: %s"), msg);
        qof_backend_set_error(&be->be, ERR_BACKEND_SERVER_ERR);
    }

    PQclear(result);
    return status;
}


/* ============================================================= */
/* The sendQuery function sends the sql statement off to the server.
 * It performs a minimal check to see that the send succeeded. The
 * return value indicates success or failure of the send.
 */
int sendQuery(PGBackend *be, char * buff)
{
    int rc = 0;

    ENTER(" ");
    if (NULL == be->connection) return ERR_BACKEND_CONN_LOST;
    PINFO("Connectionn is %p", be->connection);
    PINFO ("sending query %s", buff);
    rc = PQsendQuery (be->connection, buff);
    if (!rc)
    {
        gchar * msg = (gchar *)PQerrorMessage(be->connection);
        PERR("send query failed:\n"
             "\t%s", msg);
        qof_backend_set_message(&be->be, _("From the Postgresql Server: %s"), msg);
        qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
        return ERR_BACKEND_SERVER_ERR;
    }
    LEAVE("PQsendQuery rc = %d", rc);
    return ERR_BACKEND_NO_ERR;
}

/* --------------------------------------------------------------- */
/* The finishQuery function makes sure that the previously sent
 * query completed with no errors.  It assumes that the query
 * does not produce any results; if it did those results are
 * discarded (only error conditions are checked for).  The number of
 * rows affected by the query is returned.
 */

int finishQuery(PGBackend *be)
{
    int i = 0;
    PGresult *result;

    ENTER(" ");
    /* complete/commit the transaction, check the status */
    PINFO("Connection is %p", be->connection);
    do
    {
        int x;
        ExecStatusType status;
        result = PQgetResult(be->connection);
        if (!result)
        {
            PINFO("Result is (null)");
            break;
        }

        status = PQresultStatus(result);
        x = atoi(PQcmdTuples(result));
        PINFO("Result status: %s, rows affected: %d, by %s",
              PQresStatus(status), x, PQcmdStatus(result));
        i += x;

        if (PGRES_COMMAND_OK != status)
        {
            gchar * msg = (gchar *)PQerrorMessage(be->connection);
            PERR("finish query failed:\n\t%s", msg);
            PQclear(result);
            qof_backend_set_message(&be->be, _("From the Postgresql Server: %s"), msg);
            qof_backend_set_error (&be->be, ERR_BACKEND_SERVER_ERR);
            break;
        }
        PQclear(result);
    }
    while (result);

    LEAVE("%d rows affected by SQL statement", i);
    return i;
}

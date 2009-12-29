/********************************************************************	\
 * events.c -- implements event handling for postgres backend       *
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

#include <libpq-fe.h>
#include <stdlib.h>

#include "events.h"
#include "gnc-engine.h"
#include "Transaction.h"
#include "PostgresBackend.h"
#include "account.h"
#include "putil.h"
#include "txn.h"

static QofLogModule log_module = "gnucash-postgres-event";


/* ============================================================= */
/* ============================================================= */
/*              EVENT NOTIFICATION HANDLER                       */
/* ============================================================= */
/* ============================================================= */

gboolean
pgendEventsPending (QofBackend *bend)
{
    PGBackend *be = (PGBackend *) bend;
    PGnotify *note;
    int rc;

    if (!be) return FALSE;
    ENTER ("mypid=%d", be->my_pid);

    /* No need to handle events in single-modes */
    if ((MODE_SINGLE_UPDATE == be->session_mode) ||
            (MODE_SINGLE_FILE == be->session_mode))
    {
        return FALSE;
    }

    /* consolidate multiple event notifications */
    rc = PQconsumeInput (be->connection);
    if (1 != rc)
    {
        PERR ("consume input failed: %s", PQerrorMessage(be->connection));
    }

    note = PQnotifies (be->connection);
    while (note)
    {
        /* ignore notifies from myself */
        if (note->be_pid == be->my_pid)
        {
            PINFO ("this event from myself: %s from pid=%d", note->relname, note->be_pid);
            free (note);
            note = PQnotifies (be->connection);
            continue;
        }

        PINFO ("notify event %s from pid=%d", note->relname, note->be_pid);

        if (0 == strcasecmp ("gncTransaction", note->relname))
        {
            be->do_transaction ++;
        }
        else if (0 == strcasecmp ("gncCheckpoint", note->relname))
        {
            be->do_checkpoint ++;
        }
        else if (0 == strcasecmp ("gncPrice", note->relname))
        {
            be->do_price ++;
        }
        else if (0 == strcasecmp ("gncAccount", note->relname))
        {
            be->do_account ++;
        }
        else if (0 == strcasecmp ("gncBook", note->relname))
        {
            be->do_book ++;
        }
        else if (0 == strcasecmp ("gncSession", note->relname))
        {
            be->do_session ++;
        }
        else
        {
            PERR ("unexpected notify %s", note->relname);
        }

        /* get the next one */
        free (note);
        note = PQnotifies (be->connection);
    }

    /* for now, we ignore session and checkpoint events */
    if (be->do_transaction + be->do_price + be->do_account) return TRUE;
    return FALSE;
}

/* ============================================================= */

typedef struct _event
{
    Timespec stamp;
    QofEventId type;
    GUID guid;
    QofIdType obj_type;
} Event;


static gpointer
get_event_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    GList *node, *list = (GList *) data;
    char *guid_str;
    Event *ev = NULL;
    GUID guid;
    Timespec ts;
    QofEventId type;
    char change = (DB_GET_VAL("change", j))[0];
    char objtype = (DB_GET_VAL("objtype", j))[0];
    QofIdType obj_type = GNC_ID_NONE;

    guid_str = DB_GET_VAL("guid", j);
    PINFO ("event %c for %s", change, guid_str);

    /* convert from SQL type to engine type */
    switch (change)
    {
    case 'a':
        type = QOF_EVENT_CREATE;
        break;
    case 'm':
        type = QOF_EVENT_MODIFY;
        break;
    case 'd':
        type = QOF_EVENT_DESTROY;
        break;
    default:
        PERR ("unknown change type %c for guid=%s", change, guid_str);
        return data;
    }
    switch (objtype)
    {
    case 'a':
        obj_type = GNC_ID_ACCOUNT;
        break;
    case 'b':
        obj_type = GNC_ID_BOOK;
        break;
    case 'c':
        obj_type = GNC_ID_NONE;
        break;  /* should be commodity */
    case 'e':
        obj_type = GNC_ID_SPLIT;
        break;
    case 'p':
        obj_type = GNC_ID_PRICE;
        break;
    case 't':
        obj_type = GNC_ID_TRANS;
        break;
    case 'x':
        obj_type = GNC_ID_NONE;
        break;
    case ' ':
        obj_type = GNC_ID_NONE;
        break;
    case 'k': /* we are not expecting kvp's in here */
    default:
        PERR ("unexpected class type %c for guid=%s", objtype, guid_str);
        return data;
    }

    string_to_guid (guid_str, &guid);
    ts = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_changed", j));

    /* Compress multiple events for the same object.  In other
     * words, keep only the last event for this object.
     */
    for (node = list; node; node = node->next)
    {
        ev = (Event *) node->data;
        if (guid_equal (&(ev->guid), &guid))
        {
            if (0 >= timespec_cmp (&(ev->stamp), &ts))
            {
                ev->type = type;
                ev->guid = guid;
                ev->stamp = ts;
                ev->obj_type = obj_type;
            }
            return (gpointer) list;
        }
    }

    ev = g_new (Event, 1);

    ev->type = type;
    ev->guid = guid;
    ev->stamp = ts;
    ev->obj_type = obj_type;

    /* add it to our list */
    list = g_list_prepend (list, ev);

    return (gpointer) list;
}

#define GET_EVENTS(guid_name,table, timestamp)	                    \
{                                                                   \
   char *p;                                                         \
   p = be->buff; *p = 0;                                            \
   p = stpcpy (p, "SELECT objtype, change, date_changed, "          \
                  #guid_name " AS guid  FROM " #table               \
                  "  WHERE sessionGuid <> '");                      \
   p = stpcpy (p, be->session_guid_str);                            \
   p = stpcpy (p, "' AND date_changed >= '");                       \
   p = gnc_timespec_to_iso8601_buff (timestamp, p);                 \
   p = stpcpy (p, "';");                                            \
                                                                    \
   SEND_QUERY (be, be->buff, FALSE);                                \
   pending = (GList *) pgendGetResults (be, get_event_cb, pending); \
}

gboolean
pgendProcessEvents (QofBackend *bend)
{
    PGBackend *be = (PGBackend *) bend;
    GList *node, *pending = NULL;

    if (!be) return FALSE;

    ENTER (" ");

    /* Get all recent events from the SQL db. */
    if (be->do_account)
    {
        GET_EVENTS (accountGuid, gncAccountTrail, be->last_account);
    }
    if (be->do_price)
    {
        GET_EVENTS (priceGuid, gncPriceTrail, be->last_price);
    }
    if (be->do_transaction)
    {
        GET_EVENTS (transGuid, gncTransactionTrail, be->last_transaction);

        /* gnc_cm_event_handler() doesn't really want to see any split guids */
        // GET_EVENTS (splitGuid, gncSplitTrail, be->last_transaction);
    }

    /* Loop over each item, updating the engine, and dispatching events */
    for (node = pending; node; node = node->next)
    {
        Event *ev = (Event *) node->data;
        QofIdType local_obj_type;
        QofInstance *ent;

        ent = NULL;
        /* lets see if the local cache has this item in it */
        local_obj_type = pgendGUIDType (be, &(ev->guid));
        if ((local_obj_type != GNC_ID_NONE) &&
                (safe_strcmp (local_obj_type, ev->obj_type)))
        {
            PERR ("ouch! object type mismatch, local=%s, event=%s",
                  local_obj_type, ev->obj_type);
            g_free (ev);
            continue;
        }

        if (!safe_strcmp (ev->obj_type, GNC_ID_ACCOUNT))
        {
            if (0 < timespec_cmp(&(ev->stamp), &(be->last_account)))
            {
                be->last_account = ev->stamp;
            }
            switch (ev->type)
            {
            default:
                PERR ("account: cant' happen !!!!!!!");
                break;
            case QOF_EVENT_CREATE:
            case QOF_EVENT_MODIFY:
            {
                Account *acc;

                /* if the remote user created an account, mirror it here */
                acc = pgendCopyAccountToEngine (be, &(ev->guid));
                ent = QOF_INSTANCE(acc);
                break;
            }
            case QOF_EVENT_DESTROY:
            {
                Account * acc = pgendAccountLookup (be, &(ev->guid));
                xaccAccountBeginEdit (acc);
                xaccAccountDestroy (acc);
                ent = QOF_INSTANCE(acc);
                break;
            }
            }
        }
        else if (!safe_strcmp (ev->obj_type, GNC_ID_TRANS))
        {
            if (0 < timespec_cmp(&(ev->stamp), &(be->last_transaction)))
            {
                be->last_transaction = ev->stamp;
            }
            switch (ev->type)
            {
            default:
                PERR ("transaction: cant' happen !!!!!!!");
                break;
            case QOF_EVENT_CREATE:
            {
                Transaction *trans;
                /* don't mirror transaction creations. If a register needs
                 * it, it will do a query. */
                trans = pgendTransLookup (be, &(ev->guid));
                ent = QOF_INSTANCE(trans);
                PINFO ("create transaction");
                break;
            }
            case QOF_EVENT_MODIFY:
            {
                Transaction *trans;
                trans = pgendTransLookup (be, &(ev->guid));
                pgendCopyTransactionToEngine (be, &(ev->guid));
                ent = QOF_INSTANCE(trans);
                break;
            }
            case QOF_EVENT_DESTROY:
            {
                Transaction *trans = pgendTransLookup (be, &(ev->guid));
                xaccTransBeginEdit (trans);
                /* mark trans for freeing */
                xaccTransDestroy (trans);
                xaccTransCommitEdit (trans);
                ent = QOF_INSTANCE(trans);
                break;
            }
            }
        }
        else if (!safe_strcmp (ev->obj_type, GNC_ID_SPLIT))
        {
            if (0 < timespec_cmp(&(ev->stamp), &(be->last_transaction)))
            {
                be->last_transaction = ev->stamp;
            }
        }
        else if (!safe_strcmp (ev->obj_type, GNC_ID_PRICE))
        {
            if (0 < timespec_cmp(&(ev->stamp), &(be->last_price)))
            {
                be->last_price = ev->stamp;
            }
        }
        else
        {
            PERR ("unknown guid type %s", ev->obj_type);
        }

        /* test the local type again, since we created/modified/destroyed
         * the guid above */
        if (GNC_ID_NONE == local_obj_type)
        {
            local_obj_type = pgendGUIDType (be, &(ev->guid));
            if (GNC_ID_NONE != local_obj_type)
            {
                qof_event_gen(ent, QOF_EVENT_CREATE, NULL);
            }
        }
        else
        {
            local_obj_type = pgendGUIDType (be, &(ev->guid));
            if (GNC_ID_NONE != local_obj_type)
            {
                qof_event_gen(ent, QOF_EVENT_MODIFY, NULL);
            }
            else
            {
                qof_event_gen(ent, QOF_EVENT_DESTROY, NULL);
            }
        }

        g_free (ev);
    }
    g_list_free (pending);

    be->do_account = 0;
    be->do_checkpoint = 0;
    be->do_price = 0;
    be->do_session = 0;
    be->do_transaction = 0;
    return FALSE;
}


/* ============================================================= */
/* find the backend pid */

void
pgendSessionGetPid (PGBackend *be)
{
    PGnotify *note;
    char *p;
    long int r;

    r = random ();

    p = be->buff;
    *p = 0;
    sprintf (p, "LISTEN \"%ld\";\n NOTIFY \"%ld\";", r, r);
    SEND_QUERY (be, be->buff, );
    FINISH_QUERY(be->connection);
    note = PQnotifies (be->connection);
    if (!note)
    {
        PERR ("didn't receive notification");
        return;
    }

    be->my_pid = note->be_pid;
    PINFO ("postgres backend pid =%d", be->my_pid);

    p = be->buff;
    *p = 0;
    sprintf (p, "UNLISTEN \"%ld\";", r);
    SEND_QUERY (be, be->buff, );
    FINISH_QUERY(be->connection);
}

/* ============================================================= */

static gpointer
get_latest_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
    Timespec latest;

    /* get event timestamp */
    latest = gnc_iso8601_to_timespec_gmt (DB_GET_VAL("date_changed", j));
    latest.tv_sec ++;  /* ignore old, pre-logon events */

    be->last_account = latest;
    be->last_price = latest;
    be->last_transaction = latest;

    return data;
}

void
pgendSessionSetupNotifies (PGBackend *be)
{
    char *p;

    /* Get latest times from the database; this to avoid clock
     * skew between database and this local process */
    p = "SELECT date_changed FROM gncAuditTrail* ORDER BY date_changed DESC LIMIT 1;";
    SEND_QUERY (be, p, );
    pgendGetResults (be, get_latest_cb, NULL);

    p = "LISTEN gncSession;\nLISTEN gncAccount;\n"
        "LISTEN gncPrice;\nLISTEN gncTransaction;\n"
        "LISTEN gncCheckpoint;\nLISTEN gncBook;\n";
    SEND_QUERY (be, p, );
    FINISH_QUERY(be->connection);
}

/* ======================== END OF FILE ======================== */

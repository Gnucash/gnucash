/********************************************************************\
 * events.c -- implements event handling for postgres backend       *
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
 * FILE:
 * events.c
 *
 * FUNCTION:
 * Implements the event-handling callbacks for the Postgres backend.
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas
 * 
 */

#define _GNU_SOURCE

#include "config.h"

#include <libpq-fe.h>  

#include "Backend.h"
#include "BackendP.h"
#include "events.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-event-p.h"
#include "guid.h"
#include "GNCId.h"
#include "GNCIdP.h"

#include "PostgresBackend.h"

#include "putil.h"

static short module = MOD_BACKEND; 


/* ============================================================= */
/* ============================================================= */
/*              EVENT NOTIFICATION HANDLER                       */
/* ============================================================= */
/* ============================================================= */

gboolean
pgendEventsPending (Backend *bend)
{
   PGBackend *be = (PGBackend *) bend;
   PGnotify *note;
   char *p;
   int rc;

   if (!be) return FALSE;
   ENTER ("mypid=%d", be->my_pid);
PWARN("mypid=%d", be->my_pid);

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
PWARN ("notify event %s from pid=%d", note->relname, note->be_pid);

      if (0 == strcasecmp ("gncTransaction", note->relname))
      {
         be->do_transaction ++;
      } 
      else
      if (0 == strcasecmp ("gncCheckpoint", note->relname))
      {
         be->do_checkpoint ++;
      } 
      else
      if (0 == strcasecmp ("gncPrice", note->relname))
      {
         be->do_price ++;
      } 
      else
      if (0 == strcasecmp ("gncAccount", note->relname))
      {
         be->do_account ++;
      } 
      else
      if (0 == strcasecmp ("gncSession", note->relname))
      {
         be->do_session ++;
      } 
      else
      {
         PERR ("unexpected notify %s", note->relname)
      }

      /* get the next one */
      free (note);
      note = PQnotifies (be->connection);
   } 

   /* for now, we ignore session and checkpoint events */
   if (be->do_transaction + be->do_price + be->do_account) return TRUE;
   return FALSE;
}

static gpointer
get_event_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GNCEngineEventType type;
   GUID guid;
   Timespec ts;
   Timespec *latest = (Timespec *) data;
   char change = (DB_GET_VAL("change",j))[0];

   string_to_guid (DB_GET_VAL("guid",j), &guid);
   ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_changed",j));

   if (0 < timespec_cmp(&ts, latest)) *latest = ts;

   switch (change)
   {
      case 'a': type = GNC_EVENT_CREATE; break;
      case 'm': type = GNC_EVENT_MODIFY; break;
      case 'd': type = GNC_EVENT_DESTROY; break;
      default:
         PERR ("unknown change type %c", change);
         return data;
   }

   PINFO ("event %c for %s", change, DB_GET_VAL("guid",j));
PWARN ("event %c for %s", change, DB_GET_VAL("guid",j));
   gnc_engine_generate_event (&guid, type);

   return data;
}

#define GET_EVENTS(guid_name,table,timestamp)			\
{								\
   Timespec latest;						\
   char *p;							\
   latest.tv_sec = -2;						\
   latest.tv_nsec = 0;						\
								\
   p = be->buff; *p = 0;					\
   p = stpcpy (p, "SELECT change, date_changed, " #guid_name 	\
                  " AS guid  FROM " #table			\
                  "  WHERE sessionGuid <> '");			\
   p = stpcpy (p, be->session_guid_str);			\
   p = stpcpy (p, "' AND date_changed >= '");			\
   p = gnc_timespec_to_iso8601_buff (timestamp, p);		\
   p = stpcpy (p, "';");					\
								\
   SEND_QUERY (be, be->buff, FALSE);				\
   pgendGetResults (be, get_event_cb, &latest);			\
								\
   if (0 < timespec_cmp(&latest, &(timestamp))) 		\
   {								\
      (timestamp) = latest;					\
   }								\
}

gboolean
pgendProcessEvents (Backend *bend)
{
   PGBackend *be = (PGBackend *) bend;

   if (!be) return FALSE;

   ENTER (" ");

   /* handle each event type */
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
      // GET_EVENTS (entryGuid, gncEntryTrail, be->last_transaction);
   }

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
   
   p = be->buff; *p=0;
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

   p = be->buff; *p=0;
   sprintf (p, "UNLISTEN \"%ld\";", r);
   SEND_QUERY (be, be->buff, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */

void 
pgendSessionSetupNotifies (PGBackend *be)
{
   char *p;

   p = "LISTEN gncSession;\nLISTEN gncAccount;\n"
       "LISTEN gncPrice;\nLISTEN gncTransaction;\n"
       "LISTEN gncCheckpoint;";
   SEND_QUERY (be, p, );
   FINISH_QUERY(be->connection);
}

/* ======================== END OF FILE ======================== */

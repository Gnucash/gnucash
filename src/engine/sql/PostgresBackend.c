/* 
 * FILE:
 * PostgressBackend.c
 *
 * FUNCTION:
 * Implements the callbacks for the postgress backend.
 * this is code kinda usually works.
 * it needs review and design checking
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas
 * 
 */

#define _GNU_SOURCE
#include <glib.h>
#include <pwd.h>
#include <stdio.h>  
#include <string.h>  
#include <sys/types.h>  
#include <unistd.h>  

#include <pgsql/libpq-fe.h>  

#include "AccountP.h"
#include "BackendP.h"
#include "Group.h"
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "guid.h"
#include "GNCId.h"
#include "GNCIdP.h"
#include "TransactionP.h"

#include "builder.h"
#include "gncquery.h"
#include "PostgresBackend.h"

static short module = MOD_BACKEND; 

static void pgendDisable (PGBackend *be);
static void pgendEnable (PGBackend *be);
static void pgendInit (PGBackend *be);

static const char * pgendSessionGetMode (PGBackend *be);

/* hack alert -- this is the query buffer size, it can be overflowed.
 * Ideally, its dynamically resized.  On the other hand, Postgres
 * rejects queries longer than 8192 bytes,(according to the
 * documentation) so theres not much point in getting fancy ... 
 */
#define QBUFSIZE 16350

/* ============================================================= */
/* The SEND_QUERY macro sends the sql statement off to the server. 
 * It performs a minimal check to see that the send succeeded. 
 */

#define SEND_QUERY(be,buff,retval) 				\
{								\
   int rc;							\
   rc = PQsendQuery (be->connection, buff);			\
   if (!rc)							\
   {								\
      /* hack alert -- we need kinder, gentler error handling */\
      PERR("send query failed:\n"				\
           "\t%s", PQerrorMessage(be->connection));		\
      PQfinish (be->connection);				\
      xaccBackendSetError (&be->be, ERR_SQL_SEND_QUERY_FAILED);	\
      return retval;						\
   }								\
}

/* --------------------------------------------------------------- */
/* The FINISH_QUERY macro makes sure that the previously sent
 * query complete with no errors.  It assumes that the query
 * is does not produce any results; if it did, those results are
 * discarded (only error conditions are checked for).
 */

#define FINISH_QUERY(conn) 					\
{								\
   int i=0;							\
   PGresult *result; 						\
   /* complete/commit the transaction, check the status */	\
   do {								\
      ExecStatusType status;					\
      result = PQgetResult((conn));				\
      if (!result) break;					\
      PINFO ("clearing result %d", i);				\
      status = PQresultStatus(result);  			\
      if (PGRES_COMMAND_OK != status) {				\
         PERR("finish query failed:\n"				\
              "\t%s", PQerrorMessage((conn)));			\
         PQclear(result);					\
         PQfinish ((conn));					\
         xaccBackendSetError (&be->be, ERR_SQL_FINISH_QUERY_FAILED);	\
      }								\
      PQclear(result);						\
      i++;							\
   } while (result);						\
}

/* --------------------------------------------------------------- */
/* The GET_RESULTS macro grabs the result of an pgSQL query off the
 * wire, and makes sure that no errors occured. Results are left 
 * in the result buffer.
 */
#define GET_RESULTS(conn,result) 				\
{								\
   ExecStatusType status;  					\
   result = PQgetResult (conn);					\
   if (!result) break;						\
   status = PQresultStatus(result);				\
   if ((PGRES_COMMAND_OK != status) &&				\
       (PGRES_TUPLES_OK  != status))				\
   {								\
      PERR("failed to get result to query:\n"			\
           "\t%s", PQerrorMessage((conn)));			\
      PQclear (result);						\
      /* hack alert need gentler, kinder error recovery */	\
      PQfinish (conn);						\
      xaccBackendSetError (&be->be, ERR_SQL_GET_RESULT_FAILED);	\
      break;							\
   }								\
}

/* --------------------------------------------------------------- */
/* The IF_ONE_ROW macro counts the number of rows returned by 
 * a query, reports an error if there is more than one row, and
 * conditionally executes a block for the first row.
 */
 
#define IF_ONE_ROW(result,nrows,loopcounter)			\
   {								\
      int ncols = PQnfields (result);				\
      nrows += PQntuples (result);				\
      PINFO ("query result %d has %d rows and %d cols",		\
           loopcounter, nrows, ncols);				\
   }								\
   if (1 < nrows) {						\
      PERR ("unexpected duplicate records");			\
      xaccBackendSetError (&be->be, ERR_SQL_CORRUPT_DB);	\
      break;							\
   } else if (1 == nrows) 

/* --------------------------------------------------------------- */
/* Some utility macros for comparing values returned from the
 * database to values in the engine structs.  These macros
 * all take three arguments:
 * -- sqlname -- input -- the name of the field in the sql table
 * -- fun -- input -- a subroutine returning a value
 * -- ndiffs -- input/output -- integer, incremented if the 
 *              value ofthe field and the value returned by
 *              the subroutine differ.
 *
 * The different macros compare different field types.
 */

#define DB_GET_VAL(str,n) (PQgetvalue (result, n, PQfnumber (result, str)))

/* compare string types.  null strings and emty strings are  
 * considered to be equal */
#define COMP_STR(sqlname,fun,ndiffs) { 				\
   if (null_strcmp (DB_GET_VAL(sqlname,0),fun)) {		\
      PINFO("mis-match: %s sql='%s', eng='%s'", sqlname, 	\
         DB_GET_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

/* compare guids */
#define COMP_GUID(sqlname,fun, ndiffs) { 			\
   char guid_str[GUID_ENCODING_LENGTH+1];			\
   guid_to_string_buff(fun, guid_str); 				\
   if (null_strcmp (DB_GET_VAL(sqlname,0),guid_str)) { 		\
      PINFO("mis-match: %s sql='%s', eng='%s'", sqlname, 	\
         DB_GET_VAL(sqlname,0), guid_str); 			\
      ndiffs++; 						\
   }								\
} 

/* comapre one char only */
#define COMP_CHAR(sqlname,fun, ndiffs) { 			\
    if (tolower((DB_GET_VAL(sqlname,0))[0]) != tolower(fun)) {	\
       PINFO("mis-match: %s sql=%c eng=%c", sqlname, 		\
         tolower((DB_GET_VAL(sqlname,0))[0]), tolower(fun)); 	\
      ndiffs++; 						\
   }								\
}

/* Compare dates.
 * Assumes the datestring is in ISO-8601 format 
 * i.e. looks like 1998-07-17 11:00:00.68-05  
 * hack-alert doesn't compare nano-seconds ..  
 * this is intentional,  its because I suspect
 * the sql db round nanoseconds off ... 
 */
#define COMP_DATE(sqlname,fun,ndiffs) { 			\
    Timespec eng_time = fun;					\
    Timespec sql_time = gnc_iso8601_to_timespec_local(		\
                     DB_GET_VAL(sqlname,0)); 			\
    if (eng_time.tv_sec != sql_time.tv_sec) {			\
       char buff[80];						\
       gnc_timespec_to_iso8601_buff(eng_time, buff);		\
       PINFO("mis-match: %s sql='%s' eng=%s", sqlname, 		\
         DB_GET_VAL(sqlname,0), buff); 				\
      ndiffs++; 						\
   }								\
}

/* Compare the date of last modification. 
 * This is a special date comp to make the m4 macros simpler.
 */
#define COMP_NOW(sqlname,fun,ndiffs) { 	 			\
    Timespec eng_time = xaccTransRetDateEnteredTS(ptr);		\
    Timespec sql_time = gnc_iso8601_to_timespec_local(		\
                     DB_GET_VAL(sqlname,0)); 			\
    if (eng_time.tv_sec != sql_time.tv_sec) {			\
       char buff[80];						\
       gnc_timespec_to_iso8601_buff(eng_time, buff);		\
       PINFO("mis-match: %s sql='%s' eng=%s", sqlname, 		\
         DB_GET_VAL(sqlname,0), buff); 				\
      ndiffs++; 						\
   }								\
}


/* Compare long-long integers */
#define COMP_INT64(sqlname,fun,ndiffs) { 			\
   if (atoll (DB_GET_VAL(sqlname,0)) != fun) {			\
      PINFO("mis-match: %s sql='%s', eng='%lld'", sqlname, 	\
         DB_GET_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

/* compare 32-bit ints */
#define COMP_INT32(sqlname,fun,ndiffs) { 			\
   if (atol (DB_GET_VAL(sqlname,0)) != fun) {			\
      PINFO("mis-match: %s sql='%s', eng='%d'", sqlname, 	\
         DB_GET_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

/* ============================================================= */
/* misc bogus utility routines */

static char *
pgendGetHostname (PGBackend *be)
{
   char * p;

   p = be->buff;
   *p = 0;
   if (0 == gethostname (p, QBUFSIZE/3)) 
   {
      p += strlen (p);
      p = stpcpy (p, ".");
   }
   getdomainname (p, QBUFSIZE/3);
   return be->buff;
}

static char *
pgendGetUsername (PGBackend *be)
{
   uid_t uid = getuid();
   struct passwd *pw = getpwuid (uid);
   if (pw) return (pw->pw_name);
   return NULL;
}

static char *
pgendGetUserGecos (PGBackend *be)
{
   uid_t uid = getuid();
   struct passwd *pw = getpwuid (uid);
   if (pw) return (pw->pw_gecos);
   return NULL;
}

/* ============================================================= */
/* This routine finds the commodity by parsing a string 
 * of the form NAMESPACE::MNEMONIC 
 */

static const gnc_commodity *
gnc_string_to_commodity (const char *str)
{
   /* hop through a couple of hoops for the commodity */
   /* it would be nice to simplify this ... */
   gnc_commodity_table *comtab = gnc_engine_commodities();
   gnc_commodity *com;
   char *space, *name;

   space = g_strdup(str);
   name = strchr (space, ':');
   *name = 0;
   name += 2;

   com = gnc_commodity_table_lookup(comtab, space, name);
   g_free (space);
   return com;
}

/* ============================================================= */
/* include the auto-generated code */

#include "autogen.c"

/* ============================================================= */
/* This routine updates the account structure if needed, and/or
 * stores it the first time if it hasn't yet been stored.
 * Note that it sets a mark to avoid excessive recursion:
 * This routine shouldn't be used outside of locks,
 * where the recursion prevention clears the marks ...
 */

static void
pgendStoreAccountNoLock (PGBackend *be, Account *acct,
                         gboolean do_mark)
{
   const gnc_commodity *com;

   if (!be || !acct) return;
   ENTER ("acct=%p, mark=%d", acct, do_mark);

   if (do_mark) 
   { 
      /* Check to see if we've processed this account recently.
       * If so, then return.  The goal here is to avoid excess
       * hits to the database, leading to poor performance.
       * Note that this marking makes this routine unsafe to use 
       * outside a lock (since we never clear the mark)
       */
      if (xaccAccountGetMark (acct)) return;
      xaccAccountSetMark (acct, 1);
   }

   pgendPutOneAccountOnly (be, acct);

   /* make sure the account's commodity is in the commodity table */
   /* hack alert -- it would be more efficient to do this elsewhere,
    * and not here. */
   com = xaccAccountGetCommodity (acct);
   pgendPutOneCommodityOnly (be, (gnc_commodity *) com);

   LEAVE(" ");
}

/* ============================================================= */
/* This routine traverses the transaction structure and stores/updates
 * it in the database.  If checks the transaction splits as well,
 * updating those.  Finally, it makes sure that each account is present
 * as well. 
 */

static void
pgendStoreTransactionNoLock (PGBackend *be, Transaction *trans, 
                             gboolean do_mark)
{
   GUID nullguid = *(xaccGUIDNULL());
   GList *deletelist=NULL, *node;
   PGresult *result;
   char * p;
   int i, nrows, nsplits;

   if (!be || !trans) return;
   ENTER ("trans=%p, mark=%d", trans, do_mark);


   /* first, we need to see which splits are in the database
    * since what is there may not match what we have cached in 
    * the engine. */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT entryGuid FROM gncEntry WHERE transGuid='");
   p = guid_to_string_buff(xaccTransGetGUID(trans), p);
   p = stpcpy (p, "';");

   SEND_QUERY (be,be->buff, );

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            GUID guid = nullguid;
            string_to_guid (DB_GET_VAL ("entryGuid", j), &guid);

            /* If the database has splits that the engine doesn't,
             * collect 'em up & we'll have to delete em */
            if (NULL == xaccLookupEntity (&guid, GNC_ID_SPLIT))
            {
               deletelist = g_list_prepend (deletelist, 
                        g_strdup(DB_GET_VAL ("entryGuid", j)));
            }
         }
      }
      i++;
      PQclear (result);
   } while (result);


   /* delete those that don't belong */
   for (node=deletelist; node; node=node->next)
   {
      p = be->buff; *p = 0;
      p = stpcpy (p, "DELETE FROM gncEntry WHERE entryGuid='");
      p = stpcpy (p, node->data);
      p = stpcpy (p, "';");
      SEND_QUERY (be,be->buff, );
      FINISH_QUERY(be->connection);
      g_free (node->data);
   }

   /* Update the rest */
   nsplits = xaccTransCountSplits (trans);

   if ((nsplits) && !(trans->open & BEING_DESTROYED))
   { 
      for (i=0; i<nsplits; i++) {
         Split * s = xaccTransGetSplit (trans, i);
         pgendPutOneSplitOnly (be, s);
      }
      pgendPutOneTransactionOnly (be, trans);
   }
   else
   {
      for (i=0; i<nsplits; i++) {
         Split * s = xaccTransGetSplit (trans, i);
         p = be->buff; *p = 0;
         p = stpcpy (p, "DELETE FROM gncEntry WHERE entryGuid='");
         p = guid_to_string_buff (xaccSplitGetGUID(s), p);
         p = stpcpy (p, "';");
         PINFO ("%s\n", be->buff);
         SEND_QUERY (be,be->buff, );
         FINISH_QUERY(be->connection);
      }
      p = be->buff; *p = 0;
      p = stpcpy (p, "DELETE FROM gncTransaction WHERE transGuid='");
      p = guid_to_string_buff (xaccTransGetGUID(trans), p);
      p = stpcpy (p, "';");
      PINFO ("%s\n", be->buff);
      SEND_QUERY (be,be->buff, );
      FINISH_QUERY(be->connection);
   }

   LEAVE(" ");
}

static void
pgendStoreTransaction (PGBackend *be, Transaction *trans)
{
   char * bufp;
   if (!be || !trans) return;
   ENTER ("be=%p, trans=%p", be, trans);

   /* lock it up so that we store atomically */
   bufp = "BEGIN;"
          "LOCK TABLE gncTransaction IN EXCLUSIVE MODE; "
          "LOCK TABLE gncEntry IN EXCLUSIVE MODE; "
          "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
          "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; ";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   pgendStoreTransactionNoLock (be, trans, FALSE);

   bufp = "COMMIT;";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}

/* ============================================================= */
/* This routine traverses the group structure and stores it into 
 * the database.  The NoLock version doesn't lock up the tables.
 */

static int
traverse_cb (Transaction *trans, void *cb_data)
{
   /* the callback is only called when marking... */
   pgendStoreTransactionNoLock ((PGBackend *) cb_data, trans, TRUE);
   return 0;
}

static void
pgendStoreGroupNoLock (PGBackend *be, AccountGroup *grp, 
                       gboolean do_mark)
{
   int i, nacc;

   if (!be || !grp) return;
   ENTER("grp=%p mark=%d", grp, do_mark);

   /* walk the account tree, and store subaccounts */
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      AccountGroup *subgrp;
      Account *acc = xaccGroupGetAccount(grp, i);

      pgendStoreAccountNoLock (be, acc, do_mark);

      /* recursively walk to child accounts */
      subgrp = xaccAccountGetChildren (acc);
      if (subgrp) pgendStoreGroupNoLock(be, subgrp, do_mark);
   }
   LEAVE(" ");
}


static void
pgendStoreGroup (PGBackend *be, AccountGroup *grp)
{
   char *bufp;
   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return;

   /* lock it up so that we store atomically */
   bufp = "BEGIN;";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   /* Clear the account marks; useful later to avoid recurision
    * during account consistency checks. */
   xaccClearMarkDownGr (grp, 0);

   /* reset the write flags. We use this to make sure we don't
    * get caught in infinite recursion */
   xaccGroupBeginStagedTransactionTraversals(grp);
   pgendStoreGroupNoLock (be, grp, TRUE);

   /* recursively walk transactions */
   xaccGroupStagedTransactionTraversal (grp, 1, traverse_cb, be);

   /* reset the write flags again */
   xaccClearMarkDownGr (grp, 0);

   bufp = "COMMIT;";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}

/* ============================================================= */
/* recompute *all* checkpoints for the account */

static void
pgendAccountRecomputeAllCheckpoints (PGBackend *be, const GUID *acct_guid)
{
   Timespec this_ts, prev_ts;
   GMemChunk *chunk;
   GList *node, *checkpoints = NULL;
   PGresult *result;
   Checkpoint *bp;
   char *p;
   int i, nrows, nsplits;
   Account *acc;
   const char *commodity_name;

   if (!be) return;
   ENTER("be=%p", be);

   acc = xaccLookupEntity (acct_guid, GNC_ID_ACCOUNT);
   commodity_name = gnc_commodity_get_unique_name (xaccAccountGetCommodity(acc));

   chunk = g_mem_chunk_create (Checkpoint, 300, G_ALLOC_ONLY);

   /* prevent others from inserting any splits while we recompute 
    * the checkpoints. (hack alert -verify that this is the correct
    * lock) */
   p = "BEGIN WORK; "
       "LOCK TABLE gncEntry IN SHARE MODE; "
       "LOCK TABLE gncCheckpoint IN ACCESS EXCLUSIVE MODE; ";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* Blow all the old checkpoints for this account out of the water.
    * This should help ensure against accidental corruption.
    */
   p = be->buff; *p = 0;
   p = stpcpy (p, "DELETE FROM gncCheckpoint WHERE accountGuid='");
   p = guid_to_string_buff (acct_guid, p);
   p = stpcpy (p, "';");
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

   /* and now, fetch *all* of the splits in this account */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT gncEntry.amountNum AS amountNum, "
                  "       gncEntry.reconciled AS reconciled,"
                  "       gncTransaction.date_posted AS date_posted "
                  "FROM gncEntry, gncTransaction "
                  "WHERE gncEntry.transGuid = gncTransaction.transGuid "
                  "AND accountGuid='");
   p = guid_to_string_buff (acct_guid, p);
   p = stpcpy (p, "' ORDER BY gncTransaction.date_posted ASC;");
   SEND_QUERY (be,be->buff, );

   /* malloc a new checkpoint, set it to the dawn of AD time ... */
   bp = g_chunk_new0 (Checkpoint, chunk);
   checkpoints = g_list_prepend (checkpoints, bp);
   this_ts = gnc_iso8601_to_timespec_local ("1970-04-15 08:35:46.00");
   bp->datetime = this_ts;
   bp->account_guid = acct_guid;
   bp->commodity = commodity_name;

   /* malloc a new checkpoint ... */
   nsplits = 0;
   bp = g_chunk_new0 (Checkpoint, chunk);
   checkpoints = g_list_prepend (checkpoints, bp);
   bp->account_guid = acct_guid;
   bp->commodity = commodity_name;

   /* start adding up balances */
   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            gint64 amt;
            char recn;
            
            /* lets see if its time to start a new checkpoint */
            /* look for splits that occur at least ten seconds apart */
            prev_ts = this_ts;
            prev_ts.tv_sec += 10;
            this_ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_posted",j));
            if ((MIN_CHECKPOINT_COUNT < nsplits) &&
                (timespec_cmp (&prev_ts, &this_ts) < 0))
            {
               Checkpoint *next_bp;

               /* Set checkpoint five seconds back. This is safe,
                * because we looked for a 10 second gap above */
               this_ts.tv_sec -= 5;
               bp->datetime = this_ts;

               /* and now, build a new checkpoint */
               nsplits = 0;
               next_bp = g_chunk_new0 (Checkpoint, chunk);
               checkpoints = g_list_prepend (checkpoints, next_bp);
               *next_bp = *bp;
               bp = next_bp;
               bp->account_guid = acct_guid;
               bp->commodity = commodity_name;
            }
            nsplits ++;

            /* accumulate balances */
            amt = atoll (DB_GET_VAL("amountNum",j));
            recn = (DB_GET_VAL("reconciled",j))[0];
            bp->balance += amt;
            if (NREC != recn)
            {
               bp->cleared_balance += amt;
            }
            if (YREC == recn)
            {
               bp->reconciled_balance += amt;
            }

         }
      }

      PQclear (result);
      i++;
   } while (result);
   
   /* set the timestamp on the final checkpoint,
    *  8 seconds past the very last split */
   this_ts.tv_sec += 8;
   bp->datetime = this_ts;

   /* now store the checkpoints */
   for (node = checkpoints; node; node = node->next)
   {
      bp = (Checkpoint *) node->data;
      pgendStoreOneCheckpointOnly (be, bp, SQL_INSERT);
   }

   g_list_free (checkpoints);
   g_mem_chunk_destroy (chunk);

   p = "COMMIT WORK;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

}

/* ============================================================= */
/* recompute fresh balance checkpoints for every account */

static void
pgendGroupRecomputeAllCheckpoints (PGBackend *be, AccountGroup *grp)
{
   GList *acclist, *node;

   acclist = xaccGroupGetSubAccounts(grp);
   for (node = acclist; node; node=node->next)
   {
      Account *acc = (Account *) node->data;
      pgendAccountRecomputeAllCheckpoints (be, xaccAccountGetGUID(acc));
   }
   g_list_free (acclist);
}

/* ============================================================= */
/* get checkpoint value for the account 
 * We find the checkpoint which matches the account and commodity,
 * for the first date immediately preceeding the date.  
 * Then we fill in the balance fields for the returned query.
 */

static void
pgendAccountGetCheckpoint (PGBackend *be, Checkpoint *chk)
{
   PGresult *result;
   int i, nrows;
   char * p;

   if (!be || !chk) return;
   ENTER("be=%p", be);

   /* create the query we need */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT balance, cleared_balance, reconciled_balance "
                  "FROM gncCheckpoint "
                  "WHERE accountGuid='");
   p = guid_to_string_buff (chk->account_guid, p);
   p = stpcpy (p, "' AND commodity='");
   p = stpcpy (p, chk->commodity);
   p = stpcpy (p, "' AND date_xpoint <'");
   p = gnc_timespec_to_iso8601_buff (chk->datetime, p);
   p = stpcpy (p, "' ORDER BY date_xpoint DESC LIMIT 1;");
   SEND_QUERY (be,be->buff, );

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j=0, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         if (1 < nrows) 
         {
            PERR ("excess data");
            PQclear (result);
            return;
         }
         chk->balance = atoll(DB_GET_VAL("balance", j));
         chk->cleared_balance = atoll(DB_GET_VAL("cleared_balance", j));
         chk->reconciled_balance = atoll(DB_GET_VAL("reconciled_balance", j));
      }

      PQclear (result);
      i++;
   } while (result);

   LEAVE("be=%p", be);
}

/* ============================================================= */
/* get checkpoint value for all accounts */

static void
pgendGroupGetAllCheckpoints (PGBackend *be, AccountGroup*grp)
{
   Checkpoint chk;
   GList *acclist, *node;

   if (!be || !grp) return;
   ENTER("be=%p", be);

   chk.datetime.tv_sec = time(0);
   chk.datetime.tv_nsec = 0;

   acclist = xaccGroupGetSubAccounts (grp);

   /* loop over all accounts */
   for (node=acclist; node; node=node->next)
   {
      Account *acc;
      const gnc_commodity *com;
      gint64 deno;
      gnc_numeric baln;
      gnc_numeric cleared_baln;
      gnc_numeric reconciled_baln;

      /* setupwhat we will match for */
      acc = (Account *) node->data;
      com = xaccAccountGetCommodity(acc);
      chk.commodity = gnc_commodity_get_unique_name(com);
      chk.account_guid = xaccAccountGetGUID (acc);
      chk.balance = 0;
      chk.cleared_balance = 0;
      chk.reconciled_balance = 0;

      /* get the checkpoint */
      pgendAccountGetCheckpoint (be, &chk);

      /* set the account balances */
      deno = gnc_commodity_get_fraction (com);
      baln = gnc_numeric_create (chk.balance, deno);
      cleared_baln = gnc_numeric_create (chk.cleared_balance, deno);
      reconciled_baln = gnc_numeric_create (chk.reconciled_balance, deno);

      xaccAccountSetStartingBalance (acc, baln,
                                     cleared_baln, reconciled_baln);
   }

   g_list_free (acclist);
   LEAVE("be=%p", be);
}

/* ============================================================= */

static void
pgendSync (Backend *bend, AccountGroup *grp)
{
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, grp=%p", be, grp);

   /* hack alert -- this is *not* the correct implementation
    * of what the synchronize function is supposed to do. 
    * This is a sick placeholder.
    */
   pgendStoreGroup (be, grp);

   if ((MODE_SINGLE_FILE != be->session_mode) &&
       (MODE_SINGLE_UPDATE != be->session_mode))
   {
      /* Maybe this should be part of store group ?? */
      pgendGroupRecomputeAllCheckpoints (be, grp);
   }

   LEAVE(" ");
}

/* ============================================================= */

static void
pgendSyncSingleFile (Backend *bend, AccountGroup *grp)
{
   char *p;
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, grp=%p", be, grp);

   /* In single file mode, we treat 'sync' as 'file save'.
    * We start by deleting *everything*, and then writing 
    * everything out.  This is rather nasty, ugly and dangerous,
    * but that's the nature of single-file mode.  Note: we
    * have to delete everything because there is no other way 
    * of finding out that an account, transaction or split
    * was deleted. i.e. there's no other way to delete.  So
    * start with a clean slate.
    */
    
   p = "DELETE FROM gncEntry; "
       "DELETE FROM gncTransaction; "
       "DELETE FROM gncAccount; "
       "DELETE FROM gncCommodity; ";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   pgendStoreGroup (be, grp);

   LEAVE(" ");
}

/* ============================================================= */
/* 
 * The pgendCopyTransactionToEngine() routine 'copies' data out of 
 *    the SQL database and into the engine, for the indicated 
 *    Transaction GUID.  It starts by looking for an existing
 *    transaction in the engine with such a GUID.  If found, then
 *    it compares the date of last update to what's in the sql DB.
 *    If the engine data is older, or the engine doesn't yet have 
 *    this transaction, then the full update happens.  The full
 *    update sets up the stransaction structure, all of the splits
 *    in the transaction, and makes sure that all of the splits 
 *    are in the proper accounts.  If the pre-existing tranasaction
 *    in the engine has more splits than what's in the DB, then these
 *    are pruned so that the structure exactly matches what's in the 
 *    DB.  This routine then returns FALSE.
 *
 *    If this routine finds a pre-existing transaction in the engine,
 *    and the date of last modification of this transaction is 
 *    *newer* then what the DB holds, then this routine returns
 *    TRUE, and does *not* perform any update.
 */

static gboolean
pgendCopyTransactionToEngine (PGBackend *be, GUID *trans_guid)
{
   const gnc_commodity *modity=NULL;
   GUID nullguid = *(xaccGUIDNULL());
   char *pbuff;
   Transaction *trans;
   PGresult *result;
   Account *acc, *previous_acc=NULL;
   gboolean do_set_guid=FALSE;
   gboolean engine_data_is_newer = FALSE;
   int i, j, nrows;
   GList *node, *db_splits=NULL, *engine_splits, *delete_splits=NULL;
   
   ENTER ("be=%p", be);
   if (!be || !trans_guid) return FALSE;

   /* disable callbacks into the backend, and events to GUI */
   gnc_engine_suspend_events();
   pgendDisable(be);

   /* first, see if we already have such a transaction */
   trans = (Transaction *) xaccLookupEntity (trans_guid, GNC_ID_TRANS);
   if (!trans)
   {
      trans = xaccMallocTransaction();
      do_set_guid=TRUE;
      engine_data_is_newer = FALSE;
   }

   /* build the sql query to get the transaction */
   pbuff = be->buff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncTransaction WHERE transGuid='");
   pbuff = guid_to_string_buff(trans_guid, pbuff);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,be->buff, FALSE);
   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         j = 0;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         if (1 < nrows)
         {
             /* since the guid is primary key, this error is totally
              * and completely impossible, theoretically ... */
             PERR ("!!!!!!!!!!!SQL database is corrupt!!!!!!!\n"
                   "too many transactions with GUID=%s\n",
                    guid_to_string (trans_guid));
             if (jrows != nrows) xaccTransCommitEdit (trans);
             xaccBackendSetError (&be->be, ERR_SQL_CORRUPT_DB);
             return FALSE;
         }

         /* First order of business is to determine whose data is
          * newer: the engine cache, or the database.  If the 
          * database has newer stuff, we update eh engine. If the
          * engine is newer, we need to poke into the database.
          * Of course, we know the database has newer data if this
          * transaction doesn't exist in the engine yet.
          * Also, make the date comparison so that engine
          * is considered newer only if engine is strictly newer,
          * so that 'equals' doesn't cause a database write.
          */
         if (!do_set_guid)
         {
            Timespec db_ts, cache_ts;
            db_ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_entered",j));
            cache_ts = xaccTransRetDateEnteredTS (trans);
            if (0 < timespec_cmp (&db_ts, &cache_ts)) {
               engine_data_is_newer = TRUE;
            } else {
               engine_data_is_newer = FALSE;
            }
         }

         /* if the DB data is newer, copy it to engine */
         if (FALSE == engine_data_is_newer)
         {
            Timespec ts;
            xaccTransBeginEdit (trans);
            if (do_set_guid) xaccTransSetGUID (trans, trans_guid);
            xaccTransSetNum (trans, DB_GET_VAL("num",j));
            xaccTransSetDescription (trans, DB_GET_VAL("description",j));
            ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_posted",j));
            xaccTransSetDatePostedTS (trans, &ts);
            ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_entered",j));
            xaccTransSetDateEnteredTS (trans, &ts);

            /* hack alert -- don't set the transaction currency until
             * after all splits are restored. This hack is used to set
             * the reporting currency in an account. This hack will be 
             * obsolete when reporting currencies are removed from the
             * account. */
            modity = gnc_string_to_commodity (DB_GET_VAL("currency",j));
#if 0
             xaccTransSetCurrency (trans, 
                    gnc_string_to_commodity (DB_GET_VAL("currency",j)));
#endif
         }
      }
      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) 
   {
      /* hack alert -- not sure how to handle this case; we'll just 
       * punt for now ... */
      PERR ("no such transaction in the database. This is unexpected ...\n");
      xaccBackendSetError (&be->be, ERR_SQL_MISSING_DATA);
      return FALSE;
   }

   /* if engine data was newer, we are done */
   if (TRUE == engine_data_is_newer) return TRUE;

   /* build the sql query the splits */
   pbuff = be->buff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncEntry WHERE transGuid='");
   pbuff = guid_to_string_buff(trans_guid, pbuff);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,be->buff, FALSE);
   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            Split *s;
            GUID guid;
            Timespec ts;
            gint64 num, denom;
            gnc_numeric value, amount;

            /* --------------------------------------------- */
            /* first, lets see if we've already got this one */
            PINFO ("split GUID=%s", DB_GET_VAL("entryGUID",j));
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("entryGUID",j), &guid);
            s = (Split *) xaccLookupEntity (&guid, GNC_ID_SPLIT);
            if (!s)
            {
               s = xaccMallocSplit();
               xaccSplitSetGUID(s, &guid);
            }

            /* next, restore some split data */
            /* hack alert - not all split fields handled */
            xaccSplitSetMemo(s, DB_GET_VAL("memo",j));
            xaccSplitSetAction(s, DB_GET_VAL("action",j));
            ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_reconciled",j));
            xaccSplitSetDateReconciledTS (s, &ts);

            num = atoll (DB_GET_VAL("amountNum", j));
            denom = atoll (DB_GET_VAL("amountDenom", j));
            amount = gnc_numeric_create (num, denom);
            xaccSplitSetShareAmount (s, amount);

            num = atoll (DB_GET_VAL("valueNum", j));
            denom = atoll (DB_GET_VAL("valueDenom", j));
            value = gnc_numeric_create (num, denom);
            xaccSplitSetValue (s, value);

            xaccSplitSetReconcile (s, (DB_GET_VAL("reconciled", j))[0]);

            xaccTransAppendSplit (trans, s);

            /* --------------------------------------------- */
            /* next, find the account that this split goes into */
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("accountGUID",j), &guid);
            acc = (Account *) xaccLookupEntity (&guid, GNC_ID_ACCOUNT);
            if (!acc)
            {
               PERR ("account not found, don't know what to do ");
            }
            else
            {
               if (acc != previous_acc)
               {
                  xaccAccountCommitEdit (previous_acc);
                  xaccAccountBeginEdit (acc);
                  previous_acc = acc;
               }
               xaccAccountInsertSplit(acc, s);
            }

            /* --------------------------------------------- */
            /* finally tally them up; we use this below to clean 
             * out deleted splits */
            db_splits = g_list_prepend (db_splits, s);
         }
      }
      i++;
      PQclear (result);
   } while (result);

   /* close out dangling edit session */
   xaccAccountCommitEdit (previous_acc);

   i=0; j=0;
   engine_splits = xaccTransGetSplitList(trans);
   for (node = engine_splits; node; node=node->next)
   {
      /* if not found, mark for deletion */
      if (NULL == g_list_find (db_splits, node->data))
      {
         delete_splits = g_list_prepend (delete_splits, node->data);
         j++;
      }
      i++;
   }
   PINFO ("%d of %d splits marked for deletion", j, i);

   /* now, delete them ... */
   for (node=delete_splits; node; node=node->next)
   {
      xaccSplitDestroy ((Split *) node->data);
   }
   g_list_free (delete_splits);
   g_list_free (db_splits);

   /* see note about as to why we do this set here ... */
   xaccTransSetCurrency (trans, modity);

   xaccTransCommitEdit (trans);

   /* re-enable events to the backend and GUI */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
   return FALSE;
}

/* ============================================================= */
/* This routine 'synchronizes' the Transaction structure 
 * associated with the GUID.  Data is pulled out of the database,
 * the versions are compared, and updates made, if needed.
 * The splits are handled as well ...
 *
 * hack alert unfinished, incomplete 
 * hack alert -- philosophically speaking, not clear that this is the 
 * right metaphor.  Its OK to poke date into the engine, but writing
 * data out to the database should make use of versioning, and this
 * routine doesn't.
 */

static void
pgendSyncTransaction (PGBackend *be, GUID *trans_guid)
{
   Transaction *trans;
   gboolean engine_data_is_newer = FALSE;
   
   ENTER ("be=%p", be);
   if (!be || !trans_guid) return;

   /* disable callbacks into the backend, and events to GUI */
   gnc_engine_suspend_events();
   pgendDisable(be);

   engine_data_is_newer = pgendCopyTransactionToEngine (be, trans_guid);

   /* if engine data was newer, we save to the db. */
   if (TRUE == engine_data_is_newer) 
   {
      /* XXX hack alert -- fixme */
      PERR ("Data in the local cache is newer than the data in\n"
            "\tthe database.  Thus, the local data will be sent\n"
            "\tto the database.  This mode of operation is\n"
            "\tguarenteed to clobber other user's updates.\n");

      trans = (Transaction *) xaccLookupEntity (trans_guid, GNC_ID_TRANS);

      /* hack alert -- basically, we should use the pgend_commit_transaction
       * routine instead, and in fact, 'StoreTransaction'
       * pretty much shouldn't be allowed to exist in this
       * framework */
      pgendStoreTransaction (be, trans);
      return;
   }

   /* re-enable events to the backend and GUI */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
}

/* ============================================================= */
/* The pgendRunQuery() routine performs a search on the SQL database for 
 * all of the splits that correspond to gnc-style query, and then 
 * integrates them into the engine cache.  It does this in several steps:
 *
 * 1) convert the engine style query to SQL.
 * 2) run the SQL query to get the splits that satisfy the query
 * 3) pull the transaction ids out of the split, and
 * 4) 'synchronize' the transactions.
 *
 * That is, we only ever pull complete transactions out of the 
 * engine, and never dangling splits. This helps make sure that
 * the splits always balance in a transaction; it also allows
 * the ledger to operate in 'journal' mode.
 *
 * The pgendRunQueryHelper() routine does most of the dirty work.
 *    It takes as an argument an sql command that must be of the
 *    form "SELECT * FROM gncEntry [...]"
 */

static gboolean 
IsGuidInList (GList *list, GUID *guid)
{
   GList *node;
   for (node=list; node; node=node->next)
   {
      if (guid_equal ((GUID *)node->data, guid)) return TRUE;
   }
   return FALSE;
}

static void 
pgendRunQueryHelper (PGBackend *be, const char *qstring)
{
   GUID nullguid = *(xaccGUIDNULL());
   PGresult *result;
   int i, nrows;
   GList *node, *xact_list = NULL;

   ENTER ("string=%s\n", qstring);

   SEND_QUERY (be, qstring, );

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            GUID *trans_guid;

            /* find the transaction this goes into */
            trans_guid = xaccGUIDMalloc();
	    *trans_guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("transGUID",j), trans_guid);

            /* don't put transaction into the list more than once ... */
            if (FALSE == IsGuidInList(xact_list, trans_guid))
            {
               xact_list = g_list_prepend (xact_list, trans_guid);
            }
         }
      }

      PQclear (result);
      i++;
   } while (result);

   /* restore the transactions */
   for (node=xact_list; node; node=node->next)
   {
      pgendCopyTransactionToEngine (be, (GUID *)node->data);
      xaccGUIDFree (node->data);
   }
   g_list_free(xact_list);

   LEAVE (" ");
}

static void 
pgendRunQuery (Backend *bend, Query *q)
{
   PGBackend *be = (PGBackend *)bend;
   const char * sql_query_string;
   sqlQuery *sq;

   ENTER (" ");
   if (!be || !q) return;

   gnc_engine_suspend_events();
   pgendDisable(be);

   /* first thing we do is convert the gnc-engine query into
    * an sql string. */
   sq = sqlQuery_new();
   sql_query_string = sqlQuery_build (sq, q);

   pgendRunQueryHelper (be, sql_query_string);

   sql_Query_destroy(sq);

   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
}

/* ============================================================= */
/* This routine restores all commodities in the database.
 */

static void
pgendGetAllCommodities (PGBackend *be)
{
   gnc_commodity_table *comtab = gnc_engine_commodities();
   PGresult *result;
   char * bufp;
   int i, nrows;

   ENTER ("be=%p", be);
   if (!be) return;

   if (!comtab) {
      PERR ("can't get global commodity table");
      return;
   }

   /* Get them ALL */
   bufp = "SELECT * FROM gncCommodity;";
   SEND_QUERY (be, bufp, );

   i=0; nrows=0; 
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            gnc_commodity *com;

            /* first, lets see if we've already got this one */
            com = gnc_commodity_table_lookup(comtab, 
                     DB_GET_VAL("namespace",j), DB_GET_VAL("mnemonic",j));

            if (com) continue;
            /* no we don't ... restore it */
            com = gnc_commodity_new (
                     DB_GET_VAL("fullname",j), 
                     DB_GET_VAL("namespace",j), 
                     DB_GET_VAL("mnemonic",j),
                     DB_GET_VAL("code",j),
                     atoi(DB_GET_VAL("fraction",j)));

            gnc_commodity_table_insert (comtab, com);
         }
      }

      PQclear (result);
      i++;
   } while (result);

   LEAVE (" ");
}

/* ============================================================= */
/* This routine restores the account heirarchy of *all* accounts in the DB.
 * It implicitly assumes that the database has only one account
 * heirarchy in it, i.e. anny accounts without a parent will be stuffed
 * into the same top group.
 *
 * hack alert -- not all account fields being restored.
 * specifically, need to handle kvp data
 */

static AccountGroup *
pgendGetAllAccounts (PGBackend *be)
{
   PGresult *result;
   AccountGroup *topgrp;
   char * bufp;
   int i, nrows, iacc;
   GUID nullguid = *(xaccGUIDNULL());

   ENTER ("be=%p", be);
   if (!be) return NULL;

   /* first, make sure commodities table is up to date */
   pgendGetAllCommodities (be);

   /* Get them ALL */
   bufp = "SELECT * FROM gncAccount;";
   SEND_QUERY (be, bufp, NULL);

   i=0; nrows=0; iacc=0;
   topgrp = xaccMallocAccountGroup();
   
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            Account *parent;
            Account *acc;
            GUID guid;

            /* first, lets see if we've already got this one */
	    PINFO ("account GUID=%s", DB_GET_VAL("accountGUID",j));
	    guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("accountGUID",j), &guid);
            acc = (Account *) xaccLookupEntity (&guid, GNC_ID_ACCOUNT);
            if (!acc) 
            {
               acc = xaccMallocAccount();
               xaccAccountBeginEdit(acc);
               xaccAccountSetGUID(acc, &guid);
            }

            xaccAccountSetName(acc, DB_GET_VAL("accountName",j));
            xaccAccountSetDescription(acc, DB_GET_VAL("description",j));
            xaccAccountSetCode(acc, DB_GET_VAL("accountCode",j));
            xaccAccountSetType(acc, xaccAccountStringToEnum(DB_GET_VAL("type",j)));
            xaccAccountSetCommodity(acc, 
                   gnc_string_to_commodity (DB_GET_VAL("commodity",j)));

            /* try to find the parent account */
	    PINFO ("parent GUID=%s", DB_GET_VAL("parentGUID",j));
	    guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("parentGUID",j), &guid);
            if (guid_equal(xaccGUIDNULL(), &guid)) 
            {
               /* if the parent guid is null, then this
                * account belongs in the top group */
               xaccGroupInsertAccount (topgrp, acc);
            }
            else
            {
               /* if we haven't restored the parent account, create
                * an empty holder for it */
               parent = (Account *) xaccLookupEntity (&guid, GNC_ID_ACCOUNT);
               if (!parent)
               {
                  parent = xaccMallocAccount();
                  xaccAccountBeginEdit(parent);
                  xaccAccountSetGUID(parent, &guid);
               }
               xaccAccountInsertSubAccount(parent, acc);
            }

            xaccAccountCommitEdit(acc);
         }
      }

      PQclear (result);
      i++;
   } while (result);

   /* Mark the newly read group as saved, since the act of putting
    * it together will have caused it to be marked up as not-saved.
    */
   xaccGroupMarkSaved (topgrp);

   LEAVE (" ");
   return topgrp;
}

/* ============================================================= */
/* Like the title suggests, this one sucks *all* of the 
 * transactions out of the database.  This is a potential 
 * CPU and memory-burner; its use is not suggested for anything
 * but single-user mode.
 *
 * To add injury to insult, this routine fetches in a rather 
 * inefficient manner, in particular, the account query.
 */

static void
pgendGetAllTransactions (PGBackend *be, AccountGroup *grp)
{

   gnc_engine_suspend_events();
   pgendDisable(be);

   pgendRunQueryHelper (be, "SELECT * FROM gncEntry;");

   pgendEnable(be);
   gnc_engine_resume_events();
}

/* ============================================================= */
/* return TRUE if this appears to be a fresh, 'null' transaction */
/* it would be better is somehow we could get the gui to mark this
 * as a fresh transaction, rather than having to scan a bunch of 
 * fields.  But this is minor in the scheme of things.
 */

static gboolean
is_trans_empty (Transaction *trans)
{
   Split *s;
   if (!trans) return TRUE;
   if (0 != (xaccTransGetDescription(trans))[0]) return FALSE;
   if (0 != (xaccTransGetNum(trans))[0]) return FALSE;
   if (1 != xaccTransCountSplits(trans)) return FALSE;

   s = xaccTransGetSplit(trans, 0);
   if (TRUE != gnc_numeric_zero_p(xaccSplitGetShareAmount(s))) return FALSE;
   if (TRUE != gnc_numeric_zero_p(xaccSplitGetValue(s))) return FALSE;
   if ('n' != xaccSplitGetReconcile(s)) return FALSE;
   if (0 != (xaccSplitGetMemo(s))[0]) return FALSE;
   if (0 != (xaccSplitGetAction(s))[0]) return FALSE;
   return TRUE;
}

/* ============================================================= */

static int
pgend_trans_commit_edit (Backend * bend, 
                         Transaction * trans,
                         Transaction * oldtrans)
{
   char * bufp;
   int i, ndiffs, nsplits, rollback=0;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, trans=%p", be, trans);
   if (!be || !trans) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   /* its not at all clear to me that this isn't rife with deadlocks. */
   bufp = "BEGIN; "
          "LOCK TABLE gncTransaction IN EXCLUSIVE MODE; "
          "LOCK TABLE gncEntry IN EXCLUSIVE MODE; "
          "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
          "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; ";
   SEND_QUERY (be,bufp, 555);
   FINISH_QUERY(be->connection);

   /* Check to see if this is a 'new' transaction, or not. 
    * The hallmark of a 'new' transaction is that all the 
    * fields are empty.  If its new, then we just go ahead 
    * and commit.  If its old, then we need some consistency 
    * checks.
    */
   if (FALSE == is_trans_empty (oldtrans))
   {
      /* See if the database is in the state that we last left it in.
       * Basically, the database should contain the 'old transaction'.
       * If it doesn't, then someone else has modified this transaction,
       * and thus, any further action on our part would be unsafe.  It
       * is recommended that this be spit back at the GUI, and let a 
       * human decide what to do next.
       */
      ndiffs = pgendCompareOneTransactionOnly (be, oldtrans); 
      if (0 < ndiffs) rollback++;
   
      /* be sure to check the old splits as well ... */
      nsplits = xaccTransCountSplits (oldtrans);
      for (i=0; i<nsplits; i++) {
         Split * s = xaccTransGetSplit (oldtrans, i);
         ndiffs = pgendCompareOneSplitOnly (be, s);
         if (0 < ndiffs) rollback++;
      }
   
      if (rollback) {
         bufp = "ROLLBACK;";
         SEND_QUERY (be,bufp,444);
         FINISH_QUERY(be->connection);
   
         PWARN ("Some other user changed this transaction. Please\n"
                "refresh your GUI, type in your changes and try again.\n"
                "(old tranasction didn't match DB, edit rolled back)\n");
         return 666;   /* hack alert */
      } 
   }

   /* if we are here, we are good to go */
   pgendStoreTransactionNoLock (be, trans, FALSE);

   bufp = "COMMIT;";
   SEND_QUERY (be,bufp,333);
   FINISH_QUERY(be->connection);

   /* hack alert -- the following code will get rid of that annoying
    * message from the GUI about saving one's data. However, it doesn't
    * do the right thing if the connection to the backend was ever lost.
    * what should happen is the user should get a chance to
    * resynchronize thier data with the backend, before quiting out.
    */
   {
      Split * s = xaccTransGetSplit (trans, 0);
      Account *acc = xaccSplitGetAccount (s);
      AccountGroup *top = xaccGetAccountRoot (acc);
      xaccGroupMarkSaved (top);
   }

   LEAVE ("commited");
   return 0;
}

/* ============================================================= */

static int
pgend_account_commit_edit (Backend * bend, 
                           Account * acct)
{
   char * bufp;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, acct=%p", be, acct);
   if (!be || !acct) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   /* its not at all clear to me that this isn't rife with deadlocks. */
   bufp = "BEGIN; "
          "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
          "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; ";

   SEND_QUERY (be,bufp, 555);
   FINISH_QUERY(be->connection);

   /* hack alert -- we aren't comparing old to new, 
    * i.e. not comparing version numbers, to see if 
    * we're clobbering someone elses changes.  */
   pgendStoreAccountNoLock (be, acct, FALSE);

   bufp = "COMMIT;";
   SEND_QUERY (be,bufp,333);
   FINISH_QUERY(be->connection);

   /* Mark this up so that we don't get that annoying gui dialog
    * about having to save to file.  unfortunately,however, this
    * is too liberal, and could screw up synchronization if we've lost
    * contact with the back end at some point.  So hack alert -- fix 
    * this. */
   xaccGroupMarkSaved (xaccAccountGetParent(acct));
   LEAVE ("commited");
   return 0;
}

/* ============================================================= */

static const char *
pgendSessionGetMode (PGBackend *be)
{
   switch (be->session_mode)
   {
      case MODE_SINGLE_FILE:
         return "SINGLE-FILE";
      case MODE_SINGLE_UPDATE:
         return "SINGLE-UPDATE";
      case MODE_POLL:
         return "POLL";
      case MODE_EVENT:
         return "EVENT";
      default:
   }
   return "ERROR";
}

/* ============================================================= */
/* Determine whether we can start a session of the desired type.
 * The logic used is as follows:
 * -- if there is any session at all, and we want single
 *    (exclusive) access, then fail.
 * -- if we want any kind of session, and there is a single
 *    (exclusive) session going, then fail.
 * -- otherwise, suceed.
 * Return TRUE if we can get a session.
 *
 * This routine does not lock, but may be used inside a 
 * test-n-set atomic operation.
 */

static gboolean
pgendSessionCanStart (PGBackend *be)
{
   gboolean retval = TRUE;
   PGresult *result;
   int i, nrows;
   char *p;

   ENTER (" ");
   /* Find out if there are any open sessions.
    * If 'time_off' is infinity, then user hasn't logged off yet  */
   p = "SELECT * FROM gncSession "
       "WHERE time_off='INFINITY';";
   SEND_QUERY (be,p, FALSE);
  
   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         for (j=0; j<jrows; j++)
         {
            char * mode = DB_GET_VAL("session_mode", j);

            if ((MODE_SINGLE_FILE == be->session_mode) ||
                (MODE_SINGLE_UPDATE == be->session_mode) ||
                (0 == strcasecmp (mode, "SINGLE-FILE")) ||
                (0 == strcasecmp (mode, "SINGLE-UPDATE")))
            {
               char * hostname = DB_GET_VAL("hostname", j);
               char * username = DB_GET_VAL("login_name",j);
               char * gecos = DB_GET_VAL("gecos",j);
               char * datestr = DB_GET_VAL("time_on", j);
         
               PWARN ("This database is already opened by \n"
                      "\t%s@%s (%s) in mode %s on %s \n",
                      username, hostname, gecos, mode, datestr);
         
               retval = FALSE;
            }
         }
      }
      PQclear (result);
      i++;
   } while (result);

   LEAVE (" ");
   return retval;
}


/* ============================================================= */
/* Determine whether a valid session could be obtained.
 * Return TRUE if we have a session
 * This routine is implemented attomically as a test-n-set.
 */

static gboolean
pgendSessionValidate (PGBackend *be)
{
   gboolean retval = FALSE;
   char *p;
   ENTER(" ");

   if (MODE_NONE == be->session_mode) return FALSE;

   /* Lock it up so that we test-n-set atomically 
    * i.e. we want to avoid a race condition when testing
    * for the single-user session.
    */
   p = "BEGIN;"
       "LOCK TABLE gncSession IN EXCLUSIVE MODE; ";
   SEND_QUERY (be,p, FALSE);
   FINISH_QUERY(be->connection);

   /* check to see if we can start a session of the desired type.  */
   if (FALSE == pgendSessionCanStart (be))
   {
      /* this error should be treated just like the 
       * file-lock error from the GUI perspective */
      xaccBackendSetError (&be->be, ERR_SQL_BUSY);
      retval = FALSE;
   } else {

      /* make note of the session. */
      be->sessionGuid = xaccGUIDMalloc();
      guid_new (be->sessionGuid);
      pgendStoreOneSessionOnly (be, (void *)-1, SQL_INSERT);
      retval = TRUE;
   }

   p = "COMMIT;";
   SEND_QUERY (be,p, FALSE);
   FINISH_QUERY(be->connection);

   LEAVE(" ");
   return retval;
}

/* ============================================================= */
/* log end of session in the database. */

static void
pgendSessionEnd (PGBackend *be)
{
   char *p;

   if (!be->sessionGuid) return;

   p = be->buff; *p=0;
   p = stpcpy (p, "UPDATE gncSession SET time_off='NOW' "
                  "WHERE sessionGuid='");
   p = guid_to_string_buff (be->sessionGuid, p);
   p = stpcpy (p, "';");
  
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

   xaccGUIDFree (be->sessionGuid); be->sessionGuid = NULL;
}

/* ============================================================= */

static void
pgend_session_end (Backend *bend)
{
   PGBackend *be = (PGBackend *)bend;
   if (!be) return;

   ENTER("be=%p", be);

   /* prevent further callbacks into backend */
   pgendDisable(be);

   /* note the logoff time in the session directory */
   pgendSessionEnd (be);

   /* disconnect from the backend */
   if(be->connection) PQfinish (be->connection);
   be->connection = 0;

   if (be->dbName) { g_free(be->dbName); be->dbName = NULL; }
   if (be->portno) { g_free(be->portno); be->portno = NULL; }
   if (be->hostname) { g_free(be->hostname); be->hostname = NULL; }

   sqlBuilder_destroy (be->builder); be->builder = NULL;
   g_free (be->buff); be->buff = NULL; 

   LEAVE("be=%p", be);
}

/* ============================================================= */
/* the poll & event style load only loads accounts, never the
 * transactions. */

static AccountGroup *
pgend_book_load_poll (Backend *bend)
{
   AccountGroup *grp;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callaback to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   grp = pgendGetAllAccounts (be);
   pgendGroupGetAllCheckpoints (be, grp);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   return grp;
}

/* ============================================================= */
/* The single-user mode loads all transactions.  Doesn't bother
 * with checkpoints */

static AccountGroup *
pgend_book_load_single (Backend *bend)
{
   AccountGroup *grp;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callaback to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   grp = pgendGetAllAccounts (be);
   pgendGetAllTransactions (be, grp);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   return grp;
}

/* ============================================================= */

static void
pgend_session_begin (GNCBook *sess, const char * sessionid)
{
   int rc;
   PGBackend *be;
   char *url, *start, *end;
   char * bufp;

   if (!sess) return;
   be = (PGBackend *) xaccGNCBookGetBackend (sess);

   ENTER("be=%p, sessionid=%s", be, sessionid);

   /* close any dangling sessions from before; reinitialize */
   pgend_session_end ((Backend *) be);
   pgendInit (be);

   /* connect to a bogus database ... */
   /* Parse the sessionid for the hostname, port number and db name.
    * The expected URL format is
    * postgres://some.host.com/db_name
    * postgres://some.host.com:portno/db_name
    * postgres://localhost/db_name
    * postgres://localhost:nnn/db_name
    * 
    * In the future, we might parse urls of the form
    * postgres://some.host.com/db_name?pgkey=pgval&pgkey=pgval
    * 
    */

   if (strncmp (sessionid, "postgres://", 11)) return;
   url = g_strdup(sessionid);
   start = url + 11;
   end = strchr (start, ':');
   if (end) 
   {
     /* if colon found, then extract port number */
     *end = 0x0;
     be->hostname = g_strdup (start);
     start = end+1;
     end = strchr (start, '/');
     if (!end) { g_free(url); return; }
     *end = 0;
     be->portno = g_strdup (start);
   } 
   else 
   {
     end = strchr (start, '/');
     if (!end) { g_free(url); return; }
     *end = 0;
     be->hostname = g_strdup (start);
   }
   start = end+1;
   if (0x0 == *start) { g_free(url); return; }

   /* chop of trailing url-encoded junk, if present */
   end = strchr (start, '?');
   if (end) *end = 0;
   be->dbName = g_strdup (start);

   g_free(url);

   /* handle localhost as a special case */
   if (!safe_strcmp("localhost", be->hostname))
   {
      g_free (be->hostname);
      be->hostname = NULL;
   }

   be->connection = PQsetdbLogin (be->hostname, 
                                  be->portno,
                                  NULL, /* trace/debug options */
                                  NULL, /* file or tty for debug output */
                                  be->dbName, 
                                  NULL,  /* login */
                                  NULL);  /* pwd */

   /* check the connection status */
   if (CONNECTION_BAD == PQstatus(be->connection))
   {
      PERR("Connection to database '%s' failed:\n"
           "\t%s", 
           be->dbName, PQerrorMessage(be->connection));
      PQfinish (be->connection);
      xaccBackendSetError (&be->be, ERR_SQL_CANT_CONNECT);
      return;
   }

   // DEBUGCMD (PQtrace(be->connection, stderr));

   /* set the datestyle to something we can parse */
   bufp = "SET DATESTYLE='ISO';";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   /* OK, lets see if we can get a valid session */
   /* hack alert -- we hard-code the access mode here,
    * but it should be user-adjustable.  */
   be->session_mode = MODE_SINGLE_UPDATE;
   rc = pgendSessionValidate (be);

   /* set up pointers for appropriate behaviour */
   /* In single mode, we load all transactions right away.
    *    and we never have to query the database.
    */
   if (rc)
   {
      switch (be->session_mode)
      {
         case MODE_SINGLE_FILE:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_single;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = NULL;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = NULL;
            be->be.trans_rollback_edit = NULL;
            be->be.run_query = NULL;
            be->be.sync = pgendSyncSingleFile;
            PWARN ("MODE_SINGLE_FILE is experimental");
            break;

         case MODE_SINGLE_UPDATE:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_single;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = pgend_account_commit_edit;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = pgend_trans_commit_edit;
            be->be.trans_rollback_edit = NULL;
            be->be.run_query = NULL;
            be->be.sync = pgendSync;
            PWARN ("MODE_SINGLE_UPDATE is experimental");
            break;

         case MODE_POLL:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_poll;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = pgend_account_commit_edit;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = pgend_trans_commit_edit;
            be->be.trans_rollback_edit = NULL;
            be->be.run_query = pgendRunQuery;
            be->be.sync = pgendSync;
            PWARN ("MODE_EVENT is experimental");
            break;

         case MODE_EVENT:
            PERR ("MODE_EVENT is unimplemented");
            break;

         default:
            PERR ("bad mode specified");
            break;
      }
   }

   LEAVE("be=%p, sessionid=%s", be, sessionid);
}

/* ============================================================= */

static void
pgendDisable (PGBackend *be)
{
   if (0 > be->nest_count)
   {
      PERR ("too many nested enables");
   }
   be->nest_count ++;
   PINFO("nest count=%d", be->nest_count);
   if (1 < be->nest_count) return;

   /* save hooks */
   be->snr.account_begin_edit  = be->be.account_begin_edit;
   be->snr.account_commit_edit = be->be.account_commit_edit;
   be->snr.trans_begin_edit    = be->be.trans_begin_edit;
   be->snr.trans_commit_edit   = be->be.trans_commit_edit;
   be->snr.trans_rollback_edit = be->be.trans_rollback_edit;
   be->snr.run_query           = be->be.run_query;
   be->snr.sync                = be->be.sync;

   be->be.account_begin_edit  = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit    = NULL;
   be->be.trans_commit_edit   = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.run_query           = NULL;
   be->be.sync                = NULL;
}

/* ============================================================= */

static void
pgendEnable (PGBackend *be)
{
   if (0 >= be->nest_count)
   {
      PERR ("too many nested disables");
   }
   be->nest_count --;
   PINFO("nest count=%d", be->nest_count);
   if (be->nest_count) return;

   /* restore hooks */
   be->be.account_begin_edit  = be->snr.account_begin_edit;
   be->be.account_commit_edit = be->snr.account_commit_edit;
   be->be.trans_begin_edit    = be->snr.trans_begin_edit;
   be->be.trans_commit_edit   = be->snr.trans_commit_edit;
   be->be.trans_rollback_edit = be->snr.trans_rollback_edit;
   be->be.run_query           = be->snr.run_query;
   be->be.sync                = be->snr.sync;

}

/* ============================================================= */

static void 
pgendInit (PGBackend *be)
{
   /* access mode */
   be->session_mode = MODE_NONE;
   be->sessionGuid = NULL;

   /* generic backend handlers */
   be->be.book_begin = pgend_session_begin;
   be->be.book_load = NULL;
   be->be.book_end = pgend_session_end;

   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.run_query = NULL;
   be->be.sync = NULL;

   be->nest_count = 0;
   pgendDisable(be);

   be->be.last_err = ERR_BACKEND_NO_ERR;

   /* postgres specific data */
   be->hostname = NULL;
   be->portno = NULL;
   be->dbName = NULL;
   be->connection = NULL;

   be->builder = sqlBuilder_new();

   be->buff = g_malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;
}

/* ============================================================= */

Backend * 
pgendNew (void)
{
   PGBackend *be;
   be = (PGBackend *) g_malloc (sizeof (PGBackend));
   pgendInit (be);
   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

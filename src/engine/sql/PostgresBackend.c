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
#include <stdio.h>  
#include <string.h>  

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
         PERR("bad status");					\
         PQclear(result);					\
         PQfinish ((conn));					\
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
      PERR ("failed to get result to query");			\
      PQclear (result);						\
      /* hack alert need gentler, kinder error recovery */	\
      PQfinish (conn);						\
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
      break;							\
   } else if (1 == nrows) 

/* --------------------------------------------------------------- */
/* Some utility macros for comparing values returned from the
 * database to values in the engine structs.
 */

#define GET_DB_VAL(str,n) (PQgetvalue (result, n, PQfnumber (result, str)))

#define COMP_STR(sqlname,fun,ndiffs) { 				\
   if (null_strcmp (GET_DB_VAL(sqlname,0),fun)) {		\
      PINFO("%s sql='%s', eng='%s'", sqlname, 			\
         GET_DB_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

#define COMP_GUID(sqlname,fun, ndiffs) { 			\
   const char *tmp = guid_to_string(fun); 			\
   if (null_strcmp (GET_DB_VAL(sqlname,0),tmp)) { 		\
      PINFO("%s sql='%s', eng='%s'", sqlname, 			\
         GET_DB_VAL(sqlname,0), tmp); 				\
      ndiffs++; 						\
   }								\
   free ((char *) tmp); 					\
} 

/* comapre one char only */
#define COMP_CHAR(sqlname,fun, ndiffs) { 			\
    if (tolower((GET_DB_VAL(sqlname,0))[0]) != tolower(fun)) {	\
       PINFO("%s sql=%c eng=%c", sqlname, 			\
         tolower((GET_DB_VAL(sqlname,0))[0]), tolower(fun)); 	\
      ndiffs++; 						\
   }								\
}

/* assumes the datestring is in ISO-8601 format 
 * i.e. looks like 1998-07-17 11:00:00.68-05  
 * hack-alert doesn't compare nano-seconds ..  
 * that's becuase I suspect the sql db round nanoseconds off ... 
 */
#define COMP_DATE(sqlname,fun,ndiffs) { 			\
    Timespec eng_time = fun;					\
    Timespec sql_time = gnc_iso8601_to_timespec(		\
                     GET_DB_VAL(sqlname,0)); 			\
    if (eng_time.tv_sec != sql_time.tv_sec) {			\
       time_t tmp = eng_time.tv_sec;				\
       PINFO("%s sql='%s' eng=%s", sqlname, 			\
         GET_DB_VAL(sqlname,0), ctime(&tmp)); 			\
      ndiffs++; 						\
   }								\
}

/* a very special date comp */
#define COMP_NOW(sqlname,fun,ndiffs) { 	 			\
    Timespec eng_time = xaccTransRetDateEnteredTS(ptr);		\
    Timespec sql_time = gnc_iso8601_to_timespec(		\
                     GET_DB_VAL(sqlname,0)); 			\
    if (eng_time.tv_sec != sql_time.tv_sec) {			\
       time_t tmp = eng_time.tv_sec;				\
       PINFO("%s sql='%s' eng=%s", sqlname, 			\
         GET_DB_VAL(sqlname,0), ctime(&tmp)); 			\
      ndiffs++; 						\
   }								\
}


#define COMP_INT64(sqlname,fun,ndiffs) { 			\
   if (atoll (GET_DB_VAL(sqlname,0)) != fun) {			\
      PINFO("%s sql='%s', eng='%lld'", sqlname, 		\
         GET_DB_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

#define COMP_INT32(sqlname,fun,ndiffs) { 			\
   if (atol (GET_DB_VAL(sqlname,0)) != fun) {			\
      PINFO("%s sql='%s', eng='%d'", sqlname, 			\
         GET_DB_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

/* ============================================================= */

#include "tmp.c"

/* ============================================================= */
/* This routine updates the commodity structure if needed, and/or
 * stores it the first time if it hasn't yet been stored.
 */

static void
pgendStoreCommodityNoLock (PGBackend *be, const gnc_commodity *com)
{
   gnc_commodity *commie = (gnc_commodity *) com;
   int ndiffs;
   if (!be || !com) return;

   ndiffs = pgendCompareOnegnc_commodityOnly (be, commie);

   /* update commodity if there are differences ... */
   if (0<ndiffs) pgendStoreOnegnc_commodityOnly (be, commie, SQL_UPDATE);
   /* insert commodity if it doesn't exist */
   if (0>ndiffs) pgendStoreOnegnc_commodityOnly (be, commie, SQL_INSERT);

   LEAVE(" ");
}

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
   int ndiffs;

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

   ndiffs = pgendCompareOneAccountOnly (be, acct);

   /* update account if there are differences ... */
   if (0<ndiffs) pgendStoreOneAccountOnly (be, acct, SQL_UPDATE);
   /* insert account if it doesn't exist */
   if (0>ndiffs) pgendStoreOneAccountOnly (be, acct, SQL_INSERT);

   /* make sure the account's commodity is in the commodity table */
   com = xaccAccountGetCommodity (acct);
   pgendStoreCommodityNoLock (be, com);

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
   int i, ndiffs, nsplits;

   if (!be || !trans) return;
   ENTER ("trans=%p, mark=%d", trans, do_mark);

   ndiffs = pgendCompareOneTransactionOnly (be, trans);

   /* update transaction if there are differences ... */
   if (0<ndiffs) pgendStoreOneTransactionOnly (be, trans, SQL_UPDATE);
   /* insert trans if it doesn't exist */
   if (0>ndiffs) pgendStoreOneTransactionOnly (be, trans, SQL_INSERT);

   /* walk over the list of splits */
   nsplits = xaccTransCountSplits (trans);
   for (i=0; i<nsplits; i++) {
      Split * s = xaccTransGetSplit (trans, i);
      Account *acct = xaccSplitGetAccount (s);

      ndiffs = pgendCompareOneSplitOnly (be, s);
      /* update split if there are differences ... */
      if (0<ndiffs) pgendStoreOneSplitOnly (be, s, SQL_UPDATE);
      /* insert split if it doesn't exist */
      if (0>ndiffs) pgendStoreOneSplitOnly (be, s, SQL_INSERT);

      /* check to see if the account that this split references is in
       * storage; if not, add it */
      pgendStoreAccountNoLock (be, acct, do_mark);
   }
   LEAVE(" ");
}

static void
pgendStoreTransaction (PGBackend *be, Transaction *trans)
{
   if (!be || !trans) return;
   ENTER ("be=%p, trans=%p", be, trans);

   /* lock it up so that we store atomically */
   snprintf (be->buff, be->bufflen, "BEGIN;"
             "LOCK TABLE gncTransaction IN EXCLUSIVE MODE; "
             "LOCK TABLE gncEntry IN EXCLUSIVE MODE; "
             "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
             "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; "
             );
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

   pgendStoreTransactionNoLock (be, trans, FALSE);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff, );
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
   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return;

   /* lock it up so that we store atomically */
   snprintf (be->buff, be->bufflen, "BEGIN;");
   SEND_QUERY (be,be->buff, );
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

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}

static void
pgendSync (Backend *bend, AccountGroup *grp)
{
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, grp=%p", be, grp);

   /* hack alert -- this is *not* the correct implementation
    * of what they synchronize function is supposed to do. 
    * This is a sick placeholder.
    */
   pgendStoreGroup (be, grp);
   LEAVE(" ");
}

/* ============================================================= */
/* This routine returns the update Transaction structure 
 * associated with the GUID.  Data is pulled out of the database,
 * the versions are compared, and updates made, if needed.
 * The splits are handled as well ...
 *
 * hack alert unfinished, incomplete 
 */

static void
pgendSyncTransaction (PGBackend *be, GUID *trans_guid)
{
   GUID nullguid = *(xaccGUIDNULL());
   char qbuff[120], *pbuff;
   Transaction *trans;
   char trans_guid_str[GUID_ENCODING_LENGTH+1];
   PGresult *result;
   Account *acc, *previous_acc=NULL;
   gboolean do_set_guid=FALSE;
   gboolean engine_data_is_newer = FALSE;
   int i, j, nrows;
   GList *node, *db_splits=NULL, *engine_splits, *delete_splits=NULL;
   
   ENTER ("be=%p", be);
   if (!be || !trans_guid) return;

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
   guid_to_string_buff(trans_guid, trans_guid_str);
   pbuff = qbuff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncTransaction WHERE transGuid='");
   pbuff = stpcpy (pbuff, trans_guid_str);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,qbuff, );
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
             PERR ("!!!!!!!!!!!SQL database is corrupt!!!!!!!\n"
                   "too many transactions with GUID=%s\n",
                    trans_guid_str);
             if (jrows != nrows) xaccTransCommitEdit (trans);
             return;
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
            db_ts = gnc_iso8601_to_timespec (GET_DB_VAL("date_entered",j));
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
            xaccTransSetNum (trans, GET_DB_VAL("num",j));
            xaccTransSetDescription (trans, GET_DB_VAL("description",j));
            ts = gnc_iso8601_to_timespec (GET_DB_VAL("date_posted",j));
            xaccTransSetDatePostedTS (trans, &ts);
            ts = gnc_iso8601_to_timespec (GET_DB_VAL("date_entered",j));
            xaccTransSetDateEnteredTS (trans, &ts);
         }
         else
         {
            /* XXX hack alert -- fixme */
            PERR ("Data in the local cache is newer than the data in\n"
                  "\tthe database.  Thus, the local data will be sent\n"
                  "\tto the database.  This mode of operation is\n"
                  "\tguerenteed to clobber other user's updates\n");

            /* basically, we should use the pgend_commit_transaction
             * routine instead, and in fact, 'StoreTransaction'
             * pretty much shouldn't be allowed to exist in this
             * framework */
            pgendStoreTransaction (be, trans);
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
      return;
   }

   /* if engine data was newer, we should be done at this point */
   if (TRUE == engine_data_is_newer) return;

   /* build the sql query the splits */
   pbuff = qbuff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncEntry WHERE transGuid='");
   pbuff = stpcpy (pbuff, trans_guid_str);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,qbuff, );
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
            PINFO ("split GUID=%s", GET_DB_VAL("entryGUID",j));
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (GET_DB_VAL("entryGUID",j), &guid);
            s = (Split *) xaccLookupEntity (&guid, GNC_ID_SPLIT);
            if (!s)
            {
               s = xaccMallocSplit();
               xaccSplitSetGUID(s, &guid);
            }

            /* next, restore some split data */
            /* hack alert - not all split fields handled */
            xaccSplitSetMemo(s, GET_DB_VAL("memo",j));
            xaccSplitSetAction(s, GET_DB_VAL("action",j));
            ts = gnc_iso8601_to_timespec (GET_DB_VAL("date_reconciled",j));
            xaccSplitSetDateReconciledTS (s, &ts);

            num = atoll (GET_DB_VAL("amountNum", j));
            denom = atoll (GET_DB_VAL("amountDenom", j));
            amount = gnc_numeric_create (num, denom);
            xaccSplitSetShareAmount (s, amount);

            num = atoll (GET_DB_VAL("valueNum", j));
            denom = atoll (GET_DB_VAL("valueDenom", j));
            value = gnc_numeric_create (num, denom);
            xaccSplitSetValue (s, value);

            xaccSplitSetReconcile (s, (GET_DB_VAL("reconciled", j))[0]);

            xaccTransAppendSplit (trans, s);

            /* --------------------------------------------- */
            /* next, find the account that this split goes into */
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (GET_DB_VAL("accountGUID",j), &guid);
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

   xaccTransCommitEdit (trans);

   /* reneable events to the backend and GUI */
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
   char * buff;
   int i, nrows;

   ENTER ("be=%p", be);
   if (!be) return;

   if (!comtab) {
      PERR ("can't get global commodity table");
      return;
   }

   /* Get them ALL */
   buff = "SELECT * FROM gncCommodity;";
   SEND_QUERY (be, buff, );

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
                     GET_DB_VAL("namespace",j), GET_DB_VAL("mnemonic",j));

            if (com) continue;
            /* no we don't ... restore it */
            com = gnc_commodity_new (
                     GET_DB_VAL("fullname",j), 
                     GET_DB_VAL("namespace",j), 
                     GET_DB_VAL("mnemonic",j),
                     GET_DB_VAL("code",j),
                     atoi(GET_DB_VAL("fraction",j)));

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
   gnc_commodity_table *comtab = gnc_engine_commodities();
   PGresult *result;
   AccountGroup *topgrp;
   char * buff;
   int i, nrows, iacc;
   GUID nullguid = *(xaccGUIDNULL());

   ENTER ("be=%p", be);
   if (!be) return NULL;

   /* first, make sure commodities table is up to date */
   pgendGetAllCommodities (be);

   /* Get them ALL */
   buff = "SELECT * FROM gncAccount;";
   SEND_QUERY (be, buff, NULL);

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
	    PINFO ("account GUID=%s", GET_DB_VAL("accountGUID",j));
	    guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (GET_DB_VAL("accountGUID",j), &guid);
            acc = (Account *) xaccLookupEntity (&guid, GNC_ID_ACCOUNT);
            if (!acc) 
            {
               acc = xaccMallocAccount();
               xaccAccountBeginEdit(acc);
               xaccAccountSetGUID(acc, &guid);
            }

            xaccAccountSetName(acc, GET_DB_VAL("accountName",j));
            xaccAccountSetDescription(acc, GET_DB_VAL("description",j));
            xaccAccountSetCode(acc, GET_DB_VAL("accountCode",j));
            xaccAccountSetType(acc, xaccAccountStringToEnum(GET_DB_VAL("type",j)));

            /* hop through a couple of hoops for the commodity */
            /* it would be nice to simplify this ... */
            {
               gnc_commodity *com;
               char *str, *name;

               str = g_strdup(GET_DB_VAL("commodity",j));
               name = strchr (str, ':');
               *name = 0;
               name += 2;

               com = gnc_commodity_table_lookup(comtab, str, name);
PINFO ("found %p for %s-%s", com, str, name);
PINFO ("found %p is for %s", com, 
gnc_commodity_get_unique_name(com));

               xaccAccountSetCommodity(acc, com);
               g_free (str);
            }

            /* try to find the parent account */
	    PINFO ("parent GUID=%s", GET_DB_VAL("parentGUID",j));
	    guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (GET_DB_VAL("parentGUID",j), &guid);
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
   int i, ndiffs, nsplits, rollback=0;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, trans=%p", be, trans);
   if (!be || !trans) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   /* its not at all clear to me that this isn't rife with deadlocks. */
   snprintf (be->buff, be->bufflen, 
             "BEGIN; "
             "LOCK TABLE gncTransaction IN EXCLUSIVE MODE; "
             "LOCK TABLE gncEntry IN EXCLUSIVE MODE; "
             "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
             "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; "
             );
   SEND_QUERY (be,be->buff, 555);
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
         snprintf (be->buff, be->bufflen, "ROLLBACK;");
         SEND_QUERY (be,be->buff,444);
         FINISH_QUERY(be->connection);
   
         LEAVE ("rolled back");
         return 666;   /* hack alert */
      } 
   }

   /* if we are here, we are good to go */
   pgendStoreTransactionNoLock (be, trans, FALSE);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff,333);
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
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, acct=%p", be, acct);
   if (!be || !acct) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   /* its not at all clear to me that this isn't rife with deadlocks. */
   snprintf (be->buff, be->bufflen, 
             "BEGIN; "
             "LOCK TABLE gncAccount IN EXCLUSIVE MODE; "
             "LOCK TABLE gncCommodity IN EXCLUSIVE MODE; "
             );
   SEND_QUERY (be,be->buff, 555);
   FINISH_QUERY(be->connection);

   /* hack alert -- we aren't comparing old to new, 
    * i.e. not comparing version numbers, to see if 
    * we're clobbering someone elses changes.  */
   pgendStoreAccountNoLock (be, acct, FALSE);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff,333);
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

static void
pgend_session_end (Backend *bend)
{
   PGBackend *be = (PGBackend *)bend;
   if (!be) return;

   ENTER("be=%p", be);

   /* prevent further callbacks into backend */
   pgendDisable(be);

   /* disconnect from the backend */
   if(be->connection) PQfinish (be->connection);
   be->connection = 0;

   if (be->dbName) { g_free(be->dbName); be->dbName = NULL; }
   if (be->portno) { g_free(be->portno); be->portno = NULL; }
   if (be->hostname) { g_free(be->hostname); be->hostname = NULL; }

   LEAVE("be=%p", be);
}

/* ============================================================= */

static void
pgend_session_begin (GNCBook *sess, const char * sessionid)
{
   PGBackend *be;
   char *url, *start, *end;

   if (!sess) return;
   be = (PGBackend *) xaccGNCBookGetBackend (sess);

   ENTER("be=%p, sessionid=%s", be, sessionid);

   /* close any dangling sessions from before */
   pgend_session_end ((Backend *) be);
   pgendEnable(be);  /* re-enable after session end */

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
      return;
   }

   // DEBUGCMD (PQtrace(be->connection, stderr));

   /* set the datestyle to something we can parse */
   snprintf (be->buff, be->bufflen, "SET DATESTYLE='ISO';");
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

   LEAVE("be=%p, sessionid=%s", be, sessionid);
}

/* ============================================================= */

static AccountGroup *
pgend_book_load (Backend *bend)
{
   AccountGroup *grp;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callaback to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   grp = pgendGetAllAccounts (be);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   return grp;
}

/* ============================================================= */
/* This routine performs a search on the SQL database for all of 
 * the splits that correspond to gnc-style query, and then integrates
 * them into the engine cache.  It does this in several steps:
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
 */

static void 
pgendRunQuery (Backend *bend, Query *q)
{
   PGBackend *be = (PGBackend *)bend;
   GUID nullguid = *(xaccGUIDNULL());
   const char * sql_query_string;
   sqlQuery *sq;
   PGresult *result;
   int i, nrows;
   GList *node, *xact_list = NULL;

   ENTER (" ");
   if (!be || !q) return;

   gnc_engine_suspend_events();
   pgendDisable(be);

   /* first thing we do is convert the gnc-engine query into
    * an sql string. */
   sq = sqlQuery_new();
   sql_query_string = sqlQuery_build (sq, q);
   PINFO ("string=%s\n", sql_query_string);

   SEND_QUERY (be,sql_query_string, );

   sql_Query_destroy(sq);

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
            string_to_guid (GET_DB_VAL("transGUID",j), trans_guid);
            xact_list = g_list_prepend (xact_list, trans_guid);
         }
      }

      PQclear (result);
      i++;
   } while (result);

   /* restore the transactions */
   for (node=xact_list; node; node=node->next)
   {
      pgendSyncTransaction (be, (GUID *)node->data);
      xaccGUIDFree (node->data);
   }
   g_list_free(xact_list);

   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
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
   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.run_query = NULL;
   be->be.sync = NULL;
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
   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = pgend_account_commit_edit;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = pgend_trans_commit_edit;
   be->be.trans_rollback_edit = NULL;
   be->be.run_query = pgendRunQuery;
   be->be.sync = pgendSync;
}

/* ============================================================= */

/* hack alert -- this is the query buffer, it can be overflowed.
 * we need to make it dynamic sized.  On the other hand, Postgres
 * rejects queries longer than 8192 bytes,(according to the
 * documentation) so... 
 */
#define QBUFSIZE 16350

Backend * 
pgendNew (void)
{
   PGBackend *be;

   be = (PGBackend *) g_malloc (sizeof (PGBackend));

   /* generic backend handlers */
   be->be.book_begin = pgend_session_begin;
   be->be.book_load = pgend_book_load;
   be->be.book_end = pgend_session_end;

   be->nest_count = 1;
   pgendEnable(be);

   be->be.last_err = ERR_BACKEND_NO_ERR;

   /* postgres specific data */
   be->hostname = NULL;
   be->portno = NULL;
   be->dbName = NULL;
   be->connection = NULL;

   be->builder = sqlBuilder_new();

   be->buff = g_malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

/* 
 * FILE:
 * PostgressBackend.c
 *
 * FUNCTION:
 * Implements the callbacks for the postgress backend.
 * this is somewhat broken code.
 * its a quick hack just to check things out.
 * it needs extensive review and design checking
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas
 * 
 */

#include <pgsql/libpq-fe.h>  
#include <stdio.h>  
#include <string.h>  

#include <glib.h>

#include "AccountP.h"
#include "BackendP.h"
#include "builder.h"
#include "Group.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "guid.h"
#include "GNCId.h"
#include "GNCIdP.h"
#include "TransactionP.h"

#include "PostgresBackend.h"

static short module = MOD_BACKEND; 

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
   PGresult *result; 						\
   /* complete/commit the transaction, check the status */	\
   do {								\
      ExecStatusType status;					\
      result = PQgetResult((conn));				\
      if (!result) break;					\
      PINFO ("got result");					\
      status = PQresultStatus(result);  			\
      if (PGRES_COMMAND_OK != status) {				\
         PERR("bad status");					\
         PQclear(result);					\
         PQfinish ((conn));					\
      }								\
      PQclear(result);						\
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
   if (strcmp (GET_DB_VAL(sqlname,0),fun)) {			\
      PINFO("%s sql='%s', eng='%s'", sqlname, 			\
         GET_DB_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}

#define COMP_GUID(sqlname,fun, ndiffs) { 			\
   const char *tmp = guid_to_string(fun); 			\
   if (strcmp (GET_DB_VAL(sqlname,0),tmp)) { 			\
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

#define COMP_INT64(sqlname,fun,ndiffs) { 			\
   if (atoll (GET_DB_VAL(sqlname,0)) != fun) {			\
      PINFO("%s sql='%s', eng='%lld'", sqlname, 		\
         GET_DB_VAL (sqlname,0), fun); 				\
      ndiffs++; 						\
   }								\
}
/* ============================================================= */

#include "tmp.c"

/* ============================================================= */
/* This routine updates the account structure if needed, and/or
 * stores it the first time if it hasn't yet been stored.
 * Note that it sets a mark to avoid excessive recursion:
 * This routine shouldn't be used outside of locks,
where the recursion prevention clears the marks ...
 */

static void
pgendStoreAccountMarkNoLock (PGBackend *be, Account *acct)
{
   int ndiffs;

   if (!be || !acct) return;

   /* Check to see if we've processed this account recently.
    * If so, then return.  The goal here is to avoid excess
    * hits to the database, leading to poor performance.
    * Note that this marking makes this routine unsafe to use 
    * outside a lock (since we never clear the mark)
    */
   if (xaccAccountGetMark (acct)) return;
   xaccAccountSetMark (acct, 1);

   ndiffs = pgendCompareOneAccountOnly (be, acct);

   /* update account if there are differences ... */
   if (0<ndiffs) pgendStoreOneAccountOnly (be, acct, SQL_UPDATE);
   /* insert account if it doesn't exist */
   if (0>ndiffs) pgendStoreOneAccountOnly (be, acct, SQL_INSERT);

}

/* ============================================================= */
/* This routine traverses the transaction structure and stores/updates
 * it in the database.  If checks the transaction splits as well,
 * updating those.  Finally, it makes sure that each account is present
 * as well.
 */

static void
pgendStoreTransactionNoLock (PGBackend *be, Transaction *trans)
{
   int i, ndiffs, nsplits;

   if (!be || !trans) return;

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
      pgendStoreAccountMarkNoLock (be, acct);
   }
}

/* ============================================================= */
/* This routine traverses the group structure and stores it into 
 * the database.  The NoLock version doesn't lock up the tables.
 */

static int
traverse_cb (Transaction *trans, void *cb_data)
{
   pgendStoreTransactionNoLock ((PGBackend *) cb_data, trans);
   return 0;
}

static void
pgendStoreGroupMarkNoLock (PGBackend *be, AccountGroup *grp)
{
   int i, nacc;

   if (!be || !grp) return;

   /* walk the account tree, and store subaccounts */
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      AccountGroup *subgrp;
      Account *acc = xaccGroupGetAccount(grp, i);

      pgendStoreAccountMarkNoLock (be, acc);

      /* recursively walk to child accounts */
      subgrp = xaccAccountGetChildren (acc);
      if (subgrp) pgendStoreGroupMarkNoLock(be, subgrp);
   }
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
   pgendStoreGroupMarkNoLock (be, grp);

   /* recursively walk transactions */
   xaccGroupStagedTransactionTraversal (grp, 1, traverse_cb, be);

   /* reset the write flags again */
   xaccClearMarkDownGr (grp, 0);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);
}

/* ============================================================= */
/* this routine fills in the structure pointed at by split
 * with data sucked out of the database.  It does only that 
 * one split,
 * hack alert unfinished, incomplete 
 */

static void
pgendGetOneSplitOnly (PGBackend *be, Split *split, GUID *guid)
{
   int rc;

   ENTER ("be=%p, split=%p", be, split);
   if (!be || !split || !guid) return;

   rc = PQsendQuery (be->connection, "SELECT * FROM gncEntry;");
   if (!rc)
   {
      PERR("send query failed:\n"
           "\t%s", PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return;
   }


   LEAVE (" ");

}

/* ============================================================= */
/* This routine restores the account heirarchy of *all* accounts in the DB.
 * It implicitly assumes that the database has only one account
 * heirarchy in it, i.e. anny accounts without a parent will be stuffed
 * into the same top group.
 *
 * hack alert -- not all account fields being restored.
 * sepcifically, need to handle currency
 */

static AccountGroup *
pgendGetAllAccounts (PGBackend *be)
{
   PGresult *result;
   AccountGroup *topgrp;
   char * buff;
   int i, nrows, iacc;

   ENTER ("be=%p", be);
   if (!be) return NULL;

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
            string_to_guid (GET_DB_VAL("accountGUID",j), &guid);
            acc = (Account *) xaccLookupEntity (&guid, GNC_ID_ACCOUNT);
            if (!acc) 
            {
               acc = xaccMallocAccount();
               xaccAccountSetGUID(acc, &guid);
            }

            xaccAccountSetName(acc, GET_DB_VAL("accountName",j));
            xaccAccountSetDescription(acc, GET_DB_VAL("description",j));
            xaccAccountSetCode(acc, GET_DB_VAL("accountCode",j));
            xaccAccountSetType(acc, xaccAccountStringToEnum(GET_DB_VAL("type",j)));
            /* try to find the parent account */
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
                  xaccAccountSetGUID(parent, &guid);
               }
               xaccAccountInsertSubAccount(parent, acc);
            }
         }
      }

      PQclear (result);
      i++;
   } while (result);

   LEAVE (" ");
   return topgrp;
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
             );
   SEND_QUERY (be,be->buff, 555);
   FINISH_QUERY(be->connection);

   /* See if the database is in the state that we last left it in.
    * Basically, the database should contain the 'old transaction'.
    * If it doesn't, then someone else has modified this transaction,
    * and thus, any further action on our part would be unsafe.  It
    * would be best to spit this back at the GUI, and let a human
    * decide.
    */
   ndiffs = pgendCompareOneTransactionOnly (be, oldtrans); 
   if (ndiffs) rollback++;

   /* be sure to check the old splits as well ... */
   nsplits = xaccTransCountSplits (oldtrans);
   for (i=0; i<nsplits; i++) {
      Split * s = xaccTransGetSplit (oldtrans, i);
      ndiffs = pgendCompareOneSplitOnly (be, s);
      if (ndiffs) rollback++;
   }

   if (rollback) {
      snprintf (be->buff, be->bufflen, "ROLLBACK;");
      SEND_QUERY (be,be->buff,444);
      FINISH_QUERY(be->connection);

      LEAVE (" ");
      return 666;   /* hack alert */
   } 

   /* if we are here, we are good to go */
   pgendStoreTransactionNoLock (be, trans);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be,be->buff,333);
   FINISH_QUERY(be->connection);

   LEAVE (" ");
   return 0;
}

/* ============================================================= */

static AccountGroup *
pgend_session_begin (GNCBook *sess, const char * sessionid)
{
   AccountGroup *grp;
   PGBackend *be;

   if (!sess) return NULL;
   be = (PGBackend *) xaccGNCBookGetBackend (sess);

   ENTER("sessionid=%s", sessionid);
   /* connect to a bogus database ... */
   /* hack alert -- we should be parsing the sessionid for the
    * hostname, port number, db name, etc... clean this up ... 
    * format should be something like
    * postgres://some.host.com:portno/db_name
    */
   be->dbName = g_strdup ("gnc_bogus");
   be->connection = PQsetdbLogin (NULL, NULL, NULL, NULL, be->dbName, NULL, NULL);

   /* check the connection status */
   if (CONNECTION_BAD == PQstatus(be->connection))
   {
      PERR("Connection to database '%s' failed:\n"
           "\t%s", 
           be->dbName, PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return NULL;
   }

   DEBUGCMD (PQtrace(be->connection, stderr));

   /* set the datestyle to something we can parse */
   snprintf (be->buff, be->bufflen, "SET DATESTYLE='ISO';");
   SEND_QUERY (be,be->buff,NULL);
   FINISH_QUERY(be->connection);

   grp = pgendGetAllAccounts (be);

{
/* stupid test */
GUID guid;
Transaction *trans=xaccMallocTransaction();
Split *s=xaccMallocSplit();
string_to_guid ("2ebc806e72c17bdc3c2c4e964b82eff8", &guid);
xaccTransSetGUID(trans,&guid);
pgendCompareOneTransactionOnly (be,trans);
string_to_guid ("d56a1146e414a30d6f2e251af2075f71", &guid);
xaccSplitSetGUID(s,&guid);
pgendCompareOneSplitOnly (be,s);
}

   LEAVE(" ");
   return grp;
}

/* ============================================================= */

/* hack alert -- this is the query buffer, it might be too small.
   we need to make it dynamic sized */
#define QBUFSIZE 16350

Backend * 
pgendNew (void)
{
   PGBackend *be;

   be = (PGBackend *) malloc (sizeof (PGBackend));

   /* generic backend handlers */
   be->be.book_load = pgend_session_begin;
   be->be.book_end = NULL;

   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = pgend_trans_commit_edit;
   be->be.trans_rollback_edit= NULL;
   be->be.run_query= NULL;

   /* postgres specific data */
   be->dbName = NULL;
   be->connection = NULL;

   be->builder = sqlBuilder_new();

   be->buff = malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

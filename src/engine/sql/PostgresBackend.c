/* 
 * PostgressBackend.c
 *
 * Implements the callbacks for the postgress backend.
 * this is somewhat seriously broken, poorly designed code.
 * its meant to be a quick hack just ot check things out.
 * it needs extensive review and design checking
 * 
 */

#include <pgsql/libpq-fe.h>  
#include <stdio.h>  
#include <string.h>  

#include <glib.h>

#include "BackendP.h"
#include "Group.h"
#include "gnc-book.h"
#include "guid.h"
#include "gnc-engine-util.h"

#include "PostgresBackend.h"

static short module = MOD_BACKEND; 

/* ============================================================= */
/* The SEND_QUERY macro sends the sql statement off to the server. 
 * It performs a minimal check to see that the send succeeded. 
 */

#define SEND_QUERY(be) 						\
{								\
   int rc;							\
   rc = PQsendQuery (be->connection, be->buff);			\
   if (!rc)							\
   {								\
      /* hack alert -- we need kinder, gentler error handling */\
      PERR("send query failed:\n"				\
           "\t%s", PQerrorMessage(be->connection));		\
      PQfinish (be->connection);				\
      return;							\
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
      PINFO ("got result\n");					\
      status = PQresultStatus(result);  			\
      if (PGRES_COMMAND_OK != status) {				\
         PERR("bad status\n");					\
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
      PERR ("failed to get result to query\n");			\
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
      PINFO ("query result %d has %d rows and %d cols\n",	\
           loopcounter, nrows, ncols);				\
   }								\
   if (1 < nrows) {						\
      PERR ("unexpected duplicate records\n");			\
      break;							\
   } else if (1 == nrows) 

/* --------------------------------------------------------------- */
/* Some utility macros for comparing values returned from the
 * database to values in the engine structs.
 */

#define GET_DB_VAL(str) (PQgetvalue (result, 0, PQfnumber (result, str)))

#define COMP_STR(sqlname,fun,ndiffs) { 				\
   if (strcmp (GET_DB_VAL(sqlname),fun)) {			\
      PINFO("%s sql='%s', eng='%s'\n", sqlname, 		\
         GET_DB_VAL (sqlname), fun); 				\
      ndiffs++; 						\
   }								\
}

#define COMP_GUID(sqlname,fun, ndiffs) { 			\
   const char *tmp = guid_to_string(fun); 			\
   if (strcmp (GET_DB_VAL(sqlname),tmp)) { 			\
      PINFO("%s sql='%s', eng='%s'\n", sqlname, 		\
         GET_DB_VAL(sqlname), tmp); 				\
      ndiffs++; 						\
   }								\
   free ((char *) tmp); 					\
} 

/* ============================================================= */
/* This routine stores the indicated group structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneGroupOnly (PGBackend *be, AccountGroup *grp, int update)
{
   Account *parent;
   const char *parent_guid, *grp_guid;
   int i, nacc;

   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return;

#if 0
   grp_guid = guid_to_string(xaccGroupGetGUID (grp));
   parent = xaccGroupGetParentAccount(grp);
   parent_guid = guid_to_string(xaccAccountGetGUID (parent));
   nacc = xaccGroupGetNumAccounts(grp);

   if (update) {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "UPDATE gncGroup SET "
               "parentGuid ='%s' "
               "WHERE"
               "groupGuid='%s';",
               parent_guid,
               grp_guid
               );
   } else {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncGroup "
               "(groupGuid, parentGuid)"
               " values "
               "('%s', '%s');",
               grp_guid,
               parent_guid
               );
   }
   free ((char *) grp_guid);
   free ((char *) parent_guid);

   SEND_QUERY(be);
   
   /* complete/commit the transaction, check the status */
   FINISH_QUERY(be->connection);
#endif

   LEAVE (" ");
}

/* ============================================================= */
/* this routine routine returns non-zero if the indicated group 
 * differs from that in the SQL database.
 * this routine grabs no locks.
 */

static int 
pgendCompareOneGroupOnly (PGBackend *be, AccountGroup *grp)
{
   const char *grp_guid;
   PGresult *result;
   int i=0, nrows=0, ndiffs=-1;

   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return -1;

#if 0
   grp_guid = guid_to_string(xaccGroupGetGUID (grp));

   /* try to find this group in the database */
   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "SELECT parentGuid "
            "FROM gncGroup "
            "WHERE groupGuid = '%s';",
            grp_guid
            );
   free ((char *) grp_guid);

   /* hack alert -- if error occurs here, what is the return value ????  */
   SEND_QUERY (be);

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      IF_ONE_ROW (result, nrows, i) {
 
         /* compared queried values to input values */
         COMP_GUID ("parentGuid", 
            xaccAccountGetGUID (xaccGroupGetParentAccount(grp)), ndiffs);
      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;
#endif

   LEAVE (" ");
   return ndiffs;
}

/* ============================================================= */
/* This routine stores the indicated account structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneAccountOnly (PGBackend *be, Account *acct, int update)
{
   const char *acct_guid, *parent_guid, *child_guid, *notes; 
   ENTER ("be=%p, acct=%p", be, acct);
   if (!be || !acct) return;

#if 0
   acct_guid = guid_to_string(xaccAccountGetGUID (acct));
   parent_guid = guid_to_string(xaccGroupGetGUID (xaccAccountGetParent(acct)));
   child_guid = guid_to_string(xaccGroupGetGUID (xaccAccountGetChildren(acct)));

   /* This is technically incorrect since notes could be NULL */
   notes = xaccAccountGetNotes(acct);
   if(!notes) notes = "";

   if (update) {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "UPDATE gncAccount SET "
               "parentGuid='%s', childrenGuid='%s', "
               "accountName='%s', accountCode='%s', description='%s', "
               "notes='%s', type=%d, currency='%s', security='%s' "
               "WHERE accountGuid='%s';",
               parent_guid,
               child_guid,
               xaccAccountGetName (acct),
               xaccAccountGetCode (acct),
               xaccAccountGetDescription (acct),
               notes,
               xaccAccountGetType (acct),
               xaccAccountGetCurrency (acct),
               xaccAccountGetSecurity (acct),
               acct_guid
               );
   } else {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncAccount "
               "(accountGuid, parentGuid, childrenGuid, "
               "accountName, accountCode, description, notes, "
               "type, currency, security)"
               " values "
               "('%s', '%s', '%s', '%s', '%s', '%s', '%s', "
               "%d, '%s', '%s');",
               acct_guid,
               parent_guid,
               child_guid,
               xaccAccountGetName (acct),
               xaccAccountGetCode (acct),
               xaccAccountGetDescription (acct),
               xaccAccountGetNotes (acct),
               xaccAccountGetType (acct),
               xaccAccountGetCurrency (acct),
               xaccAccountGetSecurity (acct)
               );
   }
   free ((char *) acct_guid);
   free ((char *) parent_guid);
   free ((char *) child_guid);

   SEND_QUERY (be);
   
   /* complete/commit the transaction, check the status */
   FINISH_QUERY(be->connection);
#endif

   LEAVE (" ");
}

/* ============================================================= */
/* this routine routine returns non-zero if the indicated acount 
 * differs from that in the SQL database.
 * this routine grabs no locks.
 */

static int 
pgendCompareOneAccountOnly (PGBackend *be, Account *acct)
{
   const char *acct_guid, *notes;
   PGresult *result;
   int i=0, nrows=0, ndiffs=0;

   ENTER ("be=%p, acct=%p", be, acct);
   if (!be || !acct) return 0;

   acct_guid = guid_to_string(xaccAccountGetGUID (acct));

   /* try to find this account in the database */
   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "SELECT parentGuid, childrenGuid, accountName, accountCode, "
            "description, notes, type, currency, security "
            "FROM gncAccount "
            "WHERE accountGuid = '%s';",
            acct_guid
            );
   free ((char *) acct_guid);

   /* hack alert -- if error occurs here, what is the return value ????  */
   SEND_QUERY (be);

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      IF_ONE_ROW (result, nrows, i) {
 
        notes = xaccAccountGetNotes(acct);
        if(!notes) notes = "";

         /* compared queried values to input values */
         COMP_STR ("accountName", xaccAccountGetName(acct), ndiffs);
         COMP_STR ("description", xaccAccountGetDescription(acct), ndiffs);
         COMP_STR ("notes", notes, ndiffs);
         // COMP_STR ("currency", xaccAccountGetCurrency(acct), ndiffs);
         // COMP_STR ("security", xaccAccountGetSecurity(acct), ndiffs);

         // COMP_GUID ("parentGuid", 
         //   xaccGroupGetGUID (xaccAccountGetParent(acct)), ndiffs);
         // COMP_GUID ("childrenGuid", 
         //   xaccGroupGetGUID (xaccAccountGetChildren(acct)), ndiffs);

         /* hack alert -- need to also do the account type */
         /* hack alert -- need to also do additional acct info for
            the specialty account types */
      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;

   LEAVE (" ");
   return ndiffs;
}

/* ============================================================= */
/* This routine stores the indicated transaction structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneTransactionOnly (PGBackend *be, Transaction *trans, int update)
{
   const char * trans_guid;

   ENTER ("be=%p, trans=%p", be, trans);
   if (!be || !trans) return;

   trans_guid = guid_to_string(xaccTransGetGUID (trans));

   if (update) {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "UPDATE gncTransaction SET "
               "num='%s', description='%s' "
               "WHERE transGuid='%s';",
               xaccTransGetNum (trans),
               xaccTransGetDescription (trans),
               trans_guid
               );
   } else {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncTransaction "
               "(transGuid, num, description)"
               " values "
               "('%s', '%s', '%s');",
               trans_guid,
               xaccTransGetNum (trans),
               xaccTransGetDescription (trans)
               );
   }

   free ((char *) trans_guid);

   SEND_QUERY (be);
   
   /* complete/commit the transaction, check the status */
   FINISH_QUERY(be->connection);

   LEAVE (" ");
}

/* ============================================================= */
/* this routine routine returns non-zero if the indicated transaction 
 * differs from that in the SQL database.
 * this routine grabs no locks.
 */

static int 
pgendCompareOneTransactionOnly (PGBackend *be, Transaction *trans)
{
   const char *trans_guid;
   PGresult *result;
   int i=0, nrows=0, ndiffs=0;

   ENTER ("be=%p, trans=%p", be, trans);
   if (!be || !trans) return 0;

   trans_guid = guid_to_string(xaccTransGetGUID (trans));

   /* try to find this transaction in the database */
   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "SELECT transGuid, date_entered, date_posted, num, description "
            "FROM gncTransaction "
            "WHERE transGuid = '%s';",
            trans_guid
            );
   free ((char *) trans_guid);

   /* hack alert -- if error occurs here, what is the return value ????  */
   SEND_QUERY (be);

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      IF_ONE_ROW (result, nrows, i) {
 
         /* compared queried values to input values */
         COMP_STR ("num", xaccTransGetNum(trans), ndiffs);
         COMP_STR ("description", xaccTransGetDescription(trans), ndiffs);

         /* hack alert -- need to also do the dates */
      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;

   LEAVE (" ");
   return ndiffs;
}

/* ============================================================= */
/* this routine stores the indicated split in the database
 */

static void
pgendStoreOneSplitOnly (PGBackend *be, Split *split, int update)
{
   Timespec ts;
   const char *split_guid, *acct_guid, *trans_guid;

   ENTER ("be=%p, split=%p", be, split);
   if (!be || !split) return;

   split_guid = guid_to_string(xaccSplitGetGUID (split));
   acct_guid =  guid_to_string(xaccAccountGetGUID (xaccSplitGetAccount(split)));
   trans_guid = guid_to_string(xaccTransGetGUID (xaccSplitGetParent(split)));

   /* hack alert date is not stored ... */
   xaccSplitGetDateReconciledTS (split, &ts);

   if (update) {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "UPDATE gncEntry SET "
               "accountGuid='%s', transGuid='%s', memo='%s', action='%s', "
               "reconciled='%c', amount=%g, share_price=%g "
               "WHERE entryGuid='%s';",
               acct_guid,
               trans_guid,
               xaccSplitGetMemo(split),
               xaccSplitGetAction(split),
               xaccSplitGetReconcile(split),
               DxaccSplitGetShareAmount(split),
               DxaccSplitGetSharePrice(split),
               split_guid
               );
   } else {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncEntry "
               "(entryGuid, accountGuid, transGuid, memo, action,"
               "reconciled, amount, share_price)"
               " values "
               "('%s', '%s', '%s', '%s', '%s', '%c', %g, %g);",
               split_guid,
               acct_guid,
               trans_guid,
               xaccSplitGetMemo(split),
               xaccSplitGetAction(split),
               xaccSplitGetReconcile(split),
               DxaccSplitGetShareAmount(split),
               DxaccSplitGetSharePrice(split)
               );
   }
   free ((char *) split_guid);
   free ((char *) acct_guid);
   free ((char *) trans_guid);

   SEND_QUERY (be);

   /* complete/commit the transaction, check the status */
   FINISH_QUERY(be->connection);

   LEAVE (" ");
}

/* ============================================================= */
/* this routine routine returns non-zero if the indicated split 
 * differs from that in the SQL database.
 * this routine grabs no locks.
 */

static int 
pgendCompareOneSplitOnly (PGBackend *be, Split *split)
{
   const char *split_guid;
   PGresult *result;
   int i=0, nrows=0, ndiffs=0;

   ENTER ("be=%p, split=%p", be, split);
   if (!be || !split) return;

   split_guid = guid_to_string(xaccSplitGetGUID (split));

   /* try to find this split in the database */
   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "SELECT accountGuid, transGuid, memo, action, "
            "reconciled, amount, share_price "
            "FROM gncEntry "
            "WHERE entryGuid = '%s';",
            split_guid
            );
   free ((char *) split_guid);

   /* hack alert -- if error occurs here, what is the return value ????  */
   SEND_QUERY (be);

   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      IF_ONE_ROW (result, nrows, i) {
 
         /* compared queried values to input values */
         COMP_STR ("memo", xaccSplitGetMemo(split), ndiffs);
         COMP_STR ("action", xaccSplitGetAction(split), ndiffs);
         COMP_GUID ("accountGuid", 
            xaccAccountGetGUID (xaccSplitGetAccount(split)), ndiffs);
         COMP_GUID ("transGuid", 
            xaccTransGetGUID (xaccSplitGetParent(split)), ndiffs);

/* hack alert -- need to also compare recconcile flag, amount, and price */
PINFO ("recn=%s amt=%s price=%s\n", GET_DB_VAL("reconciled"), 
GET_DB_VAL("amount"),
GET_DB_VAL("share_price"));
/*
            xaccSplitGetReconcile(split),
            DxaccSplitGetShareAmount(split),
            DxaccSplitGetSharePrice(split)
*/

      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;

   LEAVE (" ");
   return ndiffs;
}

/* ============================================================= */
/* This routine updates the account structure if needed, and/or
 * stores it the first time if it hasn't yet been stored.
 * Note that it sets a mark to avoid excessive recursion:
 * This routine shouldn't be used outside of locks,
where the recursion prevention clears the marks ...
ah hell. this is a bad idea maybe ....
 */

static void
pgendStoreAccountMarkNoLock (PGBackend *be, Account *acct)
{
   int i, ndiffs, nsplits;

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
   if (0<ndiffs) pgendStoreOneAccountOnly (be, acct, 1);
   /* insert account if it doesn't exist */
   if (0>ndiffs) pgendStoreOneAccountOnly (be, acct, 0);

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
   if (0<ndiffs) pgendStoreOneTransactionOnly (be, trans, 1);
   /* insert trans if it doesn't exist */
   if (0>ndiffs) pgendStoreOneTransactionOnly (be, trans, 0);

   /* walk over the list of splits */
   nsplits = xaccTransCountSplits (trans);
   for (i=0; i<nsplits; i++) {
      Split * s = xaccTransGetSplit (trans, i);
      Account *acct = xaccSplitGetAccount (s);

      ndiffs = pgendCompareOneSplitOnly (be, s);
      /* update split if there are differences ... */
      if (0<ndiffs) pgendStoreOneSplitOnly (be, s, 1);
      /* insert split if it doesn't exist */
      if (0>ndiffs) pgendStoreOneSplitOnly (be, s, 0);

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
   int i, nacc, ndiffs;

   if (!be || !grp) return;

   /* first, store the top-group */
   ndiffs = pgendCompareOneGroupOnly (be, grp);
   /* update group if there are differences ... */
   if (0<ndiffs) pgendStoreOneGroupOnly (be, grp, 1);
   /* insert group if it doesn't exist */
   if (0>ndiffs) pgendStoreOneGroupOnly (be, grp, 0);

   /* next, walk the account tree, and store subaccounts */
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
   SEND_QUERY (be);
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
   SEND_QUERY (be);
   FINISH_QUERY(be->connection);
}

/* ============================================================= */
/* this routine fills in the structure pointed at by split
 * with data sucked out of the database.  It does only that 
 * one split,
 * hack alert unfinished, incom[plete 
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

AccountGroup *
pgend_session_begin (GNCBook *sess, const char * sessionid)
{
   PGBackend *be;

   if (!sess) return;
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

   /* check the conmnection status */
   if (CONNECTION_BAD == PQstatus(be->connection))
   {
      PERR("Connection to database '%s' failed:\n"
           "\t%s", 
           be->dbName, PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return NULL;
   }

   DEBUGCMD (PQtrace(be->connection, stderr));

/* hack alert --- */
/* just a quickie place to dump stuff */
// xaccGroupSetBackend (xaccGNCBookGetGroup(sess), &(be->be));
// pgendStoreGroup (be, xaccGNCBookGetGroup(sess));

   LEAVE(" ");
   return NULL;
}

/* ============================================================= */

int
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
   SEND_QUERY (be);
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
      SEND_QUERY (be);
      FINISH_QUERY(be->connection);

      LEAVE (" ");
      return 666;   /* hack alert */
   } 

   /* if we are here, we are good to go */
   pgendStoreTransactionNoLock (be, trans);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be);
   FINISH_QUERY(be->connection);

   LEAVE (" ");
   return 0;
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

   be->buff = malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

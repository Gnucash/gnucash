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

#include "BackendP.h"
#include "Group.h"
#include "Session.h"
#include "guid.h"
#include "util.h"

#include "PostgresBackend.h"

static short module = MOD_BACKEND; 

/* ============================================================= */

#define SEND_QUERY(be) {					\
   int rc;							\
   rc = PQsendQuery (be->connection, be->buff);			\
   if (!rc)							\
   {								\
      /* hack alert -- we need knider, gentler error handling */\
      PERR("send query failed:\n"				\
           "\t%s", PQerrorMessage(be->connection));		\
      PQfinish (be->connection);				\
      return;							\
   }								\
}

#define FLUSH(conn) {						\
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

/* ============================================================= */
/* This routine stores the indicated group structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneGroupOnly (PGBackend *be, AccountGroup *grp)
{
   Account *parent;
   const char *parent_guid, *grp_guid;
   int i, nacc;

   ENTER ("be=%p, grp=%p\n", be, grp);
   if (!be || !grp) return;

   grp_guid = guid_to_string(xaccGroupGetGUID (grp));
   parent = xaccGroupGetParentAccount(grp);
   parent_guid = guid_to_string(xaccAccountGetGUID (parent));
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      Account *acc = xaccGroupGetAccount(grp, i);
      const char * acc_guid = guid_to_string(xaccAccountGetGUID (acc));

      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncGroup "
               "(groupGuid, parentGuid, childGuid)"
               " values "
               "('%s', '%s', '%s');",
               grp_guid,
               parent_guid,
               acc_guid
               );

      free ((char *) acc_guid);
      SEND_QUERY(be);
   
      /* complete/commit the transaction, check the status */
      FLUSH(be->connection);
   }
   free ((char *) grp_guid);
   free ((char *) parent_guid);

   LEAVE ("\n");
}

/* ============================================================= */
/* This routine stores the indicated account structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneAccountOnly (PGBackend *be, Account *acct)
{
   const char *acct_guid, *parent_guid, *child_guid; 
   ENTER ("be=%p, acct=%p\n", be, acct);
   if (!be || !acct) return;

   acct_guid = guid_to_string(xaccAccountGetGUID (acct));
   parent_guid = guid_to_string(xaccGroupGetGUID (xaccAccountGetParent(acct)));
   child_guid = guid_to_string(xaccGroupGetGUID (xaccAccountGetChildren(acct)));

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
   free ((char *) acct_guid);
   free ((char *) parent_guid);
   free ((char *) child_guid);

   SEND_QUERY (be);
   
   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
}

/* ============================================================= */
/* This routine stores the indicated transaction structure into the database.
 * It does *not* chase pointers, traverse the tree, etc. 
 * It performs no locking.
 */

static void
pgendStoreOneTransactionOnly (PGBackend *be, Transaction *trans)
{
   const char * trans_guid;

   ENTER ("be=%p, trans=%p\n", be, trans);
   if (!be || !trans) return;

   trans_guid = guid_to_string(xaccTransGetGUID (trans));
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

   free ((char *) trans_guid);

   SEND_QUERY (be);
   
   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
}

/* ============================================================= */
/* this routine stores the indicated split in the database
 */

static void
pgendStoreOneSplitOnly (PGBackend *be, Split *split, int update)
{
   Timespec ts;
   const char *split_guid, *acct_guid, *trans_guid;

   ENTER ("be=%p, split=%p\n", be, split);
   if (!be || !split) return;

   split_guid = guid_to_string(xaccSplitGetGUID (split));
   acct_guid =  guid_to_string(xaccAccountGetGUID (xaccSplitGetAccount(split)));
   trans_guid = guid_to_string(xaccTransGetGUID (xaccSplitGetParent(split)));

   /* hack alert date is not stored ... */
   xaccSplitGetDateReconciledTS (split, &ts);

   if (update) {
      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "UPDATE gncEntry SET"
               "accountGuid='%s', transGuid='%s', memo='%s', action='%s', "
               "reconciled='%c', amount=%g, share_price=%g "
               "WHERE entryGuid='%s';",
               acct_guid,
               trans_guid,
               xaccSplitGetMemo(split),
               xaccSplitGetAction(split),
               xaccSplitGetReconcile(split),
               xaccSplitGetShareAmount(split),
               xaccSplitGetSharePrice(split),
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
               xaccSplitGetShareAmount(split),
               xaccSplitGetSharePrice(split)
               );
   }
   free ((char *) split_guid);
   free ((char *) acct_guid);
   free ((char *) trans_guid);

   SEND_QUERY (be);

   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
}

/* ============================================================= */
/* this routine routine returns non-zero if the indicated split 
 * differs fropm that in the SQL database.
 * this routine grabs no locks.
 */

static int 
pgendCompareOneSplitOnly (PGBackend *be, Split *split)
{
   const char *split_guid;
   PGresult *result;
   int i, nrows, ncols, ndiffs=0;

   ENTER ("be=%p, split=%p\n", be, split);
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

   SEND_QUERY (be);

   i=0; nrows=0;
   do {
      ExecStatusType status;  
 
      result = PQgetResult (be->connection);
      if (!result) break;
 
      status = PQresultStatus(result);
 
      if ((PGRES_COMMAND_OK != status) &&
          (PGRES_TUPLES_OK  != status))
      {
         PERR ("failed to get result to query\n");
         PQclear (result);
         /* hack alert need gentler, kinder error recovery */
         PQfinish (be->connection);
         break;
      }
 
      nrows += PQntuples (result);
      ncols = PQnfields (result);
 
      PINFO ("query result %d has %d rows and %d cols\n",
               i, nrows, ncols);
      if (1 < nrows) {
         PERR ("unexpected duplicate records\n");
         break;
      } else if (1 == nrows) {
 
#define GETV(str) (PQgetvalue (result, 0, PQfnumber (result, str)))
#define COMP(sqlname,fun) if (strcmp (GETV(sqlname),fun)) { 	\
	PINFO("%s sql='%s', split='%s'\n", sqlname, GETV(sqlname), fun); \
        ndiffs++; }
#define GCOMP(sqlname,fun) { \
	const char *tmp = guid_to_string(fun); \
        if (strcmp (GETV(sqlname),tmp)) { 	\
	   PINFO("%s sql='%s', split='%s'\n", sqlname, GETV(sqlname), tmp); \
           ndiffs++; } \
        free ((char *) tmp); } 

         /* compared queried values to input values */
         COMP ("memo", xaccSplitGetMemo(split));
         COMP ("action", xaccSplitGetAction(split));
         GCOMP ("accountGuid", 
            xaccAccountGetGUID (xaccSplitGetAccount(split)));
         GCOMP ("transGuid", 
            xaccTransGetGUID (xaccSplitGetParent(split)));

PINFO ("recn=%s amt=%s price=%s\n", GETV("reconciled"), GETV("amount"),
GETV("share_price"));
      }

      PQclear (result);
      i++;
   } while (result);

   if (0 == nrows) ndiffs = -1;

/*
            xaccSplitGetReconcile(split),
            xaccSplitGetShareAmount(split),
            xaccSplitGetSharePrice(split)
*/


   LEAVE ("\n");
   return ndiffs;
}

/* ============================================================= */
/* This routine traverses the group structure and stores it into 
 * the database.  The NoLock version doesn't lock up the tables.
 */

static int
traverse_cb (Transaction *trans, void *cb_data)
{
   PGBackend *be = (PGBackend *) cb_data;
   int i, nsplits;

   if (!be || !trans) return;

   pgendStoreOneTransactionOnly (be, trans);

   /* walk over the list of splits */
   nsplits = xaccTransCountSplits (trans);
   for (i=0; i<nsplits; i++) {
      Split * s = xaccTransGetSplit (trans, i);
      int ndiffs = pgendCompareOneSplitOnly (be, s);
      /* update split if there are differences ... */
      if (0<ndiffs) pgendStoreOneSplitOnly (be, s, 1);
      /* insert split if it doesn't exist */
      if (0>ndiffs) pgendStoreOneSplitOnly (be, s, 0);
   }

   return 0;
}

static void
pgendStoreGroupNoLock (PGBackend *be, AccountGroup *grp)
{
   int i, nacc;

   if (!be || !grp) return;

   /* first, store the top-group */
   pgendStoreOneGroupOnly (be, grp);

   /* next, walk the account tree, and store subaccounts */
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      AccountGroup *subgrp;
      Account *acc = xaccGroupGetAccount(grp, i);
      pgendStoreOneAccountOnly (be, acc);

      /* recursively walk to child accounts */
      subgrp = xaccAccountGetChildren (acc);
      if (subgrp) pgendStoreGroupNoLock(be, subgrp);
   }
}


static void
pgendStoreGroup (PGBackend *be, AccountGroup *grp)
{
   ENTER ("be=%p, grp=%p\n", be, grp);
   if (!be || !grp) return;

   /* lock it up so that we store atomically */
   snprintf (be->buff, be->bufflen, "BEGIN;");
   SEND_QUERY (be);
   FLUSH(be->connection);

   /* reset the write flags. We use this to amek sure we don't
    * get caught in infinite recursion */
   xaccGroupBeginStagedTransactionTraversals(grp);
   pgendStoreGroupNoLock (be, grp);

   /* recursively walk transactions */
   xaccGroupStagedTransactionTraversal (grp, 1, traverse_cb, be);

   snprintf (be->buff, be->bufflen, "COMMIT;");
   SEND_QUERY (be);
   FLUSH(be->connection);
}

/* ============================================================= */
/* this routine fills in the structure pointed at by split
 * with data sucked out of the database.  It does only that 
 * one split,
 */

static void
pgendGetOneSplitOnly (PGBackend *be, Split *split, GUID *guid)
{
   int rc;

   ENTER ("be=%p, split=%p\n", be, split);
   if (!be || !split || !guid) return;

   rc = PQsendQuery (be->connection, "SELECT * FROM gncEntry;");
   if (!rc)
   {
      PERR("send query failed:\n"
           "\t%s", PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return;
   }


   LEAVE ("\n");

}

/* ============================================================= */

AccountGroup *
pgend_session_begin (Session *sess, const char * sessionid)
{
   PGBackend *be;

   if (!sess) return;
   be = (PGBackend *) xaccSessionGetBackend (sess);

   ENTER("sessionid=%s\n", sessionid);
   /* connect to a bogus database ... */
   /* hack alert -- we should be parsing the sessionid for the
    * hostname, port number, db name, etc... clean this up ... */
   be->dbName = strdup ("gnc_bogus");
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
/* just a quickie place to duimp stuff */
xaccGroupSetBackend (xaccSessionGetGroup(sess), &(be->be));
pgendStoreGroup (be, xaccSessionGetGroup(sess));

   LEAVE("\n");
   return NULL;
}

/* ============================================================= */

int
pgend_trans_commit_edit (Backend * bend, Transaction * trans)
{
   PGBackend *be = (PGBackend *)bend;
   int i, nsplits;
   int rc;

   ENTER ("be=%p, trans=%p\n", be, trans);
   if (!be || !trans) return 1;  /* hack alert hardcode literal */

   nsplits = xaccTransCountSplits (trans);
   for (i=0; i<nsplits; i++) {
      Split *s =  xaccTransGetSplit (trans, i);
      int ndiffs = pgendCompareOneSplitOnly (be, s);
      /* update split if there are differences ... */
      if (0<ndiffs) pgendStoreOneSplitOnly (be, s, 1);
      /* insert split if it doesn't exist */
      if (0>ndiffs) pgendStoreOneSplitOnly (be, s, 0);
   }

#if 0
   rc = PQsendQuery (be->connection, "SELECT * FROM gncAccount;");
   if (!rc)
   {
      PERR("send query failed:\n"
           "\t%s", PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return 2; /* hack alert hardcode literal */ 
   }
#endif


   LEAVE ("\n");
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
   be->be.session_begin = pgend_session_begin;
   be->be.session_end = NULL;

   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = pgend_trans_commit_edit;
   be->be.trans_rollback_edit= NULL;

   /* postgres specific data */
   be->dbName = NULL;
   be->connection = NULL;

   be->buff = malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

/* 
 * PostgressBackend.c
 *
 * Implements the callbacks for the postgress backend.
 * this is some seriously broken, poorly designed code.
 * its meant to be a quiick hack just ot check things out.
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
   const GUID *parent_guid, *grp_guid;
   int i, nacc;

   ENTER ("be=%p, grp=%p\n", be, grp);
   if (!be || !grp) return;

   grp_guid = xaccGroupGetGUID (grp);
   parent = xaccGroupGetParentAccount(grp);
   parent_guid = xaccAccountGetGUID (parent);
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      Account *acc = xaccGroupGetAccount(grp, i);

      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (be->buff, be->bufflen, 
               "INSERT INTO gncGroup "
               "(groupGuid, parentGuid, childGuid)"
               " values "
               "('%s', '%s', '%s');",
               guid_to_string(grp_guid),
               guid_to_string(parent_guid),
               guid_to_string(xaccAccountGetGUID (acc))
               );

      SEND_QUERY(be);
   
      /* complete/commit the transaction, check the status */
      FLUSH(be->connection);
   }

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

   ENTER ("be=%p, acct=%p\n", be, acct);
   if (!be || !acct) return;

   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "INSERT INTO gncAccount "
            "(accountGuid, parentGuid, childrenGuid, "
            "accountName, accountCode, description, notes, "
            "type, currency, security)"
            " values "
            "('%s', '%s', '%s', '%s', '%s', '%s', '%s', "
            "%d, '%s', '%s');",
            guid_to_string(xaccAccountGetGUID (acct)),
            guid_to_string(xaccGroupGetGUID (xaccAccountGetParent(acct))),
            guid_to_string(xaccGroupGetGUID (xaccAccountGetChildren(acct))),
            xaccAccountGetName (acct),
            xaccAccountGetCode (acct),
            xaccAccountGetDescription (acct),
            xaccAccountGetNotes (acct),
            xaccAccountGetType (acct),
            xaccAccountGetCurrency (acct),
            xaccAccountGetSecurity (acct)
            );

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

   ENTER ("be=%p, trans=%p\n", be, trans);
   if (!be || !trans) return;

   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "INSERT INTO gncTransaction "
            "(transGuid, num, description)"
            " values "
            "('%s', '%s', '%s');",
            guid_to_string(xaccTransGetGUID (trans)),
            xaccTransGetNum (trans),
            xaccTransGetDescription (trans)
            );

   SEND_QUERY (be);
   
   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
}

/* ============================================================= */
/* this routine stores the indicated split in the database
 */

static void
pgendStoreOneSplitOnly (PGBackend *be, Split *split)
{
   Timespec ts;

   ENTER ("be=%p, split=%p\n", be, split);
   if (!be || !split) return;

   /* hack alert date is not stored ... */
   xaccSplitGetDateReconciledTS (split, &ts);

   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (be->buff, be->bufflen, 
            "INSERT INTO gncEntry "
            "(entryGuid, accountGuid, transGuid, memo, action,"
            "reconciled, amount, share_price)"
            " values "
            "('%s', '%s', '%s', '%s', '%s', '%c', %g, %g);",
            guid_to_string(xaccSplitGetGUID (split)),
            guid_to_string(xaccAccountGetGUID (xaccSplitGetAccount(split))),
            guid_to_string(xaccTransGetGUID (xaccSplitGetParent(split))),
            xaccSplitGetMemo(split),
            xaccSplitGetAction(split),
            xaccSplitGetReconcile(split),
            xaccSplitGetShareAmount(split),
            xaccSplitGetSharePrice(split)
            );

   SEND_QUERY (be);

   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
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
   nsplits = xaccTransGetNumSplits (trans);
   for (i=0; i<nsplits; i++) {
      Split * s = xaccTransGetSplit (trans, i);
      pgendStoreOneSplitOnly (be, s);
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
 * with data sucked out of the database
 */

static void
pgendGetSplit (PGBackend *be, Split *split, GUID *guid)
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
   /* hack alert -- clean this up ... */
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
      pgendStoreOneSplitOnly (be, s);
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

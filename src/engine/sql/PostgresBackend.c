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

/* hack alert -- this is the query buffer, it might be too small.
   we need to make it dynamic sized */
#define QBUFSIZE 16350

/* ============================================================= */

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
 */

static void
pgendStoreOneGroupOnly (PGBackend *be, AccountGroup *grp)
{
   Account *parent;
   const GUID *parent_guid, *grp_guid;
   char buff[QBUFSIZE];
   int i, nacc, rc;

   ENTER ("be=%p, grp=%p\n", be, grp);
   if (!be || !grp) return;

   grp_guid = xaccGroupGetGUID (grp);
   parent = xaccGroupGetParentAccount(grp);
   parent_guid = xaccAccountGetGUID (parent);
   nacc = xaccGroupGetNumAccounts(grp);

   for (i=0; i<nacc; i++) {
      Account *acc = xaccGroupGetAccount(grp, i);

      /* hack alert -- values should be escaped so that no '' apear in them */
      snprintf (buff, QBUFSIZE, 
               "INSERT INTO gncGroup "
               "(groupGuid, parentGuid, childGuid)"
               " values "
               "('%s', '%s', '%s');",
               guid_to_string(grp_guid),
               guid_to_string(parent_guid),
               guid_to_string(xaccAccountGetGUID (acc))
               );
      rc = PQsendQuery (be->connection, buff);
      if (!rc)
      {
         PERR("send query failed:\n"
              "\t%s", PQerrorMessage(be->connection));
         PQfinish (be->connection);
         return;
      }
   
      /* complete/commit the transaction, check the status */
      FLUSH(be->connection);
   }

   LEAVE ("\n");
}

/* ============================================================= */
/* this routine stores the indicated split in the database
 */

static void
pgendStoreSplit (PGBackend *be, Split *split)
{
   Timespec ts;
   char buff[QBUFSIZE];
   int rc;

   ENTER ("be=%p, split=%p\n", be, split);
   if (!be || !split) return;

   /* hack alert date is not stored ... */
   xaccSplitGetDateReconciledTS (split, &ts);

   /* hack alert -- values should be escaped so that no '' apear in them */
   snprintf (buff, QBUFSIZE, 
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
   rc = PQsendQuery (be->connection, buff);
   if (!rc)
   {
      PERR("send query failed:\n"
           "\t%s", PQerrorMessage(be->connection));
      PQfinish (be->connection);
      return;
   }

   /* complete/commit the transaction, check the status */
   FLUSH(be->connection);

   LEAVE ("\n");
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
      pgendStoreSplit (be, s);
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

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

/********************************************************************\
 * PostgresBackend.c -- implements postgres backend                 *
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
 * PostgresBackend.c
 *
 * FUNCTION:
 * Implements the callbacks for the Postgres backend.
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas
 * 
 */

#define _GNU_SOURCE

#include "config.h"

#include <ctype.h>
#include <glib.h>
#include <netdb.h>
#include <pwd.h>
#include <stdio.h>  
#include <string.h>  
#include <sys/types.h>  
#include <unistd.h>  

#include <libpq-fe.h>  

#include "AccountP.h"
#include "Backend.h"
#include "BackendP.h"
#include "Group.h"
#include "GroupP.h"
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-event-p.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"
#include "guid.h"
#include "GNCId.h"
#include "GNCIdP.h"
#include "TransactionP.h"

#include "builder.h"
#include "checkpoint.h"
#include "events.h"
#include "gncquery.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"

#include "putil.h"

static short module = MOD_BACKEND; 

static void pgendDisable (PGBackend *be);
static void pgendEnable (PGBackend *be);
static void pgendInit (PGBackend *be);

static const char * pgendSessionGetMode (PGBackend *be);

GUID nullguid;

/* hack alert -- this is the query buffer size, it can be overflowed.
 * Ideally, its dynamically resized.  On the other hand, Postgres
 * rejects queries longer than 8192 bytes, (according to the
 * documentation) so there's not much point in getting fancy ... 
 */
#define QBUFSIZE 16350

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
      extern int h_errno;
      struct hostent *hent;

      hent = gethostbyname (be->buff);
      if (hent) 
      {
         strcpy (be->buff, hent->h_name);
      }
      else
      {
         PERR ("can't get domainname: %s", hstrerror(h_errno));
      }
   } 
   else
   {
      *p = 0;
      PERR ("can't get hostname");
   }

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

static gnc_commodity *
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
/* send the query, process the results */

gpointer
pgendGetResults (PGBackend *be, 
             gpointer (*handler) (PGBackend *, PGresult *, int, gpointer),
             gpointer data)
{   
   PGresult *result;
   int i=0;
   be->nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int j, jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         be->nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, jrows, ncols);

         for (j=0; j<jrows; j++)
         {
            data = handler (be, result, j, data);
         }
      }
      i++;
      PQclear (result);
   } while (result);

   return data;
}

/* ============================================================= */
/* version number callback for pgendGetResults */

static gpointer
get_version_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   int version = atoi(DB_GET_VAL ("version", j));
   int incoming = (int) data;
   if (version < incoming) version = incoming;
   return (gpointer) version;
}

/* ============================================================= */
/* include the auto-generated code */

#include "base-autogen.c"

static const char *table_audit_str = 
#include "table-audit.c"
;

static const char *table_create_str = 
#include "table-create.c"
;

static const char *sql_functions_str = 
#include "functions.c"
;

static const char *table_drop_str = 
#include "table-drop.c"
;


/* ============================================================= */
/* ============================================================= */
/*               ACCOUNT AND GROUP STUFF                         */
/*      (UTILITIES FIRST, THEN SETTERS, THEN GETTERS)            */
/* ============================================================= */
/* ============================================================= */

/* ============================================================= */
/* the pgendStoreAccount() routine stores an account to the 
 * database.  That is, the engine data is written out to the 
 * database.  It does not do any of the account children; nor 
 * does it handle any of the splits or transactions associated 
 * with the account.  It does, however, store the associated 
 * commodity. 
 *
 * If do_mark is set to TRUE, then this routine sets a mark
 * to terminate recursion.  That is,  it will only store the
 * account once; a second call on a marked account will simply 
 * return.  Be sure to clear the mark when done!
 *
 * If the do_check_version flag is set, then this routine
 * will compare the engine and sql db version numbrs, and
 * perform the store only if the engine version is equal 
 * or newer than the sql version.
 *
 * This routine doesn't perform any locks, and shouldn't be 
 * used outside of locks,
 */

static void
pgendStoreAccountNoLock (PGBackend *be, Account *acct,
                         gboolean do_mark, 
                         gboolean do_check_version)
{
   const gnc_commodity *com;

   if (!be || !acct) return;
   if ((FALSE == do_mark) && (FALSE == acct->core_dirty)) return;

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

   if (do_check_version)
   {
     if (0 < pgendAccountCompareVersion (be, acct)) return;
   }
   acct->version ++;  /* be sure to update the version !! */

   pgendPutOneAccountOnly (be, acct);

   /* make sure the account's commodity is in the commodity table */
   /* XXX hack alert FIXME -- it would be more efficient to do 
    * this elsewhere, and not here.  Or put a mark on it ... 
    * See StoreAllPrices for an example of how to do this. 
    */
   com = xaccAccountGetCommodity (acct);
   pgendPutOneCommodityOnly (be, (gnc_commodity *) com);

   pgendKVPStore (be, &(acct->guid), acct->kvp_data);
   LEAVE(" ");
}

/* ============================================================= */
/* The pgendStoreGroup() routine stores the account hierarchy to
 * the sql database.  That is, it stores not oonly the top-level
 * accounts, but all of thier children too.   It also stores the
 * commodities associated with the accounts.  It does *not* store
 * any of the transactions.
 *
 * Note that it checks the version numbers, and only stores 
 * those accounts whose version number is equal or newer than 
 * what's in the DB.
 *
 * The NoLock version doesn't lock up the tables.
 */

static void
pgendStoreGroupNoLock (PGBackend *be, AccountGroup *grp, 
                       gboolean do_mark, gboolean do_check_version)
{
   GList *start, *node;

   if (!be || !grp) return;
   ENTER("grp=%p mark=%d", grp, do_mark);

   /* walk the account tree, and store subaccounts */
   start = xaccGroupGetAccountList (grp);
   for (node=start; node; node=node->next) 
   {
      AccountGroup *subgrp;
      Account *acc = node->data;

      pgendStoreAccountNoLock (be, acc, do_mark, do_check_version);

      /* recursively walk to child accounts */
      subgrp = xaccAccountGetChildren (acc);
      if (subgrp) pgendStoreGroupNoLock(be, subgrp, do_mark,
                                        do_check_version);
   }
   LEAVE(" ");
}


static void
pgendStoreGroup (PGBackend *be, AccountGroup *grp)
{
   char *p;
   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return;

   /* lock it up so that we store atomically */
   p = "BEGIN;\n"
       "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* Clear the account marks; this is used to avoid visiting
    * the same account more than once. */
   xaccClearMarkDownGr (grp, 0);

   pgendStoreGroupNoLock (be, grp, TRUE, TRUE);

   /* reset the write flags again */
   xaccClearMarkDownGr (grp, 0);

   p = "COMMIT;\n"
       "NOTIFY gncAccount;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}

/* ============================================================= */
/*        ACCOUNT GETTERS (SETTERS ARE ABOVE)                    */
/* ============================================================= */

/* ============================================================= */
/* This hack is a work-around for obtaining the account currency.
 * The sql backend doens't actually store one, so we work around 
 * it here. This routine goes away when the rest of gnucash stops 
 * using account currencies. 
 */

static gpointer
get_hack_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   Account *acc = (Account *) data;
   xaccAccountSetCurrency (acc, 
                 gnc_string_to_commodity (DB_GET_VAL("currency",j)));
   return data;
}

static void 
pgendGetAccountCurrencyHack (PGBackend *be, Account *acc)
{
   char *p;

   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT gncTransaction.currency FROM "
                  "    gncAccount, gncEntry, gncTransaction WHERE "
                  "    gncEntry.accountGuid = '");
   p = guid_to_string_buff (xaccAccountGetGUID (acc), p);
   p = stpcpy (p, "'  AND "
                  "    gncEntry.transGuid = gncTransaction.transGuid AND "
                  "    gncTransaction.currency <> gncAccount.commodity "
                  "    LIMIT 1;");
   SEND_QUERY (be, be->buff, );
   pgendGetResults (be, get_hack_cb, acc);
}
  
static gpointer 
get_account_currency_hack_cb (Account *acc, gpointer data)
{
   PGBackend *be = (PGBackend *) data;
   pgendGetAccountCurrencyHack (be, acc);
   return NULL;
}

/* ============================================================= */
/* This routine walks the account group, gets all KVP values */

static gpointer
restore_cb (Account *acc, void * cb_data)
{
   PGBackend *be = (PGBackend *) cb_data;
   acc->kvp_data = pgendKVPFetch (be, &(acc->guid), acc->kvp_data);
   return NULL;
}

static void 
pgendGetAllAccountKVP (PGBackend *be, AccountGroup *grp)
{
   if (!grp) return;

   xaccGroupForEachAccount (grp, restore_cb, be, TRUE);
}

/* ============================================================= */
/* This routine restores all commodities in the database.
 */

static gpointer
get_commodities_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   gnc_commodity_table *comtab = (gnc_commodity_table *) data;
   gnc_commodity *com;

   /* first, lets see if we've already got this one */
   com = gnc_commodity_table_lookup(comtab, 
         DB_GET_VAL("namespace",j), DB_GET_VAL("mnemonic",j));

   if (com) return comtab;

   /* no we don't ... restore it */
   com = gnc_commodity_new (
                    DB_GET_VAL("fullname",j), 
                     DB_GET_VAL("namespace",j), 
                     DB_GET_VAL("mnemonic",j),
                     DB_GET_VAL("code",j),
                     atoi(DB_GET_VAL("fraction",j)));

   gnc_commodity_table_insert (comtab, com);
   return comtab;
}

static void
pgendGetAllCommodities (PGBackend *be)
{
   gnc_commodity_table *comtab;
   char * p;
   if (!be) return;

   ENTER ("be=%p, conn=%p", be, be->connection);

   comtab = gnc_engine_commodities();
   if (!comtab) {
      PERR ("can't get global commodity table");
      return;
   }

   /* Get them ALL */
   p = "SELECT * FROM gncCommodity;";
   SEND_QUERY (be, p, );
   pgendGetResults (be, get_commodities_cb, comtab);

   LEAVE (" ");
}

/* ============================================================= */
/* The pgendGetAllAccounts() routine restores the account hierarchy 
 * of *all* accounts in the DB.
 * It implicitly assumes that the database has only one account
 * hierarchy in it, i.e. any accounts without a parent will be stuffed
 * into the same top group.
 */

static gpointer
get_account_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   AccountGroup *topgrp = (AccountGroup *) data;
   Account *parent;
   Account *acc;
   GUID guid;

   /* first, lets see if we've already got this one */
   PINFO ("account GUID=%s", DB_GET_VAL("accountGUID",j));
   guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("accountGUID",j), &guid);
   acc = xaccAccountLookup (&guid);
   if (!acc) 
   {
      acc = xaccMallocAccount();
      xaccAccountBeginEdit(acc);
      xaccAccountSetGUID(acc, &guid);
   }
   else 
   {
      xaccAccountBeginEdit(acc);
   }

   xaccAccountSetName(acc, DB_GET_VAL("accountName",j));
   xaccAccountSetDescription(acc, DB_GET_VAL("description",j));
   xaccAccountSetCode(acc, DB_GET_VAL("accountCode",j));
   xaccAccountSetType(acc, xaccAccountStringToEnum(DB_GET_VAL("type",j)));
   xaccAccountSetCommodity(acc, 
                 gnc_string_to_commodity (DB_GET_VAL("commodity",j)));
   xaccAccountSetVersion(acc, atoi(DB_GET_VAL("version",j)));

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
      parent = xaccAccountLookup (&guid);
      if (!parent)
      {
         parent = xaccMallocAccount();
         xaccAccountBeginEdit(parent);
         xaccAccountSetGUID(parent, &guid);
      }
      else
      {
         xaccAccountBeginEdit(parent);
      }
      xaccAccountInsertSubAccount(parent, acc);
      xaccAccountCommitEdit(parent);
   }
   xaccAccountCommitEdit(acc);

   return topgrp;
}

static AccountGroup *
pgendGetAllAccounts (PGBackend *be, AccountGroup *topgrp)
{
   char * bufp;

   ENTER ("be=%p", be);
   if (!be) return NULL;

   /* first, make sure commodities table is up to date */
   pgendGetAllCommodities (be);

   if (!topgrp)
   {
      topgrp = xaccMallocAccountGroup();
   }

   /* Get them ALL */
   bufp = "SELECT * FROM gncAccount;";
   SEND_QUERY (be, bufp, NULL);
   pgendGetResults (be, get_account_cb, topgrp);

   /* hack alert -- get account currencies */
   xaccGroupForEachAccount (topgrp,
       get_account_currency_hack_cb, be, TRUE);

   pgendGetAllAccountKVP (be, topgrp);

   /* Mark the newly read group as saved, since the act of putting
    * it together will have caused it to be marked up as not-saved.
    */
   xaccGroupMarkSaved (topgrp);

   LEAVE (" ");
   return topgrp;
}

/* ============================================================= */
/* ============================================================= */
/*            TRANSACTION STUFF                                  */
/* ============================================================= */
/* ============================================================= */
/* The is_trans_empty() routine returns TRUE if this appears to 
 * be a fresh, 'null' transaction.  It would be better if somehow 
 * we could get the gui to mark this as a fresh transaction, rather 
 * than having to scan a bunch of fields.  But, oh well, this is 
 * a minor quibble in the grand scheme of things.
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
/* The pgendStoreTransactionNoLock() routine traverses the transaction 
 * structure and stores/updates it in the database.  If checks the 
 * transaction splits as well, updating those.  If the database
 * has splits which the transaction doesn't, those are deleted.  
 * Then any new splits are poked into the database.
 *
 * If the do_check_version flag is set, then the database version
 * is compared to the engine version.  If the database version is 
 * newer, then the engine transaction is not stored.
 *
 * The pgendStoreTransaction() routine does the same, except that
 * it locks the tables appropriately.
 */

static gpointer
delete_list_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GList * deletelist = (GList *) data;
   GUID guid = nullguid;

   string_to_guid (DB_GET_VAL ("entryGuid", j), &guid);
   /* If the database has splits that the engine doesn't,
    * collect 'em up & we'll have to delete em */
   if (NULL == xaccSplitLookup (&guid))
   {
      deletelist = g_list_prepend (deletelist, 
                   g_strdup(DB_GET_VAL ("entryGuid", j)));
   }
   return deletelist;
}

static void
pgendStoreTransactionNoLock (PGBackend *be, Transaction *trans,
                             gboolean do_check_version)
{
   GList *start, *deletelist=NULL, *node;
   char * p;

   if (!be || !trans) return;
   ENTER ("trans=%p", trans);

   /* don't update the database if the database is newer ... */
   if (do_check_version)
   {
      if (0 < pgendTransactionCompareVersion (be, trans)) return;
   }
   trans->version ++;  /* be sure to update the version !! */

   /* first, we need to see which splits are in the database
    * since what is there may not match what we have cached in 
    * the engine. */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT entryGuid FROM gncEntry WHERE transGuid='");
   p = guid_to_string_buff(xaccTransGetGUID(trans), p);
   p = stpcpy (p, "';");

   SEND_QUERY (be,be->buff, );
   deletelist = pgendGetResults (be, delete_list_cb, deletelist);

   /* delete those splits that don't belong */
   p = be->buff; *p = 0;
   for (node=deletelist; node; node=node->next)
   {
      Split *s;
      GUID guid;
      string_to_guid ((char *)(node->data), &guid);
      s = xaccSplitLookup(&guid);
      pgendStoreAuditSplit (be, s, SQL_DELETE);

      p = stpcpy (p, "DELETE FROM gncEntry WHERE entryGuid='");
      p = stpcpy (p, node->data);
      p = stpcpy (p, "';\n");
   }
   if (p != be->buff)
   {
      PINFO ("%s", be->buff ? be->buff : "(null)");
      SEND_QUERY (be,be->buff, );
      FINISH_QUERY(be->connection);

      /* destroy any associated kvp data as well */
      for (node=deletelist; node; node=node->next)
      {
         pgendKVPDeleteStr (be, (char *)(node->data));
         g_free (node->data);
      }
   }

   /* Update the rest */
   start = xaccTransGetSplitList(trans);

   if ((start) && !(trans->do_free))
   { 
      for (node=start; node; node=node->next) 
      {
         Split * s = node->data;
         pgendPutOneSplitOnly (be, s);
         pgendKVPStore (be, &(s->guid), s->kvp_data);
      }
      pgendPutOneTransactionOnly (be, trans);
      pgendKVPStore (be, &(trans->guid), trans->kvp_data);
   }
   else
   {
      p = be->buff; *p = 0;
      for (node=start; node; node=node->next) 
      {
         Split * s = node->data;
         pgendStoreAuditSplit (be, s, SQL_DELETE);
         p = stpcpy (p, "DELETE FROM gncEntry WHERE entryGuid='");
         p = guid_to_string_buff (xaccSplitGetGUID(s), p);
         p = stpcpy (p, "';\n");
      }
 
      /* If this trans is marked for deletetion, use the 'orig' values
       * as the base for recording the audit.  This wouldn't be normally
       * reqquired, except that otherwise one gets a trashed currency 
       * value.
       */
      pgendStoreAuditTransaction (be, trans->orig, SQL_DELETE);
      p = be->buff; 
      p = stpcpy (p, "DELETE FROM gncTransaction WHERE transGuid='");
      p = guid_to_string_buff (xaccTransGetGUID(trans), p);
      p = stpcpy (p, "';");
      PINFO ("%s\n", be->buff ? be->buff : "(null)");
      SEND_QUERY (be,be->buff, );
      FINISH_QUERY(be->connection);

      /* destroy any associated kvp data as well */
      for (node=start; node; node=node->next) 
      {
         Split * s = node->data;
         pgendKVPDelete (be, &(s->guid));
      }
      pgendKVPDelete (be, &(trans->guid));
   }

   LEAVE(" ");
}

#if 0
/* This routine isn't used anywhere, and probably shouldn't
 * be, in part because its balance checkpointing algorithm
 * is wrong. */

static void
pgendStoreTransaction (PGBackend *be, Transaction *trans)
{
   char * bufp;
   if (!be || !trans) return;
   ENTER ("be=%p, trans=%p", be, trans);

   /* lock it up so that we store atomically */
   bufp = "BEGIN;\n"
          "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
          "LOCK TABLE gncEntry IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   pgendStoreTransactionNoLock (be, trans, TRUE);

   bufp = "COMMIT;\n"
          "NOTIFY  gncTransaction;";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   /* If this is the multi-user mode, we need to update the
    * balances as well.  */
   if ((MODE_POLL == be->session_mode) ||
       (MODE_EVENT == be->session_mode))
   {
      /* hack alert --  we should also recompute
       * the checkpoints for any accounts from which splits have
       * been deleted ... but we don't have these handy here ...
       * is this is actually kinda wrong ...
       */
      pgendTransactionRecomputeCheckpoints (be, trans);
   }

   LEAVE(" ");
}
 #endif

/* ============================================================= */
/* The pgendStoreAllTransactions() routine traverses through *all*
 * transactions in the account group, storing these to the database.
 * During the store, it checks the transaction version numbers,
 * and only stores those transactions that were newer in the engine.
 */

static int
trans_traverse_cb (Transaction *trans, void *cb_data)
{
   pgendStoreTransactionNoLock ((PGBackend *) cb_data, trans, TRUE);
   return 0;
}


static void
pgendStoreAllTransactions (PGBackend *be, AccountGroup *grp)
{
   char *p;
   ENTER ("be=%p, grp=%p", be, grp);
   if (!be || !grp) return;

   /* lock it up so that we store atomically */
   p = "BEGIN;\n"
       "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncEntry IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* Recursively walk transactions. Start by reseting the write 
    * flags. We use this to avoid infinite recursion */
   xaccGroupBeginStagedTransactionTraversals(grp);
   xaccGroupStagedTransactionTraversal (grp, 1, trans_traverse_cb, be);

   p = "COMMIT;\n"
       "NOTIFY gncTransaction;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* If this is the multi-user mode, we need to update the
    * balances as well.  */
   if ((MODE_POLL == be->session_mode) ||
       (MODE_EVENT == be->session_mode))
   {
      pgendGroupRecomputeAllCheckpoints(be, grp);
   }
   LEAVE(" ");
}

/* ============================================================= */
/* 
 * The pgendCopyTransactionToEngine() routine 'copies' data out of 
 *    the SQL database and into the engine, for the indicated 
 *    Transaction GUID.  It starts by looking for an existing
 *    transaction in the engine with such a GUID.  If found, then
 *    it compares the version of last update to what's in the sql DB.
 *    If the engine data is older, or the engine doesn't yet have 
 *    this transaction, then the full update happens.  The full
 *    update sets up the transaction structure, all of the splits
 *    in the transaction, and makes sure that all of the splits 
 *    are in the proper accounts.  If the pre-existing tranasaction
 *    in the engine has more splits than what's in the DB, then these
 *    are pruned so that the structure exactly matches what's in the 
 *    DB.  This routine then returns -1.
 *
 *    If this routine finds a pre-existing transaction in the engine,
 *    and the version of last modification of this transaction is 
 *    equal to or *newer* then what the DB holds, then this routine
 *    returns 0 if equal, and +1 if newer, and does *not* perform any 
 *    update.  (Note that 0 is returned for various error conditions.
 *    Thus, testing for 0 is a bad idea.  This is a hack, and should
 *    probably be fixed.
 */

static int
pgendCopyTransactionToEngine (PGBackend *be, const GUID *trans_guid)
{
   char *pbuff;
   Transaction *trans;
   PGresult *result;
   Account *acc, *previous_acc=NULL;
   gboolean do_set_guid=FALSE;
   int engine_data_is_newer = 0;
   int i, j, nrows;
   int save_state = 1;
   GList *node, *db_splits=NULL, *engine_splits, *delete_splits=NULL;
   gnc_commodity *currency = NULL;
   gint64 trans_frac = 0;
   
   ENTER ("be=%p", be);
   if (!be || !trans_guid) return 0;

   /* disable callbacks into the backend, and events to GUI */
   gnc_engine_suspend_events();
   pgendDisable(be);

   /* first, see if we already have such a transaction */
   trans = xaccTransLookup (trans_guid);
   if (!trans)
   {
      trans = xaccMallocTransaction();
      do_set_guid=TRUE;
      engine_data_is_newer = -1;
   }

   /* build the sql query to get the transaction */
   pbuff = be->buff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncTransaction WHERE transGuid='");
   pbuff = guid_to_string_buff(trans_guid, pbuff);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,be->buff, 0);
   i=0; nrows=0;
   do {
      GET_RESULTS (be->connection, result);
      {
         int jrows;
         int ncols = PQnfields (result);
         jrows = PQntuples (result);
         nrows += jrows;
         PINFO ("query result %d has %d rows and %d cols",
            i, nrows, ncols);

         j = 0;
         if (1 < nrows)
         {
             /* since the guid is primary key, this error is totally
              * and completely impossible, theoretically ... */
             PERR ("!!!!!!!!!!!SQL database is corrupt!!!!!!!\n"
                   "too many transactions with GUID=%s\n",
                    guid_to_string (trans_guid));
             if (jrows != nrows) xaccTransCommitEdit (trans);
             xaccBackendSetError (&be->be, ERR_BACKEND_DATA_CORRUPT);
             pgendEnable(be);
             gnc_engine_resume_events();
             return 0;
         }

         /* First order of business is to determine whose data is
          * newer: the engine cache, or the database.  If the 
          * database has newer stuff, we update the engine. If the
          * engine is equal or newer, we do nothing in this routine.
          * Of course, we know the database has newer data if this
          * transaction doesn't exist in the engine yet.
          */
         if (!do_set_guid)
         {
            gint32 db_version, cache_version;
            db_version = atoi (DB_GET_VAL("version",j));
            cache_version = xaccTransGetVersion (trans);
            if (db_version == cache_version) {
               engine_data_is_newer = 0;
            } else 
            if (db_version < cache_version) {
               engine_data_is_newer = +1;
            } else {
               engine_data_is_newer = -1;
            }
         }

         /* if the DB data is newer, copy it to engine */
         if (0 > engine_data_is_newer)
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
            xaccTransSetVersion (trans, atoi(DB_GET_VAL("version",j)));

            /* hack alert -- don't set the transaction currency until
             * after all splits are restored. This hack is used to set
             * the reporting currency in an account. This hack will be 
             * obsolete when reporting currencies are removed from the
             * account. */
            currency = gnc_string_to_commodity (DB_GET_VAL("currency",j));
            trans_frac = gnc_commodity_get_fraction (currency);
#if 0
            xaccTransSetCurrency
              (trans, gnc_string_to_commodity (DB_GET_VAL("currency",j)));
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
      pgendEnable(be);
      gnc_engine_resume_events();
      return 0;
   }

   /* if engine data was newer, we are done */
   if (0 <= engine_data_is_newer) 
   {
      pgendEnable(be);
      gnc_engine_resume_events();
      return engine_data_is_newer;
   }

   /* ------------------------------------------------- */
   /* If we are here, then the sql database contains data that is
    * newer than what we have in the engine.  And so, below, 
    * we finish the job of yanking data out of the db.
    */

   /* build the sql query the splits */
   pbuff = be->buff;
   pbuff[0] = 0;
   pbuff = stpcpy (pbuff, 
         "SELECT * FROM gncEntry WHERE transGuid='");
   pbuff = guid_to_string_buff(trans_guid, pbuff);
   pbuff = stpcpy (pbuff, "';");

   SEND_QUERY (be,be->buff, 0);
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
            gint64 num;
            gnc_numeric value, amount;

            /* --------------------------------------------- */
            /* first, lets see if we've already got this one */
            PINFO ("split GUID=%s", DB_GET_VAL("entryGUID",j));
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("entryGUID",j), &guid);
            s = xaccSplitLookup (&guid);
            if (!s)
            {
               s = xaccMallocSplit();
               xaccSplitSetGUID(s, &guid);
            }

            /* next, restore some split data */
            /* hack alert - not all split fields handled */
            xaccSplitSetMemo(s, DB_GET_VAL("memo",j));
            xaccSplitSetAction(s, DB_GET_VAL("action",j));
            ts = gnc_iso8601_to_timespec_local
              (DB_GET_VAL("date_reconciled",j));
            xaccSplitSetDateReconciledTS (s, &ts);

            num = atoll (DB_GET_VAL("value", j));
            value = gnc_numeric_create (num, trans_frac);
            xaccSplitSetValue (s, value);

            xaccSplitSetReconcile (s, (DB_GET_VAL("reconciled", j))[0]);

            /* --------------------------------------------- */
            /* next, find the account that this split goes into */
            guid = nullguid;  /* just in case the read fails ... */
            string_to_guid (DB_GET_VAL("accountGUID",j), &guid);
            acc = xaccAccountLookup (&guid);
            if (!acc)
            {
               PERR ("account not found, will delete this split\n"
                     "\t(split with  guid=%s\n" 
                     "\twants an acct with guid=%s)\n", 
                     DB_GET_VAL("entryGUID",j),
                     DB_GET_VAL("accountGUID",j)
                     );
               xaccSplitDestroy (s);
            }
            else
            {
               gnc_commodity *modity;
               gint64 acct_frac;
               num = atoll (DB_GET_VAL("amount", j));
               modity = xaccAccountGetCommodity (acc);
               acct_frac = gnc_commodity_get_fraction (modity);
               amount = gnc_numeric_create (num, acct_frac);
               xaccSplitSetShareAmount (s, amount);

               xaccTransAppendSplit (trans, s);

               if (acc != previous_acc)
               {
                  xaccAccountCommitEdit (previous_acc);
                  xaccAccountBeginEdit (acc);
                  previous_acc = acc;
               }
               if (acc->parent) save_state = acc->parent->saved;
               xaccAccountInsertSplit(acc, s);
               if (acc->parent) acc->parent->saved = save_state;

               /* finally tally them up; we use this below to 
                * clean out deleted splits */
               db_splits = g_list_prepend (db_splits, s);
            }
         }
      }
      i++;
      PQclear (result);
   } while (result);

   /* close out dangling edit session */
   xaccAccountCommitEdit (previous_acc);

   /* ------------------------------------------------- */
   /* destroy any splits that the engine has that the DB didn't */

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

   /* ------------------------------------------------- */
   /* restore any kvp data associated with the transaction and splits */

   trans->kvp_data = pgendKVPFetch (be, &(trans->guid), trans->kvp_data);

   engine_splits = xaccTransGetSplitList(trans);
   for (node = engine_splits; node; node=node->next)
   {
      Split *s = node->data;
      s->kvp_data = pgendKVPFetch (be, &(s->guid), s->kvp_data);
   }

   /* ------------------------------------------------- */

   /* see note above as to why we do this set here ... */
   xaccTransSetCurrency (trans, currency);

   xaccTransCommitEdit (trans);

   /* re-enable events to the backend and GUI */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
   return -1;
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
 *
 * THIS IS NOT USED ANYWHERE should probably go away.  Although
 * this kind of a routine could be handy for resyncing after a lost
 * contact to the backend.  Note, however, that it would
 * mangle balance checkpoints, and these would need to be
 * recomputed.
 */

#if 0

static void
pgendSyncTransaction (PGBackend *be, GUID *trans_guid)
{
   Transaction *trans;
   int engine_data_is_newer = 0;
   
   ENTER ("be=%p", be);
   if (!be || !trans_guid) return;

   /* disable callbacks into the backend, and events to GUI */
   gnc_engine_suspend_events();
   pgendDisable(be);

   engine_data_is_newer = pgendCopyTransactionToEngine (be, trans_guid);

   /* if engine data was newer, we save to the db. */
   if (0 < engine_data_is_newer) 
   {
      /* XXX hack alert -- fixme */
      PERR ("Data in the local cache is newer than the data in\n"
            "\tthe database.  Thus, the local data will be sent\n"
            "\tto the database.  This mode of operation is\n"
            "\tguarenteed to clobber other user's updates.\n");

      trans = xaccTransLookup (trans_guid);

      /* hack alert -- basically, we should use the pgend_commit_transaction
       * routine instead, and in fact, 'StoreTransaction'
       * pretty much shouldn't be allowed to exist in this
       * framework */
      pgendStoreTransaction (be, trans);

      gnc_engine_resume_events();
      return;
   }

   /* re-enable events to the backend and GUI */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
}

#endif

/* ============================================================= */
/*             QUERY STUFF                                       */
/* ============================================================= */
/* The pgendRunQuery() routine performs a search on the SQL database for 
 * all of the splits that correspond to gnc-style query, and then 
 * integrates them into the engine cache.  It then performs a 'closure'
 * in order to maintain accurate balances.  Warning: this routine
 * is a bit of a pig, and should be replaced with a better algorithm.
 * See below.
 * 
 * The problem that this routine is trying to solve is the need to
 * to run a query *and* maintain consistent balance checkpoints
 * within the engine data. As a by-product, it can pull in a vast 
 * amount of sql data into the engine.  The steps of the algorithm
 * are:
 *
 * 1) convert the engine style query to an SQL query string.
 * 2) run the SQL query to get the splits that satisfy the query
 * 3) pull the transaction ids out of the matching splits,
 * 4) fetch the corresponding transactions, put them into the engine.
 * 5) get the balance checkpoint with the latest date earlier
 *    than the earliest transaction,
 * 6) get all splits later than the checkpoint start,
 * 7) go to step 3) until a consistent set of transactions
 *    has been pulled into the engine.
 *
 * Note regarding step 4): 
 * We only ever pull complete transactions out of the engine, 
 * and never dangling splits. This helps make sure that the 
 * splits always balance in a transaction; it also allows the
 * ledger to operate in 'journal' mode.
 *
 * Note regarding step 6):
 * During the fill-out up to the checkpoint, new transactions may
 * pulled in.  These splits may link accounts we haven't seen before,
 * which is why we need to go back to step 3.
 *
 * The process may pull in a huge amount of data.
 *
 * Oops: mega-bug: if the checkpoints on all accounts don't share 
 * a common set of dates, then the above process will 'walk' until
 * the start of time, essentially pulling *all* data, and not that 
 * efficiently, either.  This is a killer bug with this implementation.
 * We can work around it by fixing checkpoints in time ...
 *
 * There are certainly alternate possible implementations.  In one
 * alternate, 'better' implementation, we don't fill out to to the 
 * checkpoint for all accounts, but only for the one being displayed.
 * However, doing so would require considerable jiggering in the 
 * engine proper, where we'd have to significantly modify 
 * RecomputeBalance() to do the 'right thing' when it has access to 
 * only some of the splits.   Yow.  Wait til after gnucash-1.6 for 
 * this tear-up.
 */

static gpointer
query_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GList *node, *xaction_list = (GList *) data;
   GUID *trans_guid;

   /* find the transaction this goes into */
   trans_guid = xaccGUIDMalloc();
   *trans_guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("transGUID",j), trans_guid);

   /* don't put transaction into the list more than once ... */
   for (node=xaction_list; node; node=node->next)
   {
      if (guid_equal ((GUID *)node->data, trans_guid)) 
      {
         xaccGUIDFree (trans_guid);
         return xaction_list;
      }
   }

   xaction_list = g_list_prepend (xaction_list, trans_guid);
   return xaction_list;
}

typedef struct acct_earliest {
   Account *acct;
   Timespec ts;
} AcctEarliest;

static int ncalls = 0;

static void 
pgendFillOutToCheckpoint (PGBackend *be, const char *query_string)
{
   GList *node, *anode, *xaction_list= NULL, *acct_list = NULL;

   ENTER (" ");
   if (!be) return;
   ncalls ++;

   SEND_QUERY (be, query_string, );
   xaction_list = pgendGetResults (be, query_cb, xaction_list);
   if (NULL == xaction_list) return;

   /* restore the transactions */
   for (node=xaction_list; node; node=node->next)
   {
      int engine_data_is_newer;
      GUID *trans_guid = (GUID *)node->data;

      engine_data_is_newer = pgendCopyTransactionToEngine (be, trans_guid);

      /* if we restored this transaction from the db, scan over the accounts 
       * it affects and see how far back the data goes.
       */
      if (0 > engine_data_is_newer) 
      {
         GList *split_list, *snode;
         Timespec ts;
         Transaction *trans;

         trans = xaccTransLookup (trans_guid);
         ts = xaccTransRetDatePostedTS (trans);

         /* Back off by a second to disambiguate time.
          * This is safe, because the fill-out will recurse
          * if something got into this one-second gap. */
         ts.tv_sec --; 
         split_list = xaccTransGetSplitList (trans);
         for (snode=split_list; snode; snode=snode->next)
         {
            int found = 0;
            Split *s = (Split *) snode->data;
            Account *acc = xaccSplitGetAccount (s);

            /* lets see if we have a record of this account already */
            for (anode = acct_list; anode; anode = anode->next)
            {
               AcctEarliest * ae = (AcctEarliest *) anode->data;
               if (ae->acct == acc) 
               {
                  if (0 > timespec_cmp(&ts, &(ae->ts)))
                  {
                     ae->ts = ts;
                  }
                  found = 1;
                  break;
               }
            }

            /* if not found, make note of this account, and the date */
            if (0 == found)
            {
               AcctEarliest * ae = g_new (AcctEarliest, 1);
               ae->acct = acc;
               ae->ts = ts;
               acct_list = g_list_prepend (acct_list, ae);
            }
         }
      }
      xaccGUIDFree (trans_guid);
   }
   g_list_free(xaction_list);

   if (NULL == acct_list) return;

   /* OK, at this point, we have a list of accounts, including the 
    * date of the earliest split in that account.  Now, we need to 
    * do two queries: first, get the running balances to that point,
    * and then all of the splits from that date onwards.
    */
   for (anode = acct_list; anode; anode = anode->next)
   {
      char *p;
      AcctEarliest * ae = (AcctEarliest *) anode->data;
      pgendAccountGetBalance (be, ae->acct, ae->ts);
   
      /* n.b. date_posted compare must be strictly greater than, since the 
       * GetBalance goes to less-then-or-equal-to because of the BETWEEN
       * that appears in the gncSubTotalBalance sql function. */
      p = be->buff; *p = 0;
      p = stpcpy (p, "SELECT DISTINCT gncEntry.transGuid from gncEntry, gncTransaction WHERE "
                     "   gncEntry.transGuid = gncTransaction.transGuid AND accountGuid='");
      p = guid_to_string_buff(xaccAccountGetGUID(ae->acct), p);
      p = stpcpy (p, "' AND gncTransaction.date_posted > '");
      p = gnc_timespec_to_iso8601_buff (ae->ts, p);
      p = stpcpy (p, "';");
     
      pgendFillOutToCheckpoint (be, be->buff);

      g_free (ae);
   }
   g_list_free(acct_list);

   LEAVE (" ");
}

static void 
pgendRunQuery (Backend *bend, Query *q)
{
   PGBackend *be = (PGBackend *)bend;
   const char * sql_query_string;
   sqlQuery *sq;
   GList *node, *anode, *xaction_list= NULL, *acct_list = NULL;

   ENTER (" ");
   if (!be || !q) return;

   gnc_engine_suspend_events();
   pgendDisable(be);

   /* first thing we do is convert the gnc-engine query into
    * an sql string. */
   sq = sqlQuery_new();
   sql_query_string = sqlQuery_build (sq, q);

   ncalls = 0;
   pgendFillOutToCheckpoint (be, sql_query_string);
   PINFO ("number of calls to fill out=%d", ncalls);

   sql_Query_destroy(sq);

   /* the fill-out will dirty a lot of data. That's irrelevent,
    * mark it all as having been saved. */
   xaccGroupMarkSaved (be->topgroup);

   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE (" ");
}


/* ============================================================= */
/* The pgendGetAllTransactions() routine sucks *all* of the 
 *    transactions out of the database.  This is a potential 
 *    CPU and memory-burner; its use is not suggested for anything
 *    but single-user mode.
 *
 *    To add injury to insult, this routine fetches in a rather 
 *    inefficient manner, in particular, the account query.
 */

static void
pgendGetAllTransactions (PGBackend *be, AccountGroup *grp)
{
   GList *node, *xaction_list = NULL;

   gnc_engine_suspend_events();
   pgendDisable(be);

   SEND_QUERY (be, "SELECT transGuid FROM gncTransaction;", );
   xaction_list = pgendGetResults (be, query_cb, xaction_list);

   /* restore the transactions */
   for (node=xaction_list; node; node=node->next)
   {
      pgendCopyTransactionToEngine (be, (GUID *)node->data);
      xaccGUIDFree (node->data);
   }
   g_list_free(xaction_list);

   pgendEnable(be);
   gnc_engine_resume_events();
}

/* ============================================================= */
/* ============================================================= */
/*                PRICE  STUFF                                   */
/* ============================================================= */
/* ============================================================= */
/* store just one price */

static void
pgendStorePriceNoLock (PGBackend *be, GNCPrice *pr,
                        gboolean do_check_version)
{
   gnc_commodity *modity;

   if (do_check_version)
   {
     if (0 < pgendPriceCompareVersion (be, pr)) return;
   }
   pr->version ++;  /* be sure to update the version !! */

   /* make sure that we've stored the commodity 
    * and currency before we store the price.
    */
   modity = gnc_price_get_commodity (pr);
   pgendPutOneCommodityOnly (be, modity);

   modity = gnc_price_get_currency (pr);
   pgendPutOneCommodityOnly (be, modity);

   pgendPutOnePriceOnly (be, pr);
}

/* ============================================================= */
/* store entire price database */

static gboolean 
foreach_price_cb (GNCPrice *pr, gpointer bend)
{
   PGBackend *be = (PGBackend *) bend;
   gnc_commodity *modity;
   gint16 mark;

   /* make sure that we've stored the commodity 
    * and currency before we store the price.
    * We use marks to avoid redundant stores. 
    */
   modity = gnc_price_get_commodity (pr);
   mark = gnc_commodity_get_mark (modity);
   if (!mark) {
      pgendPutOneCommodityOnly (be, modity);
      gnc_commodity_set_mark (modity, 1);
   }

   modity = gnc_price_get_currency (pr);
   mark = gnc_commodity_get_mark (modity);
   if (!mark) {
      pgendPutOneCommodityOnly (be, modity);
      gnc_commodity_set_mark (modity, 1);
   }

   pgendPutOnePriceOnly (be, pr);

   return TRUE;
}

static gboolean
commodity_mark_cb (gnc_commodity *cm, gpointer user_data)
{
   gint32 v = ((gint32) user_data) & 0xffff;
   gnc_commodity_set_mark (cm, (gint16) v);
   return TRUE;
}


static void
pgendStorePriceDBNoLock (PGBackend *be, GNCPriceDB *prdb)
{
   gnc_commodity_table *comtab = gnc_engine_commodities();

   /* clear the marks on commodities -- we use this to mark 
    * the thing as 'already stored', avoiding redundant stores */
   gnc_commodity_table_foreach_commodity (comtab, commodity_mark_cb, 0);

   gnc_pricedb_foreach_price (prdb, foreach_price_cb,
                              (gpointer) be, FALSE);

   gnc_commodity_table_foreach_commodity (comtab, commodity_mark_cb, 0);
}

static void
pgendStorePriceDB (PGBackend *be, GNCPriceDB *prdb)
{
   char *p;
   ENTER ("be=%p, prdb=%p", be, prdb);
   if (!be || !prdb) return;

   /* lock it up so that we store atomically */
   p = "BEGIN;\n"
       "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   pgendStorePriceDBNoLock (be, prdb);

   p = "COMMIT;\n"
       "NOTIFY gncPrice;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);
   LEAVE(" ");
}

/* ============================================================= */
/* The pgendGetAllPrices() routine sucks *all* of the 
 *    prices out of the database.  This is a potential 
 *    CPU and memory-burner; its use is not suggested for anything
 *    but single-user mode.
 */

static gpointer
get_price_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GNCPriceDB *prdb = (GNCPriceDB *) data;
   GNCPrice *pr;
   gint32 sql_vers, local_vers;
   Timespec ts;
   gint64 num, denom;
   gnc_numeric value;
   GUID guid = nullguid;
   int not_found = 0;

   gnc_commodity * modity;

   /* first, lets see if we've already got this one */
   string_to_guid (DB_GET_VAL ("priceGuid", j), &guid);
   pr = gnc_price_lookup (&guid);

   if (!pr) 
   { 
      pr = gnc_price_create();
      gnc_price_begin_edit (pr);
      gnc_price_set_guid (pr, &guid);
      not_found = 1;
   } 
   else
   {
      gnc_price_ref (pr);
      gnc_price_begin_edit (pr);
      not_found = 0;
   }

   /* compare versions. Hack alert -- Not sure how to handle failures */
   sql_vers = atoi (DB_GET_VAL("version",j));
   local_vers = gnc_price_get_version(pr);
   if (sql_vers < local_vers) {
      PERR ("local price version is higher than db !!! local=%d sql=%d",
         local_vers, sql_vers);
      gnc_price_commit_edit (pr);
      gnc_price_unref (pr);
      return prdb;
   }
   gnc_price_set_version (pr, sql_vers);

   modity = gnc_string_to_commodity (DB_GET_VAL("commodity",j));
   gnc_price_set_commodity (pr, modity);

   modity = gnc_string_to_commodity (DB_GET_VAL("currency",j));
   gnc_price_set_currency (pr, modity);

   ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("time",j));
   gnc_price_set_time (pr, ts);

   gnc_price_set_source (pr, DB_GET_VAL("source",j));
   gnc_price_set_type (pr, DB_GET_VAL("type",j));

   num = atoll (DB_GET_VAL("valueNum", j));
   denom = atoll (DB_GET_VAL("valueDenom", j));
   value = gnc_numeric_create (num, denom);
   gnc_price_set_value (pr, value);

   if (not_found) gnc_pricedb_add_price(prdb, pr);
   gnc_price_commit_edit (pr);
   gnc_price_unref (pr);

   return prdb;
}


static GNCPriceDB *
pgendGetAllPrices (PGBackend *be, GNCPriceDB *prdb)
{
   char * p;

   if (!be) return NULL;
   ENTER ("be=%p, conn=%p", be, be->connection);

   if (!prdb) {
      prdb = gnc_pricedb_create();
   }

   /* first, make sure commodities table is up to date */
   pgendGetAllCommodities (be);

   /* Get them ALL */
   p = "SELECT * FROM gncPrice;";
   SEND_QUERY (be, p, prdb);
   pgendGetResults (be, get_price_cb, prdb);

   LEAVE (" ");
   return prdb;
}

/* ============================================================= */

static void
pgendPriceLookup (Backend *bend, GNCPriceLookup *look)
{
   PGBackend *be = (PGBackend *)bend;
   char * p;

   ENTER ("be=%p, lookup=%p", be, look);
   if (!be || !look) return;

   /* special case the two-way search in terms of more basic primitives */
   if (LOOKUP_NEAREST_IN_TIME == look->type)
   {
      look->type = LOOKUP_LATEST_BEFORE;
      pgendPriceLookup (bend, look);
      look->type = LOOKUP_EARLIEST_AFTER;
      pgendPriceLookup (bend, look);
      return;
   }

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   /* set up the common part of the query */
   p = be->buff; *p = 0;
   p = stpcpy (p, "SELECT * FROM gncPrice"
                  "  WHERE commodity='");
   p = stpcpy (p, gnc_commodity_get_unique_name(look->commodity));
   p = stpcpy (p, "'  AND currency='");
   p = stpcpy (p, gnc_commodity_get_unique_name(look->currency));
   p = stpcpy (p, "' ");

   switch (look->type)
   {
      case LOOKUP_LATEST:
         p = stpcpy (p, "ORDER BY time DESC LIMIT 1;");
         break;
      case LOOKUP_ALL:
         /* Get all prices for this commodity and currency */
         p = stpcpy (p, ";");
         break;
      case LOOKUP_AT_TIME:
         p = stpcpy (p, "AND time='");
         p = gnc_timespec_to_iso8601_buff (look->date, p);
         p = stpcpy (p, "';");
         break;
      case LOOKUP_NEAREST_IN_TIME:
         PERR ("this can't possibly happen but it did!!!");
         p = stpcpy (p, ";");
         break;
      case LOOKUP_LATEST_BEFORE:
         p = stpcpy (p, "AND time <= '");
         p = gnc_timespec_to_iso8601_buff (look->date, p);
         p = stpcpy (p, "' ORDER BY time DESC LIMIT 1;");
         break;
      case LOOKUP_EARLIEST_AFTER:
         p = stpcpy (p, "AND time >= '");
         p = gnc_timespec_to_iso8601_buff (look->date, p);
         p = stpcpy (p, "' ORDER BY time ASC LIMIT 1;");
         break;
      default:
         PERR ("unknown lookup type %d", look->type);
         /* re-enable events */
         pgendEnable(be);
         gnc_engine_resume_events();
         return;
   }

   SEND_QUERY (be, be->buff, );
   pgendGetResults (be, get_price_cb, look->prdb);

   /* insertion into the price db will mark it dirty;
    * but it really isn't at this point. */
   gnc_pricedb_mark_clean (look->prdb);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

}

/* ============================================================= */
/* ============================================================= */
/*         HIGHER LEVEL ROUTINES AND BACKEND PROPER              */
/* ============================================================= */
/* ============================================================= */

static int
pgend_account_commit_edit (Backend * bend, 
                           Account * acct)
{
   AccountGroup *parent;
   char *p;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, acct=%p", be, acct);
   if (!be || !acct) return 1;  /* hack alert hardcode literal */

   if (FALSE == acct->core_dirty)
   {
      parent = xaccAccountGetParent(acct);
      if (parent) parent->saved = 1;
      return 0;
   }

   /* lock it up so that we query and store atomically */
   /* its not at all clear to me that this isn't rife with deadlocks. */
   p = "BEGIN;\n"
       "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n";

   SEND_QUERY (be,p, 555);
   FINISH_QUERY(be->connection);

   /* check to see that the engine version is equal or newer than 
    * whats in the database.  It its not, then some other user has 
    * made changes, and we must roll back. */
   if (0 < pgendAccountCompareVersion (be, acct))
   {
      acct->do_free = FALSE;
      p = "ROLLBACK;";
      SEND_QUERY (be,p,444);
      FINISH_QUERY(be->connection);

      /* hack alert -- we should restore the account data from the 
       * sql back end at this point ! !!! */
      PWARN(" account data in engine is newer\n"
            " account must be rolled back.  This function\n"
            " is not completely implemented !! \n");
      LEAVE ("rolled back");
      return 445;
   }
   acct->version ++;   /* be sure to update the version !! */

   if (acct->do_free)
   {
      const GUID *guid = xaccAccountGetGUID(acct);
      pgendStoreAuditAccount (be, acct, SQL_DELETE);
      pgendKVPDelete (be, guid);

      p = be->buff; *p = 0;
      p = stpcpy (p, "DELETE FROM gncAccount WHERE accountGuid='");
      p = guid_to_string_buff (guid, p);
      p = stpcpy (p, "';");
      SEND_QUERY (be,be->buff, 444);
      FINISH_QUERY(be->connection);
   }
   else
   {
      pgendStoreAccountNoLock (be, acct, FALSE, FALSE);
   }

   p = "COMMIT;\n"
       "NOTIFY gncAccount;";
   SEND_QUERY (be,p,336);
   FINISH_QUERY(be->connection);

   /* Mark this up so that we don't get that annoying gui dialog
    * about having to save to file.  unfortunately,however, this
    * is too liberal, and could screw up synchronization if we've lost
    * contact with the back end at some point.  So hack alert -- fix 
    * this. */
   parent = xaccAccountGetParent(acct);
   if (parent) parent->saved = 1;
   LEAVE ("commited");
   return 0;
}

/* ============================================================= */

static int
pgend_trans_commit_edit (Backend * bend, 
                         Transaction * trans,
                         Transaction * oldtrans)
{
   char * bufp;
   int rollback=0;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, trans=%p", be, trans);
   if (!be || !trans) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   bufp = "BEGIN;\n"
          "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
          "LOCK TABLE gncEntry IN EXCLUSIVE MODE;\n";
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
       *
       * We could directly compare all of the data ... but instead,
       * its more efficient to just compare the version number.
       */
 
#ifdef COMPARE_ALL_TRANSACTION_DATA
      {
      int ndiffs;
      GList *start, *node;

      ndiffs = pgendCompareOneTransactionOnly (be, oldtrans); 
      if (0 < ndiffs) rollback++;

      /* be sure to check the old splits as well ... */
      start = xaccTransGetSplitList (oldtrans);
      for (node=start; node; node=node->next) 
      {
         Split * s = node->data;
         ndiffs = pgendCompareOneSplitOnly (be, s);
         if (0 < ndiffs) rollback++;
      }
      }
#else
      /* roll things back is sql version is newer */
      if (0 < pgendTransactionCompareVersion (be, oldtrans)) { rollback = 1; }

      /* first, see if someone else has already deleted this transaction */
      if (-1 < pgendTransactionGetDeletedVersion (be, oldtrans)) 
      { 
         if (rollback)
         {
            /* Although this situation should never happen, we'll try
             * to gracefully handle it anyway, because otherwuise the 
             * transaction becomes un-modifiable, undeleteable.
             * (This situation might occur with the right combo of bugs 
             * and crashes. We've fixed the bugs, but ...
             */
            char buf[80];
            gnc_timespec_to_iso8601_buff (xaccTransRetDatePostedTS (trans), buf);
            PERR ("The impossible has happened, and thats not good!\n"
                  "\tThe SQL database contains an active transaction that\n"
                  "\talso appears in the audit trail as deleted !!\n"
                  "\tWill try to delete transaction for good\n"
                  "\ttransaction is '%s' %s\n",
                  xaccTransGetDescription (trans), buf);
            rollback = 0;
            trans->do_free = TRUE;
         }
         else
         {
            rollback = 1;
         }
      }
#endif
   
      if (rollback) {
         bufp = "ROLLBACK;";
         SEND_QUERY (be,bufp,444);  /* hack alert hard coded literal */
         FINISH_QUERY(be->connection);
   
         PINFO ("old tranasction didn't match DB, edit rolled back)\n");

         /* What happens here:  We return to the engine with an 
          * error code.  This causes the engine to call 
          * xaccTransRollback(), with then invokes our backend rollback 
          * routine.  Our rollback routine updates from the latest in 
          * the sql database, and voila! we are good to go. 
          */
         return 666;   /* hack alert- hard coded literal */
      } 
   }

   /* if we are here, we are good to go */
   pgendStoreTransactionNoLock (be, trans, FALSE);

   bufp = "COMMIT;\n"
          "NOTIFY gncTransaction;";
   SEND_QUERY (be,bufp,334);
   FINISH_QUERY(be->connection);

   /* If this is the multi-user mode, we need to update the
    * balances as well.  */
   if ((MODE_POLL == be->session_mode) || 
       (MODE_EVENT == be->session_mode))
   {
      GList *node;

      /* loop over the old accounts, as they used to be. */
      for (node = xaccTransGetSplitList(trans->orig); node; node=node->next)
      {
         Split *s = (Split *) node->data;
         Account *acc = xaccSplitGetAccount (s);
         pgendAccountRecomputeOneCheckpoint (be, acc, trans->orig->date_posted);
      }

      /* set checkpoints for the new accounts */
      pgendTransactionRecomputeCheckpoints (be, trans);
   }

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
/* transaction rollback routine.  This routine can be invoked
 * in one of two ways: if the user canceled an edited transaction 
 * by hand, from the gui, or automatically, due to a multi-user
 * edit conflict.  In this latter case, the commit_edit routine
 * above failed, and returned to the engine.  Then the engine
 * xaccTransRollback routine got invoked, which called us.
 * What we do here is to copy the transaction out of the dataabse
 * and into the engine.  This will bring the local engine up
 * to sync from the changes that other users had made.
 */

static int
pgend_trans_rollback_edit (Backend * bend, Transaction * trans)
{
   PGBackend *be = (PGBackend *)bend;
   const GUID * trans_guid;

   if (!be || !trans) return 0;
   ENTER ("be=%p, trans=%p", be, trans);

   /* First, lets see if the other user had deleted this transaction.
    * If so, then we want to delete it from the local cache as well.
    */
   if (-1 < pgendTransactionGetDeletedVersion (be, trans))
   {
      LEAVE ("destroyed");
      return BACKEND_ROLLBACK_DESTROY;
   }

   trans_guid = xaccTransGetGUID (trans);
   pgendCopyTransactionToEngine (be, trans_guid);

   LEAVE ("rolled back");
   return 0;
}

/* ============================================================= */

static int
pgend_price_begin_edit (Backend * bend, GNCPrice *pr)
{
   if (pr && pr->db && pr->db->dirty) 
   {
      PERR ("price db is unexpectedly dirty");
   }
   return 0;
}

static int
pgend_price_commit_edit (Backend * bend, GNCPrice *pr)
{
   char * bufp;
   PGBackend *be = (PGBackend *)bend;

   ENTER ("be=%p, price=%p", be, pr);
   if (!be || !pr) return 1;  /* hack alert hardcode literal */

   /* lock it up so that we query and store atomically */
   bufp = "BEGIN;\n"
          "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n";
   SEND_QUERY (be,bufp, 555);
   FINISH_QUERY(be->connection);

   /* check to see that the engine version is equal or newer than 
    * whats in the database.  It its not, then some other user has 
    * made changes, and we must roll back. */
   if (0 < pgendPriceCompareVersion (be, pr))
   {
      pr->do_free = FALSE;
      bufp = "ROLLBACK;";
      SEND_QUERY (be,bufp,444);
      FINISH_QUERY(be->connection);

      /* hack alert -- we should restore the price data from the 
       * sql back end at this point ! !!! */
      PWARN(" price data in engine is newer\n"
            " price must be rolled back.  This function\n"
            " is not completely implemented !! \n");
      LEAVE ("rolled back");
      return 445;
   }
   pr->version ++;   /* be sure to update the version !! */

   if (pr->do_free) 
   {
      pgendStoreAuditPrice (be, pr, SQL_DELETE);
      bufp = be->buff; *bufp = 0;
      bufp = stpcpy (bufp, "DELETE FROM gncPrice WHERE priceGuid='");
      bufp = guid_to_string_buff (gnc_price_get_guid(pr), bufp);
      bufp = stpcpy (bufp, "';");
      PINFO ("%s\n", be->buff ? be->buff : "(null)");
      SEND_QUERY (be,be->buff, 444);
      FINISH_QUERY(be->connection);
   }
   else 
   { 
      pgendStorePriceNoLock (be, pr, FALSE);
   }

   bufp = "COMMIT;\n"
          "NOTIFY gncPrice;";
   SEND_QUERY (be,bufp,335);
   FINISH_QUERY(be->connection);

   if (pr->db) pr->db->dirty = FALSE;

   LEAVE ("commited");
   return 0;
}

/* ============================================================= */
/* hack alert -- the sane-ness of this algorithm should be reviewed.
 * I can't vouch that there aren't any subtle issues or race conditions
 * lurking in this.  Anyway, with that introduction:
 *
 * The pgendSync() routine 'synchronizes' the accounts & commodities 
 * cached in the engine to those in the database.  It does this first 
 * by writing out all of the accounts and transactions, from the 
 * top-group down, and then re-reading from the database.  This
 * write-then-read cycle has the effect of merging the engine data
 * into the sql database.  Note that version checking is done during
 * the writing: only accounts and transactions that are 'newer' in
 * the engine are written out.  Then during the read cycle, anything
 * in the DB that is newer than what's in the engine is sucked back 
 * into the engine.
 *
 * There are three scenarios to contemplate with the update with
 * this 'sync' operation:
 *
 * 1) Database merge:  the user has two substantialy similar copies
 *    of the same data; the first copy was read into the engine earlier,
 *    and now, in this routine, it is being written into the second. 
 *    Because the merge uses version numbers, this merge should be
 *    'safe' in that only the newer copy of any account or transaction
 *    is merged.  But this 'safety' can break down, in certain cases;
 *    see below.
 * 1a) Same situation as above, except the 'first' copy is a file
 *    that resulted because the user was kicked off-line (off-network)
 *    and saved the data to a file.  Now, coming back on-line, they
 *    are merging the file data back into the central store.
 *
 * This merge is *not* safe when two different users made a change
 * to the same account or transaction.  This routine does not check
 * for such conflicts or report them.  Hack alert: this is a bug that
 * should be fixed.
 *
 * This routine should also check for deleted transactions (that
 * other users have deleted, but are still present in out cache).
 */


static void
pgendSync (Backend *bend, AccountGroup *grp)
{
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, grp=%p", be, grp);

   /* store the account group hierarchy, and then all transactions */
   pgendStoreGroup (be, grp);
   pgendStoreAllTransactions (be, grp);

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   pgendKVPInit(be);
   pgendGetAllAccounts (be, grp);
   if ((MODE_SINGLE_FILE != be->session_mode) &&
       (MODE_SINGLE_UPDATE != be->session_mode))
   {
      Timespec ts = gnc_iso8601_to_timespec_local (CK_BEFORE_LAST_DATE);
      pgendGroupGetAllBalances (be, grp, ts);
   } 
   else
   {
      /* in single user mode, read all the transactions */
      pgendGetAllTransactions (be, grp);
   }

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE(" ");
}

/* ============================================================= */
/* The pgendSyncSingleFile() routine syncs the engine and database.
 *    In single file mode, we treat 'sync' as 'file save'.
 *    We start by deleting *everything*, and then writing 
 *    everything out.  This is rather nasty, ugly and dangerous,
 *    but that's the nature of single-file mode.  Note: we
 *    have to delete everything because in this mode, there is 
 *    no other way of finding out that an account, transaction 
 *    or split was deleted. i.e. there's no other way to delete.  
 *    So start with a clean slate.
 *
 *    The use of this routine/this mode is 'depricated'.
 *    Its handy for testing, sanity-checking, and as a failsafe,
 *    but its use shouldn't be encouraged.
 */

static void
pgendSyncSingleFile (Backend *bend, AccountGroup *grp)
{
   char *p;
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, grp=%p", be, grp);
    
   p = "BEGIN;\n"
       "LOCK TABLE gncAccount IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncCommodity IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncTransaction IN EXCLUSIVE MODE;\n"
       "LOCK TABLE gncEntry IN EXCLUSIVE MODE;\n"
       "DELETE FROM gncEntry;\n"
       "DELETE FROM gncTransaction;\n"
       "DELETE FROM gncAccount;\n"
       "DELETE FROM gncCommodity;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* Store accounts and commodities */
   xaccClearMarkDownGr (grp, 0);
   pgendStoreGroupNoLock (be, grp, TRUE, TRUE);
   xaccClearMarkDownGr (grp, 0);

   /* Recursively walk transactions. Start by reseting the write 
    * flags. We use this to avoid infinite recursion */
   xaccGroupBeginStagedTransactionTraversals(grp);
   xaccGroupStagedTransactionTraversal (grp, 1, trans_traverse_cb, be);

   p = "COMMIT;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   LEAVE(" ");
}

/* ============================================================= */
/* Please read the commend for pgendSync to truly understand
 * how this routine works.  Its somewhat subtle.
 */

static void
pgendSyncPriceDB (Backend *bend, GNCPriceDB *prdb)
{
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, prdb=%p", be, prdb);

   pgendStorePriceDB (be, prdb);

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   pgendGetAllPrices (be, prdb);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   LEAVE(" ");
}

/* ============================================================= */
/* The pgendSyncPriceSingleFile() routine syncs the prices in the 
 *    engine with the database.
 *    In single file mode, we treat 'sync' as 'file save'.
 *    We start by deleting *everything*, and then writing 
 *    everything out.  This is rather nasty, ugly and dangerous,
 *    but that's the nature of single-file mode.  Note: we
 *    have to delete everything because in this mode, there is 
 *    no other way of finding out that a price was deleted. 
 *    i.e. there's no other way to delete.  
 *    So start with a clean slate.
 *
 *    The use of this routine/this mode is 'depricated'.
 *    Its handy for testing, sanity-checking, and as a failsafe,
 *    but its use shouldn't be encouraged.
 */

static void
pgendSyncPriceDBSingleFile (Backend *bend, GNCPriceDB *prdb)
{
   char *p;
   PGBackend *be = (PGBackend *)bend;
   ENTER ("be=%p, prdb=%p", be, prdb);
    
   p = "BEGIN;\n"
       "LOCK TABLE gncPrice IN EXCLUSIVE MODE;\n"
       "DELETE FROM gncPrice;\n";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   /* Store accounts and commodities */
   pgendStorePriceDBNoLock (be, prdb);

   p = "COMMIT;";
   SEND_QUERY (be,p, );
   FINISH_QUERY(be->connection);

   LEAVE(" ");
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
/* Instead of loading the book, just set the lock error */

static AccountGroup *
pgend_book_load_single_lockerr (Backend *bend)
{
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   xaccBackendSetError (&be->be, ERR_BACKEND_LOCKED);
   return NULL;
}

/* ============================================================= */
/* The get_session_cb() routine can determine whether we can start 
 *    a session of the desired type.
 *    The logic used is as follows:
 *    -- if there is any (other) session at all, and we want single
 *       (exclusive) access, then fail.
 *    -- if we want any kind of session, and there is a single
 *       (exclusive) session going, then fail.
 *    -- otherwise, suceed.
 *    Return TRUE if we can get a session.
 *
 *    This routine does not lock, but may be used inside a 
 *    test-n-set atomic operation.
 */

static gpointer
get_session_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   char *lock_holder = (char *) data;
   char *mode = DB_GET_VAL("session_mode", j);

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
             username ? username : "(null)",
             hostname ? hostname : "(null)",
             gecos ? gecos : "(null)",
             mode ? mode : "(null)",
             datestr ? datestr : "(null)");

      PWARN ("The above messages should be handled by the GUI\n");

      if (lock_holder) return lock_holder;
      lock_holder = g_strdup (DB_GET_VAL("sessionGUID",j));
   }
   return lock_holder;
}

static gboolean
pgendSessionCanStart (PGBackend *be, int break_lock)
{
   gboolean retval = TRUE;
   char *p, *lock_holder;

   ENTER (" ");
   /* Find out if there are any open sessions.
    * If 'time_off' is infinity, then user hasn't logged off yet  */
   p = "SELECT * FROM gncSession "
       "WHERE time_off='INFINITY';";
   SEND_QUERY (be,p, FALSE);
   lock_holder = pgendGetResults (be, get_session_cb, NULL);
  
   if (lock_holder) retval = FALSE;

   /* If just one other user has a lock, then will go ahead and 
    * break the lock... If the user approved.  I don't like this
    * but that's what the GUI is set up to do ...
    */
   PINFO ("break_lock=%d nrows=%d lock_holder=%s\n", 
           break_lock, be->nrows,
          lock_holder ? lock_holder : "(null)");
   if (break_lock && (1==be->nrows) && lock_holder)
   {
      p = be->buff; *p=0;
      p = stpcpy (p, "UPDATE gncSession SET time_off='NOW' "
                     "WHERE sessionGuid='");
      p = stpcpy (p, lock_holder);
      p = stpcpy (p, "';");
     
      SEND_QUERY (be,be->buff, retval);
      FINISH_QUERY(be->connection);
      retval = TRUE;
   }

   if (lock_holder) g_free (lock_holder);

   LEAVE (" ");
   return retval;
}


/* ============================================================= */
/* The pgendSessionValidate() routine determines whether a valid 
 *    session could be obtained.  It checks to see if:
 *    1) Database appers to have gnucash data in it
 *    2) the session table can be locked and updated to start
 *       a session.  The update is handled as an atomic test-n-set.
 *    Return TRUE if we have a session.
 */

static gpointer 
is_gnucash_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   if (TRUE == (gboolean) data) return (gpointer) TRUE;

   if (0 == strcmp ("gncsession", (DB_GET_VAL ("tablename", j)))) 
      return (gpointer) TRUE;
   return FALSE;
}

static gboolean
pgendSessionValidate (PGBackend *be, int break_lock)
{
   gboolean retval = FALSE;
   char *p;
   ENTER(" ");

   if (MODE_NONE == be->session_mode) return FALSE;

   /* check to see if this database actually contains 
    * GnuCash data... */
   p = "SELECT * FROM pg_tables; ";
   SEND_QUERY (be,p, FALSE);
   retval = (gboolean) pgendGetResults (be, is_gnucash_cb, (gpointer) FALSE);
   if (FALSE == retval) {
      xaccBackendSetError (&be->be, ERR_BACKEND_DATA_CORRUPT);
      return FALSE;
   }
   
   /* Lock it up so that we test-n-set atomically 
    * i.e. we want to avoid a race condition when testing
    * for the single-user session.
    */
   p = "BEGIN;"
       "LOCK TABLE gncSession IN EXCLUSIVE MODE; ";
   SEND_QUERY (be,p, FALSE);
   FINISH_QUERY(be->connection);

   /* Check to see if we can start a session of the desired type.  */
   if (FALSE == pgendSessionCanStart (be, break_lock))
   {
      /* This error should be treated just like the 
       * file-lock error from the GUI perspective:
       * (The GUI allows users to break the lock, if desired).
       */
      be->be.book_load = pgend_book_load_single_lockerr;
      xaccBackendSetError (&be->be, ERR_BACKEND_LOCKED);
      retval = FALSE;
   } else {

      /* make note of the session. */
      be->sessionGuid = xaccGUIDMalloc();
      guid_new (be->sessionGuid);
      guid_to_string_buff (be->sessionGuid, be->session_guid_str);
      pgendStoreOneSessionOnly (be, (void *)-1, SQL_INSERT);
      retval = TRUE;
   }

   p = "COMMIT;\n"
       "NOTIFY gncSession;";
   SEND_QUERY (be,p, FALSE);
   FINISH_QUERY(be->connection);

   LEAVE(" ");
   return retval;
}

/* ============================================================= */
/* The pgendSessionEnd() routine will log the end of session in 
 *    the session table of the database. 
 */

static void
pgendSessionEnd (PGBackend *be)
{
   char *p;

   if (!be->sessionGuid) return;

   p = be->buff; *p=0;
   p = stpcpy (p, "UPDATE gncSession SET time_off='NOW' "
                  "WHERE sessionGuid='");
   p = stpcpy (p, be->session_guid_str);
   p = stpcpy (p, "';\n"
                  "NOTIFY gncSession;");
  
   SEND_QUERY (be,be->buff, );
   FINISH_QUERY(be->connection);

   xaccGUIDFree (be->sessionGuid); be->sessionGuid = NULL;
   guid_to_string_buff (&nullguid, be->session_guid_str);
}

/* ============================================================= */
/* The pgend_session_end() routine is the main entrypoint into
 *    this backend for terminating a session.  It logs the
 *    end of the session into the gncsession table,  disconnects
 *    from the database, and finally frees all malloced memory.
 */

static void
pgend_session_end (Backend *bend)
{
   int i;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return;

   ENTER("be=%p", be);

   /* mode-specific shutdowns */
   switch (be->session_mode)
   {
      case MODE_SINGLE_FILE:
      case MODE_SINGLE_UPDATE:
         /* although the book may be open in 'single-user' mode right now,
          * it might be opened in multi-user mode next time. Thus, update
          * the account balance checkpoints just in case. 
          */
         pgendGroupRecomputeAllCheckpoints (be, be->topgroup);
         break;

      case MODE_POLL:
         break;

      case MODE_EVENT:
         break;

      default:
         PERR ("bad mode specified");
         break;
   }

   /* prevent further callbacks into backend */
   pgendDisable(be);
   be->be.book_begin = NULL;
   be->be.book_end = NULL;

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

   /* free the path strings */
   for (i=0; i< be->path_cache_size; i++) 
   {
      if ((be->path_cache)[i]) g_free ((be->path_cache)[i]);
      (be->path_cache)[i] = NULL;
   }
   g_free (be->path_cache);
   be->path_cache = NULL;
   be->path_cache_size = 0;
   be->ipath_max = 0;

   LEAVE("be=%p", be);
}

/* ============================================================= */
/* The pgend_book_load_poll() routine loads account info from
 *    the database into the engine.   Its to be used only for 
 *    the poll & event style load, where only the accounts, 
 *    and never the transactions, need to be loaded. 
 */

static AccountGroup *
pgend_book_load_poll (Backend *bend)
{
   Timespec ts = gnc_iso8601_to_timespec_local (CK_BEFORE_LAST_DATE);
   AccountGroup *grp;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   pgendKVPInit(be);
   grp = pgendGetAllAccounts (be, NULL);
   pgendGroupGetAllBalances (be, grp, ts);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   be->topgroup = grp;
   return grp;
}

/* ============================================================= */
/* The pgend_price_load_poll() routine creates the pricedb, but
 * doesn't actually put any prices in it.  These are polled on 
 * an as-needed basis.
 */

static GNCPriceDB *
pgend_price_load_poll (Backend *bend)
{
   GNCPriceDB *prdb;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI  */
   gnc_engine_suspend_events();

   prdb = gnc_pricedb_create();

   /* re-enable events */
   gnc_engine_resume_events();

   return prdb;
}

/* ============================================================= */
/* The pgend_book_load_single() routine loads the engine with
 *    data from the database.  Used only in single-user mode,
 *    it loads account *and* transaction data.  Single-user
 *    mode doesn't require balance checkpoingts, to these are
 *    not handled.
 */

static AccountGroup *
pgend_book_load_single (Backend *bend)
{
   AccountGroup *grp;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   pgendKVPInit(be);
   grp = pgendGetAllAccounts (be, NULL);
   pgendGetAllTransactions (be, grp);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   be->topgroup = grp;
   return grp;
}

/* ============================================================= */
/* The pgend_price_load_single() routine loads the engine with
 *    price data from the database.  
 */

static GNCPriceDB *
pgend_price_load_single (Backend *bend)
{
   GNCPriceDB *prdb;
   PGBackend *be = (PGBackend *)bend;
   if (!be) return NULL;

   /* don't send events  to GUI, don't accept callbacks to backend */
   gnc_engine_suspend_events();
   pgendDisable(be);

   prdb = pgendGetAllPrices (be, NULL);

   /* re-enable events */
   pgendEnable(be);
   gnc_engine_resume_events();

   return prdb;
}

/* ============================================================= */
/* The pgend_session_begin() routine implements the main entrypoint
 *    into the SQL backend code.
 *
 *    1) It parses the URL to find the database, username, password, etc.
 *    2) It makes the first contact to the database, and tries to 
 *       initiate a user session.
 *    3) It creates the GnuCash tables for the first time, if these
 *       need to be created.
 *    4) It logs the user session in the database (gncsession table).
 *    5) loads data from the database into the engine.
 */

static void
pgend_session_begin (GNCBook *sess, const char * sessionid, 
                    gboolean ignore_lock, gboolean create_new_db)
{
   int really_do_create = 0;
   int rc;
   PGBackend *be;
   char *url, *start, *end;
   char *password=NULL;
   char *pg_options=NULL;
   char *pg_tty=NULL;
   char *bufp;

   if (!sess) return;
   be = (PGBackend *) xaccGNCBookGetBackend (sess);

   ENTER("be=%p, sessionid=%s", be,
         sessionid ? sessionid : "(null)");

   /* close any dangling sessions from before; reinitialize */
   pgend_session_end ((Backend *) be);
   pgendInit (be);

   /* Parse the sessionid for the hostname, port number and db name.
    * The expected URL format is
    * postgres://some.host.com/db_name
    * postgres://some.host.com:portno/db_name
    * postgres://localhost/db_name
    * postgres://localhost:nnn/db_name
    * 
    * Also parse urls of the form
    * postgres://some.host.com/db_name?pgkey=pgval&pgkey=pgval
    * e.g.
    * postgres://some.host.com/db_name?user=r00t&pass=3733t&mode=multi-user
    */

   if (strncmp (sessionid, "postgres://", 11)) 
   {
      xaccBackendSetError (&be->be, ERR_BACKEND_BAD_URL);
      return;
   }
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
   if (0x0 == *start) 
   { 
      xaccBackendSetError (&be->be, ERR_BACKEND_BAD_URL);
      g_free(url); 
      return; 
   }

   /* dbname is the last thing before the url-encoded data */
   end = strchr (start, '?');
   if (end) *end = 0;
   be->dbName = g_strdup (start);

   /* loop and parse url-encoded data */
   while (end)
   {
      start = end+1;
      end = strchr (start, '&');
      if (end) *end = 0;

      /* mode keyword */
      if (0 == strncasecmp (start, "mode=", 5))
      {
         start += 5;
         if (0 == strcasecmp (start, "single-file")) {
             be->session_mode = MODE_SINGLE_FILE;
         } else
         if (0 == strcasecmp (start, "single-update")) {
             be->session_mode = MODE_SINGLE_UPDATE;
         } else
         if (0 == strcasecmp (start, "multi-user-poll")) {
             be->session_mode = MODE_POLL;
         } else
         if (0 == strcasecmp (start, "multi-user")) {
             be->session_mode = MODE_EVENT;
         } else
         {
             PWARN ("the following message should be shown in a gui");
             PWARN ("unknown mode %s, will use multi-user mode",
                    start ? start : "(null)");
             be->session_mode = MODE_EVENT;
         } 
         
      } else

      /* username and password */
      if ((0 == strncasecmp (start, "username=", 9)) ||
          (0 == strncasecmp (start, "user=", 5)) ||
          (0 == strncasecmp (start, "login=", 6)))
      {
         start = strchr (start, '=') +1;
         be->username = g_strdup (start);
      } else

      if ((0 == strncasecmp (start, "password=", 9)) ||
          (0 == strncasecmp (start, "passwd=", 7)) ||
          (0 == strncasecmp (start, "pass=", 5)) ||
          (0 == strncasecmp (start, "pwd=", 4)))
      {
         start = strchr (start, '=') +1;
         password = start;
         if (0 == strcmp (password, "''")) password = "";
      } else

      /* postgres-specific options and debug tty  */
      if (0 == strncasecmp (start, "options=", 8))
      {
         start = strchr (start, '=') +1;
         pg_options = start;
      } else

      if (0 == strncasecmp (start, "tty=", 4))
      {
         start = strchr (start, '=') +1;
         pg_tty = start;
      } else

      /* ignore other postgres-specific keywords */
      if ((0 == strncasecmp (start, "host=", 5)) ||
          (0 == strncasecmp (start, "port=", 5)) ||
          (0 == strncasecmp (start, "dbname=", 7)) ||
          (0 == strncasecmp (start, "authtype=", 9)))
      {
         PWARN ("the following message should be shown in a gui");
         PWARN ("ignoring the postgres keyword %s",
                start ? start : "(null)");
      } else
      {
         PWARN ("the following message should be shown in a gui");
         PWARN ("unknown keyword %s, ignoring",
                start ? start : "(null)");
      }
   }


   /* handle localhost as a special case */
   if (!safe_strcmp("localhost", be->hostname))
   {
      g_free (be->hostname);
      be->hostname = NULL;
   }

#ifdef NEW_LOGIN
/* not fully implemented */
   /* Login algorithm.  First, we connect to a default, existing
    * database.  (Hopefully it allows any username and password
    * to connect.  We have a problem we don't know how to recover 
    * from if we can't connect to this.)  We then query pg_database 
    * to see if the desired dabase exists.  (We have a problem if 
    * the dbadmin has set permissions to prevent this query.) 
    * If the user-named db exists, then we connect to it, otherwise
    * we create it before connecting.
    */
   be->connection = PQsetdbLogin (be->hostname,
                                  be->portno,
                                  pg_options, /* trace/debug options */
                                  pg_tty, /* file or tty for debug output */
                                  "template1",
                                  be->username,  /* login */
                                  password);  /* pwd */

   /* check the connection status */
   if (CONNECTION_BAD == PQstatus(be->connection))
   {
      PWARN("Can't connect to default database 'template1':\n"
           "\t%s", 
           PQerrorMessage(be->connection));
      PQfinish (be->connection);

      /* Well, maybe the user-requested database exists, and we 
       * can connect to that ... */
      be->connection = PQsetdbLogin (be->hostname, 
                                     be->portno,
                                     pg_options, /* trace/debug options */
                                     pg_tty, /* file or tty for debug output */
                                     be->dbName, 
                                     be->username,  /* login */
                                     password);  /* pwd */

      /* check the connection status */
      if (CONNECTION_BAD == PQstatus(be->connection))
      {
         PWARN("Connection to database '%s' failed:\n"
               "\t%s", 
               be->dbName ? be->dbName : "(null)",
               PQerrorMessage(be->connection));
   
         PQfinish (be->connection);
         be->connection = NULL;
   
         /* OK, this part is convoluted.
          * I wish that postgres returned usable error codes. 
          * Alas, it does not, so we just bomb out.
          */
         xaccBackendSetError (&be->be, ERR_BACKEND_CANT_CONNECT);
         return;
      }
   } else {

      /* if we are here, then we're connected to 'template1'.
       * Look for entry in the system table pgdatabase i.e. 
       * SELECT datname FROM pg_database; this should tell us 
       * if it exists already, or if it needs to be created. 
       */

      PERR ("not implemented");

   }

#else
   /* Old login algorithm.  We try to connect to the database that
    * the user requested.  If it fails, we get a fatal message from
    * postgres. (Porblem: we don't really know why there was a fatal
    * error, there may be many reasons.  This is the fundamental 
    * problem with this approach.)  If the connect failed, then we
    * create the database, and try again.
    */
   be->connection = PQsetdbLogin (be->hostname, 
                                  be->portno,
                                  pg_options, /* trace/debug options */
                                  pg_tty, /* file or tty for debug output */
                                  be->dbName, 
                                  be->username,  /* login */
                                  password);  /* pwd */


   /* check the connection status */
   if (CONNECTION_BAD == PQstatus(be->connection))
   {
      PWARN("Connection to database '%s' failed:\n"
            "\t%s", 
            be->dbName ? be->dbName : "(null)",
            PQerrorMessage(be->connection));

      PQfinish (be->connection);
      be->connection = NULL;

      /* OK, this part is convoluted.
       * I wish that postgres returned usable error codes. 
       * Alas, it does not, so we guess the true error.
       * If the host is 'localhost', and we couldn't connect,
       * then we assume that its because the database doesn't
       * exist (although this might also happen if the database
       * existed, but the user supplied a bad username/password)
       */
      if (NULL == be->hostname)
      {
         if (create_new_db) {
            really_do_create = TRUE;
         } else {
            xaccBackendSetError (&be->be, ERR_BACKEND_NO_SUCH_DB);
            return;
         }
      }
      else
      {
         xaccBackendSetError (&be->be, ERR_BACKEND_CANT_CONNECT);
         return;
      }
   }

   if (really_do_create)
   {
      char * p;
      be->connection = PQsetdbLogin (be->hostname,
                                     be->portno,
                                     pg_options, /* trace/debug options */
                                     pg_tty, /* file or tty for debug output */
                                     "template1",
                                     be->username,  /* login */
                                     password);  /* pwd */

      /* check the connection status */
      if (CONNECTION_BAD == PQstatus(be->connection))
      {
         PERR("Can't connect to database 'template1':\n"
              "\t%s", 
              PQerrorMessage(be->connection));
         PQfinish (be->connection);
         be->connection = NULL;
         xaccBackendSetError (&be->be, ERR_BACKEND_CANT_CONNECT);
         return;
      }

      /* create the database */
      p = be->buff; *p =0;
      p = stpcpy (p, "CREATE DATABASE ");
      p = stpcpy (p, be->dbName);
      p = stpcpy (p, ";");
      SEND_QUERY (be,be->buff, );
      FINISH_QUERY(be->connection);
      PQfinish (be->connection);

      /* now connect to the newly created database */
      be->connection = PQsetdbLogin (be->hostname, 
                                  be->portno,
                                  pg_options, /* trace/debug options */
                                  pg_tty, /* file or tty for debug output */
                                  be->dbName, 
                                  be->username,  /* login */
                                  password);  /* pwd */

      /* check the connection status */
      if (CONNECTION_BAD == PQstatus(be->connection))
      {
         PERR("Can't connect to the newly created database '%s':\n"
              "\t%s", 
              be->dbName ? be->dbName : "(null)",
              PQerrorMessage(be->connection));
         PQfinish (be->connection);
         be->connection = NULL;
         xaccBackendSetError (&be->be, ERR_BACKEND_CANT_CONNECT);
         return;
      }

      /* Finally, create all the tables and indexes.
       * We do this in pieces, so as not to exceed the max length
       * for postgres queries (which is 8192). 
       */
      SEND_QUERY (be,table_create_str, );
      FINISH_QUERY(be->connection);
      SEND_QUERY (be,table_audit_str, );
      FINISH_QUERY(be->connection);
      SEND_QUERY (be,sql_functions_str, );
      FINISH_QUERY(be->connection);
   }
#endif

   /* free url only after login completed */
   g_free(url);

   // DEBUGCMD (PQtrace(be->connection, stderr));

   /* set the datestyle to something we can parse */
   bufp = "SET DATESTYLE='ISO';";
   SEND_QUERY (be,bufp, );
   FINISH_QUERY(be->connection);

   /* OK, lets see if we can get a valid session */
   rc = pgendSessionValidate (be, ignore_lock);

   /* set up pointers for appropriate behaviour */
   if (rc)
   {
      switch (be->session_mode)
      {
         case MODE_SINGLE_FILE:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_single;
            be->be.price_load = pgend_price_load_single;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = NULL;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = NULL;
            be->be.trans_rollback_edit = NULL;
            be->be.price_begin_edit = NULL;
            be->be.price_commit_edit = NULL;
            be->be.run_query = NULL;
            be->be.price_lookup = NULL;
            be->be.sync = pgendSyncSingleFile;
            be->be.sync_price = pgendSyncPriceDBSingleFile;
            be->be.events_pending = NULL;
            be->be.process_events = NULL;
            PWARN ("mode=single-file is final beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! We think its safe to use.\n");
            break;

         case MODE_SINGLE_UPDATE:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_single;
            be->be.price_load = pgend_price_load_single;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = pgend_account_commit_edit;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = pgend_trans_commit_edit;
            be->be.trans_rollback_edit = NULL;  /* no-op for single user */
            be->be.price_begin_edit = pgend_price_begin_edit;
            be->be.price_commit_edit = pgend_price_commit_edit;
            be->be.run_query = NULL;
            be->be.price_lookup = NULL;
            be->be.sync = pgendSync;
            be->be.sync_price = pgendSyncPriceDB;
            be->be.events_pending = NULL;
            be->be.process_events = NULL;
            PWARN ("mode=single-update is final beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! We think its safe to use.\n");
            break;

         case MODE_POLL:
            pgendEnable(be);
            be->be.book_load = pgend_book_load_poll;
            be->be.price_load = pgend_price_load_poll;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = pgend_account_commit_edit;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = pgend_trans_commit_edit;
            be->be.trans_rollback_edit = pgend_trans_rollback_edit;
            be->be.price_begin_edit = pgend_price_begin_edit;
            be->be.price_commit_edit = pgend_price_commit_edit;
            be->be.run_query = pgendRunQuery;
            be->be.price_lookup = pgendPriceLookup;
            // be->be.sync = pgendSync;
            be->be.sync = NULL;
            be->be.sync_price = pgendSyncPriceDB;
            be->be.events_pending = NULL;
            be->be.process_events = NULL;

            PWARN ("mode=multi-user-poll is beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! If something seems weird, let us know.\n");
            break;

         case MODE_EVENT:
            pgendEnable(be);

            pgendSessionGetPid (be);
            pgendSessionSetupNotifies (be);

            be->be.book_load = pgend_book_load_poll;
            be->be.price_load = pgend_price_load_poll;
            be->be.account_begin_edit = NULL;
            be->be.account_commit_edit = pgend_account_commit_edit;
            be->be.trans_begin_edit = NULL;
            be->be.trans_commit_edit = pgend_trans_commit_edit;
            be->be.trans_rollback_edit = pgend_trans_rollback_edit;
            be->be.price_begin_edit = pgend_price_begin_edit;
            be->be.price_commit_edit = pgend_price_commit_edit;
            be->be.run_query = pgendRunQuery;
            be->be.price_lookup = pgendPriceLookup;
            // be->be.sync = pgendSync;
            be->be.sync = NULL;
            be->be.sync_price = pgendSyncPriceDB;
            be->be.events_pending = pgendEventsPending;
            be->be.process_events = pgendProcessEvents;

            PWARN ("mode=multi-user is beta -- \n"
                   "we've fixed all known bugs but that doesn't mean\n"
                   "there aren't any! If something seems weird, let us know.\n");

            break;

         default:
            PERR ("bad mode specified");
            break;
      }
   }

   LEAVE("be=%p, sessionid=%s", be,
         sessionid ? sessionid : "(null)");
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
   be->snr.price_begin_edit    = be->be.price_begin_edit;
   be->snr.price_commit_edit   = be->be.price_commit_edit;
   be->snr.run_query           = be->be.run_query;
   be->snr.price_lookup        = be->be.price_lookup;
   be->snr.sync                = be->be.sync;
   be->snr.sync_price          = be->be.sync_price;
   be->snr.events_pending      = be->be.events_pending;
   be->snr.process_events      = be->be.process_events;

   be->be.account_begin_edit  = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit    = NULL;
   be->be.trans_commit_edit   = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.price_begin_edit    = NULL;
   be->be.price_commit_edit   = NULL;
   be->be.run_query           = NULL;
   be->be.price_lookup        = NULL;
   be->be.sync                = NULL;
   be->be.sync_price          = NULL;
   be->be.events_pending      = NULL;
   be->be.process_events      = NULL;
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
   be->be.price_begin_edit    = be->snr.price_begin_edit;
   be->be.price_commit_edit   = be->snr.price_commit_edit;
   be->be.run_query           = be->snr.run_query;
   be->be.price_lookup        = be->snr.price_lookup;
   be->be.sync                = be->snr.sync;
   be->be.sync_price          = be->snr.sync_price;
   be->be.events_pending      = be->snr.events_pending;
   be->be.process_events      = be->snr.process_events;
}

/* ============================================================= */
/* The pgendInit() routine initializes the backend private 
 *    structures, mallocs any needed memory, etc.
 */

static void 
pgendInit (PGBackend *be)
{
   int i;
   Timespec ts;

   /* initialize global variable */
   nullguid = *(xaccGUIDNULL());

   /* access mode */
   be->session_mode = MODE_EVENT;
   be->sessionGuid = NULL;
   guid_to_string_buff (&nullguid, be->session_guid_str);

   /* generic backend handlers */
   be->be.book_begin = pgend_session_begin;
   be->be.book_load = NULL;
   be->be.price_load = NULL;
   be->be.book_end = pgend_session_end;

   be->be.account_begin_edit = NULL;
   be->be.account_commit_edit = NULL;
   be->be.trans_begin_edit = NULL;
   be->be.trans_commit_edit = NULL;
   be->be.trans_rollback_edit = NULL;
   be->be.price_begin_edit = NULL;
   be->be.price_commit_edit = NULL;
   be->be.run_query = NULL;
   be->be.price_lookup = NULL;
   be->be.sync = NULL;
   be->be.sync_price = NULL;
   be->be.events_pending = NULL;
   be->be.process_events = NULL;

   be->nest_count = 0;
   pgendDisable(be);

   be->be.last_err = ERR_BACKEND_NO_ERR;

   /* postgres specific data */
   be->hostname = NULL;
   be->portno = NULL;
   be->dbName = NULL;
   be->username = NULL;
   be->connection = NULL;

   be->my_pid = 0;
   be->do_account = 0;
   be->do_checkpoint = 0;
   be->do_price = 0;
   be->do_session = 0;
   be->do_transaction = 0;

   ts.tv_sec = time (0);
   ts.tv_nsec = 0;

   be->last_account = ts;
   be->last_price = ts;
   be->last_transaction = ts;

   be->builder = sqlBuilder_new();

   be->buff = g_malloc (QBUFSIZE);
   be->bufflen = QBUFSIZE;
   be->nrows = 0;

#define INIT_CACHE_SZ 1000
   be->path_cache = (char **) g_malloc (INIT_CACHE_SZ * sizeof(char *));
   be->path_cache_size = INIT_CACHE_SZ;
   for (i=0; i< be->path_cache_size; i++) {
      (be->path_cache)[i] = NULL;
   }
   be->ipath_max = 0;

   be->topgroup = NULL;
}

/* ============================================================= */

Backend * 
pgendNew (void)
{
   PGBackend *be;

   be = g_new0 (PGBackend, 1);
   pgendInit (be);

   return (Backend *) be;
}

/* ======================== END OF FILE ======================== */

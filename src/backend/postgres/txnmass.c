/********************************************************************\
 * txnmass.c -- implements mass transaction fetch                   *
 * Copyright (c) 2000, 2001 Linas Vepstas                           *
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


#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <libpq-fe.h>

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "guid.h"
#include "Transaction.h"
#include "TransactionP.h"

#include "checkpoint.h"
#include "kvp-sql.h"
#include "PostgresBackend.h"
#include "txnmass.h"

#include "putil.h"

static short module = MOD_TXN;

/* ============================================================= */

static gpointer
get_mass_trans_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   GList *node, *xaction_list = (GList *) data;
   Transaction *trans;
   gnc_commodity *currency = NULL;
   Timespec ts;
   GUID trans_guid;

   /* first, see if we already have such a transaction */
   string_to_guid (DB_GET_VAL("transGUID",j), &trans_guid);
   trans = xaccTransLookup (&trans_guid, be->book);
   if (trans)
   {
      /* If transaction already exists, determine whose data is 
       * newer: the engine cache, or the database.  If the
       * engine has newer stuff, ignore the databae contents.
       */

      gint32 db_version, cache_version;
      db_version = atoi (DB_GET_VAL("version",j));
      cache_version = xaccTransGetVersion (trans);
      if (db_version < cache_version) {
         xaccTransBeginEdit (trans);
         xaction_list = g_list_prepend (xaction_list, trans);
         return xaction_list;
       }
      xaccTransBeginEdit (trans);
   }
   else
   {
      trans = xaccMallocTransaction(be->book);
      xaccTransBeginEdit (trans);
      xaccTransSetGUID (trans, &trans_guid);
   }

   xaccTransSetNum (trans, DB_GET_VAL("num",j));
   xaccTransSetDescription (trans, DB_GET_VAL("description",j));
   ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_posted",j));
   xaccTransSetDatePostedTS (trans, &ts);
   ts = gnc_iso8601_to_timespec_local (DB_GET_VAL("date_entered",j));
   xaccTransSetDateEnteredTS (trans, &ts);
   xaccTransSetVersion (trans, atoi(DB_GET_VAL("version",j)));
   trans->idata = atoi (DB_GET_VAL("iguid",j));

   currency = gnc_string_to_commodity (DB_GET_VAL("currency",j), be->book);

   xaccTransSetCurrency (trans, currency);

   /* set timestamp as 'recent' for this data */
   trans->version_check = be->version_check;

   xaction_list = g_list_prepend (xaction_list, trans);

   return xaction_list;
}

/* ============================================================= */

static gpointer
get_mass_entry_cb (PGBackend *be, PGresult *result, int j, gpointer data)
{
   Transaction *trans;
   Account *acc;
   Split *s;
   GUID guid;
   Timespec ts;
   gnc_commodity *modity;
   gint64 acct_frac;
   gint64 num;
   gnc_numeric value, amount;
   gint64 trans_frac = 0;


   /* --------------------------------------------- */
   PINFO ("split GUID=%s", DB_GET_VAL("entryGUID",j));
   guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("entryGUID",j), &guid);
   s = xaccSplitLookup (&guid, be->book);
   if (!s)
   {
      s = xaccMallocSplit(be->book);
      xaccSplitSetGUID(s, &guid);
   }

   /* next, restore all split data */
   xaccSplitSetMemo(s, DB_GET_VAL("memo",j));
   xaccSplitSetAction(s, DB_GET_VAL("action",j));
   ts = gnc_iso8601_to_timespec_local
     (DB_GET_VAL("date_reconciled",j));
   xaccSplitSetDateReconciledTS (s, &ts);

   xaccSplitSetReconcile (s, (DB_GET_VAL("reconciled", j))[0]);
   s->idata = atoi (DB_GET_VAL("iguid",j));

   guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("transGUID",j), &guid);
   trans = xaccTransLookup (&guid, be->book);
   if (!trans)
   {
      PERR ("trans not found, will delete this split\n"
            "\t(split with  guid=%s\n"
            "\twants a trans with guid=%s)\n",
            DB_GET_VAL("entryGUID",j),
            DB_GET_VAL("transGUID",j)
            );
      xaccSplitDestroy (s);
      return NULL;
   }

   xaccTransAppendSplit (trans, s);

   /* --------------------------------------------- */
   /* next, find the account that this split goes into */
   guid = nullguid;  /* just in case the read fails ... */
   string_to_guid (DB_GET_VAL("accountGUID",j), &guid);
   acc = xaccAccountLookup (&guid, be->book);
   if (!acc)
   {
      PERR ("account not found, will delete this split\n"
            "\t(split with  guid=%s\n"
            "\twants an acct with guid=%s)\n",
            DB_GET_VAL("entryGUID",j),
            DB_GET_VAL("accountGUID",j)
            );
      xaccSplitDestroy (s);
      return NULL;
   }

   /* We must set value after split has been inserted into account,
    * since engine references the account SCU to set the value. */
   xaccAccountInsertSplit(acc, s);

   /* we don't know the fraction until after we inserted into the account */
   num = strtoll (DB_GET_VAL("amount", j), NULL, 0);
   modity = xaccAccountGetCommodity (acc);
   acct_frac = gnc_commodity_get_fraction (modity);
   amount = gnc_numeric_create (num, acct_frac);
   xaccSplitSetAmount (s, amount);

   num = strtoll (DB_GET_VAL("value", j), NULL, 0);
   trans_frac = gnc_commodity_get_fraction (xaccTransGetCurrency(trans));
   value = gnc_numeric_create (num, trans_frac);
   xaccSplitSetValue (s, value);

   return NULL;
}

/* ============================================================= */

void
pgendGetMassTransactions (PGBackend *be, AccountGroup *grp)
{
   GList *node, *xaction_list = NULL;

   gnc_engine_suspend_events();
   pgendDisable(be);

   SEND_QUERY (be, "SELECT * FROM gncTransaction;", );

   /* restore the transactions */
   xaccAccountGroupBeginEdit (grp);
   xaction_list = pgendGetResults (be, get_mass_trans_cb, NULL);

   SEND_QUERY (be, "SELECT * FROM gncEntry;", );
   pgendGetResults (be, get_mass_entry_cb, NULL);

   for (node=xaction_list; node; node=node->next)
   {
      Transaction *trans = (Transaction *)node->data;
      GList *splits, *snode;

      /* ------------------------------------------------- */
      /* Restore any kvp data associated with the transaction and splits.
       * We won't do this en-mass, as there currently seems to be no
       * performance advantage to doing so */
   
      if (trans->idata)
      {
         trans->kvp_data = pgendKVPFetch (be, trans->idata, trans->kvp_data);
      }
   
      splits = xaccTransGetSplitList(trans);
      for (snode = splits; snode; snode=snode->next)
      {
         Split *s = snode->data;
         if (s->idata)
         {
            s->kvp_data = pgendKVPFetch (be, s->idata, s->kvp_data);
         }
      }

      /* ------------------------------------------------- */
      xaccTransCommitEdit (trans);
   }
   g_list_free(xaction_list);

   xaccAccountGroupCommitEdit (grp);

   pgendEnable(be);
   gnc_engine_resume_events();
}

/* ======================== END OF FILE ======================== */

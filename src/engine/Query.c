/*
 * FILE:
 * Query.c
 *
 * DESCRIPTION:
 * Provide a simple query engine interface.
 * Note that the query engine is officially a part of the transaction engine, 
 * and thus has direct access to internal structures.
 *
 * HISTORY:
 * created by Linas Vepstas Sept 1998
 * Copyright (c) 1998 Linas Vepstas
 */

#include <limits.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "Query.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"

struct _Query {
   Account ** acc_list;

   /* maximum number of splits to return */
   int max_num_splits;

   /* the earliest, and the latest transaction dates to include */
   Timespec earliest;
   Timespec latest;

   char changed;     /* flag, has the query changed? */
   Split **split_list;
};

/* ================================================== */

Query *
xaccMallocQuery (void)
{
   Query * ret;
   ret =  (Query *) _malloc (sizeof (Query));
   xaccInitQuery (ret);
   return ret;
}

/* ================================================== */

void 
xaccInitQuery (Query *q)
{
   if (!q) return;

   q->acc_list = NULL;
   q->split_list = NULL;
   q->changed = 0; 
   q->max_num_splits = INT_MAX ;

   q->earliest.tv_sec = 0; 
   q->earliest.tv_nsec = 0; 

   /* q->latest.tv_sec = ULONG_MAX; */
   q->latest.tv_sec = LONG_MAX;
   q->latest.tv_nsec = 0; 
}

/* ================================================== */

void 
xaccFreeQuery (Query *q)
{
   if (!q) return;

   if (q->acc_list) _free (q->acc_list);
   q->acc_list = 0x0;

   if (q->split_list) _free (q->split_list);
   q->split_list = 0x0;

   _free (q);
}

/* ================================================== */

void
xaccQuerySetAccounts (Query *q, Account **list)
{
   int i=0;
   Account *acc;

   if (!q) return;
   q->changed = 1; 
   if (q->acc_list) free (q->acc_list);
   q->acc_list = NULL;
   if (!list) return;

   /* copy in the account list */
   i=0; acc = list[0];
   while (acc) {
      i++;  acc = list[i];
   }  

   q->acc_list = (Account **) _malloc ( (i+1) * sizeof (Account *));

   i=0; acc = list[0];
   while (acc) {
      q->acc_list[i] = acc;
      i++;  acc = list[i];
   }  
   q->acc_list[i] = NULL;
}

/* ================================================== */

void
xaccQueryAddAccount (Query *q, Account *addme)
{
   int i=0;
   Account *acc;
   Account **oldlist;

   if (!q || !addme) return;
   q->changed = 1; 
   
   oldlist = q->acc_list;
   i = 0;
   if (oldlist) {
      i=0; acc = oldlist[0];
      while (acc) {
         i++;  acc = oldlist[i];
      }  
   }

   q->acc_list = (Account **) _malloc ( (i+2) * sizeof (Account *));

   i=0; 
   if (oldlist) {
      acc = oldlist[0];
      while (acc) {
         q->acc_list[i] = acc;
         i++;  acc = oldlist[i];
      }  
      free (oldlist);
   }
   q->acc_list[i] = addme;
   i++;
   q->acc_list[i] = NULL;
}

/* ================================================== */

void  
xaccQuerySetMaxSplits (Query *q, int max)
{
   if (!q) return;
   q->max_num_splits = max;
}

/* ================================================== */

static void
SortSplits (Query *q, Split **slist)
{

}

/* ================================================== */

Split **
xaccQueryGetSplits (Query *q)
{
   int i=0, j=0, k=0;
   int nstart, nret, nsplits;
   Split *s, **slist;
   Account *acc;

   if (!q) return NULL;

   /* tmp hack alert */
   q->changed = 1;
   /* if not changed then don't recompute cache */
   if (!(q->changed)) return q->split_list;
   q->changed = 0;

   if (q->split_list) _free (q->split_list);
   q->split_list = NULL;

   /* count up the number of splits we'll have to deal with.
    * Try to limit the CPU pain by trimming out stuff that's 
    * too early and too late now. 
    */
   nsplits = 0;
   if (q->acc_list) {
      i=0; acc = q->acc_list[0];
      while (acc) {

         /* now go through the splits */
         j=0; s = acc->splits[0];
         while (s) {
            if (s->parent->date_posted.tv_sec >= q->earliest.tv_sec) {
               nsplits ++;
            }
            if (s->parent->date_posted.tv_sec > q->latest.tv_sec) {
               break;
            }
            j++; s = acc->splits[j];
         }
         i++; acc = q->acc_list[i];
      }
   }

   /* OK, now get some storage ... concatenate all of the accounts in */
   slist = (Split **) malloc ((nsplits+1) * sizeof (Split *));

   k=0;
   if (q->acc_list) {
      i=0; acc = q->acc_list[0];
      while (acc) {

         /* now go through the splits */
         j=0; s = acc->splits[0];
         while (s) {
            if (s->parent->date_posted.tv_sec >= q->earliest.tv_sec) {
               slist[k] = s; k++;
            }
            if (s->parent->date_posted.tv_sec > q->latest.tv_sec) {
               break;
            }
            j++; s = acc->splits[j];
         }
         i++; acc = q->acc_list[i];
      }
   }
   slist[k] = NULL;

   /* sort them ... */
   SortSplits (q, slist);

   /* make sure we don't return too many splits */
   nret = nsplits;
   if (nret > q->max_num_splits) {
      nret = q->max_num_splits;
      q->split_list =  (Split **) malloc ((nret+1) * sizeof (Split *));
   
      /* return only the last few splits */
      nstart = nsplits - nret;
      if (0 > nstart) nstart = 0;

      /* copy over */
      i=nstart; s = slist[i];
      j = 0;
      while (s) { 
         q->split_list [j] = s;
         j++; i++; s = slist [i];
      }
      q->split_list [j] = NULL;

      /* cleanup memory */
      free (slist);
   } else {

      /* avoid excess mallocs, copies, etc. */
      q->split_list = slist;
   }
   
   return q->split_list;
}

/* ================================================== */



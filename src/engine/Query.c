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

#include "config.h"
#include "Query.h"
#include "util.h"

struct _Query {
   Account ** acc_list;

   /* maximum number of splits to return */
   int max_num_splits;

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
   q->max_num_splits = 2<<30;
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
SortSplits (Query *q)
{

}

/* ================================================== */

Split **
xaccQueryGetSplits (Query *q)
{
   int i=0, j=0;
   int nlist, nstart, nret, nsplits;
   Split *s, **slist;
   Account *acc;

   if (!q) return NULL;

   /* if not changed then don't recompute cache */
   if (!(q->changed)) return q->split_list;
   q->changed = 0;

   if (q->split_list) _free (q->split_list);
   q->split_list = NULL;

   /* count the number of splits in each account */
   nsplits = 0;
   if (q->acc_list) {
      i=0; acc = q->acc_list[0];
      while (acc) {
         nsplits += xaccAccountGetNumSplits (acc);
         i++; acc = q->acc_list[i];
      }
   }

   /* hack alert */
   slist = xaccAccountGetSplitList (q->acc_list[0]);
   if (!slist) return NULL;

   i=0; s = slist[0];
   while (s) { i++; s = slist [i]; }
   nlist = i;
   
   /* make sure we don't return too many splits */
   nret = nlist;
   if (nret > q->max_num_splits) nret = q->max_num_splits;
   q->split_list =  (Split **) malloc ((nret+1) * sizeof (Split *));
   
   /* return only the last few splits */
   nstart = nlist - nret;
   if (0 > nstart) nstart = 0;

   /* copy over */
   i=nstart; s = slist[i];
   j = 0;
   while (s) { 
      q->split_list [j] = s;
      j++; i++; s = slist [i];
   }
   q->split_list [j] = NULL;
   
   return q->split_list;
}

/* ================================================== */



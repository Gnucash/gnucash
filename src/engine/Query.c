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
/********************************************************************\
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <limits.h>
#include <strings.h>

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

#define PROLOG 					\
  char *da, *db;				\
  Transaction *ta;				\
  Transaction *tb;				\
  int retval;					\
						\
  if ( (*sa) && !(*sb) ) return -1;		\
  if ( !(*sa) && (*sb) ) return +1;		\
  if ( !(*sa) && !(*sb) ) return 0;		\
						\
  ta = (*sa)->parent;				\
  tb = (*sb)->parent;				\
						\
  if ( (ta) && !(tb) ) return -1;		\
  if ( !(ta) && (tb) ) return +1;		\
  if ( !(ta) && !(tb) ) return 0;		\



#define CDATE {					\
  /* if dates differ, return */			\
  if ( (ta->date_posted.tv_sec) <		\
       (tb->date_posted.tv_sec)) {		\
    return -1;					\
  } else					\
  if ( (ta->date_posted.tv_sec) >		\
       (tb->date_posted.tv_sec)) {		\
    return +1;					\
  }						\
						\
  /* else, seconds match. check nanoseconds */	\
  if ( (ta->date_posted.tv_nsec) <		\
       (tb->date_posted.tv_nsec)) {		\
    return -1;					\
  } else					\
  if ( (ta->date_posted.tv_nsec) >		\
       (tb->date_posted.tv_nsec)) {		\
    return +1;					\
  }						\
}

#define CNUM {					\
  /* sort on transaction number */		\
  da = ta->num;					\
  db = tb->num;					\
  if (da && db) {				\
    retval = strcmp (da, db);			\
    /* if strings differ, return */		\
    if (retval) return retval;			\
  } else					\
  if (!da && db) {				\
    return -1;					\
  } else					\
  if (da && !db) {				\
    return +1;					\
  }						\
}

#define CMEMO {					\
  /* sort on memo strings */			\
  da = (*sa)->memo;				\
  db = (*sb)->memo;				\
  if (da && db) {				\
    retval = strcmp (da, db);			\
    /* if strings differ, return */		\
    if (retval) return retval;			\
  } else					\
  if (!da && db) {				\
    return -1;					\
  } else					\
  if (da && !db) {				\
    return +1;					\
  }						\
}

#define CDESC {					\
  /* sort on transaction strings */		\
  da = ta->description;				\
  db = tb->description;				\
  if (da && db) {				\
    retval = strcmp (da, db);			\
    /* if strings differ, return */		\
    if (retval) return retval;			\
  } else					\
  if (!da && db) {				\
    return -1;					\
  } else					\
  if (da && !db) {				\
    return +1;					\
  }						\
}

#define CAMT {					\
  double fa, fb;				\
  fa = ((*sa)->damount) * ((*sa)->share_price); \
  fb = ((*sb)->damount) * ((*sb)->share_price); \
  if (fa < fb) {				\
    return -1;					\
  } else   					\
  if (fa > fb) {				\
    return +1;					\
  }						\
}

#define DECLARE(ONE,TWO,THREE,FOUR,FIVE)	\
static int Sort_##ONE##_##TWO##_##THREE##_##FOUR##_##FIVE	\
   (Split **sa, Split **sb) 			\
{						\
   PROLOG; 					\
   C##ONE; C##TWO; C##THREE; C##FOUR; C##FIVE;  \
   return 0;					\
}

/* ================================================== */
/*

#!/usr/bin/perl
#
# This is a short perl script that prints all permutations 
# of five objects; should be easy to generalize to more.

sub rotate {
  local ($n, $i);
  $n = $_[0];

  $tmp = $arr[0];
  for ($i=0; $i<$n-1; $i++) {
     $arr[$i] = $arr[$i+1];
  }
  $arr[$n-1] = $tmp;
}

sub recur {
  local ($n, $i);
  $n = $_[0];

  if (2>=$n) { 
    print "DECLARE ($arr[0], $arr[1], $arr[2], $arr[3],$arr[4])\n";
    return; 
  }

  for ($i=0; $i<$n-1; $i++) {
    &rotate ($n-1);
    &recur ($n-1);
  }
}

@arr=(DESC,MEMO,AMT,NUM,DATE);

&recur (6);

*/
/* ================================================== */

DECLARE (DATE, NUM, AMT, MEMO,DESC)
DECLARE (NUM, DATE, AMT, MEMO,DESC)
DECLARE (AMT, DATE, NUM, MEMO,DESC)
DECLARE (DATE, AMT, NUM, MEMO,DESC)
DECLARE (NUM, AMT, DATE, MEMO,DESC)
DECLARE (AMT, NUM, DATE, MEMO,DESC)
DECLARE (MEMO, DATE, NUM, AMT,DESC)
DECLARE (DATE, MEMO, NUM, AMT,DESC)
DECLARE (NUM, MEMO, DATE, AMT,DESC)
DECLARE (MEMO, NUM, DATE, AMT,DESC)
DECLARE (DATE, NUM, MEMO, AMT,DESC)
DECLARE (NUM, DATE, MEMO, AMT,DESC)
DECLARE (AMT, MEMO, DATE, NUM,DESC)
DECLARE (MEMO, AMT, DATE, NUM,DESC)
DECLARE (DATE, AMT, MEMO, NUM,DESC)
DECLARE (AMT, DATE, MEMO, NUM,DESC)
DECLARE (MEMO, DATE, AMT, NUM,DESC)
DECLARE (DATE, MEMO, AMT, NUM,DESC)
DECLARE (NUM, AMT, MEMO, DATE,DESC)
DECLARE (AMT, NUM, MEMO, DATE,DESC)
DECLARE (MEMO, NUM, AMT, DATE,DESC)
DECLARE (NUM, MEMO, AMT, DATE,DESC)
DECLARE (AMT, MEMO, NUM, DATE,DESC)
DECLARE (MEMO, AMT, NUM, DATE,DESC)
DECLARE (DESC, DATE, NUM, AMT,MEMO)
DECLARE (DATE, DESC, NUM, AMT,MEMO)
DECLARE (NUM, DESC, DATE, AMT,MEMO)
DECLARE (DESC, NUM, DATE, AMT,MEMO)
DECLARE (DATE, NUM, DESC, AMT,MEMO)
DECLARE (NUM, DATE, DESC, AMT,MEMO)
DECLARE (AMT, DESC, DATE, NUM,MEMO)
DECLARE (DESC, AMT, DATE, NUM,MEMO)
DECLARE (DATE, AMT, DESC, NUM,MEMO)
DECLARE (AMT, DATE, DESC, NUM,MEMO)
DECLARE (DESC, DATE, AMT, NUM,MEMO)
DECLARE (DATE, DESC, AMT, NUM,MEMO)
DECLARE (NUM, AMT, DESC, DATE,MEMO)
DECLARE (AMT, NUM, DESC, DATE,MEMO)
DECLARE (DESC, NUM, AMT, DATE,MEMO)
DECLARE (NUM, DESC, AMT, DATE,MEMO)
DECLARE (AMT, DESC, NUM, DATE,MEMO)
DECLARE (DESC, AMT, NUM, DATE,MEMO)
DECLARE (DATE, NUM, AMT, DESC,MEMO)
DECLARE (NUM, DATE, AMT, DESC,MEMO)
DECLARE (AMT, DATE, NUM, DESC,MEMO)
DECLARE (DATE, AMT, NUM, DESC,MEMO)
DECLARE (NUM, AMT, DATE, DESC,MEMO)
DECLARE (AMT, NUM, DATE, DESC,MEMO)
DECLARE (MEMO, DESC, DATE, NUM,AMT)
DECLARE (DESC, MEMO, DATE, NUM,AMT)
DECLARE (DATE, MEMO, DESC, NUM,AMT)
DECLARE (MEMO, DATE, DESC, NUM,AMT)
DECLARE (DESC, DATE, MEMO, NUM,AMT)
DECLARE (DATE, DESC, MEMO, NUM,AMT)
DECLARE (NUM, MEMO, DESC, DATE,AMT)
DECLARE (MEMO, NUM, DESC, DATE,AMT)
DECLARE (DESC, NUM, MEMO, DATE,AMT)
DECLARE (NUM, DESC, MEMO, DATE,AMT)
DECLARE (MEMO, DESC, NUM, DATE,AMT)
DECLARE (DESC, MEMO, NUM, DATE,AMT)
DECLARE (DATE, NUM, MEMO, DESC,AMT)
DECLARE (NUM, DATE, MEMO, DESC,AMT)
DECLARE (MEMO, DATE, NUM, DESC,AMT)
DECLARE (DATE, MEMO, NUM, DESC,AMT)
DECLARE (NUM, MEMO, DATE, DESC,AMT)
DECLARE (MEMO, NUM, DATE, DESC,AMT)
DECLARE (DESC, DATE, NUM, MEMO,AMT)
DECLARE (DATE, DESC, NUM, MEMO,AMT)
DECLARE (NUM, DESC, DATE, MEMO,AMT)
DECLARE (DESC, NUM, DATE, MEMO,AMT)
DECLARE (DATE, NUM, DESC, MEMO,AMT)
DECLARE (NUM, DATE, DESC, MEMO,AMT)
DECLARE (AMT, MEMO, DESC, DATE,NUM)
DECLARE (MEMO, AMT, DESC, DATE,NUM)
DECLARE (DESC, AMT, MEMO, DATE,NUM)
DECLARE (AMT, DESC, MEMO, DATE,NUM)
DECLARE (MEMO, DESC, AMT, DATE,NUM)
DECLARE (DESC, MEMO, AMT, DATE,NUM)
DECLARE (DATE, AMT, MEMO, DESC,NUM)
DECLARE (AMT, DATE, MEMO, DESC,NUM)
DECLARE (MEMO, DATE, AMT, DESC,NUM)
DECLARE (DATE, MEMO, AMT, DESC,NUM)
DECLARE (AMT, MEMO, DATE, DESC,NUM)
DECLARE (MEMO, AMT, DATE, DESC,NUM)
DECLARE (DESC, DATE, AMT, MEMO,NUM)
DECLARE (DATE, DESC, AMT, MEMO,NUM)
DECLARE (AMT, DESC, DATE, MEMO,NUM)
DECLARE (DESC, AMT, DATE, MEMO,NUM)
DECLARE (DATE, AMT, DESC, MEMO,NUM)
DECLARE (AMT, DATE, DESC, MEMO,NUM)
DECLARE (MEMO, DESC, DATE, AMT,NUM)
DECLARE (DESC, MEMO, DATE, AMT,NUM)
DECLARE (DATE, MEMO, DESC, AMT,NUM)
DECLARE (MEMO, DATE, DESC, AMT,NUM)
DECLARE (DESC, DATE, MEMO, AMT,NUM)
DECLARE (DATE, DESC, MEMO, AMT,NUM)
DECLARE (NUM, AMT, MEMO, DESC,DATE)
DECLARE (AMT, NUM, MEMO, DESC,DATE)
DECLARE (MEMO, NUM, AMT, DESC,DATE)
DECLARE (NUM, MEMO, AMT, DESC,DATE)
DECLARE (AMT, MEMO, NUM, DESC,DATE)
DECLARE (MEMO, AMT, NUM, DESC,DATE)
DECLARE (DESC, NUM, AMT, MEMO,DATE)
DECLARE (NUM, DESC, AMT, MEMO,DATE)
DECLARE (AMT, DESC, NUM, MEMO,DATE)
DECLARE (DESC, AMT, NUM, MEMO,DATE)
DECLARE (NUM, AMT, DESC, MEMO,DATE)
DECLARE (AMT, NUM, DESC, MEMO,DATE)
DECLARE (MEMO, DESC, NUM, AMT,DATE)
DECLARE (DESC, MEMO, NUM, AMT,DATE)
DECLARE (NUM, MEMO, DESC, AMT,DATE)
DECLARE (MEMO, NUM, DESC, AMT,DATE)
DECLARE (DESC, NUM, MEMO, AMT,DATE)
DECLARE (NUM, DESC, MEMO, AMT,DATE)
DECLARE (AMT, MEMO, DESC, NUM,DATE)
DECLARE (MEMO, AMT, DESC, NUM,DATE)
DECLARE (DESC, AMT, MEMO, NUM,DATE)
DECLARE (AMT, DESC, MEMO, NUM,DATE)
DECLARE (MEMO, DESC, AMT, NUM,DATE)
DECLARE (DESC, MEMO, AMT, NUM,DATE)

/* ================================================== */

static void
SortSplits (Query *q, Split **slist)
{
  int nsplits =0;
  
  nsplits = 0;
  while (slist[nsplits]) nsplits ++;

  /* run the sort routine on the array */
  qsort (slist, nsplits, sizeof (Split *),
            (int(*)(const void*, const void *)) xaccSplitOrder);
  

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



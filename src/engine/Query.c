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
 * Copyright (c) 1998-2000 Linas Vepstas
 */

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "Query.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ================================================== */

struct _Query {
   Account ** acc_list;

   /* maximum number of splits to return */
   int max_num_splits;

   /* the earliest, and the latest transaction dates to include */
   Timespec earliest;
   Timespec latest;

   Timespec earliest_found;
   Timespec latest_found;

   int (*sort_func)(const void*, const void *);

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

static int Sort_STANDARD_NONE_NONE (Split **, Split **);

#ifndef LONG_LONG_MAX
#define LONG_LONG_MAX 0x7fffffffffffffffLL
#endif
#ifndef LONG_LONG_MIN
#define LONG_LONG_MIN (- (0x7fffffffffffffffLL) -1)
#endif

void 
xaccInitQuery (Query *q)
{
   if (!q) return;

   q->acc_list = NULL;
   q->split_list = NULL;
   q->changed = 0; 
   q->max_num_splits = INT_MAX;

   q->earliest.tv_sec = LONG_LONG_MIN;
   q->earliest.tv_nsec = 0; 

   /* Its a signed, 64-bit int */
   q->latest.tv_sec = LONG_LONG_MAX;
   q->latest.tv_nsec = 0; 

   q->earliest_found.tv_sec = LONG_LONG_MAX;
   q->earliest_found.tv_nsec = 0; 

   q->latest_found.tv_sec = LONG_LONG_MIN;
   q->latest_found.tv_nsec = 0; 

   q->sort_func = (int (*)(const void*, const void *)) Sort_STANDARD_NONE_NONE;
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
   q->changed = 1; 
   q->max_num_splits = max;
}

/* ================================================== */

void  
xaccQuerySetDateRange (Query *q, time_t early, time_t late)
{
   if (!q) return;
   q->changed = 1; 
   q->earliest.tv_sec = early;
   q->latest.tv_sec = late;
}

void  
xaccQuerySetDateRangeL (Query *q, long long early, long long late)
{
   if (!q) return;
   q->changed = 1; 
   q->earliest.tv_sec = early;
   q->latest.tv_sec = late;
}

void
xaccQuerySetEarliest (Query *q, time_t earliest)
{
   if (!q) return;
   q->changed = 1; 
   q->earliest.tv_sec = earliest;
}

void
xaccQuerySetLatest (Query *q, time_t latest)
{
   if (!q) return;
   q->changed = 1; 
   q->latest.tv_sec = latest;
}

void
xaccQuerySetEarliestTS (Query *q, Timespec earliest)
{
  if (!q) return;
  q->changed = 1;
  q->earliest = earliest;
}

void
xaccQuerySetLatestTS (Query *q, Timespec latest)
{
  if (!q) return;
  q->changed = 1;
  q->latest = latest;
}

time_t
xaccQueryGetEarliest (Query *q)
{
  assert(q != NULL);

  return q->earliest.tv_sec;
}

time_t
xaccQueryGetLatest (Query *q)
{
  assert(q != NULL);

  return q->latest.tv_sec;
}

/* ================================================== */

void
xaccQueryShowEarliestDateFound (Query *q)
{
   if (!q) return;
   q->changed = 1; 
   q->earliest.tv_sec = LONG_LONG_MIN;
   q->earliest.tv_nsec = 0; 
}

void
xaccQueryShowLatestDateFound (Query *q)
{
   if (!q) return;
   q->changed = 1; 
   q->latest.tv_sec = LONG_LONG_MAX;
   q->latest.tv_nsec = 0; 
}

/* ================================================== */

/* ================================================== */
/* Note that the sort order for a transaction that is
 * currently being edited is based on its old values, 
 * not in its current edit values.  This is somewhat
 * arbitrary, but it does alleviate annoying behaviour 
 * in the GUI, behaviour that could not be easily
 * rectified there.
 */

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
  if (ta->orig) ta = ta->orig;			\
  if (tb->orig) tb = tb->orig;			\
						\
  if ( (ta) && !(tb) ) return -1;		\
  if ( !(ta) && (tb) ) return +1;		\
  if ( !(ta) && !(tb) ) return 0;		\


#define CSTANDARD {                             \
  retval = xaccSplitDateOrder(sa, sb);          \
  if (retval) return retval;                    \
}

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

static gncBoolean
gnc_strisnum(char *s)
{
  if (s == NULL) return GNC_F;
  if (*s == 0) return GNC_F;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return GNC_F;
  if (!isdigit(*s)) return GNC_F;

  while (*s && isdigit(*s))
    s++;

  if (*s == 0) return GNC_T;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return GNC_T;

  return GNC_F;
}

#define CNUM {					\
  /* sort on transaction number */		\
  unsigned long n1;                             \
  unsigned long n2;                             \
  da = ta->num;					\
  db = tb->num;					\
  if (gnc_strisnum(da)) {                       \
    if (!gnc_strisnum(db)) {                    \
      return -1;                                \
    }                                           \
    sscanf(da, "%lu", &n1);                     \
    sscanf(db, "%lu", &n2);                     \
    if (n1 < n2) {                              \
      return -1;                                \
    }                                           \
    if (n1 == n2) {                             \
      return 0;                                 \
    }                                           \
    return +1;                                  \
  }                                             \
  if (gnc_strisnum(db)) {                       \
    return +1;                                  \
  }                                             \
  if (!da && db) {				\
    return -1;					\
  }     					\
  if (da && !db) {				\
    return +1;					\
  }						\
  if (!da && !db) {                             \
    return 0;                                   \
  }                                             \
  retval = strcmp (da, db);			\
  return retval;                                \
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

#define CAMOUNT {				\
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

#define CNONE

#define DECLARE(ONE,TWO,THREE)			\
static int Sort_##ONE##_##TWO##_##THREE		\
   (Split **sa, Split **sb) 			\
{						\
   PROLOG; 					\
   C##ONE; C##TWO; C##THREE;			\
   return 0;					\
}

/* ================================================== */
/*
#!/usr/bin/perl
#
# This is a short perl script that prints all permutations 
# of three out of five objects; should be easy to generalize to more.
# It was used to generate the code below.

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

  if (3>=$n) { 
    print "DECLARE ($arr[4], $arr[3], $arr[2])\n";
    return; 
  }

  for ($i=0; $i<$n-1; $i++) {
    &rotate ($n-1);
    &recur ($n-1);
  }
}

# @arr=(1..5);
@arr=(DESC,MEMO,AMOUNT,NUM,DATE);

&recur (6);
*/

/* ================================================== */
/* Define the sorting comparison functions */

DECLARE (STANDARD, NONE, NONE)
DECLARE (DESC, MEMO, AMOUNT)
DECLARE (DESC, MEMO, NUM)
DECLARE (DESC, MEMO, DATE)
DECLARE (DESC, AMOUNT, NUM)
DECLARE (DESC, AMOUNT, DATE)
DECLARE (DESC, AMOUNT, MEMO)
DECLARE (DESC, NUM, DATE)
DECLARE (DESC, NUM, MEMO)
DECLARE (DESC, NUM, AMOUNT)
DECLARE (DESC, DATE, MEMO)
DECLARE (DESC, DATE, AMOUNT)
DECLARE (DESC, DATE, NUM)
DECLARE (MEMO, AMOUNT, NUM)
DECLARE (MEMO, AMOUNT, DATE)
DECLARE (MEMO, AMOUNT, DESC)
DECLARE (MEMO, NUM, DATE)
DECLARE (MEMO, NUM, DESC)
DECLARE (MEMO, NUM, AMOUNT)
DECLARE (MEMO, DATE, DESC)
DECLARE (MEMO, DATE, AMOUNT)
DECLARE (MEMO, DATE, NUM)
DECLARE (MEMO, DESC, AMOUNT)
DECLARE (MEMO, DESC, NUM)
DECLARE (MEMO, DESC, DATE)
DECLARE (AMOUNT, NUM, DATE)
DECLARE (AMOUNT, NUM, DESC)
DECLARE (AMOUNT, NUM, MEMO)
DECLARE (AMOUNT, DATE, DESC)
DECLARE (AMOUNT, DATE, MEMO)
DECLARE (AMOUNT, DATE, NUM)
DECLARE (AMOUNT, DESC, MEMO)
DECLARE (AMOUNT, DESC, NUM)
DECLARE (AMOUNT, DESC, DATE)
DECLARE (AMOUNT, MEMO, NUM)
DECLARE (AMOUNT, MEMO, DATE)
DECLARE (AMOUNT, MEMO, DESC)
DECLARE (NUM, DATE, DESC)
DECLARE (NUM, DATE, MEMO)
DECLARE (NUM, DATE, AMOUNT)
DECLARE (NUM, DESC, MEMO)
DECLARE (NUM, DESC, AMOUNT)
DECLARE (NUM, DESC, DATE)
DECLARE (NUM, MEMO, AMOUNT)
DECLARE (NUM, MEMO, DATE)
DECLARE (NUM, MEMO, DESC)
DECLARE (NUM, AMOUNT, DATE)
DECLARE (NUM, AMOUNT, DESC)
DECLARE (NUM, AMOUNT, MEMO)
DECLARE (DATE, DESC, MEMO)
DECLARE (DATE, DESC, AMOUNT)
DECLARE (DATE, DESC, NUM)
DECLARE (DATE, MEMO, AMOUNT)
DECLARE (DATE, MEMO, NUM)
DECLARE (DATE, MEMO, DESC)
DECLARE (DATE, AMOUNT, NUM)
DECLARE (DATE, AMOUNT, DESC)
DECLARE (DATE, AMOUNT, MEMO)
DECLARE (DATE, NUM, DESC)
DECLARE (DATE, NUM, MEMO)
DECLARE (DATE, NUM, AMOUNT)

/* ================================================== */

#define DECIDE(ONE,TWO,THREE)						\
   if ((BY_##ONE == arga) && (BY_##TWO==argb) && (BY_##THREE==argc)) {	\
      q->sort_func = (int (*)(const void*, const void *)) 		\
         Sort_##ONE##_##TWO##_##THREE;					\
   } else


void 
xaccQuerySetSortOrder (Query *q, int arga, int argb, int argc)
{
   if (!q) return;
   q->changed = 1; 

   DECIDE (STANDARD, NONE, NONE)
   DECIDE (DESC, MEMO, AMOUNT)
   DECIDE (DESC, MEMO, NUM)
   DECIDE (DESC, MEMO, DATE)
   DECIDE (DESC, AMOUNT, NUM)
   DECIDE (DESC, AMOUNT, DATE)
   DECIDE (DESC, AMOUNT, MEMO)
   DECIDE (DESC, NUM, DATE)
   DECIDE (DESC, NUM, MEMO)
   DECIDE (DESC, NUM, AMOUNT)
   DECIDE (DESC, DATE, MEMO)
   DECIDE (DESC, DATE, AMOUNT)
   DECIDE (DESC, DATE, NUM)
   DECIDE (MEMO, AMOUNT, NUM)
   DECIDE (MEMO, AMOUNT, DATE)
   DECIDE (MEMO, AMOUNT, DESC)
   DECIDE (MEMO, NUM, DATE)
   DECIDE (MEMO, NUM, DESC)
   DECIDE (MEMO, NUM, AMOUNT)
   DECIDE (MEMO, DATE, DESC)
   DECIDE (MEMO, DATE, AMOUNT)
   DECIDE (MEMO, DATE, NUM)
   DECIDE (MEMO, DESC, AMOUNT)
   DECIDE (MEMO, DESC, NUM)
   DECIDE (MEMO, DESC, DATE)
   DECIDE (AMOUNT, NUM, DATE)
   DECIDE (AMOUNT, NUM, DESC)
   DECIDE (AMOUNT, NUM, MEMO)
   DECIDE (AMOUNT, DATE, DESC)
   DECIDE (AMOUNT, DATE, MEMO)
   DECIDE (AMOUNT, DATE, NUM)
   DECIDE (AMOUNT, DESC, MEMO)
   DECIDE (AMOUNT, DESC, NUM)
   DECIDE (AMOUNT, DESC, DATE)
   DECIDE (AMOUNT, MEMO, NUM)
   DECIDE (AMOUNT, MEMO, DATE)
   DECIDE (AMOUNT, MEMO, DESC)
   DECIDE (NUM, DATE, DESC)
   DECIDE (NUM, DATE, MEMO)
   DECIDE (NUM, DATE, AMOUNT)
   DECIDE (NUM, DESC, MEMO)
   DECIDE (NUM, DESC, AMOUNT)
   DECIDE (NUM, DESC, DATE)
   DECIDE (NUM, MEMO, AMOUNT)
   DECIDE (NUM, MEMO, DATE)
   DECIDE (NUM, MEMO, DESC)
   DECIDE (NUM, AMOUNT, DATE)
   DECIDE (NUM, AMOUNT, DESC)
   DECIDE (NUM, AMOUNT, MEMO)
   DECIDE (DATE, DESC, MEMO)
   DECIDE (DATE, DESC, AMOUNT)
   DECIDE (DATE, DESC, NUM)
   DECIDE (DATE, MEMO, AMOUNT)
   DECIDE (DATE, MEMO, NUM)
   DECIDE (DATE, MEMO, DESC)
   DECIDE (DATE, AMOUNT, NUM)
   DECIDE (DATE, AMOUNT, DESC)
   DECIDE (DATE, AMOUNT, MEMO)
   DECIDE (DATE, NUM, DESC)
   DECIDE (DATE, NUM, MEMO)
   DECIDE (DATE, NUM, AMOUNT)
   {
      printf ("Error: xaccQuerySetSortOrder(): "
         "Invalid or unsupported sort order specified \n");
   }
}

/* ================================================== */

static void
SortSplits (Query *q, Split **slist)
{
  int nsplits =0;
  
  if (!q) return;
  if (!q->sort_func) return;

  nsplits = 0;
  while (slist[nsplits]) nsplits ++;

  /* run the sort routine on the array */
  qsort (slist, nsplits, sizeof (Split *), q->sort_func);

}

/* ================================================== */

Split **
xaccQueryGetSplits (Query *q)
{
   int i=0, j=0, k=0;
   int nstart, nret, nsplits;
   Split *s, **slist;
   Account *acc;

   ENTER ("xaccQueryGetSplits()\n");

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
            if (s->parent->date_posted.tv_sec > q->latest.tv_sec) {
               break;
            }
            if (s->parent->date_posted.tv_sec >= q->earliest.tv_sec) {
               nsplits ++;
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
            if (s->parent->date_posted.tv_sec > q->latest.tv_sec) {
               break;
            }
            if (s->parent->date_posted.tv_sec >= q->earliest.tv_sec) {
               slist[k] = s; k++;
            }
            j++; s = acc->splits[j];
         }
         i++; acc = q->acc_list[i];
      }
   }
   slist[k] = NULL;

   DEBUG ("xaccQueryGetSplits(): will sort %d splits\n", nsplits);

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

   /* gather some data about what we just found */
   /* serious Y2K hack alert -- this should be ULONG_MAX not LONG_MAX */
   q->earliest_found.tv_sec = LONG_MAX; 
   q->earliest_found.tv_nsec = 0; 
   q->latest_found.tv_sec = 0;
   q->latest_found.tv_nsec = 0; 

   slist =  q->split_list;
   i=0; s=slist[0];
   while (s) {
      if (q->earliest_found.tv_sec > s->parent->date_posted.tv_sec) {
         q->earliest_found.tv_sec = s->parent->date_posted.tv_sec;
      }
      if (q->latest_found.tv_sec < s->parent->date_posted.tv_sec) {
         q->latest_found.tv_sec = s->parent->date_posted.tv_sec;
      }
      i++; s=slist[i];
   }
   LEAVE ("xaccQueryGetSplits(): returning  %d splits\n", nret);
   
   return q->split_list;
}

/* ================================================== */

time_t
xaccQueryGetEarliestDateFound (Query *q)
{
   if (!q) return 0;

   return ((time_t) q->earliest_found.tv_sec);
}

time_t
xaccQueryGetLatestDateFound (Query *q)
{
   if (!q) return ULONG_MAX;

   return ((time_t) q->latest_found.tv_sec);
}

/* ================ END OF FILE  ==================== */

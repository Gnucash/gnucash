/********************************************************************\
 * Query.c : api for finding transactions                           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include "config.h"

#include <ctype.h>
#include <glib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>

#include <regex.h>
#include <sys/time.h>
#include <unistd.h>

#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"
#include "AccountP.h"
#include "BackendP.h"
#include "GNCId.h"
#include "GroupP.h"
#include "Query.h"
#include "TransactionP.h"

static short module = MOD_QUERY;

/* the Query makes a subset of all splits based on 3 things: 
 *   - an AND-OR tree of predicates which combine to make a 
 *     split filter 
 *   - a sorting order for the matched splits
 *   - a chop limit which gives the maximum number of sorted
 *     splits to return. */

struct _querystruct {
  /* terms is a list of the OR-terms in a sum-of-products 
   * logical expression. */
  GList *  terms;  

  /* sorting and chopping is independent of the search filter */
  sort_type_t primary_sort;
  sort_type_t secondary_sort;
  sort_type_t tertiary_sort;
  gboolean    primary_increasing;
  gboolean    secondary_increasing;
  gboolean    tertiary_increasing;
  int         max_splits;

  /* cache the results so we don't have to run the whole search 
   * again until it's really necessary */
  int          changed;
  query_run_t  last_run_type; 
  AccountGroup * acct_group;

  GList        * split_list;
  GList        * xtn_list;  
};

/*******************************************************************
 *  predicates for standard match types
 *******************************************************************/

static int  xaccAccountMatchPredicate(Split * s, PredicateData * pd);
static int  xaccActionMatchPredicate(Split * s, PredicateData * pd);
static int  xaccAmountMatchPredicate(Split * s, PredicateData * pd);
static int  xaccBalanceMatchPredicate(Split * s, PredicateData * pd);
static int  xaccClearedMatchPredicate(Split * s, PredicateData * pd);
static int  xaccDateMatchPredicate(Split * s, PredicateData * pd);
static int  xaccDescriptionMatchPredicate(Split * s, PredicateData * pd);
static int  xaccGUIDMatchPredicate(Split * s, PredicateData * pd);
static int  xaccKVPMatchPredicate(Split * s, PredicateData * pd);
static int  xaccMemoMatchPredicate(Split * s, PredicateData * pd);
static int  xaccNumberMatchPredicate(Split * s, PredicateData * pd);
static int  xaccSharePriceMatchPredicate(Split * s, PredicateData * pd);
static int  xaccSharesMatchPredicate(Split * s, PredicateData * pd);

/********************************************************************
 ********************************************************************/


void 
xaccQueryPrint(Query * q) 
{
  GList * aterms;
  GList * i, * j;
  QueryTerm * qt;

  if (!q)
  {
    printf("Query: null\n");
    return;
  }

  printf("Query: max splits = %d\n", q->max_splits);

  /* print and & or terms */
  for(i=q->terms; i; i=i->next) {
    aterms = i->data;
    printf("(");
    for(j=aterms; j; j=j->next) {
      qt = (QueryTerm *)j->data;
      if(!qt->data.base.sense) printf("~");
      printf("%d ", qt->data.type);
    }
    printf(")");
    if(i->next) {
      printf(" | ");
    }    
  }
  printf("\n");

  /* print the node contents */
  for(i=q->terms; i; i=i->next) {
    aterms = i->data;
    printf("aterm=%p\n", aterms);
    for(j=aterms; j; j=j->next) {
      qt = (QueryTerm *)j->data;
      switch (qt->data.base.term_type) 
      {
        case PR_ACCOUNT: {
          GList *p;
          char buff[40];
          printf ("account sense=%d how=%d\n",
                  qt->data.base.sense,
                  qt->data.acct.how);
          for (p=qt->data.acct.account_guids; p; p=p->next) {
             guid_to_string_buff (p->data, buff);
             printf ("\tguid=%s\n", buff);
          }
          for (p=qt->data.acct.accounts; p; p=p->next) {
             printf ("\tacct ptr=%p\n", p->data);
          }
          break;
        }
        case PR_ACTION:
          printf ("action sense=%d case sensitive=%d\n", qt->data.str.sense,
                  qt->data.str.case_sens);
          printf ("\tmatch string=%s \n", qt->data.str.matchstring);
          break;
        case PR_AMOUNT:
          printf ("amount sense=%d how=%d\n", qt->data.amount.sense,
                  qt->data.amount.how);
          printf ("\tsign=%d amount=%f\n", qt->data.amount.amt_sgn,
                  qt->data.amount.amount);
          break;
        case PR_BALANCE:
          printf ("balance sense=%d how=%d\n", qt->data.balance.sense,
                  qt->data.balance.how);
          break;
        case PR_CLEARED:
          printf ("cleared sense=%d how=%d\n", qt->data.cleared.sense,
                  qt->data.cleared.how);
          break;
        case PR_DATE: {
          char buff[40];
          printf ("date sense=%d use_start=%d use_end=%d\n", 
                  qt->data.base.sense,
                  qt->data.date.use_start,
                  qt->data.date.use_end
                  );
          if (qt->data.date.use_start) {
             gnc_timespec_to_iso8601_buff (qt->data.date.start, buff);
             printf ("\tstart date=%s\n", buff);
          }
          if (qt->data.date.use_end) {
             gnc_timespec_to_iso8601_buff (qt->data.date.end, buff);
             printf ("\tend date=%s\n", buff);
          }
          break;
        }
        case PR_DESC:
          printf ("desc sense=%d case sensitive=%d\n", qt->data.str.sense,
                  qt->data.str.case_sens);
          printf ("\tmatch string=%s \n", qt->data.str.matchstring);
          break;

        case PR_GUID: {
          char buff[40];
          printf ("guid sense=%d\n", qt->data.guid.sense);
          guid_to_string_buff (&qt->data.guid.guid, buff);
          printf ("\tguid=%s\n", buff);
          printf ("\tid type=%s\n", qt->data.guid.id_type);
          break;
        }
        case PR_KVP: {
          GSList *node;
          char *str;
          printf ("kvp sense=%d how=%d where=%d\n", qt->data.kvp.sense,
                  qt->data.kvp.how, qt->data.kvp.where);
          printf ("path:");
          for (node = qt->data.kvp.path; node; node = node->next)
          {
            printf (node->data);
            if (node->next)
              printf ("/");
          }
          printf ("\n");
          str = kvp_value_to_string (qt->data.kvp.value);
          printf ("value: %s\n", str);
          g_free (str);
          break;
        }
        case PR_MEMO:
          printf ("memo sense=%d case sensitive=%d\n", qt->data.str.sense,
                  qt->data.str.case_sens);
          printf ("\tmatch string=%s \n", qt->data.str.matchstring);
          break;
        case PR_MISC:
          printf ("misc\n");
          break;
        case PR_NUM:
          printf ("num sense=%d case sensitive=%d\n", qt->data.str.sense,
                  qt->data.str.case_sens);
          printf ("\tmatch string=%s \n", qt->data.str.matchstring);
          break;
        case PR_PRICE:
          printf ("price sense=%d how=%d\n", qt->data.amount.sense,
                  qt->data.amount.how);
          printf ("\tsign=%d amount=%f\n", qt->data.amount.amt_sgn,
                  qt->data.amount.amount);
          break;
        case PR_SHRS:
          printf ("shrs sense=%d how=%d\n", qt->data.amount.sense,
                  qt->data.amount.how);
          printf ("\tsign=%d amount=%f\n", qt->data.amount.amt_sgn,
                  qt->data.amount.amount);
          break;

        default:
          printf ("unkown term type=%d \n", qt->data.base.term_type);
      }
    }
    printf("\n");
    if(i->next) {
      printf("\n");
    }    
  }
}


/********************************************************************
 * xaccInitQuery
 ********************************************************************/

/* initial_term has hand-over semantics! Thus, initial_term must point
 * to newly allocated memory or be NULL. */
static void
xaccInitQuery(Query * q, QueryTerm * initial_term) {
  GList * or  = NULL;
  GList * and = NULL;

  if(initial_term) {
    or   = g_list_alloc();
    and  = g_list_alloc();
    and->data = initial_term;
    or->data  = and;
  }

  if(q->terms)
    xaccQueryClear(q);

  q->terms      = or;
  q->split_list = NULL;
  q->changed    = 1;

  q->max_splits = -1;

  q->primary_sort = BY_STANDARD;
  q->secondary_sort = BY_NONE;
  q->tertiary_sort = BY_NONE;

  q->primary_increasing = TRUE;
  q->secondary_increasing = TRUE;
  q->tertiary_increasing = TRUE;
}


/********************************************************************
 * xaccMallocQuery 
 ********************************************************************/

Query * 
xaccMallocQuery(void) {
  Query * qp     = g_new0(Query, 1);
  xaccInitQuery(qp, NULL);
  return qp;
}


/********************************************************************
 * xaccQuerySwapTerms
 * swaps the terms fields of two queries, mostly to 
 * allow quick pass-off of results.
 ********************************************************************/

static void
xaccQuerySwapTerms(Query * q1, Query * q2) {
  GList * g;

  if (!q1 || !q2)
    return;

  g = q1->terms;
  q1->terms = q2->terms;
  q2->terms = g;

  q1->changed = 1;
  q2->changed = 1;
}


/********************************************************************
 * xaccQueryHasTerms
 * returns the number of 'OR' terms in the query, which is generally
 * used as a truth test.
 ********************************************************************/

int
xaccQueryHasTerms(Query * q) 
{
  if (!q)
    return 0;

  return g_list_length(q->terms);    
}

int
xaccQueryNumTerms(Query * q) 
{
  GList *o;
  int n=0;
  if (!q)
    return 0;

  for(o=q->terms; o; o=o->next) {
     n += g_list_length(o->data);    
  }
  return n;
}

GList *
xaccQueryGetTerms (Query *q)
{
   if (!q) return NULL;
   return q->terms;
}


/********************************************************************
 * xaccQueryHasTermType
 * returns TRUE if the query has any terms of the given type
 ********************************************************************/
gboolean
xaccQueryHasTermType(Query * q, pd_type_t type) {
  GList *or;
  GList *and;

  if (!q)
    return FALSE;

  for(or = q->terms; or; or = or->next) {
    for(and = or->data; and; and = and->next) {
      QueryTerm *qt = and->data;
      if(qt->data.type == type)
        return TRUE;
    }
  }

  return FALSE;
}

static void
free_query_term(QueryTerm *qt)
{
  GList *node;

  if (qt == NULL)
    return;

  switch (qt->data.type)
  {
    case PD_ACCOUNT:
      g_list_free (qt->data.acct.accounts);
      qt->data.acct.accounts = NULL;

      for (node = qt->data.acct.account_guids; node; node = node->next)
        xaccGUIDFree (node->data);

      g_list_free (qt->data.acct.account_guids);
      qt->data.acct.account_guids = NULL;
      break;

    case PD_KVP:
      g_slist_free (qt->data.kvp.path);
      qt->data.kvp.path = NULL;
      kvp_value_delete (qt->data.kvp.value);
      qt->data.kvp.value = NULL;
      break;

    case PD_STRING:
      g_free(qt->data.str.matchstring);
      qt->data.str.matchstring = NULL;
      break;

    default:
      break;
  }

  g_free(qt);
}

static QueryTerm *
copy_query_term(QueryTerm * qt) {
  QueryTerm * nqt;
  GList *node;

  if (qt == NULL)
    return NULL;

  nqt = g_new0(QueryTerm, 1);

  memcpy(nqt, qt, sizeof(QueryTerm));

  switch (nqt->data.type)
  {
    case PD_ACCOUNT:
      nqt->data.acct.accounts = g_list_copy (nqt->data.acct.accounts);
      nqt->data.acct.account_guids =
        g_list_copy (nqt->data.acct.account_guids);
      for (node = nqt->data.acct.account_guids; node; node = node->next)
      {
        GUID *old = node->data;
        GUID *new = xaccGUIDMalloc ();

        *new = *old;
        node->data = new;
      }
      break;

    case PD_KVP: {
      GSList *node;

      nqt->data.kvp.path = g_slist_copy (nqt->data.kvp.path);
      nqt->data.kvp.value = kvp_value_copy (nqt->data.kvp.value);

      for (node = nqt->data.kvp.path; node; node = node->next)
        node->data = g_strdup (node->data);

      break;
    }

    case PD_STRING:
      nqt->data.str.matchstring = g_strdup(nqt->data.str.matchstring);
      break;

    default:
      break;
  }

  return nqt;
}

static GList *
copy_and_terms(GList *and_terms) {
  GList *and = NULL;
  GList *cur_and;

  for(cur_and = and_terms; cur_and; cur_and = cur_and->next)
    and = g_list_prepend(and, copy_query_term (cur_and->data));

  return g_list_reverse(and);
}

static GList * 
copy_or_terms(GList * or_terms) {
  GList * or = NULL;
  GList * cur_or;

  for(cur_or = or_terms; cur_or; cur_or = cur_or->next)
    or = g_list_prepend(or, copy_and_terms(cur_or->data));

  return g_list_reverse(or);
}


/********************************************************************
 * xaccFreeQuery 
 * note that the terms list is freed, so you must have newly 
 * allocated it 
 ********************************************************************/

static void
xaccFreeQueryMembers(Query *q) {
  GList * cur_or;

  if (q == NULL)
    return;

  for(cur_or = q->terms; cur_or; cur_or = cur_or->next) {
    GList * cur_and;

    for(cur_and = cur_or->data; cur_and; cur_and = cur_and->next) {
      free_query_term(cur_and->data);
      cur_and->data = NULL;
    }

    g_list_free(cur_or->data);
    cur_or->data = NULL;
  }

  g_list_free(q->terms);
  q->terms = NULL;

  g_list_free(q->split_list);
  q->split_list = NULL;
}

void    
xaccFreeQuery(Query * q) {
  if (q == NULL)
    return;

  xaccFreeQueryMembers (q);

  g_free(q);
}

Query *
xaccQueryCopy(Query *q) {
  Query *copy;

  if (q == NULL)
    return NULL;

  copy = xaccMallocQuery ();
  xaccFreeQueryMembers (copy);

  copy->terms = copy_or_terms (q->terms);

  copy->primary_sort = q->primary_sort;
  copy->secondary_sort = q->secondary_sort;
  copy->tertiary_sort = q->tertiary_sort;

  copy->primary_increasing = q->primary_increasing;
  copy->secondary_increasing = q->secondary_increasing;      
  copy->tertiary_increasing = q->tertiary_increasing;
  copy->max_splits = q->max_splits;

  copy->changed = q->changed;
  copy->acct_group = q->acct_group;
  copy->split_list = g_list_copy (q->split_list);

  return copy;
}

/********************************************************************
 * xaccQueryInvert 
 * return a newly-allocated Query object which is the 
 * logical inverse of the original.
 ********************************************************************/

Query *
xaccQueryInvert(Query * q) {
  Query  * retval;
  Query  * right, * left, * iright, * ileft;
  QueryTerm * qt;
  GList  * aterms;
  GList  * cur;
  GList  * new_oterm;
  int    num_or_terms;

  num_or_terms = g_list_length(q->terms);

  switch(num_or_terms) {
  case 0:
    retval = xaccMallocQuery();
    retval->max_splits     = q->max_splits;
    retval->acct_group     = q->acct_group;
    break;

    /* this is demorgan expansion for a single AND expression. */
    /* !(abc) = !a + !b + !c */
  case 1:
    retval = xaccMallocQuery();
    retval->max_splits     = q->max_splits;
    retval->acct_group     = q->acct_group;

    aterms = g_list_nth_data(q->terms, 0);
    new_oterm = NULL;
    for(cur=aterms; cur; cur=cur->next) {
      qt = copy_query_term(cur->data);
      qt->data.base.sense = !(qt->data.base.sense);
      new_oterm = g_list_append(NULL, qt);
      retval->terms = g_list_append(retval->terms, new_oterm);
    }
    break;

    /* if there are multiple OR-terms, we just recurse by 
     * breaking it down to !(a + b + c) = 
     * !a * !(b + c) = !a * !b * !c.  */
  default:
    right        = xaccMallocQuery();
    right->terms = copy_or_terms(g_list_nth(q->terms, 1));

    left         = xaccMallocQuery();
    left->terms  = g_list_append(NULL, 
                                 copy_and_terms(g_list_nth_data(q->terms, 0)));

    iright       = xaccQueryInvert(right);
    ileft        = xaccQueryInvert(left);

    retval = xaccQueryMerge(iright, ileft, QUERY_AND);
    retval->max_splits     = q->max_splits;
    retval->acct_group     = q->acct_group;
    retval->changed        = 1;

    xaccFreeQuery(iright);
    xaccFreeQuery(ileft);
    xaccFreeQuery(right);
    xaccFreeQuery(left);
    break;
  }

  return retval;
}


/********************************************************************
 * xaccQueryMerge
 * combine 2 Query objects by the logical operation in "op".
 ********************************************************************/

Query * 
xaccQueryMerge(Query * q1, Query * q2, QueryOp op) {
  
  Query * retval = NULL;
  Query * i1, * i2;
  Query * t1, * t2;
  GList * i, * j;

  if(!q1 || !q2 || !(q1->acct_group == q2->acct_group)) {
    return NULL;
  }

  switch(op) {
  case QUERY_OR:
    retval = xaccMallocQuery();
    retval->terms = 
      g_list_concat(copy_or_terms(q1->terms), copy_or_terms(q2->terms));
    retval->max_splits     = q1->max_splits;
    retval->split_list     = NULL; /* fixme */
    retval->changed        = 1;
    retval->acct_group     = q1->acct_group;
    break;

  case QUERY_AND:
    retval = xaccMallocQuery();
    retval->max_splits     = q1->max_splits;
    retval->split_list     = NULL; /* fixme */
    retval->changed        = 1;
    retval->acct_group     = q1->acct_group;

    for(i=q1->terms; i; i=i->next) {
      for(j=q2->terms; j; j=j->next) {
        retval->terms = 
          g_list_append(retval->terms, 
                        g_list_concat
                        (copy_and_terms(i->data),
                         copy_and_terms(j->data)));
      }
    }
    break;

  case QUERY_NAND:
    /* !(a*b) = (!a + !b) */
    i1     = xaccQueryInvert(q1);
    i2     = xaccQueryInvert(q2);
    retval = xaccQueryMerge(i1, i2, QUERY_OR);
    xaccFreeQuery(i1);
    xaccFreeQuery(i2);
    break;

  case QUERY_NOR:
    /* !(a+b) = (!a*!b) */
    i1     = xaccQueryInvert(q1);
    i2     = xaccQueryInvert(q2);
    retval = xaccQueryMerge(i1, i2, QUERY_AND);
    xaccFreeQuery(i1);
    xaccFreeQuery(i2);
    break;

  case QUERY_XOR:
    /* a xor b = (a * !b) + (!a * b) */
    i1     = xaccQueryInvert(q1);
    i2     = xaccQueryInvert(q2);
    t1     = xaccQueryMerge(q1, i2, QUERY_AND);
    t2     = xaccQueryMerge(i1, q2, QUERY_AND);
    retval = xaccQueryMerge(t1, t2, QUERY_OR);

    xaccFreeQuery(i1);
    xaccFreeQuery(i2);
    xaccFreeQuery(t1);
    xaccFreeQuery(t2);     
    break;
  }

  return retval;
}


/* this sort function just puts account queries at the top of the
 * list.  this lets us skip accounts that have no chance. */
static gint
query_sort_func(gconstpointer pa, gconstpointer pb) {
  const QueryTerm * a = pa;
  const QueryTerm * b = pb;
  if(a->data.type == PD_ACCOUNT) {
    return -1;
  }
  else if(b->data.type == PD_ACCOUNT) {
    return 1;
  }
  else if(a->data.type == PD_AMOUNT) {
    return -1;
  }
  else if(b->data.type == PD_AMOUNT) {
    return 1;
  }
  else {
    return 0;
  }
}

static int 
acct_query_matches(QueryTerm * qt, Account * acct) {
  GList *node;
  gboolean account_in_set = FALSE;
  gboolean first_account = TRUE;

  g_return_val_if_fail(qt && acct, FALSE);
  g_return_val_if_fail(qt->data.type == PD_ACCOUNT, FALSE);

  for(node = qt->data.acct.accounts; node ; node = node->next) {
    if(acct == node->data) {
      account_in_set = TRUE;
      break;
    }
    first_account = FALSE;
  }

  /* if we need the query to match "ALL" accounts, we only return 
   * true for the first acct in the set. */
  switch(qt->data.acct.how) {
  case ACCT_MATCH_ALL:
    return (account_in_set && first_account);
    break;

  case ACCT_MATCH_ANY:
    return account_in_set;
    break;

  case ACCT_MATCH_NONE:
    return !account_in_set;
    break;
  }

  return FALSE;
}

static Query * split_sort_query = NULL;

static int
date_cmp_func(Timespec *t1, Timespec *t2) {
  /* check seconds */
  if (t1->tv_sec < t2->tv_sec) {
    return -1;
  }
  else if (t1->tv_sec > t2->tv_sec) {
    return +1;
  }

  /* else, seconds match. check nanoseconds */
  if (t1->tv_nsec < t2->tv_nsec) {
    return -1;
  }
  else if (t1->tv_nsec > t2->tv_nsec) {
    return +1;
  }

  return 0;
}

/* compared by dates but return 0 if on same day */

static int
date_rounded_cmp_func(Timespec *t1, Timespec *t2)
{
  Timespec canon_t1, canon_t2;
  canon_t1 = timespecCanonicalDayTime(*t1);
  canon_t2 = timespecCanonicalDayTime(*t2);
  return date_cmp_func(&canon_t1, &canon_t2);
}


static int
split_cmp_func(sort_type_t how, gconstpointer ga, gconstpointer gb) 
{
  Split       * sa = (Split *)ga;
  Split       * sb = (Split *)gb;
  Transaction * ta;
  Transaction * tb;
  unsigned long n1;                             
  unsigned long n2;                             
  const char   *da, *db;                               
  gnc_numeric fa, fb;                                
  
  if (sa && !sb)  return -1; 
  if (!sa && sb)  return 1; 
  if (!sa && !sb) return 0; 

  ta = sa->parent; 
  tb = sb->parent; 

  if (ta->orig) ta = ta->orig; 
  if (tb->orig) tb = tb->orig; 

  if ( (ta) && !(tb) ) return -1; 
  if ( !(ta) && (tb) ) return +1; 
  if ( !(ta) && !(tb) ) return 0; 


  switch(how) {
  case BY_STANDARD:
    return xaccSplitDateOrder(sa, sb);
    break;

  case BY_DATE:
    return date_cmp_func(&(ta->date_posted), &(tb->date_posted));

    break;

  case BY_DATE_ROUNDED:
    return date_rounded_cmp_func(&(ta->date_posted), &(tb->date_posted));
    
    break;
    

  case BY_DATE_ENTERED:
    return date_cmp_func(&(ta->date_entered), &(tb->date_entered));

    break;
 
  case BY_DATE_ENTERED_ROUNDED:
    return date_rounded_cmp_func(&(ta->date_entered), &(tb->date_entered));

    break;

  case BY_DATE_RECONCILED:
    return date_cmp_func(&(sa->date_reconciled), &(sb->date_reconciled));

    break;

  case BY_DATE_RECONCILED_ROUNDED:
    return date_rounded_cmp_func(&(sa->date_reconciled), &(sb->date_reconciled));

    break;

  case BY_NUM:
    /* sort on transaction number */              
    da = ta->num;
    db = tb->num;
    if (gnc_strisnum(da)) {
      if (!gnc_strisnum(db)) {                    
        return -1;                                
      }                                           
      sscanf(da, "%lu", &n1);                     
      sscanf(db, "%lu", &n2);                     
      if (n1 < n2) {                              
        return -1;                                
      }                                           
      if (n1 == n2) {                             
        return 0;                                 
      }                                           
      return +1;                                  
    }                                             
    if (gnc_strisnum(db)) {                       
      return +1;                                  
    }                                             
    return safe_strcmp (da, db); 
    break;

  case BY_MEMO:
    /* sort on memo strings */                    
    return safe_strcmp (sa->memo, sb->memo);
    break;

  case BY_DESC:
    /* sort on transaction strings */             
    return safe_strcmp (ta->description, tb->description);
    break;

  case BY_AMOUNT:    
    fa = sa->value;
    fb = sb->value;
    return gnc_numeric_compare(fa, fb);
    break;

  case BY_RECONCILE:
    /* Reconcile flags are sorted as: FREC = YREC < CREC = NREC */
    switch (sa->reconciled) {
      case YREC:
      case FREC:
        if (sb->reconciled == YREC)
          return 0;
        if (sb->reconciled == FREC)
          return 0;
        return -1;
        break;

      case CREC:
      case NREC:
        if (sb->reconciled == CREC)
          return 0;
        if (sb->reconciled == NREC)
          return 0;
        return 1;
        break;
    }
    break;

  case BY_ACCOUNT_FULL_NAME:
    return xaccSplitCompareAccountFullNames(sa, sb);
    
  case BY_ACCOUNT_CODE:
    return xaccSplitCompareAccountCodes(sa, sb);
    break;

  case BY_CORR_ACCOUNT_FULL_NAME:
    return xaccSplitCompareOtherAccountFullNames(sa, sb);

  case BY_CORR_ACCOUNT_CODE:
    return xaccSplitCompareOtherAccountCodes(sa, sb);

  case BY_NONE:
    return 0;
    break;
  }

  return 0;
}

static int
split_sort_func(gconstpointer a, gconstpointer b) {
  int retval;
 
  g_return_val_if_fail (split_sort_query, 0);

  retval = split_cmp_func(split_sort_query->primary_sort, a, b);
  if((retval == 0) && 
     (split_sort_query->secondary_sort != BY_NONE)) {
    retval = split_cmp_func(split_sort_query->secondary_sort, a, b);
    if((retval == 0) &&
       (split_sort_query->tertiary_sort != BY_NONE)) {
      retval = split_cmp_func(split_sort_query->tertiary_sort, a, b);
      return split_sort_query->tertiary_increasing ? retval : - retval;
    }
    else {
      return split_sort_query->secondary_increasing ? retval : - retval;
    }
  }
  else {
    return split_sort_query->primary_increasing ? retval : - retval;
  }
}


/********************************************************************
 * xaccQueryCheckSplit
 * check a single split against the query.  This is a short-circuited
 * and-or check, so sorting it with best criteria first is a win.
 ********************************************************************/

static int
xaccQueryCheckSplit(Query * q, Split * s) {
  GList     * and_ptr;
  GList     * or_ptr;
  QueryTerm * qt;
  int       and_terms_ok=1;
  
  for(or_ptr = q->terms; or_ptr; or_ptr = or_ptr->next) {
    and_terms_ok = 1;
    for(and_ptr = or_ptr->data; and_ptr; and_ptr = and_ptr->next) {
      qt = (QueryTerm *)(and_ptr->data);
      if(((qt->p)(s, &(qt->data))) != qt->data.base.sense) {
        and_terms_ok = 0;
        break;
      }
    }
    if(and_terms_ok) {
      return 1;
    }
  }
  return 0;
}

static GList *
account_list_to_guid_list (GList *accounts)
{
  GList *guids = NULL;
  GList *node;

  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;
    GUID *guid;

    if (!account)
      continue;

    guid = xaccGUIDMalloc ();
    *guid = *xaccAccountGetGUID (account);

    guids = g_list_prepend (guids, guid);
  }

  return g_list_reverse (guids);
}

static GList *
copy_guid_list (GList *guids)
{
  GList *new_guids = NULL;
  GList *node;

  for (node = guids; node; node = node->next)
  {
    GUID *guid = node->data;
    GUID *new_guid;

    if (!guid)
      continue;

    new_guid = xaccGUIDMalloc ();
    *new_guid = *guid;

    new_guids = g_list_prepend (new_guids, new_guid);
  }

  return g_list_reverse (new_guids);
}

static GList *
guid_list_to_account_list (Query * q, GList *guids)
{
  GList *accounts = NULL;
  GList *node;

  if (!q || !q->acct_group || !q->acct_group->book)
    return NULL;

  for (node = guids; node; node = node->next)
  {
    GUID *guid = node->data;
    Account *account;

    if (!guid)
      continue;

    account = xaccAccountLookupEntityTable (guid, q->acct_group->book->entity_table);
    if (!account)
      continue;

    accounts = g_list_prepend (accounts, account);
  }

  return g_list_reverse (accounts);
}

/********************************************************************
 * xaccQueryCompileTerms
 * Prepare terms for processing by xaccQueryGetSplits
 ********************************************************************/
static void
xaccQueryCompileTerms (Query *q)
{
  GList * or_ptr, * and_ptr;

  for(or_ptr = q->terms; or_ptr ; or_ptr = or_ptr->next) {
    for(and_ptr = or_ptr->data; and_ptr; and_ptr = and_ptr->next) {
      QueryTerm *qt = and_ptr->data;
      switch (qt->data.type)
      {
        case PD_ACCOUNT:
          g_list_free (qt->data.acct.accounts);
          qt->data.acct.accounts =
            guid_list_to_account_list (q, qt->data.acct.account_guids);
          break;

        default:
          break;
      }
    }
  }
}

/********************************************************************
 * xaccQueryGetSplits
 * Run the search.
 ********************************************************************/

SplitList *
xaccQueryGetSplits(Query * q) 
{
  GList     * matching_splits=NULL;
  GList     * or_ptr, * and_ptr, * mptr;  
  GList     * all_accts, * node;
  Account   * current;
  QueryTerm * qt;
  Backend   * be;

  int       total_splits_checked = 0;
  int       split_count = 0;
  int       acct_ok;

  if (!q) return NULL;
  ENTER("query=%p", q);

  /* tmp hack alert */
  q->changed = 1;

  if(q->changed == 0) {
    return q->split_list;
  }

  /* prioritize the query terms for a faster search.  This
   * will put account queries at the top of the AND chains. */

  /* FIXME : sort for securities queries and eliminate non-
   * security accounts */
  for(or_ptr = q->terms; or_ptr ; or_ptr = or_ptr->next) {
    and_ptr = or_ptr->data;
    or_ptr->data = g_list_sort(and_ptr, query_sort_func);
  }

  /* prepare the terms for processing */
  xaccQueryCompileTerms (q);

  /* if there is a backend, query the backend, let it fetch the data */
  be = xaccGroupGetBackend (q->acct_group);
  if (be && be->run_query) {
   (be->run_query) (be, q);
  }
  
  /* iterate over accounts */
  all_accts = xaccGroupGetSubAccounts (q->acct_group);

  for (node = all_accts; node; node = node->next) {
    current = node->data;

    if (!current)
      continue;

    /* first, check this account to see if we need to look at it at
     * all.  If the query is "ANY" matching or "NONE" matching, you
     * get what you expect; if it's "ALL" matching, only the first
     * account in the query matches.  This could be optimized to only
     * look at the smallest account. */
    acct_ok = 0;
    for(or_ptr = q->terms; or_ptr ; or_ptr = or_ptr->next) {
      and_ptr = or_ptr->data;
      qt = and_ptr->data;
      
      if(qt->data.type == PD_ACCOUNT) {
        if(acct_query_matches(qt, current)) {
          acct_ok = 1;
          break;
        }
      }
      else {
        /* if the first query term isn't an account, then we have to
         * look at every split in every account. */

        /* FIXME : security accounts can be ruled out */
        acct_ok = 1;
        break;
      }
    }
    
    if(acct_ok) {
      GList *lp;

      /* iterate over splits */
      for(lp = xaccAccountGetSplitList(current); lp; lp = lp->next) {
        Split *s = (Split *) lp->data;
        if(xaccQueryCheckSplit(q, s)) {
          matching_splits = g_list_prepend(matching_splits, s);
          split_count++;
        }
        total_splits_checked++;
      }      
    }
  }

  g_list_free (all_accts);

  /* There is no absolute need to reverse this list, since it's
   * being sorted below. However, in the common case, we will be
   * searching in a single account and returning in the account
   * order, thus reversing will put us in the correct order we
   * want and make the sorting go much faster. */
  matching_splits = g_list_reverse(matching_splits);

  /* now sort the matching splits based on the search criteria 
   * split_sort_query is an unforgivable use of static global data...
   * I just can't figure out how else to do this sanely. */
  split_sort_query = q;
  matching_splits = g_list_sort(matching_splits, split_sort_func);

  /* crop the list to limit the number of splits */
  if((split_count > q->max_splits) && (q->max_splits > -1)) {
    if(q->max_splits > 0) {
      /* mptr is set to the first node of what will be the new list */
      mptr = g_list_nth(matching_splits, split_count - q->max_splits);
      /* mptr should not be NULL, but let's be safe */
      if (mptr != NULL) {
        if (mptr->prev != NULL)
          mptr->prev->next = NULL;
        mptr->prev = NULL;
      }
      g_list_free(matching_splits);
      matching_splits = mptr;
    }
    else { /* q->max_splits == 0 */
      g_list_free(matching_splits);
      matching_splits = NULL;
    }
    split_count = q->max_splits;
  }
  
  q->changed = 0;
  
  g_list_free(q->split_list);
  q->split_list = matching_splits;
  
  return matching_splits;
}

/********************************************************************
 * xaccQueryGetSplitsUniqueTrans 
 * Get splits but no more than one from a given transaction.
 ********************************************************************/

SplitList *
xaccQueryGetSplitsUniqueTrans(Query *q)
{
  GList       * splits = xaccQueryGetSplits(q);
  GList       * current;
  GList       * result = NULL;
  GHashTable  * trans_hash = g_hash_table_new(g_direct_hash, g_direct_equal);

  for (current = splits; current; current = current->next)
  {
    Split *split = current->data;
    Transaction *trans = xaccSplitGetParent (split);

    if (!g_hash_table_lookup (trans_hash, trans))
    {
      g_hash_table_insert (trans_hash, trans, trans);
      result = g_list_prepend (result, split);
    }
  }

  g_hash_table_destroy (trans_hash);

  return g_list_reverse (result);
}

/********************************************************************
 * xaccQueryGetTransactions 
 * Get transactions matching the query terms, specifying whether 
 * we require some or all splits to match 
 ********************************************************************/

static void
query_match_all_filter_func(gpointer key, gpointer value, gpointer user_data) 
{
  Transaction * t = key;
  int         num_matches = GPOINTER_TO_INT(value);
  GList       ** matches = user_data;

  if(num_matches == xaccTransCountSplits(t)) {
    *matches = g_list_prepend(*matches, t);
  }
}

static void
query_match_any_filter_func(gpointer key, gpointer value, gpointer user_data) 
{
  Transaction * t = key;
  GList       ** matches = user_data;
  *matches = g_list_prepend(*matches, t);
}

TransList * 
xaccQueryGetTransactions (Query * q, query_run_t runtype) 
{
  GList       * splits = xaccQueryGetSplits(q);
  GList       * current = NULL;
  GList       * retval = NULL;
  GHashTable  * trans_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
  Transaction * trans = NULL;
  gpointer    val = NULL;
  int         count = 0;
  
  /* iterate over matching splits, incrementing a match-count in
   * the hash table */
  for(current = splits; current; current=current->next) {
    trans = xaccSplitGetParent((Split *)(current->data));
    
    /* don't waste time looking up unless we need the count 
     * information */
    if(runtype == QUERY_MATCH_ALL) {
      val   = g_hash_table_lookup(trans_hash, trans);
      count = GPOINTER_TO_INT(val);
    }
    g_hash_table_insert(trans_hash, trans, GINT_TO_POINTER(count + 1));
  }
  
  /* now pick out the transactions that match */
  if(runtype == QUERY_MATCH_ALL) {
    g_hash_table_foreach(trans_hash, query_match_all_filter_func, 
                         &retval);
  }
  else {
    g_hash_table_foreach(trans_hash, query_match_any_filter_func, 
                         &retval);
  }

  /* ATM we rerun the xtn filter every time.  Need to add checks and 
   * updates for using cached results. */
  q->last_run_type = runtype;
  q->xtn_list = retval;
  
  g_hash_table_destroy(trans_hash);

  return retval;
}

/********************************************************************
 * xaccQueryEqual
 * Compare two queries for equality
 ********************************************************************/

static gboolean
xaccQueryTermEqual (QueryTerm *qt1, QueryTerm *qt2)
{
  GList *l1, *l2;

  if (qt1 == qt2) return TRUE;
  if (!qt1 || !qt2) return FALSE;

  if (qt1->p != qt2->p) return FALSE;
  if (qt1->data.type != qt2->data.type) return FALSE;
  if (qt1->data.base.term_type != qt2->data.base.term_type) return FALSE;
  if (qt1->data.base.sense != qt2->data.base.sense) return FALSE;

  switch (qt1->data.type)
  {
    case PD_DATE:
      if (qt1->data.date.use_start != qt2->data.date.use_start) return FALSE;
      if (qt1->data.date.use_end != qt2->data.date.use_end) return FALSE;
      if (!timespec_equal (&qt1->data.date.start, &qt2->data.date.start))
        return FALSE;
      if (!timespec_equal (&qt1->data.date.end, &qt2->data.date.end))
        return FALSE;
      break;

    case PD_AMOUNT:
      if (qt1->data.amount.how != qt2->data.amount.how) return FALSE;
      if (qt1->data.amount.amt_sgn != qt2->data.amount.amt_sgn) return FALSE;
      if (qt1->data.amount.amount != qt2->data.amount.amount)
        return FALSE;
      break;

    case PD_ACCOUNT:
      if (qt1->data.acct.how != qt2->data.acct.how) return FALSE;
      l1 = qt1->data.acct.account_guids;
      l2 = qt2->data.acct.account_guids;
      if (g_list_length (l1) != g_list_length (l2)) return FALSE;
      for ( ; l1; l1 = l1->next, l2 = l2->next)
        if (!guid_equal (l1->data, l2->data))
          return FALSE;
      break;

    case PD_STRING:
      if (qt1->data.str.case_sens != qt2->data.str.case_sens)
        return FALSE;
      if (qt1->data.str.use_regexp != qt2->data.str.use_regexp)
        return FALSE;
      if (strcmp (qt1->data.str.matchstring, qt2->data.str.matchstring) != 0)
        return FALSE;
      break;

    case PD_CLEARED:
      if (qt1->data.cleared.how != qt2->data.cleared.how) return FALSE;
      break;

    case PD_BALANCE:
      if (qt1->data.balance.how != qt2->data.balance.how) return FALSE;
      break;

    case PD_GUID:
      if (!guid_equal (&qt1->data.guid.guid, &qt2->data.guid.guid))
        return FALSE;
      if (safe_strcmp (qt1->data.guid.id_type, qt2->data.guid.id_type))
        return FALSE;
      break;

    case PD_KVP: {
      GSList *n1, *n2;

      n1 = qt1->data.kvp.path;
      n2 = qt2->data.kvp.path;

      for ( ; n1 && n2; n1 = n1->next, n2 = n2->next)
        if (safe_strcmp (n1->data, n2->data) != 0)
          return FALSE;

      if (n1 || n2)
        return FALSE;

      if (kvp_value_compare (qt1->data.kvp.value, qt2->data.kvp.value) != 0)
        return FALSE;

      break;
    }

    case PD_MISC:
      if (qt1->data.misc.how != qt2->data.misc.how) return FALSE;
      if (qt1->data.misc.data != qt2->data.misc.data) return FALSE;
      break;

    default:
      PERR ("bad query term type");
      return FALSE;
  }

  return TRUE;
}

gboolean
xaccQueryEqual (Query *q1, Query *q2)
{
  GList *or1, *or2;

  if (q1 == q2) return TRUE;
  if (!q1 || !q2) return FALSE;

  if (g_list_length (q1->terms) != g_list_length (q2->terms)) return FALSE;

  for (or1 = q1->terms, or2 = q2->terms; or1;
       or1 = or1->next, or2 = or2->next)
  {
    GList *and1, *and2;

    and1 = or1->data;
    and2 = or2->data;

    if (g_list_length (and1) != g_list_length (and2)) return FALSE;

    for ( ; and1; and1 = and1->next, and2 = and2->next)
      if (!xaccQueryTermEqual (and1->data, and2->data))
        return FALSE;
  }

  if (q1->primary_sort != q2->primary_sort) return FALSE;
  if (q1->secondary_sort != q2->secondary_sort) return FALSE;
  if (q1->tertiary_sort != q2->tertiary_sort) return FALSE;

  if (q1->primary_increasing != q2->primary_increasing) return FALSE;
  if (q1->secondary_increasing != q2->secondary_increasing) return FALSE;
  if (q1->tertiary_increasing != q2->tertiary_increasing) return FALSE;

  if (q1->max_splits != q2->max_splits) return FALSE;

  return TRUE;
}

Predicate
xaccQueryGetPredicate (pr_type_t term_type)
{
  Predicate p = NULL;

  /* the predicates are only known in the local 
   * address space, which is why we have to set them 
   * from the abstract type here. 
   */
  switch (term_type) 
  {
    case PR_ACCOUNT:
      p = & xaccAccountMatchPredicate;
      break;
    case PR_ACTION:
      p = & xaccActionMatchPredicate;
      break;
    case PR_AMOUNT:
      p = & xaccAmountMatchPredicate;
      break;
    case PR_BALANCE:
      p = & xaccBalanceMatchPredicate;
      break;
    case PR_CLEARED:
      p = & xaccClearedMatchPredicate;
      break;
    case PR_DATE:
      p = & xaccDateMatchPredicate;
      break;
    case PR_DESC:
      p = & xaccDescriptionMatchPredicate;
      break;
    case PR_GUID:
      p = & xaccGUIDMatchPredicate;
      break;
    case PR_KVP:
      p = & xaccKVPMatchPredicate;
      break;
    case PR_MEMO:
      p = & xaccMemoMatchPredicate;
      break;
    case PR_NUM:
      p = & xaccNumberMatchPredicate;
      break;
    case PR_PRICE:
      p = & xaccSharePriceMatchPredicate;
      break;
    case PR_SHRS:
      p = & xaccSharesMatchPredicate;
      break;
    case PR_MISC:
      PERR ("misc term must not appear");
      break;
  }
  return p;
}

/********************************************************************
 * xaccQueryAddPredicate
 * Add a predicate an existing query. 
 ********************************************************************/

void
xaccQueryAddPredicate (Query * q, 
                       PredicateData *pred,
                       QueryOp op) 
{
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->data   = *pred;
  qt->p = xaccQueryGetPredicate (qt->data.base.term_type);
  
  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddAccountMatch
 * Add an account filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddAccountMatch(Query * q, GList * accounts, acct_match_t how,
                         QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                       = & xaccAccountMatchPredicate;
  qt->data.type               = PD_ACCOUNT;
  qt->data.base.term_type     = PR_ACCOUNT;
  qt->data.base.sense         = 1;
  qt->data.acct.how           = how;
  qt->data.acct.accounts      = NULL;
  qt->data.acct.account_guids = account_list_to_guid_list (accounts);

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }        
  xaccQuerySwapTerms(q, qr);

  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddAccountGUIDMatch
 * Add an account filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddAccountGUIDMatch(Query * q, GList * account_guids,
                             acct_match_t how, QueryOp op)
{
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                       = & xaccAccountMatchPredicate;
  qt->data.type               = PD_ACCOUNT;
  qt->data.base.term_type     = PR_ACCOUNT;
  qt->data.base.sense         = 1;
  qt->data.acct.how           = how;
  qt->data.acct.accounts      = NULL;
  qt->data.acct.account_guids = copy_guid_list (account_guids);

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }        
  xaccQuerySwapTerms(q, qr);

  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddSingleAccountMatch
 * Add an account filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddSingleAccountMatch(Query * q, Account * acct,
                               QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                   = & xaccAccountMatchPredicate;
  qt->data.type           = PD_ACCOUNT;
  qt->data.base.term_type = PR_ACCOUNT;
  qt->data.base.sense     = 1;
  qt->data.acct.how       = ACCT_MATCH_ANY;
  qt->data.acct.accounts  = g_list_prepend(NULL, acct);
  qt->data.acct.account_guids
    = account_list_to_guid_list (qt->data.acct.accounts);

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * xaccQueryAddDescriptionMatch
 * Add a description filter to an existing query
 ********************************************************************/

void
xaccQueryAddDescriptionMatch(Query * q, const char * matchstring,
                             int case_sens, int use_regexp,
                             QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  int       flags = REG_EXTENDED;

  qt->p                    = & xaccDescriptionMatchPredicate;
  qt->data.type            = PD_STRING;
  qt->data.base.term_type  = PR_DESC;
  qt->data.base.sense      = 1;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = g_strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    char *s;

    for(s = qt->data.str.matchstring; *s; s++) {
      *s = tolower(*s);
    }
  }

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }

  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * xaccQueryAddMemoMatch
 * Add a memo conparison to an existing query
 ********************************************************************/

void
xaccQueryAddMemoMatch(Query * q, const char * matchstring,
                      int case_sens, int use_regexp,
                      QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  int       flags = REG_EXTENDED;

  qt->p                    = & xaccMemoMatchPredicate;
  qt->data.type            = PD_STRING;
  qt->data.base.term_type  = PR_MEMO;
  qt->data.base.sense      = 1;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = g_strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    char *s;

    for(s = qt->data.str.matchstring; *s; s++) {
      *s = tolower(*s);
    }
  }

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }

  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * xaccQueryAddDateMatchTS
 * Add a date filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddDateMatchTS(Query * q, 
                        int use_start,
                        Timespec sts,
                        int use_end,
                        Timespec ets,
                        QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                   = & xaccDateMatchPredicate;
  qt->data.type           = PD_DATE;
  qt->data.base.term_type = PR_DATE;
  qt->data.base.sense     = 1;
  qt->data.date.use_start = use_start;
  qt->data.date.use_end   = use_end;
  qt->data.date.start     = sts;  
  qt->data.date.end       = ets;
  
  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddDateMatch
 * Add a date filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddDateMatch(Query * q, 
                      int use_start, int sday, int smonth, int syear,
                      int use_end, int eday, int emonth, int eyear,
                      QueryOp op) 
{
  /* gcc -O3 will auto-inline this function, avoiding a call overhead */
  xaccQueryAddDateMatchTS (q, use_start,
                           gnc_dmy2timespec(sday, smonth, syear),
                           use_end,
                           gnc_dmy2timespec_end(eday, emonth, eyear),
                           op);
}

/********************************************************************
 * xaccQueryAddDateMatchTT
 * Add a date filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddDateMatchTT(Query * q, 
                        int    use_start,
                        time_t stt,
                        int    use_end,
                        time_t ett,
                        QueryOp op) 
{
  Timespec   sts;
  Timespec   ets;
  
  sts.tv_sec  = (long long)stt;
  sts.tv_nsec = 0;

  ets.tv_sec  = (long long)ett;
  ets.tv_nsec = 0;

  /* gcc -O3 will auto-inline this function, avoiding a call overhead */
  xaccQueryAddDateMatchTS (q, use_start, sts,
                           use_end, ets, op);
  
}

/********************************************************************
 * xaccQueryAddNumberMatch
 * Add a number-field filter 
 ********************************************************************/
void
xaccQueryAddNumberMatch(Query * q, const char * matchstring, int case_sens,
                        int use_regexp, QueryOp op) {
  
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  int       flags = REG_EXTENDED;

  qt->p                    = & xaccNumberMatchPredicate;
  qt->data.type            = PD_STRING;
  qt->data.base.term_type  = PR_NUM;
  qt->data.base.sense      = 1;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = g_strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    char *s;

    for(s = qt->data.str.matchstring; *s; s++) {
      *s = tolower(*s);
    }
  }

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }

  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * xaccQueryAddActionMatch
 * Add a action-field filter 
 ********************************************************************/
void
xaccQueryAddActionMatch(Query * q, const char * matchstring, int case_sens,
                        int use_regexp, QueryOp op) {
  
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  int       flags = REG_EXTENDED;

  qt->p                    = & xaccActionMatchPredicate;
  qt->data.type            = PD_STRING;
  qt->data.base.term_type  = PR_ACTION;
  qt->data.base.sense      = 1;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = g_strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    char *s;

    for(s = qt->data.str.matchstring; *s; s++) {
      *s = tolower(*s);
    }
  }

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }

  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * DxaccQueryAddAmountMatch
 * Add a value filter to an existing query. 
 * FIXME ?? fix what ??
 ********************************************************************/

void
DxaccQueryAddAmountMatch(Query * q, double amt, 
                         amt_match_sgn_t amt_sgn, 
                         amt_match_t how,
                         QueryOp op) {

  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                     = & xaccAmountMatchPredicate;
  qt->data.type             = PD_AMOUNT;
  qt->data.base.term_type   = PR_AMOUNT;
  qt->data.base.sense       = 1;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = amt_sgn;
  qt->data.amount.amount    = amt;

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * DxaccQueryAddSharePriceMatch
 * Add a share-price filter to an existing query. 
 * FIXME  ?? fix what ??
 ********************************************************************/

void
DxaccQueryAddSharePriceMatch(Query * q, double amt, 
                            amt_match_t how,
                            QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  
  qt->p                     = & xaccSharePriceMatchPredicate;
  qt->data.type             = PD_AMOUNT;
  qt->data.base.term_type   = PR_PRICE;
  qt->data.base.sense       = 1;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = AMT_SGN_MATCH_EITHER;
  qt->data.amount.amount    = amt;
  
  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}
 

/********************************************************************
 * DxaccQueryAddSharesMatch
 * Add a quantity filter to an existing query. 
 * FIXME ?? fix what ??
 ********************************************************************/
 
void
DxaccQueryAddSharesMatch(Query * q, double amt, 
                        amt_match_t how,
                        QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  
  qt->p                     = & xaccSharesMatchPredicate;
  qt->data.type             = PD_AMOUNT;
  qt->data.base.term_type   = PR_SHRS;
  qt->data.base.sense       = 1;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = AMT_SGN_MATCH_EITHER;
  qt->data.amount.amount    = amt;
  
  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}


/********************************************************************
 * xaccQueryAddMiscMatch
 * Add an arbitrary predicate to a query.  You really shouldn't
 * do this. 
 ********************************************************************/

void
xaccQueryAddMiscMatch(Query * q, Predicate p, int how, int data,
                      QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                   = p;
  qt->data.type           = PD_MISC;
  qt->data.base.term_type = PR_MISC;
  qt->data.base.sense     = 1;
  qt->data.misc.how       = how;
  qt->data.misc.data      = data;

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddClearedMatch
 * Add a 'cleared' filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddClearedMatch(Query * q, cleared_match_t how,
                         QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  
  qt->p                   = & xaccClearedMatchPredicate;
  qt->data.type           = PD_CLEARED;
  qt->data.base.term_type = PR_CLEARED;
  qt->data.base.sense     = 1;
  qt->data.cleared.how    = how;

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);
  
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddBalanceMatch
 * Add a 'balance' filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddBalanceMatch(Query * q, balance_match_t how, QueryOp op)
{
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                   = & xaccBalanceMatchPredicate;
  qt->data.type           = PD_BALANCE;
  qt->data.base.term_type = PR_BALANCE;
  qt->data.base.sense     = 1;
  qt->data.balance.how    = how;

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddGUIDMatch
 * Add a 'guid' filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddGUIDMatch(Query * q, const GUID *guid,
                      GNCIdType id_type, QueryOp op)
{
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p                   = & xaccGUIDMatchPredicate;
  qt->data.type           = PD_GUID;
  qt->data.base.term_type = PR_GUID;
  qt->data.base.sense     = 1;
  qt->data.guid.guid      = guid ? *guid : *xaccGUIDNULL ();
  qt->data.guid.id_type   = id_type;

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/********************************************************************
 * xaccQueryAddKVPMatch
 * Add a 'kvp' filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddKVPMatch(Query *q, GSList *path, const kvp_value *value,
                     kvp_match_t how, kvp_match_where_t where, QueryOp op)
{
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  GSList    * node;

  qt->p                   = &xaccKVPMatchPredicate;
  qt->data.type           = PD_KVP;
  qt->data.base.term_type = PR_KVP;
  qt->data.base.sense     = 1;
  qt->data.kvp.how        = how;
  qt->data.kvp.where      = where;
  qt->data.kvp.path       = g_slist_copy (path);
  qt->data.kvp.value      = kvp_value_copy (value);

  for (node = qt->data.kvp.path; node; node = node->next)
    node->data = g_strdup (node->data);

  xaccInitQuery(qs, qt);
  xaccQuerySetGroup(qs, q->acct_group);

  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qs);
  xaccFreeQuery(qr);
}

/*******************************************************************
 *  xaccQueryPurgeTerms
 *  delete any terms of a particular type
 *******************************************************************/

void
xaccQueryPurgeTerms(Query * q, pd_type_t type) {
  QueryTerm * qt;
  GList * or;
  GList * and;

  if (!q)
    return;

  for(or = q->terms; or; or = or->next) {
    for(and = or->data; and; and = and->next) {
      qt = and->data;
      if(qt->data.type == type) {
        if(g_list_length(or->data) == 1) {          
          q->terms = g_list_remove_link(q->terms, or);
          g_list_free_1(or);
          or = q->terms;
          break;
        }
        else {
          or->data = g_list_remove_link(or->data, and);
          g_list_free_1(and);
          and = or->data;
          if(!and) break;
        }
        q->changed = 1;
        free_query_term(qt);
      }
    }
    if(!or) break; 
  }
}


/*******************************************************************
 *  xaccQueryClear
 *  remove all terms from a query. 
 *******************************************************************/

void
xaccQueryClear(Query * q) 
{
  Query * q2 = xaccMallocQuery();
  xaccQuerySwapTerms(q, q2);
  q->changed = 1;
  xaccFreeQuery(q2);
}


/*******************************************************************
 *  string_match_predicate
 *  internal subroutine for description and memo matching
 *******************************************************************/

static int
string_match_predicate(const char * s, PredicateData * pd) 
{
  regmatch_t match;

  g_return_val_if_fail(s && pd && (pd->type == PD_STRING), FALSE);

  if(!pd->str.matchstring) return 0;

  if(pd->str.use_regexp) {
    if(!regexec(&pd->str.compiled, s, 1, &match, 0)) {
      return 1;
    }
    else {
      return 0;
    }
  }
  else if(pd->str.case_sens) {
    if(strstr(s, pd->str.matchstring)) return 1;
    else return 0;
  }
  else {
    /* use case-insensitive compare */
    if(strcasestr(s, pd->str.matchstring)) return 1;
    else return 0;
  }

}


/*******************************************************************
 *  value_match_predicate
 *******************************************************************/
static int 
value_match_predicate(double splitamt, PredicateData * pd) {
  switch(pd->amount.how) {
  case AMT_MATCH_ATLEAST:
    return fabs(splitamt) >= pd->amount.amount;
    break;
  case AMT_MATCH_ATMOST:
    return fabs(splitamt) <= pd->amount.amount;
    break;
  case AMT_MATCH_EXACTLY:
    return fabs(fabs(splitamt) - fabs(pd->amount.amount)) < .0001;
    break;
  }

  return 0;
}


/*******************************************************************
 *  xaccAccountMatchPredicate 
 *******************************************************************/
static int 
xaccAccountMatchPredicate(Split * s, PredicateData * pd) { 
  Transaction * parent;
  Split       * split;
  Account     * split_acct;
  GList       * acct_node;
  int         i;
  int         numsplits;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_ACCOUNT, FALSE);

  switch(pd->acct.how) {
  case ACCT_MATCH_ALL:
    /* there must be a split in parent that matches each of the 
     * accounts listed in pd. */
    parent = xaccSplitGetParent(s);
    g_return_val_if_fail(parent, FALSE);
    numsplits = xaccTransCountSplits(parent);
    for(acct_node=pd->acct.accounts; acct_node; acct_node=acct_node->next) {
      for(i=0; i < numsplits; i++) {
        split = xaccTransGetSplit(parent, i);
        if(acct_node->data == xaccSplitGetAccount(split)) {
          /* break here means we found a match before running out 
           * of splits (success) */
          break;
        }
      }
      if(i == numsplits) {
        /* break here means we ran out of splits before finding 
         * an account (failure) */
        break;
      }
    }
    if(acct_node) return 0;
    else return 1;

    break;

  case ACCT_MATCH_ANY:
    /* s must match an account in pd */
    split_acct = xaccSplitGetAccount(s);
    return (g_list_find(pd->acct.accounts, split_acct) != NULL);
    break;

  case ACCT_MATCH_NONE:
    /* s must match no account in pd */
    split_acct = xaccSplitGetAccount(s);
    return (g_list_find(pd->acct.accounts, split_acct) == NULL);
    break;
  }

  return 0;
}


/*******************************************************************
 *  xaccDescriptionMatchPredicate 
 *******************************************************************/
static int
xaccDescriptionMatchPredicate(Split * s, PredicateData * pd) {
  Transaction * parent;
  const char  * descript;
  
  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_STRING, FALSE);

  parent = xaccSplitGetParent(s);
  g_return_val_if_fail(parent, FALSE);

  descript = xaccTransGetDescription(parent);
  return string_match_predicate(descript, pd);
}

/*******************************************************************
 *  xaccGUIDMatchPredicate
 *******************************************************************/
static int
xaccGUIDMatchPredicate(Split * s, PredicateData * pd)
{
  GUID *guid;

  g_return_val_if_fail (s, 0);
  g_return_val_if_fail (pd, 0);
  g_return_val_if_fail (pd->type == PD_GUID, 0);

  guid = &pd->guid.guid;

  if (pd->guid.id_type == GNC_ID_NONE ||
      !safe_strcmp (pd->guid.id_type, GNC_ID_NULL))
    return 0;
  else if (!safe_strcmp (pd->guid.id_type, GNC_ID_ACCOUNT))
    return (xaccSplitGetAccount (s) ==
	    xaccAccountLookupEntityTable (guid, s->entity_table));
  else if (!safe_strcmp (pd->guid.id_type, GNC_ID_TRANS))
      return (xaccSplitGetParent (s) ==
              xaccTransLookupEntityTable (guid, s->entity_table));
  else if (!safe_strcmp (pd->guid.id_type, GNC_ID_SPLIT))
    return s == xaccSplitLookupEntityTable (guid, s->entity_table);
  else
    return 0;
}

/*******************************************************************
 *  xaccKVPMatchPredicate
 *******************************************************************/
static int
kvp_match_helper (GSList *path, kvp_value *value, kvp_match_t how,
                  kvp_frame *frame)
{
  kvp_value *value_2;
  int compare;

  if (!path || !value || !frame || !how) return 0;

  value_2 = kvp_frame_get_slot_path_gslist (frame, path);
  if (!value_2)
    return 0;

  if (kvp_value_get_type (value) != kvp_value_get_type (value_2))
    return 0;

  compare = kvp_value_compare (value_2, value);

  switch (how)
  {
    case KVP_MATCH_LT:
      return (compare < 0);
    case KVP_MATCH_LTE:
      return (compare <= 0);
    case KVP_MATCH_EQ:
      return (compare == 0);
    case KVP_MATCH_GTE:
      return (compare >= 0);
    case KVP_MATCH_GT:
      return (compare > 0);
    default:
      PWARN ("bad match type: %d", how);
      return FALSE;
  }
}

static int
xaccKVPMatchPredicate(Split * s, PredicateData * pd)
{
  KVPPredicateData *kpd;

  g_return_val_if_fail (s, FALSE);
  g_return_val_if_fail (pd, FALSE);
  g_return_val_if_fail (pd->type == PD_KVP, FALSE);

  kpd = &pd->kvp;

  if (kpd->where && KVP_MATCH_SPLIT)
  {
    kvp_frame *frame = xaccSplitGetSlots (s);

    if (kvp_match_helper (kpd->path, kpd->value, kpd->how, frame))
      return TRUE;
  }

  if (kpd->where && KVP_MATCH_TRANS)
  {
    Transaction *trans = xaccSplitGetParent (s);
    kvp_frame *frame = xaccTransGetSlots (trans);

    if (kvp_match_helper (kpd->path, kpd->value, kpd->how, frame))
      return TRUE;
  }

  if (kpd->where && KVP_MATCH_ACCOUNT)
  {
    Account *account = xaccSplitGetAccount (s);
    kvp_frame *frame = xaccAccountGetSlots (account);

    if (kvp_match_helper (kpd->path, kpd->value, kpd->how, frame))
      return TRUE;
  }

  return 0;
}

/*******************************************************************
 *  xaccNumberMatchPredicate 
 *******************************************************************/
static int
xaccNumberMatchPredicate(Split * s, PredicateData * pd) {
  Transaction * parent;
  const char  * number;
  
  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_STRING, FALSE);

  parent = xaccSplitGetParent(s);
  g_return_val_if_fail(parent, FALSE);

  number = xaccTransGetNum(parent);
  return string_match_predicate(number, pd);
}


/*******************************************************************
 *  xaccActionMatchPredicate 
 *******************************************************************/
static int
xaccActionMatchPredicate(Split * s, PredicateData * pd) {
  const char  * action;
  
  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_STRING, FALSE);

  action = xaccSplitGetAction(s);
  return string_match_predicate(action, pd);
}


/*******************************************************************
 *  xaccMemoMatchPredicate 
 *******************************************************************/
static int
xaccMemoMatchPredicate(Split * s, PredicateData * pd) {
  const char  * memo;
  
  g_return_val_if_fail(s && pd, FALSE);
  memo = xaccSplitGetMemo(s);
  
  return string_match_predicate(memo, pd);
}


/*******************************************************************
 *  xaccAmountMatchPredicate 
 *******************************************************************/
static int
xaccAmountMatchPredicate(Split * s, PredicateData * pd) {
  double splitamt;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_AMOUNT, FALSE);

  splitamt = DxaccSplitGetValue(s);
  
  switch(pd->amount.amt_sgn) {
  case AMT_SGN_MATCH_CREDIT:
    if(splitamt > 0.0) return 0;
    break;
  case AMT_SGN_MATCH_DEBIT:
    if(splitamt < 0.0) return 0;
    break;
  default:
    break;
  }

  return value_match_predicate(splitamt, pd);  
}

/*******************************************************************
 *  xaccSharePriceMatchPredicate 
 *******************************************************************/
static int
xaccSharePriceMatchPredicate(Split * s, PredicateData * pd) {
  double   splitamt;
  Account  * acct;
  int      type;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_AMOUNT, FALSE);

  acct = xaccSplitGetAccount(s);
  type = xaccAccountGetType(acct);

  splitamt = DxaccSplitGetSharePrice(s);

  return value_match_predicate(splitamt, pd);  
}


/*******************************************************************
 *  xaccSharesMatchPredicate 
 *******************************************************************/
static int
xaccSharesMatchPredicate(Split * s, PredicateData * pd) {
  double   splitamt;
  Account  * acct;
  int      type;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_AMOUNT, FALSE);
  
  acct = xaccSplitGetAccount(s);
  type = xaccAccountGetType(acct);

  splitamt = DxaccSplitGetShareAmount(s);
  
  return value_match_predicate(splitamt, pd);  
}


/*******************************************************************
 *  xaccDateMatchPredicate 
 *******************************************************************/
static int
xaccDateMatchPredicate(Split * s, PredicateData * pd) {
  Timespec transtime;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_DATE, FALSE);
  
  xaccTransGetDatePostedTS(xaccSplitGetParent(s), &transtime);

  if(pd->date.use_start && pd->date.use_end) {
    return ((transtime.tv_sec >= pd->date.start.tv_sec) &&
            (transtime.tv_sec <= pd->date.end.tv_sec));
  }
  else if(pd->date.use_start) {
    return ((transtime.tv_sec >= pd->date.start.tv_sec));
  }
  else if(pd->date.use_end) {
    return ((transtime.tv_sec <= pd->date.end.tv_sec));
  }
  else {
    return 1;
  }
}

/*******************************************************************
 *  xaccClearedMatchPredicate 
 *******************************************************************/
static int
xaccClearedMatchPredicate(Split * s, PredicateData * pd) {
  int      cstate;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_CLEARED, FALSE);
  
  cstate = xaccSplitGetReconcile(s);
  switch(cstate) {
  case CREC:
    return ((pd->cleared.how & CLEARED_CLEARED) ? 1 : 0);
    break;
  case YREC:
    return ((pd->cleared.how & CLEARED_RECONCILED) ? 1 : 0);
    break;
  case FREC:
    return ((pd->cleared.how & CLEARED_FROZEN) ? 1 : 0);
    break;
  case NREC:
    return ((pd->cleared.how & CLEARED_NO) ? 1 : 0);
    break;      
  case VREC:
    return ((pd->cleared.how & CLEARED_VOIDED) ? 1 : 0);
    break;
    
  }

  return 0;
}

/*******************************************************************
 *  xaccBalanceMatchPredicate 
 *******************************************************************/
static int
xaccBalanceMatchPredicate(Split * s, PredicateData * pd) {
  gboolean balanced;

  g_return_val_if_fail(s && pd, FALSE);
  g_return_val_if_fail(pd->type == PD_BALANCE, FALSE);

  if ((pd->balance.how & BALANCE_BALANCED) &&
      (pd->balance.how & BALANCE_UNBALANCED))
    return 1;

  balanced =
    gnc_numeric_zero_p (xaccTransGetImbalance (xaccSplitGetParent (s)));

  if (balanced && (pd->balance.how & BALANCE_BALANCED))
    return 1;

  if (!balanced && (pd->balance.how & BALANCE_UNBALANCED))
    return 1;

  return 0;
}


/*******************************************************************
 *  xaccQuerySetSortOrder
 *******************************************************************/
void
xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                      sort_type_t secondary, sort_type_t tertiary) {
  if (!q) return;
  q->primary_sort   = primary;
  q->secondary_sort = secondary;
  q->tertiary_sort  = tertiary;

  q->changed = 1;
}

/*******************************************************************
 *  xaccQueryGetPrimarySortOrder
 *******************************************************************/
sort_type_t
xaccQueryGetPrimarySortOrder(Query * q)
{
  if (!q) return BY_NONE;
  return q->primary_sort;
}

/*******************************************************************
 *  xaccQueryGetSecondarySortOrder
 *******************************************************************/
sort_type_t
xaccQueryGetSecondarySortOrder(Query * q)
{
  if (!q) return BY_NONE;
  return q->secondary_sort;
}

/*******************************************************************
 *  xaccQueryGetTertiarySortOrder
 *******************************************************************/
sort_type_t
xaccQueryGetTertiarySortOrder(Query * q)
{
  if (!q) return BY_NONE;
  return q->tertiary_sort;
}

/*******************************************************************
 *  xaccQuerySetSortIncreasing
 *******************************************************************/
void
xaccQuerySetSortIncreasing(Query * q, gboolean prim_increasing,
			   gboolean sec_increasing, 
			   gboolean tert_increasing)
{
  if (!q) return;
  q->primary_increasing = prim_increasing;
  q->secondary_increasing = sec_increasing;
  q->tertiary_increasing = tert_increasing;
  return;
}

/*******************************************************************
 *  xaccQueryGetSortPrimaryIncreasing
 *******************************************************************/
gboolean
xaccQueryGetSortPrimaryIncreasing (Query *q)
{
  if (!q) return TRUE;
  return q->primary_increasing;
}

/*******************************************************************
 *  xaccQueryGetSortSecondaryIncreasing
 *******************************************************************/
gboolean
xaccQueryGetSortSecondaryIncreasing (Query *q)
{
  if (!q) return TRUE;
  return q->secondary_increasing;
}

/*******************************************************************
 *  xaccQueryGetSortTertiaryIncreasing
 *******************************************************************/
gboolean
xaccQueryGetSortTertiaryIncreasing (Query *q)
{
  if (!q) return TRUE;
  return q->tertiary_increasing;
}

/*******************************************************************
 *  xaccQuerySetMaxSplits
 *******************************************************************/
void
xaccQuerySetMaxSplits(Query * q, int n) {
  if (!q) return;
  q->max_splits = n;
}

int
xaccQueryGetMaxSplits(Query * q) {
  if (!q) return 0;
  return q->max_splits;
}


/*******************************************************************
 *  xaccQuerySetGroup
 *******************************************************************/
void
xaccQuerySetGroup(Query * q, AccountGroup * g) {
  if (!q) return;
  q->acct_group = g;
}


/*******************************************************************
 *  xaccQueryGetGroup
 *******************************************************************/
AccountGroup *
xaccQueryGetGroup(Query * q) {
  if (!q) return NULL;
  return (q->acct_group);
}


/*******************************************************************
 *  xaccQueryGetEarliestDateFound
 *******************************************************************/
time_t
xaccQueryGetEarliestDateFound(Query * q) {
  GList * spl;
  Split * sp;
  time_t earliest = LONG_MAX;

  if (!q) return 0;
  if (!q->split_list) return 0;

  for(spl = q->split_list; spl; spl=spl->next) {
    sp = spl->data;
    if(sp->parent->date_posted.tv_sec < earliest) {
      earliest = (time_t) sp->parent->date_posted.tv_sec;
    }
  }
  return earliest;
}

/*******************************************************************
 *  xaccQueryGetEarliestDateFound
 *******************************************************************/
time_t
xaccQueryGetLatestDateFound(Query * q) {
  Split  * sp;
  GList  * spl;
  time_t latest = 0;

  if(!q) return 0;
  if(!q->split_list) return 0;

  for(spl = q->split_list; spl; spl=spl->next) {
    sp = spl->data;
    if(sp->parent->date_posted.tv_sec > latest) {
      latest = (time_t) sp->parent->date_posted.tv_sec;
    }
  }
  return latest;
}

/********************************************************************\
 * Query.c : api for finding transactions                           *
 * Copyright 2000 Bill Gribble <grib@billgribble.com>               *
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

#include <ctype.h>
#include <glib.h>
#include <sys/types.h>
#include <regex.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#include <assert.h>

#include "gnc-common.h"
#include "TransactionP.h"
#include "Transaction.h"
#include "Account.h"
#include "Group.h"
#include "Query.h"

#if 0
static void 
print_query(Query * q) {
  GList * aterms;
  GList * i, * j;
  QueryTerm * qt;

  printf("Query: max splits = %d\n", q->max_splits);
  for(i=q->terms; i; i=i->next) {
    aterms = i->data;
    printf("(");
    for(j=aterms; j; j=j->next) {
      qt = (QueryTerm *)j->data;
      if(!qt->sense) printf("~");
      printf("%d ", qt->data.type);
    }
    printf(")");
    if(i->next) {
      printf(" | ");
    }    
  }
  printf("\n");
}
#endif

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
 * xaccInitQuery
 ********************************************************************/

void
xaccInitQuery(Query * q, QueryTerm * initial_term) {
  GList * or  = NULL;
  GList * and = NULL;

  if(initial_term) {
    or   = g_list_alloc();
    and  = g_list_alloc();
    and->data = initial_term;
    or->data  = and;
  }

  q->terms      = or;
  q->split_list = NULL;
  q->changed    = 1;

  q->max_splits = -1;

  q->primary_sort = BY_STANDARD;
  q->secondary_sort = BY_NONE;
  q->tertiary_sort = BY_NONE;
}


/********************************************************************
 * xaccQuerySwapTerms
 * swaps the terms fields of two queries, mostly to 
 * allow quick pass-off of results.
 ********************************************************************/

void
xaccQuerySwapTerms(Query * q1, Query * q2) {
  GList * g;
  g = q1->terms;
  q1->terms = q2->terms;
  q2->terms = g;

  q1->changed = 1;
  q2->changed = 1;
}


/********************************************************************
 * xaccQuerySingleTerm
 * initialize the query with 1 term 
 ********************************************************************/

void
xaccQuerySingleTerm(Query * q, QueryTerm * qt) {
  GList * or  = NULL;
  GList * and = NULL;

  or   = g_list_alloc();
  and  = g_list_alloc();
  and->data = qt;
  or->data  = and;
  q->terms = or;
}


/********************************************************************
 * xaccQueryHasTerms
 * returns the number of terms in the query, which is generally
 * used as a truth test.
 ********************************************************************/

int
xaccQueryHasTerms(Query * q) {
  return g_list_length(q->terms);    
}


/********************************************************************
 * xaccFreeQuery 
 * note that the terms list is freed, so you must have newly 
 * allocated it 
 ********************************************************************/

void    
xaccFreeQuery(Query * q) {
  GList * gl;

  if (q == NULL)
    return;

  for(gl=q->terms; gl; gl=gl->next) {
    g_list_free(gl->data);
  }
  g_list_free(q->terms);
  g_free(q->split_list);
  g_free(q);
}

static QueryTerm *
copy_query_term(QueryTerm * qt) {
  QueryTerm * nqt = g_new0(QueryTerm, 1);
  memcpy(nqt, qt, sizeof(QueryTerm));
  return nqt;
}

static GList * 
deep_copy_terms(GList * t) {
  GList * r = NULL;
  GList * cur;
  for(cur=t; cur; cur=cur->next) {
    r = g_list_append(r, g_list_copy(cur->data));
  }
  return r;  
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
  
  DEBUG("inverting query: ");
  DEBUGCMD (print_query(q));

  switch(num_or_terms) {
  case 0:
    retval = xaccMallocQuery();
    retval->max_splits     = q->max_splits;
    retval->acct_group     = q->acct_group;
    retval->split_list     = NULL; 
    retval->terms          = NULL;
    retval->changed        = 1;
    break;

    /* this is demorgan expansion for a single AND expression. */
    /* !(abc) = !a + !b + !c */
  case 1:
    retval = xaccMallocQuery();
    retval->max_splits     = q->max_splits;
    retval->acct_group     = q->acct_group;
    retval->split_list     = NULL; 
    retval->terms          = NULL;
    retval->changed        = 1;

    aterms = g_list_nth_data(q->terms, 0);
    new_oterm = NULL;
    for(cur=aterms; cur; cur=cur->next) {
      qt = copy_query_term(cur->data);
      qt->sense = !(qt->sense);
      new_oterm = g_list_append(NULL, qt);
      retval->terms = g_list_append(retval->terms, new_oterm);
    }
    break;

    /* if there are multiple OR-terms, we just recurse by 
     * breaking it down to !(a + b + c) = 
     * !a * !(b + c) = !a * !b * !c.  */
  default:
    right        = xaccMallocQuery();
    right->terms = deep_copy_terms(g_list_nth(q->terms, 1));
    
    left         = xaccMallocQuery();
    left->terms  = g_list_append(NULL, 
                                 g_list_copy(g_list_nth_data(q->terms, 0)));
    
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

  DEBUG("merging queries: op=%d\n", op);
  DEBUGCMD(print_query(q1); print_query(q2);) 

  switch(op) {
  case QUERY_OR:
    retval = xaccMallocQuery();
    retval->terms = 
      g_list_concat(deep_copy_terms(q1->terms), deep_copy_terms(q2->terms));
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
                        (g_list_copy(i->data),
                         g_list_copy(j->data)));
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
  Account ** ptr;
  int     account_in_set = 0;
  int     first_account = 1;

  assert(qt && acct);
  assert(qt->data.type == PD_ACCOUNT);
  assert(qt->data.acct.accounts);

  for(ptr = qt->data.acct.accounts; *ptr ; ptr++) {
    if(acct == *ptr) {
      account_in_set = 1;
      break;
    }
    first_account = 0;
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

  return 0;
}

static Query * split_sort_query = NULL;


static int
split_cmp_func(sort_type_t how, gconstpointer ga, gconstpointer gb) {
  Split       * sa = (Split *)ga;
  Split       * sb = (Split *)gb;
  Transaction * ta;
  Transaction * tb;
  int retval;
  unsigned long n1;                             
  unsigned long n2;                             
  char   *da, *db;                               
  double fa, fb;                                
  
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
    return xaccSplitDateOrder(&sa, &sb);
    break;

  case BY_DATE:
    /* if dates differ, return */ 
    if ( (ta->date_posted.tv_sec) < 
         (tb->date_posted.tv_sec)) { 
      return -1; 
    } 
    else if ((ta->date_posted.tv_sec) > 
             (tb->date_posted.tv_sec)) { 
      return +1; 
    } 
    
    /* else, seconds match. check nanoseconds */ 
    if ( (ta->date_posted.tv_nsec) < 
         (tb->date_posted.tv_nsec)) { 
      return -1; 
    } 
    else if ( (ta->date_posted.tv_nsec) > 
              (tb->date_posted.tv_nsec)) { 
      return +1; 
    }
    return 0;
    break;

  case BY_DATE_ENTERED:
    /* if dates differ, return */ 
    if ( (ta->date_entered.tv_sec) < 
         (tb->date_entered.tv_sec)) { 
      return -1; 
    } 
    else if ((ta->date_entered.tv_sec) > 
             (tb->date_entered.tv_sec)) { 
      return +1; 
    } 

    /* else, seconds match. check nanoseconds */ 
    if ( (ta->date_entered.tv_nsec) < 
         (tb->date_entered.tv_nsec)) { 
      return -1; 
    } 
    else if ( (ta->date_entered.tv_nsec) > 
              (tb->date_entered.tv_nsec)) { 
      return +1; 
    }
    return 0;
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
    if (!da && db) {                              
      return -1;                                  
    }                                             
    if (da && !db) {                              
      return +1;                                  
    }                                             
    if (!da && !db) {                             
      return 0;                                   
    } 
    return strcmp (da, db); 

    break;

  case BY_MEMO:
    /* sort on memo strings */                    
    da = sa->memo;                             
    db = sb->memo;                             
    if (da && db) {                               
      return strcmp (da, db);                   
    } 
    else if (!da && db) {                              
      return -1;                                  
    } 
    else if (da && !db) {                              
      return +1;                                  
    }                                             
    else {
      return 0;
    }
    break;

  case BY_DESC:
    /* sort on transaction strings */             
    da = ta->description;                         
    db = tb->description;   
    
    if (da && db) {                               
      retval = strcmp (da, db);                   
      /* if strings differ, return */             
      if (retval) return retval;                  
    } 
    else if (!da && db) {                              
      return -1;                                  
    } 
    else if (da && !db) {                              
      return +1;                                  
    }         
    else {
      return 0;
    }
    break;

  case BY_AMOUNT:    
    fa = (sa->damount) * (sa->share_price); 
    fb = (sb->damount) * (sb->share_price); 
    if (fa < fb) { 
      return -1; 
    } 
    else if (fa > fb) { 
      return +1; 
    } 
    else {
      return 0;
    }
    break;
    
  case BY_NONE:
    return 0;
    break;
  }

  return 0;
}

static int
split_sort_func(gconstpointer a, gconstpointer b) {
  int retval; 

  assert(split_sort_query);

  retval = split_cmp_func(split_sort_query->primary_sort, a, b);
  if((retval == 0) && 
     (split_sort_query->secondary_sort != BY_NONE)) {
    retval = split_cmp_func(split_sort_query->secondary_sort, a, b);
    if((retval == 0) &&
       (split_sort_query->tertiary_sort != BY_NONE)) {       
      return split_cmp_func(split_sort_query->tertiary_sort, a, b);
    }
    else {
      return retval;
    }
  }
  else {
    return retval;
  }
}


/********************************************************************
 * xaccQueryCheckSplit
 * check a single split against the query.  This is a short-circuited
 * and-or check, so sorting it with best criteria first is a win.
 ********************************************************************/

int
xaccQueryCheckSplit(Query * q, Split * s) {
  GList     * and_ptr;
  GList     * or_ptr;
  QueryTerm * qt;
  int       and_terms_ok=1;
  
  for(or_ptr = q->terms; or_ptr; or_ptr = or_ptr->next) {
    and_terms_ok = 1;
    for(and_ptr = or_ptr->data; and_ptr; and_ptr = and_ptr->next) {
      qt = (QueryTerm *)(and_ptr->data);
      if(((qt->p)(s, &(qt->data))) != qt->sense) {
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


/********************************************************************
 * xaccQueryGetSplits
 * Run the search.
 ********************************************************************/

Split **
xaccQueryGetSplits(Query * q) {
  GList     * matching_splits=NULL;
  GList     * or_ptr, * and_ptr, * mptr;  
  Account   ** all_accts, ** ptr;
  Account   * current;
  Split     ** splits;
  Split     ** sptr;
  QueryTerm * qt;

  int       total_splits_checked = 0;
  int       split_count = 0;
  int       acct_ok;
  int       posn;

  struct timeval start, end;

  gettimeofday(&start, NULL);

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

  /* iterate over accounts */
  all_accts = xaccGetAccounts(q->acct_group);
  for(ptr = all_accts; ptr && *ptr; ptr++) {
    current = *ptr;

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
      splits = xaccAccountGetSplitList(current);

      /* iterate over splits */
      for(sptr = splits; *sptr; sptr++) {
        if(xaccQueryCheckSplit(q, *sptr)) {
          matching_splits = g_list_append(matching_splits, *sptr);
          split_count++;
        }
        total_splits_checked++;
      }      
    }
  }

  /* now sort the matching splits based on the search criteria 
   * split_sort_query is an unforgivable use of static global data...
   * I just can't figure out how else to do this sanely. */
  split_sort_query = q;
  matching_splits = g_list_sort(matching_splits, split_sort_func);

  /* crop the list to limit the number of splits */
  if((split_count > q->max_splits) && (q->max_splits > -1)) {
    if(q->max_splits > 0) {
      mptr = g_list_nth(matching_splits, q->max_splits - 1);
      g_list_free(mptr->next);
      mptr->next = NULL;
    }
    split_count = q->max_splits;
  }

  /* convert the g_list into a split array. */
  splits = g_new0(Split *, split_count+1);
  posn = 0;
  for(mptr = matching_splits; mptr; mptr=mptr->next) {
    splits[posn] = mptr->data;
    posn++;
  }
  splits[split_count] = NULL;
  g_list_free(matching_splits);

  gettimeofday(&end, NULL);

  PINFO("elapsed time = %e ms\n",
         (end.tv_sec - start.tv_sec)*1000.0 +
         (end.tv_usec - start.tv_usec)/1000.0);
  PINFO("%d splits checked, %d splits matched.\n",
         total_splits_checked, split_count);

  q->changed = 0;
  if(q->split_list) {
    g_free(q->split_list);
  }
  q->split_list = splits;
  
  return splits;
}


/********************************************************************
 * xaccQueryAddAccountMatch
 * Add an account filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddAccountMatch(Query * q, Account ** acclist, acct_match_t how,
                         QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccAccountMatchPredicate;
  qt->sense  = 1;
  qt->data.type           = PD_ACCOUNT;
  qt->data.acct.how       = how;
  qt->data.acct.accounts  = acclist;

  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddTransMatch
 * match splits in the given transaction
 ********************************************************************/

void
xaccQueryAddTransMatch(Query * q, Transaction * trans, int how,
                       QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccTransMatchPredicate;
  qt->sense  = how;
  qt->data.type         = PD_TRANS;
  qt->data.trans.trans  = trans;

  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddSplitMatch
 * match exactly the given split
 ********************************************************************/

void
xaccQueryAddSplitMatch(Query * q, Split * split, int how,
                       QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccSplitMatchPredicate;
  qt->sense  = how;
  qt->data.type         = PD_SPLIT;
  qt->data.split.split  = split;

  xaccQuerySingleTerm(qs, qt);
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
  Account   ** acctlist = g_new0(Account *, 2);
  Query     * qr;

  acctlist[0] = acct;
  acctlist[1] = NULL;
  
  qt->p      = & xaccAccountMatchPredicate;
  qt->sense  = 1;
  qt->data.type           = PD_ACCOUNT;
  qt->data.acct.how       = ACCT_MATCH_ANY;
  qt->data.acct.accounts  = acctlist;
  
  xaccQuerySingleTerm(qs, qt);
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
xaccQueryAddDescriptionMatch(Query * q, char * matchstring,
                             int case_sens, int use_regexp,
                             QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  char      * teststr;
  char      * src, * dest;
  int       flags = REG_EXTENDED;

  qt->p      = & xaccDescriptionMatchPredicate;
  qt->sense  = 1;
  qt->data.type            = PD_STRING;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    teststr = g_new0(char, strlen(matchstring)+1);
    dest    = teststr;
    for(src=matchstring; *src; src++) {
      *dest = tolower(*src);
      dest++;      
    }
    *dest = 0;    
    qt->data.str.matchstring = teststr;
  }
  
  xaccQuerySingleTerm(qs, qt);
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
xaccQueryAddMemoMatch(Query * q, char * matchstring,
                      int case_sens, int use_regexp,
                      QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  char      * teststr;
  char      * src, * dest;
  int       flags = REG_EXTENDED;

  qt->p      = & xaccMemoMatchPredicate;
  qt->sense  = 1;
  qt->data.type            = PD_STRING;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    teststr = g_new0(char, strlen(matchstring)+1);
    dest    = teststr;
    for(src=matchstring; *src; src++) {
      *dest = tolower(*src);
      dest++;      
    }
    *dest = 0;    
    qt->data.str.matchstring = teststr;
  }
  
  xaccQuerySingleTerm(qs, qt);
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
                      int sday, int smonth, int syear,
                      int eday, int emonth, int eyear,
                      QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccDateMatchPredicate;
  qt->sense  = 1;
  qt->data.type           = PD_DATE;
  qt->data.date.start     = gnc_dmy2timespec(sday, smonth, syear);
  qt->data.date.end       = gnc_dmy2timespec(eday, emonth, eyear);

  xaccQuerySingleTerm(qs, qt);
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
                        Timespec sts,
                        Timespec ets,
                        QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccDateMatchPredicate;
  qt->sense  = 1;
  qt->data.type           = PD_DATE;
  qt->data.date.start     = sts;  
  qt->data.date.end       = ets;
  
  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddDateMatchTT
 * Add a date filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddDateMatchTT(Query * q, 
                        time_t stt,
                        time_t ett,
                        QueryOp op) {
  Query      * qs  = xaccMallocQuery(); 
  QueryTerm  * qt  = g_new0(QueryTerm, 1);
  Query      * qr;  
  Timespec   sts;
  Timespec   ets;
  
  sts.tv_sec  = (long long)stt;
  sts.tv_nsec = 0;

  ets.tv_sec  = (long long)ett;
  ets.tv_nsec = 0;
  
  qt->p      = & xaccDateMatchPredicate;
  qt->sense  = 1;
  qt->data.type           = PD_DATE;
  qt->data.date.start     = sts;
  qt->data.date.end       = ets;
  
  xaccQuerySingleTerm(qs, qt);
  if(xaccQueryHasTerms(q)) {
    qr = xaccQueryMerge(q, qs, op);
  }
  else {
    qr = xaccQueryMerge(q, qs, QUERY_OR);
  }
  xaccQuerySwapTerms(q, qr);
  xaccFreeQuery(qr);
  xaccFreeQuery(qs);
}

/********************************************************************
 * xaccQueryAddNumberMatch
 * Add a number-field filter 
 ********************************************************************/
void
xaccQueryAddNumberMatch(Query * q, char * matchstring, int case_sens,
                        int use_regexp, QueryOp op) {
  
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  char      * teststr;
  char      * src, * dest;
  int       flags = REG_EXTENDED;

  qt->p      = & xaccNumberMatchPredicate;
  qt->sense  = 1;
  qt->data.type            = PD_STRING;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    teststr = g_new0(char, strlen(matchstring)+1);
    dest    = teststr;
    for(src=matchstring; *src; src++) {
      *dest = tolower(*src);
      dest++;      
    }
    *dest = 0;    
    qt->data.str.matchstring = teststr;
  }
  
  xaccQuerySingleTerm(qs, qt);
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
xaccQueryAddActionMatch(Query * q, char * matchstring, int case_sens,
                        int use_regexp, QueryOp op) {
  
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  char      * teststr;
  char      * src, * dest;
  int       flags = REG_EXTENDED;

  qt->p      = & xaccActionMatchPredicate;
  qt->sense  = 1;
  qt->data.type            = PD_STRING;
  qt->data.str.case_sens   = case_sens;
  qt->data.str.use_regexp  = use_regexp;
  qt->data.str.matchstring = strdup(matchstring);

  if(!case_sens) {
    flags |= REG_ICASE;
  }

  if(use_regexp) {
    regcomp(&qt->data.str.compiled, matchstring, flags);
  }
  else if(!case_sens) {
    teststr = g_new0(char, strlen(matchstring)+1);
    dest    = teststr;
    for(src=matchstring; *src; src++) {
      *dest = tolower(*src);
      dest++;      
    }
    *dest = 0;    
    qt->data.str.matchstring = teststr;
  }
  
  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddAmountMatch
 * Add a value filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddAmountMatch(Query * q, double amt, 
                        amt_match_sgn_t amt_sgn, 
                        amt_match_t how,
                        QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;

  qt->p      = & xaccAmountMatchPredicate;
  qt->sense  = 1;
  qt->data.type             = PD_AMOUNT;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = amt_sgn;
  qt->data.amount.amount    = amt;

  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddSharePriceMatch
 * Add a share-price filter to an existing query. 
 ********************************************************************/

void
xaccQueryAddSharePriceMatch(Query * q, double amt, 
                            amt_match_t how,
                            QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  
  qt->p      = & xaccSharePriceMatchPredicate;
  qt->sense  = 1;
  qt->data.type             = PD_AMOUNT;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = 0;
  qt->data.amount.amount    = amt;
  
  xaccQuerySingleTerm(qs, qt);
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
 * xaccQueryAddSharesMatch
 * Add a share-price filter to an existing query. 
 ********************************************************************/
 
void
xaccQueryAddSharesMatch(Query * q, double amt, 
                        amt_match_t how,
                        QueryOp op) {
  Query     * qs  = xaccMallocQuery(); 
  QueryTerm * qt  = g_new0(QueryTerm, 1);
  Query     * qr;
  
  qt->p      = & xaccSharesMatchPredicate;
  qt->sense  = 1;
  qt->data.type             = PD_AMOUNT;
  qt->data.amount.how       = how;
  qt->data.amount.amt_sgn   = 0;
  qt->data.amount.amount    = amt;
  
  xaccQuerySingleTerm(qs, qt);
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

  qt->p      = p;
  qt->sense  = 1;
  qt->data.type           = PD_MISC;
  qt->data.misc.how       = how;
  qt->data.misc.data      = data;

  xaccQuerySingleTerm(qs, qt);
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

  for(or = q->terms; or; or = or->next) {
    for(and = or->data; and; and = and->next) {
      qt = and->data;
      if(qt->data.type == type) {
        if(g_list_length(or->data) == 1) {          
          q->terms = g_list_remove_link(q->terms, or);          
          or = q->terms;
          break;
        }
        else {
          or->data = g_list_remove_link(or->data, and);
          and = or->data;
          if(!and) break;
        }
        q->changed = 1;
        g_free(qt);
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
xaccQueryClear(Query * q) {
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
string_match_predicate(char * s, PredicateData * pd) {
  regmatch_t match;
  char       * teststr; 
  char       * src, * dest;

  assert(s && pd && (pd->type == PD_STRING));

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
    /* fix this: need a case-insensitive strstr */
    teststr = g_new0(char, strlen(s)+1);
    dest    = teststr;
    for(src=s; *src; src++) {
      *dest = tolower(*src);
      dest++;      
    }
    *dest = 0;

    if(strstr(teststr, pd->str.matchstring)) return 1;
    else return 0;
  }

}


/*******************************************************************
 *  value_match_predicate
 *******************************************************************/
int 
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
int 
xaccAccountMatchPredicate(Split * s, PredicateData * pd) { 
  Transaction * parent;
  Split       * split;
  Account     * split_acct;
  Account     ** acct;
  int         i;
  int         numsplits;

  assert(s && pd);
  assert(pd->type == PD_ACCOUNT);

  parent    = xaccSplitGetParent(s);
  assert(parent);

  switch(pd->acct.how) {
  case ACCT_MATCH_ALL:
    /* there must be a split in parent that matches each of the 
     * accounts listed in pd. */
    numsplits = xaccTransCountSplits(parent);
    for(acct=pd->acct.accounts; *acct; acct++) {
      for(i=0; i < numsplits; i++) {
        split = xaccTransGetSplit(parent, i);
        if(*acct == xaccSplitGetAccount(split)) {
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
    if(*acct) return 0;
    else return 1;

    break;

  case ACCT_MATCH_ANY:
    /* s must match an account in pd */
    split_acct = xaccSplitGetAccount(s);
    for(acct = pd->acct.accounts; *acct; acct++) {
      if(*acct == split_acct) {
        break;
      }
    }
    if(*acct) return 1;
    else return 0;
    break;

  case ACCT_MATCH_NONE:
    /* s must match an account in pd */
    split_acct = xaccSplitGetAccount(s);
    for(acct = pd->acct.accounts; *acct; acct++) {
      if(*acct == split_acct) {
        break;
      }
    }
    if(*acct) return 0;
    else return 1;
    break;
  }

  return 0;
}


/*******************************************************************
 *  xaccDescriptionMatchPredicate 
 *******************************************************************/
int
xaccDescriptionMatchPredicate(Split * s, PredicateData * pd) {
  Transaction * parent;
  char        * descript;
  
  assert(s && pd);  
  assert(pd->type == PD_STRING);

  parent = xaccSplitGetParent(s);
  assert(parent);

  descript = xaccTransGetDescription(parent);
  return string_match_predicate(descript, pd);
}

/*******************************************************************
 *  xaccNumberMatchPredicate 
 *******************************************************************/
int
xaccNumberMatchPredicate(Split * s, PredicateData * pd) {
  Transaction * parent;
  char        * number;
  
  assert(s && pd);  
  assert(pd->type == PD_STRING);

  parent = xaccSplitGetParent(s);
  assert(parent);

  number = xaccTransGetNum(parent);
  return string_match_predicate(number, pd);
}


/*******************************************************************
 *  xaccTransMatchPredicate 
 *******************************************************************/
int
xaccTransMatchPredicate(Split * s, PredicateData * pd) {
  Transaction * parent;
  
  assert(s && pd);  
  assert(pd->type == PD_TRANS);

  parent = xaccSplitGetParent(s);
  assert(parent);

  return (parent == pd->trans.trans);
}


/*******************************************************************
 *  xaccSplitMatchPredicate 
 *******************************************************************/
int
xaccSplitMatchPredicate(Split * s, PredicateData * pd) {
  assert(s && pd);  
  assert(pd->type == PD_SPLIT);
  return (s == pd->split.split);
}


/*******************************************************************
 *  xaccActionMatchPredicate 
 *******************************************************************/
int
xaccActionMatchPredicate(Split * s, PredicateData * pd) {
  char        * action;
  
  assert(s && pd);  
  assert(pd->type == PD_STRING);

  action = xaccSplitGetAction(s);
  return string_match_predicate(action, pd);
}


/*******************************************************************
 *  xaccMemoMatchPredicate 
 *******************************************************************/
int
xaccMemoMatchPredicate(Split * s, PredicateData * pd) {
  char        * memo;
  
  assert(s && pd);  
  memo = xaccSplitGetMemo(s);
  
  return string_match_predicate(memo, pd);
}


/*******************************************************************
 *  xaccMiscMatchPredicate
 *  *** Bill, please complete! ***
 *******************************************************************/
int xaccMiscMatchPredicate(Split * s, PredicateData * pd) {
  return 0;
}


/*******************************************************************
 *  xaccAmountMatchPredicate 
 *******************************************************************/
int
xaccAmountMatchPredicate(Split * s, PredicateData * pd) {
  double splitamt;

  assert(s && pd);
  assert(pd->type == PD_AMOUNT);

  splitamt = xaccSplitGetValue(s);
  
  switch(pd->amount.amt_sgn) {
  case AMT_SGN_MATCH_CREDIT:
    if(splitamt < 0.0) return 0;
    break;
  case AMT_SGN_MATCH_DEBIT:
    if(splitamt > 0.0) return 0;
    break;
  default:
    break;
  }  
  return value_match_predicate(splitamt, pd);  
}

/*******************************************************************
 *  xaccSharePriceMatchPredicate 
 *******************************************************************/
int
xaccSharePriceMatchPredicate(Split * s, PredicateData * pd) {
  double   splitamt;
  Account  * acct;
  int      type;

  assert(s && pd);
  assert(pd->type == PD_AMOUNT);
  
  acct = xaccSplitGetAccount(s);
  type = xaccAccountGetType(acct);

  if((type != STOCK) && (type != MUTUAL)) {
    return 0;
  }
  splitamt = xaccSplitGetSharePrice(s);
  
  return value_match_predicate(splitamt, pd);  
}


/*******************************************************************
 *  xaccSharesMatchPredicate 
 *******************************************************************/
int
xaccSharesMatchPredicate(Split * s, PredicateData * pd) {
  double   splitamt;
  Account  * acct;
  int      type;

  assert(s && pd);
  assert(pd->type == PD_AMOUNT);
  
  acct = xaccSplitGetAccount(s);
  type = xaccAccountGetType(acct);

  if((type != STOCK) && (type != MUTUAL)) {
    return 0;
  }

  splitamt = xaccSplitGetShareAmount(s);
  
  return value_match_predicate(splitamt, pd);  
}


/*******************************************************************
 *  xaccDateMatchPredicate 
 *******************************************************************/
int
xaccDateMatchPredicate(Split * s, PredicateData * pd) {
  Timespec transtime;

  assert(s && pd);
  assert(pd->type == PD_DATE);
  
  xaccTransGetDateTS(xaccSplitGetParent(s), &transtime);
  
  return ((transtime.tv_sec >= pd->date.start.tv_sec) &&
          (transtime.tv_sec <= pd->date.end.tv_sec));
}


/*******************************************************************
 *  xaccQuerySetSortOrder
 *******************************************************************/
void
xaccQuerySetSortOrder(Query * q, sort_type_t primary, 
                      sort_type_t secondary, sort_type_t tertiary) {
  q->primary_sort   = primary;
  q->secondary_sort = secondary;
  q->tertiary_sort  = tertiary;

  q->changed = 1;
}


/*******************************************************************
 *  xaccQuerySetMaxSplits
 *******************************************************************/
void
xaccQuerySetMaxSplits(Query * q, int n) {
  q->max_splits = n;
}


/*******************************************************************
 *  xaccQuerySetGroup
 *******************************************************************/
void
xaccQuerySetGroup(Query * q, AccountGroup * g) {
  q->acct_group = g;
}


/*******************************************************************
 *  xaccQueryGetEarliestDateFound
 *******************************************************************/
time_t
xaccQueryGetEarliestDateFound(Query * q) {
  Split  ** sp;
  time_t earliest = LONG_MAX;

  if(!q->split_list) { return 0; }

  for(sp = q->split_list; *sp; sp++) {
    if((*sp)->parent->date_posted.tv_sec < earliest) {
      earliest = (*sp)->parent->date_posted.tv_sec;
    }
  }
  return earliest;
}

/*******************************************************************
 *  xaccQueryGetEarliestDateFound
 *******************************************************************/
time_t
xaccQueryGetLatestDateFound(Query * q) {
  Split  ** sp;
  time_t latest = 0;

  if(!q->split_list) { return 0; }

  for(sp = q->split_list; *sp; sp++) {
    if((*sp)->parent->date_posted.tv_sec > latest) {
      latest = (*sp)->parent->date_posted.tv_sec;
    }
  }
  return latest;
}

#if 0
int
main(int argc, char ** argv) {
  Query * q = xaccMallocQuery();
  Query * q2 = xaccMallocQuery();
  Query * r;

  printf("testing queries...\n");

  xaccQueryAddMiscMatch(q, NULL, 1);
  xaccQueryAddMiscMatch(q, NULL, 2);
  xaccQueryAddMiscMatch(q, NULL, 3);
  print_query(q);

  xaccQueryAddMiscMatch(q2, NULL, 4);
  xaccQueryAddMiscMatch(q2, NULL, 5);
  xaccQueryAddMiscMatch(q2, NULL, 6);
  print_query(q2);

  printf("AND of two simple queries:\n");

  r = xaccQueryMerge(q, q2, QUERY_AND);
  print_query(r);

  printf("OR of two simple queries:\n");

  r = xaccQueryMerge(q, q2, QUERY_OR);
  print_query(r);

  printf("NAND of two simple queries:\n");
  r = xaccQueryMerge(q, q2, QUERY_NAND);
  print_query(r);

  printf("XOR of two simple queries:\n");
  r = xaccQueryMerge(q, q2, QUERY_XOR);
  print_query(r);
    
}
#endif

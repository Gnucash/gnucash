/*
 * QueryNew.c -- API for finding Gnucash objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#include "config.h"

#include <sys/types.h>
#include <time.h>
#include <glib.h>
#include <regex.h>
#include <string.h>

#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "gncObject.h"
#include "BackendP.h"

#include "QueryObjectP.h"
#include "QueryCoreP.h"
#include "QueryNewP.h"

static short module = MOD_QUERY;

typedef struct query_new_term {
  GSList *		param_list;
  QueryPredData_t	pdata;
  gboolean		invert;

  /* These values are filled in during "compilation" of the query
   * term, based upon the obj_name, param_name, and searched-for
   * object type.  If conv_fcn is NULL, then we don't know how to
   * convert types.
   */
  GSList *		param_fcns;
  QueryPredicate	pred_fcn;
} QueryNewTerm;

typedef struct query_new_sort {
  GSList *	param_list;
  gint		options;
  gboolean	increasing;

  /* These values are filled in during "compilation" of the query
   * term, based upon the obj_name, param_name, and searched-for
   * object type.  If conv_fcn is NULL, then we don't know how to
   * convert types.
   */
  gboolean	use_default;
  GSList *	param_fcns;
  QuerySort	obj_cmp;	/* In case you are comparing objects */
  QueryCompare	comp_fcn;	/* When you are comparing core types */
} QueryNewSort;

/* The QUERY structure */
struct querynew_s {
  /* The object type that we're searching for */
  GNCIdType	search_for;

  /* terms is a list of the OR-terms in a sum-of-products 
   * logical expression. */
  GList *	terms;  

  /* sorting and chopping is independent of the search filter */

  QueryNewSort	primary_sort;
  QueryNewSort	secondary_sort;
  QueryNewSort	tertiary_sort;
  QuerySort	defaultSort;	/* <- Computed from search_for */

  /* The maximum number of results to return */
  int		max_results;

  /* list of books that will be participating in the query */
  GList *	books;

  /* cache the results so we don't have to run the whole search 
   * again until it's really necessary */
  int		changed;

  GList *	results;
};

typedef struct query_cb {
  QueryNew *	query;
  GList *	list;
  int		count;
} query_cb_t;

/* initial_term will be owned by the new Query */
static void query_init (QueryNew *q, QueryNewTerm *initial_term)
{
  GList * or = NULL;
  GList *and = NULL;

  if (initial_term) {
    or = g_list_alloc ();
    and = g_list_alloc ();
    and->data = initial_term;
    or->data = and;
  }

  if(q->terms)
    gncQueryClear (q);

  g_list_free (q->results);
  g_list_free (q->books);

  g_slist_free (q->primary_sort.param_list);
  g_slist_free (q->secondary_sort.param_list);
  g_slist_free (q->tertiary_sort.param_list);

  g_slist_free (q->primary_sort.param_fcns);
  g_slist_free (q->secondary_sort.param_fcns);
  g_slist_free (q->tertiary_sort.param_fcns);

  memset (q, 0, sizeof (*q));

  q->terms = or;
  q->changed = 1;
  q->max_results = -1;

  q->primary_sort.param_list = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
  q->primary_sort.increasing = TRUE;
  q->secondary_sort.increasing = TRUE;
  q->tertiary_sort.increasing = TRUE;
}

static void swap_terms (QueryNew *q1, QueryNew *q2)
{
  GList *g;

  if (!q1 || !q2) return;

  g = q1->terms;
  q1->terms = q2->terms;
  q2->terms = g;

  g = q1->books;
  q1->books = q2->books;
  q2->books = g;

  q1->changed = 1;
  q2->changed = 1;
}

static void free_query_term (QueryNewTerm *qt)
{
  if (!qt) return;

  gncQueryCorePredicateFree (qt->pdata);
  g_slist_free (qt->param_list);
  g_slist_free (qt->param_fcns);
  g_free (qt);
}

static QueryNewTerm * copy_query_term (QueryNewTerm *qt)
{
  QueryNewTerm *new_qt;
  if (!qt) return NULL;

  new_qt = g_new0 (QueryNewTerm, 1);
  memcpy (new_qt, qt, sizeof(QueryNewTerm));
  new_qt->param_list = g_slist_copy (new_qt->param_list);
  new_qt->param_fcns = g_slist_copy (new_qt->param_fcns);
  new_qt->pdata = gncQueryCorePredicateCopy (qt->pdata);
  return new_qt;
}

static GList * copy_and_terms (GList *and_terms)
{
  GList *and = NULL;
  GList *cur_and;

  for(cur_and = and_terms; cur_and; cur_and = cur_and->next)
  {
    and = g_list_prepend(and, copy_query_term (cur_and->data));
  }

  return g_list_reverse(and);
}

static GList * 
copy_or_terms(GList * or_terms) 
{
  GList * or = NULL;
  GList * cur_or;

  for(cur_or = or_terms; cur_or; cur_or = cur_or->next)
  {
    or = g_list_prepend(or, copy_and_terms(cur_or->data));
  }

  return g_list_reverse(or);
}

static void copy_sort (QueryNewSort_t dst, const QueryNewSort_t src)
{
  memcpy (dst, src, sizeof (*dst));
  dst->param_list = g_slist_copy (src->param_list);
  dst->param_fcns = g_slist_copy (src->param_fcns);
}

static void free_sort (QueryNewSort_t s)
{
  g_slist_free (s->param_list);
  s->param_list = NULL;

  g_slist_free (s->param_fcns);
  s->param_fcns = NULL;
}

static void free_members (QueryNew *q)
{
  GList * cur_or;

  if (q == NULL) return;

  for(cur_or = q->terms; cur_or; cur_or = cur_or->next) 
  {
    GList * cur_and;

    for(cur_and = cur_or->data; cur_and; cur_and = cur_and->next) 
    {
      free_query_term(cur_and->data);
      cur_and->data = NULL;
    }

    g_list_free(cur_or->data);
    cur_or->data = NULL;
  }

  free_sort (&(q->primary_sort));
  free_sort (&(q->secondary_sort));
  free_sort (&(q->tertiary_sort));

  g_list_free(q->terms);
  q->terms = NULL;

  g_list_free(q->books);
  q->books = NULL;

  g_list_free(q->results);
  q->results = NULL;
}

static int cmp_func (QueryNewSort_t sort, QuerySort default_sort,
		     gconstpointer a, gconstpointer b)
{
  GSList *node;
  gpointer conva, convb;
  QueryAccess get_fcn = NULL;	/* to appease the compiler */

  g_return_val_if_fail (sort, 0);
  g_return_val_if_fail (default_sort, 0);

  /* See if this is a default sort */
  if (sort->use_default) {
    if (default_sort)
      return default_sort ((gpointer)a, (gpointer)b);
    return 0;
  }

  /* If no parameters, consider them equal */
  if (!sort->param_fcns) return 0;

  /* no compare function, consider the two objects equal */
  if (!sort->comp_fcn && !sort->obj_cmp) return 0;
  
  /* Do the list of conversions */
  conva = (gpointer)a;
  convb = (gpointer)b;
  for (node = sort->param_fcns; node; node = node->next) {
    get_fcn = node->data;

    /* The last term is really the "parameter getter",
     * unless we're comparing objects ;) */
    if (!node->next && !sort->obj_cmp)
      break;

    /* Do the converstions */
    conva = get_fcn (conva);
    convb = get_fcn (convb);
  }

  /* And now return the (appropriate) compare */
  if (sort->comp_fcn)
    return sort->comp_fcn (conva, convb, sort->options, get_fcn);

  return sort->obj_cmp (conva, convb);
}

static QueryNew * sortQuery = NULL;

static int sort_func (gconstpointer a, gconstpointer b)
{
  int retval;

  g_return_val_if_fail (sortQuery, 0);

  retval = cmp_func (&(sortQuery->primary_sort), sortQuery->defaultSort, a, b);
  if (retval == 0) {
    retval = cmp_func (&(sortQuery->secondary_sort), sortQuery->defaultSort,
		       a, b);
    if (retval == 0) {
      retval = cmp_func (&(sortQuery->tertiary_sort), sortQuery->defaultSort,
			 a, b);
      return sortQuery->tertiary_sort.increasing ? retval : -retval;
    } else {
      return sortQuery->secondary_sort.increasing ? retval : -retval;
    }
  } else {
    return sortQuery->primary_sort.increasing ? retval : -retval;
  }
}

static int check_object (QueryNew *q, gpointer object)
{
  GList     * and_ptr;
  GList     * or_ptr;
  QueryNewTerm * qt;
  int       and_terms_ok=1;
  
  for(or_ptr = q->terms; or_ptr; or_ptr = or_ptr->next) {
    and_terms_ok = 1;
    for(and_ptr = or_ptr->data; and_ptr; and_ptr = and_ptr->next) {
      qt = (QueryNewTerm *)(and_ptr->data);
      if (qt->param_fcns && qt->pred_fcn) {
	GSList *node;
	QueryAccess get_fcn;
	gpointer conv_obj = object;

	/* iterate through the conversions */
	for (node = qt->param_fcns; node; node = node->next) {
	  get_fcn = node->data;

	  /* The last term is the actual parameter getter */
	  if (!node->next)
	    break;

	  conv_obj = get_fcn (conv_obj);
	}

	if (((qt->pred_fcn)(conv_obj, get_fcn, qt->pdata))
	    == qt->invert) {
	  and_terms_ok = 0;
	  break;
	}
      } else {
	/* XXX: Don't know how to do this conversion -- do we care? */
      }
    }
    if(and_terms_ok) {
      return 1;
    }
  }
  return 0;
}

/* walk the list of parameters, starting with the given object, and
 * compile the list of parameter get-functions.  Save the last valid
 * parameter definition in "final" and return the list of functions.
 *
 * returns NULL if the first parameter is bad (and final is unchanged).
 */
static GSList * compile_params (GSList *param_list, GNCIdType start_obj,
				QueryObjectDef const **final)
{
  const QueryObjectDef *objDef = NULL;
  GSList *fcns = NULL;

  g_return_val_if_fail (param_list, NULL);
  g_return_val_if_fail (start_obj, NULL);
  g_return_val_if_fail (final, NULL);

  for (; param_list; param_list = param_list->next) {
    GNCIdType param_name = param_list->data;
    objDef = gncQueryObjectGetParameter (start_obj, param_name);

    /* If it doesn't exist, then we've reached the end */
    if (!objDef)
      break;

    /* Save off this function */
    fcns = g_slist_prepend (fcns, objDef->param_getfcn);

    /* Save this off, just in case */
    *final = objDef;

    /* And reset for the next parameter */
    start_obj = (GNCIdType) objDef->param_type;
  }

  return (g_slist_reverse (fcns));
}

static void compile_sort (QueryNewSort_t sort, GNCIdType obj)
{
  const QueryObjectDef *resObj = NULL;

  sort->use_default = FALSE;

  g_slist_free (sort->param_fcns);
  sort->param_fcns = NULL;
  sort->comp_fcn = NULL;
  sort->obj_cmp = NULL;

  /* An empty param_list implies "no sort" */
  if (!sort->param_list)
    return;

  /* Walk the parameter list of obtain the parameter functions */
  sort->param_fcns = compile_params (sort->param_list, obj, &resObj);

  /* If we have valid parameters, grab the compare function,
   * If not, check if this is the default sort.
   */
  if (sort->param_fcns) {
    sort->comp_fcn = gncQueryCoreGetCompare (resObj->param_type);

    /* Hrm, perhaps this is an object compare, not a core compare? */
    if (sort->comp_fcn == NULL)
      sort->obj_cmp = gncQueryObjectDefaultSort (resObj->param_type);

  } else if (!safe_strcmp (sort->param_list->data, QUERY_DEFAULT_SORT))
    sort->use_default = TRUE;
}

static void compile_terms (QueryNew *q)
{
  GList *or_ptr, *and_ptr;

  /* Find the specific functions for this Query.  Note that the
   * Query's search_for should now be set to the new type.
   */
  for (or_ptr = q->terms; or_ptr; or_ptr = or_ptr->next) {
    for (and_ptr = or_ptr->data; and_ptr; and_ptr = and_ptr->next) {
      QueryNewTerm *qt = and_ptr->data;
      const QueryObjectDef *resObj = NULL;
      
      g_slist_free (qt->param_fcns);
      qt->param_fcns = NULL;

      /* Walk the parameter list of obtain the parameter functions */
      qt->param_fcns = compile_params (qt->param_list, q->search_for,
				       &resObj);

      /* If we have valid parameters, grab the predicate function,
       * If not, see if this is the default sort.
       */

      if (qt->param_fcns)
	qt->pred_fcn = gncQueryCoreGetPredicate (resObj->param_type);
      else
	qt->pred_fcn = NULL;
    }
  }

  /* Update the sort functions */
  compile_sort (&(q->primary_sort), q->search_for);
  compile_sort (&(q->secondary_sort), q->search_for);
  compile_sort (&(q->tertiary_sort), q->search_for);

  q->defaultSort = gncQueryObjectDefaultSort (q->search_for);
}

static void check_item_cb (gpointer object, gpointer user_data)
{
  query_cb_t *ql = user_data;

  if (!object || !ql) return;

  if (check_object (ql->query, object)) {
    ql->list = g_list_prepend (ql->list, object);
    ql->count++;
  }

  return;
}

static int param_list_cmp (GSList *l1, GSList *l2)
{
  while (1) {
    int ret;

    /* Check the easy stuff */
    if (!l1 && !l2) return 0;
    if (!l1 && l2) return -1;
    if (l1 && !l2) return 1;

    ret = safe_strcmp (l1->data, l2->data);
    if (ret)
      return ret;

    l1 = l1->next;
    l2 = l2->next;
  }
}

static GList * merge_books (GList *l1, GList *l2)
{
  GList *res = NULL;
  GList *node;

  res = g_list_copy (l1);

  for (node = l2; node; node = node->next) {
    if (g_list_index (res, node->data) == -1)
      res = g_list_prepend (res, node->data);
  }

  return res;
}

/********************************************************************/
/* PUBLISHED API FUNCTIONS */

void gncQueryAddTerm (QueryNew *q, GSList *param_list,		      
		      QueryPredData_t pred_data, QueryOp op)
{
  QueryNewTerm *qt;
  QueryNew *qr, *qs;

  if (!q || !param_list || !pred_data) return;

  qt = g_new0 (QueryNewTerm, 1);
  qt->param_list = param_list;
  qt->pdata = pred_data;

  qs = gncQueryCreate ();
  query_init (qs, qt);

  if (gncQueryHasTerms (q))
    qr = gncQueryMerge (q, qs, op);
  else
    qr = gncQueryMerge (q, qs, QUERY_OR);

  swap_terms (q, qr);
  gncQueryDestroy (qs);
  gncQueryDestroy (qr);
}

void gncQueryPurgeTerms (QueryNew *q, GSList *param_list)
{
  QueryNewTerm *qt;
  GList *or, *and;

  if (!q || !param_list) return;

  for (or = q->terms; or; or = or->next) {
    for (and = or->data; and; and = and->next) {
      qt = and->data;
      if (!param_list_cmp (qt->param_list, param_list)) {
	if (g_list_length (or->data) == 1) {
	  q->terms = g_list_remove_link (q->terms, or);
	  g_list_free_1 (or);
	  or = q->terms;
	  break;
	} else {
	  or->data = g_list_remove_link (or->data, and);
	  g_list_free_1 (and);
	  and = or->data;
	  if (!and) break;
	}
	q->changed = 1;
	free_query_term (qt);
      }
    }
    if (!or) break;
  }
}

GList * gncQueryRun (QueryNew *q)
{
  GList *matching_objects = NULL;
  GList *node;
  int	object_count = 0;

  if (!q) return NULL;
  g_return_val_if_fail (q->search_for, NULL);

  /* XXX: Prioritize the query terms? */

  /* prepare the Query for processing */
  if (q->changed)
    compile_terms (q);

  /* Now run the query over all the objects and save the results */
  {
    query_cb_t qcb;

    memset (&qcb, 0, sizeof (qcb));
    qcb.query = q;

    /* For each book */
    for (node=q->books; node; node=node->next) {
      GNCBook *book = node->data;
      Backend *be = book->backend;

      /* query the backend */
      if (be && be->run_query)
	(be->run_query) (be, q);

      /* and then iterate over all the objects */
      gncObjectForeach (q->search_for, book, check_item_cb, &qcb);
    }

    matching_objects = qcb.list;
    object_count = qcb.count;
  }

  /* There is no absolute need to reverse this list, since it's being
   * sorted below. However, in the common case, we will be searching
   * in a confined location where the objects are already in order,
   * thus reversing will put us in the correct order we want and make
   * the sorting go much faster.
   */
  matching_objects = g_list_reverse(matching_objects);

  /* now sort the matching objects based on the search criteria
   * sortQuery is an unforgivable use of static global data...  I just
   * can't figure out how else to do this sanely.
   */
  sortQuery = q;
  matching_objects = g_list_sort(matching_objects, sort_func);
  sortQuery = NULL;

  /* crop the list to limit the number of splits */
  if((object_count > q->max_results) && (q->max_results > -1)) 
  {
    if(q->max_results > 0) 
    {
      GList *mptr;

      /* mptr is set to the first node of what will be the new list */
      mptr = g_list_nth(matching_objects, object_count - q->max_results);
      /* mptr should not be NULL, but let's be safe */
      if (mptr != NULL) 
      {
        if (mptr->prev != NULL) mptr->prev->next = NULL;
        mptr->prev = NULL;
      }
      g_list_free(matching_objects);
      matching_objects = mptr;
    }
    else 
    { 
      /* q->max_results == 0 */
      g_list_free(matching_objects);
      matching_objects = NULL;
    }
    object_count = q->max_results;
  }
  
  q->changed = 0;
  
  g_list_free(q->results);
  q->results = matching_objects;
  
  return matching_objects;
}

GList *
gncQueryLastRun (QueryNew *query)
{
  if (!query)
    return NULL;

  return query->results;
}

void gncQueryClear (QueryNew *query)
{
  QueryNew *q2 = gncQueryCreate ();
  swap_terms (query, q2);
  gncQueryDestroy (q2);

  g_list_free (query->books);
  query->books = NULL;
  g_list_free (query->results);
  query->results = NULL;
  query->changed = 1;
}

QueryNew * gncQueryCreate (void)
{
  QueryNew *qp = g_new0 (QueryNew, 1);
  query_init (qp, NULL);
  return qp;
}

void gncQuerySearchFor (QueryNew *q, GNCIdTypeConst obj_type)
{
  if (!q || !obj_type)
    return;

  if (safe_strcmp (q->search_for, obj_type)) {
    q->search_for = (GNCIdType) obj_type;
    q->changed = 1;
  }
}

QueryNew * gncQueryCreateFor (GNCIdTypeConst obj_type)
{
  QueryNew *q;
  if (!obj_type)
    return NULL;
  q = gncQueryCreate ();
  gncQuerySearchFor (q, obj_type);
  return q;
}

int gncQueryHasTerms (QueryNew *q)
{
  if (!q) return 0;
  return g_list_length (q->terms);
}

int gncQueryNumTerms (QueryNew *q)
{
  GList *o;
  int n = 0;
  if (!q) return 0;
  for (o = q->terms; o; o=o->next)
    n += g_list_length(o->data);
  return n;
}

gboolean gncQueryHasTermType (QueryNew *q, GSList *term_param)
{
  GList *or;
  GList *and;

  if (!q || !term_param)
    return FALSE;

  for(or = q->terms; or; or = or->next) {
    for(and = or->data; and; and = and->next) {
      QueryNewTerm *qt = and->data;
      if (!param_list_cmp (term_param, qt->param_list))
	return TRUE;
    }
  }

  return FALSE;
}

void gncQueryDestroy (QueryNew *q)
{
  if (!q) return;
  free_members (q);
  g_free (q);
}

QueryNew * gncQueryCopy (QueryNew *q)
{
  QueryNew *copy;
  if (!q) return NULL;
  copy = gncQueryCreate ();
  free_members (copy);

  memcpy (copy, q, sizeof (QueryNew));

  copy->terms = copy_or_terms (q->terms);
  copy->books = g_list_copy (q->books);
  copy->results = g_list_copy (q->results);

  copy_sort (&(copy->primary_sort), &(q->primary_sort));
  copy_sort (&(copy->secondary_sort), &(q->secondary_sort));
  copy_sort (&(copy->tertiary_sort), &(q->tertiary_sort));

  return copy;
}

/********************************************************************
 * gncQueryInvert 
 * return a newly-allocated Query object which is the 
 * logical inverse of the original.
 ********************************************************************/

QueryNew * gncQueryInvert (QueryNew *q)
{
  QueryNew  * retval;
  QueryNew  * right, * left, * iright, * ileft;
  QueryNewTerm * qt;
  GList  * aterms;
  GList  * cur;
  GList  * new_oterm;
  int    num_or_terms;

  if (!q)
    return NULL;

  num_or_terms = g_list_length(q->terms);

  switch(num_or_terms) 
  {
  case 0:
    retval = gncQueryCreate();
    retval->max_results = q->max_results;
    break;

    /* this is demorgan expansion for a single AND expression. */
    /* !(abc) = !a + !b + !c */
  case 1:
    retval = gncQueryCreate();
    retval->max_results = q->max_results;
    retval->books = g_list_copy (q->books);
    retval->search_for = q->search_for;
    retval->changed = 1;

    aterms = g_list_nth_data(q->terms, 0);
    new_oterm = NULL;
    for(cur=aterms; cur; cur=cur->next) {
      qt = copy_query_term(cur->data);
      qt->invert = !(qt->invert);
      new_oterm = g_list_append(NULL, qt);
      retval->terms = g_list_append(retval->terms, new_oterm);
    }
    break;

    /* if there are multiple OR-terms, we just recurse by 
     * breaking it down to !(a + b + c) = 
     * !a * !(b + c) = !a * !b * !c.  */
  default:
    right        = gncQueryCreate();
    right->terms = copy_or_terms(g_list_nth(q->terms, 1));

    left         = gncQueryCreate();
    left->terms  = g_list_append(NULL, 
                                 copy_and_terms(g_list_nth_data(q->terms, 0)));

    iright       = gncQueryInvert(right);
    ileft        = gncQueryInvert(left);

    retval = gncQueryMerge(iright, ileft, QUERY_AND);
    retval->books          = g_list_copy (q->books);
    retval->max_results    = q->max_results;
    retval->search_for	   = q->search_for;
    retval->changed        = 1;

    gncQueryDestroy(iright);
    gncQueryDestroy(ileft);
    gncQueryDestroy(right);
    gncQueryDestroy(left);
    break;
  }

  return retval;
}

/********************************************************************
 * gncQueryMerge
 * combine 2 Query objects by the logical operation in "op".
 ********************************************************************/

QueryNew * gncQueryMerge(QueryNew *q1, QueryNew *q2, QueryOp op)
{
  
  QueryNew * retval = NULL;
  QueryNew * i1, * i2;
  QueryNew * t1, * t2;
  GList * i, * j;
  GNCIdType search_for;

  if(!q1 || !q2 ) return NULL;
  if (q1->search_for && q2->search_for)
    g_return_val_if_fail (safe_strcmp (q1->search_for, q2->search_for) == 0,
			  NULL);

  search_for = (q1->search_for ? q1->search_for : q2->search_for);

  switch(op) 
  {
  case QUERY_OR:
    retval = gncQueryCreate();
    retval->terms = 
      g_list_concat(copy_or_terms(q1->terms), copy_or_terms(q2->terms));
    retval->books	   = merge_books (q1->books, q2->books);
    retval->max_results    = q1->max_results;
    retval->changed        = 1;
    break;

  case QUERY_AND:
    retval = gncQueryCreate();
    retval->books          = merge_books (q1->books, q2->books);
    retval->max_results    = q1->max_results;
    retval->changed        = 1;

    for(i=q1->terms; i; i=i->next) 
    {
      for(j=q2->terms; j; j=j->next) 
      {
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
    i1     = gncQueryInvert(q1);
    i2     = gncQueryInvert(q2);
    retval = gncQueryMerge(i1, i2, QUERY_OR);
    gncQueryDestroy(i1);
    gncQueryDestroy(i2);
    break;

  case QUERY_NOR:
    /* !(a+b) = (!a*!b) */
    i1     = gncQueryInvert(q1);
    i2     = gncQueryInvert(q2);
    retval = gncQueryMerge(i1, i2, QUERY_AND);
    gncQueryDestroy(i1);
    gncQueryDestroy(i2);
    break;

  case QUERY_XOR:
    /* a xor b = (a * !b) + (!a * b) */
    i1     = gncQueryInvert(q1);
    i2     = gncQueryInvert(q2);
    t1     = gncQueryMerge(q1, i2, QUERY_AND);
    t2     = gncQueryMerge(i1, q2, QUERY_AND);
    retval = gncQueryMerge(t1, t2, QUERY_OR);

    gncQueryDestroy(i1);
    gncQueryDestroy(i2);
    gncQueryDestroy(t1);
    gncQueryDestroy(t2);     
    break;
  }

  retval->search_for = search_for;
  return retval;
}

void
gncQueryMergeInPlace(QueryNew *q1, QueryNew *q2, QueryOp op)
{
  QueryNew *tmp_q;

  if (!q1 || !q2)
    return;

  tmp_q = gncQueryMerge (q1, q2, op);
  swap_terms (q1, tmp_q);
  gncQueryDestroy (tmp_q);
}

void
gncQuerySetSortOrder (QueryNew *q,
		      GSList *params1, GSList *params2, GSList *params3)
{
  if (!q) return;
  if (q->primary_sort.param_list)
    g_slist_free (q->primary_sort.param_list);
  q->primary_sort.param_list = params1;
  q->primary_sort.options = 0;

  if (q->secondary_sort.param_list)
    g_slist_free (q->secondary_sort.param_list);
  q->secondary_sort.param_list = params2;
  q->secondary_sort.options = 0;

  if (q->tertiary_sort.param_list)
    g_slist_free (q->tertiary_sort.param_list);
  q->tertiary_sort.param_list = params3;
  q->tertiary_sort.options = 0;

  q->changed = 1;
}

void gncQuerySetSortOptions (QueryNew *q, gint prim_op, gint sec_op,
			     gint tert_op)
{
  if (!q) return;
  q->primary_sort.options = prim_op;
  q->secondary_sort.options = sec_op;
  q->tertiary_sort.options = tert_op;
}

void gncQuerySetSortIncreasing (QueryNew *q, gboolean prim_inc,
				gboolean sec_inc, gboolean tert_inc)
{
  if (!q) return;
  q->primary_sort.increasing = prim_inc;
  q->secondary_sort.increasing = sec_inc;
  q->tertiary_sort.increasing = tert_inc;
}

void gncQuerySetMaxResults (QueryNew *q, int n)
{
  if (!q) return;
  q->max_results = n;
}

void gncQueryAddGUIDListMatch (QueryNew *q, GSList *param_list,
			       GList *guid_list, guid_match_t options,
			       QueryOp op)
{
  QueryPredData_t pdata;

  if (!q || !param_list) return;

  if (!guid_list)
    g_return_if_fail (options == GUID_MATCH_NULL);

  pdata = gncQueryGUIDPredicate (options, guid_list);
  gncQueryAddTerm (q, param_list, pdata, op);
}

void gncQueryAddGUIDMatch (QueryNew *q, GSList *param_list,
			   const GUID *guid, QueryOp op)
{
  GList *g = NULL;

  if (!q || !param_list) return;

  if (guid)
    g = g_list_prepend (g, (gpointer)guid);

  gncQueryAddGUIDListMatch (q, param_list, g,
			    g ? GUID_MATCH_ANY : GUID_MATCH_NULL, op);

  g_list_free (g);
}

void gncQuerySetBook (QueryNew *q, GNCBook *book)
{
  if (!q || !book) return;

  q->books = g_list_prepend (q->books, book);
  gncQueryAddGUIDMatch (q, g_slist_prepend (g_slist_prepend (NULL,
							     QUERY_PARAM_GUID),
					    QUERY_PARAM_BOOK),
			gnc_book_get_guid(book), QUERY_AND);
}

/**********************************************************************/
/* PRIVATE PUBLISHED API FUNCTIONS                                    */

void gncQueryNewInit (void)
{
  gncQueryCoreInit ();
  gncQueryObjectInit ();
}

void gncQueryNewShutdown (void)
{
  gncQueryObjectShutdown ();
  gncQueryCoreShutdown ();
}

int gncQueryGetMaxResults (QueryNew *q)
{
  if (!q) return 0;
  return q->max_results;
}

GNCIdType gncQueryGetSearchFor (QueryNew *q)
{
  if (!q) return NULL;
  return q->search_for;
}

GList * gncQueryGetTerms (QueryNew *q)
{
  if (!q) return NULL;
  return q->terms;
}

GSList * gncQueryTermGetParamPath (QueryNewTerm_t qt)
{
  if (!qt)
    return NULL;
  return qt->param_list;
}

QueryPredData_t gncQueryTermGetPredData (QueryNewTerm_t qt)
{
  if (!qt)
    return NULL;
  return qt->pdata;
}

gboolean gncQueryTermIsInverted (QueryNewTerm_t qt)
{
  if (!qt)
    return FALSE;
  return qt->invert;
}

void gncQueryGetSorts (QueryNew *q, QueryNewSort_t *primary,
		       QueryNewSort_t *secondary, QueryNewSort_t *tertiary)
{
  if (!q)
    return;
  if (primary)
    *primary = &(q->primary_sort);
  if (secondary)
    *secondary = &(q->secondary_sort);
  if (tertiary)
    *tertiary = &(q->tertiary_sort);
}

GSList * gncQuerySortGetParamPath (QueryNewSort_t qs)
{
  if (!qs)
    return NULL;
  return qs->param_list;
}

gint gncQuerySortGetSortOptions (QueryNewSort_t qs)
{
  if (!qs)
    return 0;
  return qs->options;
}

gboolean gncQuerySortGetIncreasing (QueryNewSort_t qs)
{
  if (!qs)
    return FALSE;
  return qs->increasing;
}

static gboolean gncQueryTermEqual (QueryNewTerm_t qt1, QueryNewTerm_t qt2)
{
  if (qt1 == qt2) return TRUE;
  if (!qt1 || !qt2) return FALSE;

  if (qt1->invert != qt2->invert) return FALSE;
  if (param_list_cmp (qt1->param_list, qt2->param_list)) return FALSE;
  return gncQueryCorePredicateEqual (qt1->pdata, qt2->pdata);
}

static gboolean gncQuerySortEqual (QueryNewSort_t qs1, QueryNewSort_t qs2)
{
  if (qs1 == qs2) return TRUE;
  if (!qs1 || !qs2) return FALSE;

  /* "Empty" sorts are equivalent, regardless of the flags */
  if (!qs1->param_list && !qs2->param_list) return TRUE;

  if (qs1->options != qs2->options) return FALSE;
  if (qs1->increasing != qs2->increasing) return FALSE;
  return (param_list_cmp (qs1->param_list, qs2->param_list) == 0);
}

gboolean gncQueryEqual (QueryNew *q1, QueryNew *q2)
{
  GList *or1, *or2;

  if (q1 == q2) return TRUE;
  if (!q1 || !q2) return FALSE;

  if (g_list_length (q1->terms) != g_list_length (q2->terms)) return FALSE;
  if (q1->max_results != q2->max_results) return FALSE;

  for (or1 = q1->terms, or2 = q2->terms; or1;
       or1 = or1->next, or2 = or2->next)
  {
    GList *and1, *and2;

    and1 = or1->data;
    and2 = or2->data;

    if (g_list_length (and1) != g_list_length (and2)) return FALSE;

    for ( ; and1; and1 = and1->next, and2 = and2->next)
      if (!gncQueryTermEqual (and1->data, and2->data))
        return FALSE;
  }

  if (!gncQuerySortEqual (&(q1->primary_sort), &(q2->primary_sort)))
    return FALSE;
  if (!gncQuerySortEqual (&(q1->secondary_sort), &(q2->secondary_sort)))
    return FALSE;
  if (!gncQuerySortEqual (&(q1->tertiary_sort), &(q2->tertiary_sort)))
    return FALSE;

  return TRUE;
}

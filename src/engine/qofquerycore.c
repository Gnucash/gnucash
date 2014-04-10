/********************************************************************\
 * QueryCore.c -- API for providing core Query data types           *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "gnc-engine-util.h"
#include "qofquerycore.h"
#include "qofquerycore-p.h"
#include "QueryNew.h"

static short module = MOD_QUERY;

/* A function to destroy a query predicate's pdata */
typedef void (*QueryPredDataFree) (QofQueryPredData *pdata);

/* A function to copy a query's predicate data */
typedef QofQueryPredData *(*QueryPredicateCopy) (QofQueryPredData *pdata);

/* A function to take the object, apply the get_fcn, and return
 * a printable string.  Note that this QofQueryAccess function should
 * be returning a type equal to this core object type.
 *
 * Note that this string MUST be freed by the caller.
 */
typedef char * (*QueryToString) (gpointer object, QofQueryAccess get_fcn);

/* A function to test for equality of predicate data */
typedef gboolean (*QueryPredicateEqual) (QofQueryPredData *p1, 
					 QofQueryPredData *p2);

/* This function registers a new Core Object with the QueryNew
 * subsystem.  It maps the "core_name" object to the given
 * query_predicate, predicate_copy, and predicate_data_free functions.
 */
static void gncQueryRegisterCoreObject (char const *type_name,
				 QueryPredicate pred,
				 QueryCompare comp,
				 QueryPredicateCopy copy,
				 QueryPredDataFree pd_free,
				 QueryToString to_string,
				 QueryPredicateEqual pred_equal);
/* An example:
 *
 * gncQueryRegisterCoreObject (QOF_QUERYCORE_STRING, string_match_predicate,
 *			       string_compare_fcn, string_free_pdata,
 *			       string_print_fcn, pred_equal_fcn);
 */

static QueryPredicateCopy gncQueryCoreGetCopy (char const *type);
static QueryPredDataFree gncQueryCoreGetPredFree (char const *type);

/* Core Type Predicate helpers */
typedef const char * (*query_string_getter) (gpointer);
static const char * query_string_type = QOF_QUERYCORE_STRING;

typedef Timespec (*query_date_getter) (gpointer);
static const char * query_date_type = QOF_QUERYCORE_DATE;

typedef gnc_numeric (*query_numeric_getter) (gpointer);
static const char * query_numeric_type = QOF_QUERYCORE_NUMERIC;

typedef GList * (*query_glist_getter) (gpointer);
typedef const GUID * (*query_guid_getter) (gpointer);
static const char * query_guid_type = QOF_QUERYCORE_GUID;

typedef gint32 (*query_int32_getter) (gpointer);
static const char * query_int32_type = QOF_QUERYCORE_INT32;

typedef gint64 (*query_int64_getter) (gpointer);
static const char * query_int64_type = QOF_QUERYCORE_INT64;

typedef double (*query_double_getter) (gpointer);
static const char * query_double_type = QOF_QUERYCORE_DOUBLE;

typedef gboolean (*query_boolean_getter) (gpointer);
static const char * query_boolean_type = QOF_QUERYCORE_BOOLEAN;

typedef char (*query_char_getter) (gpointer);
static const char * query_char_type = QOF_QUERYCORE_CHAR;

typedef kvp_frame * (*query_kvp_getter) (gpointer);
static const char * query_kvp_type = QOF_QUERYCORE_KVP;

/* Tables for predicate storage and lookup */
static gboolean initialized = FALSE;
static GHashTable *predTable = NULL;
static GHashTable *cmpTable = NULL;
static GHashTable *copyTable = NULL;
static GHashTable *freeTable = NULL;
static GHashTable *toStringTable = NULL;
static GHashTable *predEqualTable = NULL;

#define COMPARE_ERROR -3
#define PREDICATE_ERROR -2
#define VERIFY_PDATA(str) { \
	g_return_if_fail (pd != NULL); \
	g_return_if_fail (pd->type_name == str || \
			!safe_strcmp (str, pd->type_name)); \
}
#define VERIFY_PDATA_R(str) { \
	g_return_val_if_fail (pd != NULL, NULL); \
	g_return_val_if_fail (pd->type_name == str || \
				!safe_strcmp (str, pd->type_name), \
				NULL); \
}
#define VERIFY_PREDICATE(str) { \
	g_return_val_if_fail (get_fcn != NULL, PREDICATE_ERROR); \
	g_return_val_if_fail (pd != NULL, PREDICATE_ERROR); \
	g_return_val_if_fail (pd->type_name == str || \
				!safe_strcmp (str, pd->type_name), \
				PREDICATE_ERROR); \
}

/********************************************************************/
/* TYPE-HANDLING FUNCTIONS */

/* QOF_QUERYCORE_STRING */

static int string_match_predicate (gpointer object, QofQueryAccess get_fcn,
				   QofQueryPredData *pd)
{
  query_string_t pdata = (query_string_t) pd;
  const char *s;
  int ret = 0;

  VERIFY_PREDICATE (query_string_type);

  s = ((query_string_getter)get_fcn) (object);

  if (!s) s = "";

  if (pdata->is_regex) {
    regmatch_t match;
    if (!regexec (&pdata->compiled, s, 1, &match, 0))
      ret = 1;

  } else if (pdata->options == QOF_STRING_MATCH_CASEINSENSITIVE) {
    if (strcasestr (s, pdata->matchstring))
      ret = 1;

  } else {
    if (strstr (s, pdata->matchstring))
      ret = 1;
  }

  switch (pd->how) {
  case QOF_COMPARE_EQUAL:
    return ret;
  case QOF_COMPARE_NEQ:
    return !ret;
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int string_compare_func (gpointer a, gpointer b, gint options,
				QofQueryAccess get_fcn)
{
  const char *s1, *s2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  s1 = ((query_string_getter)get_fcn) (a);
  s2 = ((query_string_getter)get_fcn) (b);

  if (options == QOF_STRING_MATCH_CASEINSENSITIVE)
    return safe_strcasecmp (s1, s2);

  return safe_strcmp (s1, s2);
}

static void string_free_pdata (QofQueryPredData *pd)
{
  query_string_t pdata = (query_string_t) pd;

  VERIFY_PDATA (query_string_type);

  if (pdata->is_regex)
    regfree (&pdata->compiled);
  else
    g_free (pdata->matchstring);

  g_free (pdata);
}

static QofQueryPredData *string_copy_predicate (QofQueryPredData *pd)
{
  query_string_t pdata = (query_string_t) pd;

  VERIFY_PDATA_R (query_string_type);

  return qof_query_string_predicate (pd->how, pdata->matchstring, pdata->options,
				  pdata->is_regex);
}

static gboolean string_predicate_equal (QofQueryPredData *p1,
QofQueryPredData *p2)
{
  query_string_t pd1 = (query_string_t) p1;
  query_string_t pd2 = (query_string_t) p2;

  if (pd1->options != pd2->options) return FALSE;
  if (pd1->is_regex != pd2->is_regex) return FALSE;
  return (safe_strcmp (pd1->matchstring, pd2->matchstring) == 0);
}

QofQueryPredData *qof_query_string_predicate (QofQueryCompare how,
					 char *str, QofStringMatch options,
					 gboolean is_regex)
{
  query_string_t pdata;

  g_return_val_if_fail (str, NULL);
  g_return_val_if_fail (*str != '\0', NULL);
  g_return_val_if_fail (how == QOF_COMPARE_EQUAL || how == QOF_COMPARE_NEQ, NULL);

  pdata = g_new0 (query_string_def, 1);
  pdata->pd.type_name = query_string_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->matchstring = g_strdup (str);

  if (is_regex) {
    int flags = REG_EXTENDED;
    if (options == QOF_STRING_MATCH_CASEINSENSITIVE)
      flags |= REG_ICASE;

    regcomp(&pdata->compiled, str, flags);
    pdata->is_regex = TRUE;
  }

  return ((QofQueryPredData*)pdata);
}

static char * string_to_string (gpointer object, QofQueryAccess get)
{
  const char *res = ((query_string_getter)get)(object);
  if (res)
    return g_strdup (res);
  return NULL;
}

/* QOF_QUERYCORE_DATE */

static int date_compare (Timespec ta, Timespec tb, QofDateMatch options)
{
  if (options == QOF_DATE_MATCH_ROUNDED) {
    ta = timespecCanonicalDayTime (ta);
    tb = timespecCanonicalDayTime (tb);
  }

  if (ta.tv_sec < tb.tv_sec)
    return -1;
  if (ta.tv_sec > tb.tv_sec)
    return 1;

  if (ta.tv_nsec < tb.tv_nsec)
    return -1;
  if (ta.tv_nsec > tb.tv_nsec)
    return 1;

  return 0;
}

static int date_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  query_date_t pdata = (query_date_t)pd;
  Timespec objtime;
  int compare;

  VERIFY_PREDICATE (query_date_type);

  objtime = ((query_date_getter)get_fcn) (object);
  compare = date_compare (objtime, pdata->date, pdata->options);

  switch (pd->how) {
  case QOF_COMPARE_LT:
    return (compare < 0);
  case QOF_QOF_COMPARE_LTE:
    return (compare <= 0);
  case QOF_COMPARE_EQUAL:
    return (compare == 0);
  case QOF_COMPARE_GT:
    return (compare > 0);
  case QOF_QOF_COMPARE_GTE:
    return (compare >= 0);
  case QOF_COMPARE_NEQ:
    return (compare != 0);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int date_compare_func (gpointer a, gpointer b, gint options,
			      QofQueryAccess get_fcn)
{
  Timespec ta, tb;

  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  ta = ((query_date_getter)get_fcn) (a);
  tb = ((query_date_getter)get_fcn) (b);

  return date_compare (ta, tb, options);
}

static void date_free_pdata (QofQueryPredData *pd)
{
  query_date_t pdata = (query_date_t)pd;

  VERIFY_PDATA (query_date_type);

  g_free (pdata);
}

static QofQueryPredData *
date_copy_predicate (QofQueryPredData *pd)
{
  query_date_t pdata = (query_date_t)pd;

  VERIFY_PDATA_R (query_date_type);

  return qof_query_date_predicate (pd->how, pdata->options, pdata->date);
}

static gboolean date_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_date_t pd1 = (query_date_t) p1;
  query_date_t pd2 = (query_date_t) p2;

  if (pd1->options != pd2->options) return FALSE;
  return timespec_equal (&(pd1->date), &(pd2->date));
}

QofQueryPredData *
qof_query_date_predicate (QofQueryCompare how,
				       QofDateMatch options, Timespec date)
{
  query_date_t pdata;

  pdata = g_new0 (query_date_def, 1);
  pdata->pd.type_name = query_date_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->date = date;
  return ((QofQueryPredData*)pdata);
}

static char * date_to_string (gpointer object, QofQueryAccess get)
{
  Timespec ts = ((query_date_getter)get)(object);

  if (ts.tv_sec || ts.tv_nsec)
    return g_strdup (gnc_print_date (ts));

  return NULL;
}

/* QOF_QUERYCORE_NUMERIC */

static int numeric_match_predicate (gpointer object, QofQueryAccess get_fcn,
				    QofQueryPredData* pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  gnc_numeric obj_val;
  int compare;

  VERIFY_PREDICATE (query_numeric_type);

  obj_val = ((query_numeric_getter)get_fcn) (object);

  switch (pdata->options) {
  case QOF_NUMERIC_MATCH_CREDIT:
    if (gnc_numeric_positive_p (obj_val)) return 0;
    break;
  case QOF_NUMERIC_MATCH_DEBIT:
    if (gnc_numeric_negative_p (obj_val)) return 0;
    break;
  default:
    break;
  }

  if (pd->how == QOF_COMPARE_EQUAL || pd->how == QOF_COMPARE_NEQ) {
    gnc_numeric cmp_val = gnc_numeric_create (1, 10000);
    compare =
      (gnc_numeric_compare (gnc_numeric_abs
			    (gnc_numeric_sub (gnc_numeric_abs (obj_val),
					      gnc_numeric_abs (pdata->amount),
					      100000, GNC_RND_ROUND)),
			    cmp_val) < 0);
  } else
    compare = gnc_numeric_compare (gnc_numeric_abs (obj_val), pdata->amount);

  switch (pd->how) {
  case QOF_COMPARE_LT:
    return (compare < 0);
  case QOF_QOF_COMPARE_LTE:
    return (compare <= 0);
  case QOF_COMPARE_EQUAL:
    return compare;
  case QOF_COMPARE_GT:
    return (compare > 0);
  case QOF_QOF_COMPARE_GTE:
    return (compare >= 0);
  case QOF_COMPARE_NEQ:
    return !compare;
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int numeric_compare_func (gpointer a, gpointer b, gint options,
				 QofQueryAccess get_fcn)
{
  gnc_numeric va, vb;

  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  va = ((query_numeric_getter)get_fcn) (a);
  vb = ((query_numeric_getter)get_fcn) (b);

  return gnc_numeric_compare (va, vb);  
}

static void numeric_free_pdata (QofQueryPredData* pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  VERIFY_PDATA (query_numeric_type);
  g_free (pdata);
}

static QofQueryPredData *
numeric_copy_predicate (QofQueryPredData *pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  VERIFY_PDATA_R (query_numeric_type);
  return qof_query_numeric_predicate (pd->how, pdata->options, pdata->amount);
}

static gboolean 
numeric_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_numeric_t pd1 = (query_numeric_t) p1;
  query_numeric_t pd2 = (query_numeric_t) p2;

  if (pd1->options != pd2->options) return FALSE;
  return gnc_numeric_equal (pd1->amount, pd2->amount);
}

QofQueryPredData *
qof_query_numeric_predicate (QofQueryCompare how,
					  QofNumericMatch options,
					  gnc_numeric value)
{
  query_numeric_t pdata;
  pdata = g_new0 (query_numeric_def, 1);
  pdata->pd.type_name = query_numeric_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->amount = value;
  return ((QofQueryPredData*)pdata);
}

static char * numeric_to_string (gpointer object, QofQueryAccess get)
{
  gnc_numeric num = ((query_numeric_getter)get)(object);

  return g_strdup (gnc_numeric_to_string (num));
}

static char * debcred_to_string (gpointer object, QofQueryAccess get)
{
  gnc_numeric num = ((query_numeric_getter)get)(object);

  return g_strdup (gnc_numeric_to_string (num));
}

/* QOF_QUERYCORE_GUID */

static int guid_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  GList *node, *o_list;
  const GUID *guid = NULL;

  VERIFY_PREDICATE (query_guid_type);

  switch (pdata->options) {

  case QOF_GUID_MATCH_ALL:
    /* object is a GList of objects; get_fcn must be called on each one.
     * See if every guid in the predicate is accounted-for in the
     * object list
     */

    for (node = pdata->guids; node; node = node->next) {

      /* See if this GUID matches the object's guid */
      for (o_list = object; o_list; o_list = o_list->next) {
	guid = ((query_guid_getter)get_fcn) (o_list->data);
	if (guid_equal (node->data, guid))
	  break;
      }

      /* 
       * If o_list is NULL, we've walked the whole list without finding
       * a match.  Therefore break out now, the match has failed.
       */
      if (o_list == NULL)
	break;
    }

    /* 
     * The match is complete.  If node == NULL then we've succesfully
     * found a match for all the guids in the predicate.  Return
     * appropriately below.
     */

    break;

  case QOF_GUID_MATCH_LIST_ANY:
    /* object is a single object, getter returns a GList* of GUID*
     *
     * see if any GUID* in the returned list matches any guid in the
     * predicate match list
     */

    o_list = ((query_glist_getter)get_fcn) (object);

    for (node = o_list; node; node = node->next) {
      GList *node2;

      /* Search the predicate data for a match */
      for (node2 = pdata->guids; node2; node2 = node2->next) {
	if (guid_equal (node->data, node2->data))
	  break;
      }

      /* Check to see if we found a match.  If so, break now */
      if (node2 != NULL)
	break;
    }

    g_list_free(o_list);

    /* yea, node may point to an invalid location, but that's ok.
     * we're not _USING_ the value, just checking that it's non-NULL
     */

    break;

  default:
    /* object is a single object, getter returns a GUID* 
     *
     * See if the guid is in the list
     */

    guid = ((query_guid_getter)get_fcn) (object);
    for (node = pdata->guids; node; node = node->next) {
      if (guid_equal (node->data, guid))
	break;
    }
  }

  switch (pdata->options) {
  case QOF_GUID_MATCH_ANY:
  case QOF_GUID_MATCH_LIST_ANY:
    return (node != NULL);
    break;
  case QOF_GUID_MATCH_NONE:
  case QOF_GUID_MATCH_ALL:
    return (node == NULL);
    break;
  case QOF_GUID_MATCH_NULL:
    return (guid == NULL);
    break;
  default:
    PWARN ("bad match type");
    return 0;
  }
}

static void guid_free_pdata (QofQueryPredData *pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  GList *node;
  VERIFY_PDATA (query_guid_type);
  for (node = pdata->guids; node; node = node->next)
    xaccGUIDFree (node->data);
  g_list_free (pdata->guids);
  g_free (pdata);
}

static QofQueryPredData *
guid_copy_predicate (QofQueryPredData *pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  VERIFY_PDATA_R (query_guid_type);
  return qof_query_guid_predicate (pdata->options, pdata->guids);
}

static gboolean 
guid_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_guid_t pd1 = (query_guid_t) p1;
  query_guid_t pd2 = (query_guid_t) p2;
  GList *l1 = pd1->guids, *l2 = pd2->guids;

  if (pd1->options != pd2->options) return FALSE;
  if (g_list_length (l1) != g_list_length (l2)) return FALSE;
  for ( ; l1 ; l1 = l1->next, l2 = l2->next) 
    if (!guid_equal (l1->data, l2->data))
      return FALSE;
  return TRUE;
}

QofQueryPredData *
qof_query_guid_predicate (QofGuidMatch options, GList *guids)
{
  query_guid_t pdata;
  GList *node;

  pdata = g_new0 (query_guid_def, 1);
  pdata->pd.how = QOF_COMPARE_EQUAL;
  pdata->pd.type_name = query_guid_type;
  pdata->options = options;
  pdata->guids = g_list_copy (guids);
  for (node = pdata->guids; node; node = node->next) {
    GUID *guid = xaccGUIDMalloc ();
    *guid = *((GUID *)node->data);
    node->data = guid;
  }
  return ((QofQueryPredData*)pdata);
}

/* ================================================================ */
/* QOF_QUERYCORE_INT32 */

static int int32_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  gint32 val;
  query_int32_t pdata = (query_int32_t)pd;

  VERIFY_PREDICATE (query_int32_type);

  val = ((query_int32_getter)get_fcn) (object);

  switch (pd->how) {
  case QOF_COMPARE_LT:
    return (val < pdata->val);
  case QOF_QOF_COMPARE_LTE:
    return (val <= pdata->val);
  case QOF_COMPARE_EQUAL:
    return (val == pdata->val);
  case QOF_COMPARE_GT:
    return (val > pdata->val);
  case QOF_QOF_COMPARE_GTE:
    return (val >= pdata->val);
  case QOF_COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int int32_compare_func (gpointer a, gpointer b, gint options,
			      QofQueryAccess get_fcn)
{
  gint32 v1, v2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  v1 = ((query_int32_getter)get_fcn)(a);
  v2 = ((query_int32_getter)get_fcn)(b);

  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}

static void int32_free_pdata (QofQueryPredData *pd)
{
  query_int32_t pdata = (query_int32_t)pd;
  VERIFY_PDATA (query_int32_type);
  g_free (pdata);
}

static QofQueryPredData *
int32_copy_predicate (QofQueryPredData *pd)
{
  query_int32_t pdata = (query_int32_t)pd;
  VERIFY_PDATA_R (query_int32_type);
  return qof_query_int32_predicate (pd->how, pdata->val);
}

static gboolean 
int32_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_int32_t pd1 = (query_int32_t) p1;
  query_int32_t pd2 = (query_int32_t) p2;

  return (pd1->val == pd2->val);
}

QofQueryPredData *
qof_query_int32_predicate (QofQueryCompare how, gint32 val)
{
  query_int32_t pdata = g_new0 (query_int32_def, 1);
  pdata->pd.type_name = query_int32_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QofQueryPredData*)pdata);
}

static char * int32_to_string (gpointer object, QofQueryAccess get)
{
  gint32 num = ((query_int32_getter)get)(object);

  return g_strdup_printf ("%d", num);
}

/* ================================================================ */
/* QOF_QUERYCORE_INT64 */

static int int64_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  gint64 val;
  query_int64_t pdata = (query_int64_t)pd;

  VERIFY_PREDICATE (query_int64_type);

  val = ((query_int64_getter)get_fcn) (object);

  switch (pd->how) {
  case QOF_COMPARE_LT:
    return (val < pdata->val);
  case QOF_QOF_COMPARE_LTE:
    return (val <= pdata->val);
  case QOF_COMPARE_EQUAL:
    return (val == pdata->val);
  case QOF_COMPARE_GT:
    return (val > pdata->val);
  case QOF_QOF_COMPARE_GTE:
    return (val >= pdata->val);
  case QOF_COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int int64_compare_func (gpointer a, gpointer b, gint options,
			      QofQueryAccess get_fcn)
{
  gint64 v1, v2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  v1 = ((query_int64_getter)get_fcn)(a);
  v2 = ((query_int64_getter)get_fcn)(b);

  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}

static void int64_free_pdata (QofQueryPredData *pd)
{
  query_int64_t pdata = (query_int64_t)pd;
  VERIFY_PDATA (query_int64_type);
  g_free (pdata);
}

static QofQueryPredData *
int64_copy_predicate (QofQueryPredData *pd)
{
  query_int64_t pdata = (query_int64_t)pd;
  VERIFY_PDATA_R (query_int64_type);
  return qof_query_int64_predicate (pd->how, pdata->val);
}

static gboolean 
int64_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_int64_t pd1 = (query_int64_t) p1;
  query_int64_t pd2 = (query_int64_t) p2;

  return (pd1->val == pd2->val);
}

QofQueryPredData *
qof_query_int64_predicate (QofQueryCompare how, gint64 val)
{
  query_int64_t pdata = g_new0 (query_int64_def, 1);
  pdata->pd.type_name = query_int64_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QofQueryPredData*)pdata);
}

static char * int64_to_string (gpointer object, QofQueryAccess get)
{
  gint64 num = ((query_int64_getter)get)(object);

  return g_strdup_printf (GNC_SCANF_LLD, num);
}

/* ================================================================ */
/* QOF_QUERYCORE_DOUBLE */

static int double_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  double val;
  query_double_t pdata = (query_double_t)pd;

  VERIFY_PREDICATE (query_double_type);

  val = ((query_double_getter)get_fcn) (object);

  switch (pd->how) {
  case QOF_COMPARE_LT:
    return (val < pdata->val);
  case QOF_QOF_COMPARE_LTE:
    return (val <= pdata->val);
  case QOF_COMPARE_EQUAL:
    return (val == pdata->val);
  case QOF_COMPARE_GT:
    return (val > pdata->val);
  case QOF_QOF_COMPARE_GTE:
    return (val >= pdata->val);
  case QOF_COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int double_compare_func (gpointer a, gpointer b, gint options,
			      QofQueryAccess get_fcn)
{
  double v1, v2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  v1 = ((query_double_getter)get_fcn) (a);
  v2 = ((query_double_getter)get_fcn) (b);

  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}

static void double_free_pdata (QofQueryPredData *pd)
{
  query_double_t pdata = (query_double_t)pd;
  VERIFY_PDATA (query_double_type);
  g_free (pdata);
}

static QofQueryPredData *
double_copy_predicate (QofQueryPredData *pd)
{
  query_double_t pdata = (query_double_t)pd;
  VERIFY_PDATA_R (query_double_type);
  return qof_query_double_predicate (pd->how, pdata->val);
}

static gboolean 
double_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_double_t pd1 = (query_double_t) p1;
  query_double_t pd2 = (query_double_t) p2;

  return (pd1->val == pd2->val);
}

QofQueryPredData *
qof_query_double_predicate (QofQueryCompare how, double val)
{
  query_double_t pdata = g_new0 (query_double_def, 1);
  pdata->pd.type_name = query_double_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QofQueryPredData*)pdata);
}

static char * double_to_string (gpointer object, QofQueryAccess get)
{
  double num = ((query_double_getter)get)(object);

  return g_strdup_printf ("%f", num);
}

/* QOF_QUERYCORE_BOOLEAN */

static int boolean_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  gboolean val;
  query_boolean_t pdata = (query_boolean_t)pd;

  VERIFY_PREDICATE (query_boolean_type);

  val = ((query_boolean_getter)get_fcn) (object);

  switch (pd->how) {
  case QOF_COMPARE_EQUAL:
    return (val == pdata->val);
  case  QOF_COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int boolean_compare_func (gpointer a, gpointer b, gint options,
				 QofQueryAccess get_fcn)
{
  gboolean va, vb;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);
  va = ((query_boolean_getter)get_fcn) (a);
  vb = ((query_boolean_getter)get_fcn) (b);
  if (!va && vb) return -1;
  if (va && !vb) return 1;
  return 0;
}

static void boolean_free_pdata (QofQueryPredData *pd)
{
  query_boolean_t pdata = (query_boolean_t)pd;
  VERIFY_PDATA (query_boolean_type);
  g_free (pdata);
}

static QofQueryPredData *
boolean_copy_predicate (QofQueryPredData *pd)
{
  query_boolean_t pdata = (query_boolean_t)pd;
  VERIFY_PDATA_R (query_boolean_type);
  return qof_query_boolean_predicate (pd->how, pdata->val);
}

static gboolean 
boolean_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_boolean_t pd1 = (query_boolean_t) p1;
  query_boolean_t pd2 = (query_boolean_t) p2;

  return (pd1->val == pd2->val);
}

QofQueryPredData *
qof_query_boolean_predicate (QofQueryCompare how, gboolean val)
{
  query_boolean_t pdata;
  g_return_val_if_fail (how == QOF_COMPARE_EQUAL || how == QOF_COMPARE_NEQ, NULL);

  pdata = g_new0 (query_boolean_def, 1);
  pdata->pd.type_name = query_boolean_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QofQueryPredData*)pdata);
}

static char * boolean_to_string (gpointer object, QofQueryAccess get)
{
  gboolean num = ((query_boolean_getter)get)(object);

  return g_strdup_printf ("%s", (num ? "X" : ""));
}

/* QOF_QUERYCORE_CHAR */

static int char_match_predicate (gpointer object, QofQueryAccess get_fcn,
				 QofQueryPredData *pd)
{
  char c;
  query_char_t pdata = (query_char_t)pd;

  VERIFY_PREDICATE (query_char_type);

  c = ((query_char_getter)get_fcn) (object);

  switch (pdata->options) {
  case QOF_CHAR_MATCH_ANY:
    if (strchr (pdata->char_list, c)) return 1;
    return 0;
  case QOF_CHAR_MATCH_NONE:
    if (!strchr (pdata->char_list, c)) return 1;
    return 0;
  default:
    PWARN ("bad match type");
    return 0;
  }
}

static int char_compare_func (gpointer a, gpointer b, gint options,
			      QofQueryAccess get_fcn)
{
  char va, vb;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);
  va = ((query_char_getter)get_fcn)(a);
  vb = ((query_char_getter)get_fcn)(b);
  return (va-vb);
}

static void char_free_pdata (QofQueryPredData *pd)
{
  query_char_t pdata = (query_char_t)pd;
  VERIFY_PDATA (query_char_type);
  g_free (pdata->char_list);
  g_free (pdata);  
}

static QofQueryPredData *
char_copy_predicate (QofQueryPredData *pd)
{
  query_char_t pdata = (query_char_t)pd;
  VERIFY_PDATA_R (query_char_type);
  return qof_query_char_predicate (pdata->options, pdata->char_list);
}

static gboolean 
char_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_char_t pd1 = (query_char_t) p1;
  query_char_t pd2 = (query_char_t) p2;

  if (pd1->options != pd2->options) return FALSE;
  return (safe_strcmp (pd1->char_list, pd2->char_list) == 0);
}

QofQueryPredData *
qof_query_char_predicate (QofCharMatch options, const char *chars)
{
  query_char_t pdata;
  g_return_val_if_fail (chars, NULL);
  pdata = g_new0 (query_char_def, 1);
  pdata->pd.type_name = query_char_type;
  pdata->pd.how = QOF_COMPARE_EQUAL;
  pdata->options = options;
  pdata->char_list = g_strdup (chars);
  return ((QofQueryPredData*)pdata);
}

static char * char_to_string (gpointer object, QofQueryAccess get)
{
  char num = ((query_char_getter)get)(object);

  return g_strdup_printf ("%c", num);
}

/* QOF_QUERYCORE_KVP */

static int kvp_match_predicate (gpointer object, QofQueryAccess get_fcn,
				QofQueryPredData *pd)
{
  int compare;
  kvp_frame *kvp;
  kvp_value *value;
  query_kvp_t pdata = (query_kvp_t)pd;

  VERIFY_PREDICATE (query_kvp_type);

  kvp = ((query_kvp_getter)get_fcn) (object);
  if (!kvp)
    return 0;

  value = kvp_frame_get_slot_path_gslist (kvp, pdata->path);
  if (!value)
    return 0;

  if (kvp_value_get_type (value) != kvp_value_get_type (pdata->value))
    return 0;

  compare = kvp_value_compare (value, pdata->value);

  switch (pd->how)
  {
  case QOF_COMPARE_LT:
    return (compare < 0);
  case QOF_QOF_COMPARE_LTE:
    return (compare <= 0);
  case QOF_COMPARE_EQUAL:
    return (compare == 0);
  case QOF_QOF_COMPARE_GTE:
    return (compare >= 0);
  case QOF_COMPARE_GT:
    return (compare > 0);
  case QOF_COMPARE_NEQ:
    return (compare != 0);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static void kvp_free_pdata (QofQueryPredData *pd)
{
  query_kvp_t pdata = (query_kvp_t)pd;
  GSList *node;

  VERIFY_PDATA (query_kvp_type);
  kvp_value_delete (pdata->value);
  for (node = pdata->path; node; node = node->next) {
    g_free (node->data);
    node->data = NULL;
  }
  g_slist_free (pdata->path);
  g_free (pdata);  
}

static QofQueryPredData *
kvp_copy_predicate (QofQueryPredData *pd)
{
  query_kvp_t pdata = (query_kvp_t)pd;
  VERIFY_PDATA_R (query_kvp_type);
  return qof_query_kvp_predicate (pd->how, pdata->path, pdata->value);
}

static gboolean 
kvp_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  query_kvp_t pd1 = (query_kvp_t) p1;
  query_kvp_t pd2 = (query_kvp_t) p2;
  GSList *n1, *n2;

  n1 = pd1->path;
  n2 = pd2->path;

  for ( ; n1 && n2; n1 = n1->next, n2 = n2->next)
    if (safe_strcmp (n1->data, n2->data) != 0)
      return FALSE;

  if (n1 || n2)
    return FALSE;

  return (kvp_value_compare (pd1->value, pd2->value) == 0);
}

QofQueryPredData *
qof_query_kvp_predicate (QofQueryCompare how,
				      GSList *path, const kvp_value *value)
{
  query_kvp_t pdata;
  GSList *node;

  g_return_val_if_fail (path && value, NULL);

  pdata = g_new0 (query_kvp_def, 1);
  pdata->pd.type_name = query_kvp_type;
  pdata->pd.how = how;
  pdata->value = kvp_value_copy (value);
  pdata->path = g_slist_copy (path);
  for (node = pdata->path; node; node = node->next)
    node->data = g_strdup (node->data);

  return ((QofQueryPredData*)pdata);
}

/* initialization */

static void init_tables (void)
{
  unsigned int i;
  struct {
    char const *name;
    QueryPredicate pred;
    QueryCompare comp;
    QueryPredicateCopy copy;
    QueryPredDataFree pd_free;
    QueryToString toString;
    QueryPredicateEqual pred_equal;
  } knownTypes[] = {
    { QOF_QUERYCORE_STRING, string_match_predicate, string_compare_func,
      string_copy_predicate, string_free_pdata, string_to_string, 
      string_predicate_equal },
    { QOF_QUERYCORE_DATE, date_match_predicate, date_compare_func,
      date_copy_predicate, date_free_pdata, date_to_string,
      date_predicate_equal },
    { QOF_QUERYCORE_DEBCRED, numeric_match_predicate, numeric_compare_func,
      numeric_copy_predicate, numeric_free_pdata, debcred_to_string,
      numeric_predicate_equal },
    { QOF_QUERYCORE_NUMERIC, numeric_match_predicate, numeric_compare_func,
      numeric_copy_predicate, numeric_free_pdata, numeric_to_string,
      numeric_predicate_equal },
    { QOF_QUERYCORE_GUID, guid_match_predicate, NULL,
      guid_copy_predicate, guid_free_pdata, NULL,
      guid_predicate_equal },
    { QOF_QUERYCORE_INT32, int32_match_predicate, int32_compare_func,
      int32_copy_predicate, int32_free_pdata, int32_to_string,
      int32_predicate_equal },
    { QOF_QUERYCORE_INT64, int64_match_predicate, int64_compare_func,
      int64_copy_predicate, int64_free_pdata, int64_to_string,
      int64_predicate_equal },
    { QOF_QUERYCORE_DOUBLE, double_match_predicate, double_compare_func,
      double_copy_predicate, double_free_pdata, double_to_string,
      double_predicate_equal },
    { QOF_QUERYCORE_BOOLEAN, boolean_match_predicate, boolean_compare_func,
      boolean_copy_predicate, boolean_free_pdata, boolean_to_string,
      boolean_predicate_equal },
    { QOF_QUERYCORE_CHAR, char_match_predicate, char_compare_func,
      char_copy_predicate, char_free_pdata, char_to_string,
      char_predicate_equal },
    { QOF_QUERYCORE_KVP, kvp_match_predicate, NULL, kvp_copy_predicate,
      kvp_free_pdata, NULL, kvp_predicate_equal },
  };

  /* Register the known data types */
  for (i = 0; i < (sizeof(knownTypes)/sizeof(*knownTypes)); i++) {
    gncQueryRegisterCoreObject (knownTypes[i].name,
				knownTypes[i].pred,
				knownTypes[i].comp,
				knownTypes[i].copy,
				knownTypes[i].pd_free,
				knownTypes[i].toString,
				knownTypes[i].pred_equal);
  }
}

static QueryPredicateCopy gncQueryCoreGetCopy (char const *type)
{
  QueryPredicateCopy rc;
  g_return_val_if_fail (type, NULL);
  rc = g_hash_table_lookup (copyTable, type);
  return rc;
}

static QueryPredDataFree gncQueryCoreGetPredFree (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (freeTable, type);
}

static void gncQueryRegisterCoreObject (char const *core_name,
				 QueryPredicate pred,
				 QueryCompare comp,
				 QueryPredicateCopy copy,
				 QueryPredDataFree pd_free,
				 QueryToString toString,
				 QueryPredicateEqual pred_equal)
{
  g_return_if_fail (core_name);
  g_return_if_fail (*core_name != '\0');

  if (pred)
    g_hash_table_insert (predTable, (char *)core_name, pred);

  if (comp)
    g_hash_table_insert (cmpTable, (char *)core_name, comp);

  if (copy)
    g_hash_table_insert (copyTable, (char *)core_name, copy);

  if (pd_free)
    g_hash_table_insert (freeTable, (char *)core_name, pd_free);

  if (toString)
    g_hash_table_insert (toStringTable, (char *)core_name, toString);

  if (pred_equal)
    g_hash_table_insert (predEqualTable, (char *)core_name, pred_equal);
}

/********************************************************************/
/* PUBLISHED API FUNCTIONS */

void qof_query_core_init (void)
{
  /* Only let us initialize once */
  if (initialized) return;
  initialized = TRUE;

  /* Create the tables */
  predTable = g_hash_table_new (g_str_hash, g_str_equal);
  cmpTable = g_hash_table_new (g_str_hash, g_str_equal);
  copyTable = g_hash_table_new (g_str_hash, g_str_equal);
  freeTable = g_hash_table_new (g_str_hash, g_str_equal);
  toStringTable = g_hash_table_new (g_str_hash, g_str_equal);
  predEqualTable = g_hash_table_new (g_str_hash, g_str_equal);

  init_tables ();
}

void qof_query_core_shutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_destroy (predTable);
  g_hash_table_destroy (cmpTable);
  g_hash_table_destroy (copyTable);
  g_hash_table_destroy (freeTable);
  g_hash_table_destroy (toStringTable);
  g_hash_table_destroy (predEqualTable);
}

QueryPredicate qof_query_core_get_predicate (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (predTable, type);
}

QueryCompare qof_query_core_get_compare (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (cmpTable, type);
}

void qof_query_core_predicate_free (QofQueryPredData *pdata)
{
  QueryPredDataFree free_fcn;

  g_return_if_fail (pdata);
  g_return_if_fail (pdata->type_name);

  free_fcn = gncQueryCoreGetPredFree (pdata->type_name);
  free_fcn (pdata);
}

QofQueryPredData *
qof_query_core_predicate_copy (QofQueryPredData *pdata)
{
  QueryPredicateCopy copy;

  g_return_val_if_fail (pdata, NULL);
  g_return_val_if_fail (pdata->type_name, NULL);

  copy = gncQueryCoreGetCopy (pdata->type_name);
  return (copy (pdata));
}

char * qof_query_core_to_string (char const *type, gpointer object,
			     QofQueryAccess get)
{
  QueryToString toString;

  g_return_val_if_fail (type, NULL);
  g_return_val_if_fail (object, NULL);
  g_return_val_if_fail (get, NULL);

  toString = g_hash_table_lookup (toStringTable, type);
  g_return_val_if_fail (toString, NULL);

  return toString (object, get);
}

gboolean 
qof_query_core_predicate_equal (QofQueryPredData *p1, QofQueryPredData *p2)
{
  QueryPredicateEqual pred_equal;

  if (p1 == p2) return TRUE;
  if (!p1 || !p2) return FALSE;

  if (p1->how != p2->how) return FALSE;
  if (safe_strcmp (p1->type_name, p2->type_name)) return FALSE;

  pred_equal = g_hash_table_lookup (predEqualTable, p1->type_name);
  g_return_val_if_fail (pred_equal, FALSE);

  return pred_equal (p1, p2);
}

/*
 * QueryCore.c -- provide core Query data types
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
#include "QueryCoreP.h"
#include "QueryNew.h"

static short module = MOD_QUERY;

/* Core Type Predicate definitions */
typedef struct {
  QueryPredDataDef	pd;
  string_match_t	options;
  gboolean		is_regex;
  char *		matchstring;
  regex_t		compiled;
} query_string_def, *query_string_t;
typedef const char * (*query_string_getter) (gpointer);
static const char * query_string_type = QUERYCORE_STRING;

typedef struct {
  QueryPredDataDef	pd;
  date_match_t	options;
  Timespec	date;
} query_date_def, *query_date_t;
typedef Timespec (*query_date_getter) (gpointer);
static const char * query_date_type = QUERYCORE_DATE;

typedef struct {
  QueryPredDataDef	pd;
  numeric_match_t	options;
  gnc_numeric		amount;
} query_numeric_def, *query_numeric_t;
typedef gnc_numeric (*query_numeric_getter) (gpointer);
static const char * query_numeric_type = QUERYCORE_NUMERIC;

typedef struct {
  QueryPredDataDef	pd;
  guid_match_t	options;
  GList *	guids;
} query_guid_def, *query_guid_t;
typedef const GUID * (*query_guid_getter) (gpointer);
static const char * query_guid_type = QUERYCORE_GUID;

typedef struct {
  QueryPredDataDef	pd;
  gint64	val;
} query_int64_def, *query_int64_t;
typedef gint64 (*query_int64_getter) (gpointer);
static const char * query_int64_type = QUERYCORE_INT64;

typedef struct {
  QueryPredDataDef	pd;
  double	val;
} query_double_def, *query_double_t;
typedef double (*query_double_getter) (gpointer);
static const char * query_double_type = QUERYCORE_DOUBLE;

typedef struct {
  QueryPredDataDef	pd;
  gboolean	val;
} query_boolean_def, *query_boolean_t;
typedef gboolean (*query_boolean_getter) (gpointer);
static const char * query_boolean_type = QUERYCORE_BOOLEAN;

typedef struct {
  QueryPredDataDef	pd;
  char_match_t	options;
  char *	char_list;
} query_char_def, *query_char_t;
typedef char (*query_char_getter) (gpointer);
static const char * query_char_type = QUERYCORE_CHAR;

typedef struct {
  QueryPredDataDef	pd;
  GSList *	path;
  kvp_value *	value;
} query_kvp_def, *query_kvp_t;
typedef kvp_frame * (*query_kvp_getter) (gpointer);
static const char * query_kvp_type = QUERYCORE_KVP;

static gboolean initialized = FALSE;
static GHashTable *predTable = NULL;
static GHashTable *cmpTable = NULL;
static GHashTable *copyTable = NULL;
static GHashTable *freeTable = NULL;

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

/* QUERYCORE_STRING */

static int string_match_predicate (gpointer object, QueryAccess get_fcn,
				   QueryPredData_t pd)
{
  query_string_t pdata = (query_string_t) pd;
  const char *s;
  int ret = 0;

  VERIFY_PREDICATE (query_string_type);

  s = ((query_string_getter)get_fcn) (object);

  do {
    if (pdata->is_regex) {
      regmatch_t match;
      if (!regexec (&pdata->compiled, s, 1, &match, 0))
	ret = 1;

      break;
    }

    if (pdata->options == STRING_MATCH_CASEINSENSITIVE) {
      if (strcasestr (s, pdata->matchstring))
	ret = 1;

      break;
    }

    if (strstr (s, pdata->matchstring))
      ret = 1;

  } while (FALSE);

  switch (pd->how) {
  case COMPARE_EQUAL:
    return ret;
  case COMPARE_NEQ:
    return !ret;
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int string_compare_func (gpointer a, gpointer b, gint options,
				QueryAccess get_fcn)
{
  const char *s1, *s2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  s1 = ((query_string_getter)get_fcn) (a);
  s2 = ((query_string_getter)get_fcn) (b);

  if (options == STRING_MATCH_CASEINSENSITIVE)
    return safe_strcasecmp (s1, s2);

  return safe_strcmp (s1, s2);
}

static void string_free_pdata (QueryPredData_t pd)
{
  query_string_t pdata = (query_string_t) pd;

  VERIFY_PDATA (query_string_type);

  if (pdata->is_regex)
    regfree (&pdata->compiled);
  else
    g_free (pdata->matchstring);

  g_free (pdata);
}

static QueryPredData_t string_copy_predicate (QueryPredData_t pd)
{
  query_string_t pdata = (query_string_t) pd;

  VERIFY_PDATA_R (query_string_type);

  return gncQueryStringPredicate (pd->how, pdata->matchstring, pdata->options,
				  pdata->is_regex);
}

QueryPredData_t gncQueryStringPredicate (query_compare_t how,
					 char *str, string_match_t options,
					 gboolean is_regex)
{
  query_string_t pdata;

  g_return_val_if_fail (str, NULL);
  g_return_val_if_fail (*str != '\0', NULL);
  g_return_val_if_fail (how == COMPARE_EQUAL || how == COMPARE_NEQ, NULL);

  pdata = g_new0 (query_string_def, 1);
  pdata->pd.type_name = query_string_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->matchstring = g_strdup (str);

  if (is_regex) {
    int flags = REG_EXTENDED;
    if (options == STRING_MATCH_CASEINSENSITIVE)
      flags |= REG_ICASE;

    regcomp(&pdata->compiled, str, flags);
    pdata->is_regex = TRUE;
  }

  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_DATE */

static int date_compare (Timespec ta, Timespec tb, date_match_t options)
{
  if (options == DATE_MATCH_ROUNDED) {
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

static int date_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  query_date_t pdata = (query_date_t)pd;
  Timespec objtime;
  int compare;

  VERIFY_PREDICATE (query_date_type);

  objtime = ((query_date_getter)get_fcn) (object);
  compare = date_compare (objtime, pdata->date, pdata->options);

  switch (pd->how) {
  case COMPARE_LT:
    return (compare < 0);
  case COMPARE_LTE:
    return (compare <= 0);
  case COMPARE_EQUAL:
    return (compare == 0);
  case COMPARE_GT:
    return (compare > 0);
  case COMPARE_GTE:
    return (compare >= 0);
  case COMPARE_NEQ:
    return (compare != 0);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int date_compare_func (gpointer a, gpointer b, gint options,
			      QueryAccess get_fcn)
{
  Timespec ta, tb;

  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  ta = ((query_date_getter)get_fcn) (a);
  tb = ((query_date_getter)get_fcn) (b);

  return date_compare (ta, tb, options);
}

static void date_free_pdata (QueryPredData_t pd)
{
  query_date_t pdata = (query_date_t)pd;

  VERIFY_PDATA (query_date_type);

  g_free (pdata);
}

static QueryPredData_t date_copy_predicate (QueryPredData_t pd)
{
  query_date_t pdata = (query_date_t)pd;

  VERIFY_PDATA_R (query_date_type);

  return gncQueryDatePredicate (pd->how, pdata->options, pdata->date);
}

QueryPredData_t gncQueryDatePredicate (query_compare_t how,
				       date_match_t options, Timespec date)
{
  query_date_t pdata;

  pdata = g_new0 (query_date_def, 1);
  pdata->pd.type_name = query_date_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->date = date;
  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_NUMERIC */

static int numeric_match_predicate (gpointer object, QueryAccess get_fcn,
				    QueryPredData_t pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  gnc_numeric obj_val;
  int compare;

  VERIFY_PREDICATE (query_numeric_type);

  obj_val = ((query_numeric_getter)get_fcn) (object);

  switch (pdata->options) {
  case NUMERIC_MATCH_CREDIT:
    if (gnc_numeric_positive_p (obj_val)) return 0;
    break;
  case NUMERIC_MATCH_DEBIT:
    if (gnc_numeric_negative_p (obj_val)) return 0;
    break;
  default:
    break;
  }

  if (pd->how == COMPARE_EQUAL) {
    gnc_numeric cmp_val = gnc_numeric_create (1, 10000);
    compare =
      (gnc_numeric_compare (gnc_numeric_sub (gnc_numeric_abs (obj_val),
					     gnc_numeric_abs (pdata->amount),
					     100000, GNC_RND_ROUND),
			    cmp_val) < 0);
  } else
    compare = gnc_numeric_compare (gnc_numeric_abs (obj_val), pdata->amount);

  switch (pd->how) {
  case COMPARE_LT:
    return (compare < 0);
  case COMPARE_LTE:
    return (compare <= 0);
  case COMPARE_EQUAL:
    return compare;
  case COMPARE_GT:
    return (compare > 0);
  case COMPARE_GTE:
    return (compare >= 0);
  case COMPARE_NEQ:
    return (!compare);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int numeric_compare_func (gpointer a, gpointer b, gint options,
				 QueryAccess get_fcn)
{
  gnc_numeric va, vb;

  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  va = ((query_numeric_getter)get_fcn) (a);
  vb = ((query_numeric_getter)get_fcn) (b);

  return gnc_numeric_compare (va, vb);  
}

static void numeric_free_pdata (QueryPredData_t pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  VERIFY_PDATA (query_numeric_type);
  g_free (pdata);
}

static QueryPredData_t numeric_copy_predicate (QueryPredData_t pd)
{
  query_numeric_t pdata = (query_numeric_t)pd;
  VERIFY_PDATA_R (query_numeric_type);
  return gncQueryNumericPredicate (pd->how, pdata->options, pdata->amount);
}

QueryPredData_t gncQueryNumericPredicate (query_compare_t how,
					  numeric_match_t options,
					  gnc_numeric value)
{
  query_numeric_t pdata;
  pdata = g_new0 (query_numeric_def, 1);
  pdata->pd.type_name = query_numeric_type;
  pdata->pd.how = how;
  pdata->options = options;
  pdata->amount = value;
  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_GUID */

static int guid_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  GList *node;
  const GUID *guid = NULL;

  VERIFY_PREDICATE (query_guid_type);

  /* object is a GList of objects; get_gcn must be called on each one.
   * See if every guid in the predicate is accounted-for in the
   * object list
   */
  if (pdata->options == GUID_MATCH_ALL) {
    for (node = pdata->guids; node; node = node->next) {
      GList *o_list;

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

  } else {			/* ! MATCH_ALL */

    /* See if the guid is in the list */
    guid = ((query_guid_getter)get_fcn) (object);
    for (node = pdata->guids; node; node = node->next) {
      if (guid_equal (node->data, guid))
	break;
    }
  }

  switch (pdata->options) {
  case GUID_MATCH_ANY:
    return (node != NULL);
    break;
  case GUID_MATCH_NONE:
    return (node == NULL);
    break;
  case GUID_MATCH_NULL:
    return (guid == NULL);
    break;
  case GUID_MATCH_ALL:
    return (node == NULL);
    break;
  default:
    PWARN ("bad match type");
    return 0;
  }
}

static void guid_free_pdata (QueryPredData_t pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  VERIFY_PDATA (query_guid_type);
  g_list_free (pdata->guids);
  g_free (pdata);
}

static QueryPredData_t guid_copy_predicate (QueryPredData_t pd)
{
  query_guid_t pdata = (query_guid_t)pd;
  VERIFY_PDATA_R (query_guid_type);
  return gncQueryGUIDPredicate (pdata->options, pdata->guids);
}

QueryPredData_t gncQueryGUIDPredicate (guid_match_t options, GList *guids)
{
  query_guid_t pdata;

  pdata = g_new0 (query_guid_def, 1);
  pdata->pd.type_name = query_guid_type;
  pdata->options = options;
  pdata->guids = g_list_copy (guids);
  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_INT64 */

static int int64_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  gint64 val;
  query_int64_t pdata = (query_int64_t)pd;

  VERIFY_PREDICATE (query_int64_type);

  val = ((query_int64_getter)get_fcn) (object);

  switch (pd->how) {
  case COMPARE_LT:
    return (val < pdata->val);
  case COMPARE_LTE:
    return (val <= pdata->val);
  case COMPARE_EQUAL:
    return (val == pdata->val);
  case COMPARE_GT:
    return (val > pdata->val);
  case COMPARE_GTE:
    return (val >= pdata->val);
  case COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int int64_compare_func (gpointer a, gpointer b, gint options,
			      QueryAccess get_fcn)
{
  gint64 v1, v2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  v1 = ((query_int64_getter)get_fcn)(a);
  v2 = ((query_int64_getter)get_fcn)(b);

  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}

static void int64_free_pdata (QueryPredData_t pd)
{
  query_int64_t pdata = (query_int64_t)pd;
  VERIFY_PDATA (query_int64_type);
  g_free (pdata);
}

static QueryPredData_t int64_copy_predicate (QueryPredData_t pd)
{
  query_int64_t pdata = (query_int64_t)pd;
  VERIFY_PDATA_R (query_int64_type);
  return gncQueryInt64Predicate (pd->how, pdata->val);
}

QueryPredData_t gncQueryInt64Predicate (query_compare_t how, gint64 val)
{
  query_int64_t pdata = g_new0 (query_int64_def, 1);
  pdata->pd.type_name = query_int64_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_DOUBLE */

static int double_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  double val;
  query_double_t pdata = (query_double_t)pd;

  VERIFY_PREDICATE (query_double_type);

  val = ((query_double_getter)get_fcn) (object);

  switch (pd->how) {
  case COMPARE_LT:
    return (val < pdata->val);
  case COMPARE_LTE:
    return (val <= pdata->val);
  case COMPARE_EQUAL:
    return (val == pdata->val);
  case COMPARE_GT:
    return (val > pdata->val);
  case COMPARE_GTE:
    return (val >= pdata->val);
  case COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int double_compare_func (gpointer a, gpointer b, gint options,
			      QueryAccess get_fcn)
{
  double v1, v2;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);

  v1 = ((query_double_getter)get_fcn) (a);
  v2 = ((query_double_getter)get_fcn) (b);

  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}

static void double_free_pdata (QueryPredData_t pd)
{
  query_double_t pdata = (query_double_t)pd;
  VERIFY_PDATA (query_double_type);
  g_free (pdata);
}

static QueryPredData_t double_copy_predicate (QueryPredData_t pd)
{
  query_double_t pdata = (query_double_t)pd;
  VERIFY_PDATA_R (query_double_type);
  return gncQueryDoublePredicate (pd->how, pdata->val);
}

QueryPredData_t gncQueryDoublePredicate (query_compare_t how, double val)
{
  query_double_t pdata = g_new0 (query_double_def, 1);
  pdata->pd.type_name = query_double_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QueryPredData_t)pdata);
}


/* QUERYCORE_BOOLEAN */

static int boolean_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  gboolean val;
  query_boolean_t pdata = (query_boolean_t)pd;

  VERIFY_PREDICATE (query_boolean_type);

  val = ((query_boolean_getter)get_fcn) (object);

  switch (pd->how) {
  case COMPARE_EQUAL:
    return (val == pdata->val);
  case  COMPARE_NEQ:
    return (val != pdata->val);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static int boolean_compare_func (gpointer a, gpointer b, gint options,
				 QueryAccess get_fcn)
{
  gboolean va, vb;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);
  va = ((query_boolean_getter)get_fcn) (a);
  vb = ((query_boolean_getter)get_fcn) (b);
  if (!va && vb) return -1;
  if (va && !vb) return 1;
  return 0;
}

static void boolean_free_pdata (QueryPredData_t pd)
{
  query_boolean_t pdata = (query_boolean_t)pd;
  VERIFY_PDATA (query_boolean_type);
  g_free (pdata);
}

static QueryPredData_t boolean_copy_predicate (QueryPredData_t pd)
{
  query_boolean_t pdata = (query_boolean_t)pd;
  VERIFY_PDATA_R (query_boolean_type);
  return gncQueryBooleanPredicate (pd->how, pdata->val);
}

QueryPredData_t gncQueryBooleanPredicate (query_compare_t how, gboolean val)
{
  query_boolean_t pdata;
  g_return_val_if_fail (how == COMPARE_EQUAL || how == COMPARE_NEQ, NULL);

  pdata = g_new0 (query_boolean_def, 1);
  pdata->pd.type_name = query_boolean_type;
  pdata->pd.how = how;
  pdata->val = val;
  return ((QueryPredData_t)pdata);
}

/* QUERYCORE_CHAR */

static int char_match_predicate (gpointer object, QueryAccess get_fcn,
				 QueryPredData_t pd)
{
  char c;
  query_char_t pdata = (query_char_t)pd;

  VERIFY_PREDICATE (query_char_type);

  c = ((query_char_getter)get_fcn) (object);

  switch (pdata->options) {
  case CHAR_MATCH_ANY:
    if (strchr (pdata->char_list, c)) return 1;
    return 0;
  case CHAR_MATCH_NONE:
    if (!strchr (pdata->char_list, c)) return 1;
    return 0;
  default:
    PWARN ("bad match type");
    return 0;
  }
}

static int char_compare_func (gpointer a, gpointer b, gint options,
			      QueryAccess get_fcn)
{
  char va, vb;
  g_return_val_if_fail (a && b && get_fcn, COMPARE_ERROR);
  va = ((query_char_getter)get_fcn)(a);
  vb = ((query_char_getter)get_fcn)(b);
  return (va-vb);
}

static void char_free_pdata (QueryPredData_t pd)
{
  query_char_t pdata = (query_char_t)pd;
  VERIFY_PDATA (query_char_type);
  g_free (pdata->char_list);
  g_free (pdata);  
}

static QueryPredData_t char_copy_predicate (QueryPredData_t pd)
{
  query_char_t pdata = (query_char_t)pd;
  VERIFY_PDATA_R (query_char_type);
  return gncQueryCharPredicate (pdata->options, pdata->char_list);
}

QueryPredData_t gncQueryCharPredicate (char_match_t options, const char *chars)
{
  query_char_t pdata;
  g_return_val_if_fail (chars, NULL);
  pdata = g_new0 (query_char_def, 1);
  pdata->pd.type_name = query_char_type;
  pdata->options = options;
  pdata->char_list = g_strdup (chars);
  return ((QueryPredData_t)pdata);
}


/* QUERYCORE_KVP */

static int kvp_match_predicate (gpointer object, QueryAccess get_fcn,
				QueryPredData_t pd)
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
  case COMPARE_LT:
    return (compare < 0);
  case COMPARE_LTE:
    return (compare <= 0);
  case COMPARE_EQUAL:
    return (compare == 0);
  case COMPARE_GTE:
    return (compare >= 0);
  case COMPARE_GT:
    return (compare > 0);
  case COMPARE_NEQ:
    return (compare != 0);
  default:
    PWARN ("bad match type: %d", pd->how);
    return 0;
  }
}

static void kvp_free_pdata (QueryPredData_t pd)
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

static QueryPredData_t kvp_copy_predicate (QueryPredData_t pd)
{
  query_kvp_t pdata = (query_kvp_t)pd;
  VERIFY_PDATA_R (query_kvp_type);
  return gncQueryKVPPredicate (pd->how, pdata->path, pdata->value);
}

QueryPredData_t gncQueryKVPPredicate (query_compare_t how,
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

  return ((QueryPredData_t)pdata);
}

/* initialization */

static void init_tables (void)
{
  int i;
  struct {
    char const *name;
    QueryPredicate pred;
    QueryCompare comp;
    QueryPredicateCopy copy;
    QueryPredDataFree pd_free;
  } knownTypes[] = {
    { QUERYCORE_STRING, string_match_predicate, string_compare_func,
      string_copy_predicate, string_free_pdata },
    { QUERYCORE_DATE, date_match_predicate, date_compare_func,
      date_copy_predicate, date_free_pdata },
    { QUERYCORE_DEBCRED, numeric_match_predicate, numeric_compare_func,
      numeric_copy_predicate, numeric_free_pdata },
    { QUERYCORE_NUMERIC, numeric_match_predicate, numeric_compare_func,
      numeric_copy_predicate, numeric_free_pdata },
    { QUERYCORE_GUID, guid_match_predicate, NULL,
      guid_copy_predicate, guid_free_pdata },
    { QUERYCORE_INT64, int64_match_predicate, int64_compare_func,
      int64_copy_predicate, int64_free_pdata },
    { QUERYCORE_DOUBLE, double_match_predicate, double_compare_func,
      double_copy_predicate, double_free_pdata },
    { QUERYCORE_BOOLEAN, boolean_match_predicate, boolean_compare_func,
      boolean_copy_predicate, boolean_free_pdata },
    { QUERYCORE_CHAR, char_match_predicate, char_compare_func,
      char_copy_predicate, char_free_pdata },
    { QUERYCORE_KVP, kvp_match_predicate, NULL, kvp_copy_predicate,
      kvp_free_pdata },
  };

  /* Register the known data types */
  for (i = 0; i < (sizeof(knownTypes)/sizeof(*knownTypes)); i++) {
    gncQueryRegisterCoreObject (knownTypes[i].name,
				knownTypes[i].pred,
				knownTypes[i].comp,
				knownTypes[i].copy,
				knownTypes[i].pd_free);
  }
}

/********************************************************************/
/* PUBLISHED API FUNCTIONS */

void gncQueryRegisterCoreObject (char const *core_name,
				 QueryPredicate pred,
				 QueryCompare comp,
				 QueryPredicateCopy copy,
				 QueryPredDataFree pd_free)
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
}

void gncQueryCoreInit (void)
{
  /* Only let us initialize once */
  if (initialized) return;
  initialized = TRUE;

  /* Create the tables */
  predTable = g_hash_table_new (g_str_hash, g_str_equal);
  cmpTable = g_hash_table_new (g_str_hash, g_str_equal);
  copyTable = g_hash_table_new (g_str_hash, g_str_equal);
  freeTable = g_hash_table_new (g_str_hash, g_str_equal);

  init_tables ();
}

void gncQueryCoreShutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_destroy (predTable);
  g_hash_table_destroy (cmpTable);
  g_hash_table_destroy (copyTable);
  g_hash_table_destroy (freeTable);
}

QueryPredicate gncQueryCoreGetPredicate (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (predTable, type);
}

QueryCompare gncQueryCoreGetCompare (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (cmpTable, type);
}

QueryPredicateCopy gncQueryCoreGetCopy (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (copyTable, type);
}

QueryPredDataFree gncQueryCoreGetPredFree (char const *type)
{
  g_return_val_if_fail (type, NULL);
  return g_hash_table_lookup (freeTable, type);
}

void gncQueryCorePredicateFree (QueryPredData_t pdata)
{
  QueryPredDataFree free_fcn;

  g_return_if_fail (pdata);
  g_return_if_fail (pdata->type_name);

  free_fcn = gncQueryCoreGetPredFree (pdata->type_name);
  free_fcn (pdata);
}

QueryPredData_t gncQueryCorePredicateCopy (QueryPredData_t pdata)
{
  QueryPredicateCopy copy;

  g_return_val_if_fail (pdata, NULL);
  g_return_val_if_fail (pdata->type_name, NULL);

  copy = gncQueryCoreGetCopy (pdata->type_name);
  return (copy (pdata));
}

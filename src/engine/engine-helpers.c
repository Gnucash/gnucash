/********************************************************************\
 * engine-helpers.c -- gnucash g-wrap helper functions              *
 * Copyright (C) 2000 Linas Vepstas                                 *
 * Copyright (C) 2001 Linux Developers Group, Inc.                  *
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

#include <g-wrap-wct.h>
#include <libguile.h>
#include <string.h>

#include "Account.h"
#include "Backend.h"
#include "Group.h"
#include "QueryNewP.h"
#include "QueryCoreP.h"
#include "date.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-numeric.h"

static short module = MOD_ENGINE;

Timespec
gnc_transaction_get_date_posted(Transaction *t) 
{
  Timespec result;
  xaccTransGetDatePostedTS(t, &result);
  return(result);
}

Timespec
gnc_transaction_get_date_entered(Transaction *t) 
{
  Timespec result;
  xaccTransGetDateEnteredTS(t, &result);
  return(result);
}

Timespec
gnc_split_get_date_reconciled(Split *s) 
{
  Timespec result;
  xaccSplitGetDateReconciledTS(s, &result);
  return(result);
}

void
gnc_transaction_set_date_posted(Transaction *t, const Timespec d) 
{
  xaccTransSetDatePostedTS(t, &d);
}

void
gnc_transaction_set_date_entered(Transaction *t, const Timespec d) 
{
  xaccTransSetDateEnteredTS(t, &d);
}

void
gnc_transaction_set_date(Transaction *t, Timespec ts)
{
  xaccTransSetDatePostedTS(t, &ts);
}

SCM
gnc_timespec2timepair(Timespec t)
{
  SCM secs;
  SCM nsecs;

  secs = gnc_gint64_to_scm(t.tv_sec);
  nsecs = gh_long2scm(t.tv_nsec);
  return(gh_cons(secs, nsecs));
}

Timespec
gnc_timepair2timespec(SCM x)
{
  Timespec result = {0,0};
  if (gnc_timepair_p (x))
  {
    result.tv_sec = gnc_scm_to_gint64(gh_car(x));
    result.tv_nsec = gh_scm2long(gh_cdr(x));
  }
  return(result);
}

int
gnc_timepair_p(SCM x)
{
  return(gh_pair_p(x) &&
         gnc_gh_gint64_p(gh_car(x)) &&
         gnc_gh_gint64_p(gh_cdr(x)));
}

SCM
gnc_guid2scm(GUID guid)
{
  char string[GUID_ENCODING_LENGTH + 1];

  if (!guid_to_string_buff(&guid, string))
    return SCM_UNDEFINED;

  return gh_str02scm(string);
}

GUID
gnc_scm2guid(SCM guid_scm) {
  char string[GUID_ENCODING_LENGTH + 1];
  GUID guid;

  gh_get_substr(guid_scm, string, 0, GUID_ENCODING_LENGTH);
  string[GUID_ENCODING_LENGTH] = '\0';

  string_to_guid(string, &guid);

  return guid;
}

int
gnc_guid_p(SCM guid_scm) {
  char string[GUID_ENCODING_LENGTH + 1];
  GUID guid;

  if (!gh_string_p(guid_scm))
    return FALSE;

  gh_get_substr(guid_scm, string, 0, GUID_ENCODING_LENGTH);
  string[GUID_ENCODING_LENGTH] = '\0';

  return string_to_guid(string, &guid);
}


/********************************************************************
 * type converters for query API  
 ********************************************************************/

/* The query scm representation is a list of pairs, where the
 * car of each pair is one of the following symbols:
 *
 *   Symbol                cdr
 *   'terms                list of OR terms
 *   'primary-sort         scm rep of sort_type_t
 *   'secondary-sort       scm rep of sort_type_t
 *   'tertiary-sort        scm rep of sort_type_t
 *   'primary-increasing   boolean
 *   'secondary-increasing boolean
 *   'tertiary-increasing  boolean
 *   'max-splits           integer
 *
 *   Each OR term is a list of AND terms.
 *   Each AND term is a list of one of the following forms:
 *
 *   ('pd-date pr-type sense-bool use-start-bool start-timepair
 *             use-end-bool use-end-timepair)
 *   ('pd-amount pr-type sense-bool amt-match-how amt-match-sign amount)
 *   ('pd-account pr-type sense-bool acct-match-how list-of-account-guids)
 *   ('pd-string pr-type sense-bool case-sense-bool use-regexp-bool string)
 *   ('pd-cleared pr-type sense-bool cleared-field)
 *   ('pd-balance pr-type sense-bool balance-field)
 */

typedef enum {
  gnc_QUERY_v1 = 1,
  gnc_QUERY_v2
} query_version_t;

static SCM
gnc_gw_enum_val2scm (const char *typestr, int value)
{
  char *func_name;
  SCM func;
  SCM scm;

  func_name = g_strdup_printf ("gw:enum-%s-val->sym", typestr);

  func = gh_eval_str (func_name);
  if (gh_procedure_p (func))
    scm = gh_call2 (func, gh_int2scm (value), gh_bool2scm (FALSE));
  else
    scm = SCM_BOOL_F;

  g_free (func_name);

  return scm;
}

static int
gnc_gw_enum_scm2val (const char *typestr, SCM enum_scm)
{
  char *func_name;
  SCM func;
  SCM scm;

  func_name = g_strdup_printf ("gw:enum-%s-val->int", typestr);

  func = gh_eval_str (func_name);
  if (gh_procedure_p (func))
    scm = gh_call1 (func, enum_scm);
  else
    scm = gh_int2scm (0);

  g_free (func_name);

  return gh_scm2int (scm);
}

/* query_compare_t */

static SCM
gnc_query_compare2scm (query_compare_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:query-compare-how>", how);
}

static query_compare_t
gnc_query_scm2compare (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:query-compare-how>", how_scm);
}

/* string_match_t */
static SCM
gnc_query_string2scm (string_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:string-match-how>", how);
}

static string_match_t
gnc_query_scm2string (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:string-match-how>", how_scm);
}

/* date_match_t */
static SCM
gnc_query_date2scm (date_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:date-match-how>", how);
}

static date_match_t
gnc_query_scm2date (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:date-match-how>", how_scm);
}

/* numeric_match_t */
static SCM
gnc_query_numericop2scm (numeric_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:numeric-match-how>", how);
}

static numeric_match_t
gnc_query_scm2numericop (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:numeric-match-how>", how_scm);
}

/* guid_match_t */
static SCM
gnc_query_guid2scm (guid_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:guid-match-how>", how);
}

static guid_match_t
gnc_query_scm2guid (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:guid-match-how>", how_scm);
}

/* char_match_t */
static SCM
gnc_query_char2scm (char_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:char-match-how>", how);
}

static char_match_t
gnc_query_scm2char (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:char-match-how>", how_scm);
}

static guid_match_t
gnc_scm2acct_match_how (SCM how_scm)
{
  guid_match_t res;
  char *how = gh_symbol2newstr (how_scm, NULL);

  if (!safe_strcmp (how, "acct-match-all"))
    res = GUID_MATCH_ALL;
  else if (!safe_strcmp (how, "acct-match-any"))
    res = GUID_MATCH_ANY;
  else if (!safe_strcmp (how, "acct-match-none"))
    res = GUID_MATCH_NONE;
  else {
    PINFO ("invalid account match: %s", how);
    res = GUID_MATCH_NULL;
  }

  if (how) free (how);
  return res;
}

static query_compare_t
gnc_scm2amt_match_how (SCM how_scm)
{
  query_compare_t res;
  char *how = gh_symbol2newstr (how_scm, NULL);

  if (!safe_strcmp (how, "amt-match-atleast"))
    res = COMPARE_GTE;
  else if (!safe_strcmp (how, "amt-match-atmost"))
    res = COMPARE_LTE;
  else if (!safe_strcmp (how, "amt-match-exactly"))
    res = COMPARE_EQUAL;
  else {
    PINFO ("invalid amount match: %s", how);
    res = COMPARE_EQUAL;
  }

  if (how) free (how);
  return res;
}

static query_compare_t
gnc_scm2kvp_match_how (SCM how_scm)
{
  query_compare_t res;
  char *how = gh_symbol2newstr (how_scm, NULL);

  if (!safe_strcmp (how, "kvp-match-lt"))
    res = COMPARE_LT;
  else if (!safe_strcmp (how, "kvp-match-lte"))
    res = COMPARE_LTE;
  else if (!safe_strcmp (how, "kvp-match-eq"))
    res = COMPARE_EQUAL;
  else if (!safe_strcmp (how, "kvp-match-gte"))
    res = COMPARE_GTE;
  else if (!safe_strcmp (how, "kvp-match-gt"))
    res = COMPARE_GT;
  else {
    PINFO ("invalid kvp match: %s", how);
    res = COMPARE_EQUAL;
  }

  if (how) free (how);
  return res;
}

static int
gnc_scm2bitfield (const char *typestr, SCM field_scm)
{
  int field = 0;

  if (!gh_list_p (field_scm))
    return 0;

  while (!gh_null_p (field_scm))
  {
    SCM scm;
    int bit;

    scm = gh_car (field_scm);
    field_scm = gh_cdr (field_scm);

    bit = gnc_gw_enum_scm2val (typestr, scm);
    field |= bit;
  }

  return field;
}

static cleared_match_t
gnc_scm2cleared_match_how (SCM how_scm)
{
  return gnc_scm2bitfield ("<gnc:cleared-match-how>", how_scm);
}

static gboolean
gnc_scm2balance_match_how (SCM how_scm, gboolean *resp)
{
  char *how;

  if (!gh_list_p (how_scm))
    return FALSE;

  if (gh_null_p (how_scm))
    return FALSE;

  /* Only allow a single-entry list */
  if (!gh_null_p (gh_cdr (how_scm)))
    return FALSE;

  how = gh_symbol2newstr (gh_car (how_scm), NULL);

  if (!safe_strcmp (how, "balance-match-balanced"))
    *resp = TRUE;
  else
    *resp = FALSE;

  if (how) free (how);

  return TRUE;
}

static GNCIdType
gnc_scm2kvp_match_where (SCM where_scm)
{
  GNCIdType res;
  char *where;

  if (!gh_list_p (where_scm))
    return NULL;

  where = gh_symbol2newstr (gh_car (where_scm), NULL);

  if (!safe_strcmp (where, "kvp-match-split"))
    res = GNC_ID_SPLIT;
  else if (!safe_strcmp (where, "kvp-match-trans"))
    res = GNC_ID_TRANS;
  else if (!safe_strcmp (where, "kvp-match-account"))
    res = GNC_ID_ACCOUNT;
  else {
    PINFO ("Unknown kvp-match-where: %s", where);
    res = NULL;
  }

  if (where) free (where);
  return res;
}

static SCM
gnc_guid_glist2scm (GList *account_guids)
{
  SCM guids = SCM_EOL;
  GList *node;

  for (node = account_guids; node; node = node->next)
  {
    GUID *guid = node->data;

    if (guid)
      guids = gh_cons (gnc_guid2scm (*guid), guids);
  }

  return gh_reverse (guids);
}

static GList *
gnc_scm2guid_glist (SCM guids_scm)
{
  GList *guids = NULL;

  if (!gh_list_p (guids_scm))
    return NULL;

  while (!gh_null_p (guids_scm))
  {
    SCM guid_scm = gh_car (guids_scm);
    GUID *guid;

    guid = xaccGUIDMalloc ();
    *guid = gnc_scm2guid (guid_scm);

    guids = g_list_prepend (guids, guid);

    guids_scm = gh_cdr (guids_scm);
  }

  return g_list_reverse (guids);
}

static void
gnc_guid_glist_free (GList *guids)
{
  GList *node;

  for (node = guids; node; node = node->next)
    xaccGUIDFree (node->data);

  g_list_free (guids);
}

static SCM
gnc_query_numeric2scm (gnc_numeric val)
{
  return gh_cons (gnc_gint64_to_scm (val.num),
		  gnc_gint64_to_scm (val.denom));
}

static gboolean
gnc_query_numeric_p (SCM pair)
{
  return (gh_pair_p (pair));
}

static gnc_numeric
gnc_query_scm2numeric (SCM pair)
{
  SCM denom;
  SCM num;

  num = gh_car (pair);
  denom = gh_cdr (pair);

  return gnc_numeric_create (gnc_scm_to_gint64 (num),
			     gnc_scm_to_gint64 (denom));
}

static SCM
gnc_query_path2scm (GSList *path)
{
  SCM path_scm = SCM_EOL;
  GSList *node;

  for (node = path; node; node = node->next)
  {
    const char *key = node->data;

    if (key) /* FIXME: remove cast */
      path_scm = gh_cons (gh_str02scm ((char *) key), path_scm);
  }

  return gh_reverse (path_scm);
}

static GSList *
gnc_query_scm2path (SCM path_scm)
{
  GSList *path = NULL;

  if (!gh_list_p (path_scm))
    return NULL;

  while (!gh_null_p (path_scm))
  {
    SCM key_scm = gh_car (path_scm);
    char *key, *tmp;

    if (!gh_string_p (key_scm))
      break;

    tmp = gh_scm2newstr (key_scm, NULL);
    key = g_strdup (tmp);
    if (tmp) free (tmp);

    path = g_slist_prepend (path, key);

    path_scm = gh_cdr (path_scm);
  }

  return g_slist_reverse (path);
}

static void
gnc_query_path_free (GSList *path)
{
  GSList *node;

  for (node = path; node; node = node->next)
    g_free (node->data);

  g_slist_free (path);
}

static SCM
gnc_kvp_value_type2scm (kvp_value_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:kvp-value-t>", how);
}

static kvp_value_t
gnc_scm2kvp_value_type (SCM value_type_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:kvp-value-t>", value_type_scm);
}

static SCM gnc_kvp_frame2scm (kvp_frame *frame);

static SCM
gnc_kvp_value2scm (kvp_value *value)
{
  SCM value_scm = SCM_EOL;
  kvp_value_t value_t;
  SCM scm;

  if (!value) return SCM_BOOL_F;

  value_t = kvp_value_get_type (value);

  value_scm = gh_cons (gnc_kvp_value_type2scm (value_t), value_scm);

  switch (value_t)
  {
    case KVP_TYPE_GINT64:
      scm = gnc_gint64_to_scm (kvp_value_get_gint64 (value));
      break;

    case KVP_TYPE_DOUBLE:
      scm = gh_double2scm (kvp_value_get_double (value));
      break;

    case KVP_TYPE_STRING:
      scm = gh_str02scm (kvp_value_get_string (value));
      break;

    case KVP_TYPE_GUID:
      scm = gnc_guid2scm (*kvp_value_get_guid (value));
      break;

    case KVP_TYPE_TIMESPEC:
      scm = gnc_timespec2timepair (kvp_value_get_timespec (value));
      break;

    case KVP_TYPE_BINARY:
      scm = SCM_BOOL_F;
      break;

    case KVP_TYPE_NUMERIC: {
      gnc_numeric n = kvp_value_get_numeric (value);
      scm = gnc_query_numeric2scm (n);
      break;
    }

    case KVP_TYPE_GLIST: {
      GList *node;

      scm = SCM_EOL;
      for (node = kvp_value_get_glist (value); node; node = node->next)
        scm = gh_cons (gnc_kvp_value2scm (node->data), scm);
      scm = gh_reverse (scm);
      break;
    }

    case KVP_TYPE_FRAME:
      scm = gnc_kvp_frame2scm (kvp_value_get_frame (value));
      break;

    default:
      scm = SCM_BOOL_F;
      break;
  }

  value_scm = gh_cons (scm, value_scm);

  return gh_reverse (value_scm);
}

typedef struct
{
  SCM scm;
} KVPSCMData;

static void
kvp_frame_slot2scm (const char *key, kvp_value *value, gpointer data)
{
  KVPSCMData *ksd = data;
  SCM value_scm;
  SCM key_scm;
  SCM pair;

  /* FIXME: remove cast */
  key_scm = gh_str02scm ((char *) key);
  value_scm = gnc_kvp_value2scm (value);
  pair = gh_cons (key_scm, value_scm);

  ksd->scm = gh_cons (pair, ksd->scm);
}

static SCM
gnc_kvp_frame2scm (kvp_frame *frame)
{
  KVPSCMData ksd;

  if (!frame) return SCM_BOOL_F;

  ksd.scm = SCM_EOL;

  kvp_frame_for_each_slot (frame, kvp_frame_slot2scm, &ksd);

  return ksd.scm;
}

static kvp_frame * gnc_scm2kvp_frame (SCM frame_scm);

static kvp_value *
gnc_scm2kvp_value (SCM value_scm)
{
  kvp_value_t value_t;
  kvp_value *value;
  SCM type_scm;
  SCM val_scm;

  if (!gh_list_p (value_scm) || gh_null_p (value_scm))
    return NULL;

  type_scm = gh_car (value_scm);
  value_t = gnc_scm2kvp_value_type (type_scm);

  value_scm = gh_cdr (value_scm);
  if (!gh_list_p (value_scm) || gh_null_p (value_scm))
    return NULL;

  val_scm = gh_car (value_scm);

  switch (value_t)
  {
    case KVP_TYPE_GINT64:
      value = kvp_value_new_gint64 (gnc_scm_to_gint64 (val_scm));
      break;

    case KVP_TYPE_DOUBLE:
      value = kvp_value_new_double (gh_scm2double (val_scm));
      break;

    case KVP_TYPE_STRING: {
      char *str = gh_scm2newstr (val_scm, NULL);
      value = kvp_value_new_string (str);
      if (str) free (str);
      break;
    }

    case KVP_TYPE_GUID: {
      GUID guid = gnc_scm2guid (val_scm);
      value = kvp_value_new_guid (&guid);
      break;
    }

    case KVP_TYPE_TIMESPEC: {
      Timespec ts = gnc_timepair2timespec (val_scm);
      value = kvp_value_new_timespec(ts);
      break;
    }

    case KVP_TYPE_BINARY:
      return NULL;
      break;

    case KVP_TYPE_NUMERIC: {
      gnc_numeric n;

      if (!gnc_query_numeric_p (val_scm))
	return NULL;

      n = gnc_query_scm2numeric (val_scm);

      value = kvp_value_new_gnc_numeric (n);
      break;
    }

    case KVP_TYPE_GLIST: {
      GList *list = NULL;
      GList *node;

      for (; gh_list_p (val_scm) && !gh_null_p (val_scm);
           val_scm = gh_cdr (val_scm))
      {
        SCM scm = gh_car (val_scm);

        list = g_list_prepend (list, gnc_scm2kvp_value (scm));
      }

      list = g_list_reverse (list);

      value = kvp_value_new_glist (list);

      for (node = list; node; node = node->next)
        kvp_value_delete (node->data);
      g_list_free (list);
      break;
    }

    case KVP_TYPE_FRAME: {
      kvp_frame *frame;

      frame = gnc_scm2kvp_frame (val_scm);
      value = kvp_value_new_frame (frame);
      kvp_frame_delete (frame);
      break;
    }

    default:
      PWARN ("unexpected type: %d", value_t);
      return NULL;
  }

  return value;
}

static kvp_frame *
gnc_scm2kvp_frame (SCM frame_scm)
{
  kvp_frame * frame;

  if (!gh_list_p (frame_scm)) return NULL;

  frame = kvp_frame_new ();

  for (; gh_list_p (frame_scm) && !gh_null_p (frame_scm);
       frame_scm = gh_cdr (frame_scm))
  {
    SCM pair = gh_car (frame_scm);
    kvp_value *value;
    SCM key_scm;
    SCM val_scm;
    char *key;

    if (!gh_pair_p (pair))
      continue;

    key_scm = gh_car (pair);
    val_scm = gh_cdr (pair);

    if (!gh_string_p (key_scm))
      continue;

    key = gh_scm2newstr (key_scm, NULL);
    if (!key)
      continue;

    value = gnc_scm2kvp_value (val_scm);
    if (!value)
    {
      free (key);
      continue;
    }

    kvp_frame_set_slot_nc (frame, key, value);

    free (key);
  }

  return frame;
}

static SCM
gnc_queryterm2scm (QueryNewTerm_t qt)
{
  SCM qt_scm = SCM_EOL;
  QueryPredData_t pd = NULL;

  qt_scm = gh_cons (gnc_query_path2scm (gncQueryTermGetParamPath (qt)),
		    qt_scm);
  qt_scm = gh_cons (gh_bool2scm (gncQueryTermIsInverted (qt)), qt_scm);

  pd = gncQueryTermGetPredData (qt);
  qt_scm = gh_cons (gh_symbol2scm (pd->type_name), qt_scm);
  qt_scm = gh_cons (gnc_query_compare2scm (pd->how), qt_scm);

  if (!safe_strcmp (pd->type_name, QUERYCORE_STRING)) {
    query_string_t pdata = (query_string_t) pd;

    qt_scm = gh_cons (gnc_query_string2scm (pdata->options), qt_scm);
    qt_scm = gh_cons (gh_bool2scm (pdata->is_regex), qt_scm);
    qt_scm = gh_cons (gh_str02scm (pdata->matchstring), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_DATE)) {
    query_date_t pdata = (query_date_t) pd;

    qt_scm = gh_cons (gnc_query_date2scm (pdata->options), qt_scm);
    qt_scm = gh_cons (gnc_timespec2timepair (pdata->date), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_NUMERIC)) {
    query_numeric_t pdata = (query_numeric_t) pd;

    qt_scm = gh_cons (gnc_query_numericop2scm (pdata->options), qt_scm);
    qt_scm = gh_cons (gnc_query_numeric2scm (pdata->amount), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_GUID)) {
    query_guid_t pdata = (query_guid_t) pd;

    qt_scm = gh_cons (gnc_query_guid2scm (pdata->options), qt_scm);
    qt_scm = gh_cons (gnc_guid_glist2scm (pdata->guids), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_INT64)) {
    query_int64_t pdata = (query_int64_t) pd;

    qt_scm = gh_cons (gnc_gint64_to_scm (pdata->val), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_DOUBLE)) {
    query_double_t pdata = (query_double_t) pd;

    qt_scm = gh_cons (gh_double2scm (pdata->val), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_BOOLEAN)) {
    query_boolean_t pdata = (query_boolean_t) pd;

    qt_scm = gh_cons (gh_bool2scm (pdata->val), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_CHAR)) {
    query_char_t pdata = (query_char_t) pd;

    qt_scm = gh_cons (gnc_query_char2scm (pdata->options), qt_scm);
    qt_scm = gh_cons (gh_str02scm (pdata->char_list), qt_scm);

  } else if (!safe_strcmp (pd->type_name, QUERYCORE_KVP)) {
    query_kvp_t pdata = (query_kvp_t) pd;

    qt_scm = gh_cons (gnc_query_path2scm (pdata->path), qt_scm);
    qt_scm = gh_cons (gnc_kvp_value2scm (pdata->value), qt_scm);

  } else {
    PWARN ("query core type %s not supported", pd->type_name);
    return SCM_BOOL_F;
  }

  return gh_reverse (qt_scm);
}

static Query *
gnc_scm2query_term_query_v2 (SCM qt_scm)
{
  Query *q = NULL;
  QueryPredData_t pd = NULL;
  SCM scm;
  char *type = NULL;
  GSList *path = NULL;
  gboolean inverted = FALSE;
  query_compare_t compare_how;

  if (!gh_list_p (qt_scm) || gh_null_p (qt_scm))
    return NULL;

  do {
    /* param path */
    scm = gh_car (qt_scm);
    qt_scm = gh_cdr (qt_scm);
    if (!gh_list_p (scm))
      break;
    path = gnc_query_scm2path (scm);

    /* inverted */
    scm = gh_car (qt_scm);
    qt_scm = gh_cdr (qt_scm);
    if (!gh_boolean_p (scm))
      break;
    inverted = gh_scm2bool (scm);

    /* type */
    scm = gh_car (qt_scm);
    qt_scm = gh_cdr (qt_scm);
    if (!gh_symbol_p (scm))
      break;
    type = gh_symbol2newstr (scm, NULL);

    /* query_compare_t */
    scm = gh_car (qt_scm);
    qt_scm = gh_cdr (qt_scm);
    if (gh_null_p (scm))
      break;
    compare_how = gnc_query_scm2compare (scm);

    /* Now compute the predicate */

    if (!safe_strcmp (type, QUERYCORE_STRING)) {
      string_match_t options;
      gboolean is_regex;
      char *matchstring;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      options = gnc_query_scm2string (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_boolean_p (scm))
	break;
      is_regex = gh_scm2bool (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_string_p (scm))
	break;
      matchstring = gh_scm2newstr (scm, NULL);

      pd = gncQueryStringPredicate (compare_how, matchstring,
				    options, is_regex);
      free (matchstring);

    } else if (!safe_strcmp (type, QUERYCORE_DATE)) {
      date_match_t options;
      Timespec date;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      options = gnc_query_scm2date (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      date = gnc_timepair2timespec (scm);

      pd = gncQueryDatePredicate (compare_how, options, date);

    } else if (!safe_strcmp (type, QUERYCORE_NUMERIC)) {
      numeric_match_t options;
      gnc_numeric val;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      options = gnc_query_scm2numericop (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gnc_query_numeric_p (scm))
	break;
      val = gnc_query_scm2numeric (scm);

      pd = gncQueryNumericPredicate (compare_how, options, val);

    } else if (!safe_strcmp (type, QUERYCORE_GUID)) {
      guid_match_t options;
      GList *guids;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      options = gnc_query_scm2guid (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_list_p (scm))
	break;
      guids = gnc_scm2guid_glist (scm);

      pd = gncQueryGUIDPredicate (options, guids);

      gnc_guid_glist_free (guids);

    } else if (!safe_strcmp (type, QUERYCORE_INT64)) {
      gint64 val;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      val = gnc_scm_to_gint64 (scm);

      pd = gncQueryInt64Predicate (compare_how, val);

    } else if (!safe_strcmp (type, QUERYCORE_DOUBLE)) {
      double val;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_number_p (scm))
	break;
      val = gh_scm2double (scm);

      pd = gncQueryDoublePredicate (compare_how, val);

    } else if (!safe_strcmp (type, QUERYCORE_BOOLEAN)) {
      gboolean val;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_boolean_p (scm))
	break;
      val = gh_scm2bool (scm);

      pd = gncQueryBooleanPredicate (compare_how, val);

    } else if (!safe_strcmp (type, QUERYCORE_CHAR)) {
      char_match_t options;
      char *char_list;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm))
	break;
      options = gnc_query_scm2char (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_string_p (scm))
	break;
      char_list = gh_scm2newstr (scm, NULL);

      pd = gncQueryCharPredicate (options, char_list);
      free (char_list);

    } else if (!safe_strcmp (type, QUERYCORE_KVP)) {
      GSList *kvp_path;
      kvp_value *value;

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (!gh_list_p (scm))
	break;
      kvp_path = gnc_query_scm2path (scm);

      scm = gh_car (qt_scm);
      qt_scm = gh_cdr (qt_scm);
      if (gh_null_p (scm)) {
	gnc_query_path_free (kvp_path);
	break;
      }
      value = gnc_scm2kvp_value (scm);

      pd = gncQueryKVPPredicate (compare_how, kvp_path, value);
      gnc_query_path_free (kvp_path);
      kvp_value_delete (value);
      
    } else {
      PWARN ("query core type %s not supported", type);
      break;
    }

  } while (FALSE);

  if (pd) {
    q = gncQueryCreate ();
    gncQueryAddTerm (q, path, pd, QUERY_OR);
    if (inverted) {
      Query *outq = gncQueryInvert (q);
      gncQueryDestroy (q);
      q = outq;
    }
  } else {
    gnc_query_path_free (path);
  }

  if (type)
    free (type);

  return q;
}

static Query *
gnc_scm2query_term_query_v1 (SCM query_term_scm)
{
  gboolean ok = FALSE;
  char * pd_type = NULL;
  char * pr_type = NULL;
  gboolean sense = FALSE;
  Query *q = NULL;
  SCM scm;

  if (!gh_list_p (query_term_scm) ||
      gh_null_p (query_term_scm)) {
    PINFO ("null term");
    return NULL;
  }

  do {
    /* pd_type */
    scm = gh_car (query_term_scm);
    query_term_scm = gh_cdr (query_term_scm);
    pd_type = gh_symbol2newstr (scm, NULL);

    /* pr_type */
    if (gh_null_p (query_term_scm)) {
      PINFO ("null pr_type");
      break;
    }
    scm = gh_car (query_term_scm);
    query_term_scm = gh_cdr (query_term_scm);
    pr_type = gh_symbol2newstr (scm, NULL);

    /* sense */
    if (gh_null_p (query_term_scm)) {
      PINFO ("null sense");
      break;
    }
    scm = gh_car (query_term_scm);
    query_term_scm = gh_cdr (query_term_scm);
    sense = gh_scm2bool (scm);

    q = xaccMallocQuery ();

    if (!safe_strcmp (pd_type, "pd-date")) {
      gboolean use_start;
      gboolean use_end;
      Timespec start;
      Timespec end;

      /* use_start */
      if (gh_null_p (query_term_scm)) {
	PINFO ("null use_start");
	break;
      }

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      use_start = gh_scm2bool (scm);

      /* start */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      start = gnc_timepair2timespec (scm);

      /* use_end */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      use_end = gh_scm2bool (scm);

      /* end */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      end = gnc_timepair2timespec (scm);

      xaccQueryAddDateMatchTS (q, use_start, start, use_end, end, QUERY_OR);

      ok = TRUE;

    } else if (!safe_strcmp (pd_type, "pd-amount")) {
      query_compare_t how;
      numeric_match_t amt_sgn;
      double amount;
      gnc_numeric val;

      /* how */
      if (gh_null_p (query_term_scm))
	break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      how = gnc_scm2amt_match_how (scm);

      /* amt_sgn */
      if (gh_null_p (query_term_scm))
	break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      amt_sgn = gnc_query_scm2numericop (scm);

      /* amount */
      if (gh_null_p (query_term_scm))
	break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      amount = gh_scm2double (scm);

      val = double_to_gnc_numeric (amount, GNC_DENOM_AUTO, GNC_RND_ROUND);

      if (!safe_strcmp (pr_type, "pr-price")) {
	xaccQueryAddSharePriceMatch (q, val, how, QUERY_OR);
	ok = TRUE;

      } else if (!safe_strcmp (pr_type, "pr-shares")) {
	xaccQueryAddSharesMatch (q, val, how, QUERY_OR);
	ok = TRUE;

      } else if (!safe_strcmp (pr_type, "pr-value")) {
	xaccQueryAddValueMatch (q, val, amt_sgn, how, QUERY_OR);
	ok = TRUE;

      } else {
	PINFO ("unknown amount predicate: %s", pr_type);
      }

    } else if (!safe_strcmp (pd_type, "pd-account")) {
      guid_match_t how;
      GList *account_guids;

      /* how */
      if (gh_null_p (query_term_scm)) {
	PINFO ("pd-account: null how");
	break;
      }

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      how = gnc_scm2acct_match_how (scm);

      /* account guids */
      if (gh_null_p (query_term_scm)) {
	PINFO ("pd-account: null guids");
	break;
      }

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);

      account_guids = gnc_scm2guid_glist (scm);

      xaccQueryAddAccountGUIDMatch (q, account_guids, how, QUERY_OR);

      gnc_guid_glist_free (account_guids);

      ok = TRUE;

    } else if (!safe_strcmp (pd_type, "pd-string")) {
      gboolean case_sens;
      gboolean use_regexp;
      char *matchstring;

      /* case_sens */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      case_sens = gh_scm2bool (scm);

      /* use_regexp */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      use_regexp = gh_scm2bool (scm);

      /* matchstring */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      matchstring = gh_scm2newstr (scm, NULL);

      if (!safe_strcmp (pr_type, "pr-action")) {
	xaccQueryAddActionMatch (q, matchstring, case_sens, use_regexp,
				 QUERY_OR);
	ok = TRUE;

      } else if (!safe_strcmp (pr_type, "pr-desc")) {
	xaccQueryAddDescriptionMatch (q, matchstring, case_sens,
				      use_regexp, QUERY_OR);
	ok = TRUE;

      } else if (!safe_strcmp (pr_type, "pr-memo")) {
	xaccQueryAddMemoMatch (q, matchstring, case_sens, use_regexp,
			       QUERY_OR);
	ok = TRUE;

      } else if (!safe_strcmp (pr_type, "pr-num")) {        
	xaccQueryAddNumberMatch (q, matchstring, case_sens, use_regexp,
				 QUERY_OR);
	ok = TRUE;

      } else {
	PINFO ("Unknown string predicate: %s", pr_type);
      }

    } else if (!safe_strcmp (pd_type, "pd-cleared")) {
      cleared_match_t how;

      /* how */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      how = gnc_scm2cleared_match_how (scm);

      xaccQueryAddClearedMatch (q, how, QUERY_OR);
      ok = TRUE;

    } else if (!safe_strcmp (pd_type, "pd-balance")) {
      gboolean how;

      /* how */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      if (gnc_scm2balance_match_how (scm, &how) == FALSE)
	break;

      xaccQueryAddBalanceMatch (q, how, QUERY_OR);
      ok = TRUE;

    } else if (!safe_strcmp (pd_type, "pd-guid")) {
      GUID guid;
      GNCIdType id_type;
      char *tmp;

      /* guid */
      if (gh_null_p (query_term_scm))
	break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      guid = gnc_scm2guid (scm);

      /* id type */
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      tmp = gh_scm2newstr (scm, NULL);
      id_type = g_strdup (tmp);
      if (tmp) free (tmp);

      xaccQueryAddGUIDMatch (q, &guid, id_type, QUERY_OR);
      ok = TRUE;

    } else if (!safe_strcmp (pd_type, "pd-kvp")) {
      GSList *path;
      kvp_value *value;
      query_compare_t how;
      GNCIdType where;

      /* how */
      if (gh_null_p (query_term_scm))
        break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      how = gnc_scm2kvp_match_how (scm);

      /* where */
      if (gh_null_p (query_term_scm))
        break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      where = gnc_scm2kvp_match_where (scm);

      /* path */
      if (gh_null_p (query_term_scm))
        break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      path = gnc_query_scm2path (scm);

      /* value */
      if (gh_null_p (query_term_scm))
        break;
      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);
      value = gnc_scm2kvp_value (scm);

      xaccQueryAddKVPMatch (q, path, value, how, where, QUERY_OR);

      gnc_query_path_free (path);
      kvp_value_delete (value);
      ok = TRUE;

    } else {
      PINFO ("Unknown Predicate: %s", pd_type);
    }

  } while (FALSE);

  if (pd_type)
    free (pd_type);

  if (pr_type)
    free (pr_type);

  if (ok)
  {
    Query *out_q;

    if (sense)
      out_q = q;
    else {
      out_q = xaccQueryInvert (q);
      xaccFreeQuery (q);
    }

    return out_q;
  }

  xaccFreeQuery (q);
  return NULL;
}

static Query *
gnc_scm2query_term_query (SCM query_term_scm, query_version_t vers)
{
  switch (vers) {
  case gnc_QUERY_v1:
    return gnc_scm2query_term_query_v1 (query_term_scm);
  case gnc_QUERY_v2:
    return gnc_scm2query_term_query_v2 (query_term_scm);
  default:
    return NULL;
  }
}

static SCM
gnc_query_terms2scm (GList *terms)
{
  SCM or_terms = SCM_EOL;
  GList *or_node;

  for (or_node = terms; or_node; or_node = or_node->next)
  {
    SCM and_terms = SCM_EOL;
    GList *and_node;

    for (and_node = or_node->data; and_node; and_node = and_node->next)
    {
      QueryNewTerm_t qt = and_node->data;
      SCM qt_scm;

      qt_scm = gnc_queryterm2scm (qt);

      and_terms = gh_cons (qt_scm, and_terms);
    }

    and_terms = gh_reverse (and_terms);

    or_terms = gh_cons (and_terms, or_terms);
  }

  return gh_reverse (or_terms);
}

static Query *
gnc_scm2query_and_terms (SCM and_terms, query_version_t vers)
{
  Query *q = NULL;

  if (!gh_list_p (and_terms))
    return NULL;

  while (!gh_null_p (and_terms))
  {
    SCM term;

    term = gh_car (and_terms);
    and_terms = gh_cdr (and_terms);

    if (!q)
      q = gnc_scm2query_term_query (term, vers);
    else
    {
      Query *q_and;
      Query *q_new;

      q_and = gnc_scm2query_term_query (term, vers);

      if (q_and)
      {
        q_new = xaccQueryMerge (q, q_and, QUERY_AND);

        if (q_new)
        {
          xaccFreeQuery (q);
          q = q_new;
        }
      }
    }
  }

  return q;
}

static Query *
gnc_scm2query_or_terms (SCM or_terms, query_version_t vers)
{
  Query *q = NULL;

  if (!gh_list_p (or_terms))
    return NULL;

  q = xaccMallocQuery ();

  while (!gh_null_p (or_terms))
  {
    SCM and_terms;

    and_terms = gh_car (or_terms);
    or_terms = gh_cdr (or_terms);

    if (!q)
      q = gnc_scm2query_and_terms (and_terms, vers);
    else
    {
      Query *q_or;
      Query *q_new;

      q_or = gnc_scm2query_and_terms (and_terms, vers);

      if (q_or)
      {
        q_new = xaccQueryMerge (q, q_or, QUERY_OR);

        if (q_new)
        {
          xaccFreeQuery (q);
          q = q_new;
        }
      }
    }
  }

  return q;
}

static SCM
gnc_query_sort2scm (QueryNewSort_t qs)
{
  SCM sort_scm = SCM_EOL;
  GSList *path;

  path = gncQuerySortGetParamPath (qs);
  if (path == NULL)
    return SCM_BOOL_F;

  sort_scm = gh_cons (gnc_query_path2scm (path), sort_scm);
  sort_scm = gh_cons (gh_int2scm (gncQuerySortGetSortOptions (qs)), sort_scm);
  sort_scm = gh_cons (gh_bool2scm (gncQuerySortGetIncreasing (qs)), sort_scm);

  return gh_reverse (sort_scm);
}

static gboolean
gnc_query_scm2sort (SCM sort_scm, GSList **path, gint *options, gboolean *inc)
{
  SCM val;
  GSList *p;
  gint o;
  gboolean i;

  g_return_val_if_fail (path && options && inc, FALSE);
  g_return_val_if_fail (*path == NULL, FALSE);

  /* This is ok -- it means we have an empty sort.  Don't do anything */
  if (gh_boolean_p (sort_scm))
    return TRUE;

  /* Ok, this had better be a list */
  if (!gh_list_p (sort_scm))
    return FALSE;
  
  /* Parse the path, options, and increasing */
  val = gh_car (sort_scm);
  sort_scm = gh_cdr (sort_scm);
  if (!gh_list_p (val))
    return FALSE;
  p = gnc_query_scm2path (val);

  /* options */
  val = gh_car (sort_scm);
  sort_scm = gh_cdr (sort_scm);
  if (!gh_number_p (val)) {
    gnc_query_path_free (p);
    return FALSE;
  }
  o = gh_scm2int (val);

  /* increasing */
  val = gh_car (sort_scm);
  sort_scm = gh_cdr (sort_scm);
  if (!gh_boolean_p (val)) {
    gnc_query_path_free (p);
    return FALSE;
  }
  i = gh_scm2bool (val);

  /* EOL */
  if (!gh_null_p (sort_scm)) {
    gnc_query_path_free (p);
    return FALSE;
  }
  *path = p;
  *options = o;
  *inc = i;

  return TRUE;
}

SCM
gnc_query2scm (Query *q)
{
  SCM query_scm = SCM_EOL;
  SCM pair;
  QueryNewSort_t s1, s2, s3;

  if (!q) return SCM_BOOL_F;

  /* terms */
  pair = gh_cons (gnc_query_terms2scm (gncQueryGetTerms (q)), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("terms"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* search-for */
  pair = gh_cons (gh_symbol2scm (gncQueryGetSearchFor (q)), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("search-for"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* sorts... */
  gncQueryGetSorts (q, &s1, &s2, &s3);

  /* primary-sort */
  pair = gh_cons (gnc_query_sort2scm (s1), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("primary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* secondary-sort */
  pair = gh_cons (gnc_query_sort2scm (s2), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("secondary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* tertiary-sort */
  pair = gh_cons (gnc_query_sort2scm (s3), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("tertiary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* max results */
  pair = gh_cons (gh_int2scm (gncQueryGetMaxResults (q)), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("max-results"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* Reverse this list; tag it as 'query-v2' */
  pair = gh_reverse (query_scm);
  return gh_cons (gh_symbol2scm ("query-v2"), pair);
}

static GSList *
gnc_query_sort_to_list (char * symbol)
{
  GSList *path = NULL;

  if (!symbol)
    return NULL;

  if (!safe_strcmp (symbol, "by-none")) {
    path = NULL;
  } else if (!safe_strcmp (symbol, "by-standard")) {
    path = g_slist_prepend (path, QUERY_DEFAULT_SORT);

  } else if (!safe_strcmp (symbol, "by-date") ||
	     !safe_strcmp (symbol, "by-date-rounded")) {
    path = g_slist_prepend (path, TRANS_DATE_POSTED);
    path = g_slist_prepend (path, SPLIT_TRANS);

  } else if (!safe_strcmp (symbol, "by-date-entered") ||
	     !safe_strcmp (symbol, "by-date-entered-rounded")) {
    path = g_slist_prepend (path, TRANS_DATE_ENTERED);
    path = g_slist_prepend (path, SPLIT_TRANS);

  } else if (!safe_strcmp (symbol, "by-date-reconciled") ||
	     !safe_strcmp (symbol, "by-date-reconciled-rounded")) {
    path = g_slist_prepend (path, SPLIT_DATE_RECONCILED);

  } else if (!safe_strcmp (symbol, "by-num")) {
    path = g_slist_prepend (path, TRANS_NUM);
    path = g_slist_prepend (path, SPLIT_TRANS);

  } else if (!safe_strcmp (symbol, "by-amount")) {
    path = g_slist_prepend (path, SPLIT_VALUE);

  } else if (!safe_strcmp (symbol, "by-memo")) {
    path = g_slist_prepend (path, SPLIT_MEMO);

  } else if (!safe_strcmp (symbol, "by-desc")) {
    path = g_slist_prepend (path, TRANS_DESCRIPTION);
    path = g_slist_prepend (path, SPLIT_TRANS);

  } else if (!safe_strcmp (symbol, "by-reconcile")) {
    path = g_slist_prepend (path, SPLIT_RECONCILE);

  } else if (!safe_strcmp (symbol, "by-account-full-name")) {
    path = g_slist_prepend (path, SPLIT_ACCT_FULLNAME);

  } else if (!safe_strcmp (symbol, "by-account-code")) {
    path = g_slist_prepend (path, ACCOUNT_CODE_);
    path = g_slist_prepend (path, SPLIT_ACCOUNT);

  } else if (!safe_strcmp (symbol, "by-corr-account-full-name")) {
    path = g_slist_prepend (path, SPLIT_CORR_ACCT_NAME);

  } else if (!safe_strcmp (symbol, "by-corr-account-code")) {
    path = g_slist_prepend (path, SPLIT_CORR_ACCT_CODE);

  } else {
    PERR ("Unknown sort-type, %s", symbol);
  }

  free (symbol);

  return path;
}

static Query *
gnc_scm2query_v1 (SCM query_scm)
{
  Query *q = NULL;
  gboolean ok = TRUE;
  char * primary_sort = NULL;
  char * secondary_sort = NULL;
  char * tertiary_sort = NULL;
  gboolean primary_increasing = TRUE;
  gboolean secondary_increasing = TRUE;
  gboolean tertiary_increasing = TRUE;
  int max_splits = -1;

  while (!gh_null_p (query_scm))
  {
    char *symbol;
    SCM sym_scm;
    SCM value;
    SCM pair;

    pair = gh_car (query_scm);
    query_scm = gh_cdr (query_scm);

    if (!gh_pair_p (pair)) {
      PERR ("Not a Pair");
      ok = FALSE;
      break;
    }

    sym_scm = gh_car (pair);
    value = gh_cadr (pair);

    if (!gh_symbol_p (sym_scm)) {
      PERR ("Not a symbol");
      ok = FALSE;
      break;
    }

    symbol = gh_symbol2newstr (sym_scm, NULL);
    if (!symbol) {
      PERR ("No string found");
      ok = FALSE;
      break;
    }

    if (safe_strcmp ("terms", symbol) == 0) {
      if (q)
        xaccFreeQuery (q);

      q = gnc_scm2query_or_terms (value, gnc_QUERY_v1);
      if (!q) {
	PINFO ("invalid terms");
        ok = FALSE;
        free (symbol);
        break;
      }

    } else if (safe_strcmp ("primary-sort", symbol) == 0) {
      if (!gh_symbol_p (value)) {
	PINFO ("Invalid primary sort");
        ok = FALSE;
        free (symbol);
        break;
      }

      primary_sort = gh_symbol2newstr (value, NULL);

    } else if (safe_strcmp ("secondary-sort", symbol) == 0) {
      if (!gh_symbol_p (value)) {
	PINFO ("Invalid secondary sort");
        ok = FALSE;
        free (symbol);
        break;
      }

      secondary_sort = gh_symbol2newstr (value, NULL);

    } else if (safe_strcmp ("tertiary-sort", symbol) == 0) {
      if (!gh_symbol_p (value)) {
	PINFO ("Invalid tertiary sort");
        ok = FALSE;
        free (symbol);
        break;
      }

      tertiary_sort = gh_symbol2newstr (value, NULL);

    } else if (safe_strcmp ("primary-increasing", symbol) == 0) {
      primary_increasing = gh_scm2bool (value);

    } else if (safe_strcmp ("secondary-increasing", symbol) == 0) {
      secondary_increasing = gh_scm2bool (value);

    } else if (safe_strcmp ("tertiary-increasing", symbol) == 0) {
      tertiary_increasing = gh_scm2bool (value);

    } else if (safe_strcmp ("max-splits", symbol) == 0) {
      if (!gh_number_p (value)) {
	PERR ("invalid max-splits");
        ok = FALSE;
        free (symbol);
        break;
      }

      max_splits = gh_scm2int (value);

    } else {
      PERR ("Unknown symbol: %s", symbol);
      ok = FALSE;
      free (symbol);
      break;
    }

    free (symbol);
  }

  if (ok) {
    GSList *s1, *s2, *s3;
    s1 = gnc_query_sort_to_list (primary_sort);
    s2 = gnc_query_sort_to_list (secondary_sort);
    s3 = gnc_query_sort_to_list (tertiary_sort);

    gncQuerySetSortOrder (q, s1, s2, s3);
    gncQuerySetSortIncreasing (q, primary_increasing, secondary_increasing,
			       tertiary_increasing);
    xaccQuerySetMaxSplits (q, max_splits);

    return q;
  }

  if (primary_sort)
    free (primary_sort);

  if (secondary_sort)
    free (secondary_sort);

  if (tertiary_sort)
    free (tertiary_sort);

  xaccFreeQuery (q);
  return NULL;
}

static Query *
gnc_scm2query_v2 (SCM query_scm)
{
  Query *q = NULL;
  gboolean ok = TRUE;
  char * search_for = NULL;
  GSList *sp1 = NULL, *sp2 = NULL, *sp3 = NULL;
  gint so1 = 0, so2 = 0, so3 = 0;
  gboolean si1 = TRUE, si2 = TRUE, si3 = TRUE;
  int max_results = -1;

  while (!gh_null_p (query_scm))
  {
    char *symbol;
    SCM sym_scm;
    SCM value;
    SCM pair;

    pair = gh_car (query_scm);
    query_scm = gh_cdr (query_scm);

    if (!gh_pair_p (pair)) {
      ok = FALSE;
      break;
    }

    sym_scm = gh_car (pair);
    value = gh_cadr (pair);

    if (!gh_symbol_p (sym_scm)) {
      ok = FALSE;
      break;
    }

    symbol = gh_symbol2newstr (sym_scm, NULL);
    if (!symbol) {
      ok = FALSE;
      break;
    }

    if (!safe_strcmp ("terms", symbol)) {
      if (q)
        xaccFreeQuery (q);

      q = gnc_scm2query_or_terms (value, gnc_QUERY_v2);
      if (!q) {
        ok = FALSE;
        free (symbol);
        break;
      }

    } else if (!safe_strcmp ("search-for", symbol)) {
      if (!gh_symbol_p (value)) {
	ok = FALSE;
	free (symbol);
	break;
      }
      search_for = gh_symbol2newstr (value, NULL);

    } else if (safe_strcmp ("primary-sort", symbol) == 0) {
      if (! gnc_query_scm2sort (value, &sp1, &so1, &si1)) {
        ok = FALSE;
        free (symbol);
        break;
      }

    } else if (!safe_strcmp ("secondary-sort", symbol)) {
      if (! gnc_query_scm2sort (value, &sp2, &so2, &si2)) {
        ok = FALSE;
        free (symbol);
        break;
      }

    } else if (!safe_strcmp ("tertiary-sort", symbol)) {
      if (! gnc_query_scm2sort (value, &sp3, &so3, &si3)) {
        ok = FALSE;
        free (symbol);
        break;
      }

    } else if (!safe_strcmp ("max-results", symbol)) {
      if (!gh_number_p (value)) {
        ok = FALSE;
        free (symbol);
        break;
      }

      max_results = gh_scm2int (value);

    } else {
      ok = FALSE;
      free (symbol);
      break;
    }

    free (symbol);
  }

  if (ok && search_for) {
    gncQuerySearchFor (q, search_for);
    gncQuerySetSortOrder (q, sp1, sp2, sp3);
    gncQuerySetSortOptions (q, so1, so2, so3);
    gncQuerySetSortIncreasing (q, si1, si2, si3);
    gncQuerySetMaxResults (q, max_results);

    return q;
  }

  xaccFreeQuery (q);
  return NULL;
}

Query *
gnc_scm2query (SCM query_scm)
{
  SCM q_type;
  char *type;
  Query *q = NULL;

  /* Not a list or NULL?  No need to go further */
  if (!gh_list_p (query_scm) || gh_null_p (query_scm))
    return NULL;

  /* Grab the 'type' (for v2 and above) */
  q_type = gh_car (query_scm);

  if (!gh_symbol_p (q_type)) {
    if (gh_pair_p (q_type)) {
      /* Version-1 queries are just a list */
      return gnc_scm2query_v1 (query_scm);
    } else {
      return NULL;
    }
  }

  /* Ok, the LHS is the version and the RHS is the actual query list */
  type = gh_symbol2newstr (q_type, NULL);
  if (!type)
    return NULL;

  if (!safe_strcmp (type, "query-v2"))
    q = gnc_scm2query_v2 (gh_cdr (query_scm));

  free (type);
  return q;
}

static int
gnc_scm_traversal_adapter(Transaction *t, void *data)
{
  static SCM trans_type = SCM_BOOL_F;
  SCM result;
  SCM scm_trans;
  SCM thunk = *((SCM *) data);

  if(trans_type == SCM_BOOL_F) {
    trans_type = gh_eval_str("<gnc:Transaction*>");
    /* don't really need this - types are bound globally anyway. */
    if(trans_type != SCM_BOOL_F) scm_protect_object(trans_type);
  }
  
  scm_trans = gw_wcp_assimilate_ptr(t, trans_type);
  result = gh_call1(thunk, scm_trans);

  return (result != SCM_BOOL_F);
}

gboolean
gnc_scmGroupStagedTransactionTraversal(AccountGroup *grp,
                                       unsigned int new_marker,
                                       SCM thunk)
{
  return xaccGroupStagedTransactionTraversal(grp, new_marker,
                                             gnc_scm_traversal_adapter,
                                             &thunk);
}

gboolean
gnc_scmAccountStagedTransactionTraversal(Account *a,
                                         unsigned int new_marker,
                                         SCM thunk) 
{
  return xaccAccountStagedTransactionTraversal(a, new_marker,
					       gnc_scm_traversal_adapter,
                                               &thunk);
}

SCM
gnc_gint64_to_scm(const gint64 x)
{
#if GUILE_LONG_LONG_OK 
  return scm_long_long2num(x);
#else
  const gchar negative_p = (x < 0);
  const guint64 magnitude = negative_p ? -x : x;
  const guint32 lower_half = (guint32) (magnitude & 0xFFFFFFFF);
  const guint32 upper_half = (guint32) (magnitude >> 32);
  SCM result;

  result = scm_sum(scm_ash(gh_ulong2scm(upper_half), SCM_MAKINUM(32)),
                   gh_ulong2scm(lower_half));
  
  if(negative_p) {
    return scm_difference(SCM_INUM0, result);
  } else {
    return result;
  }
#endif
}

gint64
gnc_scm_to_gint64(SCM num)
{
#if GUILE_LONG_LONG_OK 
  return scm_num2long_long(num, (char *) SCM_ARG1, "gnc_scm_to_gint64");
#else
  static SCM bits00to15_mask = SCM_BOOL_F;
  SCM magnitude  = scm_abs(num);
  SCM bits;
  unsigned long c_bits;
  long long     c_result = 0;
  int		i;

  /* This doesn't work -- atm (bit-extract 4000 0 32) proves it */
  /*
  SCM lower = scm_bit_extract(magnitude, SCM_MAKINUM(0), SCM_MAKINUM(32));
  */
  
  if (bits00to15_mask == SCM_BOOL_F) {
    bits00to15_mask = gh_ulong2scm(0xFFFF);
    scm_protect_object (bits00to15_mask);
  }

  /*
   * This isn't very complicated (IMHO).  We work from the "top" of
   * the number downwards.  We assume this is no more than a 64-bit
   * number, otherwise it will fail right away.  Anyways, we keep
   * taking the top 16 bits of the number and move it to c_result.
   * Then we 'remove' those bits from the original number and continue
   * with the next 16 bits down, and so on.  -- warlord@mit.edu
   * 2001/02/13
   */
  for (i = 48; i >=0; i-= 16) {
    bits = scm_ash(magnitude, SCM_MAKINUM(-i));
    c_bits = gh_scm2ulong(scm_logand(bits, bits00to15_mask));
    c_result += ((long long)c_bits << i);
    magnitude = scm_difference(magnitude, scm_ash(bits, SCM_MAKINUM(i)));
  }
  
  if(scm_negative_p(num) != SCM_BOOL_F) {
    return(- c_result);
  } 
  else {
    return(c_result);
  }
#endif
}

int
gnc_gh_gint64_p(SCM num)
{
  static int initialized = 0;
  static SCM maxval;
  static SCM minval;

  if (!initialized)
  {
    /* to be super safe, we have to build these manually because
       though we know that we have gint64's here, we *don't* know how
       to portably specify a 64bit constant to the compiler (i.e. like
       0x7FFFFFFFFFFFFFFF). */
    gint64 tmp;
    
    tmp = 0x7FFFFFFF;
    tmp <<= 32;
    tmp |= 0xFFFFFFFF;
    maxval = gnc_gint64_to_scm(tmp);

    tmp = 0x80000000;
    tmp <<= 32;
    minval = gnc_gint64_to_scm(tmp);

    scm_protect_object(maxval);
    scm_protect_object(minval);
    initialized = 1;
  }

  return (gh_exact_p(num) &&
          (scm_geq_p(num, minval) != SCM_BOOL_F) &&
          (scm_leq_p(num, maxval) != SCM_BOOL_F));
}

gnc_numeric
gnc_scm_to_numeric(SCM gncnum)
{
  static SCM get_num   = SCM_BOOL_F;
  static SCM get_denom = SCM_BOOL_F;
  
  if(get_num == SCM_BOOL_F) {
    get_num = gh_eval_str("gnc:gnc-numeric-num");
  }
  if(get_denom == SCM_BOOL_F) {
    get_denom = gh_eval_str("gnc:gnc-numeric-denom");
  }
  
  return gnc_numeric_create(gnc_scm_to_gint64(gh_call1(get_num, gncnum)),
                            gnc_scm_to_gint64(gh_call1(get_denom, gncnum)));
}

SCM
gnc_numeric_to_scm(gnc_numeric arg)
{
  static SCM maker = SCM_BOOL_F;

  if(maker == SCM_BOOL_F) {
    maker = gh_eval_str("gnc:make-gnc-numeric");
  }
  
  return gh_call2(maker, gnc_gint64_to_scm(gnc_numeric_num(arg)),
                  gnc_gint64_to_scm(gnc_numeric_denom(arg)));
}

int
gnc_numeric_p(SCM arg)
{
  static SCM type_p = SCM_BOOL_F;
  SCM        ret    = SCM_BOOL_F;

  if(type_p == SCM_BOOL_F) {
    type_p = gh_eval_str("gnc:gnc-numeric?");
  }
  ret = gh_call1(type_p, arg);

  if(ret == SCM_BOOL_F) {
    return FALSE;
  }
  else {
    return TRUE;
  }
}

/********************************************************************
 * gnc_scm_to_commodity
 ********************************************************************/
gnc_commodity *
gnc_scm_to_commodity(SCM scm)
{
  static SCM commodity_type = SCM_UNDEFINED;

  if(commodity_type == SCM_UNDEFINED) {
    commodity_type = gh_eval_str("<gnc:commodity*>");
    /* don't really need this - types are bound globally anyway. */
    if(commodity_type != SCM_UNDEFINED) scm_protect_object(commodity_type);
  }

  if(!gw_wcp_is_of_type_p(commodity_type, scm)) {
    return NULL;
  }

  return gw_wcp_get_ptr(scm);
}


/********************************************************************
 * gnc_commodity_to_scm
 ********************************************************************/
SCM
gnc_commodity_to_scm (const gnc_commodity *commodity)
{
  static SCM commodity_type = SCM_UNDEFINED;

  if(commodity == NULL) return SCM_BOOL_F;

  if(commodity_type == SCM_UNDEFINED) {
    commodity_type = gh_eval_str("<gnc:commodity*>");
    /* don't really need this - types are bound globally anyway. */
    if(commodity_type != SCM_UNDEFINED) scm_protect_object(commodity_type);
  }
  
  return gw_wcp_assimilate_ptr((void *) commodity, commodity_type);
}

/********************************************************************
 * gnc_book_to_scm
 ********************************************************************/
SCM
gnc_book_to_scm (GNCBook *book)
{
  static SCM book_type = SCM_UNDEFINED;

  if (!book)
    return SCM_BOOL_F;

  if (book_type == SCM_UNDEFINED)
  {
    book_type = gh_eval_str ("<gnc:Book*>");

    /* don't really need this - types are bound globally anyway. */
    if (book_type != SCM_UNDEFINED)
      scm_protect_object (book_type);
  }
  
  return gw_wcp_assimilate_ptr ((void *) book, book_type);
}

/********************************************************************
 * gnc_session_to_scm
 ********************************************************************/
SCM
gnc_session_to_scm (GNCSession *session)
{
  static SCM session_type = SCM_UNDEFINED;

  if (!session)
    return SCM_BOOL_F;

  if (session_type == SCM_UNDEFINED)
  {
    session_type = gh_eval_str ("<gnc:Session*>");

    /* don't really need this - types are bound globally anyway. */
    if (session_type != SCM_UNDEFINED)
      scm_protect_object (session_type);
  }

  return gw_wcp_assimilate_ptr ((void *) session, session_type);
}


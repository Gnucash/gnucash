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

#include <g-wrap-runtime-guile.h>
#include <libguile.h>
#include <string.h>

#include "Account.h"
#include "Backend.h"
#include "Group.h"
#include "Query.h"
#include "date.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-numeric.h"

gnc_commodity_table *
gnc_engine_commodity_table_new (void)
{
  static SCM commodity_table_type = SCM_UNDEFINED;
  gnc_commodity_table *commodity_table;
  SCM ct_scm;
  SCM func;

  commodity_table = gnc_commodity_table_new ();

  if (commodity_table_type == SCM_UNDEFINED)
  {
    commodity_table_type = gh_eval_str("<gnc:commodity-table*>");
    /* don't really need this - types are bound globally anyway. */
    if (commodity_table_type != SCM_UNDEFINED)
      scm_protect_object (commodity_table_type);
  }
  
  ct_scm =  gw_wcp_assimilate_ptr ((void *) commodity_table,
                                   commodity_table_type);

  func = gh_eval_str ("gnc:engine-commodity-table-construct");

  gh_call1 (func, ct_scm);

  return commodity_table;
}

Timespec
gnc_transaction_get_date_posted(Transaction *t) {
  Timespec result;
  xaccTransGetDatePostedTS(t, &result);
  return(result);
}

Timespec
gnc_transaction_get_date_entered(Transaction *t) {
  Timespec result;
  xaccTransGetDateEnteredTS(t, &result);
  return(result);
}

Timespec
gnc_split_get_date_reconciled(Split *s) {
  Timespec result;
  xaccSplitGetDateReconciledTS(s, &result);
  return(result);
}

void
gnc_transaction_set_date_posted(Transaction *t, const Timespec d) {
  xaccTransSetDatePostedTS(t, &d);
}

void
gnc_transaction_set_date_entered(Transaction *t, const Timespec d) {
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

static SCM
gnc_query_term_type2scm (pd_type_t pd_type)
{
  return gnc_gw_enum_val2scm ("<gnc:query-term-type>", pd_type);
}

static pd_type_t
gnc_query_scm2term_type (SCM pd_type_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:query-term-type>", pd_type_scm);
}

static SCM
gnc_query_pred_type2scm (pr_type_t pr_type)
{
  return gnc_gw_enum_val2scm ("<gnc:query-pred-type>", pr_type);
}

static pr_type_t
gnc_query_scm2pred_type (SCM pr_type_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:query-pred-type>", pr_type_scm);
}

static SCM
gnc_acct_match_how2scm (acct_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:acct-match-how>", how);
}

static acct_match_t
gnc_scm2acct_match_how (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:acct-match-how>", how_scm);
}

static SCM
gnc_amt_match_how2scm (amt_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:amt-match-how>", how);
}

static amt_match_t
gnc_scm2amt_match_how (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:amt-match-how>", how_scm);
}

static SCM
gnc_amt_match_sign2scm (amt_match_sgn_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:amt-match-sign>", how);
}

static amt_match_sgn_t
gnc_scm2amt_match_sign (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:amt-match-sign>", how_scm);
}

static SCM
gnc_kvp_match_how2scm (kvp_match_t how)
{
  return gnc_gw_enum_val2scm ("<gnc:kvp-match-how>", how);
}

static kvp_match_t
gnc_scm2kvp_match_how (SCM how_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:kvp-match-how>", how_scm);
}

static SCM
gnc_sort_type2scm (sort_type_t sort_type)
{
  return gnc_gw_enum_val2scm ("<gnc:sort-type>", sort_type);
}

static sort_type_t
gnc_scm2sort_type (SCM sort_type_scm)
{
  return gnc_gw_enum_scm2val ("<gnc:sort-type>", sort_type_scm);
}

static SCM
gnc_bitfield2scm (const char *typestr, int value)
{
  SCM field = SCM_EOL;
  int bit = 1;

  while (value)
  {
    if (value & bit)
    {
      SCM scm;

      scm = gnc_gw_enum_val2scm (typestr, bit);
      if (gh_symbol_p (scm))
        field = gh_cons (scm, field);
    }

    value &= ~bit;
    bit <<= 1;
  }

  return gh_reverse (field);
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

static SCM
gnc_cleared_match_how2scm (cleared_match_t how)
{
  return gnc_bitfield2scm ("<gnc:cleared-match-how>", how);
}

static cleared_match_t
gnc_scm2cleared_match_how (SCM how_scm)
{
  return gnc_scm2bitfield ("<gnc:cleared-match-how>", how_scm);
}

static SCM
gnc_balance_match_how2scm (balance_match_t how)
{
  return gnc_bitfield2scm ("<gnc:balance-match-how>", how);
}

static balance_match_t
gnc_scm2balance_match_how (SCM how_scm)
{
  return gnc_scm2bitfield ("<gnc:balance-match-how>", how_scm);
}

static SCM
gnc_kvp_match_where2scm (kvp_match_where_t where)
{
  return gnc_bitfield2scm ("<gnc:kvp-match-where>", where);
}

static kvp_match_where_t
gnc_scm2kvp_match_where (SCM where_scm)
{
  return gnc_scm2bitfield ("<gnc:kvp-match-where>", where_scm);
}

static SCM
gnc_acct_guid_glist2scm (GList *account_guids)
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
gnc_scm2acct_guid_glist (SCM guids_scm)
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
acct_guid_glist_free (GList *guids)
{
  GList *node;

  for (node = guids; node; node = node->next)
    xaccGUIDFree (node->data);

  g_list_free (guids);
}

static SCM
gnc_kvp_path2scm (GSList *path)
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
gnc_scm2kvp_path (SCM path_scm)
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
gnc_kvp_path_free (GSList *path)
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
      scm = gh_cons (gnc_gint64_to_scm (n.num),
                     gnc_gint64_to_scm (n.denom));
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
      SCM denom;
      SCM num;

      if (!gh_pair_p (val_scm))
        return NULL;

      num = gh_car (val_scm);
      denom = gh_cdr (val_scm);

      n = gnc_numeric_create (gnc_scm_to_gint64 (num),
                              gnc_scm_to_gint64 (denom));

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
      g_warning ("unexpected type: %d", value_t);
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
gnc_queryterm2scm (QueryTerm *qt)
{
  SCM qt_scm = SCM_EOL;

  qt_scm = gh_cons (gnc_query_term_type2scm (qt->data.type), qt_scm);
  qt_scm = gh_cons (gnc_query_pred_type2scm (qt->data.base.term_type), qt_scm);
  qt_scm = gh_cons (gh_bool2scm (qt->data.base.sense), qt_scm);

  switch (qt->data.type)
  {
    case PD_DATE:
      qt_scm = gh_cons (gh_bool2scm (qt->data.date.use_start), qt_scm);
      qt_scm = gh_cons (gnc_timespec2timepair (qt->data.date.start), qt_scm);
      qt_scm = gh_cons (gh_bool2scm (qt->data.date.use_end), qt_scm);
      qt_scm = gh_cons (gnc_timespec2timepair (qt->data.date.end), qt_scm);
      break;

    case PD_AMOUNT:
      qt_scm = gh_cons (gnc_amt_match_how2scm (qt->data.amount.how), qt_scm);
      qt_scm = gh_cons (gnc_amt_match_sign2scm (qt->data.amount.amt_sgn),
                        qt_scm);
      qt_scm = gh_cons (gh_double2scm (qt->data.amount.amount), qt_scm);
      break;

    case PD_ACCOUNT:
      qt_scm = gh_cons (gnc_acct_match_how2scm (qt->data.acct.how), qt_scm);
      qt_scm = gh_cons (gnc_acct_guid_glist2scm (qt->data.acct.account_guids),
                        qt_scm);
      break;

    case PD_STRING:
      qt_scm = gh_cons (gh_bool2scm (qt->data.str.case_sens), qt_scm);
      qt_scm = gh_cons (gh_bool2scm (qt->data.str.use_regexp), qt_scm);
      qt_scm = gh_cons (gh_str02scm (qt->data.str.matchstring), qt_scm);
      break;

    case PD_CLEARED:
      qt_scm = gh_cons (gnc_cleared_match_how2scm (qt->data.cleared.how),
                        qt_scm);
      break;

    case PD_BALANCE:
      qt_scm = gh_cons (gnc_balance_match_how2scm (qt->data.balance.how),
                        qt_scm);
      break;

    case PD_GUID:
      qt_scm = gh_cons (gnc_guid2scm (qt->data.guid.guid), qt_scm);
      qt_scm = gh_cons (gh_str02scm (qt->data.guid.id_type), qt_scm);
      break;

    case PD_KVP:
      qt_scm = gh_cons (gnc_kvp_match_how2scm (qt->data.kvp.how), qt_scm);
      qt_scm = gh_cons (gnc_kvp_match_where2scm (qt->data.kvp.where), qt_scm);
      qt_scm = gh_cons (gnc_kvp_path2scm (qt->data.kvp.path), qt_scm);
      qt_scm = gh_cons (gnc_kvp_value2scm (qt->data.kvp.value), qt_scm);
      break;

    default:
      g_warning ("query type %d not supported", qt->data.type);
      return SCM_BOOL_F;
  }

  return gh_reverse (qt_scm);
}

static Query *
gnc_scm2query_term_query (SCM query_term_scm)
{
  gboolean ok = FALSE;
  pd_type_t pd_type;
  pr_type_t pr_type;
  gboolean sense;
  Query *q;
  SCM scm;

  if (!gh_list_p (query_term_scm) ||
      gh_null_p (query_term_scm))
    return NULL;

  /* pd_type */
  scm = gh_car (query_term_scm);
  query_term_scm = gh_cdr (query_term_scm);

  pd_type = gnc_query_scm2term_type (scm);

  /* pr_type */
  if (gh_null_p (query_term_scm))
    return NULL;

  scm = gh_car (query_term_scm);
  query_term_scm = gh_cdr (query_term_scm);

  pr_type = gnc_query_scm2pred_type (scm);

  /* sense */
  if (gh_null_p (query_term_scm))
    return NULL;

  scm = gh_car (query_term_scm);
  query_term_scm = gh_cdr (query_term_scm);

  sense = gh_scm2bool (scm);

  q = xaccMallocQuery ();

  switch (pd_type)
  {
    case PD_DATE:
      {
        gboolean use_start;
        gboolean use_end;
        Timespec start;
        Timespec end;

        /* use_start */
        if (gh_null_p (query_term_scm))
          break;

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
      }

      ok = TRUE;
      break;

    case PD_AMOUNT:
      {
        amt_match_t how;
        amt_match_sgn_t amt_sgn;
        double amount;

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

        amt_sgn = gnc_scm2amt_match_sign (scm);

        /* amount */
        if (gh_null_p (query_term_scm))
          break;

        scm = gh_car (query_term_scm);
        query_term_scm = gh_cdr (query_term_scm);

        amount = gh_scm2double (scm);

        switch (pr_type)
        {
          case PR_AMOUNT:
            DxaccQueryAddAmountMatch (q, amount, amt_sgn, how, QUERY_OR);
            ok = TRUE;
            break;

          case PR_PRICE:
            DxaccQueryAddSharePriceMatch (q, amount, how, QUERY_OR);
            ok = TRUE;
            break;

          case PR_SHRS:
            DxaccQueryAddSharesMatch (q, amount, how, QUERY_OR);
            ok = TRUE;
            break;

          default:
            ok = FALSE;
            break;
        }
      }

      break;

    case PD_ACCOUNT:
      {
        acct_match_t how;
        GList *account_guids;

        /* how */
        if (gh_null_p (query_term_scm))
          break;

        scm = gh_car (query_term_scm);
        query_term_scm = gh_cdr (query_term_scm);

        how = gnc_scm2acct_match_how (scm);

        /* account guids */
        if (gh_null_p (query_term_scm))
          break;

        scm = gh_car (query_term_scm);
        query_term_scm = gh_cdr (query_term_scm);

        account_guids = gnc_scm2acct_guid_glist (scm);

        xaccQueryAddAccountGUIDMatch (q, account_guids, how, QUERY_OR);

        acct_guid_glist_free (account_guids);
      }

      ok = TRUE;
      break;

    case PD_STRING:
      {
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

        switch (pr_type)
        {
          case PR_ACTION:
            xaccQueryAddActionMatch (q, matchstring, case_sens, use_regexp,
                                     QUERY_OR);
            ok = TRUE;
            break;

          case PR_DESC:
            xaccQueryAddDescriptionMatch (q, matchstring, case_sens,
                                          use_regexp, QUERY_OR);
            ok = TRUE;
            break;

          case PR_MEMO:
            xaccQueryAddMemoMatch (q, matchstring, case_sens, use_regexp,
                                   QUERY_OR);
            ok = TRUE;
            break;

          case PR_NUM:
            xaccQueryAddNumberMatch (q, matchstring, case_sens, use_regexp,
                                     QUERY_OR);
            ok = TRUE;
            break;

          default:
            ok = FALSE;
            break;
        }
      }

      break;

    case PD_CLEARED:
      {
        cleared_match_t how;

        /* how */
        if (gh_null_p (query_term_scm))
          break;

        scm = gh_car (query_term_scm);
        query_term_scm = gh_cdr (query_term_scm);

        how = gnc_scm2cleared_match_how (scm);

        xaccQueryAddClearedMatch (q, how, QUERY_OR);
      }

      ok = TRUE;
      break;

    case PD_BALANCE:
      {
        balance_match_t how;

        /* how */
        if (gh_null_p (query_term_scm))
          break;

        scm = gh_car (query_term_scm);
        query_term_scm = gh_cdr (query_term_scm);

        how = gnc_scm2balance_match_how (scm);

        xaccQueryAddBalanceMatch (q, how, QUERY_OR);
      }

      ok = TRUE;
      break;

    case PD_GUID:
      {
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
      }

      ok = TRUE;
      break;

    case PD_KVP: {
      GSList *path;
      kvp_value *value;
      kvp_match_t how;
      kvp_match_where_t where;

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

      path = gnc_scm2kvp_path (scm);

      /* value */
      if (gh_null_p (query_term_scm))
        break;

      scm = gh_car (query_term_scm);
      query_term_scm = gh_cdr (query_term_scm);

      value = gnc_scm2kvp_value (scm);

      xaccQueryAddKVPMatch (q, path, value, how, where, QUERY_OR);

      gnc_kvp_path_free (path);
      kvp_value_delete (value);
    }

      ok = TRUE;
      break;

    default:
      break;
  }

  if (ok)
  {
    Query *out_q;

    if (sense)
      out_q = q;
    else
    {
      out_q = xaccQueryInvert (q);
      xaccFreeQuery (q);
    }

    return out_q;
  }

  xaccFreeQuery (q);
  return NULL;
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
      QueryTerm *qt = and_node->data;
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
gnc_scm2query_and_terms (SCM and_terms)
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
      q = gnc_scm2query_term_query (term);
    else
    {
      Query *q_and;
      Query *q_new;

      q_and = gnc_scm2query_term_query (term);

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
gnc_scm2query_or_terms (SCM or_terms)
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
      q = gnc_scm2query_and_terms (and_terms);
    else
    {
      Query *q_or;
      Query *q_new;

      q_or = gnc_scm2query_and_terms (and_terms);

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

SCM
gnc_query2scm (Query *q)
{
  SCM query_scm = SCM_EOL;
  SCM pair;

  if (!q) return SCM_BOOL_F;

  /* terms */
  pair = gh_cons (gnc_query_terms2scm (xaccQueryGetTerms (q)), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("terms"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* primary sort type */
  pair = gh_cons (gnc_sort_type2scm (xaccQueryGetPrimarySortOrder (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("primary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* secondary sort type */
  pair = gh_cons (gnc_sort_type2scm (xaccQueryGetSecondarySortOrder (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("secondary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* tertiary sort type */
  pair = gh_cons (gnc_sort_type2scm (xaccQueryGetTertiarySortOrder (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("tertiary-sort"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* primary sort increasing */
  pair = gh_cons (gh_bool2scm (xaccQueryGetSortPrimaryIncreasing (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("primary-increasing"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* secondary sort increasing */
  pair = gh_cons (gh_bool2scm (xaccQueryGetSortSecondaryIncreasing (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("secondary-increasing"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* tertiary sort increasing */
  pair = gh_cons (gh_bool2scm (xaccQueryGetSortTertiaryIncreasing (q)),
                  SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("tertiary-increasing"), pair);
  query_scm = gh_cons (pair, query_scm);

  /* max splits */
  pair = gh_cons (gh_int2scm (xaccQueryGetMaxSplits (q)), SCM_EOL);
  pair = gh_cons (gh_symbol2scm ("max-splits"), pair);
  query_scm = gh_cons (pair, query_scm);

  return gh_reverse (query_scm);
}

Query *
gnc_scm2query (SCM query_scm)
{
  Query *q = NULL;
  gboolean ok = TRUE;
  sort_type_t primary_sort = BY_STANDARD;
  sort_type_t secondary_sort = BY_NONE;
  sort_type_t tertiary_sort = BY_NONE;
  gboolean primary_increasing = TRUE;
  gboolean secondary_increasing = TRUE;
  gboolean tertiary_increasing = TRUE;
  int max_splits = -1;

  if (!gh_list_p (query_scm))
    return NULL;

  while (!gh_null_p (query_scm))
  {
    char *symbol;
    SCM sym_scm;
    SCM value;
    SCM pair;

    pair = gh_car (query_scm);
    query_scm = gh_cdr (query_scm);

    if (!gh_pair_p (pair))
    {
      ok = FALSE;
      break;
    }

    sym_scm = gh_car (pair);
    value = gh_cadr (pair);

    if (!gh_symbol_p (sym_scm))
    {
      ok = FALSE;
      break;
    }

    symbol = gh_symbol2newstr (sym_scm, NULL);
    if (!symbol)
    {
      ok = FALSE;
      break;
    }

    if (safe_strcmp ("terms", symbol) == 0)
    {
      if (q)
        xaccFreeQuery (q);

      q = gnc_scm2query_or_terms (value);
      if (!q)
      {
        ok = FALSE;
        free (symbol);
        break;
      }
    }
    else if (safe_strcmp ("primary-sort", symbol) == 0)
    {
      if (!gh_symbol_p (value))
      {
        ok = FALSE;
        free (symbol);
        break;
      }

      primary_sort = gnc_scm2sort_type (value);
    }
    else if (safe_strcmp ("secondary-sort", symbol) == 0)
    {
      if (!gh_symbol_p (value))
      {
        ok = FALSE;
        free (symbol);
        break;
      }

      secondary_sort = gnc_scm2sort_type (value);
    }
    else if (safe_strcmp ("tertiary-sort", symbol) == 0)
    {
      if (!gh_symbol_p (value))
      {
        ok = FALSE;
        free (symbol);
        break;
      }

      tertiary_sort = gnc_scm2sort_type (value);
    }
    else if (safe_strcmp ("primary-increasing", symbol) == 0)
    {
      primary_increasing = gh_scm2bool (value);
    }
    else if (safe_strcmp ("secondary-increasing", symbol) == 0)
    {
      secondary_increasing = gh_scm2bool (value);
    }
    else if (safe_strcmp ("tertiary-increasing", symbol) == 0)
    {
      tertiary_increasing = gh_scm2bool (value);
    }
    else if (safe_strcmp ("max-splits", symbol) == 0)
    {
      if (!gh_number_p (value))
      {
        ok = FALSE;
        free (symbol);
        break;
      }

      max_splits = gh_scm2int (value);
    }
    else
    {
      ok = FALSE;
      free (symbol);
      break;
    }

    free (symbol);
  }

  if (ok)
  {
    xaccQuerySetSortOrder (q, primary_sort, secondary_sort, tertiary_sort);
    xaccQuerySetSortIncreasing (q, primary_increasing, secondary_increasing,
                                tertiary_increasing);
    xaccQuerySetMaxSplits (q, max_splits);

    return q;
  }

  xaccFreeQuery (q);
  return NULL;
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

static SCM
gnc_glist_account_ptr_to_scm_internal (GList *account_list, gboolean free_list)
{
  static SCM acct_type = SCM_UNDEFINED;
  SCM result;

  if (acct_type == SCM_UNDEFINED)
  {
    acct_type = gh_eval_str("<gnc:Account*>");
    /* don't really need this - types are bound globally anyway. */
    if(acct_type != SCM_UNDEFINED) scm_protect_object(acct_type);
  }

  result = gnc_glist_to_scm_list(account_list, acct_type);

  if (free_list)
    g_list_free (account_list);

  return result;
}

/********************************************************************\
 * gnc_account_list_to_scm                                          *
 *   Turn a list of accounts into an SCM.                           *
 *   The list is freed.                                             *
 *                                                                  *
 * Args: account_list - list of accounts to SCMify                  *
 * Return: SCM list of accounts                                     *
\********************************************************************/
SCM
gnc_glist_account_ptr_to_scm (GList *account_list)
{
  return gnc_glist_account_ptr_to_scm_internal (account_list, TRUE);
}

/********************************************************************\
 * gnc_account_list_to_scm_no_free                                  *
 *   Turn a list of accounts into an SCM.                           *
 *   The list is not freed.                                         *
 *                                                                  *
 * Args: account_list - list of accounts to SCMify                  *
 * Return: SCM list of accounts                                     *
\********************************************************************/
SCM
gnc_glist_account_ptr_to_scm_no_free (GList *account_list)
{
  return gnc_glist_account_ptr_to_scm_internal (account_list, FALSE);
}

/********************************************************************\
 * gnc_scm_to_account_list                                          *
 *   Turn an SCM into a g_malloc's account list                     *
 *                                                                  *
 * Args: scm_list - SCM list of accounts                            *
 * Return: GList of accounts                                        *
\********************************************************************/
GList *
gnc_scm_to_glist_account_ptr(SCM scm_list)
{
  return gnc_scm_list_to_glist(scm_list);
}

/********************************************************************
 * gnc_glist_account_ptr_p
 ********************************************************************/

int
gnc_glist_account_ptr_p(SCM list)
{
  return gh_list_p(list);
}

static SCM
gnc_glist_transaction_ptr_to_scm_internal (GList *trans_list,
                                           gboolean free_list)
{
  static SCM trans_type = SCM_UNDEFINED;
  SCM result;

  if (trans_type == SCM_UNDEFINED)
  {
    trans_type = gh_eval_str("<gnc:Transaction*>");
    /* don't really need this - types are bound globally anyway. */
    if(trans_type != SCM_UNDEFINED) scm_protect_object(trans_type);
  }

  result = gnc_glist_to_scm_list(trans_list, trans_type);

  if (free_list)
    g_list_free (trans_list);

  return result;
}

SCM
gnc_glist_transaction_ptr_to_scm (GList *transaction_list)
{
  return gnc_glist_transaction_ptr_to_scm_internal (transaction_list, TRUE);
}

SCM
gnc_glist_transaction_ptr_to_scm_no_free (GList *transaction_list)
{
  return gnc_glist_transaction_ptr_to_scm_internal (transaction_list, FALSE);
}

GList *
gnc_scm_to_glist_transaction_ptr (SCM scm_list)
{
  return gnc_scm_list_to_glist (scm_list);
}

int
gnc_glist_transaction_ptr_p (SCM list)
{
  return gh_list_p (list);
}

static SCM
gnc_glist_split_ptr_to_scm_internal (GList *trans_list,
                                     gboolean free_list)
{
  static SCM trans_type = SCM_UNDEFINED;
  SCM result;

  if (trans_type == SCM_UNDEFINED)
  {
    trans_type = gh_eval_str("<gnc:Split*>");
    /* don't really need this - types are bound globally anyway. */
    if(trans_type != SCM_UNDEFINED) scm_protect_object(trans_type);
  }

  result = gnc_glist_to_scm_list(trans_list, trans_type);

  if (free_list)
    g_list_free (trans_list);

  return result;
}

SCM
gnc_glist_split_ptr_to_scm (GList *split_list)
{
  return gnc_glist_split_ptr_to_scm_internal (split_list, TRUE);
}

SCM
gnc_glist_split_ptr_to_scm_no_free (GList *split_list)
{
  return gnc_glist_split_ptr_to_scm_internal (split_list, FALSE);
}

GList *
gnc_scm_to_glist_split_ptr (SCM scm_list)
{
  return gnc_scm_list_to_glist (scm_list);
}

int
gnc_glist_split_ptr_p (SCM list)
{
  return gh_list_p (list);
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

/********************************************************************
 * gnc_glist_commodity_ptr_to_scm
 ********************************************************************/
SCM
gnc_glist_commodity_ptr_to_scm(GList * l)
{
  static SCM commodity_type = SCM_UNDEFINED;
  SCM result_list;

  if(commodity_type == SCM_UNDEFINED) {
    commodity_type = gh_eval_str("<gnc:commodity*>");
    /* don't really need this - types are bound globally anyway. */
    if(commodity_type != SCM_UNDEFINED) scm_protect_object(commodity_type);
  }
  result_list = gnc_glist_to_scm_list(l, commodity_type);
  g_list_free(l);
  return result_list;
}

/********************************************************************
 * gnc_scm_to_glist_commodity_ptr
 ********************************************************************/

GList *
gnc_scm_to_glist_commodity_ptr(SCM scm_list)
{
  return gnc_scm_list_to_glist(scm_list);
}

/********************************************************************
 * gnc_glist_commodity_ptr_p
 ********************************************************************/

int
gnc_glist_commodity_ptr_p(SCM list)
{
  return gh_list_p(list);
}

/********************************************************************
 * gnc_glist_price_ptr_to_scm
 ********************************************************************/
SCM
gnc_glist_price_ptr_to_scm(GList * l)
{
  static SCM price_type = SCM_UNDEFINED;
  SCM result_list;

  if(price_type == SCM_UNDEFINED) {
    price_type = gh_eval_str("<gnc:Price*>");
    /* don't really need this - types are bound globally anyway. */
    if(price_type != SCM_UNDEFINED) scm_protect_object(price_type);
  }
  result_list = gnc_glist_to_scm_list(l, price_type);
  g_list_free(l);
  return result_list;
}

/********************************************************************
 * gnc_scm_to_glist_price_ptr
 ********************************************************************/

GList *
gnc_scm_to_glist_price_ptr(SCM scm_list)
{
  return gnc_scm_list_to_glist(scm_list);
}

/********************************************************************
 * gnc_glist_price_ptr_p
 ********************************************************************/

int
gnc_glist_price_ptr_p(SCM list)
{
  return gh_list_p(list);
}

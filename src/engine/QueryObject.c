/*
 * QueryObject.c -- provide Gnucash Queriable data objects
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
#include "QueryObjectP.h"
#include "QueryNew.h"

#include "Account.h"
#include "Transaction.h"

static short module = MOD_QUERY;

static GHashTable *paramTable = NULL;
static GHashTable *sortTable = NULL;
static gboolean initialized = FALSE;

static gpointer split_account_guid_getter (gpointer obj)
{
  Split *s = obj;
  Account *acc;

  if (!s) return NULL;
  acc = xaccSplitGetAccount (s);
  if (!acc) return NULL;
  return ((gpointer)xaccAccountGetGUID (acc));
}

static void init_split (void)
{
  static const QueryObjectDef params[] = {
    { SPLIT_KVP, QUERYCORE_KVP, (QueryAccess)xaccSplitGetSlots },
    { SPLIT_GUID, QUERYCORE_GUID, (QueryAccess) xaccSplitGetGUID },
    { SPLIT_DATE_RECONCILED, QUERYCORE_DATE,
      (QueryAccess)xaccSplitRetDateReconciledTS },
    { "d-share-amount", QUERYCORE_DOUBLE,
      (QueryAccess)DxaccSplitGetShareAmount },
    { "d-share-int64", QUERYCORE_INT64, (QueryAccess)xaccSplitGetGUID },
    { SPLIT_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccSplitGetBalance },
    { SPLIT_CLEARED_BALANCE, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitGetClearedBalance },
    { SPLIT_RECONCILED_BALANCE, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitGetReconciledBalance },
    { SPLIT_MEMO, QUERYCORE_STRING, (QueryAccess)xaccSplitGetMemo },
    { SPLIT_ACTION, QUERYCORE_STRING, (QueryAccess)xaccSplitGetAction },
    { SPLIT_RECONCILE, QUERYCORE_CHAR, (QueryAccess)xaccSplitGetReconcile },
    { SPLIT_AMOUNT, QUERYCORE_NUMERIC, (QueryAccess)xaccSplitGetAmount },
    { SPLIT_SHARE_PRICE, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitGetSharePrice },
    { SPLIT_VALUE, QUERYCORE_DEBCRED, (QueryAccess)xaccSplitGetValue },
    { SPLIT_TYPE, QUERYCORE_STRING, (QueryAccess)xaccSplitGetType },
    { SPLIT_VOIDED_AMOUNT, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitVoidFormerAmount },
    { SPLIT_VOIDED_VALUE, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitVoidFormerValue },
    { SPLIT_TRANS, GNC_ID_TRANS, (QueryAccess)xaccSplitGetParent },
    { SPLIT_ACCOUNT, GNC_ID_ACCOUNT, (QueryAccess)xaccSplitGetAccount },
    { SPLIT_ACCOUNT_GUID, QUERYCORE_GUID, split_account_guid_getter },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)xaccSplitGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_SPLIT, (QuerySort)xaccSplitDateOrder, params);
}

static void init_txn (void)
{
  static QueryObjectDef params[] = {
    { TRANS_KVP, QUERYCORE_KVP, (QueryAccess)xaccTransGetSlots },
    { TRANS_GUID, QUERYCORE_GUID, (QueryAccess)xaccTransGetGUID },
    { TRANS_NUM, QUERYCORE_STRING, (QueryAccess)xaccTransGetNum },
    { TRANS_DESCRIPTON, QUERYCORE_STRING, (QueryAccess)xaccTransGetDescription },
    { TRANS_DATE_ENTERED, QUERYCORE_DATE, (QueryAccess)xaccTransRetDateEnteredTS },
    { TRANS_DATE_POSTED, QUERYCORE_DATE, (QueryAccess)xaccTransRetDatePostedTS },
    { TRANS_DATE_DUE, QUERYCORE_DATE, (QueryAccess)xaccTransRetDateDueTS },
    { TRANS_TYPE, QUERYCORE_CHAR, (QueryAccess)xaccTransGetTxnType },
    { TRANS_VOID_STATUS, QUERYCORE_BOOLEAN, (QueryAccess)xaccTransGetVoidStatus },
    { TRANS_VOID_REASON, QUERYCORE_STRING, (QueryAccess)xaccTransGetVoidReason },
    { TRANS_VOID_TIME, QUERYCORE_DATE, (QueryAccess)xaccTransGetVoidTime },
    { TRANS_SPLITLIST, GNC_ID_SPLIT, (QueryAccess)xaccTransGetSplitList },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)xaccTransGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_TRANS, (QuerySort)xaccTransOrder, params);
}

static void init_account (void)
{
  static QueryObjectDef params[] = {
    { ACCOUNT_KVP, QUERYCORE_KVP, (QueryAccess)xaccAccountGetSlots },
    { ACCOUNT_GUID, QUERYCORE_GUID, (QueryAccess)xaccAccountGetGUID },
    { ACCOUNT_NAME_, QUERYCORE_STRING, (QueryAccess)xaccAccountGetName },
    { ACCOUNT_CODE_, QUERYCORE_STRING, (QueryAccess)xaccAccountGetCode },
    { ACCOUNT_DESCRIPTION_, QUERYCORE_STRING, (QueryAccess)xaccAccountGetDescription },
    { ACCOUNT_NOTES_, QUERYCORE_STRING, (QueryAccess)xaccAccountGetNotes },
    { ACCOUNT_BALANCE_, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetBalance },
    { ACCOUNT_CLEARED_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetClearedBalance },
    { ACCOUNT_RECONCILED_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetReconciledBalance },
    { ACCOUNT_TAX_RELATED, QUERYCORE_BOOLEAN, (QueryAccess)xaccAccountGetTaxRelated },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)xaccAccountGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_ACCOUNT, (QuerySort)xaccAccountOrder, params);
}

static void init_book (void)
{
  static QueryObjectDef params[] = {
    { BOOK_KVP, QUERYCORE_KVP, (QueryAccess)gnc_book_get_slots },
    { BOOK_GUID, QUERYCORE_GUID, (QueryAccess)gnc_book_get_guid },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_BOOK, NULL, params);
}

static void init_tables (void)
{
  init_split ();
  init_txn ();
  init_account ();
  init_book ();
}

static gboolean clear_table (gpointer key, gpointer value, gpointer user_data)
{
  g_hash_table_destroy (value);
  return TRUE;
}

/********************************************************************/
/* PUBLISHED API FUNCTIONS */

void gncQueryObjectRegister (GNCIdTypeConst obj_name,
			     QuerySort default_sort_function,
			     const QueryObjectDef *params)
{
  int i;

  if (!obj_name) return;

  if (default_sort_function)
    g_hash_table_insert (sortTable, (char *)obj_name, default_sort_function);

  if (params) {
    GHashTable *ht = g_hash_table_lookup (paramTable, obj_name);

    /* If it doesn't already exist, create a new table for this object */
    if (!ht) {
      ht = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (paramTable, (char *)obj_name, ht);
    }

    /* Now insert all the parameters */
    for (i = 0; params[i].param_name; i++)
      g_hash_table_insert (ht,
			   (char *)params[i].param_name,
			   (gpointer)&(params[i]));
  }
}

void gncQueryObjectInit(void)
{
  if (initialized) return;
  initialized = TRUE;

  paramTable = g_hash_table_new (g_str_hash, g_str_equal);
  sortTable = g_hash_table_new (g_str_hash, g_str_equal);

  init_tables ();
}

void gncQueryObjectShutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_foreach_remove (paramTable, clear_table, NULL);
  g_hash_table_destroy (paramTable);
  g_hash_table_destroy (sortTable);
}


const QueryObjectDef * gncQueryObjectGetParameter (GNCIdTypeConst obj_name,
						   const char *parameter)
{
  GHashTable *ht;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  ht = g_hash_table_lookup (paramTable, obj_name);
  g_return_val_if_fail (ht, NULL);

  return (g_hash_table_lookup (ht, parameter));
}

QueryAccess gncQueryObjectGetParameterGetter (GNCIdTypeConst obj_name,
					      const char *parameter)
{
  const QueryObjectDef *obj;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  obj = gncQueryObjectGetParameter (obj_name, parameter);
  if (obj)
    return obj->param_getfcn;

  return NULL;
}

QueryCoreType gncQueryObjectParameterType (GNCIdTypeConst obj_name,
					   const char *param_name)
{
  const QueryObjectDef *obj;

  if (!obj_name || !param_name) return NULL;

  obj = gncQueryObjectGetParameter (obj_name, param_name);
  if (!obj) return NULL;

  return (obj->param_type);
}

QuerySort gncQueryObjectDefaultSort (GNCIdTypeConst obj_name)
{
  if (!obj_name) return NULL;
  return g_hash_table_lookup (sortTable, obj_name);
}

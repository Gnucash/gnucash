/*
 * QueryObject.c -- provide Gnucash Queriable data objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#include "config.h"

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
static GHashTable *convTable = NULL;
static GHashTable *sortTable = NULL;
static gboolean initialized = FALSE;

typedef enum {
  TYPE_PARAM = 1,
  TYPE_CONV
} QueryObjectType;

/* Stupid function to perform a no-op, to make the interface clean */
static gpointer self_convert (gpointer obj)
{
  return obj;
}

static GHashTable * get_object_table (GNCIdType name, QueryObjectType type)
{
  GHashTable *ht = NULL, *obj_ht = NULL;

  g_return_val_if_fail (name, NULL);
  g_return_val_if_fail (initialized, NULL);

  switch (type) {
  case TYPE_PARAM:
    ht = paramTable;
    break;
  case TYPE_CONV:
    ht = convTable;
  }

  if (!ht) {
    PWARN ("Aiee -- no hash table");
    return NULL;
  }

  obj_ht = g_hash_table_lookup (ht, name);

  /* If it doesn't already exist, create a new table for this object */
  if (!obj_ht) {
    obj_ht = g_hash_table_new (g_str_hash, g_str_equal);
    g_hash_table_insert (ht, (char *)name, obj_ht);
  }

  return obj_ht;
}

static void insert_method (GNCIdType objname, const char *param,
			   gconstpointer method, QueryObjectType type)
{
  GHashTable *ht;

  g_return_if_fail (objname);
  g_return_if_fail (param);
  g_return_if_fail (method);

  ht = get_object_table (objname, type);
  g_return_if_fail (ht);

  g_hash_table_insert (ht, (char *)param, (gpointer)method);
}

static void init_split (void)
{
  static const QueryObjectDef params[] = {
    { SPLIT_KVP, QUERYCORE_KVP, (QueryAccess)xaccSplitGetSlots },
    { SPLIT_GUID, QUERYCORE_GUID, (QueryAccess) xaccSplitGetGUID },
    { SPLIT_DATE_RECONCILED, QUERYCORE_DATE,
      (QueryAccess)xaccSplitRetDateReconciledTS },
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
    { SPLIT_VALUE, QUERYCORE_NUMERIC, (QueryAccess)xaccSplitGetValue },
    { SPLIT_TYPE, QUERYCORE_STRING, (QueryAccess)xaccSplitGetType },
    { SPLIT_VOIDED_AMOUNT, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitVoidFormerAmount },
    { SPLIT_VOIDED_VALUE, QUERYCORE_NUMERIC,
      (QueryAccess)xaccSplitVoidFormerValue },
    { NULL },
  };
  static const QueryConvertDef converters[] = {
    { GNC_ID_TRANS, (QueryConvert)xaccSplitGetParent },
    { GNC_ID_ACCOUNT, (QueryConvert)xaccSplitGetAccount },
    { GNC_ID_BOOK, (QueryConvert)xaccSplitGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_SPLIT, (QuerySort)xaccSplitDateOrder,
			  params, converters);
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
    { NULL },
  };
  static const QueryConvertDef converters[] = {
    { GNC_ID_BOOK, (QueryConvert)xaccTransGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_TRANS, (QuerySort)xaccTransOrder,
			  params, converters);
}

static void init_account (void)
{
  static QueryObjectDef params[] = {
    { ACCOUNT_KVP, QUERYCORE_KVP, (QueryAccess)xaccAccountGetSlots },
    { ACCOUNT_GUID, QUERYCORE_GUID, (QueryAccess)xaccAccountGetGUID },
    { ACCOUNT_NAME, QUERYCORE_STRING, (QueryAccess)xaccAccountGetName },
    { ACCOUNT_CODE, QUERYCORE_STRING, (QueryAccess)xaccAccountGetCode },
    { ACCOUNT_DESCRIPTION, QUERYCORE_STRING, (QueryAccess)xaccAccountGetDescription },
    { ACCOUNT_NOTES, QUERYCORE_STRING, (QueryAccess)xaccAccountGetNotes },
    { ACCOUNT_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetBalance },
    { ACCOUNT_CLEARED_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetClearedBalance },
    { ACCOUNT_RECONCILED_BALANCE, QUERYCORE_NUMERIC, (QueryAccess)xaccAccountGetReconciledBalance },
    { ACCOUNT_TAX_RELATED, QUERYCORE_BOOLEAN, (QueryAccess)xaccAccountGetTaxRelated },
    { NULL },
  };
  static const QueryConvertDef converters[] = {
    { GNC_ID_BOOK, (QueryConvert)xaccAccountGetBook },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_ACCOUNT, (QuerySort)xaccAccountOrder,
			  params, converters);
}

static void init_book (void)
{
  static QueryObjectDef params[] = {
    { BOOK_KVP, QUERYCORE_KVP, (QueryAccess)gnc_book_get_slots },
    { BOOK_GUID, QUERYCORE_GUID, (QueryAccess)gnc_book_get_guid },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_BOOK, NULL, params, NULL);
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

void gncQueryObjectRegister (GNCIdType  obj_name,
			     QuerySort default_sort_function,
			     const QueryObjectDef *params,
			     const QueryConvertDef *converters)
{
  int i;

  if (!obj_name) return;

  if (default_sort_function)
    g_hash_table_insert (sortTable, (char *)obj_name, default_sort_function);

  if (params) {
    for (i = 0; params[i].param_name; i++)
      insert_method (obj_name, params[i].param_name, &(params[i]), TYPE_PARAM);
  }

  if (converters) {
    for (i = 0; converters[i].desired_object_name; i++)
      insert_method (obj_name, converters[i].desired_object_name,
		     &(converters[i]), TYPE_CONV);
  }
}

void gncQueryObjectInit(void)
{
  if (initialized) return;
  initialized = TRUE;

  paramTable = g_hash_table_new (g_str_hash, g_str_equal);
  convTable = g_hash_table_new (g_str_hash, g_str_equal);
  sortTable = g_hash_table_new (g_str_hash, g_str_equal);

  init_tables ();
}

void gncQueryObjectShutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_foreach_remove (paramTable, clear_table, NULL);
  g_hash_table_destroy (paramTable);

  g_hash_table_foreach_remove (convTable, clear_table, NULL);
  g_hash_table_destroy (convTable);

  g_hash_table_destroy (sortTable);
}


const QueryObjectDef * gncQueryObjectGetParameter (GNCIdType obj_name,
						   const char *parameter)
{
  GHashTable *ht;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  ht = get_object_table (obj_name, TYPE_PARAM);
  g_return_val_if_fail (ht, NULL);

  return (g_hash_table_lookup (ht, parameter));
}

QueryAccess gncQueryObjectGetParamaterGetter (GNCIdType obj_name,
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

QueryConvert gncQueryObjectGetConverter (GNCIdType from_obj,
					 GNCIdType to_obj)
{
  GHashTable *ht;
  QueryConvertDef *conv;

  g_return_val_if_fail (from_obj, NULL);
  g_return_val_if_fail (to_obj, NULL);

  if (from_obj == to_obj || !safe_strcmp (from_obj, to_obj))
    return self_convert;

  ht = get_object_table (from_obj, TYPE_CONV);
  g_return_val_if_fail (ht, NULL);

  conv = g_hash_table_lookup (ht, to_obj);
  if (conv)
    return conv->object_getfcn;

  return NULL;
}

QueryCoreType gncQueryObjectParameterType (GNCIdType obj_name,
					   const char *param_name)
{
  const QueryObjectDef *obj;

  if (!obj_name || !param_name) return NULL;

  obj = gncQueryObjectGetParameter (obj_name, param_name);
  if (!obj) return NULL;

  return (obj->param_type);
}

QuerySort gncQueryObjectDefaultSort (GNCIdType obj_name)
{
  if (!obj_name) return NULL;
  return g_hash_table_lookup (sortTable, obj_name);
}

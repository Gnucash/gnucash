/*
 * FILE:
 * RpcUtils.c
 *
 * FUNCTION:
 * Implements some utility functions for the RPC (client) backend.
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#include "RpcUtils.h"
#include "AccountP.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "gnc-engine-util.h"

struct _txnlistinfo {
  gnc_txnlist **end;
  int count;
};

struct _acclistinfo {
  gnc_acctlist **end;
  int count;
};

struct _listinfo {
  gnc_vers_list **end;
  gboolean	copyguid;
  int 		count;
};

struct _kvpinfo {
  gnc_kvp_frame **end;
};

#define STRDUP(x)  ((x) ? strdup (x) : strdup (""))

static short module = MOD_BACKEND;

static void rpcend_free_kvp_value (gnc_kvp_value *val)
{
  switch (val->type) {
  case KVP_TYPE_GINT64:
  case KVP_TYPE_DOUBLE:
  case KVP_TYPE_NUMERIC:
  case KVP_TYPE_GUID:
    free (val);
    break;
  case KVP_TYPE_STRING:
    free (val->gnc_kvp_value_u.str);
    free (val);
    break;
  case KVP_TYPE_BINARY:
    free (val->gnc_kvp_value_u.binary.binary_val);
    free (val);
    break;
  case KVP_TYPE_GLIST:
    {
      gnc_kvp_valuelist *lst, *next;

      for (lst = val->gnc_kvp_value_u.glist; lst != NULL; lst = next) {
	next = lst->next;
	rpcend_free_kvp_value (lst->val);
	free (lst);
      }
      free (val);
    }
    break;
  case KVP_TYPE_FRAME:
    rpcend_free_gnckvp (val->gnc_kvp_value_u.frame);
    free (val);
    break;
  default:
    /* EEP! Unknown type */
  }
}

void rpcend_free_gnckvp (gnc_kvp_frame *frame)
{
  gnc_kvp *data;
  gnc_kvp_frame *next;

  for (; frame != NULL; frame = next) {
    next = frame->next;
    data = frame->data;
    rpcend_free_kvp_value (data->value);
    free (data->key);
    free (data);
    free (frame);
  }
}

static gnc_kvp_value * rpcend_build_kvp_value (kvp_value *val)
{
  gnc_kvp_value *	this_val = NULL;
  kvp_value_t 		val_type = kvp_value_get_type (val);

  switch (val_type) {
  case KVP_TYPE_GINT64:
  case KVP_TYPE_DOUBLE:
  case KVP_TYPE_NUMERIC:
    this_val = malloc (sizeof (*this_val));
    this_val->type = val_type;
    switch (val_type) {
    case KVP_TYPE_GINT64:
      this_val->gnc_kvp_value_u.int64 = kvp_value_get_gint64 (val);
      break;
    case KVP_TYPE_DOUBLE:
      this_val->gnc_kvp_value_u.dbl = kvp_value_get_double (val);
      break;
    case KVP_TYPE_NUMERIC:
      {
	gnc_numeric num = kvp_value_get_numeric (val);

	memcpy (&(this_val->gnc_kvp_value_u.numeric), &num,
		sizeof (gncNumeric));
	break;
      }
    default:
    }
    break;
  case KVP_TYPE_GUID:
    {
      GUID *guid = kvp_value_get_guid (val);

      this_val = malloc (sizeof (*this_val));
      this_val->type = KVP_TYPE_GUID;
      memcpy (&(this_val->gnc_kvp_value_u.guid), guid->data, sizeof (gncGUID));
      break;
    }
  case KVP_TYPE_STRING:
    {
      char *str = kvp_value_get_string (val);

      this_val = malloc (sizeof (*this_val));
      this_val->type = KVP_TYPE_STRING;
      this_val->gnc_kvp_value_u.str = strdup (str);
      break;
    }
  case KVP_TYPE_BINARY:
    {
      char *bytes;
      guint64 len;

      this_val = malloc (sizeof (*this_val));
      this_val->type = KVP_TYPE_BINARY;
      bytes = kvp_value_get_binary (val, &len);
      this_val->gnc_kvp_value_u.binary.binary_val = malloc (len);
      memcpy (this_val->gnc_kvp_value_u.binary.binary_val, bytes, len);
      this_val->gnc_kvp_value_u.binary.binary_len = len;
    }
    break;
  case KVP_TYPE_GLIST:
    /* A list of KVP_VALUES */
    {
      GList *lst = kvp_value_get_glist (val);
      gnc_kvp_valuelist *vlist = NULL, **vlistend = &vlist;

      for (; lst != NULL; lst = g_list_next (lst)) {
	gnc_kvp_value * new_val;

	new_val = rpcend_build_kvp_value ((kvp_value *)lst->data);
	if (new_val != NULL) {
	  gnc_kvp_valuelist *this_item = malloc (sizeof (*this_item));
	  this_item->val = new_val;
	  this_item->next = NULL;
	  *vlistend = this_item;
	  vlistend = &(this_item->next);
	}
      }

      if (vlist != NULL) {
	this_val = malloc (sizeof (*this_val));
	this_val->type = KVP_TYPE_GLIST;
	this_val->gnc_kvp_value_u.glist = vlist;
      }
    }
    break;
  case KVP_TYPE_FRAME:
    this_val = malloc (sizeof (*this_val));
    this_val->type = KVP_TYPE_FRAME;
    this_val->gnc_kvp_value_u.frame =
      rpcend_build_gnckvp (kvp_value_get_frame (val));
    break;
  default:
    /* EEP! Unknown type */
  }
  return this_val;
}

static void rpcend_build_kvp_frame (gpointer keyp, gpointer valp,
				    gpointer rpckvp)
{
  kvp_value *val = (kvp_value *) valp;
  struct _kvpinfo *	info = (struct _kvpinfo *) rpckvp;
  gnc_kvp_value *	this_val;

  this_val = rpcend_build_kvp_value (val);

  if (this_val != NULL) {
    gnc_kvp_frame *	this_frame;
    gnc_kvp * 		this_data;

    this_frame = malloc (sizeof (*this_frame));
    this_data = malloc (sizeof (*this_data));

    this_data->key = STRDUP ((char *)keyp);
    this_data->value = this_val;

    this_frame->data = this_data;
    this_frame->next = NULL;

    *(info->end) = this_frame;
    (info->end) = &(this_frame->next);
  }
}

gnc_kvp_frame *rpcend_build_gnckvp (kvp_frame *frame)
{
  gnc_kvp_frame *rpckv = NULL;
  GHashTable *hash;

  if (!frame)
    return NULL;

  hash = kvp_frame_get_hash (frame);
  if (hash) {
    struct _kvpinfo info;
    info.end = &rpckv;

    g_hash_table_foreach (hash, rpcend_build_kvp_frame, &info);
  }

  return rpckv;
}

static kvp_value *rpcend_parse_kvp_value (gnc_kvp_value *data)
{
  kvp_value *kv = NULL;

  switch (data->type) {
  case KVP_TYPE_GINT64:
    kv = kvp_value_new_gint64 (data->gnc_kvp_value_u.int64);
    break;
  case KVP_TYPE_DOUBLE:
    kv = kvp_value_new_double (data->gnc_kvp_value_u.dbl);
    break;
  case KVP_TYPE_NUMERIC:
    kv = kvp_value_new_gnc_numeric (*((gnc_numeric *)&(data->gnc_kvp_value_u.numeric)));
    break;
  case KVP_TYPE_STRING:
    kv = kvp_value_new_string (data->gnc_kvp_value_u.str);
    break;
  case KVP_TYPE_GUID:
    kv = kvp_value_new_guid ((GUID *)&(data->gnc_kvp_value_u.guid));
    break;
  case KVP_TYPE_BINARY:
    kv = kvp_value_new_binary ((void *)
			       data->gnc_kvp_value_u.binary.binary_val,
			       data->gnc_kvp_value_u.binary.binary_len);
    break;
  case KVP_TYPE_GLIST:
    /* A list of KVP_VALUES */
    {
      gnc_kvp_valuelist *vlist;
      GList *lst = NULL;

      for (vlist = data->gnc_kvp_value_u.glist; vlist != NULL;
	   vlist = vlist->next) {
	kvp_value *kvv = NULL;

	/* Grab the type, and append to the list (if it exists) */
	kvv = rpcend_parse_kvp_value (vlist->val);
	if (kvv != NULL)
	  lst = g_list_append (lst, kvv);
      }
      /* when we're done, make a new glist kvp_value */
      if (lst != NULL)
	kv = kvp_value_new_glist_nc (lst);
    }
    break;
  case KVP_TYPE_FRAME:
    kv = kvp_value_new_frame (rpcend_parse_gnckvp
			      (data->gnc_kvp_value_u.frame));
    break;
  default:
    /* EEP! Unknown type */
  }
  return kv;
}

kvp_frame * rpcend_parse_gnckvp (gnc_kvp_frame *data)
{
  kvp_frame *frame = NULL;

  for (; data != NULL; data = data->next) {
    kvp_value *kv;

    if (frame == NULL)
      frame = kvp_frame_new ();

    kv = rpcend_parse_kvp_value (data->data->value);

    if (kv != NULL)
      kvp_frame_set_slot_nc (frame, data->data->key, kv);
  }
  return frame;
}

static gnc_splitlist * rpcend_build_splitlist (GList *lst)
{
  gnc_splitlist *slist = NULL, **endlist = &slist;

  for (; lst != NULL; lst = g_list_next (lst)) {
    Split *olds = (Split *)lst->data;
    gncSplit *split = NULL;
    gnc_splitlist *new;

    if (olds == NULL)
      continue;

    split = malloc (sizeof (*split));
    memset (split, 0, sizeof (*split));
    memcpy (split->guid, olds->guid.data, sizeof (split->guid));
    {
      Account *acc = xaccSplitGetAccount (olds);
      if (!acc) {
	PERR ("account not found for this split\n");
      } else {
	memcpy (split->acct_guid, acc->guid.data, sizeof (split->acct_guid));
      }
    }
    memcpy (split->txn_guid, olds->parent->guid.data, sizeof (split->txn_guid));
    split->memo = STRDUP (olds->memo);
    split->action = STRDUP (olds->action);
    split->kvp_data = rpcend_build_gnckvp (olds->kvp_data);
    split->reconciled = olds->reconciled;
    split->date_reconciled = *((gncTimespec *)&(olds->date_reconciled));
    split->value = *((gncNumeric *)&(olds->value));
    split->damount = *((gncNumeric *)&(olds->damount));

    new = malloc (sizeof (*new));
    new->split = split;
    new->next = NULL;
    *endlist = new;
    endlist = &(new->next);
  }

  return slist;
}

static void rpcend_free_splitlist (gnc_splitlist *lst)
{
  gnc_splitlist *next;

  for (; lst != NULL; lst = next) {
    gncSplit *split = lst->split;

    next = lst->next;
    rpcend_free_gnckvp (split->kvp_data);
    if (split->memo)
      free (split->memo);
    if (split->action)
      free (split->action);
    free (split);
    free (lst);
  }
}

void rpcend_build_gnctxn (gncTransaction *rpctxn, Transaction *txn)
{
  gnc_commodity *c;

  if (!rpctxn || !txn)
    return;

  c = xaccTransGetCurrency (txn);
  ENTER ("%p, com=%p", txn, c);

  /* Copy the Transaction information */
  memcpy (rpctxn->guid, txn->guid.data, sizeof (rpctxn->guid));
  rpctxn->date_entered = *((gncTimespec *) &(txn->date_entered));
  rpctxn->date_posted = *((gncTimespec*) &(txn->date_posted));
  rpctxn->num = STRDUP (txn->num);
  rpctxn->desc = STRDUP (txn->description);
  /* build kvp list */ 
  rpctxn->kvp_data = rpcend_build_gnckvp (txn->kvp_data);
  rpctxn->common_currency.namespace = STRDUP (gnc_commodity_get_namespace (c));
  rpctxn->common_currency.mnemonic = STRDUP (gnc_commodity_get_mnemonic (c));
  rpctxn->vers = txn->version;
  /* build splitlist */
  rpctxn->splits = rpcend_build_splitlist (txn->splits);
  rpctxn->do_free = txn->do_free;
  LEAVE ("ok");
}

void rpcend_free_gnctxn (gncTransaction *txn, gboolean freetxn)
{
  if (txn == NULL)
    return;

  if (txn->splits)
    rpcend_free_splitlist (txn->splits);

  if (txn->kvp_data)
    rpcend_free_gnckvp (txn->kvp_data);

  if (txn->num)
    free (txn->num);

  if (txn->desc)
    free (txn->desc);
  
  if (txn->common_currency.namespace)
    free (txn->common_currency.namespace);

  if (txn->common_currency.mnemonic)
    free (txn->common_currency.mnemonic);

  if (freetxn)
    free (txn);
}

void rpcend_build_gncacct (gncAccount *gncacct, Account *acc)
{
  if (!gncacct || !acc)
    return;

  memcpy (gncacct->guid, acc->guid.data, sizeof (gncacct->guid));
  gncacct->name = STRDUP (acc->accountName);
  gncacct->code = STRDUP (acc->accountCode);
  gncacct->desc = STRDUP (acc->description);
  gncacct->kvp_data = rpcend_build_gnckvp (acc->kvp_data);
  gncacct->type = acc->type;
  /* Leave children blank */
  gncacct->vers = acc->version;
  gncacct->core_dirty = acc->core_dirty;
  gncacct->do_free = acc->do_free;
  {
    gnc_commodity *c;
#ifdef GNCACCT_COMMODITY
    c = xaccAccountGetCommodity (acc);
    if (!c)
      PWARN ("Account (%s) had NULL commodity",
             gncacct->name ? gncacct->name : "");
    gncacct->commodity.namespace = STRDUP (gnc_commodity_get_namespace (c));
    gncacct->commodity.mnemonic = STRDUP (gnc_commodity_get_mnemonic (c));
#else
    c = xaccAccountGetCurrency (acc);
    gncacct->currency.namespace = STRDUP (gnc_commodity_get_namespace (c));
    gncacct->currency.mnemonic = STRDUP (gnc_commodity_get_mnemonic (c));

    c = xaccAccountGetSecurity (acc);
    gncacct->security.namespace = STRDUP (gnc_commodity_get_namespace (c));
    gncacct->security.mnemonic = STRDUP (gnc_commodity_get_mnemonic (c));
#endif
  }
  {
    /* Determine parent GUID, if there is a parent account */
    Account *a = xaccGroupGetParentAccount (acc->parent);
    if (a == NULL) {
      /* top-level account */
      gncacct->parent = NULL;
    } else {
      gncacct->parent = malloc (sizeof (gncGUID));
      memcpy (gncacct->parent, a->guid.data, sizeof (gncGUID));
    }
  }

  /* Copy balances */
  gncacct->balance = *((gncNumeric *)&(acc->balance));
  gncacct->cleared_balance = *((gncNumeric *)&(acc->cleared_balance));
  gncacct->reconciled_balance = *((gncNumeric *)&(acc->reconciled_balance));
  gncacct->share_balance = *((gncNumeric *)&(acc->share_balance));
  gncacct->share_cleared_balance =
    *((gncNumeric *)&(acc->share_cleared_balance));
  gncacct->share_reconciled_balance =
    *((gncNumeric *)&(acc->share_reconciled_balance));
}

void rpcend_free_gncacct (gncAccount *acc, gboolean freeacct)
{
  if (!acc)
    return;

  if (acc->name)
    free (acc->name);

  if (acc->code)
    free (acc->code);

  if (acc->desc)
    free (acc->desc);

  if (acc->kvp_data)
    rpcend_free_gnckvp (acc->kvp_data);

#ifdef GNCACCT_COMMODITY
  if (acc->commodity.namespace)
    free (acc->commodity.namespace);
  if (acc->commodity.mnemonic)
    free (acc->commodity.namespace);
#else
  if (acc->currency.namespace)
    free (acc->currency.namespace);
  if (acc->currency.mnemonic)
    free (acc->currency.namespace);

  if (acc->security.namespace)
    free (acc->security.namespace);
  if (acc->security.mnemonic)
    free (acc->security.namespace);
#endif

  if (acc->parent)
    free (acc->parent);

  if (freeacct)
    free (acc);
}

static gpointer add_txnlist_cb (Transaction *t, void *data)
{
  struct _txnlistinfo *listinfo = (struct _txnlistinfo *) data;
  gnc_txnlist *new;
  gncTransaction *txn;

  if (!t || !data)
    return NULL;

  new = malloc (sizeof (*new));
  txn = malloc (sizeof (*txn));
  memset (txn, 0, sizeof (*txn));

  rpcend_build_gnctxn (txn, t);
  new->txn = txn;
  new->next = NULL;
  *(listinfo->end) = new;
  listinfo->end = &(new->next);
  listinfo->count++;
  return NULL;
}

gnc_txnlist * rpcend_build_gnctxnlist_list (AccountGroup *ag,
					    gnc_vers_list *txnlist)
{
  gnc_txnlist *tlist = NULL;
  struct _txnlistinfo listinfo;

  if (!ag || !txnlist)
    return NULL;

  ENTER ("Building txn list");

  listinfo.end = &tlist;
  listinfo.count = 0;

  for ( ; txnlist != NULL; txnlist = txnlist->next) {
    GUID *guid = (GUID *)txnlist->guid;
    Transaction *t = xaccTransLookup (guid);

    if (t)
      add_txnlist_cb (t, (void *)&listinfo);
  }
  LEAVE ("%d txns", listinfo.count);
  return tlist;
}

void rpcend_free_gnctxnlist (gnc_txnlist *txnlist)
{
  gnc_txnlist *next;

  for ( ; txnlist != NULL; txnlist = next) {
    next = txnlist->next;
    rpcend_free_gnctxn (txnlist->txn, TRUE);
    free (txnlist);
  }    
}

static gpointer add_acctlist_cb (Account *a, void *data)
{
  struct _acclistinfo *listinfo = (struct _acclistinfo *) data;
  gnc_acctlist *new;
  gncAccount *acc;

  if (!a || !data)
    return NULL;

  new = malloc (sizeof (*new));
  acc = malloc (sizeof (*acc));

  rpcend_build_gncacct (acc, a);
  new->acct = acc;
  new->next = NULL;
  *(listinfo->end) = new;
  listinfo->end = &(new->next);
  listinfo->count++;
  return NULL;
}

gnc_acctlist * rpcend_build_gncacctlist (AccountGroup *ag)
{
  gnc_acctlist *alist = NULL;
  struct _acclistinfo listinfo;

  ENTER ("ag=%p", ag);
  listinfo.end = &alist;
  listinfo.count = 0;
  xaccGroupForEachAccount (ag, add_acctlist_cb, (void *) &listinfo, TRUE);
  LEAVE ("%d accts", listinfo.count);
  return alist;
}

gnc_acctlist * rpcend_build_gncacctlist_list (AccountGroup *ag,
					      gnc_vers_list *acctlist)
{
  gnc_acctlist *alist = NULL;
  struct _acclistinfo listinfo;

  if (!ag || !acctlist)
    return NULL;

  ENTER ("ag=%p, list=%p", ag, acctlist);

  listinfo.end = &alist;
  listinfo.count = 0;

  for ( ; acctlist != NULL; acctlist = acctlist->next) {
    GUID *guid = (GUID *)acctlist->guid;
    Account *a = xaccAccountLookup (guid);

    if (a)
      add_acctlist_cb (a, (void *)&listinfo);
  }
  LEAVE ("%d accts", listinfo.count);
  return alist;
}

void rpcend_free_gncacctlist (gnc_acctlist *ag)
{
  gnc_acctlist *next;

  for ( ; ag != NULL; ag = next) {
    next = ag->next;
    rpcend_free_gncacct (ag->acct, TRUE);
    free (ag);
  }    
}

static gboolean add_to_verslist (struct _listinfo *listinfo,
				 GUID *real_guid, int version)
{
  gnc_vers_list *new = malloc (sizeof (*new));
  gncGUID *guid = NULL;
  
  if (listinfo->copyguid) {
    guid = malloc (sizeof (*guid));
    memcpy (guid, real_guid->data, sizeof (gncGUID));
  } else {
    guid = (gncGUID *) real_guid;
  }

  new->vers = version;
  new->guid = guid;
  new->next = NULL;
  *(listinfo->end) = new;
  listinfo->end = &(new->next);
  listinfo->count++;

  return TRUE;
}

static gboolean add_txnvers_cb (Transaction *t, void *data)
{
  if (!t || !data)
    return FALSE;

  return add_to_verslist ((struct _listinfo *)data, &(t->guid), t->version);
}

gnc_vers_list * rpcend_build_gncverslist_txn (GList *txnlist,
					      gboolean copyguid)
{
  gnc_vers_list *vlist = NULL;
  struct _listinfo listinfo;

  ENTER ("list=%p, copy=%s", txnlist, copyguid ? "true" : "false");
  listinfo.end = &vlist;
  listinfo.copyguid = copyguid;
  listinfo.count = 0;

  for ( ; txnlist; txnlist = g_list_next (txnlist)) {
    add_txnvers_cb ((Transaction *)txnlist->data, (void *) &listinfo);
  }

  LEAVE ("%d txns", listinfo.count);
  return vlist;
}

gnc_vers_list * rpcend_build_gnctxn_verslist (AccountGroup *ag,
					      gboolean copyguid)
{
  gnc_vers_list *vlist = NULL;
  struct _listinfo listinfo;

  ENTER ("ag=%p, copy=%s", ag, copyguid ? "true" : "false");
  listinfo.end = &vlist;
  listinfo.copyguid = copyguid;
  listinfo.count = 0;

  if (xaccGroupForEachTransaction (ag, add_txnvers_cb, (void *) &listinfo))
    /* Didn't traverse every txn once? */
    ;

  LEAVE ("%d txns", listinfo.count);
  return vlist;
}

static gpointer add_acctvers_cb (Account *a, void *data)
{
  if (!a || !data)
    return FALSE;

  add_to_verslist ((struct _listinfo *)data, &(a->guid), a->version);
  return NULL;
}

gnc_vers_list * rpcend_build_gncacct_verslist (AccountGroup *ag,
					       gboolean copyguid)
{
  gnc_vers_list *vlist = NULL;
  struct _listinfo listinfo;

  ENTER ("ag=%p, copy=%s", ag, copyguid ? "true" : "false");
  listinfo.end = &vlist;
  listinfo.copyguid = copyguid;
  listinfo.count = 0;

  xaccGroupForEachAccount (ag, add_acctvers_cb, (void *) &listinfo, TRUE);

  LEAVE ("%d accts", listinfo.count);
  return vlist;
}

void rpcend_free_verslist (gnc_vers_list *vlist, gboolean freeguid)
{
  gnc_vers_list *next;

  for (; vlist != NULL; vlist = next) {
    next = vlist->next;

    if (freeguid)
      free (vlist->guid);

    free (vlist);
  }    
}

void rpcend_load_gnccommodity (gnc_commodity_table *ct, gncCommodity *com)
{
  gnc_commodity *gc;

  if (!ct || !com)
    return;

  if ((gc = gnc_commodity_table_lookup (ct, com->namespace, com->mnemonic))
      == NULL) {
    gc = gnc_commodity_new (com->fullname, com->namespace,
			    com->mnemonic, com->exchange_code, com->fraction);
    gnc_commodity_table_insert (ct, gc);	
  }
}

void rpcend_load_commoditylist (gnc_commodity_table *ct,
				gnc_commoditylist *cl)
{
  if (!ct || !cl)
    return;

  for (; cl != NULL; cl = cl->next) {
    rpcend_load_gnccommodity (ct, cl->commodity);
  }
}

gnc_commoditylist * rpcend_build_gnccommoditylist (gnc_commodity_table *ct,
						   gboolean copycom)
{
  GList *ns, *this_ns;
  gnc_commoditylist *comlist = NULL, **endlist = &comlist;

  if (!ct)
    return NULL;

  ns = gnc_commodity_table_get_namespaces (ct);
  for (this_ns = ns; this_ns != NULL; this_ns = g_list_next (this_ns)) {
    const char *namespace = (char *) this_ns->data;
    GList *cl, *this_cl;

    /* Ignore all the ISO4217 commodities */
    if (!strcmp (namespace, GNC_COMMODITY_NS_ISO))
      continue;

    cl = gnc_commodity_table_get_commodities (ct, namespace);
    for (this_cl = cl; this_cl != NULL; this_cl = g_list_next (this_cl)) {
      gnc_commodity *c = (gnc_commodity *) this_cl->data;
      gncCommodity *new = NULL;
      gnc_commoditylist *newl;
      
      if (copycom) {
	char const *str;
	new = malloc (sizeof (*new));
	memset (new, 0, sizeof (*new));
	str = gnc_commodity_get_fullname (c);
	new->fullname = STRDUP (str);
	str = gnc_commodity_get_namespace (c);
	new->namespace = STRDUP (str);
	str = gnc_commodity_get_mnemonic (c);
	new->mnemonic = STRDUP (str);
	str = gnc_commodity_get_printname (c);
	new->printname = STRDUP (str);
	str = gnc_commodity_get_exchange_code (c);
	new->exchange_code = STRDUP (str);
	new->fraction = gnc_commodity_get_fraction (c);
      } else {
	new = (gncCommodity *)c;
      }
      newl = malloc (sizeof (*newl));
      newl->commodity = new;
      newl->next = NULL;
      *endlist = newl;
      endlist = &(newl->next);
    }
    if (cl)
      g_list_free (cl);
  }
  if (ns)
    g_list_free (ns);

  return comlist;
}

void rpcend_free_gnccommoditylist (gnc_commoditylist *clist, gboolean freecom)
{
  gnc_commoditylist *next;

  for ( ; clist != NULL; clist = next) {
    next = clist->next;

    if (freecom && clist->commodity) {
      if (clist->commodity->fullname)
	free (clist->commodity->fullname);
      if (clist->commodity->namespace)
	free (clist->commodity->namespace);
      if (clist->commodity->mnemonic)
	free (clist->commodity->mnemonic);
      if (clist->commodity->printname)
	free (clist->commodity->printname);
      if (clist->commodity->exchange_code)
	free (clist->commodity->exchange_code);
      free (clist->commodity);
    }

    free (clist);
  }
}

int rpcend_do_add_acct (AccountGroup *topgrp, gncAccount * acct,
			gnc_commodity_table *ct)
{
  Account *a = NULL;
  Account *parent;
  GUID *guid = (GUID *)&(acct->guid);
  GUID *parent_guid = (GUID *)(acct->parent);
  gboolean new_acct = FALSE;
  int cache_is_newer = 0;

  /* Find or build the base account */
  a = xaccAccountLookup (guid);
  if (a == NULL) {
    a = xaccMallocAccount ();
    new_acct = TRUE;
    cache_is_newer = -1;
  }

  /* Now figure out which data is newer.  If the incoming txn is newer,
   * then update the engine cache.  If the cache is newer, do nothing.
   * Obviously, if the transaction doesn't exist in the cache then the
   * incoming data is newer :)
   */
  if (!new_acct) {
    gint32 cache_version = xaccAccountGetVersion (a);
    if (cache_version == acct->vers)
      cache_is_newer = 0;
    else if (cache_version > acct->vers)
      cache_is_newer = 1;
    else
      cache_is_newer = -1;
  }


  if (cache_is_newer < 0) {
    xaccAccountBeginEdit (a);
    if (new_acct) xaccAccountSetGUID (a, guid);

    /* Set the account information */
    xaccAccountSetName (a, acct->name);
    xaccAccountSetCode (a, acct->code);
    xaccAccountSetDescription (a, acct->desc);
    xaccAccountSetType (a, acct->type);
    xaccAccountSetVersion (a, acct->vers);
    {
      gnc_commodity *c = NULL;

#ifdef GNCACCT_COMMODITY
      if (acct->commodity.namespace && acct->commodity.mnemonic &&
	  *(acct->commodity.namespace) && *(acct->commodity.mnemonic))
	c = gnc_commodity_table_lookup (ct, acct->commodity.namespace,
					acct->commodity.mnemonic);
      if (c)
	xaccAccountSetCommodity (a, c);
#else
      if (acct->currency.namespace && acct->currency.mnemonic &&
	  *(acct->currency.namespace) && *(acct->currency.mnemonic))
	c = gnc_commodity_table_lookup (ct, acct->currency.namespace,
					acct->currency.mnemonic);
      if (c)
	xaccAccountSetCurrency (a, c);
      
      if (acct->security.namespace && acct->security.mnemonic &&
	  *(acct->security.namespace) && *(acct->security.mnemonic))
	c = gnc_commodity_table_lookup (ct, acct->security.namespace,
					acct->security.mnemonic);
      if (c)
	xaccAccountSetSecurity (a, c);
#endif
    }

    /* Find the parent group and inset this new account into the parent */
    if (parent_guid == NULL) {
      /* top-level account */
      xaccGroupInsertAccount (topgrp, a);
    } else {
      parent = xaccAccountLookup (parent_guid);
      if (!parent) {
	/* build a placeholder for the parent */
	parent = xaccMallocAccount ();
	xaccAccountBeginEdit (parent);
	xaccAccountSetGUID (parent, parent_guid);
      } else {
	xaccAccountBeginEdit (parent);
      }
      xaccAccountInsertSubAccount (parent, a);
      xaccAccountCommitEdit (parent);
    }  

    /* Cope with KVP data */
    a->kvp_data = rpcend_parse_gnckvp (acct->kvp_data);
  
    if (new_acct) {
      /* Add in the balances */
      a->balance = *((gnc_numeric *)&(acct->balance));
      a->starting_balance = *((gnc_numeric *)&(acct->balance));
      a->cleared_balance = *((gnc_numeric *)&(acct->cleared_balance));
      a->starting_cleared_balance = *((gnc_numeric *)&(acct->cleared_balance));
      a->reconciled_balance = *((gnc_numeric *)&(acct->reconciled_balance));
      a->starting_reconciled_balance =
	*((gnc_numeric *)&(acct->reconciled_balance));
      
      a->share_balance = *((gnc_numeric *)&(acct->share_balance));
      a->share_cleared_balance =
	*((gnc_numeric *)&(acct->share_cleared_balance));
      a->share_reconciled_balance =
	*((gnc_numeric *)&(acct->share_reconciled_balance));
    }

    /* And finish the edit */
    xaccAccountCommitEdit (a);
  }
  
  return cache_is_newer;
}

int rpcend_do_add_txn (gncTransaction * txn, gnc_commodity_table *ct)
{
  Transaction *trans;
  gboolean set_guid = FALSE;
  int cache_is_newer = 0;
  GUID *guid;

  /* First, see if we already have this txn */
  guid = (GUID *)&(txn->guid);
  trans = xaccTransLookup (guid);
  if (!trans) {
    trans = xaccMallocTransaction ();
    set_guid = TRUE;
    cache_is_newer = -1;
  }

  /* Now figure out which data is newer.  If the incoming txn is newer,
   * then update the engine cache.  If the cache is newer, do nothing.
   * Obviously, if the transaction doesn't exist in the cache then the
   * incoming data is newer :)
   */
  if (!set_guid) {
    gint32 cache_version = xaccTransGetVersion (trans);
    if (cache_version == txn->vers)
      cache_is_newer = 0;
    else if (cache_version > txn->vers)
      cache_is_newer = 1;
    else
      cache_is_newer = -1;
  }

  /* If the incoming txn is newer, copy it to the engine */
  if (cache_is_newer < 0) {
    Account *previous_acc = NULL;
    GList *db_splits = NULL;
    gnc_splitlist *sl;

    xaccTransBeginEdit (trans);
    if (set_guid) xaccTransSetGUID (trans, guid);
    xaccTransSetDateEnteredTS (trans, (Timespec *)&(txn->date_entered));
    xaccTransSetDatePostedTS (trans, (Timespec *)&(txn->date_posted));
    xaccTransSetNum (trans, txn->num);
    xaccTransSetDescription (trans, txn->desc);
    trans->kvp_data = rpcend_parse_gnckvp (txn->kvp_data);

    xaccTransSetVersion (trans, txn->vers);

    /* hack alert -- don't set the transaction currency until
     * after all splits are restored. This hack is used to set
     * the reporting currency in an account. This hack will be 
     * obsolete when reporting currencies are removed from the
     * account. */

    /* Convert the splits */
    for (sl = txn->splits; sl != NULL; sl = sl->next) {
      GUID *sguid = (GUID *)&(sl->split->guid);
      GUID *acct_guid = (GUID *)&(sl->split->acct_guid);
      Split *s = xaccSplitLookup (sguid);
      Account *acc;

      /* First, load the main Split values */
      if (!s) {
	s = xaccMallocSplit ();
	xaccSplitSetGUID (s, sguid);
      }
      xaccSplitSetMemo (s, sl->split->memo);
      xaccSplitSetAction (s, sl->split->action);
      xaccSplitSetReconcile (s, sl->split->reconciled);
      xaccSplitSetDateReconciledTS (s, (Timespec *)&sl->split->date_reconciled);
      xaccSplitSetValue (s, *((gnc_numeric *)&(sl->split->value)));
      xaccSplitSetAmount (s, *((gnc_numeric *)&(sl->split->damount)));

      s->kvp_data = rpcend_parse_gnckvp (sl->split->kvp_data);

      /* find the account */
      acc = xaccAccountLookup (acct_guid);
      if (!acc) {
	char buf1[GUID_ENCODING_LENGTH+1], buf2[GUID_ENCODING_LENGTH+1];

	PERR ("account not found, will delete this split\n"
	      "\t(split with  guid=%s\n" 
	      "\twants an acct with guid=%s)\n", 
	      guid_to_string_buff (guid, buf1),
	      guid_to_string_buff (acct_guid, buf2)
	      );
	xaccSplitDestroy (s);

      } else {
	int save_state = 0;

	xaccTransAppendSplit (trans, s);
	if (acc != previous_acc) {
	  xaccAccountCommitEdit (previous_acc);
	  xaccAccountBeginEdit (acc);
	  previous_acc = acc;
	}
	if (acc->parent) save_state = acc->parent->saved;
	xaccAccountInsertSplit (acc, s);
	if (acc->parent) acc->parent->saved = save_state;

	/* finally, save this split so we can delete any old splits */
	db_splits = g_list_prepend (db_splits, s);
      }
    } /* for splits */
    /* Finish the dangling account edit */
    xaccAccountCommitEdit (previous_acc);

    /* Destroy any cached splits that we didn't just get */
    {
      GList *c_splits, *del_splits = NULL, *node;
      int i = 0, j = 0;

      c_splits = xaccTransGetSplitList (trans);
      for (node = c_splits; node != NULL; node = node->next) {
	if (g_list_find (db_splits, node->data) == NULL) {
	  del_splits = g_list_prepend (del_splits, node->data);
	  j++;
	}
	i++;
      }
      PINFO ("%d of %d splits marked for deletion", j, i);
      /* Now delete them.. */
      for (node = del_splits; node; node = node->next)
	xaccSplitDestroy ((Split *) node->data);
      g_list_free (del_splits);
      g_list_free (db_splits);
    }

    /* Now set the currency */
    {
      gnc_commodity *c = NULL;
      if (txn->common_currency.namespace && txn->common_currency.mnemonic &&
	  *(txn->common_currency.namespace) &&
	  *(txn->common_currency.mnemonic))
	c = gnc_commodity_table_lookup (ct,
					txn->common_currency.namespace,
					txn->common_currency.mnemonic);
      xaccTransSetCurrency (trans, c);
    }

    /* Commit the edit on the transaction */
    xaccTransCommitEdit (trans);
  }

  return cache_is_newer;
}

static void rpcend_do_build_gncquery (gncQuery *gq, gncQuery *from_q,
				      gboolean toRpc)
{
  gncQTOrlist *orlist, **endor;

  ENTER ("Copying %p to %s %p", from_q, (toRpc?"rpc":"query"), gq);

  memcpy (gq, from_q, sizeof (*gq));	/* top-level is the same */

  orlist = gq->terms;
  gq->terms = NULL;
  endor = &(gq->terms);

  /* Copy the Query Terms */
  for ( ; orlist; orlist = orlist->next) {
    gncQTList *andlist, **endand;
    gncQTOrlist *this_or = malloc (sizeof (*this_or));

    /* Initialize this OrList, and add it to newor */
    this_or->next = NULL;
    this_or->andlist = NULL;
    *endor = this_or;
    endor = &(this_or->next);

    /* Build the And List */
    endand = &(this_or->andlist);
    for (andlist = orlist->andlist; andlist; andlist = andlist->next) {
      gncQTList *this_and = malloc (sizeof (*this_and));
      gncQueryTerm *term = NULL;
      QueryTerm *qt = (QueryTerm *) andlist->qt;

      /* Handle 'special' terms */
      switch (andlist->qt->data.type) {
      case PD_ACCOUNT:
	if (toRpc) {
	  term = malloc (sizeof (*term));
	  term->p = NULL;
	  term->data.type = PD_ACCOUNT;
	  term->data.gncPredicateData_u.acct.term_type =
	    qt->data.acct.term_type;
	  term->data.gncPredicateData_u.acct.sense = qt->data.acct.sense;
	  term->data.gncPredicateData_u.acct.how = qt->data.acct.how;
	  term->data.gncPredicateData_u.acct.acct_guids =
	    (gnc_guidlist *)qt->data.acct.account_guids;
	} else {
	  /* from RPC */
	  gnc_guidlist *thisguid;

	  term = andlist->qt;
	  qt = malloc (sizeof (*qt));
	  qt->data.acct.type = PD_ACCOUNT;
	  qt->data.acct.term_type =
	    term->data.gncPredicateData_u.acct.term_type;
	  qt->data.acct.sense = term->data.gncPredicateData_u.acct.sense;
	  qt->data.acct.how = term->data.gncPredicateData_u.acct.how;
	  qt->data.acct.account_guids =
	    (GList *)term->data.gncPredicateData_u.acct.acct_guids;
	  qt->data.acct.accounts = NULL;
	  for (thisguid = term->data.gncPredicateData_u.acct.acct_guids;
	       thisguid; thisguid = thisguid->next) {
	    Account *acc = xaccAccountLookup ((GUID *)thisguid->guid);
	    if (acc)
	      g_list_append (qt->data.acct.accounts, acc);
	  }
	  term = (gncQueryTerm *)qt;
	}
	break;
      case PD_STRING:
	PINFO ("BLAH...  It's a string.  I hope it doesn't have a Regex!");
      default:
	/* Allocate the right size */
	if (toRpc) {
	  term = malloc (sizeof (*term));
	  memset (term, 0, sizeof (*term));
	} else {
	  term = malloc (sizeof (*qt));
	  memset (term, 0, sizeof (*qt));
	}
	/* Only copy the smaller size */
	memcpy (term, andlist->qt, MIN(sizeof (*qt), sizeof(*term)));
      }

      /* Set the predicate for non-rpc copies */
      if (toRpc) {
	term->p = NULL;	
      } else {
	qt = (QueryTerm *) term;
	qt->p = 
	  xaccQueryGetPredicate (term->data.gncPredicateData_u.misc.term_type);
      }

      this_and->qt = term;
      this_and->next = NULL;
      *endand = this_and;
      endand = &(this_and->next);
    }
  }
  LEAVE ("done");
}

static void rpcend_do_free_gncquery (gncQuery *gq, gboolean isRpc)
{
  gncQTOrlist *orlist, *nextor;

  for (orlist = gq->terms; orlist; orlist = nextor) {
    gncQTList *andlist, *nextand;
    nextor = orlist->next;

    for (andlist = orlist->andlist; andlist; andlist = nextand) {
      nextand = andlist->next;

      /* Free the 'special' terms */
      switch (andlist->qt->data.type) {
      case PD_ACCOUNT:
	if (!isRpc) {
	  QueryTerm *qt = (QueryTerm *) andlist->qt;
	  g_list_free (qt->data.acct.accounts);
	}
	break;
      default:
      }
      free (andlist->qt);
      free (andlist);
    }
    free (orlist);
  }
}

void rpcend_build_gncquery (gncQuery *gq, Query *q)
{
  rpcend_do_build_gncquery (gq, (gncQuery *)q, TRUE);
}

void rpcend_parse_gncquery (gncQuery *gq, Query *q)
{
  rpcend_do_build_gncquery ((gncQuery *)q, gq, FALSE);
}

void rpcend_free_gncquery (gncQuery *q)
{
  rpcend_do_free_gncquery (q, TRUE);
}

void rpcend_free_query (Query *q)
{
  rpcend_do_free_gncquery ((gncQuery *)q, FALSE);
}

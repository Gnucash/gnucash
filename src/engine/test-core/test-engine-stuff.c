/**
 * @file test-engine-stuff.c
 * @brief tools to set up random, but finanically consistent books.
 * Create transactions with random values, random accounts, random
 * account heirarchies, etc.
 *
 * XXX We should modify routines to create really, ugly, dirty
 * transactions 
 * -- 3 or more splits (TBD) 
 * -- splits without parent accounts  (done)
 * -- splits that have accounts but aren't in a transaction (TBD)
 * -- splits that share a currency with the transaction, but whose
 *    value doesn't equal amount (done)
 *
 * Created by Linux Developers Group, 2001
 * Updates Linas Vepstas July 2004
 */
#include "config.h"

#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "Account.h"
#include "AccountP.h"
#include "gnc-engine.h"
#include "gnc-session.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "Recurrence.h"
#include "SchedXaction.h"
#include "SX-book.h"

#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "test-engine-strings.h"

static gboolean glist_strings_only = FALSE;

static GHashTable *exclude_kvp_types = NULL;
static gint kvp_max_depth = 5;
static gint kvp_frame_max_elements = 10;

static gint max_tree_depth = 1;
static gint max_level_accounts = 3;
static gint max_total_accounts = 10;
static gint max_trans_num = 1000;
static gint total_num_accounts = 0;
/* SCU == smallest currency unit -- the value of the denominator */
static gint max_scu = 100; //6000;
static gint min_scu = 100; //1;


/* The inverse fraction of split/transaction data that should 
 * contain invalid/inconsistent fields/values.  Thus, 
 * if borked==1000, then one in 1000 fields will have bad data.
 * This is used to test the data integrity scrubbers, which are 
 * supposed to clean up any crud they find.
 */
static gint borked = 80;

gboolean gnc_engine_debug_random = FALSE;

/* ========================================================== */
/* Set control parameters governing the run. */

void
set_max_account_tree_depth (gint max_tree_depth_in)
{
  max_tree_depth = MAX (max_tree_depth_in, 1);
}

void
set_max_accounts_per_level (gint max_level_accounts_in)
{
  max_level_accounts = MAX (max_level_accounts_in, 1);
}

void
set_max_kvp_depth (gint max_kvp_depth)
{
  kvp_max_depth = MAX (max_kvp_depth, 1);
}

void
set_max_kvp_frame_elements (gint max_kvp_frame_elements)
{
  kvp_frame_max_elements = MAX (max_kvp_frame_elements, 1);
}

void
kvp_exclude_type (KvpValueType kvp_type)
{
  gint *key;

  if (!exclude_kvp_types)
    exclude_kvp_types = g_hash_table_new (g_int_hash, g_int_equal);

  key = g_new (gint, 1);
  *key = kvp_type;

  g_hash_table_insert (exclude_kvp_types, key, exclude_kvp_types);
}

static gboolean
kvp_type_excluded (KvpValueType kvp_type)
{
  gint key = kvp_type;

  if (!exclude_kvp_types)
    return FALSE;

  if (g_hash_table_lookup (exclude_kvp_types, &key))
    return TRUE;

  return FALSE;
}

void
random_glist_strings_only (gboolean strings_only)
{
  glist_strings_only = strings_only;
}

static gboolean zero_nsec = FALSE;

void
random_timespec_zero_nsec (gboolean zero_nsec_in)
{
  zero_nsec = zero_nsec_in;
}

static gboolean usec_resolution = FALSE;

void
random_timespec_usec_resolution (gboolean usec_resolution_in)
{
  usec_resolution = usec_resolution_in;
}

/* ========================================================== */

static inline gboolean
do_bork (void)
{
  if (1 == get_random_int_in_range (0, borked)) 
  {
    return TRUE;
  }
  return FALSE;
}

/* ========================================================== */
/* GList stuff */

static gpointer
get_random_list_element (GList *list)
{
  g_return_val_if_fail (list, NULL);

  return g_list_nth_data (list,
                          get_random_int_in_range (0,
                                                   g_list_length (list) - 1));
}

static kvp_value* get_random_kvp_value_depth (int type, gint depth);

static GList*
get_random_glist_depth (gint depth)
{
    GList *ret = NULL;
    int count = get_random_int_in_range(1, 5);
    int i;

    if (depth >= kvp_max_depth)
      return NULL;

    for (i = 0; i < count; i++)
    {
        KvpValueType kvpt;
        KvpValue *value;

        kvpt = glist_strings_only ? KVP_TYPE_STRING : -2;

        do
        {
          value = get_random_kvp_value_depth (kvpt, depth + 1);
        }
        while (!value);

        ret = g_list_prepend(ret, value);
    }

    return ret;
}

GList*
get_random_glist(void)
{
  return get_random_glist_depth (0);
}

/* ========================================================== */
/* Time/Date, GUID, binary data stuff */

Timespec*
get_random_timespec(void)
{
  Timespec *ret;

  ret = g_new0(Timespec, 1);

  while (ret->tv_sec <= 0)
    ret->tv_sec = rand();

  if (zero_nsec)
    ret->tv_nsec = 0;
  else
  {
    ret->tv_nsec = rand();

    if (usec_resolution)
    {
      ret->tv_nsec = MIN (ret->tv_nsec, 999999999);
      ret->tv_nsec /= 1000;
      ret->tv_nsec *= 1000;
    }
  }

  return ret;
}

GUID*
get_random_guid(void)
{
    GUID *ret;

    ret = g_new(GUID, 1);
    guid_new(ret);

    return ret;
}

bin_data*
get_random_binary_data(void)
{
    int len;
    bin_data *ret;

    len = get_random_int_in_range(20,100);
    ret = g_new(bin_data, 1);
    ret->data = g_new(guchar, len);
    ret->len = len;

    for(len--; len >= 0; len--)
    {
        ret->data[len] = (guchar)get_random_int_in_range(0,255);
    }

    return ret;
}

/* ========================================================== */
/* KVP stuff */

static KvpFrame* get_random_kvp_frame_depth (gint depth);

static KvpValue*
get_random_kvp_value_depth (int type, gint depth)
{
    int datype = type;
    KvpValue *ret;

    if (datype == -1)
    {
        datype = get_random_int_in_range(KVP_TYPE_GINT64, KVP_TYPE_FRAME);
    }

    if (datype == -2)
    {
        datype = get_random_int_in_range(KVP_TYPE_GINT64, KVP_TYPE_FRAME - 1);
    }

    if (datype == KVP_TYPE_FRAME && depth >= kvp_max_depth)
      return NULL;

    if (datype == KVP_TYPE_GLIST && depth >= kvp_max_depth)
      return NULL;

    if (kvp_type_excluded (datype))
      return NULL;

    switch(datype)
    {
    case KVP_TYPE_GINT64:
        ret = kvp_value_new_gint64(get_random_gint64());
        break;

    case KVP_TYPE_DOUBLE:
        ret = NULL;
        break;

    case KVP_TYPE_NUMERIC:
        ret = kvp_value_new_gnc_numeric(get_random_gnc_numeric());
        break;

    case KVP_TYPE_STRING:
    {
        gchar *tmp_str;
        tmp_str = get_random_string();
        if(!tmp_str)
	  return NULL;
        
        ret = kvp_value_new_string(tmp_str);
        g_free(tmp_str);
    }
        break;

    case KVP_TYPE_GUID:
    {
        GUID *tmp_guid;
        tmp_guid = get_random_guid();
        ret = kvp_value_new_guid(tmp_guid);
        g_free(tmp_guid);
    }
        break;

    case KVP_TYPE_TIMESPEC:
    {
        Timespec *ts = get_random_timespec();
        ret = kvp_value_new_timespec (*ts);
        g_free(ts);
    }
	break;
    
    case KVP_TYPE_BINARY:
    {
        bin_data *tmp_data;
        tmp_data = get_random_binary_data();
        ret = kvp_value_new_binary(tmp_data->data, tmp_data->len);
        g_free(tmp_data->data);
        g_free(tmp_data);
    }
        break;
 
    case KVP_TYPE_GLIST:
        ret = kvp_value_new_glist_nc(get_random_glist_depth (depth + 1));
        break;

    case KVP_TYPE_FRAME:
    {
        KvpFrame *tmp_frame;
        tmp_frame = get_random_kvp_frame_depth(depth + 1);
        ret = kvp_value_new_frame(tmp_frame);
        kvp_frame_delete(tmp_frame);
    }
        break;

    default:
        ret = NULL;
        break;
    }
    return ret;
}

static KvpFrame*
get_random_kvp_frame_depth (gint depth)
{
    KvpFrame *ret;
    int vals_to_add;
    gboolean val_added;

    if (depth >= kvp_max_depth)
      return NULL;

    ret = kvp_frame_new();

    vals_to_add = get_random_int_in_range(1,kvp_frame_max_elements);
    val_added = FALSE;

    for (;vals_to_add > 0; vals_to_add--)
    {
        gchar *key;
        KvpValue *val;

	key = NULL;
	while (key == NULL) {
	  key = get_random_string_without("/");
	  if (*key == '\0') {
	    g_free(key);
	    key = NULL;
	  }
	}
	
        val = get_random_kvp_value_depth (-1, depth + 1);
        if (!val)
        {
	  g_free(key);
          if (!val_added)
            vals_to_add++;
          continue;
        }

        val_added = TRUE;

        kvp_frame_set_slot_nc(ret, key, val);

        g_free(key);
    }

    return ret;
}

KvpFrame *
get_random_kvp_frame (void)
{
  return get_random_kvp_frame_depth (0);
}

KvpValue *
get_random_kvp_value(int type)
{
  return get_random_kvp_value_depth (type, 0);
}

/* ================================================================= */
/* Numeric stuff */

#define RAND_IN_RANGE(X) (((X)*((gint64) (rand()+1)))/RAND_MAX)

gnc_numeric
get_random_gnc_numeric(void)
{
    gint64 numer;
    gint64 deno;

    if (RAND_MAX/8 > rand())
    {
       /* Random number between 1 and 6000 */
       deno = RAND_IN_RANGE(6000ULL);
    }
    else
    {
       gint64 norm = RAND_IN_RANGE (7ULL);

       /* multiple of 10, between 1 and 1 million */
       deno = 1;
       while (norm) 
       {
          deno *= 10;
          norm --;
       }
    }

    /* Arbitrary random numbers can cause pointless overflow 
     * during calculations.  Limit dynamic range in hopes 
     * of avoiding overflow. Right now limit it to approx 2^48.
     * The initial division is to help us down towards the range.
     * The loop is to "make sure" we get there.  We might
     * want to make this dependent on "deno" in the future.
     */
    do {
      numer = get_random_gint64()/1000000;
    } while ((numer >> 31) > 0x1FFFF);
    if (0 == numer) numer = 1;
    /* Make sure we have a non-zero denominator */
    if (0 == deno) deno = 1;
    return gnc_numeric_create(numer, deno);
}

/* ================================================================= */
/* Commodity stuff */

const char *types[] =
{
    "NASDAQ",
    "NYSE",
    "EUREX",
    "FUND",
    "AMEX",
    NULL
};

const char*
get_random_commodity_namespace(void)
{
    return get_random_string_in_array(types);
}

static gnc_commodity *
get_random_commodity_from_table (gnc_commodity_table *table)
{
  GList *namespaces;
  gnc_commodity *com = NULL;

  g_return_val_if_fail (table, NULL);

  namespaces = gnc_commodity_table_get_namespaces (table);

  do
  {
    GList *commodities;
    char *namespace;

    namespace = get_random_list_element (namespaces);

    commodities = gnc_commodity_table_get_commodities (table, namespace);
    if (!commodities)
      continue;

    com = get_random_list_element (commodities);

    g_list_free (commodities);

  } while (!com);


  g_list_free (namespaces);

  return com;
}

gnc_commodity*
get_random_commodity (QofBook *book)
{
    gnc_commodity *ret;
    gchar *name;
    const gchar *space;
    gchar *mn;
    gchar *cusip;
    int ran_int;
    gnc_commodity_table *table;

    table = gnc_commodity_table_get_table (book);

#if 0
    if (table &&
        (gnc_commodity_table_get_size (table) > 0) &&
        get_random_int_in_range (1, 5) < 5)
      return get_random_commodity_from_table (table);
#endif

    mn = get_random_string_length_in_range(1, 3);
    space = get_random_commodity_namespace();

    if (table)
    {
      ret = gnc_commodity_table_lookup (table, space, mn);

      if (ret)
      {
        g_free (mn);
        return ret;
      }
    }

    name = get_random_string();
    cusip = get_random_string();

    ran_int = get_random_int_in_range(min_scu, max_scu);

    ret = gnc_commodity_new(book, name, space, mn, cusip, ran_int);

    g_free(mn);
    g_free(name);
    g_free(cusip);

    if (table)
      ret = gnc_commodity_table_insert (table, ret);

    return ret;
}

void
make_random_changes_to_commodity (gnc_commodity *com)
{
  char *str;

  g_return_if_fail (com);

  str = get_random_string ();
  gnc_commodity_set_namespace (com, str);
  g_free (str);

  str = get_random_string ();
  gnc_commodity_set_mnemonic (com, str);
  g_free (str);

  str = get_random_string ();
  gnc_commodity_set_fullname (com, str);
  g_free (str);

  str = get_random_string ();
  gnc_commodity_set_cusip (com, str);
  g_free (str);

  gnc_commodity_set_fraction (com, get_random_int_in_range (1, 100000));
}

void
make_random_changes_to_commodity_table (gnc_commodity_table *table)
{
  GList *namespaces;
  GList *node;

  g_return_if_fail (table);

  namespaces = gnc_commodity_table_get_namespaces (table);

  for (node = namespaces; node; node = node->next)
  {
    const char *ns = node->data;
    GList *commodities;
    GList *com_node;

    if (gnc_commodity_namespace_is_iso (ns))
      continue;

    commodities = gnc_commodity_table_get_commodities (table, ns);

    for (com_node = commodities; com_node; com_node = com_node->next)
    {
      gnc_commodity *com = com_node->data;

      gnc_commodity_table_remove (table, com);
      make_random_changes_to_commodity (com);
      gnc_commodity_table_insert (table, com);
    }

    g_list_free (commodities);
  }

  g_list_free (namespaces);
}
/* ================================================================= */
/* Price stuff */

void
make_random_changes_to_price (QofBook *book, GNCPrice *p)
{
  Timespec *ts;
  char *string;
  gnc_commodity *c;

  g_return_if_fail (book && p);

  gnc_price_begin_edit (p);

  c = get_random_commodity (book);
  gnc_price_set_commodity (p, c);

  c = get_random_commodity (book);
  gnc_price_set_currency (p, c);

  ts = get_random_timespec ();
  gnc_price_set_time (p, *ts);
  g_free (ts);

  string = get_random_string ();
  gnc_price_set_source (p, string);
  g_free (string);

  string = get_random_string ();
  gnc_price_set_typestr (p, string);
  g_free (string);

  gnc_price_set_value (p, get_random_gnc_numeric ());

  gnc_price_commit_edit (p);
}

GNCPrice *
get_random_price(QofBook *book)
{
  GNCPrice *p;

  p = gnc_price_create (book);
  if(!p)
  {
      failure_args("engine-stuff", __FILE__, __LINE__,
                   "get_random_price failed");
      return NULL;
  }

  make_random_changes_to_price (book, p);
  if(!p)
  {
      failure_args("engine-stuff", __FILE__, __LINE__,
                   "make_random_changes_to_price failed");
      return NULL;
  }

  return p;
}

gboolean
make_random_pricedb (QofBook *book, GNCPriceDB *db)
{
  int num_prices;
   gboolean check;

  num_prices = get_random_int_in_range (1, 41);
  if(num_prices < 1) /* should be impossible */
  {
      failure_args("engine-stuff", __FILE__, __LINE__,
                   "get_random_int_in_range failed");
        return FALSE;
  }

  while (num_prices-- > 0)
  {
    GNCPrice *p;

    p = get_random_price (book);
    if(!p)
    {
        failure_args("engine-stuff", __FILE__, __LINE__,
                     "get_random_price failed");
                return FALSE;
    }

        check = gnc_pricedb_add_price (db, p);
        if(!check) { return check; }

    gnc_price_unref (p);
  }
	return TRUE;
}

GNCPriceDB *
get_random_pricedb(QofBook *book)
{
  GNCPriceDB *db;

  db = gnc_pricedb_get_db (book);
  if(!db)
  {
      failure_args("engine-stuff", __FILE__, __LINE__,
                   "gnc_pricedb_get_db failed");
      return NULL;
  }
  if(!make_random_pricedb (book, db)) { return NULL; }

  return db;
}

static gboolean
price_accumulator (GNCPrice *p, gpointer data)
{
  GList **list = data;

  *list = g_list_prepend (*list, p);

  return TRUE;
}

void
make_random_changes_to_pricedb (QofBook *book, GNCPriceDB *pdb)
{
  GList *list = NULL;
  GList *node;

  g_return_if_fail (pdb);

  gnc_pricedb_foreach_price (pdb, price_accumulator, &list, FALSE);

  for (node = list; node; node = node->next)
  {
    GNCPrice *p = node->data;

    switch (get_random_int_in_range (0, 5))
    {
      case 0: /* Delete */
        gnc_pricedb_remove_price (pdb, p);
        break;

      case 1:
      case 2: /* Change */
        make_random_changes_to_price (book, p);
        break;

      default: /* nothing */
        break;
    }
  }

  g_list_free (list);

  /* Add a few new ones */
  {
    int i = get_random_int_in_range (1, 5);

    while (i--)
    {
      GNCPrice *p = get_random_price (book);

      gnc_pricedb_add_price (pdb, p);

      gnc_price_unref (p);
    }
  }
}

/* ================================================================= */
/* Account stuff */

static void
set_account_random_string(Account* act,
                          void(*func)(Account *act, const gchar*str))
{
    gchar *tmp_str = get_random_string();
    if(tmp_str)
    {
        (func)(act, tmp_str);
        g_free(tmp_str);
    }
}

static void
set_account_random_string_from_array(
    Account* act, void(*func)(Account *act, const gchar*str),
    const gchar *list[])
{
    const gchar *tmp_str = get_random_string_in_array(list);
    if(tmp_str)
        (func)(act, tmp_str);

}

static void
account_add_subaccounts (QofBook *book, Account *account, int depth)
{
  int num_accounts;

  if (depth == 0)
    return;

  num_accounts = get_random_int_in_range (1, 10);
  while (num_accounts-- > 0)
  {
    Account *sub = get_random_account (book);

    gnc_account_append_child (account, sub);

    total_num_accounts ++;
    if (total_num_accounts > max_total_accounts) return;

    account_add_subaccounts (book, sub, depth - 1);
  }
}

static void
make_random_account_tree (QofBook *book, Account *root)
{
  int depth;

  g_return_if_fail (book);
  g_return_if_fail (root);

  total_num_accounts = 0;
  depth = get_random_int_in_range (1, max_tree_depth);

  account_add_subaccounts (book, root, depth);

  /* Make sure we have at least two accounts! */
  if (total_num_accounts <= 1)
    account_add_subaccounts (book, root, 1);
}

Account *
get_random_account_tree (QofBook *book)
{
  Account * root;

  g_return_val_if_fail (book, NULL);

  root = gnc_book_get_root_account (book);
  if (!root)
  {
    root = xaccMallocAccount (book);
    gnc_book_set_root_account (book, root);
  }

  make_random_account_tree (book, root);

  return root;
}

/* ================================================================= */
/* transaction stuff */

/** This routine creates a random, but otherwise self-consistent,
 *  'legal' transaction.  It's been modified to occasionally build
 *   cruddy, inconsistent transactions, so that the engine 'scrub' 
 *   routines get tested.
 */
static void
add_random_splits(QofBook *book, Transaction *trn, GList *account_list)
{
    Account *acc, *bcc;
    Split *s;
    gnc_numeric val, amt;

    /* Gotta have at least two different accounts */
    if (1 >= g_list_length (account_list)) return;

    acc = get_random_list_element (account_list);
    xaccTransBeginEdit(trn);
    s = get_random_split(book, acc, trn);

    bcc = get_random_list_element (account_list);
    if ((bcc == acc) && (!do_bork()))
    { 
      /* Make sure that each side of the transaction is in 
       * a different account; otherwise get weirdness in lot
       * calculcations.  ... Hmm maybe should fix lots in 
       * this case? */
      while (bcc == acc) {
         bcc = get_random_list_element (account_list);
      }
    }

    /* Set up two splits whose values really are opposites. */
    val = xaccSplitGetValue(s);
    s = get_random_split(book, bcc, trn);

    /* Other split should have equal and opposite value */
    if (do_bork()) 
    {
       val = get_random_gnc_numeric();
    }
    val = gnc_numeric_neg(val);
    xaccSplitSetValue(s, val);

    if (gnc_commodity_equal (xaccTransGetCurrency(trn), 
                             xaccAccountGetCommodity(bcc)) && 
        (!do_bork()))
    {
        amt = val;
    } else {
        gnc_numeric amt2 = xaccSplitGetAmount(s);
        if (gnc_numeric_positive_p(amt2) ^ gnc_numeric_positive_p(val))
            amt = gnc_numeric_neg(amt2);
    }   
    
    if (gnc_numeric_zero_p(val))
        amt = val;

    xaccSplitSetAmount(s, val);
    xaccTransCommitEdit(trn);
}

typedef struct
{
  GUID guid;
} TransInfo;

void
make_random_changes_to_transaction_and_splits (QofBook *book,
                                               Transaction *trans,
                                               GList *accounts)
{
  GList *splits;
  GList *node;
  Split *split;

  g_return_if_fail (book);
  g_return_if_fail (trans);
  g_return_if_fail (accounts);

  xaccTransBeginEdit (trans);

  make_random_changes_to_transaction (book, trans);

  switch (get_random_int_in_range (0, 3))
  {
    case 0: /* delete some splits, add some more */
      if (xaccTransGetVoidStatus (trans))
        break;

      do
      {
        split = xaccTransGetSplit (trans, 0);

        xaccSplitDestroy (split);
      } while (split);

      add_random_splits (book, trans, accounts);

      /* fall through */

    case 1: /* move the splits around */
      if (xaccTransGetVoidStatus (trans))
        break;

      splits = xaccTransGetSplitList (trans);
      for (node = splits; node; node = node->next)
      {
        Split *split = node->data;
        Account *account;

        account = get_random_list_element (accounts);

        xaccAccountInsertSplit (account, split);
      }
      break;

    case 2: /* destroy the transaction */
      xaccTransDestroy (trans);
      xaccTransCommitEdit (trans);
      return;

    default: /* do nothing */
      break;
  }

  if (xaccTransGetVoidStatus (trans))
  {
    xaccTransCommitEdit (trans);
    return;
  }

  /* mess with the splits */
  splits = xaccTransGetSplitList (trans);
  for (node = splits; node; node = node->next)
  {
    Split *split = node->data;

    if (get_random_boolean ())
      make_random_changes_to_split (split);
  }

  if (get_random_boolean ())
    xaccTransCommitEdit (trans);
  else
    xaccTransRollbackEdit (trans);
}

static int
add_trans_helper (Transaction *trans, gpointer data)
{
  TransInfo *ti;
  GList **list = data;

  ti = g_new (TransInfo, 1);

  ti->guid = *xaccTransGetGUID (trans);

  *list = g_list_prepend (*list, ti);
  return 0;
}

void
make_random_changes_to_level (QofBook *book, Account *parent)
{
  Account *new_account;
  Account *account;
  GList *accounts;
  GList *transes;
  GList *splits;
  GList *node;

  g_return_if_fail (parent && book);

  accounts = gnc_account_get_descendants (parent);

  /* Add a new account */
  new_account = get_random_account (book);

  if (get_random_boolean () || !accounts)
    gnc_account_append_child (parent, new_account);
  else
  {
    account = get_random_list_element (accounts);

    gnc_account_append_child (account, new_account);
  }

  g_list_free (accounts);
  accounts = gnc_account_get_descendants (parent);

  /* Add some new transactions */
  add_random_transactions_to_book (book, get_random_int_in_range (1, 6));

  /* Mess with the accounts */
  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;

    if (get_random_boolean ())
      make_random_changes_to_account (book, account);
  }

  /* Mess with the transactions & splits */
  transes = NULL;
  xaccAccountTreeForEachTransaction (parent, add_trans_helper, &transes);

  for (node = transes; node; node = node->next)
  {
    TransInfo *ti = node->data;
    Transaction *trans = xaccTransLookup (&ti->guid, book);

    if (!trans)
      continue;

    make_random_changes_to_transaction_and_splits (book, trans, accounts);
  }

  for (node = transes; node; node = node->next)
  {
    TransInfo *ti = node->data;

    g_free (ti);
  }
  g_list_free (transes);
  transes = NULL;

  /* delete an account */
  account = get_random_list_element (accounts);

  splits = xaccAccountGetSplitList (account);
  splits = g_list_copy (splits);

  for (node = splits; node; node = node->next)
  {
    Split *split = node->data;

    do
    {
      new_account = get_random_list_element (accounts);
    } while (new_account == account);

    xaccAccountInsertSplit (new_account, split);
  }

  xaccAccountBeginEdit (account);
  xaccAccountDestroy (account);

  g_list_free (splits);
  g_list_free (accounts);

  accounts = gnc_account_get_descendants (parent);

  /* move some accounts around */
  if (accounts && (g_list_length (accounts) > 1))
  {
    int i = get_random_int_in_range (1, 4);

    while (i--)
    {
      Account *a1, *a2;

      a1 = get_random_list_element (accounts);

      if (get_random_boolean ())
        a2 = get_random_list_element (accounts);
      else
        a2 = NULL;

      if (!a2)
      {
        gnc_account_append_child (parent, a1);
        continue;
      }

      if (a1 == a2 ||
          xaccAccountHasAncestor (a1, a2) ||
          xaccAccountHasAncestor (a2, a1))
      {
        i++;
        continue;
      }

      gnc_account_append_child (a2, a1);
    }
  }

  g_list_free (accounts);
}

Account*
get_random_account(QofBook *book)
{
    Account *root, *ret;
    int tmp_int;

    ret = xaccMallocAccount(book);

    xaccAccountBeginEdit(ret);

    set_account_random_string_from_array(ret, xaccAccountSetName,
                                         sane_account_names);

    tmp_int = get_random_int_in_range(ACCT_TYPE_BANK, NUM_ACCOUNT_TYPES - 1);
    xaccAccountSetType(ret, tmp_int);

    set_account_random_string(ret, xaccAccountSetCode);
    set_account_random_string(ret, xaccAccountSetDescription);

    xaccAccountSetCommodity(ret, get_random_commodity(book));

    qof_instance_set_slots(QOF_INSTANCE(ret), get_random_kvp_frame());

    root = gnc_book_get_root_account (book);
    if (!root) 
    {
        root = xaccMallocAccount (book);
        gnc_book_set_root_account (book, root);
    }
    gnc_account_append_child (root, ret);
    xaccAccountCommitEdit(ret);

    return ret;
}

void
make_random_changes_to_account (QofBook *book, Account *account)
{
    int tmp_int;

    g_return_if_fail (account);

    xaccAccountBeginEdit (account);

    set_account_random_string (account, xaccAccountSetName);

    tmp_int = get_random_int_in_range (ACCT_TYPE_BANK, ACCT_TYPE_CREDITLINE);
    xaccAccountSetType (account, tmp_int);

    set_account_random_string (account, xaccAccountSetCode);
    set_account_random_string (account, xaccAccountSetDescription);

    xaccAccountSetCommodity (account, get_random_commodity(book));

    qof_instance_set_slots(QOF_INSTANCE(account), get_random_kvp_frame());

    xaccAccountCommitEdit (account);
}

static void
set_split_random_string(Split *spl,
                       void(*func)(Split *act, const gchar*str))
{
    gchar *tmp_str = get_random_string();
    if(tmp_str)
    {
        (func)(spl, tmp_str);
        g_free(tmp_str);
    }
}

/* Don't do voiding here, it should be done by xaccTransVoid */
static char possible_chars[] = { NREC, CREC, YREC, FREC };

Split*
get_random_split(QofBook *book, Account *acct, Transaction *trn)
{
    Split *ret;
    gnc_numeric amt, val, rate;
    const gchar *str;
    gnc_commodity *com;
    int scu, denom;
    Timespec *ts;

    com = xaccTransGetCurrency (trn);
    scu = gnc_commodity_get_fraction(com);

    ret = xaccMallocSplit(book);

    str = get_random_string_in_array(sane_descriptions);
    xaccSplitSetMemo(ret, str);
    str = get_random_string_in_array(sane_actions);
    xaccSplitSetAction(ret, str);

    xaccSplitSetReconcile(ret, possible_chars[get_random_int_in_range(0, 3)]);

    ts = get_random_timespec();
    xaccSplitSetDateReconciledTS(ret, ts);
    g_free(ts);

    /* Split must be in an account before we can set an amount */
    /* and in a transaction before it can be added to an account. */
    xaccTransBeginEdit(trn);
    xaccSplitSetParent(ret, trn);
    xaccSplitSetAccount(ret, acct);

    do {
        val = get_random_gnc_numeric ();
        if (val.num == 0)
            fprintf(stderr, "get_random_split: Created split with zero value: %p\n", ret);

        if (!do_bork()) 
            val.denom = scu;
    } while (gnc_numeric_check(val) != GNC_ERROR_OK);
    xaccSplitSetValue(ret, val);

    /* If the currencies are the same, the split amount should equal
     * the split value (unless we bork it on purpose) */
    if (gnc_commodity_equal (xaccTransGetCurrency(trn), 
                             xaccAccountGetCommodity(acct)) &&
        (!do_bork())) {
        amt = val;
    } else {
        denom = gnc_commodity_get_fraction(xaccAccountGetCommodity(
                                               xaccSplitGetAccount(ret)));
        do {
            rate = gnc_numeric_abs(get_random_gnc_numeric());
            amt = gnc_numeric_mul(val, rate, 
                                  GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
            amt = gnc_numeric_convert(amt, denom, GNC_HOW_RND_ROUND);
        } while (gnc_numeric_check(amt) != GNC_ERROR_OK);
    }
    xaccSplitSetAmount(ret, amt);

    /* Make sure val and amt have the same sign. Note that amt is
       also allowed to be zero, because that is caused by a small
       rate. */
    if (gnc_numeric_positive_p(val))
        g_assert(!gnc_numeric_negative_p(amt)); /* non-negative amt */
    else
        g_assert(!gnc_numeric_positive_p(amt)); /* non-positive amt */
    
    xaccSplitSetSlots_nc(ret, get_random_kvp_frame());
    xaccTransCommitEdit(trn);

    return ret;
}

void
make_random_changes_to_split (Split *split)
{
  Transaction *trans;
  Timespec *ts;

  g_return_if_fail (split);

  trans = xaccSplitGetParent (split);

  xaccTransBeginEdit (trans);

  set_split_random_string (split, xaccSplitSetMemo);
  set_split_random_string (split, xaccSplitSetAction);

  xaccSplitSetReconcile (split, possible_chars[get_random_int_in_range(0, 3)]);

  ts = get_random_timespec();
  xaccSplitSetDateReconciledTS (split, ts);
  g_free(ts);

  xaccSplitSetSlots_nc (split, get_random_kvp_frame());

  /* Don't change share values/prices here, since that would
   * throw transactions out of balance. Do that in the corresponding
   * change transaction function. */

  xaccTransCommitEdit (trans);
}

static void
set_tran_random_string(Transaction* trn,
                       void(*func)(Transaction *act, const gchar*str))
{
    gchar *tmp_str = get_random_string();
    if(!trn || !(&trn->inst)) { return; }
    if(tmp_str)
    {
        xaccTransBeginEdit(trn);
        (func)(trn, tmp_str);
        g_free(tmp_str);
        xaccTransCommitEdit(trn);
    }
}

static void
set_tran_random_string_from_array(
    Transaction* trn, void(*func)(Transaction *trn, const gchar*str),
    const gchar *list[])
{
    const gchar *tmp_str = get_random_string_in_array(list);
    if(tmp_str)
        (func)(trn, tmp_str);
}


static void
trn_add_ran_timespec(Transaction *trn, void (*func)(Transaction*,
                                                    const Timespec*))
{
    Timespec *to_set;

    to_set = get_random_timespec();
    func(trn, to_set);
    g_free(to_set);
}

    
Transaction *
get_random_transaction_with_currency(QofBook *book,
                                     gnc_commodity *currency,
                                     GList *account_list)
{
    Transaction* trans;
    KvpFrame *f;
    gint num;
    gchar *numstr;

    numstr = g_new0(gchar, 10);
    if (!account_list) 
    {
      account_list = gnc_account_get_descendants (gnc_book_get_root_account (book));
    }

    /* Gotta have at least two different accounts */
    if (1 >= g_list_length (account_list)) {
      failure_args("engine-stuff", __FILE__, __LINE__,
              "get_random_transaction_with_currency: account_list too short");
      return NULL;
    }

    trans = xaccMallocTransaction(book);

    xaccTransBeginEdit(trans);

    xaccTransSetCurrency (trans,
                          currency ? currency :
                          get_random_commodity (book));

    num = get_random_int_in_range (1, max_trans_num);
    g_snprintf(numstr, 10, "%d", num);
    xaccTransSetNum(trans, numstr);
    set_tran_random_string_from_array(trans, xaccTransSetDescription,
                                      sane_descriptions);
    trn_add_ran_timespec(trans, xaccTransSetDatePostedTS);
    trn_add_ran_timespec(trans, xaccTransSetDateEnteredTS);

    f = get_random_kvp_frame();
    xaccTransSetSlots_nc(trans, f);

    add_random_splits(book, trans, account_list);

    if (get_random_int_in_range (1, 10) == 1)
    {
      char *reason = get_random_string ();
      xaccTransVoid (trans, reason);
      g_free (reason);
    }

    xaccTransCommitEdit(trans);
    if (!trans)
    {
        failure_args("engine-stuff", __FILE__, __LINE__,
                     "get_random_transaction_with_currency failed");
        return NULL;
    }

    return trans;
}

Transaction*
get_random_transaction (QofBook *book)
{
	Transaction *ret;

	g_return_val_if_fail(book, NULL);
	ret = get_random_transaction_with_currency (book, NULL, NULL);
    if(!ret)
    {
        failure_args("engine-stuff", __FILE__, __LINE__,
                     "get_random_transaction failed");
        return NULL;
    }
	return ret;
}

void
make_random_changes_to_transaction (QofBook *book, Transaction *trans)
{
  g_return_if_fail (trans && book);

  if (xaccTransGetVoidStatus (trans)) {
    if (get_random_int_in_range (1, 2) == 1)
      xaccTransUnvoid (trans);
    return;
  }

  xaccTransBeginEdit (trans);

  xaccTransSetCurrency (trans, get_random_commodity (book));

  set_tran_random_string (trans, xaccTransSetNum);

  trn_add_ran_timespec (trans, xaccTransSetDatePostedTS);
  trn_add_ran_timespec (trans, xaccTransSetDateEnteredTS);

  set_tran_random_string (trans, xaccTransSetDescription);

  xaccTransSetSlots_nc (trans, get_random_kvp_frame());

  /* Do split manipulations in higher-level functions */

  xaccTransCommitEdit (trans);
}


static GList *
get_random_guids(int max)
{
  GList *guids = NULL;
  int num_guids;

  if (max < 1) return NULL;

  num_guids = get_random_int_in_range (1, max);

  while (num_guids-- > 0)
    guids = g_list_prepend (guids, get_random_guid ());

  return guids;
}

static void
free_random_guids(GList *guids)
{
  GList *node;

  for (node = guids; node; node = node->next)
    g_free (node->data);

  g_list_free (guids);
}

static QofQueryOp
get_random_queryop(void)
{
  int op_num = get_random_int_in_range(1,11);
  QofQueryOp op = QOF_QUERY_AND;
  /* = get_random_int_in_range (1, QOF_QUERY_XOR); */

  /* Let's make it MUCH more likely to get AND and OR */
  switch(op_num) {
  case 1:  case 2:  case 3:  case 4:
    op = QOF_QUERY_AND;
    break;
  case 5:  case 6:  case 7:  case 8:
    op = QOF_QUERY_OR;
    break;
  case 9:
    op = QOF_QUERY_NAND;
    break;
  case 10:
    op = QOF_QUERY_NOR;
    break;
  case 11:
    op = QOF_QUERY_XOR;
    break;
  default:
    g_assert_not_reached();
  };
  if (gnc_engine_debug_random) printf ("op = %d (int was %d), ", op, op_num);
  return op;
}

static GSList *
get_random_kvp_path (void)
{
  GSList *path;
  gint len;

  path = NULL;
  len = get_random_int_in_range (1, kvp_max_depth);

  while (len--)
    path = g_slist_prepend (path, get_random_string ());

  return g_slist_reverse (path);
}

static void
free_random_kvp_path (GSList *path)
{
  GSList *node;

  for (node = path; node; node = node->next)
    g_free (node->data);

  g_slist_free (path);
}

static QofIdType
get_random_id_type (void)
{
  switch (get_random_int_in_range (1,3)) {
  case 1:
    return GNC_ID_SPLIT;
  case 2:
    return GNC_ID_TRANS;
  case 3:
    return GNC_ID_ACCOUNT;
  default:
    return get_random_string ();
  }
}

typedef enum {
  BY_STANDARD = 1,
  BY_DATE,
  BY_DATE_ENTERED,
  BY_DATE_RECONCILED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC,
  BY_NONE
} sort_type_t;

static void
set_query_sort (Query *q, sort_type_t sort_code)
{
  GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;

  standard = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);

  switch (sort_code)
  {
    case BY_STANDARD:
      p1 = standard;
      break;
    case BY_DATE:
      p1 = g_slist_prepend (p1, TRANS_DATE_POSTED);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_DATE_ENTERED:
      p1 = g_slist_prepend (p1, TRANS_DATE_ENTERED);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_DATE_RECONCILED:
      p1 = g_slist_prepend (p1, SPLIT_RECONCILE);
      p2 = g_slist_prepend (p2, SPLIT_DATE_RECONCILED);
      p3 = standard;
      break;
    case BY_NUM:
      p1 = g_slist_prepend (p1, TRANS_NUM);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_AMOUNT:
      p1 = g_slist_prepend (p1, SPLIT_VALUE);
      p2 = standard;
      break;
    case BY_MEMO:
      p1 = g_slist_prepend (p1, SPLIT_MEMO);
      p2 = standard;
      break;
    case BY_DESC:
      p1 = g_slist_prepend (p1, TRANS_DESCRIPTION);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_NONE:
      g_slist_free (standard);
      break;
    default:
      g_slist_free (standard);
      g_return_if_fail (FALSE);
  }

  qof_query_set_sort_order (q, p1, p2, p3);
}

Query *
get_random_query(void)
{
  Query *q;
  int num_terms;

  num_terms = get_random_int_in_range (1, 3);
  if (gnc_engine_debug_random) printf("num_terms = %d", num_terms);

  q = xaccMallocQuery ();

  while (num_terms-- > 0)
  {
    gint pr_type;
    KvpValue *value;
    Timespec *start;
    Timespec *end;
    GList *guids;
    GSList *path;
    char *string;
    GUID *guid;

    pr_type = get_random_int_in_range (1, 20);
    if (gnc_engine_debug_random) printf("\n pr_type = %d ", pr_type);

    switch (pr_type)
    {
      case 1: /*PR_ACCOUNT */
        guids = get_random_guids (10);
        xaccQueryAddAccountGUIDMatch
          (q,
           guids,
           get_random_int_in_range (1, QOF_GUID_MATCH_NONE),
           get_random_queryop ());
        free_random_guids (guids);
        break;

      case 2: /*PR_ACTION */
        string = get_random_string ();
        xaccQueryAddActionMatch (q,
                                 string,
                                 get_random_boolean (),
                                 get_random_boolean (),
                                 get_random_queryop ());
        g_free (string);
        break;

      case 3: /* PR_BALANCE */
        xaccQueryAddBalanceMatch
          (q,
           get_random_boolean (),
           get_random_queryop ());
        break;

      case 4: /* PR_CLEARED */
        xaccQueryAddClearedMatch
          (q,
           get_random_int_in_range (1,
                                    CLEARED_NO |
                                    CLEARED_CLEARED |
                                    CLEARED_RECONCILED |
                                    CLEARED_FROZEN |
                                    CLEARED_VOIDED),
           get_random_queryop ());
        break;

      case 5: /* PR_DATE */
        start = get_random_timespec ();
        end = get_random_timespec ();
        xaccQueryAddDateMatchTS (q, 
                                 get_random_boolean (),
                                 *start,
                                 get_random_boolean (),
                                 *end,
                                 get_random_queryop ());
        g_free (start);
        g_free (end);
        break;

      case 6: /* PR_DESC */
        string = get_random_string ();
        xaccQueryAddDescriptionMatch (q,
                                      string,
                                      get_random_boolean (),
                                      get_random_boolean (),
                                      get_random_queryop ());
        g_free (string);
        break;

      case 7: /* PR_GUID */
        guid = get_random_guid ();
        xaccQueryAddGUIDMatch (q,
                               guid,
                               get_random_id_type (),
                               get_random_queryop ());
        g_free (guid);
        break;

      case 8: /* PR_KVP */
        path = get_random_kvp_path ();
        do
        {
          value = get_random_kvp_value_depth (-2, kvp_max_depth);
        } while (!value);
        xaccQueryAddKVPMatch (q,
                              path,
                              value,
                              get_random_int_in_range (1, QOF_COMPARE_NEQ),
                              get_random_id_type (),
                              get_random_queryop ());
        kvp_value_delete (value);
        free_random_kvp_path (path);
        break;

      case 9: /* PR_MEMO */
        string = get_random_string ();
        xaccQueryAddMemoMatch (q,
                               string,
                               get_random_boolean (),
                               get_random_boolean (),
                               get_random_queryop ());
        g_free (string);
        break;

      case 10: /* PR_NUM */
        string = get_random_string ();
        xaccQueryAddNumberMatch (q,
                               string,
                               get_random_boolean (),
                               get_random_boolean (),
                               get_random_queryop ());
        g_free (string);
        break;

      case 11: /*  PR_PRICE */
        xaccQueryAddSharePriceMatch
          (q,
           get_random_gnc_numeric (), 
           get_random_int_in_range (1, QOF_COMPARE_NEQ),
           get_random_queryop ());
        break;

      case 12: /* PR_SHRS */
        xaccQueryAddSharesMatch
          (q,
           get_random_gnc_numeric (), 
           get_random_int_in_range (1, QOF_COMPARE_NEQ),
           get_random_queryop ());
        break;

      case 13: /* PR_VALUE */
        xaccQueryAddValueMatch
          (q,
           get_random_gnc_numeric (),
           get_random_int_in_range (1, QOF_NUMERIC_MATCH_ANY),
           get_random_int_in_range (1, QOF_COMPARE_NEQ),
           get_random_queryop ());
        break;

      default:
	if (gnc_engine_debug_random) printf("ignored..");
        num_terms++;
        break;
    }
  }

  if (gnc_engine_debug_random) printf ("\n");
  set_query_sort (q, get_random_int_in_range (1, BY_NONE));

  xaccQuerySetSortIncreasing (q,
                              get_random_boolean (),
                              get_random_boolean (),
                              get_random_boolean ());

  xaccQuerySetMaxSplits (q, get_random_int_in_range (-50000, 50000));

  return q;
}

QofBook *
get_random_book (void)
{
  QofBook *book;

  book = qof_book_new ();

  get_random_account_tree (book);
  get_random_pricedb (book);

  return book;
}

QofSession *
get_random_session (void)
{
  QofSession *session;
  QofBook *book;

  session = qof_session_new ();

  book = qof_session_get_book (session);

  get_random_account_tree (book);
  get_random_pricedb (book);

  return session;
}

void
add_random_transactions_to_book (QofBook *book, gint num_transactions)
{
  gnc_commodity_table *table;
  GList *accounts;

  if (num_transactions <= 0) return;

  g_return_if_fail (book);

  accounts = gnc_account_get_descendants (gnc_book_get_root_account (book));
  g_return_if_fail (accounts);

  table = gnc_commodity_table_get_table (book);

  while (num_transactions--)
  {
    gnc_commodity *com;
    Transaction *trans;

    com = get_random_commodity_from_table (table);
    trans = get_random_transaction_with_currency (book, com, accounts);
  }
  g_list_free (accounts);
}

void
make_random_changes_to_book (QofBook *book)
{
  g_return_if_fail (book);

  make_random_changes_to_level (book, gnc_book_get_root_account (book));
  make_random_changes_to_pricedb (book, gnc_pricedb_get_db (book));

#if 0
  make_random_changes_to_commodity_table (gnc_commodity_table_get_table (book));
#endif
}

void
make_random_changes_to_session (QofSession *session)
{
  g_return_if_fail (session);

  make_random_changes_to_book (qof_session_get_book (session));
}

typedef struct
{
  QofIdType where;
  GSList *path;
  Query *q;
} KVPQueryData;

static void
add_kvp_value_query (const char *key, KvpValue *value, gpointer data)
{
  KVPQueryData *kqd = data;
  GSList *node;

  kqd->path = g_slist_append (kqd->path, (gpointer) key);

  if (kvp_value_get_type (value) == KVP_TYPE_FRAME)
    kvp_frame_for_each_slot (kvp_value_get_frame (value),
                             add_kvp_value_query, data);
  else
    xaccQueryAddKVPMatch (kqd->q, kqd->path, value,
                          QOF_COMPARE_EQUAL, kqd->where,
                          QOF_QUERY_AND);

  node = g_slist_last (kqd->path);
  kqd->path = g_slist_remove_link (kqd->path, node);
  g_slist_free_1 (node);
}

static void
add_kvp_query (Query *q, KvpFrame *frame, QofIdType where)
{
  KVPQueryData kqd;

  kqd.where = where;
  kqd.path = NULL;
  kqd.q = q;

  kvp_frame_for_each_slot (frame, add_kvp_value_query, &kqd);
}

static gboolean include_price = TRUE;

void
trans_query_include_price (gboolean include_price_in)
{
  include_price = include_price_in;
}

TestQueryTypes
get_random_query_type (void)
{
  switch (get_random_int_in_range (0, 4))
  {
    case 0: return SIMPLE_QT;
    case 1: return SPLIT_KVP_QT;
    case 2: return TRANS_KVP_QT;
    case 3: return ACCOUNT_KVP_QT;
    case 4: return GUID_QT;
    default: return SIMPLE_QT;
  }
}

Query *
make_trans_query (Transaction *trans, TestQueryTypes query_types)
{
  Account *a;
  gnc_numeric n;
  Query *q;
  Split *s;

  if (query_types == RANDOM_QT)
    query_types = get_random_query_type ();

  q = xaccMallocQuery ();

  s = xaccTransGetSplit (trans, 0);
  a = xaccSplitGetAccount (s);

  if (query_types & SIMPLE_QT)
  {
    xaccQueryAddSingleAccountMatch (q, xaccSplitGetAccount (s), QOF_QUERY_AND);

    if (xaccTransGetDescription(trans) && *xaccTransGetDescription(trans) != '\0')
    {
      xaccQueryAddDescriptionMatch (q, xaccTransGetDescription (trans),
                                    TRUE, FALSE, QOF_QUERY_AND);
    }

    if (xaccTransGetNum(trans) && *xaccTransGetNum(trans) != '\0')
    {
      xaccQueryAddNumberMatch (q, xaccTransGetNum (trans),
                               TRUE, FALSE, QOF_QUERY_AND);
    }
     
    if (xaccSplitGetAction(s) && *xaccSplitGetAction(s) != '\0')
    {
      xaccQueryAddActionMatch (q, xaccSplitGetAction (s),
                               TRUE, FALSE, QOF_QUERY_AND);
    }

    n = xaccSplitGetValue (s);
    xaccQueryAddValueMatch (q, n, QOF_NUMERIC_MATCH_ANY,
                              QOF_COMPARE_EQUAL, QOF_QUERY_AND);

    n = xaccSplitGetAmount (s);
    xaccQueryAddSharesMatch (q, n, QOF_COMPARE_EQUAL, QOF_QUERY_AND);

    if (include_price)
    {
      n = xaccSplitGetSharePrice (s);
      xaccQueryAddSharePriceMatch (q, n, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    }

    {
      Timespec ts;

      xaccTransGetDatePostedTS (trans, &ts);
      xaccQueryAddDateMatchTS (q, TRUE, ts, TRUE, ts, QOF_QUERY_AND);
    }

    if (xaccSplitGetMemo(s) && *xaccSplitGetMemo(s) != '\0')
    {
      xaccQueryAddMemoMatch (q, xaccSplitGetMemo (s), TRUE, FALSE, QOF_QUERY_AND);
    }

    {
      cleared_match_t how;

      switch (xaccSplitGetReconcile (s))
      {
        case NREC:
          how = CLEARED_NO;
          break;
        case CREC:
          how = CLEARED_CLEARED;
          break;
        case YREC:
          how = CLEARED_RECONCILED;
          break;
        case FREC:
          how = CLEARED_FROZEN;
          break;
        case VREC:
          how = CLEARED_VOIDED;
          break;
        default:
          failure ("bad reconcile flag");
          xaccFreeQuery (q);
          return NULL;
      }

      xaccQueryAddClearedMatch (q, how, QOF_QUERY_AND);
    }
  }

  if (query_types & ACCOUNT_QT)
  {
    GList * list;
    GList * node;

    /* GUID_MATCH_ALL */
    list = NULL;
    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      Split * split = node->data;
      list = g_list_prepend (list, xaccSplitGetAccount (split));
    }
    xaccQueryAddAccountMatch (q, list, QOF_GUID_MATCH_ALL, QOF_QUERY_AND);
    g_list_free (list);

    /* GUID_MATCH_NONE */
    list = NULL;
    list = g_list_prepend (list, get_random_guid ());
    list = g_list_prepend (list, get_random_guid ());
    list = g_list_prepend (list, get_random_guid ());
    xaccQueryAddAccountGUIDMatch (q, list, QOF_GUID_MATCH_NONE, QOF_QUERY_AND);

    /* GUID_MATCH_ANY */
    {
      GUID * guid = get_random_guid ();
      *guid = *xaccAccountGetGUID (a);
      list = g_list_prepend (list, guid);
    }
    xaccQueryAddAccountGUIDMatch (q, list, QOF_GUID_MATCH_ANY, QOF_QUERY_AND);

    for (node = list; node; node = node->next)
      g_free (node->data);
    g_list_free (list);
  }

  if (query_types & GUID_QT)
  {
    xaccQueryAddGUIDMatch (q, xaccSplitGetGUID (s),
                           GNC_ID_SPLIT, QOF_QUERY_AND);

    xaccQueryAddGUIDMatch (q, xaccTransGetGUID (trans),
                           GNC_ID_TRANS, QOF_QUERY_AND);

    xaccQueryAddGUIDMatch (q, xaccAccountGetGUID (a),
                           GNC_ID_ACCOUNT, QOF_QUERY_AND);
  }

  if (query_types & SPLIT_KVP_QT)
    add_kvp_query (q, xaccSplitGetSlots (s), GNC_ID_SPLIT);

  if (query_types & TRANS_KVP_QT)
    add_kvp_query (q, xaccTransGetSlots (trans), GNC_ID_TRANS);

  if (query_types & ACCOUNT_KVP_QT)
    add_kvp_query (q, xaccAccountGetSlots (a), GNC_ID_ACCOUNT);

  return q;
}

static Recurrence*
daily_freq(GDate* start, int multiplier)
{
     Recurrence *r = g_new0(Recurrence, 1);
     recurrenceSet(r, multiplier, PERIOD_DAY, start);
     return r;
}

static Recurrence*
once_freq(GDate *when)
{
     Recurrence *r = g_new0(Recurrence, 1);
     recurrenceSet(r, 1, PERIOD_ONCE, when);
     return r;
}

static SchedXaction*
add_sx(gchar *name, GDate *start, GDate *end, GDate *last_occur, Recurrence *r)
{
     QofBook *book = qof_session_get_book(gnc_get_current_session());
     SchedXaction *sx = xaccSchedXactionMalloc(book);
     xaccSchedXactionSetName(sx, name);
     xaccSchedXactionSetStartDate(sx, start);
     if (end != NULL)
          xaccSchedXactionSetEndDate(sx, end);
     if (last_occur != NULL)
          xaccSchedXactionSetLastOccurDate(sx, last_occur);
     {
         GList *recurrences = NULL;
         recurrences = g_list_append(recurrences, r);
         gnc_sx_set_schedule(sx, recurrences);
     }

     gnc_sxes_add_sx(gnc_book_get_schedxactions(book), sx);

     return sx;
}

SchedXaction*
add_daily_sx(gchar *name, GDate *start, GDate *end, GDate *last_occur)
{
     return add_sx(name, start, end, last_occur, daily_freq(start, 1));
}

SchedXaction*
add_once_sx(gchar *name, GDate *when)
{
     return add_sx(name, when, NULL, NULL, once_freq(when));
}

void
remove_sx(SchedXaction *sx)
{
     QofBook *book = qof_session_get_book(gnc_get_current_session());
     SchedXactions *sxes = gnc_book_get_schedxactions(book);
     gnc_sxes_del_sx(sxes, sx);
}

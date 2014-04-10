#include "config.h"

#include <dirent.h>
#include <fcntl.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "date.h"
#include "Group.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static gboolean glist_strings_only = FALSE;

static GHashTable *exclude_kvp_types = NULL;
static gint kvp_max_depth = 5;
static gint kvp_frame_max_elements = 10;

static gint max_group_depth = 4;
static gint max_group_accounts = 10;

static kvp_value* get_random_kvp_value_depth (int type, gint depth);
static gpointer get_random_list_element (GList *list);
static void add_random_splits(GNCBook *book, Transaction *trn);


/***********************************************************************/

void
set_max_group_depth (gint max_group_depth_in)
{
  max_group_depth = MAX (max_group_depth_in, 1);
}

void
set_max_group_accounts (gint max_group_accounts_in)
{
  max_group_accounts = MAX (max_group_accounts_in, 1);
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
kvp_exclude_type (kvp_value_t kvp_type)
{
  gint *key;

  if (!exclude_kvp_types)
    exclude_kvp_types = g_hash_table_new (g_int_hash, g_int_equal);

  key = g_new (gint, 1);
  *key = kvp_type;

  g_hash_table_insert (exclude_kvp_types, key, exclude_kvp_types);
}

static gboolean
kvp_type_excluded (kvp_value_t kvp_type)
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

Timespec*
get_random_timespec(void)
{
  Timespec *ret;

  ret = g_new(Timespec, 1);

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

gnc_numeric
get_random_gnc_numeric(void)
{
    return gnc_numeric_create(get_random_gint64(), rand());
}

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

void
make_random_changes_to_price (GNCBook *book, GNCPrice *p)
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
  gnc_price_set_type (p, string);
  g_free (string);

  gnc_price_set_value (p, get_random_gnc_numeric ());

  gnc_price_commit_edit (p);
}

GNCPrice *
get_random_price(GNCBook *book)
{
  GNCPrice *p;

  p = gnc_price_create (book);

  make_random_changes_to_price (book, p);

  return p;
}

void
make_random_pricedb (GNCBook *book, GNCPriceDB *db)
{
  int num_prices;

  num_prices = get_random_int_in_range (0, 40);

  while (num_prices-- > 0)
  {
    GNCPrice *p;

    p = get_random_price (book);

    gnc_pricedb_add_price (db, p);

    gnc_price_unref (p);
  }
}

GNCPriceDB *
get_random_pricedb(GNCBook *book)
{
  GNCPriceDB *db;

  db = gnc_pricedb_create ();
  make_random_pricedb (book, db);

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
make_random_changes_to_pricedb (GNCBook *book, GNCPriceDB *pdb)
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

GUID*
get_random_guid(void)
{
    GUID *ret;
    
    ret = g_new(GUID, 1);
    guid_new(ret);

    return ret;
}

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
        kvp_value_t kvpt;
        kvp_value *value;

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

bin_data*
get_random_binary_data(void)
{
    int len;
    bin_data *ret;

    len = get_random_int_in_range(20,100);
    ret = g_new(bin_data, 1);
    ret->data = g_new(char, len);
    ret->len = len;

    for(len--; len >= 0; len--)
    {
        ret->data[len] = (char)get_random_int_in_range(0,255);
    }

    return ret;
}

static kvp_frame*
get_random_kvp_frame_depth (gint depth)
{
    kvp_frame *ret;
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
        kvp_value *val;

        do
        {
          key = get_random_string_without("/");
        } while (!key || *key == '\0');

        val = get_random_kvp_value_depth (-1, depth + 1);
        if (!val)
        {
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

kvp_frame*
get_random_kvp_frame (void)
{
  return get_random_kvp_frame_depth (0);
}

static kvp_value*
get_random_kvp_value_depth (int type, gint depth)
{
    int datype = type;

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
        return kvp_value_new_gint64(get_random_gint64());
        break;

    case KVP_TYPE_DOUBLE:
        return kvp_value_new_double(get_random_double());
        break;

    case KVP_TYPE_NUMERIC:
        return kvp_value_new_gnc_numeric(get_random_gnc_numeric());
        break;

    case KVP_TYPE_STRING:
    {
        gchar *tmp_str;
        kvp_value *ret;
        tmp_str = get_random_string();
        if(!tmp_str)
        {
            return NULL;
        }
        
        ret = kvp_value_new_string(tmp_str);
        g_free(tmp_str);
        return ret;
    }
        break;

    case KVP_TYPE_GUID:
    {
        GUID *tmp_guid;
        kvp_value *ret;
        tmp_guid = get_random_guid();
        ret = kvp_value_new_guid(tmp_guid);
        g_free(tmp_guid);
        return ret;
    }
        break;

    case KVP_TYPE_TIMESPEC:
    {
        Timespec *ts = get_random_timespec();
        return kvp_value_new_timespec (*ts);
    }
	break;
    
    case KVP_TYPE_BINARY:
    {
        bin_data *tmp_data;
        kvp_value *ret;
        tmp_data = get_random_binary_data();
        ret = kvp_value_new_binary(tmp_data->data, tmp_data->len);
        g_free(tmp_data->data);
        g_free(tmp_data);
        return ret;
    }
        break;
 
    case KVP_TYPE_GLIST:
        return kvp_value_new_glist_nc(get_random_glist_depth (depth + 1));
        break;

    case KVP_TYPE_FRAME:
    {
        kvp_frame *tmp_frame;
        kvp_value *ret;
        tmp_frame = get_random_kvp_frame_depth(depth + 1);
        ret = kvp_value_new_frame(tmp_frame);
        kvp_frame_delete(tmp_frame);
        return ret;
    }
        break;

    default:
        return NULL;
        break;
    }
}

kvp_value *
get_random_kvp_value(int type)
{
  return get_random_kvp_value_depth (type, 0);
}

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
account_add_subaccounts (GNCBook *book, Account *account, int depth)
{
  int num_accounts;

  if (depth == 0)
    return;

  num_accounts = get_random_int_in_range (1, 10);

  while (num_accounts-- > 0)
  {
    Account *sub = get_random_account (book);

    xaccAccountInsertSubAccount (account, sub);

    account_add_subaccounts (book, sub, depth - 1);
  }
}

static AccountGroup *
get_random_group_depth(GNCBook *book, int depth)
{
  AccountGroup *group;
  int num_accounts;

  if (depth <= 0)
    return NULL;

  group = xaccMallocAccountGroup (book);

  num_accounts = get_random_int_in_range (1, max_group_accounts);

  while (num_accounts-- > 0)
  {
    Account *account = get_random_account (book);

    xaccGroupInsertAccount (group, account);

    account_add_subaccounts (book, account, depth - 1);
  }

  return group;
}

AccountGroup *
get_random_group (GNCBook *book)
{
  int depth;

  depth = get_random_int_in_range (1, max_group_depth);

  return get_random_group_depth (book, depth);
}

typedef struct
{
  GUID guid;
} TransInfo;

static void
change_trans_helper (GNCBook *book, Transaction *trans, GList *accounts)
{
  GList *splits;
  GList *node;
  Split *split;

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

      add_random_splits (book, trans);

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

static gboolean
add_trans_helper (Transaction *trans, gpointer data)
{
  TransInfo *ti;
  GList **list = data;

  ti = g_new (TransInfo, 1);

  ti->guid = *xaccTransGetGUID (trans);

  *list = g_list_prepend (*list, ti);

  return TRUE;
}

void
make_random_changes_to_group (GNCBook *book, AccountGroup *group)
{
  Account *new_account;
  Account *account;
  GList *accounts;
  GList *transes;
  GList *splits;
  GList *node;

  g_return_if_fail (group && book);

  accounts = xaccGroupGetSubAccounts (group);

  /* Add a new account */
  new_account = get_random_account (book);

  if (get_random_boolean ())
    xaccGroupInsertAccount (group, new_account);
  else
  {
    account = get_random_list_element (accounts);

    xaccAccountInsertSubAccount (account, new_account);
  }

  g_list_free (accounts);
  accounts = xaccGroupGetSubAccounts (group);

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
  xaccGroupForEachTransaction (group, add_trans_helper, &transes);

  for (node = transes; node; node = node->next)
  {
    TransInfo *ti = node->data;
    Transaction *trans = xaccTransLookup (&ti->guid, book);

    if (!trans)
      continue;

    change_trans_helper (book, trans, accounts);
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

  accounts = xaccGroupGetSubAccounts (group);

  /* move some accounts around */
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
        xaccGroupInsertAccount (group, a1);
        continue;
      }

      if (a1 == a2 ||
          xaccAccountHasAncestor (a1, a2) ||
          xaccAccountHasAncestor (a2, a1))
      {
        i++;
        continue;
      }

      xaccAccountInsertSubAccount (a2, a1);
    }
  }

  g_list_free (accounts);
}

Account*
get_random_account(GNCBook *book)
{
    Account *ret;
    int tmp_int;

    ret = xaccMallocAccount(book);

    xaccAccountBeginEdit(ret);

    set_account_random_string(ret, xaccAccountSetName);

    tmp_int = get_random_int_in_range(BANK, NUM_ACCOUNT_TYPES - 1);
    xaccAccountSetType(ret, tmp_int);

    set_account_random_string(ret, xaccAccountSetCode);
    set_account_random_string(ret, xaccAccountSetDescription);

    xaccAccountSetCommodity(ret, get_random_commodity(book));

    xaccAccountSetSlots_nc(ret, get_random_kvp_frame());

    xaccAccountCommitEdit(ret);

    return ret;
}

void
make_random_changes_to_account (GNCBook *book, Account *account)
{
    int tmp_int;

    g_return_if_fail (account);

    xaccAccountBeginEdit (account);

    set_account_random_string (account, xaccAccountSetName);

    tmp_int = get_random_int_in_range (BANK, CREDITLINE);
    xaccAccountSetType (account, tmp_int);

    set_account_random_string (account, xaccAccountSetCode);
    set_account_random_string (account, xaccAccountSetDescription);

    xaccAccountSetCommodity (account, get_random_commodity(book));

    xaccAccountSetSlots_nc (account, get_random_kvp_frame());

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
get_random_split(GNCBook *book, gnc_numeric num)
{
    Split *ret;
    gnc_numeric oneVal;

    ret = xaccMallocSplit(book);

    set_split_random_string(ret, xaccSplitSetMemo);
    set_split_random_string(ret, xaccSplitSetAction);

    xaccSplitSetReconcile(ret, possible_chars[get_random_int_in_range(0, 3)]);

    xaccSplitSetDateReconciledTS(ret, get_random_timespec());

    xaccSplitSetValue(ret, num);
    xaccSplitSetAmount(ret, num);

    oneVal = gnc_numeric_create(1,1);
    xaccSplitSetSharePrice(ret, oneVal);

    xaccSplitSetSlots_nc(ret, get_random_kvp_frame());
    
    {
        GUID *ranguid = get_random_guid();
        xaccSplitSetAccountGUID(ret, *ranguid);
        g_free(ranguid);
    }

    return ret;
}

void
make_random_changes_to_split (Split *split)
{
  Transaction *trans;

  g_return_if_fail (split);

  trans = xaccSplitGetParent (split);

  xaccTransBeginEdit (trans);

  set_split_random_string (split, xaccSplitSetMemo);
  set_split_random_string (split, xaccSplitSetAction);

  xaccSplitSetReconcile (split, possible_chars[get_random_int_in_range(0, 3)]);

  xaccSplitSetDateReconciledTS (split, get_random_timespec());

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
    if(tmp_str)
    {
        (func)(trn, tmp_str);
        g_free(tmp_str);
    }
}

static void
add_random_splits(GNCBook *book, Transaction *trn)
{
    gnc_numeric num = get_random_gnc_numeric();

    xaccTransAppendSplit(trn, get_random_split(book, num));
    xaccTransAppendSplit(trn, get_random_split(book, gnc_numeric_neg(num)));
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
get_random_transaction_with_currency(GNCBook *book,
                                     gnc_commodity *currency)
{
    Transaction* ret;

    ret = xaccMallocTransaction(book);

    xaccTransBeginEdit(ret);

    xaccTransSetCurrency (ret,
                          currency ? currency :
                          get_random_commodity (book));

    set_tran_random_string(ret, xaccTransSetNum);

    trn_add_ran_timespec(ret, xaccTransSetDatePostedTS);
    trn_add_ran_timespec(ret, xaccTransSetDateEnteredTS);

    set_tran_random_string(ret, xaccTransSetDescription);

    xaccTransSetSlots_nc(ret, get_random_kvp_frame());

    add_random_splits(book, ret);

    if (get_random_int_in_range (1, 10) == 1)
    {
      char *reason = get_random_string ();
      xaccTransVoid (ret, reason);
      g_free (reason);
    }

    xaccTransCommitEdit(ret);

    return ret;
}

Transaction*
get_random_transaction (GNCBook *book)
{
  return get_random_transaction_with_currency (book, NULL);
}

void
make_random_changes_to_transaction (GNCBook *book, Transaction *trans)
{
  GList *list;
  GList *node;

  g_return_if_fail (trans && book);

  if (xaccTransGetVoidStatus (trans))
    return;

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

static gpointer
get_random_list_element (GList *list)
{
  g_return_val_if_fail (list, NULL);

  return g_list_nth_data (list,
                          get_random_int_in_range (0,
                                                   g_list_length (list) - 1));
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
get_random_commodity (GNCBook *book)
{
    gnc_commodity *ret;
    gchar *name;
    const gchar *space;
    gchar *mn;
    gchar *xcode;
    int ran_int;
    gnc_commodity_table *table;

    table = gnc_book_get_commodity_table (book);

#if 0
    if (table &&
        (gnc_commodity_table_get_size (table) > 0) &&
        get_random_int_in_range (1, 5) < 5)
      return get_random_commodity_from_table (table);
#endif

    mn = get_random_string();
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
    xcode = get_random_string();
    ran_int = get_random_int_in_range(1, 100000);

    ret = gnc_commodity_new (name, space, mn, xcode, ran_int);

    g_free(mn);
    g_free(name);
    g_free(xcode);

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
  gnc_commodity_set_exchange_code (com, str);
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

    if (strcmp (ns, GNC_COMMODITY_NS_ISO) == 0)
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

static GList *
get_random_guids(int max)
{
  GList *guids = NULL;
  int num_guids;

  if (max < 1) return NULL;

  num_guids = get_random_int_in_range (1, max);

  while (num_guids-- > 0)
    g_list_prepend (guids, get_random_guid ());

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

static QueryOp
get_random_queryop(void)
{
  return get_random_int_in_range (1, QUERY_XOR);
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

static GNCIdType
get_random_id_type (void)
{
  return get_random_int_in_range (0, LAST_GNC_ID);
}

Query *
get_random_query(void)
{
  Query *q;
  Query *temp_q;
  int num_terms;

  num_terms = get_random_int_in_range (1, 4);

  q = xaccMallocQuery ();

  while (num_terms-- > 0)
  {
    pr_type_t pr_type;
    kvp_value *value;
    Timespec *start;
    Timespec *end;
    GList *guids;
    GSList *path;
    char *string;
    GUID *guid;

    pr_type = get_random_int_in_range (1, 20);

    switch (pr_type)
    {
      case PR_ACCOUNT:
        guids = get_random_guids (10);
        xaccQueryAddAccountGUIDMatch
          (q,
           guids,
           get_random_int_in_range (1, ACCT_MATCH_NONE),
           get_random_queryop ());
        free_random_guids (guids);
        break;

      case PR_ACTION:
        string = get_random_string ();
        xaccQueryAddActionMatch (q,
                                 string,
                                 get_random_boolean (),
                                 get_random_boolean (),
                                 get_random_queryop ());
        g_free (string);
        break;

      case PR_AMOUNT:
        DxaccQueryAddAmountMatch
          (q,
           get_random_double (),
           get_random_int_in_range (1, AMT_SGN_MATCH_DEBIT),
           get_random_int_in_range (1, AMT_MATCH_EXACTLY),
           get_random_queryop ());
        break;

      case PR_BALANCE:
        xaccQueryAddBalanceMatch
          (q,
           get_random_int_in_range (1, BALANCE_BALANCED | BALANCE_UNBALANCED),
           get_random_queryop ());
        break;

      case PR_CLEARED:
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

      case PR_DATE:
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

      case PR_DESC:
        string = get_random_string ();
        xaccQueryAddDescriptionMatch (q,
                                      string,
                                      get_random_boolean (),
                                      get_random_boolean (),
                                      get_random_queryop ());
        g_free (string);
        break;

      case PR_GUID:
        guid = get_random_guid ();
        xaccQueryAddGUIDMatch (q,
                               guid,
                               get_random_id_type (),
                               get_random_queryop ());
        g_free (guid);
        break;

      case PR_KVP:
        path = get_random_kvp_path ();
        do
        {
          value = get_random_kvp_value_depth (-2, kvp_max_depth);
        } while (!value);
        xaccQueryAddKVPMatch (q,
                              path,
                              value,
                              get_random_int_in_range (1, KVP_MATCH_GT),
                              get_random_int_in_range (1,
                                                       KVP_MATCH_SPLIT |
                                                       KVP_MATCH_TRANS |
                                                       KVP_MATCH_ACCOUNT),
                              get_random_queryop ());
        kvp_value_delete (value);
        free_random_kvp_path (path);
        break;

      case PR_MEMO:
        string = get_random_string ();
        xaccQueryAddMemoMatch (q,
                               string,
                               get_random_boolean (),
                               get_random_boolean (),
                               get_random_queryop ());
        g_free (string);
        break;

      case PR_NUM:
        string = get_random_string ();
        xaccQueryAddNumberMatch (q,
                               string,
                               get_random_boolean (),
                               get_random_boolean (),
                               get_random_queryop ());
        g_free (string);
        break;

      case PR_PRICE:
        DxaccQueryAddSharePriceMatch
          (q,
           get_random_double (), 
           get_random_int_in_range (1, AMT_MATCH_EXACTLY),
           get_random_queryop ());
        break;

      case PR_SHRS:
        DxaccQueryAddSharesMatch
          (q,
           get_random_double (), 
           get_random_int_in_range (1, AMT_MATCH_EXACTLY),
           get_random_queryop ());
        break;

      case PR_MISC: /* PR_MISC shouldn't be used anyway :) */
      default:
        num_terms++;
        break;
    }
  }

  xaccQuerySetSortOrder (q,
                         get_random_int_in_range (1, BY_NONE),
                         get_random_int_in_range (1, BY_NONE),
                         get_random_int_in_range (1, BY_NONE));

  xaccQuerySetSortIncreasing (q,
                              get_random_boolean (),
                              get_random_boolean (),
                              get_random_boolean ());

  xaccQuerySetMaxSplits (q, get_random_int_in_range (-50000, 50000));

  return q;
}

GNCBook *
get_random_book (void)
{
  GNCBook *book;

  book = gnc_book_new ();

  gnc_book_set_group (book, get_random_group (book));

  make_random_pricedb (book, gnc_book_get_pricedb (book));

  return book;
}

GNCSession *
get_random_session (void)
{
  GNCSession *session;
  GNCBook *book;

  session = gnc_session_new ();

  book = gnc_session_get_book (session);

  gnc_book_set_group (book, get_random_group (book));

  make_random_pricedb (book, gnc_book_get_pricedb (book));

  return session;
}

void
add_random_transactions_to_book (GNCBook *book, gint num_transactions)
{
  gnc_commodity_table *table;
  GList *accounts;
  gint num_accounts;

  if (num_transactions <= 0) return;

  g_return_if_fail (book);

  accounts = xaccGroupGetSubAccounts (gnc_book_get_group (book));

  g_return_if_fail (accounts);

  num_accounts = g_list_length (accounts);

  table = gnc_book_get_commodity_table (book);

  while (num_transactions--)
  {
    gnc_commodity *com;
    Transaction *trans;
    Account *account;
    Split *split;

    com = get_random_commodity_from_table (table);

    trans = get_random_transaction_with_currency (book, com);

    xaccTransBeginEdit (trans);

    split = xaccTransGetSplit (trans, 0);
    account = get_random_list_element (accounts);
    xaccAccountInsertSplit (account, split);

    split = xaccTransGetSplit (trans, 1);
    account = get_random_list_element (accounts);
    xaccAccountInsertSplit (account, split);

    xaccTransCommitEdit (trans);
  }

  g_list_free (accounts);
}

void
make_random_changes_to_book (GNCBook *book)
{
  g_return_if_fail (book);

  make_random_changes_to_group (book, gnc_book_get_group (book));
  make_random_changes_to_pricedb (book, gnc_book_get_pricedb (book));

#if 0
  make_random_changes_to_commodity_table (gnc_book_get_commodity_table (book));
#endif
}

void
make_random_changes_to_session (GNCSession *session)
{
  g_return_if_fail (session);

  make_random_changes_to_book (gnc_session_get_book (session));
}

typedef struct
{
  kvp_match_where_t where;
  GSList *path;
  Query *q;
} KVPQueryData;

static void
add_kvp_value_query (const char *key, kvp_value *value, gpointer data)
{
  KVPQueryData *kqd = data;
  GSList *node;

  kqd->path = g_slist_append (kqd->path, (gpointer) key);

  if (kvp_value_get_type (value) == KVP_TYPE_FRAME)
    kvp_frame_for_each_slot (kvp_value_get_frame (value),
                             add_kvp_value_query, data);
  else
    xaccQueryAddKVPMatch (kqd->q, kqd->path, value,
                          KVP_MATCH_EQ, kqd->where,
                          QUERY_AND);

  node = g_slist_last (kqd->path);
  kqd->path = g_slist_remove_link (kqd->path, node);
  g_slist_free_1 (node);
}

static void
add_kvp_query (Query *q, kvp_frame *frame, kvp_match_where_t where)
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
  double d;
  Query *q;
  Split *s;

  if (query_types == RANDOM_QT)
    query_types = get_random_query_type ();

  q = xaccMallocQuery ();

  s = xaccTransGetSplit (trans, 0);
  a = xaccSplitGetAccount (s);

  if (query_types & SIMPLE_QT)
  {
    xaccQueryAddSingleAccountMatch (q, xaccSplitGetAccount (s), QUERY_AND);

    xaccQueryAddDescriptionMatch (q, xaccTransGetDescription (trans),
                                  TRUE, FALSE, QUERY_AND);

    xaccQueryAddNumberMatch (q, xaccTransGetNum (trans),
                             TRUE, FALSE, QUERY_AND);

    xaccQueryAddActionMatch (q, xaccSplitGetAction (s),
                             TRUE, FALSE, QUERY_AND);

    d = gnc_numeric_to_double (xaccSplitGetValue (s));
    DxaccQueryAddAmountMatch (q, d, AMT_SGN_MATCH_EITHER,
                              AMT_MATCH_EXACTLY, QUERY_AND);

    d = gnc_numeric_to_double (xaccSplitGetAmount (s));
    DxaccQueryAddSharesMatch (q, d, AMT_MATCH_EXACTLY, QUERY_AND);

    if (include_price)
    {
      d = gnc_numeric_to_double (xaccSplitGetSharePrice (s));
      DxaccQueryAddSharePriceMatch (q, d, AMT_MATCH_EXACTLY, QUERY_AND);
    }

    {
      Timespec ts;

      xaccTransGetDatePostedTS (trans, &ts);
      xaccQueryAddDateMatchTS (q, TRUE, ts, TRUE, ts, QUERY_AND);
    }

    xaccQueryAddMemoMatch (q, xaccSplitGetMemo (s), TRUE, FALSE, QUERY_AND);

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

      xaccQueryAddClearedMatch (q, how, QUERY_AND);
    }
  }

  if (query_types & GUID_QT)
  {
    xaccQueryAddGUIDMatch (q, xaccSplitGetGUID (s),
                           GNC_ID_SPLIT, QUERY_AND);

    xaccQueryAddGUIDMatch (q, xaccTransGetGUID (trans),
                           GNC_ID_TRANS, QUERY_AND);

    xaccQueryAddGUIDMatch (q, xaccAccountGetGUID (a),
                           GNC_ID_ACCOUNT, QUERY_AND);
  }

  if (query_types & SPLIT_KVP_QT)
    add_kvp_query (q, xaccSplitGetSlots (s), KVP_MATCH_SPLIT);

  if (query_types & TRANS_KVP_QT)
    add_kvp_query (q, xaccTransGetSlots (trans), KVP_MATCH_TRANS);

  if (query_types & ACCOUNT_KVP_QT)
    add_kvp_query (q, xaccAccountGetSlots (a), KVP_MATCH_ACCOUNT);

  return q;
}

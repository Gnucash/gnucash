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
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static gboolean glist_strings_only = FALSE;

static GHashTable *exclude_kvp_types = NULL;
static gint kvp_max_depth = G_MAXINT;
static gint kvp_frame_max_elements = 10;

static gint max_group_depth = 4;
static gint max_group_accounts = 10;

static kvp_value* get_random_kvp_value_depth (int type, gint depth);


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
glist_exclude_type (kvp_value_t kvp_type)
{
  gint *key;

  if (!exclude_kvp_types)
    exclude_kvp_types = g_hash_table_new (g_int_hash, g_int_equal);

  key = g_new (gint, 1);
  *key = kvp_type;

  g_hash_table_insert (exclude_kvp_types, key, exclude_kvp_types);
}

static gboolean
glist_type_excluded (kvp_value_t kvp_type)
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

Timespec*
get_random_timespec(void)
{
    Timespec *ret;

    ret = g_new(Timespec, 1);

    ret->tv_sec = rand();
    ret->tv_nsec = rand();

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

GNCPrice *
get_random_price(GNCSession *session)
{
  GNCPrice *p;
  Timespec *ts;
  char *string;
  gnc_commodity *c;

  p = gnc_price_create (session);

  c = get_random_commodity (session);
  gnc_price_set_commodity (p, c);

  c = get_random_commodity (session);
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

  return p;
}

void
make_random_pricedb (GNCSession *session, GNCPriceDB *db)
{
  int num_prices;

  num_prices = get_random_int_in_range (0, 40);

  while (num_prices-- > 0)
  {
    GNCPrice *p;

    p = get_random_price (session);

    gnc_pricedb_add_price (db, p);

    gnc_price_unref (p);
  }
}

GNCPriceDB *
get_random_pricedb(GNCSession *session)
{
  GNCPriceDB *db;

  db = gnc_pricedb_create ();
  make_random_pricedb (session, db);

  return db;
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

        kvpt = glist_strings_only ? KVP_TYPE_STRING : -2;

        ret = g_list_prepend(ret,
                             get_random_kvp_value_depth (kvpt, depth + 1));
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

    if (depth >= kvp_max_depth)
      return NULL;

    ret = kvp_frame_new();

    vals_to_add = get_random_int_in_range(1,kvp_frame_max_elements);

    for(;vals_to_add > 0; vals_to_add--)
    {
        gchar *key;
        kvp_value *val;

        key = get_random_string_without("/");
        val = get_random_kvp_value_depth (-1, depth + 1);

        if(!key)
        {
            return NULL;
        }
        if (!val)
        {
          vals_to_add++;
          continue;
        }

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

    if(datype == -1)
    {
        datype = get_random_int_in_range(KVP_TYPE_GINT64, KVP_TYPE_FRAME + 1);
    }

    if(datype == -2)
    {
        datype = get_random_int_in_range(KVP_TYPE_GINT64, KVP_TYPE_FRAME);
    }

    if (datype == KVP_TYPE_FRAME && depth >= kvp_max_depth)
      return NULL;

    if (datype == KVP_TYPE_GLIST && depth >= kvp_max_depth)
      return NULL;

    if (glist_type_excluded (datype))
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
account_add_subaccounts (GNCSession *session, Account *account, int depth)
{
  int num_accounts;

  if (depth == 0)
    return;

  num_accounts = get_random_int_in_range (1, 10);

  while (num_accounts-- > 0)
  {
    Account *sub = get_random_account (session);

    xaccAccountInsertSubAccount (account, sub);

    account_add_subaccounts (session, sub, depth - 1);
  }
}

static AccountGroup *
get_random_group_depth(GNCSession *session, int depth)
{
  AccountGroup *group;
  int num_accounts;

  if (depth == 0)
    return NULL;

  group = xaccMallocAccountGroup (session);

  num_accounts = get_random_int_in_range (1, max_group_accounts);

  while (num_accounts-- > 0)
  {
    Account *account = get_random_account (session);

    xaccGroupInsertAccount (group, account);

    account_add_subaccounts (session, account, depth - 1);
  }

  return group;
}

AccountGroup *
get_random_group (GNCSession *session)
{
  int depth;

  depth = get_random_int_in_range (1, max_group_depth);

  return get_random_group_depth (session, depth);
}

Account*
get_random_account(GNCSession *session)
{
    Account *ret;
    int tmp_int;

    ret = xaccMallocAccount(session);

    xaccAccountBeginEdit(ret);

    set_account_random_string(ret, xaccAccountSetName);

    tmp_int = get_random_int_in_range(BANK, CREDITLINE);
    xaccAccountSetType(ret, tmp_int);

    set_account_random_string(ret, xaccAccountSetCode);
    set_account_random_string(ret, xaccAccountSetDescription);

    xaccAccountSetCommodity(ret, get_random_commodity(session));

    xaccAccountSetSlots_nc(ret, get_random_kvp_frame());

    xaccAccountCommitEdit(ret);

    return ret;
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

static char possible_chars[] = { 'c', 'y', 'f', 'n' };

Split*
get_random_split(GNCSession *session, gnc_numeric num)
{
    Split *ret;
    gnc_numeric oneVal;

    ret = xaccMallocSplit(session);

    set_split_random_string(ret, xaccSplitSetMemo);
    set_split_random_string(ret, xaccSplitSetAction);

    xaccSplitSetReconcile(ret, possible_chars[get_random_int_in_range(0, 4)]);

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
add_random_splits(GNCSession *session, Transaction *trn)
{
    gnc_numeric num = get_random_gnc_numeric();

    xaccTransAppendSplit(trn, get_random_split(session, num));
    xaccTransAppendSplit(trn, get_random_split(session, gnc_numeric_neg(num)));
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

    
Transaction*
get_random_transaction(GNCSession *session)
{
    Transaction* ret;

    ret = xaccMallocTransaction(session);

    xaccTransBeginEdit(ret);

    xaccTransSetCurrency(ret, get_random_commodity (session));

    set_tran_random_string(ret, xaccTransSetNum);

    trn_add_ran_timespec(ret, xaccTransSetDatePostedTS);
    trn_add_ran_timespec(ret, xaccTransSetDateEnteredTS);

    set_tran_random_string(ret, xaccTransSetDescription);

    xaccTransSetSlots_nc(ret, get_random_kvp_frame());

    add_random_splits(session, ret);

    xaccTransCommitEdit(ret);
    
    return ret;
}

gnc_commodity*
get_random_commodity (GNCSession *session)
{
    gnc_commodity *ret;
    gchar *name;
    const gchar *space;
    gchar *mn;
    gchar *xcode;
    int ran_int;
    gnc_commodity_table *table;

    mn = get_random_string();
    space = get_random_commodity_namespace();

    table = gnc_book_get_commodity_table (gnc_session_get_book (session));

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
    Timespec *start;
    Timespec *end;
    GList *guids;
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
           get_random_int_in_range (1, BALANCE_UNBALANCED),
           get_random_queryop ());
        break;

      case PR_CLEARED:
        xaccQueryAddClearedMatch
          (q,
           get_random_int_in_range (1, CLEARED_VOIDED),
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
        xaccQueryAddGUIDMatch (q, guid, get_random_queryop ());
        g_free (guid);
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
get_random_book (GNCSession *session)
{
  GNCBook *book;

  book = gnc_book_new (session);

  gnc_book_set_group (book, get_random_group (session));

  /* make_random_pricedb (gnc_book_get_pricedb (book)); */

  return book;
}

GNCSession *
get_random_session (void)
{
  GNCSession *session;
  GNCBook *book;

  session = gnc_session_new ();

  book = gnc_session_get_book (session);

  gnc_book_set_group (book, get_random_group (session));

  return session;
}

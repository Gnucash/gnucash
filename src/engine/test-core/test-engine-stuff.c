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
#include "gnc-engine-util.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

/***********************************************************************/

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

gnc_commodity*
get_random_gnc_commodity_ref(void)
{
    const gchar *name;
    gchar *mnemonic;
    gnc_commodity *ret;
    
    name = get_random_commodity_namespace();
    mnemonic = get_random_string();

    if(!name || !mnemonic)
    {
        return NULL;
    }
    
    ret = gnc_commodity_new("", name, mnemonic, "", 0);

    g_free(mnemonic);

    return ret;
}

GNCPrice *
get_random_price(void)
{
  GNCPrice *p;
  Timespec *ts;
  char *string;

  p = gnc_price_create ();

  gnc_price_set_commodity (p, get_random_gnc_commodity_ref ());
  gnc_price_set_currency (p, get_random_gnc_commodity_ref ());

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

GUID*
get_random_guid(void)
{
    GUID *ret;
    
    ret = g_new(GUID, 1);
    guid_new(ret);

    return ret;
}

GList*
get_random_glist(void)
{
    GList *ret = NULL;
    int i;
    int count = get_random_int_in_range(1, 5);
    
    for(i = 0; i < count; i++)
    {
        ret = g_list_prepend(ret, (gpointer)get_random_kvp_value(-2));
    }
    
    return ret;
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

kvp_frame*
get_random_kvp_frame(void)
{
    kvp_frame *ret;
    int vals_to_add;
    
    ret = kvp_frame_new();

    vals_to_add = get_random_int_in_range(1,10);

    for(;vals_to_add > 0; vals_to_add--)
    {
        gchar *key;
        kvp_value *val;
        
        key = get_random_string();
        val = get_random_kvp_value(-1);

        if(!key)
        {
            return NULL;
        }
        
        kvp_frame_set_slot_nc(ret, key, val);

        g_free(key);
    }
    
    return ret;
}

kvp_value*
get_random_kvp_value(int type)
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
        return kvp_value_new_glist_nc(get_random_glist());
        break;

    case KVP_TYPE_FRAME:
    {
        kvp_frame *tmp_frame;
        kvp_value *ret;
        tmp_frame = get_random_kvp_frame();
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

Account*
get_random_account(void)
{
    Account *ret;
    int tmp_int;
    
    ret = xaccMallocAccount();

    xaccAccountBeginEdit(ret);

    set_account_random_string(ret, xaccAccountSetName);

    tmp_int = get_random_int_in_range(BANK, CREDITLINE);
    xaccAccountSetType(ret, tmp_int);

    set_account_random_string(ret, xaccAccountSetCode);
    set_account_random_string(ret, xaccAccountSetDescription);

    xaccAccountSetCommodity(ret, get_random_gnc_commodity_ref());
    
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
get_random_split(gnc_numeric num)
{
    Split *ret;
    gnc_numeric oneVal;
    
    ret = xaccMallocSplit();

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
add_random_splits(Transaction *trn)
{
    gnc_numeric num = get_random_gnc_numeric();
    xaccTransAppendSplit(trn, get_random_split(num));
    xaccTransAppendSplit(trn, get_random_split(gnc_numeric_neg(num)));
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
get_random_transaction(void)
{
    Transaction* ret;

    ret = xaccMallocTransaction();

    xaccTransBeginEdit(ret);

    xaccTransSetCurrency(ret, get_random_gnc_commodity_ref());

    set_tran_random_string(ret, xaccTransSetNum);

    trn_add_ran_timespec(ret, xaccTransSetDatePostedTS);
    trn_add_ran_timespec(ret, xaccTransSetDateEnteredTS);

    set_tran_random_string(ret, xaccTransSetDescription);

    xaccTransSetSlots_nc(ret, get_random_kvp_frame());

    add_random_splits(ret);

    xaccTransCommitEdit(ret);
    
    return ret;
}

gnc_commodity*
get_random_commodity(void)
{
    gnc_commodity *ret;
    gchar *name;
    const gchar *space;
    gchar *mn;
    gchar *xcode;
    int ran_int;

    name = get_random_string();
    space = get_random_commodity_namespace();
    mn = get_random_string();
    xcode = get_random_string();
    ran_int = get_random_int_in_range(1, 100000);

    ret = gnc_commodity_new(name, space, mn, xcode, ran_int);

    g_free(name);
    g_free(mn);
    g_free(xcode);

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
           get_random_int_in_range (1, CLEARED_FROZEN),
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


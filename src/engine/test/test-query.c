
#include <glib.h>
#include <guile/gh.h>

#include "TransLog.h"
#include "Transaction.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

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

static Query *
make_trans_query (Transaction *trans)
{
  Account *a;
  double d;
  Query *q;
  Split *s;

  q = xaccMallocQuery ();

  s = xaccTransGetSplit (trans, 0);
  a = xaccSplitGetAccount (s);

  xaccQueryAddSingleAccountMatch (q, xaccSplitGetAccount (s), QUERY_AND);

  xaccQueryAddDescriptionMatch (q, xaccTransGetDescription (trans),
                                TRUE, FALSE, QUERY_AND);

  xaccQueryAddNumberMatch (q, xaccTransGetNum (trans), TRUE, FALSE, QUERY_AND);

  xaccQueryAddActionMatch (q, xaccSplitGetAction (s), TRUE, FALSE, QUERY_AND);

  d = gnc_numeric_to_double (xaccSplitGetValue (s));
  DxaccQueryAddAmountMatch (q, d, AMT_SGN_MATCH_EITHER,
                            AMT_MATCH_EXACTLY, QUERY_AND);

  d = gnc_numeric_to_double (xaccSplitGetSharePrice (s));
  DxaccQueryAddSharePriceMatch (q, d, AMT_MATCH_EXACTLY, QUERY_AND);

  d = gnc_numeric_to_double (xaccSplitGetAmount (s));
  DxaccQueryAddSharesMatch (q, d, AMT_MATCH_EXACTLY, QUERY_AND);

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

  add_kvp_query (q, xaccSplitGetSlots (s), KVP_MATCH_SPLIT);
  add_kvp_query (q, xaccTransGetSlots (trans), KVP_MATCH_TRANS);
  add_kvp_query (q, xaccAccountGetSlots (a), KVP_MATCH_ACCOUNT);

  return q;
}

static gboolean
test_trans_query (Transaction *trans, gpointer data)
{
  GNCSession *session = data;
  AccountGroup *group;
  GNCBook *book;
  GList *list;
  Query *q;

  book = gnc_session_get_book (session);
  group = gnc_book_get_group (book);

  q = make_trans_query (trans);
  xaccQuerySetGroup (q, group);

  list = xaccQueryGetTransactions (q, QUERY_MATCH_ANY);
  if (g_list_length (list) != 1)
  {
    failure ("number of matching transactions not 1");
    return FALSE;
  }

  if (list->data != trans)
  {
    failure ("matching transaction is wrong");
    return FALSE;
  }

  success ("found right transaction");
  xaccFreeQuery (q);

  return TRUE;
}

static void
run_test (void)
{
  GNCSession *session;
  AccountGroup *group;
  GNCBook *book;

  session = get_random_session ();
  book = gnc_session_get_book (session);
  group = gnc_book_get_group (book);

  add_random_transactions_to_session (session, 20);

  xaccGroupForEachTransaction (group, test_trans_query, session);

  gnc_session_destroy (session);
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);

  xaccLogDisable ();

  run_test ();

  success("queries seem to work");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

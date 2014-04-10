
#include <glib.h>
#include <guile/gh.h>

#include "Group.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static gboolean
test_trans_query (Transaction *trans, gpointer data)
{
  AccountGroup *group;
  GNCBook *book = data;
  GList *list;
  Query *q;

  group = gnc_book_get_group (book);

  q = make_trans_query (trans, ALL_QT);
  xaccQuerySetGroup (q, group);

  list = xaccQueryGetTransactions (q, QUERY_MATCH_ANY);
  if (g_list_length (list) != 1)
  {
    failure_args ("test number returned", __FILE__, __LINE__,
                  "number of matching transactions %d not 1",
                  g_list_length (list));
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

  add_random_transactions_to_book (book, 20);

  xaccGroupForEachTransaction (group, test_trans_query, book);

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

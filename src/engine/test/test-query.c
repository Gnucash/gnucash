
#include <glib.h>
#include <libguile.h>

#include "Group.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static int
test_trans_query (Transaction *trans, gpointer data)
{
  QofBook *book = data;
  GList *list;
  Query *q;

  q = make_trans_query (trans, ALL_QT);
  xaccQuerySetBook (q, book);

  list = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
  if (g_list_length (list) != 1)
  {
    failure_args ("test number returned", __FILE__, __LINE__,
                  "number of matching transactions %d not 1",
                  g_list_length (list));
    g_list_free (list);
    return 13;
  }

  if (list->data != trans)
  {
    failure ("matching transaction is wrong");
    g_list_free (list);
    return 13;
  }

  success ("found right transaction");
  xaccFreeQuery (q);
  g_list_free (list);

  return 0;
}

static void
run_test (void)
{
  QofSession *session;
  AccountGroup *group;
  QofBook *book;

  session = get_random_session ();
  book = qof_session_get_book (session);
  group = xaccGetAccountGroup (book);

  add_random_transactions_to_book (book, 20);

  xaccGroupForEachTransaction (group, test_trans_query, book);

  qof_session_destroy (session);
}

static void
main_helper (void *closure, int argc, char **argv)
{
  int i;

  gnc_module_load("gnucash/engine", 0);

  g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

  xaccLogDisable ();

  /* Always start from the same random seed so we fail consistently */
  srand(0);

  /* Loop the test. */
  for (i=0; i < 10; i++)
    run_test ();

  success("queries seem to work");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, main_helper, NULL);
  return 0;
}

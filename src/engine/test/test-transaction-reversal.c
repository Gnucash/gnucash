#include <glib.h>
#include <libguile.h>
#include <string.h>

#include "Account.h"
#include "TransLog.h"
#include "Transaction.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "qofsession.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Transaction.h"
#include "guid.h"



#define print_gnc_numeric(num) fprintf(stderr, "%s\n", gnc_numeric_to_string(num))

static void
transaction_set_splits_to_accounts(Transaction *tr, Account *a1, Account *a2)
{
  Split *split;

  split  = xaccTransGetSplit(tr, 0);
  
  xaccAccountInsertSplit(a1, split);

  split = xaccTransGetSplit(tr, 1);
  xaccAccountInsertSplit(a2, split);
  return;
}

static void
run_test (void)
{
  Account *acc1, *acc2;
  Transaction *transaction, *new_trans;
  gnc_numeric old, new, result;
  QofBook *book;
  char *msg;
  int i;

  book = qof_book_new();

  acc1 = get_random_account(book);
  acc2 = get_random_account(book);

  if (!acc1 || !acc2)
  {
    failure("accounts not created");
    return;
  }

  /* Find a transaction that isn't voided */
  do
  {
    gboolean voyd;
    
    transaction = get_random_transaction (book);
    voyd = xaccTransGetVoidStatus (transaction);
    if (voyd)
    {
      xaccTransBeginEdit (transaction);
      xaccTransDestroy (transaction);
      xaccTransCommitEdit (transaction);
      transaction = NULL;
    }
  } while (!transaction);
  transaction_set_splits_to_accounts(transaction, acc1, acc2);
  xaccTransSortSplits(transaction);

  new_trans = xaccTransClone(transaction);
  if (!xaccTransEqual(transaction, new_trans, FALSE, TRUE, FALSE, TRUE))
  {
    failure("xaccTransClone failed.");
  }

  xaccTransReverse(new_trans);
  for (i = 0; i < 2; i++) 
  {
    old = xaccSplitGetAmount(xaccTransGetSplit(transaction, i));
    new = xaccSplitGetAmount(xaccTransGetSplit(new_trans, i));
    result = gnc_numeric_add(old, new, GNC_DENOM_AUTO, GNC_DENOM_FIXED);
    if (gnc_numeric_eq(old, gnc_numeric_neg(new))) 
    {
      msg = g_strdup_printf("Amount of split %d wrong after reversal\n", i);
      failure(msg);
    }

    old = xaccSplitGetValue(xaccTransGetSplit(transaction, i));
    new = xaccSplitGetValue(xaccTransGetSplit(new_trans, i));
    result = gnc_numeric_add(old, new, GNC_DENOM_AUTO, GNC_DENOM_FIXED);
    if (gnc_numeric_eq(old, gnc_numeric_neg(new))) 
    {
      msg = g_strdup_printf("Value of split %d wrong after reversal\n", i);
      failure(msg);
    }

  }
  return;
}

static void
main_helper (void *closure, int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);

  xaccLogDisable ();
  set_success_print (TRUE);

  run_test ();

  success("transaction voiding seems OK");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, main_helper, NULL);
  return 0;
}

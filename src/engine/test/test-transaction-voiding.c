#include <glib.h>
#include <guile/gh.h>
#include <string.h>

#include "GNCIdP.h"

#include "Account.h"
#include "Transaction.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
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
  
  xaccSplitSetAccount(split, a1);

  split = xaccTransGetSplit(tr, 1);
  xaccSplitSetAccount(split, a2);
  return;
}

static void
run_test (void)
{
 
  Account *acc1, *acc2;
  Transaction *transaction;
  gnc_numeric old_amt, new_amt, new_kvp_amt, old_val, new_val, new_kvp_val;
  int rval;
  GNCSession *session;
 
  char *reason = "because I can";

  session = gnc_session_new();

  acc1 = get_random_account(session);
  acc2 = get_random_account(session);

  if (!acc1 || !acc2)
  {
    failure("accounts not created");
  }
   

 transaction = get_random_transaction(session);
 
 transaction_set_splits_to_accounts(transaction, acc1, acc2);
   

 /*  Compromise, check amount on one and value on the other */

 old_amt = xaccSplitGetAmount(xaccTransGetSplit(transaction, 0));
 old_val = xaccSplitGetValue(xaccTransGetSplit(transaction, 1));


 
 xaccTransVoid(transaction, reason);


 if (!xaccTransGetVoidStatus(transaction))
 {
   failure("void status reports false after setting void");
 }

 if (strcmp(reason, xaccTransGetVoidReason(transaction)) != 0)
 {
   failure("Reasons didn't match");
 }
 
 new_amt = xaccSplitGetAmount(xaccTransGetSplit(transaction, 0));
 print_gnc_numeric(new_amt);

 if (!gnc_numeric_zero_p( new_amt))
 {
   failure("Amount not zero after voiding");
 }      

 new_val = xaccSplitGetValue(xaccTransGetSplit(transaction, 1));
 
 if (!(gnc_numeric_zero_p(new_val)))
 {
   failure("Amount not zero after voiding");
 }
 

 if(!(gnc_numeric_eq(old_amt, xaccSplitVoidFormerAmount(xaccTransGetSplit(transaction, 0)))))
 {
   failure("former amount (after voiding) didn't match actual old amount");
 }

 if(!(gnc_numeric_eq(old_val, xaccSplitVoidFormerValue(xaccTransGetSplit(transaction, 1)))))
 {
   failure("former value (after voiding) didn't match actual old value");
 }

 return;
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);

  run_test ();

  success("transaction voiding seems OK");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}


#include <glib.h>
#include <guile/gh.h>

#include "GNCIdP.h"

#include "AccountP.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Transaction.h"
#include "guid.h"


static void
run_test (void)
{
    Account *act1;
    Account *act2;
    Split *spl;
    gnc_numeric num;
    GNCSession *session;
    GNCBook *book;

    session = gnc_session_new ();
    book = gnc_session_get_book (session);

    act1 = get_random_account(book);
    if(!act1)
    {
        failure("act1 not created");
        return;
    }
    
    act2 = get_random_account(book);
    if(!act2)
    {
        failure("act2 not created");
        return;
    }

    num = get_random_gnc_numeric();
    spl = get_random_split(book, num);
    if(!spl)
    {
        failure("spl not created");
        return;
    }

    xaccAccountInsertSplit(act1, spl);

    if(act1 != xaccSplitGetAccount(spl))
    {
        failure("xaccAccountInsertSplit is broken");
        return;
    }

    /* this is weird -- we are testing an engine private function.
     * is this really what is intended here ??? */
    xaccAccountRemoveSplit (act1, spl);

    if(xaccSplitGetAccount(spl))
    {
        failure_args("xaccAccountRemoveSplit()", 
		     __FILE__, __LINE__, "account not NULL");
        return;
    }
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);

  xaccLogDisable ();

  run_test ();

  success("split account crap seems to work");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

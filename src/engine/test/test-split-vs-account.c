
#include <glib.h>
#include <guile/gh.h>

#include "GNCIdP.h"

#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Transaction.h"
#include "guid.h"


static gboolean
is_null_guid(const GUID *id)
{
    int i;
    for(i = 0; i < 16; i++)
    {
        if(id->data[i] != '\0')
        {
            return FALSE;
        }
    }
    return TRUE;
}

static void
run_test (void)
{
    Account *act1;
    Account *act2;
    Split *spl;
    gnc_numeric num;
    GNCSession *session;

    session = gnc_session_new ();

    act1 = get_random_account(session);
    if(!act1)
    {
        failure("act1 not created");
        return;
    }
    
    act2 = get_random_account(session);
    if(!act2)
    {
        failure("act2 not created");
        return;
    }

    num = get_random_gnc_numeric();
    spl = get_random_split(session, num);
    if(!spl)
    {
        failure("spl not created");
        return;
    }

    xaccSplitSetAccount(spl, act1);

    if(act1 != xaccSplitGetAccount(spl))
    {
        failure("xaccSplitSetAccount is broken");
        return;
    }

    if(!guid_equal(xaccAccountGetGUID(act1), xaccSplitGetAccountGUID(spl)))
    {
        failure("xaccSplitGetAccountGUID "
                "after xaccSplitSetAccount failed");
        return;
    }

    xaccSplitSetAccountGUID(spl, *xaccAccountGetGUID(act2));

    if(act2 != xaccSplitGetAccount(spl))
    {
        failure("xaccSplitSetAccountGUID is broken");
        return;
    }

    xaccSplitSetAccount(spl, NULL);

    if(xaccSplitGetAccount(spl))
    {
        failure_args("xaccSplitSetAccount(NULL)", 
		     __FILE__, __LINE__, "account not NULL");
        return;
    }

    if(!is_null_guid(xaccSplitGetAccountGUID(spl)))
    {
        failure_args("xaccSplitSetAccount(NULL)", 
		     __FILE__, __LINE__, "account guid not NULL");
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

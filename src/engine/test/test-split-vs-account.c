
#include <glib.h>

#include "GNCIdP.h"

#include "Account.h"
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

int
main(int argc, char **argv)
{
    Account *act1;
    Account *act2;
    Split *spl;
    gnc_numeric num;

    xaccGUIDInit();

    act1 = get_random_account();
    if(!act1)
    {
        failure("act1 not created");
        return(get_rv());
    }
    
    act2 = get_random_account();
    if(!act2)
    {
        failure("act2 not created");
        return(get_rv());
    }

    num = get_random_gnc_numeric();
    spl = get_random_split(num);
    if(!spl)
    {
        failure("spl not created");
        return(get_rv());
    }

    xaccSplitSetAccount(spl, act1);

    if(act1 != xaccSplitGetAccount(spl))
    {
        failure("xaccSplitSetAccount is broken");
        return(get_rv());
    }

    if(!guid_equal(xaccAccountGetGUID(act1), xaccSplitGetAccountGUID(spl)))
    {
	 failure("xaccSplitGetAccountGUID "
                 "after xaccSplitSetAccount failed");
        return(get_rv());
    }

    xaccSplitSetAccountGUID(spl, *xaccAccountGetGUID(act2));

    if(act2 != xaccSplitGetAccount(spl))
    {
        failure("xaccSplitSetAccountGUID is broken");
        return(get_rv());
    }

    xaccSplitSetAccount(spl, NULL);

    if(xaccSplitGetAccount(spl))
    {
        failure_args("xaccSplitSetAccount(NULL)", 
		     __FILE__, __LINE__, "account not NULL");
        return(get_rv());
    }

    if(!is_null_guid(xaccSplitGetAccountGUID(spl)))
    {
        failure_args("xaccSplitSetAccount(NULL)", 
		     __FILE__, __LINE__, "account guid not NULL");
        return(get_rv());
    }
    
    success("split account crap seems to work");

    print_test_results();
    return(get_rv());
}

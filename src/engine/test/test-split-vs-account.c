/***************************************************************************
 *            test-split-vs-account.c
 *
 *  Tue Sep 27 19:44:50 2005
 *  Copyright  2005  GnuCash team
 *  linux@codehelp.co.uk
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include <glib.h>
#include "qof.h"
#include "cashobjects.h"
#include "AccountP.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Transaction.h"

static void
run_test (void)
{
    Account *act1;
    Account *act2;
    Split *spl;
    QofSession *session;
    QofBook *book;

    session = qof_session_new ();
    book = qof_session_get_book (session);

    act1 = get_random_account(book);
    if(!act1)
    {
        failure("act1 not created");
        return;
    }
    success("act1 created");
    act2 = get_random_account(book);
    if(!act2)
    {
        failure("act2 not created");
        return;
    }
	success("act2 created");
	/* if we use a trans here, the scrub routines
	 will add a pointer to a newly created Orphan-...
	account. */
    spl = get_random_split(book, act1, NULL);
    if(!spl)
    {
        failure("spl not created");
        return;
    }
	success("random split created");
    if(act1 != xaccSplitGetAccount(spl))
    {
        failure("xaccAccountInsertSplit is broken");
        return;
    }
	success("xaccAccountInsertSplit works");
    /* this is weird -- we are testing an engine private function.
     * is this really what is intended here ??? */
    xaccAccountRemoveSplit (act1, spl);

    if(xaccSplitGetAccount(spl))
    {
        failure_args("xaccAccountRemoveSplit()", 
		     __FILE__, __LINE__, "account not NULL");
        return;
    }
	success("xaccSplitGetAccount works");
}

int
main (int argc, char **argv)
{
	qof_init();
	if(cashobjects_register()) {
  xaccLogDisable ();
  run_test ();
  print_test_results();
	}
	qof_close();
  return 0;
}

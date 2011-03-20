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

#include "config.h"
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
    //Split *spl;
    QofSession *session;
    QofBook *book;

    session = qof_session_new ();
    book = qof_session_get_book (session);

    act1 = get_random_account(book);
    do_test(act1 != NULL, "random account created");

    act2 = get_random_account(book);
    do_test(act2 != NULL, "random account created");
#if 0
    spl = get_random_split(book, act1, NULL);
    do_test(spl != NULL, "random split created");

    do_test(act1 == xaccSplitGetAccount(spl), "xaccAccountInsertSplit()");
#endif
    //FIXME
    //xaccSplitSetAccount (spl, NULL);
    //do_test(xaccSplitGetAccount(spl) == NULL, "xaccAccountRemoveSplit()");
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        xaccLogDisable ();
        run_test ();
        print_test_results();
    }
    qof_close();
    return get_rv();
}

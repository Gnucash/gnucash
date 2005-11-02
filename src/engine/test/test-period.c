/***************************************************************************
 *            test-period.c
 *
 *  December 2001
 *  Copyright  2001 Linas Vepstas <linas@linas.org
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 /* 
 * Minimal test to see if a book can be split into two periods
 * without crashing.
 */

#include <ctype.h>
#include <glib.h>
#include "qof.h"
#include <time.h>
#include "cashobjects.h"
#include "Account.h"
#include "Group.h"
#include "Period.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"

static void
run_test (void)
{
  QofSession *sess;
  QofBook *openbook, *closedbook;
  AccountGroup *grp;
  AccountList *acclist, *anode;
  Account *acc, *equity;
  SplitList *splist;
  Split *sfirst, *slast;
  Transaction *tfirst, *tlast;
  Timespec tsfirst, tslast, tsmiddle;
  
  sess = get_random_session ();
  openbook = qof_session_get_book (sess);
  sess = get_random_session ();
  closedbook = qof_session_get_book(sess);
  acc = NULL;
  equity = get_random_account(openbook);
  if (!openbook)
  {
    failure("book not created");
    exit(get_rv());
  }

  add_random_transactions_to_book (openbook, 120);

  grp = xaccGetAccountGroup (openbook);

  acclist = xaccGroupGetSubAccounts (grp);
  for (anode=acclist; anode; anode=anode->next)
  {
    int ns;
    acc = anode->data;
    ns = g_list_length (xaccAccountGetSplitList (acc));
    if (2 <= ns) break;
    acc = NULL;
  }

  if(!acc)
  {
    failure("group didn't have accounts with enough splits");
    exit(get_rv());
  }

  splist = xaccAccountGetSplitList(acc);
  if(!splist)
  {
    failure("account has no transactions");
    exit(get_rv());
  }

  sfirst = splist->data;
  slast = g_list_last(splist) ->data;
  if (sfirst == slast)
  {
    failure("account doesn't have enough transactions");
    exit(get_rv());
  }

  tfirst = xaccSplitGetParent (sfirst);
  tlast = xaccSplitGetParent (slast);
  
  if (!tfirst || !tlast)
  {
    failure("malformed transactions in account");
    exit(get_rv());
  }

  tsfirst = xaccTransRetDatePostedTS (tfirst);
  tslast = xaccTransRetDatePostedTS (tlast);

  if (tsfirst.tv_sec == tslast.tv_sec)
  {
    failure("transactions not time separated");
    exit(get_rv());
  }

  tsmiddle = tsfirst;
  tsmiddle.tv_sec = (tsfirst.tv_sec + tslast.tv_sec)/2;
  gnc_set_logfile (stdout);
  gnc_set_log_level_global (GNC_LOG_FATAL);
  closedbook = gnc_book_close_period (openbook, tsmiddle, 
                  equity, "this is opening balance dude");

  if (!closedbook)
  {
    failure("closed book not created");
    exit(get_rv());
  }

  success ("periods lightly tested and seem to work");
}

int
main (int argc, char **argv)
{
	qof_init();
	if(cashobjects_register()) {
		run_test ();
		print_test_results();
	}
	qof_close();
  return 0;
}

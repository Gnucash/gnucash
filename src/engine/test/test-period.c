
/* test file created by Linas Vepstas <linas@linas.org>
 * December 2001
 * License: GPL
 */

#include <ctype.h>
#include <glib.h>
#include <guile/gh.h>
#include <time.h>

#include "Account.h"
#include "Group.h"
#include "Period.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"



static void
run_test (void)
{
  GNCSession *sess;
  GNCBook *openbook, *closedbook;
  AccountGroup *grp;
  AccountList *acclist, *anode;
  Account * acc = NULL;
  Timespec ts;
  SplitList *splist;
  Split *sfirst, *slast;
  Transaction *tfirst, *tlast;
  Timespec tsfirst, tslast, tsmiddle;
  


  if(!gnc_module_load("gnucash/engine", 0))
  {
    failure("couldn't load gnucash/engine");
    exit(get_rv());
  }

  sess = get_random_session ();
  openbook = gnc_session_get_book (sess);
  if (!openbook)
  {
    failure("book not created");
    exit(get_rv());
  }

  add_random_transactions_to_book (openbook, 120);

  grp = gnc_book_get_group (openbook);

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
    failure("group didn't have accounts with enogh splits");
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

  // stdout is broken with guile for some reason
  // gnc_set_logfile (stdout);
  // gnc_set_log_level_global (GNC_LOG_INFO);
  closedbook = gnc_book_close_period (openbook, tsmiddle, 
                  NULL, "this is opening balance dude");

  if (!closedbook)
  {
    failure("closed book not created");
    exit(get_rv());
  }


  success ("periods work but wern't really tested yet");
}

static void
main_helper (void *closure, int argc, char **argv)
{
  run_test ();

  success ("periods aren't realy tested yet");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile(argc, argv, main_helper, NULL);
  return 0;
}

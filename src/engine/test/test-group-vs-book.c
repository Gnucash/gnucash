
#include <glib.h>
#include <guile/gh.h>

#include "GroupP.h"
#include "GNCIdP.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"


static gboolean
group_has_book (AccountGroup *group, GNCBook *book)
{
  GList *node;

  if (!group)
    return (book == NULL);

  if (xaccGroupGetBook (group) != book)
    return FALSE;

  for (node = xaccGroupGetAccountList (group); node; node = node->next)
  {
    AccountGroup *children = xaccAccountGetChildren (node->data);

    if (!children)
      continue;

    if (!group_has_book (children, book))
      return FALSE;
  }

  return TRUE;
}


static void
run_test (void)
{
  AccountGroup *group1;
  AccountGroup *group2;
  Account *account1;
  Account *account2;
  GNCBook *book;

  book = gnc_book_new ();
  if (!book)
  {
    failure("book not created");
    exit(get_rv());
  }

  group1 = get_random_group (book);
  if(!group1)
  {
    failure("group1 not created");
    exit(get_rv());
  }

  if (!group_has_book (group1, book))
  {
    failure("new group has wrong book");
    exit(get_rv());
  }

  /* this test is testing routines that are private
   * to the engine. these tests are intended to test
   * the engine as a whole, not just the public
   * interface. the maintenance of the correct
   * book pointers is important for correct
   * engine operation. */
  gnc_book_set_group (book, group1);
  if (!group_has_book (group1, book))
  {
    failure("gnc_book_set_group didn't take");
    exit(get_rv());
  }

  group2 = get_random_group (book);
  if(!group2)
  {
    failure("group2 not created");
    exit(get_rv());
  }

  gnc_book_set_group (book, group2);

  if (!group_has_book (group1, NULL))
  {
    failure("gnc_book_set_group didn't clear old");
    exit(get_rv());
  }

  if (!group_has_book (group2, book))
  {
    failure("gnc_book_set_group didn't take");
    exit(get_rv());
  }

  account1 = get_random_account (book);
  if(!account1)
  {
    failure("account1 not created");
    exit(get_rv());
  }

  xaccGroupInsertAccount (group2, account1);
  if (group2 != xaccAccountGetParent (account1))
  {
    failure("group insert account didn't work");
    exit(get_rv());
  }

  account2 = get_random_account (book);
  if(!account2)
  {
    failure("account2 not created");
    exit(get_rv());
  }

  xaccAccountInsertSubAccount (account1, account2);
  if (!group_has_book (xaccAccountGetParent (account2), book))
  {
    failure("account2 has wrong book");
    exit(get_rv());
  }

  xaccGroupRemoveAccount (group2, account1);
  if (xaccAccountGetParent (account1) != NULL)
  {
    failure("remove group didn't take");
    exit(get_rv());
  }

  if (!group_has_book (xaccAccountGetParent (account2), NULL))
  {
    failure("remove group didn't clear book");
    exit(get_rv());
  }
}

static void
main_helper (int argc, char **argv)
{
  int i;

  gnc_module_load("gnucash/engine", 0);

  xaccLogDisable ();

  for (i = 0; i < 10; i++)
    run_test ();

  success ("group/book stuff seems to work");
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

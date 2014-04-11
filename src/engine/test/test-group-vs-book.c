/***************************************************************************
 *            test-group-vs-book.c
 *
 *  Tue Sep 27 19:32:31 2005
 *  Copyright  2005  Gnucash team
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
#include "Group.h"
#include "GroupP.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static gboolean
group_has_book (AccountGroup *group, QofBook *book)
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
  QofBook *book;

  book = qof_book_new ();
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

  /* This test is testing routines that are private
   * to the engine. these tests are intended to test
   * the engine as a whole, not just the public
   * interface. the maintenance of the correct
   * book pointers is important for correct
   * engine operation. */
  xaccSetAccountGroup (book, group1);
  if (!group_has_book (group1, book))
  {
    failure("xaccSetAccountGroup didn't take");
    exit(get_rv());
  }

  group2 = get_random_group (book);
  if(!group2)
  {
    failure("group2 not created");
    exit(get_rv());
  }

  xaccSetAccountGroup (book, group2);

#if 0
  /* a group cannot have a 'null' book; this test is nonsense. */
  if (!group_has_book (group1, NULL))
  {
    failure("xaccSetAccountGroup didn't clear old");
    exit(get_rv());
  }
#endif

  if (!group_has_book (group2, book))
  {
    failure("xaccSetAccountGroup didn't take");
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
}

int
main (int argc, char **argv)
{
	gint i;
	qof_init();
	if(cashobjects_register()) {
  xaccLogDisable ();
  for (i = 0; i < 10; i++)
		{
    run_test ();
		}
  success ("group/book stuff seems to work");
  print_test_results();
	}
	qof_close();
  return get_rv();
}

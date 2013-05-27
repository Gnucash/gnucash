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
#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static gboolean
account_tree_has_book (Account *parent, QofBook *book)
{
    GList *children, *node;

    if (!parent)
        return (book == NULL);

    if (gnc_account_get_book(parent) != book)
        return FALSE;

    children = gnc_account_get_children(parent);
    for (node = children; node; node = node->next)
    {
        if (!account_tree_has_book (node->data, book))
            return FALSE;
    }
    g_list_free(children);

    return TRUE;
}


static void
run_test (void)
{
    Account *root1;
    Account *root2;
    Account *account1;
    Account *account2;
    QofBook *book;

    book = qof_book_new ();
    if (!book)
    {
        failure("book not created");
        exit(get_rv());
    }

    root1 = get_random_account (book);
    if (!root1)
    {
        failure("root1 not created");
        exit(get_rv());
    }

    if (!account_tree_has_book (root1, book))
    {
        failure("new root has wrong book");
        exit(get_rv());
    }

    /* This test is testing routines that are private
     * to the engine. these tests are intended to test
     * the engine as a whole, not just the public
     * interface. the maintenance of the correct
     * book pointers is important for correct
     * engine operation. */
    gnc_book_set_root_account (book, root1);
    if (!account_tree_has_book (root1, book))
    {
        failure("gnc_book_set_root_account didn't take");
        exit(get_rv());
    }

    root2 = get_random_account (book);
    if (!root2)
    {
        failure("root2 not created");
        exit(get_rv());
    }

    gnc_book_set_root_account (book, root2);

#if 0
    /* a group cannot have a 'null' book; this test is nonsense. */
    if (!account_tree_has_book (root1, NULL))
    {
        failure("gnc_book_set_root_account didn't clear old");
        exit(get_rv());
    }
#endif

    if (!account_tree_has_book (root2, book))
    {
        failure("gnc_book_set_root_account didn't take");
        exit(get_rv());
    }

    account1 = get_random_account (book);
    if (!account1)
    {
        failure("account1 not created");
        exit(get_rv());
    }

    gnc_account_append_child (root2, account1);
    if (root2 != gnc_account_get_parent (account1))
    {
        failure("group insert account didn't work");
        exit(get_rv());
    }

    account2 = get_random_account (book);
    if (!account2)
    {
        failure("account2 not created");
        exit(get_rv());
    }

    gnc_account_append_child (account1, account2);
    if (!account_tree_has_book (gnc_account_get_parent (account2), book))
    {
        failure("account2 has wrong book");
        exit(get_rv());
    }

    gnc_account_remove_child (root2, account1);
    if (gnc_account_get_parent (account1) != NULL)
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
    if (cashobjects_register())
    {
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

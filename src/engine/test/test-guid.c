/***************************************************************************
 *            test-guid.c
 *
 *  October 2003
 *  Copyright  2003 Linas Vepstas <linas@linas.org
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

/* Test file created by Linas Vepstas <linas@linas.org>
 * Try to create duplicate GncGUID's, which should never happen.
 *
 */

#include "config.h"
#include <ctype.h>
#include <glib.h>
#include "cashobjects.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "qof.h"

#define NENT 500123

static void test_null_guid(void)
{
    GncGUID g;
    GncGUID *gp;

    g = guid_new_return();
    gp = guid_malloc();
    guid_new(gp);

    do_test(guid_equal(guid_null(), guid_null()), "null guids equal");
    do_test(!guid_equal(&g, gp), "two guids equal");
}

static void
run_test (void)
{
    int i;
    QofSession *sess;
    QofBook *book;
    QofInstance *ent, *eblk[NENT];
    QofCollection *col;
    QofIdType type;
    GncGUID guid;

    sess = get_random_session ();
    book = qof_session_get_book (sess);
    do_test ((NULL != book), "book not created");

    col = qof_book_get_collection (book, "asdf");
    type = qof_collection_get_type (col);

    for (i = 0; i < NENT; i++)
    {
        ent = g_object_new(QOF_TYPE_INSTANCE, NULL);
        eblk[i] = ent;
        guid_new(&guid);
        ent = g_object_new(QOF_TYPE_INSTANCE, "guid", &guid, NULL);
        do_test ((NULL == qof_collection_lookup_entity (col, &guid)),
                 "duplicate guid");
        ent->e_type = type;
        qof_collection_insert_entity (col, ent);
        do_test ((NULL != qof_collection_lookup_entity (col, &guid)),
                 "guid not found");
    }

    /* Make valgrind happy -- destroy the session. */
    qof_session_destroy(sess);
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        test_null_guid();
        run_test ();
        print_test_results();
    }
    qof_close();
    return get_rv();
}

/***************************************************************************
 *            test-object.c
 *
 *  Tue Sep 27 19:37:28 2005
 *  Copyright  2005  GnuCash team
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
/*
* Lightly test the QofObject infrastructure.
*/
#include "config.h"
#include <glib.h>
#include <glib/gi18n.h>
#include "qof.h"
#include "cashobjects.h"
#include "test-stuff.h"

#define TEST_MODULE_NAME "object-test"
#define TEST_MODULE_DESC "Test Object"

static void obj_foreach (const QofCollection *, QofInstanceForeachCB, gpointer);
static const char * printable (gpointer obj);
static void test_printable (const char *name, gpointer obj);
static void test_foreach (QofBook *, const char *);

static QofObject bus_obj =
{
interface_version:
    QOF_OBJECT_VERSION,
e_type:
    TEST_MODULE_NAME,
type_label:
    TEST_MODULE_DESC,
create:
    NULL,
book_begin:
    NULL,
book_end:
    NULL,
is_dirty:
    NULL,
mark_clean:
    NULL,
foreach:
    obj_foreach,
printable:
    printable,
version_cmp:
    NULL,
};

static void
test_object (void)
{
    QofBook *book = qof_book_new();

    do_test ((NULL != book), "book null");

    /* Test the global registration and lookup functions */
    {
        do_test (!qof_object_register (NULL), "register NULL");
        do_test (qof_object_register (&bus_obj), "register test object");
        do_test (!qof_object_register (&bus_obj), "register test object again");
        do_test (qof_object_lookup (TEST_MODULE_NAME) == &bus_obj,
                 "lookup our installed object");
        do_test (qof_object_lookup ("snm98sn snml say  dyikh9y9ha") == NULL,
                 "lookup non-existant object object");

        do_test (!safe_strcmp (qof_object_get_type_label (TEST_MODULE_NAME),
                               _(TEST_MODULE_DESC)),
                 "test description return");
    }

    test_foreach (book, TEST_MODULE_NAME);
    test_printable (TEST_MODULE_NAME, (gpointer)1);
}

static void
obj_foreach (const QofCollection *col, QofInstanceForeachCB cb, gpointer u_d)
{
    int *foo = u_d;

    do_test (col != NULL, "foreach: NULL collection");
    success ("called foreach callback");

    *foo = 1;
}

static void foreachCB (QofInstance *ent, gpointer u_d)
{
    do_test (FALSE, "FAIL");
}

static const char *
printable (gpointer obj)
{
    do_test (obj != NULL, "printable: object is NULL");
    success ("called printable callback");
    return ((const char *)obj);
}

static void
test_foreach (QofBook *book, const char *name)
{
    int res = 0;

    qof_object_foreach (NULL, NULL, NULL, &res);
    do_test (res == 0, "object: Foreach: NULL, NULL, NULL");
    qof_object_foreach (NULL, NULL, foreachCB, &res);
    do_test (res == 0, "object: Foreach: NULL, NULL, foreachCB");

    qof_object_foreach (NULL, book, NULL, &res);
    do_test (res == 0, "object: Foreach: NULL, book, NULL");
    qof_object_foreach (NULL, book, foreachCB, &res);
    do_test (res == 0, "object: Foreach: NULL, book, foreachCB");

    qof_object_foreach (name, NULL, NULL, &res);
    do_test (res == 0, "object: Foreach: name, NULL, NULL");
    qof_object_foreach (name, NULL, foreachCB, &res);
    do_test (res == 0, "object: Foreach: name, NULL, foreachCB");

    qof_object_foreach (name, book, NULL, &res);
    do_test (res != 0, "object: Foreach: name, book, NULL");

    res = 0;
    qof_object_foreach (name, book, foreachCB, &res);
    do_test (res != 0, "object: Foreach: name, book, foreachCB");
}

static void
test_printable (const char *name, gpointer obj)
{
    const char *res;

    do_test (qof_object_printable (NULL, NULL) == NULL,
             "object: Printable: NULL, NULL");
    do_test (qof_object_printable (NULL, obj) == NULL,
             "object: Printable: NULL, object");
    do_test (qof_object_printable (name, NULL) == NULL,
             "object: Printable: mod_name, NULL");
    res = qof_object_printable (name, obj);
    do_test (res != NULL, "object: Printable: mod_name, object");
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        test_object();
        print_test_results();
    }
    qof_close();
    return get_rv();
}

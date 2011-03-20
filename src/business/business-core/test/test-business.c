/*********************************************************************
 * test-business.c
 * Test the business code.
 *
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 *********************************************************************/

#include "config.h"
#include <glib.h>
#include <libguile.h>

#include "qof.h"
#include "gnc-module.h"

#include "gncBusiness.h"
#include "test-stuff.h"

#define TEST_MODULE_NAME "business-test"
#define TEST_MODULE_DESC "Test Business"

#if 0
static GList * get_list (QofBook *, gboolean show_all);
static const char * printable (gpointer obj);
static void test_printable (const char *name, gpointer obj);
static void test_get_list (QofBook *, const char *);

static GncBusinessObject bus_obj =
{
    GNC_BUSINESS_VERSION,
    TEST_MODULE_NAME,
    TEST_MODULE_DESC,
    NULL,				/* create */
    NULL,				/* destroy */
    get_list,
    printable,
};

static void test_business (void)
{
    /* Test the global registration and lookup functions */
    {
        do_test (!gncBusinessRegister (NULL), "register NULL");
        do_test (gncBusinessRegister (&bus_obj), "register test object");
        do_test (!gncBusinessRegister (&bus_obj), "register test object again");
        do_test (gncBusinessLookup (TEST_MODULE_NAME) == &bus_obj,
                 "lookup our installed object");
        do_test (gncBusinessLookup ("snm98sn snml say  dyikh9y9ha") == NULL,
                 "lookup non-existant business object");

        do_test (!safe_strcmp (gncBusinessGetTypeLabel (TEST_MODULE_NAME),
                               _(TEST_MODULE_DESC)),
                 "test description return");
    }

    test_get_list ((QofBook*)1, TEST_MODULE_NAME);
    test_printable (TEST_MODULE_NAME, (gpointer)1);
}

static GList *
get_list (QofBook *book, gboolean show_all)
{
    do_test (book != NULL, "get_list: NULL business");
    success ("called get_list callback");
    return ((GList *)1);
}

static const char *
printable (gpointer obj)
{
    do_test (obj != NULL, "printable: object is NULL");
    success ("called printable callback");
    return ((const char *)obj);
}

static void
test_get_list (QofBook *book, const char *name)
{
    GList *res;

    do_test (gncBusinessGetList (NULL, NULL, FALSE) == NULL,
             "business: GetList: NULL, NULL, FALSE");
    do_test (gncBusinessGetList (NULL, name, FALSE) == NULL,
             "business: GetList: NULL, mod_name, FALSE");
    do_test (gncBusinessGetList (book, NULL, FALSE) == NULL,
             "business: GetList: book, NULL, FALSE");
    res = gncBusinessGetList (book, name, FALSE);
    do_test (res != NULL, "business: GetList: book, mod_name, FALSE");
}

static void
test_printable (const char *name, gpointer obj)
{
    const char *res;

    do_test (gncBusinessPrintable (NULL, NULL) == NULL,
             "business: Printable: NULL, NULL");
    do_test (gncBusinessPrintable (NULL, obj) == NULL,
             "business: Printable: NULL, object");
    do_test (gncBusinessPrintable (name, NULL) == NULL,
             "business: Printable: mod_name, NULL");
    res = gncBusinessPrintable (name, obj);
    do_test (res != NULL, "business: Printable: mod_name, object");
}

static void
main_helper (void *closure, int argc, char **argv)
{
    gnc_module_load("gnucash/business-core", 0);
    test_business();
    print_test_results();
    exit(get_rv());
}
#endif

int
main (int argc, char **argv)
{
    //  scm_boot_guile (argc, argv, main_helper, NULL);
    return 0;
}

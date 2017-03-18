/*********************************************************************
 * test-customer.c
 * Test the customer object (without Guile/Scheme)
 *
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
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

#include <config.h>
#include <glib.h>
#include <qof.h>
#include <qofinstance-p.h>

#include "cashobjects.h"
#include "gncCustomerP.h"
#include "gncInvoiceP.h"
#include "gncJobP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (QofBook *book, const char *message,
                 void (*set) (GncCustomer *, const char *str),
                 const char * (*get)(const GncCustomer *));

static void
test_numeric_fcn (QofBook *book, const char *message,
                  void (*set) (GncCustomer *, gnc_numeric),
                  gnc_numeric (*get)(const GncCustomer *));

static void
test_bool_fcn (QofBook *book, const char *message,
               void (*set) (GncCustomer *, gboolean),
               gboolean (*get) (const GncCustomer *));

static void
test_customer (void)
{
    QofBook *book;
    GncCustomer *customer;

    book = qof_book_new ();

    /* Test creation/destruction */
    {
        do_test (gncCustomerCreate (NULL) == NULL, "customer create NULL");
        customer = gncCustomerCreate (book);
        do_test (customer != NULL, "customer create");
        do_test (gncCustomerGetBook (customer) == book, "getbook");

        gncCustomerBeginEdit (customer);
        gncCustomerDestroy (customer);
        success ("create/destroy");
    }

    /* Test setting/getting routines; does the active flag get set right? */
    {
        GncGUID guid;

        test_string_fcn (book, "Id", gncCustomerSetID, gncCustomerGetID);
        test_string_fcn (book, "Name", gncCustomerSetName, gncCustomerGetName);
        test_string_fcn (book, "Notes", gncCustomerSetNotes, gncCustomerGetNotes);

        //test_string_fcn (book, "Terms", gncCustomerSetTerms, gncCustomerGetTerms);

        test_numeric_fcn (book, "Discount", gncCustomerSetDiscount, gncCustomerGetDiscount);
        test_numeric_fcn (book, "Credit", gncCustomerSetCredit, gncCustomerGetCredit);

        test_bool_fcn (book, "Active", gncCustomerSetActive, gncCustomerGetActive);

        do_test (gncCustomerGetAddr (customer) != NULL, "Addr");
        do_test (gncCustomerGetShipAddr (customer) != NULL, "ShipAddr");

        guid_replace (&guid);
        customer = gncCustomerCreate (book);
        count++;
        gncCustomerSetGUID (customer, &guid);
        do_test (guid_equal (&guid, gncCustomerGetGUID (customer)), "guid compare");
    }
    {
        GList *list;

        list = gncBusinessGetList (book, GNC_ID_CUSTOMER, TRUE);
        do_test (list != NULL, "getList all");
        do_test (g_list_length (list) == count, "correct length: all");
        g_list_free (list);

        list = gncBusinessGetList (book, GNC_ID_CUSTOMER, FALSE);
        do_test (list != NULL, "getList active");
        do_test (g_list_length (list) == 1, "correct length: active");
        g_list_free (list);
    }
    {
        const char *str = get_random_string();
        const char *res;

        res = NULL;
        gncCustomerBeginEdit(customer);
        gncCustomerSetName (customer, str);
        gncCustomerCommitEdit(customer);
        res = qof_object_printable (GNC_ID_CUSTOMER, customer);
        do_test (res != NULL, "Printable NULL?");
        do_test (g_strcmp0 (str, res) == 0, "Printable equals");
    }

    do_test (gncCustomerGetJoblist (customer, TRUE) == NULL, "joblist empty");

    /* Test the Entity Table */
    {
        const GncGUID *guid;

        guid = gncCustomerGetGUID (customer);
        do_test (gncCustomerLookup (book, guid) == customer, "Entity Table");
    }

    /* Note: JobList is tested from the Job tests */
    qof_book_destroy (book);
}

static void
test_string_fcn (QofBook *book, const char *message,
                 void (*set) (GncCustomer *, const char *str),
                 const char * (*get)(const GncCustomer *))
{
    GncCustomer *customer = gncCustomerCreate (book);
    char const *str = get_random_string ();

    do_test (!gncCustomerIsDirty (customer), "test if start dirty");
    gncCustomerBeginEdit (customer);
    set (customer, str);
    /* Customer record should be dirty */
    do_test (gncCustomerIsDirty (customer), "test dirty later");
    gncCustomerCommitEdit (customer);
    /* Customer record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
    do_test (g_strcmp0 (get (customer), str) == 0, message);
    gncCustomerSetActive (customer, FALSE);
    count++;
}

static void
test_numeric_fcn (QofBook *book, const char *message,
                  void (*set) (GncCustomer *, gnc_numeric),
                  gnc_numeric (*get)(const GncCustomer *))
{
    GncCustomer *customer = gncCustomerCreate (book);
    gnc_numeric num = gnc_numeric_create (17, 1);

    do_test (!gncCustomerIsDirty (customer), "test if start dirty");
    gncCustomerBeginEdit (customer);
    set (customer, num);
    /* Customer record should be dirty */
    do_test (gncCustomerIsDirty (customer), "test dirty later");
    gncCustomerCommitEdit (customer);
    /* Customer record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
    do_test (gnc_numeric_equal (get (customer), num), message);
    gncCustomerSetActive (customer, FALSE);
    count++;
}

static void
test_bool_fcn (QofBook *book, const char *message,
               void (*set) (GncCustomer *, gboolean),
               gboolean (*get) (const GncCustomer *))
{
    GncCustomer *customer = gncCustomerCreate (book);
    gboolean num = get_random_boolean ();

    do_test (!gncCustomerIsDirty (customer), "test if start dirty");
    gncCustomerBeginEdit (customer);
    set (customer, FALSE);
    set (customer, TRUE);
    set (customer, num);
    /* Customer record should be dirty */
    do_test (gncCustomerIsDirty (customer), "test dirty later");
    gncCustomerCommitEdit (customer);
    /* Customer record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
    do_test (get (customer) == num, message);
    gncCustomerSetActive (customer, FALSE);
    count++;
}

int
main (int argc, char **argv)
{
    qof_init();
    do_test (cashobjects_register(), "Cannot register cash objects");
    /* These three registrations are done during cashobjects_register,
       so trying to register them again naturally fails. */
#if 0
    do_test (gncInvoiceRegister(), "Cannot register GncInvoice");
    do_test (gncJobRegister (),  "Cannot register GncJob");
    do_test (gncCustomerRegister(), "Cannot register GncCustomer");
#endif
    test_customer();
    print_test_results();
    qof_close ();
    return get_rv();
}

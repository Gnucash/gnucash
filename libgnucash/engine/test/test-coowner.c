/*********************************************************************
 * test-coowner.c
 * Test the coowner object
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
#include "gncCoownerP.h"
#include "gncInvoiceP.h"
#include "gncJobP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncCoowner *, const char *str),
		 const char * (*get)(const GncCoowner *));

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncCoowner *, gnc_numeric),
		  gnc_numeric (*get)(const GncCoOwner *));

static void
test_bool_fcn (QofBook *book, const char *message,
	       void (*set) (GncCoOwner *, gboolean),
	       gboolean (*get) (const GncCoOwner *));

static void
test_coowner (void)
{
    QofBook *book;
    GncCoOwner *coowner;

    book = qof_book_new ();

    /* Test creation/destruction */
    {
	do_test (gncCoOwnerCreate (NULL) == NULL, "coowner create NULL");
	coowner = gncCoOwnerCreate (book);
	do_test (coowner != NULL, "coowner create");
	do_test (gncCoOwnerGetBook (coowner) == book, "getbook");

	gncCoOwnerBeginEdit (coowner);
	gncCoOwnerDestroy (coowner);
	success ("create/destroy");
    }

    /* Test setting/getting routines; does the active flag get set right? */
    {
	GncGUID guid;

	test_string_fcn (book, "Id", gncCoOwnerSetID, gncCoOwnerGetID);
	test_bool_fcn (book, "Active", gncCoOwnerSetActive, gncCoOwnerGetActive);
	test_numerc_fcn (book, "AptUnit", gncCoOwnerSetAptUnit, gncCoOwnerGetAptUnit);
	test_numerc_fcn (book, "AptShare", gncCoOwnerSetAptShare, gncCoOwnerGetAptShare);
	test_numeric_fcn (book, "Credit", gncCoOwnerSetCredit, gncCoOwnerGetCredit);
	test_numeric_fcn (book, "Discount", gncCoOwnerSetDiscount, gncCoOwnerGetDiscount);
	test_string_fcn (book, "DistributionKey", gncCoOwnerSetDistributionKey, gncCoOwnerGetDistributionKey);
	test_string_fcn (book, "Notes", gncCoOwnerSetNotes, gncCoOwnerGetNotes);
	//test_string_fcn (book, "Terms", gncCoOwnerSetTerms, gncCoOwnerGetTerms);
	test_string_fcn (book, "Usermame", gncCoOwnerSetUsername, gncCoOwnerGetUsername);

	do_test (gncCoOwnerGetAddr (coowner) != NULL, "Addr");

	guid_replace (&guid);
	coowner = gncCoOwnerCreate (book);
	count++;
	gncCoOwnerSetGUID (coowner, &guid);
	do_test (guid_equal (&guid, gncCoOwnerGetGUID (coowner)), "guid compare");
    }
    {
	GList *list;

	list = gncBusinessGetList (book, GNC_ID_COOWNER, TRUE);
	do_test (list != NULL, "getList all");
	do_test (g_list_length (list) == count, "correct length: all");
	g_list_free (list);

	list = gncBusinessGetList (book, GNC_ID_COOWNER, FALSE);
	do_test (list != NULL, "getList active");
	do_test (g_list_length (list) == 1, "correct length: active");
	g_list_free (list);
    }
    {
	const char *str = get_random_string();
	const char *res;

	res = NULL;
	gncCoOwnerBeginEdit(coowner);
	gncCoOwnerSetName (coowner, str);
	gncCoOwnerCommitEdit(coowner);
	res = qof_object_printable (GNC_ID_COOWNER, coowner);
	do_test (res != NULL, "Printable NULL?");
	do_test (g_strcmp0 (str, res) == 0, "Printable equals");
    }

    do_test (gncCoOwnerGetJoblist (coowner, TRUE) == NULL, "joblist empty");

    /* Test the Entity Table */
    {
	const GncGUID *guid;

	guid = gncCoOwnerGetGUID (coowner);
	do_test (gncCoOwnerLookup (book, guid) == coowner, "Entity Table");
    }

    /* Note: JobList is tested from the Job tests */
    qof_book_destroy (book);
}

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncCoOwner *, const char *str),
		 const char * (*get)(const GncCoOwner *))
{
    GncCoOwner *coowner = gncCoOwnerCreate (book);
    char const *str = get_random_string ();

    do_test (!gncCoOwnerIsDirty (coowner), "test if start dirty");
    gncCoOwnerBeginEdit (coowner);
    set (coowner, str);
    /* CoOwner record should be dirty */
    do_test (gncCoOwnerIsDirty (coowner), "test dirty later");
    gncCoOwnerCommitEdit (coowner);
    /* CoOwner record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCoOwnerIsDirty (coowner), "test dirty after commit");
    do_test (g_strcmp0 (get (coowner), str) == 0, message);
    gncCoOwnerSetActive (coowner, FALSE);
    count++;
}

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncCoOwner *, gnc_numeric),
		  gnc_numeric (*get)(const GncCoOwner *))
{
    GncCoOwner *coowner = gncCoOwnerCreate (book);
    gnc_numeric num = gnc_numeric_create (17, 1);

    do_test (!gncCoOwnerIsDirty (coowner), "test if start dirty");
    gncCoOwnerBeginEdit (coowner);
    set (coowner, num);
    /* CoOwner record should be dirty */
    do_test (gncCoOwnerIsDirty (coowner), "test dirty later");
    gncCoOwnerCommitEdit (coowner);
    /* CoOwner record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCoOwnerIsDirty (coowner), "test dirty after commit");
    do_test (gnc_numeric_equal (get (coowner), num), message);
    gncCoOwnerSetActive (coowner, FALSE);
    count++;
}

static void
test_bool_fcn (QofBook *book, const char *message,
	       void (*set) (GncCoOwner *, gboolean),
	       gboolean (*get) (const GncCoOwner *))
{
    GncCoOwner *coowner = gncCoOwnerCreate (book);
    gboolean num = get_random_boolean ();

    do_test (!gncCoOwnerIsDirty (coowner), "test if start dirty");
    gncCoOwnerBeginEdit (coowner);
    set (coowner, FALSE);
    set (coowner, TRUE);
    set (coowner, num);
    /* CoOwner record should be dirty */
    do_test (gncCoOwnerIsDirty (coowner), "test dirty later");
    gncCoOwnerCommitEdit (coowner);
    /* CoOwner record should be not dirty */
    /* Skip, because will always fail without a backend.
     * It's not possible to load a backend in the engine code
     * without having circular dependencies.
     */
    // do_test (!gncCoOwnerIsDirty (coowner), "test dirty after commit");
    do_test (get (coowner) == num, message);
    gncCoOwnerSetActive (coowner, FALSE);
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
    do_test (gncCoOwnerRegister(), "Cannot register GncCoOwner");
#endif
    test_coowner();
    print_test_results();
    qof_close ();
    return get_rv();
}

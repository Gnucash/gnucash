/*********************************************************************
 * test-vendor.c
 * Test the vendor object.
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

#include "config.h"
#include <glib.h>
#include "gncInvoiceP.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncVendorP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncVendor *, const char *str),
		 const char * (*get)(const GncVendor *));

#if 0
static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncVendor *, gnc_numeric),
		  gnc_numeric (*get)(const GncVendor *));
#endif

static void
test_bool_fcn (QofBook *book, const char *message,
		  void (*set) (GncVendor *, gboolean),
		  gboolean (*get) (const GncVendor *));

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncVendor *, gint),
	       gint (*get) (const GncVendor *));
#endif

static void
test_vendor (void)
{
  QofBackend *be;
  QofSession *session;
  QofBook *book;
  GncVendor *vendor;

  session = qof_session_new();
  be = NULL;
  qof_session_begin(session, QOF_STDOUT, FALSE, FALSE);
  book = qof_session_get_book (session);
  be = qof_book_get_backend(book);

  /* The book *must* have a backend to pass the test of the 'dirty' flag */
  /* See the README file for details */
  do_test (be != NULL, "qsf backend could not be set");

  /* Test creation/destruction */
  {
    do_test (gncVendorCreate (NULL) == NULL, "vendor create NULL");
    vendor = gncVendorCreate (book);
    do_test (vendor != NULL, "vendor create");
    do_test (qof_instance_get_book (QOF_INSTANCE(vendor)) == book,
	     "getbook");

    gncVendorBeginEdit (vendor);
    gncVendorDestroy (vendor);
    success ("create/destroy");
  }

  /* Test setting/getting routines; does the active flag get set right? */
  {
    GUID guid;

    test_string_fcn (book, "Id", gncVendorSetID, gncVendorGetID);
    test_string_fcn (book, "Name", gncVendorSetName, gncVendorGetName);
    test_string_fcn (book, "Notes", gncVendorSetNotes, gncVendorGetNotes);

    //test_string_fcn (book, "Terms", gncVendorSetTerms, gncVendorGetTerms);

    //test_bool_fcn (book, "TaxIncluded", gncVendorSetTaxIncluded, gncVendorGetTaxIncluded);
    test_bool_fcn (book, "Active", gncVendorSetActive, gncVendorGetActive);

    do_test (gncVendorGetAddr (vendor) != NULL, "Addr");

    guid_new (&guid);
    vendor = gncVendorCreate (book); count++;
    gncVendorSetGUID (vendor, &guid);
    do_test (guid_equal (&guid, qof_instance_get_guid(QOF_INSTANCE(vendor))), "guid compare");
  }
#if 0
  {
    GList *list;

    list = gncBusinessGetList (book, GNC_ID_VENDOR, TRUE);
    do_test (list != NULL, "getList all");
    do_test (g_list_length (list) == count, "correct length: all");
    g_list_free (list);

    list = gncBusinessGetList (book, GNC_ID_VENDOR, FALSE);
    do_test (list != NULL, "getList active");
    do_test (g_list_length (list) == 1, "correct length: active");
    g_list_free (list);
  }
#endif
  {
    const char *str = get_random_string();
    const char *res;

    gncVendorSetName (vendor, str);
    res = qof_object_printable (GNC_ID_VENDOR, vendor);
    do_test (res != NULL, "Printable NULL?");
    do_test (safe_strcmp (str, res) == 0, "Printable equals");
  }    
}

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncVendor *, const char *str),
		 const char * (*get)(const GncVendor *))
{
  GncVendor *vendor = gncVendorCreate (book);
  char const *str = get_random_string ();

  do_test (!gncVendorIsDirty (vendor), "test if start dirty");
  gncVendorBeginEdit (vendor);
  set (vendor, str);
  do_test (gncVendorIsDirty (vendor), "test dirty later");
  gncVendorCommitEdit (vendor);
  do_test (gncVendorIsDirty (vendor), "test dirty after commit");
  do_test (safe_strcmp (get (vendor), str) == 0, message);
  gncVendorSetActive (vendor, FALSE); count++;
}

#if 0
static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncVendor *, gnc_numeric),
		  gnc_numeric (*get)(const GncVendor *))
{
  GncVendor *vendor = gncVendorCreate (book);
  gnc_numeric num = gnc_numeric_create (17, 1);

  do_test (!gncVendorIsDirty (vendor), "test if start dirty");
  gncVendoryBeginEdit (vendor);
  set (vendor, num);
  do_test (gncVendorIsDirty (vendor), "test dirty later");
  gncVendorCommitEdit (vendor);
  do_test (!gncVendorIsDirty (vendor), "test dirty after commit");
  do_test (gnc_numeric_equal (get (vendor), num), message);
  gncVendorSetActive (vendor, FALSE); count++;
}
#endif

static void
test_bool_fcn (QofBook *book, const char *message,
	       void (*set) (GncVendor *, gboolean),
	       gboolean (*get) (const GncVendor *))
{
  GncVendor *vendor = gncVendorCreate (book);
  gboolean num = get_random_boolean ();

  do_test (!gncVendorIsDirty (vendor), "test if start dirty");
  gncVendorBeginEdit (vendor);
  set (vendor, FALSE);
  set (vendor, TRUE);
  set (vendor, num);
  do_test (gncVendorIsDirty (vendor), "test dirty later");
  gncVendorCommitEdit (vendor);
  do_test (gncVendorIsDirty (vendor), "test dirty after commit");
  do_test (get (vendor) == num, message);
  gncVendorSetActive (vendor, FALSE); count++;
}

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncVendor *, gint),
	       gint (*get) (const GncVendor *))
{
  GncVendor *vendor = gncVendorCreate (book);
  gint num = 17;

  do_test (!gncVendorIsDirty (vendor), "test if start dirty");
  gncVendorBeginEdit (vendor);
  set (vendor, num);
  do_test (gncVendorIsDirty (vendor), "test dirty later");
  gncVendorCommitEdit (vendor);
  do_test (!gncVendorIsDirty (vendor), "test dirty after commit");
  do_test (get (vendor) == num, message);
  gncVendorSetActive (vendor, FALSE); count++;
}
#endif

int
main (int argc, char **argv)
{
  qof_init();
  do_test (gncInvoiceRegister(), "Cannot register GncInvoice");
  do_test (gncJobRegister (),  "Cannot register GncJob");
  do_test (gncCustomerRegister(), "Cannot register GncCustomer");
  do_test (gncVendorRegister(), "Cannot register GncVendor");		
  test_vendor();
  print_test_results();
  qof_close();
  return 0;
}

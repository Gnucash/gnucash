/*********************************************************************
 * test-employee.c
 * Test the employee object (without Guile).
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
#include "qof.h"
#include "gncEmployeeP.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncInvoiceP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncEmployee *, const char *str),
		 const char * (*get)(const GncEmployee *));

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncEmployee *, gnc_numeric),
		  gnc_numeric (*get)(const GncEmployee *));

static void
test_bool_fcn (QofBook *book, const char *message,
		  void (*set) (GncEmployee *, gboolean),
		  gboolean (*get) (const GncEmployee *));

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncEmployee *, gint),
	       gint (*get) (GncEmployee *));
#endif

static void
test_employee (void)
{
  QofBackend *be;
  QofBook *book;
  QofSession *session;
  GncEmployee *employee;

  session = qof_session_new();
  qof_session_begin(session, QOF_STDOUT, FALSE, FALSE);
  book = qof_session_get_book(session);
  /* The book *must* have a backend to pass the test of the 'dirty' flag */
  /* See the README file for details */
  
  be = qof_book_get_backend (book);

  /* Test creation/destruction */
  {
    do_test (gncEmployeeCreate (NULL) == NULL, "employee create NULL");
    employee = gncEmployeeCreate (book);
    do_test (employee != NULL, "employee create");
    do_test (qof_instance_get_book(QOF_INSTANCE(employee)) == book,
	     "getbook");

    gncEmployeeBeginEdit (employee);
    gncEmployeeDestroy (employee);
    success ("create/destroy");
  }

  /* Test setting/getting routines; does the active flag get set right? */
  {
    GUID guid;

    test_string_fcn (book, "Id", gncEmployeeSetID, gncEmployeeGetID);
    test_string_fcn (book, "Username", gncEmployeeSetUsername, gncEmployeeGetUsername);
    test_string_fcn (book, "Language", gncEmployeeSetLanguage, gncEmployeeGetLanguage);
    test_string_fcn (book, "Acl", gncEmployeeSetAcl, gncEmployeeGetAcl);

    test_numeric_fcn (book, "Workday", gncEmployeeSetWorkday, gncEmployeeGetWorkday);
    test_numeric_fcn (book, "Rate", gncEmployeeSetRate, gncEmployeeGetRate);

    test_bool_fcn (book, "Active", gncEmployeeSetActive, gncEmployeeGetActive);

    do_test (gncEmployeeGetAddr (employee) != NULL, "Addr");

    guid_new (&guid);
    employee = gncEmployeeCreate (book); count++;
    gncEmployeeSetGUID (employee, &guid);
    do_test (guid_equal (&guid, qof_instance_get_guid(QOF_INSTANCE(employee))), "guid compare");
  }
#if 0
  {
    GList *list;

    list = gncBusinessGetList (book, GNC_EMPLOYEE_MODULE_NAME, TRUE);
    do_test (list != NULL, "getList all");
    do_test (g_list_length (list) == count, "correct length: all");
    g_list_free (list);

    list = gncBusinessGetList (book, GNC_EMPLOYEE_MODULE_NAME, FALSE);
    do_test (list != NULL, "getList active");
    do_test (g_list_length (list) == 1, "correct length: active");
    g_list_free (list);
  }
#endif
  {
    const char *str = get_random_string();
    const char *res;
    GncAddress *addr;

    addr = gncEmployeeGetAddr (employee);
    gncAddressSetName (addr, str);
    res = qof_object_printable (GNC_ID_EMPLOYEE, employee);
    do_test (res != NULL, "Printable NULL?");
    do_test (safe_strcmp (str, res) == 0, "Printable equals");
  }    
}

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncEmployee *, const char *str),
		 const char * (*get)(const GncEmployee *))
{
  GncEmployee *employee = gncEmployeeCreate (book);
  char const *str = get_random_string ();

  do_test (!gncEmployeeIsDirty (employee), "test if start dirty");
  gncEmployeeBeginEdit (employee);
  set (employee, str);
  do_test (gncEmployeeIsDirty (employee), "test dirty later");
  gncEmployeeCommitEdit (employee);
  do_test (gncEmployeeIsDirty (employee), "test dirty after commit");
  do_test (safe_strcmp (get (employee), str) == 0, message);
  gncEmployeeSetActive (employee, FALSE);
  count++;
}

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncEmployee *, gnc_numeric),
		  gnc_numeric (*get)(const GncEmployee *))
{
  GncEmployee *employee = gncEmployeeCreate (book);
  gnc_numeric num = gnc_numeric_create (17, 1);

  do_test (!gncEmployeeIsDirty (employee), "test if start dirty");
  gncEmployeeBeginEdit (employee);
  set (employee, num);
  do_test (gncEmployeeIsDirty (employee), "test dirty later");
  gncEmployeeCommitEdit (employee);
  do_test (gncEmployeeIsDirty (employee), "test dirty after commit");
  do_test (gnc_numeric_equal (get (employee), num), message);
  gncEmployeeSetActive (employee, FALSE);
  count++;
}

static void
test_bool_fcn (QofBook *book, const char *message,
	       void (*set) (GncEmployee *, gboolean),
	       gboolean (*get) (const GncEmployee *))
{
  GncEmployee *employee = gncEmployeeCreate (book);
  gboolean num = get_random_boolean ();

  do_test (!gncEmployeeIsDirty (employee), "test if start dirty");
  gncEmployeeBeginEdit (employee);
  set (employee, FALSE);
  set (employee, TRUE);
  set (employee, num);
  do_test (gncEmployeeIsDirty (employee), "test dirty later");
  gncEmployeeCommitEdit (employee);
  do_test (gncEmployeeIsDirty (employee), "test dirty after commit");
  do_test (get (employee) == num, message);
  gncEmployeeSetActive (employee, FALSE);
  count++;
}

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncEmployee *, gint),
	       gint (*get) (GncEmployee *))
{
  GncEmployee *employee = gncEmployeeCreate (book);
  gint num = 17;

  do_test (!gncEmployeeIsDirty (employee), "test if start dirty");
  gncEmployeeBeginEdit (employee);
  set (employee, num);
  do_test (gncEmployeeIsDirty (employee), "test dirty later");
  gncEmployeeCommitEdit (employee);
  do_test (!gncEmployeeIsDirty (employee), "test dirty after commit");
  do_test (get (employee) == num, message);
  gncEmployeeSetActive (employee, FALSE);
  count++;
}
#endif

int
main (int argc, char **argv)
{ 
  qof_init();
  do_test (gncInvoiceRegister(), "Cannot register GncInvoice");
  do_test (gncJobRegister (),  "Cannot register GncJob");
  do_test (gncCustomerRegister(), "Cannot register GncCustomer");
  do_test (gncEmployeeRegister(), "Cannot register GncEmployee");
  test_employee();
  print_test_results();
  qof_close();
  return 0;
}

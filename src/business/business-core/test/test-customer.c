#include <glib.h>
#include <libguile.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"
#include "gncObject.h"

#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncCustomer *, const char *str),
		 const char * (*get)(GncCustomer *));

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncCustomer *, gnc_numeric),
		  gnc_numeric (*get)(GncCustomer *));

static void
test_bool_fcn (QofBook *book, const char *message,
		  void (*set) (GncCustomer *, gboolean),
		  gboolean (*get) (GncCustomer *));

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncCustomer *, gint),
	       gint (*get) (GncCustomer *));
#endif

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
    do_test (gncCustomerGetBook (customer) == book,
	     "getbook");

    gncCustomerBeginEdit (customer);
    gncCustomerDestroy (customer);
    success ("create/destroy");
  }

  /* Test setting/getting routines; does the active flag get set right? */
  {
    GUID guid;

    test_string_fcn (book, "Id", gncCustomerSetID, gncCustomerGetID);
    test_string_fcn (book, "Name", gncCustomerSetName, gncCustomerGetName);
    test_string_fcn (book, "Notes", gncCustomerSetNotes, gncCustomerGetNotes);

    //test_string_fcn (book, "Terms", gncCustomerSetTerms, gncCustomerGetTerms);

    test_numeric_fcn (book, "Discount", gncCustomerSetDiscount, gncCustomerGetDiscount);
    test_numeric_fcn (book, "Credit", gncCustomerSetCredit, gncCustomerGetCredit);

    //test_bool_fcn (book, "TaxIncluded", gncCustomerSetTaxIncluded, gncCustomerGetTaxIncluded);
    test_bool_fcn (book, "Active", gncCustomerSetActive, gncCustomerGetActive);

    do_test (gncCustomerGetAddr (customer) != NULL, "Addr");
    do_test (gncCustomerGetShipAddr (customer) != NULL, "ShipAddr");

    guid_new (&guid);
    customer = gncCustomerCreate (book); count++;
    gncCustomerSetGUID (customer, &guid);
    do_test (guid_equal (&guid, gncCustomerGetGUID (customer)), "guid compare");
  }
#if 0
  {
    GList *list;

    list = gncBusinessGetList (book, GNC_CUSTOMER_MODULE_NAME, TRUE);
    do_test (list != NULL, "getList all");
    do_test (g_list_length (list) == count, "correct length: all");
    g_list_free (list);

    list = gncBusinessGetList (book, GNC_CUSTOMER_MODULE_NAME, FALSE);
    do_test (list != NULL, "getList active");
    do_test (g_list_length (list) == 1, "correct length: active");
    g_list_free (list);
  }
#endif
  {
    const char *str = get_random_string();
    const char *res;

    gncCustomerSetName (customer, str);
    res = gncObjectPrintable (GNC_CUSTOMER_MODULE_NAME, customer);
    do_test (res != NULL, "Printable NULL?");
    do_test (safe_strcmp (str, res) == 0, "Printable equals");
  }    

  do_test (gncCustomerGetJoblist (customer, TRUE) == NULL, "joblist empty");

  /* Test the Entity Table */
  {
    const GUID *guid;

    guid = gncCustomerGetGUID (customer);
    do_test (gncCustomerLookup (book, guid) == customer, "Entity Table");
  }

  /* Note: JobList is tested from the Job tests */
}

static void
test_string_fcn (QofBook *book, const char *message,
		 void (*set) (GncCustomer *, const char *str),
		 const char * (*get)(GncCustomer *))
{
  GncCustomer *customer = gncCustomerCreate (book);
  char const *str = get_random_string ();

  do_test (!gncCustomerIsDirty (customer), "test if start dirty");
  gncCustomerBeginEdit (customer);
  set (customer, str);
  do_test (gncCustomerIsDirty (customer), "test dirty later");
  gncCustomerCommitEdit (customer);
  do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
  do_test (safe_strcmp (get (customer), str) == 0, message);
  gncCustomerSetActive (customer, FALSE);
  count++;
}

static void
test_numeric_fcn (QofBook *book, const char *message,
		  void (*set) (GncCustomer *, gnc_numeric),
		  gnc_numeric (*get)(GncCustomer *))
{
  GncCustomer *customer = gncCustomerCreate (book);
  gnc_numeric num = gnc_numeric_create (17, 1);

  do_test (!gncCustomerIsDirty (customer), "test if start dirty");
  gncCustomerBeginEdit (customer);
  set (customer, num);
  do_test (gncCustomerIsDirty (customer), "test dirty later");
  gncCustomerCommitEdit (customer);
  do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
  do_test (gnc_numeric_equal (get (customer), num), message);
  gncCustomerSetActive (customer, FALSE);
  count++;
}

static void
test_bool_fcn (QofBook *book, const char *message,
	       void (*set) (GncCustomer *, gboolean),
	       gboolean (*get) (GncCustomer *))
{
  GncCustomer *customer = gncCustomerCreate (book);
  gboolean num = get_random_boolean ();

  do_test (!gncCustomerIsDirty (customer), "test if start dirty");
  gncCustomerBeginEdit (customer);
  set (customer, FALSE);
  set (customer, TRUE);
  set (customer, num);
  do_test (gncCustomerIsDirty (customer), "test dirty later");
  gncCustomerCommitEdit (customer);
  do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
  do_test (get (customer) == num, message);
  gncCustomerSetActive (customer, FALSE);
  count++;
}

#if 0
static void
test_gint_fcn (QofBook *book, const char *message,
	       void (*set) (GncCustomer *, gint),
	       gint (*get) (GncCustomer *))
{
  GncCustomer *customer = gncCustomerCreate (book);
  gint num = 17;

  do_test (!gncCustomerIsDirty (customer), "test if start dirty");
  gncCustomerBeginEdit (customer);
  set (customer, num);
  do_test (gncCustomerIsDirty (customer), "test dirty later");
  gncCustomerCommitEdit (customer);
  do_test (!gncCustomerIsDirty (customer), "test dirty after commit");
  do_test (get (customer) == num, message);
  gncCustomerSetActive (customer, FALSE);
  count++;
}
#endif

static void
main_helper (void *closure, int argc, char **argv)
{
  gnc_module_load("gnucash/business-core", 0);
  test_customer();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, main_helper, NULL);
  return 0;
}

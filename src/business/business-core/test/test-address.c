#include <glib.h>
#include <guile/gh.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"

#include "gncBusiness.h"
#include "gncAddress.h"
#include "test-stuff.h"

static void
test_string_fcn (GncAddress *address, const char *message,
		 void (*set) (GncAddress *, const char *str),
		 const char * (*get)(const GncAddress *));

static void
test_address (void)
{
  GncAddress *address;
  GNCBook *book = gnc_book_new ();
  gncBusinessCreateBook (book);

  /* Test creation/destruction */
  {
    do_test (gncAddressCreate (NULL) == NULL, "address create NULL");
    address = gncAddressCreate (book);
    do_test (address != NULL, "address create");

    gncAddressDestroy (address);
    success ("create/destroy");
  }

  /* Test setting routines */
  {
    address = gncAddressCreate (book);
    test_string_fcn (address, "Name", gncAddressSetName, gncAddressGetName);
    test_string_fcn (address, "Addr1", gncAddressSetAddr1, gncAddressGetAddr1);
    test_string_fcn (address, "Addr2", gncAddressSetAddr2, gncAddressGetAddr2);
    test_string_fcn (address, "Addr3", gncAddressSetAddr3, gncAddressGetAddr3);
    test_string_fcn (address, "Addr4", gncAddressSetAddr4, gncAddressGetAddr4);
    test_string_fcn (address, "Phone", gncAddressSetPhone, gncAddressGetPhone);
    test_string_fcn (address, "Fax", gncAddressSetFax, gncAddressGetFax);
    test_string_fcn (address, "Email", gncAddressSetEmail, gncAddressGetEmail);
  }
}

static void
test_string_fcn (GncAddress *address, const char *message,
		 void (*set) (GncAddress *, const char *str),
		 const char * (*get)(const GncAddress *))
{
  char const *str = get_random_string ();

  do_test (!gncAddressIsDirty (address), "test if start dirty");
  set (address, str);
  do_test (gncAddressIsDirty (address), "test dirty later");
  do_test (safe_strcmp (get (address), str) == 0, message);
  gncAddressClearDirty (address);
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/business-core", 0);
  test_address();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

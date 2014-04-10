#include <glib.h>
#include <guile/gh.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"
#include "messages.h"

#include "gncBusiness.h"
#include "test-stuff.h"

#define TEST_MODULE_NAME "business-test"
#define TEST_MODULE_DESC "Test Business"

#if 0
static GList * get_list (GNCBook *, gboolean show_all);
static const char * printable (gpointer obj);
static void test_printable (const char *name, gpointer obj);
static void test_get_list (GNCBook *, const char *);

static GncBusinessObject bus_obj = {
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

  test_get_list ((GNCBook*)1, TEST_MODULE_NAME);
  test_printable (TEST_MODULE_NAME, (gpointer)1);
}

static GList *
get_list (GNCBook *book, gboolean show_all)
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
test_get_list (GNCBook *book, const char *name)
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
main_helper (int argc, char **argv)
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
  //  gh_enter (argc, argv, main_helper);
  return 0;
}

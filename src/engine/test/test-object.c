#include <glib.h>
#include <guile/gh.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"
#include "messages.h"

#include "gncObject.h"
#include "test-stuff.h"

#define TEST_MODULE_NAME "object-test"
#define TEST_MODULE_DESC "Test Object"

static void foreach (GNCBook *, foreachObjectCB, gpointer);
static const char * printable (gpointer obj);
static void test_printable (const char *name, gpointer obj);
static void test_foreach (GNCBook *, const char *);

static GncObject_t bus_obj = {
  GNC_OBJECT_VERSION,
  TEST_MODULE_NAME,
  TEST_MODULE_DESC,
  NULL,				/* create */
  NULL,				/* destroy */
  foreach,
  printable,
};

static void test_object (void)
{
  /* Test the global registration and lookup functions */
  {
    do_test (!gncObjectRegister (NULL), "register NULL");
    do_test (gncObjectRegister (&bus_obj), "register test object");
    do_test (!gncObjectRegister (&bus_obj), "register test object again");
    do_test (gncObjectLookup (TEST_MODULE_NAME) == &bus_obj,
	     "lookup our installed object");
    do_test (gncObjectLookup ("snm98sn snml say  dyikh9y9ha") == NULL,
	     "lookup non-existant object object");

    do_test (!safe_strcmp (gncObjectGetTypeLabel (TEST_MODULE_NAME),
		      _(TEST_MODULE_DESC)),
	     "test description return");
  }

  test_foreach ((GNCBook*)1, TEST_MODULE_NAME);
  test_printable (TEST_MODULE_NAME, (gpointer)1);
}

static void
foreach (GNCBook *book, foreachObjectCB cb, gpointer u_d)
{
  int *foo = u_d;

  do_test (book != NULL, "foreach: NULL object");
  success ("called foreach callback");

  *foo = 1;
}

static void foreachCB (gpointer obj, gpointer u_d)
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
test_foreach (GNCBook *book, const char *name)
{
  int res = 0;

  gncObjectForeach (NULL, NULL, NULL, &res);
  do_test (res == 0, "object: Foreach: NULL, NULL, NULL");
  gncObjectForeach (NULL, NULL, foreachCB, &res);
  do_test (res == 0, "object: Foreach: NULL, NULL, foreachCB");

  gncObjectForeach (NULL, book, NULL, &res);
  do_test (res == 0, "object: Foreach: NULL, book, NULL");
  gncObjectForeach (NULL, book, foreachCB, &res);
  do_test (res == 0, "object: Foreach: NULL, book, foreachCB");

  gncObjectForeach (name, NULL, NULL, &res);
  do_test (res == 0, "object: Foreach: name, NULL, NULL");
  gncObjectForeach (name, NULL, foreachCB, &res);
  do_test (res == 0, "object: Foreach: name, NULL, foreachCB");

  gncObjectForeach (name, book, NULL, &res);
  do_test (res != 0, "object: Foreach: name, book, NULL");

  res = 0;
  gncObjectForeach (name, book, foreachCB, &res);
  do_test (res != 0, "object: Foreach: name, book, foreachCB");
}

static void
test_printable (const char *name, gpointer obj)
{
  const char *res;

  do_test (gncObjectPrintable (NULL, NULL) == NULL,
	   "object: Printable: NULL, NULL");
  do_test (gncObjectPrintable (NULL, obj) == NULL,
	   "object: Printable: NULL, object");
  do_test (gncObjectPrintable (name, NULL) == NULL,
	   "object: Printable: mod_name, NULL");
  res = gncObjectPrintable (name, obj);
  do_test (res != NULL, "object: Printable: mod_name, object");
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);
  test_object();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

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

static GList * get_list (GncBusiness *, gboolean show_all);
static const char * printable (gpointer obj);
static void test_printable (GncBusiness *bus, const char *name, gpointer obj);

static GncBusinessObject bus_obj = {
  GNC_BUSINESS_VERSION,
  TEST_MODULE_NAME,
  TEST_MODULE_DESC,
  NULL,				/* destroy */
  get_list,
  printable,
};

static void test_business (void)
{
  GncBusiness *bus;

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

  /* Test creation and session handling */
  {
    bus = gncBusinessCreate (NULL);
    do_test (bus == NULL, "business create NULL");
    bus = gncBusinessCreate ((GNCSession *)1);
    do_test (bus != NULL, "business create non-NULL");

    do_test (gncBusinessGetSession (NULL) == NULL,
	     "business get session NULL");
    do_test (gncBusinessGetSession (bus) == (GNCSession *)1,
	     "business get session");

    gncBusinessDestroy (bus);
    success ("business create and destroy");
  }

  /* Test the entity tables */
  {
    GUID guid;
    gpointer ptr, res;
    GHashTable *ht;

    bus = gncBusinessCreate ((GNCSession *)1);
    do_test (gncBusinessEntityTable (NULL, NULL) == NULL,
	     "business entity table NULL NULL");
    do_test (gncBusinessEntityTable (NULL, TEST_MODULE_NAME) == NULL,
	     "business entity table NULL module_name");
    do_test (gncBusinessEntityTable (bus, NULL) == NULL,
	     "business entity table bus NULL");
    ht = gncBusinessEntityTable (bus, TEST_MODULE_NAME);
    do_test (ht != NULL, "business entity table bus, module_name");

    guid_new (&guid);

    /* Check the lookup functions before we add anything to the table */
    do_test (gncBusinessLookupGUID (NULL, NULL, NULL) == NULL,
	     "business: lookupGUID: NULL, NULL, NULL");
    do_test (gncBusinessLookupGUID (NULL, NULL, &guid) == NULL,
	     "business: lookupGUID: NULL, NULL, guid");
    do_test (gncBusinessLookupGUID (NULL, TEST_MODULE_NAME, NULL) == NULL,
	     "business: lookupGUID: NULL, mod_name, NULL");
    do_test (gncBusinessLookupGUID (NULL, TEST_MODULE_NAME, &guid) == NULL,
	     "business: lookupGUID: NULL, mod_name, guid");
    do_test (gncBusinessLookupGUID (bus, NULL, NULL) == NULL,
	     "business: lookupGUID: bus, NULL, NULL");
    do_test (gncBusinessLookupGUID (bus, NULL, &guid) == NULL,
	     "business: lookupGUID: bus, NULL, guid");
    do_test (gncBusinessLookupGUID (bus, TEST_MODULE_NAME, NULL) == NULL,
	     "business: lookupGUID: bus, mod_name, NULL");
    do_test (gncBusinessLookupGUID (bus, TEST_MODULE_NAME, &guid) == NULL,
	     "business: lookupGUID: bus, mod_name, guid");

    /* Now add to the table and look up again */
    ptr = get_random_string ();
    g_hash_table_insert (ht, &guid, ptr);

    /* now let's see if we can look for it */
    do_test (gncBusinessLookupGUID (NULL, NULL, NULL) == NULL,
	     "business: lookupGUID(2): NULL, NULL, NULL");
    do_test (gncBusinessLookupGUID (NULL, NULL, &guid) == NULL,
	     "business: lookupGUID(2): NULL, NULL, guid");
    do_test (gncBusinessLookupGUID (NULL, TEST_MODULE_NAME, NULL) == NULL,
	     "business: lookupGUID(2): NULL, mod_name, NULL");
    do_test (gncBusinessLookupGUID (NULL, TEST_MODULE_NAME, &guid) == NULL,
	     "business: lookupGUID(2): NULL, mod_name, guid");
    do_test (gncBusinessLookupGUID (bus, NULL, NULL) == NULL,
	     "business: lookupGUID(2): bus, NULL, NULL");
    do_test (gncBusinessLookupGUID (bus, NULL, &guid) == NULL,
	     "business: lookupGUID(2): bus, NULL, guid");
    do_test (gncBusinessLookupGUID (bus, TEST_MODULE_NAME, NULL) == NULL,
	     "business: lookupGUID(2): bus, mod_name, NULL");
    res = gncBusinessLookupGUID (bus, TEST_MODULE_NAME, &guid);
    do_test (res != NULL, "business: lookupGUID(2): bus, mod_name, guid");
    do_test (res == ptr, "business: lookupGUID(2): check the same pointer");

    test_printable (bus, TEST_MODULE_NAME, ptr);
  }
}

static GList *
get_list (GncBusiness *bus, gboolean show_all)
{
  do_test (bus != NULL, "get_list: NULL business");
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
test_get_list (GncBusiness *bus, const char *name)
{
  GList *res;

  do_test (gncBusinessGetList (NULL, NULL, FALSE) == NULL,
	   "business: GetList: NULL, NULL, FALSE");
  do_test (gncBusinessGetList (NULL, name, FALSE) == NULL,
	   "business: GetList: NULL, mod_name, FALSE");
  do_test (gncBusinessGetList (bus, NULL, FALSE) == NULL,
	   "business: GetList: bus, NULL, FALSE");
  res = gncBusinessGetList (bus, name, FALSE);
  do_test (res != NULL, "business: GetList: bus, mod_name, FALSE");
}

static void
test_printable (GncBusiness *bus, const char *name, gpointer obj)
{
  const char *res;

  do_test (gncBusinessPrintable (NULL, NULL, NULL) == NULL,
	   "business: Printable: NULL, NULL, NULL");
  do_test (gncBusinessPrintable (NULL, NULL, obj) == NULL,
	   "business: Printable: NULL, NULL, object");
  do_test (gncBusinessPrintable (NULL, name, NULL) == NULL,
	   "business: Printable: NULL, mod_name, NULL");
  do_test (gncBusinessPrintable (NULL, name, obj) == NULL,
	   "business: Printable: NULL, mod_name, object");
  do_test (gncBusinessPrintable (bus, NULL, NULL) == NULL,
	   "business: Printable: bus, NULL, NULL");
  do_test (gncBusinessPrintable (bus, NULL, obj) == NULL,
	   "business: Printable: bus, NULL, object");
  do_test (gncBusinessPrintable (bus, name, NULL) == NULL,
	   "business: Printable: bus, mod_name, NULL");

  res = gncBusinessPrintable (bus, name, obj);
  do_test (res != NULL, "business: Printable: bus, mod_name, object");
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/business-core", 0);
  test_business();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

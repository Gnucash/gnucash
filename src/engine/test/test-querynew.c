#include <glib.h>
#include <guile/gh.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"
#include "messages.h"

#include "QueryObjectP.h"
#include "QueryCoreP.h"
#include "QueryNew.h"

#include "test-stuff.h"

#define TEST_MODULE_NAME	"TestModuleName"
#define TEST_CORE		"TestCoreType"
#define TEST_PARAM		"test-param"
#define BAD_PARAM		"bad-param"

static int test_sort (gpointer a, gpointer b)
{
  return 0;
}

static int test_core_param (gpointer a)
{
  return 0;
}

static void test_query_object (void)
{
  static QueryObjectDef params[] = {
    { TEST_PARAM, TEST_CORE, (QueryAccess)test_core_param },
    { NULL },
  };

  gncQueryObjectRegister (TEST_MODULE_NAME, (QuerySort)test_sort, params);

  do_test (gncQueryObjectGetParameter (TEST_MODULE_NAME, TEST_PARAM)
	   == &params[0], "gncQueryObjectGetParameter");
  do_test (gncQueryObjectGetParameter (NULL, NULL) == NULL,
	   "gncQueryObjectGetParamter (NULL, NULL)");
  do_test (gncQueryObjectGetParameter (TEST_MODULE_NAME, NULL) == NULL,
	   "gncQueryObjectGetParamter (TEST_MODULE_NAME, NULL)");
  do_test (gncQueryObjectGetParameter (TEST_MODULE_NAME, BAD_PARAM) == NULL,
	   "gncQueryObjectGetParamter (TEST_MODULE_NAME, BAD_PARAM)");
  do_test (gncQueryObjectGetParameter (NULL, TEST_PARAM) == NULL,
	   "gncQueryObjectGetParamter (NULL, TEST_PARAM)");

  do_test (gncQueryObjectGetParameterGetter (TEST_MODULE_NAME, TEST_PARAM)
	   == (QueryAccess)test_core_param,
	   "gncQueryObjectGetParameterGetter");

  do_test (safe_strcmp (gncQueryObjectParameterType (TEST_MODULE_NAME,
						     TEST_PARAM),
			TEST_CORE) == 0, "gncQueryObjectParameterType");

  do_test (gncQueryObjectDefaultSort (TEST_MODULE_NAME) == test_sort,
	   "gncQueryObjectDefaultSort");
  do_test (gncQueryObjectDefaultSort (NULL) == NULL,
	   "gncQueryObjectDefaultSort (NULL)");
}

static void test_query_core (void)
{

}

static void test_querynew (void)
{
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);
  test_query_core();
  test_query_object();
  test_querynew();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

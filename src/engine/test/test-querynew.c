#include <glib.h>
#include <libguile.h>
#include <stdio.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"
#include "messages.h"

#include "qofclass-p.h"
#include "qofquery.h"
#include "qofquerycore.h"
#include "qofquerycore-p.h"

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

static void test_class (void)
{
  static QofParam params[] = {
    { TEST_PARAM, TEST_CORE, (QofAccessFunc)test_core_param, NULL },
    { NULL },
  };

  fprintf (stderr, "\tTesting the qof_query_object interface. \n"
	   "\tYou may see some \"** CRITICAL **\" messages, which you can safely ignore\n");

  qof_class_register (TEST_MODULE_NAME, (QofSortFunc)test_sort, params);

  do_test (qof_class_get_parameter (TEST_MODULE_NAME, TEST_PARAM)
	   == &params[0], "qof_class_get_parameter");
  do_test (qof_class_get_parameter (NULL, NULL) == NULL,
	   "qof_class_get_parameter (NULL, NULL)");
  do_test (qof_class_get_parameter (TEST_MODULE_NAME, NULL) == NULL,
	   "qof_class_get_parameter (TEST_MODULE_NAME, NULL)");
  do_test (qof_class_get_parameter (TEST_MODULE_NAME, BAD_PARAM) == NULL,
	   "qof_class_get_parameter (TEST_MODULE_NAME, BAD_PARAM)");
  do_test (qof_class_get_parameter (NULL, TEST_PARAM) == NULL,
	   "qof_class_get_parameter (NULL, TEST_PARAM)");

  do_test (qof_class_get_parameter_getter (TEST_MODULE_NAME, TEST_PARAM)
	   == (QofAccessFunc)test_core_param,
	   "qof_class_get_parameter_getter");

  do_test (safe_strcmp (qof_class_get_parameter_type (TEST_MODULE_NAME,
						     TEST_PARAM),
			TEST_CORE) == 0, "qof_class_get_parameter_type");

  do_test (qof_class_get_default_sort (TEST_MODULE_NAME) == test_sort,
	   "qof_class_get_default_sort");
  do_test (qof_class_get_default_sort (NULL) == NULL,
	   "qof_class_get_default_sort (NULL)");
}

static void test_query_core (void)
{

}

static void test_querynew (void)
{
}

static void
main_helper (void *closure, int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);
  test_query_core();
  test_class();
  test_querynew();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, main_helper, NULL);
  return 0;
}

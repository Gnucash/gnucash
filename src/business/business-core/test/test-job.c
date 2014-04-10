#include <glib.h>
#include <guile/gh.h>

#include "guid.h"
#include "gnc-module.h"
#include "gnc-engine-util.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncJobP.h"
#include "test-stuff.h"

static int count = 0;

static void
test_string_fcn (GncBusiness *bus, const char *message,
		 void (*set) (GncJob *, const char *str),
		 const char * (*get)(GncJob *));

static void
test_numeric_fcn (GncBusiness *bus, const char *message,
		  void (*set) (GncJob *, gnc_numeric),
		  gnc_numeric (*get)(GncJob *));

static void
test_bool_fcn (GncBusiness *bus, const char *message,
		  void (*set) (GncJob *, gboolean),
		  gboolean (*get) (GncJob *));

static void
test_gint_fcn (GncBusiness *bus, const char *message,
	       void (*set) (GncJob *, gint),
	       gint (*get) (GncJob *));

static void
test_job (void)
{
  GncBusiness *bus;
  GncJob *job;

  bus = gncBusinessCreate ((GNCBook *)1);

  /* Test creation/destruction */
  {
    do_test (gncJobCreate (NULL) == NULL, "job create NULL");
    job = gncJobCreate (bus);
    do_test (job != NULL, "job create");
    do_test (gncJobGetBusiness (job) == bus,
	     "getbusiness");

    gncJobDestroy (job);
    success ("create/destroy");
  }

  /* Test setting/getting routines; does the active flag get set right? */
  {
    GUID guid;

    test_string_fcn (bus, "Id", gncJobSetID, gncJobGetID);
    test_string_fcn (bus, "Name", gncJobSetName, gncJobGetName);
    test_string_fcn (bus, "Desc", gncJobSetDesc, gncJobGetDesc);

    test_bool_fcn (bus, "Active", gncJobSetActive, gncJobGetActive);

    guid_new (&guid);
    job = gncJobCreate (bus); count++;
    gncJobSetGUID (job, &guid);
    do_test (guid_equal (&guid, gncJobGetGUID (job)), "guid compare");
  }
#if 0
  {
    GList *list;

    list = gncBusinessGetList (bus, GNC_JOB_MODULE_NAME, TRUE);
    do_test (list != NULL, "getList all");
    do_test (g_list_length (list) == count, "correct length: all");
    g_list_free (list);

    list = gncBusinessGetList (bus, GNC_JOB_MODULE_NAME, FALSE);
    do_test (list != NULL, "getList active");
    do_test (g_list_length (list) == 1, "correct length: active");
    g_list_free (list);
  }
#endif
  {
    const char *str = get_random_string();
    const char *res;

    gncJobSetName (job, str);
    res = gncBusinessPrintable (bus, GNC_JOB_MODULE_NAME, job);
    do_test (res != NULL, "Printable NULL?");
    do_test (safe_strcmp (str, res) == 0, "Printable equals");
  }    
  {
    GList *list;
    GncCustomer *cust = gncCustomerCreate (bus);

    do_test (gncCustomerGetJoblist (cust, TRUE) == NULL, "empty list at start");
    gncJobSetCustomer (job, cust);
    list = gncCustomerGetJoblist (cust, FALSE);
    do_test (list != NULL, "added to cust");
    do_test (g_list_length (list) == 1, "correct joblist length");
    do_test (list->data == job, "verify job in list");
    gncJobSetActive (job, FALSE);
    list = gncCustomerGetJoblist (cust, FALSE);
    do_test (list == NULL, "no active jobs");
    list = gncCustomerGetJoblist (cust, TRUE);
    do_test (list != NULL, "all jobs");
    gncJobDestroy (job);
    list = gncCustomerGetJoblist (cust, TRUE);
    do_test (list == NULL, "no more jobs");
  }
}

static void
test_string_fcn (GncBusiness *bus, const char *message,
		 void (*set) (GncJob *, const char *str),
		 const char * (*get)(GncJob *))
{
  GncJob *job = gncJobCreate (bus);
  char const *str = get_random_string ();

  do_test (!gncJobIsDirty (job), "test if start dirty");
  set (job, str);
  do_test (gncJobIsDirty (job), "test dirty later");
  do_test (safe_strcmp (get (job), str) == 0, message);
  gncJobSetActive (job, FALSE); count++;
}

static void
test_numeric_fcn (GncBusiness *bus, const char *message,
		  void (*set) (GncJob *, gnc_numeric),
		  gnc_numeric (*get)(GncJob *))
{
  GncJob *job = gncJobCreate (bus);
  gnc_numeric num = gnc_numeric_create (17, 1);

  do_test (!gncJobIsDirty (job), "test if start dirty");
  set (job, num);
  do_test (gncJobIsDirty (job), "test dirty later");
  do_test (gnc_numeric_equal (get (job), num), message);
  gncJobSetActive (job, FALSE); count++;
}

static void
test_bool_fcn (GncBusiness *bus, const char *message,
	       void (*set) (GncJob *, gboolean),
	       gboolean (*get) (GncJob *))
{
  GncJob *job = gncJobCreate (bus);
  gboolean num = get_random_boolean ();

  do_test (!gncJobIsDirty (job), "test if start dirty");
  set (job, FALSE);
  set (job, TRUE);
  set (job, num);
  do_test (gncJobIsDirty (job), "test dirty later");
  do_test (get (job) == num, message);
  gncJobSetActive (job, FALSE); count++;
}

static void
test_gint_fcn (GncBusiness *bus, const char *message,
	       void (*set) (GncJob *, gint),
	       gint (*get) (GncJob *))
{
  GncJob *job = gncJobCreate (bus);
  gint num = 17;

  do_test (!gncJobIsDirty (job), "test if start dirty");
  set (job, num);
  do_test (gncJobIsDirty (job), "test dirty later");
  do_test (get (job) == num, message);
  gncJobSetActive (job, FALSE); count++;
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/business-core", 0);
  test_job();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}

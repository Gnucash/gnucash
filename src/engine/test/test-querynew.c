/***************************************************************************
 *            test-querynew.c
 *
 *  Tue Sep 27 19:18:57 2005
 *  Copyright  2005  GnuCash team
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include "config.h"
#include <glib.h>
#include <stdio.h>
#include <qof.h>
#include <test-stuff.h>
#include <unittest-support.h>
#include "../cashobjects.h"

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
    static QofParam params[] =
    {
        { TEST_PARAM, TEST_CORE, (QofAccessFunc)test_core_param, NULL },
        { NULL },
    };

    gchar *msg1 = "qof_class_get_parameter: assertion `obj_name' failed";
    gchar *msg2 = "qof_class_get_parameter: assertion `parameter' failed";
    gchar *logdomain = "qof";
    guint loglevel = G_LOG_LEVEL_CRITICAL;
    TestErrorStruct check1 = { loglevel, logdomain, msg1 };
    TestErrorStruct check2 = { loglevel, logdomain, msg2 };
    test_add_error (&check1);
    test_add_error (&check2);
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_list_handler, NULL);


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

    do_test (g_strcmp0 (qof_class_get_parameter_type (TEST_MODULE_NAME,
                        TEST_PARAM),
                        TEST_CORE) == 0, "qof_class_get_parameter_type");

    /*  do_test (qof_class_get_default_sort (TEST_MODULE_NAME) == test_sort,
    	   "qof_class_get_default_sort");
      do_test (qof_class_get_default_sort (NULL) == NULL,
    	   "qof_class_get_default_sort (NULL)");*/
    test_clear_error_list ();
}

static void test_query_core (void)
{

}

static void test_querynew (void)
{
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        test_query_core();
        test_class();
        test_querynew();
    }
    qof_close();
    print_test_results();
    return get_rv();
}

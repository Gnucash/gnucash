/********************************************************************
 * test_module.c: Example GLib g_test test execution file.	    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/


#include <glib.h>

/* Declare the test suite assembly functions (see test-suite.c) for
 * each sub-suite; avoids having header files. */
extern GTestSuite *test_suite_module1();
extern GTestSuite *test_suite_module2();
extern GTestSuite *test_suite_module3();
extern GTestSuite *test_suite_module4();

int
main (int   argc,
      char *argv[])
{
    g_type_init();     /* You may or may not need this, depending on
			* whether the module you're testing or any
			* dependencies use GObject. */
    g_test_init ( &argc, &argv ); /* initialize test program */
    qof_log_init_filename_special("/dev/null");    /* Initialize the
			* gnucash logging system. Your tests will
			* crash on the first logging call otherwise */
    test_suite_module1();           /* Call each suite assembly function */
    test_suite_module2();
    test_suite_module3();
    test_suite_module4();

    return g_test_run();           /* Run the result */
}



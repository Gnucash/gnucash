/********************************************************************
 * testmain.c: GLib g_test test execution file.			    *
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


/* This is a template test program. Copy it to the test sudirectory and rename it test_modulename.c. (Use the same modulename that you gave Makefile.am in the same directory.
Write and link other test files */
#include <glib/glib.h>

int
main (int   argc,
      char *argv[])
{
    gtk_test_init (&argc, &argv); // initialize test program
/* Add test functions and suites. See
 * http://library.gnome.org/devel/glib/stable/glib-Testing.html for
 * details. Unfortunately, GLib-Testing doesn't provide the automatic
 * registration features of more sophisitcated frameworks. */
    g_test_add_func ("/TESTPROG/Test Case Name", test_case_test_func);
	    ScannerFixture,            // fixture structure type
	    NULL,                      // unused data argument
	    scanner_fixture_setup,     // fixture setup
	    test_scanner_symbols,      // test function
	    scanner_fixture_teardown); // fixture teardown
    return g_test_run();
}



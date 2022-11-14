/********************************************************************
 * testmain.c: GLib g_test test execution file.	                    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>                   *
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


/**
 * This is a template test program. Copy it to the test sudirectory and rename
 * it test_modulename.c. (Use the same modulename that you gave Makefile.am in
 * the same directory.
 */
#include <config.h>
#include <glib.h>
/* Add any headers you need for the functions you're testing. */

/* Test fixture: A struct, a setup function, and a teardown function are passed
 * to g_test_add(); add getters, setters, and whatever other functions you need
 * to call from your test functions.
 */
typedef struct
{
    int a;
    char* b;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    /* Whatever is needed to populate the fixture and initialize your module for
     * running a single test.
     */
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    /* Whatever cleanup is needed at the end of the test. */
}

static void
example_without_fixture (void)
{
    /* This test doesn't need the fixture or any preconditions. You might use
     * this for a constructor test, for example.
     */
}

static void
example_with_data( gconstpointer data )
{
    /* We want to be able to call this function more than once with different
     * data.
     */
}

static void
example_with_fixture (Fixture *fixture, gconstpointer pData)
{
    /* This one uses the fixture. */
}

int
main (int argc, char *argv[])
{
    qof_init();     /* You may or may not need this, depending on
                     * whether the module you're testing or any
                     * dependencies use GObject. */
    qof_log_init_filename_special("/dev/null");    /* Initialize the
                           * gnucash logging system. Your tests will
                           * crash on the first logging call otherwise */
    g_test_init (&argc, &argv, NULL); // initialize test program
    /* Add test functions. See
     * http://library.gnome.org/devel/glib/stable/glib-Testing.html for
     * details. Unfortunately, GLib-Testing doesn't provide the automatic
     * registration features of more sophisticated frameworks. */
    g_test_add_func ("/TESTPROG/Test Case Name 1", example_without_fixture);
    g_test_add_data_func ("/TESTPROG/Test Case Name 2", NULL);
    g_test_add ("/TESTPROG/Test Case Name", Fixture, NULL, setup,
                example_with_fixture, teardown);
    return g_test_run();
}

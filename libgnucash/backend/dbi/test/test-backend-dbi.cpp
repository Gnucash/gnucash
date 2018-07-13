/********************************************************************
 * test-backend-dbi.c: GLib test execution file for backend/dbi     *
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

extern "C"
{
#include <config.h>
#include <glib.h>
#include "qof.h"
#include "cashobjects.h"
}
extern void test_suite_gnc_backend_dbi ();

#define GNC_LIB_NAME_1 "gncmod-backend-dbi"
#define GNC_LIB_REL_PATH_1 "dbi"
#define GNC_LIB_NAME_2 "gncmod-backend-xml"
#define GNC_LIB_REL_PATH_2 "xml"

int
main (int   argc,
      char* argv[])
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    qof_init (); /* equally initializes gobject system */
    qof_log_init_filename_special ("stderr"); /* Init the log system */
    g_test_init (&argc, &argv, NULL);       /* initialize test program */
    g_test_bug_base ("https://bugs.gnucash.org/show_bug.cgi?id="); /* init the bugzilla URL */
    cashobjects_register ();
    g_assert (qof_load_backend_library (GNC_LIB_REL_PATH_1, GNC_LIB_NAME_1));
    g_assert (qof_load_backend_library (GNC_LIB_REL_PATH_2, GNC_LIB_NAME_2));

    test_suite_gnc_backend_dbi ();

    return g_test_run ();
}

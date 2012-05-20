/********************************************************************
 * test-backend-dbi.c: GLib test execution file for backend/dbi     *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		            *
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


#include "config.h"
#include <glib.h>
#include "qof.h"
#include "cashobjects.h"

extern void test_suite_gnc_backend_dbi_basic();
extern void test_suite_gnc_backend_dbi_business();

#define GNC_LIB_NAME "gncmod-backend-dbi"

int
main (int   argc,
      char *argv[])
{
    g_type_init(); 			/* Initialize the GObject system */
    g_test_init ( &argc, &argv, NULL ); 	/* initialize test program */
    qof_log_init_filename_special("stderr"); /* Init the log system */
    g_test_bug_base("https://bugzilla.gnome.org/show_bug.cgi?id="); /* init the bugzilla URL */
    qof_init();
    cashobjects_register();
    qof_load_backend_library ("../.libs/", GNC_LIB_NAME);

    test_suite_gnc_backend_dbi_basic();
    test_suite_gnc_backend_dbi_business();

    return g_test_run( );
}

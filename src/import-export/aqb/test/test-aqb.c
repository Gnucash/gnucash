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
#include <glib.h>
#include "config.h"
#include <qof.h>
#include "backend/xml/gnc-backend-xml.h"
#include "gnc-module/gnc-module.h"
#include "engine/gnc-engine.h"
#include <engine/TransLog.h>

void test_qofsession_aqb_kvp( void );

int
main (int   argc,
      char *argv[])
{
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */
    g_test_init ( &argc, &argv, NULL ); 	/* initialize test program */
    qof_log_set_level("gnc", G_LOG_LEVEL_DEBUG);
    g_test_bug_base("https://bugzilla.gnome.org/show_bug.cgi?id="); /* init the bugzilla URL */
    /* Disable the transaction log */
    xaccLogDisable();

    gnc_module_system_init();
    gnc_engine_init_static(argc, argv);
    qof_load_backend_library ("../../../backend/xml/.libs/",
			      "gncmod-backend-xml");

    /* Add test functions and suites. See
     * http://library.gnome.org/devel/glib/stable/glib-Testing.html for
     * details. Unfortunately, GLib-Testing doesn't provide the automatic
     * registration features of more sophisticated frameworks. */
    g_test_add_func ("/src/import-export/aqb/kvp", test_qofsession_aqb_kvp);

    return g_test_run();
}



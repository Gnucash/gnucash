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


#include "config.h"
#include <glib.h>
#include "qof.h"

extern void test_suite_account();
extern void test_suite_gncInvoice();
//extern void test_suite_transaction();
extern void test_suite_split();

int
main (int   argc,
      char *argv[])
{
    g_type_init(); 			/* Initialize the GObject system */
    g_test_init ( &argc, &argv, NULL ); 	/* initialize test program */
    qof_log_init_filename_special("stderr"); /* Init the log system */
    //qof_log_set_level("gnc", G_LOG_LEVEL_DEBUG);
    g_test_bug_base("https://bugzilla.gnome.org/show_bug.cgi?id="); /* init the bugzilla URL */

    test_suite_account();
    test_suite_gncInvoice();
//    test_suite_transaction();
    test_suite_split();

    return g_test_run( );
}

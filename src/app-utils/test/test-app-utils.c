/********************************************************************
 * test-app-utils.c: GLib g_test test execution file.		    *
 * Copyright 2013 John Ralls <jralls@ceridwen.us>		    *
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

#include <config.h>
#include <glib.h>
#include <qof.h>
#include <libguile.h>
#include <gnc-module.h>

extern void test_suite_option_util (void);

static void
guile_main (void *closure, int argc, char **argv)
{
    GNCModule mod;
    int retval;
    gnc_module_system_init ();
    mod = gnc_module_load ("gnucash/app-utils", 0);

    test_suite_option_util ();
    retval = g_test_run ();

    exit (retval);
}

int
main (int argc, char *argv[])
{
    qof_init (); 			/* Initialize the GObject system */
    qof_log_init_filename_special ("stderr"); /* Init the log system */
    g_test_init (&argc, &argv, NULL); 	/* initialize test program */
    //qof_log_set_level("gnc", G_LOG_LEVEL_DEBUG);
    g_test_bug_base("https://bugzilla.gnome.org/show_bug.cgi?id="); /* init the bugzilla URL */
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    scm_boot_guile (argc, argv, guile_main, NULL);
    return 0;
}

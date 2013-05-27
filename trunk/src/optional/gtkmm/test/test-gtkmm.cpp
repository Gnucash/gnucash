/********************************************************************
 * testmain.c: GLib g_test test execution file.			    *
 * Copyright 2011 Christian Stimming
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
#include <glibmm.h>
extern "C" {
#include "qof.h"

//    gint libgncmod_gtkmm_gnc_module_init(gint refcount);
}
// c++ includes
//#include <gtkmm.h>
// And our own plugin
#include "gncmm/wrap_init.hpp"


extern void test_suite_gtkmm_book();

int
main (int   argc,
      char *argv[])
{
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */
    g_test_init ( &argc, &argv, NULL ); 	/* initialize test program */
    g_test_bug_base("https://bugzilla.gnome.org/show_bug.cgi?id="); /* init the bugzilla URL */

    // Initialize glibmm
    Glib::init();
    gnc::wrap_init();

    // The below only needed if we use gtkmm stuff
    //Gtk::Main::init_gtkmm_internals();

    //libgncmod_gtkmm_gnc_module_init(1);

    test_suite_gtkmm_book();

    return g_test_run( );
}



/********************************************************************
 * test_qofbackend.c: GLib g_test test suite for qofbackend.	    *
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
#include <string.h>
#include <glib.h>
#include <qof.h>
#include <unittest-support.h>

static const gchar *suitename = "/qof/gnc_numeric";

static void
test_gnc_numeric_add (void)
{
    gnc_numeric a = { 123456789987654321, 1000000000 };
    gnc_numeric b = { 65432198765432198, 100000000 };
    gnc_numeric goal_ab = { 777778777641976301, 1000000000 };
    gnc_numeric result;

    result = gnc_numeric_add (a, b, GNC_DENOM_AUTO,
			      GNC_HOW_DENOM_EXACT | GNC_HOW_RND_NEVER);
    g_assert (gnc_numeric_equal (result, goal_ab));
}

void
test_suite_gnc_numeric ( void )
{
    GNC_TEST_ADD_FUNC( suitename, "gnc-numeric add", test_gnc_numeric_add );
}

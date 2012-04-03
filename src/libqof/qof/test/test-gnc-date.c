
/********************************************************************
 * test-gnc-date.c: GLib g_test test suite for gnc-date.h functions *
 * Copyright 2011 Christian Stimming		    *
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
#include <unittest-support.h>
#include "qof.h"
#include "qofbook-p.h"
#include "qofbookslots.h"

static const gchar *suitename = "/qof/gnc-date";
void test_suite_gnc_date ( void );

typedef struct
{
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
}

static void
test_gnc_date_dmy2gdate( void )
{
    GDate *p_gdate;
    GDate gdate;
    p_gdate = g_date_new_dmy(1, 2, 2011);
    gdate = gnc_dmy2gdate(1, 2, 2011);
    g_assert(g_date_compare(&gdate, p_gdate) == 0);
    gdate = gnc_dmy2gdate(2, 2, 2011);
    g_assert(g_date_compare(&gdate, p_gdate) > 0);
    g_date_subtract_days(&gdate, 1);
    g_assert(g_date_compare(&gdate, p_gdate) == 0);
    g_date_free(p_gdate);
}

void
test_suite_gnc_date ( void )
{
    GNC_TEST_ADD_FUNC( suitename, "dmy2gdate", test_gnc_date_dmy2gdate);
}

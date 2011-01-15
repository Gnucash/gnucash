/********************************************************************
 * test_qofbook.c: GLib g_test test suite for qofbook.		    *
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
#include <config.h>
#include <string.h>
#include <glib.h>
#include <qof.h>

static const gchar *suitename = "/qof/qofbook";
void test_suite_qofbook ( void );

typedef struct
{
    QofBook *book;
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->book = qof_book_new();
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    qof_book_destroy( fixture->book );
}

static void
test_book_readonly( Fixture *fixture, gconstpointer pData )
{
    g_assert( fixture->book != NULL );
    g_assert( !qof_book_is_readonly( fixture->book ) );
    qof_book_mark_readonly( fixture->book );
    g_assert( qof_book_is_readonly( fixture->book ) );
}

void
test_suite_qofbook ( void )
{
    g_test_add( suitename, Fixture, NULL, setup, test_book_readonly, teardown );
}

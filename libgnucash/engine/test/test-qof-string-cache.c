
/********************************************************************
 * test-qof-string-cache.c: GLib g_test test suite for string cache *
 *                          functions                               *
 * Copyright 2011 Christian Stimming                                *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>                   *
 * Copyright 2012 Phil Longstaff <phil.longstaff@yahoo.ca>          *
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
#include <unittest-support.h>
#include "qof.h"

static const gchar *suitename = "/qof/qof-string-cache";
void test_suite_qof_string_cache ( void );

typedef struct
{
} Fixture;

G_GNUC_UNUSED static void
setup( Fixture *fixture, gconstpointer pData )
{
    return;
}

G_GNUC_UNUSED static void
teardown( Fixture *fixture, gconstpointer pData )
{
    return;
}

static void
test_qof_string_cache( void )
{
    /* Strings added to the cache should always return the same string address
     * as long as the refcount > 0. */
    gchar str[100];
    const gchar* str1_1;
    const gchar* str1_2;
    const gchar* str1_3;
    const gchar* str1_4;

    strncpy(str, "str1", sizeof(str));
    str1_1 = qof_string_cache_insert(str);      /* Refcount = 1 */
    g_assert(str1_1 != str);
    str1_2 = qof_string_cache_insert(str);      /* Refcount = 2 */
    g_assert(str1_1 == str1_2);
    qof_string_cache_remove(str);               /* Refcount = 1 */
    str1_3 = qof_string_cache_insert(str);      /* Refcount = 2 */
    g_assert(str1_1 == str1_3);
    qof_string_cache_remove(str);               /* Refcount = 1 */
    qof_string_cache_remove(str);               /* Refcount = 0 */
    strncpy(str, "str2", sizeof(str));
    qof_string_cache_insert(str);               /* Refcount = 1 */
    strncpy(str, "str1", sizeof(str));
    str1_4 = qof_string_cache_insert(str);      /* Refcount = 1 */
    g_assert(str1_1 != str1_4);
}

void
test_suite_qof_string_cache ( void )
{
    GNC_TEST_ADD_FUNC( suitename, "string-cache", test_qof_string_cache);
}

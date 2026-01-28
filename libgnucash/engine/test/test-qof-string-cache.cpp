/********************************************************************
 * test-qof-string-cache.c: google test test suite for string cache *
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
#include <gtest/gtest.h>
#include "../qof-string-cache.h"
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
#include "qof.h"

TEST(QODStringCache, qof_string_cache)
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
    EXPECT_NE(str1_1, str);
    str1_2 = qof_string_cache_insert(str);      /* Refcount = 2 */
    EXPECT_EQ(str1_1, str1_2);
    qof_string_cache_remove(str);               /* Refcount = 1 */
    str1_3 = qof_string_cache_insert(str);      /* Refcount = 2 */
    EXPECT_EQ(str1_1, str1_3);
    qof_string_cache_remove(str);               /* Refcount = 1 */
    qof_string_cache_remove(str);               /* Refcount = 0 */
    strncpy(str, "str2", sizeof(str));
    qof_string_cache_insert(str);               /* Refcount = 1 */
    strncpy(str, "str1", sizeof(str));
    str1_4 = qof_string_cache_insert(str);      /* Refcount = 1 */
    EXPECT_NE(str1_1, str1_4);

    // Edge cases.
    EXPECT_EQ(qof_string_cache_insert(nullptr), nullptr);
    EXPECT_STREQ(qof_string_cache_insert(""), "");

    // Replace
    gchar test[100];
    strncpy(str, "str1", sizeof(str));
    strncpy(test, "test", sizeof(test));
    EXPECT_EQ(str1_4, qof_string_cache_insert(str));        // Refcount = 2
    auto test_1 = qof_string_cache_replace(str, test);      // Refcount = 1
    EXPECT_EQ(str1_4, qof_string_cache_insert(str));        // Refcount = 2
    EXPECT_EQ(test_1, qof_string_cache_replace(str, test)); // Refcount = 1
    qof_string_cache_remove(str);                           // Refcount = 0
    EXPECT_NE(str1_4, qof_string_cache_insert(str));
}


int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);

    setlocale (LC_ALL, "");
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */

    auto ret = RUN_ALL_TESTS();
    qof_string_cache_destroy();
    return ret;
}

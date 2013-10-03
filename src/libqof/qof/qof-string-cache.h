/********************************************************************\
 * qof-string-cache.h -- QOF string cache functions                 *
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

/** @addtogroup Utilities
    @{ */
/** @file qof-string-cache.h
    @brief QOF String cache functions
    @author Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 1997-2002,2004 Linas Vepstas <linas@linas.org>
    @author Copyright 2006  Neil Williams  <linux@codehelp.co.uk>
    @author Copyright 2012  Phil Longstaff <phil.longstaff@yahoo.ca>
*/

#ifndef QOF_STRING_UTIL_H
#define QOF_STRING_UTIL_H

#define QOF_MOD_UTIL "qof.utilities"

/** The QOF String Cache:
 *
 * Many strings used throughout QOF and QOF applications are likely to
 * be duplicated.
 *
 * QOF provides a reference counted cache system for the strings, which
 * shares strings whenever possible.
 *
 * Use qof_string_cache_insert to insert a string into the cache (it
 * will return a pointer to the cached string).  Basically you should
 * use this instead of g_strdup.
 *
 * Use qof_string_cache_remove (giving it a pointer to a cached
 * string) if the string is unused.  If this is the last reference to
 * the string it will be removed from the cache, otherwise it will
 * just decrement the reference count.  Basically you should use this
 * instead of g_free.
 *
 * Just in case it's not clear: The remove function must NOT be called
 * for the string you passed INTO the insert function.  It must be
 * called for the _cached_ string that is _returned_ by the insert
 * function.
 *
 * Note that all the work is done when inserting or removing.  Once
 * cached the strings are just plain C strings.
 *
 * The string cache is demand-created on first use.
 *
 **/

/** Initialize the string cache */
void qof_string_cache_init(void);

/** Destroy the qof_string_cache */
void qof_string_cache_destroy(void);

/** You can use this function as a destroy notifier for a GHashTable
   that uses common strings as keys (or values, for that matter.)
*/
void qof_string_cache_remove(gconstpointer key);

/** You can use this function with g_hash_table_insert(), for the key
   (or value), as long as you use the destroy notifier above.
*/
gpointer qof_string_cache_insert(gconstpointer key);

#define CACHE_INSERT(str) qof_string_cache_insert((gconstpointer)(str))
#define CACHE_REMOVE(str) qof_string_cache_remove((str))

/* Replace cached string currently in 'dst' with string in 'src'.
 * Typical usage:
 *     void foo_set_name(Foo *f, const char *str) {
 *        CACHE_REPLACE(f->name, str);
 *     }
 * It avoids unnecessary ejection by doing INSERT before REMOVE.
*/
#define CACHE_REPLACE(dst, src) do {          \
        gpointer tmp = CACHE_INSERT((src));   \
        CACHE_REMOVE((dst));                  \
        (dst) = tmp;                          \
    } while (0)

#define QOF_CACHE_NEW(void) qof_string_cache_insert("")

#endif /* QOF_STRING_CACHE_H */
/** @} */

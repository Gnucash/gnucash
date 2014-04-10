/********************************************************************\
 * gnc-engine-util.h -- GnuCash engine utility functions            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @file gnc-engine-util.h 
    @brief GnuCash engine utility functions 
    @author Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 1997-2002 Linas Vepstas <linas@linas.org>
*/

#ifndef GNC_ENGINE_UTIL_H
#define GNC_ENGINE_UTIL_H

#include "config.h"

#include <glib.h>
#include <stddef.h>

#include "gnc-trace.h"

/** Macros *****************************************************/

#define SAFE_STRCMP_REAL(fcn,da,db) {	\
  if ((da) && (db)) {			\
    int retval = fcn ((da), (db));	\
    /* if strings differ, return */	\
    if (retval) return retval;		\
  } else 				\
  if ((!(da)) && (db)) {		\
    return -1;				\
  } else 				\
  if ((da) && (!(db))) {		\
    return +1;				\
  }					\
}

#define SAFE_STRCMP(da,db) SAFE_STRCMP_REAL(strcmp,(da),(db))
#define SAFE_STRCASECMP(da,db) SAFE_STRCMP_REAL(strcasecmp,(da),(db))

/* Define the long long int conversion for scanf */
#if HAVE_SCANF_LLD
# define GNC_SCANF_LLD "%lld"
#else
# define GNC_SCANF_LLD "%qd"
#endif


/** Prototypes *************************************************/

/* The safe_strcmp compares strings a and b the same way that strcmp()
 * does, except that either may be null.  This routine assumes that
 * a non-null string is always greater than a null string.
 */
int safe_strcmp (const char * da, const char * db);
int safe_strcasecmp (const char * da, const char * db);

/* The null_strcmp compares strings a and b the same way that strcmp()
 * does, except that either may be null.  This routine assumes that
 * a null string is equal to the empty string.
 */
int null_strcmp (const char * da, const char * db);

/* Search for str2 in first nchar chars of str1, ignore case. Return
 * pointer to first match, or null. These are just like that strnstr
 * and the strstr functions, except that they ignore the case. */
extern char *strncasestr(const char *str1, const char *str2, size_t len);
extern char *strcasestr(const char *str1, const char *str2);

/* The ultostr() subroutine is the inverse of strtoul(). It accepts a
 * number and prints it in the indicated base.  The returned string
 * should be g_freed when done.  */
char * ultostr (unsigned long val, int base);

/* Returns true if string s is a number, possibly surrounded by
 * whitespace. */
gboolean gnc_strisnum(const char *s);

/* Define a gnucash stpcpy */
char * gnc_stpcpy (char *dest, const char *src);

#ifndef HAVE_STPCPY
#define stpcpy gnc_stpcpy
#endif



/** Many strings used throughout the engine are likely to be duplicated.
 * So we provide a reference counted cache system for the strings, which
 * shares strings whenever possible.
 *
 * Use g_cache_insert to insert a string into the cache (it will return a
 * pointer to the cached string).
 * Basically you should use this instead of g_strdup.
 *
 * Use g_cache_remove (giving it a pointer to a cached string) if the string
 * is unused.  If this is the last reference to the string it will be
 * removed from the cache, otherwise it will just decrement the
 * reference count.
 * Basically you should use this instead of g_free.
 *
 * Note that all the work is done when inserting or removing.  Once
 * cached the strings are just plain C strings.
 */

/* get the gnc_string_cache.  Create it if it doesn't exist already */
GCache* gnc_engine_get_string_cache(void);

void gnc_engine_string_cache_destroy (void);

#endif
/** @} */

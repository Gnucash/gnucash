/********************************************************************\
 * gnc-engine-util.h -- GnuCash engine utility functions            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2002 Linas Vepstas <linas@linas.org>          *
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
 *                                                                  *
 *   Author: Rob Clark (rclark@cs.hmc.edu)                          *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

/** @file gnc-engine-util.h @brief GnuCash engine utility functions */

#ifndef GNC_ENGINE_UTIL_H
#define GNC_ENGINE_UTIL_H

#include "config.h"

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


#endif

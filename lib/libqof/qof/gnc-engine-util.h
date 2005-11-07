/********************************************************************\
 * gnc-engine-util.h -- QOF utility functions                       *
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

/** @addtogroup Utilities
    @{ */
/** @file gnc-engine-util.h
    @brief QOF utility functions
    @author Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 1997-2002,2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QOF_UTIL_H
#define QOF_UTIL_H

#include <glib.h>
#include <stddef.h>
#include "config.h"
#include "qof.h"

/** Macros *****************************************************/

/* CAS: Notice that this macro does nothing if pointer args are equal.
   Otherwise, it returns an integer.  Actually, perhaps these macro
   should be private.  They are NOT good substitutes for the function
   versions like safe_strcmp().  Maybe external users of these 3
   macros should be converted to use safe_strcmp().  Actually, THESE
   MACROS AFFECT CONTROL FLOW.  YUCK!  */
#define SAFE_STRCMP_REAL(fcn,da,db) {    \
  if ((da) && (db)) {                    \
    if ((da) != (db)) {                  \
      int retval = fcn ((da), (db));     \
      /* if strings differ, return */    \
      if (retval) return retval;         \
    }                                    \
  } else                                 \
  if ((!(da)) && (db)) {                 \
    return -1;                           \
  } else                                 \
  if ((da) && (!(db))) {                 \
    return +1;                           \
  }                                      \
}

#define SAFE_STRCMP(da,db) SAFE_STRCMP_REAL(strcmp,(da),(db))
#define SAFE_STRCASECMP(da,db) SAFE_STRCMP_REAL(strcasecmp,(da),(db))

/** \name typedef enum as string macros
@{
*/
#define ENUM_BODY(name, value)           \
    name value,

#define AS_STRING_CASE(name, value)      \
    case name: { return #name; }

#define FROM_STRING_CASE(name, value)    \
    if (strcmp(str, #name) == 0) {       \
        return name;  }

#define DEFINE_ENUM(name, list)          \
    typedef enum {                       \
        list(ENUM_BODY)                  \
    }name;

#define AS_STRING_DEC(name, list)        \
    const char* name##asString(name n);

#define AS_STRING_FUNC(name, list)       \
    const char* name##asString(name n) { \
        switch (n) {                     \
            list(AS_STRING_CASE)         \
            default: return "";  } }

#define FROM_STRING_DEC(name, list)      \
    name name##fromString                \
    (const char* str);

#define FROM_STRING_FUNC(name, list)     \
    name name##fromString                \
    (const char* str) {                  \
    if(str == NULL) { return 0; }        \
        list(FROM_STRING_CASE)           \
        return 0;  }

/** @} */

/** \name enum as string with no typedef
@{

  Similar but used when the enum is NOT a typedef
 note the LACK of a define_enum macro - don't use one!

 ENUM_BODY is used in both types.
 */

#define FROM_STRING_DEC_NON_TYPEDEF(name, list)   \
   void name##fromString                          \
   (const char* str, enum name *type);

#define FROM_STRING_CASE_NON_TYPEDEF(name, value) \
   if (strcmp(str, #name) == 0) { *type = name; }

#define FROM_STRING_FUNC_NON_TYPEDEF(name, list)  \
   void name##fromString                          \
   (const char* str, enum name *type) {           \
   if(str == NULL) { return; }                    \
    list(FROM_STRING_CASE_NON_TYPEDEF) }

#define AS_STRING_DEC_NON_TYPEDEF(name, list)     \
   const char* name##asString(enum name n);

#define AS_STRING_FUNC_NON_TYPEDEF(name, list)    \
   const char* name##asString(enum name n) {     \
       switch (n) {                              \
           list(AS_STRING_CASE_NON_TYPEDEF)      \
           default: return ""; } }

#define AS_STRING_CASE_NON_TYPEDEF(name, value)   \
   case name: { return #name; }

/** @} */

/* Define the long long int conversion for scanf */
#if HAVE_SCANF_LLD
# define GNC_SCANF_LLD "%lld"
#else
# define GNC_SCANF_LLD "%qd"
#endif

/** @name Convenience wrappers
   @{
*/
   
/** \brief Initialise the Query Object Framework 

Used for non-Guile applications or test routines.
*/
void qof_init (void);

/** \brief Safely close down the Query Object Framework 

Used for non-Guile applications or test routines.
*/
void qof_close (void);

/** @} */

/** Prototypes *************************************************/

/** The safe_strcmp compares strings a and b the same way that strcmp()
 * does, except that either may be null.  This routine assumes that
 * a non-null string is always greater than a null string.
 */
int safe_strcmp (const char * da, const char * db);
int safe_strcasecmp (const char * da, const char * db);

/** The null_strcmp compares strings a and b the same way that strcmp()
 * does, except that either may be null.  This routine assumes that
 * a null string is equal to the empty string.
 */
int null_strcmp (const char * da, const char * db);

/** Search for str2 in first nchar chars of str1, ignore case. Return
 * pointer to first match, or null. These are just like that strnstr
 * and the strstr functions, except that they ignore the case. */
extern char *strncasestr(const char *str1, const char *str2, size_t len);
extern char *strcasestr(const char *str1, const char *str2);

/** The ultostr() subroutine is the inverse of strtoul(). It accepts a
 * number and prints it in the indicated base.  The returned string
 * should be g_freed when done.  */
char * ultostr (unsigned long val, int base);

/** Returns true if string s is a number, possibly surrounded by
 * whitespace. */
gboolean gnc_strisnum(const char *s);

/** Local copy of stpcpy, used wtih libc's that don't have one. */
char * gnc_stpcpy (char *dest, const char *src);

#ifndef HAVE_STPCPY
#define stpcpy gnc_stpcpy
#endif

/** Return NULL if the field is whitespace (blank, tab, formfeed etc.)
 *  Else return pointer to first non-whitespace character.
 */
const char * qof_util_whitespace_filter (const char * val);

/** Return integer 1 if the string starts with 't' or 'T' or
 *  contains the word 'true' or 'TRUE'; if string is a number,
 *  return that number. (Leading whitespace is ignored). */
int qof_util_bool_to_int (const char * val);

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

/** Get the gnc_string_cache.  Create it if it doesn't exist already

\todo hide the gcache as a static */
GCache* gnc_engine_get_string_cache(void);

void gnc_engine_string_cache_destroy (void);

/* You can use this function as a destroy notifier for a GHashTable
   that uses common strings as keys (or values, for that matter.) */
void gnc_string_cache_remove(gpointer key);

/* You can use this function with g_hash_table_insert(), or the key
  (or value), as long as you use the destroy notifier above. */
gpointer gnc_string_cache_insert(gpointer key);

#endif /* QOF_UTIL_H */
/** @} */

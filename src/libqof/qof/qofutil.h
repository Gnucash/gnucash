/********************************************************************\
 * qof-util.h -- QOF utility functions                              *
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
/** @file qofutil.h
    @brief QOF utility functions
    @author Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 1997-2002,2004 Linas Vepstas <linas@linas.org>
    @author Copyright 2006  Neil Williams  <linux@codehelp.co.uk>
*/

#ifndef QOF_UTIL_H
#define QOF_UTIL_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <stddef.h>
#include "qof.h"
#include "qoflog.h"
#include "qofutil.h"
#include "qofbackend.h"
#include "qofclass.h"
#include "qofbook.h"
#include "qofinstance.h"

/** Do not use these for printf, only scanf */
#if HAVE_SCANF_LLD
# define QOF_SCANF_LLD "%lld"
#else
# if HAVE_SCANF_QD
#  define QOF_SCANF_LLD "%qd"
# else
#  if HAVE_SCANF_I64D
#   define QOF_SCANF_LLD "%I64d"
#  else
#   error "No scanf format string is known for LLD. Fix your ./configure so that the correct one is detected!"
#  endif
# endif
#endif

#define QOF_MOD_UTIL "qof.utilities"

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
    const gchar* name##asString(name n);

#define AS_STRING_FUNC(name, list)        \
    const gchar* name##asString(name n) { \
        switch (n) {                      \
            list(AS_STRING_CASE)          \
            default: return "";  } }

#define FROM_STRING_DEC(name, list)      \
    name name##fromString                \
    (const gchar* str);

#define FROM_STRING_FUNC(name, list)     \
    name name##fromString                \
    (const gchar* str) {                 \
    if(str == NULL) { return 0; }        \
        list(FROM_STRING_CASE)           \
        return 0;  }

/** @} */

/** \name enum as string with no typedef
@{

  Similar but used when the enum is NOT a typedef
  Make sure you use the DEFINE_ENUM_NON_TYPEDEF macro.

 You can precede the FROM_STRING_FUNC_NON_TYPEDEF
 and AS_STRING_FUNC_NON_TYPEDEF macros with the
 keyword static if appropriate.

 ENUM_BODY is used in both types.
 */

#define DEFINE_ENUM_NON_TYPEDEF(name, list)   \
    enum name {                               \
        list(ENUM_BODY)                       \
    };

#define FROM_STRING_DEC_NON_TYPEDEF(name, list)   \
   void name##fromString                          \
   (const gchar* str, enum name *type);

#define FROM_STRING_CASE_NON_TYPEDEF(name, value) \
   if (strcmp(str, #name) == 0) { *type = name; }

#define FROM_STRING_FUNC_NON_TYPEDEF(name, list)  \
   void name##fromString                          \
   (const gchar* str, enum name *type) {          \
   if(str == NULL) { return; }                    \
    list(FROM_STRING_CASE_NON_TYPEDEF) }

#define AS_STRING_DEC_NON_TYPEDEF(name, list)     \
   const gchar* name##asString(enum name n);

#define AS_STRING_FUNC_NON_TYPEDEF(name, list)    \
   const gchar* name##asString(enum name n) {     \
       switch (n) {                               \
           list(AS_STRING_CASE_NON_TYPEDEF)       \
           default: return ""; } }

#define AS_STRING_CASE_NON_TYPEDEF(name, value)   \
   case name: { return #name; }

/** @} */

/** @name Convenience wrappers
   @{
*/

/** \brief Initialise the Query Object Framework

Use in place of separate init functions (like qof_query_init(),
etc.) to protect against future changes.
*/
void qof_init (void);

/** \brief Safely close down the Query Object Framework

Use in place of separate close / shutdown functions
(like qof_query_shutdown(), etc.) to protect
against future changes.
*/
void qof_close (void);

/** @} */

/* **** Prototypes *********************************************/

/** Calls the given function for each of the key/value pairs in the
 *  GHashTable in an order determined by the GCompareFunc applied to
 *  the keys. The function is passed the key and value of each pair,
 *  and the given user_data parameter. */
void g_hash_table_foreach_sorted(GHashTable *hash_table, GHFunc func, gpointer user_data, GCompareFunc compare_func);

/** Search for an occurence of the substring needle in the string
 * haystack, ignoring case. Return TRUE if one is found or FALSE
 * otherwise. */
gboolean qof_utf8_substr_nocase (const gchar *haystack, const gchar *needle);

/** case sensitive comparison of strings da and db - either
may be NULL. A non-NULL string is greater than a NULL string.

 @param da string 1.
 @param db string 2.

 @return If da == NULL && db != NULL, returns -1.
         If da != NULL && db == NULL, returns +1.
         If da != NULL && db != NULL, returns the result of
                   strcmp(da, db).
         If da == NULL && db == NULL, returns 0.
*/
gint safe_strcasecmp (const gchar * da, const gchar * db);

/** The null_strcmp compares strings a and b the same way that strcmp()
 * does, except that either may be null.  This routine assumes that
 * a null string is equal to the empty string.
 */
gint null_strcmp (const gchar * da, const gchar * db);

/** The ultostr() subroutine is the inverse of strtoul(). It accepts a
 * number and prints it in the indicated base.  The returned string
 * should be g_freed when done.  */
gchar * ultostr (gulong val, gint base);

/** Returns true if string s is a number, possibly surrounded by
 * whitespace. */
gboolean gnc_strisnum(const gchar *s);

#ifndef HAVE_STPCPY
#define stpcpy g_stpcpy
#endif

/** begin_edit
 *
 * @param  inst: an instance of QofInstance
 *
 * The caller should use this macro first and then perform any other operations.
 */
gboolean qof_begin_edit(QofInstance *inst);

/**
 * commit_edit helpers
 *
 * The caller should call PART1 as the first thing, then
 * perform any local operations prior to calling the backend.
 * Then call PART2.
 */

/**
 * part1 -- deal with the editlevel
 *
 * @param inst: an instance of QofInstance
 */
gboolean qof_commit_edit(QofInstance *inst);

/**
 * part2 -- deal with the backend
 *
 * @param inst: an instance of QofInstance
 * @param on_error: a function called if there is a backend error.
 *                void (*on_error)(inst, QofBackendError)
 * @param on_done: a function called after the commit is completed
 *                successfully for an object which remained valid.
 *                void (*on_done)(inst)
 * @param on_free: a function called if the commit succeeded and the instance
 *                 is to be freed.
 *                void (*on_free)(inst)
 *
 * Note that only *one* callback will be called (or zero, if that
 * callback is NULL).  In particular, 'on_done' will not be called for
 * an object which is to be freed.
 *
 * Returns TRUE, if the commit succeeded, FALSE otherwise.
 */
gboolean
qof_commit_edit_part2(QofInstance *inst,
                      void (*on_error)(QofInstance *, QofBackendError),
                      void (*on_done)(QofInstance *),
                      void (*on_free)(QofInstance *));

#ifdef __cplusplus
}
#endif

#endif /* QOF_UTIL_H */
/** @} */

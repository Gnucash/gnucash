/********************************************************************\
 * guid.h -- globally unique ID User API                            *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
 *               2014 Aaron Laws <dartmetrash@gmail.com>            *
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
 *                                                                  *
\********************************************************************/

#ifndef GUID_H
#define GUID_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <stddef.h>
#include <glib-object.h>

/** @addtogroup Entity
    @{ */
/** @addtogroup GncGUID
    Globally Unique IDs provide a way to uniquely identify
    something.  A GncGUID is a unique, cryptographically
    random 128-bit value.  The identifier is so random that
    it is safe to assume that there is no other such item
    on the planet Earth, and indeed, not even in the Galaxy
    or beyond.

    QOF GncGUIDs can be used independently of any other subsystem
    in QOF. In particular, they do not require the use of
    other parts of the object subsystem. New GncGUIDs are usually
    created by initializing a new entity using qof_instance_init,
    rather than calling GncGUID functions directly.

    @{ */
/** @file guid.h
    @brief  globally unique ID User API
    @author Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
        Copyright 2014 Aaron Laws <dartmetrash@gmail.com>
*/

#define GUID_DATA_SIZE	16

#define GNC_TYPE_GUID (gnc_guid_get_type())
#define GNC_VALUE_HOLDS_GUID(value) G_VALUE_HOLDS(value, GNC_TYPE_GUID)

/** The type used to store guids */
typedef struct _gncGuid {
    unsigned char reserved[GUID_DATA_SIZE];
} GncGUID;

GType gnc_guid_get_type (void);
const GncGUID* gnc_value_get_guid (const GValue *value);

/** Number of characters needed to encode a guid as a string
 * not including the null terminator. */
#define GUID_ENCODING_LENGTH 32

/** Generate a new guid.
 *
 *  @param guid A pointer to an allocated guid data structure.  The
 *  existing value will be replaced with a new value.
 */
void guid_replace (GncGUID *guid);

/** Generate a new id.
 *
 * @return guid A data structure containing a copy of a newly constructed GncGUID.
 */
GncGUID guid_new_return (void);

/** Returns a GncGUID which is guaranteed to never reference any entity.
 * 
 * Do not free this value! The same pointer is returned on each call.*/
const GncGUID * guid_null (void);

/** 
 * Allocate memory for a GUID. This does not construct a GUID. In other words,
 * the returned pointer has not necessarily been initialized. The returned 
 * pointer must be freed with * guid_free.
 */
GncGUID * guid_malloc (void);

/**
 * Allocate and construct a new GUID. The returned pointer must be 
 * released with guid_free.
 */
GncGUID * guid_new (void);

/*Free the guid pointed to. Do not use this guid any more.*/
void   guid_free (GncGUID *guid);

/**
 * Returns a newly allocated GncGUID that matches the passed-in GUID.
 * The returned pointer must be freed using guid_free.
 */
GncGUID *guid_copy (const GncGUID *guid);

/** The guid_to_string() routine returns a null-terminated string
 *  encoding of the id. String encodings of identifiers are hex
 *  numbers printed only with the characters '0' through '9' and
 *  'a' through 'f'. The encoding will always be GUID_ENCODING_LENGTH
 *  characters long (not including the null terminator).
 *
 *  @param guid The guid to print.
 *
 *  @return A pointer to the starting character of the string.  The
 *  returned memory is owned by the calling routine and must be freed
 *  using g_free.
 */
gchar * guid_to_string (const GncGUID * guid);

/** The guid_to_string_buff() routine puts a null-terminated string
 *  encoding of the id into the memory pointed at by buff.  The
 *  buffer must be at least GUID_ENCODING_LENGTH+1 characters long.
 *  This routine is handy for avoiding a malloc/free cycle.  It
 *  returns a pointer to the >>end<< of what was written.  (i.e. it
 *  can be used like 'stpcpy' during string concatenation)
 *
 *  @param guid The guid to print.
 *
 *  @param buff The buffer to print it into.
 *
 *  @return A pointer to the terminating null character of the string,
 *     or, if no copy took place, NULL.
 */
gchar * guid_to_string_buff (const GncGUID * guid, /*@ out @*/ gchar *buff);


/** Given a string, replace the given guid with the parsed one unless
 * the given value is null.
 * If null is passed as guid or string, false is returned and nothing
 * is done, otherwise, the function returns true.
 * This function accepts both uppor and lower case hex digits. If
 * letters outside the range of [a-fA-F] are passed, they are silently
 * replaced. If non-alphanumeric digits are given, this function will
 * either return false or replace those values with others.
 */
gboolean string_to_guid(const gchar * string, /*@ out @*/ GncGUID * guid);


/** Given two GUIDs, return TRUE if they are non-NULL and equal.
 * Return FALSE, otherwise. */
gboolean guid_equal(const GncGUID *guid_1, const GncGUID *guid_2);
gint     guid_compare(const GncGUID *g1, const GncGUID *g2);

/** Hash function for a GUID. Given a GncGUID *, hash it to a guint */
guint guid_hash_to_guint(gconstpointer ptr);

/** Equality function for two GUIDs in a GHashTable. */
gint guid_g_hash_table_equal (gconstpointer guid_a, gconstpointer guid_b);

/** Returns a GHashTable with <GUID*> as key and a <gpointer> as
 * value and no destructor functions for key or value set. */
GHashTable *guid_hash_table_new(void);

/* @} */
/* @} */
#ifdef __cplusplus
}
#endif

#endif

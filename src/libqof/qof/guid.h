/********************************************************************\
 * guid.h -- globally unique ID User API                            *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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

#include <stddef.h>
#include <glib-object.h>

/** @addtogroup Entity
    @{ */
/** @addtogroup GUID
    Globally Unique ID's provide a way to uniquely identify
    some thing.  A GUID is a unique, cryptographically
    random 128-bit value.  The identifier is so random that
    it is safe to assume that there is no other such item
    on the planet Earth, and indeed, not even in the Galaxy
    or beyond.

    QOF GUID's can be used independently of any other subsystem
    in QOF. In particular, they do not require the use of
    other parts of the object subsystem. New GUID's are usually
    created by initialising a new entity using qof_instance_init,
    rather than calling GUID functions directly.

    @{ */
/** @file guid.h
    @brief  globally unique ID User API
    @author Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
*/

/** The type used to store guids */
#define GUID_DATA_SIZE	16
typedef union _GUID
{
    guchar data[GUID_DATA_SIZE];

    gint __align_me; /* this just ensures that GUIDs are 32-bit
                   * aligned on systems that need them to be. */
} GUID;


#define GNC_TYPE_GUID (gnc_guid_get_type())
#define GNC_VALUE_HOLDS_GUID(value) G_VALUE_HOLDS(value, GNC_TYPE_GUID)

GType gnc_guid_get_type (void);
G_CONST_RETURN GUID* gnc_value_get_guid (const GValue *value);

/** number of characters needed to encode a guid as a string
 * not including the null terminator. */
#define GUID_ENCODING_LENGTH 32


/** Initialize the id generator with a variety of random
 *  sources.
 *
 *  @note Only one of guid_init(), guid_init_with_salt() and
 *  guid_init_only_salt() should be called.  Calling any
 *  initialization function a second time will reset the generator and
 *  erase the effect of the first call.
 */
void guid_init(void);

/** Initialize the id generator with a variety of random sources. and
 *  with the data given in the salt argument. This argument can be
 *  used to add additional randomness to the generated ids.
 *
 *  @param salt The additional random values to add to the generator.
 *
 *  @param salt_len The length of the additional random values.
 *
 *  @note Only one of guid_init(), guid_init_with_salt() and
 *  guid_init_only_salt() should be called.  Calling any
 *  initialization function a second time will reset the generator and
 *  erase the effect of the first call.
 */
void guid_init_with_salt(const void *salt, size_t salt_len);

/** Initialize the id generator with the data given in the salt
 *  argument, but not with any other source. Calling this function with
 *  a specific argument will reliably produce a specific sequence of
 *  ids.
 *
 *  @param salt The additional random values to add to the generator.
 *
 *  @param salt_len The length of the additional random values.
 *
 *  @note Only one of guid_init(), guid_init_with_salt() and
 *  guid_init_only_salt() should be called.  Calling any
 *  initialization function a second time will reset the generator and
 *  erase the effect of the first call.
 */
void guid_init_only_salt(const void *salt, size_t salt_len);

/** Release the memory chunk associated with gui storage. Use this
 *  only when shutting down the program, as it invalidates *all*
 *  GUIDs at once. */
void guid_shutdown (void);

/** Generate a new id. If no initialization function has been called,
 *  guid_init() will be called before the id is created.
 *
 *  @param guid A pointer to an existing guid data structure.  The
 *  existing value will be replaced with a new value.
 *
 * This routine uses the md5 algorithm to build strong random guids.
 * Note that while guid's are generated randomly, the odds of this
 * routine returning a non-unique id are astronomically small.
 * (Literally astronomically: If you had Cray's on every solar
 * system in the universe running for the entire age of the universe,
 * you'd still have less than a one-in-a-million chance of coming up
 * with a duplicate id.  2^128 == 10^38 is a really really big number.)
 */
void guid_new(GUID *guid);

/** Generate a new id. If no initialization function has been called,
 *  guid_init() will be called before the id is created.
 *
 * @return guid A data structure containing a newly allocated GUID.
 *  Caller is responsible for calling guid_free().
 */
GUID guid_new_return(void);

/** Returns a GUID which is guaranteed
to never reference any entity. */
const GUID * guid_null (void);

/** Efficiently allocate & free memory for GUIDs */
GUID * guid_malloc (void);

/* Return a guid set to all zero's */
void   guid_free (GUID *guid);

GUID *guid_copy (const GUID *guid);

/** The guid_to_string() routine returns a null-terminated string
 *  encoding of the id. String encodings of identifiers are hex
 *  numbers printed only with the characters '0' through '9' and
 *  'a' through 'f'. The encoding will always be GUID_ENCODING_LENGTH
 *  characters long.
 *
 *  XXX This routine is not thread safe and is deprecated. Please
 *  use the routine guid_to_string_buff() instead.
 *
 *  @param guid The guid to print.
 *
 *  @return A pointer to the starting character of the string.  The
 *  returned memory is owned by this routine and may not be freed by
 *  the caller.
 */
const gchar * guid_to_string (const GUID * guid);

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
 *  @return A pointer to the terminating null character of the string.
 */
gchar * guid_to_string_buff (const GUID * guid, /*@ out @*/ gchar *buff);


/** Given a string, decode the id into the guid if guid is non-NULL.
 * The function returns TRUE if the string was a valid 32 character
 * hexadecimal number. This function accepts both upper and lower case
 * hex digits. If the return value is FALSE, the effect on guid is
 * undefined. */
gboolean string_to_guid(const gchar * string, /*@ out @*/ GUID * guid);


/** Given two GUIDs, return TRUE if they are non-NULL and equal.
 * Return FALSE, otherwise. */
gboolean guid_equal(const GUID *guid_1, const GUID *guid_2);
gint     guid_compare(const GUID *g1, const GUID *g2);

/** Given a GUID *, hash it to a guint */
guint guid_hash_to_guint(gconstpointer ptr);

GHashTable *guid_hash_table_new(void);

/* @} */
/* @} */
#endif

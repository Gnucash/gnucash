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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __GUID_H__
#define __GUID_H__ 

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <glib.h>
#include <stddef.h>

/* This file defines an API for using globally unique identifiers. */

/* The type used to store guids */
typedef union _GUID
{
  unsigned char data[16];

  int __align_me; /* this just ensures that GUIDs are 32-bit
                   * aligned on systems that need them to be. */
} GUID;


/* number of characters needed to encode a guid as a string
 * not including the null terminator. */
#define GUID_ENCODING_LENGTH 32


/* Three functions to initialize the id generator. Only one needs to
 * be called. Calling any initialization function a second time will
 * reset the generator and erase the effect of the first call.
 *
 * guid_init() will initialize the generator with a variety of random
 * sources.
 *
 * guid_init_with_salt() will initialize the generator with guid_init()
 * and with the data given in the salt argument. This argument can be
 * used to add additional randomness to the generated ids.
 *
 * guid_init_only_salt() will initialize the generator with the data
 * given in the salt argument, but not with any other source. Calling
 * guid_init_only_salt() with a specific argument will produce a
 * specific sequence of ids reliably. */
void guid_init(void);
void guid_init_with_salt(const void *salt, size_t salt_len);
void guid_init_only_salt(const void *salt, size_t salt_len);


/* Generate a new id. If no initialization function has been called,
 * guid_init() will be called before the id is created. */
void guid_new(GUID *guid);


/* The guid_to_string() routine returns a null-terminated string 
 *    encoding of the id. String encodings of identifiers are hex 
 *    numbers printed only with the characters '0' through '9' and 
 *    'a' through 'f'. The encoding will always be GUID_ENCODING_LENGTH 
 *    characters long. The returned string should be freed when no 
 *    longer needed. 
 *
 * The guid_to_string_buff() routine does the same, except that the
 *    string is written into the memory pointed at by buff.  The
 *    buffer must be at least GUID_ENCODING_LENGTH+1 characters long.
 *    This routine is handy for avoiding a malloc/free cycle.
 *    It returns a pointer to the >>end<< of what was written.
 *    (i.e. it can be used like 'stpcpy' during string concatenation)
 */
char * guid_to_string (const GUID * guid);
char * guid_to_string_buff (const GUID * guid, char *buff);


/* Given a string, decode the id into the guid if guid is non-NULL.
 * The function returns TRUE if the string was a valid 32 character
 * hexadecimal number. This function accepts both upper and lower case
 * hex digits. If the return value is FALSE, the effect on guid is
 * undefined. */
gboolean string_to_guid(const char * string, GUID * guid);


/* Given two GUIDs, return TRUE if they are non-NULL and equal.
 * Return FALSE, otherwise. */
gboolean guid_equal(const GUID *guid_1, const GUID *guid_2);
gint     guid_compare(const GUID *g1, const GUID *g2);

/* Given a GUID *, hash it to a guint */
guint guid_hash_to_guint(gconstpointer ptr);

GHashTable *guid_hash_table_new(void);

#endif

/********************************************************************\
 * kvp_frame.h -- a key-value frame system for gnucash.
 * Copyright (C) 2000 Bill Gribble                                  *
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

#ifndef __KVP_FRAME_H__
#define __KVP_FRAME_H__

#include <sys/types.h>
#include <guid.h>
#include <glib.h>

/* a kvp_frame is a set of associations between character strings
 * (keys) and kvp_value structures.  A kvp_value is a union with
 * possible types enumerated in the kvp_value_t enum.
 * 
 * Pointers passed as arguments into set_slot and get_slot are the
 * responsibility of the caller.  Pointers returned by get_slot are
 * the responsibility of the caller (they point to newly-allocated
 * structures which are deep value copies of those in the frame).
 * 
 * kvp_frame_delete and kvp_value_delete are deep (recursive) deletes.
 * kvp_frame_copy and kvp_value_copy are deep value copies. */
 
typedef enum { KVP_TYPE_NONE, 
               KVP_TYPE_INT64, KVP_TYPE_FLOAT64,
               KVP_TYPE_STRING, KVP_TYPE_GUID, KVP_TYPE_BINARY, 
               KVP_TYPE_LIST, KVP_TYPE_FRAME } kvp_value_t;

typedef struct _kvp_frame kvp_frame;
typedef struct _kvp_list  kvp_list;
typedef union  _kvp_value kvp_value;

/* kvp_frame functions */
kvp_frame   * kvp_frame_new();
void        kvp_frame_delete(kvp_frame * frame);
kvp_frame   * kvp_frame_copy(const kvp_frame * frame);
void        kvp_frame_set_slot(kvp_frame * frame, 
                               const char * key, const kvp_value * value);
kvp_value   * kvp_frame_get_slot(kvp_frame * frame, 
                                 const char * key);

kvp_value   * kvp_value_new();
void        kvp_value_delete(kvp_value * value);
kvp_value   * kvp_value_copy(const kvp_value * value);

/* kvp_list functions */
kvp_list    * kvp_list_new();
void        kvp_list_delete(kvp_list * list);
kvp_list    * kvp_list_copy(const kvp_list * list);
int         kvp_list_null_p(const kvp_list * list);

kvp_value   * kvp_list_car(kvp_list * list);
kvp_list    * kvp_list_cdr(kvp_list * list);
kvp_list    * kvp_list_cons(kvp_value * car, kvp_list * cdr);

/* value constructors (copying for pointer args) */
kvp_value   * kvp_value_new_int64(gint64 value);
kvp_value   * kvp_value_new_float64(double value);
kvp_value   * kvp_value_new_string(const char * value);
kvp_value   * kvp_value_new_guid(const GUID * guid);
kvp_value   * kvp_value_new_binary(const void * data, int datasize);
kvp_value   * kvp_value_new_list(const kvp_list * value);
kvp_value   * kvp_value_new_frame(const kvp_frame * value);

/* value accessors (NON-copying for frames/lists/guids/binary) */
kvp_value_t kvp_value_get_type(const kvp_value * val);

gint64      kvp_value_get_int64(const kvp_value * val);
double      kvp_value_get_float64(const kvp_value * val);
char        * kvp_value_get_string(const kvp_value * val);
GUID        * kvp_value_get_guid(const kvp_value * val);
void        * kvp_value_get_binary(const kvp_value * val, int * size_return); 
kvp_list    * kvp_value_get_list(const kvp_value * val);
kvp_frame   * kvp_value_get_frame(const kvp_value * val);

#endif

/********************************************************************\
 * kvp_frame.h -- a key-value frame system for gnucash.             *
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
 * owned by the kvp_frame.  Make copies as needed.
 * 
 * kvp_frame_delete and kvp_value_delete are deep (recursive) deletes.
 * kvp_frame_copy and kvp_value_copy are deep value copies. */
 
typedef enum {
  KVP_TYPE_GINT64,
  KVP_TYPE_DOUBLE,
  KVP_TYPE_STRING,
  KVP_TYPE_GUID,
  KVP_TYPE_BINARY, 
  KVP_TYPE_GLIST,
  KVP_TYPE_FRAME
} kvp_value_t;

typedef struct _kvp_frame kvp_frame;

#if 0
typedef union _kvp_value kvp_value;
#else
typedef struct _kvp_value kvp_value;
#endif

/* kvp_frame functions */
kvp_frame   * kvp_frame_new(void);
void          kvp_frame_delete(kvp_frame * frame);
kvp_frame   * kvp_frame_copy(const kvp_frame * frame);
void          kvp_frame_set_slot(kvp_frame * frame, 
                               const char * key, const kvp_value * value);
kvp_value   * kvp_frame_get_slot(kvp_frame * frame, 
                                 const char * key);

gint          kvp_frame_compare(const kvp_frame *fa, const kvp_frame *fb);

void          kvp_value_delete(kvp_value * value);
kvp_value   * kvp_value_copy(const kvp_value * value);
gint          kvp_value_compare(const kvp_value *va, const kvp_value *vb);

/* list convenience funcs. */
gint        kvp_glist_compare(const GList * list1, const GList * list2);

GList     * kvp_glist_copy(const GList * list);
            /* deep copy: same as mapping kvp_value_copy over the
               elements and then copying the spine. */
void        kvp_glist_delete(GList * list);
            /* deep delete: same as mapping kvp_value_delete over the
               elements and then deleting the GList. */

/* value constructors (copying for pointer args) */
kvp_value   * kvp_value_new_gint64(gint64 value);
kvp_value   * kvp_value_new_double(double value);
kvp_value   * kvp_value_new_string(const char * value);
kvp_value   * kvp_value_new_guid(const GUID * guid);
kvp_value   * kvp_value_new_binary(const void * data, guint64 datasize);
kvp_value   * kvp_value_new_glist(const GList * value);
kvp_value   * kvp_value_new_frame(const kvp_frame * value);

/* value constructors (non-copying - kvp_value takes pointer ownership)
   values *must* have been allocated via glib allocators! (gnew, etc.) */
kvp_value   * kvp_value_new_binary_nc(void * data, guint64 datasize);
kvp_value   * kvp_value_new_glist_nc(GList *lst);

/* value accessors (NON-copying for frames/lists/guids/binary) */
kvp_value_t kvp_value_get_type(const kvp_value * value);

gint64      kvp_value_get_gint64(const kvp_value * value);
double      kvp_value_get_double(const kvp_value * value);
char        * kvp_value_get_string(const kvp_value * value);
GUID        * kvp_value_get_guid(const kvp_value * value);
void        * kvp_value_get_binary(const kvp_value * value,
                                   guint64 * size_return); 
GList       * kvp_value_get_glist(const kvp_value * value);
kvp_frame   * kvp_value_get_frame(const kvp_value * value);

/* manipulators */

gboolean kvp_value_binary_append(kvp_value *v, void *data, guint64 size);
/* copying - but more efficient than creating a new kvp_value manually. */

/* iterators */

/* Traverse all of the slots in the given kvp_frame.  This function
   does not descend recursively to traverse any kvp_frames stored as
   slot values.  You must handle that in proc, with a suitable
   recursive call if desired. */
void kvp_frame_for_each_slot(kvp_frame *f,
                             void (*proc)(const char *key,
                                          kvp_value *value,
                                          gpointer data),
                             gpointer data);

#endif

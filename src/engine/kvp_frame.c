/********************************************************************
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
 ********************************************************************/

#include "kvp_frame.h"
#include "guid.h"

#include <string.h>
#include <glib.h>

struct _kvp_frame {
  GHashTable  * hash;
};

struct _kvp_list {
  GList       * list;
};

struct _kvp_value_int64 {
  kvp_value_t type;
  gint64      value;
};

struct _kvp_value_float64 {
  kvp_value_t type;
  double      value;
};

struct _kvp_value_string {
  kvp_value_t type;
  char        * value;
};

struct _kvp_value_guid {
  kvp_value_t type;
  GUID        * value;
};

struct _kvp_value_binary {
  kvp_value_t type;
  void        * value;
  int         datasize;
};

struct _kvp_value_frame {
  kvp_value_t type;
  kvp_frame   * value;
};

struct _kvp_value_list {
  kvp_value_t type;
  kvp_list    * value;
};

union _kvp_value {
  kvp_value_t               type;
  struct _kvp_value_int64   int64;
  struct _kvp_value_float64 float64;
  struct _kvp_value_string  str;
  struct _kvp_value_guid    guid;
  struct _kvp_value_binary  binary;
  struct _kvp_value_list    list;
  struct _kvp_value_frame   frame;
};


/********************************************************************
 * kvp_frame functions
 ********************************************************************/

static guint 
kvp_hash_func(gconstpointer v) {
  return g_str_hash(v);
}

static gint
kvp_comp_func(gconstpointer v, gconstpointer v2) {
  return g_str_equal(v, v2);
}

kvp_frame * 
kvp_frame_new() {
  kvp_frame * retval = g_new0(kvp_frame, 1);
  retval->hash = g_hash_table_new(&kvp_hash_func, 
                                  &kvp_comp_func);
  return retval;
}

static void
kvp_frame_delete_worker(gpointer key, gpointer value, gpointer user_data) {
  g_free(key);
  kvp_value_delete((kvp_value *)value);  
}

void
kvp_frame_delete(kvp_frame * frame) {
  /* free any allocated resource for frame or its children */
  g_hash_table_foreach(frame->hash, & kvp_frame_delete_worker, 
                       (gpointer)frame);
  
  /* delete the hash table */
  g_hash_table_destroy(frame->hash);
  frame->hash = NULL;
  g_free(frame);
}

static void
kvp_frame_copy_worker(gpointer key, gpointer value, gpointer user_data) {
  kvp_frame * dest = (kvp_frame *)user_data;
  g_hash_table_freeze(dest->hash);
  g_hash_table_insert(dest->hash,
                      (gpointer)g_strdup(key), 
                      (gpointer)kvp_value_copy(value));
  g_hash_table_thaw(dest->hash);
}

kvp_frame * 
kvp_frame_copy(const kvp_frame * frame) {
  kvp_frame * retval = kvp_frame_new();
  g_hash_table_foreach(frame->hash,
                       & kvp_frame_copy_worker, 
                       (gpointer)retval);
  return retval;
}

void
kvp_frame_set_slot(kvp_frame * frame, const char * slot, 
                   const kvp_value * value) {
  gpointer orig_key;
  gpointer orig_value;
  gpointer new_value;
  int      key_exists;

  new_value = kvp_value_copy(value);

  g_hash_table_freeze(frame->hash);

  key_exists = g_hash_table_lookup_extended(frame->hash, slot,
                                            & orig_key, & orig_value);
  if (key_exists) {
    g_hash_table_remove(frame->hash, slot);
    g_free(orig_key);
    kvp_value_delete(orig_value);
  }

  g_hash_table_insert(frame->hash, g_strdup(slot), new_value);

  g_hash_table_thaw(frame->hash);
}

kvp_value * 
kvp_frame_get_slot(kvp_frame * frame, const char * slot) {
  return (kvp_value *)g_hash_table_lookup(frame->hash, slot);
}

/********************************************************************
 * kvp_list functions
 ********************************************************************/

kvp_list *
kvp_list_new() {
  kvp_list * retval = g_new0(kvp_list, 1);
  retval->list = NULL;
  return retval;
}

static void
kvp_list_delete_worker(gpointer datum, gpointer user_data) {
  kvp_value * val = (kvp_value *)datum;
  kvp_value_delete(val);
}

void
kvp_list_delete(kvp_list * list) {
  if(list) {    
    if(list->list) {
      /* delete the data in the list */
      g_list_foreach(list->list, & kvp_list_delete_worker, NULL);
      
      /* free the backbone */
      g_list_free(list->list);
    }
    g_free(list);
  }
}

kvp_list *
kvp_list_copy(const kvp_list * list) {
  kvp_list * retval = kvp_list_new();
  GList * lptr;

  if(!list) return retval;
  
  /* duplicate the backbone of the list (this duplicates the POINTERS
   * to the values; we need to deep-copy the values separately) */
  retval->list = g_list_copy(list->list);
  
  /* this step deep-copies the values */
  for(lptr = retval->list; lptr; lptr = lptr->next) {
    lptr->data = kvp_value_copy(lptr->data);
  }
  
  return retval;
}

int
kvp_list_null_p(const kvp_list * list) {
  return (!list || (list->list == NULL));
}

kvp_value * 
kvp_list_car(kvp_list * list) {
  if(!list || (list->list == NULL)) {
    return NULL;
  }
  else {
    return (kvp_value *)(list->list->data);
  }
}

kvp_list *
kvp_list_cdr(kvp_list * list) {
  kvp_list * retval;
  if(!list || (list->list == NULL)) {
    return NULL;
  }
  else {
    retval = kvp_list_new();
    retval->list = list->list->next;
    return retval;
  }
}

/* cons semantics are "hand over": you give it pointers to 
 * your objects and the list handles them after that. */

kvp_list *
kvp_list_cons(kvp_value * car, kvp_list * cdr) {
  kvp_list * retval;
  if(!car || !cdr) {
    return NULL;
  }
  else {
    retval = kvp_list_new();
    retval->list = g_list_prepend( cdr->list, (gpointer)car);

    cdr->list = NULL;
    kvp_list_delete(cdr);
    return retval;
  }
}

kvp_list *
kvp_list_1(kvp_value * value) {
  kvp_list * retval = kvp_list_new();

  retval = kvp_list_cons(value, retval);

  return retval;
}

kvp_list *
kvp_list_2(kvp_value * value1, kvp_value * value2) {
  kvp_list * retval = kvp_list_new();

  retval = kvp_list_cons(value2, retval);
  retval = kvp_list_cons(value1, retval);

  return retval;
}

kvp_list *
kvp_list_3(kvp_value * value1, kvp_value * value2,
           kvp_value * value3) {
  kvp_list * retval = kvp_list_new();

  retval = kvp_list_cons(value3, retval);
  retval = kvp_list_cons(value2, retval);
  retval = kvp_list_cons(value1, retval);

  return retval;
}

kvp_list *
kvp_list_4(kvp_value * value1, kvp_value * value2,
           kvp_value * value3, kvp_value * value4) {
  kvp_list * retval = kvp_list_new();

  retval = kvp_list_cons(value4, retval);
  retval = kvp_list_cons(value3, retval);
  retval = kvp_list_cons(value2, retval);
  retval = kvp_list_cons(value1, retval);

  return retval;
}


kvp_list *
kvp_list_5(kvp_value * value1, kvp_value * value2,
           kvp_value * value3, kvp_value * value4,
           kvp_value * value5) {
  kvp_list * retval = kvp_list_new();
  
  retval = kvp_list_cons(value5, retval);
  retval = kvp_list_cons(value4, retval);
  retval = kvp_list_cons(value3, retval);
  retval = kvp_list_cons(value2, retval);
  retval = kvp_list_cons(value1, retval);
  
  return retval;
}

/********************************************************************
 * kvp_value functions
 ********************************************************************/

kvp_value *
kvp_value_new() {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type = KVP_TYPE_NONE;
  return retval;
}

kvp_value *
kvp_value_new_int64(gint64 value) {
  kvp_value * retval  = g_new0(kvp_value, 1);
  retval->type        = KVP_TYPE_INT64;
  retval->int64.value = value;
  return retval;
}  

kvp_value *
kvp_value_new_float64(double value) {
  kvp_value * retval  = g_new0(kvp_value, 1);
  retval->type        = KVP_TYPE_FLOAT64;
  retval->float64.value = value;
  return retval;
}  

kvp_value *
kvp_value_new_string(const char * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_STRING;
  retval->str.value  = g_strdup(value);
  return retval;
}  

kvp_value *
kvp_value_new_guid(const GUID * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_GUID;
  retval->guid.value = g_new0(GUID, 1);
  memcpy(retval->guid.value, value, sizeof(GUID));
  return retval;
}  

kvp_value *
kvp_value_new_binary(const void * value, int datasize) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_BINARY;
  retval->binary.value    = g_new0(char, datasize);
  retval->binary.datasize = datasize;
  memcpy(retval->binary.value, value, datasize);
  return retval;
}

kvp_value *
kvp_value_new_list(const kvp_list * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_LIST;
  retval->list.value = kvp_list_copy(value);
  return retval;
}  

kvp_value *
kvp_value_new_frame(const kvp_frame * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_FRAME;
  retval->frame.value  = kvp_frame_copy(value);
  return retval;  
}

void
kvp_value_delete(kvp_value * value) {
  if(!value) return;

  switch(value->type) {
  case KVP_TYPE_STRING:
    g_free(value->str.value);
    break;
  case KVP_TYPE_GUID:
    g_free(value->guid.value);
    break;
  case KVP_TYPE_BINARY:
    g_free(value->binary.value);
    break;
  case KVP_TYPE_LIST:
    kvp_list_delete(value->list.value);
    break;
  case KVP_TYPE_FRAME:
    kvp_frame_delete(value->frame.value);
    break;
    
  case KVP_TYPE_NONE:
  case KVP_TYPE_INT64:    
  case KVP_TYPE_FLOAT64:
  default:
  }
  g_free(value);
}

kvp_value_t
kvp_value_get_type(const kvp_value * value) {
  return value->type;
}

gint64
kvp_value_get_int64(const kvp_value * value) {
  if(value->type == KVP_TYPE_INT64) {
    return value->int64.value;
  }
  else {
    return 0;
  }
}

double 
kvp_value_get_float64(const kvp_value * value) {
  if(value->type == KVP_TYPE_FLOAT64) {
    return value->float64.value;
  }
  else {
    return 0.0;
  }
}

char *
kvp_value_get_string(const kvp_value * value) {
  if(value->type == KVP_TYPE_STRING) {
    return value->str.value;
  }
  else { 
    return NULL; 
  }
}

GUID *
kvp_value_get_guid(const kvp_value * value) {
  if(value->type == KVP_TYPE_GUID) {
    return value->guid.value;
  }
  else {
    return NULL;
  }
}

void *
kvp_value_get_binary(const kvp_value * value, int * size_return) {
  if(value->type == KVP_TYPE_BINARY) {
    *size_return = value->binary.datasize;
    return value->binary.value;
  }
  else {
    *size_return = 0;
    return NULL;
  }
}

kvp_list *
kvp_value_get_list(const kvp_value * value) {
  if(value->type == KVP_TYPE_LIST) {
    return value->list.value;
  }
  else {
    return NULL;
  }
}

kvp_frame *
kvp_value_get_frame(const kvp_value * value) {
  if(value->type == KVP_TYPE_FRAME) {
    return value->frame.value;
  }
  else {
    return NULL;
  }
}

kvp_value *
kvp_value_copy(const kvp_value * value) {  

  if(!value) return NULL;

  switch(value->type) {
  case KVP_TYPE_INT64:
    return kvp_value_new_int64(value->int64.value);
    break;
  case KVP_TYPE_FLOAT64:
    return kvp_value_new_float64(value->float64.value);
    break;
  case KVP_TYPE_STRING:
    return kvp_value_new_string(value->str.value);
    break;
  case KVP_TYPE_GUID:
    return kvp_value_new_guid(value->guid.value);
    break;
  case KVP_TYPE_BINARY:
    return kvp_value_new_binary(value->binary.value,
                                value->binary.datasize);
    break;
  case KVP_TYPE_LIST:
    return kvp_value_new_list(value->list.value);
    break;
  case KVP_TYPE_FRAME:
    return kvp_value_new_frame(value->frame.value);
    break;
  case KVP_TYPE_NONE:
    return kvp_value_new();
    break;
  }  
  return NULL;
}


/********************************************************************
 * kvp_frame.c -- a key-value frame system for gnucash.             *
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

#include "config.h"

#define _GNU_SOURCE
#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"
#include "guid.h"
#include "kvp_frame.h"


 /* Note that we keep the keys for this hash table in a GCache
  * (gnc_string_cache), as it is likely we will see the same keys
  * over and over again  */
struct _kvp_frame {
  GHashTable  * hash;
};


typedef struct {
  void        *data;
  int         datasize;
} kvp_value_binary_data;

struct _kvp_value {
  kvp_value_t type;
  union {
    gint64 int64;
    double dbl;
    gnc_numeric numeric;
    gchar *str;
    GUID *guid;
    Timespec timespec;
    kvp_value_binary_data binary;
    GList *list;
    kvp_frame *frame;    
  } value;
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

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

static gboolean
init_frame_body_if_needed(kvp_frame *f) {
  if(!f->hash) {
    f->hash = g_hash_table_new(&kvp_hash_func, 
                               &kvp_comp_func);
  }
  return(f->hash != NULL);
}

kvp_frame * 
kvp_frame_new(void) {
  kvp_frame * retval = g_new0(kvp_frame, 1);
  /* save space until we have data */
  retval->hash = NULL;
  return retval;
}

static void
kvp_frame_delete_worker(gpointer key, gpointer value, gpointer user_data) {
  g_cache_remove(gnc_engine_get_string_cache(), key);
  kvp_value_delete((kvp_value *)value);  
}

void
kvp_frame_delete(kvp_frame * frame) {
  if (!frame)
    return;

  if(frame->hash) {
    /* free any allocated resource for frame or its children */
    g_hash_table_foreach(frame->hash, & kvp_frame_delete_worker, 
                         (gpointer)frame);
    
    /* delete the hash table */
    g_hash_table_destroy(frame->hash);
    frame->hash = NULL;
  }
  g_free(frame);
}

gboolean
kvp_frame_is_empty(kvp_frame * frame) 
{
  if (!frame) return TRUE;
  if (!frame->hash) return TRUE;
  return FALSE;
}

static void
kvp_frame_copy_worker(gpointer key, gpointer value, gpointer user_data) {
  kvp_frame * dest = (kvp_frame *)user_data;
  g_hash_table_freeze(dest->hash);
  g_hash_table_insert(dest->hash,
                      (gpointer)g_cache_insert(gnc_engine_get_string_cache(), key), 
                      (gpointer)kvp_value_copy(value));
  g_hash_table_thaw(dest->hash);
}

kvp_frame * 
kvp_frame_copy(const kvp_frame * frame) {
  kvp_frame * retval = kvp_frame_new();

  if (!frame)
    return retval;

  if(frame->hash) {
    if(!init_frame_body_if_needed(retval)) return(NULL);
    g_hash_table_foreach(frame->hash,
                         & kvp_frame_copy_worker, 
                         (gpointer)retval);
  }
  return retval;
}

static void
kvp_frame_set_slot_destructively(kvp_frame * frame, const char * slot, 
                                 kvp_value * new_value) {
  /* FIXME: no way to indicate errors... */

  gpointer orig_key;
  gpointer orig_value;
  int      key_exists;

  if(!new_value && !frame->hash) return; /* don't need to do anything */
  if(!init_frame_body_if_needed(frame)) return;

  g_hash_table_freeze(frame->hash);

  key_exists = g_hash_table_lookup_extended(frame->hash, slot,
                                            & orig_key, & orig_value);
  if(key_exists) {
    g_hash_table_remove(frame->hash, slot);
    g_cache_remove(gnc_engine_get_string_cache(), orig_key);
    kvp_value_delete(orig_value);
  }

  if(new_value) {
    g_hash_table_insert(frame->hash,
                        g_cache_insert(gnc_engine_get_string_cache(),
                                       (gpointer) slot),
      new_value);
  }

  g_hash_table_thaw(frame->hash);
}

void
kvp_frame_set_slot(kvp_frame * frame, const char * slot, 
                   const kvp_value * value) {
  /* FIXME: no way to indicate errors... */

  kvp_value *new_value = NULL;

  if (!frame)
    return;

  if(value) new_value = kvp_value_copy(value);
  kvp_frame_set_slot_destructively(frame, slot, new_value);
}

void
kvp_frame_set_slot_nc(kvp_frame * frame, const char * slot, 
                      kvp_value * value) {
  /* FIXME: no way to indicate errors... */

  if (!frame)
    return;

  kvp_frame_set_slot_destructively(frame, slot, value);
}

kvp_value * 
kvp_frame_get_slot(kvp_frame * frame, const char * slot) {
  if (!frame)
    return NULL;
  if(!frame->hash) return(NULL);
  return (kvp_value *)g_hash_table_lookup(frame->hash, slot);
}

/* ============================================================ */

void
kvp_frame_set_slot_path (kvp_frame *frame,
                         const kvp_value *new_value,
                         const char *first_key, ...) {
  va_list ap;
  const char *key;

  if (!frame || !first_key)
    return;

  va_start (ap, first_key);

  key = first_key;

  while (TRUE) {
    kvp_value *value;
    const char *next_key;

    next_key = va_arg (ap, const char *);
    if (!next_key) {
      kvp_frame_set_slot (frame, key, new_value);
      break;
    }

    value = kvp_frame_get_slot (frame, key);
    if (!value) {
      kvp_frame *new_frame = kvp_frame_new ();
      kvp_value *frame_value = kvp_value_new_frame (new_frame);

      kvp_frame_set_slot_nc (frame, key, frame_value);

      value = kvp_frame_get_slot (frame, key);
      if (!value)
        break;
    }

    frame = kvp_value_get_frame (value);
    if (!frame)
      break;

    key = next_key;
  }

  va_end (ap);
}

void
kvp_frame_set_slot_path_gslist (kvp_frame *frame,
                                const kvp_value *new_value,
                                GSList *key_path) {
  if (!frame || !key_path)
    return;

  while (TRUE) {
    const char *key = key_path->data;
    kvp_value *value;

    if (!key)
      return;

    key_path = key_path->next;
    if (!key_path) {
      kvp_frame_set_slot (frame, key, new_value);
      return;
    }

    value = kvp_frame_get_slot (frame, key);
    if (!value) {
      kvp_frame *new_frame = kvp_frame_new ();
      kvp_value *frame_value = kvp_value_new_frame (new_frame);

      kvp_frame_set_slot_nc (frame, key, frame_value);

      value = kvp_frame_get_slot (frame, key);
      if (!value)
        return;
    }

    frame = kvp_value_get_frame (value);
    if (!frame)
      return;
  }
}

/* ============================================================ */


kvp_frame *
kvp_frame_get_frame_gslist (kvp_frame *frame, GSList *key_path) 
{
  if (!frame || !key_path)
    return frame;

  while (TRUE) {
    const char *key = key_path->data;
    kvp_value *value;

    if (!key)
      return frame;  /* an unusual but valid exit for this routine. */

    /* get the named frame, or create it if it doesn't exist */
    value = kvp_frame_get_slot (frame, key);
    if (!value) {
      kvp_frame *new_frame = kvp_frame_new ();
      kvp_value *frame_value = kvp_value_new_frame (new_frame);

      kvp_frame_set_slot_nc (frame, key, frame_value);

      value = kvp_frame_get_slot (frame, key);
      if (!value)
        return frame; /* this should never happen */
    }

    frame = kvp_value_get_frame (value);
    if (!frame)
      return NULL;  /* this should never happen */

    key_path = key_path->next;
    if (!key_path) {
      return frame;  /* this is the normal exit for this func */
    }
  }
}

static GSList*
charstar_va_list_to_gslist(va_list lst)
{
    const char *val;
    GSList *ret = NULL;

    while(TRUE)
    {
        val = va_arg(lst, const char*);

        if(!val)
        {
            break;
        }

        ret = g_slist_append(ret, (gpointer)val);
    }

    return ret;
}

kvp_frame *
kvp_frame_get_frame (kvp_frame *frame,  ...) 
{
  va_list ap;
  GSList *lst;
  kvp_frame *ret;

  if (!frame)
    return NULL;

  va_start (ap, frame);

  lst = charstar_va_list_to_gslist(ap);
  
  ret = kvp_frame_get_frame_gslist(frame, lst);

  va_end (ap);

  g_slist_free(lst);

  return ret;
}

static GSList*
kvp_frame_parse_slash_path(const char *key_path)
{
  char *root, *key, *next;
  GSList *ret;

  ret = NULL;

  root = g_strdup (key_path);
  key = root;
  key --;

  while (key) {
    key ++;
    while ('/' == *key) { key++; }
    if ('\0' == *key) break;    /* trailing slash */   
    next = strchr (key, '/');
    if (next) *next = '\0';

    ret = g_slist_append(ret, g_strdup(key));

    key = next;
  }

  g_free(root);
  return ret;
}

static void
kvp_frame_sp_free_string(gpointer data, gpointer ignored)
{
    g_free(data);
}

static void
kvp_frame_delete_slash_path_gslist(GSList *lst)
{
    g_slist_foreach(lst, kvp_frame_sp_free_string, NULL);
    g_slist_free(lst);
}

kvp_frame *
kvp_frame_get_frame_slash (kvp_frame *frame, const char *key_path) 
{
  GSList *lst;
  kvp_frame *ret;
  
  if (!frame || !key_path)
    return frame;

  lst = kvp_frame_parse_slash_path(key_path);
  ret = kvp_frame_get_frame_gslist(frame, lst);
  kvp_frame_delete_slash_path_gslist(lst);
  
  return ret;
}

/* ============================================================ */

kvp_value *
kvp_frame_get_slot_path (kvp_frame *frame,
                         const char *first_key, ...) {
  va_list ap;
  kvp_value *value;
  const char *key;

  if (!frame || !first_key)
    return NULL;

  va_start (ap, first_key);

  key = first_key;
  value = NULL;

  while (TRUE) {
    value = kvp_frame_get_slot (frame, key);
    if (!value)
      break;

    key = va_arg (ap, const char *);
    if (!key)
      break;

    frame = kvp_value_get_frame (value);
    if (!frame)
    {
      value = NULL;
      break;
    }
  }

  va_end (ap);

  return value;
}

kvp_value *
kvp_frame_get_slot_path_gslist (kvp_frame *frame,
                                GSList *key_path) {
  if (!frame || !key_path)
    return NULL;

  while (TRUE) {
    const char *key = key_path->data;
    kvp_value *value;

    if (!key)
      return NULL;

    value = kvp_frame_get_slot (frame, key);
    if (!value)
      return NULL;

    key_path = key_path->next;
    if (!key_path)
      return value;

    frame = kvp_value_get_frame (value);
    if (!frame)
      return NULL;
  }
}

/********************************************************************
 * kvp glist functions
 ********************************************************************/

static void
kvp_glist_delete_worker(gpointer datum, gpointer user_data) {
  kvp_value * val = (kvp_value *)datum;
  kvp_value_delete(val);
}

void
kvp_glist_delete(GList * list) {
  if(list) {
    /* delete the data in the list */
    g_list_foreach(list, & kvp_glist_delete_worker, NULL);
    
    /* free the backbone */
    g_list_free(list);
  }
}

GList *
kvp_glist_copy(const GList * list) {
  GList * retval = NULL;
  GList * lptr;

  if(!list) return retval;
  
  /* duplicate the backbone of the list (this duplicates the POINTERS
   * to the values; we need to deep-copy the values separately) */
  retval = g_list_copy((GList *) list);
  
  /* this step deep-copies the values */
  for(lptr = retval; lptr; lptr = lptr->next) {
    lptr->data = kvp_value_copy(lptr->data);
  }
  
  return retval;
}

gint
kvp_glist_compare(const GList * list1, const GList * list2) {
  const GList *lp1;
  const GList *lp2;

  if(list1 == list2) return 0;
  /* nothing is always less than something */
  if(!list1 && list2) return -1;
  if(list1 && !list2) return 1;

  lp1 = list1;
  lp2 = list2;
  while(lp1 && lp2) {
    kvp_value *v1 = (kvp_value *) lp1->data;
    kvp_value *v2 = (kvp_value *) lp2->data;
    gint vcmp = kvp_value_compare(v1, v2);
    if(vcmp != 0) return vcmp;
    lp1 = lp1->next;
    lp2 = lp2->next;
  }
  if(!lp1 && lp2) return -1;
  if(!lp2 && lp1) return 1;
  return 0;
}

/********************************************************************
 * kvp_value functions
 ********************************************************************/

kvp_value *
kvp_value_new_gint64(gint64 value) {
  kvp_value * retval  = g_new0(kvp_value, 1);
  retval->type        = KVP_TYPE_GINT64;
  retval->value.int64 = value;
  return retval;
}  

kvp_value *
kvp_value_new_double(double value) {
  kvp_value * retval  = g_new0(kvp_value, 1);
  retval->type        = KVP_TYPE_DOUBLE;
  retval->value.dbl   = value;
  return retval;
}

kvp_value *
kvp_value_new_gnc_numeric(gnc_numeric value) {
  kvp_value * retval    = g_new0(kvp_value, 1);
  retval->type          = KVP_TYPE_NUMERIC;
  retval->value.numeric = value;
  return retval;
}

kvp_value *
kvp_value_new_string(const char * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_STRING;
  retval->value.str  = g_strdup(value);
  return retval;
}  

kvp_value *
kvp_value_new_guid(const GUID * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_GUID;
  retval->value.guid = g_new0(GUID, 1);
  memcpy(retval->value.guid, value, sizeof(GUID));
  return retval;
}  

kvp_value *
kvp_value_new_timespec(Timespec value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_TIMESPEC;
  retval->value.timespec = value;
  return retval;
}  

kvp_value *
kvp_value_new_binary(const void * value, guint64 datasize) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type = KVP_TYPE_BINARY;
  retval->value.binary.data = g_new0(char, datasize);
  retval->value.binary.datasize = datasize;
  memcpy(retval->value.binary.data, value, datasize);
  return retval;
}

kvp_value *
kvp_value_new_binary_nc(void * value, guint64 datasize) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type = KVP_TYPE_BINARY;
  retval->value.binary.data = value;
  retval->value.binary.datasize = datasize;
  return retval;
}

kvp_value *
kvp_value_new_glist(const GList * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_GLIST;
  retval->value.list = kvp_glist_copy(value);
  return retval;
}  

kvp_value *
kvp_value_new_glist_nc(GList * value) {
  kvp_value * retval = g_new0(kvp_value, 1);
  retval->type       = KVP_TYPE_GLIST;
  retval->value.list = value;
  return retval;
}  

kvp_value *
kvp_value_new_frame(const kvp_frame * value) {
  kvp_value * retval  = g_new0(kvp_value, 1);
  retval->type        = KVP_TYPE_FRAME;
  retval->value.frame = kvp_frame_copy(value);
  return retval;  
}

void
kvp_value_delete(kvp_value * value) {
  if(!value) return;

  switch(value->type) {
  case KVP_TYPE_STRING:
    g_free(value->value.str);
    break;
  case KVP_TYPE_GUID:
    g_free(value->value.guid);
    break;
  case KVP_TYPE_BINARY:
    g_free(value->value.binary.data);
    break;
  case KVP_TYPE_GLIST:
    kvp_glist_delete(value->value.list);
    break;
  case KVP_TYPE_FRAME:
    kvp_frame_delete(value->value.frame);
    break;
    
  case KVP_TYPE_GINT64:    
  case KVP_TYPE_DOUBLE:
  case KVP_TYPE_NUMERIC:
  default:
    break;
  }
  g_free(value);
}

kvp_value_t
kvp_value_get_type(const kvp_value * value) {
  if (!value) return -1;
  return value->type;
}

gint64
kvp_value_get_gint64(const kvp_value * value) {
  if (!value) return 0;
  if(value->type == KVP_TYPE_GINT64) {
    return value->value.int64;
  }
  else {
    return 0;
  }
}

double 
kvp_value_get_double(const kvp_value * value) {
  if (!value) return 0.0;
  if(value->type == KVP_TYPE_DOUBLE) {
    return value->value.dbl;
  }
  else {
    return 0.0;
  }
}

gnc_numeric 
kvp_value_get_numeric(const kvp_value * value) {
  if (!value) return gnc_numeric_zero ();
  if(value->type == KVP_TYPE_NUMERIC) {
    return value->value.numeric;
  }
  else {
    return gnc_numeric_zero ();
  }
}

char *
kvp_value_get_string(const kvp_value * value) {
  if (!value) return NULL;
  if(value->type == KVP_TYPE_STRING) {
    return value->value.str;
  }
  else { 
    return NULL; 
  }
}

GUID *
kvp_value_get_guid(const kvp_value * value) {
  if (!value) return NULL;
  if(value->type == KVP_TYPE_GUID) {
    return value->value.guid;
  }
  else {
    return NULL;
  }
}

Timespec
kvp_value_get_timespec(const kvp_value * value) {
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!value) return ts;
  if (value->type == KVP_TYPE_TIMESPEC)
    return value->value.timespec;
  else
    return ts;
}

void *
kvp_value_get_binary(const kvp_value * value, guint64 * size_return) {
  if (!value)
  {
    if (size_return)
      *size_return = 0;
    return NULL;
  }

  if(value->type == KVP_TYPE_BINARY) {
    if (size_return)
      *size_return = value->value.binary.datasize;
    return value->value.binary.data;
  }
  else {
    if (size_return)
      *size_return = 0;
    return NULL;
  }
}

GList *
kvp_value_get_glist(const kvp_value * value) {
  if (!value) return NULL;
  if(value->type == KVP_TYPE_GLIST) {
    return value->value.list;
  }
  else {
    return NULL;
  }
}

kvp_frame *
kvp_value_get_frame(const kvp_value * value) {
  if (!value) return NULL;
  if(value->type == KVP_TYPE_FRAME) {
    return value->value.frame;
  }
  else {
    return NULL;
  }
}

/* manipulators */

kvp_value *
kvp_value_copy(const kvp_value * value) {  

  if(!value) return NULL;

  switch(value->type) {
  case KVP_TYPE_GINT64:
    return kvp_value_new_gint64(value->value.int64);
    break;
  case KVP_TYPE_DOUBLE:
    return kvp_value_new_double(value->value.dbl);
    break;
  case KVP_TYPE_NUMERIC:
    return kvp_value_new_gnc_numeric(value->value.numeric);
    break;
  case KVP_TYPE_STRING:
    return kvp_value_new_string(value->value.str);
    break;
  case KVP_TYPE_GUID:
    return kvp_value_new_guid(value->value.guid);
    break;
  case KVP_TYPE_TIMESPEC:
    return kvp_value_new_timespec(value->value.timespec);
    break;
  case KVP_TYPE_BINARY:
    return kvp_value_new_binary(value->value.binary.data,
                                value->value.binary.datasize);
    break;
  case KVP_TYPE_GLIST:
    return kvp_value_new_glist(value->value.list);
    break;
  case KVP_TYPE_FRAME:
    return kvp_value_new_frame(value->value.frame);
    break;
  }  
  return NULL;
}

void
kvp_frame_for_each_slot(kvp_frame *f,
                        void (*proc)(const char *key,
                                     kvp_value *value,
                                     gpointer data),
                        gpointer data) {

  if(!f) return;
  if(!proc) return;
  if(!(f->hash)) return;

  g_hash_table_foreach(f->hash, (GHFunc) proc, data);
}

gint
double_compare(double d1, double d2)
{
  if(isnan(d1) && isnan(d2)) return 0;
  if(d1 < d2) return -1;
  if(d1 > d2) return 1;
  return 0;
}

gint
kvp_value_compare(const kvp_value * kva, const kvp_value * kvb) {
  if(kva == kvb) return 0;
  /* nothing is always less than something */
  if(!kva && kvb) return -1;
  if(kva && !kvb) return 1;

  if(kva->type < kvb->type) return -1;
  if(kva->type > kvb->type) return 1;

  switch(kva->type) {
  case KVP_TYPE_GINT64:
    if(kva->value.int64 < kvb->value.int64) return -1;
    if(kva->value.int64 > kvb->value.int64) return 1;
    return 0;
    break;
  case KVP_TYPE_DOUBLE:
    return double_compare(kva->value.dbl, kvb->value.dbl);
    break;
  case KVP_TYPE_NUMERIC:
    return gnc_numeric_compare (kva->value.numeric, kvb->value.numeric);
    break;
  case KVP_TYPE_STRING:
    return strcmp(kva->value.str, kvb->value.str);
    break;
  case KVP_TYPE_GUID:
    return guid_compare(kva->value.guid, kvb->value.guid);
    break;
  case KVP_TYPE_TIMESPEC:
    return timespec_cmp(&(kva->value.timespec), &(kvb->value.timespec));
    break;
  case KVP_TYPE_BINARY:
    /* I don't know that this is a good compare. Ab is bigger than Acef.
       But I'm not sure that actually matters here. */
    if(kva->value.binary.datasize < kvb->value.binary.datasize) return -1;
    if(kva->value.binary.datasize > kvb->value.binary.datasize) return 1;
    return memcmp(kva->value.binary.data,
                  kvb->value.binary.data,
                  kva->value.binary.datasize);
    break;
  case KVP_TYPE_GLIST:
    return kvp_glist_compare(kva->value.list, kvb->value.list);
    break;
  case KVP_TYPE_FRAME:
    return kvp_frame_compare(kva->value.frame, kvb->value.frame);
    break;
  }
  PERR ("reached unreachable code.");
  return FALSE;
}

typedef struct {
  gint compare;
  kvp_frame *other_frame;
} kvp_frame_cmp_status;

static void
kvp_frame_compare_helper(const char *key, kvp_value* val, gpointer data) {
  kvp_frame_cmp_status *status = (kvp_frame_cmp_status *) data;
  if(status->compare == 0) {
    kvp_frame *other_frame = status->other_frame;
    kvp_value *other_val = kvp_frame_get_slot(other_frame, key);
    
    if(other_val) {
      status->compare = kvp_value_compare(val, other_val);
    } else {
      status->compare = 1;
    }
  }
}

gint
kvp_frame_compare(const kvp_frame *fa, const kvp_frame *fb) {
  kvp_frame_cmp_status status;

  if(fa == fb) return 0;
  /* nothing is always less than something */
  if(!fa && fb) return -1;
  if(fa && !fb) return 1;

  /* nothing is always less than something */
  if(!fa->hash && fb->hash) return -1;
  if(fa->hash && !fb->hash) return 1;

  status.compare = 0;
  status.other_frame = (kvp_frame *) fb;

  kvp_frame_for_each_slot((kvp_frame *) fa, kvp_frame_compare_helper, &status);

  if (status.compare != 0)
    return status.compare;

  status.other_frame = (kvp_frame *) fa;

  kvp_frame_for_each_slot((kvp_frame *) fb, kvp_frame_compare_helper, &status);

  return(-status.compare);
}

gchar*
binary_to_string(const void *data, guint32 size)
{
    GString *output;
    guint32 i;
    guchar *data_str = (guchar*)data;
    
    output = g_string_sized_new(size * sizeof(char));
    
    for(i = 0; i < size; i++)
    {
        g_string_sprintfa(output, "%02x", (unsigned int) (data_str[i]));
    }

    return output->str;
}

gchar*
kvp_value_glist_to_string(const GList *list)
{
    gchar *tmp1;
    gchar *tmp2;
    const GList *cursor;

    tmp1 = g_strdup_printf("[ ");

    for(cursor = list; cursor; cursor = cursor->next)
    {
        gchar *tmp3;

        tmp3 = kvp_value_to_string((kvp_value*)cursor->data);
        tmp2 = g_strdup_printf("%s %s,", tmp1, tmp3 ? tmp3 : "");
        g_free(tmp1);
        g_free(tmp3);
        tmp1 = tmp2;
    }

    tmp2 = g_strdup_printf("%s ]", tmp1);
    g_free(tmp1);
    
    return tmp2;
}

gchar*
kvp_value_to_string(const kvp_value *val)
{
    gchar *tmp1;
    gchar *tmp2;
    
    g_return_val_if_fail(val, NULL);
    
    switch(kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        return g_strdup_printf("KVP_VALUE_GINT64(%lld)",
                               (long long int) kvp_value_get_gint64(val));
        break;

    case KVP_TYPE_DOUBLE:
        return g_strdup_printf("KVP_VALUE_DOUBLE(%g)",
                               kvp_value_get_double(val));
        break;

    case KVP_TYPE_NUMERIC:
        tmp1 = gnc_numeric_to_string(kvp_value_get_numeric(val));
        tmp2 = g_strdup_printf("KVP_VALUE_NUMERIC(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_STRING:
        tmp1 = kvp_value_get_string (val);
        return g_strdup_printf("KVP_VALUE_STRING(%s)", tmp1 ? tmp1 : "");
        break;

    case KVP_TYPE_GUID:
        tmp1 = guid_to_string(kvp_value_get_guid(val));
        tmp2 = g_strdup_printf("KVP_VALUE_GUID(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_TIMESPEC:
        tmp1 = g_new0 (char, 40);
	gnc_timespec_to_iso8601_buff (kvp_value_get_timespec (val), tmp1);
	tmp2 = g_strdup_printf("KVP_VALUE_TIMESPEC(%s)", tmp1);
	g_free(tmp1);
	return tmp2;
	break;

    case KVP_TYPE_BINARY:
    {
        guint64 len;
        void *data;
        data = kvp_value_get_binary(val, &len);
        tmp1 = binary_to_string(data, len);
        return g_strdup_printf("KVP_VALUE_BINARY(%s)", tmp1 ? tmp1 : "");
    }
        break;
 
    case KVP_TYPE_GLIST:
        tmp1 = kvp_value_glist_to_string(kvp_value_get_glist(val));
        tmp2 = g_strdup_printf("KVP_VALUE_GLIST(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    case KVP_TYPE_FRAME:
        tmp1 = kvp_frame_to_string(kvp_value_get_frame(val));
        tmp2 = g_strdup_printf("KVP_VALUE_FRAME(%s)", tmp1 ? tmp1 : "");
        g_free(tmp1);
        return tmp2;
        break;

    default:
        return g_strdup_printf(" ");
        break;
    }
}

static void
kvp_frame_to_string_helper(gpointer key, gpointer value, gpointer data)
{
    gchar *tmp_val;
    gchar **str = (gchar**)data;
    gchar *old_data = *str;

    tmp_val = kvp_value_to_string((kvp_value*)value);

    *str = g_strdup_printf("%s    %s => %s,\n",
                           *str ? *str : "",
                           key ? (char *) key : "",
                           tmp_val ? tmp_val : "");

    g_free(old_data);
    g_free(tmp_val);
}

gchar*
kvp_frame_to_string(const kvp_frame *frame)
{
    gchar *tmp1;

    g_return_val_if_fail (frame != NULL, NULL);

    tmp1 = g_strdup_printf("{\n");

    if (frame->hash)
      g_hash_table_foreach(frame->hash, kvp_frame_to_string_helper, &tmp1);

    {
        gchar *tmp2;
        tmp2 = g_strdup_printf("%s}\n", tmp1);
        g_free(tmp1);
        tmp1 = tmp2;
    }

    return tmp1;
}

GHashTable*
kvp_frame_get_hash(const kvp_frame *frame)
{
    g_return_val_if_fail (frame != NULL, NULL);
    return frame->hash;
}

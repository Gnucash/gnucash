/*
 * gncObject.c -- the Core Object Object Registry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-engine-util.h"

#include "gncObjectP.h"

static gboolean object_is_initialized = FALSE;
static GList *object_modules = NULL;
static GList *book_list = NULL;
static GHashTable *backend_data = NULL;

void gncObjectBookBegin (GNCBook *book)
{
  GList *l;

  if (!book) return;
  for (l = object_modules; l; l = l->next) {
    GncObject_t *obj = l->data;
    if (obj->book_begin)
      obj->book_begin (book);
  }

  /* Remember this book for later */
  book_list = g_list_prepend (book_list, book);
}

void gncObjectBookEnd (GNCBook *book)
{
  GList *l;

  if (!book) return;
  for (l = object_modules; l; l = l->next) {
    GncObject_t *obj = l->data;
    if (obj->book_end)
      obj->book_end (book);
  }

  /* Remove it from the list */
  book_list = g_list_remove (book_list, book);
}

gboolean gncObjectIsDirty (GNCBook *book)
{
  GList *l;

  if (!book) return FALSE;
  for (l = object_modules; l; l = l->next) {
    GncObject_t *obj = l->data;
    if (obj->is_dirty)
      if (obj->is_dirty (book))
	return TRUE;
  }
  return FALSE;
}

void gncObjectMarkClean (GNCBook *book)
{
  GList *l;

  if (!book) return;
  for (l = object_modules; l; l = l->next) {
    GncObject_t *obj = l->data;
    if (obj->mark_clean)
      (obj->mark_clean) (book);
  }
}

void gncObjectForeachType (foreachTypeCB cb, gpointer user_data)
{
  GList *l;

  if (!cb) return;

  for (l = object_modules; l; l = l->next) {
    GncObject_t *obj = l->data;
    (cb) (obj, user_data);
  }
}

void gncObjectForeach (GNCIdTypeConst type_name, GNCBook *book, 
		       foreachObjectCB cb, gpointer user_data)
{
  const GncObject_t *obj;

  if (!book || !type_name) return;

  obj = gncObjectLookup (type_name);
  if (!obj) return;

  if (obj->foreach)
    return (obj->foreach (book, cb, user_data));

  return;
}

const char *
gncObjectPrintable (GNCIdTypeConst type_name, gpointer obj)
{
  const GncObject_t *b_obj;

  if (!type_name || !obj) return NULL;

  b_obj = gncObjectLookup (type_name);
  if (!b_obj) return NULL;

  if (b_obj->printable)
    return (b_obj->printable (obj));

  return NULL;	    
}

const char * gncObjectGetTypeLabel (GNCIdTypeConst type_name)
{
  const GncObject_t *obj;

  if (!type_name) return NULL;

  obj = gncObjectLookup (type_name);
  if (!obj) return NULL;

  return _(obj->type_label);
}

static gboolean clear_table (gpointer key, gpointer value, gpointer user_data)
{
  g_hash_table_destroy (value);
  return TRUE;
}

/* INITIALIZATION and PRIVATE FUNCTIONS */

void gncObjectInitialize (void)
{
  if (object_is_initialized) return;
  backend_data = g_hash_table_new (g_str_hash, g_str_equal);
  object_is_initialized = TRUE;
}

void gncObjectShutdown (void)
{
  g_return_if_fail (object_is_initialized == TRUE);

  g_hash_table_foreach_remove (backend_data, clear_table, NULL);
  g_hash_table_destroy (backend_data);
  backend_data = NULL;

  g_list_free (object_modules);
  object_modules = NULL;
  g_list_free (book_list);
  book_list = NULL;
  object_is_initialized = FALSE;
}

/* Register new types of object objects.
 * Return TRUE if successful,
 * return FALSE if it fails, invalid arguments, or if the object
 * already exists
 */
gboolean gncObjectRegister (const GncObject_t *object)
{
  g_return_val_if_fail (object_is_initialized, FALSE);

  if (!object) return FALSE;
  g_return_val_if_fail (object->interface_version == GNC_OBJECT_VERSION, FALSE);

  if (g_list_index (object_modules, (gpointer)object) == -1)
    object_modules = g_list_prepend (object_modules, (gpointer)object);
  else
    return FALSE;

  /* Now initialize all the known books */
  if (object->book_begin && book_list) {
    GList *node;
    for (node = book_list; node; node = node->next)
      object->book_begin (node->data);
  }

  return TRUE;
}

const GncObject_t * gncObjectLookup (GNCIdTypeConst name)
{
  GList *iter;
  const GncObject_t *obj;

  g_return_val_if_fail (object_is_initialized, NULL);

  if (!name) return NULL;

  for (iter = object_modules; iter; iter = iter->next) {
    obj = iter->data;
    if (!safe_strcmp (obj->name, name))
      return obj;
  }
  return NULL;
}

gboolean gncObjectRegisterBackend (GNCIdTypeConst type_name,
				   const char *backend_name,
				   gpointer be_data)
{
  GHashTable *ht;
  g_return_val_if_fail (object_is_initialized, FALSE);

  if (!type_name || *type_name == '\0' ||
      !backend_name || *backend_name == '\0' ||
      !be_data)
    return FALSE;

  ht = g_hash_table_lookup (backend_data, backend_name);

  /* If it doesn't already exist, create a new table for this backend */
  if (!ht) {
    ht = g_hash_table_new (g_str_hash, g_str_equal);
    g_hash_table_insert (backend_data, (char *)backend_name, ht);
  }

  /* Now insert the data */
  g_hash_table_insert (ht, (char *)type_name, be_data);

  return TRUE;
}

gpointer gncObjectLookupBackend (GNCIdTypeConst type_name,
				 const char *backend_name)
{
  GHashTable *ht;

  if (!type_name || *type_name == '\0' ||
      !backend_name || *backend_name == '\0')
    return NULL;

  ht = g_hash_table_lookup (backend_data, (char *)backend_name);
  if (!ht)
    return NULL;

  return g_hash_table_lookup (ht, (char *)type_name);
}

struct foreach_data {
  foreachBackendTypeCB	cb;
  gpointer 		user_data;
};

static void foreach_backend (gpointer key, gpointer be_item, gpointer arg)
{
  char *data_type = key;
  struct foreach_data *cb_data = arg;

  g_return_if_fail (key && be_item && arg);

  /* Call the callback for this data type */
  (cb_data->cb) (data_type, be_item, cb_data->user_data);
}

void gncObjectForeachBackend (const char *backend_name,
			      foreachBackendTypeCB cb,
			      gpointer user_data)
{
  GHashTable *ht;
  struct foreach_data cb_data;

  if (!backend_name || *backend_name == '\0' || !cb)
    return;

  ht = g_hash_table_lookup (backend_data, (char *)backend_name);
  if (!ht)
    return;

  cb_data.cb = cb;
  cb_data.user_data = user_data;

  g_hash_table_foreach (ht, foreach_backend, &cb_data);
}

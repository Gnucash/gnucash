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

/* INITIALIZATION and PRIVATE FUNCTIONS */

void gncObjectInitialize (void)
{
  if (object_is_initialized) return;
  object_is_initialized = TRUE;
}

void gncObjectShutdown (void)
{
  g_return_if_fail (object_is_initialized == TRUE);
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
  if (!object) return FALSE;
  if (object->version != GNC_OBJECT_VERSION) return FALSE;
  if (!object_is_initialized) return FALSE;

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

/*
 * gncBusiness.c -- the Core Business Object Registry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "messages.h"

#include "gncBusiness.h"
#include "gncBusinessP.h"

static gboolean business_is_initialized = FALSE;
static GList *business_modules = NULL;

void gncBusinessCreateBook (GNCBook *book)
{
  GList *l;

  if (!book) return;
  for (l = business_modules; l; l = l->next) {
    GncBusinessObject *obj = l->data;
    if (obj->create)
      obj->create (book);
  }
}

void gncBusinessDestroyBook (GNCBook *book)
{
  GList *l;

  if (!book) return;
  for (l = business_modules; l; l = l->next) {
    GncBusinessObject *obj = l->data;
    if (obj->destroy)
      obj->destroy (book);
  }
}

GList *
gncBusinessGetList (GNCBook *book, const char *type_name,
		    gboolean show_all)
{
  const GncBusinessObject *obj;

  if (!book || !type_name) return NULL;

  obj = gncBusinessLookup (type_name);
  if (!obj) return NULL;

  if (obj->get_list)
    return ((*(obj->get_list))(book, show_all));

  return NULL;	    
}

const char *
gncBusinessPrintable (const char *type_name, gpointer obj)
{
  const GncBusinessObject *b_obj;

  if (!type_name || !obj) return NULL;

  b_obj = gncBusinessLookup (type_name);
  if (!b_obj) return NULL;

  if (b_obj->printable)
    return ((*(b_obj->printable))(obj));

  return NULL;	    
}

const char * gncBusinessGetTypeLabel (const char *type_name)
{
  const GncBusinessObject *obj;

  if (!type_name) return NULL;

  obj = gncBusinessLookup (type_name);
  if (!obj) return NULL;

  return _(obj->type_label);
}

/* INITIALIZATION and PRIVATE FUNCTIONS */

void
gncBusinessInitialize (int argc, char **argv)
{
  if (business_is_initialized) return;
  business_is_initialized = TRUE;
}

/* Register new types of business objects.
 * Return TRUE if successful,
 * return FALSE if it fails, invalid arguments, or if the object
 * already exists
 */
gboolean gncBusinessRegister (const GncBusinessObject *object)
{
  if (!object) return FALSE;
  if (object->version != GNC_BUSINESS_VERSION) return FALSE;
  if (!business_is_initialized) return FALSE;

  if (g_list_index (business_modules, (gpointer)object) == -1)
    business_modules = g_list_append (business_modules, (gpointer)object);
  else
    return FALSE;

  return TRUE;
}

const GncBusinessObject * gncBusinessLookup (const char *name)
{
  GList *iter;
  const GncBusinessObject *obj;

  if (!name) return NULL;

  for (iter = business_modules; iter; iter = iter->next) {
    obj = iter->data;
    if (!strcmp (obj->name, name))
      return obj;
  }
  return NULL;
}

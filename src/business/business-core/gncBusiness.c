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

struct _gncBusiness {
  GHashTable *	objects;
  GNCSession *	session;
};

GncBusiness *gncBusinessCreate (GNCSession *session)
{
  GncBusiness *bus;
  GList *iter;

  if (!session) return NULL;

  bus = g_new0 (GncBusiness, 1);
  bus->objects = g_hash_table_new (g_str_hash, g_str_equal);
  bus->session = session;

  /* Populate the objects hash table with hash tables for
   * each of the business objects.
   */
  for (iter = business_modules; iter; iter = iter->next) {
    const GncBusinessObject *obj = iter->data;
    g_hash_table_insert (bus->objects, (gpointer)obj->name,
			 guid_hash_table_new ());
  }

  return bus;
}

void gncBusinessDestroy (GncBusiness *bus)
{
  if (!bus) return;

  /* XXX: destroy the objects under us... */
  g_hash_table_destroy (bus->objects);
  g_free (bus);
}

GNCSession * gncBusinessGetSession (const GncBusiness *bus)
{
  if (!bus) return NULL;

  return bus->session;
}

gpointer
gncBusinessLookupGUID (GncBusiness *business, const char *type_name,
		       const GUID * guid)
{
  GHashTable *table;

  if (!business || !type_name || !guid) return NULL;

  table = gncBusinessEntityTable (business, type_name);
  if (!table) return NULL;

  return g_hash_table_lookup (table, guid);
}

GList *
gncBusinessGetList (GncBusiness *business, const char *type_name,
		    gboolean show_all)
{
  const GncBusinessObject *obj;

  if (!business || !type_name) return NULL;

  obj = gncBusinessLookup (type_name);
  if (!obj) return NULL;

  if (obj->get_list)
    return ((*(obj->get_list))(business, show_all));

  return NULL;	    
}

const char *
gncBusinessPrintable (GncBusiness *business, const char *type_name,
		      gpointer obj)
{
  const GncBusinessObject *b_obj;

  if (!business || !type_name || !obj) return NULL;

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

GHashTable * gncBusinessEntityTable (GncBusiness *bus, const char *name)
{
  if (!bus || !name) return NULL;
  return (g_hash_table_lookup (bus->objects, name));
}

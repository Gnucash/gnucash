/*
 * QueryObject.c -- provide Gnucash Queriable data objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#include "config.h"

#include <sys/types.h>
#include <time.h>
#include <glib.h>
#include <regex.h>
#include <string.h>

#include "gnc-engine-util.h"
#include "QueryObjectP.h"
#include "QueryNew.h"

static short module = MOD_QUERY;

static GHashTable *paramTable = NULL;
static GHashTable *sortTable = NULL;
static gboolean initialized = FALSE;

static gboolean clear_table (gpointer key, gpointer value, gpointer user_data)
{
  g_hash_table_destroy (value);
  return TRUE;
}

/********************************************************************/
/* PUBLISHED API FUNCTIONS */

void gncQueryObjectRegister (GNCIdTypeConst obj_name,
			     QuerySort default_sort_function,
			     const QueryObjectDef *params)
{
  int i;

  if (!obj_name) return;

  if (default_sort_function)
    g_hash_table_insert (sortTable, (char *)obj_name, default_sort_function);

  if (params) {
    GHashTable *ht = g_hash_table_lookup (paramTable, obj_name);

    /* If it doesn't already exist, create a new table for this object */
    if (!ht) {
      ht = g_hash_table_new (g_str_hash, g_str_equal);
      g_hash_table_insert (paramTable, (char *)obj_name, ht);
    }

    /* Now insert all the parameters */
    for (i = 0; params[i].param_name; i++)
      g_hash_table_insert (ht,
			   (char *)params[i].param_name,
			   (gpointer)&(params[i]));
  }
}

void gncQueryObjectInit(void)
{
  if (initialized) return;
  initialized = TRUE;

  paramTable = g_hash_table_new (g_str_hash, g_str_equal);
  sortTable = g_hash_table_new (g_str_hash, g_str_equal);
}

void gncQueryObjectShutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_foreach_remove (paramTable, clear_table, NULL);
  g_hash_table_destroy (paramTable);
  g_hash_table_destroy (sortTable);
}


const QueryObjectDef * gncQueryObjectGetParameter (GNCIdTypeConst obj_name,
						   const char *parameter)
{
  GHashTable *ht;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  ht = g_hash_table_lookup (paramTable, obj_name);
  g_return_val_if_fail (ht, NULL);

  return (g_hash_table_lookup (ht, parameter));
}

QueryAccess gncQueryObjectGetParameterGetter (GNCIdTypeConst obj_name,
					      const char *parameter)
{
  const QueryObjectDef *obj;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  obj = gncQueryObjectGetParameter (obj_name, parameter);
  if (obj)
    return obj->param_getfcn;

  return NULL;
}

QueryCoreType gncQueryObjectParameterType (GNCIdTypeConst obj_name,
					   const char *param_name)
{
  const QueryObjectDef *obj;

  if (!obj_name || !param_name) return NULL;

  obj = gncQueryObjectGetParameter (obj_name, param_name);
  if (!obj) return NULL;

  return (obj->param_type);
}

QuerySort gncQueryObjectDefaultSort (GNCIdTypeConst obj_name)
{
  if (!obj_name) return NULL;
  return g_hash_table_lookup (sortTable, obj_name);
}

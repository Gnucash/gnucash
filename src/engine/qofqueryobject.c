/********************************************************************\
 * QueryObject.c -- provide QOF Queriable data objects              *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "config.h"

#include <glib.h>

#include "gnc-engine-util.h"
#include "gnc-trace.h"
#include "qofqueryobject-p.h"
#include "qofquery.h"

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

void qof_class_register (QofIdTypeConst obj_name,
                 QofSortFunc default_sort_function,
                 const QofParam *params)
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

void qof_class_init(void)
{
  if (initialized) return;
  initialized = TRUE;

  paramTable = g_hash_table_new (g_str_hash, g_str_equal);
  sortTable = g_hash_table_new (g_str_hash, g_str_equal);
}

void qof_class_shutdown (void)
{
  if (!initialized) return;
  initialized = FALSE;

  g_hash_table_foreach_remove (paramTable, clear_table, NULL);
  g_hash_table_destroy (paramTable);
  g_hash_table_destroy (sortTable);
}


const QofParam * 
qof_class_get_parameter (QofIdTypeConst obj_name,
                          const char *parameter)
{
  GHashTable *ht;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  ht = g_hash_table_lookup (paramTable, obj_name);
  if (!ht)
    PERR ("no object type %s", obj_name);
  g_return_val_if_fail (ht, NULL);

  return (g_hash_table_lookup (ht, parameter));
}

QofAccessFunc 
qof_class_get_parameter_getter (QofIdTypeConst obj_name,
                                 const char *parameter)
{
  const QofParam *prm;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  prm = qof_class_get_parameter (obj_name, parameter);
  if (prm)
    return prm->param_getfcn;

  return NULL;
}

QofSetterFunc 
qof_class_get_parameter_setter (QofIdTypeConst obj_name,
                                 const char *parameter)
{
  const QofParam *prm;

  g_return_val_if_fail (obj_name, NULL);
  g_return_val_if_fail (parameter, NULL);

  prm = qof_class_get_parameter (obj_name, parameter);
  if (prm)
    return prm->param_setfcn;

  return NULL;
}

QofType 
qof_class_get_parameter_type (QofIdTypeConst obj_name,
                               const char *param_name)
{
  const QofParam *prm;

  if (!obj_name || !param_name) return NULL;

  prm = qof_class_get_parameter (obj_name, param_name);
  if (!prm) return NULL;

  return (prm->param_type);
}

QofSortFunc 
qof_class_get_default_sort (QofIdTypeConst obj_name)
{
  if (!obj_name) return NULL;
  return g_hash_table_lookup (sortTable, obj_name);
}

/* ============================= END OF FILE ======================== */

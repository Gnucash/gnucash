/********************************************************************\
 * qofid.c -- QOF entity identifier implementation                  *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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

#include <string.h>
#include <glib.h>

#include "qofid.h"
#include "qofid-p.h"
#include "gnc-engine-util.h"

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (gpointer)(str));

/** #defines ********************************************************/
#define QOFID_DEBUG 0


/** Type definitions ************************************************/
typedef struct entity_node
{
  QofIdType entity_type;
  gpointer entity;
} EntityNode;

struct _QofEntityTable
{
  GHashTable * hash_by_guid;
  GHashTable * hash_of_types;
};


/** Static global variables *****************************************/
static short module = MOD_ENGINE;


/** Function implementations ****************************************/

static gboolean
entity_node_destroy(gpointer key, gpointer value, gpointer not_used)
{
  GUID *guid = key;
  EntityNode *e_node = value;

  CACHE_REMOVE (e_node->entity_type);
  e_node->entity_type = QOF_ID_NONE;
  e_node->entity = NULL;

  guid_free(guid);
  g_free(e_node);

  return TRUE;
}

static gboolean
entity_types_table_destroy(gpointer key, gpointer value, gpointer not_used)
{
  GHashTable *ht = value;
  g_hash_table_destroy(ht);
  return TRUE;
}

void
qof_entity_destroy (QofEntityTable *entity_table)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach_remove (entity_table->hash_by_guid, entity_node_destroy,
			       NULL);
  g_hash_table_destroy (entity_table->hash_by_guid);
  entity_table->hash_by_guid = NULL;

  g_hash_table_foreach_remove (entity_table->hash_of_types,
			       entity_types_table_destroy, NULL);
  g_hash_table_destroy (entity_table->hash_of_types);
  entity_table->hash_of_types = NULL;

  g_free (entity_table);
}

static guint
id_hash (gconstpointer key)
{
  const GUID *guid = key;

  if (key == NULL)
    return 0;

  if (sizeof(guint) <= 16)
    return *((guint *) guid->data);
  else
  {
    guint hash = 0;
    unsigned int i, j;

    for (i = 0, j = 0; i < sizeof(guint); i++, j++)
    {
      if (j == 16)
        j = 0;

      hash <<= 4;
      hash |= guid->data[j];
    }

    return hash;
  }
}

static gboolean
id_compare(gconstpointer key_1, gconstpointer key_2)
{
  return guid_equal (key_1, key_2);
}

#if QOFID_DEBUG
static void
print_node(gpointer key, gpointer value, gpointer not_used)
{
  GUID *guid = key;
  EntityNode *node = value;

  fprintf(stderr, "%s %s %p\n",
          guid_to_string(guid), node->entity_type, node->entity);
}

static void
summarize_table (QofEntityTable *entity_table)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach (entity_table->hash_by_guid, print_node, NULL);
}
#endif /* QOFID_DEBUG */

QofEntityTable *
qof_entity_new (void)
{
  QofEntityTable *entity_table;

  entity_table = g_new0 (QofEntityTable, 1);

  entity_table->hash_by_guid = g_hash_table_new (id_hash, id_compare);
  entity_table->hash_of_types = g_hash_table_new (g_str_hash, g_str_equal);

  qof_entity_store (entity_table, NULL, guid_null(), QOF_ID_NULL);

  return entity_table;
}

QofIdType 
qof_entity_type (QofEntityTable *entity_table, const GUID * guid)
{
  EntityNode *e_node;

  if (guid == NULL)
    return QOF_ID_NONE;

  g_return_val_if_fail (entity_table, QOF_ID_NONE);

  e_node = g_hash_table_lookup (entity_table->hash_by_guid, guid->data);
  if (e_node == NULL)
    return QOF_ID_NONE;

  return e_node->entity_type;
}

void
qof_entity_guid_new (QofEntityTable *entity_table, GUID *guid)
{
  if (guid == NULL)
    return;

  g_return_if_fail (entity_table);

  do
  {
    guid_new(guid);

    if (qof_entity_type (entity_table, guid) == QOF_ID_NONE)
      break;

    PWARN("duplicate id created, trying again");
  } while(1);
}

gpointer
qof_entity_lookup (QofEntityTable *entity_table,
                  const GUID * guid, QofIdType entity_type)
{
  EntityNode *e_node;

  g_return_val_if_fail (entity_table, NULL);

  if (guid == NULL)
    return NULL;

  e_node = g_hash_table_lookup (entity_table->hash_by_guid, guid->data);
  if (e_node == NULL)
    return NULL;

  if (safe_strcmp (e_node->entity_type, entity_type))
    return NULL;

  return e_node->entity;
}

static GHashTable *
entity_get_types_table (QofEntityTable *entity_table, QofIdType entity_type)
{
  GHashTable *ht;

  ht = g_hash_table_lookup (entity_table->hash_of_types, entity_type);
  if (ht)
    return ht;

  ht = g_hash_table_new (id_hash, id_compare);
  g_assert(ht);
  g_hash_table_insert (entity_table->hash_of_types, (gpointer)entity_type, ht);
  return ht;
}

static void
entity_store_by_type (QofEntityTable *entity_table,
		      GUID *new_guid, EntityNode *e_node)
{
  GHashTable *ht;

  ht = entity_get_types_table (entity_table, e_node->entity_type);
  g_hash_table_insert (ht, new_guid, e_node);
}

void
qof_entity_store (QofEntityTable *entity_table, gpointer entity,
                 const GUID * guid, QofIdType entity_type)
{
  EntityNode *e_node;
  GUID *new_guid;

  g_return_if_fail (entity_table);

  if (guid == NULL)
    return;

  if (!entity_type) return;

  if (guid_equal(guid, guid_null())) return;

  qof_entity_remove (entity_table, guid);

  e_node = g_new(EntityNode, 1);
  e_node->entity_type = CACHE_INSERT (entity_type);
  e_node->entity = entity;

  new_guid = guid_malloc ();

  if (!new_guid) return;
  
  *new_guid = *guid;

  g_hash_table_insert (entity_table->hash_by_guid, new_guid, e_node);
  entity_store_by_type (entity_table, new_guid, e_node);
}

static void
entity_remove_by_type (QofEntityTable *entity_table, GUID *guid, QofIdType type)
{
  GHashTable *ht;

  ht = entity_get_types_table (entity_table, type);
  g_hash_table_remove (ht, guid);
}

void
qof_entity_remove (QofEntityTable *entity_table, const GUID * guid)
{
  EntityNode *e_node;
  gpointer old_guid;
  gpointer node;

  g_return_if_fail (entity_table);

  if (guid == NULL)
    return;

  if (g_hash_table_lookup_extended(entity_table->hash_by_guid, guid, &old_guid, &node))
  {
    e_node = node;

    if (!safe_strcmp (e_node->entity_type, QOF_ID_NULL))
      return;

    g_hash_table_remove (entity_table->hash_by_guid, old_guid);
    entity_remove_by_type (entity_table, old_guid, e_node->entity_type);
    entity_node_destroy (old_guid, node, NULL);
  }
}

struct _iterate {
  QofEntityForeachCB	fcn;
  gpointer		data;
  QofIdType		type;
};

static void foreach_cb (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  EntityNode *e_node = item;

  iter->fcn (e_node->entity, iter->data);
}

void
qof_entity_foreach (QofEntityTable *entity_table, QofIdType type,
		   QofEntityForeachCB cb_func, gpointer user_data)
{
  GHashTable *ht;
  struct _iterate iter;

  g_return_if_fail (entity_table);
  g_return_if_fail (type);
  g_return_if_fail (*type);
  g_return_if_fail (cb_func);

  iter.fcn = cb_func;
  iter.data = user_data;
  iter.type = type;

  /* Iterate over the objects of the particular type */
  ht = entity_get_types_table (entity_table, type);
  g_hash_table_foreach (ht, foreach_cb, &iter);
}

/********************************************************************\
 * GNCId.c -- Gnucash entity identifier implementation              *
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

#include "GNCIdP.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (gpointer)(str));

/** #defines ********************************************************/
#define GNCID_DEBUG 0


/** Type definitions ************************************************/
typedef struct entity_node
{
  GNCIdType entity_type;
  gpointer entity;
} EntityNode;

struct gnc_entity_table
{
  GHashTable * hash;
};


/** Static global variables *****************************************/
static GMemChunk *guid_memchunk = NULL;
static short module = MOD_ENGINE;


/** Function implementations ****************************************/

void
xaccGUIDInit (void)
{
  if (!guid_memchunk)
    guid_memchunk = g_mem_chunk_create (GUID, 512, G_ALLOC_AND_FREE);
}

void
xaccGUIDShutdown (void)
{
  if (guid_memchunk)
  {
    g_mem_chunk_destroy (guid_memchunk);
    guid_memchunk = NULL;
  }
}

GUID *
xaccGUIDMalloc (void)
{
  return g_chunk_new (GUID, guid_memchunk);
}

void
xaccGUIDFree (GUID *guid)
{
  if (!guid)
    return;

  g_chunk_free (guid, guid_memchunk);
}

static gboolean
entity_node_destroy(gpointer key, gpointer value, gpointer not_used)
{
  GUID *guid = key;
  EntityNode *e_node = value;

  CACHE_REMOVE (e_node->entity_type);
  e_node->entity_type = GNC_ID_NONE;
  e_node->entity = NULL;

  xaccGUIDFree(guid);
  g_free(e_node);

  return TRUE;
}

void
xaccEntityTableDestroy (GNCEntityTable *entity_table)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach_remove (entity_table->hash, entity_node_destroy, NULL);
  g_hash_table_destroy (entity_table->hash);
  entity_table->hash = NULL;

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
    int i, j;

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

#if GNCID_DEBUG
static void
print_node(gpointer key, gpointer value, gpointer not_used)
{
  GUID *guid = key;
  EntityNode *node = value;

  fprintf(stderr, "%s %s %p\n",
          guid_to_string(guid), node->entity_type, node->entity);
}

static void
summarize_table (GNCEntityTable *entity_table)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach (entity_table->hash, print_node, NULL);
}
#endif

GNCEntityTable *
xaccEntityTableNew (void)
{
  GNCEntityTable *entity_table;

  entity_table = g_new0 (GNCEntityTable, 1);

  entity_table->hash = g_hash_table_new (id_hash, id_compare);

  xaccStoreEntity (entity_table, NULL, xaccGUIDNULL(), GNC_ID_NULL);

  return entity_table;
}

GNCIdType
xaccGUIDTypeEntityTable (const GUID * guid, GNCEntityTable *entity_table)
{
  EntityNode *e_node;

  if (guid == NULL)
    return GNC_ID_NONE;

  g_return_val_if_fail (entity_table, GNC_ID_NONE);

  e_node = g_hash_table_lookup (entity_table->hash, guid->data);
  if (e_node == NULL)
    return GNC_ID_NONE;

  return e_node->entity_type;
}

GNCIdType
xaccGUIDType (const GUID * guid, GNCBook *book)
{
  return xaccGUIDTypeEntityTable (guid,
                                  gnc_book_get_entity_table (book));
}

void
xaccGUIDNewEntityTable (GUID *guid, GNCEntityTable *entity_table)
{
  if (guid == NULL)
    return;

  g_return_if_fail (entity_table);

  do
  {
    guid_new(guid);

    if (xaccGUIDTypeEntityTable (guid, entity_table) == GNC_ID_NONE)
      break;

    PWARN("duplicate id created, trying again");
  } while(1);
}

void
xaccGUIDNew (GUID *guid, GNCBook *book)
{
  xaccGUIDNewEntityTable (guid, gnc_book_get_entity_table (book));
}

const GUID *
xaccGUIDNULL(void)
{
  static int null_inited = (0 == 1);
  static GUID null_guid;

  if (!null_inited)
  {
    int i;

    for (i = 0; i < 16; i++)
      null_guid.data[i] = 0;

    null_inited = (0 == 0);
  }

  return &null_guid;
}

gpointer
xaccLookupEntity (GNCEntityTable *entity_table,
                  const GUID * guid, GNCIdType entity_type)
{
  EntityNode *e_node;

  g_return_val_if_fail (entity_table, NULL);

  if (guid == NULL)
    return NULL;

  e_node = g_hash_table_lookup (entity_table->hash, guid->data);
  if (e_node == NULL)
    return NULL;

  if (safe_strcmp (e_node->entity_type, entity_type))
    return NULL;

  return e_node->entity;
}

void
xaccStoreEntity (GNCEntityTable *entity_table, gpointer entity,
                 const GUID * guid, GNCIdType entity_type)
{
  EntityNode *e_node;
  GUID *new_guid;

  g_return_if_fail (entity_table);

  if (guid == NULL)
    return;

  if (!entity_type) return;

  if (guid_equal(guid, xaccGUIDNULL())) return;

  xaccRemoveEntity (entity_table, guid);

  e_node = g_new(EntityNode, 1);
  e_node->entity_type = CACHE_INSERT (entity_type);
  e_node->entity = entity;

  new_guid = xaccGUIDMalloc ();

  if (!new_guid) return;
  
  *new_guid = *guid;

  g_hash_table_insert (entity_table->hash, new_guid, e_node);
}

void
xaccRemoveEntity (GNCEntityTable *entity_table, const GUID * guid)
{
  EntityNode *e_node;
  gpointer old_guid;
  gpointer node;

  g_return_if_fail (entity_table);

  if (guid == NULL)
    return;

  if (g_hash_table_lookup_extended(entity_table->hash, guid, &old_guid, &node))
  {
    e_node = node;

    if (!safe_strcmp (e_node->entity_type, GNC_ID_NULL))
      return;

    g_hash_table_remove (entity_table->hash, old_guid);
    entity_node_destroy (old_guid, node, NULL);
  }
}

struct _iterate {
  foreachObjectCB	fcn;
  gpointer		data;
  GNCIdType		type;
};

static void foreach_cb (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  EntityNode *e_node = item;

  /* Call the callback if this entity is of the proper type */
  if (!safe_strcmp (e_node->entity_type, iter->type))
    iter->fcn (e_node->entity, iter->data);
}

void
xaccForeachEntity (GNCEntityTable *entity_table, GNCIdType type,
		   foreachObjectCB cb_func, gpointer user_data)
{
  struct _iterate iter;

  g_return_if_fail (entity_table);
  g_return_if_fail (type);
  g_return_if_fail (*type);
  g_return_if_fail (cb_func);

  iter.fcn = cb_func;
  iter.data = user_data;
  iter.type = type;

  g_hash_table_foreach (entity_table->hash, foreach_cb, &iter);
}

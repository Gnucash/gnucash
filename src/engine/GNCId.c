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


/** #defines ********************************************************/
#define GNCID_DEBUG 0


/** Type definitions ************************************************/
typedef struct entity_node
{
  GNCIdType entity_type;
  gpointer entity;
} EntityNode;


/** Static global variables *****************************************/
static GHashTable * entity_table = NULL;
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

  e_node->entity_type = GNC_ID_NONE;
  e_node->entity = NULL;

  g_free(guid);
  g_free(e_node);

  return TRUE;
}

static void
entity_table_destroy (void)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach_remove(entity_table, entity_node_destroy, NULL);
  g_hash_table_destroy(entity_table);

  entity_table = NULL;
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

  fprintf(stderr, "%s %d %p\n",
          guid_to_string(guid), node->entity_type, node->entity);
}

static void
summarize_table(void)
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach(entity_table, print_node, NULL);
}
#endif

static void
entity_table_init(void)
{
  if (entity_table != NULL)
    entity_table_destroy();

  entity_table = g_hash_table_new(id_hash, id_compare);

  xaccStoreEntity(NULL, xaccGUIDNULL(), GNC_ID_NULL);

#if GNCID_DEBUG
  atexit(summarize_table);
#endif
}

GNCIdType
xaccGUIDType(const GUID * guid)
{
  EntityNode *e_node;
  GNCIdType entity_type;

  if (guid == NULL)
    return GNC_ID_NONE;

  if (entity_table == NULL)
    entity_table_init();

  e_node = g_hash_table_lookup(entity_table, guid->data);
  if (e_node == NULL)
    return GNC_ID_NONE;

  entity_type = e_node->entity_type;
  if ((entity_type <= GNC_ID_NONE) || (entity_type > LAST_GNC_ID))
    return GNC_ID_NONE;

  return entity_type;
}

void
xaccGUIDNew(GUID *guid)
{
  if (guid == NULL)
    return;

  do
  {
    guid_new(guid);

    if (xaccGUIDType(guid) == GNC_ID_NONE)
      break;

    PWARN("duplicate id created, trying again");
  } while(1);
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

void *
xaccLookupEntity(const GUID * guid, GNCIdType entity_type)
{
  EntityNode *e_node;

  if (guid == NULL)
    return NULL;

  if (entity_table == NULL)
    entity_table_init();

  e_node = g_hash_table_lookup(entity_table, guid->data);
  if (e_node == NULL)
    return NULL;

  if (e_node->entity_type != entity_type)
    return NULL;

  return e_node->entity;
}

void
xaccStoreEntity(void * entity, const GUID * guid, GNCIdType entity_type)
{
  EntityNode *e_node;
  GUID *new_guid;

  if (guid == NULL)
    return;

  if ((entity_type <= GNC_ID_NONE) || (entity_type > LAST_GNC_ID))
    return;

  if (guid_equal(guid, xaccGUIDNULL())) return;

  xaccRemoveEntity(guid);

  e_node = g_new(EntityNode, 1);
  e_node->entity_type = entity_type;
  e_node->entity = entity;

  new_guid = g_new(GUID, 1);
  *new_guid = *guid;

  g_hash_table_insert(entity_table, new_guid, e_node);
}

void
xaccRemoveEntity(const GUID * guid)
{
  EntityNode *e_node;
  gpointer old_guid;
  gpointer node;

  if (guid == NULL)
    return;

  if (entity_table == NULL)
    entity_table_init();

  if (g_hash_table_lookup_extended(entity_table, guid, &old_guid, &node))
  {
    e_node = node;
    if (e_node->entity_type == GNC_ID_NULL)
      return;

    g_hash_table_remove(entity_table, old_guid);
    entity_node_destroy(old_guid, node, NULL);
  }
}

GHashTable *
xaccGetAndResetEntityTable(void) {
  GHashTable *result = entity_table;
  entity_table = NULL;
  return(result);
}

void
xaccSetEntityTable(GHashTable *et) {
  if(entity_table) entity_table_destroy();
  entity_table = et;
}

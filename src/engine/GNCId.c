/********************************************************************\
 * GNCId.c -- Gnucash entity identifier implementation              *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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


/** Type definitions ************************************************/
typedef struct entity_node
{
  GNCIdType entity_type;
  gpointer entity;
} EntityNode;


/** Static global variables *****************************************/
static GHashTable * entity_table = NULL;


/** Function implementations ****************************************/

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

static void entity_table_destroy()
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach_remove(entity_table, entity_node_destroy, NULL);
  g_hash_table_destroy(entity_table);

  entity_table = NULL;
}

static guint
id_hash(gconstpointer key)
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
  return memcmp(key_1, key_2, sizeof(GUID)) == 0;
}

#ifdef USE_DEBUG
static void
print_node(gpointer key, gpointer value, gpointer not_used)
{
  GUID *guid = key;
  EntityNode *node = value;

  fprintf(stderr, "%s %d %p\n",
          guid_to_string(guid), node->entity_type, node->entity);
}

static void
summarize_table()
{
  if (entity_table == NULL)
    return;

  g_hash_table_foreach(entity_table, print_node, NULL);
}
#endif

static void
entity_table_init()
{
  if (entity_table != NULL)
    entity_table_destroy();

  entity_table = g_hash_table_new(id_hash, id_compare);

#ifdef USE_DEBUG
  atexit(summarize_table);
#endif
}

GNCIdType
xaccGUIDType(GUID * guid)
{
  EntityNode *e_node;

  if (guid == NULL)
    return GNC_ID_NONE;

  if (entity_table == NULL)
    entity_table_init();

  e_node = g_hash_table_lookup(entity_table, guid->data);
  if (e_node == NULL)
    return GNC_ID_NONE;

  return e_node->entity_type;
}

void *
xaccLookupEntity(GUID * guid, GNCIdType entity_type)
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
xaccStoreEntity(void * entity, GUID * guid, GNCIdType entity_type)
{
  EntityNode *e_node;
  GUID *new_guid;

  if (entity == NULL)
    return;

  if (guid == NULL)
    return;

  if ((entity_type <= GNC_ID_NONE) || (entity_type > LAST_GNC_ID))
    return;

  xaccRemoveEntity(guid);

  e_node = g_new(EntityNode, 1);
  e_node->entity_type = entity_type;
  e_node->entity = entity;

  new_guid = g_new(GUID, 1);
  *new_guid = *guid;

  g_hash_table_insert(entity_table, new_guid, e_node);
}

void
xaccRemoveEntity(GUID * guid)
{
  gpointer e_node;
  gpointer old_guid;

  if (guid == NULL)
    return;

  if (entity_table == NULL)
    entity_table_init();

  if (g_hash_table_lookup_extended(entity_table, guid, &old_guid, &e_node))
  {
    g_hash_table_remove(entity_table, old_guid);
    entity_node_destroy(old_guid, e_node, NULL);
  }
}

/********************************************************************\
 * qofid.c -- QOF entity identifier implementation                  *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <string.h>
#include <glib.h>

#include "qof.h"
#include "qofid-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

struct QofCollection_s
{
  QofIdType    e_type;
  gboolean     is_dirty;
  
  GHashTable * hash_of_entities;
  gpointer     data;       /* place where object class can hang arbitrary data */
};

/* =============================================================== */

static void qof_collection_remove_entity (QofEntity *ent);

void
qof_entity_init (QofEntity *ent, QofIdType type, QofCollection * tab)
{
  g_return_if_fail (NULL != tab);
  
  /* XXX We passed redundant info to this routine ... but I think that's
   * OK, it might eliminate programming errors. */
  if (safe_strcmp(tab->e_type, type))
  {
    PERR ("attempt to insert \"%s\" into \"%s\"", type, tab->e_type);
    return;
  }
  ent->e_type = CACHE_INSERT (type);

  do
  {
    guid_new(&ent->guid);

    if (NULL == qof_collection_lookup_entity (tab, &ent->guid)) break;

    PWARN("duplicate id created, trying again");
  } while(1);
 
  ent->collection = tab;

  qof_collection_insert_entity (tab, ent);
}

void
qof_entity_release (QofEntity *ent)
{
  if (!ent->collection) return;
  qof_collection_remove_entity (ent);
  CACHE_REMOVE (ent->e_type);
  ent->e_type = NULL;
}


/* This is a restricted function, should be used only during 
 * read from file */
void
qof_entity_set_guid (QofEntity *ent, const GUID *guid)
{
  QofCollection *col;
  if (guid_equal (guid, &ent->guid)) return;

  col = ent->collection;
  qof_collection_remove_entity (ent);
  ent->guid = *guid;
  qof_collection_insert_entity (col, ent);
}

const GUID *
qof_entity_get_guid (QofEntity *ent)
{
  if (!ent) return guid_null();
  return &ent->guid;
}

/* =============================================================== */

static guint
id_hash (gconstpointer key)
{
  const GUID *guid = key;

  if (key == NULL)
    return 0;

  /* Compiler should optimize this all away! */
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

QofCollection *
qof_collection_new (QofIdType type)
{
  QofCollection *col;
  col = g_new0(QofCollection, 1);
  col->e_type = CACHE_INSERT (type);
  col->hash_of_entities = g_hash_table_new (id_hash, id_compare);
  col->data = NULL;
  return col;
}

void
qof_collection_destroy (QofCollection *col)
{
  CACHE_REMOVE (col->e_type);
  g_hash_table_destroy(col->hash_of_entities);
  col->e_type = NULL;
  col->hash_of_entities = NULL;
  col->data = NULL;   /** XXX there should be a destroy notifier for this */
  g_free (col);
}

/* =============================================================== */
/* getters */

QofIdType
qof_collection_get_type (QofCollection *col)
{
  return col->e_type;
}

/* =============================================================== */

static void
qof_collection_remove_entity (QofEntity *ent)
{
  QofCollection *col;
  if (!ent) return;
  col = ent->collection;
  if (!col) return;
  g_hash_table_remove (col->hash_of_entities, &ent->guid);
  ent->collection = NULL;
}

void
qof_collection_insert_entity (QofCollection *col, QofEntity *ent)
{
  if (!col || !ent) return;
  if (guid_equal(&ent->guid, guid_null())) return;
  g_return_if_fail (col->e_type == ent->e_type);
  qof_collection_remove_entity (ent);
  g_hash_table_insert (col->hash_of_entities, &ent->guid, ent);
  ent->collection = col;
}

gboolean
qof_collection_add_entity (QofCollection *coll, QofEntity *ent)
{
	QofEntity *e;

	e = NULL;
	if (!coll || !ent) { return FALSE; }
	if (guid_equal(&ent->guid, guid_null())) { return FALSE; }
	g_return_val_if_fail (coll->e_type == ent->e_type, FALSE);
	e = qof_collection_lookup_entity(coll, &ent->guid);
	if ( e != NULL ) { return FALSE; }
	g_hash_table_insert (coll->hash_of_entities, &ent->guid, ent);
	return TRUE;
}

static void
collection_merge_cb (QofEntity *ent, gpointer data)
{
	QofCollection *target;

	target = (QofCollection*)data;
	qof_collection_add_entity(target, ent);	
}

gboolean
qof_collection_merge (QofCollection *target, QofCollection *merge)
{
	if(!target || !merge) { return FALSE; }
	g_return_val_if_fail (target->e_type == merge->e_type, FALSE);
	qof_collection_foreach(merge, collection_merge_cb, target);
	return TRUE;
}

static void
collection_compare_cb (QofEntity *ent, gpointer user_data)
{
	QofCollection *target;
	QofEntity *e;
	gint value;

	e = NULL;
	target = (QofCollection*)user_data;
	if (!target || !ent) { return; }
	value = *(gint*)qof_collection_get_data(target);
	if (value != 0) { return; }
	if (guid_equal(&ent->guid, guid_null())) 
	{
		value = -1;
		qof_collection_set_data(target, &value);
		return; 
	}
	g_return_if_fail (target->e_type == ent->e_type);
	e = qof_collection_lookup_entity(target, &ent->guid);
	if ( e == NULL )
	{
		value = 1;
		qof_collection_set_data(target, &value);
		return;
	}
	value = 0;
	qof_collection_set_data(target, &value);
}

gint
qof_collection_compare (QofCollection *target, QofCollection *merge)
{
	gint value;

	value = 0;
	if (!target && !merge) { return 0; }
	if (target == merge) { return 0; }
	if (!target && merge) { return -1; }
	if (target && !merge) { return 1; }
	if(target->e_type != merge->e_type) { return -1; }
	qof_collection_set_data(target, &value);
	qof_collection_foreach(merge, collection_compare_cb, target);
	value = *(gint*)qof_collection_get_data(target);
	if(value == 0) {
		qof_collection_set_data(merge, &value);
		qof_collection_foreach(target, collection_compare_cb, merge);
		value = *(gint*)qof_collection_get_data(merge);
	}
	return value;
}

QofEntity *
qof_collection_lookup_entity (QofCollection *col, const GUID * guid)
{
  QofEntity *ent;
  g_return_val_if_fail (col, NULL);
  if (guid == NULL) return NULL;
  ent = g_hash_table_lookup (col->hash_of_entities, guid);
  return ent;
}

QofCollection *
qof_collection_from_glist (QofIdType type, GList *glist)
{
	QofCollection *coll;
	QofEntity *ent;
	GList *list;

	coll = qof_collection_new(type);
	for(list = glist; list != NULL; list = list->next)
	{
		ent = (QofEntity*)list->data;
		if(FALSE == qof_collection_add_entity(coll, ent))
		{
			return NULL;
		}
	}
	return coll;
}

guint
qof_collection_count (QofCollection *col)
{
	guint c;

	c = g_hash_table_size(col->hash_of_entities);
	return c;
}

/* =============================================================== */

gboolean 
qof_collection_is_dirty (QofCollection *col)
{
   return col ? col->is_dirty : FALSE;
}

void 
qof_collection_mark_clean (QofCollection *col)
{
   if (col) { col->is_dirty = FALSE; }
}

void 
qof_collection_mark_dirty (QofCollection *col)
{
   if (col) { col->is_dirty = TRUE; }
}

/* =============================================================== */

gpointer 
qof_collection_get_data (QofCollection *col)
{
   return col ? col->data : NULL;
}

void 
qof_collection_set_data (QofCollection *col, gpointer user_data)
{
   if (col) { col->data = user_data; }
}

/* =============================================================== */

struct _iterate {
  QofEntityForeachCB      fcn;
  gpointer                data;
};

static void foreach_cb (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  QofEntity *ent = item;

  iter->fcn (ent, iter->data);
}

void
qof_collection_foreach (QofCollection *col, QofEntityForeachCB cb_func, 
                        gpointer user_data)
{
  struct _iterate iter;

  g_return_if_fail (col);
  g_return_if_fail (cb_func);

  iter.fcn = cb_func;
  iter.data = user_data;

  g_hash_table_foreach (col->hash_of_entities, foreach_cb, &iter);
}

/* =============================================================== */

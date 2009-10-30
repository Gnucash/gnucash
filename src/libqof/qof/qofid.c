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
static gboolean qof_alt_dirty_mode = FALSE;

struct QofCollection_s
{
    QofIdType    e_type;
    gboolean     is_dirty;

    GHashTable * hash_of_entities;
    gpointer     data;       /* place where object class can hang arbitrary data */
};

/* =============================================================== */

gboolean
qof_get_alt_dirty_mode (void)
{
    return qof_alt_dirty_mode;
}

void
qof_set_alt_dirty_mode (gboolean enabled)
{
    qof_alt_dirty_mode = enabled;
}

/* =============================================================== */

QofCollection *
qof_collection_new (QofIdType type)
{
    QofCollection *col;
    col = g_new0(QofCollection, 1);
    col->e_type = CACHE_INSERT (type);
    col->hash_of_entities = guid_hash_table_new();
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
qof_collection_get_type (const QofCollection *col)
{
    return col->e_type;
}

/* =============================================================== */

void
qof_collection_remove_entity (QofInstance *ent)
{
    QofCollection *col;
    const GUID *guid;

    if (!ent) return;
    col = qof_instance_get_collection(ent);
    if (!col) return;
    guid = qof_instance_get_guid(ent);
    g_hash_table_remove (col->hash_of_entities, guid);
    if (!qof_alt_dirty_mode)
        qof_collection_mark_dirty(col);
    qof_instance_set_collection(ent, NULL);
}

void
qof_collection_insert_entity (QofCollection *col, QofInstance *ent)
{
    const GUID *guid;

    if (!col || !ent) return;
    guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null())) return;
    g_return_if_fail (col->e_type == ent->e_type);
    qof_collection_remove_entity (ent);
    g_hash_table_insert (col->hash_of_entities, (gpointer)guid, ent);
    if (!qof_alt_dirty_mode)
        qof_collection_mark_dirty(col);
    qof_instance_set_collection(ent, col);
}

gboolean
qof_collection_add_entity (QofCollection *coll, QofInstance *ent)
{
    QofInstance *e;
    const GUID *guid;

    e = NULL;
    if (!coll || !ent)
    {
        return FALSE;
    }
    guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null()))
    {
        return FALSE;
    }
    g_return_val_if_fail (coll->e_type == ent->e_type, FALSE);
    e = qof_collection_lookup_entity(coll, guid);
    if ( e != NULL )
    {
        return FALSE;
    }
    g_hash_table_insert (coll->hash_of_entities, (gpointer)guid, ent);
    if (!qof_alt_dirty_mode)
        qof_collection_mark_dirty(coll);
    return TRUE;
}

static void
collection_merge_cb (QofInstance *ent, gpointer data)
{
    QofCollection *target;

    target = (QofCollection*)data;
    qof_collection_add_entity(target, ent);
}

gboolean
qof_collection_merge (QofCollection *target, QofCollection *merge)
{
    if (!target || !merge)
    {
        return FALSE;
    }
    g_return_val_if_fail (target->e_type == merge->e_type, FALSE);
    qof_collection_foreach(merge, collection_merge_cb, target);
    return TRUE;
}

static void
collection_compare_cb (QofInstance *ent, gpointer user_data)
{
    QofCollection *target;
    QofInstance *e;
    const GUID *guid;
    gint value;

    e = NULL;
    target = (QofCollection*)user_data;
    if (!target || !ent)
    {
        return;
    }
    value = *(gint*)qof_collection_get_data(target);
    if (value != 0)
    {
        return;
    }
    guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null()))
    {
        value = -1;
        qof_collection_set_data(target, &value);
        return;
    }
    g_return_if_fail (target->e_type == ent->e_type);
    e = qof_collection_lookup_entity(target, guid);
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
    if (!target && !merge)
    {
        return 0;
    }
    if (target == merge)
    {
        return 0;
    }
    if (!target && merge)
    {
        return -1;
    }
    if (target && !merge)
    {
        return 1;
    }
    if (target->e_type != merge->e_type)
    {
        return -1;
    }
    qof_collection_set_data(target, &value);
    qof_collection_foreach(merge, collection_compare_cb, target);
    value = *(gint*)qof_collection_get_data(target);
    if (value == 0)
    {
        qof_collection_set_data(merge, &value);
        qof_collection_foreach(target, collection_compare_cb, merge);
        value = *(gint*)qof_collection_get_data(merge);
    }
    return value;
}

QofInstance *
qof_collection_lookup_entity (const QofCollection *col, const GUID * guid)
{
    QofInstance *ent;
    g_return_val_if_fail (col, NULL);
    if (guid == NULL) return NULL;
    ent = g_hash_table_lookup (col->hash_of_entities, guid);
    return ent;
}

QofCollection *
qof_collection_from_glist (QofIdType type, const GList *glist)
{
    QofCollection *coll;
    QofInstance *ent;
    const GList *list;

    coll = qof_collection_new(type);
    for (list = glist; list != NULL; list = list->next)
    {
        ent = QOF_INSTANCE(list->data);
        if (FALSE == qof_collection_add_entity(coll, ent))
        {
            return NULL;
        }
    }
    return coll;
}

guint
qof_collection_count (const QofCollection *col)
{
    guint c;

    c = g_hash_table_size(col->hash_of_entities);
    return c;
}

/* =============================================================== */

gboolean
qof_collection_is_dirty (const QofCollection *col)
{
    return col ? col->is_dirty : FALSE;
}

void
qof_collection_mark_clean (QofCollection *col)
{
    if (col)
    {
        col->is_dirty = FALSE;
    }
}

void
qof_collection_mark_dirty (QofCollection *col)
{
    if (col)
    {
        col->is_dirty = TRUE;
    }
}

void
qof_collection_print_dirty (const QofCollection *col, gpointer dummy)
{
    if (col->is_dirty)
        printf("%s collection is dirty.\n", col->e_type);
    qof_collection_foreach(col, (QofInstanceForeachCB)qof_instance_print_dirty, NULL);
}

/* =============================================================== */

gpointer
qof_collection_get_data (const QofCollection *col)
{
    return col ? col->data : NULL;
}

void
qof_collection_set_data (QofCollection *col, gpointer user_data)
{
    if (col)
    {
        col->data = user_data;
    }
}

/* =============================================================== */

struct _iterate
{
    QofInstanceForeachCB      fcn;
    gpointer                data;
};

static void foreach_cb (gpointer key, gpointer item, gpointer arg)
{
    struct _iterate *iter = arg;
    QofInstance *ent = item;

    iter->fcn (ent, iter->data);
}

void
qof_collection_foreach (const QofCollection *col, QofInstanceForeachCB cb_func,
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

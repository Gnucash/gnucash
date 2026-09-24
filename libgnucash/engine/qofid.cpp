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

#include <glib.h>

#include <config.h>
#include <string.h>
#include <inttypes.h>
#include <guid.hpp>
#include "qof.h"
#include "qofid-p.h"
#include "qofinstance-p.h"

#include <boost/unordered/unordered_flat_map.hpp>
using GuidEntityMap = boost::unordered_flat_map<GncGUID, QofInstance*, std::hash<GncGUID>>;

static QofLogModule log_module = QOF_MOD_ENGINE;

struct QofCollection_s
{
    QofIdType    e_type;
    gboolean     is_dirty;

    GuidEntityMap guid_entity_map;
    guint64 mutation_generation;
    gpointer     data;       /* place where object class can hang arbitrary data */

    QofCollection_s (QofIdType type) : e_type{static_cast<QofIdType>(CACHE_INSERT(type))}
                                     , is_dirty{FALSE}
                                     , mutation_generation{1}
                                     , data{NULL} {}
    ~QofCollection_s ()
    {
        CACHE_REMOVE (e_type);
    }

    void bump_generation ()
    {
        if (++mutation_generation == 0)
            mutation_generation = 1;
    }
};

struct QofCollectionCursor_s
{
    const QofCollection *collection;
    guint64 generation;
    GuidEntityMap::const_iterator current;
    GuidEntityMap::const_iterator end;
};

/* =============================================================== */

QofCollection *
qof_collection_new (QofIdType type)
{
    return new QofCollection (type);
}

void
qof_collection_destroy (QofCollection *col)
{
    delete col;
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
    const GncGUID *guid;

    if (!ent) return;
    col = qof_instance_get_collection(ent);
    if (!col) return;
    guid = qof_instance_get_guid(ent);
    if (col->guid_entity_map.erase(*guid) != 0)
        col->bump_generation ();
    qof_instance_set_collection(ent, NULL);
}

void
qof_collection_insert_entity (QofCollection *col, QofInstance *ent)
{
    if (!col || !ent) return;
    const GncGUID *guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null())) return;
    g_return_if_fail (col->e_type == ent->e_type);
    qof_collection_remove_entity (ent);
    col->guid_entity_map.insert_or_assign (*guid, ent);
    col->bump_generation ();
    qof_instance_set_collection(ent, col);
}

gboolean
qof_collection_add_entity (QofCollection *coll, QofInstance *ent)
{
    if (!coll || !ent) return FALSE;
    const GncGUID *guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null())) return FALSE;
    g_return_val_if_fail (coll->e_type == ent->e_type, FALSE);
    if (!coll->guid_entity_map.try_emplace (*guid, ent).second)
        return FALSE;
    coll->bump_generation ();
    return TRUE;
}


static void
collection_compare_cb (QofInstance *ent, gpointer user_data)
{
    QofCollection *target;
    QofInstance *e;
    const GncGUID *guid;
    gint* value;

    e = NULL;
    target = (QofCollection*)user_data;
    if (!target || !ent)
    {
        return;
    }
    value = (gint*)qof_collection_get_data(target);
    if (*value != 0)
    {
        return;
    }
    guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null()))
    {
        *value = -1;
        return;
    }
    g_return_if_fail (target->e_type == ent->e_type);
    e = qof_collection_lookup_entity(target, guid);
    if ( e == NULL )
    {
        *value = 1;
        return;
    }
    *value = 0;
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
qof_collection_lookup_entity (const QofCollection *col, const GncGUID * guid)
{
    g_return_val_if_fail (col, NULL);
    if (guid == NULL) return NULL;
    auto it = col->guid_entity_map.find(*guid);
    if (it == col->guid_entity_map.end() || qof_instance_get_destroying(it->second))
        return nullptr;
    return it->second;
}

QofCollectionCursor *
qof_collection_cursor_new (const QofCollection *collection)
{
    if (!collection)
        return nullptr;
    return new (std::nothrow) QofCollectionCursor {
        collection,
        collection->mutation_generation,
        collection->guid_entity_map.cbegin (),
        collection->guid_entity_map.cend ()};
}

QofCollectionCursorStatus
qof_collection_cursor_next (QofCollectionCursor *cursor, GncGUID *guid)
{
    if (!cursor || !cursor->collection || !guid)
        return QOF_COLLECTION_CURSOR_STALE;

    /* Never touch an iterator after its collection generation drifted: a
     * boost::unordered_flat_map mutation may have invalidated it. */
    if (cursor->generation != cursor->collection->mutation_generation)
        return QOF_COLLECTION_CURSOR_STALE;
    if (cursor->current == cursor->end)
        return QOF_COLLECTION_CURSOR_END;

    *guid = cursor->current->first;
    ++cursor->current;
    return QOF_COLLECTION_CURSOR_ITEM;
}

void
qof_collection_cursor_free (QofCollectionCursor *cursor)
{
    delete cursor;
}

guint
qof_collection_count (const QofCollection *col)
{
    return col->guid_entity_map.size();
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

void
qof_collection_foreach_sorted (const QofCollection *col, QofInstanceForeachCB cb_func,
                               gpointer user_data, GCompareFunc sort_fn)
{
    g_return_if_fail (col);
    g_return_if_fail (cb_func);

    PINFO("Hash Table size of %s before is %zu", col->e_type, col->guid_entity_map.size());

    std::vector<QofInstance*> entries (col->guid_entity_map.size());
    std::transform (col->guid_entity_map.cbegin(), col->guid_entity_map.cend(),
                    entries.begin(), [](const auto& kv) { return kv.second; });
    if (sort_fn)
        std::sort (entries.begin(), entries.end(),
                   [sort_fn](auto a, auto b) { return sort_fn (a, b) < 0; });
    std::for_each (entries.cbegin(), entries.cend(),
                   [&](auto ent) { cb_func (ent, user_data); });

    PINFO("Hash Table size of %s after is %zu", col->e_type, col->guid_entity_map.size());
}

void
qof_collection_foreach (const QofCollection *col, QofInstanceForeachCB cb_func,
                        gpointer user_data)
{
    qof_collection_foreach_sorted (col, cb_func, user_data, nullptr);
}
/* =============================================================== */

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

#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"

#include <string.h>
#include <glib.h>

#ifdef __cplusplus
}
#endif

#include "qof.h"
#include "qofid-p.h"
#include "qofinstance-p.h"


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
    col->e_type = static_cast<QofIdType>(CACHE_INSERT (type));
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
    const GncGUID *guid;

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
    const GncGUID *guid;

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
    const GncGUID *guid;

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
collection_compare_cb (QofInstance *ent, gpointer user_data)
{
    QofCollection *target;
    QofInstance *e;
    const GncGUID *guid;
    gint value;

    e = NULL;
    target = (QofCollection*)user_data;
    if (!target || !ent)
    {
        return;
    }
    value = GPOINTER_TO_INT(qof_collection_get_data(target));
    if (value != 0)
    {
        return;
    }
    guid = qof_instance_get_guid(ent);
    if (guid_equal(guid, guid_null()))
    {
        value = -1;
        qof_collection_set_data(target, GINT_TO_POINTER(value));
        return;
    }
    g_return_if_fail (target->e_type == ent->e_type);
    e = qof_collection_lookup_entity(target, guid);
    if ( e == NULL )
    {
        value = 1;
        qof_collection_set_data(target, GINT_TO_POINTER(value));
        return;
    }
    value = 0;
    qof_collection_set_data(target, GINT_TO_POINTER(value));
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
    qof_collection_set_data(target, GINT_TO_POINTER(value));
    qof_collection_foreach(merge, collection_compare_cb, target);
    value = GPOINTER_TO_INT(qof_collection_get_data(target));
    if (value == 0)
    {
        qof_collection_set_data(merge, GINT_TO_POINTER(value));
        qof_collection_foreach(target, collection_compare_cb, merge);
        value = GPOINTER_TO_INT(qof_collection_get_data(merge));
    }
    return value;
}

QofInstance *
qof_collection_lookup_entity (const QofCollection *col, const GncGUID * guid)
{
    QofInstance *ent;
    g_return_val_if_fail (col, NULL);
    if (guid == NULL) return NULL;
    ent = static_cast<QofInstance*>(g_hash_table_lookup (col->hash_of_entities,
							 guid));
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

static void
foreach_cb (gpointer item, gpointer arg)
{
    struct _iterate *iter = static_cast<_iterate*>(arg);
    QofInstance *ent = static_cast<QofInstance*>(item);

    iter->fcn (ent, iter->data);
}

void
qof_collection_foreach (const QofCollection *col, QofInstanceForeachCB cb_func,
                        gpointer user_data)
{
    struct _iterate iter;
    GList *entries;

    g_return_if_fail (col);
    g_return_if_fail (cb_func);

    iter.fcn = cb_func;
    iter.data = user_data;

    PINFO("Hash Table size of %s before is %d", col->e_type, g_hash_table_size(col->hash_of_entities));

    entries = g_hash_table_get_values (col->hash_of_entities);
    g_list_foreach (entries, foreach_cb, &iter);
    g_list_free (entries);

    PINFO("Hash Table size of %s after is %d", col->e_type, g_hash_table_size(col->hash_of_entities));
}
/* =============================================================== */

/* === QofCollection C++ class implementation === */

using namespace gnucash::qof;

QofCollectionClass::QofCollectionClass (QofIdType type)
    : m_data (NULL)
{
    m_e_type = static_cast<QofIdType>(CACHE_INSERT (type));
    m_hash_of_entities = guid_hash_table_new ();
}

QofCollectionClass::~QofCollectionClass ()
{
    CACHE_REMOVE (m_e_type);
    g_hash_table_destroy (m_hash_of_entities);
    m_e_type = NULL;
    m_hash_of_entities = NULL;
    m_data = NULL;   /** XXX there should be a destroy notifier for this */
}

QofCollectionClass *
QofCollectionClass::from_glist (QofIdType type, const GList *inst_list)
{
    const GList *list;

    QofCollectionClass *coll = new QofCollectionClass (type);
    for (list = inst_list; list != NULL; list = list->next)
    {
        QofInstance *ent = QOF_INSTANCE(list->data);
        if (FALSE == coll->add_entity (ent))
        {
            delete coll;
            return NULL;
        }
    }
    return coll;
}

void QofCollectionClass::foreach (QofInstanceForeachCB callback,
                                  gpointer user_data)
{
    g_return_if_fail (callback);

    struct _iterate iter;

    iter.fcn = callback;
    iter.data = user_data;

    PINFO("Hash Table size of %s before is %d",
          m_e_type, g_hash_table_size (m_hash_of_entities));

    GList* entries = g_hash_table_get_values (m_hash_of_entities);
    g_list_foreach (entries, foreach_cb, &iter);
    g_list_free (entries);

    PINFO("Hash Table size of %s after is %d",
          m_e_type, g_hash_table_size (m_hash_of_entities));
}

void QofCollectionClass::print_dirty (gpointer dummy)
{
    if (is_dirty ())
        printf("%s collection is dirty.\n", m_e_type);
    foreach ((QofInstanceForeachCB)qof_instance_print_dirty, NULL);
}

QofInstance *QofCollectionClass::lookup_entity (const GncGUID *guid)
{
    if (guid == NULL) return NULL;
    return static_cast<QofInstance*>(
        g_hash_table_lookup (m_hash_of_entities, guid));
}

gboolean QofCollectionClass::add_entity (QofInstance* ent)
{
    if (!ent)
        return FALSE;

    g_return_val_if_fail (m_e_type == ent->e_type, FALSE);

    const GncGUID *guid = qof_instance_get_guid (ent);
    if (guid_equal (guid, guid_null ()))
        return FALSE;
    QofInstance *e = lookup_entity (guid);
    if (e != NULL)
        return FALSE;
    g_hash_table_insert (m_hash_of_entities, (gpointer)guid, ent);
    if (!qof_alt_dirty_mode)
        mark_dirty ();
    return TRUE;
}

void QofCollectionClass::remove_entity (QofInstance *ent)
{
    gboolean ret;

    if (!ent) return;

    const GncGUID *guid = qof_instance_get_guid (ent);
    ret = g_hash_table_remove (m_hash_of_entities, guid);
    /* if instance doesn't exist in collection, quit quietly and do nothing. */
    if (ret == FALSE) return;

    if (!qof_alt_dirty_mode)
        mark_dirty ();
}

void QofCollectionClass::insert_entity (QofInstance *ent)
{
    if (!ent) return;
    g_return_if_fail (m_e_type == ent->e_type);

    const GncGUID *guid = qof_instance_get_guid (ent);
    if (guid_equal (guid, guid_null ())) return;
    remove_entity (ent);
    g_hash_table_insert (m_hash_of_entities, (gpointer)guid, ent);
    if (!qof_alt_dirty_mode)
        mark_dirty ();
}

static void
collection_class_compare_cb (QofInstance *ent, gpointer user_data)
{
    QofCollectionClass *target = static_cast<QofCollectionClass *>(user_data);
    if (!target || !ent)
    {
        return;
    }
    gint value = GPOINTER_TO_INT (target->get_data ());
    if (value != 0)
    {
        return;
    }
    const GncGUID *guid = qof_instance_get_guid (ent);
    if (guid_equal (guid, guid_null ()))
    {
        value = -1;
        target->set_data (GINT_TO_POINTER (value));
        return;
    }
    g_return_if_fail (target->get_type () == ent->e_type);
    QofInstance *e = target->lookup_entity (guid);
    if (e == NULL)
    {
        value = 1;
        target->set_data (GINT_TO_POINTER (value));
        return;
    }
    value = 0;
    target->set_data (GINT_TO_POINTER (value));
}

gint QofCollectionClass::compare_to (QofCollectionClass *rhs)
{
    if (rhs == NULL)
        return 1;
    return compare_to (*rhs);
}

gint QofCollectionClass::compare_to (QofCollectionClass &rhs)
{
    gint value = 0;

    if (m_e_type != rhs.get_type ())
    {
        return -1;
    }
    set_data (GINT_TO_POINTER (value));
    rhs.foreach (collection_class_compare_cb, this);
    value = GPOINTER_TO_INT (get_data ());
    if (value == 0)
    {
        rhs.set_data (GINT_TO_POINTER (value));
        foreach (collection_class_compare_cb, &rhs);
        value = GPOINTER_TO_INT (rhs.get_data ());
    }
    return value;
}

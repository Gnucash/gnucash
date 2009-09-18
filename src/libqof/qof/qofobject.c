/********************************************************************\
 * qofobject.c -- the Core Object Registration/Lookup Interface     *
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
/*
 * qofobject.c -- the Core Object Object Registry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "qof.h"
#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_OBJECT;

static gboolean object_is_initialized = FALSE;
static GList *object_modules = NULL;
static GList *book_list = NULL;
static GHashTable *backend_data = NULL;

gpointer
qof_object_new_instance (QofIdTypeConst type_name, QofBook *book)
{
    const QofObject *obj;

    if (!type_name) return NULL;

    obj = qof_object_lookup (type_name);
    if (!obj) return NULL;

    if (obj->create)
        return (obj->create (book));

    return NULL;
}

void qof_object_book_begin (QofBook *book)
{
    GList *l;

    if (!book) return;
    ENTER (" ");
    for (l = object_modules; l; l = l->next)
    {
        QofObject *obj = l->data;
        if (obj->book_begin)
            obj->book_begin (book);
    }

    /* Remember this book for later */
    book_list = g_list_prepend (book_list, book);
    LEAVE (" ");
}

void qof_object_book_end (QofBook *book)
{
    GList *l;

    if (!book) return;
    ENTER (" ");
    for (l = object_modules; l; l = l->next)
    {
        QofObject *obj = l->data;
        if (obj->book_end)
            obj->book_end (book);
    }

    /* Remove it from the list */
    book_list = g_list_remove (book_list, book);
    LEAVE (" ");
}

gboolean
qof_object_is_dirty (const QofBook *book)
{
    GList *l;

    if (!book) return FALSE;
    for (l = object_modules; l; l = l->next)
    {
        QofObject *obj = l->data;
        if (obj->is_dirty)
        {
            QofCollection *col;
            col = qof_book_get_collection (book, obj->e_type);
            if (obj->is_dirty (col)) return TRUE;
        }
    }
    return FALSE;
}

void
qof_object_mark_clean (QofBook *book)
{
    GList *l;

    if (!book) return;
    for (l = object_modules; l; l = l->next)
    {
        QofObject *obj = l->data;
        if (obj->mark_clean)
        {
            QofCollection *col;
            col = qof_book_get_collection (book, obj->e_type);
            (obj->mark_clean) (col);
        }
    }
}

void qof_object_foreach_type (QofForeachTypeCB cb, gpointer user_data)
{
    GList *l;

    if (!cb) return;

    for (l = object_modules; l; l = l->next)
    {
        QofObject *obj = l->data;
        (cb) (obj, user_data);
    }
}

gboolean
qof_object_compliance (QofIdTypeConst type_name, gboolean warn)
{
    const QofObject *obj;

    obj = qof_object_lookup(type_name);
    if ((obj->create == NULL) || (obj->foreach == NULL))
    {
        if (warn)
        {
            PINFO (" Object type %s is not fully QOF compliant", obj->e_type);
        }
        return FALSE;
    }
    return TRUE;
}


void
qof_object_foreach (QofIdTypeConst type_name, QofBook *book,
                    QofInstanceForeachCB cb, gpointer user_data)
{
    QofCollection *col;
    const QofObject *obj;

    if (!book || !type_name)
    {
        return;
    }
    PINFO ("type=%s", type_name);

    obj = qof_object_lookup (type_name);
    if (!obj)
    {
        PERR ("No object of type %s", type_name);
        return;
    }
    col = qof_book_get_collection (book, obj->e_type);
    if (!obj)
    {
        return;
    }
    if (obj->foreach)
    {
        obj->foreach (col, cb, user_data);
    }
    return;
}

const char *
qof_object_printable (QofIdTypeConst type_name, gpointer obj)
{
    const QofObject *b_obj;

    if (!type_name || !obj) return NULL;

    b_obj = qof_object_lookup (type_name);
    if (!b_obj) return NULL;

    if (b_obj->printable)
        return (b_obj->printable (obj));

    return NULL;
}

const char * qof_object_get_type_label (QofIdTypeConst type_name)
{
    const QofObject *obj;

    if (!type_name) return NULL;

    obj = qof_object_lookup (type_name);
    if (!obj) return NULL;

    return (obj->type_label);
}

static gboolean clear_table (gpointer key, gpointer value, gpointer user_data)
{
    g_hash_table_destroy (value);
    return TRUE;
}

/* INITIALIZATION and PRIVATE FUNCTIONS */

void qof_object_initialize (void)
{
    if (object_is_initialized) return;
    backend_data = g_hash_table_new (g_str_hash, g_str_equal);
    object_is_initialized = TRUE;
}

void qof_object_shutdown (void)
{
    g_return_if_fail (object_is_initialized == TRUE);

    g_hash_table_foreach_remove (backend_data, clear_table, NULL);
    g_hash_table_destroy (backend_data);
    backend_data = NULL;

    g_list_free (object_modules);
    object_modules = NULL;
    g_list_free (book_list);
    book_list = NULL;
    object_is_initialized = FALSE;
}

/* Register new types of object objects.
 * Return TRUE if successful,
 * return FALSE if it fails, invalid arguments, or if the object
 * already exists
 */
gboolean qof_object_register (const QofObject *object)
{
    g_return_val_if_fail (object_is_initialized, FALSE);

    if (!object) return FALSE;
    g_return_val_if_fail (object->interface_version == QOF_OBJECT_VERSION, FALSE);

    if (g_list_index (object_modules, (gpointer)object) == -1)
        object_modules = g_list_prepend (object_modules, (gpointer)object);
    else
        return FALSE;

    /* Now initialize all the known books */
    if (object->book_begin && book_list)
    {
        GList *node;
        for (node = book_list; node; node = node->next)
            object->book_begin (node->data);
    }

    return TRUE;
}

const QofObject * qof_object_lookup (QofIdTypeConst name)
{
    GList *iter;
    const QofObject *obj;

    g_return_val_if_fail (object_is_initialized, NULL);

    if (!name) return NULL;

    for (iter = object_modules; iter; iter = iter->next)
    {
        obj = iter->data;
        if (!safe_strcmp (obj->e_type, name))
            return obj;
    }
    return NULL;
}

gboolean qof_object_register_backend (QofIdTypeConst type_name,
                                      const char *backend_name,
                                      gpointer be_data)
{
    GHashTable *ht;
    g_return_val_if_fail (object_is_initialized, FALSE);

    if (!type_name || *type_name == '\0' ||
            !backend_name || *backend_name == '\0' ||
            !be_data)
        return FALSE;

    ht = g_hash_table_lookup (backend_data, backend_name);

    /* If it doesn't already exist, create a new table for this backend */
    if (!ht)
    {
        ht = g_hash_table_new (g_str_hash, g_str_equal);
        g_hash_table_insert (backend_data, (char *)backend_name, ht);
    }

    /* Now insert the data */
    g_hash_table_insert (ht, (char *)type_name, be_data);

    return TRUE;
}

gpointer qof_object_lookup_backend (QofIdTypeConst type_name,
                                    const char *backend_name)
{
    GHashTable *ht;

    if (!type_name || *type_name == '\0' ||
            !backend_name || *backend_name == '\0')
        return NULL;

    ht = g_hash_table_lookup (backend_data, (char *)backend_name);
    if (!ht)
        return NULL;

    return g_hash_table_lookup (ht, (char *)type_name);
}

struct foreach_data
{
    QofForeachBackendTypeCB        cb;
    gpointer                 user_data;
};

static void foreach_backend (gpointer key, gpointer be_item, gpointer arg)
{
    char *data_type = key;
    struct foreach_data *cb_data = arg;

    g_return_if_fail (key && be_item && arg);

    /* Call the callback for this data type */
    (cb_data->cb) (data_type, be_item, cb_data->user_data);
}

void qof_object_foreach_backend (const char *backend_name,
                                 QofForeachBackendTypeCB cb,
                                 gpointer user_data)
{
    GHashTable *ht;
    struct foreach_data cb_data;

    if (!backend_name || *backend_name == '\0' || !cb)
        return;

    ht = g_hash_table_lookup (backend_data, (char *)backend_name);
    if (!ht)
        return;

    cb_data.cb = cb;
    cb_data.user_data = user_data;

    g_hash_table_foreach (ht, foreach_backend, &cb_data);
}

/* ========================= END OF FILE =================== */

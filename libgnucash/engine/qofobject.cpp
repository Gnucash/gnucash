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
extern "C"
{
#include <config.h>
#include <glib.h>
}

#include "qof.h"
#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_OBJECT;

static gboolean object_is_initialized = FALSE;
static GList *object_modules = NULL;
static GList *book_list = NULL;

/*
 * These getters are used in tests to reach static vars from outside
 * They should be removed when no longer needed
 */

#ifdef __cplusplus
extern "C"
{
#endif

gboolean get_object_is_initialized( void );
GList* get_object_modules( void );
GList* get_book_list( void );

#ifdef __cplusplus
}
#endif

gboolean
get_object_is_initialized( void )
{
    return object_is_initialized;
}

GList*
get_object_modules( void )
{
    return object_modules;
}

GList*
get_book_list( void )
{
    return book_list;
}

/*********/

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
        QofObject *obj = static_cast<QofObject*>(l->data);
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
        QofObject *obj = static_cast<QofObject*>(l->data);
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
        QofObject *obj = static_cast<QofObject*>(l->data);
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
        QofObject *obj = static_cast<QofObject*>(l->data);
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
        QofObject *obj = static_cast<QofObject*>(l->data);
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

static void
do_prepend (QofInstance *qof_p, gpointer list_p)
{
    GList **list = static_cast<GList**>(list_p);
    *list = g_list_prepend(*list, qof_p);
}

void
qof_object_foreach_sorted (QofIdTypeConst type_name, QofBook *book, QofInstanceForeachCB cb, gpointer user_data)
{
    GList *list = NULL;
    GList *iter;

    qof_object_foreach(type_name, book, do_prepend, &list);

    list = g_list_sort(list, qof_instance_guid_compare);

    for (iter = list; iter; iter = iter->next)
    {
        cb(static_cast<QofInstance*>(iter->data), user_data);
    }

    g_list_free(list);

    // FIXME: Apparently this is a memory leak, as this g_list_free doesn't
    // free all of the allocated memory of g_list_append in do_append(). Why?!?
    // Does g_list_sort have special side-effects on the memory of the list?
    // Subsequently, I've changed the g_list_append into g_list_prepend, but
    // solely for performance reasons. To my surprise, this also makes the
    // dubious memory leak go away. But again why?!?
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
    g_hash_table_destroy (static_cast<GHashTable*>(value));
    return TRUE;
}

/* INITIALIZATION and PRIVATE FUNCTIONS */

void qof_object_initialize (void)
{
    if (object_is_initialized) return;
    object_is_initialized = TRUE;
}

void qof_object_shutdown (void)
{
    g_return_if_fail (object_is_initialized == TRUE);

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
            object->book_begin (static_cast<QofBook*>(node->data));
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
        obj = static_cast<QofObject*>(iter->data);
        if (!g_strcmp0 (obj->e_type, name))
            return obj;
    }
    return NULL;
}

/* ========================= END OF FILE =================== */

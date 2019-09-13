/********************************************************************\
 * qofinstance.c -- handler for fields common to all objects        *
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

/*
 * Object instance holds many common fields that most
 * gnucash objects use.
 *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2007 David Hampton <hampton@employees.org>
 * Copyright 2017 Aaron Laws <dartme18@gmail.com>
 */

#include "guid.hpp"
extern "C"
{
#include <config.h>
#include <glib.h>
}

#include <utility>
#include "qof.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "kvp-frame.hpp"
#include "qofinstance-p.h"
#include "qof-backend.hpp"

static QofLogModule log_module = QOF_MOD_ENGINE;

/* ========================================================== */

enum
{
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_TYPE,
    PROP_GUID,
    PROP_COLLECTION,
    PROP_BOOK,
    PROP_LAST_UPDATE,
    PROP_EDITLEVEL,
    PROP_DESTROYING,
    PROP_DIRTY,
    PROP_INFANT,

    PROP_VERSION,
    PROP_VERSION_CHECK,
    PROP_IDATA,
};

typedef struct QofInstancePrivate
{
//    QofIdType        e_type;    /**<	Entity type */
    GncGUID guid;                  /**< GncGUID for the entity */
    QofCollection  *collection; /**< Entity collection */

    /* The entity_table in which this instance is stored */
    QofBook * book;

    /*  Timestamp used to track the last modification to this
     *  instance.  Typically used to compare two versions of the
     *  same object, to see which is newer.  When used with the
     *  SQL backend, this field is reserved for SQL use, to compare
     *  the version in local memory to the remote, server version.
     */
    time64 last_update;

    /*  Keep track of nesting level of begin/end edit calls */
    int editlevel;

    /*  In process of being destroyed */
    gboolean do_free;

    /*  dirty/clean flag. If dirty, then this instance has been modified,
     *  but has not yet been written out to storage (file/database)
     */
    gboolean dirty;

    /* True iff this instance has never been committed. */
    gboolean infant;

    /* version number, used for tracking multiuser updates */
    gint32 version;
    guint32 version_check;  /* data aging timestamp */

    /* -------------------------------------------------------------- */
    /* Backend private expansion data */
    guint32  idata;   /* used by the sql backend for kvp management */
}  QofInstancePrivate;

#define GET_PRIVATE(o)  \
    ((QofInstancePrivate*)g_type_instance_get_private((GTypeInstance*)o, QOF_TYPE_INSTANCE))

G_DEFINE_TYPE_WITH_PRIVATE(QofInstance, qof_instance, G_TYPE_OBJECT);
QOF_GOBJECT_FINALIZE(qof_instance);
#undef G_PARAM_READWRITE
#define G_PARAM_READWRITE static_cast<GParamFlags>(G_PARAM_READABLE | G_PARAM_WRITABLE)

static void qof_instance_get_property (GObject         *object,
                                       guint            prop_id,
                                       GValue          *value,
                                       GParamSpec      *pspec);
static void qof_instance_set_property (GObject         *object,
                                       guint            prop_id,
                                       const GValue    *value,
                                       GParamSpec      *pspec);
static void qof_instance_dispose(GObject*);
static void qof_instance_class_init(QofInstanceClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    object_class->finalize = qof_instance_finalize;
    object_class->dispose = qof_instance_dispose;
    object_class->set_property = qof_instance_set_property;
    object_class->get_property = qof_instance_get_property;

    klass->get_display_name = NULL;
    klass->refers_to_object = NULL;
    klass->get_typed_referring_object_list = NULL;

    g_object_class_install_property
    (object_class,
     PROP_GUID,
     g_param_spec_boxed ("guid",
                         "Object GncGUID",
                         "The object Globally Unique ID.",
                         GNC_TYPE_GUID,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_COLLECTION,
     g_param_spec_pointer ("collection",
                           "Object Collection",
                           "A collection of like objects of which this "
                           "particular object is amember.  E.g.. A "
                           "collection of accounts, or a collection of "
                           "splits.",
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_BOOK,
     g_param_spec_object ("book",
                          "Object Book",
                          "The book that contains this object.",
                          QOF_TYPE_BOOK,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_LAST_UPDATE,
     g_param_spec_pointer ("last-update",
                           "Object Last Update",
                           "A pointer to the last time this object was "
                           "updated.  This value is present for use by "
                           "backends and shouldnot be written by other "
                           "code.",
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_EDITLEVEL,
     g_param_spec_int ("editlevel",
                       "Object Edit Level",
                       "The object edit level.",
                       0, G_MAXINT32, 0,
                       G_PARAM_READABLE));

    g_object_class_install_property
    (object_class,
     PROP_DESTROYING,
     g_param_spec_boolean ("destroying",
                           "Object Destroying",
                           "This flag is set to TRUE if the object is "
                           "about to be destroyed.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_DIRTY,
     g_param_spec_boolean ("dirty",
                           "Object Dirty",
                           "This flag is set to TRUE if the object has "
                           "unsaved changes.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_INFANT,
     g_param_spec_boolean ("infant",
                           "Object Infant",
                           "This flag is set to TRUE if the object has "
                           "never been added to a book.  This implies "
                           "that its destruction does not affect the "
                           "state of the book, and therefore the saved "
                           "state of the data file.",
                           FALSE,
                           G_PARAM_READABLE));

    g_object_class_install_property
    (object_class,
     PROP_VERSION,
     g_param_spec_int ("version",
                       "Version",
                       "The version number of the current instance state.",
                       0,
                       G_MAXINT32,
                       0,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_VERSION_CHECK,
     g_param_spec_uint ("version-check",
                        "Version Check",
                        "The version check number of the current instance state.",
                        0,
                        G_MAXUINT32,
                        0,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (object_class,
     PROP_EDITLEVEL,
     g_param_spec_uint ("idata",
                        "Object IData",
                        "Per instance backend private data.",
                        0, G_MAXUINT32, 0,
                        G_PARAM_READWRITE));
}

static void
qof_instance_init (QofInstance *inst)
{
    QofInstancePrivate *priv;

    priv = GET_PRIVATE(inst);
    priv->book = NULL;
    inst->kvp_data = new KvpFrame;
    priv->last_update = 0;
    priv->editlevel = 0;
    priv->do_free = FALSE;
    priv->dirty = FALSE;
    priv->infant = TRUE;
}

void
qof_instance_init_data (QofInstance *inst, QofIdType type, QofBook *book)
{
    QofInstancePrivate *priv;
    QofCollection *col;
    QofIdType col_type;

    g_return_if_fail(QOF_IS_INSTANCE(inst));
    priv = GET_PRIVATE(inst);
    g_return_if_fail(!priv->book);

    priv->book = book;
    col = qof_book_get_collection (book, type);
    g_return_if_fail(col != NULL);

    /* XXX We passed redundant info to this routine ... but I think that's
     * OK, it might eliminate programming errors. */

    col_type = qof_collection_get_type(col);
    if (g_strcmp0(col_type, type))
    {
        PERR ("attempt to insert \"%s\" into \"%s\"", type, col_type);
        return;
    }
    priv = GET_PRIVATE(inst);
    inst->e_type = static_cast<QofIdType>(CACHE_INSERT (type));

    do
    {
        guid_replace(&priv->guid);

        if (NULL == qof_collection_lookup_entity (col, &priv->guid))
            break;

        PWARN("duplicate id created, trying again");
    }
    while (1);

    priv->collection = col;

    qof_collection_insert_entity (col, inst);
}

static void
qof_instance_dispose (GObject *instp)
{
    QofInstancePrivate *priv;
    QofInstance* inst = QOF_INSTANCE(instp);

    priv = GET_PRIVATE(instp);
    if (!priv->collection)
        return;
    qof_collection_remove_entity(inst);

    CACHE_REMOVE(inst->e_type);
    inst->e_type = NULL;

    G_OBJECT_CLASS(qof_instance_parent_class)->dispose(instp);
}

static void
qof_instance_finalize_real (GObject *instp)
{
    QofInstancePrivate *priv;
    QofInstance* inst = QOF_INSTANCE(instp);

    delete inst->kvp_data;
    inst->kvp_data = nullptr;

    priv = GET_PRIVATE(inst);
    priv->editlevel = 0;
    priv->do_free = FALSE;
    priv->dirty = FALSE;
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
qof_instance_get_property (GObject         *object,
                           guint            prop_id,
                           GValue          *value,
                           GParamSpec      *pspec)
{
    QofInstance *inst;
    QofInstancePrivate *priv;

    g_return_if_fail(QOF_IS_INSTANCE(object));

    inst = QOF_INSTANCE(object);
    priv = GET_PRIVATE(inst);

    switch (prop_id)
    {
    case PROP_GUID:
        g_value_set_boxed(value, &priv->guid);
        break;
    case PROP_COLLECTION:
        g_value_set_pointer(value, priv->collection);
        break;
    case PROP_BOOK:
        g_value_take_object(value, priv->book);
        break;
    case PROP_LAST_UPDATE:
        g_value_set_pointer(value, &priv->last_update);
        break;
    case PROP_EDITLEVEL:
        g_value_set_int(value, priv->editlevel);
        break;
    case PROP_DESTROYING:
        g_value_set_boolean(value, priv->do_free);
        break;
    case PROP_DIRTY:
        g_value_set_boolean(value, qof_instance_get_dirty(inst));
        break;
    case PROP_INFANT:
        g_value_set_boolean(value, priv->infant);
        break;
    case PROP_VERSION:
        g_value_set_int(value, priv->version);
        break;
    case PROP_VERSION_CHECK:
        g_value_set_uint(value, priv->version_check);
        break;
    case PROP_IDATA:
        g_value_set_uint(value, priv->idata);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
qof_instance_set_property (GObject         *object,
                           guint            prop_id,
                           const GValue    *value,
                           GParamSpec      *pspec)
{
    QofInstance *inst;
    Time64 t;

    g_return_if_fail(QOF_IS_INSTANCE(object));

    inst = QOF_INSTANCE(object);

    switch (prop_id)
    {
    case PROP_GUID:
        qof_instance_set_guid(inst,
			      static_cast<GncGUID*>(g_value_get_boxed(value)));
        break;
    case PROP_COLLECTION:
        qof_instance_set_collection(inst, static_cast<QofCollection*>(g_value_get_pointer(value)));
        break;
    case PROP_BOOK:
        qof_instance_set_book(inst,
			      static_cast<QofBook*>(g_value_get_object(value)));
        break;
    case PROP_LAST_UPDATE:
        t = *(static_cast<Time64*>(g_value_get_pointer(value)));
        qof_instance_set_last_update(inst, t.t);
        break;
    case PROP_DESTROYING:
        qof_instance_set_destroying(inst, g_value_get_boolean(value));
        break;
    case PROP_DIRTY:
        qof_instance_set_dirty(inst);
        break;
    case PROP_VERSION:
        qof_instance_set_version(inst, g_value_get_int(value));
        break;
    case PROP_VERSION_CHECK:
        qof_instance_set_version_check(inst, g_value_get_uint(value));
        break;
    case PROP_IDATA:
        qof_instance_set_idata(inst, g_value_get_uint(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

const GncGUID *
qof_instance_get_guid (gconstpointer inst)
{
    QofInstancePrivate *priv;

    if (!inst) return NULL;
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), guid_null());
    priv = GET_PRIVATE(inst);
    return &(priv->guid);
}

const GncGUID *
qof_entity_get_guid (gconstpointer ent)
{
    return ent ? qof_instance_get_guid(ent) : guid_null();
}

void
qof_instance_set_guid (gpointer ptr, const GncGUID *guid)
{
    QofInstancePrivate *priv;
    QofInstance *inst;
    QofCollection *col;

    g_return_if_fail(QOF_IS_INSTANCE(ptr));

    inst = QOF_INSTANCE(ptr);
    priv = GET_PRIVATE(inst);
    if (guid_equal (guid, &priv->guid))
        return;

    col = priv->collection;
    qof_collection_remove_entity(inst);
    priv->guid = *guid;
    qof_collection_insert_entity(col, inst);
}

void
qof_instance_copy_guid (gpointer to, gconstpointer from)
{
    g_return_if_fail(QOF_IS_INSTANCE(to));
    g_return_if_fail(QOF_IS_INSTANCE(from));

    GET_PRIVATE(to)->guid = GET_PRIVATE(from)->guid;
}

gint
qof_instance_guid_compare(gconstpointer ptr1, gconstpointer ptr2)
{
    const QofInstancePrivate *priv1, *priv2;

    g_return_val_if_fail(QOF_IS_INSTANCE(ptr1), -1);
    g_return_val_if_fail(QOF_IS_INSTANCE(ptr2),  1);

    priv1 = GET_PRIVATE(ptr1);
    priv2 = GET_PRIVATE(ptr2);

    return guid_compare(&priv1->guid, &priv2->guid);
}

QofCollection *
qof_instance_get_collection (gconstpointer ptr)
{

    g_return_val_if_fail(QOF_IS_INSTANCE(ptr), NULL);
    return GET_PRIVATE(ptr)->collection;
}

void
qof_instance_set_collection (gconstpointer ptr, QofCollection *col)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr));
    GET_PRIVATE(ptr)->collection = col;
}

QofBook *
qof_instance_get_book (gconstpointer inst)
{
    if (!inst) return NULL;
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), NULL);
    return GET_PRIVATE(inst)->book;
}

void
qof_instance_set_book (gconstpointer inst, QofBook *book)
{
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    GET_PRIVATE(inst)->book = book;
}

void
qof_instance_copy_book (gpointer ptr1, gconstpointer ptr2)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr1));
    g_return_if_fail(QOF_IS_INSTANCE(ptr2));

    GET_PRIVATE(ptr1)->book = GET_PRIVATE(ptr2)->book;
}

gboolean
qof_instance_books_equal (gconstpointer ptr1, gconstpointer ptr2)
{
    const QofInstancePrivate *priv1, *priv2;

    g_return_val_if_fail(QOF_IS_INSTANCE(ptr1), FALSE);
    g_return_val_if_fail(QOF_IS_INSTANCE(ptr2), FALSE);

    priv1 = GET_PRIVATE(ptr1);
    priv2 = GET_PRIVATE(ptr2);

    return (priv1->book == priv2->book);
}

/* Watch out: This function is still used (as a "friend") in src/import-export/aqb/gnc-ab-kvp.c */
KvpFrame*
qof_instance_get_slots (const QofInstance *inst)
{
    if (!inst) return NULL;
    return inst->kvp_data;
}

void
qof_instance_set_slots (QofInstance *inst, KvpFrame *frm)
{
    QofInstancePrivate *priv;

    if (!inst) return;

    priv = GET_PRIVATE(inst);
    if (inst->kvp_data && (inst->kvp_data != frm))
    {
        delete inst->kvp_data;
    }

    priv->dirty = TRUE;
    inst->kvp_data = frm;
}

void
qof_instance_set_last_update (QofInstance *inst, time64 t)
{
    if (!inst) return;
    GET_PRIVATE(inst)->last_update = t;
}

gint
qof_instance_get_editlevel (gconstpointer ptr)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(ptr), 0);
    return GET_PRIVATE(ptr)->editlevel;
}

void qof_instance_increase_editlevel (gpointer ptr)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr));
    GET_PRIVATE(ptr)->editlevel++;
}

void qof_instance_decrease_editlevel (gpointer ptr)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr));
    GET_PRIVATE(ptr)->editlevel--;
}

void qof_instance_reset_editlevel (gpointer ptr)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr));
    GET_PRIVATE(ptr)->editlevel = 0;
}

int
qof_instance_version_cmp (const QofInstance *left, const QofInstance *right)
{
    QofInstancePrivate *lpriv, *rpriv;

    if (!left && !right) return 0;
    if (!left) return -1;
    if (!right) return +1;

    lpriv = GET_PRIVATE(left);
    rpriv = GET_PRIVATE(right);
    return lpriv->last_update < rpriv->last_update ? -1 :
        lpriv->last_update > rpriv->last_update ? 1 : 0;
}

gboolean
qof_instance_get_destroying (gconstpointer ptr)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(ptr), FALSE);
    return GET_PRIVATE(ptr)->do_free;
}

void
qof_instance_set_destroying (gpointer ptr, gboolean value)
{
    g_return_if_fail(QOF_IS_INSTANCE(ptr));
    GET_PRIVATE(ptr)->do_free = value;
}

gboolean
qof_instance_get_dirty_flag (gconstpointer ptr)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(ptr), FALSE);
    return GET_PRIVATE(ptr)->dirty;
}

void
qof_instance_set_dirty_flag (gconstpointer inst, gboolean flag)
{
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    GET_PRIVATE(inst)->dirty = flag;
}

void
qof_instance_mark_clean (QofInstance *inst)
{
    if (!inst) return;
    GET_PRIVATE(inst)->dirty = FALSE;
}

void
qof_instance_print_dirty (const QofInstance *inst, gpointer dummy)
{
    QofInstancePrivate *priv;

    priv = GET_PRIVATE(inst);
    if (priv->dirty)
    {
        gchar guidstr[GUID_ENCODING_LENGTH+1];
        guid_to_string_buff(&priv->guid, guidstr);
        printf("%s instance %s is dirty.\n", inst->e_type, guidstr);
    }
}

gboolean
qof_instance_get_dirty (QofInstance *inst)
{
    QofInstancePrivate *priv;
    QofCollection *coll;

    if (!inst)
    {
        return FALSE;
    }

    priv = GET_PRIVATE(inst);
    return priv->dirty;
}

void
qof_instance_set_dirty(QofInstance* inst)
{
    QofInstancePrivate *priv;
    QofCollection *coll;

    priv = GET_PRIVATE(inst);
    priv->dirty = TRUE;
}

gboolean
qof_instance_get_infant(const QofInstance *inst)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), FALSE);
    return GET_PRIVATE(inst)->infant;
}

gint32
qof_instance_get_version (gconstpointer inst)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), 0);
    return GET_PRIVATE(inst)->version;
}

void
qof_instance_set_version (gpointer inst, gint32 vers)
{
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    GET_PRIVATE(inst)->version = vers;
}

void
qof_instance_copy_version (gpointer to, gconstpointer from)
{
    g_return_if_fail(QOF_IS_INSTANCE(to));
    g_return_if_fail(QOF_IS_INSTANCE(from));
    GET_PRIVATE(to)->version = GET_PRIVATE(from)->version;
}

guint32
qof_instance_get_version_check (gconstpointer inst)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), 0);
    return GET_PRIVATE(inst)->version_check;
}

void
qof_instance_set_version_check (gpointer inst, guint32 value)
{
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    GET_PRIVATE(inst)->version_check = value;
}

void
qof_instance_copy_version_check (gpointer to, gconstpointer from)
{
    g_return_if_fail(QOF_IS_INSTANCE(to));
    g_return_if_fail(QOF_IS_INSTANCE(from));
    GET_PRIVATE(to)->version_check = GET_PRIVATE(from)->version_check;
}

guint32 qof_instance_get_idata (gconstpointer inst)
{
    if (!inst)
    {
        return 0;
    }
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), 0);
    return GET_PRIVATE(inst)->idata;
}

void qof_instance_set_idata(gpointer inst, guint32 idata)
{
    if (!inst)
    {
        return;
    }
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    GET_PRIVATE(inst)->idata = idata;
}

/* ========================================================== */

/* Returns a displayable name to represent this object */
gchar* qof_instance_get_display_name(const QofInstance* inst)
{
    g_return_val_if_fail( inst != NULL, NULL );

    if ( QOF_INSTANCE_GET_CLASS(inst)->get_display_name != NULL )
    {
        return QOF_INSTANCE_GET_CLASS(inst)->get_display_name(inst);
    }
    else
    {
        /* Not implemented - return default string */
        return g_strdup_printf("Object %s %p",
                               qof_collection_get_type(qof_instance_get_collection(inst)),
                               inst);
    }
}

typedef struct
{
    const QofInstance* inst;
    GList* list;
} GetReferringObjectHelperData;

static void
get_referring_object_instance_helper(QofInstance* inst, gpointer user_data)
{
    QofInstance** pInst = (QofInstance**)user_data;

    if (*pInst == NULL)
    {
        *pInst = inst;
    }
}

static void
get_referring_object_helper(QofCollection* coll, gpointer user_data)
{
    QofInstance* first_instance = NULL;
    GetReferringObjectHelperData* data = (GetReferringObjectHelperData*)user_data;

    qof_collection_foreach(coll, get_referring_object_instance_helper, &first_instance);

    if (first_instance != NULL)
    {
        GList* new_list = qof_instance_get_typed_referring_object_list(first_instance, data->inst);
        data->list = g_list_concat(data->list, new_list);
    }
}

/* Returns a list of objects referring to this object */
GList* qof_instance_get_referring_object_list(const QofInstance* inst)
{
    GetReferringObjectHelperData data;

    g_return_val_if_fail( inst != NULL, NULL );

    /* scan all collections */
    data.inst = inst;
    data.list = NULL;

    qof_book_foreach_collection(qof_instance_get_book(inst),
                                get_referring_object_helper,
                                &data);
    return data.list;
}

static void
get_typed_referring_object_instance_helper(QofInstance* inst, gpointer user_data)
{
    GetReferringObjectHelperData* data = (GetReferringObjectHelperData*)user_data;

    if (qof_instance_refers_to_object(inst, data->inst))
    {
        data->list = g_list_prepend(data->list, inst);
    }
}

GList*
qof_instance_get_referring_object_list_from_collection(const QofCollection* coll, const QofInstance* ref)
{
    GetReferringObjectHelperData data;

    g_return_val_if_fail( coll != NULL, NULL );
    g_return_val_if_fail( ref != NULL, NULL );

    data.inst = ref;
    data.list = NULL;

    qof_collection_foreach(coll, get_typed_referring_object_instance_helper, &data);
    return data.list;
}

GList*
qof_instance_get_typed_referring_object_list(const QofInstance* inst, const QofInstance* ref)
{
    g_return_val_if_fail( inst != NULL, NULL );
    g_return_val_if_fail( ref != NULL, NULL );

    if ( QOF_INSTANCE_GET_CLASS(inst)->get_typed_referring_object_list != NULL )
    {
        return QOF_INSTANCE_GET_CLASS(inst)->get_typed_referring_object_list(inst, ref);
    }
    else
    {
        /* Not implemented - by default, loop through all objects of this object's type and check
           them individually. */
        QofCollection* coll;

        coll = qof_instance_get_collection(inst);
        return qof_instance_get_referring_object_list_from_collection(coll, ref);
    }
}

/* Check if this object refers to a specific object */
gboolean qof_instance_refers_to_object(const QofInstance* inst, const QofInstance* ref)
{
    g_return_val_if_fail( inst != NULL, FALSE );
    g_return_val_if_fail( ref != NULL, FALSE );

    if ( QOF_INSTANCE_GET_CLASS(inst)->refers_to_object != NULL )
    {
        return QOF_INSTANCE_GET_CLASS(inst)->refers_to_object(inst, ref);
    }
    else
    {
        /* Not implemented - default = NO */
        return FALSE;
    }
}

/* g_object_set/get wrappers */
void
qof_instance_get (const QofInstance *inst, const gchar *first_prop, ...)
{
    va_list ap;
    g_return_if_fail (QOF_IS_INSTANCE (inst));

    va_start (ap, first_prop);
    g_object_get_valist (G_OBJECT (inst), first_prop, ap);
    va_end (ap);
}

void
qof_instance_set (QofInstance *inst, const gchar *first_prop, ...)
{
    va_list ap;
    g_return_if_fail (QOF_IS_INSTANCE (inst));

    qof_instance_set_dirty (inst);
    va_start (ap, first_prop);
    g_object_set_valist (G_OBJECT (inst), first_prop, ap);
    va_end (ap);
}


/* =================================================================== */
/* Entity edit and commit utilities */
/* =================================================================== */

gboolean
qof_begin_edit (QofInstance *inst)
{
    QofInstancePrivate *priv;

    if (!inst) return FALSE;

    priv = GET_PRIVATE(inst);
    priv->editlevel++;
    if (1 < priv->editlevel) return FALSE;
    if (0 >= priv->editlevel)
        priv->editlevel = 1;

    auto be = qof_book_get_backend(priv->book);
    if (be)
        be->begin(inst);
    else
        priv->dirty = TRUE;

    return TRUE;
}

gboolean qof_commit_edit (QofInstance *inst)
{
    QofInstancePrivate *priv;

    if (!inst) return FALSE;

    priv = GET_PRIVATE(inst);
    priv->editlevel--;
    if (0 < priv->editlevel) return FALSE;

    if (0 > priv->editlevel)
    {
        PERR ("unbalanced call - resetting (was %d)", priv->editlevel);
        priv->editlevel = 0;
    }
    return TRUE;
}

gboolean
qof_commit_edit_part2(QofInstance *inst,
                      void (*on_error)(QofInstance *, QofBackendError),
                      void (*on_done)(QofInstance *),
                      void (*on_free)(QofInstance *))
{
    QofInstancePrivate *priv;

    priv = GET_PRIVATE(inst);

    if (priv->dirty &&
        !(priv->infant && priv->do_free)) {
      qof_collection_mark_dirty(priv->collection);
      qof_book_mark_session_dirty(priv->book);
    }

    /* See if there's a backend.  If there is, invoke it. */
    auto be = qof_book_get_backend(priv->book);
    if (be)
    {
        QofBackendError errcode;

        /* clear errors */
        do
        {
            errcode = be->get_error();
        }
        while (errcode != ERR_BACKEND_NO_ERR);

        be->commit(inst);
        errcode = be->get_error();
        if (errcode != ERR_BACKEND_NO_ERR)
        {
            /* XXX Should perform a rollback here */
            priv->do_free = FALSE;

            /* Push error back onto the stack */
            be->set_error (errcode);
            if (on_error)
                on_error(inst, errcode);
            return FALSE;
        }
        /* XXX the backend commit code should clear dirty!! */
        priv->dirty = FALSE;
    }
    priv->infant = FALSE;

    if (priv->do_free)
    {
        if (on_free)
            on_free(inst);
        return TRUE;
    }

    if (on_done)
        on_done(inst);
    return TRUE;
}

gboolean
qof_instance_has_kvp (QofInstance *inst)
{
    return (inst->kvp_data != NULL && !inst->kvp_data->empty());
}

void qof_instance_set_path_kvp (QofInstance * inst, GValue const * value, std::vector<std::string> const & path)
{
    delete inst->kvp_data->set_path (path, kvp_value_from_gvalue (value));
}

void
qof_instance_set_kvp (QofInstance * inst, GValue const * value, unsigned count, ...)
{
    std::vector<std::string> path;
    va_list args;
    va_start (args, count);
    for (unsigned i{0}; i < count; ++i)
        path.push_back (va_arg (args, char const *));
    va_end (args);
    delete inst->kvp_data->set_path (path, kvp_value_from_gvalue (value));
}

void qof_instance_get_path_kvp (QofInstance * inst, GValue * value, std::vector<std::string> const & path)
{
    auto temp = gvalue_from_kvp_value (inst->kvp_data->get_slot (path));
    if (G_IS_VALUE (temp))
    {
        if (G_IS_VALUE (value))
            g_value_unset (value);
        g_value_init (value, G_VALUE_TYPE (temp));
        g_value_copy (temp, value);
        gnc_gvalue_free (temp);
    }
}

void
qof_instance_get_kvp (QofInstance * inst, GValue * value, unsigned count, ...)
{
    std::vector<std::string> path;
    va_list args;
    va_start (args, count);
    for (unsigned i{0}; i < count; ++i)
        path.push_back (va_arg (args, char const *));
    va_end (args);
    auto temp = gvalue_from_kvp_value (inst->kvp_data->get_slot (path));
    if (G_IS_VALUE (temp))
    {
        if (G_IS_VALUE (value))
            g_value_unset (value);
        g_value_init (value, G_VALUE_TYPE (temp));
        g_value_copy (temp, value);
        gnc_gvalue_free (temp);
    }
}

void
qof_instance_copy_kvp (QofInstance *to, const QofInstance *from)
{
    delete to->kvp_data;
    to->kvp_data = new KvpFrame(*from->kvp_data);
}

void
qof_instance_swap_kvp (QofInstance *a, QofInstance *b)
{
    std::swap(a->kvp_data, b->kvp_data);
}

int
qof_instance_compare_kvp (const QofInstance *a, const QofInstance *b)
{
    return compare(a->kvp_data, b->kvp_data);
}

char*
qof_instance_kvp_as_string (const QofInstance *inst)
{
    //The std::string is a local temporary and doesn't survive this function.
    return g_strdup(inst->kvp_data->to_string().c_str());
}

void
qof_instance_kvp_add_guid (const QofInstance *inst, const char* path,
                           time64 time, const char *key,
                           const GncGUID *guid)
{
    g_return_if_fail (inst->kvp_data != NULL);

    auto container = new KvpFrame;
    Time64 t{time};
    container->set({key}, new KvpValue(const_cast<GncGUID*>(guid)));
    container->set({"date"}, new KvpValue(t));
    delete inst->kvp_data->set_path({path}, new KvpValue(container));
}

inline static gboolean
kvp_match_guid (KvpValue *v, std::vector<std::string> const & path, const GncGUID *guid)
{
    if (v->get_type() != KvpValue::Type::FRAME)
        return FALSE;
    auto frame = v->get<KvpFrame*>();
    auto val = frame->get_slot(path);
    if (val == nullptr || val->get_type() != KvpValue::Type::GUID)
        return FALSE;
    auto this_guid = val->get<GncGUID*>();

    return guid_equal (this_guid, guid);
}

gboolean
qof_instance_kvp_has_guid (const QofInstance *inst, const char *path,
                           const char* key, const GncGUID *guid)
{
    g_return_val_if_fail (inst->kvp_data != NULL, FALSE);
    g_return_val_if_fail (guid != NULL, FALSE);

    auto v = inst->kvp_data->get_slot({path});
    if (v == nullptr) return FALSE;

    switch (v->get_type())
    {
    case KvpValue::Type::FRAME:
        return kvp_match_guid (v, {key}, guid);
        break;
    case KvpValue::Type::GLIST:
    {
        auto list = v->get<GList*>();
        for (auto node = list; node != NULL; node = node->next)
        {
            auto val = static_cast<KvpValue*>(node->data);
            if (kvp_match_guid (val, {key}, guid))
            {
                return TRUE;
            }
        }
        break;
    }
    default:
        PWARN ("Instance KVP on path %s contains the wrong type.", path);
        break;
    }
    return FALSE;
}

void
qof_instance_kvp_remove_guid (const QofInstance *inst, const char *path,
                          const char *key, const GncGUID *guid)
{
    g_return_if_fail (inst->kvp_data != NULL);
    g_return_if_fail (guid != NULL);

    auto v = inst->kvp_data->get_slot({path});
    if (v == NULL) return;

    switch (v->get_type())
    {
    case KvpValue::Type::FRAME:
        if (kvp_match_guid (v, {key}, guid))
        {
            delete inst->kvp_data->set_path({path}, nullptr);
            delete v;
        }
        break;
    case KvpValue::Type::GLIST:
    {
        auto list = v->get<GList*>();
        for (auto node = list; node != nullptr; node = node->next)
        {
            auto val = static_cast<KvpValue*>(node->data);
            if (kvp_match_guid (val, {key}, guid))
            {
                list = g_list_delete_link (list, node);
                v->set(list);
                delete val;
                break;
            }
        }
        break;
    }
    default:
        PWARN ("Instance KVP on path %s contains the wrong type.", path);
        break;
    }
    return;
}

void
qof_instance_kvp_merge_guids (const QofInstance *target,
                              const QofInstance *donor, const char *path)
{
    g_return_if_fail (target != NULL);
    g_return_if_fail (donor != NULL);

    if (! qof_instance_has_slot (donor, path)) return;
    auto v = donor->kvp_data->get_slot({path});
    if (v == NULL) return;

    auto target_val = target->kvp_data->get_slot({path});
    switch (v->get_type())
    {
    case KvpValue::Type::FRAME:
        if (target_val)
            target_val->add(v);
        else
            target->kvp_data->set_path({path}, v);
        donor->kvp_data->set({path}, nullptr); //Contents moved, Don't delete!
        break;
    case KvpValue::Type::GLIST:
        if (target_val)
        {
            auto list = target_val->get<GList*>();
            list = g_list_concat(list, v->get<GList*>());
            target_val->set(list);
        }
        else
            target->kvp_data->set({path}, v);
        donor->kvp_data->set({path}, nullptr); //Contents moved, Don't delete!
        break;
    default:
        PWARN ("Instance KVP on path %s contains the wrong type.", path);
        break;
    }
}

bool qof_instance_has_path_slot (QofInstance const * inst, std::vector<std::string> const & path)
{
    return inst->kvp_data->get_slot (path) != nullptr;
}

gboolean
qof_instance_has_slot (const QofInstance *inst, const char *path)
{
    return inst->kvp_data->get_slot({path}) != NULL;
}

void qof_instance_slot_path_delete (QofInstance const * inst, std::vector<std::string> const & path)
{
    delete inst->kvp_data->set (path, nullptr);
}

void
qof_instance_slot_delete (QofInstance const *inst, char const * path)
{
    delete inst->kvp_data->set ({path}, nullptr);
}

void qof_instance_slot_path_delete_if_empty (QofInstance const * inst, std::vector<std::string> const & path)
{
    auto slot = inst->kvp_data->get_slot (path);
    if (slot)
    {
        auto frame = slot->get <KvpFrame*> ();
        if (frame && frame->empty())
            delete inst->kvp_data->set (path, nullptr);
    }
}

void
qof_instance_slot_delete_if_empty (QofInstance const *inst, char const * path)
{
    auto slot = inst->kvp_data->get_slot ({path});
    if (slot)
    {
        auto frame = slot->get <KvpFrame*> ();
        if (frame && frame->empty ())
            delete inst->kvp_data->set ({path}, nullptr);
    }
}

std::vector <std::pair <std::string, KvpValue*>>
qof_instance_get_slots_prefix (QofInstance const * inst, std::string const & prefix)
{
    std::vector <std::pair <std::string, KvpValue*>> ret;
    inst->kvp_data->for_each_slot_temp ([&prefix, &ret] (std::string const & key, KvpValue * val) {
        if (key.find (prefix) == 0)
            ret.emplace_back (key, val);
    });
    return ret;
}

namespace {
struct wrap_param
{
    void (*proc)(const char*, const GValue*, void*);
    void *user_data;
};
}

static void
wrap_gvalue_function (const char* key, KvpValue *val, wrap_param & param)
{
    GValue *gv;
    if (val->get_type() != KvpValue::Type::FRAME)
        gv = gvalue_from_kvp_value(val);
    else
    {
        gv = g_slice_new0 (GValue);
        g_value_init (gv, G_TYPE_STRING);
        g_value_set_string (gv, nullptr);
    }
    param.proc(key, gv, param.user_data);
    g_slice_free (GValue, gv);
}

void
qof_instance_foreach_slot (const QofInstance *inst, const char* head, const char* category,
                           void (*proc)(const char*, const GValue*, void*), void* data)
{
    std::vector<std::string> path {head};
    if (category)
        path.emplace_back (category);

    auto slot = inst->kvp_data->get_slot(path);
    if (slot == nullptr || slot->get_type() != KvpValue::Type::FRAME)
        return;
    auto frame = slot->get<KvpFrame*>();
    wrap_param new_data {proc, data};
    frame->for_each_slot_temp(&wrap_gvalue_function, new_data);
}

/* ========================== END OF FILE ======================= */


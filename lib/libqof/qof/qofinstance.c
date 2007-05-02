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
 */

#include "config.h"
#include <glib.h>
#include "qof.h"
#include "kvp-util-p.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "qofinstance-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

/* ========================================================== */

enum {
    LAST_SIGNAL
};

enum {
    PROP_0,
    PROP_TYPE,
    PROP_GUID,
    PROP_COLLECTION,
    PROP_BOOK,
    PROP_KVP_DATA,
    PROP_LAST_UPDATE,
    PROP_EDITLEVEL,
    PROP_DESTROYING,
    PROP_DIRTY,
    PROP_INFANT,
};

typedef struct QofInstancePrivate
{
//    QofIdType        e_type;    /**<	Entity type */
//    GUID             guid;      /**< GUID for the entity */
    QofCollection  *collection; /**< Entity collection */

    /* The entity_table in which this instance is stored */
//    QofBook * book;

    /* kvp_data is a key-value pair database for storing arbirtary
     * information associated with this instance.  
     * See src/engine/kvp_doc.txt for a list and description of the 
     * important keys. */
//    KvpFrame *kvp_data;

    /*  Timestamp used to track the last modification to this 
     *  instance.  Typically used to compare two versions of the
     *  same object, to see which is newer.  When used with the 
     *  SQL backend, this field is reserved for SQL use, to compare
     *  the version in local memory to the remote, server version.
     */
    Timespec last_update;

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
}  QofInstancePrivate;

#define GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), QOF_TYPE_INSTANCE,  QofInstancePrivate))

QOF_GOBJECT_GET_TYPE(QofInstance, qof_instance, G_TYPE_OBJECT, {});
QOF_GOBJECT_FINALIZE(qof_instance);

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

    g_type_class_add_private(klass, sizeof(QofInstancePrivate));

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
         PROP_KVP_DATA,
         g_param_spec_pointer ("kvp-data",
                               "Object KVP Data",
                               "A pointer to the key-value data associated "
                               "with this object.",
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
                           0, G_MAXINT, 0,
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
}

static void
qof_instance_init (QofInstance *inst)
{
        QofInstancePrivate *priv;

        priv = GET_PRIVATE(inst);
	inst->book = NULL;
	inst->kvp_data = kvp_frame_new();
	priv->last_update.tv_sec = 0;
	priv->last_update.tv_nsec = -1;
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
	g_return_if_fail(!inst->book);

	inst->book = book;
	col = qof_book_get_collection (book, type);
        g_return_if_fail(col != NULL);
  
        /* XXX We passed redundant info to this routine ... but I think that's
         * OK, it might eliminate programming errors. */

        col_type = qof_collection_get_type(col);
        if (safe_strcmp(col_type, type)) {
            PERR ("attempt to insert \"%s\" into \"%s\"", type, col_type);
            return;
        }
        inst->e_type = CACHE_INSERT (type);

        do {
          guid_new(&inst->guid);

          if (NULL == qof_collection_lookup_entity (col, &inst->guid))
            break;

          PWARN("duplicate id created, trying again");
        } while(1);
 
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

	kvp_frame_delete (inst->kvp_data);
	inst->kvp_data = NULL;

        priv = GET_PRIVATE(inst);
        priv->editlevel = 0;
        priv->do_free = FALSE;
        priv->dirty = FALSE;
}

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

    switch (prop_id) {
        case PROP_GUID:
            g_value_set_boxed(value, &inst->guid);
            break;
        case PROP_COLLECTION:
            g_value_set_pointer(value, priv->collection);
            break;
        case PROP_BOOK:
            g_value_set_object(value, inst->book);
            break;
        case PROP_KVP_DATA:
            g_value_set_pointer(value, inst->kvp_data);
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
    QofInstancePrivate *priv;
    Timespec *ts;

    g_return_if_fail(QOF_IS_INSTANCE(object));

    inst = QOF_INSTANCE(object);
    priv = GET_PRIVATE(inst);

    switch (prop_id) {
        case PROP_GUID:
            qof_instance_set_guid(inst, g_value_get_boxed(value));
            break;
        case PROP_COLLECTION:
            qof_instance_set_collection(inst, g_value_get_pointer(value));
            break;
        case PROP_BOOK:
            qof_instance_set_book(inst, g_value_get_object(value));
            break;
        case PROP_KVP_DATA:
            qof_instance_set_slots(inst, g_value_get_pointer(value));
            break;
        case PROP_LAST_UPDATE:
            ts = g_value_get_pointer(value);
            qof_instance_set_last_update(inst, *ts);
            break;
        case PROP_DESTROYING:
            qof_instance_set_destroying(inst, g_value_get_boolean(value));
            break;
        case PROP_DIRTY:
            qof_instance_set_dirty(inst);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
            break;
    }
}

const GUID *
qof_instance_get_guid (const QofInstance *inst)
{
    QofInstancePrivate *priv;

    if (!inst) return guid_null();
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), guid_null());
    priv = GET_PRIVATE(inst);
    return &(inst->guid);
}

void
qof_instance_set_guid (QofInstance *ent, const GUID *guid)
{
    QofInstancePrivate *priv;
    QofCollection *col;

    priv = GET_PRIVATE(ent);
    if (guid_equal (guid, &ent->guid))
        return;

    col = priv->collection;
    qof_collection_remove_entity (ent);
    ent->guid = *guid;
    qof_collection_insert_entity (col, ent);
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
qof_instance_get_book (const QofInstance *inst)
{
    if (!inst) return NULL;
    return inst->book;
}

void
qof_instance_set_book (gconstpointer inst, QofBook *book)
{
    g_return_if_fail(QOF_IS_INSTANCE(inst));
    QOF_INSTANCE(inst)->book = book;
}

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
    if (inst->kvp_data && (inst->kvp_data != frm)) {
        kvp_frame_delete(inst->kvp_data);
    }

    priv->dirty = TRUE;
    inst->kvp_data = frm;
}

Timespec
qof_instance_get_last_update (const QofInstance *inst)
{
    if (!inst) {
        Timespec ts = {0,-1};
        return ts;
    }
    return GET_PRIVATE(inst)->last_update;
}

void
qof_instance_set_last_update (QofInstance *inst, Timespec ts)
{
    if (!inst) return;
    GET_PRIVATE(inst)->last_update = ts;
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

gboolean
qof_instance_check_edit(const  QofInstance *inst)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), FALSE);
    return (GET_PRIVATE(inst)->editlevel > 0);
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
        if (lpriv->last_update.tv_sec  < rpriv->last_update.tv_sec) return -1;
        if (lpriv->last_update.tv_sec  > rpriv->last_update.tv_sec) return +1;
        if (lpriv->last_update.tv_nsec < rpriv->last_update.tv_nsec) return -1;
        if (lpriv->last_update.tv_nsec > rpriv->last_update.tv_nsec) return +1;
	return 0;
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
  if(!inst) return;
  GET_PRIVATE(inst)->dirty = FALSE;
}

void
qof_instance_print_dirty (const QofInstance *inst, gpointer dummy)
{
    QofInstancePrivate *priv;

    priv = GET_PRIVATE(inst);
    if (priv->dirty) {
        printf("%s instance %s is dirty.\n", inst->e_type,
               guid_to_string(&inst->guid));
    }
}

gboolean
qof_instance_get_dirty (QofInstance *inst)
{
        QofInstancePrivate *priv;
	QofCollection *coll;

	if (!inst) { return FALSE; }

        priv = GET_PRIVATE(inst);
	if (qof_get_alt_dirty_mode())
	  return priv->dirty;
	coll = priv->collection;
	if(qof_collection_is_dirty(coll)) { return priv->dirty; }
	priv->dirty = FALSE;
	return FALSE;
}

void
qof_instance_set_dirty(QofInstance* inst)
{
        QofInstancePrivate *priv;
	QofCollection *coll;

        priv = GET_PRIVATE(inst);
        priv->dirty = TRUE;
	if (!qof_get_alt_dirty_mode()) {
	  coll = priv->collection;
	  qof_collection_mark_dirty(coll);
	}
}

gboolean
qof_instance_get_infant(const QofInstance *inst)
{
    g_return_val_if_fail(QOF_IS_INSTANCE(inst), FALSE);
    return GET_PRIVATE(inst)->infant;
}

/* ========================================================== */

void
qof_instance_gemini (QofInstance *to, const QofInstance *from)
{
  QofInstancePrivate *from_priv, *to_priv, *fb_priv, *tb_priv;
  time_t now;

  g_return_if_fail(QOF_IS_INSTANCE(to));
  g_return_if_fail(QOF_IS_INSTANCE(from));

  from_priv = GET_PRIVATE(from);
  to_priv = GET_PRIVATE(to);
  fb_priv = GET_PRIVATE(QOF_INSTANCE(from)->book);
  tb_priv = GET_PRIVATE(QOF_INSTANCE(to)->book);

  /* Books must differ for a gemini to be meaningful */
  if (from->book == to->book)
    return;

  now = time(0);

  /* Make a note of where the copy came from */
  gnc_kvp_bag_add (to->kvp_data, "gemini", now,
                                  "inst_guid", &from->guid,
                                  "book_guid", &from->book->inst.guid,
                                  NULL);
  gnc_kvp_bag_add (from->kvp_data, "gemini", now,
                                  "inst_guid", &to->guid,
                                  "book_guid", &to->book->inst.guid,
                                  NULL);

  to_priv->dirty = TRUE;
}

QofInstance *
qof_instance_lookup_twin (const QofInstance *src, QofBook *target_book)
{
	QofCollection *col;
	KvpFrame *fr;
	GUID * twin_guid;
	QofInstance * twin;

	if (!src || !target_book) return NULL;
	ENTER (" ");

	fr = gnc_kvp_bag_find_by_guid (src->kvp_data, "gemini",
	                             "book_guid", &target_book->inst.guid);

	twin_guid = kvp_frame_get_guid (fr, "inst_guid");

	col = qof_book_get_collection (target_book, src->e_type);
	twin = (QofInstance *) qof_collection_lookup_entity (col, twin_guid);

	LEAVE (" found twin=%p", twin);
	return twin;
}

/* =================================================================== */
/* Entity edit and commit utilities */
/* =================================================================== */

gboolean
qof_begin_edit (QofInstance *inst)
{
    QofInstancePrivate *priv;
    QofBackend * be;

    if (!inst) return FALSE;

    priv = GET_PRIVATE(inst);
    priv->editlevel++;
    if (1 < priv->editlevel) return FALSE;
    if (0 >= priv->editlevel) 
        priv->editlevel = 1;

    be = qof_book_get_backend(inst->book);
    if (be && qof_backend_begin_exists(be))
        qof_backend_run_begin(be, inst);
    else
        priv->dirty = TRUE; 
  
    return TRUE;
}

gboolean qof_commit_edit (QofInstance *inst)
{
    QofInstancePrivate *priv;
    QofBackend * be;

    if (!inst) return FALSE;

    priv = GET_PRIVATE(inst);
    priv->editlevel--;
    if (0 < priv->editlevel) return FALSE;

    if ((0 == priv->editlevel) && priv->dirty) {
        be = qof_book_get_backend(inst->book);
        if (be && qof_backend_commit_exists(be)) {
            qof_backend_run_commit(be, inst);
        }
    }
    if (0 > priv->editlevel) { 
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
    QofBackend * be;
    gboolean dirty;

    priv = GET_PRIVATE(inst);
    dirty = priv->dirty;

    /* See if there's a backend.  If there is, invoke it. */
    be = qof_book_get_backend(inst->book);
    if (be && qof_backend_commit_exists(be)) {
        QofBackendError errcode;
        
        /* clear errors */
        do {
            errcode = qof_backend_get_error(be);
        } while (ERR_BACKEND_NO_ERR != errcode);

        qof_backend_run_commit(be, inst);
        errcode = qof_backend_get_error(be);
        if (ERR_BACKEND_NO_ERR != errcode) {
            /* XXX Should perform a rollback here */
            priv->do_free = FALSE;

            /* Push error back onto the stack */
            qof_backend_set_error (be, errcode);
            if (on_error)
                on_error(inst, errcode);
            return FALSE;
        }   
        /* XXX the backend commit code should clear dirty!! */
        priv->dirty = FALSE;
    }
    if (dirty && qof_get_alt_dirty_mode() && 
        !(priv->infant && priv->do_free)) {
      qof_collection_mark_dirty(priv->collection);
      qof_book_mark_dirty(inst->book);
    }
    priv->infant = FALSE;

    if (priv->do_free) {
        if (on_free)
            on_free(inst);
        return TRUE;
    }
    
    if (on_done)
        on_done(inst);
    return TRUE;
}

/* ========================== END OF FILE ======================= */

// Local Variables:
// mode: c
// indent-tabs-mode: nil
// c-block-comment-prefix: "* "
// eval: (c-add-style "gnc" '("k&r" (c-basic-offset . 4) (c-offsets-alist (case-label . +))) t)
// End:

/********************************************************************\
 * gncOrder.c -- the Core Business Order                            *
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
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <qofinstance-p.h>
#include <stdint.h>

#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncOrder.h"
#include "gncOrderP.h"
#include "gncOwner.h"
#include "gncOwnerP.h"

struct _gncOrder
{
    QofInstance inst;

    char *	id;
    char *	notes;
    gboolean 	active;

    char *	reference;
    char *	printname;
    GncOwner	owner;
    GList *	entries;
    time64 	opened;
    time64 	closed;
};

struct _gncOrderClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_ID_ORDER

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!g_strcmp0 (member, str)) return; \
	gncOrderBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

static inline void mark_order (GncOrder *order);
void mark_order (GncOrder *order)
{
    qof_instance_set_dirty(&order->inst);
    qof_event_gen (&order->inst, QOF_EVENT_MODIFY, NULL);
}

/* =============================================================== */

enum
{
    PROP_0,
    PROP_ID,		/* Table */
    PROP_NOTES,		/* Table */
    PROP_REFERENCE,	/* Table */
    PROP_ACTIVE,	/* Table */
    PROP_DATE_OPENED,	/* Table */
    PROP_DATE_CLOSED,	/* Table */
//  PROP_OWNER_TYPE,	/* Table */
//  PROP_OWNER,		/* Table */
};

/* GObject Initialization */
G_DEFINE_TYPE(GncOrder, gnc_order, QOF_TYPE_INSTANCE);

static void
gnc_order_init(GncOrder* order)
{
    order->closed = INT64_MAX;
}

static void
gnc_order_dispose(GObject *orderp)
{
    G_OBJECT_CLASS(gnc_order_parent_class)->dispose(orderp);
}

static void
gnc_order_finalize(GObject* orderp)
{
    G_OBJECT_CLASS(gnc_order_parent_class)->dispose(orderp);
}

static void
gnc_order_get_property (GObject         *object,
                        guint            prop_id,
                        GValue          *value,
                        GParamSpec      *pspec)
{
    GncOrder *priv;

    g_return_if_fail(GNC_IS_ORDER(object));

    priv = GNC_ORDER(object);
    switch (prop_id)
    {
    case PROP_ID:
        g_value_set_string(value, priv->id);
        break;
    case PROP_NOTES:
        g_value_set_string(value, priv->notes);
        break;
    case PROP_ACTIVE:
        g_value_set_boolean(value, priv->active);
        break;
    case PROP_DATE_OPENED:
        g_value_set_boxed(value, &priv->opened);
        break;
    case PROP_DATE_CLOSED:
        g_value_set_boxed(value, &priv->closed);
        break;
    case PROP_REFERENCE:
        g_value_set_string(value, priv->reference);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_order_set_property (GObject         *object,
                        guint            prop_id,
                        const GValue          *value,
                        GParamSpec      *pspec)
{
    GncOrder *order;

    g_return_if_fail(GNC_IS_ORDER(object));

    order = GNC_ORDER(object);
    g_assert (qof_instance_get_editlevel(order));

    switch (prop_id)
    {
    case PROP_ID:
        gncOrderSetID(order, g_value_get_string(value));
        break;
    case PROP_NOTES:
        gncOrderSetNotes(order, g_value_get_string(value));
        break;
    case PROP_ACTIVE:
        gncOrderSetActive(order, g_value_get_boolean(value));
        break;
    case PROP_DATE_OPENED:
        gncOrderSetDateOpened(order, g_value_get_int64(value));
        break;
    case PROP_DATE_CLOSED:
        gncOrderSetDateClosed(order, g_value_get_int64(value));
        break;
    case PROP_REFERENCE:
        gncOrderSetReference(order, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** Returns a list of my type of object which refers to an object.  For example, when called as
        qof_instance_get_typed_referring_object_list(taxtable, account);
    it will return the list of taxtables which refer to a specific account.  The result should be the
    same regardless of which taxtable object is used.  The list must be freed by the caller but the
    objects on the list must not.
 */
static GList*
impl_get_typed_referring_object_list(const QofInstance* inst, const QofInstance* ref)
{
    /* Refers to nothing */
    return NULL;
}

static void
gnc_order_class_init (GncOrderClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    QofInstanceClass* qof_class = QOF_INSTANCE_CLASS(klass);

    gobject_class->dispose = gnc_order_dispose;
    gobject_class->finalize = gnc_order_finalize;
    gobject_class->set_property = gnc_order_set_property;
    gobject_class->get_property = gnc_order_get_property;

    qof_class->get_display_name = NULL;
    qof_class->refers_to_object = NULL;
    qof_class->get_typed_referring_object_list = impl_get_typed_referring_object_list;

    g_object_class_install_property
    (gobject_class,
     PROP_ID,
     g_param_spec_string ("id",
                          "Order ID",
                          "The order id is an arbitrary string "
                          "assigned by the user to identify the order.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_NOTES,
     g_param_spec_string ("notes",
                          "Order Notes",
                          "The order notes is an arbitrary string "
                          "assigned by the user to provide notes about "
                          "this order.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ACTIVE,
     g_param_spec_boolean ("active",
                           "Active",
                           "TRUE if the order is active.  FALSE if inactive.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_DATE_OPENED,
     g_param_spec_boxed("date-opened",
                        "Date Opened",
                        "The date the order was opened.",
                        GNC_TYPE_TIME64,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_DATE_CLOSED,
     g_param_spec_boxed("date-closed",
                        "Date Closed",
                        "The date the order was closed.",
                        GNC_TYPE_TIME64,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_REFERENCE,
     g_param_spec_string ("reference",
                          "Order Reference",
                          "The order reference is an arbitrary string "
                          "assigned by the user to provide a reference for "
                          "this order.",
                          NULL,
                          G_PARAM_READWRITE));
}

/* Create/Destroy Functions */
GncOrder *gncOrderCreate (QofBook *book)
{
    GncOrder *order;

    if (!book) return NULL;

    order = g_object_new (GNC_TYPE_ORDER, NULL);
    qof_instance_init_data (&order->inst, _GNC_MOD_NAME, book);

    order->id = CACHE_INSERT ("");
    order->notes = CACHE_INSERT ("");
    order->reference = CACHE_INSERT ("");

    order->active = TRUE;

    qof_event_gen (&order->inst, QOF_EVENT_CREATE, NULL);

    return order;
}

void gncOrderDestroy (GncOrder *order)
{
    if (!order) return;
    qof_instance_set_destroying(order, TRUE);
    gncOrderCommitEdit (order);
}

static void gncOrderFree (GncOrder *order)
{
    if (!order) return;

    qof_event_gen (&order->inst, QOF_EVENT_DESTROY, NULL);

    g_list_free (order->entries);
    CACHE_REMOVE (order->id);
    CACHE_REMOVE (order->notes);
    CACHE_REMOVE (order->reference);

    if (order->printname) g_free (order->printname);

    /* qof_instance_release (&order->inst); */
    g_object_unref (order);
}

/* =============================================================== */
/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id)
{
    if (!order || !id) return;
    SET_STR (order, order->id, id);
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetOwner (GncOrder *order, GncOwner *owner)
{
    if (!order || !owner) return;
    if (gncOwnerEqual (&order->owner, owner)) return;

    gncOrderBeginEdit (order);
    gncOwnerCopy (owner, &order->owner);
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetDateOpened (GncOrder *order, time64 date)
{
    if (!order) return;
    if (order->opened == date) return;
    gncOrderBeginEdit (order);
    order->opened = date;
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetDateClosed (GncOrder *order, time64 date)
{
    if (!order) return;
    if (order->closed == date) return;
    gncOrderBeginEdit (order);
    order->closed = date;
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetNotes (GncOrder *order, const char *notes)
{
    if (!order || !notes) return;
    SET_STR (order, order->notes, notes);
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetReference (GncOrder *order, const char *reference)
{
    if (!order || !reference) return;
    SET_STR (order, order->reference, reference);
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderSetActive (GncOrder *order, gboolean active)
{
    if (!order) return;
    if (order->active == active) return;
    gncOrderBeginEdit (order);
    order->active = active;
    mark_order (order);
    gncOrderCommitEdit (order);
}

/* =============================================================== */
/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry)
{
    GncOrder *old;

    if (!order || !entry) return;

    old = gncEntryGetOrder (entry);
    if (old == order) return;			/* I already own it */
    if (old) gncOrderRemoveEntry (old, entry);

    gncOrderBeginEdit (order);
    order->entries = g_list_insert_sorted (order->entries, entry,
                                           (GCompareFunc)gncEntryCompare);

    /* This will send out an event -- make sure we're attached */
    gncEntrySetOrder (entry, order);
    mark_order (order);
    gncOrderCommitEdit (order);
}

void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry)
{
    if (!order || !entry) return;

    gncOrderBeginEdit (order);
    gncEntrySetOrder (entry, NULL);
    order->entries = g_list_remove (order->entries, entry);
    mark_order (order);
    gncOrderCommitEdit (order);
}

/* Get Functions */

const char * gncOrderGetID (const GncOrder *order)
{
    if (!order) return NULL;
    return order->id;
}

GncOwner * gncOrderGetOwner (GncOrder *order)
{
    if (!order) return NULL;
    return &order->owner;
}

time64 gncOrderGetDateOpened (const GncOrder *order)
{
    if (!order) return INT64_MAX;
    return order->opened;
}

time64 gncOrderGetDateClosed (const GncOrder *order)
{
    if (!order) return INT64_MAX;
    return order->closed;
}

const char * gncOrderGetNotes (const GncOrder *order)
{
    if (!order) return NULL;
    return order->notes;
}

const char * gncOrderGetReference (const GncOrder *order)
{
    if (!order) return NULL;
    return order->reference;
}

gboolean gncOrderGetActive (const GncOrder *order)
{
    if (!order) return FALSE;
    return order->active;
}

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order)
{
    if (!order) return NULL;
    return order->entries;
}

gboolean gncOrderIsClosed (const GncOrder *order)
{
    if (!order) return FALSE;
    if (order->closed != INT64_MAX) return TRUE;
    return FALSE;
}

/* =============================================================== */

void gncOrderBeginEdit (GncOrder *order)
{
    qof_begin_edit(&order->inst);
}

static void gncOrderOnError (QofInstance *order, QofBackendError errcode)
{
    PERR("Order QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void gncOrderOnDone (QofInstance *order) {}

static void order_free (QofInstance *inst)
{
    GncOrder *order = (GncOrder *) inst;
    gncOrderFree (order);
}

void gncOrderCommitEdit (GncOrder *order)
{
    if (!qof_commit_edit (QOF_INSTANCE(order))) return;
    qof_commit_edit_part2 (&order->inst, gncOrderOnError,
                           gncOrderOnDone, order_free);
}

int gncOrderCompare (const GncOrder *a, const GncOrder *b)
{
    int compare;

    if (a == b) return 0;
    if (!a) return -1;
    if (!b) return 1;

    compare = g_strcmp0 (a->id, b->id);
    if (compare) return compare;

    if (a->opened != b->opened) return a->opened - b->opened;
    if (a->closed != b->closed) return a->closed - b->closed;

    return qof_instance_guid_compare(a, b);
}


/* =========================================================== */
/* Package-Private functions */

static const char *
_gncOrderPrintable (gpointer obj)
{
    GncOrder *order = obj;

    g_return_val_if_fail (order, NULL);

    if (qof_instance_get_dirty_flag(order) || order->printname == NULL)
    {
        if (order->printname) g_free (order->printname);

        order->printname =
            g_strdup_printf ("%s%s", order->id,
                             gncOrderIsClosed (order) ? _(" (closed)") : "");
    }

    return order->printname;
}

static QofObject gncOrderDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) _GNC_MOD_NAME,
    DI(.type_label        = ) "Order",
    DI(.create            = ) (gpointer)gncOrderCreate,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) NULL,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) _gncOrderPrintable,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncOrderRegister (void)
{
    static QofParam params[] =
    {
        { ORDER_ID, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetID, (QofSetterFunc)gncOrderSetID },
        { ORDER_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetReference, (QofSetterFunc)gncOrderSetReference },
        { ORDER_OWNER, GNC_ID_OWNER, (QofAccessFunc)gncOrderGetOwner, (QofSetterFunc)gncOrderSetOwner },
        { ORDER_OPENED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateOpened, (QofSetterFunc)gncOrderSetDateOpened },
        { ORDER_IS_CLOSED, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderIsClosed, NULL },
        { ORDER_CLOSED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateClosed, (QofSetterFunc)gncOrderSetDateClosed },
        { ORDER_NOTES, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetNotes, (QofSetterFunc)gncOrderSetNotes },
        { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderGetActive, (QofSetterFunc)gncOrderSetActive },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncOrderCompare, params);

    return qof_object_register (&gncOrderDesc);
}

gchar *gncOrderNextID (QofBook *book)
{
    return qof_book_increment_and_format_counter (book, _GNC_MOD_NAME);
}

/*
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/* GTK4 GListModel implementation for business owners. */
#include <config.h>

#include <gtk/gtk.h>

#include "gnc-tree-model-owner.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gobject-utils.h"
#include "gnc-ui-util.h"

struct _GncOwnerRow
{
    GObject parent_instance;
    GncOwner owner;
};

typedef struct _GncOwnerRow GncOwnerRow;
typedef struct _GncOwnerRowClass { GObjectClass parent_class; } GncOwnerRowClass;

GType gnc_owner_row_get_type (void);

#define GNC_TYPE_OWNER_ROW (gnc_owner_row_get_type ())
G_DEFINE_TYPE (GncOwnerRow, gnc_owner_row, G_TYPE_OBJECT)

static void gnc_owner_row_class_init (GncOwnerRowClass *klass) { (void)klass; }
static void gnc_owner_row_init (GncOwnerRow *row) { gncOwnerInitUndefined (&row->owner, NULL); }

struct _GncTreeModelOwner
{
    GObject parent_instance;
    QofBook *book;
    GncOwnerType owner_type;
    GListStore *rows;
    gint event_handler_id;
};

G_DEFINE_TYPE (GncTreeModelOwner, gnc_tree_model_owner, G_TYPE_OBJECT)

static void
reload_rows (GncTreeModelOwner *model)
{
    GList *owners, *node;

    g_return_if_fail (GNC_IS_TREE_MODEL_OWNER (model));
    owners = gncBusinessGetOwnerList (model->book,
                                      gncOwnerTypeToQofIdType (model->owner_type),
                                      TRUE);
    g_list_store_remove_all (model->rows);
    for (node = owners; node; node = node->next)
    {
        GncOwnerRow *row = g_object_new (GNC_TYPE_OWNER_ROW, NULL);
        gncOwnerCopy (node->data, &row->owner);
        g_list_store_append (model->rows, row);
        g_object_unref (row);
    }
    g_list_free_full (owners, (GDestroyNotify)gncOwnerFree);
}

static void
gnc_tree_model_owner_event_handler (QofInstance *entity, QofEventId event_type,
                                    GncTreeModelOwner *model, GncEventData *data)
{
    GncOwner owner;
    (void)event_type;
    (void)data;

    if (!GNC_IS_TREE_MODEL_OWNER (model) || !GNC_IS_OWNER (entity) ||
        qof_instance_get_book (entity) != model->book)
        return;
    qofOwnerSetEntity (&owner, entity);
    if (gncOwnerGetType (&owner) == model->owner_type)
        reload_rows (model);
}

static void
gnc_tree_model_owner_dispose (GObject *object)
{
    GncTreeModelOwner *model = GNC_TREE_MODEL_OWNER (object);
    if (model->event_handler_id)
    {
        qof_event_unregister_handler (model->event_handler_id);
        model->event_handler_id = 0;
    }
    g_clear_object (&model->rows);
    G_OBJECT_CLASS (gnc_tree_model_owner_parent_class)->dispose (object);
}

static void
gnc_tree_model_owner_class_init (GncTreeModelOwnerClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = gnc_tree_model_owner_dispose;
}

static void
gnc_tree_model_owner_init (GncTreeModelOwner *model)
{
    model->rows = g_list_store_new (GNC_TYPE_OWNER_ROW);
}

GncTreeModelOwner *
gnc_tree_model_owner_new (GncOwnerType owner_type)
{
    GncTreeModelOwner *model;

    model = g_object_new (GNC_TYPE_TREE_MODEL_OWNER, NULL);
    model->book = gnc_get_current_book ();
    model->owner_type = owner_type;
    reload_rows (model);
    model->event_handler_id = qof_event_register_handler (
        (QofEventHandler)gnc_tree_model_owner_event_handler, model);
    return model;
}

GListModel *
gnc_tree_model_owner_get_model (GncTreeModelOwner *model)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), NULL);
    return G_LIST_MODEL (model->rows);
}

GncOwner *
gnc_tree_model_owner_get_row_owner (GObject *row)
{
    g_return_val_if_fail (row && G_TYPE_CHECK_INSTANCE_TYPE (row, GNC_TYPE_OWNER_ROW), NULL);
    return &((GncOwnerRow *)row)->owner;
}

guint
gnc_tree_model_owner_find_owner (GncTreeModelOwner *model, const GncOwner *owner)
{
    guint n_items;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), GTK_INVALID_LIST_POSITION);
    if (!owner)
        return GTK_INVALID_LIST_POSITION;
    n_items = g_list_model_get_n_items (G_LIST_MODEL (model->rows));
    for (guint i = 0; i < n_items; i++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (model->rows), i);
        gboolean match = gncOwnerEqual (gnc_tree_model_owner_get_row_owner (row), owner);
        g_object_unref (row);
        if (match)
            return i;
    }
    return GTK_INVALID_LIST_POSITION;
}

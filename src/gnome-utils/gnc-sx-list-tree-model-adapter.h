/*
 * gnc-sx-list-tree-model-adapter.h
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#ifndef _GNC_SX_LIST_TREE_MODEL_ADAPTER_H
#define _GNC_SX_LIST_TREE_MODEL_ADAPTER_H

#include "config.h"
#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include "gnc-sx-instance-model.h"

G_BEGIN_DECLS

#define GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER	       (gnc_sx_list_tree_model_adapter_get_type ())
#define GNC_SX_LIST_TREE_MODEL_ADAPTER(obj)	       (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapter))
#define GNC_SX_LIST_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapterClass))
#define GNC_IS_SX_LIST_TREE_MODEL_ADAPTER(obj)	       (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_LIST_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER))
#define GNC_SX_LIST_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapterClass))

typedef struct _GncSxListTreeModelAdapter GncSxListTreeModelAdapter;
typedef struct _GncSxListTreeModelAdapterClass GncSxListTreeModelAdapterClass;

// model columns
enum
{
    SXLTMA_COL_NAME = 0,
    SXLTMA_COL_ENABLED,
    SXLTMA_COL_FREQUENCY,
    SXLTMA_COL_LAST_OCCUR,
    SXLTMA_COL_NEXT_OCCUR
};

GType gnc_sx_list_tree_model_adapter_get_type(void);
GncSxListTreeModelAdapter* gnc_sx_list_tree_model_adapter_new(GncSxInstanceModel *instances);

GncSxInstances* gnc_sx_list_tree_model_adapter_get_sx_instances(GncSxListTreeModelAdapter *model, GtkTreeIter *iter);

G_END_DECLS

#endif // _GNC_SX_LIST_TREE_MODEL_ADAPTER_H

/*
 * gnc-tree-model.c -- base implementation for a tree model in
 *                     Gnucash.  This only implements the object, not
 *                     the model interface.
 *
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
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

#include <config.h>

#include <gtk/gtk.h>
#include <string.h>

#include "gnc-tree-model.h"
#include "gnc-gobject-utils.h"
#include "gnc-engine.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_model_constructed (GObject *object);
static void gnc_tree_model_finalize (GObject *object);

/** The instance private data for a generic tree model. */
typedef struct GncTreeModelPrivate
{
    gpointer dummy;
} GncTreeModelPrivate;

G_DEFINE_TYPE_WITH_CODE(GncTreeModel, gnc_tree_model, G_TYPE_OBJECT,
		        G_ADD_PRIVATE(GncTreeModel))

#define GNC_TREE_MODEL_GET_PRIVATE(o)  \
   ((GncTreeModelPrivate*)gnc_tree_model_get_instance_private((GncTreeModel*)o))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

static void
gnc_tree_model_class_init (GncTreeModelClass *klass)
{
    GObjectClass *o_class;

    o_class = G_OBJECT_CLASS (klass);

    /* GObject signals */
    o_class->constructed = gnc_tree_model_constructed;
    o_class->finalize = gnc_tree_model_finalize;
}

static void
gnc_tree_model_init (GncTreeModel *model)
{
}

/** The object has been fully constructed.
 * This function adds the object to the tracking system.
 *
 *  @param obj The new object instance created by the object
 *  system.
 */
static void
gnc_tree_model_constructed (GObject *obj)
{
    ENTER("model %p", obj);

    gnc_gobject_tracking_remember(obj);

    G_OBJECT_CLASS (gnc_tree_model_parent_class)->constructed (obj);

    LEAVE(" ");
}

static void
gnc_tree_model_finalize (GObject *object)
{
    ENTER("model %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL (object));

    gnc_gobject_tracking_forget(object);

    G_OBJECT_CLASS (gnc_tree_model_parent_class)->finalize (object);
    LEAVE(" ");
}

/*
 * gnc-tree-model.h -- base implementation for a tree model in
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiTreeModel GnuCash Tree Model
    @{ */
/** @file gnc-tree-model.h
    @brief GtkTreeModel implementation for a generic gnucash tree.
    @author David Hampton <hampton@employees.org>
*/

#ifndef __GNC_TREE_MODEL_H
#define __GNC_TREE_MODEL_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL            (gnc_tree_model_get_type ())
G_DECLARE_DERIVABLE_TYPE (GncTreeModel, gnc_tree_model, GNC, TREE_MODEL, GObject)

#define GNC_TREE_MODEL_NAME            "GncTreeModel"

/* typedefs & structures */

/** The class data structure for a generic tree model. */
struct _GncTreeModelClass
{
    GObjectClass g_object;	/**< The parent object data. */
};

G_END_DECLS

#endif /* __GNC_TREE_MODEL_H */

/** @} */
/** @} */

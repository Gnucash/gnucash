/*
 * gnc-tree-model-owner.h -- GtkTreeModel implementation to
 *	display owners in a GtkTreeView.
 *
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiTreeModel GnuCash Tree Model
    @{ */
/** @file gnc-tree-model-owner.h
    @brief GtkTreeModel implementation for gnucash owner tree.
    @author Geert Janssens <geert@kobaltwit.be>
*/

#ifndef __GNC_TREE_MODEL_OWNER_H
#define __GNC_TREE_MODEL_OWNER_H

#include <gtk/gtk.h>
#include "gnc-tree-model.h"

#include "gncOwner.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_OWNER            (gnc_tree_model_owner_get_type ())
#define GNC_TREE_MODEL_OWNER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_OWNER, GncTreeModelOwner))
#define GNC_TREE_MODEL_OWNER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_OWNER, GncTreeModelOwnerClass))
#define GNC_IS_TREE_MODEL_OWNER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_OWNER))
#define GNC_IS_TREE_MODEL_OWNER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_OWNER))
#define GNC_TREE_MODEL_OWNER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_OWNER, GncTreeModelOwnerClass))
#define GNC_TREE_MODEL_OWNER_NAME            "GncTreeModelOwner"


typedef enum
{
    GNC_TREE_MODEL_OWNER_COL_NAME,
    GNC_TREE_MODEL_OWNER_COL_TYPE,
    GNC_TREE_MODEL_OWNER_COL_ID,
    GNC_TREE_MODEL_OWNER_COL_CURRENCY,
    GNC_TREE_MODEL_OWNER_COL_ADDRESS_NAME,
    GNC_TREE_MODEL_OWNER_COL_ADDRESS_1,
    GNC_TREE_MODEL_OWNER_COL_ADDRESS_2,
    GNC_TREE_MODEL_OWNER_COL_ADDRESS_3,
    GNC_TREE_MODEL_OWNER_COL_ADDRESS_4,
    GNC_TREE_MODEL_OWNER_COL_PHONE,
    GNC_TREE_MODEL_OWNER_COL_FAX,
    GNC_TREE_MODEL_OWNER_COL_EMAIL,
    GNC_TREE_MODEL_OWNER_COL_BALANCE,
    GNC_TREE_MODEL_OWNER_COL_BALANCE_REPORT,
    GNC_TREE_MODEL_OWNER_COL_NOTES,
    GNC_TREE_MODEL_OWNER_COL_ACTIVE,

    GNC_TREE_MODEL_OWNER_COL_LAST_VISIBLE = GNC_TREE_MODEL_OWNER_COL_ACTIVE,

    /* internal hidden columns */
    GNC_TREE_MODEL_OWNER_COL_COLOR_BALANCE,

    GNC_TREE_MODEL_OWNER_NUM_COLUMNS
} GncTreeModelOwnerColumn;

/* typedefs & structures */

/** The instance data structure for an owner tree model. */
typedef struct
{
    GncTreeModel gnc_tree_model;    /**< The parent object data. */
    int stamp;                      /**< The state of the model. Any state
                                     *   change increments this number. */
} GncTreeModelOwner;


/** The class data structure for an owner tree model. */
typedef struct
{
    GncTreeModelClass gnc_tree_model;/**< The parent object data. */
} GncTreeModelOwnerClass;



/** Get the type of an owner tree plugin.
 *
 *  @return A GType.
 */
GType gnc_tree_model_owner_get_type (void);


/** @name Owner Tree Model Constructors
 @{ */

/** Create a new GtkTreeModel for manipulating gnucash owners.
 *
 *  @param root The owner group to put at the top level of the tree
 *  hierarchy. */
GtkTreeModel *gnc_tree_model_owner_new (GncOwnerType owner_type);
/** @} */


/** @name Owner Tree Model Get/Set Functions
  @{ */

/** Convert a model/iter pair to a gnucash owner.  This routine should
 *  only be called from an owner tree view filter function.  The
 *  model and iter values will be provided as part of the call to the
 *  filter.
 *
 *  @param model A pointer to the owner tree model.
 *
 *  @param iter A gtk_tree_iter corresponding to a single owner in
 *  the model.
 *
 *  @return A pointer to the corresponding owner.
 */
GncOwner *gnc_tree_model_owner_get_owner (GncTreeModelOwner *model,
        GtkTreeIter *iter);


/** Convert a model/owner pair into a gtk_tree_model_iter.  This
 *  routine should only be called from the file
 *  gnc-tree-view-owner.c.
 *
 *  @internal
 *
 *  @param model The model that an owner belongs to.
 *
 *  @param owner The owner to convert.
 *
 *  @param iter A pointer to an iter.  This iter will be rewritten to
 *  contain the results of the query.
 *
 *  @return TRUE if the owner was found and the iter filled
 *  in. FALSE otherwise.
 */
gboolean gnc_tree_model_owner_get_iter_from_owner (GncTreeModelOwner *model,
        GncOwner *owner,
        GtkTreeIter *iter);


/** Convert a model/owner pair into a gtk_tree_model_path.  This
 *  routine should only be called from the file
 *  gnc-tree-view-owner.c.
 *
 *  @internal
 *
 *  @param model The model that an owner belongs to.
 *
 *  @param owner The owner to convert.
 *
 *  @return A pointer to a path describing the owner.  It is the
 *  responsibility of the caller to free this path when done.
 */
GtkTreePath *gnc_tree_model_owner_get_path_from_owner (GncTreeModelOwner *model,
        GncOwner *owner);
/** @} */

G_END_DECLS

#endif /* __GNC_TREE_MODEL_OWNER_H */

/** @} */
/** @} */

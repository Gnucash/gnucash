/*
 * gnc-tree-model-account-types.h -- GtkTreeModel implementation
 *	to display account types in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2005, 2006 Chris Shoemaker <c.shoemaker@cox.net>
 * Copyright (C) 2006 Eskil Bylund <eskil.bylund@gmail.com>
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
 *     @{ */
/** @addtogroup GuiTreeModel
 * @{ */
/** @file gnc-tree-model-account-types.h
 *  @brief GtkTreeModel implementation to display account types in a
 *     GtkTreeView.
 *  @author Copyright (C) 2003 Jan Arne Petersen
 *  @author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 *
 */

#ifndef __GNC_TREE_MODEL_ACCOUNT_TYPES_H
#define __GNC_TREE_MODEL_ACCOUNT_TYPES_H

#include "Account.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES         (gnc_tree_model_account_types_get_type ())
#define GNC_TREE_MODEL_ACCOUNT_TYPES(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypes))
#define GNC_TREE_MODEL_ACCOUNT_TYPES_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypesClass))
#define GNC_IS_TREE_MODEL_ACCOUNT_TYPES(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES))
#define GNC_IS_TREE_MODEL_ACCOUNT_TYPES_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES))
#define GNC_TREE_MODEL_ACCOUNT_TYPES_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, GncTreeModelAccountTypesClass))

typedef enum
{
    GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE,
    GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME,
    GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED,
    GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS
} GncTreeModelAccountTypesColumn;

/* typedefs & structures */
typedef struct
{
    GObject gobject;
    int stamp;
} GncTreeModelAccountTypes;

typedef struct
{
    GObjectClass gobject;
} GncTreeModelAccountTypesClass;

/* function prototypes */
GType gnc_tree_model_account_types_get_type (void);

/* Choose one of two methods to use the GncTreeModelAccountTypes
   objects defined here, depending on how you want to deal with
   selection state.  Method 1 is simpler and light-weight, but Method
   2 is more flexible.

   Method 1: If you just want to allow selection of a subset of all
   account types while showing all account types, use
   gnc_tree_model_account_types_master() to get the treemodel.
   Connect it to your tree view and use
   gnc_tree_model_account_types_{sg}et_selection() to convert between
   bitmasks and GtkTreeView states.  No need to free the treemodel.

   Method 2: If you must store selection state in the model for some
   reason (maybe you want to use a checkbox column to indicate
   selection?)  then you need your own instance from
   gnc_tree_model_account_types_new().  Use
   gnc_tree_model_account_types_{gs}et_selected() to get and set the
   models "SELECTED" column values.  You must free the model when
   you're done with it.

*/

/*************** Method 1 functions ***************/

/* Returns a GtkTreeModelFilter that wraps the model. Deprecated and root
   account types will be filtered. Caller is responsible for
   ref/unref. */
GtkTreeModel * gnc_tree_model_account_types_valid (void);

/* Returns a GtkTreeModelFilter that wraps the model. Only account
   types specified by the 'types' bitmask are visible.  To force the
   visibility of deprecated account types, pass
   (xaccAccountTypesValid() | (1 << xaccAccountGetType(acct))).

   To get the GtkTreeModel that shows all account types, including
   deprecated account types, pass (-1).

   To get the GtkTreeModel that only shows non-deprecated account types,
   use gnc_tree_model_account_types_valid().

   Caller is responsible for ref/unref. */
GtkTreeModel * gnc_tree_model_account_types_filter_using_mask (guint32 types);

/* Update the set of the visibible account types in 'f_model' to 'types'. */
void gnc_tree_model_account_types_set_mask (GtkTreeModel *f_model,
        guint32 types);

/* Return the current set of the visibible account types. */
guint32 gnc_tree_model_account_types_get_mask (GtkTreeModel *f_model);

/* Return the bitmask of the account type enums reflecting the state
   of the tree selection.  If your view allows the selection of
   multiple account types, use must use this function to get the
   selection. */
guint32 gnc_tree_model_account_types_get_selection(GtkTreeSelection *sel);

/* Gets the selected account type.  Use the function if your view
   allows the selection of only one account type. If no types are
   selected, returns ACCT_TYPE_NONE.  If more than one type is
   selected, arbitrarily returns one of the selected types. */
GNCAccountType
gnc_tree_model_account_types_get_selection_single(GtkTreeSelection *sel);

/* Set the selection state of the tree selection to match the bitmask
   of account-type enums in 'selected'.  This will also scroll to a
   selected row in the TreeView.*/
void gnc_tree_model_account_types_set_selection(GtkTreeSelection *sel,
        guint32 selected);


/**************** Method 2 functions **************/

GtkTreeModel *gnc_tree_model_account_types_new(guint32 selected);

guint32 gnc_tree_model_account_types_get_selected(
    GncTreeModelAccountTypes * model);

void gnc_tree_model_account_types_set_selected(
    GncTreeModelAccountTypes * model, guint32 selected);


G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_H */

/** @} */
/** @} */

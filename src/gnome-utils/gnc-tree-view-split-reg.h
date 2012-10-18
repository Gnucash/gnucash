/********************************************************************\
 * gnc-tree-view-split-reg.h -- GtkTreeView implementation to       *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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


#ifndef __GNC_TREE_VIEW_SPLIT_REG_H
#define __GNC_TREE_VIEW_SPLIT_REG_H

#include <gtk/gtk.h>
#include "gnc-tree-view.h"

#include "gnc-tree-model-split-reg.h"
#include "gnc-ui-util.h"

G_BEGIN_DECLS

#define GNC_TYPE_TREE_VIEW_SPLIT_REG            (gnc_tree_view_split_reg_get_type ())
#define GNC_TREE_VIEW_SPLIT_REG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_SPLIT_REG, GncTreeViewSplitReg))
#define GNC_TREE_VIEW_SPLIT_REG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_SPLIT_REG, GncTreeViewSplitRegClass))
#define GNC_IS_TREE_VIEW_SPLIT_REG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_SPLIT_REG))
#define GNC_IS_TREE_VIEW_SPLIT_REG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_SPLIT_REG))
#define GNC_TREE_VIEW_SPLIT_REG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_SPLIT_REG, GncTreeViewSplitRegClass))

/* typedefs & structures */
typedef struct GncTreeViewSplitRegPrivate GncTreeViewSplitRegPrivate;

typedef struct
{
    GncTreeView gnc_tree_view;
    GncTreeViewSplitRegPrivate *priv;
    int stamp;

    GFunc moved_cb;
    gpointer moved_cb_data;

} GncTreeViewSplitReg;

typedef struct
{
    GncTreeViewClass gnc_tree_view;
} GncTreeViewSplitRegClass;

typedef enum {
    TOP,    //0
    TRANS1, //1
    TRANS2, //2
    SPLIT3, //3
}RowDepth;

/* Standard g_object type */
GType gnc_tree_view_split_reg_get_type (void);

GncTreeViewSplitReg *gnc_tree_view_split_reg_new_with_model (GncTreeModelSplitReg *model);

void gnc_tree_view_split_reg_block_selection (GncTreeViewSplitReg *view, gboolean block);

void gnc_tree_view_split_reg_default_selection (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_set_read_only (GncTreeViewSplitReg *view, gboolean read_only);

/*************************************************************************************/

Split * gnc_tree_view_split_reg_get_current_split (GncTreeViewSplitReg *view);

Split * gnc_tree_view_split_reg_get_blank_split (GncTreeViewSplitReg *view);

Split * gnc_tree_view_reg_get_current_trans_split (GncTreeViewSplitReg *view);

RowDepth gnc_tree_view_reg_get_selected_row_depth (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_delete_current_split (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_delete_current_trans (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_jump_to_blank (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_jump_to_split (GncTreeViewSplitReg *view, Split *split);

void gnc_tree_view_split_reg_reinit_trans (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_goto_rel_trans_row (GncTreeViewSplitReg *view, gint relative);

gboolean gnc_tree_view_split_reg_enter (GncTreeViewSplitReg *view);

gboolean gnc_tree_view_split_reg_current_trans_expanded (GncTreeViewSplitReg *view);

Transaction * gnc_tree_view_split_reg_get_current_trans (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_cancel_edit (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_expand_current_trans (GncTreeViewSplitReg *view, gboolean expand);

void gnc_tree_view_split_reg_moved_cb (GncTreeViewSplitReg *view, GFunc cb, gpointer cb_data);

G_END_DECLS

#endif /* __GNC_TREE_VIEW_SPLIT_REG_H */

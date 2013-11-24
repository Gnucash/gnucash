/********************************************************************\
 * gnc-tree-view-split-reg.h -- GtkTreeView implementation to       *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
 * Copyright (C) 2012 Robert Fewell                                 *
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
    GncTreeView                 gnc_tree_view;
    GncTreeViewSplitRegPrivate *priv;
    int                         stamp;

    GtkWidget                  *window;                   // Parent Window.
    GFunc                       uiupdate_cb;                 // Used for page gui update
    gpointer                    uiupdate_cb_data;            // Used for page gui update

    gchar                      *help_text;                // This is the help text to be displayed.
    gboolean                    reg_closing;              // This is set when closing the register.
    gboolean                    change_allowed;           // This is set when we allow the reconciled split to change.
    gboolean                    editing_now;              // This is set while editing of a cell.
    gboolean                    show_extra_dates;         // Show the entered date below the posted date and reconciled date on split row


} GncTreeViewSplitReg;

typedef struct
{
    GncTreeViewClass gnc_tree_view;

    /* This signal is emitted when we update the view */
    void (*update_signal) (GncTreeViewSplitReg *view, gpointer user_data);

    /* This signal is emitted when we update the help text */
    void (*help_signal) (GncTreeViewSplitReg *view, gpointer user_data);

} GncTreeViewSplitRegClass;

typedef enum {
    TOP,    //0
    TRANS1, //1
    TRANS2, //2
    SPLIT3, //3
}RowDepth;

typedef enum {
    COL_END_OF_LIST = -1,
    COL_CONTROL,      //0 TreeView Control columns
    COL_DATE,         //1
    COL_DUEDATE,      //2
    COL_NUMACT,       //3
    COL_DESCNOTES,    //4
    COL_TRANSFERVOID, //5
    COL_RECN,         //6
    COL_TYPE,         //7
    COL_VALUE,        //8
    COL_AMOUNT,       //9
    COL_AMTVAL,       //10
    COL_RATE,         //11
    COL_PRICE,        //12
    COL_DEBIT,        //13
    COL_CREDIT,       //14
    COL_BALANCE,      //15
    COL_STATUS,       //16
    COL_COMM,         //17
} ViewCol;

/* Standard g_object type */
GType gnc_tree_view_split_reg_get_type (void);

GncTreeViewSplitReg *gnc_tree_view_split_reg_new_with_model (GncTreeModelSplitReg *model);

void gnc_tree_view_split_reg_block_selection (GncTreeViewSplitReg *view, gboolean block);

void gnc_tree_view_split_reg_default_selection (GncTreeViewSplitReg *view);

gboolean gnc_tree_view_split_reg_set_format (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_set_read_only (GncTreeViewSplitReg *view, gboolean read_only);

void gnc_tree_view_split_reg_set_dirty_trans (GncTreeViewSplitReg *view, Transaction *trans);

Transaction * gnc_tree_view_split_reg_get_current_trans (GncTreeViewSplitReg *view);

Split * gnc_tree_view_split_reg_get_current_split (GncTreeViewSplitReg *view);

Transaction * gnc_tree_view_split_reg_get_dirty_trans (GncTreeViewSplitReg *view);

GtkTreePath * gnc_tree_view_split_reg_get_current_path (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_set_current_path (GncTreeViewSplitReg *view, GtkTreePath *mpath);

RowDepth gnc_tree_view_reg_get_selected_row_depth (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_set_uiupdate_cb (GncTreeViewSplitReg *view, GFunc cb, gpointer cb_data);

gboolean gnc_tree_view_split_reg_call_uiupdate_cb(GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_refresh_from_prefs (GncTreeViewSplitReg *view);

GtkWidget * gnc_tree_view_split_reg_get_parent (GncTreeViewSplitReg *view);

gboolean gnc_tree_view_split_reg_trans_expanded (GncTreeViewSplitReg *view, Transaction *trans);

void gnc_tree_view_split_reg_expand_trans (GncTreeViewSplitReg *view, Transaction *trans);

void gnc_tree_view_split_reg_collapse_trans (GncTreeViewSplitReg *view, Transaction *trans);

const char * gnc_tree_view_split_reg_get_credit_debit_string (GncTreeViewSplitReg *view, gboolean credit);

gnc_commodity * gnc_tree_view_split_reg_get_reg_commodity (GncTreeViewSplitReg *view);

/*************************************************************************************/
/* Get sort model path from the model path */
GtkTreePath * gnc_tree_view_split_reg_get_sort_path_from_model_path (GncTreeViewSplitReg *view, GtkTreePath *mpath);

/* Get model path from the sort model path */
GtkTreePath * gnc_tree_view_split_reg_get_model_path_from_sort_path (GncTreeViewSplitReg *view, GtkTreePath *spath);

/* Get model from the view */
GncTreeModelSplitReg * gnc_tree_view_split_reg_get_model_from_view (GncTreeViewSplitReg *view);

/* Scroll the view to the current transaction */
gboolean gnc_tree_view_split_reg_scroll_to_cell (GncTreeViewSplitReg *view);

/* Scroll the view to the blank split */
gboolean gnc_tree_view_split_reg_scroll_to_bsplit (GncTreeViewSplitReg *view);

/*************************************************************************************/

void gnc_tree_view_split_reg_delete_current_split (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_delete_current_trans (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_reinit_trans (GncTreeViewSplitReg *view);

gboolean gnc_tree_view_split_reg_enter (GncTreeViewSplitReg *view);

void gnc_tree_view_split_reg_cancel_edit (GncTreeViewSplitReg *view, gboolean reg_closing);

void gnc_tree_view_split_reg_finish_edit (GncTreeViewSplitReg *view);


G_END_DECLS

#endif /* __GNC_TREE_VIEW_SPLIT_REG_H */

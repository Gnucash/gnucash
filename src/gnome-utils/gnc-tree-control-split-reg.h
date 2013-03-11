/********************************************************************\
 * gnc-tree-control-split-reg.h -- GtkTreeView implementation       *
 *                     to display registers   in a GtkTreeView.     *
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


#ifndef __GNC_TREE_CONTROL_SPLIT_REG_H
#define __GNC_TREE_CONTROL_SPLIT_REG_H

#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-view-split-reg.h"

G_BEGIN_DECLS

/*****************************************************************************/

void gnc_tree_control_split_reg_exchange_rate (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_void_current_trans (GncTreeViewSplitReg *view, const char *reason);

void gnc_tree_control_split_reg_unvoid_current_trans (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_jump_to_blank (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_jump_to_split (GncTreeViewSplitReg *view, Split *split);

void gnc_tree_control_split_reg_cancel_edit (GncTreeViewSplitReg *view, gboolean reg_closing);

void gnc_tree_control_split_reg_goto_rel_trans_row (GncTreeViewSplitReg *view, gint relative);

void gnc_tree_control_split_reg_enter (GncTreeViewSplitReg *view, gboolean next_transaction);

void gnc_tree_control_split_reg_reinit (GncTreeViewSplitReg *view, gpointer data);

void gnc_tree_control_split_reg_reverse_current (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_delete (GncTreeViewSplitReg *view, gpointer data);

Transaction * gnc_tree_control_split_reg_get_blank_trans (GncTreeViewSplitReg *view);

Split * gnc_tree_control_split_reg_get_current_trans_split (GncTreeViewSplitReg *view);

Split * gnc_tree_control_split_reg_get_blank_split (GncTreeViewSplitReg *view);

gboolean gnc_tree_control_split_reg_duplicate_current (GncTreeViewSplitReg *view);

gboolean gnc_tree_control_split_reg_save (GncTreeViewSplitReg *view, gboolean reg_closing);

gboolean gnc_tree_control_split_reg_recn_change (GncTreeViewSplitReg *view);

gboolean gnc_tree_control_split_reg_recn_test (GncTreeViewSplitReg *view);

/*****************************************************************************/

/* Cut transaction and copy to clipboard */
void gnc_tree_control_split_reg_cut_trans (GncTreeViewSplitReg *view);

/* Copy transaction to clipboard */
void gnc_tree_control_split_reg_copy_trans (GncTreeViewSplitReg *view);

/* Paste transaction from clipboard */
void gnc_tree_control_split_reg_paste_trans (GncTreeViewSplitReg *view);

/* Copy the last transaction with given description to the blank transaction */
void gnc_tree_control_auto_complete (GncTreeViewSplitReg *view, Transaction *trans,  const gchar *new_text);

/*****************************************************************************/

/* Sort changed callback */
void gnc_tree_control_split_reg_sort_changed_cb (GtkTreeSortable *sortable, gpointer user_data);

/* Sort by date */
gint gnc_tree_control_split_reg_sort_by_date (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                       gpointer user_data);

/* Sort by Description / Notes / Memo */
gint gnc_tree_control_split_reg_sort_by_dnm (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                       gpointer user_data);

/* Sort function for Number / Action column */
gint gnc_tree_control_split_reg_sort_by_numact (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data);

/* Sort function for Reconcile column */
gint gnc_tree_control_split_reg_sort_by_recn (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data);

/* Sort function for transfer column */
gint gnc_tree_control_split_reg_sort_by_account (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data);

/* Sort function for debit / credit column */
gint gnc_tree_control_split_reg_sort_by_value (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data);

/*****************************************************************************/

G_END_DECLS

#endif /* __GNC_TREE_CONTROL_SPLIT_REG_H */

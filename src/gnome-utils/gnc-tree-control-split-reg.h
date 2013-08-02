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

gboolean gnc_tree_control_split_reg_trans_open_and_warn (GncTreeViewSplitReg *view, Transaction *trans);

gboolean gnc_tree_control_split_reg_trans_test_for_edit (GncTreeViewSplitReg *view, Transaction *trans);

/*****************************************************************************/

void gnc_tree_control_split_reg_exchange_rate (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_schedule_current_trans (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_void_current_trans (GncTreeViewSplitReg *view, const char *reason);

void gnc_tree_control_split_reg_unvoid_current_trans (GncTreeViewSplitReg *view);

gboolean gnc_tree_control_split_reg_jump_to_blank (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_jump_to (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean amount);

void gnc_tree_control_split_reg_cancel_edit (GncTreeViewSplitReg *view, gboolean reg_closing);

void gnc_tree_control_split_reg_goto_rel_trans_row (GncTreeViewSplitReg *view, gint relative);

void gnc_tree_control_split_reg_enter (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_reinit (GncTreeViewSplitReg *view, gpointer data);

void gnc_tree_control_split_reg_reverse_current (GncTreeViewSplitReg *view);

void gnc_tree_control_split_reg_delete (GncTreeViewSplitReg *view, gpointer data);

Transaction * gnc_tree_control_split_reg_get_blank_trans (GncTreeViewSplitReg *view);

Split * gnc_tree_control_split_reg_get_current_trans_split (GncTreeViewSplitReg *view);

Split * gnc_tree_control_split_reg_get_blank_split (GncTreeViewSplitReg *view);

gboolean gnc_tree_control_split_reg_duplicate_current (GncTreeViewSplitReg *view);

/** This implements the command of moving the current entry (where the
 * cursor is currently located) one row upwards or downwards (depending on the move_up parameter),
 * effectively swapping this row and the other row. If the other row
 * is empty (or it is the blank entry), nothing will happen.
 *
 * \param move_up If TRUE, the current entry is moved upwards,
 * otherwise downwards.
 * \return Whether the current entry has been moved into the queried direction
 */
gboolean gnc_tree_control_split_reg_move_current_entry_updown (GncTreeViewSplitReg *reg,
                                                            gboolean move_up);

/** Query whether the current entry (where the cursor is currently located)
 * can be moved one row upwards or downwards (depending on the move_up parameter).
 *
 * \param move_up If TRUE, it is asked whether the current entry can be moved upwards,
 * otherwise downwards.
 * \return Whether the current entry can be moved into the queried direction
 */
gboolean gnc_tree_control_split_reg_is_current_movable_updown (GncTreeViewSplitReg *view,
                                                               gboolean move_up);


gboolean gnc_tree_control_split_reg_save (GncTreeViewSplitReg *view, gboolean reg_closing);

gboolean gnc_tree_control_split_reg_recn_change (GncTreeViewSplitReg *view, GtkTreePath *spath);

gboolean gnc_tree_control_split_reg_recn_test (GncTreeViewSplitReg *view, GtkTreePath *spath);

gboolean gnc_tree_control_split_reg_balance_trans (GncTreeViewSplitReg *view, Transaction *trans);

Account * gnc_tree_control_split_reg_get_account_by_name (GncTreeViewSplitReg *view, const char *name);

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

G_END_DECLS

#endif /* __GNC_TREE_CONTROL_SPLIT_REG_H */

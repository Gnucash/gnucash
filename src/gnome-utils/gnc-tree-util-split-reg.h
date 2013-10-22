/********************************************************************\
 * gnc-tree-util-split-reg.h -- GtkTreeView implementation          *
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

#ifndef __GNC_TREE_UTIL_SPLIT_REG_H
#define __GNC_TREE_UTIL_SPLIT_REG_H

#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-view-split-reg.h"

G_BEGIN_DECLS


/*****************************************************************************/

gboolean gnc_tree_util_split_reg_has_rate (GncTreeViewSplitReg *view);

gboolean gnc_tree_util_split_reg_needs_conv_rate (GncTreeViewSplitReg *view,
                                    Transaction *trans, Account *acc);

const char * gnc_tree_util_split_reg_get_transfer_entry (Split *split, gboolean *is_multi);

const char * gnc_tree_util_split_reg_template_get_transfer_entry (Split *split);

const char * gnc_tree_util_split_reg_template_get_fdebt_entry (Split *split);

const char * gnc_tree_util_split_reg_template_get_fcred_entry (Split *split);

gchar * gnc_tree_util_split_reg_get_date_help (GDate *date);

void gnc_tree_util_split_reg_parse_date (GDate *parsed, const char *datestr);

gboolean gnc_tree_util_split_reg_rotate (GncTreeViewSplitReg *view, GtkTreeViewColumn *col,
                                         Transaction *trans, Split *split);

gboolean gnc_tree_util_split_reg_is_multi (Split *split);

gboolean gnc_tree_util_split_reg_needs_amount (GncTreeViewSplitReg *view, Split *split);

void gnc_tree_util_split_reg_set_value_for (GncTreeViewSplitReg *view, Transaction *trans,
                                            Split *split, gnc_numeric input, gboolean force);

void gnc_tree_util_split_reg_save_amount_values (GncTreeViewSplitReg *view, Transaction *trans,
                                                 Split *split, gnc_numeric input);

gnc_numeric gnc_tree_util_split_reg_get_value_for (GncTreeViewSplitReg *view, Transaction *trans,
                                                   Split *split, gboolean is_blank);

gboolean gnc_tree_util_split_reg_get_debcred_entry (GncTreeViewSplitReg *view,
                                                    Transaction *trans, Split *split,
                                                    gboolean is_blank,gnc_numeric *ret_num,
                                                    GNCPrintAmountInfo *ret_print_info);

void gnc_tree_util_set_number_for_input (GncTreeViewSplitReg *view, Transaction *trans,
                                         Split *split, gnc_numeric input, gint viewcol);

void gnc_tree_util_set_value_for_amount (GncTreeViewSplitReg *view, Transaction *trans,
                                         Split *split, gnc_numeric input);

gnc_numeric gnc_tree_util_get_rate_for (GncTreeViewSplitReg *view, Transaction *trans,
                                        Split *split, gboolean is_blank);

/*****************************************************************************/

G_END_DECLS

#endif /* __GNC_TREE_UTIL_SPLIT_REG_H */

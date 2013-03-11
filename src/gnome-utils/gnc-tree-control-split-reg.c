/********************************************************************\
 * gnc-tree-control-split-reg.c -- GtkTreeView implementation       *
 *                     to display registers in a GtkTreeView.       *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>
#include <string.h>

#include "gnc-tree-control-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gconf-utils.h"
#include "gnc-gdate-utils.h"
#include "dialog-utils.h"
#include "guile-util.h"
#include "dialog-dup-trans.h"

#include "Transaction.h"
#include "engine-helpers.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;


static SCM copied_item = SCM_UNDEFINED;

/** implementations *******************************************************/

/* Uses the scheme split copying routines */
static void
gtc_copy_split_onto_split (Split *from, Split *to, gboolean use_cut_semantics)
{
    SCM split_scm;

    if ((from == NULL) || (to == NULL))
        return;

    split_scm = gnc_copy_split(from, use_cut_semantics);
    if (split_scm == SCM_UNDEFINED)
        return;

    gnc_copy_split_scm_onto_split (split_scm, to, gnc_get_current_book ());
}

/* Uses the scheme transaction copying routines */
static void
gtc_copy_trans_onto_trans (Transaction *from, Transaction *to,
                          gboolean use_cut_semantics,
                          gboolean do_commit)
{
    SCM trans_scm;

    if ((from == NULL) || (to == NULL))
        return;

    trans_scm = gnc_copy_trans(from, use_cut_semantics);
    if (trans_scm == SCM_UNDEFINED)
        return;

    gnc_copy_trans_scm_onto_trans (trans_scm, to, do_commit,
                                  gnc_get_current_book ());
}


/*************************************************************************/


/* Read only dialoue */
static gboolean
gtc_is_trans_readonly_and_warn (const Transaction *trans)
{
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans) return FALSE;

    if (xaccTransIsReadonlyByPostedDate (trans))
    {
        dialog = gtk_message_dialog_new (NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book.  "
                        "This setting can be changed in File -> Properties -> Accounts."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new (NULL,
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                message, reason);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }
    return FALSE;
}


/* Cancel the edit and Rollback */
void
gnc_tree_control_split_reg_cancel_edit (GncTreeViewSplitReg *view, gboolean reg_closing)
{
    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    gnc_tree_view_split_reg_cancel_edit (view, reg_closing);
}


/* Amend the Exchange Rate of the transaction */
void
gnc_tree_control_split_reg_exchange_rate (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkWidget *window;
    Account *anchor;
    Transaction *trans;
    Split *split = NULL;
    Split *osplit = NULL;
    gnc_numeric value;
    gboolean expanded;
    gint depth;
    gint num_splits;
    const char *message;
    gnc_commodity *txn_com;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    trans = gnc_tree_view_split_reg_get_current_trans (view);
    expanded = gnc_tree_view_split_reg_trans_expanded (view, NULL);
    depth = gnc_tree_view_reg_get_selected_row_depth (view);
    num_splits = xaccTransCountSplits (trans);
    anchor = gnc_tree_model_split_reg_get_anchor (model);
    txn_com = xaccTransGetCurrency (trans);

    if (gtc_is_trans_readonly_and_warn (trans))
        return;

    if (num_splits < 2)
        return;

    window = gnc_tree_view_split_reg_get_parent (view);

    /* If the anchor commodity is not a currency, cancel */
    if (anchor && !gnc_commodity_is_currency (xaccAccountGetCommodity (anchor)))
    {
        message = _("This register does not support editing exchange rates.");
        gnc_error_dialog (window, "%s", message);
        return;
    }

    /* If we're not expanded AND number of splits greater than two, nothing to do */
    if ((num_splits > 2) && !expanded)
    {
        message = _("You need to expand the transaction in order to modify its "
                    "exchange rates.");
        gnc_error_dialog (window, "%s", message);
        return;
    }

    if (num_splits == 2 && anchor != NULL && !expanded)
    {
        split = gnc_tree_control_split_reg_get_current_trans_split (view);
        osplit = xaccSplitGetOtherSplit (split);

        value = xaccSplitGetValue (split);

        xaccTransBeginEdit (trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, trans);

        if (txn_com == xaccAccountGetCommodity (xaccSplitGetAccount(split)))
           gnc_tree_view_split_reg_set_value_for (view, trans, osplit, gnc_numeric_neg (value), TRUE);
        else
           gnc_tree_view_split_reg_set_value_for (view, trans, split, value, TRUE);

        xaccTransCommitEdit (trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
    }

    if (num_splits > 1 && expanded && depth == 3)
    {
        split = gnc_tree_view_split_reg_get_current_split (view);

        value = xaccSplitGetValue (split);

        if (txn_com == xaccAccountGetCommodity (xaccSplitGetAccount(split)))
        {
            message = _("The two currencies involved equal each other.");
            gnc_error_dialog (window, "%s", message);
            return;
        }
        else
        {
            gnc_tree_view_split_reg_set_dirty_trans (view, trans);
            xaccTransBeginEdit (trans);

            gnc_tree_view_split_reg_set_value_for (view, trans, split, value, TRUE);

            xaccTransCommitEdit (trans);
            gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
        }
    }
}


/* Void current transaction */
void
gnc_tree_control_split_reg_void_current_trans (GncTreeViewSplitReg *view, const char *reason)
{
    Transaction *trans;
    Split *blank_split;
    Split *split;

    if (!view) return;

    blank_split = gnc_tree_control_split_reg_get_blank_split (view);

    /* get the current split based on cursor position */
    split = gnc_tree_view_split_reg_get_current_split (view);
    if (split == NULL)
        return;

    /* Bail if trying to void the blank split. */
    if (split == blank_split)
        return;

    /* already voided. */
    if (xaccSplitGetReconcile (split) == VREC)
        return;

    trans = xaccSplitGetParent (split);

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    xaccTransVoid (trans, reason);

    if (xaccTransIsOpen (trans))
    {
        PERR("We should not be voiding an open transaction.");
        xaccTransCommitEdit (trans);
    }
    gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
}


/* Unvoid current transaction */
void
gnc_tree_control_split_reg_unvoid_current_trans (GncTreeViewSplitReg *view)
{
    Transaction *trans;
    Split *blank_split;
    Split *split;

    if (!view) return;

    blank_split = gnc_tree_control_split_reg_get_blank_split (view);

    /* get the current split based on cursor position */
    split = gnc_tree_view_split_reg_get_current_split (view);
    if (split == NULL)
        return;

    /* Bail if trying to unvoid the blank split. */
    if (split == blank_split)
        return;

    /* not voided. */
    if (xaccSplitGetReconcile (split) != VREC)
        return;

    trans = xaccSplitGetParent (split);

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    xaccTransUnvoid (trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
}


/* Jump to the Blank transaction, i.e. last in list */
void
gnc_tree_control_split_reg_jump_to_blank (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, NULL);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);

    /* scroll when view idle */
    g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_scroll_to_cell, view );
}


/* Jump to split */
void
gnc_tree_control_split_reg_jump_to_split (GncTreeViewSplitReg *view, Split *split)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, split, NULL);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    gnc_tree_view_split_reg_expand_trans (view, xaccSplitGetParent (split));

    gnc_tree_view_split_reg_set_current_path (view, spath);

    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);

    /* scroll when view idle */
    g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_scroll_to_cell, view );
}


/* Returns the Blank Transaction */
Transaction *
gnc_tree_control_split_reg_get_blank_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    return gnc_tree_model_split_get_blank_trans (model);
}


/* Return the Split for the current Transaction */
Split *
gnc_tree_control_split_reg_get_current_trans_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath;
    GtkTreeIter m_iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    Account *anchor;
    gboolean is_trow1, is_trow2, is_split, is_blank;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter, mpath);

    gnc_tree_model_split_reg_get_split_and_trans (
            GNC_TREE_MODEL_SPLIT_REG (model), &m_iter, &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    split = xaccTransFindSplitByAccount (trans, anchor);

    gtk_tree_path_free (mpath);

    return split;
}


/* Returns the Blank Split */
Split *
gnc_tree_control_split_reg_get_blank_split (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    return gnc_tree_model_split_get_blank_split (model);
}


/* Move to the relative transaction */
void
gnc_tree_control_split_reg_goto_rel_trans_row (GncTreeViewSplitReg *view, gint relative)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    GtkTreePath *new_spath;
    gint *indices;

    ENTER("Move relative, view is %p, relative is %d", view, relative);

//FIXME Do we need to do some checks on relative maybe  -1,0,1 ??

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    indices = gtk_tree_path_get_indices (spath);

    new_spath = gtk_tree_path_new_from_indices (indices[0] + (relative * view->sort_direction), -1);

    gnc_tree_view_split_reg_set_current_path (view, new_spath);

    gnc_tree_view_split_reg_block_selection (view, TRUE);
    gtk_tree_selection_unselect_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);
    gnc_tree_view_split_reg_block_selection (view, FALSE);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), new_spath);

    LEAVE("new_spath is %s", gtk_tree_path_to_string (new_spath));

    gtk_tree_path_free (new_spath);
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);
}


/* Enter the transaction */
void
gnc_tree_control_split_reg_enter (GncTreeViewSplitReg *view, gboolean next_transaction)
{
    GncTreeModelSplitReg *model;
    gboolean goto_blank;

    ENTER("view=%p, next_transaction=%s", view, next_transaction ? "TRUE" : "FALSE");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    goto_blank = gnc_gconf_get_bool (GCONF_GENERAL_REGISTER,
                                    "enter_moves_to_end", NULL);

    /* If we are in single or double line mode and we hit enter
     * on the blank split, go to the blank split instead of the
     * next row. This prevents the cursor from jumping around
     * when you are entering transactions. */
    if ( !goto_blank && !next_transaction )
    {
        SplitRegisterStyle2 style = model->style;

        if (style == REG2_STYLE_LEDGER)
        {
            Split *blank_split;

            blank_split = gnc_tree_control_split_reg_get_blank_split (view);
            if (blank_split != NULL)
            {
                Split *current_split;

                current_split = gnc_tree_view_split_reg_get_current_split (view);

                if (blank_split == current_split)
                    goto_blank = TRUE;
            }
        }
    }

    /* First record the transaction */
    if (gnc_tree_view_split_reg_enter (view))
    {
        if (!goto_blank && next_transaction)
            gnc_tree_view_split_reg_collapse_trans (view, NULL);

        /* Now move. */
        if (goto_blank)
            gnc_tree_control_split_reg_jump_to_blank (view);
        else if (next_transaction)
            gnc_tree_control_split_reg_goto_rel_trans_row (view, 1);
    }
    LEAVE(" ");
}


/* Reinit the transaction */
void
gnc_tree_control_split_reg_reinit (GncTreeViewSplitReg *view, gpointer data)
{
    Transaction *trans;
    Split *split;
    GtkWidget *dialog, *window;
    gint response;
    const gchar *warning;

    const char *title = _("Remove the splits from this transaction?");
    const char *recn_warn = _("This transaction contains reconciled splits. "
                              "Modifying it is not a good idea because that will "
                              "cause your reconciled balance to be off.");

    trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (gtc_is_trans_readonly_and_warn (trans))
        return;

    window = gnc_tree_view_split_reg_get_parent (view);

    dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_WARNING,
                                    GTK_BUTTONS_NONE,
                                    "%s", title);
    if (xaccTransHasReconciledSplits (trans))
    {
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", recn_warn);
        warning = "register_remove_all_splits2";
    }
    else
    {
        warning = "register_remove_all_splits";
    }

    gtk_dialog_add_button (GTK_DIALOG (dialog),
                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
    gnc_gtk_dialog_add_button(dialog, _("_Remove Splits"),
                              GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
    response = gnc_dialog_run (GTK_DIALOG(dialog), warning);
    gtk_widget_destroy (dialog);
    if (response != GTK_RESPONSE_ACCEPT)
        return;

    gnc_tree_view_split_reg_reinit_trans (view);
}


/* Delete the currently selected item */
void
gnc_tree_control_split_reg_delete (GncTreeViewSplitReg *view, gpointer data)
{
    RowDepth depth;
    Transaction *trans;
    Split *split;
    GtkWidget *dialog, *window;
    gint response;
    const gchar *warning;

    /* get the current split based on cursor position */
    split = gnc_tree_view_split_reg_get_current_split (view);
    if (split == NULL)
    {
        return;
    }

    trans = xaccSplitGetParent (split);
    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    /* Deleting the blank split just cancels */
    {
        Split *blank_split = gnc_tree_control_split_reg_get_blank_split (view);

        if (split == blank_split)
        {
            return;
        }
    }

    if (gtc_is_trans_readonly_and_warn (trans))
        return;

    window = gnc_tree_view_split_reg_get_parent (view);

    /* On a split cursor, just delete the one split. */
    if (depth == SPLIT3)
    {
        const char *format = _("Delete the split '%s' from the transaction '%s'?");
        const char *recn_warn = _("You would be deleting a reconciled split! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");
        const char *anchor_error = _("You cannot delete this split.");
        const char *anchor_split = _("This is the split anchoring this transaction "
                                     "to the register. You may not delete it from "
                                     "this register window.  You may delete the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "delete the split from that register.");
        char *buf = NULL;
        const char *memo;
        const char *desc;
        char recn;

        if (split == gnc_tree_control_split_reg_get_current_trans_split (view))
        {
            dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                            GTK_DIALOG_MODAL
                                            | GTK_DIALOG_DESTROY_WITH_PARENT,
                                            GTK_MESSAGE_ERROR,
                                            GTK_BUTTONS_OK,
                                            "%s", anchor_error);
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", anchor_split);
            gtk_dialog_run (GTK_DIALOG (dialog));
            gtk_widget_destroy (dialog);
            return;
        }

        memo = xaccSplitGetMemo (split);
        memo = (memo && *memo) ? memo : _("(no memo)");

        desc = xaccTransGetDescription (trans);
        desc = (desc && *desc) ? desc : _("(no description)");

        /* ask for user confirmation before performing permanent damage */
        buf = g_strdup_printf (format, memo, desc);
        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", buf);
        g_free(buf);
        recn = xaccSplitGetReconcile (split);
        if (recn == YREC || recn == FREC)
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", recn_warn);
            warning = "register_delete_split2";
        }
        else
        {
            warning = "register_delete_split";
        }

        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Delete Split"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run (GTK_DIALOG (dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_tree_view_split_reg_delete_current_split (view);
        return;
    }

    g_return_if_fail (depth == TRANS1 || depth == TRANS2);

    /* On a transaction cursor with 2 or fewer splits in single or double
     * mode, we just delete the whole transaction, kerblooie */
    {
        const char *title = _("Delete the current transaction?");
        const char *recn_warn = _("You would be deleting a transaction "
                                  "with reconciled splits! "
                                  "This is not a good idea as it will cause your "
                                  "reconciled balance to be off.");

        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_WARNING,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        if (xaccTransHasReconciledSplits (trans))
        {
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                    "%s", recn_warn);
            warning = "register_delete_trans2";
        }
        else
        {
            warning = "register_delete_trans";
        }
        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);
        gnc_gtk_dialog_add_button (dialog, _("_Delete Transaction"),
                                  GTK_STOCK_DELETE, GTK_RESPONSE_ACCEPT);
        response =  gnc_dialog_run (GTK_DIALOG (dialog), warning);
        gtk_widget_destroy (dialog);
        if (response != GTK_RESPONSE_ACCEPT)
            return;

        gnc_tree_view_split_reg_delete_current_trans (view);
        return;
    }
}


/* Add Reverse Transaction */
void
gnc_tree_control_split_reg_reverse_current (GncTreeViewSplitReg *view)
{
    GtkWidget *window;
    Transaction *trans = NULL, *new_trans = NULL;

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    if (trans == NULL)
        return;

//FIXME Need same tests as duplicate ?????

    window = gnc_tree_view_split_reg_get_parent (view);

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    /* See if we were asked to reverse a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
    {
        LEAVE("skip blank trans");
        return;
    }

    if (xaccTransGetReversedBy (trans))
    {
        gnc_error_dialog (window, "%s",
                         _("A reversing entry has already been created for this transaction."));
        return;
    }

#ifdef skip

//FIXME Test for trans in edit....

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    xaccTransBeginEdit (trans);

    new_trans = xaccTransReverse (trans);

    xaccTransBeginEdit (new_trans);

    /* Clear transaction level info */
    xaccTransSetDatePostedSecs (new_trans, gnc_time (NULL));
    xaccTransSetDateEnteredSecs (new_trans, gnc_time (NULL));

    xaccTransCommitEdit (new_trans);
    xaccTransCommitEdit (trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, NULL);

    /* Now jump to new trans */
//    gnc_tree_control_split_reg_jump_to_split (view, xaccTransGetSplit (new_trans, 0));

#endif
}


/* Duplicate the current selection */
gboolean
gnc_tree_control_split_reg_duplicate_current (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkWidget *window;
    RowDepth depth;
    Transaction *trans;
    Split *blank_split;
    Split *split, *trans_split;
    gboolean changed = FALSE;

    ENTER("");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    blank_split = gnc_tree_control_split_reg_get_blank_split (view);
    split = gnc_tree_view_split_reg_get_current_split (view);
    trans_split = gnc_tree_control_split_reg_get_current_trans_split (view);
    trans = gnc_tree_view_split_reg_get_current_trans (view);
    depth = gnc_tree_view_reg_get_selected_row_depth (view);
#ifdef skip
    /* This shouldn't happen, but be paranoid. */
    if (trans == NULL)
    {
        LEAVE("no transaction");
        return FALSE;
    }

    if (gtc_is_trans_readonly_and_warn (trans))
    {
        LEAVE("read only transaction");
        return FALSE;
    }

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    /* See if we are editing this transcation all ready */
    if (trans == gnc_tree_view_split_reg_get_dirty_trans (view))
        changed = TRUE;

    /* See if we were asked to duplicate a blank split. */
    if (split == gnc_tree_control_split_reg_get_blank_split (view))
    {
        LEAVE("skip blank split");
        return FALSE;
    }

    /* See if we were asked to duplicate a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
    {
        LEAVE("skip blank trans");
        return FALSE;
    }

    gnc_suspend_gui_refresh ();

    window = gnc_tree_view_split_reg_get_parent (view);

    /* If the cursor has been edited, we are going to have to commit
     * it before we can duplicate. Make sure the user wants to do that. */
    if (changed)
    {
        GtkWidget *dialog;
        gint response;
        const char *title = _("Save transaction before duplicating?");
        const char *message =
            _("The current transaction has been changed. Would you like to "
              "record the changes before duplicating the transaction, or "
              "cancel the duplication?");

        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_CANCEL,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", message);
        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              _("_Record"), GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run (GTK_DIALOG (dialog), "transaction_duplicated");
        gtk_widget_destroy (dialog);

        if (response != GTK_RESPONSE_ACCEPT)
        {
            gnc_resume_gui_refresh ();
            LEAVE("save cancelled");
            return FALSE;
        }

        xaccTransCommitEdit (trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
    }

    /* Ok, we are now ready to make the copy. */

    if (depth == SPLIT3)
    {
        Split *new_split;

        /* We are on a split in an expanded transaction.
         * Just copy the split and add it to the transaction. */

        if (split != trans_split)
        {
            new_split = xaccMallocSplit (gnc_get_current_book ());

            xaccTransBeginEdit (trans);
            xaccSplitSetParent (new_split, trans);
            gtc_copy_split_onto_split (split, new_split, FALSE);
            xaccTransCommitEdit (trans);
        }
        else
        {
            gnc_error_dialog (window, "%s",
                         _("This is the split anchoring this transaction to the register."
                           " You can not duplicate it from this register window."));
            LEAVE("split anchoring this transaction");
            return FALSE;
        }
    }
    else
    {
        Transaction *new_trans;
        const char *in_num = NULL;
        const char *in_tnum = NULL;
        char *out_num;
        char *out_tnum;
        time64 date;
        gboolean use_autoreadonly = qof_book_uses_autoreadonly (gnc_get_current_book());
        gboolean use_split_action_for_num_field = qof_book_use_split_action_for_num_field
                           (gnc_get_current_book());

        /* We are on a transaction row. Copy the whole transaction. */

        date = time (0);
        if (gnc_strisnum (xaccTransGetNum (trans)))
        {
            Account *account = gnc_tree_model_split_reg_get_anchor (model);
            if (account)
                in_num = xaccAccountGetLastNum (account);
            else
                in_num = xaccTransGetNum (trans);
            in_tnum = (use_split_action_for_num_field
                                        ? NULL
                                        : gnc_get_num_action (trans, NULL)); //FIXME is this right way round ?
        }

        if (!gnc_dup_trans_dialog (window, NULL, TRUE,
                                   &date, in_num, &out_num, in_tnum, &out_tnum))
        {
            gnc_resume_gui_refresh ();
            LEAVE("dup cancelled");
            return FALSE;
        }

        if (use_autoreadonly)
        {
            GDate d;
            GDate *readonly_threshold = qof_book_get_autoreadonly_gdate (gnc_get_current_book());
            gnc_gdate_set_time64 (&d, date);
            if (g_date_compare (&d, readonly_threshold) < 0)
            {
                GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                    0,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    "%s", _("Cannot store a transaction at this date"));
                gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                        "%s", _("The entered date of the duplicated transaction is older than the \"Read-Only Threshold\" set for this book.  "
                                "This setting can be changed in File -> Properties -> Accounts."));
                gtk_dialog_run (GTK_DIALOG (dialog));
                gtk_widget_destroy (dialog);

                g_date_free (readonly_threshold);
                LEAVE("entered date older than read-only threshold");
                return FALSE;
            }
            g_date_free (readonly_threshold);
        }

        new_trans = xaccMallocTransaction (gnc_get_current_book ());

        xaccTransBeginEdit (new_trans);
        gtc_copy_trans_onto_trans (trans, new_trans, FALSE, FALSE);
        xaccTransSetDatePostedSecs (new_trans, date);
        xaccTransSetNum (new_trans, out_num);
        xaccTransCommitEdit (new_trans);
    }


    /* Refresh the GUI. */
    gnc_resume_gui_refresh ();
#endif
    LEAVE(" ");
    return TRUE;
}


/* Save any open edited transactions on closing register */
gboolean
gnc_tree_control_split_reg_save (GncTreeViewSplitReg *view, gboolean reg_closing)
{
    GncTreeModelSplitReg *model;
    RowDepth depth;
    Transaction *pending_trans;
    Transaction *blank_trans;
    Transaction *trans;
    Account *account;
    Split *blank_split;
    const char *memo;
    const char *desc;
    Split *split, *current_trans_split;

    ENTER("view=%p, reg_closing=%s", view, reg_closing ? "TRUE" : "FALSE");

    if (!view)
    {
        LEAVE("no view");
        return FALSE;
    }

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    if (reg_closing)
        view->reg_closing = TRUE;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    blank_split = gnc_tree_control_split_reg_get_blank_split (view);
    pending_trans = gnc_tree_view_split_reg_get_dirty_trans (view);
    blank_trans = gnc_tree_control_split_reg_get_blank_trans (view);

    /* get the handle to the current split and transaction */
    split = gnc_tree_view_split_reg_get_current_split (view);
    trans = gnc_tree_view_split_reg_get_current_trans (view);

    current_trans_split = gnc_tree_control_split_reg_get_current_trans_split (view);

    if (trans == NULL)
    {
        LEAVE("no transaction");
        return FALSE;
    }

    if (!xaccTransIsOpen (trans))
    {
        LEAVE("transaction not open");
        return FALSE;
    }

    if (trans == pending_trans )
    {
        if (trans != blank_trans)
        {
           /* Existing Transaction, we are going to commit. */

            PINFO("committing trans (%p)", trans);
            xaccTransCommitEdit (trans);
            gnc_tree_view_split_reg_set_dirty_trans (view, NULL);

            LEAVE("Existing Transaction committed");
            return TRUE;
        }
        else
        {
           /* Blank Transaction, we are going to commit. */

            PINFO("start committing blank trans (%p)", trans);
//FIXME More stuff ?

            if (xaccTransCountSplits (trans) == 0)
            {
                GtkWidget *dialog, *window;
                gint response;
                const char *title = _("Not enough information for Blank Transaction?");
                const char *message =
                    _("The blank transaction does not have enough information to save it. Would you like to "
                      "return to the transaction to update, or cancel the save?");
                window = gnc_tree_view_split_reg_get_parent (view);
                dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_CANCEL,
                                        "%s", title);
                gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                        "%s", message);
                gtk_dialog_add_button (GTK_DIALOG (dialog),
                              _("_Return"), GTK_RESPONSE_ACCEPT);

                gtk_widget_grab_focus (gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT));

                response = gtk_dialog_run (GTK_DIALOG (dialog));
//                response = gnc_dialog_run (GTK_DIALOG (dialog), "transaction_incomplete");
                gtk_widget_destroy (dialog);

                if (response != GTK_RESPONSE_ACCEPT)
                {
                    LEAVE("save cancelled");
                    return TRUE;
                }
                LEAVE("return to transaction");
                return FALSE;
            }

            xaccTransCommitEdit (trans);
            gnc_tree_view_split_reg_set_dirty_trans (view, NULL);

            LEAVE("Blank Transaction committed");
            return TRUE;
        }
    }

    LEAVE(" ");
    return TRUE;
}


/* Allow the reconcile flag to be changed */
gboolean
gnc_tree_control_split_reg_recn_change (GncTreeViewSplitReg *view)
{
    GtkWidget *dialog, *window;
    gint response;
    Transaction *trans;
    Split *split;
    RowDepth depth;
    char rec;
    const gchar *title = _("Mark split as unreconciled?");
    const gchar *message =
        _("You are about to mark a reconciled split as unreconciled.  Doing "
          "so might make future reconciliation difficult!  Continue "
          "with this change?");

    ENTER(" ");

    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    if (depth == SPLIT3)
        split = gnc_tree_view_split_reg_get_current_split (view);
    else
        split = gnc_tree_control_split_reg_get_current_trans_split (view);

    rec = xaccSplitGetReconcile (split);

    if (rec != YREC)
    {
        LEAVE("Not reconciled");
        return TRUE;
    }

    /* Does the user want to be warned? */
    window = gnc_tree_view_split_reg_get_parent (view);
    dialog =
        gtk_message_dialog_new (GTK_WINDOW (window),
                               GTK_DIALOG_DESTROY_WITH_PARENT,
                               GTK_MESSAGE_WARNING,
                               GTK_BUTTONS_CANCEL,
                               "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
            "%s", message);
    gtk_dialog_add_button (GTK_DIALOG (dialog), _("_Unreconcile"),
                          GTK_RESPONSE_YES);
    response = gnc_dialog_run (GTK_DIALOG (dialog), "mark_split_unreconciled");
    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_YES)
    {
        char rec = 'n';
        trans = xaccSplitGetParent (split);

        gnc_tree_view_split_reg_set_dirty_trans (view, trans);
        xaccTransBeginEdit (trans);

        xaccSplitSetReconcile (split, rec);

        LEAVE("mark split unreconciled");
        return TRUE;
    }
    LEAVE("Canceled split unreconciled");
    return FALSE;
}


/* Test for splits being reconciled and decide to allow changes */
gboolean
gnc_tree_control_split_reg_recn_test (GncTreeViewSplitReg *view)
{
    Transaction *trans;
    Split *split;
    RowDepth depth;
    char recn;

    ENTER(" ");

    /* This assumes we reset the flag whenever we change splits. */
    if (view->change_allowed)
    {
        LEAVE("change allowed is set");
        return TRUE;
    }
    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    if (depth == SPLIT3)
        split = gnc_tree_view_split_reg_get_current_split (view);
    else
        split = gnc_tree_control_split_reg_get_current_trans_split (view);

    if (!split)
    {
        LEAVE("No split");
        return TRUE;
    }

    trans = xaccSplitGetParent (split);

    recn = xaccSplitGetReconcile (split);

    if (recn == YREC || xaccTransHasReconciledSplits (trans))
    {
        GtkWidget *dialog, *window;
        gint response;
        const gchar *title;
        const gchar *message;

        if(recn == YREC)
        {
            title = _("Change reconciled split?");
            message =
             _("You are about to change a reconciled split.  Doing so might make "
               "future reconciliation difficult!  Continue with this change?");
        }
        else
        {
            title = _("Change split linked to a reconciled split?");
            message =
            _("You are about to change a split that is linked to a reconciled split.  "
              "Doing so might make future reconciliation difficult!  Continue with this change?");
        }

        /* Does the user want to be warned? */
        window = gnc_tree_view_split_reg_get_parent (view);
        dialog =
            gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_WARNING,
                                   GTK_BUTTONS_CANCEL,
                                   "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", message);
        gtk_dialog_add_button (GTK_DIALOG (dialog), _("Chan_ge Split"),
                              GTK_RESPONSE_YES);
        response = gnc_dialog_run (GTK_DIALOG (dialog), "change_reconciled_split");
        gtk_widget_destroy (dialog);

        if (response != GTK_RESPONSE_YES)
        {
            LEAVE("cancel reconciled split");
            return FALSE;
        }
    }
    view->change_allowed = TRUE;
    LEAVE(" ");
    return TRUE;
}


/*****************************************************************************
 *                         ClipBoard Functions                               *
 *****************************************************************************/
static Transaction *clipboard_trans = NULL;
/* Must never dereference. */
static const Account *clipboard_acct = NULL;


void
gnc_tree_control_split_reg_cut_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    Transaction *from_trans;

    g_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (view));

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    from_trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (!from_trans)
        return;

    if (gnc_tree_model_split_reg_get_read_only (model, from_trans))
    {
        GtkWidget *window;

        window = gnc_tree_view_split_reg_get_parent (view);
        gnc_error_dialog (window, "%s",
                         _("You can not cut from a read only transaction or register."));
        return;
    }

    xaccTransBeginEdit (clipboard_trans);
    if (clipboard_trans)
        xaccTransDestroy (clipboard_trans);

    clipboard_trans = xaccTransCopyToClipBoard (from_trans);
    clipboard_acct = gnc_tree_model_split_reg_get_anchor (model);

    gnc_tree_view_split_reg_delete_current_trans (view);
}

/* Return the split account for which ancestor is it's parent */
static Account *
gtc_trans_get_account_for_splits_ancestor (const Transaction *trans, const Account *ancestor)
{
    GList *node;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
        Split *split = node->data;
        Account *split_acc = xaccSplitGetAccount(split);

        if (!xaccTransStillHasSplit(trans, split))
            continue;

        if (ancestor && xaccAccountHasAncestor(split_acc, ancestor))
            return split_acc;
    }
    return NULL;
}

void
gnc_tree_control_split_reg_copy_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    Transaction *from_trans;
    Account *from_acc, *anchor;

    g_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (view));

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    from_trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (!from_trans)
        return;

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    clipboard_acct = gtc_trans_get_account_for_splits_ancestor (from_trans, anchor);

    xaccTransBeginEdit (clipboard_trans);
    if (clipboard_trans)
        xaccTransDestroy (clipboard_trans);

    clipboard_trans = xaccTransCopyToClipBoard (from_trans);
}

void
gnc_tree_control_split_reg_paste_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    Account *anchor_acct;
    Transaction *to_trans;

    g_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (view));

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    anchor_acct = gnc_tree_model_split_reg_get_anchor (model);

    to_trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (!to_trans || !clipboard_trans)
        return;

    if (gnc_tree_model_split_reg_get_read_only (model, to_trans))
    {
        GtkWidget *window;

        window = gnc_tree_view_split_reg_get_parent (view);
        gnc_error_dialog (window, "%s",
                         _("You can not paste to a read only transaction or register."));
        return;
    }

    //FIXME You can not paste from gl to a register, is this too simplistic
    if (clipboard_acct == NULL && anchor_acct != NULL)
    {
        GtkWidget *window;

        window = gnc_tree_view_split_reg_get_parent (view);
        gnc_error_dialog (window, "%s",
                         _("You can not paste from the general ledger to a register."));
        return;
    }

    xaccTransBeginEdit (to_trans);
    gnc_tree_view_split_reg_set_dirty_trans (view, to_trans);

    // Remove the blank split
    gnc_tree_model_split_reg_set_blank_split_parent (model, to_trans, TRUE);

    xaccTransCopyFromClipBoard (clipboard_trans, to_trans, clipboard_acct, anchor_acct, FALSE);

    // Add the blank split back
    gnc_tree_model_split_reg_set_blank_split_parent (model, to_trans, FALSE);

    // Refresh the view
    g_signal_emit_by_name (model, "refresh_view", NULL);
}

void
gnc_tree_control_auto_complete (GncTreeViewSplitReg *view, Transaction *trans,  const gchar *new_text)
{
    GncTreeModelSplitReg *model;
    Transaction          *btrans, *dirty_trans;
    GtkListStore         *desc_list;
    GtkTreeIter           iter;
    gboolean              valid;

    g_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (view));
    DEBUG("auto_complete - trans %p and description '%s'", trans, new_text);

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    btrans = gnc_tree_model_split_get_blank_trans (model);

    // if we are not looking at the blank trans, return.
    if (trans != btrans)
       return;

    desc_list = gnc_tree_model_split_reg_get_description_list (model);

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (desc_list), &iter);
    while (valid)
    {
        Transaction *trans_from;
        gchar *text;
        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL (desc_list), &iter, 0, &text, 1, &trans_from, -1);

        if (g_strcmp0 (text, new_text) == 0)
        {
            xaccTransCopyOnto (trans_from, trans);
            g_free(text);
            break;
        }
        g_free(text);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (desc_list), &iter);
    }
}

/*****************************************************************************
 *                             Sort Functions                                *
 *****************************************************************************/

/* Sort changed callback */
void
gnc_tree_control_split_reg_sort_changed_cb (GtkTreeSortable *sortable, gpointer user_data)
{
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkSortType   type;
    gint          sortcol;
    gint          sort_depth;
    const gchar  *gconf_section;

    gtk_tree_sortable_get_sort_column_id (sortable, &sortcol, &type);
    ENTER("sortcol is %d", sortcol);

    sort_depth = gnc_tree_view_reg_get_selected_row_depth (view);
    if (sort_depth != 0)
        view->sort_depth = sort_depth;

    view->sort_col = sortcol - 1;

    if (type == GTK_SORT_ASCENDING)
        view->sort_direction = 1;
    else
        view->sort_direction = -1;

    /* Save the sort depth to gconf */
    gconf_section = gnc_tree_view_get_gconf_section (GNC_TREE_VIEW (view));
    gnc_gconf_set_int (gconf_section, "sort_depth", view->sort_depth, NULL);

    LEAVE("sort_col %d, sort_direction is %d  sort_depth is %d", view->sort_col, view->sort_direction, view->sort_depth );
}


static GtkTreeModel *
gtc_sort_cb_filter_iters (GtkTreeModel *f_model,
                       GtkTreeIter *f_iter_a,
                       GtkTreeIter *f_iter_b,
                       GtkTreeIter *iter_a,
                       GtkTreeIter *iter_b)
{
    GtkTreeModel *model;

    model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER (f_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model), iter_a, f_iter_a);
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model), iter_b, f_iter_b);
    return model;
}


/* Sort function for Date column */
gint
gnc_tree_control_split_reg_sort_by_date (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    int depth;
    time64 i, j;
    int retval;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    switch (depth) {
        case 1: // Date Posted
        retval = xaccTransOrder (tnodea->data, tnodeb->data);
        if (retval)
           return retval;

        break;
        case 2: // Date Entered
        i = xaccTransGetDateEntered (tnodea->data);
        j = xaccTransGetDateEntered (tnodeb->data);

        if ((gint)(i - j) == 0)
        {
            i = xaccTransGetDate (tnodeb->data);
            j = xaccTransGetDate (tnodea->data);
        }

        return (gint)(i - j);
        break;
    }
    return 0;
}


/* Sort function for Number / Action column */
gint
gnc_tree_control_split_reg_sort_by_numact (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    Account *anchor;
    int depth;
    time64 i, j;
    int na, nb, retval;
    const char *ca, *cb;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    switch (depth) {
        case 1: // Number

        na = atoi (xaccTransGetNum (tnodea->data));
        nb = atoi (xaccTransGetNum (tnodeb->data));

        if (na < nb) return -1;
        if (na > nb) return +1;

        break;
        case 2: // Action
//FIXME this may be needed for this one (!qof_book_use_split_action_for_num_field (gnc_get_current_book()))

        anchor = gnc_tree_model_split_reg_get_anchor (model);

        if (anchor != NULL)
        {
            ca = xaccSplitGetAction (xaccTransFindSplitByAccount (tnodea->data, anchor))
               ? xaccSplitGetAction (xaccTransFindSplitByAccount (tnodea->data, anchor)) : "";
            cb = xaccSplitGetAction (xaccTransFindSplitByAccount (tnodeb->data, anchor))
               ? xaccSplitGetAction (xaccTransFindSplitByAccount (tnodeb->data, anchor)) : "";
        }
        else
            ca = cb = "Text";

        retval = g_utf8_collate (ca, cb);
        if (retval)
           return retval;
        break;
    }
    return 0;
}


/* Sort function for 'Description / Notes / Memo' column */
gint
gnc_tree_control_split_reg_sort_by_dnm (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    Account *anchor;
    const char *ca, *cb;
    int depth;
    int retval;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    switch (depth) {
        case 1: // Description
        ca = xaccTransGetDescription (tnodea->data) ? xaccTransGetDescription (tnodea->data) : "";
        cb = xaccTransGetDescription (tnodeb->data) ? xaccTransGetDescription (tnodeb->data) : "";

        retval = g_utf8_collate (ca, cb);
        if (retval)
           return retval;

        break;
        case 2: // Notes
        ca = xaccTransGetNotes (tnodea->data) ? xaccTransGetNotes (tnodea->data) : "";
        cb = xaccTransGetNotes (tnodeb->data) ? xaccTransGetNotes (tnodeb->data) : "";

        retval = g_utf8_collate (ca, cb);
        if (retval)
           return retval;

        break;
        case 3: // Memo
        anchor = gnc_tree_model_split_reg_get_anchor (model);

        if (anchor != NULL)
        {
            ca = xaccSplitGetMemo (xaccTransFindSplitByAccount (tnodea->data, anchor))
               ? xaccSplitGetMemo (xaccTransFindSplitByAccount (tnodea->data, anchor)) : "";
            cb = xaccSplitGetMemo (xaccTransFindSplitByAccount (tnodeb->data, anchor))
               ? xaccSplitGetMemo (xaccTransFindSplitByAccount (tnodeb->data, anchor)) : "";
        }
        else
            ca = cb = "Text";

        retval = g_utf8_collate (ca, cb);
        if (retval)
           return retval;

        break;
    }
    return 0;
}


/* Sort function for Reconcile column */
gint
gnc_tree_control_split_reg_sort_by_recn (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    Account *anchor;
    int depth;
    int na = 0, nb = 0, retval;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    if (anchor != NULL)
    {
        Split *splita, *splitb;

        splita = xaccTransFindSplitByAccount (tnodea->data, anchor);
        splitb = xaccTransFindSplitByAccount (tnodeb->data, anchor);

        switch (xaccSplitGetReconcile (splita))
        {
        case YREC:
            na = 4;
            break;
        case FREC:
            na = 3;
            break;
        case VREC:
            na = 2;
            break;
        case NREC:
            na = 1;
            break;
        case CREC:
            na = 0;
            break;

        default:
            break;
        }

        switch (xaccSplitGetReconcile (splitb))
        {
        case YREC:
            nb = 4;
            break;
        case FREC:
            nb = 3;
            break;
        case VREC:
            nb = 2;
            break;
        case NREC:
            nb = 1;
            break;
        case CREC:
            nb = 0;
            break;

        default:
            break;
        }

        if (na < nb) return -1;
        if (na > nb) return +1;

        retval = xaccTransOrder (tnodea->data, tnodeb->data);
        if (retval)
           return retval;
    }
    return 0;
}


/* Sort function for transfer column */
gint
gnc_tree_control_split_reg_sort_by_account (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    Account *anchor, *accounta, *accountb;
    Split *splita, *splitb;
    const char *ca, *cb;
    int depth;
    int retval;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    switch (depth) {
        case 1: // Account Transfer
        if (xaccTransCountSplits (tnodea->data) > 2) return 1;
        if (xaccTransCountSplits (tnodeb->data) > 2) return -1;

        if (anchor != NULL) // Registers and sub accounts.
        {
            // Get the split parent who has anchor as a parent, think of sub accounts.
            accounta = gtc_trans_get_account_for_splits_ancestor (tnodea->data, anchor);
            accountb = gtc_trans_get_account_for_splits_ancestor (tnodeb->data, anchor);

            // Get the other split parent account.
            splita = xaccSplitGetOtherSplit (xaccTransFindSplitByAccount(tnodea->data, accounta));
            splitb = xaccSplitGetOtherSplit (xaccTransFindSplitByAccount(tnodeb->data, accountb));
        }
        else // General Ledger
        {
            // Get the first split in the transaction.
            splita = xaccTransGetSplit (tnodea->data, 0);
            splitb = xaccTransGetSplit (tnodeb->data, 0);
        }

        // Sort on split accounts.
        retval = xaccAccountOrder (xaccSplitGetAccount (splita), xaccSplitGetAccount (splitb));
        if (retval)
            return retval;
        break;

        case 2: // Void Reason
            ca = xaccTransGetVoidReason (tnodea->data) ? xaccTransGetVoidReason (tnodea->data) : "";
            cb = xaccTransGetVoidReason (tnodeb->data) ? xaccTransGetVoidReason (tnodeb->data) : "";

            retval = g_utf8_collate (ca, cb);
            if (retval)
               return retval;
        break;
    }
    return 0;
}


/* Sort function for debit / credit column */
gint
gnc_tree_control_split_reg_sort_by_value (GtkTreeModel *fm, GtkTreeIter *fa, GtkTreeIter *fb,
                  gpointer user_data)
{
    GncTreeModelSplitReg *model;
    GncTreeViewSplitReg *view = GNC_TREE_VIEW_SPLIT_REG (user_data);
    GtkTreeIter *ma = gtk_tree_iter_copy (fa);
    GtkTreeIter *mb = gtk_tree_iter_copy (fb);
    GList *tnodea, *tnodeb;
    Account *anchor, *accounta, *accountb;
    int depth;
    int na = 0, nb = 0, retval;
    gnc_numeric numa, numb;

    model = GNC_TREE_MODEL_SPLIT_REG (gtc_sort_cb_filter_iters (fm, fa, fb, ma, mb));

    if (gnc_tree_model_split_reg_is_blank_trans (model, ma)) return 1;
    if (gnc_tree_model_split_reg_is_blank_trans (model, mb)) return -1;

    tnodea = ma->user_data2;
    tnodeb = mb->user_data2;

    depth = view->sort_depth;

    gtk_tree_iter_free (ma);
    gtk_tree_iter_free (mb);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    if (anchor != NULL) // Registers and sub accounts.
    {
        // Get the split parent who has anchor as a parent, think of sub accounts.
        accounta = gtc_trans_get_account_for_splits_ancestor (tnodea->data, anchor);
        accountb = gtc_trans_get_account_for_splits_ancestor (tnodeb->data, anchor);
    }
    else // General ledger
    {
        // Get the account of the first split.
        accounta = xaccSplitGetAccount (xaccTransGetSplit (tnodea->data, 0));
        accountb = xaccSplitGetAccount (xaccTransGetSplit (tnodeb->data, 0));
    }

    // Get the value based on accounts.
    numa = xaccTransGetAccountValue (tnodea->data, accounta);
    numb = xaccTransGetAccountValue (tnodeb->data, accountb);

    retval = gnc_numeric_compare (numa, numb);
    if (retval)
        return retval;

    return 0;
}

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
#include <stdlib.h>
#include <string.h>

#include "gnc-tree-control-split-reg.h"
#include "gnc-tree-model-split-reg.h"
#include "gnc-tree-util-split-reg.h"
#include "gnc-tree-view-split-reg.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-prefs.h"
#include "gnc-gdate-utils.h"
#include "gnome-utils/gnc-warnings.h"
#include "dialog-utils.h"
#include "dialog-dup-trans.h"
#include "dialog-account.h"

#include "Transaction.h"
#include "engine-helpers.h"
#include "gnc-event.h"
#include "Scrub.h"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_LEDGER;

/*****************************************************************************/
/*****************************************************************************/

/* Read only dialog */
static gboolean
gtc_sr_is_trans_readonly_and_warn (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkWidget *window;
    GtkWidget *dialog;
    const gchar *reason;
    const gchar *title = _("Cannot modify or delete this transaction.");
    const gchar *message_reason =
        _("This transaction is marked read-only with the comment: '%s'");

    if (!trans) return FALSE;

    window = gnc_tree_view_split_reg_get_parent (view);
    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (xaccTransIsReadonlyByPostedDate (trans))
    {
        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book. "
                        "This setting can be changed in File -> Properties -> Accounts."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }

    reason = xaccTransGetReadOnly (trans);
    if (reason)
    {
        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                message_reason, reason);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }

    if (gnc_tree_model_split_reg_get_read_only (model, trans))
    {
        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        0,
                                        GTK_MESSAGE_ERROR,
                                        GTK_BUTTONS_OK,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", _("You can not change this transaction, the Book or Register is set to Read Only."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        return TRUE;
    }
    return FALSE;
}


/* Transaction is being edited dialog */
#define gtc_sr_trans_open_and_warn gnc_tree_control_split_reg_trans_open_and_warn
gboolean
gnc_tree_control_split_reg_trans_open_and_warn (GncTreeViewSplitReg *view, Transaction *trans)
{
    Transaction *dirty_trans;
    GtkWidget *window;
    GtkWidget *dialog;
    gint response;
    const char *title = _("Save Transaction before proceeding?");
    const char *message =
            _("The current transaction has been changed. Would you like to "
              "record the changes before proceeding, or cancel?");

    window = gnc_tree_view_split_reg_get_parent (view);
    dirty_trans = gnc_tree_view_split_reg_get_dirty_trans (view);

    if (trans == dirty_trans)
    {
        dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_CANCEL,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                "%s", message);
        gtk_dialog_add_button (GTK_DIALOG (dialog),
                              _("_Record"), GTK_RESPONSE_ACCEPT);
        response = gnc_dialog_run (GTK_DIALOG (dialog), GNC_PREF_WARN_REG_TRANS_MOD);
        gtk_widget_destroy (dialog);

        if (response != GTK_RESPONSE_ACCEPT)
            return TRUE;

        xaccTransCommitEdit (trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, NULL);

        return FALSE;
    }
    else
        return FALSE;
}


#define gtc_sr_trans_test_for_edit gnc_tree_control_split_reg_trans_test_for_edit
gboolean
gtc_sr_trans_test_for_edit (GncTreeViewSplitReg *view, Transaction *trans)
{
    GtkWidget *window;
    Transaction *dirty_trans;

    /* Make sure we have stopped editing */
    gnc_tree_view_split_reg_finish_edit (view);

    window = gnc_tree_view_split_reg_get_parent (view);

    /* Get dirty_trans */
    dirty_trans = gnc_tree_view_split_reg_get_dirty_trans (view);

    /* We are being edited in a different register */
    if (xaccTransIsOpen (trans) && (dirty_trans != trans))
    {
        gnc_error_dialog (window, "%s",
                         _("This transaction is being edited in a different register."));
        return TRUE;
    }
    return FALSE;
}

/*****************************************************************************/
/*****************************************************************************/

gboolean
gnc_tree_control_split_reg_balance_trans (GncTreeViewSplitReg *view, Transaction *trans)
{
    GncTreeModelSplitReg *model;
    GtkWidget *window;
    int choice;
    int default_value;
    Account *default_account;
    Account *other_account;
    Account *root;
    GList *radio_list = NULL;
    const char *title   = _("Rebalance Transaction");
    const char *message = _("The current transaction is not balanced.");
    Split *split;
    Split *other_split;
    gboolean two_accounts;
    gboolean multi_currency;


    if (xaccTransIsBalanced (trans))
        return FALSE;

    window = gnc_tree_view_split_reg_get_parent (view);
    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (xaccTransUseTradingAccounts (trans))
    {
        MonetaryList *imbal_list;
        gnc_monetary *imbal_mon;
        imbal_list = xaccTransGetImbalance (trans);

        /* See if the imbalance is only in the transaction's currency */
        if (!imbal_list)
            /* Value imbalance, but not commodity imbalance.  This shouldn't
               be something that scrubbing can cause to happen.  Perhaps someone
               entered invalid splits.  */
            multi_currency = TRUE;
        else
        {
            imbal_mon = imbal_list->data;
            if (!imbal_list->next &&
                    gnc_commodity_equiv(gnc_monetary_commodity(*imbal_mon),
                                        xaccTransGetCurrency(trans)))
                multi_currency = FALSE;
            else
                multi_currency = TRUE;
        }

        /* We're done with the imbalance list, the real work will be done
           by xaccTransScrubImbalance which will get it again. */
        gnc_monetary_list_free(imbal_list);
    }
    else
        multi_currency = FALSE;

    split = xaccTransGetSplit (trans, 0);
    other_split = xaccSplitGetOtherSplit (split);

    if (other_split == NULL)
    {
        /* Attempt to handle the inverted many-to-one mapping */
        split = xaccTransGetSplit (trans, 1);
        if (split) other_split = xaccSplitGetOtherSplit (split);
        else split = xaccTransGetSplit (trans, 0);
    }
    if (other_split == NULL || multi_currency)
    {
        two_accounts = FALSE;
        other_account = NULL;
    }
    else
    {
        two_accounts = TRUE;
        other_account = xaccSplitGetAccount (other_split);
    }

    default_account = gnc_tree_model_split_reg_get_anchor (model);

    /* If the two pointers are the same, the account from other_split
     * is actually the default account. We must make other_account
     * the account from split instead.   */

    if (default_account == other_account)
        other_account = xaccSplitGetAccount (split);

    /*  If the two pointers are still the same, we have two splits, but
     *  they both refer to the same account. While non-sensical, we don't
     *  object.   */

    if (default_account == other_account)
        two_accounts = FALSE;

    radio_list = g_list_append (radio_list,
                                _("Balance it _manually"));
    radio_list = g_list_append (radio_list,
                                _("Let GnuCash _add an adjusting split"));

    if (model->type < NUM_SINGLE_REGISTER_TYPES2 && !multi_currency)
    {
        radio_list = g_list_append (radio_list,
                                    _("Adjust current account _split total"));

        default_value = 2;
        if (two_accounts)
        {
            radio_list = g_list_append (radio_list,
                                        _("Adjust _other account split total"));
            default_value = 3;
        }
    }
    else
        default_value = 0;

    choice = gnc_choose_radio_option_dialog
             (window,
              title,
              message,
              _("_Rebalance"),
              default_value,
              radio_list);

    g_list_free (radio_list);

    root = gnc_account_get_root(default_account);
    switch (choice)
    {
    default:
    case 0:
        return TRUE;
        break;

    case 1:
        xaccTransScrubImbalance (trans, root, NULL);
        break;

    case 2:
        xaccTransScrubImbalance (trans, root, default_account);
        break;

    case 3:
        xaccTransScrubImbalance (trans, root, other_account);
        break;
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

    if (trans == NULL)
        return;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we proceed */
    if (gtc_sr_trans_open_and_warn (view, trans))
        return;

    if (num_splits < 2)
        return;

    window = gnc_tree_view_split_reg_get_parent (view);

    /* Make sure we NEED this for this type of register */
    if (!gnc_tree_util_split_reg_has_rate (view))
    {
        message = _("This register does not support editing exchange rates.");
        gnc_error_dialog(window, "%s", message);
        return;
    }

    /* If the anchor commodity is not a currency, cancel */
    if (anchor && !gnc_commodity_is_currency (xaccAccountGetCommodity (anchor)))
    {
        message = _("This register does not support editing exchange rates.");
        gnc_error_dialog (window, "%s", message);
        return;
    }

    /* If we're not expanded AND number of splits greater than two, nothing to do */
    if ((gnc_tree_util_split_reg_is_multi (xaccTransGetSplit (trans, 0))) && !expanded)
    {
        message = _("You need to expand the transaction in order to modify its "
                    "exchange rates.");
        gnc_error_dialog (window, "%s", message);
        return;
    }

    if (!gnc_tree_util_split_reg_is_multi (xaccTransGetSplit (trans, 0)) && anchor != NULL && !expanded)
    {
        split = gnc_tree_control_split_reg_get_current_trans_split (view);

        if (xaccAccountGetType (xaccSplitGetAccount (split)) == ACCT_TYPE_TRADING) // trading split
            return;

        osplit = xaccSplitGetOtherSplit (split);

        value = xaccSplitGetValue (split);

        gnc_tree_view_split_reg_set_dirty_trans (view, trans);
        xaccTransBeginEdit (trans);

        if (txn_com == xaccAccountGetCommodity (xaccSplitGetAccount(split)))
           gnc_tree_util_split_reg_set_value_for (view, trans, osplit, gnc_numeric_neg (value), TRUE);
        else
           gnc_tree_util_split_reg_set_value_for (view, trans, split, value, TRUE);

        xaccTransCommitEdit (trans);
        gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
    }

    if (num_splits > 1 && expanded && depth == 3)
    {
        split = gnc_tree_view_split_reg_get_current_split (view);

        if (xaccAccountGetType (xaccSplitGetAccount (split)) == ACCT_TYPE_TRADING) // trading split
            return;

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

            gnc_tree_util_split_reg_set_value_for (view, trans, split, value, TRUE);

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

    /* get the current split */
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

    if (trans == NULL)
        return;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we proceed */
    if (gtc_sr_trans_open_and_warn (view, trans))
        return;

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

    if (trans == NULL)
        return;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    gnc_tree_view_split_reg_set_dirty_trans (view, trans);

    xaccTransUnvoid (trans);

    gnc_tree_view_split_reg_set_dirty_trans (view, NULL);
}


/* Jump to the Blank transaction */
gboolean
gnc_tree_control_split_reg_jump_to_blank (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;
    Transaction *btrans;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    btrans = gnc_tree_model_split_get_blank_trans (model);

    model->current_trans = btrans;

    if (!gnc_tree_model_split_reg_trans_is_in_view (model, btrans))
        g_signal_emit_by_name (model, "refresh_trans");
    else
    {
        mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, btrans);

        spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

        /* Set cursor to new spath */
        gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, NULL, FALSE);

        gtk_tree_path_free (spath);
        gtk_tree_path_free (mpath);

        /* scroll when view idle */
        g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_scroll_to_cell, view );
    }
    return FALSE;
}


/* Jump to transaction or split */
void
gnc_tree_control_split_reg_jump_to (GncTreeViewSplitReg *view, Transaction *trans, Split *split, gboolean amount)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    if (split)
        trans = NULL;

    mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, split, trans);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    if (split)
        gnc_tree_view_split_reg_expand_trans (view, xaccSplitGetParent (split));

    /* Set cursor to new spath, if amount, cursor is set to correct column ready for editing */
    if (amount)
    {
        GtkCellRenderer *cr0;
        GList *renderers;
        GList *columns;
        GList  *column;
        gint i;

        columns = gtk_tree_view_get_columns (GTK_TREE_VIEW (view));

        for (column = columns, i = 1; column; column = g_list_next (column), i++)
        {
            GtkTreeViewColumn *tvc;
            ViewCol viewcol;

            tvc = column->data;

            // Get the first renderer, it has the view-column value.
            renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT (tvc));
            cr0 = g_list_nth_data (renderers, 0);
            g_list_free (renderers);

            viewcol = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cr0), "view_column"));

            if (viewcol == COL_DEBIT && gnc_numeric_positive_p (xaccSplitGetAmount (split)))
                gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, tvc, TRUE);

            if (viewcol == COL_CREDIT && gnc_numeric_negative_p (xaccSplitGetAmount (split)))
                gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, tvc, TRUE);
        }
        g_list_free (columns);
    }
    else
        gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, NULL, FALSE);

    /* Scroll to cell, mid view */
    gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW (view), spath, NULL, TRUE, 0.5, 0.0);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);
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

    split = gnc_tree_model_split_reg_trans_get_split_equal_to_ancestor (trans, anchor);

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
    GtkTreePath  *new_mpath, *new_spath;
    gint *indices, sort_direction;
    gchar *sstring;

    ENTER("Move relative, view is %p, relative is %d", view, relative);

//FIXME Do we need to do some checks on relative maybe  -1,0,1 ??

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    indices = gtk_tree_path_get_indices (spath);

    if (model->sort_direction == GTK_SORT_DESCENDING)
        sort_direction = -1;
    else
        sort_direction = 1;

    new_spath = gtk_tree_path_new_from_indices (indices[0] + (relative * sort_direction), -1);

    // if relative == 0 we block all selection changes
    gnc_tree_view_split_reg_block_selection (view, TRUE);
    gtk_tree_selection_unselect_path (gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), spath);

    if (relative != 0)
        gnc_tree_view_split_reg_block_selection (view, FALSE);

    /* Set cursor to new spath */
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), new_spath, NULL, FALSE);

    if (relative == 0)
    {
        gnc_tree_view_split_reg_block_selection (view, FALSE);

        /* Get the new model path we are pointing at */
        new_mpath = gnc_tree_view_split_reg_get_model_path_from_sort_path (view, new_spath);

        /* As we are not emitting selection change, we need to save the current path ref */
        gnc_tree_view_split_reg_set_current_path (view, new_mpath);
        gtk_tree_path_free (new_mpath);
    }

    sstring = gtk_tree_path_to_string (new_spath);
    LEAVE("new_spath is %s", sstring);
    g_free (sstring);

    gtk_tree_path_free (new_spath);
    gtk_tree_path_free (mpath);
    gtk_tree_path_free (spath);
}


/* Enter the transaction */
void
gnc_tree_control_split_reg_enter (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    Transaction *btrans, *ctrans;
    gboolean goto_blank = FALSE;
    gboolean next_trans = TRUE;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    goto_blank = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                     GNC_PREF_ENTER_MOVES_TO_END);

    ENTER("view=%p, goto_blank = %s", view, goto_blank ? "TRUE" : "FALSE");

    btrans = gnc_tree_model_split_get_blank_trans (model);

    ctrans = gnc_tree_view_split_reg_get_current_trans (view);

    /* Are we on the blank transaction */
    if (btrans == ctrans)
        next_trans = FALSE;

    /* First record the transaction */
    if (gnc_tree_view_split_reg_enter (view))
    {
        /* Now move. */
        if (goto_blank)
            gnc_tree_control_split_reg_jump_to_blank (view);
        else if (next_trans)
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

    if (trans == NULL)
        return;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we proceed */
    if (gtc_sr_trans_open_and_warn (view, trans))
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
        warning = GNC_PREF_WARN_REG_SPLIT_DEL_ALL_RECD;
    }
    else
    {
        warning = GNC_PREF_WARN_REG_SPLIT_DEL_ALL;
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
    GncTreeModelSplitReg *model;
    Account *anchor;
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
        split = gnc_tree_control_split_reg_get_current_trans_split (view);
        if (split == NULL)
        {
            LEAVE("split is NULL");
            return;
        }
    }

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    trans = xaccSplitGetParent (split);

    if (trans == NULL)
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
        return;

    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    /* Deleting the blank split just cancels */
    {
        Split *blank_split = gnc_tree_control_split_reg_get_blank_split (view);

        if (split == blank_split)
            return;
    }

    /* Deleting the blank trans just cancels */
    {
        Transaction *blank_trans = gnc_tree_control_split_reg_get_blank_trans (view);

        if (trans == blank_trans)
            return;
    }

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
                                     "this register window. You may delete the "
                                     "entire transaction from this window, or you "
                                     "may navigate to a register that shows "
                                     "another side of this same transaction and "
                                     "delete the split from that register.");
        char *buf = NULL;
        const char *memo;
        const char *desc;
        char recn;
        if ((split == gnc_tree_control_split_reg_get_current_trans_split (view)) ||
            (split == gnc_tree_model_split_reg_trans_get_split_equal_to_ancestor (trans, anchor)))
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
            warning = GNC_PREF_WARN_REG_SPLIT_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_SPLIT_DEL;
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
            warning = GNC_PREF_WARN_REG_TRANS_DEL_RECD;
        }
        else
        {
            warning = GNC_PREF_WARN_REG_TRANS_DEL;
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
    GList *snode = NULL;
    gboolean changed = FALSE;

    ENTER(" ");

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    if (trans == NULL)
    {
        LEAVE("Trans is Null");
        return;
    }

    /* See if we were asked to reverse a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
    {
        LEAVE("Skip blank trans");
        return;
    }

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
    {
        LEAVE("Read only");
        return;
    }

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
    {
        LEAVE("Open in different register");
        return;
    }

    window = gnc_tree_view_split_reg_get_parent (view);

    if (xaccTransGetReversedBy (trans))
    {
        gnc_error_dialog (window, "%s",
                         _("A reversing entry has already been created for this transaction."));
        LEAVE("Already have reversing transaction");
        return;
    }

    /* Make sure we ask to commit any changes before we add reverse transaction */
    if (gtc_sr_trans_open_and_warn (view, trans))
    {
        LEAVE("save cancelled");
        return;
    }

    /* Create reverse transaction */
    new_trans = xaccTransReverse (trans);

    xaccTransBeginEdit (new_trans);

    /* Clear transaction level info */
    xaccTransSetDatePostedSecsNormalized (new_trans, gnc_time (NULL));
    xaccTransSetDateEnteredSecs (new_trans, gnc_time (NULL));

    xaccTransCommitEdit (new_trans);

    // We need to loop through the splits and send an event to update the register.
    for (snode = xaccTransGetSplitList (new_trans); snode; snode = snode->next)
    {
        if (xaccTransStillHasSplit (new_trans, snode->data))
        {
           /* Send an event based on the split account */
           qof_event_gen (QOF_INSTANCE (xaccSplitGetAccount(snode->data)), GNC_EVENT_ITEM_ADDED, snode->data);
        }
    }

    /* give gtk+ a chance to handle pending events */
    while (gtk_events_pending ())
        gtk_main_iteration ();

    /* Now jump to new trans */
    gnc_tree_control_split_reg_jump_to (view, NULL, xaccTransGetSplit (new_trans, 0), FALSE);

    LEAVE("Reverse transaction created");
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
    gboolean use_split_action_for_num_field = FALSE;

    ENTER("");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    blank_split = gnc_tree_control_split_reg_get_blank_split (view);
    split = gnc_tree_view_split_reg_get_current_split (view);
    trans_split = gnc_tree_control_split_reg_get_current_trans_split (view);


    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    use_split_action_for_num_field = qof_book_use_split_action_for_num_field (gnc_get_current_book());

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    /* This shouldn't happen, but be paranoid. */
    if (trans == NULL)
        return FALSE;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
    {
        LEAVE("Skip blank trans");
        return FALSE;
    }

    /* See if we were asked to change a blank split. */
    if (split == blank_split)
    {
        LEAVE("Skip blank split");
        return FALSE;
    }

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, trans))
    {
        LEAVE("Read only");
        return FALSE;
    }

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, trans))
    {
        LEAVE("Open in different register");
        return FALSE;
    }

    /* Make sure we ask to commit any changes before we proceed */
    if (gtc_sr_trans_open_and_warn (view, trans))
    {
        LEAVE("save cancelled");
        return FALSE;
    }

    window = gnc_tree_view_split_reg_get_parent (view);

    /* Ok, we are now ready to make the copy. */
    if (depth == SPLIT3)
    {
        Split *new_split;
        gboolean new_act_num = FALSE;
        char *out_num;
        time64 date;

        /* We are on a split in an expanded transaction.
         * Just copy the split and add it to the transaction.
         * However, if the split-action field is being used as the register 
         * number, and the action field is a number, request a new value or
         * cancel. Need to get next number and update account last num from
         * split account not register account, which may be the same or not */

        if (split != trans_split)
        {
            if (use_split_action_for_num_field && gnc_strisnum (gnc_get_num_action (NULL, split)))
            {
                Account *account = xaccSplitGetAccount (split);
                const char* title = _("New Split Information");
                const char *in_num = NULL;
                date = time (0);

                if (account)
                    in_num = xaccAccountGetLastNum (account);
                else
                    in_num = gnc_get_num_action (NULL, split);

                if (!gnc_dup_trans_dialog (window, title, FALSE,
                                           &date, in_num, &out_num, NULL, NULL))
                {
                    LEAVE("dup cancelled");
                    return FALSE;
                }
                new_act_num = TRUE;
            }

            new_split = xaccMallocSplit (gnc_get_current_book ());

            // Remove the blank split
            gnc_tree_model_split_reg_set_blank_split_parent (model, trans, TRUE);

            if (!xaccTransIsOpen (trans))
                xaccTransBeginEdit (trans);
            gnc_tree_view_split_reg_set_dirty_trans (view, trans);

            xaccSplitCopyOnto (split, new_split);
            xaccSplitSetParent (new_split, trans);

            // Add the blank split
            gnc_tree_model_split_reg_set_blank_split_parent (model, trans, FALSE);

            if (new_act_num) /* if new number supplied by user dialog */
                gnc_set_num_action (NULL, new_split, out_num, NULL);

            if (new_act_num && gnc_strisnum (out_num))
            {
                Account *account = xaccSplitGetAccount (new_split);

                /* If current register is for account, set last num */
                if (account == gnc_tree_model_split_reg_get_anchor (model))
                    xaccAccountSetLastNum (account, out_num);
            }
            if (new_act_num)
                g_free (out_num);
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
        int trans_split_index;
        int split_index;
        const char *in_num = NULL;
        const char *in_tnum = NULL;
        char *out_num;
        char *out_tnum;
        time64 date;
        gboolean use_autoreadonly = qof_book_uses_autoreadonly (gnc_get_current_book());

        /* We are on a transaction row. Copy the whole transaction. */

        date = time (0);
        if (gnc_strisnum (gnc_get_num_action (trans, trans_split)))
        {
            Account *account = gnc_tree_model_split_reg_get_anchor (model);

            if (account)
                in_num = xaccAccountGetLastNum (account);
            else
                in_num = gnc_get_num_action (trans, trans_split);
        }

        in_tnum = (use_split_action_for_num_field
                                        ? gnc_get_num_action (trans, NULL)
                                        : NULL);

        if (!gnc_dup_trans_dialog (window, NULL, TRUE,
                                   &date, in_num, &out_num, in_tnum, &out_tnum))
        {
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
                        "%s", _("The entered date of the duplicated transaction is older than the \"Read-Only Threshold\" set for this book. "
                                "This setting can be changed in File -> Properties -> Accounts."));
                gtk_dialog_run (GTK_DIALOG (dialog));
                gtk_widget_destroy (dialog);

                g_date_free (readonly_threshold);
                LEAVE("entered date older than read-only threshold");
                return FALSE;
            }
            g_date_free (readonly_threshold);
        }

        trans_split_index = xaccTransGetSplitIndex (trans, trans_split);

        new_trans = xaccMallocTransaction (gnc_get_current_book ());

        xaccTransBeginEdit (new_trans);

        xaccTransCopyOnto (trans, new_trans);

        xaccTransSetDatePostedSecsNormalized (new_trans, date);

        /* We also must set a new DateEntered on the new entry
         * because otherwise the ordering is not deterministic */
        xaccTransSetDateEnteredSecs(new_trans, gnc_time(NULL));

        /* set per book option */
        gnc_set_num_action (new_trans, NULL, out_num, out_tnum);

        if (gnc_strisnum (out_num))
        {
            Account *account = gnc_tree_model_split_reg_get_anchor (model);

            /* If current register is for account, set last num */
            if (account)
                xaccAccountSetLastNum (account, out_num);
        }

        if (use_split_action_for_num_field)
        {
            /* find split in new_trans that equals trans_split and set
             * split_action to out_num */
            gnc_set_num_action (NULL,
                                xaccTransGetSplit (new_trans, trans_split_index),
                                out_num, NULL);
            /* note that if the transaction has multiple splits to the register
             * account, only the anchor split will be set with user input. The
             * user will have to adjust other splits manually. */
        }

        xaccTransCommitEdit (new_trans);

        if (out_num != NULL)
           g_free (out_num);

        if (use_split_action_for_num_field && out_tnum != NULL)
            g_free (out_tnum);
    }
    LEAVE(" ");
    return TRUE;
}


static gboolean gtcsr_move_current_entry_updown(GncTreeViewSplitReg *view,
                                                gboolean move_up, gboolean really_do_it)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath = NULL, *spath = NULL, *spath_target = NULL, *mpath_target = NULL;
    GtkTreeIter m_iter, m_iter_target;
    gboolean resultvalue = FALSE;
    g_return_val_if_fail(view, FALSE);

    ENTER("");

    // The allocated memory references will all be cleaned up in the
    // updown_finish: label.

    model = gnc_tree_view_split_reg_get_model_from_view (view);
    g_return_val_if_fail(model, FALSE);

    if (model->sort_col != COL_DATE)
    {
        LEAVE("Not sorted by date - no up/down move available");
        return FALSE;
    }

    mpath = gnc_tree_view_split_reg_get_current_path (view);
    if (!mpath)
    {
        LEAVE("No current path available - probably on the blank split.");
        goto updown_finish;
    }

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);
    g_return_val_if_fail(spath, FALSE);

    spath_target = gtk_tree_path_copy(spath);
    if (move_up)
    {
        gboolean move_was_made = gtk_tree_path_prev(spath_target);
        if (!move_was_made)
        {
            LEAVE("huh, no path_prev() possible");
            goto updown_finish;
        }
    }
    else
    {
        gtk_tree_path_next(spath_target);
        // The path_next() function does not give a return value, see
        // https://mail.gnome.org/archives/gtk-list/2010-January/msg00171.html
    }

    if (gtk_tree_path_compare(spath, spath_target) == 0)
    {
        LEAVE("oops, paths are equal");
        goto updown_finish;
    }

    mpath_target = gnc_tree_view_split_reg_get_model_path_from_sort_path (view, spath_target);
    if (!mpath_target)
    {
        LEAVE("no path to target row");
        goto updown_finish;
    }

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter, mpath))
    {
        LEAVE("No iter for current row");
        goto updown_finish;
    }
    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter_target, mpath_target))
    {
        LEAVE("No iter for target row");
        goto updown_finish;
    }

    {
        gboolean is_blank, is_blank_target;
        Split *current_split, *target_split;
        Transaction *current_trans, *target_trans;
        gnc_tree_model_split_reg_get_split_and_trans (GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                                                      NULL, NULL, NULL, &is_blank,
                                                      &current_split, &current_trans);
        gnc_tree_model_split_reg_get_split_and_trans (GNC_TREE_MODEL_SPLIT_REG (model), &m_iter_target,
                                                      NULL, NULL, NULL, &is_blank_target,
                                                      &target_split, &target_trans);
        if (is_blank || is_blank_target)
        {
            LEAVE("blank split involved, ignored.");
            goto updown_finish;
        }
        if (xaccTransEqual(current_trans, target_trans, TRUE, FALSE, FALSE, FALSE))
        {
            LEAVE("two times the same txn, ignored.");
            goto updown_finish;
        }
        if (xaccTransGetIsClosingTxn(current_trans)
                || xaccTransGetIsClosingTxn(target_trans))
        {
            LEAVE("One of the txn is book-closing - no re-ordering allowed.");
            goto updown_finish;
        }

        /* Only continue if both have the same date and num, because the
         * "standard ordering" is tied to the date anyway. */
        {
            Timespec t1, t2;
            GDate d1 = xaccTransGetDatePostedGDate(current_trans),
                  d2 = xaccTransGetDatePostedGDate(target_trans);
            if (g_date_compare(&d1, &d2) != 0)
            {
                LEAVE("unequal DatePosted, ignoring");
                goto updown_finish;
            }
            if (g_strcmp0(xaccTransGetNum(current_trans),
                          xaccTransGetNum(target_trans)) != 0)
            {
                LEAVE("unequal Num, ignoring");
                goto updown_finish;
            }

            /* Special treatment if the equality doesn't hold if we access the
            dates as timespec. See the comment in gncEntrySetDateGDate() for the
            reason: Some code used the timespec at noon for the EntryDate, other
            code used the timespec at the start of day. */
            t1 = xaccTransRetDatePostedTS(current_trans);
            t2 = xaccTransRetDatePostedTS(target_trans);
            if (really_do_it && !timespec_equal(&t1, &t2))
            {
                /* Timespecs are not equal, even though the GDates were equal? Then
                we set the GDates again. This will force the timespecs to be equal
                as well. */
                xaccTransSetDatePostedGDate(current_trans, d1);
                xaccTransSetDatePostedGDate(target_trans, d2);
            }
        }

        // Check whether any of the two splits are frozen
        if (xaccSplitGetReconcile(current_split) == FREC
                || xaccSplitGetReconcile(target_split) == FREC)
        {
            LEAVE("either current or target split is frozen. No modification allowed.");
            goto updown_finish;
        }

        // If really_do_it is FALSE, we are only in query mode and shouldn't
        // modify anything. But if it is TRUE, please go ahead and do the move.
        if (really_do_it)
        {
            // Check whether any of the two splits are reconciled
            if (xaccSplitGetReconcile(current_split) == YREC
                    && !gnc_tree_control_split_reg_recn_test(view, spath))
            {
                LEAVE("current split is reconciled and user chose not to modify it");
                goto updown_finish;
            }
            if (xaccSplitGetReconcile(target_split) == YREC
                    && !gnc_tree_control_split_reg_recn_test(view, spath_target))
            {
                LEAVE("target split is reconciled and user chose not to modify it");
                goto updown_finish;
            }

            PINFO("Ok, about to switch ordering for current desc='%s' target desc='%s'",
                  xaccTransGetDescription(current_trans),
                  xaccTransGetDescription(target_trans));

            gnc_suspend_gui_refresh ();

            /* Swap the date-entered of both entries. That's already
             * sufficient! */
            {
                Timespec time_current = xaccTransRetDateEnteredTS(current_trans);
                Timespec time_target = xaccTransRetDateEnteredTS(target_trans);

                /* Special treatment for identical times (potentially caused
                 * by the "duplicate entry" command) */
                if (timespec_equal(&time_current, &time_target))
                {
                    g_warning("Surprise - both DateEntered are equal.");
                    /* We just increment the DateEntered of the previously
                     * lower of the two by one second. This might still cause
                     * issues if multiple entries had this problem, but
                     * whatever. */
                    if (move_up)
                        time_current.tv_sec++;
                    else
                        time_target.tv_sec++;
                }

                /* Write the new DateEntered. */
                xaccTransSetDateEnteredTS(current_trans, &time_target);
                xaccTransSetDateEnteredTS(target_trans, &time_current);

                /* FIXME: Do we need to notify anyone about the changed ordering? */
            }

            gnc_resume_gui_refresh ();

            LEAVE("two txn switched, done.");
        }
        resultvalue = TRUE;
        goto updown_finish;
    }
updown_finish:
    // memory cleanup
    //gtk_tree_path_free (mpath); // Should this be freed??
    gtk_tree_path_free(spath);
    gtk_tree_path_free(spath_target);
    gtk_tree_path_free(mpath_target);
    return resultvalue;
}

gboolean gnc_tree_control_split_reg_move_current_entry_updown (GncTreeViewSplitReg *view,
                                                            gboolean move_up)
{
    return gtcsr_move_current_entry_updown(view, move_up, TRUE);
}

gboolean gnc_tree_control_split_reg_is_current_movable_updown (GncTreeViewSplitReg *view,
                                                               gboolean move_up)
{
    return gtcsr_move_current_entry_updown(view, move_up, FALSE);
}


/* Save any open edited transactions on closing register */
gboolean
gnc_tree_control_split_reg_save (GncTreeViewSplitReg *view, gboolean reg_closing)
{
    GncTreeModelSplitReg *model;
    RowDepth depth;
    Transaction *dirty_trans;
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
    dirty_trans = gnc_tree_view_split_reg_get_dirty_trans (view);
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

    if (trans == dirty_trans )
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

                gtk_widget_grab_focus (gtk_dialog_get_widget_for_response (GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT));

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
gnc_tree_control_split_reg_recn_change (GncTreeViewSplitReg *view, GtkTreePath *spath)
{
    GtkWidget *dialog, *window;
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath;
    GtkTreeIter m_iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    Account *anchor;
    char rec;
    const gchar *title = _("Mark split as unreconciled?");
    const gchar *message =
        _("You are about to mark a reconciled split as unreconciled. Doing "
          "so might make future reconciliation difficult! Continue "
          "with this change?");
    gint response;

    ENTER(" ");

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    mpath = gnc_tree_view_split_reg_get_model_path_from_sort_path (view, spath);

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter, mpath))
    {
        gtk_tree_path_free (mpath);
        return FALSE;
    }

    gnc_tree_model_split_reg_get_split_and_trans (GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                                 &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    if (is_trow1 || is_trow2)
        split = xaccTransFindSplitByAccount (trans, anchor);

    rec = xaccSplitGetReconcile (split);

    if (rec != YREC)
    {
        gtk_tree_path_free (mpath);
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
    response = gnc_dialog_run (GTK_DIALOG (dialog), GNC_PREF_WARN_REG_RECD_SPLIT_UNREC);
    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_YES)
    {
        char rec = 'n';
        trans = xaccSplitGetParent (split);

        gnc_tree_view_split_reg_set_dirty_trans (view, trans);
        if (!xaccTransIsOpen (trans))
            xaccTransBeginEdit (trans);

        xaccSplitSetReconcile (split, rec);

        gtk_tree_path_free (mpath);
        LEAVE("mark split unreconciled");
        return TRUE;
    }
    gtk_tree_path_free (mpath);
    LEAVE("Canceled split unreconciled");
    return FALSE;
}


/* Test for splits being reconciled and decide to allow changes */
gboolean
gnc_tree_control_split_reg_recn_test (GncTreeViewSplitReg *view, GtkTreePath *spath)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath;
    GtkTreeIter m_iter;
    Split *split = NULL;
    Transaction *trans = NULL;
    gboolean is_trow1, is_trow2, is_split, is_blank;
    Account *anchor;
    char recn;

    ENTER(" ");

    /* This assumes we reset the flag whenever we change splits. */
    if (view->change_allowed)
    {
        LEAVE("change allowed is set");
        return TRUE;
    }

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    mpath = gnc_tree_view_split_reg_get_model_path_from_sort_path (view, spath);

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &m_iter, mpath))
    {
        gtk_tree_path_free (mpath);
        LEAVE("No path");
        return TRUE;
    }

    gnc_tree_model_split_reg_get_split_and_trans (GNC_TREE_MODEL_SPLIT_REG (model), &m_iter,
                                 &is_trow1, &is_trow2, &is_split, &is_blank, &split, &trans);

    if (is_trow1 || is_trow2)
        split = xaccTransFindSplitByAccount (trans, anchor);

    if (!split)
    {
        gtk_tree_path_free (mpath);
        LEAVE("No split");
        return TRUE;
    }

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
             _("You are about to change a reconciled split. Doing so might make "
               "future reconciliation difficult! Continue with this change?");
        }
        else
        {
            title = _("Change split linked to a reconciled split?");
            message =
            _("You are about to change a split that is linked to a reconciled split. "
              "Doing so might make future reconciliation difficult! Continue with this change?");
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
        response = gnc_dialog_run (GTK_DIALOG (dialog), GNC_PREF_WARN_REG_RECD_SPLIT_MOD);
        gtk_widget_destroy (dialog);

        if (response != GTK_RESPONSE_YES)
        {
            gtk_tree_path_free (mpath);
            LEAVE("cancel reconciled split");
            return FALSE;
        }
    }
    view->change_allowed = TRUE;
    gtk_tree_path_free (mpath);
    LEAVE(" ");
    return TRUE;
}


/* Return the account for name given or create it */ 
Account *
gnc_tree_control_split_reg_get_account_by_name (GncTreeViewSplitReg *view, const char *name)
{
    GtkWidget *window;
    const char *placeholder = _("The account %s does not allow transactions.");
    const char *missing = _("The account %s does not exist. "
                            "Would you like to create it?");
    char *account_name;
    Account *account;

    if (!name || (strlen(name) == 0))
        return NULL;

    /* Find the account */
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_SHOW_LEAF_ACCT_NAMES))
        account = gnc_account_lookup_by_name (gnc_get_current_root_account(), name);
    else
        account = gnc_account_lookup_by_full_name (gnc_get_current_root_account(), name);

    if (!account)
        account = gnc_account_lookup_by_code (gnc_get_current_root_account(), name);

    window = gnc_tree_view_split_reg_get_parent (view);

    if (!account)
    {
        /* Ask if they want to create a new one. */
        if (!gnc_verify_dialog (window, TRUE, missing, name))
            return NULL;

        /* User said yes, they want to create a new account. */
        account = gnc_ui_new_accounts_from_name_window (name);
        if (!account)
            return NULL;
    }
    /* Now have the account. */

    /* See if the account (either old or new) is a placeholder. */
    if (xaccAccountGetPlaceholder (account))
        gnc_error_dialog (window, placeholder, name);

    /* Be seeing you. */
    return account;
}

/*****************************************************************************
 *                         ClipBoard Functions                               *
 *****************************************************************************/
static Transaction *clipboard_trans = NULL;
/* Must never dereference. */
static const Account *clipboard_acct = NULL;


/* Return the split account for which ancestor is it's parent */
static Account *
gtc_sr_get_account_for_trans_ancestor (const Transaction *trans, const Account *ancestor)
{
    GList *node;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
        Split *split = node->data;
        Account *split_acc = xaccSplitGetAccount (split);

        if (!xaccTransStillHasSplit (trans, split))
            continue;

        if (ancestor == split_acc)
            return split_acc;

        if (ancestor && xaccAccountHasAncestor (split_acc, ancestor))
            return split_acc;
    }
    return NULL;
}


void
gnc_tree_control_split_reg_cut_trans (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    Transaction *from_trans;
    Account *anchor;

    g_return_if_fail (GNC_IS_TREE_VIEW_SPLIT_REG (view));

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    anchor = gnc_tree_model_split_reg_get_anchor (model);

    from_trans = gnc_tree_view_split_reg_get_current_trans (view);
    if (!from_trans)
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, from_trans))
        return;

    if (!xaccTransIsOpen (clipboard_trans))
        xaccTransBeginEdit (clipboard_trans);
    if (clipboard_trans)
        xaccTransDestroy (clipboard_trans);

    clipboard_trans = xaccTransCopyToClipBoard (from_trans);
    clipboard_acct = gtc_sr_get_account_for_trans_ancestor (from_trans, anchor);

    gnc_tree_view_split_reg_delete_current_trans (view);
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

    clipboard_acct = gtc_sr_get_account_for_trans_ancestor (from_trans, anchor);

    if (!xaccTransIsOpen (clipboard_trans))
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

    /* See if we are being edited in another register */
    if (gtc_sr_trans_test_for_edit (view, to_trans))
        return;

    /* Test for read only */
    if (gtc_sr_is_trans_readonly_and_warn (view, to_trans))
        return;

    //FIXME You can not paste from gl to a register, is this too simplistic
    if (clipboard_acct == NULL && anchor_acct != NULL)
    {
        GtkWidget *window;

        window = gnc_tree_view_split_reg_get_parent (view);
        gnc_error_dialog (window, "%s",
                         _("You can not paste from the general journal to a register."));
        return;
    }

    gnc_tree_view_split_reg_set_dirty_trans (view, to_trans);
    if (!xaccTransIsOpen (to_trans))
        xaccTransBeginEdit (to_trans);

    // Remove the blank split
    gnc_tree_model_split_reg_set_blank_split_parent (model, to_trans, TRUE);

    xaccTransCopyFromClipBoard (clipboard_trans, to_trans, clipboard_acct, anchor_acct, FALSE);

    // Add the blank split back
    gnc_tree_model_split_reg_set_blank_split_parent (model, to_trans, FALSE);

    // Refresh the view
    g_signal_emit_by_name (model, "refresh_trans", NULL);
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
            g_free (text);
            break;
        }
        g_free (text);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (desc_list), &iter);
    }
}


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
#include "gnc-gconf-utils.h"
#include "gnc-gdate-utils.h"
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
gtc_is_trans_readonly_and_warn (GncTreeViewSplitReg *view, Transaction *trans)
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
                "%s", _("The date of this transaction is older than the \"Read-Only Threshold\" set for this book.  "
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
#define gtc_trans_open_and_warn gnc_tree_control_split_reg_trans_open_and_warn
gboolean
gnc_tree_control_split_reg_trans_open_and_warn (GncTreeViewSplitReg *view, Transaction *trans)
{
    Transaction *dirty_trans;
    GtkWidget *window;
    GtkWidget *dialog;
    gint response;
    const char *title = _("Save Transaction before proceding?");
    const char *message =
            _("The current transaction has been changed. Would you like to "
              "record the changes before proceding, or cancel?");

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
        response = gnc_dialog_run (GTK_DIALOG (dialog), "transaction_being_edited");
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


#define gtc_trans_test_for_edit gnc_tree_control_split_reg_trans_test_for_edit
gboolean
gtc_trans_test_for_edit (GncTreeViewSplitReg *view, Transaction *trans)
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

void
gnc_tree_control_split_reg_parse_date (GDate *parsed, const char *datestr)
{
    int day, month, year;
    gboolean use_autoreadonly = qof_book_uses_autoreadonly (gnc_get_current_book ());

    if (!parsed) return;
    if (!datestr) return;

    if (!qof_scan_date (datestr, &day, &month, &year))
    {
        // Couldn't parse date, use today
        struct tm tm_today;
        gnc_tm_get_today_start (&tm_today);
        day = tm_today.tm_mday;
        month = tm_today.tm_mon + 1;
        year = tm_today.tm_year + 1900;
    }

    // If we have an auto-read-only threshold, do not accept a date that is
    // older than the threshold.
    if (use_autoreadonly)
    {
        GDate *d = g_date_new_dmy (day, month, year);
        GDate *readonly_threshold = qof_book_get_autoreadonly_gdate (gnc_get_current_book());
        if (g_date_compare (d, readonly_threshold) < 0)
        {
            g_warning("Entered date %s is before the \"auto-read-only threshold\"; resetting to the threshold.", datestr);
#if 0
            GtkWidget *dialog = gtk_message_dialog_new (NULL,
                                                       0,
                                                       GTK_MESSAGE_ERROR,
                                                       GTK_BUTTONS_OK,
                                                       "%s", _("Cannot store a transaction at this date"));
            gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                     "%s", _("The entered date of the new transaction is older than the \"Read-Only Threshold\" set for this book.  "
                                                             "This setting can be changed in File -> Properties -> Accounts."));
            gtk_dialog_run (GTK_DIALOG (dialog));
            gtk_widget_destroy (dialog);
#endif

            // Reset the date to the threshold date
            day = g_date_get_day (readonly_threshold);
            month = g_date_get_month (readonly_threshold);
            year = g_date_get_year (readonly_threshold);
        }
        g_date_free (d);
        g_date_free (readonly_threshold);
    }
    g_date_set_dmy (parsed, day, month, year);
}

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
    if (gtc_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we procede */
    if (gtc_trans_open_and_warn (view, trans))
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



#ifdef skip

/**
 * Schedules the current transaction for recurring-entry.
 * If the selected transaction was created from a scheduled transaction,
 * opens the editor for that Scheduled Transaction.
 **/
void
gnc_tree_control_split_reg_schedule_current_trans (GncTreeViewSplitReg *view)
{
    Transaction *trans;

    trans = gnc_tree_view_split_reg_get_current_trans (view);

    if (trans == NULL)
        return;

    /* See if we were asked to schedule a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    /* Test for read only */
    if (gtc_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we procede */
    if (gtc_trans_open_and_warn (view, trans))
        return;

    /* If the transaction has a sched-xact KVP frame, then go to the editor
     * for the existing SX; otherwise, do the sx-from-trans dialog. */
    {
        kvp_frame *txn_frame;
        kvp_value *kvp_val;
        /* set a kvp-frame element in the transaction indicating and
         * pointing-to the SX this was created from. */
        txn_frame = xaccTransGetSlots (trans);
        if ( txn_frame != NULL )
        {
            kvp_val = kvp_frame_get_slot (txn_frame, "from-sched-xaction");
            if (kvp_val)
            {
                GncGUID *fromSXId = kvp_value_get_guid (kvp_val);
                SchedXaction *theSX = NULL;
                GList *sxElts;

                /* Get the correct SX */
                for ( sxElts = gnc_book_get_schedxactions (gnc_get_current_book())->sx_list;
                        (!theSX) && sxElts;
                        sxElts = sxElts->next )
                {
                    SchedXaction *sx = (SchedXaction*)sxElts->data;
                    theSX =
                        ( ( guid_equal (xaccSchedXactionGetGUID (sx), fromSXId))
                          ? sx : NULL );
                }

                if (theSX)
                {
                    gnc_ui_scheduled_xaction_editor_dialog_create (theSX, FALSE);
                    return;
                }
            }
        }
    }
    gnc_sx_create_from_trans(pending_trans);
}


#endif




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
    if (gtc_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we procede */
    if (gtc_trans_open_and_warn (view, trans))
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


/* Jump to the Blank transaction, i.e. last in list */
void
gnc_tree_control_split_reg_jump_to_blank (GncTreeViewSplitReg *view)
{
    GncTreeModelSplitReg *model;
    GtkTreePath *mpath, *spath;

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_model_split_reg_get_path_to_split_and_trans (model, NULL, NULL);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    /* Set cursor to new spath */
    gtk_tree_view_set_cursor (GTK_TREE_VIEW (view), spath, NULL, FALSE);

    gtk_tree_path_free (spath);
    gtk_tree_path_free (mpath);

    /* scroll when view idle */
    g_idle_add ((GSourceFunc)gnc_tree_view_split_reg_scroll_to_cell, view );
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
    GtkTreePath  *new_mpath, *new_spath;
    gint *indices;

    ENTER("Move relative, view is %p, relative is %d", view, relative);

//FIXME Do we need to do some checks on relative maybe  -1,0,1 ??

    model = gnc_tree_view_split_reg_get_model_from_view (view);

    mpath = gnc_tree_view_split_reg_get_current_path (view);

    spath = gnc_tree_view_split_reg_get_sort_path_from_model_path (view, mpath);

    indices = gtk_tree_path_get_indices (spath);

    new_spath = gtk_tree_path_new_from_indices (indices[0] + (relative * view->sort_direction), -1);

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

//FIXME Might need more...

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
        if (goto_blank) //FIXME What do we want to do here...
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

    if (trans == NULL)
        return;

    /* See if we were asked to change a blank trans. */
    if (trans == gnc_tree_control_split_reg_get_blank_trans (view))
        return;

    /* Test for read only */
    if (gtc_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
        return;

    /* Make sure we ask to commit any changes before we procede */
    if (gtc_trans_open_and_warn (view, trans))
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
        split = gnc_tree_control_split_reg_get_current_trans_split (view);
        if (split == NULL)
        {
            LEAVE("split is NULL");
            return;
        }
    }

    trans = xaccSplitGetParent (split);

    if (trans == NULL)
        return;

    /* Test for read only */
    if (gtc_is_trans_readonly_and_warn (view, trans))
        return;

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
        return;

    depth = gnc_tree_view_reg_get_selected_row_depth (view);

    /* Deleting the blank split just cancels */
    {
        Split *blank_split = gnc_tree_control_split_reg_get_blank_split (view);

        if (split == blank_split)
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
    if (gtc_is_trans_readonly_and_warn (view, trans))
    {
        LEAVE("Read only");
        return;
    }

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
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
    if (gtc_trans_open_and_warn (view, trans))
    {
        LEAVE("save cancelled");
        return;
    }

    /* Create reverse transaction */
    new_trans = xaccTransReverse (trans);

    xaccTransBeginEdit (new_trans);

    /* Clear transaction level info */
    xaccTransSetDatePostedSecs (new_trans, gnc_time (NULL));
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
    if (gtc_is_trans_readonly_and_warn (view, trans))
    {
        LEAVE("Read only");
        return FALSE;
    }

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, trans))
    {
        LEAVE("Open in different register");
        return FALSE;
    }

    /* Make sure we ask to commit any changes before we procede */
    if (gtc_trans_open_and_warn (view, trans))
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

        trans_split_index = xaccTransGetSplitIndex (trans, trans_split);

        new_trans = xaccMallocTransaction (gnc_get_current_book ());

        xaccTransBeginEdit (new_trans);

        xaccTransCopyOnto (trans, new_trans);

        xaccTransSetDatePostedSecs (new_trans, date);

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
    if (gnc_gconf_get_bool (GCONF_GENERAL_REGISTER, "show_leaf_account_names", NULL))
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

    /* Test for read only */
    if (gtc_is_trans_readonly_and_warn (view, from_trans))
        return;

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

    /* See if we are being edited in another register */
    if (gtc_trans_test_for_edit (view, to_trans))
        return;

    /* Test for read only */
    if (gtc_is_trans_readonly_and_warn (view, to_trans))
        return;

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

    if (type == GTK_SORT_DESCENDING)
        view->sort_direction = -1;
    else
        view->sort_direction = 1;

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

//FIXME this may be needed for this one (!qof_book_use_split_action_for_num_field (gnc_get_current_book()))

    switch (depth) {
        case 1: // Number

        na = atoi (xaccTransGetNum (tnodea->data));
        nb = atoi (xaccTransGetNum (tnodeb->data));

        if (na < nb) return -1;
        if (na > nb) return +1;

        break;
        case 2: // Action

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

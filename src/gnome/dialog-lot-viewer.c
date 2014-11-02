/*******************************************************************\
 * dialog-lot-viewer.c -- a basic lot viewer for GnuCash            *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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

/* XXX todo: The button "view lot in register" is not implemented.
 *   it needs to open register window showing only the splits in the
 *     given lot ...
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "qof.h"
#include "gnc-lot.h"
#include "Scrub3.h"
#include "ScrubBusiness.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "gncInvoice.h"

#include "dialog-utils.h"
#include "dialog-lot-viewer.h"
#include "gnc-component-manager.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "misc-gnome-utils.h"
#include "tree-view-utils.h"

#define LOT_VIEWER_CM_CLASS "dialog-lot-viewer"

enum lot_cols
{
    LOT_COL_TYPE = 0,
    LOT_COL_OPEN,
    LOT_COL_CLOSE,
    LOT_COL_TITLE,
    LOT_COL_BALN,
    LOT_COL_GAINS,
    LOT_COL_PNTR,
    NUM_LOT_COLS
};

enum split_cols
{
    SPLIT_COL_DATE = 0,
    SPLIT_COL_NUM,
    SPLIT_COL_DESCRIPTION,
    SPLIT_COL_AMOUNT,
    SPLIT_COL_VALUE,
    SPLIT_COL_GAIN_LOSS,
    SPLIT_COL_BALANCE,
    SPLIT_COL_PNTR,
    NUM_SPLIT_COLS
};

#define RESPONSE_VIEW          1
#define RESPONSE_DELETE        2
#define RESPONSE_SCRUB_LOT     3
#define RESPONSE_SCRUB_ACCOUNT 4
#define RESPONSE_NEW_LOT       5

#define GNC_PREFS_GROUP "dialogs.lot-viewer"
#define GNC_PREF_HPOS   "hpane-position"
#define GNC_PREF_VPOS   "vpane-position"

struct _GNCLotViewer
{
    GtkWidget     * window;
#ifdef LOTS_READY_FOR_SHOWTIME
    GtkButton     * regview_button;
#endif
    GtkButton     * delete_button;
    GtkButton     * scrub_lot_button;
    GtkButton     * new_lot_button;
    GtkTreeView   * lot_view;
    GtkListStore  * lot_store;
    GtkTextView   * lot_notes;
    GtkEntry      * title_entry;
    GtkTreeView   * split_in_lot_view;
    GtkListStore  * split_in_lot_store;
    GtkTreeView   * split_free_view;
    GtkListStore  * split_free_store;
    GtkButton     * add_split_to_lot_button;
    GtkButton     * remove_split_from_lot_button;
    GtkToggleButton * only_show_open_lots_checkbutton;

    Account       * account;
    GNCLot        * selected_lot;
};

static void gnc_lot_viewer_fill (GNCLotViewer *lv);
static void gnc_split_viewer_fill (GNCLotViewer *lv, GtkListStore *store, SplitList *split_list);

/* ======================================================================== */
/* Callback prototypes */

void lv_title_entry_changed_cb (GtkEntry *ent, gpointer user_data);
void lv_response_cb (GtkDialog *dialog, gint response, gpointer data);
void lv_window_destroy_cb (GtkObject *object, gpointer user_data);
void lv_paned_notify_cb (GObject *gobject,
                         GParamSpec *pspec,
                         gpointer user_data);

/* ======================================================================== */
/* Get the realized gains for this lot.  This routine or a variant of it
 * should probably be moved to gnc-lot.c.
 * The conceptual difficulty here is that this works only if all of the
 * realized gains in the lot are of the
 */

static gnc_commodity *
find_first_currency (GNCLot *lot)
{
    SplitList *split_list, *node;

    split_list = gnc_lot_get_split_list(lot);
    for (node = split_list; node; node = node->next)
    {
        Split *s = node->data;
        Transaction *trans;
        if (FALSE == gnc_numeric_zero_p(xaccSplitGetAmount(s))) continue;
        trans = xaccSplitGetParent (s);
        return xaccTransGetCurrency (trans);
    }
    return NULL;
}

static gnc_numeric
get_realized_gains (GNCLot *lot, gnc_commodity *currency)
{
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric gains = zero;
    SplitList *split_list, *node;

    if (!currency) return zero;

    split_list = gnc_lot_get_split_list(lot);
    for (node = split_list; node; node = node->next)
    {
        Split *s = node->data;
        Transaction *trans;

        if (FALSE == gnc_numeric_zero_p(xaccSplitGetAmount(s))) continue;
        trans = xaccSplitGetParent (s);
        if (FALSE == gnc_commodity_equal (xaccTransGetCurrency(trans), currency)) continue;

        gains = gnc_numeric_add (gains, xaccSplitGetValue (s), GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    }
    return gains;
}


/* ======================================================================== */
/* Populate the lot split list view based on the currently selected lot */

static void
lv_show_splits_in_lot (GNCLotViewer *lv)
{
    GNCLot *lot = lv->selected_lot;
    SplitList *split_list;

    if (NULL == lot) return;

    split_list = gnc_lot_get_split_list (lot);
    gnc_split_viewer_fill(lv, lv->split_in_lot_store, split_list);
}

/* ======================================================================== */
/* Remove all splits from the split list view */

static void
lv_clear_splits_in_lot (GNCLotViewer *lv)
{
    gtk_list_store_clear (lv->split_in_lot_store);
}

/* ======================================================================== */
/* Populate the free split list view */

static void
lv_show_splits_free (GNCLotViewer *lv)
{
    SplitList *split_list, *node;
    SplitList *filtered_list = NULL;

    /* cleanup */
    gtk_list_store_clear (lv->split_free_store);

    /* get splits */
    split_list = xaccAccountGetSplitList(lv->account);

    /* filter splits */
    for (node = split_list; node; node = node->next)
    {
        Split *split = node->data;
        if (NULL == xaccSplitGetLot(split))
        {
            filtered_list = g_list_append(filtered_list, split);
        }
    }

    /* display list */
    gnc_split_viewer_fill(lv, lv->split_free_store, filtered_list);
}

/* ======================================================================== */
/* Save potential changes to the currently selected lot */

static void
lv_save_current_lot (GNCLotViewer *lv)
{
    GNCLot *lot = lv->selected_lot;
    const char * str;
    char * notes;

    if (lot)
    {
        gnc_lot_begin_edit(lot);

        /* Get the title, save_the_title */
        str = gtk_entry_get_text (lv->title_entry);
        gnc_lot_set_title (lot, str);

        /* Get the notes, save the notes */
        notes = xxxgtk_textview_get_text (lv->lot_notes);
        gnc_lot_set_notes (lot, notes);
        g_free(notes);

        gnc_lot_commit_edit(lot);
    }
}

/* ======================================================================== */
/* Clear all information related to the currently selected lot */

static void
lv_unset_lot (GNCLotViewer *lv)
{
    /* Set immediately, to avoid recursion in gtkentry "changed" cb. */
    lv->selected_lot = NULL;

    /* Blank the title widget */
    gtk_entry_set_text (lv->title_entry, "");
    gtk_editable_set_editable (GTK_EDITABLE(lv->title_entry), FALSE);

    /* Blank the notes area */
    xxxgtk_textview_set_text (lv->lot_notes, "");
    gtk_text_view_set_editable (lv->lot_notes, FALSE);

    /* Erase the mini-view area */
    lv_clear_splits_in_lot (lv);

#ifdef LOTS_READY_FOR_SHOWTIME
    gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), FALSE);
#endif
    gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), FALSE);
}

/* ======================================================================== */
/* Select a row in the lot list */

static void
lv_select_row (GNCLotViewer *lv,
               GNCLot       *lot)
{
    const char * str;

    lv_save_current_lot (lv);

    str = gnc_lot_get_title (lot);
    if (!str) str = "";
    gtk_entry_set_text (lv->title_entry, str);
    gtk_editable_set_editable (GTK_EDITABLE(lv->title_entry), TRUE);

    /* Set the notes field */
    str = gnc_lot_get_notes (lot);
    if (!str) str = "";
    xxxgtk_textview_set_text (lv->lot_notes, str);
    gtk_text_view_set_editable (lv->lot_notes, TRUE);

    /* Don't set until end, to avoid recursion in gtkentry "changed" cb. */
    lv->selected_lot = lot;
    lv_show_splits_in_lot (lv);

#ifdef LOTS_READY_FOR_SHOWTIME
    gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), TRUE);
#endif
    gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), TRUE);
}

/* ======================================================================== */
/* Un-select a row the the lot list */

static void
lv_unselect_row (GNCLotViewer *lv)
{
    lv_save_current_lot (lv);
    lv_unset_lot (lv);
}

/* ======================================================================== */
/* Populate the lot list view */

static void
gnc_lot_viewer_fill (GNCLotViewer *lv)
{
    LotList *lot_list, *node;
    GNCLot *this_lot, *selected_lot = NULL;
    GtkListStore *store;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkTreeSelection *selection;
    gboolean found = FALSE;

    lot_list = xaccAccountGetLotList (lv->account);

    selection = gtk_tree_view_get_selection(lv->lot_view);
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
        gtk_tree_model_get(model, &iter, LOT_COL_PNTR, &selected_lot, -1);

    /* Crazy. Should update in place if possible. */
    gtk_list_store_clear (lv->lot_store);

    for (node = lot_list; node; node = node->next)
    {
        char type_buff[200];
        char baln_buff[200];
        char gain_buff[200];
        GNCLot *lot = node->data;
        Split *esplit = gnc_lot_get_earliest_split (lot);
        Transaction *etrans = xaccSplitGetParent (esplit);
        time64 open_date = xaccTransGetDate (etrans);
        gnc_numeric amt_baln = gnc_lot_get_balance (lot);
        gnc_commodity *currency = find_first_currency (lot);
        gnc_numeric gains_baln = get_realized_gains (lot, currency);

        /* Skip closed lots when only open should be shown */
        if (TRUE == gtk_toggle_button_get_active(lv->only_show_open_lots_checkbutton) && gnc_lot_is_closed (lot))
        {
            continue;
        }

        store = lv->lot_store;
        gtk_list_store_append(store, &iter);

        /* Part of invoice */
        type_buff[0] = '\0';
        if ( NULL != gncInvoiceGetInvoiceFromLot(lot) )
        {
            snprintf(type_buff, 200, "I");
        }
        gtk_list_store_set(store, &iter, LOT_COL_TYPE, type_buff, -1);

        /* Opening date */
        gtk_list_store_set(store, &iter, LOT_COL_OPEN, open_date, -1);

        /* Closing date */
        if (gnc_lot_is_closed (lot))
        {
            Split *fsplit = gnc_lot_get_latest_split (lot);
            Transaction *ftrans = xaccSplitGetParent (fsplit);
            time64 close_date = xaccTransGetDate (ftrans);

            gtk_list_store_set(store, &iter, LOT_COL_CLOSE, close_date, -1);
        }
        else
        {
            gtk_list_store_set(store, &iter, LOT_COL_CLOSE, 0, -1);
        }

        /* Title */
        gtk_list_store_set(store, &iter, LOT_COL_TITLE, gnc_lot_get_title(lot), -1);

        /* Amount */
        xaccSPrintAmount (baln_buff, amt_baln,
                          gnc_account_print_info (lv->account, TRUE));
        gtk_list_store_set(store, &iter, LOT_COL_BALN, baln_buff, -1);

        /* Capital Gains/Losses Appreciation/Depreciation */
        xaccSPrintAmount (gain_buff, gains_baln,
                          gnc_commodity_print_info (currency, TRUE));
        gtk_list_store_set(store, &iter, LOT_COL_GAINS, gain_buff, -1);

        /* Self-reference */
        gtk_list_store_set(store, &iter, LOT_COL_PNTR, lot, -1);
    }
    g_list_free(lot_list);

    /* re-select the row that the user had previously selected,
     * if possible. */
    if (selected_lot)
    {
        model = GTK_TREE_MODEL(lv->lot_store);
        if (gtk_tree_model_get_iter_first(model, &iter))
        {
            do
            {
                gtk_tree_model_get(model, &iter, LOT_COL_PNTR, &this_lot, -1);
                if (this_lot == selected_lot)
                {
                    gtk_tree_selection_select_iter(selection, &iter);
                    found = TRUE;
                    break;
                }
            }
            while (gtk_tree_model_iter_next(model, &iter));
        }
    }

    if (!found)
        gtk_tree_selection_unselect_all(selection);
}

/* ======================================================================== */
/* Get selected split in a split list view */

static Split *
lv_get_selected_split (GNCLotViewer *lv, GtkTreeView *view)
{
    Split *split = NULL;
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    GtkTreeIter iter;

    selection = gtk_tree_view_get_selection(view);
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        gtk_tree_model_get(model, &iter, SPLIT_COL_PNTR, &split, -1);
    }

    return split;
}

/* ======================================================================== */
/* Check if split is main invoice split in lot */

static gboolean
lv_can_remove_split_from_lot(Split * split, GNCLot * lot)
{
    GncInvoice *lot_invoice, *txn_invoice;
    Transaction *txn;

    lot_invoice = gncInvoiceGetInvoiceFromLot(lot);
    txn = xaccSplitGetParent(split);
    txn_invoice = gncInvoiceGetInvoiceFromTxn(txn);
    if ( lot_invoice != NULL && lot_invoice == txn_invoice )
        return FALSE;

    return TRUE;
}

/* ======================================================================== */
/* Populate a split list view */

static void
gnc_split_viewer_fill (GNCLotViewer *lv, GtkListStore *store, SplitList *split_list)
{
    SplitList *node;
    GtkTreeIter iter;

    gnc_numeric baln = gnc_numeric_zero();
    gtk_list_store_clear (lv->split_in_lot_store);
    for (node = split_list; node; node = node->next)
    {
        Split *split = node->data;
        char amtbuff[200];
        char valbuff[200];
        char gainbuff[200];
        char balnbuff[200];
        gnc_commodity *currency;
        Transaction *trans = xaccSplitGetParent (split);
        time64 date = xaccTransGetDate (trans);
        gnc_numeric amnt, valu, gains;

        /* Do not show gains splits */
        if (gnc_numeric_zero_p (xaccSplitGetAmount(split))) continue;

        gtk_list_store_append(store, &iter);

        /* Date */
        gtk_list_store_set (store, &iter, SPLIT_COL_DATE, date, -1);

        /* Num  - retrieve number based on book option */
        gtk_list_store_set (store, &iter, SPLIT_COL_NUM,
                            gnc_get_num_action (trans, split), -1);

        /* Description */
        gtk_list_store_set (store, &iter, SPLIT_COL_DESCRIPTION, xaccTransGetDescription (trans), -1);

        /* Amount */
        amnt = xaccSplitGetAmount (split);
        xaccSPrintAmount (amtbuff, amnt,
                          gnc_account_print_info (lv->account, TRUE));
        gtk_list_store_set (store, &iter, SPLIT_COL_AMOUNT, amtbuff, -1);

        /* Value. Invert the sign on the first, opening entry. */
        currency = xaccTransGetCurrency (trans);
        valu = xaccSplitGetValue (split);
        if (node != split_list)
        {
            valu = gnc_numeric_neg (valu);
        }
        xaccSPrintAmount (valbuff, valu,
                          gnc_commodity_print_info (currency, TRUE));
        gtk_list_store_set (store, &iter, SPLIT_COL_VALUE, valbuff, -1);

        /* Gains. Blank if none. */
        gains = xaccSplitGetCapGains (split);
        if (gnc_numeric_zero_p(gains))
        {
            gainbuff[0] = 0;
        }
        else
        {
            xaccSPrintAmount (gainbuff, gains,
                              gnc_commodity_print_info (currency, TRUE));
        }
        gtk_list_store_set (store, &iter, SPLIT_COL_GAIN_LOSS, gainbuff, -1);

        /* Balance of Gains */
        baln = gnc_numeric_add_fixed (baln, amnt);
        if (gnc_numeric_zero_p(baln))
        {
            balnbuff[0] = 0;
        }
        else
        {
            xaccSPrintAmount (balnbuff, baln,
                              gnc_account_print_info (lv->account, TRUE));
        }
        gtk_list_store_set (store, &iter, SPLIT_COL_BALANCE, balnbuff, -1);

        /* Self-reference */
        gtk_list_store_set(store, &iter, SPLIT_COL_PNTR, split, -1);
    }
}

/* ======================================================================== */

static void
lv_update_split_buttons(GNCLotViewer *lv)
{
    Split * split;
    gtk_widget_set_sensitive(GTK_WIDGET(lv->add_split_to_lot_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(lv->remove_split_from_lot_button), FALSE);
    if (NULL != lv->selected_lot)
    {
        if (NULL != lv_get_selected_split(lv, lv->split_free_view) )
        {
            gtk_widget_set_sensitive(GTK_WIDGET(lv->add_split_to_lot_button), TRUE);
        }
        split = lv_get_selected_split(lv, lv->split_in_lot_view);
        if (NULL != split && TRUE == lv_can_remove_split_from_lot(split, lv->selected_lot))
        {
            gtk_widget_set_sensitive(GTK_WIDGET(lv->remove_split_from_lot_button), TRUE);
        }
    }
}

static void lv_refresh(GNCLotViewer * lv)
{
    gnc_lot_viewer_fill (lv);
    lv_show_splits_free (lv);
    lv_show_splits_in_lot (lv);
}

/* ======================================================================== */

static void
lv_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    lv_refresh (lv);
}

static void
lv_close_handler (gpointer user_data)
{
    GNCLotViewer *lv = user_data;

    lv_save_current_lot (lv);

    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(lv->window));
    gtk_widget_destroy (lv->window);
}

/* ===========================    Callbacks    ============================ */
/* ======================================================================== */
/* The lot title in the entry widget changed */

void
lv_title_entry_changed_cb (GtkEntry *ent, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GtkTreeModel *model;
    GtkTreeIter iter;
    GtkTreeSelection *selection;
    const char * title;
    title = gtk_entry_get_text (lv->title_entry);

    selection = gtk_tree_view_get_selection(lv->lot_view);
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        gtk_list_store_set(GTK_LIST_STORE(model), &iter, LOT_COL_TITLE, title, -1);
    }
}

/* ======================================================================== */
/* Selection in the lot list view changed */

static void
lv_selection_changed_cb (GtkTreeSelection *selection,
                         GNCLotViewer *lv)
{
    GNCLot *lot;
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        gtk_tree_model_get(model, &iter, LOT_COL_PNTR, &lot, -1);
        lv_select_row(lv, lot);
    }
    else
    {
        lv_unselect_row(lv);
    }
    lv_update_split_buttons(lv);
}

/* ======================================================================== */
/* Lot viewer window closed */

void
lv_window_destroy_cb (GtkObject *object, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    gnc_close_gui_component_by_data (LOT_VIEWER_CM_CLASS, lv);
    gnc_unregister_gui_component_by_data (LOT_VIEWER_CM_CLASS, lv);
    g_free (lv);
}

static void
lv_split_selection_changed_cb (GtkTreeSelection *selection,
                               GNCLotViewer *lv)
{
    lv_update_split_buttons(lv);
}

static void
lv_add_split_to_lot_cb (GtkWidget *widget, GNCLotViewer * lv)
{
    Split *split;

    if ( NULL == lv->selected_lot ) return;
    split = lv_get_selected_split(lv, lv->split_free_view);
    if ( NULL == split ) return;

    xaccAccountBeginEdit(lv->account);
    gnc_lot_add_split(lv->selected_lot, split);
    xaccAccountCommitEdit(lv->account);

    lv_refresh(lv);
}

static void
lv_remove_split_from_lot_cb (GtkWidget *widget, GNCLotViewer * lv)
{
    Split *split;

    if ( NULL == lv->selected_lot ) return;
    split = lv_get_selected_split(lv, lv->split_in_lot_view);
    if ( NULL == split ) return;

    if ( FALSE == lv_can_remove_split_from_lot(split, lv->selected_lot) )
        return;

    xaccAccountBeginEdit(lv->account);
    gnc_lot_remove_split(lv->selected_lot, split);
    xaccAccountCommitEdit(lv->account);

    lv_refresh(lv);
}

static void
lv_only_show_open_lots_changed_cb (GtkWidget *widget, GNCLotViewer * lv)
{
    lv_refresh(lv);
}

/* ======================================================================== */
/* Any button was pressed */

void
lv_response_cb (GtkDialog *dialog, gint response, gpointer data)
{
    GNCLotViewer *lv = data;
    GNCLot *lot = lv->selected_lot;

    switch (response)
    {
    case GTK_RESPONSE_CLOSE:
        lv_close_handler(lv);
        return;

    case RESPONSE_VIEW:
        if (NULL == lot)
            return;
        printf ("UNIMPLEMENTED: need to display register showing only this one lot \n");
        break;

    case RESPONSE_DELETE:
        if (NULL == lot)
            return;
        /* Prevent broken invoices */
        if (NULL != gncInvoiceGetInvoiceFromLot(lot))
            return;
        xaccAccountRemoveLot (gnc_lot_get_account(lot), lot);
        gnc_lot_destroy (lot);
        lv_unset_lot (lv);
        gnc_lot_viewer_fill (lv);
        break;

    case RESPONSE_SCRUB_LOT:
        if (NULL == lot)
            return;
        if (xaccAccountIsAPARType (xaccAccountGetType(lv->account)))
            gncScrubBusinessLot (lot);
        else
            xaccScrubLot (lot);
        gnc_lot_viewer_fill (lv);
        lv_show_splits_in_lot (lv);
        break;

    case RESPONSE_SCRUB_ACCOUNT:
        gnc_suspend_gui_refresh ();
        if (xaccAccountIsAPARType (xaccAccountGetType(lv->account)))
            gncScrubBusinessAccountLots (lv->account);
        else
            xaccAccountScrubLots (lv->account);
        gnc_resume_gui_refresh ();
        gnc_lot_viewer_fill (lv);
        lv_show_splits_free (lv);
        lv_show_splits_in_lot (lv);
        break;

    case RESPONSE_NEW_LOT:
        lv_save_current_lot (lv);
        lot = gnc_lot_make_default (lv->account);
        xaccAccountInsertLot (lv->account, lot);
        break;
    }
}

/* ======================================================================== */

static void print_date (GtkTreeViewColumn *tree_column,
                        GtkCellRenderer *cell,
                        GtkTreeModel *tree_model,
                        GtkTreeIter *iter,
                        gpointer data)
{
    GValue value = { 0 };
    time64 doc_date_time;
    gchar *doc_date_str = g_strdup (_("Open"));
    gint col = GPOINTER_TO_INT(data);

    g_return_if_fail (cell && iter && tree_model);

    gtk_tree_model_get_value (tree_model, iter, col, &value);
    doc_date_time = (time64) g_value_get_int64 (&value);
    g_value_unset (&value);

    if (doc_date_time) /* assumes 0 represents an invalid date/time */
    {
        g_free (doc_date_str);
        doc_date_str = qof_print_date (doc_date_time);
    }
    g_object_set (G_OBJECT (cell), "text", doc_date_str, NULL);
    g_free (doc_date_str);
}

static void
lv_init_lot_view (GNCLotViewer *lv)
{
    GtkTreeView *view;
    GtkListStore *store;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkCellRenderer *renderer;

    g_return_if_fail(GTK_IS_TREE_VIEW(lv->lot_view));

    view = lv->lot_view;
    store = gtk_list_store_new(NUM_LOT_COLS, G_TYPE_STRING, G_TYPE_INT64,
                               G_TYPE_INT64, G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_POINTER);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);
    lv->lot_store = store;

    /* Set up the columns */
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Type"), renderer,
             "text", LOT_COL_TYPE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_TYPE);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Opened"), renderer,
             "text", LOT_COL_OPEN, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_OPEN);
    tree_view_column_set_default_width (view, column, "31-12-2013");
    gtk_tree_view_column_set_cell_data_func (column, renderer,
                                             (GtkTreeCellDataFunc) print_date,
                                             GINT_TO_POINTER (LOT_COL_OPEN), NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Closed"), renderer,
             "text", LOT_COL_CLOSE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_CLOSE);
    tree_view_column_set_default_width (view, column, "31-12-2013");
    gtk_tree_view_column_set_cell_data_func (column, renderer,
                                             (GtkTreeCellDataFunc) print_date,
                                             GINT_TO_POINTER (LOT_COL_CLOSE), NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Title"), renderer,
             "text", LOT_COL_TITLE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_TITLE);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Balance"), renderer,
             "text", LOT_COL_BALN, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_BALN);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Gains"), renderer,
             "text", LOT_COL_GAINS, NULL);
    gtk_tree_view_column_set_sort_column_id(column, LOT_COL_GAINS);
    gtk_tree_view_append_column(view, column);

    /* Set up signals */
    selection =  gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(lv_selection_changed_cb), lv);
    g_signal_connect(lv->only_show_open_lots_checkbutton, "toggled",
                     G_CALLBACK(lv_only_show_open_lots_changed_cb), lv);

}

/* ======================================================================== */

static GtkListStore *
lv_init_split_view (GNCLotViewer *lv, GtkTreeView *view)
{
    GtkListStore *store;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkCellRenderer *renderer;

    g_return_val_if_fail(GTK_IS_TREE_VIEW(view), NULL);

    store = gtk_list_store_new(NUM_SPLIT_COLS, G_TYPE_INT64, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_POINTER);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    /* Set up the columns */
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Date"), renderer,
             "text", SPLIT_COL_DATE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_DATE);
    tree_view_column_set_default_width (view, column, "31-12-2013");
    gtk_tree_view_column_set_cell_data_func (column, renderer,
                                             (GtkTreeCellDataFunc) print_date,
                                             GINT_TO_POINTER (SPLIT_COL_DATE), NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Num"), renderer,
             "text", SPLIT_COL_NUM, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_NUM);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Description"), renderer,
             "text", SPLIT_COL_DESCRIPTION, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_DESCRIPTION);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Amount"), renderer,
             "text", SPLIT_COL_AMOUNT, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_AMOUNT);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Value"), renderer,
             "text", SPLIT_COL_VALUE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_VALUE);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Gain/Loss"), renderer,
             "text", SPLIT_COL_GAIN_LOSS, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_GAIN_LOSS);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Balance"), renderer,
             "text", SPLIT_COL_BALANCE, NULL);
    gtk_tree_view_column_set_sort_column_id(column, SPLIT_COL_BALANCE);
    gtk_tree_view_append_column(view, column);

    /* Set up the selection callbacks */
    selection =  gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(lv_split_selection_changed_cb), lv);

    return store;
}

static void
lv_init_split_views (GNCLotViewer *lv)
{
    lv->split_free_store = lv_init_split_view (lv, lv->split_free_view);
    lv->split_in_lot_store = lv_init_split_view (lv, lv->split_in_lot_view);
}

static void
lv_init_split_buttons (GNCLotViewer *lv)
{
    /* Set up the add/remove callbacks */
    g_signal_connect(G_OBJECT(lv->add_split_to_lot_button), "clicked",
                     G_CALLBACK(lv_add_split_to_lot_cb), lv);
    g_signal_connect(G_OBJECT(lv->remove_split_from_lot_button), "clicked",
                     G_CALLBACK(lv_remove_split_from_lot_cb), lv);
}

/* ======================================================================== */

static void
lv_create (GNCLotViewer *lv)
{
    gchar *win_title;
    gint position;
    GtkBuilder *builder;
    GtkWidget *widget;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-lot-viewer.glade", "Lot Viewer Window");

    lv->window = GTK_WIDGET(gtk_builder_get_object (builder, "Lot Viewer Window"));

    win_title = g_strdup_printf (_("Lots in Account %s"),
                                 xaccAccountGetName(lv->account));
    gtk_window_set_title (GTK_WINDOW (lv->window), win_title);
    g_free (win_title);

#ifdef LOTS_READY_FOR_SHOWTIME
    lv->regview_button = GTK_BUTTON(glade_xml_get_widget (builder, "regview button"));
#endif
    lv->delete_button = GTK_BUTTON(gtk_builder_get_object (builder, "delete button"));
    lv->scrub_lot_button = GTK_BUTTON(gtk_builder_get_object (builder, "scrub lot button"));
    lv->new_lot_button = GTK_BUTTON(gtk_builder_get_object (builder, "new lot button"));

    lv->lot_view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "lot view"));
    lv->only_show_open_lots_checkbutton = GTK_TOGGLE_BUTTON(gtk_builder_get_object (builder, "only show open lots checkbutton"));
    lv_init_lot_view(lv);
    lv->lot_notes = GTK_TEXT_VIEW(gtk_builder_get_object (builder, "lot notes text"));
    lv->title_entry = GTK_ENTRY (gtk_builder_get_object (builder, "lot title entry"));

    lv->split_in_lot_view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "split in lot view"));
    lv->split_free_view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "split free view"));
    lv_init_split_views(lv);

    lv->add_split_to_lot_button = GTK_BUTTON(gtk_builder_get_object (builder, "add split to lot button"));
    lv->remove_split_from_lot_button = GTK_BUTTON(gtk_builder_get_object (builder, "remove split from lot button"));
    lv_init_split_buttons(lv);


    if (gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
    {
        GObject *object;
        object = gtk_builder_get_object (builder, "lot vpaned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_VPOS, object, "position");

        object = gtk_builder_get_object (builder, "lot hpaned");
        gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_HPOS, object, "position");
    }

    lv->selected_lot = NULL;

    /* Setup signals */
    gtk_builder_connect_signals(builder, lv);
    g_object_unref(G_OBJECT(builder));

    lv_update_split_buttons(lv);

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(lv->window));
}

/* ======================================================================== */

GNCLotViewer *
gnc_lot_viewer_dialog (Account *account)
{
    GNCLotViewer *lv;
    gint component_id;

    if (!account) return NULL;

    lv = g_new0 (GNCLotViewer, 1);
    lv->account = account;
    lv_create (lv);
    gnc_lot_viewer_fill (lv);
    lv_show_splits_free (lv);

    component_id = gnc_register_gui_component (LOT_VIEWER_CM_CLASS,
                   lv_refresh_handler,
                   lv_close_handler,
                   lv);

    gnc_gui_component_watch_entity_type (component_id,
                                         GNC_ID_LOT,
                                         QOF_EVENT_CREATE | QOF_EVENT_ADD | QOF_EVENT_REMOVE | QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (lv->window);
    gnc_window_adjust_for_screen (GTK_WINDOW(lv->window));

    return lv;
}

/* ============================ END OF FILE =============================== */

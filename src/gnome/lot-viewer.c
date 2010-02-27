/*******************************************************************\
 * lot-viewer.c -- a basic lot viewer for GnuCash                   *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
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
 *
 * XXX clist should be probably be removed and replaced by the gnc_query_list
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
#include "Transaction.h"

#include "dialog-utils.h"
#include "lot-viewer.h"
#include "gnc-component-manager.h"
#include "gnc-ui-util.h"
#include "gnc-gconf-utils.h"
#include "misc-gnome-utils.h"

#define LOT_VIEWER_CM_CLASS "lot-viewer"

enum lot_cols
{
    LOT_COL_OPEN = 0,
    LOT_COL_CLOSE,
    LOT_COL_TITLE,
    LOT_COL_BALN,
    LOT_COL_GAINS,
    LOT_COL_PNTR,
    NUM_LOT_COLS
};

#define RESPONSE_VIEW          1
#define RESPONSE_DELETE        2
#define RESPONSE_SCRUB_LOT     3
#define RESPONSE_SCRUB_ACCOUNT 4
#define RESPONSE_NEW_LOT       5

#define GCONF_SECTION "dialogs/lot_viewer"
#define GCONF_KEY_HPOSITION "hpane_position"
#define GCONF_KEY_VPOSITION "vpane_position"

struct _GNCLotViewer
{
    GtkWidget     * window;
#ifdef LOTS_READY_FOR_SHOWTIME
    GtkButton     * regview_button;
#endif
    GtkButton     * delete_button;
    GtkButton     * scrub_lot_button;
    GtkButton     * new_lot_button;
    GtkPaned      * lot_hpaned;
    GtkPaned      * lot_vpaned;
    GtkTreeView   * lot_view;
    GtkListStore  * lot_store;
    GtkTextView   * lot_notes;
    GtkEntry      * title_entry;
    GtkCList      * mini_clist;

    Account       * account;
    GNCLot        * selected_lot;
};

static void gnc_lot_viewer_fill (GNCLotViewer *lv);

/* ======================================================================== */
/* Callback prototypes */

void lv_title_entry_changed_cb (GtkEntry *ent, gpointer user_data);
void lv_response_cb (GtkDialog *dialog, gint response, gpointer data);
void lv_window_destroy_cb (GtkObject *object, gpointer user_data);
void lv_paned_notify_cb (GObject *gobject,
                         GParamSpec *pspec,
                         gpointer user_data);

/* ======================================================================== */
/* Put the splits into the split clist */

#define MINI_DATE_COL 0
#define MINI_NUM_COL  1
#define MINI_DESC_COL 2
#define MINI_AMNT_COL 3
#define MINI_VALU_COL 4
#define MINI_GAIN_COL 5
#define MINI_BALN_COL 6
#define MINI_NUM_COLS 7

static void
lv_show_splits (GNCLotViewer *lv)
{
    GNCLot *lot = lv->selected_lot;
    SplitList *split_list, *node;
    gnc_numeric baln = gnc_numeric_zero();

    if (NULL == lot) return;

    /* qof_event_suspend();  XXX remove when xaccSplitGetCapGains() fixed */
    gtk_clist_freeze (lv->mini_clist);
    gtk_clist_clear (lv->mini_clist);
    split_list = gnc_lot_get_split_list (lot);
    for (node = split_list; node; node = node->next)
    {
        Split *split = node->data;
        char dbuff[MAX_DATE_LENGTH];
        char amtbuff[200];
        char valbuff[200];
        char gainbuff[200];
        char balnbuff[200];
        gnc_commodity *currency;
        Transaction *trans = xaccSplitGetParent (split);
        time_t date = xaccTransGetDate (trans);
        gnc_numeric amnt, valu, gains;
        const char *row_vals[MINI_NUM_COLS];
        int row;

        /* Do not show gains splits */
        if (gnc_numeric_zero_p (xaccSplitGetAmount(split))) continue;

        /* Opening date */
        qof_print_date_buff (dbuff, MAX_DATE_LENGTH, date);
        row_vals[MINI_DATE_COL] = dbuff;

        row_vals[MINI_NUM_COL]  = xaccTransGetNum (trans);
        row_vals[MINI_DESC_COL] = xaccTransGetDescription (trans);

        /* Amount */
        amnt = xaccSplitGetAmount (split);
        xaccSPrintAmount (amtbuff, amnt,
                          gnc_account_print_info (lv->account, TRUE));
        row_vals[MINI_AMNT_COL] = amtbuff;

        /* Value. Invert the sign on the first, opening entry. */
        currency = xaccTransGetCurrency (trans);
        valu = xaccSplitGetValue (split);
        if (node != split_list)
        {
            valu = gnc_numeric_neg (valu);
        }
        xaccSPrintAmount (valbuff, valu,
                          gnc_commodity_print_info (currency, TRUE));
        row_vals[MINI_VALU_COL] = valbuff;

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
        row_vals[MINI_GAIN_COL] = gainbuff;

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
        row_vals[MINI_BALN_COL] = balnbuff;

        /* Self-reference */
        row = gtk_clist_append (lv->mini_clist, (char **)row_vals);
        gtk_clist_set_selectable (lv->mini_clist, row, FALSE);
    }
    gtk_clist_thaw (lv->mini_clist);
    /* qof_event_resume();  XXX remove when xaccSplitGetCapGains() fixed */
}

/* ======================================================================== */

static void
lv_clear_splits (GNCLotViewer *lv)
{
    gtk_clist_clear (lv->mini_clist);
}

static void
lv_save_current_row (GNCLotViewer *lv)
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
/* Callback for selecting a row the the list-of-list clist */

static void
lv_select_row (GNCLotViewer *lv,
               GNCLot       *lot)
{
    const char * str;

    lv_save_current_row (lv);

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
    lv_show_splits (lv);

#ifdef LOTS_READY_FOR_SHOWTIME
    gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), TRUE);
#endif
    gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), TRUE);
    gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), TRUE);
}

/* ======================================================================== */

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
    lv_clear_splits (lv);

#ifdef LOTS_READY_FOR_SHOWTIME
    gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), FALSE);
#endif
    gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), FALSE);
    gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), FALSE);
}

/* ======================================================================== */
/* Callback for un-selecting a row the the list-of-list clist */

static void
lv_unselect_row (GNCLotViewer *lv)
{
    lv_save_current_row (lv);

    lv_unset_lot (lv);
}

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
}


/* ======================================================================== */
/* Callback when user types a new lot title into the entry widget */

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
/* Get the realized gains for this lot.  This routine or a varient of it
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

        gains = gnc_numeric_add (gains, xaccSplitGetValue (s), GNC_DENOM_AUTO, GNC_DENOM_FIXED);
    }
    return gains;
}

/* ======================================================================== */

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
        char obuff[MAX_DATE_LENGTH];
        char cbuff[MAX_DATE_LENGTH];
        char baln_buff[200];
        char gain_buff[200];
        GNCLot *lot = node->data;
        Split *esplit = gnc_lot_get_earliest_split (lot);
        Transaction *etrans = xaccSplitGetParent (esplit);
        time_t open_date = xaccTransGetDate (etrans);
        gnc_numeric amt_baln = gnc_lot_get_balance (lot);
        gnc_commodity *currency = find_first_currency (lot);
        gnc_numeric gains_baln = get_realized_gains (lot, currency);

        store = lv->lot_store;
        gtk_list_store_append(store, &iter);

        /* Opening date */
        qof_print_date_buff (obuff, MAX_DATE_LENGTH, open_date);
        gtk_list_store_set(store, &iter, LOT_COL_OPEN, obuff, -1);

        /* Closing date */
        if (gnc_lot_is_closed (lot))
        {
            Split *fsplit = gnc_lot_get_latest_split (lot);
            Transaction *ftrans = xaccSplitGetParent (fsplit);
            time_t close_date = xaccTransGetDate (ftrans);

            qof_print_date_buff (cbuff, MAX_DATE_LENGTH, close_date);
            gtk_list_store_set(store, &iter, LOT_COL_CLOSE, cbuff, -1);
        }
        else
        {
            gtk_list_store_set(store, &iter, LOT_COL_CLOSE, _("Open"), -1);
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

static void
lv_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    gnc_lot_viewer_fill (lv);
    lv_show_splits (lv);
}

static void
lv_close_handler (gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    GNCLot *lot = lv->selected_lot;

    lv_save_current_row (lv);

    gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(lv->window));
    gtk_widget_destroy (lv->window);
}

void
lv_window_destroy_cb (GtkObject *object, gpointer user_data)
{
    GNCLotViewer *lv = user_data;
    gnc_close_gui_component_by_data (LOT_VIEWER_CM_CLASS, lv);
    gnc_unregister_gui_component_by_data (LOT_VIEWER_CM_CLASS, lv);
    g_free (lv);
}


/* ======================================================================== */
/* Divider moved */

void
lv_paned_notify_cb (GObject *gobject,
                    GParamSpec *pspec,
                    gpointer user_data)
{
    const gchar *param_name;
    gint value;

    param_name = g_param_spec_get_name(pspec);
    if (strcmp(param_name, "position") != 0)
        return;
    g_object_get(gobject, "position", &value, NULL);

    if (GTK_IS_HPANED(gobject))
    {
        gnc_gconf_set_int(GCONF_SECTION, GCONF_KEY_HPOSITION, value, NULL);
    }
    else
    {
        gnc_gconf_set_int(GCONF_SECTION, GCONF_KEY_VPOSITION, value, NULL);
    }
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
        printf ("duude UNIMPLEMENTED: need to disply register showing only this one lot \n");
        break;

    case RESPONSE_DELETE:
        if (NULL == lot)
            return;
        xaccAccountRemoveLot (gnc_lot_get_account(lot), lot);
        gnc_lot_destroy (lot);
        lv_unset_lot (lv);
        gnc_lot_viewer_fill (lv);
        break;

    case RESPONSE_SCRUB_LOT:
        if (NULL == lot)
            return;
        xaccScrubLot (lot);
        gnc_lot_viewer_fill (lv);
        lv_show_splits (lv);
        break;

    case RESPONSE_SCRUB_ACCOUNT:
        gnc_suspend_gui_refresh ();
        xaccAccountScrubLots (lv->account);
        gnc_resume_gui_refresh ();
        gnc_lot_viewer_fill (lv);
        lv_show_splits (lv);
        break;

    case RESPONSE_NEW_LOT:
        lv_save_current_row (lv);
        lot = gnc_lot_make_default (lv->account);
        xaccAccountInsertLot (lv->account, lot);
        break;
    }
}

/* ======================================================================== */

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
    store = gtk_list_store_new(NUM_LOT_COLS, G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                               G_TYPE_POINTER);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);
    lv->lot_store = store;

    /* Set up the columns */
    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Opened"), renderer,
             "text", LOT_COL_OPEN, NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Closed"), renderer,
             "text", LOT_COL_CLOSE, NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Title"), renderer,
             "text", LOT_COL_TITLE, NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Balance"), renderer,
             "text", LOT_COL_BALN, NULL);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Gains"), renderer,
             "text", LOT_COL_GAINS, NULL);
    gtk_tree_view_append_column(view, column);

    /* Set up the selection callbacks */
    selection =  gtk_tree_view_get_selection(view);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(lv_selection_changed_cb), lv);
}

/* ======================================================================== */

static void
lv_create (GNCLotViewer *lv)
{
    GladeXML *xml;
    char win_title[251];
    gint position;

    xml = gnc_glade_xml_new ("lots.glade", "Lot Viewer Window");
    lv->window = glade_xml_get_widget (xml, "Lot Viewer Window");

    snprintf (win_title, 250, _("Lots in Account %s"),
              xaccAccountGetName(lv->account));
    gtk_window_set_title (GTK_WINDOW (lv->window), win_title);

#ifdef LOTS_READY_FOR_SHOWTIME
    lv->regview_button = GTK_BUTTON(glade_xml_get_widget (xml, "regview button"));
#endif
    lv->delete_button = GTK_BUTTON(glade_xml_get_widget (xml, "delete button"));
    lv->scrub_lot_button = GTK_BUTTON(glade_xml_get_widget (xml, "scrub lot button"));
    lv->new_lot_button = GTK_BUTTON(glade_xml_get_widget (xml, "new lot button"));

    lv->lot_view = GTK_TREE_VIEW(glade_xml_get_widget (xml, "lot view"));
    lv_init_lot_view(lv);
    lv->lot_notes = GTK_TEXT_VIEW(glade_xml_get_widget (xml, "lot notes text"));
    lv->title_entry = GTK_ENTRY (glade_xml_get_widget (xml, "lot title entry"));

    lv->lot_vpaned = GTK_PANED (glade_xml_get_widget (xml, "lot vpaned"));
    position = gnc_gconf_get_int(GCONF_SECTION, GCONF_KEY_VPOSITION, NULL);
    if (position)
        gtk_paned_set_position (lv->lot_vpaned, position);

    lv->lot_hpaned = GTK_PANED (glade_xml_get_widget (xml, "lot hpaned"));
    position = gnc_gconf_get_int(GCONF_SECTION, GCONF_KEY_HPOSITION, NULL);
    if (position)
        gtk_paned_set_position (lv->lot_hpaned, position);

    lv->mini_clist = GTK_CLIST(glade_xml_get_widget (xml, "mini clist"));

    lv->selected_lot = NULL;

    /* Setup signals */
    glade_xml_signal_autoconnect_full( xml,
                                       gnc_glade_autoconnect_full_func,
                                       lv);

    gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(lv->window));
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

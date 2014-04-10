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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/* This uses the clist widget, which I know is deprecated in gnome2. 
 * Sorry, I'll try to keep it real simple.
 *
 * For example: I'd like to allow the user to edit the lot
 * title 'in-place' in the clist, but gnome-1.2 does not allow
 * entry widgets in clist cells.
 *
 * XXX todo: The button "view lot in register" is not implemented.
 *   it needs to open register window showing only the splits in the 
 *     given lot ... 
 *
 * XXX clist should be probably be removed and replaced by the gnc_query_list
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>

#include "Account.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-event.h"
#include "gnc-lot.h"
#include "messages.h"
#include "Scrub3.h"
#include "Transaction.h"

#include "dialog-utils.h"
#include "lot-viewer.h"
#include "gnc-component-manager.h"
#include "gnc-ui-util.h"
#include "gnc-gconf-utils.h"
#include "misc-gnome-utils.h"

#define LOT_VIEWER_CM_CLASS "lot-viewer"

#define OPEN_COL  0
#define CLOSE_COL 1
#define TITLE_COL 2
#define BALN_COL  3
#define GAINS_COL 4
#define NUM_COLS  5

#define RESPONSE_VIEW          1
#define RESPONSE_DELETE        2
#define RESPONSE_SCRUB_LOT     3
#define RESPONSE_SCRUB_ACCOUNT 4

#define GCONF_SECTION "dialogs/lot_viewer"
#define GCONF_KEY_HPOSITION "hpane_position"
#define GCONF_KEY_VPOSITION "vpane_position"

struct _GNCLotViewer 
{
   GtkWidget     * window;
   GtkButton     * regview_button;
   GtkButton     * delete_button;
   GtkButton     * scrub_lot_button;
   GtkPaned      * lot_hpaned;
   GtkPaned      * lot_vpaned;
   GtkCList      * lot_clist;
   GtkTextView   * lot_notes;
   GtkEntry      * title_entry;
   GtkCList      * mini_clist;

   Account       * account;
   GNCLot        * selected_lot;
   int             selected_row;
};

static void gnc_lot_viewer_fill (GNCLotViewer *lv);

/* ======================================================================== */
/* Callback prototypes */

void  lv_select_row_cb (GtkCList       *clist,
			gint            row,
			gint            column,
			GdkEvent       *event,
			gpointer        user_data);
void lv_unselect_row_cb (GtkCList       *clist,
			 gint            row,
			 gint            column,
			 GdkEvent       *event,
			 gpointer        user_data);
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

/* gnc_engine_suspend_events();  XXX remove when xaccSplitGetCapGains() fixed */
   gtk_clist_freeze (lv->mini_clist);
   gtk_clist_clear (lv->mini_clist);
   split_list = gnc_lot_get_split_list (lot);
   for (node = split_list; node; node=node->next)
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
/* gnc_engine_resume_events();  XXX remove when xaccSplitGetCapGains() fixed */
}

/* ======================================================================== */

static void
lv_clear_splits (GNCLotViewer *lv)
{
   gtk_clist_clear (lv->mini_clist);
}

/* ======================================================================== */
/* Callback for selecting a row the the list-of-list clist */

void 
lv_select_row_cb (GtkCList       *clist,
                  gint            row,
                  gint            column,
                  GdkEvent       *event,
                  gpointer        user_data)
{
   GNCLotViewer *lv = user_data;
   GNCLot *lot;
   const char * str;

   lot = gtk_clist_get_row_data (clist, row);

   str = gnc_lot_get_title (lot);
   if (!str) str = "";
   gtk_entry_set_text (lv->title_entry, str);
   gtk_entry_set_editable (lv->title_entry, TRUE);
   
   /* Set the notes field */
   str = gnc_lot_get_notes (lot);
   if (!str) str = "";
   xxxgtk_textview_set_text (lv->lot_notes, str);
   gtk_text_view_set_editable (lv->lot_notes, TRUE);

   /* Don't set until end, to avoid recursion in gtkentry "changed" cb. */
   lv->selected_lot = lot;
   lv->selected_row = row;

   lv_show_splits (lv);

   gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), TRUE);
   gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), TRUE);
   gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), TRUE);
}

/* ======================================================================== */

static void 
lv_unset_lot (GNCLotViewer *lv)
{
   /* Set immediately, to avoid recursion in gtkentry "changed" cb. */
   lv->selected_lot = NULL;
   lv->selected_row = -1;

   /* Blank the title widget */
   gtk_entry_set_text (lv->title_entry, "");
   gtk_entry_set_editable (lv->title_entry, FALSE);

   /* Blank the notes area */
   xxxgtk_textview_set_text (lv->lot_notes, "");
   gtk_text_view_set_editable (lv->lot_notes, FALSE);

   /* Erase the mini-view area */
   lv_clear_splits (lv);

   gtk_widget_set_sensitive(GTK_WIDGET(lv->regview_button), FALSE);
   gtk_widget_set_sensitive(GTK_WIDGET(lv->delete_button), FALSE);
   gtk_widget_set_sensitive(GTK_WIDGET(lv->scrub_lot_button), FALSE);
}

/* ======================================================================== */
/* Callback for un-selecting a row the the list-of-list clist */

void 
lv_unselect_row_cb (GtkCList       *clist,
                    gint            row,
                    gint            column,
                    GdkEvent       *event,
                    gpointer        user_data)
{
   GNCLotViewer *lv = user_data;
   GNCLot *lot = lv->selected_lot;
   const char * str;
   char * notes;

   /* Get the title, plunk it into ctree */
   str = gtk_entry_get_text (lv->title_entry);
   gtk_clist_set_text (lv->lot_clist, row, TITLE_COL, str);

   if (lot)
   {
      gnc_lot_set_title (lot, str);

      /* Get the notes, save the notes */
      notes = xxxgtk_textview_get_text (lv->lot_notes);
      gnc_lot_set_notes (lot, notes);
      g_free(notes);
   }

   lv_unset_lot (lv);
}

/* ======================================================================== */
/* Callback when user types a new lot title into the entry widget */

void
lv_title_entry_changed_cb (GtkEntry *ent, gpointer user_data)
{
   GNCLotViewer *lv = user_data;
   const char * title;
   title = gtk_entry_get_text (lv->title_entry);
   if (0 > lv->selected_row) return; 
   gtk_clist_set_text (lv->lot_clist, lv->selected_row, TITLE_COL, title);
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
   for (node = split_list; node; node=node->next)
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
   for (node = split_list; node; node=node->next)
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
   int row;
   LotList *lot_list, *node;
   GNCLot *selected_lot;
   int selected_row = -1;

   lot_list = xaccAccountGetLotList (lv->account);

   selected_lot = lv->selected_lot;
   gtk_clist_freeze (lv->lot_clist);
   gtk_clist_clear (lv->lot_clist);
   for (node = lot_list; node; node=node->next)
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
      const char *row_vals[NUM_COLS];

      /* Opening date */
      qof_print_date_buff (obuff, MAX_DATE_LENGTH, open_date);
      row_vals[OPEN_COL] = obuff;

      /* Closing date */
      if (gnc_lot_is_closed (lot))
      {
         Split *fsplit = gnc_lot_get_latest_split (lot);
         Transaction *ftrans = xaccSplitGetParent (fsplit);
         time_t close_date = xaccTransGetDate (ftrans);
   
         qof_print_date_buff (cbuff, MAX_DATE_LENGTH, close_date);
         row_vals[CLOSE_COL] = cbuff;
      }
      else
      {
         row_vals[CLOSE_COL] = _("Open");
      }

      /* Title */
      row_vals[TITLE_COL] = gnc_lot_get_title (lot);
      
      /* Amount */
      xaccSPrintAmount (baln_buff, amt_baln, 
                 gnc_account_print_info (lv->account, TRUE));
      row_vals[BALN_COL] = baln_buff;

      /* Capital Gains/Losses Appreciation/Depreciation */
      xaccSPrintAmount (gain_buff, gains_baln, 
                 gnc_commodity_print_info (currency, TRUE));
      row_vals[GAINS_COL] = gain_buff;

      /* Self-reference */
      row = gtk_clist_append (lv->lot_clist, (char **)row_vals);
      gtk_clist_set_row_data (lv->lot_clist, row, lot);
      if (lot == selected_lot) selected_row = row;
   }
   gtk_clist_thaw (lv->lot_clist);

   /* re-select the row that the user had previously selected, 
    * if possible. */
   if (-1 < selected_row)
   {
      gtk_clist_select_row (lv->lot_clist, selected_row, 1);
   }
   else
   {
      gtk_clist_unselect_all (lv->lot_clist);
   }
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

   if (lot)
   {
      const char * str;
      char *notes;

      /* Get the title, save the title */
      str = gtk_entry_get_text (lv->title_entry);
      gnc_lot_set_title (lot, str);

      /* Get the notes, save the notes */
      notes = xxxgtk_textview_get_text (lv->lot_notes);
      gnc_lot_set_notes (lot, notes);
      g_free(notes);
   }

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

   if (GTK_IS_HPANED(gobject)) {
     gnc_gconf_set_int(GCONF_SECTION, GCONF_KEY_HPOSITION, value, NULL);
   } else {
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

   switch (response) {
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
     xaccAccountScrubLots (lv->account);
     gnc_lot_viewer_fill (lv);
     lv_show_splits (lv);
     break;
   }
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

   lv->regview_button = GTK_BUTTON(glade_xml_get_widget (xml, "regview button"));
   lv->delete_button = GTK_BUTTON(glade_xml_get_widget (xml, "delete button"));
   lv->scrub_lot_button = GTK_BUTTON(glade_xml_get_widget (xml, "scrub lot button"));

   lv->lot_clist = GTK_CLIST(glade_xml_get_widget (xml, "lot clist"));
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
   lv->selected_row = -1;
    
   /* Setup signals */
   glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     lv);

   gtk_signal_connect (GTK_OBJECT (lv->lot_clist), "select_row",
                      GTK_SIGNAL_FUNC (lv_select_row_cb), lv);

   gtk_signal_connect (GTK_OBJECT (lv->lot_clist), "unselect_row",
                      GTK_SIGNAL_FUNC (lv_unselect_row_cb), lv);

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
               GNC_EVENT_CREATE | GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

   gnome_window_icon_set_from_default(GTK_WINDOW(lv->window));
   gtk_widget_show_all (lv->window);
   gnc_window_adjust_for_screen (GTK_WINDOW(lv->window));

   return lv;
}

/* ============================ END OF FILE =============================== */

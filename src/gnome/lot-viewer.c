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
 * XXX need to save title, notes on window cliose
 * XXX need to delete data structs on window close.
 */

#define _GNU_SOURCE

#include "config.h"


#include <glib.h>
#include <gnome.h>

#include "Account.h"
#include "gnc-date.h"
#include "gnc-lot.h"
#include "kvp_frame.h"
#include "messages.h"
#include "Transaction.h"

#include "dialog-utils.h"
#include "lot-viewer.h"
#include "gnc-ui-util.h"
#include "misc-gnome-utils.h"

#define OPEN_COL  0
#define CLOSE_COL 1
#define TITLE_COL 2
#define BALN_COL  3
#define GAINS_COL 4
#define NUM_COLS  5

struct _GNCLotViewer 
{
   GtkWidget   * window;
   GtkCList    * lot_clist;
   GtkText     * lot_notes;
   GtkWidget   * reg_area;
   GtkEntry    * title_entry;

   Account     * account;
   GNCLot      * selected_lot;
   int           selected_row;
};

/* ======================================================================== */

static void 
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

   str = kvp_frame_get_string (gnc_lot_get_slots (lot), "/title");
   if (!str) str = "";
printf ("duuude row elect =%d %p the title=%s\n",row, lot, str);
   gtk_entry_set_text (lv->title_entry, str);
   gtk_entry_set_editable (lv->title_entry, TRUE);
   
   /* Set the notes field */
   str = kvp_frame_get_string (gnc_lot_get_slots (lot), "/notes");
   if (!str) str = "";
   xxxgtk_text_set_text (lv->lot_notes, str);
   gtk_text_set_editable (lv->lot_notes, TRUE);

   /* Don't set until end, to avoid recursion in gtkentry "changed" cb. */
   lv->selected_lot = lot;
   lv->selected_row = row;
}

/* ======================================================================== */

static void 
lv_unselect_row_cb (GtkCList       *clist,
                    gint            row,
                    gint            column,
                    GdkEvent       *event,
                    gpointer        user_data)
{
   GNCLotViewer *lv = user_data;
   GNCLot *lot = lv->selected_lot;
   const char * str;

   /* Set immediately, to avoid recursion in gtkentry "changed" cb. */
   lv->selected_lot = NULL;
   lv->selected_row = -1;

   /* Get the title, blank the title widget */
   str = gtk_entry_get_text (lv->title_entry);
printf ("duuude row unselect =%d %p new tite=%s\n",row, lot, str);
   gtk_clist_set_text (lv->lot_clist, row, column, str);
   kvp_frame_set_str (gnc_lot_get_slots (lot), "/title", str);
   gtk_entry_set_text (lv->title_entry, "");
   gtk_entry_set_editable (lv->title_entry, FALSE);

   /* Get the notes, blank the notes area */
   str = xxxgtk_text_get_text (lv->lot_notes);
   kvp_frame_set_str (gnc_lot_get_slots (lot), "/notes", str);
   xxxgtk_text_set_text (lv->lot_notes, "");
   gtk_text_set_editable (lv->lot_notes, FALSE);
}

/* ======================================================================== */

static void
lv_title_entry_changed_cb (GtkEntry *ent, gpointer user_data)
{
   GNCLotViewer *lv = user_data;
   char * title;
   title = gtk_entry_get_text (lv->title_entry);
printf ("duude entry changed=%s sel=%d\n", title, lv->selected_row);
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
printf ("duude gains = %lld/%lld\n", gains.num, gains.denom);
   }
   return gains;
}

/* ======================================================================== */

static void
gnc_lot_viewer_fill (GNCLotViewer *lv)
{
   int row, nlots;
   LotList *lot_list, *node;

   lot_list = xaccAccountGetLotList (lv->account);
   nlots = g_list_length (lot_list);

printf ("duude got %d lots\n", nlots);
   gtk_clist_freeze (lv->lot_clist);
   for (node = lot_list; node; node=node->next)
   {
      char obuff[MAX_DATE_LENGTH];
      char cbuff[MAX_DATE_LENGTH];
      GNCLot *lot = node->data;
      Split *esplit = gnc_lot_get_earliest_split (lot);
      Transaction *etrans = xaccSplitGetParent (esplit);
      time_t open_date = xaccTransGetDate (etrans);
      KvpFrame *kvp = gnc_lot_get_slots (lot);
      gnc_numeric amt_baln = gnc_lot_get_balance (lot);
      gnc_commodity *currency = find_first_currency (lot);
      gnc_numeric gains_baln = get_realized_gains (lot, currency);
      char *row_vals[NUM_COLS];

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
      row_vals[TITLE_COL] = kvp_frame_get_string (kvp, "/title");
      
      /* Amount */
      row_vals[BALN_COL] = (char *) xaccPrintAmount (amt_baln, 
                 gnc_account_print_info (lv->account, TRUE));

      /* Capital Gains/Losses Appreciation/Depreciation */
      row_vals[GAINS_COL] = (char *) xaccPrintAmount (gains_baln, 
                 gnc_commodity_print_info (currency, TRUE));


      /* Self-reference */
      row = gtk_clist_append (lv->lot_clist, row_vals);
      gtk_clist_set_row_data (lv->lot_clist, row, lot);
printf ("duude amt = %lld/%lld\n", amt_baln.num, amt_baln.denom);
printf ("duude amt %s row %d\n", row_vals[BALN_COL], row);

   }
   gtk_clist_thaw (lv->lot_clist);

}

/* ======================================================================== */

GNCLotViewer * 
gnc_lot_viewer_dialog (GtkWidget *parent, Account *account)
{
   GNCLotViewer *lv;
   GladeXML *xml;

   lv = g_new0 (GNCLotViewer, 1);

   xml = gnc_glade_xml_new ("lots.glade", "Lot Viewer Window");
   lv->window = glade_xml_get_widget (xml, "Lot Viewer Window");

   lv->lot_clist = GTK_CLIST(glade_xml_get_widget (xml, "lot clist"));
   lv->lot_notes = GTK_TEXT(glade_xml_get_widget (xml, "lot notes text"));
   lv->title_entry = GTK_ENTRY (glade_xml_get_widget (xml, "lot title entry"));
   lv->reg_area = glade_xml_get_widget (xml, "lot reg area scrolledwindow");
   lv->account = account;
   lv->selected_lot = NULL;
   lv->selected_row = -1;
   
   gtk_signal_connect (GTK_OBJECT (lv->lot_clist), "select_row",
                      GTK_SIGNAL_FUNC (lv_select_row_cb), lv);

   gtk_signal_connect (GTK_OBJECT (lv->lot_clist), "unselect_row",
                      GTK_SIGNAL_FUNC (lv_unselect_row_cb), lv);

   gtk_signal_connect (GTK_OBJECT (lv->title_entry), "changed",
                      GTK_SIGNAL_FUNC (lv_title_entry_changed_cb), lv);

   gnc_lot_viewer_fill (lv);
   return lv;
}

/* ============================ END OF FILE =============================== */

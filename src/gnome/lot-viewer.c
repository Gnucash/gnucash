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

/* This uses a ctree, which I know is deprecated in gnome2. Sorry,
 * I'll try to keep it real simple.
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

struct _GNCLotViewer 
{
   GtkWidget   * window;
   GtkCList    * lot_clist;
   GtkText     * lot_notes;
   GtkWidget   * reg_area;

   Account     * account;
};

/* ======================================================================== */

static void 
lv_select_row_cb (GtkCList       *clist,
                  gint            row,
                  gint            column,
                  GdkEvent       *event,
                  gpointer user_data)
{
   // GNCLotViewer *lv = user_data;
   GNCLot *lot;

   lot = gtk_clist_get_row_data (clist, row);

printf ("duuude row selected =%d %p\n",row, lot);
}

/* ======================================================================== */
#define OPEN_COL  0
#define CLOSE_COL 1
#define TITLE_COL 2
#define BALN_COL  3
#define VALUE_COL 4
#define NUM_COLS  5

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
      char *row_vals[NUM_COLS];


      /* Opening date */
      qof_print_date_buff (obuff, MAX_DATE_LENGTH, open_date);
      row_vals[OPEN_COL] = obuff;
printf ("duude open %s\n", obuff);
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

       // gnc_split_value_print_info
      row_vals[VALUE_COL] = "xxx";

      /* Self-reference */
      row = gtk_clist_append (lv->lot_clist, row_vals);
      gtk_clist_set_row_data (lv->lot_clist, row, lot);
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
   lv->reg_area = glade_xml_get_widget (xml, "lot reg area scrolledwindow");
   lv->account = account;

   
   gtk_signal_connect (GTK_OBJECT (lv->lot_clist), "select_row",
                      GTK_SIGNAL_FUNC (lv_select_row_cb), lv);

   gnc_lot_viewer_fill (lv);
   return lv;
}

/* ============================ END OF FILE =============================== */

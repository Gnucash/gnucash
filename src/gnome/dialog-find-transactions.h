/********************************************************************\
 * dialog-find-transactions.h : locate transactions/splits matching *
 *                              criteria.                           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
\********************************************************************/

#ifndef __DIALOG_FIND_TRANSACTIONS_H_
#define __DIALOG_FIND_TRANSACTIONS_H_

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "MultiLedger.h"
#include "Query.h"

#include "glade-gnc-dialogs.h"

typedef struct {
  GtkWidget * dialog;
  GtkWidget * cal;
  GtkWidget * entry_1;
  GtkWidget * entry_2;
  GtkWidget * entry_3;
  char      * ymd_format;

} SelectDateDialog;

typedef struct {
  GtkWidget  * dialog;
  Query      * q;
  xaccLedgerDisplay * ledger;
  
  char       * ymd_format;

  int        search_type;

  GtkWidget  * new_search_radiobutton;
  GtkWidget  * narrow_search_radiobutton;
  GtkWidget  * add_search_radiobutton;
  GtkWidget  * delete_search_radiobutton;

  GtkWidget  * match_accounts_picker;
  GtkWidget  * match_accounts_scroller;
  GtkWidget  * account_tree;

  GtkWidget  * date_start_toggle;
  GtkWidget  * date_start_frame;
  GtkWidget  * date_start_entry;

  GtkWidget  * date_end_toggle;
  GtkWidget  * date_end_frame;
  GtkWidget  * date_end_entry;

  GtkWidget  * description_entry;
  GtkWidget  * description_case_toggle;
  GtkWidget  * description_regexp_toggle;

  GtkWidget  * number_entry;
  GtkWidget  * number_case_toggle;
  GtkWidget  * number_regexp_toggle;

  GtkWidget  * credit_debit_picker;
  GtkWidget  * amount_comp_picker;
  GtkWidget  * amount_entry;

  GtkWidget  * memo_entry;
  GtkWidget  * memo_case_toggle;
  GtkWidget  * memo_regexp_toggle;

  GtkWidget  * shares_comp_picker;
  GtkWidget  * shares_entry;

  GtkWidget  * price_comp_picker;
  GtkWidget  * price_entry;
  
  GtkWidget  * action_entry;
  GtkWidget  * action_case_toggle;
  GtkWidget  * action_regexp_toggle;

  GtkWidget  * cleared_not_cleared_toggle;
  GtkWidget  * cleared_cleared_toggle;
  GtkWidget  * cleared_reconciled_toggle;

  GtkWidget  * balance_balanced_toggle;
  GtkWidget  * balance_not_balanced_toggle;

  GtkWidget  * tag_entry;
  GtkWidget  * tag_case_toggle;
  GtkWidget  * tag_regexp_toggle;

} FindTransactionsDialog;

FindTransactionsDialog * 
gnc_ui_find_transactions_dialog_create(xaccLedgerDisplay * ledger);

void gnc_ui_find_transactions_dialog_destroy(FindTransactionsDialog * pcd);
void gnc_ui_find_transactions_dialog_ok_cb(GtkButton * button,
                                           gpointer  user_data);
void gnc_ui_find_transactions_dialog_cancel_cb(GtkButton * button,
                                               gpointer user_data);

SelectDateDialog *
gnc_ui_select_date_dialog_create(GtkWidget * w1, GtkWidget * w2, 
                                 GtkWidget * w3, char * ymd_format);
void gnc_ui_select_date_dialog_destroy(SelectDateDialog * sdd);
void gnc_ui_select_date_cancel_cb(GtkWidget * w, gpointer user_data);
void gnc_ui_select_date_ok_cb(GtkWidget * w, gpointer user_data);

#endif

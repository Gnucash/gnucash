/********************************************************************\
 * dialog-find-transactions.c : locate transactions and show them   *
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

#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <gnome.h>
#include <guile/gh.h>
#include <time.h>
#include <assert.h>

#include "top-level.h"
#include "messages_i18n.h"
#include "ui-callbacks.h"
#include "RegWindow.h"
#include "window-register.h"
#include "account-tree.h"
#include "MultiLedger.h"
#include "FileDialog.h"
#include "splitreg.h"
#include "glade-cb-gnc-dialogs.h"
#include "dialog-find-transactions.h"
#include "window-help.h"
#include "Query.h"


/********************************************************************\
 * gnc_ui_find_transactions_dialog_create
 * make a new print check dialog and wait for it.
\********************************************************************/

FindTransactionsDialog * 
gnc_ui_find_transactions_dialog_create(xaccLedgerDisplay * orig_ledg) {
  FindTransactionsDialog * ftd = g_new0(FindTransactionsDialog, 1);  
  SCM        lookup_option, lookup_value;

  /* call the glade-defined creator */
  ftd->dialog = create_Find_Transactions();
  ftd->ledger = orig_ledg;

  if(orig_ledg) {
    ftd->q = orig_ledg->query;
  }
  else {
    ftd->q = NULL;
  }
  
  /* initialize the radiobutton state vars */
  ftd->search_type = 0;

  /* find the important widgets */
  ftd->new_search_radiobutton = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "new_search_radiobutton");
  ftd->narrow_search_radiobutton = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "narrow_search_radiobutton");
  ftd->add_search_radiobutton = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "add_search_radiobutton");
  ftd->delete_search_radiobutton = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "delete_search_radiobutton");

  ftd->match_accounts_scroller =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "account_match_scroller");
  ftd->match_accounts_picker = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "account_match_picker");

  ftd->date_start_entry_1 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_start_entry_1");
  ftd->date_start_entry_2 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_start_entry_2");
  ftd->date_start_entry_3 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_start_entry_3");
  ftd->date_end_entry_1 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_end_entry_1");
  ftd->date_end_entry_2 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_end_entry_2");
  ftd->date_end_entry_3 =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "date_end_entry_3");

  ftd->description_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "description_entry");
  ftd->description_case_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "description_case_toggle");
  ftd->description_regexp_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "description_regexp_toggle");

  ftd->number_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "number_entry");
  ftd->number_case_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "number_case_toggle");
  ftd->number_regexp_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "number_regexp_toggle");

  ftd->credit_debit_picker =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "credit_debit_picker");
  ftd->amount_comp_picker =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "amount_comp_picker");
  ftd->amount_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "amount_entry");

  ftd->memo_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "memo_entry");
  ftd->memo_case_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "memo_case_toggle");
  ftd->memo_regexp_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "memo_regexp_toggle");

  ftd->shares_comp_picker =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "shares_comp_picker");
  ftd->shares_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "shares_entry");

  ftd->price_comp_picker =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "price_comp_picker");
  ftd->price_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "price_entry");

  ftd->action_entry =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "action_entry");
  ftd->action_case_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "action_case_toggle");
  ftd->action_regexp_toggle =
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "action_regexp_toggle");

  ftd->cleared_cleared_toggle = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "cleared_cleared_toggle");
  ftd->cleared_reconciled_toggle = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "cleared_reconciled_toggle");
  ftd->cleared_not_cleared_toggle = 
    gtk_object_get_data(GTK_OBJECT(ftd->dialog), "cleared_not_cleared_toggle");

  /* add an account picker to the first tab */
  ftd->account_tree = gnc_account_tree_new();
  gtk_clist_column_titles_hide(GTK_CLIST(ftd->account_tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(ftd->account_tree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(ftd->account_tree));
    
  gtk_container_add(GTK_CONTAINER(ftd->match_accounts_scroller), 
                    ftd->account_tree);
  gtk_clist_set_selection_mode(GTK_CLIST(ftd->account_tree),
                               GTK_SELECTION_MULTIPLE);
  gtk_widget_set_usize(GTK_WIDGET(ftd->match_accounts_scroller),
                       300, 300);
  
  /* initialize optionmenus with data indicating which option */
  /* is selected */
  gnc_option_menu_init(ftd->match_accounts_picker);
  gnc_option_menu_init(ftd->credit_debit_picker);
  gnc_option_menu_init(ftd->amount_comp_picker);
  gnc_option_menu_init(ftd->shares_comp_picker);
  gnc_option_menu_init(ftd->price_comp_picker);

  /* initialize the date to something reasonable */
  lookup_option = gh_eval_str("gnc:lookup-global-option");
  lookup_value  = gh_eval_str("gnc:option-value");
  
  ftd->ymd_format = 
    gh_symbol2newstr(gh_call1(lookup_value,
                              gh_call2(lookup_option,
                                       gh_str02scm("International"),
                                       gh_str02scm("Date Format"))),
                     NULL);
  if(!strcmp(ftd->ymd_format, "us") || 
     !strcmp(ftd->ymd_format, "uk") ||
     !strcmp(ftd->ymd_format, "europe")) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_1),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_2),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_3),
                              1900.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_1),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_2),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_3),
                              2100.0);
  }
  else {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_1),
                              1900.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_2),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_start_entry_3),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_1),
                              2100.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_2),
                              1.0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(ftd->date_end_entry_3),
                              1.0);
  }

  /* set data so we can find the struct in callbacks */
  gtk_object_set_data(GTK_OBJECT(ftd->dialog), "find_transactions_structure",
                      ftd);
  

  /* if there's no original query, make the narrow, add, delete 
   * buttons inaccessible */
  if(!ftd->q) {
    gtk_widget_set_sensitive(GTK_WIDGET(ftd->narrow_search_radiobutton),
                             0);
    gtk_widget_set_sensitive(GTK_WIDGET(ftd->add_search_radiobutton),
                             0);
    gtk_widget_set_sensitive(GTK_WIDGET(ftd->delete_search_radiobutton),
                             0);
  }
  else {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON
                                 (ftd->new_search_radiobutton),
                                 0);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON
                                 (ftd->narrow_search_radiobutton),
                                 1);
  }
  
  gtk_widget_show_all(ftd->dialog);
  return ftd;
}


/********************************************************************\
 * gnc_ui_find_transactions_dialog_destroy
\********************************************************************/

void
gnc_ui_find_transactions_dialog_destroy(FindTransactionsDialog * ftd) {
  gnome_dialog_close(GNOME_DIALOG(ftd->dialog));
  
  ftd->dialog = NULL;
  g_free(ftd->ymd_format);
  g_free(ftd);
}
  

/********************************************************************\
 * callbacks for radio button selections 
\********************************************************************/

void
gnc_ui_find_transactions_dialog_search_type_cb(GtkToggleButton * tb,
                                               gpointer user_data) {
  GSList * buttongroup = 
    gtk_radio_button_group(GTK_RADIO_BUTTON(tb));
  
  FindTransactionsDialog * ftd = 
    gtk_object_get_data(GTK_OBJECT(user_data), "find_transactions_structure");
    
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(tb))) {
    ftd->search_type = 
      g_slist_length(buttongroup) -  g_slist_index(buttongroup, tb) - 1;
  } 

}


/********************************************************************\
 * gnc_ui_find_transactions_dialog_cancel_cb
\********************************************************************/

void
gnc_ui_find_transactions_dialog_cancel_cb(GtkButton * button, 
                                    gpointer user_data) {
  FindTransactionsDialog * ftd =
    gtk_object_get_data(GTK_OBJECT(user_data), "find_transactions_structure");

  gnc_ui_find_transactions_dialog_destroy(ftd);
}


/********************************************************************\
 * gnc_ui_find_transactions_dialog_help_cb
\********************************************************************/

void
gnc_ui_find_transactions_dialog_help_cb(GtkButton * button, 
                                        gpointer user_data) {
  helpWindow(NULL, HELP_STR, HH_FIND_TRANSACTIONS);
}


/********************************************************************\
 * gnc_ui_find_transactions_dialog_early_date_select_cb
\********************************************************************/

void
gnc_ui_find_transactions_dialog_early_date_select_cb(GtkButton * button, 
                                                     gpointer user_data) {
  FindTransactionsDialog * ftd =
    gtk_object_get_data(GTK_OBJECT(user_data), "find_transactions_structure");

  gnc_ui_select_date_dialog_create(ftd->date_start_entry_1,
                                   ftd->date_start_entry_2,
                                   ftd->date_start_entry_3,
                                   ftd->ymd_format);
}

/********************************************************************\
 * gnc_ui_find_transactions_dialog_late_date_select_cb
\********************************************************************/

void
gnc_ui_find_transactions_dialog_late_date_select_cb(GtkButton * button, 
                                                    gpointer user_data) {
  FindTransactionsDialog * ftd =
    gtk_object_get_data(GTK_OBJECT(user_data), "find_transactions_structure");

  gnc_ui_select_date_dialog_create(ftd->date_end_entry_1,
                                   ftd->date_end_entry_2,
                                   ftd->date_end_entry_3,
                                   ftd->ymd_format);
}



/********************************************************************\
 * gnc_ui_find_transactions_dialog_ok_cb
\********************************************************************/

void
gnc_ui_find_transactions_dialog_ok_cb(GtkButton * button, 
                                      gpointer user_data) {
  GtkWidget              * dialog = user_data;
  FindTransactionsDialog * ftd = 
    gtk_object_get_data(GTK_OBJECT(dialog), "find_transactions_structure");
  
  GList   * selected_accounts;
  char    * descript_match_text;
  char    * memo_match_text;
  char    * number_match_text;
  char    * action_match_text;

  int     search_type = ftd->search_type;

  float   amt_temp;
  int     amt_type;
  Query   * q, * q2;
  gboolean new_ledger = FALSE;

  int start_year, start_month, start_day;
  int end_year, end_month, end_day;

  int c_cleared, c_notcleared, c_reconciled;

  if(search_type == 0) {
    if(ftd->q) xaccFreeQuery(ftd->q);
    ftd->q = xaccMallocQuery();    
  }

  assert(ftd->q);

  q = xaccMallocQuery();
  xaccQuerySetGroup(q, gncGetCurrentGroup());

  /* account selections */
  selected_accounts = 
    gnc_account_tree_get_current_accounts
    (GNC_ACCOUNT_TREE(ftd->account_tree));

  /* description */
  descript_match_text = 
    gtk_entry_get_text(GTK_ENTRY(ftd->description_entry));

  /* memo */
  memo_match_text = 
    gtk_entry_get_text(GTK_ENTRY(ftd->memo_entry));

  /* number */
  number_match_text = 
    gtk_entry_get_text(GTK_ENTRY(ftd->number_entry));

  /* action */
  action_match_text = 
    gtk_entry_get_text(GTK_ENTRY(ftd->action_entry));
  
  if(selected_accounts) {
    xaccQueryAddAccountMatch(q, selected_accounts,
                             gnc_option_menu_get_active
                             (ftd->match_accounts_picker),
                             QUERY_AND);
  }                             
  
  if(strlen(descript_match_text)) {    
    xaccQueryAddDescriptionMatch(q, descript_match_text, 
                                 gtk_toggle_button_get_active
                                 (GTK_TOGGLE_BUTTON
                                  (ftd->description_case_toggle)),
                                 gtk_toggle_button_get_active
                                 (GTK_TOGGLE_BUTTON
                                  (ftd->description_regexp_toggle)),
                                 QUERY_AND);
  }
  

  if(strlen(number_match_text)) {    
    xaccQueryAddNumberMatch(q, number_match_text, 
                            gtk_toggle_button_get_active
                            (GTK_TOGGLE_BUTTON
                             (ftd->number_case_toggle)),
                            gtk_toggle_button_get_active
                            (GTK_TOGGLE_BUTTON
                             (ftd->number_regexp_toggle)),
                            QUERY_AND);
  }
  

  amt_temp = 
    (double)gtk_spin_button_get_value_as_float
    (GTK_SPIN_BUTTON(ftd->amount_entry));
  
  amt_type = gnc_option_menu_get_active(ftd->credit_debit_picker);
  
  if((amt_temp > 0.00001) || (amt_type != 0)) {
    xaccQueryAddAmountMatch(q, 
                            amt_temp,
                            amt_type,
                            gnc_option_menu_get_active
                            (ftd->amount_comp_picker),
                            QUERY_AND);
  }
  
  if(!strcmp(ftd->ymd_format, "us")) {
    start_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_1));
    start_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_2));
    start_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_3));
    
    end_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_1));
    end_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_2));
    end_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_3));    
  }
  else if(!strcmp(ftd->ymd_format, "uk") ||
          !strcmp(ftd->ymd_format, "europe")) {
    start_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_1));
    start_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_2));
    start_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_3));
    
    end_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_1));
    end_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_2));
    end_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_3));        
  }
  else if(!strcmp(ftd->ymd_format, "iso")) {
    start_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_1));
    start_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_2));
    start_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_3));
    
    end_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_1));
    end_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_2));
    end_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_3));    
  }
  else {
    start_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_1));
    start_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_2));
    start_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_start_entry_3));
    
    end_day = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_1));
    end_month = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_2));
    end_year = gtk_spin_button_get_value_as_int
      (GTK_SPIN_BUTTON(ftd->date_end_entry_3));    
  }
  
  if(!((start_day==1) && (start_month==1) && (start_year==1900) &&
       (end_day==1) && (end_month==1) && (end_year==2100))) {
    xaccQueryAddDateMatch(q, 
                          start_day, start_month, start_year,
                          end_day, end_month, end_year,
                          QUERY_AND);
  }
  
  if(strlen(memo_match_text)) {    
    xaccQueryAddMemoMatch(q, memo_match_text, 
                          gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON(ftd->memo_case_toggle)),
                          gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON(ftd->memo_regexp_toggle)),
                          QUERY_AND);    
  }

  amt_temp = 
    (double)gtk_spin_button_get_value_as_float
    (GTK_SPIN_BUTTON(ftd->price_entry));
  amt_type = 
    gnc_option_menu_get_active(ftd->price_comp_picker);

  if((amt_temp > 0.00001) || (amt_type != 0)) {
    xaccQueryAddSharePriceMatch(q, 
                                amt_temp,
                                amt_type,
                                QUERY_AND);
  }
  
  amt_temp = 
    (double)gtk_spin_button_get_value_as_float
    (GTK_SPIN_BUTTON(ftd->shares_entry));
  amt_type = 
    gnc_option_menu_get_active(ftd->shares_comp_picker);
  
  if((amt_temp > 0.00001) || (amt_type != 0)) {
    xaccQueryAddSharesMatch(q, 
                            amt_temp,
                            amt_type,
                            QUERY_AND);
  }

  if(strlen(action_match_text)) {    
    xaccQueryAddActionMatch(q, action_match_text, 
                            gtk_toggle_button_get_active
                            (GTK_TOGGLE_BUTTON
                             (ftd->action_case_toggle)),
                            gtk_toggle_button_get_active
                            (GTK_TOGGLE_BUTTON
                             (ftd->action_regexp_toggle)),
                            QUERY_AND);
  }
  
  c_cleared = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON(ftd->cleared_cleared_toggle));
  c_notcleared = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON(ftd->cleared_not_cleared_toggle));
  c_reconciled = gtk_toggle_button_get_active
    (GTK_TOGGLE_BUTTON(ftd->cleared_reconciled_toggle));
  
  if(c_cleared || c_notcleared || c_reconciled) {
    int how = 0;
    if(c_cleared)    how = how | CLEARED_CLEARED;
    if(c_notcleared) how = how | CLEARED_NO;
    if(c_reconciled) how = how | CLEARED_RECONCILED;
    xaccQueryAddClearedMatch(q, how, QUERY_AND);
  }

  if(!ftd->ledger) {
    new_ledger = TRUE;
    ftd->ledger = xaccLedgerDisplayGeneral(NULL, NULL,
                                           SEARCH_LEDGER,
                                           REG_SINGLE_LINE);
    xaccFreeQuery(ftd->ledger->query);
  }

  switch(search_type) {
  case 0:
    ftd->ledger->query = q;
    break;
  case 1:    
    ftd->ledger->query = xaccQueryMerge(ftd->q, q, QUERY_AND);
    xaccFreeQuery(q);
    break;
  case 2:
    ftd->ledger->query = xaccQueryMerge(ftd->q, q, QUERY_OR);
    xaccFreeQuery(q);
    break;
  case 3:
    q2 = xaccQueryInvert(q);
    ftd->ledger->query = xaccQueryMerge(ftd->q, q2, QUERY_AND);
    xaccFreeQuery(q2);
    xaccFreeQuery(q);
    break;
  }

  ftd->ledger->dirty = 1;
  xaccLedgerDisplayRefresh(ftd->ledger);
  if (new_ledger) regWindowLedger(ftd->ledger);
  
  gnc_ui_find_transactions_dialog_destroy(ftd);
}

/********************************************************************\
 * gnc_ui_select_date_dialog_cancel_cb
\********************************************************************/

SelectDateDialog *
gnc_ui_select_date_dialog_create(GtkWidget * entry_1, GtkWidget * entry_2,
                                 GtkWidget * entry_3, char * ymd_default) {
  SelectDateDialog * sdd = g_new0(SelectDateDialog, 1);
  int y, m, d;

  sdd->dialog = create_Select_Date();
  sdd->cal = gtk_object_get_data(GTK_OBJECT(sdd->dialog),
                                 "calendar1");

  sdd->entry_1 = entry_1;
  sdd->entry_2 = entry_2;
  sdd->entry_3 = entry_3;

  if(!strcmp(ymd_default, "us")) {
    m = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_1));
    d = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_2));
    y = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_3));
  }
  else if(!strcmp(ymd_default, "uk") ||
          !strcmp(ymd_default, "europe")) {
    d = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_1));
    m = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_2));
    y = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_3));
  }
  else if(!strcmp(ymd_default, "iso")) {
    y = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_1));
    m = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_2));
    d = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_3));
  }
  else {
    m = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_1));
    d = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_2));
    y = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(sdd->entry_3));
  }
  
  sdd->ymd_format = strdup(ymd_default);

  gtk_calendar_select_month(GTK_CALENDAR(sdd->cal),
                            m-1, y);
  gtk_calendar_select_day(GTK_CALENDAR(sdd->cal),
                          d);
  
  gtk_object_set_data(GTK_OBJECT(sdd->dialog), "select_date_struct",
                      sdd);
  gtk_widget_show_all(GTK_WIDGET(sdd->dialog));
  return sdd;
}

/********************************************************************\
 * gnc_ui_select_date_dialog_destroy
\********************************************************************/

void
gnc_ui_select_date_dialog_destroy(SelectDateDialog * sdd) {
  gnome_dialog_close(GNOME_DIALOG(sdd->dialog));
  g_free(sdd->ymd_format);
  g_free(sdd);
}

/********************************************************************\
 * gnc_ui_select_date_dialog_cancel_cb
\********************************************************************/

void
gnc_ui_select_date_dialog_cancel_cb(GtkButton * button, 
                                    gpointer user_data) {
  SelectDateDialog * sdd = 
    (SelectDateDialog *)gtk_object_get_data(GTK_OBJECT(user_data),
                                            "select_date_struct");
  gnc_ui_select_date_dialog_destroy(sdd);
}


/********************************************************************\
 * gnc_ui_select_date_dialog_today_cb
\********************************************************************/

void
gnc_ui_select_date_dialog_today_cb(GtkButton * button, 
                                    gpointer user_data) {
  SelectDateDialog * sdd = 
    (SelectDateDialog *)gtk_object_get_data(GTK_OBJECT(user_data),
                                            "select_date_struct");
  time_t    now;
  struct tm * bdtime; 

  now = time(NULL);
  bdtime = localtime(&now);
  gtk_calendar_select_month(GTK_CALENDAR(sdd->cal),
                            bdtime->tm_mon,
                            bdtime->tm_year+1900);
  gtk_calendar_select_day(GTK_CALENDAR(sdd->cal),
                          bdtime->tm_mday);
}


/********************************************************************\
 * gnc_ui_select_date_dialog_cancel_cb
\********************************************************************/

void
gnc_ui_select_date_dialog_ok_cb(GtkButton * button, 
                                gpointer user_data) {
  SelectDateDialog * sdd = 
    (SelectDateDialog *)gtk_object_get_data(GTK_OBJECT(user_data),
                                            "select_date_struct");
  int   y, m, d;

  gtk_calendar_get_date(GTK_CALENDAR(sdd->cal), &y, &m, &d);
  
  if(!strcmp(sdd->ymd_format, "us")) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_1),
                              (float)m+1);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_2),
                              (float)d);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_3),
                              (float)y);
  }
  else if(!strcmp(sdd->ymd_format, "uk") ||
          !strcmp(sdd->ymd_format, "europe")) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_1),
                              (float)d);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_2),
                              (float)m+1);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_3),
                              (float)y);
  }
  else if(!strcmp(sdd->ymd_format, "iso")) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_1),
                              (float)y);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_2),
                              (float)m+1);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(sdd->entry_3),
                              (float)d);
  }
  
  gnc_ui_select_date_dialog_destroy(sdd);
}


/*******************************************************************\
 * window-register.c -- the register window for GnuCash             *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-1998 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <dave@krondo.com>         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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

#define _GNU_SOURCE

#include "config.h"

#include <gnome.h>
#include <time.h>
#include <g-wrap-wct.h>

#include "AccWindow.h"
#include "Scrub.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-euro.h"
#include "gnc-gui-query.h"
#include "gnc-ledger-display.h"
#include "gnc-menu-extensions.h"
#include "gnc-pricedb.h"
#include "gnc-split-reg.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnucash-sheet.h"
#include "messages.h"
#include "table-allgui.h"
#include "window-help.h"
#include "window-main.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-report.h"
#include "top-level.h"
#include "dialog-print-check.h"

typedef struct _RegDateWindow RegDateWindow;
struct _RegDateWindow
{
  GtkWidget * window;

  GtkWidget * show_earliest;
  GtkWidget * start_date;

  GtkWidget * show_latest;
  GtkWidget * end_date;
  GtkWidget * today_button;

  GtkWidget * set_button;
};

/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */
struct _RegWindow
{
  gint width;

  GtkWidget *window;
  GtkWidget *toolbar_dock;
  GtkWidget *summarybar_dock;
  GtkWidget *statusbar;

  GNCLedgerDisplay *ledger;
  GNCSplitReg *gsr;

  RegDateWindow *date_window;
  /* pcd = "print check dialog" */
  gpointer pcd;
  gboolean read_only;

  GtkWidget *reconciled_menu_item;
  GtkWidget *cleared_menu_item;
  GtkWidget *voided_menu_item;
  GtkWidget *frozen_menu_item;
  GtkWidget *unreconciled_menu_item;
};

GtkWidget *gnc_RegWindow_window (RegWindow *data)
{
  g_assert(data);
  return data->window;
}

GNCLedgerDisplay *gnc_RegWindow_ledger (RegWindow *data)
{
  g_assert(data);
  return data->ledger;
}

gpointer
gnc_RegWindow_get_pcd (RegWindow *data)
{
  return data->pcd;
}

void
gnc_RegWindow_set_pcd (RegWindow *data, gpointer pcd)
{
  data->pcd = pcd;
}


/* This static indicates the debugging module that this .o belongs to.   */
/* static short module = MOD_GUI; */
//static short module = MOD_SX; /* for the moment... */

static int last_width = 0;
static int last_stock_width = 0;

/** PROTOTYPES ******************************************************/

static void gnc_register_help_changed_cb( GNCSplitReg *gsr, gpointer data );

void gnc_register_start_recn_cb(GtkWidget *w, gpointer data);
void gnc_register_xfer_cb(GtkWidget *w, gpointer data);
void gnc_register_stock_split_cb (GtkWidget * w, gpointer data);
void gnc_register_edit_cb(GtkWidget *w, gpointer data);
void gnc_register_new_account_cb(GtkWidget * w, gpointer data);

void gnc_register_close_cb(GtkWidget *w, gpointer data);
void gnc_register_exit_cb(GtkWidget *w, gpointer data);
void gnc_register_report_account_cb(GtkWidget *w, gpointer data);
void gnc_register_report_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_print_cb(GtkWidget *w, gpointer data);
void gnc_register_show_all_status_cb(GtkWidget *widget, gpointer data);
void gnc_register_show_one_status_cb(GtkWidget *widget, gpointer data);
void gnc_register_date_cb(GtkWidget *widget, gpointer data);
void gnc_register_date_show_all_cb(GtkWidget *w, gpointer data);
void gnc_register_today_cb(GtkWidget *w, gpointer data);
void gnc_register_date_toggle_cb(GtkToggleButton *toggle, gpointer data);
void gnc_register_date_range_cb(GtkWidget *w, gpointer data);

void gnc_register_print_check_cb(GtkWidget * widget, gpointer data);
void gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data);

void gnc_register_toolbar_cb(GtkWidget *widget, gpointer data);
void gnc_register_summarybar_cb(GtkWidget *widget, gpointer data);
void gnc_register_statusbar_cb(GtkWidget *widget, gpointer data);

void gnc_register_gl_cb(GtkWidget *widget, gpointer data);
void gnc_register_prices_cb(GtkWidget *widget, gpointer data);
void gnc_register_commodities_cb(GtkWidget *widget, gpointer data);
void gnc_register_fincalc_cb(GtkWidget *widget, gpointer data);

void gnc_register_scrub_all_cb (GtkWidget *widget, gpointer data);
void gnc_register_scrub_current_cb (GtkWidget *widget, gpointer data);

gboolean gnc_register_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data);
void gnc_register_destroy_cb(GtkWidget *widget, gpointer data);
void gnc_register_size_allocate (GtkWidget *widget, GtkAllocation *allocation, gpointer user_data);

static void gnc_register_setup_menu_widgets( RegWindow *regData, GladeXML *xml );
static GtkWidget* gnc_register_setup_toolbar( RegWindow *regData );

static void gnc_register_insert_cloned_toolbar_elt( GtkToolbar *dstToolbar,
                                                    GtkToolbar *srcToolbar,
                                                    GtkWidget *srcWidget,
                                                    gchar *tooltip,
                                                    gpointer callback,
                                                    gpointer user_data,
                                                    gint idx );

static gboolean gnc_register_include_date(RegWindow *regData, time_t date);
static void gnc_register_include_date_adapter( GNCSplitReg *gsr,
                                               time_t date,
                                               gpointer user_data );

static void gnc_register_set_read_only( RegWindow *regData );

static void gnc_reg_save_size (RegWindow *regData);


/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window for Account account                 *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
GNCSplitReg*
regWindowSimple (Account *account)
{
  GNCSplitReg *gsr;
  GNCLedgerDisplay * ledger = gnc_ledger_display_simple( account );

  if (ledger == NULL)
    return NULL;

  gsr = gnc_ledger_display_get_user_data( ledger ); 
  if ( !gsr ) {
    RegWindow *rw = regWindowLedger( ledger );
    gsr = rw->gsr;
  }

  return gsr;
}


/********************************************************************\
 * regWindowAccGroup                                                *
 *   opens up a register window for a group of Accounts             *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
GNCSplitReg*
regWindowAccGroup (Account *account)
{
  GNCSplitReg *gsr;
  GNCLedgerDisplay * ledger = gnc_ledger_display_subaccounts (account);

  if (ledger == NULL)
    return NULL;

  gsr = gnc_ledger_display_get_user_data( ledger );
  if ( !gsr ) {
    RegWindow *rw;
    rw = regWindowLedger (ledger);
    gsr = rw->gsr;
  }

  return gsr;
}

/**
 * Raise an existing register window to the front.
 **/
void
gnc_register_raise (RegWindow *regData)
{
  if (regData == NULL)
    return;

  if (regData->window == NULL)
    return;

  gtk_window_present( GTK_WINDOW(regData->window) );
}

static time_t
gnc_register_min_day_time(time_t time_val)
{
  struct tm *time_struct;

  /* Get the equivalent time structure */
  time_struct = localtime(&time_val);

  /* First second of the day */
  time_struct->tm_sec = 0;
  time_struct->tm_min = 0;
  time_struct->tm_hour = 0;

  return mktime(time_struct);
}

static time_t
gnc_register_max_day_time(time_t time_val)
{
  struct tm *time_struct;

  /* Get the equivalent time structure */
  time_struct = localtime(&time_val);

  /* Last second of the day */
  time_struct->tm_sec = 59;
  time_struct->tm_min = 59;
  time_struct->tm_hour = 23;

  return mktime(time_struct);
}

static void
gnc_date_range_set_sensitivities(RegWindow *regData)
{
  RegDateWindow *regDateData;
  GtkToggleButton *toggle;
  Query *query;

  if (!regData)
    return;

  if (!regData->ledger)
    return;

  query = gnc_ledger_display_get_query (regData->ledger);
  if (!query)
    return;

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);
  if (gtk_toggle_button_get_active(toggle))
    gtk_widget_set_sensitive(regDateData->start_date, FALSE);
  else
    gtk_widget_set_sensitive(regDateData->start_date, TRUE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_latest);
  if (gtk_toggle_button_get_active(toggle))
  {
    gtk_widget_set_sensitive(regDateData->end_date, FALSE);
    gtk_widget_set_sensitive(regDateData->today_button, FALSE);
  }
  else
  {
    gtk_widget_set_sensitive(regDateData->end_date, TRUE);
    gtk_widget_set_sensitive(regDateData->today_button, TRUE);
  }
}

static void
gnc_register_show_status(RegWindow *regData)
{
  RegDateWindow *regDateData;
  GSList *param_list = NULL;
  gboolean show_reconciled, show_unreconciled;
  cleared_match_t how = 0;
  Query *query;

  if (!regData)
    return;

  if (!regData->ledger)
    return;

  if (GTK_CHECK_MENU_ITEM(regData->reconciled_menu_item)->active)
    how |= CLEARED_RECONCILED;
  if (GTK_CHECK_MENU_ITEM(regData->cleared_menu_item)->active)
    how |= CLEARED_CLEARED;
  if (GTK_CHECK_MENU_ITEM(regData->voided_menu_item)->active)
    how |= CLEARED_VOIDED;
  if (GTK_CHECK_MENU_ITEM(regData->frozen_menu_item)->active)
    how |= CLEARED_FROZEN;
  if (GTK_CHECK_MENU_ITEM(regData->unreconciled_menu_item)->active)
    how |= CLEARED_NO;

  query = gnc_ledger_display_get_query( regData->ledger );
  if (!query)
    return;

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  param_list = gncQueryBuildParamList (SPLIT_RECONCILE, NULL);
  gncQueryPurgeTerms (query, param_list);
  if (how == CLEARED_ALL)
    return;

  xaccQueryAddClearedMatch(query, how, QUERY_AND);
}

void
gnc_register_show_one_status_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_show_status(regData);

  gnc_ledger_display_refresh (regData->ledger);
}

void
gnc_register_show_all_status_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  /* 
   * Don't call gtk_check_menu_item_set_active() because this would
   * just generate a callback that would then need to be blocked. Set
   * the bit directly so the menu items will be displayed correctly
   * next time around.
   */
  GTK_CHECK_MENU_ITEM(regData->reconciled_menu_item)->active = TRUE;
  GTK_CHECK_MENU_ITEM(regData->cleared_menu_item)->active = TRUE;
  GTK_CHECK_MENU_ITEM(regData->voided_menu_item)->active = TRUE;
  GTK_CHECK_MENU_ITEM(regData->frozen_menu_item)->active = TRUE;
  GTK_CHECK_MENU_ITEM(regData->unreconciled_menu_item)->active = TRUE;
  gnc_register_show_status(regData);

  gnc_ledger_display_refresh (regData->ledger);
}

static void
gnc_register_set_date_range(RegWindow *regData)
{
  RegDateWindow *regDateData;
  GtkToggleButton *toggle;
  GSList *date_param;
  Query *query;

  if (!regData)
    return;

  if (!regData->ledger)
    return;

  query = gnc_ledger_display_get_query( regData->ledger );
  if (!query)
    return;

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  gtk_widget_set_sensitive(regDateData->set_button, FALSE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);

  date_param = gncQueryBuildParamList(TRANS_DATE_POSTED, SPLIT_TRANS, NULL);
  gncQueryPurgeTerms (query, date_param);
  g_slist_free(date_param);

  if (!gtk_toggle_button_get_active(toggle))
  {
    time_t start;

    start = gnc_date_edit_get_date(GNC_DATE_EDIT(regDateData->start_date));
    start = gnc_register_min_day_time(start);

    xaccQueryAddDateMatchTT(query, 
                            TRUE, start, 
                            FALSE, 0, 
                            QUERY_AND);
  }

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_latest);
  if (!gtk_toggle_button_get_active(toggle))
  {
    time_t end;

    end = gnc_date_edit_get_date(GNC_DATE_EDIT(regDateData->end_date));
    end = gnc_register_max_day_time(end);

    xaccQueryAddDateMatchTT(query, 
                            FALSE, 0,
                            TRUE, end,                            
                            QUERY_AND);
  }

  gnc_date_range_set_sensitivities(regData);
}

void
gnc_register_date_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_set_date_range(regData);

  gnc_ledger_display_refresh (regData->ledger);
}

void
gnc_register_date_show_all_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  RegDateWindow *regDateData;
  GtkToggleButton *toggle;

  g_return_if_fail(regData != NULL);

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);
  gtk_toggle_button_set_active(toggle, TRUE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_latest);
  gtk_toggle_button_set_active(toggle, TRUE);

  gnc_register_date_cb(widget, data);
}

void
gnc_register_today_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  RegDateWindow *regDateData;

  g_return_if_fail(regData != NULL);

  regDateData = regData->date_window;
  gnc_date_edit_set_time(GNC_DATE_EDIT(regDateData->end_date), time(NULL));

  gtk_widget_set_sensitive(regData->date_window->set_button, TRUE);
}

void
gnc_register_date_toggle_cb(GtkToggleButton *toggle, gpointer data)
{
  RegWindow *regData = data;

  gtk_widget_set_sensitive(regData->date_window->set_button, TRUE);

  gnc_date_range_set_sensitivities(regData);
}

static void
gnc_register_date_changed_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gtk_widget_set_sensitive(regData->date_window->set_button, TRUE);
}

static void
gnc_register_show_date_window(RegWindow *regData)
{
  RegDateWindow *regDateData;

  if (regData == NULL)
    return;

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  if (regDateData->window == NULL)
    return;

  gtk_window_present(GTK_WINDOW(regDateData->window));
}

static RegDateWindow *
gnc_register_date_window (RegWindow *regData, gboolean show_all)
{
  RegDateWindow *regDateData;
  GtkWidget *dialog;
  GladeXML *xml;

  regDateData = g_new0(RegDateWindow, 1);
  regData->date_window = regDateData;

  xml = gnc_glade_xml_new ("register.glade", "Date Range");
  dialog = glade_xml_get_widget (xml, "Date Range");
  regDateData->window = dialog;

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    regData);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(regData->window));

  /*
   * Get/create all the widgets up front to prevent callback errors
   * when opening the General Ledger.
   */
  regDateData->show_earliest = glade_xml_get_widget(xml, "start_earliest");
  regDateData->show_latest = glade_xml_get_widget(xml, "end_latest");
  regDateData->today_button = glade_xml_get_widget(xml, "today");
  regDateData->set_button = glade_xml_get_widget(xml, "set_range");
  regDateData->start_date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
  regDateData->end_date = gnc_date_edit_new(time(NULL), FALSE, FALSE);

  {
    GtkWidget *calendar;
    GtkWidget *entry;
    GtkWidget *radio;
    GtkWidget *date;
    GtkWidget *hbox;
    time_t time_val;

    /* Starting Date */
    radio = regDateData->show_earliest;
    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    radio = glade_xml_get_widget(xml, "start_date");
    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = glade_xml_get_widget(xml, "start_date_entry");
    date = regDateData->start_date;
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);

    time_val = xaccQueryGetEarliestDateFound
      (gnc_ledger_display_get_query (regData->ledger));
    if (time_val < time(NULL))
      gnc_date_edit_set_time(GNC_DATE_EDIT(date), time_val);

    gtk_signal_connect(GTK_OBJECT(date), "date-changed",
                       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    calendar = GNC_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    entry = GNC_DATE_EDIT(date)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "activate",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);
    gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);


    /* Ending Date */

    radio = regDateData->show_latest;
    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    radio = glade_xml_get_widget(xml, "end_date");
    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = glade_xml_get_widget(xml, "end_date_entry");
    date = regDateData->end_date;
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);

    gtk_signal_connect(GTK_OBJECT(date), "date-changed",
                       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    calendar = GNC_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    entry = GNC_DATE_EDIT(date)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "activate",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);
    gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);
  }
  gtk_widget_show_all(glade_xml_get_widget(xml, "main_frame"));

  return regDateData;
}

void
gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data)
{
  RegWindow * regData = data;
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  if (reg->type == SEARCH_LEDGER)
    gnc_ui_find_transactions_dialog_create (regData->ledger);
  else
    gnc_ui_find_transactions_dialog_create (NULL);
}

void
gnc_register_print_check_cb(GtkWidget * widget, gpointer data)
{
  RegWindow     * reg_data = data;
  SplitRegister * reg      =
    gnc_ledger_display_get_split_register (reg_data->ledger);
  Split         * split    = gnc_split_register_get_current_split(reg);
  Transaction   * trans    = xaccSplitGetParent(split);

  const char    * payee;
  const char    * memo;
  gnc_numeric   amount;
  time_t        date;

  if(split && trans)
  {
    payee  = xaccTransGetDescription(trans);
    memo   = xaccTransGetNotes(trans);
    if (memo == NULL)
      memo = "";
    amount = xaccSplitGetAmount(split);
    amount = gnc_numeric_abs (amount);
    date   = xaccTransGetDate(trans);

    gnc_ui_print_check_dialog_create(reg_data, payee, amount, date, memo);
  }
}

void
gnc_register_scrub_all_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  Query *query = gnc_ledger_display_get_query (regData->ledger);
  AccountGroup *root;
  GList *node;

  if (query == NULL)
    return;

  gnc_suspend_gui_refresh ();
  root = gnc_get_current_group ();

  for (node = xaccQueryGetSplits (query); node; node = node->next)
  {
    Split *split = node->data;
    Transaction *trans = xaccSplitGetParent (split);

    xaccTransScrubOrphans (trans, root, gnc_get_current_book ());
    xaccTransScrubImbalance (trans, root, NULL, gnc_get_current_book ());
  }

  gnc_resume_gui_refresh ();
}

void
gnc_register_scrub_current_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Transaction *trans;
  AccountGroup *root;

  reg = gnc_ledger_display_get_split_register (regData->ledger);
  trans = gnc_split_register_get_current_trans (reg);

  if (!trans)
    return;

  gnc_suspend_gui_refresh ();
  root = gnc_get_current_group ();
  xaccTransScrubOrphans (trans, root, gnc_get_current_book ());
  xaccTransScrubImbalance (trans, root, NULL, gnc_get_current_book ());
  gnc_resume_gui_refresh ();
}

gboolean
gnc_register_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  RegWindow *regData = data;

  if ( regData ) {
    gnc_reg_save_size( regData );
  }

  gnc_split_reg_check_close(regData->gsr);
  gnc_ledger_display_close (regData->ledger);

  return TRUE; /* don't close */
}

void
gnc_register_destroy_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  if (regData->date_window != NULL)
  {
    if (regData->date_window->window != NULL)
      gtk_widget_destroy(regData->date_window->window);

    g_free(regData->date_window);
    regData->date_window = NULL;
  }

  if (regData->pcd)
    gnc_ui_print_check_dialog_destroy(regData->pcd);

  gtk_widget_destroy( regData->window );

  g_free(regData);
}

static char *
gnc_reg_get_name (RegWindow *regData, gboolean for_window)
{
  Account *leader;
  SplitRegister *reg;
  gboolean single_account;
  gchar *account_name;
  gchar *reg_name;
  gchar *name;

  if (regData == NULL)
    return NULL;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  switch (reg->type)
  {
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
      if (for_window)
        reg_name = _("General Ledger");
      else
        reg_name = _("General Ledger Report");
      single_account = FALSE;
      break;
    case PORTFOLIO_LEDGER:
      if (for_window)
        reg_name = _("Portfolio");
      else
        reg_name = _("Portfolio Report");
      single_account = FALSE;
      break;
    case SEARCH_LEDGER:
      if (for_window)
        reg_name = _("Search Results");
      else
        reg_name = _("Search Results Report");
      single_account = FALSE;
      break;
    default:
      if (for_window)
        reg_name = _("Register");
      else
        reg_name = _("Register Report");
      single_account = TRUE;
      break;
  }

  leader = gnc_ledger_display_leader (regData->ledger);

  if ((leader != NULL) && single_account)
  {
    account_name = xaccAccountGetFullName (leader,
                                           gnc_get_account_separator ());

    name = g_strconcat (account_name, " - ", reg_name, NULL);

    g_free(account_name);
  }
  else
    name = g_strdup (reg_name);

  return name;
}

static void
gnc_reg_set_window_name (RegWindow *regData)
{
  gchar *windowname;

  if (regData == NULL)
    return;

  windowname = gnc_reg_get_name( regData, TRUE );
  gtk_window_set_title( GTK_WINDOW(regData->window), windowname );
  g_free( windowname );
}

void
gnc_register_size_allocate (GtkWidget *widget,
			    GtkAllocation *allocation,
			    gpointer user_data)
{
  RegWindow *regData = user_data;

  /* HACK ALERT. this seems to be the only thing to get the
   * freekin register window to stop freekin resizing itself
   * all the freekin time. */

  if (regData->width == allocation->width)
    return;

  regData->width = allocation->width;
  gtk_window_set_default_size( GTK_WINDOW(regData->window), regData->width, 0 );
}

/********************************************************************\
 * regWindowLedger                                                  *
 *   opens up a ledger window for the account list                  *
 *                                                                  *
 * Args:   ledger - ledger data structure                           *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowLedger( GNCLedgerDisplay *ledger )
{
  SplitRegister *reg;
  RegWindow *regData;
  GtkWidget *gsr;
  GtkWidget *register_window;
  GtkWidget *table_frame;
  gboolean show_all;
  gboolean has_date;
  GladeXML *xml;
  gint numRows;

  /* FIXME: This no longer holds, but something like it [attaching the window
   * as user_data to the split_reg] should exist. */
  reg = gnc_ledger_display_get_split_register (ledger);

  regData = g_new0( RegWindow, 1 );
  regData->ledger = ledger;

  xml = gnc_glade_xml_new( "register.glade", "Check Register" );
  register_window = glade_xml_get_widget( xml, "Check Register" );
  regData->window = register_window;

  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     regData );

  numRows = (guint)gnc_lookup_number_option ( "_+Advanced",
                                              "Number of Rows", 20.0 );

  gsr = gnc_split_reg_new( ledger,
                           GTK_WINDOW(register_window),
                           numRows,
                           ( CREATE_TOOLBAR
                             | CREATE_MENUS
                             | CREATE_POPUP
                             | CREATE_SUMMARYBAR ),
                           0 );
  regData->gsr = GNC_SPLIT_REG(gsr);

  /* libglade should do this next line */
  GNOME_APP(register_window)->menubar = glade_xml_get_widget( xml, "gnc_register_menubar" );

  gnc_reg_set_window_name( regData );
  if ( gnc_split_reg_get_read_only( regData->gsr ) ) {
    gnc_register_set_read_only( regData );
  }

  show_all = gnc_lookup_boolean_option( "_+Advanced",
                                        "Show All Transactions",
                                        TRUE );

  {
    GSList *date_param = gncQueryBuildParamList(TRANS_DATE_POSTED,
						SPLIT_TRANS, NULL);
    Query *q = gnc_ledger_display_get_query (regData->ledger);
    has_date = gncQueryHasTermType (q, date_param);
    g_slist_free(date_param);
  }

  if (has_date)
    show_all = FALSE;

  regData->date_window = gnc_register_date_window( regData, show_all );

  if (reg->type != SEARCH_LEDGER && !has_date)
    gnc_register_set_date_range( regData );

  /* Now that we have a date range, remove any existing
   * maximum on the number of splits returned. */
  xaccQuerySetMaxSplits( gnc_ledger_display_get_query(regData->ledger), -1 );

  /* The status bar */
  regData->statusbar = glade_xml_get_widget( xml, "appbar" );
  gtk_signal_connect( GTK_OBJECT(regData->gsr), "help-changed",
                      GTK_SIGNAL_FUNC( gnc_register_help_changed_cb ),
                      regData );

  /* The "include-date" and "read-only" signals. */
  gtk_signal_connect( GTK_OBJECT(regData->gsr), "include-date",
                      GTK_SIGNAL_FUNC( gnc_register_include_date_adapter ),
                      regData );

  regData->reconciled_menu_item = glade_xml_get_widget( xml, "show_reconciled" );
  regData->cleared_menu_item = glade_xml_get_widget( xml, "show_cleared" );
  regData->voided_menu_item = glade_xml_get_widget( xml, "show_voided" );
  regData->frozen_menu_item = glade_xml_get_widget( xml, "show_frozen" );
  regData->unreconciled_menu_item = glade_xml_get_widget( xml, "show_unreconciled" );

  /* The menu bar. Menu extension setup needs to come *after* that. */
  gnc_register_setup_menu_widgets( regData, xml );
  gnc_extensions_menu_setup_with_data( GNOME_APP(register_window),
                                       WINDOW_NAME_REGISTER, regData );

  /* The tool bar */
  {
    GtkWidget *toolbar = gnc_register_setup_toolbar( regData );
    regData->toolbar_dock = glade_xml_get_widget( xml, "toolbar_dock" );
    if ( toolbar ) {
      gtk_widget_show_all( toolbar );
      gtk_container_add( GTK_CONTAINER(regData->toolbar_dock), toolbar );
    }
  }

  /* The summary bar */
  {
    GtkWidget *summarybar = gnc_split_reg_get_summarybar( GNC_SPLIT_REG(gsr) );
    regData->summarybar_dock = glade_xml_get_widget( xml, "summarybar_dock" );
    if ( summarybar ) {
      gtk_widget_show_all(summarybar);
      gtk_container_add( GTK_CONTAINER(regData->summarybar_dock), summarybar );
    }
  }

  /* The GNCSplitReg is a widget unto itself. */
  table_frame = glade_xml_get_widget(xml, "table_frame");
  gtk_container_add( GTK_CONTAINER(table_frame), GTK_WIDGET(regData->gsr) );

  {
    int *width;
    char *prefix;

    switch (reg->type)
    {
      case STOCK_REGISTER:
      case PORTFOLIO_LEDGER:
      case CURRENCY_REGISTER:
        prefix = "reg_stock_win";
        width = &last_stock_width;
        break;

      default:
        prefix = "reg_win";
        width = &last_width;
        break;
    }

    if (*width == 0)
      gnc_get_window_size (prefix, width, NULL);

    gtk_window_set_default_size (GTK_WINDOW(regData->window), *width, 0);
  }

  gtk_widget_show_all( GTK_WIDGET(regData->window) );
  gtk_widget_hide(regData->frozen_menu_item); // Is this state supported?
  gnc_window_adjust_for_screen( GTK_WINDOW(regData->window) );

  {
    SplitRegister *sr = gnc_ledger_display_get_split_register( regData->ledger );
    gnc_split_register_config( sr, sr->type, sr->style, sr->use_double_line );
    gnc_ledger_display_refresh( regData->ledger );
  }

  return regData;
}

static
void
gnc_register_setup_menu_widgets( RegWindow *regData, GladeXML *xml )
{
  int adj = 0;
  GtkWidget *mbar, *menu, *regMenu, *regMenuItem, *tmpMi;

  /* Get our menu bar from glade. */
  mbar = glade_xml_get_widget( xml, "gnc_register_menubar" );

  /* General plan:
   * . get the GNCSplitReg menu
   * . get the RegWindow menu
   * . get, remove the additional menu[item] from the RegWindow's menu
   * . insert into the GNCSplitReg's menu.
   * . remove the RegWindow menu from the menu bar, saving it's index.
   * . insert the GNCSplitReg menu at the same index.
   * . destroy now-unused widgets. */

  if ( gnome_preferences_get_menus_have_tearoff() ) {
    /* offset by one for the tearoff menu item. */
    adj = 1;
  }

  /* Edit menu. */
  menu = gnc_split_reg_get_edit_menu( regData->gsr );
  regMenuItem = glade_xml_get_widget( xml, "gnc_register_edit_menu" );
  regMenu = glade_xml_get_widget( xml, "gnc_register_edit_menu_menu" );
  gtk_menu_shell_insert( GTK_MENU_SHELL(menu), gtk_menu_item_new(), (3 + adj));
  tmpMi = glade_xml_get_widget( xml, "gnc_register_edit_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_shell_insert( GTK_MENU_SHELL(menu), tmpMi, (4 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_find_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_shell_append( GTK_MENU_SHELL(menu), tmpMi );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM(regMenuItem) );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(regMenuItem), menu );

  /* View menu */
  menu = gnc_split_reg_get_view_menu( regData->gsr );
  regMenuItem = glade_xml_get_widget( xml, "gnc_register_view_menu" );
  regMenu = glade_xml_get_widget( xml, "gnc_register_view_menu_menu" );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_toolbar_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (0 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_summary_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (1 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_statusbar_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (2 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  gtk_menu_insert( GTK_MENU(menu), gtk_menu_item_new(), (3 + adj) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_select_trans_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (4 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM(regMenuItem) );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(regMenuItem), menu );

  /* Actions menu */
  menu = gnc_split_reg_get_action_menu( regData->gsr );
  regMenuItem = glade_xml_get_widget( xml, "gnc_register_actions_menu" );
  regMenu = glade_xml_get_widget( xml, "gnc_register_actions_menu_menu" );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_xfer_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (0 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_recn_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (1 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_stock_split_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_insert( GTK_MENU(menu), tmpMi, (2 + adj) );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  gtk_menu_insert( GTK_MENU(menu), gtk_menu_item_new(), (3 + adj) );
  /* Base this off the end of the list for a bit more flexibility. */
  gtk_menu_append( GTK_MENU(menu), gtk_menu_item_new() );
  tmpMi = glade_xml_get_widget( xml, "gnc_register_scrub_mi" );
  gtk_object_ref( GTK_OBJECT(tmpMi) );
  gtk_container_remove( GTK_CONTAINER(regMenu), tmpMi );
  gtk_menu_append( GTK_MENU(menu), tmpMi );
  gtk_object_unref( GTK_OBJECT(tmpMi) );
  gtk_menu_item_remove_submenu( GTK_MENU_ITEM(regMenuItem) );
  gtk_menu_item_set_submenu( GTK_MENU_ITEM(regMenuItem), menu );

  gtk_widget_show_all( mbar );
}

/**
 * Custom g_list_find_custom fn; returns 0 when the match is found.
 **/
static
gint
gnc_register_find_toolbarchild( gconstpointer listEltData, gconstpointer data )
{
  return !( ((GtkToolbarChild*)listEltData)->widget == data );
}

/**
 * @param srcWidget A toolbar widget to clone [label, icon, button] and insert.
 * @param idx The index to insert at, or -1 for 'append to end'.
 **/
static
void
gnc_register_insert_cloned_toolbar_elt( GtkToolbar *dstToolbar,
                                        GtkToolbar *srcToolbar,
                                        GtkWidget *srcWidget,
                                        gchar *tooltip,
                                        gpointer callback,
                                        gpointer user_data,
                                        gint idx )
{
  GtkToolbarChild *tchild;
  GList *elt;
  GtkWidget *iconCopy;
  gchar *label, *labelCopy;

  elt = g_list_find_custom( GTK_TOOLBAR(srcToolbar)->children,
                            srcWidget,
                            gnc_register_find_toolbarchild );
  g_assert( elt );
  tchild = (GtkToolbarChild*)(elt->data);
  gtk_label_get( GTK_LABEL(tchild->label), &label );
  labelCopy = g_strdup( label );
  iconCopy = gnome_pixmap_new_from_gnome_pixmap( GNOME_PIXMAP( tchild->icon ) );
  if ( idx == -1 ) {
    gtk_toolbar_append_element( dstToolbar,
                                tchild->type,
                                ( tchild->type == GTK_TOOLBAR_CHILD_RADIOBUTTON
                                  ? tchild->widget : NULL ),
                                labelCopy, tooltip, NULL, iconCopy,
                                callback, user_data );
  } else {
    gtk_toolbar_insert_element( dstToolbar,
                                tchild->type,
                                ( tchild->type == GTK_TOOLBAR_CHILD_RADIOBUTTON
                                  ? tchild->widget : NULL ),
                                labelCopy, tooltip, NULL, iconCopy,
                                callback, user_data, idx );
  }
}

static
GtkWidget*
gnc_register_setup_toolbar( RegWindow *regData )
{
  GladeXML *xml;
  GtkWidget *button;
  GtkToolbar *tbar, *regTbar;

#define CLOSE_TOOLBAR_TOOLTIP "Close this register window"
#define XFER_TOOLBAR_TOOLTIP "Transfer funds from one account to another"
#define FIND_TOOLBAR_TOOLTIP "Find transactions with a search"
#define REPORT_TOOLBAR_TOOLTIP "Open a report window for this register"
#define PRINT_TOOLBAR_TOOLTIP "Print a report for this register"

  xml = gnc_glade_xml_new( "register.glade", "gnc_register_toolbar" );
  g_assert( xml );
  regTbar = GTK_TOOLBAR(glade_xml_get_widget( xml, "gnc_register_toolbar" ));
  gtk_widget_hide( GTK_WIDGET(regTbar) );
  g_assert( regTbar );
  tbar = GTK_TOOLBAR(gnc_split_reg_get_toolbar( regData->gsr ));
  g_assert( tbar );

  /* General plan:
   * . get the GNCSplitReg toolbar
   * . get the gnc_register toolbar
   * . pull buttons from the register toolbar
   * . insert into GSR toolbar
   * . destroy [now-]unused gnc_register_toolbar */

  button = glade_xml_get_widget( xml, "gnc_register_close_b" );
  gnc_register_insert_cloned_toolbar_elt( tbar, regTbar, button,
                                          _(CLOSE_TOOLBAR_TOOLTIP),
                                          gnc_register_close_cb, regData, 0 );
  gtk_toolbar_append_space( tbar );
  button = glade_xml_get_widget( xml, "gnc_register_xfer_b" );
  gnc_register_insert_cloned_toolbar_elt( tbar, regTbar, button,
                                          _(XFER_TOOLBAR_TOOLTIP),
                                          gnc_register_xfer_cb, regData, -1 );
  gtk_toolbar_append_space( tbar );
  button = glade_xml_get_widget( xml, "gnc_register_find_b" );
  gnc_register_insert_cloned_toolbar_elt( tbar, regTbar, button,
                                          _(FIND_TOOLBAR_TOOLTIP),
                                          gnc_ui_find_transactions_cb,
                                          regData, -1 );
  button = glade_xml_get_widget( xml, "gnc_register_report_b" );
  gnc_register_insert_cloned_toolbar_elt( tbar, regTbar, button,
                                          _(REPORT_TOOLBAR_TOOLTIP),
                                          gnc_register_report_account_cb,
                                          regData, -1 );
  button = glade_xml_get_widget( xml, "gnc_register_print_b" );
  gnc_register_insert_cloned_toolbar_elt( tbar, regTbar, button,
                                          _(PRINT_TOOLBAR_TOOLTIP),
                                          gnc_register_print_cb, regData, -1 );

  gtk_widget_destroy( GTK_WIDGET(regTbar) );

  gtk_widget_show_all( GTK_WIDGET(tbar) );

  return GTK_WIDGET(tbar);
}

static void
gnc_register_help_changed_cb( GNCSplitReg *gsr, gpointer data )
{
  RegWindow *regData = data;
  SplitRegister *reg;
  const char *status;
  char *help;

  if (!regData)
    return;

  /* Get the text from the ledger */
  reg = gnc_ledger_display_get_split_register( regData->ledger );
  help = gnc_table_get_help( reg->table );
  status = help ? help : "";
  gnome_appbar_set_default( GNOME_APPBAR(regData->statusbar), status );
  g_free( help );
}

static void
gnc_reg_save_size (RegWindow *regData)
{
  SplitRegister *reg;
  int *width;
  char *prefix;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  switch (reg->type)
  {
    case STOCK_REGISTER:
    case PORTFOLIO_LEDGER:
    case CURRENCY_REGISTER:
      prefix = "reg_stock_win";
      width = &last_stock_width;
      break;
    default:
      prefix = "reg_win";
      width = &last_width;
      break;
  }

  gdk_window_get_geometry (regData->window->window, NULL, NULL,
                           width, NULL, NULL);

  gnc_save_window_size (prefix, *width, 0);
}

void 
gnc_register_new_account_cb (GtkWidget * w, gpointer data)
{
  gnc_ui_new_account_window (NULL);
}

/********************************************************************\
 * gnc_register_xfer_cb -- open up the transfer window              *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_xfer_cb (GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;

  gnc_xfer_dialog (regData->window,
                   gnc_ledger_display_leader (regData->ledger));
}


/********************************************************************\
 * gnc_register_stock_split_cb -- open up the stock split druid     *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_stock_split_cb (GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;

  gnc_stock_split_dialog (gnc_ledger_display_leader (regData->ledger));
}


/********************************************************************\
 * gnc_register_edit_cb -- open up the account edit window          *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_edit_cb(GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;
  Account *account = gnc_ledger_display_leader (regData->ledger);

  if (account == NULL)
    return;

  gnc_ui_edit_account_window(account);
}


/********************************************************************\
 * gnc_register_start_recn_cb -- open up the reconcile window...    *
 *                               called from menubar.               *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_start_recn_cb(GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;
  Account *account = gnc_ledger_display_leader (regData->ledger);

  if (account == NULL)
    return;

  recnWindow(regData->window, account);
}

static
void
gnc_register_include_date_adapter( GNCSplitReg *gsr, time_t date, gpointer user_data )
{
  RegWindow *regData;
  regData = (RegWindow*) user_data;
  if ( gnc_register_include_date( regData, date ) ) {
    gnc_ledger_display_refresh( gsr->ledger );
  }
}

static gboolean
gnc_register_include_date(RegWindow *regData, time_t date)
{
  RegDateWindow *regDateData;
  time_t start, end;
  gboolean changed = FALSE;

  regDateData = regData->date_window;

  start = gnc_date_edit_get_date(GNC_DATE_EDIT(regDateData->start_date));
  end   = gnc_date_edit_get_date(GNC_DATE_EDIT(regDateData->end_date));

  if (date < start)
  {
    gnc_date_edit_set_time(GNC_DATE_EDIT(regDateData->start_date), date);
    changed = TRUE;
  }

  if (date > end)
  {
    gnc_date_edit_set_time(GNC_DATE_EDIT(regDateData->end_date), date);
    changed = TRUE;
  }

  if (changed)
    gnc_register_set_date_range(regData);

  return changed;
}

static
void
gnc_register_set_read_only( RegWindow *regData )
{
  gchar *old_title, *new_title;
  GtkArg objarg;

  objarg.name = "GtkWindow::title";
  gtk_object_arg_get(GTK_OBJECT(regData->window), &objarg, NULL);
  old_title = GTK_VALUE_STRING(objarg);
  new_title = g_strdup_printf(_("%s [Read-Only]"), old_title);
  gtk_window_set_title( GTK_WINDOW(regData->window), new_title );
  g_free(old_title);
  g_free(new_title);

  regData->read_only = TRUE;
}

/********************************************************************\
 * gnc_register_close_cb                                            *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_close_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  gnc_split_reg_check_close( GNC_SPLIT_REG(regData->gsr) );
  gnc_ledger_display_close( regData->ledger );
}

static int
report_helper (RegWindow *regData, Split *split, Query *query)
{
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);
  Account *account;
  char *str;
  SCM qtype;
  SCM args;
  SCM func;
  SCM arg;

  args = SCM_EOL;

  func = gh_eval_str ("gnc:register-report-create");
  g_return_val_if_fail (gh_procedure_p (func), -1);

  /* FIXME: when we drop support older guiles, drop the (char *) coercion. */
  arg = gh_str02scm ((char *) gnc_split_register_get_credit_string (reg));
  args = gh_cons (arg, args);

  /* FIXME: when we drop support older guiles, drop the (char *) coercion. */
  arg = gh_str02scm ((char *) gnc_split_register_get_debit_string (reg));
  args = gh_cons (arg, args);

  str = gnc_reg_get_name (regData, FALSE);
  arg = gh_str02scm (str);
  args = gh_cons (arg, args);
  g_free (str);

  arg = gh_bool2scm (reg->use_double_line);
  args = gh_cons (arg, args);

  arg = gh_bool2scm (reg->style == REG_STYLE_JOURNAL);
  args = gh_cons (arg, args);

  if (!query)
  {
    query = gnc_ledger_display_get_query (regData->ledger);
    g_return_val_if_fail (query != NULL, -1);
  }

  qtype = gh_eval_str("<gnc:Query*>");
  g_return_val_if_fail (qtype != SCM_UNDEFINED, -1);

  arg = gw_wcp_assimilate_ptr (query, qtype);
  args = gh_cons (arg, args);
  g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


  if (split)
  {
    qtype = gh_eval_str("<gnc:Split*>");
    g_return_val_if_fail (qtype != SCM_UNDEFINED, -1);
    arg = gw_wcp_assimilate_ptr (split, qtype);
  }
  else
  {
    arg = SCM_BOOL_F;
  }
  args = gh_cons (arg, args);
  g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


  qtype = gh_eval_str("<gnc:Account*>");
  g_return_val_if_fail (qtype != SCM_UNDEFINED, -1);

  account = gnc_ledger_display_leader (regData->ledger);
  arg = gw_wcp_assimilate_ptr (account, qtype);
  args = gh_cons (arg, args);
  g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


  /* Apply the function to the args */
  arg = gh_apply (func, args);
  g_return_val_if_fail (gh_exact_p (arg), -1);

  return gh_scm2int (arg);
}

/********************************************************************\
 * gnc_register_report_account_cb                                   *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_report_account_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  int id;

  id = report_helper (regData, NULL, NULL);
  if (id >= 0)
    reportWindow (id);
}

/********************************************************************\
 * gnc_register_report_trans_cb                                     *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_report_trans_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Split *split;
  Query *query;
  int id;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  split = gnc_split_register_get_current_split (reg);
  if (!split)
    return;

  query = xaccMallocQuery ();

  xaccQuerySetBook (query, gnc_get_current_book ());

  xaccQueryAddGUIDMatch (query, xaccSplitGetGUID (split),
                         GNC_ID_SPLIT, QUERY_AND);

  id = report_helper (regData, split, query);
  if (id >= 0)
    reportWindow (id);
}

/********************************************************************\
 * gnc_register_print_cb                                            *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_print_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  int id;

  id = report_helper (regData, NULL, NULL);
  if (id >= 0)
    gnc_print_report (id);
}

/********************************************************************\
 * gnc_register_date_range_cb                                       *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_date_range_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_show_date_window(regData);
}

void
gnc_register_gl_cb(GtkWidget *widget, gpointer data)
{
  GNCLedgerDisplay *ld;
  RegWindow *regData;

  ld = gnc_ledger_display_gl();
  regData = regWindowLedger( ld );
  gnc_split_reg_raise( regData->gsr );
}

void
gnc_register_prices_cb(GtkWidget *widget, gpointer data)
{
  gnc_prices_dialog (NULL);
}

void
gnc_register_commodities_cb(GtkWidget *widget, gpointer data)
{
  gnc_commodities_dialog (NULL);
}

void
gnc_register_fincalc_cb(GtkWidget *widget, gpointer data)
{
  gnc_ui_fincalc_dialog_create();
}

void
gnc_register_toolbar_cb( GtkWidget *widget, gpointer data )
{
  RegWindow *rw = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show( rw->toolbar_dock );
  } else {
    gtk_widget_hide( rw->toolbar_dock );
    gtk_widget_queue_resize( rw->toolbar_dock );
  }
}

void
gnc_register_summarybar_cb( GtkWidget *widget, gpointer data )
{
  RegWindow *rw = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show( rw->summarybar_dock );
  } else {
    gtk_widget_hide( rw->summarybar_dock );
    gtk_widget_queue_resize( rw->summarybar_dock );
  }
}

void
gnc_register_statusbar_cb( GtkWidget *widget, gpointer data )
{
  RegWindow *rw = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show( rw->statusbar );
  } else {
    gtk_widget_hide( rw->statusbar );
    gtk_widget_queue_resize( rw->statusbar );
  }
}


/************************** END OF FILE **************************/

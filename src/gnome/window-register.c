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
#include "dialog-sx-from-trans.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-euro.h"
#include "gnc-gui-query.h"
#include "gnc-ledger-display.h"
#include "gnc-pricedb.h"
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

typedef enum {
  BY_STANDARD = 0,
  BY_DATE,
  BY_DATE_ENTERED,
  BY_DATE_RECONCILED,
  BY_NUM,
  BY_AMOUNT,
  BY_MEMO,
  BY_DESC
} sort_type_t;

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
  GNCLedgerDisplay * ledger;   

  /* Top level window */
  GtkWidget * window;

  gint width;

  GtkWidget * toolbar;
  GtkWidget * toolbar_dock;
  SCM toolbar_change_callback_id;

  GtkWidget * summarybar_dock;
  GtkWidget * statusbar;

  GtkWidget * double_line_check;

  GtkWidget * split_button;
  GtkWidget * split_menu_check;
  GtkWidget * split_popup_check;

  GtkWidget * balance_label;
  GtkWidget * cleared_label;
  GtkWidget * reconciled_label;
  GtkWidget * future_label;
  GtkWidget * shares_label;
  GtkWidget * value_label;

  GnucashRegister *reg;

  sort_type_t sort_type;

  RegDateWindow *date_window;
};


/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;

static int last_width = 0;
static int last_stock_width = 0;

static GSList *date_param = NULL;

/** PROTOTYPES ******************************************************/
static void gnc_register_redraw_all_cb (GnucashRegister *g_reg, gpointer data);
static void gnc_register_redraw_help_cb (GnucashRegister *g_reg,
                                         gpointer data);
static void gnc_reg_refresh_toolbar(RegWindow *regData);
static void regDestroy(GNCLedgerDisplay *ledger);
static void gnc_register_check_close(RegWindow *regData);
void gnc_register_cut_cb(GtkWidget *w, gpointer data);
void gnc_register_copy_cb(GtkWidget *w, gpointer data);
void gnc_register_paste_cb(GtkWidget *w, gpointer data);
void gnc_register_cut_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_copy_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_paste_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_start_recn_cb(GtkWidget *w, gpointer data);
void gnc_register_xfer_cb(GtkWidget *w, gpointer data);
void gnc_register_stock_split_cb (GtkWidget * w, gpointer data);
void gnc_register_edit_cb(GtkWidget *w, gpointer data);
void gnc_register_help_cb(GtkWidget *w, gpointer data);
void gnc_register_new_account_cb(GtkWidget * w, gpointer data);
void gnc_register_delete_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_duplicate_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_recur_cb(GtkWidget *w, gpointer data);
void gnc_register_record_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_cancel_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_close_cb(GtkWidget *w, gpointer data);
void gnc_register_exit_cb(GtkWidget *w, gpointer data);
void gnc_register_report_account_cb(GtkWidget *w, gpointer data);
void gnc_register_report_trans_cb(GtkWidget *w, gpointer data);
void gnc_register_print_cb(GtkWidget *w, gpointer data);
void gnc_register_date_cb(GtkWidget *widget, gpointer data);
void gnc_register_date_show_all_cb(GtkWidget *w, gpointer data);
void gnc_register_today_cb(GtkWidget *w, gpointer data);
void gnc_register_date_toggle_cb(GtkToggleButton *toggle, gpointer data);
void gnc_register_date_range_cb(GtkWidget *w, gpointer data);
void gnc_register_expand_trans_menu_cb(GtkWidget *widget, gpointer data);
void gnc_register_expand_trans_toolbar_cb(GtkWidget *widget, gpointer data);
void gnc_register_new_trans_cb(GtkWidget *widget, gpointer data);
void gnc_register_jump_cb(GtkWidget *widget, gpointer data);
void gnc_register_print_check_cb(GtkWidget * widget, gpointer data);
void gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data);
void gnc_register_toolbar_cb(GtkWidget *widget, gpointer data);
void gnc_register_summarybar_cb(GtkWidget *widget, gpointer data);
void gnc_register_statusbar_cb(GtkWidget *widget, gpointer data);
void gnc_register_gl_cb(GtkWidget *widget, gpointer data);
void gnc_register_prices_cb(GtkWidget *widget, gpointer data);
void gnc_register_commodities_cb(GtkWidget *widget, gpointer data);
void gnc_register_fincalc_cb(GtkWidget *widget, gpointer data);
void gnc_register_style_ledger_cb (GtkWidget *w, gpointer data);
void gnc_register_style_auto_ledger_cb (GtkWidget *w, gpointer data);
void gnc_register_style_journal_cb (GtkWidget *w, gpointer data);
void gnc_register_double_line_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_standard_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_date_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_date_entered_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_date_reconciled_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_num_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_amount_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_memo_cb (GtkWidget *w, gpointer data);
void gnc_register_sort_desc_cb (GtkWidget *w, gpointer data);
void gnc_register_scrub_all_cb (GtkWidget *widget, gpointer data);
void gnc_register_scrub_current_cb (GtkWidget *widget, gpointer data);

void gnc_register_record_cb (GnucashRegister *reg, gpointer data);
gboolean gnc_register_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data);
void gnc_register_destroy_cb(GtkWidget *widget, gpointer data);
void gnc_register_size_allocate (GtkWidget *widget, GtkAllocation *allocation, gpointer user_data);

static gboolean gnc_register_include_date(RegWindow *regData, time_t date);


/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window for Account account                 *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowSimple (Account *account)
{
  RegWindow *result = NULL;
  GNCLedgerDisplay * ledger = gnc_ledger_display_simple (account);

  if (ledger != NULL)
    result = regWindowLedger (ledger);

  return result;
}


/********************************************************************\
 * regWindowAccGroup                                                *
 *   opens up a register window for a group of Accounts             *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowAccGroup (Account *account)
{
  RegWindow *result = NULL;
  GNCLedgerDisplay * ledger = gnc_ledger_display_subaccounts (account);

  if (ledger != NULL)
    result = regWindowLedger (ledger);

  return result;
}


/********************************************************************\
 * gnc_register_raise                                               *
 *   raise an existing register window to the front                 *
 *                                                                  *
 * Args:   regData - the register data structure                    *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_register_raise (RegWindow *regData)
{
  if (regData == NULL)
    return;

  if (regData->window == NULL)
    return;

  gtk_window_present (GTK_WINDOW(regData->window));
}


/********************************************************************\
 * gnc_register_jump_to_split                                       *
 *   move the cursor to the split, if present in register           *
 *                                                                  *
 * Args:   regData - the register data structure                    *
 *         split   - the split to jump to                           *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_register_jump_to_split(RegWindow *regData, Split *split)
{
  Transaction *trans;
  VirtualCellLocation vcell_loc;
  SplitRegister *reg;

  if (!regData) return;

  trans = xaccSplitGetParent(split);
  if (trans != NULL)
    if (gnc_register_include_date(regData, xaccTransGetDate(trans)))
    {
      gnc_ledger_display_refresh (regData->ledger);
    }

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  if (gnc_split_register_get_split_virt_loc(reg, split, &vcell_loc))
    gnucash_register_goto_virt_cell(regData->reg, vcell_loc);
}


/********************************************************************\
 * gnc_register_jump_to_split_amount                                *
 *   move the cursor to the split in the non-blank amount column    *
 *                                                                  *
 * Args:   regData - the register data structure                    *
 *         split   - the split to jump to                           *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_register_jump_to_split_amount(RegWindow *regData, Split *split)
{
  Transaction *trans;
  VirtualLocation virt_loc;
  SplitRegister *reg;

  if (!regData) return;

  trans = xaccSplitGetParent(split);
  if (trans != NULL)
    if (gnc_register_include_date(regData, xaccTransGetDate(trans)))
    {
      gnc_ledger_display_refresh (regData->ledger);
    }

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  if (gnc_split_register_get_split_amount_virt_loc (reg, split, &virt_loc))
    gnucash_register_goto_virt_loc (regData->reg, virt_loc);
}


static void
gnc_register_change_style (RegWindow *regData, SplitRegisterStyle style)
{
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);

  if (style == reg->style)
    return;

  gnc_split_register_config (reg, reg->type, style, reg->use_double_line);

  gnc_ledger_display_refresh (regData->ledger);
}

void
gnc_register_style_ledger_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_LEDGER);
}

void
gnc_register_style_auto_ledger_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_AUTO_LEDGER);
}

void
gnc_register_style_journal_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_JOURNAL);
}

void
gnc_register_double_line_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);
  gboolean use_double_line;

  use_double_line = GTK_CHECK_MENU_ITEM(w)->active;

  if (use_double_line == reg->use_double_line)
    return;

  gnc_split_register_config (reg, reg->type, reg->style, use_double_line);

  gnc_ledger_display_refresh (regData->ledger);
}

static void
gnc_register_sort (RegWindow *regData, sort_type_t sort_code)
{
  Query *query = gnc_ledger_display_get_query (regData->ledger);
  gboolean show_present_divider = FALSE;
  GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;
  SplitRegister *reg;

  if (regData->sort_type == sort_code)
    return;

  standard = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);

  switch (sort_code)
  {
    case BY_STANDARD:
      p1 = standard;
      show_present_divider = TRUE;
      break;
    case BY_DATE:
      p1 = g_slist_prepend (p1, TRANS_DATE_POSTED);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      show_present_divider = TRUE;
      break;
    case BY_DATE_ENTERED:
      p1 = g_slist_prepend (p1, TRANS_DATE_ENTERED);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_DATE_RECONCILED:
      p1 = g_slist_prepend (p1, SPLIT_RECONCILE);
      p2 = g_slist_prepend (p2, SPLIT_DATE_RECONCILED);
      p3 = standard;
      break;
    case BY_NUM:
      p1 = g_slist_prepend (p1, TRANS_NUM);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    case BY_AMOUNT:
      p1 = g_slist_prepend (p1, SPLIT_VALUE);
      p2 = standard;
      break;
    case BY_MEMO:
      p1 = g_slist_prepend (p1, SPLIT_MEMO);
      p2 = standard;
      break;
    case BY_DESC:
      p1 = g_slist_prepend (p1, TRANS_DESCRIPTION);
      p1 = g_slist_prepend (p1, SPLIT_TRANS);
      p2 = standard;
      break;
    default:
      g_slist_free (standard);
      g_return_if_fail (FALSE);
  }

  gncQuerySetSortOrder (query, p1, p2, p3);

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  gnc_split_register_show_present_divider (reg, show_present_divider);

  regData->sort_type = sort_code;

  gnc_ledger_display_refresh(regData->ledger);
}

void
gnc_register_sort_standard_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_STANDARD);
}

void
gnc_register_sort_date_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE);
}

void
gnc_register_sort_date_entered_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE_ENTERED);
}

void
gnc_register_sort_date_reconciled_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE_RECONCILED);
}

void
gnc_register_sort_num_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_NUM);
}

void
gnc_register_sort_amount_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_AMOUNT);
}

void
gnc_register_sort_memo_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_MEMO);
}

void
gnc_register_sort_desc_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DESC);
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
gnc_register_set_date_range(RegWindow *regData)
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

  gtk_widget_set_sensitive(regDateData->set_button, FALSE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);

  if (date_param == NULL) {
    date_param = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    date_param = g_slist_prepend (date_param, SPLIT_TRANS);
  }

  gncQueryPurgeTerms (query, date_param);

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

static GtkWidget *
add_summary_label (GtkWidget *summarybar, const char *label_str)
{
  GtkWidget *hbox;
  GtkWidget *label;

  hbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_start (GTK_BOX(summarybar), hbox, FALSE, FALSE, 5);

  label = gtk_label_new (label_str);
  gtk_misc_set_alignment (GTK_MISC(label), 1.0, 0.5);
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);

  label = gtk_label_new ("");
  gtk_misc_set_alignment (GTK_MISC(label), 1.0, 0.5);
  gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);

  return label;
}

static GtkWidget *
gnc_register_create_summary_bar (RegWindow *regData)
{
  gboolean has_shares;
  GtkWidget *summarybar;

  regData->cleared_label    = NULL;
  regData->balance_label    = NULL;
  regData->reconciled_label = NULL;
  regData->future_label     = NULL;
  regData->shares_label     = NULL;
  regData->value_label      = NULL;

  if (gnc_ledger_display_type (regData->ledger) >= LD_SUBACCOUNT)
    return NULL;

  {
    Account *account;
    GNCAccountType atype;

    account = gnc_ledger_display_leader (regData->ledger);
    atype = xaccAccountGetType (account);

    switch (atype)
    {
      case STOCK:
      case MUTUAL:
      case CURRENCY:
        has_shares = TRUE;
        break;

      default:
        has_shares = FALSE;
        break;
    }
  }

  summarybar = gtk_hbox_new (FALSE, 4);

  if (!has_shares)
  {
    regData->balance_label    = add_summary_label (summarybar, _("Present:"));
    regData->future_label     = add_summary_label (summarybar, _("Future:"));
    regData->cleared_label    = add_summary_label (summarybar, _("Cleared:"));
    regData->reconciled_label = add_summary_label (summarybar,
                                                   _("Reconciled:"));
  }
  else
  {
    regData->shares_label     = add_summary_label (summarybar, _("Shares:"));
    regData->value_label      = add_summary_label (summarybar,
                                                   _("Current Value:"));
  }

  return summarybar;
}

void
gnc_register_jump_to_blank (RegWindow *regData)
{
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);
  VirtualCellLocation vcell_loc;
  Split *blank;

  blank = gnc_split_register_get_blank_split (reg);
  if (blank == NULL)
    return;

  if (gnc_split_register_get_split_virt_loc (reg, blank, &vcell_loc))
    gnucash_register_goto_virt_cell (regData->reg, vcell_loc);
}


void
gnc_register_expand_trans_menu_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  gboolean expand;
  SplitRegister *reg;

  if (!regData)
    return;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  expand = GTK_CHECK_MENU_ITEM (widget)->active;

  gnc_split_register_expand_current_trans (reg, expand);
}

void
gnc_register_expand_trans_toolbar_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  gboolean expand;
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  expand = GTK_TOGGLE_BUTTON (widget)->active;

  gnc_split_register_expand_current_trans (reg, expand);
}

void
gnc_register_new_trans_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  if (gnc_split_register_save (reg, TRUE))
    gnc_split_register_redraw (reg);

  gnc_register_jump_to_blank (regData);
}

void
gnc_register_jump_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Account *account;
  Account *leader;
  Split *split;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  split = gnc_split_register_get_current_split (reg);
  if (split == NULL)
    return;

  account = xaccSplitGetAccount(split);
  if (account == NULL)
    return;

  leader = gnc_ledger_display_leader (regData->ledger);

  if (account == leader)
  {
    split = xaccSplitGetOtherSplit(split);
    if (split == NULL)
      return;

    account = xaccSplitGetAccount(split);
    if (account == NULL)
      return;
    if (account == leader)
      return;
  }

  regData = regWindowSimple(account);
  if (regData == NULL)
    return;

  gnc_register_raise (regData);
  gnc_register_jump_to_split (regData, split);
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

  SCM print_check = gh_eval_str("gnc:print-check");

  if(split && trans &&
     gh_procedure_p(print_check))
  {
    payee  = xaccTransGetDescription(trans);
    memo   = xaccSplitGetMemo(split);
    amount = xaccSplitGetAmount(split);
    amount = gnc_numeric_abs (amount);
    date   = xaccTransGetDate(trans);

    gh_apply(print_check,
             /* FIXME: when we drop support older guiles, drop the
                (char *) coercions below. */
             SCM_LIST4(gh_str02scm((char *) payee),
                       gh_double2scm(gnc_numeric_to_double (amount)),
                       gh_ulong2scm(date),
                       gh_str02scm((char *) memo)));
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

static void
gnc_register_setup_menu_widgets(RegWindow *regData, GladeXML *xml)
{
  /* Make sure the right style radio item is active */
  {
    SplitRegister *reg;
    GtkWidget *widget;
    char *widget_name;

    reg = gnc_ledger_display_get_split_register (regData->ledger);

    switch (reg->style)
    {
      default:
      case REG_STYLE_LEDGER:
        widget_name = "menu_style_basic_ledger";
        break;
      case REG_STYLE_AUTO_LEDGER:
        widget_name = "menu_style_auto_split_ledger";
        break;
      case REG_STYLE_JOURNAL:
        widget_name = "menu_style_transaction_journal";
        break;
    }

    /* registers with more than one account can only use journal mode */
    if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    {
      widget = glade_xml_get_widget(xml, "menu_style_basic_ledger");
      gtk_widget_set_sensitive (widget, FALSE);

      widget = glade_xml_get_widget(xml, "menu_style_auto_split_ledger");
      gtk_widget_set_sensitive (widget, FALSE);
    }

    widget = glade_xml_get_widget(xml, widget_name);

    gtk_signal_handler_block_by_data(GTK_OBJECT(widget), regData);

    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);

    gtk_signal_handler_unblock_by_data(GTK_OBJECT(widget), regData);
  }
}


static GtkWidget *
gnc_register_create_popup_menu (RegWindow *regData)
{
  GtkWidget *popup;
  GladeXML *xml;

  xml = gnc_glade_xml_new ("register.glade", "Check Register Popup Menu");

  popup = glade_xml_get_widget (xml, "Check Register Popup Menu");

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    regData);

  /* Glade insists on making this a tearoff menu. */
  if (gnome_preferences_get_menus_have_tearoff()) {
    GtkMenuShell *ms = GTK_MENU_SHELL(popup);
    GtkWidget *tearoff;

    tearoff = g_list_nth_data(ms->children, 0);
    ms->children = g_list_remove(ms->children, tearoff);
    gtk_widget_destroy(tearoff);
  }

  regData->split_popup_check = glade_xml_get_widget (xml, "popup_splits");

  return popup;
}

static void
gnc_register_record (RegWindow *regData)
{
  SplitRegister *reg;
  Transaction *trans;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  trans = gnc_split_register_get_current_trans (reg);

  if (!gnc_split_register_save (reg, TRUE))
    return;

  if (trans != NULL)
    gnc_register_include_date (regData, xaccTransGetDate(trans));

  gnc_split_register_redraw (reg);
}

static gboolean
gnc_register_match_trans_row (VirtualLocation virt_loc,
                              gpointer user_data)
{
  RegWindow *regData = user_data;
  CursorClass cursor_class;
  SplitRegister *sr;

  sr = gnc_ledger_display_get_split_register (regData->ledger);
  cursor_class = gnc_split_register_get_cursor_class (sr, virt_loc.vcell_loc);

  return (cursor_class == CURSOR_CLASS_TRANS);
}

static void
gnc_register_goto_next_trans_row (RegWindow *regData)
{
  gnucash_register_goto_next_matching_row (regData->reg,
                                           gnc_register_match_trans_row,
                                           regData);
}

static void
gnc_register_enter (RegWindow *regData, gboolean next_transaction)
{
  SplitRegister *sr = gnc_ledger_display_get_split_register (regData->ledger);
  gboolean goto_blank;

  goto_blank = gnc_lookup_boolean_option("Register",
                                         "'Enter' moves to blank transaction",
                                         FALSE);

  /* If we are in single or double line mode and we hit enter
   * on the blank split, go to the blank split instead of the
   * next row. This prevents the cursor from jumping around
   * when you are entering transactions. */
  if (!goto_blank && !next_transaction)
  {
    SplitRegisterStyle style = sr->style;

    if (style == REG_STYLE_LEDGER)
    {
      Split *blank_split;

      blank_split = gnc_split_register_get_blank_split(sr);
      if (blank_split != NULL)
      {
        Split *current_split;

        current_split = gnc_split_register_get_current_split(sr);

        if (blank_split == current_split)
          goto_blank = TRUE;
      }
    }
  }

  /* First record the transaction. This will perform a refresh. */
  gnc_register_record (regData);

  if (!goto_blank && next_transaction)
    gnc_split_register_expand_current_trans (sr, FALSE);

  /* Now move. */
  if (goto_blank)
    gnc_register_jump_to_blank (regData);
  else if (next_transaction)
    gnc_register_goto_next_trans_row (regData);
  else
    gnucash_register_goto_next_virt_row (regData->reg);
}

void
gnc_register_record_cb (GnucashRegister *reg, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_enter (regData, FALSE);
}

gboolean
gnc_register_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_check_close (regData);

  gnc_ledger_display_close (regData->ledger);

  return TRUE; /* don't close */
}

void
gnc_register_destroy_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SCM id;

  id = regData->toolbar_change_callback_id;
  gnc_unregister_option_change_callback_id(id);

  if (regData->date_window != NULL)
  {
    if (regData->date_window->window != NULL)
      gtk_widget_destroy(regData->date_window->window);

    g_free(regData->date_window);
    regData->date_window = NULL;
  }

  g_free(regData);

  DEBUG ("destroyed RegWindow");
}

static gncUIWidget
gnc_register_get_parent(GNCLedgerDisplay *ledger)
{
  RegWindow *regData = gnc_ledger_display_get_user_data (ledger);

  if (regData == NULL)
    return NULL;

  return regData->window;
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

  windowname = gnc_reg_get_name (regData, TRUE);

  gtk_window_set_title (GTK_WINDOW(regData->window), windowname);

  g_free (windowname);
}

static void
gnc_toolbar_change_cb (void *data)
{
  RegWindow *regData = data;

  gnc_reg_refresh_toolbar (regData);
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

  gtk_window_set_default_size (GTK_WINDOW(regData->window), regData->width, 0);
}

/********************************************************************\
 * regWindowLedger                                                  *
 *   opens up a ledger window for the account list                  *
 *                                                                  *
 * Args:   ledger - ledger data structure                           *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowLedger (GNCLedgerDisplay *ledger)
{
  SplitRegister *reg;
  RegWindow *regData;
  GtkWidget *register_window;
  GtkWidget *table_frame;
  gboolean show_all;
  gboolean has_date;
  GladeXML *xml;

  reg = gnc_ledger_display_get_split_register (ledger);

  regData = gnc_ledger_display_get_user_data (ledger);
  if (regData != NULL)
    return regData;

  regData = g_new (RegWindow, 1);

  gnc_ledger_display_set_user_data (ledger, regData);

  gnc_ledger_display_set_handlers (ledger,
                                   regDestroy,
                                   gnc_register_get_parent);

  xml = gnc_glade_xml_new ("register.glade", "Check Register");
  register_window = glade_xml_get_widget (xml, "Check Register");

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
				    regData);

  regData->ledger = ledger;
  regData->window = register_window;
  regData->sort_type = BY_STANDARD;
  regData->width = -1;

  gnc_reg_set_window_name(regData);

  show_all = gnc_lookup_boolean_option ("Register",
                                        "Show All Transactions",
                                        TRUE);

  if (date_param == NULL) {
    date_param = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    date_param = g_slist_prepend (date_param, SPLIT_TRANS);
  }

  {
    Query *q = gnc_ledger_display_get_query (regData->ledger);

    has_date = gncQueryHasTermType (q, date_param);
  }

  if (has_date)
    show_all = FALSE;

  regData->date_window = gnc_register_date_window (regData, show_all);

  if (reg->type != SEARCH_LEDGER && !has_date)
    gnc_register_set_date_range (regData);

  /* Now that we have a date range, remove any existing
   * maximum on the number of splits returned. */
  xaccQuerySetMaxSplits (gnc_ledger_display_get_query (regData->ledger), -1);

  /* The status bar */
  {
    regData->statusbar = glade_xml_get_widget(xml, "appbar");
  }

  /* The menu bar */
  {
    regData->double_line_check =
      glade_xml_get_widget (xml, "menu_style_double_line");
    regData->split_menu_check =
      glade_xml_get_widget (xml, "menu_splits");
    
    gnc_register_setup_menu_widgets(regData, xml);
  }

  /* The tool bar */
  {
    SCM id;

    regData->toolbar_dock = glade_xml_get_widget (xml, "toolbar_dock");
    regData->toolbar = glade_xml_get_widget (xml, "toolbar");
    regData->split_button = glade_xml_get_widget (xml, "toolbar_split");

    id = gnc_register_option_change_callback(gnc_toolbar_change_cb, regData,
                                             "General", "Toolbar Buttons");
    regData->toolbar_change_callback_id = id;
  }

  /* The summary bar */
  {
    GtkWidget *summarybar = gnc_register_create_summary_bar (regData);
    regData->summarybar_dock = glade_xml_get_widget (xml, "summarybar_dock");
    if (summarybar) {
      gtk_widget_show_all(summarybar);
      gtk_container_add(GTK_CONTAINER(regData->summarybar_dock), summarybar);
    }
  }

  /* The CreateTable will do the actual gui init, returning a widget */
  {
    GtkWidget *register_widget;
    GtkWidget *popup;
    guint num_rows;

    table_frame = glade_xml_get_widget(xml, "table_frame");

    num_rows = (guint) gnc_lookup_number_option ("Register",
                                                 "Number of Rows", 20.0);
    gnucash_register_set_initial_rows (num_rows);

    register_widget = gnucash_register_new (reg->table);
    gnc_table_init_gui (register_widget, reg);

    gtk_container_add (GTK_CONTAINER(table_frame), register_widget);

    regData->reg = GNUCASH_REGISTER (register_widget);
    GNUCASH_SHEET(regData->reg->sheet)->window = register_window;

    gtk_signal_connect (GTK_OBJECT(register_widget), "activate_cursor",
                        GTK_SIGNAL_FUNC(gnc_register_record_cb), regData);
    gtk_signal_connect (GTK_OBJECT(register_widget), "redraw_all",
                        GTK_SIGNAL_FUNC(gnc_register_redraw_all_cb), regData);
    gtk_signal_connect (GTK_OBJECT(register_widget), "redraw_help",
                        GTK_SIGNAL_FUNC(gnc_register_redraw_help_cb), regData);

    popup = gnc_register_create_popup_menu (regData);
    gnucash_register_attach_popup (GNUCASH_REGISTER(register_widget),
                                   popup, regData);
  }

  {
    gboolean use_double_line;
    GtkCheckMenuItem *check;

    use_double_line = gnc_ledger_display_default_double_line (regData->ledger);

    /* be sure to initialize the gui elements associated with the cursor */
    gnc_split_register_config (reg, reg->type, reg->style, use_double_line);

    check = GTK_CHECK_MENU_ITEM (regData->double_line_check);

    gtk_signal_handler_block_by_func
      (GTK_OBJECT (check),
       GTK_SIGNAL_FUNC (gnc_register_double_line_cb), regData);

    gtk_check_menu_item_set_active (check, use_double_line);

    gtk_signal_handler_unblock_by_func
      (GTK_OBJECT (check),
       GTK_SIGNAL_FUNC (gnc_register_double_line_cb), regData);
  }

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

    gtk_window_set_default_size (GTK_WINDOW(register_window), *width, 0);
  }

  gtk_widget_show_all (register_window);

  gnc_split_register_show_present_divider (reg, TRUE);

  gnc_ledger_display_refresh (ledger);
  gnc_reg_refresh_toolbar (regData);

  gnc_window_adjust_for_screen (GTK_WINDOW(register_window));

  return regData;
}


static void
gnc_reg_refresh_toolbar (RegWindow *regData)
{
  GtkToolbarStyle tbstyle;

  if ((regData == NULL) || (regData->toolbar == NULL))
    return;

  tbstyle = gnc_get_toolbar_style ();

  gtk_toolbar_set_style (GTK_TOOLBAR (regData->toolbar), tbstyle);
}


static gnc_numeric
gnc_account_present_balance (Account *account)
{
  GList *list;
  GList *node;
  time_t today;
  struct tm *tm;

  if (!account)
    return gnc_numeric_zero ();

  today = time (NULL);

  tm = localtime (&today);

  tm->tm_hour = 23;
  tm->tm_min = 59;
  tm->tm_sec = 59;
  tm->tm_isdst = -1;

  today = mktime (tm);

  list = xaccAccountGetSplitList (account);

  for (node = g_list_last (list); node; node = node->prev)
  {
    Split *split = node->data;

    if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
      return xaccSplitGetBalance (split);
  }

  return gnc_numeric_zero ();
}

static GNCPrice *
account_latest_price (Account *account)
{
  GNCBook *book;
  GNCPriceDB *pdb;
  gnc_commodity *commodity;
  gnc_commodity *currency;

  commodity = xaccAccountGetCommodity (account);
  currency = gnc_default_currency ();

  book = gnc_get_current_book ();
  pdb = gnc_book_get_pricedb (book);

  return gnc_pricedb_lookup_latest (pdb, commodity, currency);
}

static void
gnc_register_redraw_all_cb (GnucashRegister *g_reg, gpointer data)
{
  RegWindow *regData = data;
  gnc_commodity * commodity;
  GNCPrintAmountInfo print_info;
  gnc_numeric amount;
  Account *leader;
  char string[256];
  gboolean reverse;
  gboolean euro;

  if (regData->window == NULL)
    return;

  leader = gnc_ledger_display_leader (regData->ledger);

  euro = gnc_lookup_boolean_option ("International",
                                    "Enable EURO support",
                                    FALSE);

  commodity = xaccAccountGetCommodity (leader);

  /* no EURO converson, if account is already EURO or no EURO currency */
  if (commodity != NULL)
    euro = (euro && gnc_is_euro_currency(commodity));
  else
    euro = FALSE;

  print_info = gnc_account_print_info (leader, TRUE);

  reverse = gnc_reverse_balance(leader);

  if (regData->balance_label != NULL)
  {
    amount = gnc_account_present_balance (leader);
    if (reverse)
      amount = gnc_numeric_neg (amount);

    xaccSPrintAmount (string, amount, print_info);
    if (euro)
    {
      strcat (string, " / ");
      xaccSPrintAmount (string + strlen (string),
                        gnc_convert_to_euro (commodity, amount),
                        gnc_commodity_print_info (gnc_get_euro (), TRUE));
    }

    gnc_set_label_color (regData->balance_label, amount);
    gtk_label_set_text (GTK_LABEL(regData->balance_label), string);
  }

  if (regData->cleared_label != NULL)
  {
    amount = xaccAccountGetClearedBalance (leader);
    if (reverse)
      amount = gnc_numeric_neg (amount);

    xaccSPrintAmount (string, amount, print_info);
    if (euro)
    {
      strcat (string, " / ");
      xaccSPrintAmount (string + strlen (string),
                        gnc_convert_to_euro (commodity, amount),
                        gnc_commodity_print_info (gnc_get_euro (), TRUE));
    }

    gnc_set_label_color (regData->cleared_label, amount);
    gtk_label_set_text (GTK_LABEL (regData->cleared_label), string);
  }

  if (regData->reconciled_label != NULL)
  {
    amount = xaccAccountGetReconciledBalance (leader);
    if (reverse)
      amount = gnc_numeric_neg (amount);

    xaccSPrintAmount (string, amount, print_info);
    if (euro)
    {
      strcat(string, " / ");
      xaccSPrintAmount (string + strlen(string),
                        gnc_convert_to_euro(commodity, amount),
                        gnc_commodity_print_info (gnc_get_euro (), TRUE));
    }

    gnc_set_label_color (regData->reconciled_label, amount);
    gtk_label_set_text (GTK_LABEL(regData->reconciled_label), string);
  }

  if (regData->future_label != NULL)
  {
    amount = xaccAccountGetBalance (leader);
    if (reverse)
      amount = gnc_numeric_neg (amount);

    xaccSPrintAmount (string, amount, print_info);
    if (euro)
    {
      strcat (string, " / ");
      xaccSPrintAmount (string + strlen(string),
                        gnc_convert_to_euro(commodity, amount),
                        gnc_commodity_print_info (gnc_get_euro (), TRUE));
    }

    gnc_set_label_color (regData->future_label, amount);
    gtk_label_set_text (GTK_LABEL (regData->future_label), string);
  }

  if (regData->shares_label != NULL)
  {
    print_info = gnc_account_print_info (leader, TRUE);

    amount = xaccAccountGetBalance (leader);
    if (reverse)
      amount = gnc_numeric_neg (amount);

    xaccSPrintAmount (string, amount, print_info);

    gnc_set_label_color (regData->shares_label, amount);
    gtk_label_set_text (GTK_LABEL (regData->shares_label), string);
  }

  if (regData->value_label != NULL)
  {
    GNCPrice *price;

    price = account_latest_price (leader);
    if (!price)
    {
      gnc_set_label_color (regData->value_label, gnc_numeric_zero ());
      gtk_label_set_text (GTK_LABEL (regData->value_label),
                          _("<No information>"));
    }
    else
    {
      gnc_commodity *currency = gnc_price_get_currency (price);

      print_info = gnc_commodity_print_info (currency, TRUE);

      amount = xaccAccountGetBalance (leader);
      if (reverse)
        amount = gnc_numeric_neg (amount);

      amount = gnc_numeric_mul (amount, gnc_price_get_value (price),
                                gnc_commodity_get_fraction (currency),
                                GNC_RND_ROUND);

      xaccSPrintAmount (string, amount, print_info);

      gnc_set_label_color (regData->value_label, amount);
      gtk_label_set_text (GTK_LABEL (regData->value_label), string);

      gnc_price_unref (price);
    }
  }

  gnc_reg_set_window_name (regData);

  {
    gboolean expand;
    gboolean sensitive;
    SplitRegister *reg;

    reg = gnc_ledger_display_get_split_register (regData->ledger);

    expand = gnc_split_register_current_trans_expanded (reg);

    gtk_signal_handler_block_by_data
      (GTK_OBJECT (regData->split_button), regData);
    gtk_toggle_button_set_active
      (GTK_TOGGLE_BUTTON (regData->split_button), expand);
    gtk_signal_handler_unblock_by_data
      (GTK_OBJECT (regData->split_button), regData);

    gtk_signal_handler_block_by_data
      (GTK_OBJECT (regData->split_menu_check), regData);
    gtk_check_menu_item_set_active
      (GTK_CHECK_MENU_ITEM (regData->split_menu_check), expand);
    gtk_signal_handler_unblock_by_data
      (GTK_OBJECT (regData->split_menu_check), regData);

    gtk_check_menu_item_set_active
      (GTK_CHECK_MENU_ITEM (regData->split_popup_check), expand);

    sensitive = reg->style == REG_STYLE_LEDGER;

    gtk_widget_set_sensitive (regData->split_button, sensitive);
    gtk_widget_set_sensitive (regData->split_menu_check, sensitive);
    gtk_widget_set_sensitive (regData->split_popup_check, sensitive);
  }
}

static void
gnc_register_redraw_help_cb (GnucashRegister *g_reg, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  const char *status;
  char *help;

  if (!regData)
    return;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  help = gnc_table_get_help (reg->table);

  status = help ? help : "";

  gnome_appbar_set_default (GNOME_APPBAR(regData->statusbar), status);

  g_free (help);
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


/********************************************************************\
 * regDestroy()
\********************************************************************/

static void
regDestroy (GNCLedgerDisplay *ledger)
{
  RegWindow *regData = gnc_ledger_display_get_user_data (ledger);

  if (regData)
  {
    SplitRegister *reg;

    gnc_reg_save_size (regData);

    reg = gnc_ledger_display_get_split_register (ledger);

    if (reg && reg->table)
      gnc_table_save_state (reg->table);

    gtk_widget_destroy (regData->window);
  }

  gnc_ledger_display_set_user_data (ledger, NULL);
}


void 
gnc_register_new_account_cb (GtkWidget * w, gpointer data)
{
  gnc_ui_new_account_window (NULL);
}


/********************************************************************\
 * gnc_register_cut_cb -- cut the selection to the clipboard        *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_cut_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_cut_clipboard (regData->reg);
}


/********************************************************************\
 * gnc_register_copy_cb -- copy the selection to the clipboard      *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_copy_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_copy_clipboard (regData->reg);
}


/********************************************************************\
 * gnc_register_paste_cb -- paste the clipboard to the selection    *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void 
gnc_register_paste_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_paste_clipboard (regData->reg);
}


/********************************************************************\
 * gnc_register_cut_trans_cb -- cut the current transaction         *
 *                              to the clipboard                    *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_cut_trans_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_split_register_cut_current
    (gnc_ledger_display_get_split_register (regData->ledger));
}


/********************************************************************\
 * gnc_register_copy_trans_cb -- copy the current transaction       *
 *                               to the clipboard                   *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_copy_trans_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_split_register_copy_current
    (gnc_ledger_display_get_split_register (regData->ledger));
}


/********************************************************************\
 * gnc_register_paste_trans_cb -- paste the transaction clipboard   *
 *                                to the selection                  *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_paste_trans_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_split_register_paste_current
    (gnc_ledger_display_get_split_register (regData->ledger));
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


/********************************************************************\
 * gnc_register_record_trans_cb                                     *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_record_trans_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_enter (regData, TRUE);
}


typedef enum
{
  DELETE_TRANS,
  DELETE_SPLITS,
  DELETE_CANCEL
} DeleteType;


static void
gnc_transaction_delete_toggle_cb(GtkToggleButton *button, gpointer data)
{
  GtkWidget *text = gtk_object_get_user_data(GTK_OBJECT(button));
  gchar *s = data;
  gint pos = 0;

  gtk_editable_delete_text(GTK_EDITABLE(text), 0, -1);
  gtk_editable_insert_text(GTK_EDITABLE(text), s, strlen(s), &pos);
}


static gboolean
trans_has_reconciled_splits (Transaction *trans)
{
  GList *node;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    switch (xaccSplitGetReconcile (split))
    {
      case YREC:
      case FREC:
        return TRUE;

      default:
        break;
    }
  }

  return FALSE;
}


/********************************************************************\
 * gnc_transaction_delete_query                                     *
 *   creates and displays a dialog which asks the user wheter they  *
 *   want to delete a whole transaction, or just a split.           *
 *   It returns a DeleteType code indicating the user's choice.     *
 *                                                                  *
 * Args: parent - the parent window the dialog should use           *
 * Returns: DeleteType choice indicator                             *
 \*******************************************************************/
static DeleteType
gnc_transaction_delete_query (GtkWindow *parent, Transaction *trans)
{
  GtkWidget *dialog;
  GtkWidget *trans_button;
  GtkWidget *splits_button;
  GtkWidget *text;
  GladeXML  *xml;
  gint       pos = 0;
  gint       result;
  gboolean   reconciled;

  const char *usual = _("This selection will delete the whole "
                        "transaction. This is what you usually want.");
  const char *usual_recn = _("This selection will delete the whole "
                             "transaction.\n\n"
                             "You would be deleting a transaction "
                             "with reconciled splits!");
  const char *warn  = _("Warning: Just deleting all the other splits will "
                        "make your account unbalanced. You probably "
                        "shouldn't do this unless you're going to "
                        "immediately add another split to bring the "
                        "transaction back into balance.");
  const char *warn_recn = _("You would be deleting reconciled splits!");
  const char *cbuf;
  char *buf;

  DeleteType return_value;

  reconciled = trans_has_reconciled_splits (trans);

  xml = gnc_glade_xml_new ("register.glade", "Delete Transaction");
  dialog = glade_xml_get_widget (xml, "Delete Transaction");
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), parent);

  text = glade_xml_get_widget (xml, "text");

  trans_button = glade_xml_get_widget (xml, "delete_all");
  gtk_object_set_user_data(GTK_OBJECT(trans_button), text);
  gtk_signal_connect(GTK_OBJECT(trans_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb),
                     (gpointer) (reconciled ? usual_recn : usual));

  splits_button = glade_xml_get_widget (xml, "delete_other");
  gtk_object_set_user_data(GTK_OBJECT(splits_button), text);

  if (reconciled)
    buf = g_strconcat (warn, "\n\n", warn_recn, NULL);
  else
    buf = g_strdup (warn);

  gtk_signal_connect(GTK_OBJECT(splits_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb),
                     (gpointer) buf);

  cbuf = reconciled ? usual_recn : usual;
  gtk_editable_insert_text(GTK_EDITABLE(text), cbuf, strlen(cbuf), &pos);
  gtk_text_set_line_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_word_wrap(GTK_TEXT(text), TRUE);

  result = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

  g_free (buf);

  if (result != 0)
    return_value = DELETE_CANCEL;
  else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(trans_button)))
    return_value = DELETE_TRANS;
  else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(splits_button)))
    return_value = DELETE_SPLITS;
  else
    return_value = DELETE_CANCEL;

  gtk_widget_destroy(dialog);

  return return_value;
}


/********************************************************************\
 * gnc_register_delete_trans_cb                                     *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_delete_trans_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegisterStyle style;
  CursorClass cursor_class;
  SplitRegister *reg;
  Transaction *trans;
  char *buf = NULL;
  Split *split;
  gint result;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  /* get the current split based on cursor position */
  split = gnc_split_register_get_current_split(reg);
  if (split == NULL)
  {
    gnc_split_register_cancel_cursor_split_changes (reg);
    return;
  }

  trans = xaccSplitGetParent(split);
  style = reg->style;
  cursor_class = gnc_split_register_get_current_cursor_class (reg);

  /* Deleting the blank split just cancels */
  {
    Split *blank_split = gnc_split_register_get_blank_split (reg);

    if (split == blank_split)
    {
      gnc_split_register_cancel_cursor_trans_changes (reg);
      return;
    }
  }

  if (cursor_class == CURSOR_CLASS_NONE)
    return;

  /* On a split cursor, just delete the one split. */
  if (cursor_class == CURSOR_CLASS_SPLIT)
  {
    const char *format = _("Are you sure you want to delete\n   %s\n"
                           "from the transaction\n   %s ?");
    const char *recn_warn = _("You would be deleting a reconciled split!");
    const char *memo;
    const char *desc;
    char recn;

    memo = xaccSplitGetMemo (split);
    memo = (memo && *memo) ? memo : _("(no memo)");

    desc = xaccTransGetDescription (trans);
    desc = (desc && *desc) ? desc : _("(no description)");

    /* ask for user confirmation before performing permanent damage */
    buf = g_strdup_printf (format, memo, desc);

    recn = xaccSplitGetReconcile (split);
    if (recn == YREC || recn == FREC)
    {
      char *new_buf;

      new_buf = g_strconcat (buf, "\n\n", recn_warn, NULL);
      g_free (buf);
      buf = new_buf;
    }

    result = gnc_verify_dialog_parented (regData->window, FALSE, buf);

    g_free(buf);

    if (!result)
      return;

    gnc_split_register_delete_current_split (reg);
    return;
  }

  g_return_if_fail(cursor_class == CURSOR_CLASS_TRANS);

  /* On a transaction cursor with 2 or fewer splits in single or double
   * mode, we just delete the whole transaction, kerblooie */
  if ((xaccTransCountSplits(trans) <= 2) && (style == REG_STYLE_LEDGER))
  {
    const char *message = _("Are you sure you want to delete the current "
                            "transaction?");
    const char *recn_warn = _("You would be deleting a transaction "
                              "with reconciled splits!");
    char *buf;

    if (trans_has_reconciled_splits (trans))
      buf = g_strconcat (message, "\n\n", recn_warn, NULL);
    else
      buf = g_strdup (message);

    result = gnc_verify_dialog_parented (regData->window, FALSE, buf);

    g_free (buf);

    if (!result)
      return;

    gnc_split_register_delete_current_trans (reg);
    return;
  }

  /* At this point we are on a transaction cursor with more than 2 splits
   * or we are on a transaction cursor in multi-line mode or an auto mode.
   * We give the user two choices: delete the whole transaction or delete
   * all the splits except the transaction split. */
  {
    DeleteType del_type;

    del_type = gnc_transaction_delete_query (GTK_WINDOW(regData->window),
                                             trans);

    if (del_type == DELETE_CANCEL)
      return;

    if (del_type == DELETE_TRANS)
    {
      gnc_split_register_delete_current_trans (reg);
      return;
    }

    if (del_type == DELETE_SPLITS)
    {
      gnc_split_register_emtpy_current_trans (reg);
      return;
    }
  }
}


/********************************************************************\
 * gnc_register_duplicate_trans_cb                                  *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_duplicate_trans_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_split_register_duplicate_current
    (gnc_ledger_display_get_split_register (regData->ledger));
}


/********************************************************************\
 * gnc_register_recur_cb                                            *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/

void
gnc_register_recur_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);
  Transaction *pending_trans = gnc_split_register_get_current_trans (reg);

  gnc_sx_create_from_trans(pending_trans);
}

  
/********************************************************************\
 * gnc_register_cancel_trans_cb                                     *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_cancel_trans_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_split_register_cancel_cursor_trans_changes
    (gnc_ledger_display_get_split_register (regData->ledger));
}


/********************************************************************\
 * gnc_register_check_close                                         *
 *                                                                  *
 * Args:   regData - the data struct for this register              *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_register_check_close(RegWindow *regData)
{
  gboolean pending_changes;
  SplitRegister *reg;

  reg = gnc_ledger_display_get_split_register (regData->ledger);

  pending_changes = gnc_split_register_changed (reg);
  if (pending_changes)
  {
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    if (gnc_verify_dialog_parented(regData->window, TRUE, message))
      gnc_register_record_trans_cb(regData->window, regData);
    else
      gnc_split_register_cancel_cursor_trans_changes (reg);
  }
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

  gnc_register_check_close (regData);

  gnc_ledger_display_close (regData->ledger);
}

static int
report_helper (RegWindow *regData, Query *query)
{
  SplitRegister *reg = gnc_ledger_display_get_split_register (regData->ledger);
  Account *account;
  char *str;
  SCM qtype;
  SCM args;
  SCM func;
  SCM arg;

  args = SCM_EOL;

  switch (reg->type) {
  case PAYABLE_REGISTER:
  case RECEIVABLE_REGISTER:
    g_return_val_if_fail (query == NULL, -1);

    if (reg->type == PAYABLE_REGISTER)
      func = gh_eval_str ("gnc:payables-report-create");
    else
      func = gh_eval_str ("gnc:receivables-report-create");
    g_return_val_if_fail (gh_procedure_p (func), -1);

    qtype = gh_eval_str("<gnc:Account*>");
    g_return_val_if_fail (qtype != SCM_UNDEFINED, -1);

    account = gnc_ledger_display_leader (regData->ledger);

    arg = gw_wcp_assimilate_ptr (account, qtype);
    args = gh_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);

    break;

  default:
    if (!query)
    {
      query = gnc_ledger_display_get_query (regData->ledger);
      g_return_val_if_fail (query != NULL, -1);
    }

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

    qtype = gh_eval_str("<gnc:Query*>");
    g_return_val_if_fail (qtype != SCM_UNDEFINED, -1);

    arg = gw_wcp_assimilate_ptr (query, qtype);
    args = gh_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);

    arg = gh_bool2scm (FALSE);
    args = gh_cons (arg, args);

    break;
  }

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

  id = report_helper (regData, NULL);
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

  id = report_helper (regData, query);
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

  id = report_helper (regData, NULL);
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
gnc_register_toolbar_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show(regData->toolbar_dock);
  } else {
    gtk_widget_hide(regData->toolbar_dock);
    gtk_widget_queue_resize(regData->toolbar_dock);
  }
}

void
gnc_register_summarybar_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show(regData->summarybar_dock);
  } else {
    gtk_widget_hide(regData->summarybar_dock);
    gtk_widget_queue_resize(regData->summarybar_dock);
  }
}

void
gnc_register_statusbar_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  GtkCheckMenuItem *checkmenu = GTK_CHECK_MENU_ITEM(widget);

  if (checkmenu->active) {
    gtk_widget_show(regData->statusbar);
  } else {
    gtk_widget_hide(regData->statusbar);
    gtk_widget_queue_resize(regData->statusbar);
  }
}

void
gnc_register_gl_cb(GtkWidget *widget, gpointer data)
{
  GNCLedgerDisplay *ld;
  RegWindow *regData;

  ld = gnc_ledger_display_gl ();

  regData = regWindowLedger (ld);

  gnc_register_raise (regData);
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

/********************************************************************\
 * gnc_register_help_cb                                             *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - not used                                        *
 * Return: none                                                     *
\********************************************************************/
void
gnc_register_help_cb (GtkWidget *widget, gpointer data)
{
  helpWindow (NULL, NULL, HH_REGWIN);
}

/************************** END OF FILE **************************/

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
#include <g-wrap-runtime-guile.h>
#include <time.h>

#include "AccWindow.h"
#include "EuroUtils.h"
#include "FileDialog.h"
#include "MainWindow.h"
#include "MultiLedger.h"
#include "RegWindow.h"
#include "Scrub.h"
#include "dialog-find-transactions.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-dateedit.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnucash-sheet.h"
#include "messages.h"
#include "query-user.h"
#include "table-allgui.h"
#include "window-help.h"
#include "window-reconcile.h"
#include "window-register.h"


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
  xaccLedgerDisplay * ledger;   

  /* Top level window */
  GtkWidget * window;

  gint width;

  GtkWidget * toolbar;
  SCM toolbar_change_callback_id;

  GtkWidget * statusbar;

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


/** PROTOTYPES ******************************************************/
static void gnc_register_redraw_all_cb (GnucashRegister *g_reg, gpointer data);
static void gnc_reg_refresh_toolbar(RegWindow *regData);
static void regDestroy(xaccLedgerDisplay *ledger);
static void regSetHelp(xaccLedgerDisplay *ledger, const char *help_str);
static void gnc_register_check_close(RegWindow *regData);
static void cutCB(GtkWidget *w, gpointer data);
static void copyCB(GtkWidget *w, gpointer data);
static void pasteCB(GtkWidget *w, gpointer data);
static void cutTransCB(GtkWidget *w, gpointer data);
static void copyTransCB(GtkWidget *w, gpointer data);
static void pasteTransCB(GtkWidget *w, gpointer data);
static void startRecnCB(GtkWidget *w, gpointer data);
static void xferCB(GtkWidget *w, gpointer data);
static void stockSplitCB (GtkWidget * w, gpointer data);
static void editCB(GtkWidget *w, gpointer data);
static void helpCB(GtkWidget *w, gpointer data);
static void newAccountCB(GtkWidget * w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void duplicateCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);
static void closeCB(GtkWidget *w, gpointer data);
static void reportCB(GtkWidget *w, gpointer data);
static void invoiceCB(GtkWidget *w, gpointer data);
static void invoiceTransCB(GtkWidget *w, gpointer data);
static void printReportCB(GtkWidget *w, gpointer data);
static void dateCB(GtkWidget *w, gpointer data);
static void expand_trans_cb(GtkWidget *widget, gpointer data);
static void new_trans_cb(GtkWidget *widget, gpointer data);
static void jump_cb(GtkWidget *widget, gpointer data);
static void print_check_cb(GtkWidget * widget, gpointer data);
static void gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data);

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
  xaccLedgerDisplay * ledger = xaccLedgerDisplaySimple (account);

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
  xaccLedgerDisplay * ledger = xaccLedgerDisplayAccGroup (account);

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

  gtk_widget_show (regData->window);

  if (regData->window->window == NULL)
    return;

  gdk_window_raise (regData->window->window);
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

  trans = xaccSplitGetParent(split);
  if (trans != NULL)
    if (gnc_register_include_date(regData, xaccTransGetDate(trans)))
    {
      xaccLedgerDisplayRefresh(regData->ledger);
    }

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  if (xaccSRGetSplitVirtLoc(reg, split, &vcell_loc))
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

  trans = xaccSplitGetParent(split);
  if (trans != NULL)
    if (gnc_register_include_date(regData, xaccTransGetDate(trans)))
    {
      xaccLedgerDisplayRefresh (regData->ledger);
    }

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  if (xaccSRGetSplitAmountVirtLoc(reg, split, &virt_loc))
    gnucash_register_goto_virt_loc(regData->reg, virt_loc);
}


static void
gnc_register_change_style (RegWindow *regData, SplitRegisterStyle style)
{
  SplitRegister *reg = xaccLedgerDisplayGetSR (regData->ledger);

  if (style == reg->style)
    return;

  xaccConfigSplitRegister (reg, reg->type, style, reg->use_double_line);

  xaccLedgerDisplayRefresh (regData->ledger);
}

static void
gnc_register_style_ledger_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_LEDGER);
}

static void
gnc_register_style_auto_ledger_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_AUTO_LEDGER);
}

static void
gnc_register_style_journal_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  if (!GTK_CHECK_MENU_ITEM (w)->active)
    return;

  gnc_register_change_style (regData, REG_STYLE_JOURNAL);
}

static void
gnc_register_double_line_cb (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg = xaccLedgerDisplayGetSR (regData->ledger);
  gboolean use_double_line;

  use_double_line = GTK_CHECK_MENU_ITEM(w)->active;

  if (use_double_line == reg->use_double_line)
    return;

  xaccConfigSplitRegister (reg, reg->type, reg->style, use_double_line);

  xaccLedgerDisplayRefresh (regData->ledger);
}

static void
gnc_register_sort (RegWindow *regData, sort_type_t sort_code)
{
  Query *query = xaccLedgerDisplayGetQuery (regData->ledger);
  gboolean show_present_divider = FALSE;
  SplitRegister *reg;

  if (regData->sort_type == sort_code)
    return;

  switch (sort_code)
  {
    case BY_STANDARD:
      xaccQuerySetSortOrder(query, BY_STANDARD, BY_NONE, BY_NONE);
      show_present_divider = TRUE;
      break;
    case BY_DATE:
      xaccQuerySetSortOrder(query, BY_DATE, BY_STANDARD, BY_NONE);
      show_present_divider = TRUE;
      break;
    case BY_DATE_ENTERED:
      xaccQuerySetSortOrder(query, BY_DATE_ENTERED, BY_STANDARD, BY_NONE);
      break;
    case BY_DATE_RECONCILED:
      xaccQuerySetSortOrder(query, BY_RECONCILE, BY_DATE_RECONCILED,
                            BY_STANDARD);
      break;
    case BY_NUM:
      xaccQuerySetSortOrder(query, BY_NUM, BY_STANDARD, BY_NONE);
      break;
    case BY_AMOUNT:
      xaccQuerySetSortOrder(query, BY_AMOUNT, BY_STANDARD, BY_NONE);
      break;
    case BY_MEMO:
      xaccQuerySetSortOrder(query, BY_MEMO, BY_STANDARD, BY_NONE);
      break;
    case BY_DESC:
      xaccQuerySetSortOrder(query, BY_DESC, BY_STANDARD, BY_NONE);
      break;
    default:
      assert(0); /* we should never be here */
  }

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  xaccSRShowPresentDivider (reg, show_present_divider);

  regData->sort_type = sort_code;

  xaccLedgerDisplayRefresh(regData->ledger);
}

static void
gnc_register_sort_standard_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_STANDARD);
}

static void
gnc_register_sort_date_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE);
}

static void
gnc_register_sort_date_entered_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE_ENTERED);
}

static void
gnc_register_sort_date_reconciled_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_DATE_RECONCILED);
}

static void
gnc_register_sort_num_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_NUM);
}

static void
gnc_register_sort_amount_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_AMOUNT);
}

static void
gnc_register_sort_memo_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_sort(regData, BY_MEMO);
}

static void
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

  query = xaccLedgerDisplayGetQuery (regData->ledger);
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

  query = xaccLedgerDisplayGetQuery (regData->ledger);
  if (!query)
    return;

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  gtk_widget_set_sensitive(regDateData->set_button, FALSE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);

  xaccQueryPurgeTerms (query, PD_DATE);

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

static void
gnc_register_date_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_set_date_range(regData);

  xaccLedgerDisplayRefresh (regData->ledger);
}

static void
show_all_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  RegDateWindow *regDateData;
  GtkToggleButton *toggle;

  assert(regData != NULL);

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);
  gtk_toggle_button_set_active(toggle, TRUE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_latest);
  gtk_toggle_button_set_active(toggle, TRUE);

  gnc_register_date_cb(widget, data);
}

static void
gnc_register_today_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  RegDateWindow *regDateData;

  assert(regData != NULL);

  regDateData = regData->date_window;
  gnc_date_edit_set_time(GNC_DATE_EDIT(regDateData->end_date), time(NULL));

  gtk_widget_set_sensitive(regData->date_window->set_button, TRUE);
}

static void
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

  gtk_widget_show_all(regDateData->window);
  gdk_window_raise(GTK_WIDGET(regDateData->window)->window);
}

static RegDateWindow *
gnc_register_date_window (RegWindow *regData, gboolean show_all)
{
  RegDateWindow *regDateData;
  GtkWidget *dialog;
  GtkWidget *frame;
  GtkWidget *dvbox;

  regDateData = g_new0(RegDateWindow, 1);
  regData->date_window = regDateData;

  dialog = gnome_dialog_new(_("Register date ranges"),
                            GNOME_STOCK_BUTTON_CLOSE,
                            NULL);

  regDateData->window = dialog;
  dvbox = GNOME_DIALOG(dialog)->vbox;

  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_set_close(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), GTK_WINDOW(regData->window));

  frame = gtk_frame_new(NULL);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 5);
  gtk_box_pack_start(GTK_BOX(dvbox), frame, FALSE, FALSE, 0);

  {
    GtkWidget *calendar;
    GtkWidget *button;
    GtkWidget *entry;
    GtkWidget *radio;
    GtkWidget *date;
    GtkWidget *vbox2;
    GtkWidget *vbox;
    GtkWidget *hbox;
    GtkWidget *line;
    time_t time_val;
    GSList *group;

    vbox = gtk_vbox_new(FALSE, 2);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    /* Starting Date */
    vbox2 = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(vbox), vbox2, FALSE, FALSE, 0);

    radio = gtk_radio_button_new_with_label(NULL, _("Show Earliest"));
    gtk_box_pack_start(GTK_BOX(vbox2), radio, FALSE, FALSE, 0);
    regDateData->show_earliest = radio;

    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);

    group = gtk_radio_button_group(GTK_RADIO_BUTTON(radio));
    radio = gtk_radio_button_new_with_label(group, _("Start date:"));
    gtk_box_pack_start(GTK_BOX(hbox), radio, FALSE, FALSE, 0);

    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    gtk_signal_connect(GTK_OBJECT(radio), "toggled",
		       GTK_SIGNAL_FUNC(gnc_register_date_toggle_cb), regData);

    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);
    regDateData->start_date = date;

    time_val = xaccQueryGetEarliestDateFound
      (xaccLedgerDisplayGetQuery (regData->ledger));
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

    /* Separator line */
    line = gtk_hseparator_new();
    gtk_box_pack_start(GTK_BOX(vbox), line, FALSE, FALSE, 5);

    /* Ending Date */
    vbox2 = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(vbox), vbox2, FALSE, FALSE, 0);

    radio = gtk_radio_button_new_with_label(NULL, _("Show Latest"));
    gtk_box_pack_start(GTK_BOX(vbox2), radio, FALSE, FALSE, 0);
    regDateData->show_latest = radio;

    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);

    group = gtk_radio_button_group(GTK_RADIO_BUTTON(radio));
    radio = gtk_radio_button_new_with_label(group, _("End date:"));
    gtk_box_pack_start(GTK_BOX(hbox), radio, FALSE, FALSE, 0);

    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    gtk_signal_connect(GTK_OBJECT(radio), "toggled",
		       GTK_SIGNAL_FUNC(gnc_register_date_toggle_cb), regData);

    date = gnc_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);
    regDateData->end_date = date;

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

    button = gtk_button_new_with_label(_("Today"));
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_register_today_cb), regData);
    regDateData->today_button = button;

    button = gtk_button_new_with_label(_("Set Date Range"));
    gtk_box_pack_start(GTK_BOX(dvbox), button, FALSE, FALSE, 5);
    gtk_widget_set_sensitive(button, FALSE);
    regDateData->set_button = button;

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
  }

  return regDateData;
}

static GtkWidget *
gnc_register_create_tool_bar (RegWindow *regData)
{
  GtkWidget *toolbar;

  GnomeUIInfo toolbar_info[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("Enter"),
      N_("Record the current transaction"),
      recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Cancel"),
      N_("Cancel the current transaction"),
      cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDELETE,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Delete"),
      N_("Delete the current transaction"),
      deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Duplicate"),
      N_("Make a copy of the current transaction"),
      duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_COPY,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_TOGGLEITEM,
      N_("Split"),
      N_("Show all splits in the current transaction"),
      expand_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_BOOK_OPEN,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Blank"),
      N_("Move to the blank transaction at the " \
         "bottom of the register"),
      new_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Jump"),
      N_("Jump to the corresponding transaction in "
         "the other account"),
      jump_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Transfer"),
      N_("Transfer funds from one account to another"),
      xferCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Find"),
      N_("Find transactions with a search"),
      gnc_ui_find_transactions_cb, 
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SEARCH,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Report"),
      N_("Open a report window for this register"),
      reportCB, 
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_BOOK_GREEN,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Print"),
      N_("Print a report for this register"),
      printReportCB,
      NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PRINT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Close"),
      N_("Close this register window"),
      closeCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  toolbar = gtk_toolbar_new (GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

  gnome_app_fill_toolbar_with_data (GTK_TOOLBAR(toolbar), toolbar_info,
                                    NULL, regData);

  regData->toolbar = toolbar;

  regData->split_button = toolbar_info[6].widget;

  return toolbar;
}

static void
gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data)
{
  RegWindow * regData = data;
  SplitRegister *reg;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

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
  GtkWidget *hbox;
  GtkWidget *label;

  regData->cleared_label    = NULL;
  regData->balance_label    = NULL;
  regData->reconciled_label = NULL;
  regData->future_label     = NULL;
  regData->shares_label     = NULL;
  regData->value_label      = NULL;

  if (xaccLedgerDisplayType (regData->ledger) >= LD_SUBACCOUNT)
    return NULL;

  {
    Account *account;
    GNCAccountType atype;

    account = xaccLedgerDisplayLeader (regData->ledger);
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

static GtkWidget *
gnc_register_create_status_bar (RegWindow *regData)
{
  GtkWidget *statusbar;

  statusbar = gnome_appbar_new (FALSE, /* no progress bar */
                                TRUE,  /* has status area */
                                GNOME_PREFERENCES_USER);

  regData->statusbar = statusbar;

  return statusbar;
}


void
gnc_register_jump_to_blank (RegWindow *regData)
{
  SplitRegister *reg = xaccLedgerDisplayGetSR (regData->ledger);
  VirtualCellLocation vcell_loc;
  Split *blank;

  blank = xaccSRGetBlankSplit (reg);
  if (blank == NULL)
    return;

  if (xaccSRGetSplitVirtLoc (reg, blank, &vcell_loc))
    gnucash_register_goto_virt_cell (regData->reg, vcell_loc);
}


static void
expand_trans_check_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  gboolean expand;
  SplitRegister *reg;

  if (!regData)
    return;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  expand = GTK_CHECK_MENU_ITEM (widget)->active;

  xaccSRExpandCurrentTrans (reg, expand);
}

static void
expand_trans_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  gboolean expand;
  SplitRegister *reg;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  expand = GTK_TOGGLE_BUTTON (widget)->active;

  xaccSRExpandCurrentTrans (reg, expand);
}

static void
new_trans_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  if (xaccSRSaveRegEntry (reg, TRUE))
    xaccSRRedrawReg (reg);

  gnc_register_jump_to_blank (regData);
}

static void
jump_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Account *account;
  Account *leader;
  Split *split;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  split = xaccSRGetCurrentSplit (reg);
  if (split == NULL)
    return;

  account = xaccSplitGetAccount(split);
  if (account == NULL)
    return;

  leader = xaccLedgerDisplayLeader (regData->ledger);

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

static void
print_check_cb(GtkWidget * widget, gpointer data)
{
  RegWindow     * reg_data = data;
  SplitRegister * reg      = xaccLedgerDisplayGetSR (reg_data->ledger);
  Split         * split    = xaccSRGetCurrentSplit(reg);
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
    amount = xaccSplitGetValue(split);
    amount = gnc_numeric_abs (amount);
    date   = xaccTransGetDate(trans);
    memo   = xaccSplitGetMemo(split);

    gh_apply(print_check,
             SCM_LIST4(gh_str02scm(payee),
                       gh_double2scm(gnc_numeric_to_double (amount)),
                       gh_ulong2scm(date),
                       gh_str02scm(memo)));
  }
}


static void
gnc_register_scrub_all_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  Query *query = xaccLedgerDisplayGetQuery (regData->ledger);
  AccountGroup *root;
  GList *node;

  if (query == NULL)
    return;

  gnc_suspend_gui_refresh ();

  root = gncGetCurrentGroup ();

  for (node = xaccQueryGetSplits (query); node; node = node->next)
  {
    Split *split = node->data;
    Transaction *trans = xaccSplitGetParent (split);

    xaccTransScrubOrphans (trans, root);
    xaccTransScrubImbalance (trans, root, NULL);
  }

  gnc_resume_gui_refresh ();
}

static void
gnc_register_scrub_current_cb (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Transaction *trans;
  AccountGroup *root;

  reg = xaccLedgerDisplayGetSR (regData->ledger);
  trans = xaccSRGetCurrentTrans (reg);

  if (!trans)
    return;

  gnc_suspend_gui_refresh ();

  root = gncGetCurrentGroup ();

  xaccTransScrubOrphans (trans, root);
  xaccTransScrubImbalance (trans, root, NULL);

  gnc_resume_gui_refresh ();
}

static GtkWidget *
gnc_register_create_menu_bar(RegWindow *regData, GtkWidget *statusbar)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group;

  static GnomeUIInfo style_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(N_("Basic Ledger"),
                               N_("Show transactions on one or two lines"),
                               gnc_register_style_ledger_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Auto-Split Ledger"),
                               N_("Show transactions on one or two lines and "
                                  "expand the current transaction"),
                               gnc_register_style_auto_ledger_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Transaction Journal"),
                               N_("Show expanded transactions with all "
                                  "splits"),
                               gnc_register_style_journal_cb, NULL, NULL),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo style_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(style_list),
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_TOGGLEITEM(N_("_Double Line"),
                           N_("Show two lines of information for each "
                              "transaction"),
                           gnc_register_double_line_cb, NULL),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(N_("Standard order"),
                               N_("Keep normal account order"),
                               gnc_register_sort_standard_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Date"),
                               N_("Sort by Date"),
                               gnc_register_sort_date_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by date of entry"),
                               N_("Sort by the date of entry"),
                               gnc_register_sort_date_entered_cb,
                               NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by statement date"),
                               N_("Sort by the statement date "
                                  "(unreconciled items last)"),
                               gnc_register_sort_date_reconciled_cb,
                               NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Num"),
                               N_("Sort by Num"),
                               gnc_register_sort_num_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Amount"),
                               N_("Sort by Amount"),
                               gnc_register_sort_amount_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Memo"),
                               N_("Sort by Memo"),
                               gnc_register_sort_memo_cb, NULL, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(N_("Sort by Description"),
                               N_("Sort by Description"),
                               gnc_register_sort_desc_cb, NULL, NULL),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo sort_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(sort_list),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo date_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("Show _All"),
      N_("Show all of the transactions in the account"),
      show_all_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Set _Range..."),
      N_("Set the date range of this register"),
      dateCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo register_menu[] =
  {
    GNOMEUIINFO_SUBTREE(N_("_Style"), style_menu),
    GNOMEUIINFO_SUBTREE(N_("Sort _Order"), sort_menu),
    GNOMEUIINFO_SUBTREE(N_("_Date Range"), date_menu),
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Report"),
      N_("Open a report window for this register"),
      reportCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Invoice"),
      N_("Open an invoice report window for this register"),
      invoiceCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Print"),
      N_("Print a report for this register"),
      printReportCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Close"),
      N_("Close this register window"),
      closeCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo edit_menu[] =
  {
    GNOMEUIINFO_MENU_CUT_ITEM(cutCB, NULL),
    GNOMEUIINFO_MENU_COPY_ITEM(copyCB, NULL),
    GNOMEUIINFO_MENU_PASTE_ITEM(pasteCB, NULL),
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Cut Transaction"),
      N_("Cut the selected transaction"),
      cutTransCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Copy Transaction"),
      N_("Copy the selected transaction"),
      copyTransCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Paste Transaction"),
      N_("Paste the transaction clipboard"),
      pasteTransCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo account_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Edit Account"),
      N_("Edit the main account for this register"),
      editCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Reconcile..."),
      N_("Reconcile the main account for this register"),
      startRecnCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Transfer..."),
      N_("Transfer funds from one account to another"),
      xferCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Stock S_plit..."),
      N_("Record a stock split or a stock merger"),
      stockSplitCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_New Account..."),
      N_("Create a new account"),
      newAccountCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Enter"),
      N_("Record the current transaction"),
      recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Cancel"),
      N_("Cancel the current transaction"),
      cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete"),
      N_("Delete the current transaction"),
      deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("D_uplicate"),
      N_("Make a copy of the current transaction"),
      duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_TOGGLEITEM,
      N_("_Split"),
      N_("Show all splits in the current transaction"),
      expand_trans_check_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Blank"),
      N_("Move to the blank transaction at the "
         "bottom of the register"),
      new_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Jump"),
      N_("Jump to the corresponding transaction in "
         "the other account"),
      jump_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Scrub All"),
      N_("Identify and fix problems in the "
         "transactions displayed in this register"),
      gnc_register_scrub_all_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Scrub Current"),
      N_("Identify and fix problems in the "
         "current transaction"),
      gnc_register_scrub_current_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Invoice"),
      N_("Open an invoice report window for this transaction"),
      invoiceTransCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Print Check... (unfinished!)"),
      N_("Print a check using a standard format"),
      print_check_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo help_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Help"),
      N_("Open the GnuCash help window"),
      helpCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo register_window_menu[] =
  {
    GNOMEUIINFO_SUBTREE(N_("_Register"), register_menu),
    GNOMEUIINFO_MENU_EDIT_TREE(edit_menu),
    GNOMEUIINFO_SUBTREE(N_("_Account"), account_menu),
    GNOMEUIINFO_SUBTREE(N_("_Transaction"), transaction_menu),
    GNOMEUIINFO_MENU_HELP_TREE(help_menu),
    GNOMEUIINFO_END
  };

  gnc_fill_menu_with_data(register_window_menu, regData);

  menubar = gtk_menu_bar_new();

  accel_group = gtk_accel_group_new();
  gtk_accel_group_attach(accel_group, GTK_OBJECT(regData->window));

  gnome_app_fill_menu(GTK_MENU_SHELL(menubar), register_window_menu,
  		      accel_group, TRUE, 0);

  gnome_app_install_appbar_menu_hints(GNOME_APPBAR(statusbar),
                                      register_window_menu);

  regData->split_menu_check = transaction_menu[6].widget;

  /* Make sure the right style radio item is active */
  {
    SplitRegister *reg;
    GtkWidget *widget;
    int index;

    reg = xaccLedgerDisplayGetSR (regData->ledger);

    switch (reg->style)
    {
      default:
      case REG_STYLE_LEDGER:
        index = 0;
        break;
      case REG_STYLE_AUTO_LEDGER:
        index = 1;
        break;
      case REG_STYLE_JOURNAL:
        index = 2;
        break;
    }

    /* registers with more than one account can only use journal mode */
    if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
    {
      widget = style_list[0].widget;
      gtk_widget_set_sensitive (widget, FALSE);

      widget = style_list[1].widget;
      gtk_widget_set_sensitive (widget, FALSE);
    }

    widget = style_list[index].widget;

    gtk_signal_handler_block_by_data(GTK_OBJECT(widget), regData);

    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);

    gtk_signal_handler_unblock_by_data(GTK_OBJECT(widget), regData);
  }

  return menubar;
}


static GtkWidget *
gnc_register_create_popup_menu (RegWindow *regData)
{
  GtkWidget *popup;

  GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Enter"),
      N_("Record the current transaction"),
      recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Cancel"),
      N_("Cancel the current transaction"),
      cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete"),
      N_("Delete the current transaction"),
      deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("D_uplicate"),
      N_("Make a copy of the current transaction"),
      duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_TOGGLEITEM,
      N_("_Split"),
      N_("Show all splits in the current transaction"),
      expand_trans_check_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Blank"),
      N_("Move to the blank transaction at the "
         "bottom of the register"),
      new_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Jump"),
      N_("Jump to the corresponding transaction in "
         "the other account"),
      jump_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  gnc_fill_menu_with_data (transaction_menu, regData);

  popup = gnome_popup_menu_new (transaction_menu);

  regData->split_popup_check = transaction_menu[6].widget;

  return popup;
}

static void
gnc_register_record (RegWindow *regData)
{
  SplitRegister *reg;
  Transaction *trans;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  trans = xaccSRGetCurrentTrans (reg);

  if (!xaccSRSaveRegEntry (reg, TRUE))
    return;

  if (trans != NULL)
    gnc_register_include_date (regData, xaccTransGetDate(trans));

  xaccSRRedrawReg (reg);
}

static void
gnc_register_enter (RegWindow *regData, gboolean next_transaction)
{
  SplitRegister *sr = xaccLedgerDisplayGetSR (regData->ledger);
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

      blank_split = xaccSRGetBlankSplit(sr);
      if (blank_split != NULL)
      {
        Split *current_split;

        current_split = xaccSRGetCurrentSplit(sr);

        if (blank_split == current_split)
          goto_blank = TRUE;
      }
    }
  }

  /* First record the transaction. This will perform a refresh. */
  gnc_register_record (regData);

  if (!goto_blank && next_transaction)
    xaccSRExpandCurrentTrans (sr, FALSE);

  /* Now move. */
  if (goto_blank)
    gnc_register_jump_to_blank (regData);
  else
    gnucash_register_goto_next_virt_row (regData->reg);
}

static void
gnc_register_record_cb (GnucashRegister *reg, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_enter (regData, FALSE);
}

static gboolean
gnc_register_delete_cb(GtkWidget *widget, GdkEvent *event, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_check_close (regData);

  xaccLedgerDisplayClose (regData->ledger);

  return TRUE; /* don't close */
}

static void
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
gnc_register_get_parent(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = xaccLedgerDisplayGetUserData (ledger);

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

  reg = xaccLedgerDisplayGetSR (regData->ledger);

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

  leader = xaccLedgerDisplayLeader (regData->ledger);

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
  SplitRegister *reg;
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

static void
size_allocate (GtkWidget *widget,
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
regWindowLedger (xaccLedgerDisplay *ledger)
{
  SplitRegister *reg;
  RegWindow *regData;
  GtkWidget *vbox;
  GtkWidget *register_window;
  GtkWidget *register_dock;
  GtkWidget *table_frame;
  GtkWidget *statusbar;
  gboolean show_all;
  gboolean has_date;

  reg = xaccLedgerDisplayGetSR (ledger);

  regData = xaccLedgerDisplayGetUserData (ledger);
  if (regData != NULL)
    return regData;

  regData = g_new (RegWindow, 1);

  xaccLedgerDisplaySetUserData (ledger, regData);

  xaccLedgerDisplaySetHandlers (ledger,
                                regDestroy,
                                gnc_register_get_parent,
                                regSetHelp);

  register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(register_window), vbox);

  register_dock = gnome_dock_new();
  gtk_box_pack_start(GTK_BOX(vbox), register_dock, TRUE, TRUE, 0);

  regData->ledger = ledger;
  regData->window = register_window;
  regData->sort_type = BY_STANDARD;
  regData->width = -1;

  gnc_reg_set_window_name(regData);

  /* Invoked when window is being destroyed. */
  gtk_signal_connect (GTK_OBJECT(regData->window), "destroy",
                      GTK_SIGNAL_FUNC (gnc_register_destroy_cb), regData);

  gtk_signal_connect (GTK_OBJECT(regData->window), "delete-event",
                      GTK_SIGNAL_FUNC (gnc_register_delete_cb), regData);

  gtk_signal_connect (GTK_OBJECT(regData->window), "size_allocate",
                      GTK_SIGNAL_FUNC (size_allocate), regData);

  show_all = gnc_lookup_boolean_option ("Register",
                                        "Show All Transactions",
                                        TRUE);

  {
    Query *q = xaccLedgerDisplayGetQuery (regData->ledger);

    has_date = xaccQueryHasTermType (q, PD_DATE);
  }

  if (has_date)
    show_all = FALSE;

  regData->date_window = gnc_register_date_window (regData, show_all);

  if (reg->type != SEARCH_LEDGER && !has_date)
    gnc_register_set_date_range (regData);

  /* Now that we have a date range, remove any existing
   * maximum on the number of splits returned. */
  xaccQuerySetMaxSplits (xaccLedgerDisplayGetQuery (regData->ledger), -1);

  statusbar = gnc_register_create_status_bar(regData);
  gtk_box_pack_start(GTK_BOX(vbox), statusbar, FALSE, FALSE, 0);

  /* The menu bar */
  {
    GtkWidget *dock_item;
    GtkWidget *menubar;

    dock_item = gnome_dock_item_new("menu", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    menubar = gnc_register_create_menu_bar(regData, statusbar);
    gtk_container_set_border_width(GTK_CONTAINER(menubar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), menubar);

    gnome_dock_add_item (GNOME_DOCK(register_dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 0, 0, 0, TRUE);
  }

  /* The tool bar */
  {
    GtkWidget *dock_item;
    GtkWidget *toolbar;
    SCM id;

    dock_item = gnome_dock_item_new("toolbar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = gnc_register_create_tool_bar(regData);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);

    id = gnc_register_option_change_callback(gnc_toolbar_change_cb, regData,
                                             "General", "Toolbar Buttons");
    regData->toolbar_change_callback_id = id;

    gnome_dock_add_item (GNOME_DOCK(register_dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 1, 0, 0, TRUE);
  }

  /* The summary bar */
  {
    GtkWidget *summarybar;

    summarybar = gnc_register_create_summary_bar (regData);
    if (summarybar)
    {
      GtkWidget *dock_item;

      dock_item = gnome_dock_item_new ("summarybar",
                                       GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

      gtk_container_set_border_width (GTK_CONTAINER (summarybar), 2);
      gtk_container_add (GTK_CONTAINER (dock_item), summarybar);

      gnome_dock_add_item (GNOME_DOCK (register_dock),
                           GNOME_DOCK_ITEM (dock_item),
                           GNOME_DOCK_TOP, 2, 0, 0, TRUE);
    }
  }

  /* The CreateTable will do the actual gui init, returning a widget */
  {
    GtkWidget *register_widget;
    GtkWidget *popup;
    guint num_rows;

    table_frame = gtk_frame_new (NULL);
    gnome_dock_set_client_area (GNOME_DOCK(register_dock), table_frame);

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

    popup = gnc_register_create_popup_menu (regData);
    gnucash_register_attach_popup (GNUCASH_REGISTER(register_widget),
                                   popup, regData);
  }

  {
    gboolean use_double_line;

    use_double_line = gnc_lookup_boolean_option ("Register",
                                                 "Double Line Mode",
                                                 FALSE);

    /* be sure to initialize the gui elements associated with the cursor */
    xaccConfigSplitRegister (reg, reg->type, reg->style, use_double_line);
  }

  /* Allow grow, allow shrink, auto-shrink */
  gtk_window_set_policy (GTK_WINDOW(register_window), TRUE, TRUE, FALSE);

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

  xaccSRShowPresentDivider (reg, TRUE);

  xaccLedgerDisplayRefresh (ledger);
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
  gnc_commodity *security;
  gnc_commodity *currency;

  security = xaccAccountGetSecurity (account);
  currency = xaccAccountGetCurrency (account);

  book = gncGetCurrentBook ();
  pdb = gnc_book_get_pricedb (book);

  return gnc_pricedb_lookup_latest (pdb, security, currency);
}

static void
gnc_register_redraw_all_cb (GnucashRegister *g_reg, gpointer data)
{
  RegWindow *regData = data;
  gnc_commodity * currency;
  GNCPrintAmountInfo print_info;
  gnc_numeric amount;
  Account *leader;
  char string[256];
  gboolean reverse;
  gboolean euro;

  if (regData->window == NULL)
    return;

  leader = xaccLedgerDisplayLeader (regData->ledger);

  euro = gnc_lookup_boolean_option ("International",
                                    "Enable EURO support",
                                    FALSE);

  currency = xaccAccountGetCurrency (leader);

  /* no EURO converson, if account is already EURO or no EURO currency */
  if (currency != NULL)
    euro = (euro && gnc_is_euro_currency(currency));
  else
    euro = FALSE;

  print_info = gnc_account_value_print_info (leader, TRUE);

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
                        gnc_convert_to_euro (currency, amount),
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
                        gnc_convert_to_euro (currency, amount),
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
                        gnc_convert_to_euro(currency, amount),
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
                        gnc_convert_to_euro(currency, amount),
                        gnc_commodity_print_info (gnc_get_euro (), TRUE));
    }

    gnc_set_label_color (regData->future_label, amount);
    gtk_label_set_text (GTK_LABEL (regData->future_label), string);
  }

  if (regData->shares_label != NULL)
  {
    print_info = gnc_account_quantity_print_info (leader, TRUE);

    amount = xaccAccountGetShareBalance (leader);
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

      amount = xaccAccountGetShareBalance (leader);
      if (reverse)
        amount = gnc_numeric_neg (amount);

      amount = gnc_numeric_mul (amount, gnc_price_get_value (price),
                                gnc_commodity_get_fraction (currency),
                                GNC_RND_ROUND);

      xaccSPrintAmount (string, amount, print_info);

      gnc_set_label_color (regData->value_label, amount);
      gtk_label_set_text (GTK_LABEL (regData->value_label), string);
    }
  }

  gnc_reg_set_window_name (regData);

  {
    gboolean expand;
    gboolean sensitive;
    SplitRegister *reg;

    reg = xaccLedgerDisplayGetSR (regData->ledger);

    expand = xaccSRCurrentTransExpanded (reg);

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
gnc_reg_save_size (RegWindow *regData)
{
  SplitRegister *reg;
  int *width;
  char *prefix;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

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
regDestroy (xaccLedgerDisplay *ledger)
{
  RegWindow *regData = xaccLedgerDisplayGetUserData (ledger);

  if (regData)
  {
    SplitRegister *reg;

    gnc_reg_save_size (regData);

    reg = xaccLedgerDisplayGetSR (ledger);

    if (reg && reg->table)
      gnc_table_save_state (reg->table);

    gtk_widget_destroy (regData->window);
  }

  xaccLedgerDisplaySetUserData (ledger, NULL);
}


static void
regSetHelp (xaccLedgerDisplay *ledger, const char *help_str)
{
  RegWindow *regData = xaccLedgerDisplayGetUserData (ledger);
  const gchar *status;

  if (!regData)
    return;

  if (help_str != NULL)
    status = help_str;
  else
    status = "";

  gnome_appbar_set_default (GNOME_APPBAR(regData->statusbar), status);
}


static void 
newAccountCB (GtkWidget * w, gpointer data)
{
  gnc_ui_new_account_window (NULL);
}


/********************************************************************\
 * cutCB -- cut the selection to the clipboard                      *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
cutCB (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_cut_clipboard (regData->reg);
}


/********************************************************************\
 * copyCB -- copy the selection to the clipboard                    *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
copyCB (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_copy_clipboard (regData->reg);
}


/********************************************************************\
 * pasteCB -- paste the clipboard to the selection                  *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
pasteCB (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_paste_clipboard (regData->reg);
}


/********************************************************************\
 * cutTransCB -- cut the current transaction to the clipboard       *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void
cutTransCB (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  xaccSRCutCurrent (xaccLedgerDisplayGetSR (regData->ledger));
}


/********************************************************************\
 * copyTransCB -- copy the current transaction to the clipboard     *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void
copyTransCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  xaccSRCopyCurrent (xaccLedgerDisplayGetSR (regData->ledger));
}


/********************************************************************\
 * pasteTransCB -- paste the transaction clipboard to the selection *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void
pasteTransCB (GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  xaccSRPasteCurrent (xaccLedgerDisplayGetSR (regData->ledger));
}


/********************************************************************\
 * xferCB -- open up the transfer window                            *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void 
xferCB (GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;

  gnc_xfer_dialog (regData->window,
                   xaccLedgerDisplayLeader (regData->ledger));
}


/********************************************************************\
 * stockSplitCB -- open up the stock split druid                    *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void
stockSplitCB (GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;

  gnc_stock_split_dialog (xaccLedgerDisplayLeader (regData->ledger));
}


/********************************************************************\
 * editCB -- open up the account edit window                        *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void 
editCB(GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;
  Account *account = xaccLedgerDisplayLeader (regData->ledger);

  if (account == NULL)
    return;

  gnc_ui_edit_account_window(account);
}


/********************************************************************\
 * startRecnCB -- open up the reconcile window... called from       *
 *   menubar.                                                       *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void 
startRecnCB(GtkWidget * w, gpointer data)
{
  RegWindow *regData = data;
  Account *account = xaccLedgerDisplayLeader (regData->ledger);

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
 * recordCB                                                         *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void
recordCB (GtkWidget *w, gpointer data)
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
gnc_transaction_delete_query(GtkWindow *parent)
{
  GtkWidget *dialog;
  GtkWidget *dvbox;
  GtkWidget *frame;
  GtkWidget *vbox;
  GtkWidget *trans_button;
  GtkWidget *splits_button;
  GtkWidget *text;
  GSList    *group;
  gint       pos = 0;
  gint       result;

  const char *usual = _("This selection will delete the whole "
                        "transaction. This is what you usually want.");
  const char *warn  = _("Warning: Just deleting all the splits will "
                        "make your account unbalanced. You probably "
                        "shouldn't do this unless you're going to "
                        "immediately add another split to bring the "
                        "transaction back into balance.");

  DeleteType return_value;

  dialog = gnome_dialog_new(_("Delete Transaction"),
                            GNOME_STOCK_BUTTON_OK,
                            GNOME_STOCK_BUTTON_CANCEL,
                            NULL);

  gnome_dialog_set_default(GNOME_DIALOG(dialog), 0);
  gnome_dialog_close_hides(GNOME_DIALOG(dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(dialog), parent);

  dvbox = GNOME_DIALOG(dialog)->vbox;

  frame = gtk_frame_new(NULL);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);

  vbox = gtk_vbox_new(TRUE, 3);
  gtk_container_border_width(GTK_CONTAINER(vbox), 5);
  gtk_container_add(GTK_CONTAINER(frame), vbox);

  text = gtk_text_new(NULL, NULL);

  trans_button =
    gtk_radio_button_new_with_label(NULL,
                                    _("Delete the whole transaction"));
  gtk_object_set_user_data(GTK_OBJECT(trans_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), trans_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(trans_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb),
                     (gpointer) usual);

  group = gtk_radio_button_group(GTK_RADIO_BUTTON(trans_button));
  splits_button = gtk_radio_button_new_with_label(group,
                                                  _("Delete all the splits"));
  gtk_object_set_user_data(GTK_OBJECT(splits_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), splits_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(splits_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb),
                     (gpointer) warn);

  gtk_box_pack_start(GTK_BOX(dvbox), frame, TRUE, TRUE, 0);

  gtk_editable_insert_text(GTK_EDITABLE(text), usual, strlen(warn), &pos);
  gtk_text_set_line_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_word_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_editable(GTK_TEXT(text), FALSE);
  gtk_box_pack_start(GTK_BOX(dvbox), text, FALSE, FALSE, 0);

  gtk_widget_show_all(dvbox);

  result = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

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
 * deleteCB                                                         *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
static void
deleteCB(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegisterStyle style;
  CursorClass cursor_class;
  SplitRegister *reg;
  Transaction *trans;
  char *buf = NULL;
  Split *split;
  gint result;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(reg);
  if (split == NULL)
  {
    xaccSRCancelCursorSplitChanges(reg);
    return;
  }

  trans = xaccSplitGetParent(split);
  style = reg->style;
  cursor_class = xaccSplitRegisterGetCurrentCursorClass(reg);

  /* Deleting the blank split just cancels */
  {
    Split *blank_split = xaccSRGetBlankSplit (reg);

    if (split == blank_split)
    {
      xaccSRCancelCursorTransChanges (reg);
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
    /* ask for user confirmation before performing permanent damage */
    buf = g_strdup_printf(format, xaccSplitGetMemo(split),
                          xaccTransGetDescription(trans));

    result = gnc_verify_dialog_parented(regData->window, buf, FALSE);

    g_free(buf);

    if (!result)
      return;

    xaccSRDeleteCurrentSplit (reg);
    return;
  }

  assert(cursor_class == CURSOR_CLASS_TRANS);

  /* On a transaction cursor with 2 or fewer splits in single or double
   * mode, we just delete the whole transaction, kerblooie */
  if ((xaccTransCountSplits(trans) <= 2) && (style == REG_STYLE_LEDGER))
  {
    const char *message = _("Are you sure you want to delete the current "
                            "transaction?");

    result = gnc_verify_dialog_parented(regData->window, message, FALSE);

    if (!result)
      return;

    xaccSRDeleteCurrentTrans (reg);
    return;
  }

  /* At this point we are on a transaction cursor with more than 2 splits
   * or we are on a transaction cursor in multi-line mode or an auto mode.
   * We give the user two choices: delete the whole transaction or delete
   * all the splits except the transaction split. */
  {
    DeleteType del_type;

    del_type = gnc_transaction_delete_query(GTK_WINDOW(regData->window));

    if (del_type == DELETE_CANCEL)
      return;

    if (del_type == DELETE_TRANS)
    {
      xaccSRDeleteCurrentTrans (reg);
      return;
    }

    if (del_type == DELETE_SPLITS)
    {
      xaccSREmptyCurrentTrans (reg);
      return;
    }
  }
}


/********************************************************************\
 * duplicateCB                                                      *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - the data struct for this register               *
 * Return: none                                                     *
\********************************************************************/
static void duplicateCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  xaccSRDuplicateCurrent (xaccLedgerDisplayGetSR (regData->ledger));
}


/********************************************************************\
 * cancelCB                                                         *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void
cancelCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  xaccSRCancelCursorTransChanges (xaccLedgerDisplayGetSR (regData->ledger));
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

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  pending_changes = xaccSRHasPendingChanges (reg);
  if (pending_changes)
  {
    const char *message = _("The current transaction has been changed.\n"
                            "Would you like to record it?");
    if (gnc_verify_dialog_parented(regData->window, message, TRUE))
      recordCB(regData->window, regData);
    else
      xaccSRCancelCursorTransChanges (reg);
  }
}

/********************************************************************\
 * closeCB                                                          *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
closeCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_check_close (regData);

  xaccLedgerDisplayClose (regData->ledger);
}

static void
report_helper (RegWindow *regData, SCM func, Query *query)
{
  SplitRegister *reg = xaccLedgerDisplayGetSR (regData->ledger);
  char *str;
  SCM qtype;
  SCM args;
  SCM arg;

  g_return_if_fail (gh_procedure_p (func));

  args = SCM_EOL;

  arg = gh_str02scm (xaccSRGetCreditString (reg));
  args = gh_cons (arg, args);

  arg = gh_str02scm (xaccSRGetDebitString (reg));
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
  g_return_if_fail (qtype != SCM_UNDEFINED);

  if (!query)
  {
    query = xaccLedgerDisplayGetQuery (regData->ledger);
    g_return_if_fail (query != NULL);

    query = xaccQueryCopy (query);
  }

  arg = gw_wcp_assimilate_ptr (query, qtype);
  args = gh_cons (arg, args);
  if (arg == SCM_UNDEFINED)
  {
    xaccFreeQuery (query);
    return;
  }

  gh_apply (func, args);
}

/********************************************************************\
 * reportCB                                                         *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
reportCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SCM func;

  func = gh_eval_str ("gnc:show-register-report");
  g_return_if_fail (gh_procedure_p (func));

  report_helper (regData, func, NULL);
}

/********************************************************************\
 * invoiceCB                                                        *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
invoiceCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SCM func;

  func = gh_eval_str ("gnc:show-invoice-report");
  g_return_if_fail (gh_procedure_p (func));

  report_helper (regData, func, NULL);
}

/********************************************************************\
 * invoiceTransCB                                                   *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
invoiceTransCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SplitRegister *reg;
  Split *split;
  Query *query;
  SCM func;

  reg = xaccLedgerDisplayGetSR (regData->ledger);

  split = xaccSRGetCurrentSplit (reg);
  if (!split)
    return;

  func = gh_eval_str ("gnc:show-invoice-report");
  g_return_if_fail (gh_procedure_p (func));

  query = xaccMallocQuery ();

  xaccQuerySetGroup (query, gncGetCurrentGroup ());

  xaccQueryAddGUIDMatch (query, xaccSplitGetGUID (split), QUERY_AND);

  report_helper (regData, func, query);
}

/********************************************************************\
 * printReportCB                                                    *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
printReportCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;
  SCM func;

  func = gh_eval_str ("gnc:print-register-report");
  g_return_if_fail (gh_procedure_p (func));

  report_helper (regData, func, NULL);
}

/********************************************************************\
 * dateCB                                                           *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
dateCB (GtkWidget *widget, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_show_date_window(regData);
}

/********************************************************************\
 * helpCB                                                           *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data   - not used                                        *
 * Return: none                                                     *
\********************************************************************/
static void
helpCB (GtkWidget *widget, gpointer data)
{
  helpWindow (NULL, NULL, HH_REGWIN);
}

/************************** END OF FILE **************************/

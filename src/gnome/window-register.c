/*******************************************************************\
 * window-register.c -- the register window for GnuCash             *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
 * Copyright (C) 1999-2000 Dave Peticolas <peticola@cs.ucdavis.edu> *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#define _GNU_SOURCE

#include "top-level.h"

#include <gnome.h>
#include <math.h>

#include "gnome-top-level.h"
#include "MultiLedger.h"
#include "LedgerUtils.h"
#include "MainWindow.h"
#include "Refresh.h"
#include "RegWindow.h"
#include "Scrub.h"
#include "window-reconcile.h"
#include "AccWindow.h"
#include "window-help.h"
#include "AdjBWindow.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "query-user.h"
#include "enriched-messages.h"
#include "table-gnome.h"
#include "table-html.h"
#include "gnucash-sheet.h"
#include "global-options.h"
#include "util.h"


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

  GtkWidget * toolbar;
  SCM toolbar_change_callback_id;

  GtkWidget * statusbar;

  GtkWidget * balance_label;
  GtkWidget * cleared_label;

  GnucashRegister *reg;

  RegDateWindow *date_window;

  /* Do we close the ledger when the window closes? */
  gncBoolean close_ledger;
};


/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;

static int last_width = 0;
static int last_stock_width = 0;


/** PROTOTYPES ******************************************************/
RegWindow *regWindowLedger(xaccLedgerDisplay *ledger);
static void regRefresh(xaccLedgerDisplay *ledger);
static void gnc_reg_refresh_toolbar(RegWindow *regData);
static void regDestroy(xaccLedgerDisplay *ledger);
static void regSetHelp(xaccLedgerDisplay *ledger, const char *help_str);

static void closeRegWindow(GtkWidget *w, RegWindow *regData);
static void gnc_register_check_close(RegWindow *regData);

static void cutCB(GtkWidget *w, gpointer data);
static void copyCB(GtkWidget *w, gpointer data);
static void pasteCB(GtkWidget *w, gpointer data);
static void startRecnCB(GtkWidget *w, gpointer data);
static void xferCB(GtkWidget *w, gpointer data);
static void editCB(GtkWidget *w, gpointer data);
static void helpCB(GtkWidget *w, gpointer data);
static void startAdjBCB(GtkWidget * w, gpointer data);
static void newAccountCB(GtkWidget * w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void duplicateCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);
static void closeCB(GtkWidget *w, gpointer data);
static void dateCB(GtkWidget *w, gpointer data);
static void new_trans_cb(GtkWidget *widget, gpointer data);
static void jump_cb(GtkWidget *widget, gpointer data);
static void print_check_cb(GtkWidget * widget, gpointer data);

static gboolean gnc_register_include_date(RegWindow *regData, time_t date);


/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window for Account account                 *
 *                                                                  *
 * Args:   account - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowSimple(Account *account)
{
  RegWindow *result = NULL;
  xaccLedgerDisplay * ledger = xaccLedgerDisplaySimple(account);

  if (ledger != NULL)
    result = regWindowLedger(ledger);

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
regWindowAccGroup(Account *account)
{
  RegWindow *result = NULL;
  xaccLedgerDisplay * ledger = xaccLedgerDisplayAccGroup(account);

  if (ledger != NULL)
    result = regWindowLedger(ledger);

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
gnc_register_raise(RegWindow *regData)
{
  if (regData == NULL)
    return;

  if (regData->window == NULL)
    return;

  gtk_widget_show(regData->window);

  if (regData->window->window == NULL)
    return;

  gdk_window_raise(regData->window->window);
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
  int vrow, vcol;

  trans = xaccSplitGetParent(split);
  if (trans != NULL)
    if (gnc_register_include_date(regData, xaccTransGetDate(trans)))
    {
      regData->ledger->dirty = 1;
      xaccLedgerDisplayRefresh(regData->ledger);
    }

  if (xaccSRGetSplitRowCol(regData->ledger->ledger, split, &vrow, &vcol))
    gnucash_register_goto_virt_row_col(regData->reg, vrow, vcol);
}


static int
gnc_register_get_default_type(SplitRegister *reg)
{
  char *style_string;
  int new_style = REG_SINGLE_LINE;
  int type = reg->type;

  type &= ~REG_STYLE_MASK;

  style_string = gnc_lookup_multichoice_option("Register", 
                                               "Default Register Mode",
                                               "single_line");

  if (safe_strcmp(style_string, "single_line") == 0)
    new_style = REG_SINGLE_LINE;
  else if (safe_strcmp(style_string, "double_line") == 0)
    new_style = REG_DOUBLE_LINE;
  else if (safe_strcmp(style_string, "multi_line") == 0)
    new_style = REG_MULTI_LINE;
  else if (safe_strcmp(style_string, "auto_single") == 0)
    new_style = REG_SINGLE_DYNAMIC;
  else if (safe_strcmp(style_string, "auto_double") == 0)
    new_style = REG_DOUBLE_DYNAMIC;

  type |= new_style;

  if (style_string != NULL)
    free(style_string);

  return type;
}


static void
gnc_register_change_style(RegWindow *regData, int style_code)
{
  SplitRegister *reg = regData->ledger->ledger;
  int type = reg->type;

  type &= ~REG_STYLE_MASK;
  type |=  style_code;

  xaccConfigSplitRegister(reg, type);

  regData->ledger->dirty = 1;
  xaccLedgerDisplayRefresh(regData->ledger);
}

static void
gnc_register_style_single_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_change_style(regData, REG_SINGLE_LINE);
}

static void
gnc_register_style_double_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_change_style(regData, REG_DOUBLE_LINE);
}

static void
gnc_register_style_multi_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_change_style(regData, REG_MULTI_LINE);
}

static void
gnc_register_style_auto_single_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_change_style(regData, REG_SINGLE_DYNAMIC);
}

static void
gnc_register_style_auto_double_cb(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnc_register_change_style(regData, REG_DOUBLE_DYNAMIC);
}


static void
gnc_register_sort(RegWindow *regData, int sort_code)
{
  Query *query = regData->ledger->query;

  switch(sort_code)
  {
    case BY_STANDARD:
      xaccQuerySetSortOrder(query, BY_STANDARD, BY_NONE, BY_NONE);
      break;
    case BY_DATE:
      xaccQuerySetSortOrder(query, BY_DATE, BY_NUM, BY_AMOUNT);
      break;
    case BY_NUM:
      xaccQuerySetSortOrder(query, BY_NUM, BY_DATE, BY_AMOUNT);
      break;
    case BY_AMOUNT:
      xaccQuerySetSortOrder(query, BY_AMOUNT, BY_DATE, BY_NUM);
      break;
    case BY_MEMO:
      xaccQuerySetSortOrder(query, BY_MEMO, BY_DATE, BY_NUM);
      break;
    case BY_DESC:
      xaccQuerySetSortOrder(query, BY_DESC, BY_DATE, BY_NUM);
      break;
    default:
      assert(0); /* we should never be here */
  }

  regData->ledger->dirty = 1;
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

  assert(regData != NULL);
  assert(regData->ledger != NULL);
  assert(regData->ledger->query != NULL);

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

  assert(regData != NULL);
  assert(regData->ledger != NULL);
  assert(regData->ledger->query != NULL);

  regDateData = regData->date_window;
  if (regDateData == NULL)
    return;

  gtk_widget_set_sensitive(regDateData->set_button, FALSE);

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_earliest);
  if (gtk_toggle_button_get_active(toggle))
    xaccQueryShowEarliestDateFound(regData->ledger->query);
  else
  {
    time_t start;

    start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regDateData->start_date));
    start = gnc_register_min_day_time(start);

    xaccQuerySetEarliest(regData->ledger->query, start);
  }

  toggle = GTK_TOGGLE_BUTTON(regDateData->show_latest);
  if (gtk_toggle_button_get_active(toggle))
    xaccQueryShowLatestDateFound(regData->ledger->query);
  else
  {
    time_t end;

    end = gnome_date_edit_get_date(GNOME_DATE_EDIT(regDateData->end_date));
    end = gnc_register_max_day_time(end);

    xaccQuerySetLatest(regData->ledger->query, end);
  }

  gnc_date_range_set_sensitivities(regData);
}

static void
gnc_register_date_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  gnc_register_set_date_range(regData);

  regData->ledger->dirty = 1;
  xaccLedgerDisplayRefresh(regData->ledger);
}

static void
show_all_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
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
  RegWindow *regData = (RegWindow *) data;
  RegDateWindow *regDateData;

  assert(regData != NULL);

  regDateData = regData->date_window;
  gnome_date_edit_set_time(GNOME_DATE_EDIT(regDateData->end_date), time(NULL));

  gtk_widget_set_sensitive(regData->date_window->set_button, TRUE);
}

static void
gnc_register_date_toggle_cb(GtkToggleButton *toggle, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

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
gnc_register_date_window(RegWindow *regData)
{
  RegDateWindow *regDateData;
  GtkWidget *dialog;
  GtkWidget *frame;
  GtkWidget *dvbox;

  regDateData = g_new0(RegDateWindow, 1);
  regData->date_window = regDateData;

  dialog = gnome_dialog_new(REG_DATE_RANGES_STR,
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
    gboolean show_all;

    show_all = gnc_lookup_boolean_option("Register",
                                         "Show All Transactions",
                                         TRUE);

    vbox = gtk_vbox_new(FALSE, 2);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);
    gtk_container_add(GTK_CONTAINER(frame), vbox);

    /* Starting Date */
    vbox2 = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(vbox), vbox2, FALSE, FALSE, 0);

    radio = gtk_radio_button_new_with_label(NULL, SHOW_EARLIEST_STR);
    gtk_box_pack_start(GTK_BOX(vbox2), radio, FALSE, FALSE, 0);
    regDateData->show_earliest = radio;
    
    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);

    group = gtk_radio_button_group(GTK_RADIO_BUTTON(radio));
    radio = gtk_radio_button_new_with_label(group, START_DATE_C_STR);
    gtk_box_pack_start(GTK_BOX(hbox), radio, FALSE, FALSE, 0);

    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    gtk_signal_connect(GTK_OBJECT(radio), "toggled",
		       GTK_SIGNAL_FUNC(gnc_register_date_toggle_cb), regData);

    date = gnome_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);
    regDateData->start_date = date;

    time_val = xaccQueryGetEarliestDateFound(regData->ledger->query);
    if (time_val < time(NULL))
      gnome_date_edit_set_time(GNOME_DATE_EDIT(date), time_val);

    gtk_signal_connect(GTK_OBJECT(date), "date-changed",
                       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    calendar = GNOME_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    entry = GNOME_DATE_EDIT(date)->date_entry;
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

    radio = gtk_radio_button_new_with_label(NULL, SHOW_LATEST_STR);
    gtk_box_pack_start(GTK_BOX(vbox2), radio, FALSE, FALSE, 0);
    regDateData->show_latest = radio;

    if (show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox2), hbox, FALSE, FALSE, 0);

    group = gtk_radio_button_group(GTK_RADIO_BUTTON(radio));
    radio = gtk_radio_button_new_with_label(group, END_DATE_C_STR);
    gtk_box_pack_start(GTK_BOX(hbox), radio, FALSE, FALSE, 0);

    if (!show_all)
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(radio), TRUE);

    gtk_signal_connect(GTK_OBJECT(radio), "toggled",
		       GTK_SIGNAL_FUNC(gnc_register_date_toggle_cb), regData);

    date = gnome_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(hbox), date, FALSE, FALSE, 0);
    regDateData->end_date = date;

    gtk_signal_connect(GTK_OBJECT(date), "date-changed",
                       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    calendar = GNOME_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    entry = GNOME_DATE_EDIT(date)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "activate",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);
    gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_register_date_changed_cb), regData);

    button = gtk_button_new_with_label(TODAY_STR);
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_register_today_cb), regData);
    regDateData->today_button = button;

    button = gtk_button_new_with_label(SET_DATE_RANGE_STR);
    gtk_box_pack_start(GTK_BOX(dvbox), button, FALSE, FALSE, 5);
    gtk_widget_set_sensitive(button, FALSE);
    regDateData->set_button = button;

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
  }

  return regDateData;
}

static GtkWidget *
gnc_register_create_tool_bar(RegWindow *regData)
{
  GtkWidget *toolbar;
  GnomeUIInfo toolbar_info[] =
  {
    {
      GNOME_APP_UI_ITEM,
      RECORD_STR, TOOLTIP_RECORD,
      recordCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      CANCEL_STR, TOOLTIP_CANCEL_TRANS,
      cancelCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDELETE,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      DELETE_STR, TOOLTIP_DEL_TRANS,
      deleteCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      DUPLICATE_STR, TOOLTIP_DUP_TRANS,
      duplicateCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_COPY,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      BLANK_STR, TOOLTIP_BLANK_TRANS,
      new_trans_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_NEW,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      JUMP_STR, TOOLTIP_JUMP_TRANS,
      jump_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      TRANSFER_STR, TOOLTIP_TRANSFER,
      xferCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      CLOSE_STR, TOOLTIP_CLOSE_REG,
      closeCB, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

  gnome_app_fill_toolbar_with_data(GTK_TOOLBAR(toolbar), toolbar_info,
                                   NULL, regData);

  regData->toolbar = toolbar;

  return toolbar;
}


static GtkWidget *
gnc_register_create_status_bar(RegWindow *regData)
{
  GtkWidget *statusbar;
  GtkWidget *hbox;
  GtkWidget *label;

  statusbar = gnome_appbar_new(GNC_F, /* no progress bar */
			       GNC_T, /* has status area */
			       GNOME_PREFERENCES_USER);

  hbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_end(GTK_BOX(statusbar), hbox, FALSE, FALSE, 5);

  label = gtk_label_new(CLEARED_C_STR);
  gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  label = gtk_label_new("");
  gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
  regData->cleared_label = label;
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  hbox = gtk_hbox_new(FALSE, 2);
  gtk_box_pack_end(GTK_BOX(statusbar), hbox, FALSE, FALSE, 5);

  label = gtk_label_new(BALN_C_STR);
  gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  label = gtk_label_new("");
  gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
  regData->balance_label = label;
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

  regData->statusbar = statusbar;

  return statusbar;
}


void
gnc_register_jump_to_blank(RegWindow *regData)
{
  SplitRegister *sr = regData->ledger->ledger;
  Split *blank;
  int vrow, vcol;

  blank = xaccSRGetBlankSplit(sr);
  if (blank == NULL)
    return;

  if (xaccSRGetSplitRowCol(sr, blank, &vrow, &vcol))
    gnucash_register_goto_virt_row_col(regData->reg, vrow, vcol);
}


static void
new_trans_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  gnc_register_jump_to_blank(regData);
}

static void
jump_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  Account *account;
  Split *split;

  split = xaccSRGetCurrentSplit(regData->ledger->ledger);
  if (split == NULL)
    return;

  account = xaccSplitGetAccount(split);
  if (account == NULL)
    return;

  if (account == regData->ledger->leader)
  {
    split = xaccGetOtherSplit(split);
    if (split == NULL)
      return;

    account = xaccSplitGetAccount(split);
    if (account == NULL)
      return;
    if (account == regData->ledger->leader)
      return;
  }

  regData = regWindowSimple(account);
  if (regData == NULL)
    return;

  gnc_register_raise(regData);
  gnc_register_jump_to_split(regData, split);
}

static void
print_check_cb(GtkWidget * widget, gpointer data)
{
  RegWindow    * reg_data = (RegWindow *)data;
#ifdef HAVE_LIBGNOMEPRINT
  Split        * split    = xaccSRGetCurrentSplit(reg_data->ledger->ledger);
  Transaction  * trans    = xaccSplitGetParent(split);

  char         * payee;
  char         * memo;
  double       amount;
  char         datestring[1024];
  struct tm    * timestruct;
  time_t       date;

  SCM print_check = gh_eval_str("gnc:print-check");

  if(split && trans &&
     gh_procedure_p(print_check))
  {
    payee  = xaccTransGetDescription(trans);
    amount = xaccSplitGetValue(split);
    date   = xaccTransGetDate(trans);
    memo   = xaccSplitGetMemo(split);

    timestruct = localtime(&date);
    if(!timestruct) {
      g_warning("print_check_cb: error in date translation\n");
      return;
    }

    strftime(datestring, sizeof(datestring), "%B %d, %Y", timestruct);

    gh_apply(print_check,
             SCM_LIST4(gh_str02scm(payee),
                       gh_double2scm(fabs(amount)),
                       gh_str02scm(datestring),
                       gh_str02scm(memo)));
  }
#else
  gnc_info_dialog_parented(reg_data->dialog,
                           _"You need to install the gnome-print library.");
#endif
}


static void
gnc_register_scrub_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  Account *account = regData->ledger->leader;

  if (account == NULL)
    return;

  xaccAccountTreeScrubOrphans(account);
  xaccAccountTreeScrubImbalance(account);

  gnc_account_ui_refresh(account);
  gnc_refresh_main_window();
}

static GtkWidget *
gnc_register_create_menu_bar(RegWindow *regData, GtkWidget *statusbar)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group;

  GnomeUIInfo style_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(SINGLE_LINE_STR, TOOLTIP_SINGLE_LINE,
                               gnc_register_style_single_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(DOUBLE_LINE_STR, TOOLTIP_DOUBLE_LINE,
                               gnc_register_style_double_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(MULTI_LINE_STR, TOOLTIP_MULTI_LINE,
                               gnc_register_style_multi_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(AUTO_SINGLE_STR, TOOLTIP_AUTO_SINGLE,
                               gnc_register_style_auto_single_cb,
                               regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(AUTO_DOUBLE_STR, TOOLTIP_AUTO_DOUBLE,
                               gnc_register_style_auto_double_cb,
                               regData, NULL),
    GNOMEUIINFO_END
  };

  GnomeUIInfo style_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(style_list),
    GNOMEUIINFO_END
  };

  GnomeUIInfo sort_list[] =
  {
    GNOMEUIINFO_RADIOITEM_DATA(STANDARD_ORDER_STR, TOOLTIP_STANDARD_ORD,
                               gnc_register_sort_standard_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(SORT_BY_DATE_STR, TOOLTIP_SORT_BY_DATE,
                               gnc_register_sort_date_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(SORT_BY_NUM_STR, TOOLTIP_SORT_BY_NUM,
                               gnc_register_sort_num_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(SORT_BY_AMNT_STR, TOOLTIP_SORT_BY_AMNT,
                               gnc_register_sort_amount_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(SORT_BY_MEMO_STR, TOOLTIP_SORT_BY_MEMO,
                               gnc_register_sort_memo_cb, regData, NULL),
    GNOMEUIINFO_RADIOITEM_DATA(SORT_BY_DESC_STR, TOOLTIP_SORT_BY_DESC,
                               gnc_register_sort_desc_cb, regData, NULL),
    GNOMEUIINFO_END
  };

  GnomeUIInfo sort_menu[] =
  {
    GNOMEUIINFO_RADIOLIST(sort_list),
    GNOMEUIINFO_END
  };

  GnomeUIInfo date_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      SHOW_ALL_MENU_STR, TOOLTIP_SHOW_ALL,
      show_all_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      SET_RANGE_MENU_E_STR, TOOLTIP_DATE_RANGE,
      dateCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo register_menu[] =
  {
    GNOMEUIINFO_SUBTREE(STYLE_MENU_STR, style_menu),
    GNOMEUIINFO_SUBTREE(SORT_ORDER_MENU_STR, sort_menu),
    GNOMEUIINFO_SUBTREE(DATE_RANGE_MENU_STR, date_menu),
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_MENU_CLOSE_ITEM(closeCB, regData),
    GNOMEUIINFO_END
  };

  GnomeUIInfo edit_menu[] =
  {
    GNOMEUIINFO_MENU_CUT_ITEM(cutCB, regData),
    GNOMEUIINFO_MENU_COPY_ITEM(copyCB, regData),
    GNOMEUIINFO_MENU_PASTE_ITEM(pasteCB, regData),
  };

  GnomeUIInfo account_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      EDIT_ACC_MENU_STR, TOOLTIP_EDIT_REG,
      editCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      RECONCILE_MENU_E_STR, TOOLTIP_RECN_REG,
      startRecnCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      TRANSFER_MENU_E_STR, TOOLTIP_TRANSFER,
      xferCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      ADJ_BALN_MENU_E_STR, TOOLTIP_ADJUST_REG,
      startAdjBCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      NEW_ACC_MENU_E_STR, TOOLTIP_NEW,
      newAccountCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      SCRUB_MENU_STR, TOOLTIP_SCRUB_REG,
      gnc_register_scrub_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      RECORD_MENU_STR, TOOLTIP_RECORD,
      recordCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      CANCEL_MENU_STR, TOOLTIP_CANCEL_TRANS,
      cancelCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      DELETE_MENU_STR, TOOLTIP_DEL_TRANS,
      deleteCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      DUPLICATE_MENU_STR, TOOLTIP_DUP_TRANS,
      duplicateCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      BLANK_MENU_STR, TOOLTIP_BLANK_TRANS,
      new_trans_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      JUMP_MENU_STR, TOOLTIP_JUMP_TRANS,
      jump_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      PRINT_CHECK_MENU_STR, TOOLTIP_PRINT_CHECK,
      print_check_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo help_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      HELP_MENU_STR, TOOLTIP_HELP,
      helpCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo register_window_menu[] =
  {
    GNOMEUIINFO_SUBTREE(REGISTER_MENU_STR, register_menu),
    GNOMEUIINFO_MENU_EDIT_TREE(edit_menu),
    GNOMEUIINFO_SUBTREE(ACCOUNT_MENU_STR, account_menu),
    GNOMEUIINFO_SUBTREE(TRANSACTION_MENU_STR, transaction_menu),
    GNOMEUIINFO_MENU_HELP_TREE(help_menu),
    GNOMEUIINFO_END
  };

  menubar = gtk_menu_bar_new();

  accel_group = gtk_accel_group_new();
  gtk_accel_group_attach(accel_group, GTK_OBJECT(regData->window));

  gnome_app_fill_menu(GTK_MENU_SHELL(menubar), register_window_menu,
  		      accel_group, TRUE, 0);

  gnome_app_install_appbar_menu_hints(GNOME_APPBAR(statusbar),
                                      register_window_menu);

  /* Make sure the right style radio item is active */
  {
    GtkWidget *widget;
    int index;
    int style;

    style = gnc_register_get_default_type(regData->ledger->ledger);
    style &= REG_STYLE_MASK;

    switch (style)
    {
      default:
      case REG_SINGLE_LINE:
        index = 0;
        break;
      case REG_DOUBLE_LINE:
        index = 1;
        break;
      case REG_MULTI_LINE:
        index = 2;
        break;
      case REG_SINGLE_DYNAMIC:
        index = 3;
        break;
      case REG_DOUBLE_DYNAMIC:
        index = 4;
        break;
    }

    widget = style_list[index].widget;

    gtk_signal_handler_block_by_data(GTK_OBJECT(widget), regData);

    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(widget), TRUE);

    gtk_signal_handler_unblock_by_data(GTK_OBJECT(widget), regData);
  }

  return menubar;
}


static GtkWidget *
gnc_register_create_popup_menu(RegWindow *regData)
{
  GtkWidget *popup;

  GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      RECORD_MENU_STR, TOOLTIP_RECORD,
      recordCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      CANCEL_MENU_STR, TOOLTIP_CANCEL_TRANS,
      cancelCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      DELETE_MENU_STR, TOOLTIP_DEL_TRANS,
      deleteCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      DUPLICATE_MENU_STR, TOOLTIP_DUP_TRANS,
      duplicateCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      BLANK_MENU_STR, TOOLTIP_BLANK_TRANS,
      new_trans_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      JUMP_MENU_STR, TOOLTIP_JUMP_TRANS,
      jump_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  popup = gnome_popup_menu_new(transaction_menu);

  return popup;
}


static void
gnc_register_record_cb(GnucashRegister *reg, gpointer data)
{
  RegWindow *regData = data;
  gncBoolean goto_blank = GNC_F;

  /* If we are in single or double line mode and we hit enter
   * on the blank split, go to the blank split instead of the
   * next row. This prevents the cursor from jumping around
   * when you are entering transactions. */
  {
    SplitRegister *sr = regData->ledger->ledger;
    int type = sr->type;

    type &= REG_STYLE_MASK;

    if ((type == REG_SINGLE_LINE) || (type == REG_DOUBLE_LINE))
    {
      Split *blank_split;

      blank_split = xaccSRGetBlankSplit(sr);
      if (blank_split != NULL)
      {
        Split *current_split;

        current_split = xaccSRGetCurrentSplit(sr);

        if (blank_split == current_split)
          goto_blank = GNC_T;
      }
    }
  }

  /* First record the transaction. This will perform a refresh. */
  recordCB(GTK_WIDGET(reg), data);

  /* Now move. */
  if (goto_blank)
    gnc_register_jump_to_blank(regData);
  else
    gnucash_register_goto_next_virt_row(reg);
}

static void
gnc_register_destroy_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  closeRegWindow(widget, regData);
}

static gncUIWidget
gnc_register_get_parent(xaccLedgerDisplay *ledger)
{
  RegWindow *regData;

  if (ledger == NULL)
    return NULL;

  regData = ledger->gui_hook;
  if (regData == NULL)
    return NULL;

  return regData->window;
}

static void
gnc_reg_set_window_name(RegWindow *regData)
{
  Account *leader;
  gchar *windowname;
  gchar *account_name;
  gchar *reg_name;

  if (regData == NULL)
    return;

  leader = regData->ledger->leader;

  if (leader != NULL)
  {
    account_name = xaccAccountGetFullName(leader, gnc_get_account_separator());

    switch (regData->ledger->type)
    {
      case GENERAL_LEDGER:
      case INCOME_LEDGER:
        reg_name = GENERAL_LEDGER_STR;
        break;
      case PORTFOLIO:
        reg_name = PORTFOLIO_STR;
        break;
      default:
        reg_name = REGISTER_STR;
        break;
    }

    windowname = g_strconcat(account_name, " - ", reg_name, NULL);

    free(account_name);
  }
  else
    windowname = g_strdup(GENERAL_LEDGER_STR);

  gtk_window_set_title(GTK_WINDOW(regData->window), windowname);

  g_free(windowname);
}

static void
gnc_toolbar_change_cb(void *data)
{
  RegWindow *regData = data;

  gnc_reg_refresh_toolbar(regData);
}

/********************************************************************\
 * regWindowLedger                                                  *
 *   opens up a ledger window for the account list                  *
 *                                                                  *
 * Args:   ledger - ledger data structure                           *
 * Return: regData  - the register window instance                  *
\********************************************************************/
RegWindow *
regWindowLedger(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = NULL;
  GtkWidget *vbox;
  GtkWidget *register_window;
  GtkWidget *register_dock;
  GtkWidget *table_frame;
  GtkWidget *statusbar;

  xaccQuerySetMaxSplits(ledger->query, INT_MAX);
  xaccQuerySetSortOrder(ledger->query, BY_STANDARD, BY_NONE, BY_NONE);

  regData = (RegWindow *) (ledger->gui_hook);
  if (regData != NULL)
    return regData;

  regData = (RegWindow *) malloc(sizeof (RegWindow));

  ledger->gui_hook = (void *) regData;
  ledger->redraw = regRefresh;
  ledger->destroy = regDestroy;
  ledger->set_help = regSetHelp;
  ledger->get_parent = gnc_register_get_parent;

  register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(register_window), vbox);

  register_dock = gnome_dock_new();
  gtk_box_pack_start(GTK_BOX(vbox), register_dock, TRUE, TRUE, 0);

  regData->ledger = ledger;
  regData->close_ledger = GNC_T;
  regData->window = register_window;

  gnc_reg_set_window_name(regData);

  /* Invoked when window is being destroyed. */
  gtk_signal_connect(GTK_OBJECT(regData->window), "destroy",
		     GTK_SIGNAL_FUNC (gnc_register_destroy_cb),
		     (gpointer) regData);

  regData->date_window = gnc_register_date_window(regData);
  gnc_register_set_date_range(regData);

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

  /* The CreateTable will do the actual gui init, returning a widget */
  {
    GtkWidget *register_widget;
    GtkWidget *popup;
    guint num_rows;

    table_frame = gtk_frame_new(NULL);
    gnome_dock_set_client_area(GNOME_DOCK(register_dock), table_frame);

    num_rows = (guint) gnc_lookup_number_range_option("Register",
                                                      "Number of Rows", 15.0);
    gnucash_register_set_initial_rows(num_rows);

    register_widget = gnucash_register_new(ledger->ledger->table);
    xaccCreateTable(register_widget, ledger->ledger);

    gtk_container_add(GTK_CONTAINER(table_frame), register_widget);

    regData->reg = GNUCASH_REGISTER(register_widget);
    GNUCASH_SHEET(regData->reg->sheet)->window = register_window;

    gtk_signal_connect(GTK_OBJECT(register_widget), "activate_cursor",
                       GTK_SIGNAL_FUNC(gnc_register_record_cb), regData);

    popup = gnc_register_create_popup_menu(regData);
    gnucash_register_attach_popup(GNUCASH_REGISTER(register_widget),
                                  popup, regData);
  }

  /* be sure to initialize the gui elements associated with the cursor */
  xaccConfigSplitRegister(ledger->ledger,
                          gnc_register_get_default_type(ledger->ledger));

  /* Allow grow, allow shrink, auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(register_window), TRUE, TRUE, TRUE);

  {
    int type;
    int *width;
    char *prefix;

    type = ledger->ledger->type & REG_TYPE_MASK;
    switch (type)
    {
      case STOCK_REGISTER:
      case PORTFOLIO:
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
      gnc_get_window_size(prefix, width, NULL);

    gtk_window_set_default_size(GTK_WINDOW(register_window), *width, 0);
  }

  gtk_widget_show_all(register_window);

  ledger->dirty = 1;
  xaccLedgerDisplayRefresh(ledger);
  gnc_reg_refresh_toolbar(regData);

  gnc_register_jump_to_blank(regData);

  return regData;
}


static void
gnc_reg_refresh_toolbar(RegWindow *regData)
{
  GtkToolbarStyle tbstyle;

  if ((regData == NULL) || (regData->toolbar == NULL))
    return;

  tbstyle = gnc_get_toolbar_style();

  gtk_toolbar_set_style(GTK_TOOLBAR(regData->toolbar), tbstyle);
}


static void
regRefresh(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);
  int print_flags = PRTSYM | PRTSEP;

  xaccSRLoadXferCells(ledger->ledger, ledger->leader);

  if (regData->window != NULL)
  {
    gboolean reverse = gnc_reverse_balance(ledger->leader);
    double amount;

    amount = ledger->balance;
    if (reverse)
      amount = -amount;

    gnc_set_label_color(regData->balance_label, amount);
    gtk_label_set_text(GTK_LABEL(regData->balance_label),
		       xaccPrintAmount(amount, print_flags));

    amount = ledger->clearedBalance;
    if (reverse)
      amount = -amount;

    gnc_set_label_color(regData->cleared_label, amount);
    gtk_label_set_text(GTK_LABEL(regData->cleared_label),
                       xaccPrintAmount(amount, print_flags));

    gnc_reg_set_window_name(regData);
  }
}


static void
gnc_reg_save_size(RegWindow *regData)
{
  int type;
  int *width;
  char *prefix;

  type = regData->ledger->ledger->type & REG_TYPE_MASK;
  switch (type)
  {
    case STOCK_REGISTER:
    case PORTFOLIO:
    case CURRENCY_REGISTER:
      prefix = "reg_stock_win";
      width = &last_stock_width;
      break;
    default:
      prefix = "reg_win";
      width = &last_width;
      break;
  }

  gdk_window_get_geometry(regData->window->window, NULL, NULL,
                          width, NULL, NULL);

  gnc_save_window_size(prefix, *width, 0);
}


/********************************************************************\
 * regDestroy()
\********************************************************************/

static void
regDestroy(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);

  if (regData)
  {
    gnc_register_check_close(regData);

    /* It will be closed elsewhere */
    regData->close_ledger = GNC_F;

    gnc_reg_save_size(regData);

    gtk_widget_destroy(regData->window);
  }
}


static void
regSetHelp(xaccLedgerDisplay *ledger, const char *help_str)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);
  const gchar *status;

  if (help_str != NULL)
    status = help_str;
  else
    status = "";

  gnome_appbar_set_default(GNOME_APPBAR(regData->statusbar), status);
}


/********************************************************************\
 * closeRegWindow                                                   *
 *   frees memory for a regWindow, and other cleanup stuff          *
 *                                                                  *
 * Args:   widget  - the widget that called us                      *
 *         regData - the data struct for this register              *
 * Return: none                                                     *
\********************************************************************/
static void 
closeRegWindow(GtkWidget * widget, RegWindow *regData)
{
  SCM id;

  if (regData->close_ledger)
    xaccLedgerDisplayClose(regData->ledger);

  id = regData->toolbar_change_callback_id;
  gnc_unregister_option_change_callback_id(id);

  if (regData->date_window != NULL)
  {
    if (regData->date_window->window != NULL)
      gtk_widget_destroy(regData->date_window->window);

    g_free(regData->date_window);
    regData->date_window = NULL;
  }

  free(regData);

  DEBUG("closed RegWindow\n");
}


static void 
newAccountCB(GtkWidget * w, gpointer data)
{
  accWindow(NULL);
}


/********************************************************************\
 * cutCB -- cut the selection to the clipboard                      *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
cutCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_cut_clipboard(regData->reg);
}

/********************************************************************\
 * copyCB -- copy the selection to the clipboard                    *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
copyCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_copy_clipboard(regData->reg);
}

/********************************************************************\
 * pasteCB -- paste the clipboard to the selection                  *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
pasteCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = data;

  gnucash_register_paste_clipboard(regData->reg);
}

/********************************************************************\
 * startAdjBCB -- open up the adjust balance window... called       *
 *   from the menubar.                                              *
 *                                                                  *
 * Args:    w - the widget that called us                           *
 *       data - the data struct for this register                   *
 * Return: none                                                     *
\********************************************************************/
static void 
startAdjBCB(GtkWidget * w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  xaccLedgerDisplay *ledger = regData->ledger;
  Account *account = ledger->leader;

  if (account == NULL)
    return;

  adjBWindow(account);
}


/********************************************************************\
 * xferCB -- open up the transfer window                            *
 *                                                                  *
 * Args:   w    - the widget that called us                         *
 *         data - the data struct for this register                 *
 * Return: none                                                     *
\********************************************************************/
static void 
xferCB(GtkWidget * w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  xaccLedgerDisplay *ledger = regData->ledger;
  Account *account = ledger->leader;

  if (account == NULL)
    account = ledger->displayed_accounts[0];

  if (account == NULL)
    return;

  gnc_xfer_dialog(regData->window, account);
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
  RegWindow *regData = (RegWindow *) data;
  xaccLedgerDisplay *ledger = regData->ledger;
  Account *account = ledger->leader;

  if (account == NULL)
    return;

  editAccWindow(account);
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
  RegWindow *regData = (RegWindow *) data;
  xaccLedgerDisplay *ledger = regData->ledger;
  Account *account = ledger->leader;

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

  start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regDateData->start_date));
  end   = gnome_date_edit_get_date(GNOME_DATE_EDIT(regDateData->end_date));

  if (date < start)
  {
    gnome_date_edit_set_time(GNOME_DATE_EDIT(regDateData->start_date), date);
    changed = TRUE;
  }

  if (date > end)
  {
    gnome_date_edit_set_time(GNOME_DATE_EDIT(regDateData->end_date), date);
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
recordCB(GtkWidget *w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  gncBoolean really_saved;
  Transaction *trans;
  Split *split;

  split = xaccSRGetCurrentSplit(regData->ledger->ledger);
  trans = xaccSplitGetParent(split);

  really_saved = xaccSRSaveRegEntry(regData->ledger->ledger, NULL);
  if (!really_saved)
    return;

  if (trans != NULL)
    gnc_register_include_date(regData, xaccTransGetDate(trans));

  xaccSRRedrawRegEntry(regData->ledger->ledger);
  gnc_refresh_main_window ();
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
DeleteType
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

  gchar *usual = DEL_USUAL_MSG;
  gchar *warn  = DEL_WARN_MSG;

  dialog = gnome_dialog_new(DEL_TRANS_STR,
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

  trans_button = gtk_radio_button_new_with_label(NULL, DEL_TRANS_MSG);
  gtk_object_set_user_data(GTK_OBJECT(trans_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), trans_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(trans_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb), usual);

  group = gtk_radio_button_group(GTK_RADIO_BUTTON(trans_button));
  splits_button = gtk_radio_button_new_with_label(group, DEL_SPLITS_MSG);
  gtk_object_set_user_data(GTK_OBJECT(splits_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), splits_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(splits_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb), warn);

  gtk_box_pack_start(GTK_BOX(dvbox), frame, TRUE, TRUE, 0);

  gtk_editable_insert_text(GTK_EDITABLE(text), usual, strlen(warn), &pos);
  gtk_text_set_line_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_word_wrap(GTK_TEXT(text), TRUE);
  gtk_text_set_editable(GTK_TEXT(text), FALSE);
  gtk_box_pack_start(GTK_BOX(dvbox), text, FALSE, FALSE, 0);

  gtk_widget_show_all(dvbox);

  result = gnome_dialog_run_and_close(GNOME_DIALOG(dialog));

  if (result != 0)
    return DELETE_CANCEL;

  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(trans_button)))
    return DELETE_TRANS;

  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(splits_button)))
    return DELETE_SPLITS;

  return DELETE_CANCEL;
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
  RegWindow *regData = (RegWindow *) data;
  CursorType cursor_type;
  Transaction *trans;
  char *buf = NULL;
  Split *split;
  gint result;
  int style;

  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(regData->ledger->ledger);
  if (split == NULL)
  {
    xaccSRCancelCursorSplitChanges(regData->ledger->ledger);
    return;
  }

  trans = xaccSplitGetParent(split);
  style = regData->ledger->ledger->type & REG_STYLE_MASK;
  cursor_type = xaccSplitRegisterGetCursorType(regData->ledger->ledger);

  /* Deleting the blank split just cancels */
  {
    Split *blank_split = xaccSRGetBlankSplit(regData->ledger->ledger);
    Transaction *blank_trans = xaccSplitGetParent(blank_split);

    if (trans == blank_trans)
    {
      xaccSRCancelCursorTransChanges(regData->ledger->ledger);
      return;
    }
  }

  if (cursor_type == CURSOR_NONE)
    return;

  /* On a split cursor, just delete the one split. */
  if (cursor_type == CURSOR_SPLIT)
  {
    /* ask for user confirmation before performing permanent damage */
    asprintf(&buf, TRANS_DEL_MSG, xaccSplitGetMemo(split),
             xaccTransGetDescription(trans));

    assert(buf != NULL);

    result = gnc_verify_dialog_parented(GTK_WINDOW(regData->window),
                                        buf, GNC_F);

    free(buf);

    if (!result)
      return;

    xaccSRDeleteCurrentSplit(regData->ledger->ledger);
    return;
  }

  assert(cursor_type == CURSOR_TRANS);

  /* On a transaction cursor with 2 or fewer splits in single or double
   * mode, we just delete the whole transaction, kerblooie */
  if ((xaccTransCountSplits(trans) <= 2) &&
      ((style == REG_SINGLE_LINE) || (style == REG_DOUBLE_LINE)))
  {
    result = gnc_verify_dialog_parented(GTK_WINDOW(regData->window),
                                        TRANS_DEL2_MSG, GNC_F);

    if (!result)
      return;

    xaccSRDeleteCurrentTrans(regData->ledger->ledger);
    return;
  }

  /* At this point we are on a transaction cursor with more than 2 splits.
   * We give the user two choices: delete the whole transaction or delete
   * all the splits except the transaction split. */
  {
    DeleteType del_type;

    del_type = gnc_transaction_delete_query(GTK_WINDOW(regData->window));

    if (del_type == DELETE_CANCEL)
      return;

    if (del_type == DELETE_TRANS)
    {
      xaccSRDeleteCurrentTrans(regData->ledger->ledger);
      return;
    }

    if (del_type == DELETE_SPLITS)
    {
      xaccSREmptyCurrentTrans(regData->ledger->ledger);
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
  RegWindow *regData = (RegWindow *) data;
  Split *new_split;

  new_split = xaccSRDuplicateCurrent(regData->ledger->ledger);

  if (new_split == NULL)
    return;

  gnc_register_jump_to_split(regData, new_split);
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
  RegWindow *regData = (RegWindow *) data;

  xaccSRCancelCursorTransChanges(regData->ledger->ledger);
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
  unsigned int changed;

  changed = xaccSplitRegisterGetChangeFlag(regData->ledger->ledger);
  if (changed)
  {
    if (gnc_verify_dialog_parented
        (GTK_WINDOW(regData->window), TRANS_CHANGED_MSG, GNC_T))
      recordCB(regData->window, regData);
    else
      xaccSRCancelCursorSplitChanges(regData->ledger->ledger);
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
closeCB(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  gnc_register_check_close(regData);

  gnc_reg_save_size(regData);

  gtk_widget_destroy(regData->window);
}

/********************************************************************\
 * dateCB                                                           *
 *                                                                  *
 * Args:   widget - the widget that called us                       *
 *         data - regData - the data struct for this register       *
 * Return: none                                                     *
\********************************************************************/
static void
dateCB(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

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
helpCB(GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, HELP_STR, HH_REGWIN);
}

/************************** END OF FILE **************************/

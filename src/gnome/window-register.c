/*******************************************************************\
 * window-register.c -- the register window for GnuCash             *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
 * Copyright (C) 1998 Rob Browning <rlb@cs.utexas.edu>              *
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

/* hack alert -- much of the code in here should be chopped
 * it is moving to the device-independent MultiLedger.c file 
 */

#define _GNU_SOURCE

#include <gnome.h>

#include "top-level.h"

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
#include "messages.h"
#include "table-gnome.h"
#include "table-html.h"
#include "gnucash-sheet.h"
#include "global-options.h"
#include "util.h"


/** STRUCTS *********************************************************/

typedef struct _StyleData StyleData;
struct _StyleData
{
  RegWindow *regData;
  int style_code;
};

typedef struct _SortData SortData;
struct _SortData
{
  RegWindow *regData;
  int sort_code;
};

/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */
struct _RegWindow
{
  xaccLedgerDisplay * ledger;   

  /* Top level window */
  GtkWidget * window;

  GtkWidget * toolbar;

  GtkWidget * balance_label;
  GtkWidget * cleared_label;

  GtkWidget * start_date;
  GtkWidget * end_date;

  StyleData * style_cb_data;
  SortData  * sort_cb_data;

  GnucashRegister *reg;

  /* Do we close the ledger when the window closes? */
  gncBoolean close_ledger;
};


/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;


/** PROTOTYPES ******************************************************/
RegWindow *regWindowLedger(xaccLedgerDisplay *ledger);
static void regRefresh(xaccLedgerDisplay *ledger);
static void regDestroy(xaccLedgerDisplay *ledger);

static void closeRegWindow(GtkWidget * mw, RegWindow * regData);
static void gnc_register_check_close(RegWindow *regData);

static void startRecnCB(GtkWidget *w, gpointer data);
static void xferCB(GtkWidget *w, gpointer data);
static void editCB(GtkWidget *w, gpointer data);
static void helpCB(GtkWidget *w, gpointer data);
static void startAdjBCB(GtkWidget * w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);
static void closeCB(GtkWidget *w, gpointer data);

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

  return type;
}


static void
ledger_change_style_cb(GtkWidget *w, gint index, gpointer data)
{
  StyleData *style_data = (StyleData *) data;
  xaccLedgerDisplay *ld = style_data->regData->ledger;
  SplitRegister *reg = ld->ledger;
  int type = reg->type;

  type &= ~REG_STYLE_MASK;
  type |=  style_data->style_code;
  
  xaccConfigSplitRegister(reg, type);

  ld->dirty = 1;
  xaccLedgerDisplayRefresh(ld);
}

static GtkWidget *
gnc_build_ledger_style_menu(RegWindow *regData)
{
  GtkWidget *omenu;
  gint num_items;
  int style;
  gint i;

  static StyleData style_data[] =
  {
    { NULL, REG_SINGLE_LINE },
    { NULL, REG_DOUBLE_LINE },
    { NULL, REG_MULTI_LINE },
    { NULL, REG_SINGLE_DYNAMIC },
    { NULL, REG_DOUBLE_DYNAMIC }
  };

  static GNCOptionInfo style_items[] =
  {
    { "Single Line", "Show transactions on single lines",
      ledger_change_style_cb, NULL },
    { "Double Line", "Show transactions on two lines with more information",
      ledger_change_style_cb, NULL },
    { "Multi Line",  "Show transactions on multiple lines with one line "
                     "for each split in the transaction",
      ledger_change_style_cb, NULL },
    { "Auto Single", "Single line mode with multi-line cursor",
      ledger_change_style_cb, NULL },
    { "Auto Double", "Double line mode with multi-line cursor",
      ledger_change_style_cb, NULL }
  };

  num_items = sizeof(style_items) / sizeof(GNCOptionInfo);

  regData->style_cb_data = g_new0(StyleData, num_items);

  for (i = 0; i < num_items; i++)
  {
    regData->style_cb_data[i].regData = regData;
    regData->style_cb_data[i].style_code = style_data[i].style_code;

    style_items[i].user_data = &regData->style_cb_data[i];
  }

  omenu = gnc_build_option_menu(style_items, num_items);

  style = gnc_register_get_default_type(regData->ledger->ledger);
  style &= REG_STYLE_MASK;

  for (i = 0; i < num_items; i++)
    if (style == regData->style_cb_data[i].style_code)
    {
      gtk_option_menu_set_history(GTK_OPTION_MENU(omenu), i);
      break;
    }

  return omenu;
}


static void
gnc_ledger_sort_cb(GtkWidget *w, gint index, gpointer data)
{
  SortData *sortData = (SortData *) data;
  Query *query = sortData->regData->ledger->query;

  switch(sortData->sort_code)
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

  sortData->regData->ledger->dirty = 1;
  xaccLedgerDisplayRefresh(sortData->regData->ledger);
}

static GtkWidget *
gnc_build_ledger_sort_order_menu(RegWindow *regData)
{
  gint num_items;
  gint i;

  static SortData sort_data[] =
  {
    { NULL, BY_STANDARD },
    { NULL, BY_DATE },
    { NULL, BY_NUM },
    { NULL, BY_AMOUNT },
    { NULL, BY_MEMO },
    { NULL, BY_DESC }
  };

  static GNCOptionInfo sort_items[] =
  {
    { "Standard order", "Keep normal account order",
      gnc_ledger_sort_cb, NULL },
    { "Sort by date", "Sort by date, then num, then amount",
      gnc_ledger_sort_cb, NULL },
    { "Sort by num", "Sort by num, then date, then amount",
      gnc_ledger_sort_cb, NULL },
    { "Sort by amount", "Sort by amount, then date, then num",
      gnc_ledger_sort_cb, NULL },
    { "Sort by memo", "Sort by memo, then date, then num",
      gnc_ledger_sort_cb, NULL },
    { "Sort by description", "Sort by description, then date, then num",
      gnc_ledger_sort_cb, NULL }
  };

  num_items = sizeof(sort_items) / sizeof(GNCOptionInfo);

  regData->sort_cb_data = g_new0(SortData, num_items);

  for (i = 0; i < num_items; i++)
  {
    regData->sort_cb_data[i].regData = regData;
    regData->sort_cb_data[i].sort_code = sort_data[i].sort_code;

    sort_items[i].user_data = &regData->sort_cb_data[i];
  }

  return gnc_build_option_menu(sort_items, num_items);
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
gnc_register_set_date_range(RegWindow *regData)
{
  time_t start;
  time_t end;

  start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->start_date));
  end   = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->end_date));

  start = gnc_register_min_day_time(start);
  end   = gnc_register_max_day_time(end);

  xaccQuerySetDateRange(regData->ledger->query, start, end);
}

static void
gnc_register_date_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  assert(regData != NULL);
  assert(regData->ledger != NULL);
  assert(regData->ledger->query != NULL);

  gnc_register_set_date_range(regData);

  regData->ledger->dirty = 1;
  xaccLedgerDisplayRefresh(regData->ledger);
}

static void
gnc_register_today_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  assert(regData != NULL);

  gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->end_date), time(NULL));

  gnc_register_date_cb(widget, regData);
}

static GtkWidget *
gnc_register_create_tool_bar(RegWindow *regData)
{
  GtkWidget *hbox;

  hbox = gtk_hbox_new(FALSE, 5);

  /* Transaction Buttons */
  {
    GtkWidget *toolbar;
    GnomeUIInfo toolbar_info[] =
    {
      {
        GNOME_APP_UI_ITEM,
        "Record", "Record the current transaction",
        recordCB, regData, NULL,
        GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
        0, 0, NULL
      },
      {
        GNOME_APP_UI_ITEM,
        "Cancel", "Cancel the current transaction",
        cancelCB, regData, NULL,
        GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_UNDO,
        0, 0, NULL
      },
      {
        GNOME_APP_UI_ITEM,
        "Delete", "Delete the current transaction",
        deleteCB, regData, NULL,
        GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_TRASH,
        0, 0, NULL
      },
      GNOMEUIINFO_END
    };

    toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

    gnome_app_fill_toolbar(GTK_TOOLBAR(toolbar), toolbar_info, NULL);

    gtk_box_pack_start(GTK_BOX(hbox), toolbar, FALSE, FALSE, 0);

    regData->toolbar = toolbar;
  }

  {
    GtkWidget *frame;
    GtkWidget *balance_hbox;
    GtkWidget *vbox;
    GtkWidget *label;

    frame = gtk_frame_new(NULL);
    gtk_box_pack_end(GTK_BOX(hbox), frame, FALSE, FALSE, 0);

    balance_hbox = gtk_hbox_new(FALSE, 2);
    gtk_container_set_border_width(GTK_CONTAINER(balance_hbox), 4);
    gtk_container_add(GTK_CONTAINER(frame), balance_hbox);

    vbox = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(balance_hbox), vbox, FALSE, FALSE, 0);
    label = gtk_label_new(BALN_C_STR);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    label = gtk_label_new(CLEARED_C_STR);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    vbox = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(balance_hbox), vbox, FALSE, FALSE, 0);
    label = gtk_label_new("");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    regData->balance_label = label;
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    label = gtk_label_new("");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    regData->cleared_label = label;
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
  }

  {
    GtkWidget *vbox;

    vbox = gtk_vbox_new(TRUE, 0);
    gtk_box_pack_end(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);

    /* Style popup */
    gtk_box_pack_start(GTK_BOX(vbox),
		       gnc_build_ledger_style_menu(regData),
		       FALSE, FALSE, 0);

    /* Sort popup */
    gtk_box_pack_start(GTK_BOX(vbox),
		       gnc_build_ledger_sort_order_menu(regData),
		       FALSE, FALSE, 0);
  }

  {
    GtkWidget *calendar;
    GtkWidget *entry;
    GtkWidget *date;
    GtkWidget *label;
    GtkWidget *hbox_date;
    GtkWidget *vbox;
    GtkWidget *button;
    time_t time_val;

    hbox_date = gtk_hbox_new(FALSE, 2);
    gtk_box_pack_end(GTK_BOX(hbox), hbox_date, FALSE, FALSE, 0);

    vbox = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(hbox_date), vbox, FALSE, FALSE, 0);
    label = gtk_label_new("Start date:");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    label = gtk_label_new("End date:");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    vbox = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(hbox_date), vbox, FALSE, FALSE, 0);

    date = gnome_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(vbox), date, FALSE, FALSE, 0);
    calendar = GNOME_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
    entry = GNOME_DATE_EDIT(date)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "activate",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
    regData->start_date = date;

    date = gnome_date_edit_new(time(NULL), FALSE, FALSE);
    gtk_box_pack_start(GTK_BOX(vbox), date, FALSE, FALSE, 0);
    calendar = GNOME_DATE_EDIT(date)->calendar;
    gtk_signal_connect(GTK_OBJECT(calendar), "day_selected_double_click",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
    entry = GNOME_DATE_EDIT(date)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "activate",
		       GTK_SIGNAL_FUNC(gnc_register_date_cb), regData);
    regData->end_date = date;

    vbox = gtk_vbox_new(TRUE, 2);
    gtk_box_pack_start(GTK_BOX(hbox_date), vbox, FALSE, FALSE, 0);
    label = gtk_label_new("");
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    button = gtk_button_new_with_label("Today");
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_signal_connect(GTK_OBJECT(button), "clicked",
		       GTK_SIGNAL_FUNC(gnc_register_today_cb), regData);

    time_val = xaccQueryGetEarliestDateFound(regData->ledger->query);
    if (time_val < time(NULL))
      gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->start_date), time_val);

    gnc_register_set_date_range(regData);
  }

  return hbox;
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
gnc_register_create_menu_bar(RegWindow *regData)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group;

  GnomeUIInfo account_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      "_Edit Info...", "Edit account information",
      editCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "Re_concile...", "Reconcile this account",
      startRecnCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Transfer...", "Transfer funds from one account to another",
      xferCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "Adjust _Balance...", "Adjust the balance of the account",
      startAdjBCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Scrub"), N_("Scrub the account and its subaccounts clean"),
      gnc_register_scrub_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      "_Close", "Close this register window",
      closeCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo transaction_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      "_Record", "Record the current transaction",
      recordCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Cancel", "Cancel the current edit",
      cancelCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Delete", "Delete the current transaction",
      deleteCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      "_New", "Edit the new new transaction",
      new_trans_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Jump", "Jump to the corresponding transaction in "
               "the other account",
      jump_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo help_menu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Help..."), N_("Gnucash Help."),
      helpCB, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo register_window_menu[] =
  {
    GNOMEUIINFO_SUBTREE("_Account", account_menu),
    GNOMEUIINFO_SUBTREE("_Transaction", transaction_menu),
    GNOMEUIINFO_MENU_HELP_TREE(help_menu),
    GNOMEUIINFO_END
  };

  menubar = gtk_menu_bar_new();

  accel_group = gtk_accel_group_new();
  gtk_accel_group_attach(accel_group, GTK_OBJECT(regData->window));

  gnome_app_fill_menu(GTK_MENU_SHELL(menubar), register_window_menu,
  		      accel_group, TRUE, 0);

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
      "_Record", "Record the current transaction",
      recordCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Cancel", "Cancel the current edit",
      cancelCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Delete", "Delete the current transaction",
      deleteCB, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      "_New", "Edit the new new transaction",
      new_trans_cb, regData, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      "_Jump", "Jump to the corresponding transaction in "
               "the other account",
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
  /* First record the transaction. This will perform a refresh. */
  recordCB(GTK_WIDGET(reg), data);

  /* Now move down. */
  gnucash_register_goto_next_virt_row(reg);
}

static void
gnc_register_destroy_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;

  closeRegWindow(widget, regData);
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
  GtkWidget *register_window;
  GtkWidget *register_dock;
  GtkWidget *table_frame;

  xaccQuerySetMaxSplits(ledger->query, INT_MAX);
  xaccQuerySetSortOrder(ledger->query, BY_STANDARD, BY_NONE, BY_NONE);

  regData = (RegWindow *) (ledger->gui_hook);
  if (regData != NULL)
    return regData;

  regData = (RegWindow *) malloc(sizeof (RegWindow));

  ledger->gui_hook = (void *) regData;
  ledger->redraw = regRefresh;
  ledger->destroy = regDestroy;

  register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  register_dock = gnome_dock_new();
  gtk_container_add(GTK_CONTAINER(register_window), register_dock);

  regData->ledger = ledger;
  regData->close_ledger = GNC_T;
  regData->window = register_window;
  regData->sort_cb_data = NULL;
  regData->style_cb_data = NULL;

  { /* pick a window name */
    char *windowname;

    if (ledger->leader)
    {
      char * acc_name = gnc_ui_get_account_full_name(ledger->leader, ":");

      switch (ledger->type)
      {
	case GENERAL_LEDGER:
	case INCOME_LEDGER:
	  asprintf(&windowname, "%s General Ledger", acc_name);
	  break;
	case PORTFOLIO:
	  asprintf(&windowname, "%s Portfolio", acc_name);
	  break;
	default:
	  asprintf(&windowname, "%s Register", acc_name);
	  break;
      }

      g_free(acc_name);
    }
    else
      asprintf(&windowname, "%s", "General Ledger");

    assert(windowname != NULL);

    gtk_window_set_title(GTK_WINDOW(register_window), windowname);

    free(windowname);
  }

  /* Invoked when window is being destroyed. */
  gtk_signal_connect(GTK_OBJECT(regData->window), "destroy",
		     GTK_SIGNAL_FUNC (gnc_register_destroy_cb),
		     (gpointer) regData);

  /* The menu bar */
  {
    GtkWidget *dock_item;
    GtkWidget *menubar;

    dock_item = gnome_dock_item_new("menu", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    menubar = gnc_register_create_menu_bar(regData);
    gtk_container_set_border_width(GTK_CONTAINER(menubar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), menubar);

    gnome_dock_add_item (GNOME_DOCK(register_dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 0, 0, 0, TRUE);
  }

  /* The CreateTable will do the actual gui init, returning a widget */
  {
    GtkWidget *register_widget;
    GtkWidget *popup;

    table_frame = gtk_frame_new(NULL);
    gnome_dock_set_client_area(GNOME_DOCK(register_dock), table_frame);

    register_widget = gnucash_register_new(ledger->ledger->table);
    xaccCreateTable(register_widget, ledger->ledger);

    gtk_container_add(GTK_CONTAINER(table_frame), register_widget);

    regData->reg = GNUCASH_REGISTER(register_widget);

    gtk_signal_connect(GTK_OBJECT(register_widget), "activate_cursor",
                       GTK_SIGNAL_FUNC(gnc_register_record_cb), regData);

    popup = gnc_register_create_popup_menu(regData);
    gnucash_register_attach_popup(GNUCASH_REGISTER(register_widget),
                                  popup, regData);
  }

  /* The tool bar */
  {
    GtkWidget *dock_item;
    GtkWidget *toolbar;

    dock_item = gnome_dock_item_new("menu", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = gnc_register_create_tool_bar(regData);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);

    gnome_dock_add_item (GNOME_DOCK(register_dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_BOTTOM, 1, 0, 0, TRUE);
  }

  /* be sure to initialize the gui elements associated with the cursor */
  xaccConfigSplitRegister(ledger->ledger,
                          gnc_register_get_default_type(ledger->ledger));

  /* Allow grow, allow shrink, no auto-shrink */
  gtk_window_set_policy(GTK_WINDOW(register_window), TRUE, TRUE, FALSE);
  gtk_widget_show_all(register_window);

  ledger->dirty = 1;
  xaccLedgerDisplayRefresh(ledger);

  gnc_register_jump_to_blank(regData);

  return regData;
}


static void
gnc_reg_refresh_toolbar(RegWindow *regData)
{
  GtkToolbarStyle tbstyle = GTK_TOOLBAR_BOTH;
  char *style_string;

  if ((regData == NULL) || (regData->toolbar == NULL))
    return;

  style_string = gnc_lookup_multichoice_option("Register", 
                                               "Toolbar Buttons",
                                               "icons_and_text");

  if (safe_strcmp(style_string, "icons_and_text") == 0)
    tbstyle = GTK_TOOLBAR_BOTH;
  else if (safe_strcmp(style_string, "icons_only") == 0)
    tbstyle = GTK_TOOLBAR_ICONS;
  else if (safe_strcmp(style_string, "text_only") == 0)
    tbstyle = GTK_TOOLBAR_TEXT;

  gtk_toolbar_set_style(GTK_TOOLBAR(regData->toolbar), tbstyle);
}


static void
regRefresh(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);         

  gnc_reg_refresh_toolbar(regData);

  xaccSRLoadXferCells(ledger->ledger, ledger->leader);

  if (regData->window != NULL)
  {
    gtk_label_set_text(GTK_LABEL(regData->balance_label),
		       xaccPrintAmount(ledger->balance, PRTSYM));

    gtk_label_set_text(GTK_LABEL(regData->cleared_label),
                       xaccPrintAmount(ledger->clearedBalance, PRTSYM));
  }
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

    gtk_widget_destroy(regData->window);
  }
}


/********************************************************************\
 * closeRegWindow                                                   *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   widget  - the widget that called us                      *
 *         regData - the data struct for this register              *
 * Return: none                                                     *
\********************************************************************/
static void 
closeRegWindow(GtkWidget * widget, RegWindow *regData)
{
  if (regData->close_ledger)
    xaccLedgerDisplayClose(regData->ledger);

  if (regData->style_cb_data != NULL)
  {
    g_free(regData->style_cb_data);
    regData->style_cb_data = NULL;
  }

  if (regData->sort_cb_data != NULL)
  {
    g_free(regData->sort_cb_data);
    regData->sort_cb_data = NULL;
  }

  free(regData);

  DEBUG("closed RegWindow\n");
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
  time_t start, end;
  gboolean changed = FALSE;

  start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->start_date));
  end   = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->end_date));

  if (date < start)
  {
    gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->start_date), date);
    changed = TRUE;
  }

  if (date > end)
  {
    gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->end_date), date);
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

  gchar *whole = "Delete the whole transaction";
  gchar *splits = "Delete all the splits";
  gchar *usual =
    "This selection will delete the whole transaction. "
    "This is what you usually want.";

  gchar *warn =
    "Warning: Just deleting all the splits will make your "
    "account unbalanced. You probably shouldn't do this unless "
    "you're going to immediately add another split to bring things "
    "back into balance.";

  dialog = gnome_dialog_new("Delete Transaction",
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

  trans_button = gtk_radio_button_new_with_label(NULL, whole);
  gtk_object_set_user_data(GTK_OBJECT(trans_button), text);
  gtk_box_pack_start(GTK_BOX(vbox), trans_button, TRUE, TRUE, 0);

  gtk_signal_connect(GTK_OBJECT(trans_button), "toggled",
                     GTK_SIGNAL_FUNC(gnc_transaction_delete_toggle_cb), usual);

  group = gtk_radio_button_group(GTK_RADIO_BUTTON(trans_button));
  splits_button = gtk_radio_button_new_with_label(group, splits);
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
    buf = "Are you sure you want to delete the current transaction?";

    result = gnc_verify_dialog_parented(GTK_WINDOW(regData->window),
                                        buf, GNC_F);

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
        (GTK_WINDOW(regData->window),
         "The current transaction has been changed.\n"
         "Would you like to record it?", GNC_T))
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

  gtk_widget_destroy(regData->window);
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

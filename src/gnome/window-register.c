/*******************************************************************\
 * RegWindow.c -- the register window for xacc (X-Accountant)       *
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

  GtkWidget * balance_label;
  GtkWidget * cleared_label;

  GtkWidget * start_date;
  GtkWidget * end_date;

  StyleData * style_cb_data;
  SortData  * sort_cb_data;

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

static void startRecnCB(GtkWidget *w, gpointer data);
static void xferCB(GtkWidget *w, gpointer data);
static void editCB(GtkWidget *w, gpointer data);
static void helpCB(GtkWidget *w, gpointer data);
static void startAdjBCB(GtkWidget * w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);
static void closeCB(GtkWidget *w, gpointer data);


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


static void
ledger_change_style_cb(GtkWidget *w, gpointer data)
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
  gint num_items;
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
    { "Single Line", ledger_change_style_cb, NULL },
    { "Double Line", ledger_change_style_cb, NULL },
    { "Multi Line",  ledger_change_style_cb, NULL },
    { "Auto Single", ledger_change_style_cb, NULL },
    { "Auto Double", ledger_change_style_cb, NULL }
  };

  num_items = sizeof(style_items) / sizeof(GNCOptionInfo);

  regData->style_cb_data = g_new0(StyleData, num_items);

  for (i = 0; i < num_items; i++)
  {
    regData->style_cb_data[i].regData = regData;
    regData->style_cb_data[i].style_code = style_data[i].style_code;

    style_items[i].user_data = &regData->style_cb_data[i];
  }

  return gnc_build_option_menu(style_items, num_items);
}


static void
gnc_ledger_sort_cb(GtkWidget *w, gpointer data)
{
  SortData *sortData = (SortData *) data;
  Query *query = sortData->regData->ledger->query;

  switch(sortData->sort_code)
  {
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
    { NULL, BY_DATE },
    { NULL, BY_NUM },
    { NULL, BY_AMOUNT },
    { NULL, BY_MEMO },
    { NULL, BY_DESC }
  };

  static GNCOptionInfo sort_items[] =
  {
    { "Sort by date", gnc_ledger_sort_cb, NULL },
    { "Sort by num", gnc_ledger_sort_cb, NULL },
    { "Sort by amount", gnc_ledger_sort_cb, NULL },
    { "Sort by memo", gnc_ledger_sort_cb, NULL },
    { "Sort by description", gnc_ledger_sort_cb, NULL }
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


static void
gnc_register_date_cb(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  time_t start;
  time_t end;

  assert(regData != NULL);
  assert(regData->ledger != NULL);
  assert(regData->ledger->query != NULL);

  start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->start_date));
  end   = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->end_date));

  xaccQuerySetDateRange(regData->ledger->query, start, end);

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
  gtk_container_set_border_width(GTK_CONTAINER(hbox), 1);

  /* Transaction Buttons */
  {
    GtkWidget *toolbar;

    toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_TEXT);
    gtk_box_pack_start(GTK_BOX(hbox), toolbar, TRUE, TRUE, 0);
    gtk_widget_show(toolbar);

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
			    "Record", "Record the current transaction",
			    NULL, NULL, recordCB, regData);

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
			    "Cancel", "Cancel the current edit",
			    NULL, NULL, cancelCB, regData);

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
			    "Delete", "Delete the current transaction",
			    NULL, NULL, deleteCB, regData);
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
    regData->balance_label = label;
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    label = gtk_label_new("");
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
    gnc_register_date_cb(date, regData);
  }

  {
    GtkWidget *handle_box;

    handle_box = gtk_handle_box_new();
    gtk_container_add(GTK_CONTAINER(handle_box), hbox);
    gtk_widget_show_all(handle_box);

    return handle_box;
  }
}


static GtkWidget *
gnc_register_create_menu_bar(RegWindow *regData)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group;
  GtkWidget *handle_box;

  GnomeUIInfo register_menu[] =
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
    GNOMEUIINFO_SUBTREE("_Register", register_menu),
    GNOMEUIINFO_SUBTREE("_Transaction", transaction_menu),
    GNOMEUIINFO_MENU_HELP_TREE(help_menu),
    GNOMEUIINFO_END
  };

  menubar = gtk_menu_bar_new();
  gtk_widget_show(menubar);

  accel_group = gtk_accel_group_new();
  gtk_accel_group_attach(accel_group, GTK_OBJECT(regData->window));

  gnome_app_fill_menu(GTK_MENU_SHELL(menubar), register_window_menu,
		      accel_group, TRUE, 0);

  handle_box = gtk_handle_box_new();
  gtk_container_add(GTK_CONTAINER(handle_box), menubar);
  gtk_widget_show(handle_box);

  return handle_box;
}


static void
gnc_register_destroy_cb(GtkWidget *widget, gpointer data)
{
  closeRegWindow(widget, (RegWindow *) data);
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
  GtkWidget *register_vbox;
  GtkWidget *table_frame;

  xaccQuerySetMaxSplits(ledger->query, INT_MAX);

  regData = (RegWindow *) (ledger->gui_hook);
  if (regData != NULL)
    return regData;

  regData = (RegWindow *) malloc(sizeof (RegWindow));

  ledger->gui_hook = (void *) regData;
  ledger->redraw = regRefresh;
  ledger->destroy = regDestroy;

  register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  register_vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(register_window), register_vbox);

  regData->ledger = ledger;
  regData->close_ledger = GNC_T;
  regData->window = register_window;
  regData->sort_cb_data = NULL;
  regData->style_cb_data = NULL;

  { /* pick a window name */
    char *windowname;

    if (ledger->leader)
    {
      char * acc_name = xaccAccountGetName(ledger->leader);
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

  gtk_box_pack_start(GTK_BOX(register_vbox),
		     gnc_register_create_menu_bar(regData), FALSE, FALSE, 0);

  /* The CreateTable will do the actual gui init, returning a widget */
  {
    GtkWidget *register_widget;

    table_frame = gtk_frame_new(NULL);
    gtk_box_pack_start(GTK_BOX(register_vbox), table_frame, TRUE, TRUE, 0); 

    register_widget = gnucash_register_new(ledger->ledger->table);
    xaccCreateTable(register_widget, ledger->ledger);

    gtk_container_add(GTK_CONTAINER(table_frame), register_widget);
  }

  /* The toolbar on the bottom */
  gtk_box_pack_end(GTK_BOX(register_vbox),
		   gnc_register_create_tool_bar(regData), FALSE, FALSE, 0);

  /* be sure to initialize the gui elements associated with the cursor */
  xaccCreateCursor(ledger->ledger->table, ledger->ledger->single_cursor);
  xaccCreateCursor(ledger->ledger->table, ledger->ledger->double_cursor);
  xaccCreateCursor(ledger->ledger->table, ledger->ledger->trans_cursor);
  xaccCreateCursor(ledger->ledger->table, ledger->ledger->split_cursor);

  /* complete GUI initialization */
  {
    AccountGroup *group;
    Account *base_account;

    group = xaccGetAccountRoot(ledger->leader);
    base_account = ledger->leader;

    assert((group != NULL) && (base_account != NULL));

    xaccLoadXferCell(ledger->ledger->xfrmCell, group, base_account);
    xaccLoadXferCell(ledger->ledger->mxfrmCell, group, base_account);
  }

  {
    Table *table = ledger->ledger->table;
    CellBlock *header;
    short *widths;
    int list_width = 0;
    int i;
    
    /* The 0'th row of the handlers is defined as the header */
    header = table->handlers[0][0];
    widths = header->widths;
    
    for(i = 0; i < table->num_phys_cols; i++)
    {
      /* Widths are in units of characters, not pixels, so we have
	 this hack. It should be fixed later... */
      list_width += widths[i] * 5;
    }

    gtk_widget_set_usize(table_frame, list_width + 219, 500);
  }

  gtk_widget_show_all(register_window);

  ledger->dirty = 1;
  xaccLedgerDisplayRefresh(ledger);
  gnc_unset_busy_cursor(gnc_get_ui_data());

  return regData;
}


static void
regRefresh(xaccLedgerDisplay *ledger)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);         

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
  Transaction *trans;
  
  xaccSRSaveRegEntry(regData->ledger->ledger);

  trans = (Transaction *) (regData->ledger->ledger->user_huck);
  if (trans != NULL)
  {
    time_t start, end, new;
    gboolean changed = FALSE;

    xaccTransCommitEdit(trans);
    regData->ledger->ledger->user_huck = NULL;

    start = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->start_date));
    end   = gnome_date_edit_get_date(GNOME_DATE_EDIT(regData->end_date));
    new   = xaccTransGetDate(trans);

    if (new < start)
    {
      start = new;
      gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->start_date), start);
      changed = TRUE;
    }

    if (new > end)
    {
      end = new;
      gnome_date_edit_set_time(GNOME_DATE_EDIT(regData->end_date), end);
      changed = TRUE;
    }

    if (changed)
    {
      xaccQuerySetDateRange(regData->ledger->query, start, end);
      regData->ledger->dirty = 1;
    }
  }

  xaccSRRedrawRegEntry(regData->ledger->ledger);
  gnc_refresh_main_window ();
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
  Split *split, *s;
  Transaction *trans;
  int i, num_splits;
  Account *account, **affected_accounts;
  
  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit(regData->ledger->ledger);
  if (split == NULL)
    return;

  /* ask for user confirmation before performing permanent damage */
  {
    char *buf = NULL;
    gint result;

    trans = xaccSplitGetParent (split);
    asprintf(&buf, TRANS_DEL_MSG, xaccSplitGetMemo(split),
	     xaccTransGetDescription(trans));

    assert(buf != NULL);

    result = gnc_verify_dialog_parented(GTK_WINDOW(regData->window),
                                        buf, GNC_F);

    free(buf);

    if (!result)
      return;
  }

  /* If we just deleted the blank split, clean up. The user is
   * allowed to delete the blank split as a method for discarding any
   * edits they may have made to it. */
  if (split == regData->ledger->ledger->user_hook)
  {
    account = xaccSplitGetAccount(split);
    xaccAccountDisplayRefresh(account);
    return;
  }

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * their register windows after the deletion.
   */
  num_splits = xaccTransCountSplits(trans);
  affected_accounts = (Account **) malloc((num_splits + 1) *
					   sizeof(Account *));
  assert(affected_accounts != NULL);

  for (i=0; i < num_splits; i++) 
  {
    s = xaccTransGetSplit(trans, i);
    affected_accounts[i] = xaccSplitGetAccount(s);
  }
  affected_accounts[num_splits] = NULL;
  
  account = xaccSplitGetAccount(split);

  xaccTransBeginEdit(trans, 1);
  xaccAccountBeginEdit(account, 1);
  xaccSplitDestroy(split);
  xaccAccountCommitEdit(account);
  xaccTransCommitEdit(trans);

  gnc_account_list_ui_refresh(affected_accounts);

  free(affected_accounts);

  gnc_refresh_main_window ();
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
  Split * split;
  
  /* We're just cancelling the current split here, not the transaction */
  /* When cancelling edits, reload the cursor from the transaction */
  split = xaccSRGetCurrentSplit(regData->ledger->ledger);
  xaccSRLoadRegEntry(regData->ledger->ledger, split);
  xaccRefreshTableGUI(regData->ledger->ledger->table);
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
  helpWindow(GTK_WIDGET(gnc_get_ui_data()), HELP_STR, HH_REGWIN);
}

/************************** END OF FILE **************************/

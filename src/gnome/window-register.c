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
#include <stdio.h>
#include <string.h>

#include <gtk/gtk.h>
#include <assert.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"  /* hack alert -- do not include P.h files !! */
#include "Group.h"
#include "MultiLedger.h"
#include "LedgerUtils.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "window-reconcile.h"

#include "messages.h"

#include "table-html.h"
#include "Transaction.h"
#include "util.h"
#include "top-level.h"

/** STRUCTS *********************************************************/
/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */

struct _RegWindow {
  xaccLedgerDisplay * ledger;   
  DateCell *early_date_handler;
  DateCell *late_date_handler;
  int query_date;

  /* display widgets */
  GtkWidget *   dialog;
  GtkWidget *   reg;               /* The matrix widget...  */
  GtkWidget *   record;            /* the record transaction button */

};


/** GLOBALS *********************************************************/
extern GtkWidget *  toplevel;

/* This static indicates the debugging module that this .o belongs to.   */
static short module = MOD_GUI;

/** PROTOTYPES ******************************************************/
RegWindow *regWindowLedger( xaccLedgerDisplay *ledger);
static void regRefresh (xaccLedgerDisplay *ledger);
static void regDestroy (xaccLedgerDisplay *ledger);

static void closeRegWindow(GtkWidget * mw, gpointer data);

static void startRecnCB(GtkWidget *w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);
static void closeCB(GtkWidget *w, gpointer data);

#if 0
static void startAdjBCB( GtkWidget * mw, XtPointer cd, XtPointer cb );
#endif

/********************************************************************\
 * regWindowSimple                                                  *
 *   opens up a register window for Account account                 *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowSimple(Account *acc) {
  RegWindow *result = NULL;
  xaccLedgerDisplay * ledger = xaccLedgerDisplaySimple(acc);

  if(ledger) {
    result = regWindowLedger(ledger);
  }
  return result;
}

/********************************************************************\
 * regWindowAccGroup                                                *
 *   opens up a register window for a group of Accounts             *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowAccGroup(Account *acc) {
  RegWindow *result = NULL;
  xaccLedgerDisplay * ledger = xaccLedgerDisplayAccGroup(acc);  

  if(ledger) {
    result = regWindowLedger(ledger);
  }
  return result;
}


static gint
delete_event(GtkWidget *widget, gpointer data) {

  /* if you return FALSE in the "delete_event" signal handler,
   * GTK will emit the "destroy" signal.  Returning TRUE means
   * you don't want the window to be destroyed.
   * This is useful for popping up 'are you sure you want to quit ?'
   * type dialogs. */
  
  /* Change TRUE to FALSE and the main window will be destroyed with
   * a "delete_event". */
  closeRegWindow(widget, data);
  return (FALSE);
}

static void
destroy (GtkWidget *widget, gpointer data) {
  closeRegWindow(widget, data);
}

static void
ledger_change_style(RegWindow *regData, const int new_style) {
  xaccLedgerDisplay *ld = regData->ledger;
  SplitRegister *reg = ld->ledger;
  int typo = reg->type;

  typo &= ~REG_STYLE_MASK;
  typo |=  new_style;
  
  xaccConfigSplitRegister (reg, typo );
  ld->dirty = 1;
  xaccLedgerDisplayRefresh (ld);
}

static void
ledger_style_single_cb(GtkWidget *w, gpointer data) {
  ledger_change_style((RegWindow *) data, REG_SINGLE_LINE);
}
static void
ledger_style_double_cb(GtkWidget *w, gpointer data) {
  ledger_change_style((RegWindow *) data, REG_DOUBLE_LINE);
}
static void
ledger_style_multi_cb(GtkWidget *w, gpointer data) {
  ledger_change_style((RegWindow *) data, REG_MULTI_LINE);
}
static void
ledger_style_auto_single_cb(GtkWidget *w, gpointer data) {
  ledger_change_style((RegWindow *) data, REG_SINGLE_DYNAMIC);
}
static void
ledger_style_auto_double_cb(GtkWidget *w, gpointer data) {
  ledger_change_style((RegWindow *) data, REG_DOUBLE_DYNAMIC);
}



typedef struct {
  gchar *name;
  GtkSignalFunc cb;
  gpointer data;
} gncOptionMenuItem;

static GtkWidget *
build_option_menu (gncOptionMenuItem items[], gint num_items) {
  GtkWidget *omenu;
  GtkWidget *menu;
  GtkWidget *menu_item;
  GSList *group;
  gint i;

  omenu = gtk_option_menu_new();
      
  menu = gtk_menu_new();
  group = NULL;
  
  for (i = 0; i < num_items; ++i) {
    menu_item = gtk_radio_menu_item_new_with_label (group, items[i].name);
    gtk_signal_connect (GTK_OBJECT (menu_item), "activate",
                        (GtkSignalFunc) items[i].cb, items[i].data);
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menu_item));
    gtk_menu_append (GTK_MENU (menu), menu_item);
    gtk_widget_show (menu_item);
  }
  
  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);

  return(omenu);
}



/********************************************************************\
 * regWindowLedger                                                  *
 *   opens up a ledger window for the account list                  *
 *                                                                  *
 * Args:   parent   - the parent of this window                     *
 *         lead_acc - the account associated with this register     *
 *                     (may be null)                                *
 *         acc_list - the list of accounts to display in register   *
 *                     (may be null)                                *
 * Return: regData  - the register window instance                  *
\********************************************************************/
RegWindow *
regWindowLedger(xaccLedgerDisplay *ledger) {
  RegWindow   *regData = NULL;
  GtkWidget *reg = NULL;
  char *windowname;
  GtkWidget *register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *register_vbox = gtk_vbox_new(FALSE, 0);

  GtkWidget *table_frame = gtk_frame_new(NULL);
#if 0
  GtkWidget *table_frame = gtk_layout_new(NULL, NULL);
#endif
  /* Menu creation */

  regData = (RegWindow *) (ledger->gui_hook);
  if(regData) return(regData);

  regData = (RegWindow *) malloc (sizeof (RegWindow));

  ledger->gui_hook = (void *) regData;
  ledger->redraw = regRefresh;
  ledger->destroy = regDestroy;
  regData->ledger = ledger;

  regData->query_date = 1;
  regData->early_date_handler = xaccMallocDateCell();
  regData->late_date_handler = xaccMallocDateCell();

  /******************************************************************\
   * Start creating the Motif Widgets ...                           *
   \******************************************************************/
  
  /* pick a window name */
  if (ledger->leader) {
    char * acc_name = xaccAccountGetName (ledger->leader);
    switch (ledger->type) {
       case GENERAL_LEDGER:
       case INCOME_LEDGER:
         asprintf (&windowname, "%s General Ledger", acc_name);
         break;
       case PORTFOLIO:
         asprintf (&windowname, "%s Portfolio", acc_name);
         break;
       default:
         asprintf (&windowname, "%s Register", acc_name);
         break;
    }
  } else {
    windowname = "General Ledger";
  }
  assert(windowname);
  
  setBusyCursor(gnc_get_ui_data());
  
  gtk_box_pack_start(GTK_BOX(register_vbox), table_frame, TRUE, TRUE, 0); 
  regData->dialog = table_frame;
  
  gtk_window_set_title(GTK_WINDOW(register_window), windowname);
  
  /* when the window is given the "delete_event" signal (this is given
   * by the window manager (usually the 'close' option, or on the
   * titlebar), we ask it to call the delete_event () function
   * as defined above.  The data passed to the callback
   * function is NULL and is ignored in the callback. */
  gtk_signal_connect (GTK_OBJECT (regData->dialog), "delete_event",
                      GTK_SIGNAL_FUNC (delete_event), (gpointer) regData);
  
  /* here we connect the "destroy" event to a signal handler.  
   * This event occurs when we call gtk_widget_destroy() on the window,
   * or if we return 'FALSE' in the "delete_event" callback. */
  gtk_signal_connect (GTK_OBJECT (regData->dialog), "destroy",
                      GTK_SIGNAL_FUNC (destroy), (gpointer) regData);


  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* The CreateTable will do the actual gui init, 
   * returning a widget */
  reg = xaccCreateTable (ledger->ledger->table, regData->dialog);
  regData->reg = reg;
    
  /* be sure to initialize the gui elements associated with the cursor */
  xaccCreateCursor (ledger->ledger->table,  ledger->ledger->single_cursor);
  xaccCreateCursor (ledger->ledger->table,  ledger->ledger->double_cursor);
  xaccCreateCursor (ledger->ledger->table,  ledger->ledger->trans_cursor);
  xaccCreateCursor (ledger->ledger->table,  ledger->ledger->split_cursor);

  /* complete GUI initialization */
  {
    AccountGroup *grp;
    Account * base_acc;
    grp = xaccGetAccountRoot (ledger->leader);
    base_acc = ledger->leader;

    /* hmm .. if grp is null, we should probably assert, but we'll 
     * make one more stab at it ... 
     */ 
    if (!grp && ledger->displayed_accounts) {
      grp = xaccGetAccountRoot (ledger->displayed_accounts[0]);
      base_acc = ledger->displayed_accounts[0];
    }
    xaccLoadXferCell (ledger->ledger->xfrmCell, grp, base_acc);
    xaccLoadXferCell (ledger->ledger->mxfrmCell, grp, base_acc);
    /* xaccLoadXferCell (ledger->ledger->xtoCell, grp);  */
  }

#if 0
  /* traverse to the buttons, when leaving the table */
  xaccNextTabGroup (regData->ledger->table, buttonform);
#endif
  
  {
    Table *table = ledger->ledger->table;
    CellBlock *curs;
    unsigned char * alignments;
    short * widths;
    int num_header_rows = 0;
    int i;
    int list_width = 0;
    
    /* The 0'th row of the handlers is defined as the header */
    alignments = NULL;
    widths = NULL;
    curs = table->handlers[0][0];
    alignments = curs->alignments;
    widths = curs->widths;
    num_header_rows = curs->numRows;
    
    for(i = 0; i < table->num_phys_cols; i++) {
      /* Widths are in units of characters, not pixels, so we have
         this hack.  It should be fixed later... */
      list_width += widths[i] * 5;
    }
    gtk_widget_set_usize(regData->dialog, list_width + 219, 500);
  }

  /* Add controls at the bottom.  The controls are all in a handle
     box.  There's a toolbar with the command butons (which I think
     should go away or be optional once we have working menus, and
     there's the menu to select the viewing style.  */
  {
    GtkWidget *handle_box = gtk_handle_box_new();
    GtkWidget *controls_hbox = gtk_hbox_new(FALSE, 0);
    GtkWidget *toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,
                                         GTK_TOOLBAR_TEXT); 
    gncOptionMenuItem ledger_style_menu_items[] = {
      {"Single Line", ledger_style_single_cb, regData},
      {"Double Line", ledger_style_double_cb, regData},
      {"Multi Line",  ledger_style_multi_cb, regData},
      {"Auto Single", ledger_style_auto_single_cb, regData},
      {"Auto Double", ledger_style_auto_double_cb, regData},
    };
    const int nstyle_menu_items =
      sizeof(ledger_style_menu_items) / sizeof(ledger_style_menu_items[0]);    
    GtkWidget *style_menu = build_option_menu(ledger_style_menu_items,
                                              nstyle_menu_items);
    

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
                            "Record",
                            "Commit modifications to "
                            "the current transaction.",
                            "Commit modifications to "
                            "the current transaction.",
                            NULL,
                            GTK_SIGNAL_FUNC(recordCB), (gpointer) regData);

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
                            "Cancel",
                            "Cancel modifications to "
                            "the current transaction.",
                            "Cancel modifications to "
                            "the current transaction.",
                            NULL,
                            GTK_SIGNAL_FUNC(cancelCB), (gpointer) regData);

    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
                            "Delete",
                            "Delete the current transaction.",
                            "Delete the current transaction.",
                            NULL,
                            GTK_SIGNAL_FUNC(deleteCB), (gpointer) regData);
  
    gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
                            "Reconcile",
                            "Reconcile transactions with bank statement.",
                            "Reconcile transactions with bank statement.",
                            NULL,
                            GTK_SIGNAL_FUNC(startRecnCB),
                            (gpointer) regData);

     gtk_toolbar_append_item(GTK_TOOLBAR(toolbar),
                             "Close",
                             "Close the register.",
                             "Close the register.",
                             NULL,
                             GTK_SIGNAL_FUNC(closeCB),
                             (gpointer) regData);

    
    gtk_box_pack_start(GTK_BOX(controls_hbox), toolbar, FALSE, FALSE, 0); 
    gtk_box_pack_end(GTK_BOX(controls_hbox), style_menu, FALSE, FALSE, 0); 
    gtk_container_add(GTK_CONTAINER(handle_box), controls_hbox);
    gtk_box_pack_end(GTK_BOX(register_vbox), handle_box, FALSE, FALSE, 0); 

    gtk_widget_show(style_menu);
    gtk_widget_show(toolbar);
    gtk_widget_show(controls_hbox);
    gtk_widget_show(handle_box);
  }

  gtk_container_add(GTK_CONTAINER(register_window), register_vbox);

  gtk_widget_show(table_frame);  
  gtk_widget_show(register_vbox);
  gtk_widget_show(register_window);

  if(windowname) free(windowname);

  ledger->dirty = 1;
  xaccLedgerDisplayRefresh (ledger);
  unsetBusyCursor(gnc_get_ui_data());

  return regData;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

static void regRefresh (xaccLedgerDisplay *ledger)
{
  RegWindow *regData = (RegWindow *) (ledger->gui_hook);         

  if( NULL != regData->dialog ) {
    char *reglabel = NULL; 
    char *balance_str, *cleared_balance_str, *reconciled_balance_str;
    
    balance_str = strdup(xaccPrintAmount(ledger->balance, PRTSYM));
    cleared_balance_str = strdup(xaccPrintAmount(ledger->clearedBalance, PRTSYM));
    reconciled_balance_str =
      strdup(xaccPrintAmount(ledger->reconciledBalance, PRTSYM));
    
    asprintf(&reglabel, "%s (Reconciled: %s) (Cleared: %s) (Final: %s)",
             xaccAccountGetName(ledger->leader),
             reconciled_balance_str,
             cleared_balance_str,
             balance_str
             );
    
    //gtk_frame_set_label(GTK_FRAME(regData->dialog), reglabel);
    free(balance_str);
    free(cleared_balance_str);
    free(reconciled_balance_str);
    free(reglabel);
  }
}


/********************************************************************\
 * regDestroy()
 * It is enought to call just XtDestroy Widget.  Any allocated
 * memory will be freed by the close callbacks.
\********************************************************************/

static void
regDestroy (xaccLedgerDisplay *ledger)
{
   RegWindow *regData = (RegWindow *) (ledger->gui_hook);
   if (regData) gtk_widget_destroy(regData->dialog);
}


/********************************************************************\
 * closeRegWindow                                                   *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
closeRegWindow( GtkWidget * mw, gpointer data)
{
  RegWindow *regData = (RegWindow *)data;
  PINFO("closeRegWindow(): Closing register safely\n");
  xaccLedgerDisplayClose (regData->ledger);
  free(regData);
  DEBUG("closed RegWindow\n");
}

#if 0

/********************************************************************\
 * startAdjBCB -- open up the adjust balance window... called       *
 *   from the menubar.                                              *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
startAdjBCB( GtkWidget * mw, XtPointer cd, XtPointer cb )
{
  RegWindow *regData = (RegWindow *)cd;
  Account *acc;
  
  /* Must have number of accounts be one.  If not one,
   * then this callback should never have been called,
   * since the menu entry is supposed to be greyed out.
   */
  if (regData->leader) {
    acc = regData->leader;
  } else {
    if (1 != regData->numAcc) return;
    acc = regData->blackacc[0];
  }
  adjBWindow( toplevel, acc );
}

#endif

/********************************************************************\
 * startRecnCB -- open up the reconcile window... called from       *
 *   menubar.                                                       *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
startRecnCB(GtkWidget * w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  xaccLedgerDisplay *ledger = regData->ledger;
  Account *acc;

  /* Must have number of accounts be one.  If not one,
   * then this callback should never have been called,
   * since the menu entry is supposed to be greyed out.
   */
  if (ledger->leader) {
    acc = ledger->leader;
  } else {
    if (1 != ledger->numAcc) return;
    acc = ledger->displayed_accounts[0];
  }
  recnWindow(w, acc);
}

/********************************************************************\
 * recordCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void
recordCB( GtkWidget *w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  
  xaccSRSaveRegEntry   (regData->ledger->ledger);
  xaccSRRedrawRegEntry (regData->ledger->ledger);
}

/********************************************************************\
 * deleteCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/

static void
deleteCB(GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  Split * split, *s;
  Transaction *trans;
  char buf[BUFSIZE];
  int i, num_splits;
  Account *acc, **affected_accounts;
  
  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit (regData->ledger->ledger);
  if (NULL == split ) return;

  /* ask for user confirmation before performing 
   * permanent damage */
  trans = xaccSplitGetParent (split);
  sprintf (buf, TRANS_DEL_MSG, xaccSplitGetMemo (split), xaccTransGetDescription (trans));
  if (!verifyBox(buf)) return;

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * thier register windows after the deletion.
   */
  num_splits = xaccTransCountSplits (trans);
  g_message("%s:%d", G_GNUC_PRETTY_FUNCTION, num_splits);
  affected_accounts = (Account **) malloc ((num_splits+1) * sizeof (Account *));
  for (i=0; i<num_splits; i++) 
  {
    s = xaccTransGetSplit (trans, i);
    affected_accounts[i] = xaccSplitGetAccount (s);
  }
  affected_accounts[num_splits] = NULL;
  
  acc = xaccSplitGetAccount (split);
  xaccAccountBeginEdit (acc, 1);
  xaccTransBeginEdit (trans, 1);
  xaccSplitDestroy (split);
  xaccTransCommitEdit (trans);
  xaccAccountCommitEdit (acc);
  xaccAccListDisplayRefresh (affected_accounts);
  free (affected_accounts);
  refreshMainWindow ();
}

/********************************************************************\
 * cancelCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void
cancelCB( GtkWidget *w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  Split * split;
  
  /* when cancelling edits, reload the cursor from the transaction */
  split = xaccSRGetCurrentSplit (regData->ledger->ledger);
  xaccSRLoadRegEntry (regData->ledger->ledger, split);
  xaccRefreshTableGUI (regData->ledger->ledger->table);
}


/********************************************************************\
 * closeCB                                                         *
 *                                                                  *
 * Args:   widget - the widget that called us                           *
 *         data - regData - the data struct for this register         *
 * Return: none                                                     *
\********************************************************************/
static void
closeCB( GtkWidget *widget, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  gtk_widget_destroy (gtk_widget_get_toplevel (regData->dialog));
}

/********************************************************************\
\********************************************************************/

/************************** END OF FILE **************************/

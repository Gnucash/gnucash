/*-*-gnucash-c-*-****************************************************\
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
#include "Ledger.h"
#include "LedgerUtils.h"
#include "MainWindow.h"
#include "main.h"
#include "messages.h"
#include "RecnWindow.h"
#include "RegWindow.h"
#include "Transaction.h"
#include "util.h"
#include "xtutil.h"
#include "table-html.h"

/** STRUCTS *********************************************************/
/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */

struct _RegWindow {
  xaccLedgerDisplay * ledger;   

  /* display widgets */
  GtkWidget *   dialog;
  GtkWidget *   reg;               /* The matrix widget...  */
  GtkWidget *   record;            /* the record transaction button */

};


/** GLOBALS *********************************************************/
extern GtkWidget *  toplevel;

/** PROTOTYPES ******************************************************/
RegWindow *regWindowLedger( xaccLedgerDisplay *ledger);
static void regRefresh (xaccLedgerDisplay *ledger);
static void regDestroy (xaccLedgerDisplay *ledger);

static void closeRegWindow(GtkWidget * mw, gpointer data);

static void startRecnCB(GtkWidget *w, gpointer data);
static void deleteCB(GtkWidget *w, gpointer data);
static void recordCB(GtkWidget *w, gpointer data);
static void cancelCB(GtkWidget *w, gpointer data);

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
  RegWindow *retval = (RegWindow *) 1; /* for error case later */
  
  xaccLedgerDisplay * ledger;

  ledger = xaccLedgerDisplaySimple (acc);
  if(retval) retval = regWindowLedger (ledger);
  return retval;
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
  xaccLedgerDisplay * ledger; 
  RegWindow *retval;

  ledger = xaccLedgerDisplayAccGroup (acc);  
  retval = regWindowLedger (ledger);
  return retval;
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
regWindowLedger( xaccLedgerDisplay *ledger)                    
{
  RegWindow   *regData = NULL;
  GtkWidget *reg = NULL;
  char *windowname;
  GtkWidget *register_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *register_vbox = gtk_vbox_new(FALSE, 0);
  GtkWidget *table_frame = gtk_frame_new(NULL);

    fprintf(stderr, "regWindowLedger\n");

#if 0
  
  /******************************************************************\
   * Set up the menubar menu-items.                                 *
   * Menu structures must be initialized before any code is         *
   * executed.  Some compilers insist on this, although gcc is      *
   * freindly about this.  Note that some of the activityMenu       *
   * values are changed below. Be careful with which row is which.  *
   \******************************************************************/
  MenuItem reportMenu[] = {
    { SIMPLE_E_STR,         &xmPushButtonWidgetClass, 'S', NULL, NULL, True,
      NULL, (XtPointer)0,  (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL, (XtPointer)0,  (MenuItem *)NULL, 0 },
  };
  
  MenuItem activityMenu[] = {
    { TRANSFER_E_STR,       &xmPushButtonWidgetClass, 'T', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_TRNS,   (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { RECONCILE_E_STR,      &xmPushButtonWidgetClass, 'C', NULL, NULL, True,
      startRecnCB,       NULL,                 (MenuItem *)NULL, 0 },
    { ADJ_BALN_E_STR,       &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      startAdjBCB,       NULL,                 (MenuItem *)NULL, 0 },
    { REPORT_E_STR,         &xmPushButtonWidgetClass, 'R', NULL, NULL, False,
      NULL,              (XtPointer)0,         (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { DEL_TRANS_STR,        &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      deleteCB,          NULL,                 (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { CLOSE_WIN_STR,        &xmPushButtonWidgetClass, 'Q', NULL, NULL, True,
      destroyShellCB,    NULL,                 (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL,              (XtPointer)0,         (MenuItem *)NULL, 0 },
  };

  
  MenuItem helpMenu[] = {
    { ABOUT_E_STR,          &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL, 0 },
    { HELP_E_STR,           &xmPushButtonWidgetClass, 'H', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_REGWIN,(MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,          NULL,                 (MenuItem *)NULL, 0 },
    { LICENSE_E_STR,        &xmPushButtonWidgetClass, 'L', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL,          (XtPointer)0,         (MenuItem *)NULL, 0 },
  };
#endif

  
  regData = (RegWindow *) (ledger->gui_hook);
  if (regData) return (regData);

  regData = (RegWindow *) malloc (sizeof (RegWindow));

  ledger->gui_hook = (void *) regData;
  ledger->redraw = regRefresh;
  ledger->destroy = regDestroy;
  regData->ledger = ledger;

  /******************************************************************\
   * Start creating the Motif Widgets ...                           *
   \******************************************************************/
  
  /* pick a window name */
  if (ledger->leader) {
    char * acc_name = xaccAccountGetName (ledger->leader);
    switch (ledger->type) {
    case GENERAL_LEDGER:
    case INCOME_LEDGER:
      asprintf(&windowname, "%s (general ledger)", acc_name);
      break;
    case PORTFOLIO:
      asprintf(&windowname, "%s (portfolio)", acc_name);
      break;
    default:
      asprintf(&windowname, "%s (register)", acc_name);
      break;
    }
  } else {
    windowname = strdup("General Ledger");
  }
  assert(windowname);

  //setBusyCursor(parent);
  
  gtk_box_pack_start(GTK_BOX(register_vbox), table_frame, TRUE, TRUE, 0); 
  regData->dialog = table_frame;
  
  ///* Initialize callbacks */
    //gtk_signal_connect(GTK_OBJECT(toolBar[exit]), "clicked",
    //                 GTK_SIGNAL_FUNC (file_cmd_quit), NULL);



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



#if 0
  /* Create a PanedWindow Manager for the dialog box... the paned 
   * window is the parent of the two forms which comprise the two
   * areas of the dialog box */
  /* Important Note: the paned window MUST have traversal enabled,
   * otherwise the matrix cells will only get focus when the pointer
   * is in the cell, which basically defeats the whole idea of a tab 
   * group.  Put is another way: it is REALLY annoying to have to
   * put the mouse in the cell being edited. */

  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, regData->dialog,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNseparatorOn,   False,
                           XmNtraversalOn,   True,
                           XmNmarginHeight,  1,
                           XmNmarginWidth,   1,
                           XmNallowResize,   True,
                           XmNpaneMaximum,   200,
                           XmNpaneMinimum,   800,
                           NULL );
  
  /******************************************************************\
   * Setup the menubar at the top of the window                     *
  \******************************************************************/

  /* Be careful not to scramble the order of the rows.  */
  activityMenu[2].callback_data=(XtPointer)regData;
  activityMenu[3].callback_data=(XtPointer)regData;
  activityMenu[6].callback_data=(XtPointer)regData;
  activityMenu[8].callback_data=(XtPointer)(regData->dialog);  /* destroy callback */

  activityMenu[4].subitems = reportMenu;

  /* can't adjust the balance on a ledger window */
  if (1 < regData->numAcc) {
    activityMenu[2].sensitive = False;
    activityMenu[3].sensitive = False;
  }

  menubar = XmCreateMenuBar( pane, "menubar", NULL, 0 );  
  
  BuildMenu( menubar, XmMENU_PULLDOWN, ACTIVITIES_STR, 'A',
             False, 0, activityMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, HELP_STR,       'H', 
             False, 0, helpMenu );
  
  XtManageChild( menubar );
  
  frame = XtVaCreateWidget( "reg", 
                            xmFrameWidgetClass, pane,
                            NULL );

#endif
  
  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  /* The CreateTable will do the actual gui init, 
   * returning a widget */
  reg = xaccCreateTable (ledger->ledger->table, regData->dialog);
  regData->reg = reg;
    
  /* complete GUI initialization */
  {
    AccountGroup *grp;
    Account * base_acc;
    grp = xaccGetAccountRoot (ledger->leader);
    base_acc = ledger->leader;

    if (!grp) {
      grp = xaccGetAccountRoot (ledger->displayed_accounts[0]);
      base_acc = ledger->displayed_accounts[0];
    }
    xaccLoadXferCell (ledger->ledger->xfrmCell, grp, base_acc);
    xaccLoadXferCell (ledger->ledger->mxfrmCell, grp, base_acc);
    /* xaccLoadXferCell (ledger->ledger->xtoCell, grp);  */
  }

#if 0

  /******************************************************************\
   * The button area... also contains balance fields                *
  \******************************************************************/
  
  buttonform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass, pane,
				 XmNfractionBase,   6,
				 XmNresizable,      False,
                                 XmNtraversalOn,    True,
                                 XmNnavigationType, XmSTICKY_TAB_GROUP,
				 NULL );

  position = 0;                    /* puts the buttons in the right place */

  /* traverse to the buttons, when leaving the table */
  xaccNextTabGroup (regData->ledger->table, buttonform); 
  
  /* The "Record" button */
  widget = XtVaCreateManagedWidget( RECORD_STR,
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
                                    XmNnavigationType,     XmTAB_GROUP, 
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 recordCB, (XtPointer)regData );
  regData->record = widget;

  
  /* The "Cancel" button */
  position++;
  widget = XtVaCreateManagedWidget( CANCEL_STR, 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
                                    XmNnavigationType,     XmEXCLUSIVE_TAB_GROUP, 
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
                 cancelCB, (XtPointer)regData );
  
  /* the "close" button */
  position++;
  widget = XtVaCreateManagedWidget( CLOSE_STR, 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
                                    XmNnavigationType,     XmEXCLUSIVE_TAB_GROUP, 
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)(regData->dialog) );
  
  position += 2;
  
  /* Fix button area of the buttonform to its current size, and not let 
   * it resize. */
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( buttonform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
  /* The balance field labels: */ 
  widget = XtVaCreateManagedWidget( BALN_C_STR,
				    xmLabelGadgetClass,    buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    NULL );
  widget = XtVaCreateManagedWidget( CLEARED_C_STR,
				    xmLabelGadgetClass,    buttonform,
				    XmNtopAttachment,      XmATTACH_WIDGET,
				    XmNtopWidget,          widget,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    NULL );
  position++;
  
  /* and the balance fields: */
  widget = XtVaCreateManagedWidget( "text",
				    xmTextWidgetClass,     buttonform,
				    XmNeditable,           False,
				    XmNeditMode,           XmMULTI_LINE_EDIT,
				    XmNcursorPositionVisible, False,
				    XmNmarginHeight,       0,
				    XmNmarginWidth,        1,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
                                    XmNnavigationType,     XmNONE,  /* don't tab here! */
				    NULL );
  regData->balance = widget;
#endif
  
#if 0
  unsetBusyCursor( parent );
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
    gtk_widget_set_usize(regData->dialog, list_width + 80, 500);
  }

  /* Add controls at the bottom. */
  {
    GtkWidget *hb = gtk_handle_box_new();
    GtkWidget *toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,
                                         GTK_TOOLBAR_TEXT); 
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

    gtk_box_pack_end(GTK_BOX(register_vbox), hb, FALSE, FALSE, 0); 
    gtk_container_add(GTK_CONTAINER(hb), toolbar); 
    gtk_widget_show(toolbar);
    gtk_widget_show(hb);
  }

  gtk_container_add(GTK_CONTAINER(register_window), register_vbox);

  gtk_widget_show(table_frame);  
  gtk_widget_show(register_vbox);
  gtk_widget_show(register_window);

  if(windowname) free(windowname);

  ledger->dirty = 1;
  xaccLedgerDisplayRefresh (ledger);

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
    
    gtk_frame_set_label(GTK_FRAME(regData->dialog), reglabel);
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
  fprintf(stderr, "Closing register safely\n");
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
  
  xaccSRSaveRegEntry (regData->ledger->ledger);
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
  Split * split;
  Transaction *trans;
  char buf[BUFSIZE];
  int i, num_splits;
  Account **affected_accounts;
  
  /* get the current split based on cursor position */
  split = xaccSRGetCurrentSplit (regData->ledger->ledger);
  if (NULL == split ) return;

  /* ask for user confirmation before performing 
   * permanent damage */
  trans = xaccSplitGetParent (split);
  sprintf (buf, TRANS_DEL_MSG, xaccSplitGetMemo (split), xaccTransGetDescription (trans));
  if (!verifyBox(toplevel, buf)) return;

  /* make a copy of all of the accounts that will be  
   * affected by this deletion, so that we can update
   * thier register windows after the deletion.
   */
  num_splits = xaccTransCountSplits (trans);
  affected_accounts = (Account **) malloc (num_splits * sizeof (Account *));
  for (i=0; i<num_splits; i++) 
  {
    split = xaccTransGetSplit (trans, i);
    affected_accounts[i] = xaccSplitGetAccount (split);
  }

  xaccSplitDestroy (split);

  xaccAccListDisplayRefresh (affected_accounts);

  free (affected_accounts);
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
\********************************************************************/

#ifdef 0
/* fileBox not implemented in GNOME version yet */

static void
reportCB( GtkWidget *w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  char *outfile = fileBox( toplevel, OPEN_STR, "*.html" );

  if (!outfile) return;
  xaccTablePrintHTML (regData->ledger->ledger->table, outfile);
}

static void
webCB( GtkWidget *w, gpointer data)
{
  RegWindow *regData = (RegWindow *) data;
  /* hack alert -- make the port number configureable */
  xaccTableWebServeHTML (regData->ledger->ledger->table, 1080);
}

#endif


/************************** END OF FILE **************************/

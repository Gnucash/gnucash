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

#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>

#include <gtk/gtk.h>
#include <assert.h>

#include "config.h"

#include "Account.h"
//#include "AdjBWindow.h"
//#include "BuildMenu.h"
#include "Group.h"
#include "Ledger.h"
#include "LedgerUtils.h"
#include "MainWindow.h"
#include "main.h"
#include "messages.h"
//#include "RecnWindow.h"
#include "RegWindow.h"
#include "Transaction.h"
#include "util.h"
//#include "xtutil.h"

/** STRUCTS *********************************************************/
/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */

struct _RegWindow {
  Account *leader;            /* leading account                         */
  Account **blackacc;         /* The list of accounts shown here         */
  short   numAcc;             /* number of accounts in list              */

  short type;                 /* register display type, usually equal to *
                               * account type                            */

  BasicRegister *ledger;      /* main ledger window                      */

  /* display widgets */
  GtkWidget *   dialog;
  GtkWidget *   reg;               /* The matrix widget...  */
  GtkWidget *   balance;           /* The balance text field */
  GtkWidget *   record;            /* the record transaction button */

};


/** GLOBALS *********************************************************/
extern GtkWidget *  toplevel;

static RegWindow **regList = NULL;     /* single-account registers */
static RegWindow **ledgerList = NULL;  /* multiple-account registers */
static RegWindow **fullList = NULL;    /* all registers */

/** PROTOTYPES ******************************************************/
RegWindow * regWindowLedger(Account *lead, Account **acclist, int type);
void        accRefresh (Account *acc);
void        regRefresh (RegWindow *regData);

static void closeRegWindow(GtkWidget * mw, gpointer data);

#if 0

static void startRecnCB( GtkWidget * mw, XtPointer cd, XtPointer cb );
static void startAdjBCB( GtkWidget * mw, XtPointer cd, XtPointer cb );
static void recordCB( GtkWidget * mw, XtPointer cd, XtPointer cb );
static void deleteCB( GtkWidget * mw, XtPointer cd, XtPointer cb );
static void cancelCB( GtkWidget * mw, XtPointer cd, XtPointer cb );

#endif

/********************************************************************\
 * Ledger utilities                                                 *
\********************************************************************/

int 
ledgerListCount (RegWindow **list) {
  int n = 0;
  if (!list) return 0;
  while (list[n]) n++;
  return n;
}

/* ------------------------------------------------------ */

RegWindow ** 
ledgerListAdd (RegWindow **oldlist, RegWindow *addreg) {
  RegWindow **newlist;
  RegWindow *reg;
  int n;
  
  if (!addreg) return oldlist;
  
  n = ledgerListCount (oldlist);
  newlist = (RegWindow **) _malloc ((n+2) * sizeof (RegWindow *));
  
  n = 0;
  if (oldlist) {
    reg = oldlist[0];
    while (reg) {
      newlist[n] = reg;
      n++;
      reg = oldlist[n];
    }
    _free (oldlist);
  }
  newlist[n] = addreg;
  newlist[n+1] = NULL;
  
  return newlist;
}

/* ------------------------------------------------------ */

void
ledgerListRemove (RegWindow **list, RegWindow *delreg) {
  int n, i;
  
  if (!list) return;
  if (!delreg) return;
  
  n = 0;
  i = 0; 
  while (list[n]) {
    list[i] = list[n];
    if (delreg == list[n]) i--;
    i++;
    n++;
  }
  list[i] = NULL;
}

/* ------------------------------------------------------ */

int
ledgerIsMember (RegWindow *reg, Account * acc) {
  int n; 
  
  if (!acc) return 0;
  if (!reg) return 0;
  
  if (acc == reg->leader) return 1;
  
  if (! (reg->blackacc)) return 0; 
  
  n = 0;
  while (reg->blackacc[n]) {
    if (acc == reg->blackacc[n]) return 1;
    n++;
  }
  return 0;
}

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
  RegWindow *retval;
  int acc_type;
  int reg_type;
  
  acc_type = xaccAccountGetType (acc);
  
  /* translate between different enumerants */
  switch (acc_type) {
  case BANK:
    reg_type = BANK_REGISTER;
    break;
  case CASH:
    reg_type = CASH_REGISTER;
    break;
  case ASSET:
    reg_type = ASSET_REGISTER;
    break;
  case CREDIT:
    reg_type = CREDIT_REGISTER;
    break;
  case LIABILITY:
    reg_type = LIABILITY_REGISTER;
    break;
  case STOCK:
  case MUTUAL:
    reg_type = STOCK_REGISTER;
    break;
  case INCOME:
    reg_type = INCOME_REGISTER;
    break;
  case EXPENSE:
    reg_type = EXPENSE_REGISTER;
    break;
  case EQUITY:
    reg_type = EQUITY_REGISTER;
    break;
  }
  
  retval = regWindowLedger (acc, NULL, reg_type);
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
  RegWindow *retval;
  Account **list;
  int ledger_type;
  Account *le;
  int n;
  int acc_type, le_type;
  
  /* build a flat list from the tree */
  list = xaccGroupToList (acc);
  
  acc_type = xaccAccountGetType (acc);
  switch (acc_type) {
  case BANK:
  case CASH:
  case ASSET:
  case CREDIT:
  case LIABILITY:
    /* if any of the sub-accounts have STOCK or MUTUAL types,
     * then we must use the PORTFOLIO type ledger.  Otherise,
     * a plain old GEN_LEDGER will do. */
    ledger_type = GENERAL_LEDGER;
    
    le = list[0];
    n = 0;
    while (le) {
      le_type = xaccAccountGetType (le);
      if ((STOCK == le_type) || (MUTUAL == le_type)) {
        ledger_type = PORTFOLIO;
      }
      n++;
      le = list[n];
    }
    break;
    
  case STOCK:
  case MUTUAL:
    ledger_type = PORTFOLIO;
    break;
    
  case INCOME:
  case EXPENSE:
    ledger_type = INCOME_LEDGER;
    break;
    
  case EQUITY:
    ledger_type = GENERAL_LEDGER;
    break;
    
  default:
    PERR (" regWindowAccGroup(): unknown account type \n");
    _free (list);
    return NULL;
  }
  retval = regWindowLedger (acc, list, ledger_type);
  
  if (list) _free (list);
  
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
regWindowLedger(Account *lead_acc, Account **acclist, int ledger_type)
{
  RegWindow   *regData = NULL;
  //GtkWidget * menubar, pane, buttonform, frame, reg, widget;
  GtkWidget *reg = NULL;
  int    position=0;
  char *windowname;
  //char buf [BUFSIZE];

    fprintf(stderr, "regWindowLedger(%p, %p, %d)\n",
            lead_acc, acclist, ledger_type);

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

  /******************************************************************\
  \******************************************************************/

  /* the two macros below will search for a register windows associated
   * with the leading account.  If they exist, then they will be returned,
   * and that will be that.  If they do not exist, they will be created.
   *
   * There are two lists for lead-accounts: simple, single-account 
   * registers, which display one account only, and multiple-account
   * registers.  A leading account can have at most one of each. 
   * For a multiple-account register with a leader, all accounts
   * shown in the register are sub-accounts of the leader.
   *
   * A third possibility exists: a multiple-account register, with
   * no leader account.  In such a case, the list of accounts being
   * displayed have no particular relationshp to each other.  There
   * can be an arbitrary number of multiple-account leader-less
   * registers.
   */
  regData = NULL;
  if (lead_acc) {
    if (!acclist) {
      FETCH_FROM_LIST (RegWindow, regList, lead_acc, leader, regData);
    } else {
      FETCH_FROM_LIST (RegWindow, ledgerList, lead_acc, leader, regData);
    }
  }
  
  /* if regData is null, then no leader account was specified */
  if (!regData) {
    regData = (RegWindow *) malloc (sizeof (RegWindow));
    regData->leader = NULL;
  }
  
  /* count the number of accounts we are supposed to display,
   * and then, store them. */
  regData->numAcc = accListCount (acclist);
  regData->blackacc = accListCopy (acclist);
  regData->type = ledger_type;
  
  fullList = ledgerListAdd (fullList, regData);
  
  /******************************************************************\
   * Start creating the Motif Widgets ...                           *
   \******************************************************************/
  
  /* pick a window name */
  if (lead_acc) {
    char * acc_name = xaccAccountGetName (lead_acc);
    switch (regData->type) {
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
  } else {
    windowname = strdup("General Ledger");
  }
  assert(windowname);

  //setBusyCursor(parent);

  regData->dialog = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  

  gtk_window_set_title(GTK_WINDOW(regData->dialog), windowname);
  
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

  /* MallocBasicRegister will malloc & initialize the 
   * register but doesn't do the gui init */
  regData->ledger = xaccMallocBasicRegister (ledger_type);

  /* The CreateTable will do the actual gui init, 
   * returning a widget */
  //reg = xaccCreateTable (regData->ledger->table, frame, accRes[ledger_type]);
  reg = xaccCreateTable (regData->ledger->table, regData->dialog);
  
  regData->reg = reg;
    
  /* complete GUI initialization */
  {
    AccountGroup *grp;
    grp = xaccGetAccountRoot (regData->leader);
    if (!grp) grp = xaccGetAccountRoot (regData->blackacc[0]);
    xaccLoadXferCell (regData->ledger->xfrmCell, grp);
  }

#if 0

  XtManageChild (reg);
  XtManageChild (frame);
  XtManageChild (pane);


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
  
  XtManageChild(buttonform);
  
  /******************************************************************/
  XtManageChild(pane);

#endif
  
  regRefresh (regData);
  
#if 0

  XtPopup( regData->dialog, XtGrabNone );
  
  unsetBusyCursor( parent );

#endif
  
  {
    Table *table = regData->ledger->table;
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

  gtk_widget_show(regData->dialog);
  if(windowname) free(windowname);
  return regData;
}

/********************************************************************\
 * refresh only the indicated register window                       *
\********************************************************************/

void regRefresh (RegWindow *regData)
{

   /* The leader account is used by the register gui to
    * assign a default source account for a "blank split"
    * that is attached to the bottom of the register.
    * The "blank split" is what the user edits to create 
    * new splits and get them into the system.
    */

   /* hack alert -- should be recomputing the list of splits */
/* ???? */
   xaccLoadRegister (regData->ledger, 
                     xaccAccountGetSplitList (regData->leader),
                     regData->leader);


  /* hack alert -- this is incorrect for multi-account ledgers */
  if( NULL != regData->balance ) {
    char buf [BUFSIZE];
    char * amt;
    double prt_balance, prt_clearedBalance;
    prt_balance = xaccAccountGetBalance (regData->leader);
    prt_clearedBalance = xaccAccountGetClearedBalance (regData->leader);

    amt = xaccPrintAmount (prt_balance, PRTSYM);
    strcpy (buf, amt);
    strcat (buf, "\n");
    amt = xaccPrintAmount (prt_clearedBalance, PRTSYM);
    strcat (buf, amt);

    //XmTextSetString( regData->balance, buf );

  }
}

/********************************************************************\
 * refresh *all* register windows which contain this account        * 
\********************************************************************/

static void Refresh (Account *acc)
{
   RegWindow *regData;
   int n;

   if (!acc) return;

   /* find all registers whch contain this account */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      if (got_one) {
        /* hack alert -- should be recomputing the list of splits */
        /* ????? */
        regRefresh (regData);
      }
      n++;
      regData = fullList[n];
   }

   /* hack alert -- refesh adjbwindow too */
   //recnRefresh (acc); ??? RLB
}

/********************************************************************\
\********************************************************************/

static void grpRefresh (AccountGroup *grp)
{
   int i;
   Account *acc;
   AccountGroup *acc_children;
   int nacc;

   if (!grp) return;

   nacc = xaccGroupGetNumAccounts (grp);
   for (i=0; i<nacc; i++) {
      acc = xaccGroupGetAccount (grp, i);
      Refresh (acc);
      acc_children = xaccAccountGetChildren (acc);
      grpRefresh (acc_children); 
   }
}

void accRefresh (Account *acc)
{
   AccountGroup * root;
   root = xaccGetAccountRoot (acc);
   grpRefresh (root);
}

/********************************************************************\
 * xaccDestroyRegWindow()
 * It is enought to call just XtDestroy Widget.  Any allocated
 * memory will be freed by the close callbacks.
\********************************************************************/

void
xaccDestroyRegWindow (Account *acc)
{
   RegWindow *regData;
   int n;

   /* find the single-account window for this account, if any */
   FIND_IN_LIST (RegWindow, regList, acc, leader, regData);
   if (regData) gtk_widget_destroy(regData->dialog);

   /* find the multiple-account window for this account, if any */
   FIND_IN_LIST (RegWindow, ledgerList, acc, leader, regData);
   if (regData) gtk_widget_destroy(regData->dialog);

   /* cruise throught the miscellanous account windows */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      if (got_one) gtk_widget_destroy(regData->dialog);
      n++;
      regData = fullList[n];
   }
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
  Account *acc = regData->leader;
  
  /* Save any unsaved changes */
  xaccSaveRegEntry (regData->ledger);

  xaccDestroyBasicRegister (regData->ledger);
  
  /* whether this is a single or multi-account window, remove it */
  REMOVE_FROM_LIST (RegWindow, regList, acc, leader);
  REMOVE_FROM_LIST (RegWindow, ledgerList, acc, leader);

  ledgerListRemove (fullList, regData);

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
startRecnCB( GtkWidget * mw, XtPointer cd, XtPointer cb )
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
  recnWindow( toplevel, acc );
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
recordCB( GtkWidget * mw, XtPointer cd, XtPointer cb )
{
  RegWindow *regData = (RegWindow *)cd;
  
  xaccSaveRegEntry (regData->ledger);
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
deleteCB( GtkWidget * mw, XtPointer cd, XtPointer cb )
{
  RegWindow *regData = (RegWindow *)cd;
  Split * split;
  Transaction *trans;
  char buf[BUFSIZE];
  int i, num_splits;
  Account **affected_accounts;
  
  /* get the current split based on cursor position */
  split = xaccGetCurrentSplit (regData->ledger);
  if (NULL == split ) return;

  /* ask for user confirmation before performing 
   * permanent damage */
  trans = xaccSplitGetParent (split);
  sprintf (buf, TRANS_DEL_MSG, xaccTransGetDescription (trans));
  if (!verifyBox (toplevel, buf)) return;

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

  for (i=0; i<num_splits; i++) 
  {
    accRefresh (affected_accounts[i]);
  }
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
cancelCB( GtkWidget * mw, XtPointer cd, XtPointer cb )
{
   RegWindow *regData = (RegWindow *)cd;
   Split * split;

  /* when cancelling edits, reload the cursor from the transaction */
  split = xaccGetCurrentSplit (regData->ledger);
  xaccLoadRegEntry (regData->ledger, split);
  xaccRefreshTableGUI (regData->ledger->table);
}
#endif /* 0 */

/************************** END OF FILE *************************/

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c
  c-indentation-style: gnu
  eval: (c-set-offset 'substatement-open 0)
  End:
*/

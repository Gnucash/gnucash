/*******************************************************************\
 * RegWindow.c -- the register window for xacc (X-Accountant)       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997 Linas Vepstas                                 *
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


#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelGP.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>

#include "config.h"

#include "Account.h"
#include "AdjBWindow.h"
#include "BuildMenu.h"
#include "Data.h"
#include "Ledger.h"
#include "LedgerUtils.h"
#include "MainWindow.h"
#include "main.h"
#include "messages.h"
#include "RecnWindow.h"
#include "Transaction.h"
#include "util.h"
#include "xtutil.h"

/* enumerate different ledger types */
enum {
   GEN_LEDGER = NUM_ACCOUNT_TYPES,
   INC_LEDGER = NUM_ACCOUNT_TYPES + 1,
   PORTFOLIO  = NUM_ACCOUNT_TYPES + 2,
};


/** STRUCTS *********************************************************/
/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */

typedef struct _RegWindow {
  Account *leader;            /* leading account                         */
  Account **blackacc;         /* The list of accounts shown here         */
  short   numAcc;             /* number of accounts in list              */

  short type;                 /* register display type, usually equal to *
                               * account type                            */

  BasicRegister *ledger;      /* main ledger window                      */

  /* display widgets */
  Widget   dialog;
  Widget   reg;               /* The matrix widget...                    */
  Widget   balance;           /* The balance text field                  */
  Widget   record;            /* the record transaction button           */

} RegWindow;


/** GLOBALS *********************************************************/
extern Widget  toplevel;

static RegWindow **regList = NULL;     /* single-account registers */
static RegWindow **ledgerList = NULL;  /* multiple-account registers */
static RegWindow **fullList = NULL;    /* all registers */

/** PROTOTYPES ******************************************************/
RegWindow * regWindowLedger( Widget parent, Account *lead, Account **acclist, int type);
void        regRefresh (Account *acc);

static void closeRegWindow( Widget mw, XtPointer cd, XtPointer cb );
static void startRecnCB( Widget mw, XtPointer cd, XtPointer cb );
static void startAdjBCB( Widget mw, XtPointer cd, XtPointer cb );
static void recordCB( Widget mw, XtPointer cd, XtPointer cb );
static void deleteCB( Widget mw, XtPointer cd, XtPointer cb );
static void cancelCB( Widget mw, XtPointer cd, XtPointer cb );

/********************************************************************\
 * Ledger utilities                                                 *
\********************************************************************/

int 
ledgerListCount (RegWindow **list)
{
   int n = 0;
   if (!list) return 0;
   while (list[n]) n++;
   return n;
}

/* ------------------------------------------------------ */

RegWindow ** 
ledgerListAdd (RegWindow **oldlist, RegWindow *addreg)
{
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
ledgerListRemove (RegWindow **list, RegWindow *delreg)
{
   RegWindow *reg;
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
ledgerIsMember (RegWindow *reg, Account * acc)
{
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
regWindowSimple( Widget parent, Account *acc )
  {
  RegWindow *retval;

  retval = regWindowLedger (parent, acc, NULL, acc->type);
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
regWindowAccGroup( Widget parent, Account *acc )
  {
  RegWindow *retval;
  Account **list;
  int ledger_type;
  Account *le;
  int n;

  /* build a flat list from the tree */
  list = xaccGroupToList (acc);

  switch (acc->type) {
    case BANK:
    case CASH:
    case ASSET:
    case CREDIT:
    case LIABILITY:
       /* if any of the sub-accounts have STOCK or MUTUAL types,
        * then we must use the PORTFOLIO type ledger.  Otherise,
        * a plain old GEN_LEDGER will do. */
       ledger_type = GEN_LEDGER;

       le = list[0];
       n = 0;
       while (le) {
          if ((STOCK == le->type) || (MUTUAL == le->type)) {
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
       ledger_type = INC_LEDGER;
       break;

    case EQUITY:
       ledger_type = GEN_LEDGER;
       break;

    default:
      PERR (" regWindowAccGroup(): unknown account type \n");
      _free (list);
      return NULL;
  }
  retval = regWindowLedger (parent, acc, list, ledger_type);

  if (list) _free (list);

  return retval;
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
regWindowLedger( Widget parent, Account *lead_acc, Account **acclist, int ledger_type )
  {
  RegWindow   *regData = NULL;
  Widget menubar, pane, buttonform, frame, reg, widget;
  int    position=0;
  char *windowname;
  char buf [BUFSIZE];

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
    switch (regData->type) {
       case GEN_LEDGER:
       case INC_LEDGER:
         sprintf (buf, "%s General Ledger", lead_acc->accountName);
         break;
       case PORTFOLIO:
         sprintf (buf, "%s Portfolio", lead_acc->accountName);
         break;
       default:
         sprintf (buf, "%s Register", lead_acc->accountName);
         break;
    }
    windowname = buf;
  } else {
    windowname = "General Ledger";
  }

  setBusyCursor( parent );

  regData->dialog =
    XtVaCreatePopupShell( "dialog", 
                          xmDialogShellWidgetClass, parent,
                          XmNdeleteResponse,   XmDESTROY,
                          XmNtitle,            windowname,
                          /*
                           * Let the window find it's own size, 
                           * based on the size of the fonts.
                           * XmNwidth,            395,
                           * XmNheight,           400,
                           * XmNminWidth,         495,
                           * XmNmaxWidth,         495,
                           * XmNminHeight,        500,
                           */
                          /* XmNresizable,        False, */
                          /* XmNallowShellResize, False, */
                          XmNtransient,        FALSE,  /* allow window to be repositioned */
                          NULL );
  
  XtAddCallback( regData->dialog, XmNdestroyCallback, 
                 closeRegWindow, (XtPointer)regData );
  
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
  if (1 != regData->numAcc) {
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
  
  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

  strcpy (buf, "reg");
  strcat(buf,accRes[regData->type]);

  regData->ledger = xaccMallocBasicRegister ();
  reg = xaccCreateTable (regData->ledger->table, frame, buf);
  
  regData->reg     = reg;
    
  /* complete GUI initialization */
  {
    AccountGroup *grp;
    grp = xaccGetAccountRoot (regData->leader);
    if (!grp) grp = xaccGetAccountRoot (regData->blackacc[0]);
    xaccLoadXferCell (regData->ledger->xfrmCell, grp);
  }

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
  
  /* hack alert -- if no leader, should be refreshing from list */
  regRefresh (regData->leader);
  
  XtPopup( regData->dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  
  return regData;
}

/********************************************************************\
 * refresh *all* register windows which contain this account        * 
\********************************************************************/

void regRefresh (Account *acc)
{
   RegWindow *regData;
   int n;

   if (!acc) return;

   xaccRecomputeBalance (acc);

   /* find all registers whch contain this account */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      if (got_one) {
        /* hack alert -- should be recomputing the list of splits */
        /* and problbly the balance too */
        xaccLoadRegister (regData->ledger, acc->splits);
      }
      n++;
      regData = fullList[n];
   }
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
   if (regData) XtDestroyWidget(regData->dialog);

   /* find the multiple-account window for this account, if any */
   FIND_IN_LIST (RegWindow, ledgerList, acc, leader, regData);
   if (regData) XtDestroyWidget(regData->dialog);

   /* cruise throught the miscellanous account windows */
   n = 0;
   regData = fullList[n];
   while (regData) {
      int got_one;

      got_one = ledgerIsMember (regData, acc);
      if (got_one) XtDestroyWidget(regData->dialog);
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
closeRegWindow( Widget mw, XtPointer cd, XtPointer cb )
{
  RegWindow *regData = (RegWindow *)cd;
  Account *acc = regData->leader;
  
  /* Save any unsaved changes */
  xaccSaveRegEntry (regData->ledger);
  
  /* whether this is a single or multi-account window, remove it */
  REMOVE_FROM_LIST (RegWindow, regList, acc, leader);
  REMOVE_FROM_LIST (RegWindow, ledgerList, acc, leader);

  ledgerListRemove (fullList, regData);

  free(regData);
  DEBUG("closed RegWindow\n");
}

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
startAdjBCB( Widget mw, XtPointer cd, XtPointer cb )
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
startRecnCB( Widget mw, XtPointer cd, XtPointer cb )
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
recordCB( Widget mw, XtPointer cd, XtPointer cb )
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
deleteCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  Transaction *trans;
  int currow;
  
#ifdef JUNK
  if( NULL != trans)
    {
    char buf[BUFSIZE];
    sprintf (buf, TRANS_DEL_MSG, trans->description);
    
    if( verifyBox( toplevel, buf ) )
      {
      Account * cred = (Account *) (trans->credit_split.acc);
      Account * deb = (Account *) (trans->debit);
      
      /* remove the transaction from both accounts */
      REMOVE_TRANS (cred, trans);
      REMOVE_TRANS (deb, trans);

      RECALC_BALANCE (deb);
      RECALC_BALANCE (cred);

      REFRESH_REGISTER (deb);
      REFRESH_REGISTER (cred);

      /* Delete the transaction */
      freeTransaction (trans);
      }
    }
#endif
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
cancelCB( Widget mw, XtPointer cd, XtPointer cb )
{
   RegWindow *regData = (RegWindow *)cd;
   Split * split;

  /* when cancelling edits, reload the cursor from the transaction */
  split = xaccGetCurrentSplit (regData->ledger);
  xaccLoadRegEntry (regData->ledger, split);
  xaccRefreshTableGUI (regData->ledger->table);
}

/************************** END OF FILE *************************/

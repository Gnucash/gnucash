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
#include "date.h"
#include "main.h"
#include "MainWindow.h"
#include "Ledger.h"
#include "LedgerUtils.h"
#include "QuickFill.h"
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
  Account **blackacc;         /* The list of accounts associated with this regwin */
  short   numAcc;             /* number of accounts in list */

  /* display widgets */
  Widget   dialog;
  Widget   reg;               /* The matrix widget...                    */
  Widget   balance;           /* The balance text field                  */
  Widget   record;            /* the record transaction button           */
  unsigned short changed;     /* bitmask of fields that have changed in  *
                               * transaction currEntry                   */
  unsigned short currEntry;   /* to keep track of last edited transaction*/

  short type;                 /* register display type, usually equal to *
                               * account type                            */

  BasicRegister *ledger;
} RegWindow;



/** PROTOTYPES ******************************************************/
RegWindow * regWindowLedger( Widget parent, Account **acclist, int type);
void regRefresh (RegWindow * regData);

static void closeRegWindow( Widget mw, XtPointer cd, XtPointer cb );
static void startRecnCB( Widget mw, XtPointer cd, XtPointer cb );
static void startAdjBCB( Widget mw, XtPointer cd, XtPointer cb );
static void recordCB( Widget mw, XtPointer cd, XtPointer cb );
static void deleteCB( Widget mw, XtPointer cd, XtPointer cb );
static void cancelCB( Widget mw, XtPointer cd, XtPointer cb );


/** GLOBALS *********************************************************/
extern Widget  toplevel;

#define MOD_NONE  0x00
#define MOD_DATE  0x01
#define MOD_NUM   0x02
#define MOD_DESC  0x04
#define MOD_RECN  0x08
#define MOD_AMNT  0x10
#define MOD_SHRS  0x20
#define MOD_PRIC  0x40
#define MOD_MEMO  0x80
#define MOD_ACTN  0x100
#define MOD_XFRM  0x200
#define MOD_XTO   0x400
#define MOD_NEW   0x800
#define MOD_ALL   0xfff


/********************************************************************\
 * xaccDestroyRegWindow()
 * It is enought to call just XtDestroy Widget.  Any allocated
 * memory will be freed by the close callbacks.
\********************************************************************/

void
xaccDestroyRegWindow (RegWindow *regData)
{
   if (!regData) return;
   XtDestroyWidget(regData->dialog);
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
  Account *acclist[2];

  acclist[0] = acc;
  acclist[1] = NULL;

  /* don't allow more than one regster window for this account */
  /* hack alert -- we should raise this window to the top, if
   * we are called, and the register already exists */
  if (acc->regData) return acc->regData;

  retval = regWindowLedger (parent, acclist, acc->type);
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

  /* don't allow more than one ledger window for this account */
  /* hack alert -- we should raise this window to the top, if
   * we are called, and the ledger already exists */
  if (acc->regLedger) return acc->regLedger;

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
  retval = regWindowLedger (parent, list, ledger_type);
  acc->regLedger = retval;

  if (list) _free (list);

  return retval;
  }

/********************************************************************\
 * regWindowLedger                                                  *
 *   opens up a ledger window for the account list                  *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindowLedger( Widget parent, Account **acclist, int ledger_type )
  {
  RegWindow   *regData;
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
    { "Simple...",          &xmPushButtonWidgetClass, 'S', NULL, NULL, True,
      NULL, (XtPointer)0,  (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL, (XtPointer)0,  (MenuItem *)NULL, 0 },
  };
  
  MenuItem activityMenu[] = {
    { "Transfer...",        &xmPushButtonWidgetClass, 'T', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_TRNS,   (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { "Reconcile...",       &xmPushButtonWidgetClass, 'C', NULL, NULL, True,
      startRecnCB,       NULL,                 (MenuItem *)NULL, 0 },
    { "Adjust Balance...",  &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      startAdjBCB,       NULL,                 (MenuItem *)NULL, 0 },
    { "Report",             &xmPushButtonWidgetClass, 'R', NULL, NULL, False,
      NULL,              (XtPointer)0,         (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { "Delete Transaction", &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      deleteCB,          NULL,                 (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,              NULL,                 (MenuItem *)NULL, 0 },
    { "Close Window",       &xmPushButtonWidgetClass, 'Q', NULL, NULL, True,
      destroyShellCB,    NULL,                 (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL,              (XtPointer)0,         (MenuItem *)NULL, 0 },
  };

  
  MenuItem helpMenu[] = {
    { "About...",           &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL, 0 },
    { "Help...",            &xmPushButtonWidgetClass, 'H', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_REGWIN,(MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,          NULL,                 (MenuItem *)NULL, 0 },
    { "License...",         &xmPushButtonWidgetClass, 'L', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL, 0 },
    { NULL,                 NULL,                      0,  NULL, NULL, False,
      NULL,          (XtPointer)0,         (MenuItem *)NULL, 0 },
  };
  

  /******************************************************************\
   * create regData, compute register display type      *
  \******************************************************************/

  setBusyCursor( parent );
  
  regData = (RegWindow *)_malloc(sizeof(RegWindow));
  regData->changed     = MOD_NONE;   /* Nothing has changed yet! */
  regData->currEntry   = 0;

  /* count the number of accounts we are supposed to display,
   * and then, store them. */
  regData->numAcc = accListCount (acclist);
  regData->blackacc = accListCopy (acclist);

  if (0 == regData->numAcc) {
    /* this is pretty much an error condition. bail out. */
    unsetBusyCursor( parent );
    _free (regData);
    return NULL;
  }

  regData->type = ledger_type;

  if (1 == regData->numAcc) {
    /* avoid having two open registers for one account */
    regData->blackacc[0]->regData = regData;    
    windowname = regData->blackacc[0]->accountName;
  } else {

    switch (regData->type) {
       case GEN_LEDGER:
       case INC_LEDGER:
         sprintf (buf, "%s General Ledger", regData->blackacc[0]->accountName);
         break;
       case PORTFOLIO:
         sprintf (buf, "%s Portfolio", regData->blackacc[0]->accountName);
         break;
    }
    windowname = buf;

    /* associate register with account, so that we can do consistent
     * updates */
    regData->blackacc[0]->regLedger = regData;    
  }
  ledgerListAddList (regData->blackacc, regData);

  /******************************************************************\
   * Start creating the Motif Widgets ...                           *
  \******************************************************************/

  regData->dialog =
    XtVaCreatePopupShell( "dialog", 
                          xmDialogShellWidgetClass, parent,
                          XmNdeleteResponse,   XmDESTROY,
                          XmNtitle,            windowname,
                          /*
                           * Let the window find ti's own size, 
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
  
  BuildMenu( menubar, XmMENU_PULLDOWN, "Activities", 'A',
             False, 0, activityMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Help",       'H', 
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

  /* traverse to the buttons, when leaving the table */
  xaccNextTabGroup (regData->ledger->table, buttonform);


  position = 0;                    /* puts the buttons in the right place */
  
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
                                    XmNnavigationType,     XmTAB_GROUP,
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
                                    XmNnavigationType,     XmTAB_GROUP,
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
  
  regRefresh( regData );
  
  XtPopup( regData->dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  
  return regData;
  }

/********************************************************************\
\********************************************************************/

void regRefresh (RegWindow * regData)
{
   xaccLoadRegister (regData->ledger, regData->blackacc[0]->splits);
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
  
  /* Save any unsaved changes */
  /* hack alert */
  /* regSaveTransaction( regData, regData->currEntry ); */
  
  regData->blackacc[0]->regData = NULL;
  regData->blackacc[0]->regLedger = NULL;

  ledgerListRemoveList (regData->blackacc, regData);
  _free(regData);
  
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
  if (1 != regData->numAcc) return;

  acc = regData->blackacc[0];
  if( acc->adjBData == NULL )
    acc->adjBData = adjBWindow( toplevel, acc );
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
  if (1 != regData->numAcc) return;

  acc = regData->blackacc[0];
  if( acc->recnData == NULL )
    acc->recnData = recnWindow( toplevel, acc );
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
  
  XbaeMatrixCommitEdit( regData->reg, False );
  /* hack alert */
  /* regSaveTransaction( regData, regData->currEntry ); */
  regData->changed = MOD_NONE;
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
  XbaeMatrixCancelEdit( regData->reg, False );
  }

/************************** END OF FILE *************************/

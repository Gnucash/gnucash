/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
 * Copyright (C) 1997 Robin D. Clark                                *
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
#include <Xm/ArrowB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/LabelGP.h>
#include <Xm/List.h>
#include <Xm/MainW.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xbae/Matrix.h>

#include "config.h"

#include "AdjBWindow.h"
#include "Account.h"
#include "AccWindow.h"
#include "BuildMenu.h"
#include "Data.h"
#include "Destroy.h"
#include "FileBox.h"
#include "FileIO.h"
#include "HelpWindow.h"
#include "LedgerUtils.h"
#include "main.h"
#include "MainWindow.h"
#include "RecnWindow.h"
#include "RegWindow.h"
#include "Reports.h"
#include "util.h"
#include "XferWindow.h"

/** PROTOTYPES ******************************************************/
static void xaccMainWindowRedisplayBalance (void);
static void closeMainWindow ( Widget mw, XtPointer cd, XtPointer cb );
static void listCB          ( Widget mw, XtPointer cd, XtPointer cb );
static void expandListCB    ( Widget mw, XtPointer cd, XtPointer cb );

static void ArrowEventCallback (Widget w, XtPointer pClientData,
                                XEvent *event, Boolean *ContDispatch);

/** GLOBALS *********************************************************/
/* hack alert -- most of these globals should be moved to a struct! */
extern Widget toplevel;
extern char   *datafile;
static Account *selected_acc = NULL;          /* The selected account */
static Widget accountlist;
static Widget baln_widget;
static Widget show_widget;
short show_categories = 1;

/* the english-language names here should match 
 * the enumerated types in Account.h */
char *account_type_name[] = 
       { "Bank","Cash","Asset","Credit Card",
         "Liability","Stock","Mutual Fund",
         "Income", "Expense", "Equity" };

/* Pixel values are used to color the balance field text 
 * when computing the balance */
#if !USE_NO_COLOR
#  define POSITIVE_BALANCE "black"
#  define NEGATIVE_BALANCE "red"
Pixel   posPixel, negPixel;
Boolean havePixels = False;
#endif

#define XACC_MAIN_ACC_ARRW 0
#define XACC_MAIN_ACC_NAME 1
#define XACC_MAIN_ACC_TYPE 2
#define XACC_MAIN_ACC_BALN 3
#define XACC_MAIN_NUM_COLS 4

/********************************************************************\
 * xaccMainWindowAddAcct                                            *
 *                                                                  *
 * Args:   none                                                     *
 * Return: none                                                     *
\********************************************************************/
void
xaccMainWindowAddAcct (Widget acctrix, AccountGroup *grp, int depth )
{

  int   i, j, k, currow;
  char  buf[BUFSIZE];
  char *amt;
  
  /* Add all the top-level accounts to the list */
  for( i=0; i<grp->numAcc; i++ )
    {
    String cols[XACC_MAIN_NUM_COLS];
    Account *acc = getAccount( grp, i );
    double dbalance;
    
    if ((0 == show_categories) && 
       ((INCOME == acc->type) || (EXPENSE == acc->type))) continue;
    /* fill in the arrow and the account type fileds */
    cols[XACC_MAIN_ACC_ARRW] = XtNewString("");
    cols[XACC_MAIN_ACC_TYPE] = XtNewString(account_type_name[acc->type]);

    /* fill in the account name field, indenting for sub-accounts */
    buf[0] = 0x0;
    for (j=0; j<depth; j++) {
       strcat (buf, "    ");
    }
    strcat (buf, acc->accountName);
    cols[XACC_MAIN_ACC_NAME] = XtNewString(buf);

    /* fill in the balance column */
    dbalance = acc->balance;
    /* if the account has children, add in thier balance */
    if (acc->children) {
       dbalance += acc->children->balance;
    }
    
    /* the meaning of "balance" for income and expense 
     * accounts is reversed, since a deposit of a paycheck in a
     * bank account will appear as a debit of the corresponding
     * amount in the income account */
    if ((EXPENSE == acc->type) ||
        (INCOME  == acc->type) ) {
      dbalance = -dbalance;
    }
    amt = xaccPrintAmount (dbalance, PRTSYM);
    cols[XACC_MAIN_ACC_BALN] = XtNewString(amt);
    
    XtVaGetValues (acctrix, XmNrows, &currow, NULL);
    XbaeMatrixAddRows( acctrix, currow, cols, NULL, NULL, 1 );

    for (k=0; k<XACC_MAIN_NUM_COLS; k++) {
      XtFree (cols[k]);
    }
    
#if !USE_NO_COLOR
    /* Set the color of the text, depending on whether the
     * balance is negative or positive */
    if( 0.0 > dbalance )
      XbaeMatrixSetCellColor( acctrix, currow, XACC_MAIN_ACC_BALN, negPixel );
    else
      XbaeMatrixSetCellColor( acctrix, currow, XACC_MAIN_ACC_BALN, posPixel );    
#endif

    /* associate a pointer to the actual account with the row */
    XbaeMatrixSetRowUserData ( acctrix, currow, (XtPointer) acc); 

    /* If the account has sub-accounts, then add an arrow button 
     * next to the account name.  Clicking on the arrow button will 
     * expand the display to list the sub-accounts.  The arrow button
     * will be a cell-wdiget, and will be stored with the account 
     * structure */
    if (acc->children) {
       /* if the arrow button doesn't exist, add it */
       if (NULL == acc->arrowb) {
          int height;
          /* adjust arrow size for font size */
          height = XbaeMatrixGetRowPixelHeight (acctrix);
          acc->arrowb = XtVaCreateManagedWidget ("accarrow", 
                                      xmArrowButtonWidgetClass, acctrix,
                                      XmNwidth, height,
                                      XmNheight, height,
                                      XmNshadowThickness, 0,
                                      XmNarrowDirection, XmARROW_DOWN, 
                                      NULL);

          XtAddCallback (acc->arrowb, XmNactivateCallback, 
                         expandListCB, (XtPointer *) acc);

#define __XACC_DO_ARROW_CALLBACK
#ifdef  __XACC_DO_ARROW_CALLBACK
          /* add a button press event handler just in case the 
           * XmNactivate callback is broken. See notes for the
           * ArrowEventCallback for details.  -- Linas */
          acc->PreviousArrowReason = 0;
          XtAddEventHandler(acc->arrowb, 
                            ButtonPressMask | ButtonReleaseMask,
                            False, (XtEventHandler) ArrowEventCallback,
                            (XtPointer) acc);
#endif /* __XACC_DO_ARROW_CALLBACK */

       }
       XbaeMatrixSetCellWidget (acctrix, currow, XACC_MAIN_ACC_ARRW, acc->arrowb);
       XtManageChild (acc->arrowb);

       /* recursively display children accounts */
       if (acc->expand) {
          xaccMainWindowAddAcct (acctrix, acc->children, depth+1);
       }
    } else {
       /* if there are no children, make sure that there is no
        * arrow too.  This situation can occur if a sub-account
        * has been deleted. 
        */
       if (acc->arrowb) {
          XbaeMatrixSetCellWidget (acctrix, currow, XACC_MAIN_ACC_ARRW, NULL);
          XtRemoveCallback (acc->arrowb, XmNactivateCallback,
                            expandListCB, (XtPointer *) acc);

#ifdef  __XACC_DO_ARROW_CALLBACK
          acc->PreviousArrowReason = 0;
          XtRemoveEventHandler(acc->arrowb, 
                            ButtonPressMask | ButtonReleaseMask,
                            False, (XtEventHandler) ArrowEventCallback,
                            (XtPointer) acc);
#endif /* __XACC_DO_ARROW_CALLBACK */
          XtUnmanageChild (acc->arrowb);
          XtDestroyWidget (acc->arrowb);
          acc->arrowb = NULL;
       }
    }
  }
}

/********************************************************************\
 * refreshMainWindow                                                *
 *                                                                  *
 * Args:   none                                                     *
 * Return: none                                                     *
 * Global: data        - the data from the datafile                 *
 *         accountlist - the widget that has the list of accounts   *
\********************************************************************/
void
refreshMainWindow( void )
  {

  int   nrows;
  int   row_from_top = 0;
  AccountGroup *grp = topgroup;    /* hack -- should pass as argument ... */
  
  /* During refresh, we remove and re-add all displayed accounts.
   * We need to do this because the refresh may be due to an account 
   * having been added, or due to an expansion of sub-accounts.
   * However, doing this will cause the window to be scrolled
   * to the top row, which is visually quite annoying.  Thus, we
   * will save the current selected, visible row, and rescroll
   * the redrawn window to put this row back to its original location.
   * So -- first, figure out whats visible, and then restore.
   */
  if (selected_acc) {
    int i, toprow;
    XtVaGetValues( accountlist, XmNrows, &nrows, NULL );

    for (i=0; i<nrows; i++) {
      Account * racc;
      racc = (Account *) XbaeMatrixGetRowUserData (accountlist, i);
      if (racc == selected_acc) break;
    }
    XtVaGetValues( accountlist, XmNtopRow, &toprow, NULL );
    row_from_top = i - toprow;
    if (0 > row_from_top) row_from_top = 0;  /* this should neve happen !? */
  }

  XtVaGetValues( accountlist, XmNrows, &nrows, NULL );
  XbaeMatrixDeleteRows( accountlist, 0, nrows );
  
  xaccRecomputeGroupBalance (grp);  
  xaccMainWindowAddAcct (accountlist, grp, 0);
  xaccMainWindowRedisplayBalance ();

  /* find the selected account in the new window, 
   * and scroll to it. */
  if (selected_acc) {
    int i, toprow;
    XtVaGetValues( accountlist, XmNrows, &nrows, NULL );

    for (i=0; i<nrows; i++) {
      Account * racc;
      racc = (Account *) XbaeMatrixGetRowUserData (accountlist, i);
      if (racc == selected_acc) break;
    }
    toprow = i - row_from_top;
    
    /* set this row to be the top visible row */
    XtVaSetValues( accountlist, XmNtopRow, toprow, NULL );
    XbaeMatrixSelectRow( accountlist, i );
  }
}

/********************************************************************\
\********************************************************************/

/* --------------------------------------------------------------------
 * This callback is provided in order to have a separate means of detecting
 * the arrow button activation.  It seems that some (all?) versions of Motif
 * have trouble correctly computing the XmCR_ACTIVATE reason for the arrow
 * button.  In particular, this occurs when the ArrowButton widget has been
 * reparented.  (XbaeMatrix will reparent a widget so that it will
 * be properly clipped, e.g. when it is inside of a scrolling window.
 * The clipping is vitally important to get the widget properly drawn).
 * 
 * In a way, one might argue that it is not surpirsing that a reparented
 * window (XReparentWindow) will confuse the widget: after all, the widget
 * coordinets with respect to the parent widget differ from the window
 * coordinates compared to the parent window.  However, this argument
 * seems flawed: Motif seems to be able to correctly compute and deliver
 * the XmCR_ARM reason when a button is pressed.  Why can't it get the
 * the XmCR_ACTIVATE reason when the very same button is relased?
 * Also, the very same versions of Motif have no problem recognizing
 * that a button press and release has occured in the window, and have
 * no problem calling this callback.  So, somehow, the activate computation 
 * seems broken.
 * 
 * Thus, this callback provides an alternate way of getting the arrow
 * button to work properly.  -- Linas Vepstas October 1997
 */

static void 
ArrowEventCallback(Widget w, XtPointer pClientData,
                   XEvent *event, Boolean *ContDispatch)

{
    XButtonEvent *bev = (XButtonEvent *) event;
    XmArrowButtonCallbackStruct many;

    /* if its not the left mouse button, return */
    if (1 != bev->button) return;

    /* emulate the arm and activate callbacks */
    switch ( event->type ) {
        case ButtonPress:
            many.reason = XmCR_ARM;
            many.event = event;
            many.click_count = 1;
            expandListCB (w, pClientData, (XtPointer) &many);
            break;
        case ButtonRelease:
            many.reason = XmCR_ACTIVATE;
            many.event = event;
            many.click_count = 1;
            expandListCB (w, pClientData, (XtPointer) &many);
            break;
    }
} /* ArrowEventCallback */


/********************************************************************\
\********************************************************************/

static void 
expandListCB( Widget mw, XtPointer pClientData, XtPointer cb)
{
  XmAnyCallbackStruct *info = (XmAnyCallbackStruct *) cb;
  Account *acc = (Account *)pClientData;

  /* a "fix" to avoid double invocation */
  switch ( info->reason ) {
      case XmCR_ACTIVATE:
          /* avoid double invocation */
          if (XmCR_ACTIVATE == acc->PreviousArrowReason) return;
          acc -> PreviousArrowReason = XmCR_ACTIVATE;
          break;

      default:
      case XmCR_ARM:
          /* avoid double invocation */
          if (XmCR_ARM == acc->PreviousArrowReason) return;
          acc -> PreviousArrowReason = XmCR_ARM;
          return;
  }

  /* change arrow direction, mark account as needing expansion */
  if (acc->expand) {
     acc->expand = 0;
     XtVaSetValues (mw, 
                    XmNarrowDirection, XmARROW_DOWN, 
                    NULL);
  } else {
     acc->expand = 1;
     XtVaSetValues (mw, 
                    XmNarrowDirection, XmARROW_UP, 
                    NULL);
  }

  /* redraw the main window */
  selected_acc = acc;
  refreshMainWindow ();
}

/********************************************************************\
 * mainWindow -- the main window... (normally) the first window     *
 *   that pops up.  Has list of accounts that the user can open.    *
 *   Opening an account produces a register window.  The user       *
 *   can also create new accounts and edit existing accounts.       *
 *                                                                  *
 * Args:   parent   - the parent of the window to be created        *
 * Return: none                                                     *
 * Global: data        - the data from the datafile                 *
 *         accountlist - the widget that has the list of accounts   *
\********************************************************************/

void
mainWindow( Widget parent )
  {
  Widget   mainwindow,menubar,actionform,buttonform,pane,button,widget;
  int      position;
  
  /******************************************************************\
   * Set up the menubar                                             *
  \******************************************************************/
  MenuItem fileMenu[] = {
    { "New File...",   &xmPushButtonWidgetClass, 'N', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_NEW,    (MenuItem *)NULL, 0 },
    { "Open File...  ",&xmPushButtonWidgetClass, 'O', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_OPEN,   (MenuItem *)NULL, 0 },
    { "Import QIF...  ",&xmPushButtonWidgetClass, 'I', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_IMPORT, (MenuItem *)NULL, 0 },
    { "",              &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                   (MenuItem *)NULL, 0 },
    { "Save",          &xmPushButtonWidgetClass, 'S', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_SAVE,   (MenuItem *)NULL, 0 },
    { "Save As...",    &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_SAVEAS, (MenuItem *)NULL, 0 },
    { "",              &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                   (MenuItem *)NULL, 0 },
    { "Quit",          &xmPushButtonWidgetClass, 'Q', NULL, NULL, True,
      fileMenubarCB, (XtPointer)FMB_QUIT,   (MenuItem *)NULL, 0 },
    { NULL,         NULL,                          0, NULL, NULL, False, 
      NULL,          (XtPointer)0,          (MenuItem *)NULL, 0 },
  };

  MenuItem accountMenu[] = {
    { "New Account...",     &xmPushButtonWidgetClass, 'N', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_NEW,  (MenuItem *)NULL, 0 },
    { "Open Account",       &xmPushButtonWidgetClass, 'O', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_OPEN, (MenuItem *)NULL, 0 },
    { "Open Subaccounts",   &xmPushButtonWidgetClass, 'S', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_LEDGER, (MenuItem *)NULL, 0 },
    { "Edit Account...",    &xmPushButtonWidgetClass, 'E', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_EDIT, (MenuItem *)NULL, 0 },
    { "Delete Account...",  &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_DEL,  (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL, 0 },
    { "Transfer",           &xmPushButtonWidgetClass, 'T', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_TRNS, (MenuItem *)NULL, 0 },
    { "Report",             &xmPushButtonWidgetClass, 'R', NULL, NULL, False,
      accountMenubarCB, (XtPointer)AMB_RPRT, (MenuItem *)NULL, 0 },
    { "Hide Inc/Exp...",    &xmPushButtonWidgetClass, 'I', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_SHOW, (MenuItem *)NULL, 0 },
#if 0
    { "Edit Categories...", &xmPushButtonWidgetClass, 'C', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_CAT,  (MenuItem *)NULL, 0 },
#endif
    { NULL,         NULL,                          0, NULL, NULL, False, 
      NULL,              (XtPointer)0,       (MenuItem *)NULL, 0 },
  };
  
  MenuItem helpMenu[] = {
    { "About...",           &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL, 0 },
    { "Help...",            &xmPushButtonWidgetClass, 'H', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_MAIN,  (MenuItem *)NULL, 0 },
    { "Accounts...",        &xmPushButtonWidgetClass, 'C', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_ACC,   (MenuItem *)NULL, 0 },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                  (MenuItem *)NULL, 0 },
    { "License...",         &xmPushButtonWidgetClass, 'L', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL, 0 },
    { NULL,         NULL,                          0, NULL, NULL, False, 
      NULL,              (XtPointer)0,     (MenuItem *)NULL, 0 },
  };
  
  mainwindow = XtVaCreateManagedWidget( "mainwindow", 
					xmMainWindowWidgetClass, parent, 
					XmNdeleteResponse,       XmDESTROY,
                                        /*
                                         * Let the window find its own size, 
                                         * based on the font sizes.
                                         * XmNwidth,     450,
                                         * XmNheight,    240,
                                         */
					NULL );
  
  /* Umm... this doesn't seem to be getting called */
  XtAddCallback( mainwindow, XmNdestroyCallback, 
		 closeMainWindow, (XtPointer)NULL );
  
  menubar = XmCreateMenuBar( mainwindow, "menubar", NULL, 0 );  
  
  BuildMenu( menubar, XmMENU_PULLDOWN, "File",   'F', False, 0, fileMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Account",'A', False, 0, accountMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Help",   'H', False, 0, helpMenu );

  /* hack alert -- 8 is very sensitive to menu changes! */
  show_widget = accountMenu[8].widget;

  XtManageChild( menubar );
  
  /******************************************************************\
   * If they haven't already been initialize, initialize the Pixel  *
   * values that are used for foreground colors for the balance     *
  \******************************************************************/
#if !USE_NO_COLOR
  if( !havePixels )
    {
    XrmValue colorValue, pixelValue;
    
    colorValue.size = strlen(POSITIVE_BALANCE);
    colorValue.addr = (XtPointer)POSITIVE_BALANCE;
    pixelValue.size = sizeof(Pixel);
    pixelValue.addr = (XtPointer)0;
    
    XtConvertAndStore( parent,
                       XtRString, &colorValue,
                       XtRPixel,  &pixelValue );
    
    posPixel = (*(Pixel *)pixelValue.addr);
    
    colorValue.size = strlen(NEGATIVE_BALANCE);
    colorValue.addr = (XtPointer)NEGATIVE_BALANCE;
    pixelValue.size = sizeof(Pixel);
    pixelValue.addr = (XtPointer)0;
    
    XtConvertAndStore( parent,
                       XtRString, &colorValue,
                       XtRPixel,  &pixelValue );
    
    negPixel = (*(Pixel *)pixelValue.addr);
    
    havePixels = True;
    }
#endif
  
  /* Create a PanedWindow Manager for the dialog box... the paned 
   * window is the parent of the two forms which comprise the two
   * areas of the dialog box */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, mainwindow,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNtraversalOn,   False,
                           NULL );
  
  
  /******************************************************************\
   * The account list -- the top part of the window                 *
  \******************************************************************/

  /* form to help place the accountlist */
  actionform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass, pane,
				 NULL );
  
  /* Create a matrix widget to hold the account list... the
   * listCB helps make this matrix think it is a list.  We
   * use the matrix instead of a list to get the accounts
   * up in columns */
    {
    String   labels[XACC_MAIN_NUM_COLS]     = {"", "Account Name","Type","Balance"};
    short    colWidths[]        = {2,20,10,12};
    unsigned char alignments[XACC_MAIN_NUM_COLS] = {
                                   XmALIGNMENT_CENTER,
                                   XmALIGNMENT_BEGINNING,
				   XmALIGNMENT_CENTER,
				   XmALIGNMENT_END};
    
    accountlist
      = XtVaCreateWidget( "list",
			  xbaeMatrixWidgetClass,  actionform,
			  XmNvisibleRows,         8,
			  XmNcolumns,             XACC_MAIN_NUM_COLS,
			  XmNcolumnWidths,        colWidths,
			  XmNcolumnAlignments,    alignments,
			  XmNcolumnLabelAlignments,    alignments,
			  XmNcolumnLabels,        labels,
			  XmNtraversalOn,         False,
			  XmNfill,                True,
			  XmNcellMarginHeight,    0,
			  XmNcellMarginWidth,     0,
			  XmNgridType,            XmGRID_NONE,
			  XmNcellShadowThickness, 0,
			  XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
			  XmNtopAttachment,       XmATTACH_FORM,
			  XmNleftAttachment,      XmATTACH_FORM,
			  XmNbottomAttachment,    XmATTACH_FORM,
			  XmNrightAttachment,     XmATTACH_FORM,
			  NULL);
    XtAddCallback( accountlist, XmNenterCellCallback,
                   listCB, (XtPointer)NULL );
    
    /* If the user double-clicks on an account in the list, open
     * up the detail view (ie the register window, or whatever) for
     * that type of account */
    XtAddCallback( accountlist, XmNdefaultActionCallback, 
                   accountMenubarCB, (XtPointer)AMB_OPEN );
    }
 
  /******************************************************************\
   * The button area -- has buttons to create a new account, or     *
   * delete an account, or whatever other button I think up         *
   * NOTE: the buttons are just shortcuts to the account menubar,   *
   *       and this is why all the callbacks are accountMenubarCB   *
  \******************************************************************/

  /* create form that will contain most everything in this window...
   * The fractionbase divides the form into segments, so we have
   * better control over where to put the buttons */
  buttonform = XtVaCreateWidget( "form", 
				  xmFormWidgetClass, pane,
				  XmNfractionBase,   22,
				  NULL );
  
  position = 0;                    /* puts the buttons in the right place */
  
  /* The "Open" button */
  widget = XtVaCreateManagedWidget( "Open", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    XmNshowAsDefault,      True,
				    NULL );

  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_OPEN );

  /* The "New" button, to create a new account */
  position += 3;
  widget = XtVaCreateManagedWidget( "New", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_NEW );
  
  /* The "Edit" button */
  position += 3;
  widget = XtVaCreateManagedWidget( "Edit", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_EDIT );
  
  /* The "Delete" button */
  position += 3;
  widget = XtVaCreateManagedWidget( "Delete", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 accountMenubarCB, (XtPointer)AMB_DEL );

  button = widget;
  
  
  /* ---------------------------------------------------------------- */

  /* The Asset and Profit field labels: */ 
  position +=5;
  widget = XtVaCreateManagedWidget( "Assets:",
				    xmLabelGadgetClass,    buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    NULL );
  widget = XtVaCreateManagedWidget( "Profits:",
				    xmLabelGadgetClass,    buttonform,
				    XmNtopAttachment,      XmATTACH_WIDGET,
				    XmNtopWidget,          widget,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+3,
				    NULL );
  
  /* and the balance fields: */
  position += 3;
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
				    XmNrightPosition,      position+5,
				    NULL );
  baln_widget = widget;
  
  /* ---------------------------------------------------------------- */
    
  refreshMainWindow();
  XtManageChild(accountlist);
  
  /* Fix button area of the pane to its current size, and not let 
   * it resize. */
  XtManageChild( buttonform );
    {
    Dimension h;
    XtVaGetValues( button, XmNheight, &h, NULL );
    XtVaSetValues( buttonform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
  
  XtManageChild( actionform );
  XtManageChild( pane );
  }

/********************************************************************\
 * closeMainWindow                                                  *
 *   frees memory allocated for an mainWindow, and other cleanup    * 
 *   stuff                                                          * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     *
\********************************************************************/
void 
closeMainWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  
#ifdef __XACC_DO_ARROW_CALLBACK
  /* this remove core-dumps motif, Don't know why --linas */
  /* XtRemoveEventHandler(mw->arrowb, 
   *                    ButtonPressMask | ButtonReleaseMask,
   *                    True, (XtEventHandler) ArrowEventCallback,
   *                    (XtPointer) mw);
   */
#endif /* __XACC_DO_ARROW_CALLBACK */

  DEBUG("closed MainWindow");
  DEBUGCMD(printf(" coresize = %d\n",_coresize()));
  exit(0);
  }

/********************************************************************\
 * compute profits and asssets
\********************************************************************/
static void
xaccMainWindowRedisplayBalance (void)
{
   int i;
   double  assets  = 0.0;
   double  profits = 0.0;
   char buf[BUFSIZE];
   char * amt;
   AccountGroup *grp = topgroup;
   Account *acc;
   
   for (i=0; i<grp->numAcc; i++) {
      acc = grp->account[i];
  
      switch (acc->type) {
         case BANK:
         case CASH:
         case ASSET:
         case STOCK:
         case MUTUAL:
         case CREDIT:
         case LIABILITY:
            assets += acc->balance;
            if (acc->children) {
               assets += acc->children->balance; 
            }
            break;
         case INCOME:
         case EXPENSE:
            profits -= acc->balance; /* flip the sign !! */
            if (acc->children) {
               profits -= acc->children->balance; /* flip the sign !! */
            }
            break;
         case EQUITY:
         default:
            break;
      }
   }
  
   amt = xaccPrintAmount (assets, PRTSYM);
   strcpy (buf, amt);
   strcat (buf, "\n");
   amt = xaccPrintAmount (profits, PRTSYM);
   strcat (buf, amt);
   
   XmTextSetString( baln_widget, buf );
}

/********************************************************************\
 * listCB -- makes the matrix widget behave like a list widget      * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: accountlist - the widget that has the list of accounts   *
\********************************************************************/
static void
listCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XbaeMatrixEnterCellCallbackStruct *cbs =
    (XbaeMatrixEnterCellCallbackStruct *)cb;
  
  cbs->doit = False;
  cbs->map  = False;
  
  selected_acc = (Account *) XbaeMatrixGetRowUserData (accountlist, cbs->row);

  XbaeMatrixDeselectAll(accountlist);
  XbaeMatrixSelectRow( accountlist, cbs->row );
  }


/********************************************************************\
 * fileMenubarCB -- handles file menubar choices                    * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb - const that lets us know which choice was selected   * 
 * Return: none                                                     * 
 * Global: data        - the data from the datafile                 *
 *         datafile    - the name of the user's datafile            *
 *         toplevel    - the toplevel widget                        *
\********************************************************************/
void
fileMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  AccountGroup *grp = topgroup;
  int button = (int)cd;
  
  /*
   * which of the file menubar options was chosen
   *   FMB_NEW    -  New datafile
   *   FMB_OPEN   -  Open datfile
   *   FMB_IMPORT -  Open & merge in Quicken QIF File
   *   FMB_SAVE   -  Save datafile
   *   FMB_SAVEAS -  Save datafile As
   *   FMB_QUIT   -  Quit
   */
  
  switch( button )
    {
    case FMB_NEW:
      DEBUG("FMB_NEW\n");
      if( xaccAccountGroupNotSaved (grp) )
        {
        if( verifyBox (toplevel, FMB_SAVE_MSG) )
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
        }
      datafile = NULL;
      /* destroy open windows first, before destroying the group itself */
      xaccGroupWindowDestroy (grp);
      freeAccountGroup (grp);
      grp = mallocAccountGroup();
      grp->new = True;             /* so we have to do a "SaveAs" when
                                    * the file is first saved */
      topgroup = grp;
      break;

    case FMB_OPEN: {
      char * newfile;
      DEBUG("FMB_OPEN\n");
      if( xaccAccountGroupNotSaved (grp) ) {
        if( verifyBox(toplevel, FMB_SAVE_MSG) ) {
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
          }
        }
      newfile = fileBox(toplevel,OPEN, "*.dat");
      if (newfile) {
        datafile = newfile;

        /* destroy open windows first, before destroying the group itself */
        xaccGroupWindowDestroy (grp);
        freeAccountGroup (grp);
      
        /* load the accounts from the users datafile */
        grp = readData (datafile);
      
        if( NULL == grp ) {
          /* the file could not be found */
          grp = mallocAccountGroup();
        }
        topgroup = grp;
      }
      break;
    }

    case FMB_IMPORT: {
      char * newfile;
      char buf[BUFSIZE];

      DEBUG("FMB_IMPORT\n");

      newfile = fileBox(toplevel,OPEN, "*.qif");
      if (newfile) {
        strcpy (buf, newfile);
        strcat (buf, ".dat");
        datafile = XtNewString (buf);
      
        /* load the accounts from the users datafile */
        grp = xaccReadQIFData (newfile);
      
        if( NULL == topgroup ) {
          /* no topgroup exists */
          topgroup = mallocAccountGroup();
        }

        /* since quicken will not export all accounts 
         * into one file, we must merge them in one by one */
        xaccConcatGroups (topgroup, grp);
        xaccMergeAccounts (topgroup);
        xaccConsolidateGrpTransactions (topgroup);
      }
      break;
    }

    case FMB_SAVE:
      DEBUG("FMB_SAVE\n");
      /* hack alert -- Somehow make sure all in-progress edits get committed! */
      if (NULL == datafile) {
        fileMenubarCB( mw, (XtPointer)FMB_SAVEAS, cb );
        break;
      }

      writeData( datafile, grp );
      xaccAccountGroupMarkSaved (grp);
      break;

    case FMB_SAVEAS: {
      char * newfile;
      DEBUG("FMB_SAVEAS\n");

      newfile = fileBox(toplevel,OPEN, "*.dat");
      if ( newfile ) {
         datafile = newfile;
         fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
      }
      break;
    }

    case FMB_QUIT:
      DEBUG("FMB_QUIT\n");
      {
      Account *acc;
      int i=0;
      while( (acc=getAccount (grp,i++)) != NULL )
        {
        if( acc->regData != NULL )
          {
          /* ??? -- hack alert -- should free */
          acc->regData = NULL;
          }
        if( acc->recnData != NULL )
          {
          /* ??? -- hack alert -- should free */
          acc->recnData = NULL;
          }
        }
      
      if( !(grp->saved) )
        {
        if( verifyBox(toplevel, FMB_SAVE_MSG) )
          fileMenubarCB( mw, (XtPointer)FMB_SAVE, cb );
        }
      
      /* destroy open windows first, before destroying the group itself */
      xaccGroupWindowDestroy (grp);
      freeAccountGroup (grp);
      topgroup = NULL;
      XtUnmapWidget(toplevel);     /* make it disappear quickly */
      XtDestroyWidget(toplevel);
      return;                      /* to avoid the refreshMainWindow */
      }
      break;

    default:
      PERR("fileMenubarCB(): We shouldn't be here!");
    }
  refreshMainWindow();
  }

/********************************************************************\
 * accountMenubarCB -- handles account menubar choices              * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - const that lets us know which choice was selected   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: data         - the data from the datafile                *
 *         selected_acc - the selected account                      *
 *         toplevel     - the toplevel widget                       *
\********************************************************************/
void
accountMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int button = (int)cd;
  
  /*
   * which of the file menubar options was chosen
   *   AMB_NEW    -  New account
   *   AMB_OPEN   -  Open account
   *   AMB_LEDGER -  Open account and subaccounts in one register
   *   AMB_EDIT   -  Edit account
   *   AMB_DEL    -  Delete account
   *   AMB_SHOW   -  Show catagories
   *   AMB_CAT    -  Edit catagories
   */
  
  switch( button )
    {
    case AMB_NEW:
      DEBUG("AMB_NEW\n");
      accWindow(toplevel);
      break;

    case AMB_OPEN:
      DEBUG("AMB_OPEN\n");
      {
        Account *acc = selected_acc;
        if( NULL == acc ) {
          int make_new = verifyBox (toplevel, ACC_NEW_MSG);
          if (make_new) {
            accWindow(toplevel);
          }
        } else {
          regWindowSimple ( toplevel, acc );
        }
      }
      break;

    case AMB_LEDGER:
      DEBUG("AMB_LEDGER\n");
      {
        Account *acc = selected_acc;
        if( NULL == acc ) {
          int make_new = verifyBox (toplevel, ACC_NEW_MSG);
          if (make_new) {
            accWindow(toplevel);
          }
        } else {
          regWindowAccGroup ( toplevel, acc );
        }
      }
      break;

    case AMB_EDIT:
      DEBUG("AMB_EDIT\n");
      {
        Account *acc = selected_acc;
        if( NULL == acc ) {
          errorBox (toplevel, ACC_EDIT_MSG);
        } else {
          editAccWindow( toplevel, acc );
        }
      }
      break;

    case AMB_DEL:
      DEBUG("AMB_DEL\n");
      {
        Account *acc = selected_acc;
        if( NULL == acc ) {
          errorBox (toplevel, ACC_DEL_MSG);
        } else {
          char msg[1000];
          sprintf (msg, ACC_DEL_SURE_MSG, acc->accountName);
          if( verifyBox(toplevel,msg) ) {

            /* before deleting the account, make 
             * sure that we close any misc register 
             * windows, if they are open */
            xaccAccountWindowDestroy (selected_acc);
            xaccRemoveAccount (selected_acc);
            freeAccount (selected_acc);
            selected_acc = NULL;
            refreshMainWindow();
            }
          }
        }
      break;

    case AMB_TRNS:
      DEBUG("AMB_TRNS\n");
      xferWindow(toplevel);
      break;

    case AMB_RPRT:
      DEBUG("AMB_RPRT\n");
      simpleReportWindow(toplevel);
      break;

    case AMB_SHOW: {
      XmString str;
      DEBUG("AMB_SHOW\n");
      if (show_categories) {
         show_categories = 0;
         str = XmStringCreateLtoR ("Show Inc/Exp...", XmSTRING_DEFAULT_CHARSET);
      } else {
         show_categories = 1;
         str = XmStringCreateLtoR ("Hide Inc/Exp...", XmSTRING_DEFAULT_CHARSET);
      }
      XtVaSetValues (show_widget, XmNlabelString, str, NULL);
      XmStringFree (str);
      refreshMainWindow();
      }
      break;

    case AMB_CAT:
      DEBUG("AMB_CAT\n");
      break;

    default:
      PERR ("AccountMenuBarCB(): We shouldn't be here!\n");
    }
  }

/********************************************************************\
 * helpMenubarCB -- handles help menubar choices                    * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - const that lets us know which choice was selected   * 
 *         cb -                                                     * 
 * Return: none                                                     * 
 * Global: toplevel    - the toplevel widget                        *
\********************************************************************/
void
helpMenubarCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int button = (int)cd;
  
  /*
   * which of the file menubar options was chosen
   *   HMB_ABOUT   -  About this program
   *   HMB_MAIN    -  Display help window for the main window
   *   HMB_REGWIN  -  Display help window for the Register Window
   *   HMB_RECNWIN -  Display help window for the Reconcile Window
   *   HMB_LIC     -  GPL Licence info
   */
  
  switch( button )
    {
    case HMB_ABOUT:
      DEBUG("HMB_ABOUT");
      helpWindow( toplevel, "About", HH_ABOUT );
      break;
    case HMB_ACC:
      DEBUG("HMB_ACC");
      helpWindow( toplevel, "Help", HH_ACC );
      break;
    case HMB_REGWIN:
      /* When the user selects "Help" in the RegWindow */
      DEBUG("HMB_REGWIN");
      helpWindow( toplevel, "Help", HH_REGWIN );
      break;
    case HMB_RECNWIN:
      /* When the user selects "Help" in the RecnWindow */
      DEBUG("HMB_RECNWIN");
      helpWindow( toplevel, "Help", HH_RECNWIN );
      break;
    case HMB_ADJBWIN:
      /* When the user selects "Help" in the AdjBWindow */
      DEBUG("HMB_ADJBWIN");
      helpWindow( toplevel, "Help", HH_ADJBWIN );
      break;
    case HMB_MAIN:
      /* When the user selects "Help" in the MainWindow */
      DEBUG("HMB_HELP");
      helpWindow( toplevel, "Help", HH_MAIN );
      break;
    case HMB_LIC:
      /* The GNU Public License */
      DEBUG("HMB_LIC");
      helpWindow( toplevel, "License", HH_GPL );
      break;
    default:
      DEBUG("We shouldn't be here!");
    }
  }

/********************* END OF FILE **********************************/

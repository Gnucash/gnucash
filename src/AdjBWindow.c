/********************************************************************\
 * AdjBWindow.c -- the adjust balance window                        *
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
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/LabelGP.h>

#include "config.h"

#include "Account.h"
#include "Data.h"
#include "date.h"
#include "main.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "RecnWindow.h"
#include "util.h"

/** GLOBALS *********************************************************/

/** STRUCTS *********************************************************/
typedef struct _AdjBWindow
{
  Account *acc;             /* The account that we are adjusting    */
  Widget  dialog;           /* The adjust balance dialog            */
  Widget  balance;          /* Text field, the new balance          */
  Widget  date;             /* Text field, the date for the balance */
} AdjBWindow;


/** PROTOTYPES ******************************************************/
static void adjBOkCB( Widget mw, XtPointer cd, XtPointer cb );
static void adjBClose( Widget mw, XtPointer cd, XtPointer cb );


/********************************************************************\
\********************************************************************/

void
xaccDestroyAdjBWindow (AdjBWindow *adjBData)
{
   if (!adjBData) return;
   XtDestroyWidget (adjBData->dialog);
}

/********************************************************************\
 * adjBWindow                                                       *
 *   opens up the window to adjust the balance                      *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to adjust                          *
 * Return: recnData - the instance of this AdjBWindow               *
\********************************************************************/
AdjBWindow *
adjBWindow( Widget parent, Account *acc )
  {
  Widget widget, pane, controlform, actionform;
  Date date;
  AdjBWindow *adjBData;
  char buf[BUFSIZE];
  
  setBusyCursor( parent );

  adjBData = (AdjBWindow *)_malloc(sizeof(AdjBWindow));
  adjBData->acc = acc;
  
  /* Create the dialog box... */
  sprintf( buf, "%s: %s", acc->accountName, ADJ_BALN_STR);
  
  adjBData->dialog =
    XtVaCreatePopupShell( "dialog", 
			  xmDialogShellWidgetClass,	parent,
			  XmNdialogStyle,    XmDIALOG_APPLICATION_MODAL,
			  XmNtitle,          buf,
			  XmNdeleteResponse, XmDESTROY,
                          XmNtransient, FALSE,  /* allow window to be repositioned */
			  NULL );
  
  /* ... and so memory gets freed: */
  XtAddCallback( adjBData->dialog, XmNdestroyCallback, 
                 adjBClose, (XtPointer)adjBData );
  
  /* Create a PanedWindow Manager for the dialog box... the child 
   * of optiondialog the paned window is the parent of the two 
   * forms which comprise the two areas of the dialog box...
   * The sash is set to minimun size to make it invisible */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, adjBData->dialog,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNtraversalOn,   False,
                           NULL );
  
  /** CONTROLFORM ****************************************
   * Create a controlform for control area of dialog box */
  controlform = XtVaCreateWidget( "controlform", 
                                  xmFormWidgetClass, pane,
                                  NULL );
  
  widget = XtVaCreateManagedWidget( DATE_STR,
                                    xmLabelGadgetClass, controlform,
                                    XmNtopAttachment,   XmATTACH_FORM,
                                    XmNtopOffset,       10,
                                    XmNrightAttachment, XmATTACH_POSITION,
                                    XmNrightPosition,   50,
                                    NULL );
  
  todaysDate(&date);
  sprintf(buf,"%2d/%2d/%4d", date.month, date.day, date.year);
  
  adjBData->date =
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  controlform,
			     XmNvalue,           buf,
			     XmNtopAttachment,   XmATTACH_FORM,
			     XmNtopOffset,       10,
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    50,
			     NULL );
  
  /* The dateCB ensures the data entered in the date field is
   * in a valid format */
  XtAddCallback( adjBData->date, XmNmodifyVerifyCallback,
                 dateCB, (XtPointer)NULL );
  
  sprintf (buf, "%s %s", NEW_BALN_STR, CURRENCY_SYMBOL);
  widget = XtVaCreateManagedWidget( buf,
                                    xmLabelGadgetClass, controlform,
                                    XmNtopAttachment,   XmATTACH_WIDGET,
                                    XmNtopWidget,       adjBData->date,
                                    XmNrightAttachment, XmATTACH_POSITION,
                                    XmNrightPosition,   50,
                                    NULL );
  
  adjBData->balance =
    XtVaCreateManagedWidget( "text",
			     xmTextWidgetClass,  controlform,
			     XmNtopAttachment,   XmATTACH_WIDGET,
			     XmNtopWidget,       adjBData->date,
			     XmNleftAttachment,  XmATTACH_POSITION,
			     XmNleftPosition,    50,
			     NULL );
  
  /* The amountCB ensures the data entered in the balance field is
   * in a valid format */
  XtAddCallback( adjBData->balance, XmNmodifyVerifyCallback,
                 amountCB, (XtPointer)NULL );
  
  XtManageChild( controlform );
  
  /** ACTIONFORM ********************************************
   * Create a Form actionform for action area of dialog box */
  actionform = XtVaCreateWidget( "actionform", 
                                 xmFormWidgetClass, pane,
                                 XmNfractionBase,   8,
                                 NULL );
  
  /* The OK button is anchored to the form, between divider 1 & 2
   * (in the fraction base) */
  widget = XtVaCreateManagedWidget( OK_STR, 
                                    xmPushButtonWidgetClass, actionform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       1,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      3,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  XtAddCallback( widget, XmNactivateCallback,
                 adjBOkCB, (XtPointer)adjBData );
  XtAddCallback( widget, XmNactivateCallback,
                 destroyShellCB, (XtPointer)(adjBData->dialog) );
  
  /* The cancel button! */
  widget = XtVaCreateManagedWidget( CANCEL_STR, 
                                    xmPushButtonWidgetClass, actionform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       3,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      5,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)(adjBData->dialog) );  
  
  /* A help button will pop-up context sensitive help */
  widget = XtVaCreateManagedWidget( HELP_STR, 
                                    xmPushButtonWidgetClass, actionform,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       5,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      7,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  XtAddCallback( widget, XmNactivateCallback,
                 helpMenubarCB, (XtPointer)HMB_ADJBWIN );
  
  /* Fix action area of the pane to its current size, and not let it
   *  resize. */
  XtManageChild( actionform );
  {
  Dimension h;
  XtVaGetValues( widget, XmNheight, &h, NULL );
  XtVaSetValues( actionform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
  }
  
  XtManageChild( pane );
  XtPopup( adjBData->dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  
  return adjBData;
  }

/********************************************************************\
 * adjBClose                                                        *
 *   frees memory allocated for an adjBWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - adjBData - the data struct for this window          *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
adjBClose( Widget mw, XtPointer cd, XtPointer cb )
  {
  AdjBWindow *adjBData = (AdjBWindow *)cd;
  Account *acc = adjBData->acc;
  
  _free(adjBData);
  acc->adjBData = NULL;
  
  DEBUG("closed AdjBWindow");
  }

/********************************************************************\
 * adjBOkCB                                                         *
 *   creates the new transaction to adjust the account when the     *
 *   user clicks "Ok"                                               *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - adjBData - the data struct for this window          *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void 
adjBOkCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  AdjBWindow  *adjBData = (AdjBWindow *)cd;
  Transaction *trans, *tempTrans;
  Account *acc;
  String   str;
  float    val = 0.0;
  int      pos=0;
  double   themount=0.0,dcurrAmount=0.0;
  int      i;
  
  acc = adjBData->acc;
  acc->parent->saved = False;
  
  /* allocate mem for the new transaction */
  trans   = mallocTransaction();
  
  /* Create the "trans" transaction */
  str = XmTextGetString(adjBData->date);
  todaysDate(&(trans->date)); /* In case the date field is empty */
  sscanf( str, "%d/%d/%d", &(trans->date.month), 
          &(trans->date.day), &(trans->date.year) );
  str = XmTextGetString(adjBData->balance);
  sscanf( str, "%f", &val );  /* sscanf must take float not double as arg */
  themount = val;
  
  /* fill out the rest of the fields */
  xaccTransSetDescription (trans, ADJ_BALN_STR);
  xaccTransSetReconcile (trans, NREC);

  xaccInsertSplit (acc, &(trans->credit_split));

  /* compute the dollar amount this transaction should have.
   * it will be the difference between the current balance, and
   * the desired balance.  */
  dcurrAmount = xaccGetBalance (&(trans->credit_split));

  xaccSetAmount (split, dcurrAmount - themount);
  
  /* Refresh the account register window */
  regRefresh(acc->regData);
  /* Refresh the account reconcile window */
  recnRefresh(acc->recnData);
  
  refreshMainWindow();
  }

/******************** END OF FILE ***********************************\
\********************************************************************/

/********************************************************************\
 * RecnWindow.c -- the reconcile window                             *
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
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelGP.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xbae/Matrix.h>

#include "Account.h"
#include "Data.h"
#include "RegWindow.h"
#include "MainWindow.h"
#include "main.h"
#include "util.h"

/** STRUCTS *********************************************************/
typedef struct _RecnWindow
{
  Account *acc;             /* The account that we are reconciling  */
  double  ddiff;            /* The amount ($$$) to reconcile        */
  Widget  dialog;           /* The reconcile window dialog          */
  Widget  difference;       /* Text field, amount left to reconcile */
  Widget  totDebit;         /* Text field, total debit reconciled   */
  Widget  totCredit;        /* Text field, total credit reconciled  */
  Widget  debit;            /* Debit matrix show unreconciled debit */
  Widget  credit;           /* Credit matrix, shows credits...      */
} RecnWindow;

/** PROTOTYPES ******************************************************/
void recnRecalculateBalance( RecnWindow *recnData );

void recnClose( Widget mw, XtPointer cd, XtPointer cb );
void recnOkCB( Widget mw, XtPointer cd, XtPointer cb );
void recnCB( Widget mw, XtPointer cd, XtPointer cb );

/** GLOBALS *********************************************************/
extern XtAppContext app;

/********************************************************************/

/********************************************************************\
 * recnRefresh                                                      *
 *   refreshes the transactions in the reconcile window             *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: none                                                     *
\********************************************************************/
void
recnRefresh( RecnWindow *recnData )
  {
  if( recnData != NULL )
    {
    int   i,nrows;
    char  buf[BUFSIZE];
    Transaction *trans;
    Account *acc = recnData->acc;
    
    /* NOTE: an improvement of the current design would be to use the
     *       user-data in the rows to detect where transactions need
     *       to be inserted/delete, instead of deleting and re-inserting
     *       all the transactions! */
    
    /* Delete all the entries in the debit matrix */
    XtVaGetValues( recnData->debit, XmNrows, &nrows, NULL );    
    XbaeMatrixDeleteRows( recnData->debit, 0, nrows );
    
    /* Delete all the entries in the credit matrix */
    XtVaGetValues( recnData->credit, XmNrows, &nrows, NULL );
    XbaeMatrixDeleteRows( recnData->credit, 0, nrows );
    
    /* Add the non-reconciled transactions */
    i=0;
    while( (trans=getTransaction(acc,i++)) != NULL )
      {
      String rows[5];
      
      if( trans->reconciled != YREC )
        {
        double themount = xaccGetAmount (acc, trans);
        sprintf( buf, "%c", trans->reconciled );
        rows[0] = XtNewString(buf);
        rows[1] = trans->num;
        sprintf( buf, "%2d/%2d/%02d\0", 
                 trans->date.month,
                 trans->date.day,
                 (trans->date.year%100) );
        rows[2] = XtNewString(buf);
        rows[3] = trans->description;
        sprintf( buf, "%.2f\0", DABS(themount) );
        rows[4] = XtNewString(buf);
        
        if( 0.0 > themount)
          {
          XtVaGetValues( recnData->debit, XmNrows, &nrows, NULL );
          XbaeMatrixAddRows( recnData->debit, nrows, rows, NULL, NULL, 1 );
          XbaeMatrixSetRowUserData( recnData->debit, nrows, (XtPointer)trans );
          }
        else
          {
          XtVaGetValues( recnData->credit, XmNrows, &nrows, NULL );
          XbaeMatrixAddRows( recnData->credit, nrows, rows, NULL, NULL, 1 );
          XbaeMatrixSetRowUserData( recnData->credit,nrows, (XtPointer)trans );
          }
        }
      }
    
    recnRecalculateBalance(recnData);
    }
  }

  
/********************************************************************\
 * recnRecalculateBalance                                           *
 *   refreshes the balances in the reconcile window                 *
 *                                                                  *
 * Args:   recnData -- the reconcile window to refresh              *
 * Return: none                                                     *
\********************************************************************/
void
recnRecalculateBalance( RecnWindow *recnData )
  {
  Transaction *trans;
  Account *acc = recnData ->acc;
  char buf[BUFSIZE];
  int  i,nrows;
  double ddebit  = 0.0;
  double dcredit = 0.0;
  double ddiff   = 0.0;
  
  /* Calculate the total debit: */
  XtVaGetValues( recnData->debit, XmNrows, &nrows, NULL );
  for( i=0; i<nrows; i++ )
    {
    String recn = XbaeMatrixGetCell( recnData->debit, i, 0 );
    if( recn[0] == YREC )
      {
      trans  = (Transaction *)XbaeMatrixGetRowUserData( recnData->debit, i );
      ddebit += xaccGetAmount (acc, trans);
      }
    }
  
  /* Calculate the total credit: */
  XtVaGetValues( recnData->credit, XmNrows, &nrows, NULL );
  for( i=0; i<nrows; i++ )
    {
    String recn = XbaeMatrixGetCell( recnData->credit, i, 0 );
    if( recn[0] == YREC )
      {
      trans  = (Transaction *)XbaeMatrixGetRowUserData( recnData->credit, i );
      dcredit += xaccGetAmount (acc, trans);
      }
    }
  
  /* Update the difference field, and the total fields */
  sprintf( buf, " $ %.2f\0", DABS(ddebit) );
  XmTextSetString( recnData->totDebit, buf );
  
  sprintf( buf, " $ %.2f\0", dcredit );
  XmTextSetString( recnData->totCredit, buf );

  ddiff = recnData->ddiff + dcredit + ddebit;
  if( 0.0 > ddiff )
    sprintf( buf, "-$ %.2f\0", DABS(ddiff) );
  else
    sprintf( buf, " $ %.2f\0", ddiff );    
  XmTextSetString( recnData->difference, buf );
  }

/********************************************************************\
 * startRecnWindow:  gets the ending balance for reconcile window   *
\********************************************************************/
void
startRecnOkCB( Widget wm, XtPointer cd, XtPointer cb )
  { *(int *)cd = 1; }
void
startRecnCancelCB( Widget wm, XtPointer cd, XtPointer cb )
  { *(int *)cd = 0; }

/********************************************************************\
 * startRecnWindow                                                  *
 *   opens up the window to prompt the user to enter the ending     *
 *   balance from bank statement                                    *
 *                                                                  *
 * NOTE: This dialog does not return until the user presses "Ok"    *
 *       or "Cancel"                                                *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account to reconcile                       *
 *         diff    - returns the amount from ending balance field   *
 * Return: True, if the user presses "Ok", else False               *
 * Global: app - the app context                                    *
\********************************************************************/
Boolean
startRecnWindow( Widget parent, Account *acc, double *diff )
  {
  Widget   dialog,
           pane,
           controlform,
           actionform,
           widget, endB, newB;
  Transaction *trans;
  char   buf[BUFSIZE];
  int    j;
  double dendBalance;
  int    done=-1;
  
  setBusyCursor( parent );
  
  /* Figure out previous ending balance: */
  dendBalance=0.0;
  j=0;
  while( (trans = getTransaction(acc,j++)) != NULL )
    if( trans->reconciled == YREC )
      dendBalance += xaccGetAmount (acc, trans);
  
  /* Create the dialog box... XmNdeleteResponse is set to
   * XmDESTROY so the dialog's memory is freed when it is closed */
  sprintf( buf, "%s: Reconcile", acc->accountName );
  dialog = XtVaCreatePopupShell( "dialog", 
                                 xmDialogShellWidgetClass,	parent,
                                 XmNdialogStyle,    XmDIALOG_APPLICATION_MODAL,
                                 XmNtitle,          buf,
                                 XmNdeleteResponse, XmDESTROY,
                                 XmNminWidth,       250,
                                 XmNminHeight,      150,
                                 XmNresizable,        FALSE,
                                 XmNallowShellResize, FALSE,
                                 XmNtransient,        FALSE,  /* allow window to be repositioned */
                                 NULL );
  
  /* Create a PanedWindow Manager for the dialog box... the child 
   * of optiondialog the paned window is the parent of the two 
   * forms which comprise the two areas of the dialog box...
   * The sash is set to minimun size to make it invisible */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, dialog,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNtraversalOn,   False,
                           NULL );
  
  /** CONTROLFORM ****************************************
   * Create a controlform for control area of dialog box */
  controlform = XtVaCreateWidget( "controlform", 
                                  xmFormWidgetClass, pane,
                                  NULL );
  
  widget = XtVaCreateManagedWidget( "Previous Balance: $",
                                    xmLabelGadgetClass, controlform,
                                    XmNtopAttachment,   XmATTACH_FORM,
                                    XmNtopOffset,       10,
                                    XmNrightAttachment, XmATTACH_POSITION,
                                    XmNrightPosition,   50,
                                    NULL );
  
  sprintf( buf, "%.2f", dendBalance );
  endB = XtVaCreateManagedWidget( "text",
                                  xmTextWidgetClass,  controlform,
                                  XmNvalue,           buf,
                                  XmNeditable,        False,
                                  XmNtopAttachment,   XmATTACH_FORM,
                                  XmNtopOffset,       10,
                                  XmNleftAttachment,  XmATTACH_POSITION,
                                  XmNleftPosition,    50,
                                  NULL );
  
  widget = XtVaCreateManagedWidget( "Ending Balance: $",
                                    xmLabelGadgetClass, controlform,
                                    XmNtopAttachment,   XmATTACH_WIDGET,
                                    XmNtopWidget,       endB,
                                    XmNrightAttachment, XmATTACH_POSITION,
                                    XmNrightPosition,   50,
                                    NULL );
  
  newB = XtVaCreateManagedWidget( "text",
                                  xmTextWidgetClass,  controlform,
                                  XmNeditable,        True,
                                  XmNtopAttachment,   XmATTACH_WIDGET,
                                  XmNtopWidget,       endB,
                                  XmNleftAttachment,  XmATTACH_POSITION,
                                  XmNleftPosition,    50,
                                  NULL );
  
  /* The amountCB ensures the data entered in the amount field is
   * in a valid format */
  XtAddCallback( newB, XmNmodifyVerifyCallback,
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
  widget = XtVaCreateManagedWidget( "Ok", 
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
                 startRecnOkCB, (XtPointer)&done );
  
  /* The cancel button! */
  widget = XtVaCreateManagedWidget( "Cancel", 
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
                 startRecnCancelCB, (XtPointer)&done );

  /* A help button will pop-up context sensitive help */
  widget = XtVaCreateManagedWidget( "Help", 
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
                 helpMenubarCB, (XtPointer)HMB_RECNWIN );

  /* Fix action area of the pane to its current size, and not let it
   *  resize. */
  XtManageChild( actionform );
  {
  Dimension h;
  XtVaGetValues( widget, XmNheight, &h, NULL );
  XtVaSetValues( actionform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
  }
  
  XtManageChild( pane );
  XtPopup( dialog, XtGrabNone );
  
  unsetBusyCursor( parent );
  
  /* while the user hasn't pushed "Ok", simulate XtMainLoop. */
  while( (done == -1) || XtAppPending(app) )
    XtAppProcessEvent( app, XtIMAll );

  /* Get the amount from the "end-balance" field */
  {
  String str;
  float val=0.0;
  str = XmTextGetString(newB);
  sscanf( str, "%f", &val ); /* sscanf must take float not double as arg */
  *diff = dendBalance - ((double) val);
  }
  
  XtDestroyWidget(dialog);
  
  return done;
  }

/********************************************************************\
 * recnWindow                                                       *
 *   opens up the window to reconcile an account                    *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to reconcile                       *
 * Return: recnData - the instance of this RecnWindow               *
\********************************************************************/
RecnWindow *
recnWindow( Widget parent, Account *acc )
  {
  Widget pane, form, widget;
  int    position;
  char   title[BUFSIZE];
  RecnWindow *recnData;
  double ddiff;

  /* Popup a little window to prompt the user to enter the
   * ending balance for his/her bank statement */
  if( !startRecnWindow(parent,acc,&ddiff) )
    return NULL;
    
  setBusyCursor(parent);
  
  recnData = (RecnWindow *)_malloc(sizeof(RecnWindow));
  recnData->acc = acc;
  recnData->ddiff = ddiff;
  
  sprintf( title, "%s: Reconcile", acc->accountName );
  
  /* force the size of the dialog so it is not resizable */
  recnData->dialog =
    XtVaCreatePopupShell( "dialog", 
                          xmDialogShellWidgetClass, parent,
                          XmNtitle,            title,
                          XmNdeleteResponse,   XmDESTROY,
                          NULL );
  
  XtAddCallback( recnData->dialog, XmNdestroyCallback, 
                 recnClose, (XtPointer)recnData );
  
  /* The reconcile window is a paned window, with the top pane
   * used for the Debits, and Credits matrices, and the bottom pane
   * has the misc stuff, like "New Balance" field, "Difference" field,
   * and the buttons.  The Debit/Credit pane have a matrix, and a
   * "total" field */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, recnData->dialog,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNtraversalOn,   False,
                           NULL );

  /******************************************************************\
   * The top pane has the debit and credit matrices                 *
  \******************************************************************/
    {
    Widget frame, rowcol;
    short  colWidths[] = {1,5,8,20,8};   /* the widths of columns */
    String labels[]    = {"","Num","Date","Description","Amount"};
    unsigned char alignments[] = {XmALIGNMENT_CENTER,
                                  XmALIGNMENT_END,
                                  XmALIGNMENT_CENTER,
                                  XmALIGNMENT_BEGINNING,
                                  XmALIGNMENT_END};
    
    rowcol = XtVaCreateWidget( "rowcol",
                               xmRowColumnWidgetClass, pane,
                               XmNnumColumns,  2,
                               XmNorientation, XmHORIZONTAL,
                               NULL );
    
    /******************************************************************\
     * The "Debits" area                                              *
    \******************************************************************/
    form = XtVaCreateWidget( "form",
                             xmFormWidgetClass, rowcol,
                             NULL );
    
    widget = XtVaCreateManagedWidget( "Debits:",
                                      xmLabelGadgetClass, form,
                                      XmNtopAttachment,   XmATTACH_FORM,
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      20,
                                      NULL );
    
    frame = XtVaCreateWidget( "frame", 
                              xmFrameWidgetClass, form,
                              XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,       widget,
                              XmNleftAttachment,  XmATTACH_FORM,
                              NULL );
    
    recnData->debit =
      XtVaCreateWidget( "recn",
                        xbaeMatrixWidgetClass,  frame,
                        XmNfixedRows,           0,
                        XmNfixedColumns,        0,
                        XmNrows,                1,
                        XmNvisibleRows,         10,
                        XmNfill,                True,
                        XmNcolumns,             5,
                        XmNcolumnLabels,        labels,
                        XmNcolumnWidths,        colWidths,
                        XmNcolumnAlignments,    alignments,
                        XmNtraverseFixedCells,  False,
                        XmNgridType,            XmGRID_SHADOW_IN,
                        XmNshadowType,          XmSHADOW_ETCHED_IN,
                        XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
                        XmNselectScrollVisible, True,
                        NULL );
    
    XtAddCallback( recnData->debit, XmNenterCellCallback,
                   recnCB, (XtPointer)recnData );
    
    XtManageChild(recnData->debit);
    widget = XtVaCreateManagedWidget( "Total:",
                                      xmLabelGadgetClass, form,
                                      XmNtopAttachment,   XmATTACH_WIDGET,
                                      XmNtopWidget,       frame,
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      20,
                                      NULL );
    
    recnData->totDebit = 
      XtVaCreateManagedWidget( "text",
                               xmTextWidgetClass,  form,
                               XmNeditable,        False,
                               XmNmarginHeight,    1,
                               XmNmarginWidth,     1,
                               XmNmaxLength,       10,
                               XmNcolumns,         10,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       frame,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,     20,
                               NULL );
    
    XtManageChild(frame);
    XtManageChild(form);

    /******************************************************************\
     * The "Credits" area                                             *
    \******************************************************************/
    form = XtVaCreateWidget( "form",
                             xmFormWidgetClass, rowcol,
                             NULL );
    
    widget = XtVaCreateManagedWidget( "Credits:",
                                      xmLabelGadgetClass, form,
                                      XmNtopAttachment,   XmATTACH_FORM,
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      20,
                                      NULL );
    
    frame = XtVaCreateWidget( "frame", 
                              xmFrameWidgetClass, form,
                              XmNtopAttachment,   XmATTACH_WIDGET,
                              XmNtopWidget,       widget,
                              XmNleftAttachment,  XmATTACH_FORM,
                              NULL );
    
    recnData->credit =
      XtVaCreateWidget( "recn",
                        xbaeMatrixWidgetClass,  frame,
                        XmNfixedRows,           0,
                        XmNfixedColumns,        0,
                        XmNrows,                1,
                        XmNvisibleRows,         10,
                        XmNfill,                True,
                        XmNcolumns,             5,
                        XmNcolumnLabels,        labels,
                        XmNcolumnWidths,        colWidths,
                        XmNcolumnAlignments,    alignments,
                        XmNtraverseFixedCells,  False,
                        XmNgridType,            XmGRID_SHADOW_IN,
                        XmNshadowType,          XmSHADOW_ETCHED_IN,
                        XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
                        XmNselectScrollVisible, True,
                        NULL );
    
    XtAddCallback( recnData->credit, XmNenterCellCallback,
                   recnCB, (XtPointer)recnData );
    
    XtManageChild(recnData->credit);
    widget = XtVaCreateManagedWidget( "Total:",
                                      xmLabelGadgetClass, form,
                                      XmNtopAttachment,   XmATTACH_WIDGET,
                                      XmNtopWidget,       frame,
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      20,
                                      NULL );
    
    recnData->totCredit = 
      XtVaCreateManagedWidget( "text",
                               xmTextWidgetClass,  form,
                               XmNeditable,        False,
                               XmNmarginHeight,    1,
                               XmNmarginWidth,     1,
                               XmNmaxLength,       10,
                               XmNcolumns,         10,
                               XmNtopAttachment,   XmATTACH_WIDGET,
                               XmNtopWidget,       frame,
                               XmNrightAttachment, XmATTACH_FORM,
                               XmNrightOffset,     20,
                               NULL );
    
    XtManageChild(frame);
    XtManageChild(form);

    XtManageChild(rowcol);
    }
    
  /******************************************************************\
   * The buttons at the bottom...                                   *
  \******************************************************************/

  form = XtVaCreateWidget( "form",
                           xmFormWidgetClass, pane,
                           XmNfractionBase,   6,
                           NULL );
  position=0;
  
  widget = XtVaCreateManagedWidget( "Difference:",
                                    xmLabelGadgetClass, form,
                                    XmNtopAttachment,   XmATTACH_FORM,
                                    XmNbottomAttachment,XmATTACH_FORM,
                                    XmNleftAttachment,  XmATTACH_POSITION,
                                    XmNleftPosition,    position,
                                    XmNrightAttachment, XmATTACH_POSITION,
                                    XmNrightPosition,   position+1,
                                    NULL );

  position ++;
  recnData->difference = 
    XtVaCreateManagedWidget( "text",
                             xmTextWidgetClass,  form,
                             XmNeditable,        False,
                             XmNmarginHeight,    1,
                             XmNmarginWidth,     1,
                             XmNmaxLength,       10,
                             XmNcolumns,         10,
                             XmNtopAttachment,   XmATTACH_FORM,
                             XmNtopOffset,       6,
                             XmNbottomAttachment,XmATTACH_FORM,
                             XmNbottomOffset,    6,
                             XmNleftAttachment,  XmATTACH_POSITION,
                             XmNleftPosition,    position,
                             NULL );
  
  position +=2;
  
  /* The "Ok" button: */
  widget = XtVaCreateManagedWidget( "Ok", 
                                    xmPushButtonWidgetClass, form,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       position,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      position+1,
                                    XmNshowAsDefault,      True,
                                    NULL );

  XtAddCallback( widget, XmNactivateCallback, 
                 recnOkCB, (XtPointer)recnData );
  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)(recnData->dialog) );  
  /* The "Cancel" button: */
  position ++;
  widget = XtVaCreateManagedWidget( "Cancel", 
                                    xmPushButtonWidgetClass, form,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       position,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      position+1,
                                    XmNshowAsDefault,      True,
                                    NULL );

  XtAddCallback( widget, XmNactivateCallback, 
                 destroyShellCB, (XtPointer)(recnData->dialog) );  
  
  /* The "Help" button pops up the reconcile window help page: */
  position ++;
  widget = XtVaCreateManagedWidget( "Help", 
                                    xmPushButtonWidgetClass, form,
                                    XmNtopAttachment,      XmATTACH_FORM,
                                    XmNbottomAttachment,   XmATTACH_FORM,
                                    XmNleftAttachment,     XmATTACH_POSITION,
                                    XmNleftPosition,       position,
                                    XmNrightAttachment,    XmATTACH_POSITION,
                                    XmNrightPosition,      position+1,
                                    XmNshowAsDefault,      True,
                                    NULL );
  
  XtAddCallback( widget, XmNactivateCallback,
                 helpMenubarCB, (XtPointer)HMB_RECNWIN );
  
  /* Fix button area of the pane to its current size, and not let 
   * it resize. */
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
  XtManageChild(form);
  XtManageChild(pane);
  XtManageChild(recnData->dialog);
  
  /* now that the matices are set up, fill 'em in with transactions: */
  recnRefresh(recnData);
  /* and then refresh the total/difference balance fields: */
  recnRecalculateBalance(recnData);
  
  unsetBusyCursor(parent);

  return recnData;
  }

/********************************************************************\
 * recnClose                                                        *
 *   frees memory allocated for an recnWindow, and other cleanup    *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - recnData - the data struct for this window          *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void 
recnClose( Widget mw, XtPointer cd, XtPointer cb )
  {
  RecnWindow *recnData = (RecnWindow *)cd;
  Account *acc = recnData->acc;
  
  _free(recnData);
  acc->recnData = NULL;
  
  DEBUG("closed RecnWindow");
  }

/********************************************************************\
 * recnOkCB                                                         *
 *   saves account stuff, when the user clicks "Ok"                 *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - recnData - the data struct for this window          *
 *         cb -                                                     *
 * Return: none                                                     *
 * Global: data                                                     *
\********************************************************************/
void 
recnOkCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int nrows,i;
  Transaction *trans;
  RecnWindow  *recnData = (RecnWindow *)cd;
  AccountGroup *grp = topgroup;  /* hack alert -- should pass as arg .. */
  
  /* Update the debit transactions: */
  XtVaGetValues( recnData->debit, XmNrows, &nrows, NULL );
  for( i=0; i<nrows; i++ )
    {
    String recn = XbaeMatrixGetCell( recnData->debit, i, 0 );
    if( recn[0] == YREC )
      {
      trans  = (Transaction *)XbaeMatrixGetRowUserData( recnData->debit, i );
      trans->reconciled = YREC;
      /* mark the datafile as needing to be saved: */
      grp->saved = False;
      }
    }
  
  /* Update the credit transactions: */
  XtVaGetValues( recnData->credit, XmNrows, &nrows, NULL );
  for( i=0; i<nrows; i++ )
    {
    String recn = XbaeMatrixGetCell( recnData->credit, i, 0 );
    if( recn[0] == YREC )
      {
      trans  = (Transaction *)XbaeMatrixGetRowUserData( recnData->credit, i );
      trans->reconciled = YREC;
      /* mark the datafile as needing to be saved: */
      grp->saved = False;
      }
    }
  
  /* refresh the register window */
  regRefresh(recnData->acc->regData);
  }

/********************************************************************\
 * recnCB                                                           *
 *   called whenever the users does anything in the debit/credit    *
 *   matrices                                                       *
 *                                                                  *
 * Args:   mw - the matrix widget that called us                    *
 *         cd - recnData - the data struct for this window          *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void
recnCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RecnWindow *recnData = (RecnWindow *)cd;
  
  XbaeMatrixEnterCellCallbackStruct *cbs =
    (XbaeMatrixEnterCellCallbackStruct *)cb;
  int rows = XbaeMatrixNumRows(mw);
  
  cbs->doit = False;
  cbs->map  = False;
  
  XbaeMatrixDeselectAll(mw);
  XbaeMatrixSelectRow( mw, cbs->row );
  
  /* If we are in the "reconciled" cell, toggle value */
  if( cbs->column == 0 )
    {
    char   buf[BUFSIZE];
    String val = XbaeMatrixGetCell( mw, cbs->row, cbs->column );

    if( val[0] == YREC )
      {
      Transaction *trans =
        (Transaction *)XbaeMatrixGetRowUserData( mw, cbs->row );
      
      sprintf( buf, "%c", trans->reconciled );
      XbaeMatrixSetCell( mw, cbs->row, cbs->column, buf );
      }
    else
      {
      sprintf( buf, "%c", YREC );
      XbaeMatrixSetCell( mw, cbs->row, cbs->column, buf );
      }
    
    /* recalculate the total/difference balance fields: */
    recnRecalculateBalance(recnData);
    }
  }

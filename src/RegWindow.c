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
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/LabelGP.h>
#include <Xbae/Matrix.h>
#include "main.h"
#include "util.h"
#include "date.h"
#include "Data.h"
#include "Account.h"
#include "MainWindow.h"
#include "QuickFill.h"
#include "BuildMenu.h"
#include "RecnWindow.h"
#include "AdjBWindow.h"

/** STRUCTS *********************************************************/
/* The RegWindow struct contains info needed by an instance of an open 
 * register.  Any state info for the regWindow goes here. */
typedef struct _RegWindow {
  Account *acc;               /* The account associated with this regwin */
  Widget   dialog;
  Widget   reg;               /* The matrix widget...                    */
  Widget   balance;           /* The balance text field                  */
  unsigned short changed;     /* bitmask of fields that have changed in  *
                               * transaction lastTrans                   */
  unsigned short lastTrans;   /* to keep track of last edited transaction*/
  XmTextPosition insert;      /* used by quickfill for detecting deletes */
  QuickFill      *qf;         /* keeps track of current quickfill node.  *
                               * Reset to Account->qfRoot when entering  *
                               * a new transaction                       */
} RegWindow;


/** PROTOTYPES ******************************************************/
double regRecalculateBalance( RegWindow *regData );
void regSaveTransaction( RegWindow *regData, int position );

void closeRegWindow( Widget mw, XtPointer cd, XtPointer cb );
void startRecnCB( Widget mw, XtPointer cd, XtPointer cb );
void startAdjBCB( Widget mw, XtPointer cd, XtPointer cb );
void recordCB( Widget mw, XtPointer cd, XtPointer cb );
void deleteCB( Widget mw, XtPointer cd, XtPointer cb );
void cancelCB( Widget mw, XtPointer cd, XtPointer cb );
void regCB( Widget mw, XtPointer cd, XtPointer cb );
void dateCellFormat( Widget mw, XbaeMatrixModifyVerifyCallbackStruct *mvcbs );


/** GLOBALS *********************************************************/
extern Widget  toplevel;
extern Data   *data;

/* The XrmQuarks are used by regCB to figure out the kind of traversal 
 * that is going on */
static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
static Boolean haveQuarks = False;

/* Pixel values are used to color the balance field text 
 * when computing the balance */
extern Pixel posPixel;
extern Pixel negPixel;

/* The modified flags... keep track of what in a transaction
 * has been modified. */
#define MOD_NONE  0x00
#define MOD_DATE  0x01
#define MOD_NUM   0x02
#define MOD_DESC  0x04
#define MOD_RECN  0x08
#define MOD_AMNT  0x10
#define MOD_SHRS  0x20
#define MOD_PRIC  0x40
#define MOD_MEMO  0x80
#define MOD_NEW   0x100
#define MOD_ALL   0x1ff

/* ??? TODO: Use these #defines, instead of hard-coding cell
 * locations throughout the code */ 
#define DATE_CELL_R  0
#define DATE_CELL_C  0
#define NUM_CELL_R   0
#define NUM_CELL_C   1
#define DESC_CELL_R  0
#define DESC_CELL_C  2
#define RECN_CELL_R  0
#define RECN_CELL_C  3
#define PAY_CELL_R   0
#define PAY_CELL_C   4
#define DEP_CELL_R   0
#define DEP_CELL_C   5

#define PRIC_CELL_C   6
#define SHRS_CELL_C   7

#define BALN_CELL_R  0
/* #define BALN_CELL_C  6 */
#define BALN_CELL_C  ((acc->numCols) -1)   /* the last column */


#define MEMO_CELL_R  1
#define MEMO_CELL_C  2

/** COOL MACROS *****************************************************/
#define IN_DATE_CELL(R,C) (((R-1)%2==0) && (C==0))  /* Date cell        */
#define IN_NUM_CELL(R,C)  (((R-1)%2==0) && (C==1))  /* Number cell      */
#define IN_DESC_CELL(R,C) (((R-1)%2==0) && (C==2))  /* Description cell */
#define IN_RECN_CELL(R,C) (((R-1)%2==0) && (C==3))  /* Reconciled cell  */
#define IN_PAY_CELL(R,C)  (((R-1)%2==0) && (C==4))  /* Payment cell     */
#define IN_DEP_CELL(R,C)  (((R-1)%2==0) && (C==5))  /* Deposit cell     */
/* #define IN_BALN_CELL(R,C) (((R-1)%2==0) && (C==6))  /* Balance cell     */
#define IN_BALN_CELL(R,C) (((R-1)%2==0) && (C==BALN_CELL_C))  /* Balance cell     */
#define IN_PRIC_CELL(R,C) (((R-1)%2==0) && (C==PRIC_CELL_C))  /* Balance cell     */
#define IN_YEAR_CELL(R,C) (((R-1)%2==1) && (C==0))  /* Year cell        */
#define IN_MEMO_CELL(R,C) (((R-1)%2==1) && (C==2))  /* Memo cell        */
#define IN_BAD_CELL(R,C)  (((R-1)%2==1) && (C==3))  /* cell after memo  */


/********************************************************************/


/********************************************************************\
 * regRefresh                                                       *
 *   refreshes the register matrix to reflect the current           *
 *   transactions                                                   *
 *                                                                  *
 * Args:   regData -- this RegWindow                                *
 * Return: NONE                                                     *
\********************************************************************/
void
regRefresh( RegWindow *regData )
  {
  if( regData != NULL )
    {
    Transaction *trans;
    int    i,j,nrows,drows,nnrows,ncols;
    char   buf[BUFSIZE];
    String **data = NULL;
    String **newData;
    Account *acc;
    
    XtVaGetValues( regData->reg, XmNrows, &nrows, NULL );
    XtVaGetValues( regData->reg, XmNcells, &data, NULL );
    
    /* The number of rows we need to add/subtract (ie, delta-rows :) */
    nnrows = (regData->acc->numTrans)*2 + 3; 
    drows  = (nnrows-1) - (nrows-1);
    ncols  = regData->acc->numCols; 
    acc = regData->acc;

    /* allocate a new matrix: */
    newData = (String **)_malloc(nnrows*sizeof(String *));
    for( i=0; i<nnrows; i++ )
      {
      newData[i] = (String *)_malloc(ncols*sizeof(String *));
      for( j=0; j<ncols; j++ )
        newData[i][j] = NULL;
      }

    /* add the column headers, from the old data: */
    for( j=0; j<ncols; j++ )
      newData[0][j] = XtNewString(data[0][j]);
    
    /* adjust the size of the matrix, only after copying old column headers: */
    if( drows < 0 )
      XbaeMatrixDeleteRows( regData->reg, 1, -drows );
    else if( drows > 0 )
      XbaeMatrixAddRows( regData->reg, 1, NULL, NULL, NULL, drows );
    
    /* and fill in the data for the matrix: */
    i=-1;
    while( (trans=getTransaction(regData->acc,++i)) != NULL )
      {
      int  row = i*2+1;
      
      sprintf( buf, "%2d/%2d\0", 
               trans->date.month,
               trans->date.day );
      newData[row][DATE_CELL_C]   = XtNewString(buf);
      
      sprintf( buf, "%4d", trans->date.year );
      newData[row+1][DATE_CELL_C] = XtNewString(buf);
      
      sprintf( buf, "%s", trans->num );
      newData[row][NUM_CELL_C]   = XtNewString(buf);
      
      newData[row+1][NUM_CELL_C] = XtNewString("");
      
      sprintf( buf, "%s", trans->description );
      newData[row][DESC_CELL_C]   = XtNewString(buf);
      
      sprintf( buf, "%s", trans->memo );
      newData[row+1][DESC_CELL_C] = XtNewString(buf);
      
      sprintf( buf, "%c", trans->reconciled );
      newData[row][RECN_CELL_C]   = XtNewString(buf);

      newData[row+1][RECN_CELL_C] = XtNewString("");
      
      if( 0.0 > trans->damount )
        {
        sprintf( buf, "%.2f ", (-1.0*(trans->damount)) );
        newData[row][PAY_CELL_C] = XtNewString(buf);
        newData[row][DEP_CELL_C] = XtNewString("");
        }
      else
        {
        sprintf( buf, "%.2f ", (trans->damount) );
        newData[row][PAY_CELL_C] = XtNewString("");
        newData[row][DEP_CELL_C] = XtNewString(buf);
        }
      
      newData[row+1][PAY_CELL_C] = XtNewString("");
      newData[row+1][DEP_CELL_C] = XtNewString("");
      
      newData[row][BALN_CELL_C]   = XtNewString("");
      newData[row+1][BALN_CELL_C] = XtNewString("");

      /* ----------------------------------- */
      /* extra columns for mutual funds, etc. */
      switch(acc->type)
        {
        case BANK:
        case CASH:
        case ASSET:
        case CREDIT:
        case LIABILITY:
          break;
        case PORTFOLIO:
        case MUTUAL:
          sprintf( buf, "%.2f ", trans->share_price );
          newData[row][PRIC_CELL_C] = XtNewString(buf);
          newData[row+1][PRIC_CELL_C] = XtNewString("");
          newData[row][SHRS_CELL_C]   = XtNewString("");
          newData[row+1][SHRS_CELL_C] = XtNewString("");
          break;
        default:
          fprintf( stderr, "Ineternal Error: Account type: %d is unknown!\n", regData->acc->type);
        }
      }

    /* fill in the empty cells at the end: */
    {
    Date date;
    todaysDate( &date );
    
    for( i=(2*i)+1; i<nnrows; i++ )
      {
      for( j=0; j<ncols; j++ )
        {
        if( IN_DATE_CELL(i,j) )
          {
          sprintf( buf, "%2d/%2d\0", date.month, date.day );
          newData[i][j] = XtNewString(buf);
          }
        else if( IN_YEAR_CELL(i,j) )
          {
          sprintf( buf, "%4d", date.year );
          newData[i][j] = XtNewString(buf);
          }
        else if( IN_RECN_CELL(i,j) )
          {
          sprintf( buf, "%c", NREC );
          newData[i][j] = XtNewString(buf);
          }
        else
          newData[i][j] = XtNewString("");
        }
      }
    }
    
    /* set the cell data: */
    XtVaSetValues( regData->reg, XmNcells, newData, NULL );
    regRecalculateBalance(regData);
    
    /* and free memory!!! */
    /* ??? */
    }
  }


/********************************************************************\
 * regRecalculateBalance                                            *
 *   called by regCB in the event that the displayed balances may   *
 *   no longer correct... recomputes and displays the new balances. *
 *                                                                  *
 * Args:   regData -- this RegWindow                                *
 * Return: the final balance                                        *
\********************************************************************/
double
regRecalculateBalance( RegWindow *regData )
  {
  int  i; 
  int  position   = 1;
  double  dbalance    = 0.0;
  double  share_balance    = 0.0;
  double  dclearedBalance = 0.0;
  double  share_clearedBalance = 0.0;
  char buf[BUFSIZE];
  Transaction *trans;
  Account *acc;
  Widget reg;
  
  if( regData != NULL ) {
    reg = regData->reg;
    acc = regData->acc;
  } else {
    reg = NULL;
    acc = NULL;
  }
  
  for( i=0; (trans=getTransaction(regData->acc,i)) != NULL; i++ )
    {
    share_balance += trans->damount;
    dbalance = trans -> share_price * share_balance;
    
    if( trans->reconciled != NREC ) {
      share_clearedBalance += trans->damount;
      dclearedBalance = trans->share_price * share_clearedBalance;
    }
    
    if( reg != NULL )
      {
#ifdef USE_NO_COLOR
      sprintf( buf, "%.2f ", dbalance );
#else
      sprintf( buf, "%.2f ", DABS(dbalance) );
      
      /* Set the color of the text, depending on whether the
       * balance is negative or positive */
      if( 0.0 > dbalance )
        XbaeMatrixSetCellColor( reg, position, BALN_CELL_C, negPixel );
      else
        XbaeMatrixSetCellColor( reg, position, BALN_CELL_C, posPixel );
#endif
      
      /* Put the value in the cell */
      XbaeMatrixSetCell( reg, position, BALN_CELL_C, buf );

      /* update share balances too ... */
      if( (MUTUAL   == acc->type) ||
          (PORTFOLIO == acc->type) ) 
        {
#ifdef USE_NO_COLOR
        sprintf( buf, "%.2f ", share_balance );
#else
        sprintf( buf, "%.2f ", DABS(share_balance) );
        
        /* Set the color of the text, depending on whether the
         * balance is negative or positive */
        if( 0.0 > share_balance )
          XbaeMatrixSetCellColor( reg, position, SHRS_CELL_C, negPixel );
        else
          XbaeMatrixSetCellColor( reg, position, SHRS_CELL_C, posPixel );
        }
#endif
      
      /* Put the value in the cell */
      XbaeMatrixSetCell( reg, position, SHRS_CELL_C, buf );

      position+=2;            /* each transaction has two rows */
      }
    }
  
  if( regData != NULL )
    {
    if( regData->balance != NULL )
      {
      sprintf( buf, "$ %.2f \n$ %.2f \0", 
               dbalance, dclearedBalance );
      
      XmTextSetString( regData->balance, buf );
      }
    }
  
  refreshMainWindow();        /* make sure the balance field in
                               * the main window is up to date */
  return dbalance;
  }

/********************************************************************\
 * regSaveTransaction                                               *
 *   saves the data in transaction num in accout from XbaeMatrix    *
 *                                                                  *
 * Args:   regData  - this RegWindow                                *
 *         position - the transaction number                        *
 * Return: none                                                     *
 * Global: data - the data for open datafile                        *
\********************************************************************/
void
regSaveTransaction( RegWindow *regData, int position )
  {
  /* save transaction structure... in order to speed this up, 
   * regData->changed is a bitfield that keeps track of things
   * that might have changed, so we only have to save the stuff
   * that has changed */
  Boolean outOfOrder = False;
  char buf[BUFSIZE];
  int  newPosition;
  int  row = (position * 2) + 1;
  
  Transaction *trans;
  trans = getTransaction( regData->acc, position );
  
  /* If anything changes, we have to set this flag, so we
   * remember to prompt the user to save! */
  if( regData->changed != MOD_NONE )
    data->saved = False;
  
  if( trans == NULL )
    {
    /* This must be a new transaction */
    DEBUG("New Transaction");
    
    /* If nothing has changed, we don't want to save this */
    if( !(regData->changed & MOD_ALL) )
      return;
    
    trans = (Transaction *)_malloc(sizeof(Transaction));
    
    /* fill in some sane defaults */
    trans->num         = NULL;
    trans->description = NULL;
    trans->memo        = NULL;
    trans->catagory    = 0;
    trans->reconciled  = NREC;
    trans->damount     = 0.0;
    trans->share_price = 1.0;
    
    regData->changed = MOD_ALL;
    }
  if( regData->changed & MOD_NUM )
    {
    DEBUG("MOD_NUM");	  
    /* ...the transaction number (String)... */
    XtFree( trans->num );
    trans->num = XtNewString( XbaeMatrixGetCell(regData->reg,row,NUM_CELL_C) );    
    }
  
  if( regData->changed & MOD_DESC )
    {
    DEBUG("MOD_DESC");
    /* ... the description... */
    XtFree( trans->description );
    trans->description = 
      XtNewString( XbaeMatrixGetCell(regData->reg,row,DESC_CELL_C) );
    }
  
  if( regData->changed & MOD_MEMO )
    {
    DEBUG("MOD_MEMO");
    /* ... the memo ... */
    XtFree( trans->memo );
    trans->memo = XtNewString( XbaeMatrixGetCell(regData->reg,row+1,MEMO_CELL_C) );
    }
  
  if( regData->changed & MOD_RECN )
    {
    DEBUG("MOD_RECN");
    /* ...the reconciled flag (char)... */
    trans->reconciled = (XbaeMatrixGetCell(regData->reg,row,RECN_CELL_C))[0];
    
    /* Remember, we need to recalculate the reconciled balance now! */
    regRecalculateBalance(regData);
    }
  
  if( regData->changed & MOD_AMNT )
    {
    String amount;
    float val=0.0;  /* must be float for sscanf to work */
    DEBUG("MOD_AMNT");
    /* ...and the amounts */
    amount = XbaeMatrixGetCell(regData->reg,row,DEP_CELL_C);
    sscanf( amount, "%f", &val );
    trans->damount = val;
    
    val = 0.0;
    amount = XbaeMatrixGetCell(regData->reg,row,PAY_CELL_C); 
    sscanf( amount, "%f", &val );
    trans->damount -= val;
    
    /* Reset so there is only one field filled */
    if( 0.0 > trans->damount )
      {
/* hack alert -- keep 3 digits for share amounts */
      sprintf( buf, "%.2f ", (-1.0*(trans->damount)) );
      XbaeMatrixSetCell( regData->reg, row, PAY_CELL_C, buf );
      XbaeMatrixSetCell( regData->reg, row, DEP_CELL_C, "" );
      }
    else
      {
      sprintf( buf, "%.2f ", (trans->damount) );
      XbaeMatrixSetCell( regData->reg, row, PAY_CELL_C, "" );
      XbaeMatrixSetCell( regData->reg, row, DEP_CELL_C, buf );
      }
    
    regRecalculateBalance(regData);
    }

  if( regData->changed & MOD_PRIC )
    {
    String price;
    float val=0.0;  /* must be float for sscanf to work */

    DEBUG("MOD_PRIC");
    /* ...the price flag ... */
    trans->reconciled = (XbaeMatrixGetCell(regData->reg,row,PRIC_CELL_C))[0];

    price = XbaeMatrixGetCell(regData->reg,row,PRIC_CELL_C);
    sscanf( price, "%f", &val );
    trans->share_price = val;
printf ("got share price %f \n", val);
    
    sprintf( buf, "%.2f ", trans->share_price );
    XbaeMatrixSetCell( regData->reg, row, PRIC_CELL_C, buf );
    
    /* Remember, we need to recalculate the reconciled balance now! */
    regRecalculateBalance(regData);
    }
  
  /* Before we check the date, and possibly insert the new
   * transaction, we need to make sure that, if this is a
   * new transaction, that the user actually entered some
   * data!  Otherwise, it would be lame to add this. */
  if( regData->changed & MOD_NEW )
    {
    if( (strcmp("",trans->num) == 0)         &&
        (strcmp("",trans->description) == 0) &&
        (strcmp("",trans->memo) == 0)        &&
        (0 == trans->catagory)               &&
        (1.0 == trans->share_price)          &&
        (0.0 == trans->damount) )
      {
      _free(trans);
      return;
      }
    else
      {
      /* If this is a valid new transaction, add a new
       * empty transaction to take its place */
      outOfOrder = TRUE;
      }
    }
  
  if( regData->changed & MOD_DATE )
    {
    Transaction *prevTrans;
    Transaction *nextTrans;
    prevTrans = getTransaction( regData->acc, position-1 );
    nextTrans = getTransaction( regData->acc, position+1 );

    DEBUG("MOD_DATE");
    /* read in the date stuff... */
    sscanf( XbaeMatrixGetCell(regData->reg,row,DATE_CELL_C),"%d/%d",
            &(trans->date.month),
            &(trans->date.day) );
    
    trans->date.year = atoi(XbaeMatrixGetCell(regData->reg,row+1,0));
    
    /* figure out if the transactions are out of order */
    if( prevTrans != NULL )
      if( datecmp(&(prevTrans->date),&(trans->date))>0 )
        outOfOrder = True;
    if( nextTrans != NULL )
      if( datecmp(&(trans->date),&(nextTrans->date))>0 )
        outOfOrder = True;
    
    /* take care of re-ordering, if necessary */
    if( outOfOrder )
      {
      /* We need to change lastTrans, because we remove and re-insert
       * the transaction... reset lastTrans to zero to prevent it from
       * indicating a row that doesn't exist.  (That shouldn't happen
       * anyways!) */
      regData->lastTrans = 0;
      
      /* We don't need to remove if it isn't in the transaction list */
      if( !(regData->changed & MOD_NEW) )
        removeTransaction( regData->acc, position );
      
      insertTransaction( regData->acc, trans );
      
      regRefresh(regData);
      }
    
    /* Scroll to the last row, if it is a new transaction */
    if( !(regData->changed & MOD_NEW) )
      {
      int lastRow = (2 * newPosition) + 1 + DESC_CELL_C;
      XbaeMatrixMakeCellVisible( regData->reg, lastRow, DESC_CELL_C );
      }
    
    /* recalculate the balances, but only after re-ordering
     * cells */
    regRecalculateBalance(regData);
    }
  
  /* reset the "changed" bitfield */
  regData->changed   = 0;

  /* If the reconcile window is open, update it!!! */
  if( regData->acc->recnData != NULL )
    recnRefresh( regData->acc->recnData );
  
  return;
  }

/********************************************************************\
 * regWindow                                                        *
 *   opens up a register window for Account account                 *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         acc     - the account associated with this register      *
 * Return: regData - the register window instance                   *
\********************************************************************/
RegWindow *
regWindow( Widget parent, Account *acc )
  {
  Transaction *trans;
  RegWindow   *regData;
  Widget menubar, pane, buttonform, frame, reg, widget;
  int    position=0;

  setBusyCursor( parent );
  
  /* If they haven't already been initialized, (ie. this is the first
   * time a RegWindow has been opened,) initialize the XrmQuarks */
  if( !haveQuarks )
    {
    QPointer = XrmPermStringToQuark("Pointer");
    QLeft    = XrmPermStringToQuark("Left");
    QRight   = XrmPermStringToQuark("Right");
    QUp      = XrmPermStringToQuark("Up");
    QDown    = XrmPermStringToQuark("Down");
    haveQuarks = True;
    }
  
  regData = (RegWindow *)_malloc(sizeof(RegWindow));
  regData->acc       = acc;
  regData->changed   = 0;          /* Nothing has changed yet! */
  regData->lastTrans = 0;
  regData->qf = acc->qfRoot;
  regData->insert    = 0;          /* the insert (cursor) position in
                                    * quickfill cells */
  
  regData->dialog =
    XtVaCreatePopupShell( "dialog", 
                          xmDialogShellWidgetClass, parent,
                          XmNdeleteResponse,   XmDESTROY,
                          XmNtitle,            acc->accountName,
                          XmNwidth,            495,
                          XmNheight,           500,
/*
                          XmNminWidth,         495,
                          XmNmaxWidth,         495,
                          XmNminHeight,        500,
*/
                          /* XmNresizable,        FALSE, */
                          /* XmNallowShellResize, FALSE, */
                          XmNtransient,        FALSE,  /* allow window to be repositioned */
                          NULL );
  
  XtAddCallback( regData->dialog, XmNdestroyCallback, 
                 closeRegWindow, (XtPointer)regData );
  
  /* Create a PanedWindow Manager for the dialog box... the paned 
   * window is the parent of the two forms which comprise the two
   * areas of the dialog box */
  pane = XtVaCreateWidget( "pane", 
                           xmPanedWindowWidgetClass, regData->dialog,
                           XmNsashWidth,     1,
                           XmNsashHeight,    1,
                           XmNseparatorOn,   False,
                           XmNtraversalOn,   False,
                           XmNmarginHeight,  1,
                           XmNmarginWidth,   1,
                           XmNallowResize,   True,
                           XmNpaneMaximum,   200,
                           XmNpaneMinimum,   200,
                           NULL );
  
  /* Setup the menubar at the top of the window */
  /******************************************************************\
   * Set up the menubar menu-items                                  *
  \******************************************************************/
  {
  MenuItem reportMenu[] = {
    { "Simple...",          &xmPushButtonWidgetClass, 'S', NULL, NULL,
      NULL, (XtPointer)0,  (MenuItem *)NULL },
    NULL,
  };
  
  MenuItem activityMenu[] = {
    { "Transfer...",        &xmPushButtonWidgetClass, 'T', NULL, NULL, 
      accountMenubarCB, (XtPointer)AMB_TRNS,  (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Reconcile...",       &xmPushButtonWidgetClass, 'C', NULL, NULL, 
      startRecnCB, (XtPointer)regData, (MenuItem *)NULL },
    { "Adjust Balance...",  &xmPushButtonWidgetClass, 'A', NULL, NULL, 
      startAdjBCB, (XtPointer)regData, (MenuItem *)NULL },
    { "Report",             &xmPushButtonWidgetClass, 'D', NULL, NULL,
      NULL, (XtPointer)0,  (MenuItem *)&reportMenu },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Delete Transaction", &xmPushButtonWidgetClass, 'D', NULL, NULL,
      deleteCB,     (XtPointer)regData,      (MenuItem *)NULL },
    NULL,
  };
  
  MenuItem helpMenu[] = {
    { "About...",           &xmPushButtonWidgetClass, 'A', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL },
    { "Help...",            &xmPushButtonWidgetClass, 'H', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_REGWIN,(MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "License...",         &xmPushButtonWidgetClass, 'L', NULL, NULL, 
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL },
    NULL,
  };
  
  menubar = XmCreateMenuBar( pane, "menubar", NULL, 0 );  
  
  BuildMenu( menubar, XmMENU_PULLDOWN, "Activities", 'A',
             False, 0, activityMenu );
  BuildMenu( menubar, XmMENU_PULLDOWN, "Help",       'H', 
             False, 0, helpMenu );
  
  XtManageChild( menubar );
  }
  
  frame = XtVaCreateWidget( "reg", 
                            xmFrameWidgetClass, pane,
                            NULL );
  
  /******************************************************************\
   * The register area                                              *
  \******************************************************************/
  
    {
    char   buf[BUFSIZE];
    String **data;
    int i,j;
    /* ----------------------------------- */
    /* set up number of displayed columns */
    switch(acc->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
        acc -> numCols = 7;
        break;
      case PORTFOLIO:
      case MUTUAL:
        acc -> numCols = 9;
        break;
      default:
        fprintf( stderr, "Ineternal Error: Account type: %d is unknown!\n", acc->type);
      }

    /* ----------------------------------- */
    /* set up column widths */

    acc -> colWidths[DATE_CELL_C] = 4;   /* the widths of columns */
    acc -> colWidths[NUM_CELL_C]  = 4;   /* the widths of columns */
    acc -> colWidths[DESC_CELL_C] = 35;  /* the widths of columns */
    acc -> colWidths[RECN_CELL_C] = 1;   /* the widths of columns */
    acc -> colWidths[PAY_CELL_C]  = 8;   /* the widths of columns */
    acc -> colWidths[DEP_CELL_C]  = 8;   /* the widths of columns */
    acc -> colWidths[BALN_CELL_C] = 8;   /* the widths of columns */

    switch(acc->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
        break;
      case PORTFOLIO:
      case MUTUAL:
        acc -> colWidths[PRIC_CELL_C] = 8;   /* price */
        acc -> colWidths[SHRS_CELL_C] = 8;   /* share balance */
        acc -> colWidths[BALN_CELL_C] = 8;   /* $ balance */
        break;
      }
    
    /* ----------------------------------- */
    /* set up column alignments */

    acc -> alignments[DATE_CELL_C] = XmALIGNMENT_END;
    acc -> alignments[NUM_CELL_C]  = XmALIGNMENT_END;
    acc -> alignments[DESC_CELL_C] = XmALIGNMENT_BEGINNING;
    acc -> alignments[RECN_CELL_C] = XmALIGNMENT_CENTER;
    acc -> alignments[PAY_CELL_C]  = XmALIGNMENT_END;
    acc -> alignments[DEP_CELL_C]  = XmALIGNMENT_END;
    acc -> alignments[BALN_CELL_C] = XmALIGNMENT_END;

    switch(acc->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
        break;

      case PORTFOLIO:
      case MUTUAL:
        acc -> alignments[PRIC_CELL_C] = XmALIGNMENT_END;  /* price */
        acc -> alignments[SHRS_CELL_C] = XmALIGNMENT_END;  /* share balance */
        acc -> alignments[BALN_CELL_C] = XmALIGNMENT_END;  /* $balance */
        break;
      }
    
    /* ----------------------------------- */
    /* Put the appropriate heading names in the column titles */
    for (i=0; i<3; i++) {
       for (j=0; j<XACC_NUM_COLS; j++) {
          acc->rows[i][j] = "";
       }
    }

    acc -> rows[0][DATE_CELL_C] = "Date";
    acc -> rows[0][NUM_CELL_C]  = "Num";
    acc -> rows[0][DESC_CELL_C] = "Description";
    switch(acc->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
        acc -> rows[0][BALN_CELL_C] = "Balance";
        break;
      case PORTFOLIO:
      case MUTUAL:
        acc -> rows[0][PRIC_CELL_C] = "Price";
        acc -> rows[0][SHRS_CELL_C] = "Tot Shrs";
        acc -> rows[0][BALN_CELL_C] = "Balance";
        break;
      }
    
    switch(acc->type)
      {
      case BANK:
        acc -> rows[0][PAY_CELL_C] = "Payment";
        acc -> rows[0][DEP_CELL_C] = "Deposit";
        break;
      case CASH:
        acc -> rows[0][PAY_CELL_C] = "Spend";
        acc -> rows[0][DEP_CELL_C] = "Receive";
        break;
      case ASSET:
        acc -> rows[0][PAY_CELL_C] = "Decrease";
        acc -> rows[0][DEP_CELL_C] = "Increase";
        break;
      case CREDIT:
        acc -> rows[0][PAY_CELL_C] = "Charge";
        acc -> rows[0][DEP_CELL_C] = "Payment";
        break;
      case LIABILITY:
        acc -> rows[0][PAY_CELL_C] = "Increase";
        acc -> rows[0][DEP_CELL_C] = "Decrease";
        break;
      case PORTFOLIO:
      case MUTUAL:
        acc -> rows[0][PAY_CELL_C] = "Sold";
        acc -> rows[0][DEP_CELL_C] = "Bought";
        break;
      }
    
    data = (String **)XtMalloc(2*sizeof(String *));
    data[0] = &(acc -> rows[0][0]);
    data[1] = &(acc -> rows[1][0]);
    data[2] = &(acc -> rows[2][0]);
    sprintf( buf, "reg" );
    reg = XtVaCreateWidget( strcat(buf,accRes[acc->type]),
                            xbaeMatrixWidgetClass,  frame,
                            XmNcells,               data,
                            XmNfixedRows,           1,
                            XmNfixedColumns,        0,
                            XmNrows,                2,
                            XmNvisibleRows,         20,
                            XmNfill,                True,
                            XmNcolumns,             acc -> numCols,
                            XmNcolumnWidths,        acc -> colWidths,
                            XmNcolumnAlignments,    acc -> alignments,
                            XmNtraverseFixedCells,  False,
                            XmNgridType,            XmGRID_SHADOW_IN,
                            XmNshadowType,          XmSHADOW_ETCHED_IN,
                            XmNverticalScrollBarDisplayPolicy,XmDISPLAY_STATIC,
                            XmNselectScrollVisible, True,
                            NULL);
    
    regData->reg     = reg;
    
    XtFree((char *)data);
    }
  
  XtAddCallback( reg, XmNenterCellCallback, regCB, (XtPointer)regData );
  XtAddCallback( reg, XmNmodifyVerifyCallback, regCB, (XtPointer)regData );
  XtAddCallback( reg, XmNtraverseCellCallback, regCB, (XtPointer)regData );
  
  XtManageChild(reg);
  XtManageChild(frame);
  
  /******************************************************************\
   * The button area... also contains balance fields                *
  \******************************************************************/
  
  buttonform = XtVaCreateWidget( "form", 
				 xmFormWidgetClass, pane,
				 XmNfractionBase,   6,
				 XmNresizable,      False,
				 NULL );
  
  position = 0;                    /* puts the buttons in the right place */
  
  /* The "Record" button */
  widget = XtVaCreateManagedWidget( "Record", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
		 recordCB, (XtPointer)regData );
  
  /* The "Cancel" button */
  position++;
  widget = XtVaCreateManagedWidget( "Cancel", 
				    xmPushButtonWidgetClass, buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNbottomAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    XmNshowAsDefault,      True,
				    NULL );
  
  XtAddCallback( widget, XmNactivateCallback, 
                 cancelCB, (XtPointer)regData );
  
  position+=2;
  
  /* Fix button area of the pane to its current size, and not let 
   * it resize. */
    {
    Dimension h;
    XtVaGetValues( widget, XmNheight, &h, NULL );
    XtVaSetValues( buttonform, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL );
    }
    
  /* The balance field labels: */ 
  widget = XtVaCreateManagedWidget( "Balance:",
				    xmLabelGadgetClass,    buttonform,
				    XmNtopAttachment,      XmATTACH_FORM,
				    XmNleftAttachment,     XmATTACH_POSITION,
				    XmNleftPosition,       position,
				    XmNrightAttachment,    XmATTACH_POSITION,
				    XmNrightPosition,      position+1,
				    NULL );
  widget = XtVaCreateManagedWidget( "Cleared:",
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
 * closeRegWindow                                                   *
 *   frees memory allocated for an regWindow, and other cleanup     *
 *   stuff                                                          *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void 
closeRegWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  Account   *acc = regData->acc;
  
  /* Save any unsaved changes */
  XbaeMatrixCommitEdit( regData->reg, False );
  regSaveTransaction( regData, regData->lastTrans );
  
  _free(regData);
  acc->regData = NULL;
  
  DEBUG("closed RegWindow");
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
void startAdjBCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  
  if( regData->acc->adjBData == NULL )
    regData->acc->adjBData = adjBWindow( toplevel, regData->acc );
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
void startRecnCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  
  if( regData->acc->recnData == NULL )
    regData->acc->recnData = recnWindow( toplevel, regData->acc );
  }

/********************************************************************\
 * recordCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void
recordCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  
  XbaeMatrixCommitEdit( regData->reg, False );
  regSaveTransaction( regData, regData->lastTrans );
  }

/********************************************************************\
 * deleteCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void
deleteCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  Account   *acc     = regData->acc;
  
  if( getTransaction(acc,regData->lastTrans) != NULL )
    {
    char *msg = "Are you sure you want\nto delete this transaction?";
    
    if( verifyBox( toplevel, msg ) )
      {
      Transaction *trans;
      int row = (2*regData->lastTrans) + 1;
      /* remove the transaction */
      trans = removeTransaction( acc, regData->lastTrans );
      
      /* remove the rows from the matrix */
      XbaeMatrixDeleteRows( regData->reg, row, 2 );
      XbaeMatrixRefresh(regData->reg);
      
      /* Delete the transaction */
      XtFree(trans->num);
      XtFree(trans->description);
      XtFree(trans->memo);
      _free(trans);
      
      regRecalculateBalance(regData);
      }
    }
  }

/********************************************************************\
 * cancelCB                                                         *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void
cancelCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  XbaeMatrixCancelEdit( regData->reg, False );
  }

/********************************************************************\
 * regCB                                                            *
 *                                                                  *
 * NOTES: All the saving of the data happens when the user leaves   *
 *        a transaction... this way it will be easier later on to   *
 *        add an undo/cancel modifications feature                  *
 *                                                                  *
 * Args:   mw - the widget that called us                           *
 *         cd - regData - the data struct for this register          *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
void
regCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int row,col;
  String **data = NULL;
  Widget reg;
  
  XbaeMatrixDefaultActionCallbackStruct *cbs = 
    (XbaeMatrixDefaultActionCallbackStruct *)cb;
  
  RegWindow *regData = (RegWindow *)cd;
  Account   *acc = regData->acc;
  
  reg = regData->reg;
  
  row  = cbs->row;
  col  = cbs->column;
  
  switch( cbs->reason )
    {
    case XbaeEnterCellReason:
      DEBUG("XbaeEnterCellReason");
      DEBUGCMD(printf(" row = %d\n col = %d\n",row,col));
      /* figure out if we are editing a different transaction... if we 
       * are, then we need to save the transaction we left */
      if( regData->lastTrans != (row-1)/2 )
        {
        DEBUG("Save Transaction");
        DEBUGCMD(printf(" lastTrans = %d\n currTrans = %d\n", 
                        regData->lastTrans, (row+1)/2 ));
        
        regSaveTransaction( regData, regData->lastTrans );
        
        regData->lastTrans = (row-1)/2;
        regData->insert    = 0;
        regData->qf = acc->qfRoot;
        }
      
      /* If this is a cell which isn't editible, then don't
       * let the user enter! */
      if( !IN_DATE_CELL(row,col) && !IN_NUM_CELL(row,col) &&
          !IN_DESC_CELL(row,col) && !IN_PAY_CELL(row,col) &&
          !IN_RECN_CELL(row,col) && !IN_DEP_CELL(row,col) &&
          !((PORTFOLIO == acc->type) && IN_PRIC_CELL(row,col)) &&
          !((MUTUAL    == acc->type) && IN_PRIC_CELL(row,col)) &&
          !IN_MEMO_CELL(row,col) )
        {
        ((XbaeMatrixEnterCellCallbackStruct *)cbs)->doit = FALSE;
        ((XbaeMatrixEnterCellCallbackStruct *)cbs)->map  = FALSE;
        }
      /* If the user enters the reconciled cell, toggle it's value */
      else if( IN_RECN_CELL(row,col) )
        {
        ((XbaeMatrixEnterCellCallbackStruct *)cbs)->doit = FALSE;
        ((XbaeMatrixEnterCellCallbackStruct *)cbs)->map  = FALSE;
        
        XtVaGetValues( mw, XmNcells, &data, NULL );
        DEBUGCMD(printf("data[%d][RECN_CELL_C] = %s\n", row, data[row][RECN_CELL_C]));
        
        if( data[row][RECN_CELL_C][0] == NREC )
          data[row][RECN_CELL_C][0] = CREC;
        else
          data[row][RECN_CELL_C][0] = NREC;
        
        /* this cell has been modified, so we need to save when we
         * leave!!! */
        regData->changed |= MOD_RECN;
        
        XtVaSetValues( mw, XmNcells, data, NULL );
        XbaeMatrixRefreshCell( mw, row, RECN_CELL_C);
        }
      break;
    case XbaeModifyVerifyReason:
      DEBUG("XbaeModifyVerifyReason");
      {	
      XbaeMatrixModifyVerifyCallbackStruct *mvcbs = 
         (XbaeMatrixModifyVerifyCallbackStruct *)cb;
      char input;
      
      /* If the user is about to change a reconciled transaction,
       * warn them... but only warn them the first time they change
       * something in the transaction.  We can tell if it is the
       * first time by looking at regData->changed */
      if( regData->changed == MOD_NONE )
        {
        Transaction *trans = getTransaction( acc, regData->lastTrans );
        /* If trans==NULL, then this must be a new transaction */
        if( (trans != NULL) && (trans->reconciled == YREC) )
          {
          char msg[BUFSIZE];
          sprintf( msg, RECN_TRANS_WARN );
          if( !verifyBox( toplevel, msg ) )
            {
            mvcbs->verify->doit = False;
            return;
            }
          }
        }
      
      /* If the user is in a cell that's data is in a certain format
       * (ex: date or deposit), then make sure the input is valid.
       * In the description field, take care of the QuickFill stuff 
       * Also, in certain cells (like the date), we need to take care
       * of special accelerator keys... */
      
#ifdef USEQUICKFILL
      /* This part only works with the patched Xbae-Matrix widget */
      if( IN_DESC_CELL(row,col) )
        {
        regData->insert++;
        if( regData->insert != mvcbs->verify->currInsert )
          {
          int i;
          regData->insert = mvcbs->verify->currInsert;
          DEBUG("resyncing quickfill!");
          DEBUGCMD(printf(" insert = %d\n currInsert = %d\n",
                          regData->insert,
                          mvcbs->verify->currInsert ));
          /* we are out of sync, which means that the user
           * must have hit delete or something... to be on
           * the safe side, rescan the description field,
           * to ensure that quickfill works correctly for
           * the data that is actually in the cell */
          regData->qf = acc->qfRoot;
          for( i=0; i<regData->insert; i++ )
            regData->qf = getQuickFill( regData->qf, mvcbs->prev_text[i] );
          }
        
        /* hack alert -- text pointer not valid if non-alpha key hit */
        /* this will core dump, since ptr is NULL */
        /* this is not fixed, since the fix is not obvious to me ... */
        input = (mvcbs->verify->text->ptr)[0];

        /* go to qf's child node that corresponds to the
         * last character inputed by the user */
        regData->qf = getQuickFill( regData->qf, input );
        
        if( (regData->qf != NULL) && (regData->qf->trans != NULL) )
          {
          //char *str = regData->qf->trans->description;
          char str[BUFSIZE];
          strcpy( str, regData->qf->trans->description );
          DEBUG(str);
          
          mvcbs->verify->doit = False;
          
          XbaeMatrixSetCell( mw, row, col, str );
          XbaeMatrixRefreshCell( mw, row, col );
/*
          XbaeMatrixSetCursorPosition( mw, regData->insert+1 );
*/
          }
        else
          {
          char str[BUFSIZE];
          strncpy( str, mvcbs->prev_text, regData->insert );
          /* Need to make sure the string is terminated: */
          str[regData->insert] = '\0';
          
          XbaeMatrixSetCell( mw, row, col, str );
          XbaeMatrixRefreshCell( mw, row, col );
/*
          XbaeMatrixSetCursorPosition( mw, regData->insert );
*/
          }
        }
#endif	
      if( IN_DATE_CELL(row,col) )
        dateCellFormat( mw, mvcbs );       /* format according to date
                                            * cell rules */

      /* look to see if numeric format is OK.  Note that
       * the share price cell exists only for certain account types */
      if( IN_PAY_CELL(row,col) || IN_DEP_CELL(row,col) ||
          ((PORTFOLIO == acc->type) && IN_PRIC_CELL(row,col)) ||
          ((MUTUAL    == acc->type) && IN_PRIC_CELL(row,col)) )
        {
        /* text pointer is NULL if non-alpha key hit */
        /* for example, the delete key */
        if (mvcbs->verify->text->ptr) {
          input = (mvcbs->verify->text->ptr)[0];
          /* Payment/Deposit format */
          switch( input )
            {
            case '.':
              /* Make sure that there is only one '.' */
              {
              int i,count=0;
              
              for( i=0; (mvcbs->prev_text)[i] != '\0'; i++ )
                if( (mvcbs->prev_text)[i] == '.' )
                  count++;
              if( count >= 1 )
                mvcbs->verify->doit = False;
              }
              break;
            default:
              /* only accept the input if it is a number */
              mvcbs->verify->doit = isNum(input);
            }
          } else {
            /* we assume that any other changes are a valid edit */
            mvcbs->verify->doit = True;
          }
        }
      }
      /* if the cell is modified, mark it as modified, so we know  to 
       * save it... (Don't mark reconciled here, because you can't
       * actually enter the reconciled cell... mark it in XbaerCellReason */
      if( IN_DATE_CELL(row,col) )
        regData->changed |= MOD_DATE;
      
      if( IN_NUM_CELL(row,col) )
        regData->changed |= MOD_NUM;
      
      if( IN_DESC_CELL(row,col) )
        regData->changed |= MOD_DESC;
      
      if( IN_PAY_CELL(row,col) || IN_DEP_CELL(row,col) )
        regData->changed |= MOD_AMNT;
      
      if( ((PORTFOLIO == acc->type) && IN_PRIC_CELL(row,col)) ||
          ((MUTUAL    == acc->type) && IN_PRIC_CELL(row,col)) )
        regData->changed |= MOD_PRIC;
      
      if( IN_MEMO_CELL(row,col) )
        regData->changed |= MOD_MEMO;
      
      break;
    case XbaeTraverseCellReason:
      DEBUG("XbaeTraverseCellReason");
      /* This ensure that whenever the user hits TAB, they go to the
       * next valid cell.  Also, if regData->qf and regData->qf->trans
       * aren't NULL, then fill the field we are entering with the
       * data from regData->qf->trans */
        {
        char buf[BUFSIZE];
        XbaeMatrixTraverseCellCallbackStruct *tcbs =
        (XbaeMatrixTraverseCellCallbackStruct *)cb;
        
        if( tcbs->qparam == QRight )
          {
          /* Don't need to check IN_DATE_CELL or IN_NUM_CELL because
           * the default transversal from these cells is correct */
          
          if( IN_DESC_CELL(row,col) )
            {
            tcbs->next_column = PAY_CELL_C;
            if( regData->qf != NULL )
              if( regData->qf->trans != NULL )
                if( 0.0 > regData->qf->trans->damount )
                  {
                  sprintf( buf, "%.2f ", (-1.0*(regData->qf->trans->damount)) );
                  XbaeMatrixSetCell( reg, tcbs->next_row, 
                                     tcbs->next_column, buf );
                  }
            }
          
          if( IN_PAY_CELL(row,col) )
            {
            /* In this field, we either go to the deposit field,
             * if the user hasn't entered any data in this field,
             * or to the memo field, if the user has entered data */
            XbaeMatrixCommitEdit(reg,True);
            if( strcmp(XbaeMatrixGetCell(reg,tcbs->row,tcbs->column),"") != 0 )
              {
              tcbs->next_row    = row+1;
              tcbs->next_column = MEMO_CELL_C;
              if( regData->qf != NULL )
                if( regData->qf->trans != NULL )
                  XbaeMatrixSetCell( reg, tcbs->next_row, tcbs->next_column,
                                     regData->qf->trans->memo );
              }
            else
              {
              tcbs->next_column = DEP_CELL_C;
              if( regData->qf != NULL )
                if( regData->qf->trans != NULL )
                  if( 0.0 <= regData->qf->trans->damount )
                    {
                    sprintf( buf, "%.2f ", (regData->qf->trans->damount) );
                    XbaeMatrixSetCell( reg, tcbs->next_row, 
                                       tcbs->next_column, buf );
                    }
              }
            }
          
          if( IN_DEP_CELL(row,col) )
            {
            tcbs->next_row    = row+1;
            tcbs->next_column = MEMO_CELL_C;
            if( regData->qf != NULL )
              if( regData->qf->trans != NULL )
                XbaeMatrixSetCell( reg, tcbs->next_row, tcbs->next_column,
                                   regData->qf->trans->memo );
            }
          
          /* If we are in the memo cell, stay there! */
          if( (IN_MEMO_CELL(row,col)) && 
              (IN_BAD_CELL(tcbs->next_row,tcbs->next_column)) )
            {
            tcbs->next_row    = row;
            tcbs->next_column = MEMO_CELL_C;
            }
          }
        }
        break;
    default:
      DEBUG("We shouldn't get here!");
    }
  }


/********************************************************************\
 * dateCellFormat                                                   *
 *   Apply the Date Cell format rules to the input... called by     *
 *   regCB in the event of a modify-verify event.  Also handles     *
 *   the possible accelerator keys the user can enter in the date   *
 *   cell                                                           *
 *                                                                  *
 * Args:   mw    - the widget that called us                        *
 *         mvcbs - the modify-verify callback struct (from regCB)   *
 * Return: none                                                     *
\********************************************************************/
void
dateCellFormat( Widget mw, XbaeMatrixModifyVerifyCallbackStruct *mvcbs )
  {
  /* Date format -- valid characters are numerals, '/', and
   * accelerator keys (accelerator keys have doit = False,
   * so they don't go into the date field, but they are 
   * not ignored) */
  
  int row,col;
  Boolean changed=False;
  Date date;
  char buf[BUFSIZE];
  char input;

  /* if user hit delete key, then ptr will be NULL */
  /* lets just accept the edit. */
  if (0x0 == (mvcbs->verify->text->ptr)) {
    mvcbs->verify->doit = True;
    return;
  }

  input = (mvcbs->verify->text->ptr)[0];
  
  row  = mvcbs->row;
  col  = mvcbs->column;
  
  date.day   = 0;
  date.month = 0;
  date.year  = 0;
  
  sscanf( mvcbs->prev_text,
          "%d/%d",&(date.month), &(date.day) );
  sscanf( XbaeMatrixGetCell(mw,row+1,col),
          "%d", &(date.year) );
  
  /* If there isn't a valid date in this field, use today's date */
  if( (date.day == 0) || (date.month == 0) || (date.year == 0) )
    todaysDate( &date );
  
  /* handle the accelerator keys */
  switch( input )
    {
    case '+':
    case '=':              /* '+' without the shift! */
      /* next day */
      DEBUG("next day");
      adjustDay( &date, 1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '-':
      /* prev day */
      DEBUG("prev day");
      adjustDay( &date, -1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 't':
      /* today */
      DEBUG("today");
      todaysDate( &date );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'm':
      /* beginning of month */
      DEBUG("beginning of month");
      date.day = 1;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'h':
      /* end of month */
      DEBUG("end of month");
      date.day = daysInMonth( date.month );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'y':
      /* beginning of year */
      DEBUG("beginning of year");
      date.day   = 1;
      date.month = 1;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'r':
      /* end of year */
      DEBUG("end of year");
      date.day   = 31;
      date.month = 12;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case ']':
      /* next month */
      DEBUG("next month");
      adjustMonth( &date, +1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '[':
      /* prev month */
      DEBUG("prev month");
      adjustMonth( &date, -1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '/':
      /* we need to keep track of the number of '/'s
       * on the first on, we accept, on the second one
       * we skip to the year cell
       */
    {
    int i,count=0;
    DEBUGCMD(printf(" = %s\n",mvcbs->prev_text));
    for( i=0; (mvcbs->prev_text)[i] != '\0'; i++ )
      if( (mvcbs->prev_text)[i] == '/' )
        count++;
    if( count >= 1 )
      {
      XbaeMatrixEditCell( mw, row+1, DATE_CELL_C );
      XbaeMatrixSelectCell( mw, row+1, DATE_CELL_C );
      }
    }
    break;
    default:
      /* only accept the input if it is a number */
      mvcbs->verify->doit = isNum(input);
    }
  
  if( changed )
    {
    sprintf( buf,"%2d/%2d\0\0", date.month, date.day );
    XbaeMatrixSetCell( mw, row, col, buf );
    XbaeMatrixRefreshCell( mw, row, col );
    
    sprintf( buf,"%4d\0", date.year );
    XbaeMatrixSetCell( mw, row+1, col, buf );
    XbaeMatrixRefreshCell( mw, row+1, col );
    }
  }
 

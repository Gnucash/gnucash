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
#include <Xbae/Matrix.h>

#include "Account.h"
#include "Action.h"
#include "AdjBWindow.h"
#include "BuildMenu.h"
#include "Data.h"
#include "date.h"
#include "main.h"
#include "MainWindow.h"
#include "LedgerUtils.h"
#include "PopBox.h"
#include "QuickFill.h"
#include "RecnWindow.h"
#include "Transaction.h"
#include "util.h"


#define NUM_COLUMNS           12
#define NUM_HEADER_ROWS   1
#define NUM_ROWS_PER_TRANS 2

/* enumerate different ledger types */
enum {
   GEN_LEDGER = NUM_ACCOUNT_TYPES,
   PORTFOLIO = NUM_ACCOUNT_TYPES +1,
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
  unsigned short changed;     /* bitmask of fields that have changed in  *
                               * transaction currEntry                   */
  unsigned short currEntry;   /* to keep track of last edited transaction*/

  char type;                  /* register display type, usually equal to *
                               * account type                            */

  /* quick-fill stuff */
  XmTextPosition insert;      /* used by quickfill for detecting deletes */
  QuickFill      *qf;         /* keeps track of current quickfill node.  *
                               * Reset to Account->qfRoot when entering  *
                               * a new transaction                       */

  /* pull-down (combo) box stuff */
  PopBox         *actbox;     /* ComboBox for actions                    */
  PopBox         *xfrmbox;    /* ComboBox for transfers                  */
  PopBox         *xtobox;     /* ComboBox for transfers                  */

  /* structures for controlling the column layout */
  short          numCols;     /* number of columns in the register       */
  short columnLocation [NUM_COLUMNS];  /* column ordering              */
  short columnWidths   [NUM_COLUMNS];  /* widths (in chars not pixels) */
  String columnLabels  [3][NUM_COLUMNS]; /* column labels              */
  unsigned char alignments[NUM_COLUMNS]; /* alignment of display chars */

} RegWindow;


/** PROTOTYPES ******************************************************/
RegWindow * regWindowLedger( Widget parent, Account **acclist );

static double regRecalculateBalance( RegWindow *regData );
static void regSaveTransaction( RegWindow *regData, int position );

static void closeRegWindow( Widget mw, XtPointer cd, XtPointer cb );
static void startRecnCB( Widget mw, XtPointer cd, XtPointer cb );
static void startAdjBCB( Widget mw, XtPointer cd, XtPointer cb );
static void recordCB( Widget mw, XtPointer cd, XtPointer cb );
static void deleteCB( Widget mw, XtPointer cd, XtPointer cb );
static void cancelCB( Widget mw, XtPointer cd, XtPointer cb );
static void regCB( Widget mw, XtPointer cd, XtPointer cb );
static void dateCellFormat( Widget mw, XbaeMatrixModifyVerifyCallbackStruct *mvcbs, int do_year );


/** GLOBALS *********************************************************/
extern Widget  toplevel;

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
#define MOD_ACTN  0x100
#define MOD_XFRM  0x200
#define MOD_XTO   0x400
#define MOD_NEW   0x800
#define MOD_ALL   0xfff

/* These defines are indexes into the column location array */
#define DATE_COL_ID  0
#define NUM_COL_ID   1
#define DESC_COL_ID  2
#define RECN_COL_ID  3
#define PAY_COL_ID   4
#define DEP_COL_ID   5
#define PRIC_COL_ID  6
#define SHRS_COL_ID  7
#define BALN_COL_ID  8 
#define XFER_COL_ID  9 

/* the actual column location is pulled out of the column location array */
#define DATE_CELL_R  0
#define DATE_CELL_C  (regData->columnLocation[DATE_COL_ID])
#define YEAR_CELL_R  1
#define YEAR_CELL_C  DATE_CELL_C  /* same column as the date */

#define NUM_CELL_R   0
#define NUM_CELL_C   (regData->columnLocation[NUM_COL_ID])
#define ACTN_CELL_R  1
#define ACTN_CELL_C  NUM_CELL_C   /* same column as the transaction number */

#define DESC_CELL_R  0
#define DESC_CELL_C  (regData->columnLocation[DESC_COL_ID])
#define MEMO_CELL_R  1
#define MEMO_CELL_C  DESC_CELL_C  /* same column as the description */

#define XFRM_CELL_R  0
#define XFRM_CELL_C  (regData->columnLocation[XFER_COL_ID])
#define XTO_CELL_R   1
#define XTO_CELL_C   XFRM_CELL_C

#define RECN_CELL_R  0
#define RECN_CELL_C  (regData->columnLocation[RECN_COL_ID])
#define PAY_CELL_R   0
#define PAY_CELL_C   (regData->columnLocation[PAY_COL_ID])
#define DEP_CELL_R   0
#define DEP_CELL_C   (regData->columnLocation[DEP_COL_ID])
#define BALN_CELL_R  0
#define BALN_CELL_C  (regData->columnLocation[BALN_COL_ID]) 

#define PRIC_CELL_R  0
#define PRIC_CELL_C  (regData->columnLocation[PRIC_COL_ID])
#define SHRS_CELL_R  0
#define SHRS_CELL_C  (regData->columnLocation[SHRS_COL_ID])


/** CELL MACROS *****************************************************/
#define RMOD(R,RM)   ((R-NUM_HEADER_ROWS)%NUM_ROWS_PER_TRANS==RM) 

        /* Date cell        */
#define IN_DATE_CELL(R,C) (RMOD(R,DATE_CELL_R) && (C==DATE_CELL_C))  

        /* Number cell      */
#define IN_NUM_CELL(R,C) (RMOD(R,NUM_CELL_R) && (C==NUM_CELL_C))  

        /* Description cell */
#define IN_DESC_CELL(R,C) (RMOD(R,DESC_CELL_R) && (C==DESC_CELL_C))  

        /* Reconciled cell  */
#define IN_RECN_CELL(R,C) (RMOD(R,RECN_CELL_R) && (C==RECN_CELL_C))  

        /* Payment cell     */
#define IN_PAY_CELL(R,C) (RMOD(R,PAY_CELL_R) && (C==PAY_CELL_C))  

        /* Deposit cell     */
#define IN_DEP_CELL(R,C) (RMOD(R,DEP_CELL_R) && (C==DEP_CELL_C))  

        /* Balance cell     */
#define IN_BALN_CELL(R,C) (RMOD(R,BALN_CELL_R) && (C==BALN_CELL_C))  

        /* Price cell       */
#define IN_PRIC_CELL(R,C) (RMOD(R,PRIC_CELL_R) && (C==PRIC_CELL_C))  

        /* Action cell      */
#define IN_ACTN_CELL(R,C) (RMOD(R,ACTN_CELL_R) && (C==ACTN_CELL_C))  

        /* Year cell        */
#define IN_YEAR_CELL(R,C) (RMOD(R,YEAR_CELL_R) && (C==YEAR_CELL_C))  

        /* Transfer cell    */
#define IN_XFRM_CELL(R,C) (RMOD(R,XFRM_CELL_R) && (C==XFRM_CELL_C))  
#define IN_XTO_CELL(R,C) (RMOD(R,XTO_CELL_R) && (C==XTO_CELL_C))  

        /* Memo cell        */
#define IN_MEMO_CELL(R,C) (RMOD(R,MEMO_CELL_R) && (C==MEMO_CELL_C))  

        /* Not the memo cell*/
#define IN_BAD_CELL(R,C) (RMOD(R,1) && (C!=MEMO_CELL_C))  


/********************************************************************/

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
    Transaction **tarray;
    int    old_num_rows, new_num_rows, delta_rows;
    int    i,j, ntrans, ncols;
    char   buf[BUFSIZE];
    String **data = NULL;
    String **newData;
    Account *main_acc=NULL, *xfer_acc=NULL;
    double themount;  /* amount */
    
    /* first, build a sorted array of transactions */
    if (1 == regData->numAcc) {
       tarray = regData->blackacc[0]->transaction;
       ntrans = regData->blackacc[0]->numTrans;

       /* if there is only one account, then allocate one
        * extra transaction row.  That extra row is used
        * to allow the user to add new transactions */
       new_num_rows = NUM_ROWS_PER_TRANS*(ntrans+1) + NUM_HEADER_ROWS;

       main_acc = regData->blackacc[0];
    } else {
       tarray = accListGetSortedTrans (regData->blackacc);
       ntrans = xaccCountTransactions (tarray);

       /* for a multi-account ledger window, the addition of
        * new transactions is not allowed. Thus, no blank 
        * rows at the end. */
       new_num_rows = NUM_ROWS_PER_TRANS*ntrans + NUM_HEADER_ROWS;
       main_acc = NULL;
    }

    XtVaGetValues( regData->reg, XmNrows, &old_num_rows, NULL );
    XtVaGetValues( regData->reg, XmNcells, &data, NULL );
    
    /* The number of rows we need to add/subtract (ie, delta-rows :) */
    delta_rows  = new_num_rows - old_num_rows;
    ncols  = regData->numCols; 


    /* allocate a new matrix: */
    newData = (String **)_malloc(new_num_rows*sizeof(String *));
    for( i=0; i<new_num_rows; i++ ) {
      newData[i] = (String *)_malloc(ncols*sizeof(String *));
      for( j=0; j<ncols; j++ ) {
        newData[i][j] = NULL;
      }
    }

    /* add the column headers, copying from the old data: */
    for( i=0; i<NUM_HEADER_ROWS; i++ ) {
      for( j=0; j<ncols; j++ ) {
        newData[i][j] = XtNewString(data[i][j]);
      }
    }
    
    /* adjust the size of the matrix, only after copying old column headers: */
    if( delta_rows < 0 ) {
      XbaeMatrixDeleteRows( regData->reg, 1, -delta_rows );
    } else if( delta_rows > 0 ) {
      XbaeMatrixAddRows( regData->reg, 1, NULL, NULL, NULL, delta_rows );
    }
    
    /* and fill in the data for the matrix: */
    for (i=0; i<ntrans; i++) {
      int  row; 

      trans = tarray[i];
      row = NUM_ROWS_PER_TRANS*i + NUM_HEADER_ROWS;

      XbaeMatrixSetRowUserData  ( regData->reg, row, (XPointer) trans);   

      sprintf( buf, "%2d/%2d", trans->date.month, trans->date.day );
      newData[row+DATE_CELL_R][DATE_CELL_C]   = XtNewString(buf);
      sprintf( buf, "%4d", trans->date.year );
      newData[row+YEAR_CELL_R][YEAR_CELL_C] = XtNewString(buf);  
      
      sprintf( buf, "%s", trans->num );
      newData[row+NUM_CELL_R][NUM_CELL_C]   = XtNewString(buf);

/* hack alert xxxxxxxxxxxxx */
/* not correct for gen ledger */
      xfer_acc = xaccGetOtherAccount (main_acc, trans);
      if (xfer_acc) {
        sprintf( buf, "%s", xfer_acc->accountName );
        newData[row+XFRM_CELL_R][XFRM_CELL_C] = XtNewString(buf);
      }
      
      sprintf( buf, "%s", trans->description );
      newData[row+DESC_CELL_R][DESC_CELL_C]   = XtNewString(buf);
      sprintf( buf, "%s", trans->memo );
      newData[row+MEMO_CELL_R][MEMO_CELL_C] = XtNewString(buf);
      
      sprintf( buf, "%c", trans->reconciled );
      newData[row+RECN_CELL_R][RECN_CELL_C]   = XtNewString(buf);

      sprintf( buf, "%s", trans->action );
      newData[row+ACTN_CELL_R][ACTN_CELL_C]   = XtNewString(buf);

      /* ----------------------------------- */
      /* display depends on account type */
      switch (regData->type) {
        case BANK:
        case CASH:
        case ASSET:
        case CREDIT:
        case LIABILITY:
        case INCOME:
        case EXPENSE:
        case EQUITY:
          themount = xaccGetAmount (main_acc, trans);
          if( 0.0 > themount ) {
            sprintf( buf, "%.2f ", -themount );
          } else {
            sprintf( buf, "%.2f ", themount );
          }
          break;
        case STOCK:
        case MUTUAL:
          themount = xaccGetShareAmount (main_acc, trans);
          if( 0.0 > themount ) {
            sprintf( buf, "%.3f ", -themount );
          } else {
            sprintf( buf, "%.3f ", themount );
          }
          break;
        default:
          fprintf( stderr, "Internal Error: Account type: %d is unknown!\n", 
                  regData->type);
      }

      if( 0.0 > themount ) {
        newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
        newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
      } else {
        newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
        newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
      }
      
      /* ----------------------------------- */
      /* extra columns for mutual funds, etc. */
      switch(regData->type) {
        case BANK:
        case CASH:
        case ASSET:
        case CREDIT:
        case LIABILITY:
        case INCOME:
        case EXPENSE:
        case EQUITY:
          break;
        case STOCK:
        case MUTUAL:
          sprintf( buf, "%.2f ", trans->share_price );
          newData[row+PRIC_CELL_R][PRIC_CELL_C] = XtNewString(buf);

          /* don't set number of shares here -- this is computed later,
           * in recomputeBalance. */
          newData[row+SHRS_CELL_R][SHRS_CELL_C]   = XtNewString("");
          break;
        default:
          break;
      }
    }

    if (1 == regData->numAcc) {
      int row;
      Date date;
      todaysDate( &date );
    
      /* if there is just one account for this ledger, then 
       * there are some empty rows at the end, where the user
       * can create new transactions. Fill them in with emptiness
       */
      row = NUM_ROWS_PER_TRANS*ntrans + NUM_HEADER_ROWS;
      XbaeMatrixSetRowUserData  ( regData->reg, row, NULL);

      sprintf( buf, "%2d/%2d", date.month, date.day );
      newData[row+DATE_CELL_R][DATE_CELL_C]   = XtNewString(buf);
      sprintf( buf, "%4d", date.year );
      newData[row+YEAR_CELL_R][YEAR_CELL_C] = XtNewString(buf);  

      sprintf( buf, "%c", NREC);
      newData[row+RECN_CELL_R][RECN_CELL_C]   = XtNewString(buf);

    } else {

      /* If the number of accounts is greater than one, 
       * then the flat transaction array was alloc'ed and
       * must be freed. */
      _free (tarray);
    }
   
    /* put null-terminated strings into cells that 
     * have been otherwise left blank */
    for( i=0; i<new_num_rows; i++ ) {
      for( j=0; j<ncols; j++ ) {
        if (NULL == newData[i][j]) newData[i][j] = XtNewString("");
      }
    }

    /* set the cell data: */
    XtVaSetValues( regData->reg, XmNcells, newData, NULL );
    regRecalculateBalance (regData);
    
    /* and free memory!!! */
    _free (newData); 
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
  double  dclearedBalance = 0.0;
  double share_balance = 0.0;
  char buf[BUFSIZE];
  Transaction *trans;
  Account *acc;
  Widget reg;
  
  if( NULL == regData ) return 0.0;

  reg = regData->reg;

  /* hack alert -- nothing here is correct for a ledger xxxxxxxxxxx*/
  acc = regData->blackacc[0];

  xaccRecomputeBalance (acc);
  
  for( i=0; (trans=getTransaction(acc,i)) != NULL; i++ )
    {
    dbalance = xaccGetBalance (acc, trans);
    dclearedBalance = xaccGetClearedBalance (acc, trans);
    share_balance = xaccGetShareBalance (acc, trans);
    
    /* for income and expense acounts, we have to reverse
     * the meaning of balance, since, in a cual entry
     * system, income will show up as a credit to a 
     * bank account, and a debit to the income account.
     * Thus, positive and negative are interchanged */
    if( (EXPENSE   == regData->type) ||
        (INCOME    == regData->type) ) {
      dbalance = - dbalance;
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
      if( (MUTUAL == regData->type) ||
          (STOCK  == regData->type) ) 
        {
#ifdef USE_NO_COLOR
        sprintf( buf, "%.3f ", share_balance );
#else
        sprintf( buf, "%.3f ", DABS(share_balance) );
        
        /* Set the color of the text, depending on whether the
         * balance is negative or positive */
        if( 0.0 > share_balance )
          XbaeMatrixSetCellColor( reg, position, SHRS_CELL_C, negPixel );
        else
          XbaeMatrixSetCellColor( reg, position, SHRS_CELL_C, posPixel );
#endif
      
        /* Put the value in the cell */
        XbaeMatrixSetCell( reg, position, SHRS_CELL_C, buf );
        }

      position += NUM_ROWS_PER_TRANS;     /* each transaction has two rows */
      }
    }
  
  if( NULL != regData->balance )
    {
    sprintf( buf, "$ %.2f\n$ %.2f", 
             dbalance, dclearedBalance );
    
    XmTextSetString( regData->balance, buf );
    }
  
  
  /* make sure the balance field in the main 
   * window is up to date.  */ 
  refreshMainWindow();       

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

/* RECALC_BALANCE recomputes the balance shown in 
 * the register window, if its is visible 
 */

#define RECALC_BALANCE(sacc) {					\
  Account * xfer_acc;						\
  RegWindow * xfer_reg;						\
  xfer_acc = (Account *) (sacc);				\
  if (xfer_acc) {						\
    xfer_reg = (RegWindow *) (xfer_acc->regData);		\
    if (xfer_reg) {						\
      regRecalculateBalance (xfer_reg);				\
    }								\
  }								\
}

/* REFRESH_REGISTER redisplays the register window,
 * if it is visible 
 */
#define REFRESH_REGISTER(sacc) {				\
  Account * xfer_acc;						\
  RegWindow * xfer_reg;						\
  xfer_acc = (Account *) (sacc);				\
  if (xfer_acc) {						\
    xfer_reg = (RegWindow *) (xfer_acc->regData);		\
    if (xfer_reg) {						\
      regRefresh (xfer_reg);					\
    }								\
  }								\
}


/* DATE_REORDER needs to null out currEntry, because 
 * during date reordering we removed and re-inserted
 * the transaction... reset currEntry to zero to prevent it from
 * indicating a row that doesn't exist.  (That shouldn't happen
 * anyways!) 
 */
#define DATE_REORDER(sacc) {					\
  Account * xfer_acc;						\
  RegWindow * xfer_reg;						\
  xfer_acc = (Account *) (sacc);				\
  if (xfer_acc) {						\
    xfer_reg = (RegWindow *) (xfer_acc->regData);		\
    if (xfer_reg) {						\
      xfer_reg->currEntry = 0;					\
    }								\
  }								\
}

/* REMOVE_TRANS will not only remove a transaction from an account,
 * but it will also delete the appropriate rows from the register 
 * window, if the register window is visible
 */
#define REMOVE_TRANS(sacc,trans) {				\
  Account   *otherAcc = (Account *) (sacc); 			\
  if (otherAcc) {						\
    RegWindow *otherRegData = otherAcc -> regData;		\
    int n = getNumOfTransaction (otherAcc, trans);		\
								\
    /* remove the transaction */				\
    xaccRemoveTransaction( otherAcc, trans );			\
								\
    /* remove the rows from the matrix */			\
    if (otherRegData) {						\
      int otherrow = NUM_ROWS_PER_TRANS*n + NUM_HEADER_ROWS;	\
      XbaeMatrixDeleteRows( otherRegData->reg, 			\
          otherrow, NUM_ROWS_PER_TRANS );			\
      XbaeMatrixRefresh( otherRegData->reg);			\
    }								\
  }								\
}

static void
regSaveTransaction( RegWindow *regData, int position )
{
  /* save transaction structure... in order to speed this up, 
   * regData->changed is a bitfield that keeps track of things
   * that might have changed, so we only have to save the stuff
   * that has changed */
  char buf[BUFSIZE];
  int  row = position * NUM_ROWS_PER_TRANS + NUM_HEADER_ROWS;
  Transaction *trans;

  /* If nothing has changed, we have nothing to do */
  if (MOD_NONE == regData->changed) return;

  trans = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, row);
  
  if( trans == NULL )
    {
    /* This must be a new transaction */
    DEBUG("New Transaction\n");
    
    if (1 == regData->numAcc) {
      /* Be sure to prompt the user to save to disk after changes are made! */
      Account *acc = regData->blackacc[0];
      acc->parent->saved = False;

      trans = mallocTransaction();
      trans->credit = (struct _account *) acc;
  
      /* insert the transaction now.  If we later discover that 
       * the user hs not made any entries, we will remove it again.
       * However, for some of the itntermediate processing, we must 
       * have a valid transaction present in the account.
       */
      insertTransaction (acc, trans);
      regData->changed = MOD_ALL;
      } else {
         PERR ("regSaveTransaction(): can't have new transaction in ledger \n");
      }
    } else {
      /* Be sure to prompt the user to save to disk after changes are made! */
      Account *acc;
      acc = (Account *) trans->credit;
      if (acc) acc->parent->saved = False;
      acc = (Account *) trans->debit;
      if (acc) acc->parent->saved = False;
    }

  if( regData->changed & MOD_NUM )
    {
    DEBUG("MOD_NUM\n");	  
    /* ...the transaction number (String)... */
    XtFree( trans->num );
    trans->num = XtNewString( 
                 XbaeMatrixGetCell(regData->reg,row+NUM_CELL_R,NUM_CELL_C)); 
    }
  
  if( regData->changed & MOD_XFRM )
    {
    /* ... the transfer ... */
    char * name;
    /* hack alert xxxxxxxxxxxxx this is incorrect for ledger */
    Account *main_acc = regData->blackacc[0];
   
    Account *xfer_acct = xaccGetOtherAccount (main_acc, trans);
    DEBUG("MOD_XFRM\n");

    if (xfer_acct) {
      /* remove the transaction from wherever it used to be */
      REMOVE_TRANS (xfer_acct, trans);
  
      /* recalculate the balance and redisplay the window for the old acct */
      RECALC_BALANCE (xfer_acct);
      REFRESH_REGISTER (xfer_acct);
      }
       
    /* get the new account name */
    name = XbaeMatrixGetCell(regData->reg,row+XFRM_CELL_R, XFRM_CELL_C);
  
    /* get the new account from the name */
    xfer_acct = xaccGetPeerAccountFromName (main_acc, name);
  
    if (xfer_acct) {
      /* insert the transaction into the new account */
      insertTransaction (xfer_acct, trans);
      }
    }
  
  if( regData->changed & MOD_DESC )
    {
    DEBUG("MOD_DESC\n");
    /* ... the description... */
    XtFree( trans->description );
    trans->description = 
      XtNewString( 
      XbaeMatrixGetCell(regData->reg,row+DESC_CELL_R,DESC_CELL_C) );
    }
  
  if( regData->changed & MOD_MEMO )
    {
    String tmp;
    String  memo = NULL;
    DEBUG("MOD_MEMO\n");
    /* ... the memo ... */
    XtFree( trans->memo );
    tmp = XbaeMatrixGetCell(regData->reg,row+MEMO_CELL_R,MEMO_CELL_C);
    trans->memo = XtNewString( tmp );
    }
  
  if( regData->changed & MOD_ACTN )
    {
    String  actn = NULL;
    DEBUG("MOD_ACTN\n");
    /* ... the action ... */
    XtFree( trans->action );
    actn = XbaeMatrixGetCell(regData->reg,row+ACTN_CELL_R,ACTN_CELL_C);
    trans->action = XtNewString( actn );
    }
  
  if( regData->changed & MOD_RECN )
    {
    DEBUG("MOD_RECN\n");
    /* ...the reconciled flag (char)... */
    trans->reconciled = 
       (XbaeMatrixGetCell(regData->reg,row+RECN_CELL_R,RECN_CELL_C))[0];
    }
  
  if( regData->changed & MOD_AMNT )
    {
    String amount;
    float val=0.0;  /* must be float for sscanf to work */
    double themount = 0.0;
    /* hack alert xxxxxxxxxx this is incorrect for ledger */
    Account *main_acc = regData->blackacc[0];

    DEBUG("MOD_AMNT\n");
    /* ...and the amounts */
    amount = XbaeMatrixGetCell(regData->reg,row+DEP_CELL_R,DEP_CELL_C);
    sscanf( amount, "%f", &val );
    themount = val;
    
    val = 0.0;
    amount = XbaeMatrixGetCell(regData->reg,row+PAY_CELL_R,PAY_CELL_C); 
    sscanf( amount, "%f", &val );
    themount -= val;
    
    xaccSetShareAmount (main_acc, trans, themount);

    /* Reset so there is only one field filled */
    if( 0.0 > themount )
      {
      /* hack alert -- should keep 3 digits for share amounts */
      sprintf( buf, "%.2f ", -themount );
      XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, buf );
      XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, "" );
      }
    else
      {
      sprintf( buf, "%.2f ", themount );
      XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, "" );
      XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, buf );
      }
    }

  /* ignore MOD_PRIC for non-stock accounts */
  if( (regData->changed & MOD_PRIC) &&
      ((MUTUAL == regData->type) || (STOCK==regData->type)) )
    {
    String price;
    float val=0.0;  /* must be float for sscanf to work */

    DEBUG("MOD_PRIC\n");
    /* ...the price flag ... */

    price = XbaeMatrixGetCell(regData->reg,row+PRIC_CELL_R,PRIC_CELL_C);
    sscanf( price, "%f", &val );
    trans->share_price = val;
    
    sprintf( buf, "%.2f ", trans->share_price );
    XbaeMatrixSetCell( regData->reg, row+PRIC_CELL_R, PRIC_CELL_C, buf );
    }
  
  if( regData->changed & MOD_DATE )
    {
    Boolean outOfOrder = False;

    DEBUG("MOD_DATE\n");
    /* read in the date stuff... */
    sscanf( XbaeMatrixGetCell(regData->reg,row+DATE_CELL_R,DATE_CELL_C),"%d/%d",
            &(trans->date.month),
            &(trans->date.day) );
    
    trans->date.year = atoi(XbaeMatrixGetCell(regData->reg,row+YEAR_CELL_R,YEAR_CELL_C));
    
    /* take care of re-ordering implications on the register.
     * If the date changed on a double-entry (transfer) transaction,
     * then make sure that both register windows are updated .. */
    outOfOrder = xaccCheckDateOrderDE (trans);
    if( outOfOrder )
      {
      int pos;
      /* hack alert xxxxxxxxxxx this is incorrect for ledger */
      Account *main_acc = regData->blackacc[0];

      DATE_REORDER ((trans->credit));
      DATE_REORDER ((trans->debit));
    
      /* Scroll to the new location of the reordered transaction;
       * but do this only for this register, not any other register 
       * windows. */  
      pos = getNumOfTransaction (main_acc, trans);
      XbaeMatrixMakeCellVisible( regData->reg, 
         pos*NUM_ROWS_PER_TRANS+NUM_HEADER_ROWS, DESC_CELL_C );
      }
    }
    
  /* 
   * If this is a new transaction, and the user did not 
   * actually enter any data, then we should not really
   * consider this to be a new transaction! */
  if (regData->changed & MOD_NEW) {
    /* hack alert xxxxxxxxxxxxxxx this is icorrect for ledger */
    Account *main_acc = regData->blackacc[0];
    if( (strcmp("",trans->num) == 0)         &&
        (strcmp("",trans->description) == 0) &&
        (strcmp("",trans->memo) == 0)        &&
        (strcmp("",trans->action) == 0)      &&
        (0 == trans->catagory)               &&
        /* (NULL == xaccGetOtherAccount (acc, trans)) && */
        (NULL == trans->debit)               &&
        (1.0 == trans->share_price)          &&
        (0.0 == trans->damount) ) 
      {
      xaccRemoveTransaction (main_acc, trans);
      freeTransaction (trans);
      return;
      }
    }
  
  /* For many, but not all changes, we need to 
   * recalculate the balances */
  if( regData->changed & (MOD_XFRM | MOD_RECN | MOD_AMNT | MOD_PRIC | MOD_NEW)) {
    RECALC_BALANCE ((trans->credit));
    RECALC_BALANCE ((trans->debit));
  }

  REFRESH_REGISTER ((trans->credit));
  REFRESH_REGISTER ((trans->debit));

  /* reset the "changed" bitfield */
  regData->changed   = 0;

#define REFRESH_RECONCILE_WIN(sacc) {			\
  /* If the reconcile window is open, update it!!! */	\
  Account * xfer_acc;					\
  xfer_acc = (Account *) (sacc);			\
  if (xfer_acc) {					\
    if (NULL != xfer_acc->recnData) {			\
      recnRefresh( xfer_acc->recnData );		\
    }							\
  }							\
}

  REFRESH_RECONCILE_WIN ((trans->credit));
  REFRESH_RECONCILE_WIN ((trans->debit));
  
  return;
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

  retval = regWindowLedger (parent, acclist);
  return retval;
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
regWindowLedger( Widget parent, Account **acclist )
  {
  Transaction *trans;
  RegWindow   *regData;
  Widget menubar, pane, buttonform, frame, reg, widget;
  int    position=0;
  char *windowname;

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
  regData->changed   = 0;          /* Nothing has changed yet! */
  regData->currEntry = 0;
  regData->insert    = 0;          /* the insert (cursor) position in
                                    * quickfill cells */


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

  /* if there is only once account that we are supposed 
   * to display, use that account type as the display type. */
  if (1 == regData->numAcc) {
    regData->type = regData->blackacc[0]->type;
    regData->qf   = regData->blackacc[0]->qfRoot;

    /* avoid having two open registers for one account */
    regData->blackacc[0]->regData = regData;    
    windowname = regData->blackacc[0]->accountName;
  } else {
    regData->type = GEN_LEDGER;
    regData->qf   = regData->blackacc[0]->qfRoot;  /* hack alert -- this probably broken */
    /* hack alert -- xxxx -- do the ledgerlist thing */
  }

  regData->dialog =
    XtVaCreatePopupShell( "dialog", 
                          xmDialogShellWidgetClass, parent,
                          XmNdeleteResponse,   XmDESTROY,
                          XmNtitle,            windowname,
/*
                          XmNwidth,            395,
                          XmNheight,           400,
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
                           XmNpaneMinimum,   800,
                           NULL );
  
  /* Setup the menubar at the top of the window */
  /******************************************************************\
   * Set up the menubar menu-items                                  *
  \******************************************************************/
  {
  MenuItem reportMenu[] = {
    { "Simple...",          &xmPushButtonWidgetClass, 'S', NULL, NULL, True,
      NULL, (XtPointer)0,  (MenuItem *)NULL },
    NULL,
  };

  
  MenuItem activityMenu[] = {
    { "Transfer...",        &xmPushButtonWidgetClass, 'T', NULL, NULL, True,
      accountMenubarCB, (XtPointer)AMB_TRNS,  (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Reconcile...",       &xmPushButtonWidgetClass, 'C', NULL, NULL, True,
      startRecnCB, NULL, (MenuItem *)NULL },
    { "Adjust Balance...",  &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      startAdjBCB, NULL, (MenuItem *)NULL },
    { "Report",             &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      NULL, (XtPointer)0,  (MenuItem *)&reportMenu },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Delete Transaction", &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      deleteCB,     NULL,      (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Close Window",       &xmPushButtonWidgetClass, 'Q', NULL, NULL, True,
      destroyShellCB, NULL, (MenuItem *)NULL },
    NULL,
  };

  
  MenuItem helpMenu[] = {
    { "About...",           &xmPushButtonWidgetClass, 'A', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_ABOUT, (MenuItem *)NULL },
    { "Help...",            &xmPushButtonWidgetClass, 'H', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_REGWIN,(MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "License...",         &xmPushButtonWidgetClass, 'L', NULL, NULL, True,
      helpMenubarCB, (XtPointer)HMB_LIC,   (MenuItem *)NULL },
    NULL,
  };
  
  /* some compilers don't like dynamic data in initializers; 
   * so rather than inlining these, we initialize here ... :-( */
  activityMenu[2].callback_data=(XtPointer)regData;
  activityMenu[3].callback_data=(XtPointer)regData;
  activityMenu[6].callback_data=(XtPointer)regData;
  activityMenu[8].callback_data=(XtPointer)(regData->dialog);  /* destroy callback */

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
    /* define where each column shows up, and total number 
     * of columns for this register type.  The number on the 
     * right hand side is the physical location of the column */
    switch(regData->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
      case INCOME:
      case EXPENSE:
      case EQUITY:
        regData->columnLocation [DATE_COL_ID] = 0;
        regData->columnLocation [NUM_COL_ID]  = 1;
        regData->columnLocation [XFER_COL_ID] = 2;
        regData->columnLocation [DESC_COL_ID] = 3;
        regData->columnLocation [RECN_COL_ID] = 4;
        regData->columnLocation [PAY_COL_ID]  = 5;
        regData->columnLocation [DEP_COL_ID]  = 6;
        regData->columnLocation [BALN_COL_ID] = 7;
        regData -> numCols = 8;

        break;
      case STOCK:
      case MUTUAL:
        regData->columnLocation [DATE_COL_ID] = 0;
        regData->columnLocation [NUM_COL_ID]  = 1;
        regData->columnLocation [XFER_COL_ID] = 2;
        regData->columnLocation [DESC_COL_ID] = 3;
        regData->columnLocation [RECN_COL_ID] = 4;
        regData->columnLocation [PAY_COL_ID]  = 5;
        regData->columnLocation [DEP_COL_ID]  = 6;
        regData->columnLocation [PRIC_COL_ID] = 7;
        regData->columnLocation [SHRS_COL_ID] = 8;
        regData->columnLocation [BALN_COL_ID] = 9;
        regData -> numCols = 10;
        break;
      default:
        fprintf( stderr, "Internal Error: Account type: %d is unknown!\n", 
               regData->type);
      }

    /* ----------------------------------- */
    /* set up column widths */

    regData -> columnWidths[DATE_CELL_C] = 5;   /* also YEAR_CELL_C */
    regData -> columnWidths[NUM_CELL_C]  = 8;   /* also ACTN_CELL_C */
    regData -> columnWidths[XFRM_CELL_C] = 14;  /* also XTO_CELL_C */
    regData -> columnWidths[DESC_CELL_C] = 30;  /* also MEMO_CELL_C */
    regData -> columnWidths[RECN_CELL_C] = 1;   /* the widths of columns */
    regData -> columnWidths[PAY_CELL_C]  = 12;  /* the widths of columns */
    regData -> columnWidths[DEP_CELL_C]  = 12;  /* the widths of columns */
    regData -> columnWidths[BALN_CELL_C] = 12;  /* dollar balance */

    switch(regData->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
      case INCOME:
      case EXPENSE:
      case EQUITY:
        break;
      case STOCK:
      case MUTUAL:
        regData -> columnWidths[PRIC_CELL_C] = 8;   /* price */
        regData -> columnWidths[SHRS_CELL_C] = 8;   /* share balance */
        break;
      }
    
    /* ----------------------------------- */
    /* set up column alignments */

    regData -> alignments[DATE_CELL_C] = XmALIGNMENT_END;
    /* ACTN is in NUM_CELL, and needs to be visible */
    regData -> alignments[NUM_CELL_C]  = XmALIGNMENT_BEGINNING;  
    regData -> alignments[XFRM_CELL_C] = XmALIGNMENT_BEGINNING;  
    regData -> alignments[DESC_CELL_C] = XmALIGNMENT_BEGINNING;
    regData -> alignments[RECN_CELL_C] = XmALIGNMENT_CENTER;
    regData -> alignments[PAY_CELL_C]  = XmALIGNMENT_END;
    regData -> alignments[DEP_CELL_C]  = XmALIGNMENT_END;
    regData -> alignments[BALN_CELL_C] = XmALIGNMENT_END;

    switch(regData->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
      case INCOME:
      case EXPENSE:
      case EQUITY:
        break;

      case STOCK:
      case MUTUAL:
        regData -> alignments[PRIC_CELL_C] = XmALIGNMENT_END;  /* price */
        regData -> alignments[SHRS_CELL_C] = XmALIGNMENT_END;  /* share balance */
        break;
      }
    
    /* ----------------------------------- */
    /* Put the appropriate heading names in the column titles */
    for (i=0; i<3; i++) {
       for (j=0; j<NUM_COLUMNS; j++) {
          regData->columnLabels[i][j] = "";
       }
    }

    regData -> columnLabels[0][DATE_CELL_C] = "Date";
    regData -> columnLabels[0][NUM_CELL_C]  = "Num";
    regData -> columnLabels[0][XFRM_CELL_C] = "Transfer";
    regData -> columnLabels[0][DESC_CELL_C] = "Description";
    regData -> columnLabels[0][BALN_CELL_C] = "Balance";
    switch(regData->type)
      {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
      case INCOME:
      case EXPENSE:
      case EQUITY:
        break;
      case STOCK:
      case MUTUAL:
        regData -> columnLabels[0][PRIC_CELL_C] = "Price";
        regData -> columnLabels[0][SHRS_CELL_C] = "Tot Shrs";
        break;
      }
    
    switch(regData->type)
      {
      case BANK:
        regData -> columnLabels[0][PAY_CELL_C] = "Payment";
        regData -> columnLabels[0][DEP_CELL_C] = "Deposit";
        break;
      case CASH:
        regData -> columnLabels[0][PAY_CELL_C] = "Spend";
        regData -> columnLabels[0][DEP_CELL_C] = "Receive";
        break;
      case ASSET:
        regData -> columnLabels[0][PAY_CELL_C] = "Decrease";
        regData -> columnLabels[0][DEP_CELL_C] = "Increase";
        break;
      case CREDIT:
        regData -> columnLabels[0][PAY_CELL_C] = "Charge";
        regData -> columnLabels[0][DEP_CELL_C] = "Payment";
        break;
      case LIABILITY:
        regData -> columnLabels[0][PAY_CELL_C] = "Increase";
        regData -> columnLabels[0][DEP_CELL_C] = "Decrease";
        break;
      case INCOME:
        regData -> columnLabels[0][PAY_CELL_C] = "Income";
        regData -> columnLabels[0][DEP_CELL_C] = "Charge";
        break;
      case EXPENSE:
        regData -> columnLabels[0][PAY_CELL_C] = "Rebate";
        regData -> columnLabels[0][DEP_CELL_C] = "Expense";
        break;
      case EQUITY:
        regData -> columnLabels[0][PAY_CELL_C] = "Surplus";
        regData -> columnLabels[0][DEP_CELL_C] = "Deficit";
        break;
      case STOCK:
      case MUTUAL:
        regData -> columnLabels[0][PAY_CELL_C] = "Sold";
        regData -> columnLabels[0][DEP_CELL_C] = "Bought";
        break;
      }
    
    data = (String **)XtMalloc(3*sizeof(String *));
    data[0] = &(regData -> columnLabels[0][0]);
    data[1] = &(regData -> columnLabels[1][0]);
    data[2] = &(regData -> columnLabels[2][0]);
    sprintf( buf, "reg" );
    reg = XtVaCreateWidget( strcat(buf,accRes[regData->type]),
                            xbaeMatrixWidgetClass,  frame,
                            XmNcells,               data,
                            XmNfixedRows,           NUM_HEADER_ROWS,
                            XmNfixedColumns,        0,
                            XmNrows,                NUM_ROWS_PER_TRANS,
                            XmNvisibleRows,         15,
                            XmNfill,                True,
                            XmNcolumns,             regData -> numCols,
                            XmNcolumnWidths,        regData -> columnWidths,
                            XmNcolumnAlignments,    regData -> alignments,
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
  
  XtManageChild (reg);
  XtManageChild (frame);
  XtManageChild (pane);


  /* create action box for the first time */
  regData->actbox = actionBox (reg);

  /* create the xfer account box for the first time */
  /* but first, find the topmost group */
  {
  AccountGroup *grp;
  grp = xaccGetRootGroupOfAcct (regData->blackacc[0]);
  regData->xfrmbox = xferBox (reg, grp);
  regData->xtobox  = xferBox (reg, grp);
  }

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
  
  /* the "close" button */
  position++;
  widget = XtVaCreateManagedWidget( "Close", 
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
                 destroyShellCB, (XtPointer)(regData->dialog) );
  
  position += NUM_ROWS_PER_TRANS;
  
  /* Fix button area of the buttonform to its current size, and not let 
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
  
  /* unmanage the ComboBoxes, until they are needed */
  SetPopBox (regData->actbox,  -1, -1);
  SetPopBox (regData->xfrmbox, -1, -1);
  SetPopBox (regData->xtobox,  -1, -1);

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
static void 
closeRegWindow( Widget mw, XtPointer cd, XtPointer cb )
  {
  RegWindow *regData = (RegWindow *)cd;
  
  /* Save any unsaved changes */
  XbaeMatrixCommitEdit( regData->reg, False );
  regSaveTransaction( regData, regData->currEntry );
  
  /* hack alert -- free the ComboBox popup boxes data structures too */


  if (1 == regData ->numAcc) {
     regData->blackacc[0]->regData = NULL;
  } else {
     /* xxxxxxxxxxxx ledgerlist */
  }
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
  regSaveTransaction( regData, regData->currEntry );
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
  
  currow = NUM_ROWS_PER_TRANS *regData->currEntry + NUM_HEADER_ROWS;
  trans = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, currow);

  if( NULL != trans)
    {
    char buf[BUFSIZE];
    sprintf (buf, TRANS_DEL_MSG, trans->description);
    
    if( verifyBox( toplevel, buf ) )
      {
      Account * cred = (Account *) (trans->credit);
      Account * deb = (Account *) (trans->debit);
      
      /* remove the transaction from both accounts */
      REMOVE_TRANS (cred, trans);
      REMOVE_TRANS (deb, trans);

      RECALC_BALANCE (deb);
      RECALC_BALANCE (cred);

      /* Delete the transaction */
      freeTransaction (trans);
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
static void
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
 *         cd - regData - the data struct for this register         *
 *         cb -                                                     *
 * Return: none                                                     *
\********************************************************************/
static void
regCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  int row,col;
  String **data = NULL;
  Widget reg;
  
  XbaeMatrixDefaultActionCallbackStruct *cbs = 
    (XbaeMatrixDefaultActionCallbackStruct *)cb;
  
  RegWindow *regData = (RegWindow *)cd;
  /* hack alert xxxxxxxxxxxxxxx incorrect for ledger */
  Account   *acc = regData->blackacc[0];
  
  reg = regData->reg;
  
  row  = cbs->row;
  col  = cbs->column;
  
  switch( cbs->reason )
    {
    case XbaeEnterCellReason: {
      DEBUG("XbaeEnterCellReason\n");
      DEBUGCMD(printf(" row = %d\n col = %d\n",row,col));
      /* figure out if we are editing a different transaction... if we 
       * are, then we need to save the transaction we left */
      if( regData->currEntry != (row-NUM_HEADER_ROWS)/NUM_ROWS_PER_TRANS )
        {
        DEBUG("Save Transaction\n");
        DEBUGCMD(printf(" currEntry = %d\n currTrans = %d\n", 
                        regData->currEntry, 
                        (row+NUM_HEADER_ROWS)/NUM_ROWS_PER_TRANS));
        
        regSaveTransaction( regData, regData->currEntry );
        
        regData->currEntry = (row-NUM_HEADER_ROWS)/NUM_ROWS_PER_TRANS;
        regData->insert    = 0;
        regData->qf = acc->qfRoot;
        }
      
      /* If this is a cell which isn't editible, then don't
       * let the user enter! */
      if( !IN_DATE_CELL(row,col) && !IN_NUM_CELL(row,col) &&
          !IN_DESC_CELL(row,col) && !IN_PAY_CELL(row,col) &&
          !IN_RECN_CELL(row,col) && !IN_DEP_CELL(row,col) &&
          !IN_XFRM_CELL(row,col) && !IN_ACTN_CELL(row,col) &&
          !((STOCK  == regData->type) && IN_PRIC_CELL(row,col)) &&
          !((MUTUAL == regData->type) && IN_PRIC_CELL(row,col)) &&
          !IN_MEMO_CELL(row,col) && !IN_YEAR_CELL(row,col))
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
        
        if( data[row][RECN_CELL_C][0] == NREC ) {
          data[row][RECN_CELL_C][0] = CREC;
        } else {
          data[row][RECN_CELL_C][0] = NREC;
        }
        
        /* this cell has been modified, so we need to save when we
         * leave!!! */
        regData->changed |= MOD_RECN;
        
        XtVaSetValues( mw, XmNcells, data, NULL );
        XbaeMatrixRefreshCell( mw, row, RECN_CELL_C);
        }

      /* otherwise, move the ACTN widget */
      else if( IN_ACTN_CELL(row,col)) 
        {
           SetPopBox (regData->actbox, row, col);
           regData->changed |= MOD_ACTN;
        }

      /* otherwise, move the XFRM widget */
      else if( IN_XFRM_CELL(row,col) )
        {
           SetPopBox (regData->xfrmbox, row, col);
           regData->changed |= MOD_XFRM;
        }

      /* otherwise, move the XTO widget */
      else if(IN_XTO_CELL(row,col)  &&
             ((GEN_LEDGER == regData->type) ||
              (PORTFOLIO  == regData->type)) ) 
        {
           SetPopBox (regData->xtobox, row, col);
           regData->changed |= MOD_XTO;
        }
      break;
    }

    case XbaeModifyVerifyReason:
      DEBUG("XbaeModifyVerifyReason\n");
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
        int currow;
        Transaction *trans;

        currow = NUM_ROWS_PER_TRANS * regData->currEntry + NUM_HEADER_ROWS;
        trans = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, currow);

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
          DEBUG("resyncing quickfill!\n");
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
          /* char *str = regData->qf->trans->description; */
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
        dateCellFormat( mw, mvcbs, 0);     /* format according to date
                                            * cell rules */

      if( IN_YEAR_CELL(row,col) )
        dateCellFormat( mw, mvcbs, 1);     /* format according to date
                                            * cell rules */

      /* look to see if numeric format is OK.  Note that
       * the share price cell exists only for certain account types */
      if( IN_PAY_CELL(row,col) || IN_DEP_CELL(row,col) ||
          ((STOCK  == regData->type) && IN_PRIC_CELL(row,col)) ||
          ((MUTUAL == regData->type) && IN_PRIC_CELL(row,col)) )
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
      if( IN_DATE_CELL(row,col) || IN_YEAR_CELL(row,col) )
        regData->changed |= MOD_DATE;
      
      if( IN_NUM_CELL(row,col) )
        regData->changed |= MOD_NUM;
      
      if( IN_DESC_CELL(row,col) )
        regData->changed |= MOD_DESC;
      
      if( IN_PAY_CELL(row,col) || IN_DEP_CELL(row,col) )
        regData->changed |= MOD_AMNT;
      
      if( IN_MEMO_CELL(row,col) )
        regData->changed |= MOD_MEMO;
      
      if( ((STOCK  == regData->type) && IN_PRIC_CELL(row,col)) ||
          ((MUTUAL == regData->type) && IN_PRIC_CELL(row,col)) )
        regData->changed |= MOD_PRIC;

      /* Note: for cell widgets, this callback will never
       * indicate a row,col with a cell widget in it.  
       * Thus, the following if statment will never be true
       */
      if( IN_ACTN_CELL(row,col))  {
        regData->changed |= MOD_ACTN;
      }
      /* Note: for cell widgets, this callback will never
       * indicate a row,col with a cell widget in it.  
       * Thus, the following if statment will never be true
       */
      if( IN_XFRM_CELL(row,col) ) {
        regData->changed |= MOD_XFRM;
      }
      if(IN_XTO_CELL(row,col)  &&
        ((GEN_LEDGER == regData->type) || 
         (PORTFOLIO  == regData->type)) ) {
        regData->changed |= MOD_XTO;
      }

      break;

    case XbaeTraverseCellReason:
      DEBUG("XbaeTraverseCellReason\n");
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
              if( regData->qf->trans != NULL ) {
                double themount;
                themount = xaccGetAmount (acc, regData->qf->trans);
                if( 0.0 > themount )
                  {
                  sprintf( buf, "%.2f ", - themount );
                  XbaeMatrixSetCell( reg, tcbs->next_row, 
                                     tcbs->next_column, buf );
                  }
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
                if( regData->qf->trans != NULL ) {
                  double themount;
                  themount = xaccGetAmount (acc, regData->qf->trans);
                  if( 0.0 <= themount )
                    {
                    sprintf( buf, "%.2f ", themount );
                    XbaeMatrixSetCell( reg, tcbs->next_row, 
                                       tcbs->next_column, buf );
                    }
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
      PERR("regDB(): We shouldn't get here!");
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
static void
dateCellFormat( Widget mw, XbaeMatrixModifyVerifyCallbackStruct *mvcbs, int do_year )
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
  
  if (do_year) {
    row  = mvcbs->row - 1;
  } else {
    row  = mvcbs->row;
  }
  col  = mvcbs->column;
  
  date.day   = 0;
  date.month = 0;
  date.year  = 0;
  
  if (do_year) {
    sscanf( XbaeMatrixGetCell(mw,row,col),
            "%d/%d",&(date.month), &(date.day) );
  }else {
    sscanf( mvcbs->prev_text, 
            "%d/%d",&(date.month), &(date.day) );
  }
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
      DEBUG("next day\n");
      adjustDay( &date, 1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '-':
      /* prev day */
      DEBUG("prev day\n");
      adjustDay( &date, -1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 't':
      /* today */
      DEBUG("today\n");
      todaysDate( &date );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'm':
      /* beginning of month */
      DEBUG("beginning of month\n");
      date.day = 1;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'h':
      /* end of month */
      DEBUG("end of month\n");
      date.day = daysInMonth( date.month );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'y':
      /* beginning of year */
      DEBUG("beginning of year\n");
      date.day   = 1;
      date.month = 1;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case 'r':
      /* end of year */
      DEBUG("end of year\n");
      date.day   = 31;
      date.month = 12;
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case ']':
      /* next month */
      DEBUG("next month\n");
      adjustMonth( &date, +1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '[':
      /* prev month */
      DEBUG("prev month\n");
      adjustMonth( &date, -1 );
      mvcbs->verify->doit = False;
      changed = True;
      break;
    case '/':
      /* we need to keep track of the number of '/'s
       * on the first on, we accept, on the second one
       * we skip to the year cell
       */
      if (0 == do_year) 
      {
      int i,count=0;
      DEBUGCMD(printf(" = %s\n",mvcbs->prev_text));
      for( i=0; (mvcbs->prev_text)[i] != '\0'; i++ )
        if( (mvcbs->prev_text)[i] == '/' )
          count++;
      if( count >= 1 )
        {
        XbaeMatrixEditCell( mw, row+1, col );
        XbaeMatrixSelectCell( mw, row+1, col );
        }
      }
      break;
    default:
      /* only accept the input if it is a number */
      mvcbs->verify->doit = isNum(input);
    }
  
  if( changed )
    {
    sprintf( buf,"%2d/%2d", date.month, date.day );
    XbaeMatrixSetCell( mw, row, col, buf );
    XbaeMatrixRefreshCell( mw, row, col );
    
    sprintf( buf,"%4d", date.year );
    XbaeMatrixSetCell( mw, row+1, col, buf );
    XbaeMatrixRefreshCell( mw, row+1, col );
    }
  }
 
/************************** END OF FILE *************************/

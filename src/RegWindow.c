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

#include "config.h"

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


#define NUM_COLUMNS        20

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

  char header_rows;           /* number of header rows                   */

  /* quick-fill stuff */
  XmTextPosition insert;      /* used by quickfill for detecting deletes */
  QuickFill      *qf;         /* keeps track of current quickfill node.  *
                               * Reset to Account->qfRoot when entering  *
                               * a new transaction                       */

  /* pull-down (combo) box stuff */
  PopBox         *actbox;     /* ComboBox for actions                    */
  PopBox         *xfrmbox;    /* ComboBox for transfers                  */
  PopBox         *xtobox;     /* ComboBox for transfers                  */

  /* traversal stuff, to traverse through the combo boxes */
  int prev_row;
  int prev_col;

  /* structures for controlling the column layout */
  short          numCols;     /* number of columns in the register       */

  short  cellColLocation [NUM_COLUMNS]; /* cell location, column         */
  short  cellRowLocation [NUM_COLUMNS]; /* cell location, row            */
  short  columnWidths    [NUM_COLUMNS]; /* widths (in chars not pixels)  */

  String columnLabels  [5][NUM_COLUMNS];/* column labels                 */
                                        /* first array index must be     *
                                         * greater than                  *
                                         * [NUM_HEADER_ROWS +            *
                                         * NUM_ROWS_PER_TRANS]           */
  unsigned char alignments[NUM_COLUMNS]; /* alignment of display chars    */

} RegWindow;


#define NUM_HEADER_ROWS    (regData->header_rows) 
#define NUM_ROWS_PER_TRANS 2


/** PROTOTYPES ******************************************************/
RegWindow * regWindowLedger( Widget parent, Account **acclist, int type);

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
#define DATE_CELL_ID  0
#define YEAR_CELL_ID  1
#define NUM_CELL_ID   2
#define ACTN_CELL_ID  3
#define XFRM_CELL_ID  4 
#define XTO_CELL_ID   5 
#define DESC_CELL_ID  6
#define MEMO_CELL_ID  7
#define RECN_CELL_ID  8
#define PAY_CELL_ID   9   /* amount debit  */
#define DEP_CELL_ID   10  /* amount credit */
#define PRCC_CELL_ID  11  /* price credit  */
#define PRCD_CELL_ID  12  /* price debit   */
#define SHRS_CELL_ID  13
#define BALN_CELL_ID  14
#define VCRD_CELL_ID  15   /* value credit */
#define VDEB_CELL_ID  16   /* value debit  */

/* the actual cell location is pulled out of the location array */
#define DATE_CELL_C  (regData->cellColLocation[DATE_CELL_ID])
#define YEAR_CELL_C  (regData->cellColLocation[YEAR_CELL_ID])
#define NUM_CELL_C   (regData->cellColLocation[NUM_CELL_ID])
#define ACTN_CELL_C  (regData->cellColLocation[ACTN_CELL_ID])
#define DESC_CELL_C  (regData->cellColLocation[DESC_CELL_ID])
#define MEMO_CELL_C  (regData->cellColLocation[MEMO_CELL_ID]) 
#define XFRM_CELL_C  (regData->cellColLocation[XFRM_CELL_ID])
#define XTO_CELL_C   (regData->cellColLocation[XTO_CELL_ID])
#define RECN_CELL_C  (regData->cellColLocation[RECN_CELL_ID])
#define PAY_CELL_C   (regData->cellColLocation[PAY_CELL_ID])
#define DEP_CELL_C   (regData->cellColLocation[DEP_CELL_ID])
#define PRCC_CELL_C  (regData->cellColLocation[PRCC_CELL_ID])
#define PRCD_CELL_C  (regData->cellColLocation[PRCD_CELL_ID])
#define SHRS_CELL_C  (regData->cellColLocation[SHRS_CELL_ID])
#define BALN_CELL_C  (regData->cellColLocation[BALN_CELL_ID]) 
#define VCRD_CELL_C  (regData->cellColLocation[VCRD_CELL_ID]) 
#define VDEB_CELL_C  (regData->cellColLocation[VDEB_CELL_ID]) 

#define DATE_CELL_R  (regData->cellRowLocation[DATE_CELL_ID])
#define YEAR_CELL_R  (regData->cellRowLocation[YEAR_CELL_ID])
#define NUM_CELL_R   (regData->cellRowLocation[NUM_CELL_ID])
#define ACTN_CELL_R  (regData->cellRowLocation[ACTN_CELL_ID])
#define DESC_CELL_R  (regData->cellRowLocation[DESC_CELL_ID])
#define MEMO_CELL_R  (regData->cellRowLocation[MEMO_CELL_ID]) 
#define XFRM_CELL_R  (regData->cellRowLocation[XFRM_CELL_ID])
#define XTO_CELL_R   (regData->cellRowLocation[XTO_CELL_ID])
#define RECN_CELL_R  (regData->cellRowLocation[RECN_CELL_ID])
#define PAY_CELL_R   (regData->cellRowLocation[PAY_CELL_ID])
#define DEP_CELL_R   (regData->cellRowLocation[DEP_CELL_ID])
#define PRCC_CELL_R  (regData->cellRowLocation[PRCC_CELL_ID])
#define PRCD_CELL_R  (regData->cellRowLocation[PRCD_CELL_ID])
#define SHRS_CELL_R  (regData->cellRowLocation[SHRS_CELL_ID])
#define BALN_CELL_R  (regData->cellRowLocation[BALN_CELL_ID]) 
#define VCRD_CELL_R  (regData->cellRowLocation[VCRD_CELL_ID]) 
#define VDEB_CELL_R  (regData->cellRowLocation[VDEB_CELL_ID]) 


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

        /* Price cell       */
#define IN_PRCC_CELL(R,C) (RMOD(R,PRCC_CELL_R) && (C==PRCC_CELL_C))  
#define IN_PRCD_CELL(R,C) (RMOD(R,PRCD_CELL_R) && (C==PRCD_CELL_C))  

        /* Action cell      */
#define IN_ACTN_CELL(R,C) (RMOD(R,ACTN_CELL_R) && (C==ACTN_CELL_C))  

        /* Year cell        */
#define IN_YEAR_CELL(R,C) (RMOD(R,YEAR_CELL_R) && (C==YEAR_CELL_C))  

        /* Transfer cell    */
#define IN_XFRM_CELL(R,C) (RMOD(R,XFRM_CELL_R) && (C==XFRM_CELL_C))  
#define IN_XTO_CELL(R,C) (RMOD(R,XTO_CELL_R) && (C==XTO_CELL_C))  

        /* Balance cell     */
#define IN_BALN_CELL(R,C) (RMOD(R,BALN_CELL_R) && (C==BALN_CELL_C))  

        /* Transaction Value credit cell     */
#define IN_VCRD_CELL(R,C) (RMOD(R,VCRD_CELL_R) && (C==VCRD_CELL_C))  

        /* Transaction Value debit cell     */
#define IN_VDEB_CELL(R,C) (RMOD(R,VDEB_CELL_R) && (C==VDEB_CELL_C))  

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
    double themount;  /* amount */
    
    /* unmap the pop boxes, otherwise they get confused about
     * which row/transaction they belong to.  The goal is to
     * do this *before* rows are added to, or deleted from
     * the XbaeMatrix.  */
    SetPopBox (regData->actbox,  -1, -1);
    SetPopBox (regData->xfrmbox, -1, -1);
    SetPopBox (regData->xtobox,  -1, -1);

    /* first, build a sorted array of transactions */
    if (1 == regData->numAcc) {
       tarray = regData->blackacc[0]->transaction;
       ntrans = regData->blackacc[0]->numTrans;

    } else {
       tarray = accListGetSortedTrans (regData->blackacc);
       ntrans = xaccCountTransactions (tarray);
    }

    /* Allocate one extra transaction row.  That extra row 
     * is used to allow the user to add new transactions */
    new_num_rows = NUM_ROWS_PER_TRANS*(ntrans+1) + NUM_HEADER_ROWS;

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
      XbaeMatrixDeleteRows( regData->reg, NUM_HEADER_ROWS, -delta_rows );
    } else if( delta_rows > 0 ) {
      XbaeMatrixAddRows( regData->reg, NUM_HEADER_ROWS, 
                        NULL, NULL, NULL, delta_rows );
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

      /* for a general ledger, we fill out the "from" and "to"
       * transfer fields */
      if ( (GEN_LEDGER == regData->type) ||
           (INC_LEDGER == regData->type) ||
           (PORTFOLIO  == regData->type) ) {
        Account *xfer_acc;
        xfer_acc = (Account *) trans -> debit;
        if (xfer_acc) {
          sprintf( buf, "%s", xfer_acc->accountName );
          newData[row+XFRM_CELL_R][XFRM_CELL_C] = XtNewString(buf);
        }
        xfer_acc = (Account *) trans -> credit;
        if (xfer_acc) {
          sprintf( buf, "%s", xfer_acc->accountName );
          newData[row+XTO_CELL_R][XTO_CELL_C] = XtNewString(buf);
        }

      } else {

        /* if here, then this is not a general ledger type display.
         * We are displaying just one account. */
        Account *main_acc, *xfer_acc;

        main_acc = regData->blackacc[0];
        xfer_acc = xaccGetOtherAccount (main_acc, trans);
        if (xfer_acc) {
          sprintf( buf, "%s", xfer_acc->accountName );
          newData[row+XFRM_CELL_R][XFRM_CELL_C] = XtNewString(buf);
        }
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
      /* display of amounts depends on account type */
      /* For-single-account displays, positive and negative 
       * quantities are sorted into "payment" and "deposit"
       * columns. The value in each column is always positive.
       *
       * For a general ledger display, the sign is preserved,
       * and the two columns are interpreted as "debit" and 
       * "credit" columns.
       *
       * Note also, that for single accounts, the amount
       * is fetched based on the account, since this may 
       * introduce and extra minus sign, depending on whether
       * the account is being debited instead of credited.
       *
       * For general ledger entries, we can fetch the amount
       * directly: we already know which accounts should be 
       * debited and credited.
       */
      switch (regData->type) {
        case BANK:
        case CASH:
        case ASSET:
        case CREDIT:
        case LIABILITY:
        case INCOME:
        case EXPENSE:
        case EQUITY: {
          Account *main_acc = regData->blackacc[0];
          themount = xaccGetAmount (main_acc, trans);
          if( 0.0 > themount ) {
            sprintf( buf, "%.2f ", -themount );
            newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
            newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
          } else {
            sprintf( buf, "%.2f ", themount );
            newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
            newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
          }
        }  
          break;

        case STOCK:
        case MUTUAL: {
          Account *main_acc = regData->blackacc[0];
          themount = xaccGetShareAmount (main_acc, trans);

          /* if the share amount is zero (e.g. for a price quote)
           * then just leave both these cells blank */
          if( DEQ (0.0,themount) ) {
            newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
            newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
          } else 
          if( 0.0 > themount ) {
            sprintf( buf, "%.3f ", -themount );
            newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
            newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
          } else {
            sprintf( buf, "%.3f ", themount );
            newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
            newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
          }
        }  
          break;

        case INC_LEDGER: 
        case GEN_LEDGER: {
           Account * acc;
           int show;
           themount = trans->damount * trans->share_price;

           /* attempt to keep all displayed quantities positive by
            * flipping signs and columns for negative entries */
           /* hack alert -- but does this really work ??? */
           if (0.0 < themount) {
              sprintf( buf, "%.2f ", themount );
              acc = (Account *) (trans->debit);
              show = xaccIsAccountInList (acc, regData->blackacc);
              if (show) {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
              } else {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
              }
   
              acc = (Account *) (trans->credit);
              show = xaccIsAccountInList (acc, regData->blackacc);
              if (show) {
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
              } else {
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
              }
           } else {
              themount = - themount;
              sprintf( buf, "%.2f ", themount );
              acc = (Account *) (trans->debit);
              show = xaccIsAccountInList (acc, regData->blackacc);
              if (show) {
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
              } else {
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
              }
          
              acc = (Account *) (trans->credit);
              show = xaccIsAccountInList (acc, regData->blackacc);
              if (show) {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
              } else {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
              }
           }
        }  
          break;

        case PORTFOLIO: {
           Account * acc;
           int show;

           /* Show the share debit amount, if the debit account 
            * is in this ledger. But don't show share amount unless
            * the account type is stock or mutual, since other types
            * do not have shares. */
           show = 0;
           acc = (Account *) (trans->debit);
           if (acc) {
             if ((MUTUAL == acc->type) || (STOCK == acc->type) ) {
               show += xaccIsAccountInList (acc, regData->blackacc);
             }
           }
           acc = (Account *) (trans->credit);
           if (acc) {
             if ((MUTUAL == acc->type) || (STOCK == acc->type) ) {
               show += xaccIsAccountInList (acc, regData->blackacc);
             }
           }

           /* if the amount is zero, then leave the cell blank */
           themount = trans->damount;
           if (show && !(DEQ(0.0, themount)) ) {
              sprintf( buf, "%.3f ", DABS(themount) );
  
              if (0.0 > themount ) {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString(buf);
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
              } else {
                 newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
                 newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString(buf);
              }
           } else {
             newData[row+PAY_CELL_R][PAY_CELL_C] = XtNewString("");
             newData[row+DEP_CELL_R][DEP_CELL_C] = XtNewString("");
           }
        }  
          break;

        default:
          fprintf( stderr, "Internal Error: Account type: %d is unknown!\n", 
                  regData->type);
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
        case GEN_LEDGER:
        case INC_LEDGER:
          break;
        case STOCK:
        case MUTUAL:
          sprintf( buf, "%.2f ", trans->share_price );
          newData[row+PRCC_CELL_R][PRCC_CELL_C] = XtNewString(buf);
          break;

        case PORTFOLIO: {
           Account * acc;
           int show;

          /* show the price, only if the transaction records a 
           * share purchase/sale.  Otherwise, its a plain dollar
           * amount, and the price should not be shown. */
          show = 0;
          acc = (Account *) (trans->debit);
          if (acc) {
             if ( (MUTUAL == acc->type) || (STOCK == acc->type) ) {
               show += xaccIsAccountInList (acc, regData->blackacc);
             }
          }
          acc = (Account *) (trans->credit);
          if (acc) {
             if ( (MUTUAL == acc->type) || (STOCK == acc->type) ) {
               show += xaccIsAccountInList (acc, regData->blackacc);
             }
          }

          /* row location of prices depends on whether this 
           * is a purchase or a sale */
          if (show) {
            sprintf( buf, "%.3f ", trans->share_price );
            themount = trans->damount;
            if (0.0 < themount) {
              newData[row+PRCC_CELL_R][PRCC_CELL_C] = XtNewString(buf);
            } else {
              newData[row+PRCD_CELL_R][PRCD_CELL_C] = XtNewString(buf);
            }
          } else {
            newData[row+PRCC_CELL_R][PRCC_CELL_C] = XtNewString("");
            newData[row+PRCD_CELL_R][PRCD_CELL_C] = XtNewString("");
          }
        }
          break;
        default:
          break;
      }
    }

    {
      int row;
      Date date;
      todaysDate( &date );
    
      /* there are some empty rows at the end, where the user
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

    } 

    /* seems that Xbae doesn't like null cell entries, so fill them up. */
    for( i=0; i<new_num_rows; i++ ) {
      for( j=0; j<ncols; j++ ) {
        if (NULL == newData[i][j]) newData[i][j] = XtNewString ("");
      }
    }

    /* set the cell data: */
    XtVaSetValues( regData->reg, XmNcells, newData, NULL );
    regRecalculateBalance (regData);
    
    /* If the number of accounts is greater than one, 
     * then the flat transaction array was alloc'ed and
     * must be freed. */
    if (1 != regData->numAcc) {
      _free (tarray);
    }
   
    /* and free memory!!! */
    /* note that the XbaeMatrix widget makes a copy of everything,
     * so it is not only safe, but necessary to free everything.
     */
    for( i=0; i<new_num_rows; i++ ) {
      for( j=0; j<ncols; j++ ) {
        if (NULL != newData[i][j]) XtFree (newData[i][j]);
      }
    }
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
  int is_debit, is_credit;
  Account *credit_acc, *debit_acc;
  int num_rows;
  int  position   = NUM_HEADER_ROWS;
  double  dbalance    = 0.0;
  double  dclearedBalance = 0.0;
  double prt_balance = 0.0;
  double prt_clearedBalance = 0.0;
  double tmp;
  char buf[BUFSIZE];
  Transaction *trans;
  Widget reg;
  
  if( NULL == regData ) return 0.0;
  reg = regData->reg;

  /* recompute the balances for each of the accounts involved */
  xaccRecomputeBalances (regData->blackacc);

  /* get the number of rows in the register */
  XtVaGetValues( regData->reg, XmNrows, &num_rows, NULL );

  /* subtract the last (not-yet-existant) transaction */
  num_rows -= NUM_ROWS_PER_TRANS;

  /* zero everything out */
  dbalance = 0.0;
  dclearedBalance = 0.0;
  xaccZeroRunningBalances (regData->blackacc);
    
  /* loop over all of the rows */
  for (position = NUM_HEADER_ROWS; position < num_rows;
       position += NUM_ROWS_PER_TRANS) {

    trans = (Transaction *) XbaeMatrixGetRowUserData (reg, position);
    if (!trans) {
      PERR ("regRecalculateBalance(): missing transaction\n");
      continue;
    }

    credit_acc = (Account *) (trans->credit);
    debit_acc = (Account *) (trans->debit);

    /* figure out if this transaction shows up as a debit
     * and/or a credit for this list of accounts.  Note
     * that it may show up as both, and that's OK. */
    is_credit = xaccIsAccountInList (credit_acc, regData -> blackacc );
    is_debit  = xaccIsAccountInList (debit_acc,  regData -> blackacc );

    /* increment and/or decrement the running balance as appropriate */
    if (is_credit) {
      tmp = xaccGetBalance (credit_acc, trans);
      dbalance -= credit_acc -> running_balance;
      dbalance += tmp;
      credit_acc -> running_balance = tmp;

      if( NREC != trans->reconciled ) {
        tmp = xaccGetClearedBalance (credit_acc, trans);
        dclearedBalance -= credit_acc -> running_cleared_balance;
        dclearedBalance += tmp;
        credit_acc -> running_cleared_balance = tmp;
      }
    }

    if (is_debit) {
      tmp = xaccGetBalance (debit_acc, trans);
      dbalance -= debit_acc -> running_balance;
      dbalance += tmp;
      debit_acc -> running_balance = tmp;

      if( NREC != trans->reconciled ) {
        tmp = xaccGetClearedBalance (debit_acc, trans);
        dclearedBalance -= debit_acc -> running_cleared_balance;
        dclearedBalance += tmp;
        debit_acc -> running_cleared_balance = tmp;
      }
    }

    /* for income and expense acounts, we have to reverse
     * the meaning of balance, since, in a dual entry
     * system, income will show up as a credit to a 
     * bank account, and a debit to the income account.
     * Thus, positive and negative are interchanged */
    prt_balance = dbalance;
    if( (EXPENSE    == regData->type) ||
        (INCOME     == regData->type) ||
        (INC_LEDGER == regData->type) ) {
      prt_balance = - dbalance;
      prt_clearedBalance = - dclearedBalance;
    }

    if( reg != NULL ) {
#if USE_NO_COLOR
      sprintf( buf, "%.2f ", prt_balance );
#else
      sprintf( buf, "%.2f ", DABS(prt_balance) );
      
      /* Set the color of the text, depending on whether the
       * balance is negative or positive */
      if( 0.0 > prt_balance ) {
        XbaeMatrixSetCellColor( reg, position+BALN_CELL_R, 
                                     BALN_CELL_C, negPixel );
      } else {
        XbaeMatrixSetCellColor( reg, position+BALN_CELL_R, 
                                     BALN_CELL_C, posPixel );
      }
#endif
      
      /* Put the value in the cell */
      XbaeMatrixSetCell( reg, position+BALN_CELL_R, BALN_CELL_C, buf );

      /* update share balances too ... */
      switch (regData->type) {
        case MUTUAL:
        case STOCK: {
          double value = 0.0;
          double share_balance = 0.0;
          Account *acc;

          /* mutual and stock types have just one account */
          acc = regData->blackacc[0];
          /* ------------------------------------ */
          /* do the value of the transaction */
          value = xaccGetAmount (acc, trans);
        
          /* if the value is zero, just leave the cell blank */
          if (DEQ (0.0, value)) {
            buf[0] = 0x0;
          } else {
#if USE_NO_COLOR
            sprintf( buf, "%.2f ", value );
#else
            sprintf( buf, "%.2f ", DABS(value) );
#endif
          }
        
#if !USE_NO_COLOR
          /* Set the color of the text, depending on whether the
           * balance is negative or positive */
          if( 0.0 > value ) {
            XbaeMatrixSetCellColor( reg, position+VCRD_CELL_R, 
                                         VCRD_CELL_C, negPixel );
          } else {
            XbaeMatrixSetCellColor( reg, position+VCRD_CELL_R, 
                                         VCRD_CELL_C, posPixel );
          }
#endif
          /* Put the value in the cell */
          XbaeMatrixSetCell( reg, position+VCRD_CELL_R, VCRD_CELL_C, buf );

          /* ------------------------------------ */
          /* now show the share balance */
          share_balance = xaccGetShareBalance (acc, trans);
#if USE_NO_COLOR
          sprintf( buf, "%.3f ", share_balance );
#else
          sprintf( buf, "%.3f ", DABS(share_balance) );
          
          /* Set the color of the text, depending on whether the
           * balance is negative or positive */
          if( 0.0 > share_balance ) {
            XbaeMatrixSetCellColor( reg, position+SHRS_CELL_R, 
                                         SHRS_CELL_C, negPixel );
          } else {
            XbaeMatrixSetCellColor( reg, position+SHRS_CELL_R, 
                                         SHRS_CELL_C, posPixel );
          }
#endif

          /* Put the share balance in the cell */
          XbaeMatrixSetCell( reg, position+SHRS_CELL_R, SHRS_CELL_C, buf );

        }
          break;

        case PORTFOLIO: {
          double value = 0.0;
          double share_balance = 0.0;
          int show = 0;
          Account *acc;

          /* ------------------------------------ */
          /* show the value of the transaction, but only if the
           * account belongs on this ledger */
          value = trans->damount * trans->share_price;

#if !USE_NO_COLOR
          /* Set the color of the text, depending on whether the
           * value is negative or positive. Remebr that we flip 
           * the color value for debit cells */
          if( 0.0 > value ) {
            XbaeMatrixSetCellColor( reg, position+VDEB_CELL_R, 
                                         VDEB_CELL_C, posPixel );
            XbaeMatrixSetCellColor( reg, position+VCRD_CELL_R, 
                                         VCRD_CELL_C, negPixel );
          } else {
            XbaeMatrixSetCellColor( reg, position+VDEB_CELL_R, 
                                         VDEB_CELL_C, negPixel );
            XbaeMatrixSetCellColor( reg, position+VCRD_CELL_R, 
                                         VCRD_CELL_C, posPixel );
          }
#endif
          value = trans->damount * trans->share_price;
#if USE_NO_COLOR
          value = -value;  /* flip sign for debit accounts */
#else 
          value = DABS(value);
#endif
          acc = (Account *) (trans->debit);
          show = xaccIsAccountInList (acc, regData->blackacc);
          /* show only if value is non-zero (it may be zero if a price
           * is recorded) */
          if (show && !(DEQ(0.0, value))) {
            sprintf( buf, "%.2f ", value );  
          } else {
            buf[0] = 0x0;
          }
          XbaeMatrixSetCell( reg, position+VDEB_CELL_R, VDEB_CELL_C, buf );

          value = trans->damount * trans->share_price;
#if USE_NO_COLOR
          value = +value;  /* DO NOT flip sign for credit accounts */
#else 
          value = DABS(value);
#endif
          acc = (Account *) (trans->credit);
          show = xaccIsAccountInList (acc, regData->blackacc);
          /* show only if value is non-zero (it may be zero if a price
           * is recorded) */
          if (show && !(DEQ(0.0, value))) {
            sprintf( buf, "%.2f ", value );
          } else {
            buf[0] = 0x0;
          }
          XbaeMatrixSetCell( reg, position+VCRD_CELL_R, VCRD_CELL_C, buf );

          /* ------------------------------------ */
          /* show the share balance */
          /* show the share balance *only* either the debit or credit
           * account belongs in this ledger, *and* if the transaction
           * type is mutual or stock. */
          show = 0;
          share_balance = 0.0;

          acc = (Account *) (trans->debit);
          if (acc) {
             if ((MUTUAL == acc->type) || (STOCK == acc->type)) {
                show += xaccIsAccountInList (acc, regData->blackacc);
                share_balance = xaccGetShareBalance (debit_acc, trans);
             }
          }

          acc = (Account *) (trans->credit);
          if (acc) {
             if ((MUTUAL == acc->type) || (STOCK == acc->type)) {
                show += xaccIsAccountInList (acc, regData->blackacc);
                share_balance = xaccGetShareBalance (credit_acc, trans);
             }
          }

          if (show) {
#if USE_NO_COLOR
            sprintf( buf, "%.3f ", share_balance );
#else
            sprintf( buf, "%.3f ", DABS(share_balance) );
            
            /* Set the color of the text, depending on whether the
             * share balance is negative or positive */
            if( 0.0 > share_balance ) {
              XbaeMatrixSetCellColor( reg, position+SHRS_CELL_R, 
                                           SHRS_CELL_C, negPixel );
            } else {
              XbaeMatrixSetCellColor( reg, position+SHRS_CELL_R, 
                                           SHRS_CELL_C, posPixel );
            }
#endif
  
            /* Put the share balance in the cell */
            XbaeMatrixSetCell( reg, position+SHRS_CELL_R, SHRS_CELL_C, buf );
          } else {
            XbaeMatrixSetCell( reg, position+SHRS_CELL_R, SHRS_CELL_C, "" );
          }

        }
          break;

        default:
          break;
      }
    }
  }
  if( NULL != regData->balance ) {
    sprintf( buf, "$ %.2f\n$ %.2f", 
             prt_balance, prt_clearedBalance );
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
 * register/ledger windows, if they are visible. 
 */

#define RECALC_BALANCE(sacc) {					\
  Account * xfer_acc;						\
  RegWindow * xfer_reg;						\
  struct _RegWindow **list;					\
  xfer_acc = (Account *) (sacc);				\
  if (xfer_acc) {						\
    list = xfer_acc->ledgerList;				\
    if (list) {							\
      int n = 0;						\
      xfer_reg = (RegWindow *) (list [0]);			\
      while (xfer_reg) {					\
        regRecalculateBalance (xfer_reg);			\
        n++;							\
        xfer_reg = (RegWindow *) (list [n]);			\
      }								\
    }								\
  }								\
}

/* REFRESH_REGISTER redisplays the register/ledger windows,
 * if they are visible. 
 */
#define REFRESH_REGISTER(sacc) {				\
  Account * xfer_acc;						\
  RegWindow * xfer_reg;						\
  struct _RegWindow **list;					\
  xfer_acc = (Account *) (sacc);				\
  if (xfer_acc) {						\
    list = xfer_acc->ledgerList;				\
    if (list) {							\
      int n = 0;						\
      xfer_reg = (RegWindow *) (list [0]);			\
      while (xfer_reg) {					\
        regRefresh (xfer_reg);					\
        n++;							\
        xfer_reg = (RegWindow *) (list [n]);			\
      }								\
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
  Boolean dateOutOfOrder = False;

  /* If nothing has changed, we have nothing to do */
  if (MOD_NONE == regData->changed) return;

  trans = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, row);
  
  if( trans == NULL )
    {
    /* This must be a new transaction */
    DEBUG("New Transaction\n");
    
    trans = mallocTransaction();
    regData->changed = MOD_ALL;

    if (1 == regData->numAcc) {
      /* Be sure to prompt the user to save to disk after changes are made! */
      Account *acc = regData->blackacc[0];
      acc->parent->saved = False;

      /* by default, new transactions are considered credits */
      trans->credit = (struct _account *) acc;
    }
  } else {
    /* Be sure to prompt the user to save to disk after changes are made! */
    Account *acc;
    acc = (Account *) trans->credit;
    if (acc) acc->parent->saved = False;
    acc = (Account *) trans->debit;
    if (acc) acc->parent->saved = False;
  }

  if( regData->changed & MOD_XFRM ) 
    {
    /* ... the transfer ... */
    char * name;
    Account *xfer_acct = NULL;

    DEBUG("MOD_XFRM\n");

    /* the way that transfers are handled depends on whether this
     * is a ledger account, or a single-account register */
    if ( (GEN_LEDGER == regData->type) ||
         (INC_LEDGER == regData->type) ||
         (PORTFOLIO  == regData->type) ) {

      /* for a general ledger, the transfer-from account is obvious */
      xfer_acct = (Account *) trans->debit;
    } else {

      /* if not a ledger, then there is only one account,
       * and the transfer account is the other half of the 
       * pairing. -- Unless, of course, this is a new transaction. */
      if( regData->changed & MOD_NEW) {
         xfer_acct = NULL;
      } else {
         xfer_acct = xaccGetOtherAccount (regData->blackacc[0], trans);
      }
    }

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
    xfer_acct = xaccGetAccountFromName (topgroup, name);
  
    /* if a transfer account exists, and we are not trying to transfer
     * from ourself to ourself, then proceed, otheriwse ignore. */
    /* hack alert -- should put up a popup warning if user tries 
     * to transfer from & to the same account -- the two must differ! */
    if (xfer_acct && (    ((1 <  regData->numAcc) && (xfer_acct != (Account *) (trans->credit)))
                       || ((1 >= regData->numAcc) && (xfer_acct != regData->blackacc[0])) )) {
      
      /* for a new transaction, the default will be that the
       * transfer occurs from the debited account */
      if( regData->changed & MOD_NEW) {
         trans->debit = (struct _account *)xfer_acct;
      }

      /* for a general ledger, the transfer *must* occur 
       * from the debited account. */
      if ( (GEN_LEDGER == regData->type) ||
           (INC_LEDGER == regData->type) ||
           (PORTFOLIO  == regData->type) ) {
         trans->debit = (struct _account *)xfer_acct;
      }

      /* for non-new transactions, the transfer may be from the
       * debited or the credited account.  Which one it was depends
       * entirely on which account pointer is null after the 
       * removal of the old entry.  The insertTransaction()
       * subroutine will find the null slot, and will insert 
       * into it automatically. */

      /* insert the transaction into the transfer account */
      insertTransaction (xfer_acct, trans);
      }
    }


  /* if not a ledger, then we shouldn't get here ... */
  if( (regData->changed & MOD_XTO ) &&
     ((GEN_LEDGER == regData->type) ||
      (INC_LEDGER == regData->type) ||
      (PORTFOLIO  == regData->type)) )
    {
    /* ... the transfer ... */
    char * name;
    Account *xfer_acct;

    DEBUG("MOD_XTO\n");

    /* for a general ledger, from and to are easy to determine */
    xfer_acct = (Account *) trans->credit;

    if (xfer_acct) {
      /* remove the transaction from wherever it used to be */
      REMOVE_TRANS (xfer_acct, trans);
 
      /* recalculate the balance and redisplay the window for the old acct */
      RECALC_BALANCE (xfer_acct);
      REFRESH_REGISTER (xfer_acct);
      }
   
    /* get the new account name */
    name = XbaeMatrixGetCell(regData->reg,row+XTO_CELL_R, XTO_CELL_C);

    /* get the new account from the name */
    xfer_acct = xaccGetAccountFromName (topgroup, name);

    if (xfer_acct) {
      /* for a ledger, the transfer-to account is always the credited
       * account. */
      trans->credit = (struct _account *)xfer_acct;

      /* insert the transaction into the transfer account */
      insertTransaction (xfer_acct, trans);
      }
    }
  
  if( regData->changed & MOD_NUM )
    {
    DEBUG("MOD_NUM\n");	  
    /* ...the transaction number (String)... */
    XtFree( trans->num );
    trans->num = XtNewString( 
                 XbaeMatrixGetCell(regData->reg,row+NUM_CELL_R,NUM_CELL_C)); 
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
    double debit_amount = 0.0;
    double credit_amount = 0.0;

    DEBUG("MOD_AMNT\n");

    val = 0.0;
    amount = XbaeMatrixGetCell(regData->reg,row+PAY_CELL_R,PAY_CELL_C); 
    sscanf( amount, "%f", &val );
    debit_amount = val;

    val = 0.0;
    amount = XbaeMatrixGetCell(regData->reg,row+DEP_CELL_R,DEP_CELL_C);
    sscanf( amount, "%f", &val );
    credit_amount = val;
    
    switch (regData->type) {
      case BANK:
      case CASH:
      case ASSET:
      case CREDIT:
      case LIABILITY:
      case INCOME:
      case EXPENSE:
      case EQUITY: {
        double themount = credit_amount - debit_amount;
        Account *main_acc = regData->blackacc[0];
        xaccSetShareAmount (main_acc, trans, themount);

        /* Reset so there is only one field filled */
        if( 0.0 > themount )
          {
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
        break;

      case STOCK:
      case MUTUAL: {
        double themount = credit_amount - debit_amount;
        Account *main_acc = regData->blackacc[0];
        xaccSetShareAmount (main_acc, trans, themount);

        /* Reset so there is only one field filled */
        /* if share amount is zero (e.g. for a price quote) 
         * then leave both cells blank */
        if (DEQ (0.0, themount) ) 
          {
          XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, "" );
          XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, "" );
          } else
        if( 0.0 > themount )
          {
          sprintf( buf, "%.3f ", -themount );
          XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, buf );
          XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, "" );
          }
        else
          {
          sprintf( buf, "%.3f ", themount );
          XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, "" );
          XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, buf );
          }
        }
        break;

      case PORTFOLIO:
      case INC_LEDGER: 
      case GEN_LEDGER: {
        Account *acc;
        double themount = 0.0;
        int show_debit, show_credit;

        acc = (Account *) (trans->debit);
        show_debit = xaccIsAccountInList (acc, regData->blackacc);
        acc = (Account *) (trans->credit);
        show_credit = xaccIsAccountInList (acc, regData->blackacc);

        if (show_debit && show_credit) {
           /* try to figure out which entry the user changed ! */
           if (DEQ (debit_amount, trans->damount)) themount = credit_amount;
           if (DEQ (credit_amount, trans->damount)) themount = debit_amount;
        } else {
           themount = credit_amount - debit_amount;
        }
        trans->damount = themount;

        /* attempt to keep all displayed quantities positive by
         * flipping signs and columns for negative entries */
        /* hack alert -- but does this really work ??? */
        if (0.0 < themount) {
           if (PORTFOLIO == regData->type) {
             sprintf( buf, "%.3f ", themount );
           } else {
             sprintf( buf, "%.2f ", themount );
           }

           if (show_debit) {
              XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, buf );
           } else {
              XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, "" );
           }
  
           if (show_credit) {
              XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, buf );
           } else {
              XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, "" );
           }
        } else {
           themount = - themount;

           if (PORTFOLIO == regData->type) {
             sprintf( buf, "%.3f ", themount );
           } else {
             sprintf( buf, "%.2f ", themount );
           }

           if (show_debit) {
              XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, buf );
           } else {
              XbaeMatrixSetCell( regData->reg, row+DEP_CELL_R, DEP_CELL_C, "" );
           }
  
           if (show_credit) {
              XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, buf );
           } else {
              XbaeMatrixSetCell( regData->reg, row+PAY_CELL_R, PAY_CELL_C, "" );
           }
        }
      }
        break;

      default:
        break;
    }
  }

  /* ignore MOD_PRIC for non-stock accounts */
  if( (regData->changed & MOD_PRIC) &&
      ((MUTUAL    == regData->type) || 
       (STOCK     == regData->type) ||
       (PORTFOLIO == regData->type)) )
    {
    String price;
    float val=0.0;  /* must be float for sscanf to work */

    DEBUG("MOD_PRIC\n");
    /* ...the price flag ... */

    price = XbaeMatrixGetCell(regData->reg,row+PRCC_CELL_R,PRCC_CELL_C);
    sscanf( price, "%f", &val );
    trans->share_price = val;
    
    sprintf( buf, "%.2f ", trans->share_price );
    XbaeMatrixSetCell( regData->reg, row+PRCC_CELL_R, PRCC_CELL_C, buf );
    }
  
  /* If this is a new transaction, and the user did not 
   * actually enter any data, then we should not really
   * consider this to be a new transaction! Free it, and 
   * bail out */
  if (regData->changed & MOD_NEW) {
    if( (strcmp("",trans->num) == 0)         &&
        (strcmp("",trans->description) == 0) &&
        (strcmp("",trans->memo) == 0)        &&
        /* (strcmp("",trans->action) == 0)      && annoying! */
        /* (0 == trans->catagory)               && not implemented ! */
        /* (NULL == trans->credit)              && annoying! */
        (NULL == trans->debit)               &&  /* must check for this !*/
        (1.0 == trans->share_price)          &&
        (0.0 == trans->damount) ) {
      freeTransaction (trans);
      return;
    }

    /* if we got to here, we've got a live one. Insert it into 
     * an account, if we haven't done so already.  Do this 
     * before we get to the date code below, since date the
     * date code can/will break if the transaction is not yet 
     * inserted. */

    if (1 == regData->numAcc) {
      Account * acc = regData->blackacc[0];

      /* for single-account registers, the insertion account
       * is always the credited account */
      trans->credit = (struct _account *) acc;
      insertTransaction (acc, trans);
    } 

    /* for ledgers, the user *MUST* specify either a 
     * credited or a debited account, or both.  If they 
     * have specified these, then the account is already 
     * inserted, and we have nothing to do.  If they have
     * not specified either one, then it is an error 
     * condition -- we cannot insert, because we don't know
     * where to insert.
     *
     * Warn the user about this.  */
    if ((NULL == trans->credit) && (NULL == trans->debit)) {
      errorBox (toplevel, XFER_NO_ACC_MSG);
      freeTransaction (trans);
      return;
    }
  }

  if( regData->changed & MOD_DATE )
    {

    DEBUG("MOD_DATE\n");
    /* read in the date stuff... */
    sscanf( XbaeMatrixGetCell(regData->reg,row+DATE_CELL_R,DATE_CELL_C),"%d/%d",
            &(trans->date.month),
            &(trans->date.day) );
    
    trans->date.year = atoi(XbaeMatrixGetCell(regData->reg,
                                    row+YEAR_CELL_R,YEAR_CELL_C));
    
    /* take care of re-ordering implications on the register.
     * If the date changed on a double-entry (transfer) transaction,
     * then make sure that both register windows are updated .. */
    dateOutOfOrder = xaccCheckDateOrderDE (trans);
    }
    
  /* For many, but not all changes, we need to 
   * recalculate the balances */
  if( regData->changed & (MOD_XFRM | MOD_XTO | MOD_RECN | 
                          MOD_AMNT | MOD_PRIC | MOD_NEW)) {
    RECALC_BALANCE ((trans->credit));
    RECALC_BALANCE ((trans->debit));

    /* if this is a ledger window, then we have to update
     * it as well.  If this is not a ledger window, one of 
     * the above two lines will have handled it already */
    if (1 < regData->numAcc) {
      regRecalculateBalance (regData);
    }
  }

  REFRESH_REGISTER ((trans->credit));
  REFRESH_REGISTER ((trans->debit));
  /* if this is a ledger window, then we have to update
   * it as well.  If this is not a ledger window, one of 
   * the above two lines will have handled it already */
  /* minor hack alert -- this is probably not needed, as 
   * above macros should handle it */
  if (1 < regData->numAcc) {
    regRefresh (regData);
  }

  /* if the update changed the date ordering of the transactions,
   * then scroll the register to the new location of the transaction. 
   * Do so only for this window, not any of the others. */
  if( dateOutOfOrder ) {
    int newrow;
    Transaction *nt;

    /* find the location of the new row ... */
    newrow = NUM_HEADER_ROWS;
    nt = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, newrow);
    while (nt) {
      if (trans == nt) break;
      newrow += NUM_ROWS_PER_TRANS;
      nt = (Transaction *) XbaeMatrixGetRowUserData (regData->reg, newrow);
    }
    XbaeMatrixMakeCellVisible( regData->reg, newrow+YEAR_CELL_R, YEAR_CELL_C );

    /* Set the currEntry transaction pointer for this register window.
     * During date reordering we removed and re-inserted
     * the transaction at a new location.  currEntry should reflect that 
     * new location, in casee the user does additional editing.
     */
    regData->currEntry = (newrow-NUM_HEADER_ROWS)/NUM_ROWS_PER_TRANS;
  }

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
      NULL, (XtPointer)0,  (MenuItem *)NULL },
    { NULL, NULL, 0, NULL, NULL, False, NULL, (XtPointer)0, (MenuItem *)NULL},
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
    { "Report",             &xmPushButtonWidgetClass, 'R', NULL, NULL, False,
      NULL, (XtPointer)0,  reportMenu },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Delete Transaction", &xmPushButtonWidgetClass, 'D', NULL, NULL, True,
      deleteCB,     NULL,      (MenuItem *)NULL },
    { "",                   &xmSeparatorWidgetClass,    0, NULL, NULL, True,
      NULL,         NULL,                    (MenuItem *)NULL },
    { "Close Window",       &xmPushButtonWidgetClass, 'Q', NULL, NULL, True,
      destroyShellCB, NULL, (MenuItem *)NULL },
    { NULL, NULL, 0, NULL, NULL, False, NULL, (XtPointer)0, (MenuItem *)NULL},
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
    { NULL, NULL, 0, NULL, NULL, False, NULL, (XtPointer)0, (MenuItem *)NULL},
  };
  

  /******************************************************************\
   * Set quarks, create regData, compute register display type      *
  \******************************************************************/

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
  regData->changed     = 0;          /* Nothing has changed yet! */
  regData->header_rows = 1;
  regData->currEntry   = 0;
  regData->insert      = 0;          /* the insert (cursor) position in
                                      * quickfill cells */
  regData->prev_row    = 0;
  regData->prev_col    = 0;

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
    regData->qf   = regData->blackacc[0]->qfRoot;

    /* avoid having two open registers for one account */
    regData->blackacc[0]->regData = regData;    
    windowname = regData->blackacc[0]->accountName;
  } else {

    regData->header_rows = 2;

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

    /* hack alert -- quickfill for ledgers is almost certainly broken */
    regData->qf   = regData->blackacc[0]->qfRoot;  

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

    regData->cellColLocation [DATE_CELL_ID] = 0;
    regData->cellColLocation [YEAR_CELL_ID] = 0;
    regData->cellColLocation [NUM_CELL_ID]  = 1;
    regData->cellColLocation [ACTN_CELL_ID] = 1;
    regData->cellColLocation [XFRM_CELL_ID] = 2;
    regData->cellColLocation [XTO_CELL_ID]  = 2;
    regData->cellColLocation [DESC_CELL_ID] = 3;
    regData->cellColLocation [MEMO_CELL_ID] = 3;
    regData->cellColLocation [RECN_CELL_ID] = 4;
    regData->cellColLocation [PAY_CELL_ID]  = 5;
    regData->cellColLocation [DEP_CELL_ID]  = 6;

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
        regData->cellColLocation [XTO_CELL_ID]  = -1;
        regData->cellColLocation [PRCC_CELL_ID] = -1;
        regData->cellColLocation [PRCD_CELL_ID] = -1;
        regData->cellColLocation [SHRS_CELL_ID] = -1;
        regData->cellColLocation [VDEB_CELL_ID] = -1;
        regData->cellColLocation [VCRD_CELL_ID] = -1;
        regData->cellColLocation [BALN_CELL_ID] = 7;
        regData -> numCols = 8;
        break;

      case STOCK:
      case MUTUAL:
        regData->cellColLocation [XTO_CELL_ID]  = -1;
        regData->cellColLocation [PRCC_CELL_ID] = 7;
        regData->cellColLocation [PRCD_CELL_ID] = -1;
        regData->cellColLocation [VDEB_CELL_ID] = -1;
        regData->cellColLocation [VCRD_CELL_ID] = 8;
        regData->cellColLocation [SHRS_CELL_ID] = 9;
        regData->cellColLocation [BALN_CELL_ID] = 10;
        regData -> numCols = 11;
        break;

      case INC_LEDGER:
      case GEN_LEDGER:
        regData->cellColLocation [XTO_CELL_ID]  = 2;
        regData->cellColLocation [PRCC_CELL_ID] = -1;
        regData->cellColLocation [PRCD_CELL_ID] = -1;
        regData->cellColLocation [SHRS_CELL_ID] = -1;
        regData->cellColLocation [VDEB_CELL_ID] = -1;
        regData->cellColLocation [VCRD_CELL_ID] = -1;
        regData->cellColLocation [BALN_CELL_ID] = 7;
        regData -> numCols = 8;
        break;

      case PORTFOLIO:
        regData->cellColLocation [XTO_CELL_ID]  = 2;
        regData->cellColLocation [PRCC_CELL_ID] = 7;
        regData->cellColLocation [PRCD_CELL_ID] = 7;
        regData->cellColLocation [VDEB_CELL_ID] = 8;
        regData->cellColLocation [VCRD_CELL_ID] = 8;
        regData->cellColLocation [SHRS_CELL_ID] = 9;
        regData->cellColLocation [BALN_CELL_ID] = 10;
        regData -> numCols = 11;
        break;

      default:
        fprintf( stderr, "Internal Error: Account type: %d is unknown!\n", 
               regData->type);
      }
    /* ----------------------------------- */
    /* define where each row shows up,  The number on the 
     * right hand side is the physical location of the cell */

    regData->cellRowLocation [DATE_CELL_ID] = 0;
    regData->cellRowLocation [YEAR_CELL_ID] = 1;
    regData->cellRowLocation [NUM_CELL_ID]  = 0;
    regData->cellRowLocation [ACTN_CELL_ID] = 1;
    regData->cellRowLocation [XFRM_CELL_ID] = 0;
    regData->cellRowLocation [XTO_CELL_ID]  = 1;
    regData->cellRowLocation [DESC_CELL_ID] = 0;
    regData->cellRowLocation [MEMO_CELL_ID] = 1;
    regData->cellRowLocation [RECN_CELL_ID] = 0;
    regData->cellRowLocation [PAY_CELL_ID]  = 0;
    regData->cellRowLocation [DEP_CELL_ID]  = 0;
    regData->cellRowLocation [VCRD_CELL_ID] = 0;
    regData->cellRowLocation [BALN_CELL_ID] = 0;

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
        regData->cellRowLocation [XTO_CELL_ID]  = -1;
        regData->cellRowLocation [PRCC_CELL_ID] = -1;
        regData->cellRowLocation [PRCD_CELL_ID] = -1;
        regData->cellRowLocation [SHRS_CELL_ID] = -1;
        regData->cellRowLocation [VDEB_CELL_ID] = -1; 
        regData->cellRowLocation [VCRD_CELL_ID] = -1; 
        break;

      case STOCK:
      case MUTUAL:
        regData->cellRowLocation [XTO_CELL_ID]  = -1;
        regData->cellRowLocation [PRCC_CELL_ID] = 0;
        regData->cellRowLocation [PRCD_CELL_ID] = -1;
        regData->cellRowLocation [SHRS_CELL_ID] = 0;
        regData->cellRowLocation [VDEB_CELL_ID] = -1; 
        regData->cellRowLocation [VCRD_CELL_ID] = 0; 
        break;

      case INC_LEDGER:
      case GEN_LEDGER:
        regData->cellRowLocation [XTO_CELL_ID]  = 1;
        regData->cellRowLocation [DEP_CELL_ID]  = 1;  /* shift credit down */
        regData->cellRowLocation [BALN_CELL_ID] = 1;  /* shift balance down */
        regData->cellRowLocation [PRCC_CELL_ID] = -1;
        regData->cellRowLocation [PRCD_CELL_ID] = -1;
        regData->cellRowLocation [SHRS_CELL_ID] = -1;
        regData->cellRowLocation [VDEB_CELL_ID] = -1; 
        regData->cellRowLocation [VCRD_CELL_ID] = -1; 
        break;

      case PORTFOLIO:
        regData->cellRowLocation [XTO_CELL_ID]  = 1;
        regData->cellRowLocation [DEP_CELL_ID]  = 1;  /* shift credit down */
        regData->cellRowLocation [PRCD_CELL_ID] = 0;
        regData->cellRowLocation [PRCC_CELL_ID] = 1;  /* shift credit down */
        regData->cellRowLocation [SHRS_CELL_ID] = 1;
        regData->cellRowLocation [VDEB_CELL_ID] = 0;  /* debit on top */
        regData->cellRowLocation [VCRD_CELL_ID] = 1;  /* credit on bottom */
        regData->cellRowLocation [BALN_CELL_ID] = 1;  /* shift balance down */
        break;

      default:
        break;
      }

    /* ----------------------------------- */
    /* set up column widths */

    /* date column needs to be six-wide to get the accelerator
     * keys to function when double-digit days & months appear */
    regData -> columnWidths[DATE_CELL_C] = 6;   /* also YEAR_CELL_C */
    regData -> columnWidths[NUM_CELL_C]  = 6;   /* also ACTN_CELL_C */
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
      case GEN_LEDGER:
      case INC_LEDGER:
        break;
      case STOCK:
      case MUTUAL:
      case PORTFOLIO:
        regData -> columnWidths[PRCC_CELL_C] = 8;   /* price */
        regData -> columnWidths[SHRS_CELL_C] = 10;  /* share balance */
        regData -> columnWidths[VCRD_CELL_C] = 10;  /* transaction value */
        break;
      }
    
    /* ----------------------------------- */
    /* set up column alignments */

    regData -> alignments[DATE_CELL_C] = XmALIGNMENT_CENTER;
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
      case GEN_LEDGER:
      case INC_LEDGER:
        break;

      case STOCK:
      case MUTUAL:
      case PORTFOLIO:
        regData -> alignments[PRCC_CELL_C] = XmALIGNMENT_END;  /* price */
        regData -> alignments[SHRS_CELL_C] = XmALIGNMENT_END;  /* share baln */
        regData -> alignments[VCRD_CELL_C] = XmALIGNMENT_END;  /* trans value */
        break;
      }
    
    /* ----------------------------------- */
    /* Put the appropriate heading names in the column titles */
    for (i=0; i<(NUM_HEADER_ROWS+NUM_ROWS_PER_TRANS); i++) {
       for (j=0; j<NUM_COLUMNS; j++) {
          regData->columnLabels[i][j] = "";
       }
    }

    regData -> columnLabels[0][DATE_CELL_C] = "Date";
    regData -> columnLabels[0][NUM_CELL_C]  = "Num";
    regData -> columnLabels[0][XFRM_CELL_C] = "Transfer From";
    regData -> columnLabels[0][DESC_CELL_C] = "Description";
    regData -> columnLabels[0][BALN_CELL_C] = "Balance";

    if (1 < NUM_HEADER_ROWS) {
      regData -> columnLabels[1][ACTN_CELL_C] = "Action";
      regData -> columnLabels[1][XTO_CELL_C]  = "Transfer To";
      regData -> columnLabels[1][MEMO_CELL_C] = "Memo";
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
        regData -> columnLabels[0][PAY_CELL_C] = "Depreciation";
        regData -> columnLabels[0][DEP_CELL_C] = "Appreciation";
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
      case PORTFOLIO:
        regData -> columnLabels[0][PAY_CELL_C] = "Sold";
        regData -> columnLabels[0][DEP_CELL_C] = "Bought";
        break;
      case GEN_LEDGER:
        regData -> columnLabels[0][PAY_CELL_C] = "Debit";
        regData -> columnLabels[0][DEP_CELL_C] = "Credit";
        break;
      case INC_LEDGER:
        regData -> columnLabels[0][PAY_CELL_C] = "Credit";
        regData -> columnLabels[0][DEP_CELL_C] = "Debit";
        break;
      }
    
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
      case GEN_LEDGER:
      case INC_LEDGER:
        break;
      case STOCK:
      case MUTUAL:
      case PORTFOLIO:
        regData -> columnLabels[0][PRCC_CELL_C] = "Price";
        regData -> columnLabels[0][SHRS_CELL_C] = "Tot Shrs";
        regData -> columnLabels[0][VCRD_CELL_C] = "Value";
        if (1 < NUM_HEADER_ROWS) {
          regData -> columnLabels[0][PRCC_CELL_C] = "Sale Price";
          regData -> columnLabels[1][PRCC_CELL_C] = "Purch Price";
          regData -> columnLabels[0][VCRD_CELL_C] = "Debit";
          regData -> columnLabels[1][VCRD_CELL_C] = "Credit";
          regData -> columnLabels[0][DEP_CELL_C] = "";
          regData -> columnLabels[1][DEP_CELL_C] = "Bought";
          regData -> columnLabels[0][SHRS_CELL_C] = "";
          regData -> columnLabels[1][SHRS_CELL_C] = "Tot Shrs";
          regData -> columnLabels[0][BALN_CELL_C] = "";
          regData -> columnLabels[1][BALN_CELL_C] = "Balance";
        }
        break;
      }

    data = (String **)XtMalloc(
                      (NUM_HEADER_ROWS+NUM_ROWS_PER_TRANS) * 
                      sizeof(String *));

    for (i=0; i<(NUM_HEADER_ROWS+NUM_ROWS_PER_TRANS); i++) {
      data[i] = &(regData -> columnLabels[i][0]);
    }

  /******************************************************************\
   * The main register window itself                                *
  \******************************************************************/

    sprintf( buf, "reg" );
    reg = XtVaCreateWidget( strcat(buf,accRes[regData->type]),
                            xbaeMatrixWidgetClass,  frame,
                            XmNcells,               data,
                            XmNfixedRows,           NUM_HEADER_ROWS,
                            XmNfixedColumns,        0,
                            XmNrows,                NUM_HEADER_ROWS+NUM_ROWS_PER_TRANS,
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
                            XmNnavigationType,      XmEXCLUSIVE_TAB_GROUP,  
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
  { 
  int width, downwidth;
  width = XbaeMatrixGetColumnPixelWidth (reg, regData->cellColLocation[ACTN_CELL_ID]);
  /* it would be nice if ComboBox had an XmNunits resource ... but it doesn't */
  downwidth =  (int) (1.3 * ((float) width));
  regData->actbox = actionBox (reg, width, downwidth);
  }

  /* create the xfer account box for the first time */
  /* but first, find the topmost group */
  {
  AccountGroup *grp;
  int width, downwidth;
  grp = xaccGetRootGroupOfAcct (regData->blackacc[0]);
  width = XbaeMatrixGetColumnPixelWidth (reg, regData->cellColLocation[XFRM_CELL_ID]);
  downwidth =  (int) (1.2 * ((float) width));
  /* it would be nice if ComboBox had an XmNunits resource ... but it doesn't */
  regData->xfrmbox = xferBox (reg, grp, width, downwidth);
  regData->xtobox  = xferBox (reg, grp, width, downwidth);
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
  regData->record = widget;
  
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
                                    XmNnavigationType,     XmNONE,  /* stop tabbing ! */
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
                                    XmNnavigationType,     XmNONE,  /* don't tab here! */
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
  
  /* free combo-box data too */
  freePopBox (regData->actbox);
  freePopBox (regData->xfrmbox);
  freePopBox (regData->xtobox);

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

      REFRESH_REGISTER (deb);
      REFRESH_REGISTER (cred);

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

        /* hack alert -- quick-fill mostly broken for ledgers */
        if (1 == regData->numAcc) {
          Account   *acc = regData->blackacc[0];
          regData->qf = acc->qfRoot;
          } else {
          regData->qf = NULL;
          }
        }
      
      /* If this is a cell which isn't editible, then don't
       * let the user enter! */
      if( !IN_DATE_CELL(row,col) && !IN_NUM_CELL (row,col) &&
          !IN_DESC_CELL(row,col) && !IN_PAY_CELL (row,col) &&
          !IN_RECN_CELL(row,col) && !IN_DEP_CELL (row,col) &&
          !IN_XFRM_CELL(row,col) && !IN_XTO_CELL (row,col) &&
          !IN_ACTN_CELL(row,col) &&
          !IN_PRCC_CELL(row,col) && !IN_PRCD_CELL(row,col) &&
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
      else if( IN_XTO_CELL(row,col) )
        {
           SetPopBox (regData->xtobox, row, col);
           regData->changed |= MOD_XTO;
        }
      break;
    }

    case XbaeModifyVerifyReason: {
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
      
#if USE_QUICKFILL
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
                          (int) regData->insert,
                          (int) (mvcbs->verify->currInsert) ));
          /* we are out of sync, which means that the user
           * must have hit delete or something... to be on
           * the safe side, rescan the description field,
           * to ensure that quickfill works correctly for
           * the data that is actually in the cell */
          regData->qf = (regData->blackacc[0])->qfRoot;
          for( i=0; i<regData->insert; i++ )
            regData->qf = getQuickFill( regData->qf, mvcbs->prev_text[i] );
          }
        
        /* ptr will be NULL if the delete key or other 
         * non-alphanumeric key hit */
        if (mvcbs->verify->text->ptr) {
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
            XbaeMatrixSetCursorPosition( mw, regData->insert+1 );
            }
          else
            {
            char str[BUFSIZE];
            strncpy( str, mvcbs->prev_text, regData->insert );
            /* Need to make sure the string is terminated: */
            str[regData->insert] = '\0';
          
            XbaeMatrixSetCell( mw, row, col, str );
            XbaeMatrixRefreshCell( mw, row, col );
            XbaeMatrixSetCursorPosition( mw, regData->insert );
            }
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
      if( IN_PAY_CELL (row,col) || IN_DEP_CELL(row,col) ||
          IN_PRCC_CELL(row,col) || IN_PRCD_CELL(row,col) )
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
      
      if( IN_PRCC_CELL(row,col) || IN_PRCD_CELL(row,col) ) 
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
      if( IN_XTO_CELL(row,col) ) {
        regData->changed |= MOD_XTO;
      }
      break;
    } 

    case XbaeTraverseCellReason: {
      DEBUG("XbaeTraverseCellReason\n");
      /* This ensure that whenever the user hits TAB, they go to the
       * next valid cell.  Also, if regData->qf and regData->qf->trans
       * aren't NULL, then fill the field we are entering with the
       * data from regData->qf->trans */
      {
        char buf[BUFSIZE];
        XbaeMatrixTraverseCellCallbackStruct *tcbs =
        (XbaeMatrixTraverseCellCallbackStruct *)cb;
        
      
        /* If the quark is zero, then it is likely that we are
         * here because we traversed out of a cell that had a 
         * PopBox in it.  The PopBox is clever enough to put
         * us back into the register after tabing out of it.
         * However, its not (cannot be) clever enough to pretend
         * that it was a tab group in the register.  Thus,
         * we will emulate that we left a tab group in the register
         * to get here.  To put it more simply, we just set the 
         * row and column to that of the PopBox, which we had
         * previously recorded, and continue on as if nothing 
         * happened.  
         * BTW -- note that we are emulating a normal, right-moving tab. 
         * Backwards tabs are probably broken. 
         */
        if (NULLQUARK == tcbs->qparam) {
          if ((0==row) && (0==col)) {
             if ((0 != regData->prev_row) || (0 != regData->prev_col)) {
               tcbs->qparam = QRight;
               row = regData->prev_row;
               col = regData->prev_col;
             }
          }
        }

        if( tcbs->qparam == QRight )
          {
          /* Don't need to check IN_DATE_CELL or IN_NUM_CELL because
           * the default transversal from these cells is correct */
          
          if( IN_XFRM_CELL(row,col) ) {
            tcbs->next_column = DESC_CELL_C;
            tcbs->next_row = row - XFRM_CELL_R + DESC_CELL_R;
          }

          if ((GEN_LEDGER == regData->type) ||
              (INC_LEDGER == regData->type) ||
              (PORTFOLIO  == regData->type)) {
            if( IN_XTO_CELL(row,col) ) {
              tcbs->next_column = MEMO_CELL_C;
              tcbs->next_row = row - XTO_CELL_R + MEMO_CELL_R;
            }
          }

          if( IN_ACTN_CELL(row,col) ) {
            /* the cell that follows action depends on 
             * whether its a ledger or not */
            if ((GEN_LEDGER == regData->type) ||
                (INC_LEDGER == regData->type) ||
                (PORTFOLIO  == regData->type)) {
              tcbs->next_column = XTO_CELL_C;
              tcbs->next_row = row - ACTN_CELL_R + XTO_CELL_R;
            } else {
              tcbs->next_column = MEMO_CELL_C;
              tcbs->next_row = row - ACTN_CELL_R + MEMO_CELL_R;
              if( regData->qf != NULL ) {
                if( regData->qf->trans != NULL ) {
                  XbaeMatrixSetCell( reg, tcbs->next_row, tcbs->next_column,
                                     regData->qf->trans->memo );
                }
              }
            }
          }

          if( IN_DESC_CELL(row,col) ) {
            tcbs->next_column = PAY_CELL_C;
            tcbs->next_row = row - DESC_CELL_R + PAY_CELL_R;
            if( regData->qf != NULL ) {
              if( regData->qf->trans != NULL ) {
                /* hack alert -- this is guarenteed to break for ledgers */
                Account *acc = regData->blackacc[0];
                double themount;
                themount = xaccGetAmount (acc, regData->qf->trans);
                if( 0.0 > themount ) {
                  sprintf( buf, "%.2f ", - themount );
                  XbaeMatrixSetCell( reg, tcbs->next_row, 
                                     tcbs->next_column, buf );
                }
              }
            }
          }
          
          if( IN_PAY_CELL(row,col) ) {
            /* In this field, we either go to the deposit field,
             * if the user hasn't entered any data in this field,
             * or to the action field, if the user has entered data */
            XbaeMatrixCommitEdit(reg,True);
            if( strcmp(XbaeMatrixGetCell(reg,tcbs->row,tcbs->column),"") != 0 ) {
              /* hmm .. for stocks & mutual funds, the next field is price.
               * ordinary accounts don't have a price so hop to action. */
              if ((STOCK     == regData->type) ||
                  (MUTUAL    == regData->type)) {
                tcbs->next_column = PRCC_CELL_C;
                tcbs->next_row    = row - PAY_CELL_R + PRCC_CELL_R;
              } else

              /* for portfolios, if a sale price is specified, hop to sale-price cell */
              if (PORTFOLIO == regData->type) {
                tcbs->next_column = PRCD_CELL_C;
                tcbs->next_row    = row - PAY_CELL_R + PRCD_CELL_R;
              } else {
                tcbs->next_column = ACTN_CELL_C;
                tcbs->next_row    = row - PAY_CELL_R + ACTN_CELL_R;
              }
            } else {
              tcbs->next_column = DEP_CELL_C;
              tcbs->next_row    = row - PAY_CELL_R + DEP_CELL_R;
              if( NULL != regData->qf ) {
                if( regData->qf->trans != NULL ) {
                  /* hack alert -- this is guarenteed to break for ledgers */
                  Account *acc = regData->blackacc[0];
                  double themount;
                  themount = xaccGetAmount (acc, regData->qf->trans);
                  if( 0.0 <= themount ) {
                    sprintf( buf, "%.2f ", themount );
                    XbaeMatrixSetCell( reg, tcbs->next_row, 
                                       tcbs->next_column, buf );
                  }
                }
              }
            }
          }

          if( IN_DEP_CELL(row,col) ) {
            /* hmm .. for stocks & mutual funds, the next field is price.
             * ordinary accounts don't have a price so hop to action. */
            if ((STOCK     == regData->type) ||
                (MUTUAL    == regData->type) ||
                (PORTFOLIO == regData->type)) {
              tcbs->next_column = PRCC_CELL_C;
              tcbs->next_row    = row - DEP_CELL_R + PRCC_CELL_R;
            } else {
              tcbs->next_column = ACTN_CELL_C;
              tcbs->next_row    = row - DEP_CELL_R + ACTN_CELL_R;
            }
          }
          
          /* price-credit cell only in stock, mutual registers */
          if ((STOCK     == regData->type) ||
              (MUTUAL    == regData->type)) {
            if( IN_PRCC_CELL(row,col) ) {
              tcbs->next_column = ACTN_CELL_C;
              tcbs->next_row    = row - PRCC_CELL_R + ACTN_CELL_R;
            }
          }

          /* price-debit cell only in portfolio ledger */
          if ((PORTFOLIO == regData->type)) {
            if( IN_PRCC_CELL(row,col) ) {
              tcbs->next_column = PRCD_CELL_C;
              tcbs->next_row    = row - PRCC_CELL_R + PRCD_CELL_R;
            }

            if( IN_PRCD_CELL(row,col) ) {
              tcbs->next_column = ACTN_CELL_C;
              tcbs->next_row    = row - PRCD_CELL_R + ACTN_CELL_R;
            }
          }

          /* If we are in the memo cell, stay there! */
          /* umm, no, don't stay, go to the record button */
          if( (IN_MEMO_CELL(row,col)) && 
              (IN_BAD_CELL(tcbs->next_row,tcbs->next_column)) ) {
#ifdef STAY_IN_THE_MEMO_CELL
            tcbs->next_row    = row;
            tcbs->next_column = MEMO_CELL_C;
#else /* STAY_IN_THE_MEMO_CELL */
            /* hack alert --  Hmm, this seems not to work */
            /* The corect solution is probably to create a new quark
             * (call it QTab), add it to the translation resources,
             * so that it calls EditCell(Tab), and then test for 
             * QTab above, instead of QRight. Just remeber to reset
             * qparam to QRight when done.  Right now, this is more 
             * complicated than I feel like getting into.  Maybe 
             * some rainy day ... */
            tcbs->next_row    = 0;
            tcbs->next_column = 0;
            tcbs->qparam = NULLQUARK;
            XmProcessTraversal(regData->record, XmTRAVERSE_CURRENT);
#endif /* STAY_IN_THE_MEMO_CELL */
          }
        }

        regData->prev_row = tcbs->next_row;
        regData->prev_col = tcbs->next_column;
      }
      break;
    }
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

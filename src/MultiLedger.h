/*******************************************************************\
 * MultiLedger.h -- utilities for dealing with multiple             *
 * register/ledger windows in GnuCash                               *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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
\********************************************************************/

#ifndef __MULTI_LEDGER_H__
#define __MULTI_LEDGER_H__

#include "config.h"

#include "Account.h"
#include "Query.h"
#include "splitreg.h"
#include "SplitLedger.h"
#include "Transaction.h"

/* the MAX_QUERY_SPLITS define determines how many transactions should be shown
 * in the register.  Its set to a default of 30.  But this should be converted
 * into a user-configurable value.  So hack-alert on the configuration aspect.
 */
#define MAX_QUERY_SPLITS 30

/* the MAX_QUERY_SPLITS_UNCLAMP define determines cap on how many transactions 
 * should be shown in the register when uiser is browsing with dates.  Its set 
 * to a default of 1000, which should give user plenty of elbow room to browse,
 * and is still small enough to keep em out of trouble.  This should be converted
 * into a user-configurable value.  So hack-alert on the configuration aspect.
 */
#define MAX_QUERY_SPLITS_UNCLAMP 1000


/** STRUCTS *********************************************************/
/* The xaccLedgerDisplay struct describes a single register/ledger instance.
 */

typedef struct _xaccLedgerDisplay xaccLedgerDisplay;

struct _xaccLedgerDisplay {
  Account *leader;               /* leading. "master" account               */
  Account **displayed_accounts;  /* The list of accounts shown here         */
  short   numAcc;                /* number of accounts in list              */
  Query   *query;                /* query engine & filter for displaying    */

  short type;                    /* register display type, usually equal to *
                                  * account type, but not always.           */
  double balance;                /* balance */
  double clearedBalance;
  double reconciledBalance;

  /* GUI related stuff */
  short dirty;                   /* dirty flag, non zero if redraw needed   */
  SplitRegister *ledger;         /* main ledger window                      */
  void *gui_hook;                /* GUI-specific state                      */
  void (*redraw) (xaccLedgerDisplay *); /* redraw callback                  */
  void (*destroy) (xaccLedgerDisplay *); /* destroy callback                */
};


/** PROTOTYPES ******************************************************/

/*
 * opens up a register window to display a single account  
 */
extern xaccLedgerDisplay * xaccLedgerDisplaySimple (Account *acc); 

/*
 * opens up a register window to display the parent account
 * and all of its children.
 */
extern xaccLedgerDisplay * xaccLedgerDisplayAccGroup (Account *acc); 

/*
 * display list of accounts in a general ledger.
 */
extern xaccLedgerDisplay * xaccLedgerDisplayGeneral 
      (Account *lead_acc, Account **acclist, int ledger_type);

/*
 * redisplay/redraw all windows that contain any transactions
 * that are associated with the indicated account.
 */
extern void        xaccAccountDisplayRefresh (Account *acc);
extern void        xaccAccListDisplayRefresh (Account **acc);

/* 
 * redisplay/redraw all windows that contain this transaction
 * (or any of its member splits).
 */
extern void        xaccTransDisplayRefresh (Transaction *trans);

/* 
 * redisplay/redraw only the indicated window.
 * both routines do same thing, they differ only by the argument they
 * take.
 */
extern void        xaccLedgerDisplayRefresh (xaccLedgerDisplay *);
extern void        xaccRegisterRefresh (SplitRegister *);

/* 
 * close the window 
 */
extern void        xaccLedgerDisplayClose (xaccLedgerDisplay *);

/********************************************************************\
 * sort of a quick hack involving the layout of the register.
\********************************************************************/

extern void        xaccRegisterCountHack (SplitRegister *splitreg);

extern void xaccDestroyLedgerDisplay (Account *acc);

#endif /* __MULTI_LEDGER_H__ */

/************************** END OF FILE *************************/

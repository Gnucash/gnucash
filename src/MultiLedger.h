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
#include "SplitLedger.h"
#include "Transaction.h"

/** STRUCTS *********************************************************/
/* The xaccLedgerDisplay struct describes a single register/ledger instance.
 */

typedef struct _xaccLedgerDisplay xaccLedgerDisplay;

struct _xaccLedgerDisplay {
  Account *leader;               /* leading. "master" account               */
  Account **displayed_accounts;  /* The list of accounts shown here         */
  short   numAcc;                /* number of accounts in list              */

  short type;                    /* register display type, usually equal to *
                                  * account type, but not always.           */
  double balance;                /* balance */
  double clearedBalance;

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
 * (opr any of it's member splits).
 */
extern void        xaccTransDisplayRefresh (Transaction *trans);

/* 
 * redisplay/redraw only the indicated window.
 */
extern void        xaccLedgerDisplayRefresh (xaccLedgerDisplay *);

/* 
 * close the window 
 */
extern void        xaccLedgerDisplayClose (xaccLedgerDisplay *);
#endif /* __MULTI_LEDGER_H__ */

/************************** END OF FILE *************************/

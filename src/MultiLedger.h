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


/** STRUCTS *********************************************************/
/* The xaccLedgerDisplay struct describes a single register/ledger instance. */

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
  gncUIWidget (*get_parent) (xaccLedgerDisplay *); /* get parent widget     */
  void (*set_help) (xaccLedgerDisplay *, const char *); /* help string      */
};


/** PROTOTYPES ******************************************************/

/*
 * opens up a register window to display a single account  
 */
xaccLedgerDisplay * xaccLedgerDisplaySimple (Account *acc); 

/*
 * opens up a register window to display the parent account
 * and all of its children.
 */
xaccLedgerDisplay * xaccLedgerDisplayAccGroup (Account *acc); 

/*
 * display list of accounts in a general ledger.
 */
xaccLedgerDisplay * xaccLedgerDisplayGeneral (Account *lead_acc,
                                              Account **acclist,
                                              int ledger_type);

/*
 * redisplay/redraw all windows that contain any transactions
 * that are associated with the indicated account.
 */
void        xaccAccountDisplayRefresh (Account *acc);
void        xaccAccListDisplayRefresh (Account **acc);

/* 
 * redisplay/redraw all windows that contain this transaction
 * (or any of its member splits).
 */
void        xaccTransDisplayRefresh (Transaction *trans);

/* 
 * redisplay/redraw only the indicated window.
 * both routines do same thing, they differ only by the argument they
 * take.
 */
void        xaccLedgerDisplayRefresh (xaccLedgerDisplay *);
void        xaccRegisterRefresh (SplitRegister *);

/*
 * Call the user refresh callback for all registers. This does not
 * perform a full refresh, i.e., it does not reload transactions.
 * This is just for updating gui controls.
 */
void        xaccRegisterRefreshAllGUI (void);

/*
 * return true if acc is a member of the ledger.
 */
int         ledgerIsMember (xaccLedgerDisplay *reg, Account * acc);

/* 
 * close the window 
 */
void        xaccLedgerDisplayClose (xaccLedgerDisplay *);

/*
 * close all ledger windows containing this account.
 */
void        xaccDestroyLedgerDisplay (Account *acc);

#endif /* __MULTI_LEDGER_H__ */

/************************** END OF FILE *************************/

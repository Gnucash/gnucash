/********************************************************************\
 * Transaction.c -- the transaction data structure                  *
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

#include "config.h"

#include "date.h"
#include "Transaction.h"
#include "util.h"

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * Transaction data structure, in order to encapsulate the knowledge       *
 * of the internals of the Transaction in one file.                        *
\********************************************************************/

/********************************************************************\
 * initTransaction
 * Initialize a transaction structure
\********************************************************************/

void
initTransaction( Transaction * trans )
  {
  
  /* fill in some sane defaults */
  trans->debit = 0x0;
  trans->credit = 0x0;
  
  trans->num         = XtNewString("");
  trans->description = XtNewString("");
  trans->memo        = XtNewString("");
  trans->action      = XtNewString("");
  trans->catagory    = 0;
  trans->reconciled  = NREC;
  trans->damount     = 0.0;
  trans->share_price = 1.0;

  trans->credit_balance = 0.0;
  trans->credit_cleared_balance = 0.0;
  trans->debit_balance = 0.0;
  trans->debit_cleared_balance = 0.0;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        

  trans->write_flag  = 0;
  }

/********************************************************************\
\********************************************************************/
Transaction *
mallocTransaction( void )
  {
  Transaction *trans = (Transaction *)_malloc(sizeof(Transaction));
  initTransaction (trans);
  return trans;
  }

/********************************************************************\
\********************************************************************/

void
freeTransaction( Transaction *trans )
  {
  if (!trans) return;

  /* free a transaction only if it is not claimed
   * by any accounts. */
  if (trans->debit) return;
  if (trans->credit) return;

  XtFree(trans->num);
  XtFree(trans->description);
  XtFree(trans->memo);
  XtFree(trans->action);

  /* just in case someone looks up freed memory ... */
  trans->num         = 0x0;
  trans->description = 0x0;
  trans->memo        = 0x0;
  trans->action      = 0x0;
  trans->catagory    = 0;
  trans->reconciled  = NREC;
  trans->damount     = 0.0;
  trans->share_price = 1.0;

  trans->credit_balance = 0.0;
  trans->credit_cleared_balance = 0.0;
  trans->debit_balance = 0.0;
  trans->debit_cleared_balance = 0.0;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        

  trans->write_flag  = 0;
  _free(trans);
}

/********************************************************************\
 * sorting comparison function
 *
 * returns a negative value if transaction a is dated earlier than b, 
 * returns a positive value if transaction a is dated later than b, 
 * returns zero if both transactions are on the same date.
 *
\********************************************************************/

int
xaccTransOrder (Transaction **ta, Transaction **tb)
{
  int retval;
  char *da, *db;

  retval = datecmp (&((*ta)->date), &((*tb)->date));

  /* if dates differ, return */
  if (retval) return retval;

  /* otherwise, sort on transaction strings */
  da = (*ta)->description;
  db = (*tb)->description;
  if (!da) return -1;
  if (!db) return +1;

  retval = strcmp (da, db);
  return retval;
}

/********************************************************************\
\********************************************************************/

int
xaccCountTransactions (Transaction **tarray)
{
   Transaction *trans;
   int ntrans = 0;

   trans = tarray[0];
   while (trans) {
      ntrans ++;
      trans = tarray[ntrans];
   }
   return ntrans;
}

/************************ END OF ************************************\
\************************* FILE *************************************/


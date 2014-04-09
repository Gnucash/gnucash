/********************************************************************\
 * AccInfo.h -- the Account Info data structures                    *
 * Copyright (C) 1998 Linas Vepstas                                 *
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

/*
 * Most of the structures here more or less resemble 
 * matching structures in the OFX DTD's.  The match 
 * is not exact.
 */

#ifndef __ACCINFO_H__
#define __ACCINFO_H__

#include "config.h"

/*
 * The account types are used to determine how the transaction data
 * in the account is displayed.   These values can be safely changed
 * from one release to the next.  Note that if values are added,
 * the file IO translation routines need to be updated. Note 
 * also that GUI code depends on these numbers.
 */
enum 
{
  BANK = 0,
  /* The bank account type denotes a savings or checking account
   * held at a bank.  Often interest bearing.
   */

  CASH = 1,
  /* The cash account type is used to denote a shoe-box or pillowcase
   * stuffed with cash.
   */

  CREDIT = 3,  
  /* The Credit card account is used to denote credit (e.g. amex) and 
   * debit (e.g. visa, mastercard) card accounts 
   */

  ASSET = 2,
  LIABILITY = 4,
  /* asset and liability accounts indicate generic, generalized accounts
   * that are none of the above.
   */

  STOCK = 5,
  MUTUAL= 6, 
  /* Stock and Mutual Fund accounts will typically be shown in registers
   * which show three columns: price, number of shares, and value.
   */

  CURRENCY = 7, 
  /* The currency account type indicates that the account is a currency trading 
   * account.  In many ways, a currency trading account is like a stock trading
   * account, where both quantities and prices are set.
   */

  INCOME = 8,
  EXPENSE = 9,
  /* Income and expense accounts are used to denote income and expenses.
   * Thus, when data in these accountsare displayed, the sign of the
   * splits (entries) must be reversed.
   */ 

  EQUITY = 10,
  /* Equity account is used to balance the balance sheet. */

  NUM_ACCOUNT_TYPES = 11,
  /* stop here; the following types just aren't ready for prime time */

  /* bank account types */
  CHECKING = 11,
  SAVINGS = 12,
  MONEYMRKT = 13,
  CREDITLINE = 14,     /* line of credit */


};

/* hack alert -- we need a better way of dealing with
 * account names!
 */
extern char *account_type_name [NUM_ACCOUNT_TYPES];

struct _BankAcct 
{
  char * bankid;       /* routing and transit number */
  char * branchid;     /* branch office bank identifier */  
  char * acctid;       /* account number */
  char * accttype;     /* account type */
  char * acctkey;      /* checksum key */
  int acctype;         /* account type.  Must be one of 
                        * CHECKING = 10;
                        * SAVINGS = 11;
                        * MONEYMRKT = 12;
                        * CREDITLINE = 13;
                        */

};


#endif /* __ACCINFO_H__ */

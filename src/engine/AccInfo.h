/********************************************************************\
 * AccInfo.h -- the Account Info data structures                    *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * Most of the structures here more or less resemble 
 * matching structures in the OFX DTD's.  The match 
 * is not exact.
 */

#ifndef __ACCINFO_H__
#define __ACCINFO_H__

#include "config.h"
#include "gnc-common.h"


/*
 * The account types are used to determine how the transaction data
 * in the account is displayed.   These values can be safely changed
 * from one release to the next.  Note that if values are added,
 * the file IO translation routines need to be updated. Note 
 * also that GUI code depends on these numbers.
 *
 * If you do change the enumeration names (not the numbers), you need
 * to update xaccAccountTypeEnumAsString --- used for text file exports
 */

typedef enum 
{
  BAD_TYPE = -1,
  NO_TYPE = -1,
  /* Not a type */

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
  /* The currency account type indicates that the account is a
   * currency trading account.  In many ways, a currency trading
   * account is like a stock trading account, where both quantities
   * and prices are set.
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
} GNCAccountType;

char * xaccAccountGetTypeStr (int type); /* GUI names */

/* Just the name of the enum as a string.  i.e. INCOME -> "INCOME".
   Used for text exports */
char * xaccAccountTypeEnumAsString (int type); 

gncBoolean xaccAccountTypesCompatible (int parent_type, int child_type);


typedef struct _BankAcct BankAcct;
typedef struct _InvAcct  InvAcct;
typedef union _AccInfo AccInfo;


/* The AccInfo structure is just a union of the other account
 *   auxilliary info types.  The xaccCastToXXX() functions simply
 *   provide a safe upcast mechanism (similar to that in C++ ...
 *   returns the address if the cast is safe, otherwise returns NULL).
 */
AccInfo * xaccMallocAccInfo (int typo);
void      xaccFreeAccInfo (AccInfo *u);
InvAcct * xaccCastToInvAcct (AccInfo *);


InvAcct * xaccMallocInvAcct (void);
void      xaccInitInvAcct (InvAcct *iacc);
void      xaccFreeInvAcct (InvAcct *iacc);

/*
 * The xaccInvAcctSetPriceSrc() and xaccInvAcctGetPriceSrc()
 *   routines are used to get and set a string that identifies the current
 *   source for investment pricing info.
 *   Currently supported values include "yahoo", "fidelity", "troweprice", etc.
 */
void      xaccInvAcctSetPriceSrc (InvAcct *iacc, const char *src);
char    * xaccInvAcctGetPriceSrc (InvAcct *iacc);

#endif /* __ACCINFO_H__ */

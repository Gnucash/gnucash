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

/* The account types. 
 * Note: the actual values of these are *very* important, 
 * as it is the values, not the enums, that are stored 
 * in the file format! 
 */
enum 
{
  BANK = 0,
  CASH = 1,
  ASSET = 2,
  CREDIT = 3,          /* credit card */
  LIABILITY = 4,
  STOCK = 5,
  MUTUAL= 6, 
  INCOME = 7,
  EXPENSE = 8,
  EQUITY = 9,

  /* bank account types */
  CHECKING = 10,
  SAVINGS = 11,
  MONEYMRKT = 12,
  CREDITLINE = 13,     /* line of credit */

  NUM_ACCINFO_TYPES = 14
};

struct _BankAcct 
{
  char * bankid;       /* routing and transit number */
  char * branchid;     /* bank identifier for international banks */  
  char * acctid;       /* account number */
  char * accttype;     /* account type */
  char * acctkey;      /* checksum for international banks */
  int acctype;         /* account type.  Must be one of 
                        * CHECKING = 10;
                        * SAVINGS = 11;
                        * MONEYMRKT = 12;
                        * CREDITLINE = 13;
                        */

};


#endif /* __ACCINFO_H__ */

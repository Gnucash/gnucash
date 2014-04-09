/********************************************************************\
 * AccInfoP.h -- the Account Info data structures                   *
 * Copyright (C) 1998, 1999 Linas Vepstas                           *
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
\********************************************************************/

/*
 * Most of the structures here more or less resemble 
 * matching structures in the OFX DTD's.  The match 
 * is not exact.
 */

#ifndef __XACC_ACCINFO_P_H__
#define __XACC_ACCINFO_P_H__

#include "config.h"
#include "AccInfo.h"

/*
 * Most of the structures here more or less resemble 
 * matching structures in the OFX DTD's.  The match 
 * is not exact.
 */

/* The BankAcct structure only applies when the account type is one of 
 * CHECKING = 10;
 * SAVINGS = 11;
 * MONEYMRKT = 12;
 * CREDITLINE = 13;
 */
struct _BankAcct 
{
  short type;          /* must match Account::type */
  char * bankid;       /* routing and transit number */
  char * branchid;     /* branch office bank identifier */  
  char * acctid;       /* account number */
  char * accttype;     /* account type */
  char * acctkey;      /* checksum key */

};

/* The InvAcct structure only applies when the account type 
 * is one of
 * MUTUAL STOCK
 */
struct _InvAcct 
{
  short type;          /* must match Account::type */
  char * pricesrc;     /* source for price quotes ...
                        * one of Yahoo, Fidelity, TRowePrice, etc. 
                        */
  char * brokerid;     /* unique identifier for the FI */
  char * acctid;       /* account number */
  char * accttype;     /* account type (OFX INVACCTYPE) */
                       /* possible values: INDIVIDUAL, JOINT 
                          TRUST, CORPORATE */
  char * prodtype;     /* account type (OFX USPRODUCTTYPE) */
                       /* possible values: 401K 403B IRA KEOGH SARSEP 
                          SIMPLE NORMAL TDA TRUST UGMA */
  char * secid;        /* security id (CUSIP) (OFX UNIQUEID) */
                       /* (9 digit alphanumeric) */
  char * secidtype;    /* "CUSIP" (OFX UNIQUEIDTYPE) */
};

union _AccInfo {
  short type;          /* must match Account::type */
  BankAcct bank_acct;
  InvAcct  inv_acct;
};

#endif /* __XACC_ACCINFO_P_H__ */

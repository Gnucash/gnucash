/********************************************************************\
 * AccInfo.c -- the Account Info data structures                    *
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

#include <stdlib.h>
#include <string.h>

#include "AccInfo.h"
#include "AccInfoP.h"
#include "messages.h"

/* =========================================================== */

char *account_type_name[NUM_ACCOUNT_TYPES] =
   { 
	BANK_STR, 
	CASH_STR, 
	ASSET_STR, 
	CREDIT_CARD_STR,
	LIABILITY_STR, 
	STOCK_STR, 
	MUTUAL_FUND_STR,
	CURRENCY_STR,
	INCOME_STR, 
	EXPENSE_STR, 
	EQUITY_STR,
/*
	CHECKING_STR,
	SAVINGS_STR,
	MONEYMRKT_STR,
	CREDITLINE_STR
*/
   };

char * xaccAccountGetTypeStr (int type)
{
   if (0 > type) return "";
   if (NUM_ACCOUNT_TYPES <= type) return "";
   return (account_type_name [type]);
}

/* =========================================================== */

InvAcct *
xaccMallocInvAcct (void) 
{
   InvAcct *iacc;
   iacc = (InvAcct *) malloc (sizeof (InvAcct));
   xaccInitInvAcct (iacc);
   return iacc;
}

void 
xaccInitInvAcct (InvAcct *iacc)
{
   if (!iacc) return;
   iacc->pricesrc = NULL;
   iacc->brokerid = NULL;
   iacc->acctid = NULL;
   iacc->accttype = NULL;
   iacc->prodtype = NULL; 
   iacc->secid = NULL;
   iacc->secidtype = strdup ("CUSIP");
}

void 
xaccFreeInvAcct (InvAcct *iacc)
{
   if (!iacc) return;
   if (iacc->pricesrc) { free(iacc->pricesrc); iacc->pricesrc = NULL; }
   if (iacc->brokerid) { free(iacc->brokerid); iacc->brokerid = NULL; }
   if (iacc->acctid) { free(iacc->acctid); iacc->acctid = NULL; }
   if (iacc->accttype) { free(iacc->accttype); iacc->accttype = NULL; }
   if (iacc->prodtype) { free(iacc->prodtype); iacc->prodtype = NULL; }
   if (iacc->secid) { free(iacc->secid); iacc->secid = NULL; }
   if (iacc->secidtype) { free(iacc->secidtype); iacc->secidtype = NULL; }
}

/* =========================================================== */

void 
xaccInvAcctSetPriceSrc (InvAcct *iacc, const char *src)
{
   if (!iacc || !src) return;
   if (iacc->pricesrc) { free(iacc->pricesrc); }
   iacc->pricesrc = strdup (src);
}

char * 
xaccInvAcctGetPriceSrc (InvAcct *iacc)
{
   if (!iacc) return NULL;
   return (iacc->pricesrc);
}

/* ==================== END OF FILE ========================== */

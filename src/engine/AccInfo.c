/********************************************************************\
 * AccInfo.c -- the Account Info data structures                    *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
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

#include <stdlib.h>
#include <string.h>

#include "AccInfo.h"
#include "AccInfoP.h"
#define DISABLE_GETTEXT_UNDERSCORE /* required to include messages.h */
#include "messages.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;


/* =========================================================== */

#define GNC_RETURN_ENUM_AS_STRING(x) case x: return #x;

char *
xaccAccountTypeEnumAsString(int type) {
  switch(type) {
    GNC_RETURN_ENUM_AS_STRING(BANK);
    GNC_RETURN_ENUM_AS_STRING(CASH);
    GNC_RETURN_ENUM_AS_STRING(CREDIT);
    GNC_RETURN_ENUM_AS_STRING(ASSET);
    GNC_RETURN_ENUM_AS_STRING(LIABILITY);
    GNC_RETURN_ENUM_AS_STRING(STOCK);
    GNC_RETURN_ENUM_AS_STRING(MUTUAL);
    GNC_RETURN_ENUM_AS_STRING(CURRENCY);
    GNC_RETURN_ENUM_AS_STRING(INCOME);
    GNC_RETURN_ENUM_AS_STRING(EXPENSE);
    GNC_RETURN_ENUM_AS_STRING(EQUITY);
    GNC_RETURN_ENUM_AS_STRING(CHECKING);
    GNC_RETURN_ENUM_AS_STRING(SAVINGS);
    GNC_RETURN_ENUM_AS_STRING(MONEYMRKT);
    GNC_RETURN_ENUM_AS_STRING(CREDITLINE);
    default:
      PERR ("asked to translate unknown account type %d.\n", type);
      break;
  };
  return(NULL);
};

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
   return gettext (account_type_name [type]);
}

/* =========================================================== */
gncBoolean
xaccAccountTypesCompatible (int parent_type, int child_type)
{
  gncBoolean compatible = GNC_F;

  switch(parent_type)
  {
    case BANK:
    case CASH: 
    case ASSET:
    case STOCK:
    case MUTUAL:
    case CURRENCY:
    case CREDIT:
    case LIABILITY:
      compatible = ((child_type == BANK)     ||
		    (child_type == CASH)     ||
		    (child_type == ASSET)    ||
		    (child_type == STOCK)    ||
		    (child_type == MUTUAL)   ||
		    (child_type == CURRENCY) ||
                    (child_type == CREDIT)   ||
                    (child_type == LIABILITY));
      break;
    case INCOME:
    case EXPENSE:
      compatible = ((child_type == INCOME) ||
                    (child_type == EXPENSE));
      break;
    case EQUITY:
      compatible = (child_type == EQUITY);
      break;
    default:
      PERR("bad account type: %d", parent_type);
      break;
  }

  return compatible;
}

/* =========================================================== */

AccInfo *
xaccMallocAccInfo (int typo)
{
  AccInfo *u = NULL;
  if ((STOCK  == typo) || (MUTUAL == typo)) {
    u = (AccInfo *) xaccMallocInvAcct ();
    u->inv_acct.type = typo;
  }
  return u;
}

void
xaccFreeAccInfo (AccInfo *u)
{
  if (!u) return;
  if ((STOCK  == u->type) || (MUTUAL == u->type)) {
    xaccFreeInvAcct ( &(u->inv_acct));
  }
}

InvAcct *
xaccCastToInvAcct (AccInfo *u)
{
  if (!u) return NULL;
  if ((STOCK  == u->type) || (MUTUAL == u->type)) {
    return ( &(u->inv_acct));
  }
  return NULL;
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
   iacc->type = STOCK;
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

   /* if the wrong type then a miscast. can't free. */
   assert ((STOCK  == iacc->type) || (MUTUAL == iacc->type));

   if (iacc->pricesrc) { free(iacc->pricesrc); iacc->pricesrc = NULL; }
   if (iacc->brokerid) { free(iacc->brokerid); iacc->brokerid = NULL; }
   if (iacc->acctid) { free(iacc->acctid); iacc->acctid = NULL; }
   if (iacc->accttype) { free(iacc->accttype); iacc->accttype = NULL; }
   if (iacc->prodtype) { free(iacc->prodtype); iacc->prodtype = NULL; }
   if (iacc->secid) { free(iacc->secid); iacc->secid = NULL; }
   if (iacc->secidtype) { free(iacc->secidtype); iacc->secidtype = NULL; }
   iacc->type = -1;
}

/* =========================================================== */

void 
xaccInvAcctSetPriceSrc (InvAcct *iacc, const char *src)
{
   if (!iacc) return;
   if (iacc->pricesrc) { free(iacc->pricesrc); }
   if (src)
     iacc->pricesrc = strdup (src);
   else
     iacc->pricesrc = NULL;
}

char * 
xaccInvAcctGetPriceSrc (InvAcct *iacc)
{
   if (!iacc) return NULL;
   return (iacc->pricesrc);
}

/* ==================== END OF FILE ========================== */

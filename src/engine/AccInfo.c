
#include "AccInfo.h"
#include "messages.h"
/* whoa! */

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

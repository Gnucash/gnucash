/*******************************************************************\
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
     @file gnc-ofx-import.c
     @brief Ofx import module code
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <libguile.h>

#include <libofx/libofx.h>
#include "import-account-matcher.h"
#include "import-commodity-matcher.h"
#include "import-utilities.h"
#include "import-main-matcher.h"

#include "Account.h"
#include "Transaction.h"
#include "gnc-associate-account.h"
#include "gnc-ofx-import.h"
#include "gnc-file.h"
#include "gnc-engine.h"
#include "gnc-book.h"
#include "gnc-ui-util.h"
#include "gnc-glib-utils.h"

#define GCONF_SECTION "dialogs/import/ofx"

static QofLogModule log_module = GNC_MOD_IMPORT;

/********************************************************************\
 * gnc_file_ofx_import
 * Entry point
\********************************************************************/

/* CS: Store the reference to the created importer gui so that the
   ofx_proc_transaction_cb can use it. */
GNCImportMainMatcher *gnc_ofx_importer_gui = NULL;

/*
int ofx_proc_status_cb(struct OfxStatusData data)
{
  return 0;
}
*/

int ofx_proc_security_cb(const struct OfxSecurityData data, void * security_user_data);
int ofx_proc_transaction_cb(struct OfxTransactionData data, void * transaction_user_data);
int ofx_proc_account_cb(struct OfxAccountData data, void * account_user_data);

int ofx_proc_security_cb(const struct OfxSecurityData data, void * security_user_data)
{
  char * tmp_cusip=NULL;
  char * tmp_default_fullname=NULL;
  char * tmp_default_mnemonic=NULL;
 
  if(data.unique_id_valid==true)
    {
      tmp_cusip=(char *)data.unique_id;
    }
  if(data.secname_valid==true)
    {
      tmp_default_fullname=(char *)data.secname;
    }
  if(data.ticker_valid==true)
    {
      tmp_default_mnemonic=(char *)data.ticker;
    }
  
  gnc_import_select_commodity(tmp_cusip,
	        	      true,
			      tmp_default_fullname,
			      tmp_default_mnemonic);
  return 0;
}

int ofx_proc_transaction_cb(struct OfxTransactionData data, void * transaction_user_data)
{
  char dest_string[255];
  time_t current_time; 
  Account *account;
  Account *investment_account=NULL;
  Account *income_account=NULL;
  kvp_frame * acc_frame;
  kvp_value * kvp_val;
  const GUID * income_acc_guid;
  gchar *investment_account_text;
  gnc_commodity *currency=NULL;
  gnc_commodity *investment_commodity=NULL;
  gnc_numeric gnc_amount, gnc_units;
  GNCBook *book;
  Transaction *transaction;
  Split *split;
  gchar *notes, *tmp;
  
  g_assert(gnc_ofx_importer_gui);

  if(data.account_id_valid==true){
    account = gnc_import_select_account(NULL, data.account_id, 0, NULL, NULL,
					ACCT_TYPE_NONE, NULL, NULL);
    if(account!=NULL)
      {
	/********** Validate the input strings to ensure utf8 ********************/
	if (data.name_valid)
	  gnc_utf8_strip_invalid(data.name);
	if (data.memo_valid)
	  gnc_utf8_strip_invalid(data.memo);
	if (data.check_number_valid)
	  gnc_utf8_strip_invalid(data.check_number);
	if (data.reference_number_valid)
	  gnc_utf8_strip_invalid(data.reference_number);

	/********** Create the transaction and setup transaction data ************/
	book = gnc_account_get_book(account);
	transaction = xaccMallocTransaction(book);
	xaccTransBeginEdit(transaction);
	
	if(data.fi_id_valid==true){
	  gnc_import_set_trans_online_id(transaction, data.fi_id);
	}
	
	if(data.date_initiated_valid==true){
	  xaccTransSetDateSecs(transaction, data.date_initiated);
	}
	else if(data.date_posted_valid==true){
	  xaccTransSetDateSecs(transaction, data.date_posted);
	}
	
	if(data.date_posted_valid==true){
	  xaccTransSetDatePostedSecs(transaction, data.date_posted);
	}
	
	current_time = time(NULL);
	xaccTransSetDateEnteredSecs(transaction, mktime(localtime(&current_time)));
	
	if(data.check_number_valid==true){
	  xaccTransSetNum(transaction, data.check_number);
	}
	else if(data.reference_number_valid==true){
	  xaccTransSetNum(transaction, data.reference_number);
	}
	/* Put transaction name in Description, or memo if name unavailable */ 
	if(data.name_valid==true){
	  xaccTransSetDescription(transaction, data.name);
	}
	else if(data.memo_valid==true){
	  xaccTransSetDescription(transaction, data.memo);
	}
	
	/* Put everything else in the Notes field */
	notes=g_strdup_printf("OFX ext. info: ");
	
	if(data.transactiontype_valid==true){
	  switch(data.transactiontype){
	  case OFX_CREDIT: strncpy(dest_string, "Generic credit", sizeof(dest_string));
	    break;
	  case OFX_DEBIT: strncpy(dest_string, "Generic debit", sizeof(dest_string));
	    break;
	  case OFX_INT: strncpy(dest_string, "Interest earned or paid (Note: Depends on signage of amount)", sizeof(dest_string));
	    break;
	  case OFX_DIV: strncpy(dest_string, "Dividend", sizeof(dest_string));
	    break;
	  case OFX_FEE: strncpy(dest_string, "FI fee", sizeof(dest_string));
	    break;
	  case OFX_SRVCHG: strncpy(dest_string, "Service charge", sizeof(dest_string));
	    break;
	  case OFX_DEP: strncpy(dest_string, "Deposit", sizeof(dest_string));
	    break;
	  case OFX_ATM: strncpy(dest_string, "ATM debit or credit (Note: Depends on signage of amount)", sizeof(dest_string));
	    break;
	  case OFX_POS: strncpy(dest_string, "Point of sale debit or credit (Note: Depends on signage of amount)", sizeof(dest_string));
	    break;
	  case OFX_XFER: strncpy(dest_string, "Transfer", sizeof(dest_string));
	    break;
	  case OFX_CHECK: strncpy(dest_string, "Check", sizeof(dest_string));
	    break;
	  case OFX_PAYMENT: strncpy(dest_string, "Electronic payment", sizeof(dest_string));
	    break;
	  case OFX_CASH: strncpy(dest_string, "Cash withdrawal", sizeof(dest_string));
	    break;
	  case OFX_DIRECTDEP: strncpy(dest_string, "Direct deposit", sizeof(dest_string));
	    break;
	  case OFX_DIRECTDEBIT: strncpy(dest_string, "Merchant initiated debit", sizeof(dest_string));
	    break;
	  case OFX_REPEATPMT: strncpy(dest_string, "Repeating payment/standing order", sizeof(dest_string));
	    break;
	  case OFX_OTHER: strncpy(dest_string, "Other", sizeof(dest_string));
	    break;
	  default : strncpy(dest_string, "Unknown transaction type", sizeof(dest_string));
	    break;
	  }
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp,"|Trans type:", dest_string);
	  g_free(tmp);
	}
	
	if(data.invtransactiontype_valid==true){
	  switch(data.invtransactiontype){
	  case OFX_BUYDEBT: strncpy(dest_string, "BUYDEBT (Buy debt security)", sizeof(dest_string));
	    break;
	  case OFX_BUYMF: strncpy(dest_string, "BUYMF (Buy mutual fund)", sizeof(dest_string));
	    break;
	  case OFX_BUYOPT: strncpy(dest_string, "BUYOPT (Buy option)", sizeof(dest_string));
	    break;
	  case OFX_BUYOTHER: strncpy(dest_string, "BUYOTHER (Buy other security type)", sizeof(dest_string));
	    break;
	  case OFX_BUYSTOCK: strncpy(dest_string, "BUYSTOCK (Buy stock))", sizeof(dest_string));
	    break;
	  case OFX_CLOSUREOPT: strncpy(dest_string, "CLOSUREOPT (Close a position for an option)", sizeof(dest_string));
	    break;
	  case OFX_INCOME: strncpy(dest_string, "INCOME (Investment income is realized as cash into the investment account)", sizeof(dest_string));
	    break;
	  case OFX_INVEXPENSE: strncpy(dest_string, "INVEXPENSE (Misc investment expense that is associated with a specific security)", sizeof(dest_string));
	    break;
	  case OFX_JRNLFUND: strncpy(dest_string, "JRNLFUND (Journaling cash holdings between subaccounts within the same investment account)", sizeof(dest_string));
	    break;
	  case OFX_MARGININTEREST: strncpy(dest_string, "MARGININTEREST (Margin interest expense)", sizeof(dest_string));
	    break;
	  case OFX_REINVEST: strncpy(dest_string, "REINVEST (Reinvestment of income)", sizeof(dest_string));
	    break;
	  case OFX_RETOFCAP: strncpy(dest_string, "RETOFCAP (Return of capital)", sizeof(dest_string));
	    break;
	  case OFX_SELLDEBT: strncpy(dest_string, "SELLDEBT (Sell debt security.  Used when debt is sold, called, or reached maturity)", sizeof(dest_string));
	    break;
	  case OFX_SELLMF: strncpy(dest_string, "SELLMF (Sell mutual fund)", sizeof(dest_string));
	    break;
	  case OFX_SELLOPT: strncpy(dest_string, "SELLOPT (Sell option)", sizeof(dest_string));
	    break;
	  case OFX_SELLOTHER: strncpy(dest_string, "SELLOTHER (Sell other type of security)", sizeof(dest_string));
	    break;
	  case OFX_SELLSTOCK: strncpy(dest_string, "SELLSTOCK (Sell stock)", sizeof(dest_string));
	    break;
	  case OFX_SPLIT: strncpy(dest_string, "SPLIT (Stock or mutial fund split)", sizeof(dest_string));
	    break;
	  case OFX_TRANSFER: strncpy(dest_string, "TRANSFER (Transfer holdings in and out of the investment account)", sizeof(dest_string));
	    break;
	  default: strncpy(dest_string, "ERROR, this investment transaction type is unknown.  This is a bug in ofxdump", sizeof(dest_string));
	    break;
	  }
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp,"|Investment Trans type:", dest_string);
	  g_free(tmp);
	}
	if(data.memo_valid==true&&data.name_valid==true){/* Copy only if memo wasn't put in Description */
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp, "|Memo:", data.memo);
	  g_free(tmp);
	}
	if(data.date_funds_available_valid==true){
	  Timespec ts;
	  timespecFromTime_t(&ts, data.date_funds_available);
	  gnc_timespec_to_iso8601_buff (ts, dest_string);
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp,"|Date funds available:", dest_string);
	  g_free(tmp);
	}
	if(data.server_transaction_id_valid==true){
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp, "|Server trans ID (conf. number):", data.server_transaction_id);
	  g_free(tmp);
	}
	if(data.standard_industrial_code_valid==true){
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%ld",tmp, "|Standard Industrial Code:",data.standard_industrial_code);
	  g_free(tmp);
	  
	}
	if(data.payee_id_valid==true){
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s",tmp,"|Payee ID:", data.payee_id);
	  g_free(tmp);
	}
	
	PERR("WRITEME: GnuCash ofx_proc_transaction():Add PAYEE and ADRESS here once supported by libofx!\n");
	
	/* Ideally, gnucash should process the corrected transactions */
	if(data.fi_id_corrected_valid==true){
	  PERR("WRITEME: GnuCash ofx_proc_transaction(): WARNING: This transaction corrected a previous transaction, but we created a new one instead!\n");
	  tmp=notes;
	  notes=g_strdup_printf("%s%s%s%s",tmp,"|This corrects transaction #",data.fi_id_corrected,"but GnuCash didn't process the correction!");
	  g_free(tmp);
	}
	xaccTransSetNotes(transaction, notes);
	g_free(notes);
	if ( data.account_ptr && data.account_ptr->currency_valid )
	  {
	    DEBUG("Currency from libofx: %s",data.account_ptr->currency);
	    currency = gnc_commodity_table_lookup( gnc_get_current_commodities (),
						   GNC_COMMODITY_NS_CURRENCY,
						   data.account_ptr->currency);
	  }
	else
	  {
	    DEBUG("Currency from libofx unavailable, defaulting to account's default");
	    currency = xaccAccountGetCommodity(account);
	  }

	xaccTransSetCurrency(transaction,currency);
	if(data.amount_valid==true)
	  {
	    if(data.invtransactiontype_valid==false)
	      {
		/*************Process a normal transaction ***************************/ 
		DEBUG("Adding split; Ordinary banking transaction, money flows from or into the source account"); 
		split=xaccMallocSplit(book);
		xaccTransAppendSplit(transaction,split);
		xaccAccountInsertSplit(account,split);

		gnc_amount = double_to_gnc_numeric (data.amount,
						    gnc_commodity_get_fraction(xaccTransGetCurrency(transaction)),
						    GNC_RND_ROUND);
		xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));
		
		/* Also put the ofx transaction's memo in the split's memo field */ 
		if(data.memo_valid==true){
		  xaccSplitSetMemo(split, data.memo);
		}
	      }
	    else if(data.unique_id_valid == true
		    && data.security_data_valid
		    && data.security_data_ptr != NULL
		    && data.security_data_ptr->secname_valid == true)
	      {
		/************************ Process an investment transaction ******************************/
		/* Note that the ACCT_TYPE_STOCK account type should be replaced with something
		   derived from data.invtranstype*/
		investment_commodity = gnc_import_select_commodity(data.unique_id,
								   0,
								   NULL,
								   NULL);
		if(investment_commodity!=NULL)
		  {
		    investment_account_text = g_strdup_printf( /* This string is a default account
								  name. It MUST NOT contain the
								  character ':' anywhere in it or
								  in any translations.  */
							      _("Stock account for security \"%s\""),
							      data.security_data_ptr->secname);
		    investment_account = gnc_import_select_account(NULL,
								   data.unique_id,
								   1,
								   investment_account_text, 
								   investment_commodity,
								   ACCT_TYPE_STOCK,
								   NULL,
								   NULL);
		    g_free (investment_account_text);
		    investment_account_text = NULL;
		    if(investment_account!=NULL&&
		       data.unitprice_valid==true&&
		       data.units_valid==true&&
		       ( data.invtransactiontype!=OFX_INCOME ) )
		      {
			DEBUG("Adding investment split; Money flows from or into the stock account"); 
			split=xaccMallocSplit(book);
			xaccTransAppendSplit(transaction,split);
			xaccAccountInsertSplit(investment_account,split);

			gnc_amount = double_to_gnc_numeric (-(data.amount),
							    gnc_commodity_get_fraction(investment_commodity),
							    GNC_RND_ROUND);
			gnc_units = double_to_gnc_numeric (data.units,
							   gnc_commodity_get_fraction(investment_commodity),
							   GNC_RND_ROUND);
			xaccSplitSetAmount(split, gnc_units);
			xaccSplitSetValue(split, gnc_amount);
			
			if(data.security_data_ptr->memo_valid==true)
			  {
			    xaccSplitSetMemo(split, data.security_data_ptr->memo);
			  }
		      }
		    else
		      {
			PERR("The investment account, units or unitprice was not found for the investment transaction");
		      }
		  }
		else
		  {
		    PERR("Commodity not found for the investment transaction");
		  }
		
		if(data.invtransactiontype_valid==true)
		  {
		    if(data.invtransactiontype==OFX_REINVEST||data.invtransactiontype==OFX_INCOME)
		      {
			DEBUG("Now let's find an account for the destination split");
			
			acc_frame=xaccAccountGetSlots(investment_account);
			kvp_val = kvp_frame_get_slot(acc_frame, 
						     "ofx/associated-income-account");
			if (kvp_val != NULL)
			  {
			    income_account = xaccAccountLookup(kvp_value_get_guid(kvp_val),book); 
			  }
			if(income_account==NULL)
			  {
			    DEBUG("Couldn't find an associated income account");
			    investment_account_text = g_strdup_printf( /* This string is a default account
									  name. It MUST NOT contain the
									  character ':' anywhere in it or
									  in any translations.  */
								      _("Income account for security \"%s\""),
								      data.security_data_ptr->secname);
			    income_account=gnc_import_select_account(NULL,
								     NULL,
								     1,
								     investment_account_text, 
								     currency,
								     ACCT_TYPE_INCOME,
								     NULL,
								     NULL);
			    income_acc_guid = xaccAccountGetGUID(income_account);
			    kvp_val = kvp_value_new_guid(income_acc_guid);
			    if( acc_frame==NULL)
			      {
				DEBUG("The kvp_frame was NULL, allocating new one");
				acc_frame = kvp_frame_new();
			      }
			    kvp_frame_set_slot_nc(acc_frame, "ofx/associated-income-account",
						  kvp_val);
			    DEBUG("KVP written");
			    
			  }
			else
			  {
			    DEBUG("Found at least one associated income account");
			  }
		      }
		    if(income_account!=NULL&&
		       data.invtransactiontype==OFX_REINVEST)
		      {
			DEBUG("Adding investment split; Money flows from the income account"); 
			split=xaccMallocSplit(book);
			xaccTransAppendSplit(transaction,split);
			xaccAccountInsertSplit(income_account,split);

			gnc_amount = double_to_gnc_numeric (data.amount,
							    gnc_commodity_get_fraction(xaccTransGetCurrency(transaction)),
							    GNC_RND_ROUND);
			xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));
		    
			/* Also put the ofx transaction name in the splits memo field, or ofx memo if name is unavailable */ 
			if(data.name_valid==true){
			  xaccSplitSetMemo(split, data.name);
			}
			else if(data.memo_valid==true){
			  xaccSplitSetMemo(split, data.memo);
			}    
		      }
		    if(income_account!=NULL&&
			data.invtransactiontype==OFX_INCOME)
		      {
			DEBUG("Adding investment split; Money flows from the income account"); 
			split=xaccMallocSplit(book);
			xaccTransAppendSplit(transaction,split);
			xaccAccountInsertSplit(income_account,split);

			gnc_amount = double_to_gnc_numeric (-data.amount,/*OFX_INCOME amounts come in as positive numbers*/
							    gnc_commodity_get_fraction(xaccTransGetCurrency(transaction)),
							    GNC_RND_ROUND);
			xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));
		    
			/* Also put the ofx transaction name in the splits memo field, or ofx memo if name is unavailable */ 
			if(data.name_valid==true){
			  xaccSplitSetMemo(split, data.name);
			}
			else if(data.memo_valid==true){
			  xaccSplitSetMemo(split, data.memo);
			}    
		      }
		

		    if(data.invtransactiontype!=OFX_REINVEST)
		      {
			DEBUG("Adding investment split; Money flows from or to the cash account");
			split=xaccMallocSplit(book);
			xaccTransAppendSplit(transaction,split);
			xaccAccountInsertSplit(account,split);

			gnc_amount = double_to_gnc_numeric (data.amount,
							    gnc_commodity_get_fraction(xaccTransGetCurrency(transaction)),
							    GNC_RND_ROUND);
			xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));
		    
			/* Also put the ofx transaction name in the splits memo field, or ofx memo if name is unavailable */ 
			if(data.name_valid==true){
			  xaccSplitSetMemo(split, data.name);
			}
			else if(data.memo_valid==true){
			  xaccSplitSetMemo(split, data.memo);
			}
		    
		      }
		  }
	      }

	    /* Use new importer GUI. */
	    DEBUG("%d splits sent to the importer gui",xaccTransCountSplits(transaction));
	    gnc_gen_trans_list_add_trans (gnc_ofx_importer_gui, transaction);
	  }
	else
	  {
	    PERR("The transaction doesn't have a valid amount");
	    xaccTransDestroy(transaction);
	    xaccTransCommitEdit(transaction);
	  }
	
      }
    else
      {
	PERR("Unable to find the account!");
      }
  }
  else
    {
      PERR("account ID for this transaction is unavailable!");
    }
  return 0;
}//end ofx_proc_transaction()

/*
int ofx_proc_statement_cb(struct OfxStatementData data, void * statement_user_data)
{
  return 0;
}//end ofx_proc_statement()
*/

int ofx_proc_account_cb(struct OfxAccountData data, void * account_user_data)
{
  Account *selected_account;
  gnc_commodity_table * commodity_table;
  gnc_commodity * default_commodity;
  GNCAccountType default_type=ACCT_TYPE_NONE;
  gchar * account_description;
  gchar * account_type_name = NULL;

  if(data.account_id_valid==true){
    //printf("ofx_proc_account() Now calling gnc_import_select_account()\n");
    printf("WRITEME:  ofx_proc_account() Fill in the account type, default name, currency, etc.  \n"); 
    commodity_table = gnc_get_current_commodities ();
    if( data.currency_valid == true)
      {
	DEBUG("Currency from libofx: %s",data.currency);
	default_commodity = gnc_commodity_table_lookup(commodity_table,
						       GNC_COMMODITY_NS_CURRENCY,
						       data.currency);
      }
    else
      {
	default_commodity = NULL;
      }
    
    if(data.account_type_valid==true){
      switch(data.account_type){
      case OFX_CHECKING : 
	default_type=ACCT_TYPE_BANK;
	account_type_name = g_strdup_printf(_("Unknown OFX checking account"));
	break;
      case OFX_SAVINGS : 
	default_type=ACCT_TYPE_BANK;
	account_type_name = g_strdup_printf(_("Unknown OFX savings account"));
	break;
      case OFX_MONEYMRKT : 
	default_type=ACCT_TYPE_MONEYMRKT;
	account_type_name = g_strdup_printf(_("Unknown OFX money market account"));
	break;
      case OFX_CREDITLINE : 
	default_type=ACCT_TYPE_CREDITLINE;
	account_type_name = g_strdup_printf(_("Unknown OFX credit line account"));
	break;
      case OFX_CMA : 
	default_type=ACCT_TYPE_NONE;
	account_type_name = g_strdup_printf(_("Unknown OFX CMA account"));
	break;
      case OFX_CREDITCARD : 
	default_type=ACCT_TYPE_CREDIT;
	account_type_name = g_strdup_printf(_("Unknown OFX credit card account"));
	break;
      case OFX_INVESTMENT :
	default_type=ACCT_TYPE_BANK;
	account_type_name = g_strdup_printf(_("Unknown OFX investment account"));
	break;
      default: PERR("WRITEME: ofx_proc_account() This is an unknown account type!");
      }
    }

    gnc_utf8_strip_invalid(data.account_name);
    account_description = g_strdup_printf( /* This string is a default account
					      name. It MUST NOT contain the
					      character ':' anywhere in it or
					      in any translation.  */
					  "%s \"%s\"",
					  account_type_name,
					  data.account_name);
    selected_account = gnc_import_select_account(NULL,
						 data.account_id, 1,
						 account_description, default_commodity,
						 default_type, NULL, NULL);
    g_free(account_description);
    g_free(account_type_name);
  }
  else
    {
      PERR("account online ID not available");
    }

  return 0;
}

void gnc_file_ofx_import (void)
{
  extern int ofx_PARSER_msg;
  extern int ofx_DEBUG_msg;
  extern int ofx_WARNING_msg;
  extern int ofx_ERROR_msg;
  extern int ofx_INFO_msg;
  extern int ofx_STATUS_msg;
  char *selected_filename;
  char *default_dir;
  LibofxContextPtr libofx_context = libofx_get_new_context();

  ofx_PARSER_msg = false;
  ofx_DEBUG_msg = false;
  ofx_WARNING_msg = true;
  ofx_ERROR_msg = true;
  ofx_INFO_msg = true;
  ofx_STATUS_msg = false;

  DEBUG("gnc_file_ofx_import(): Begin...\n");

  default_dir = gnc_get_default_directory(GCONF_SECTION);
  selected_filename = gnc_file_dialog(_("Select an OFX/QFX file to process"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir);

  if(selected_filename!=NULL)
    {
#ifdef G_OS_WIN32
      gchar *conv_name;
#endif

      /* Remember the directory as the default. */
      default_dir = g_path_get_dirname(selected_filename);
      gnc_set_default_directory(GCONF_SECTION, default_dir);
      g_free(default_dir);

      /*strncpy(file,selected_filename, 255);*/
      DEBUG("Filename found: %s",selected_filename);

      /* Create the Generic transaction importer GUI. */
      gnc_ofx_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE, 42);

      /* Initialize libofx */

      /*ofx_set_statement_cb(libofx_context, ofx_proc_statement_cb, 0);*/
      ofx_set_account_cb(libofx_context, ofx_proc_account_cb, 0);
      ofx_set_transaction_cb(libofx_context, ofx_proc_transaction_cb, 0);
      ofx_set_security_cb(libofx_context, ofx_proc_security_cb, 0);
      /*ofx_set_status_cb(libofx_context, ofx_proc_status_cb, 0);*/

#ifdef G_OS_WIN32
      conv_name = g_win32_locale_filename_from_utf8(selected_filename);
      g_free(selected_filename);
      selected_filename = conv_name;
#endif

      DEBUG("Opening selected file");
      libofx_proc_file(libofx_context, selected_filename, AUTODETECT);
      g_free(selected_filename);
    }

}


/** @} */

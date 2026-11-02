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
#include <config.h>
#include <cstdint>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include <inttypes.h>

#include <libofx/libofx.h>
#include "import-account-matcher.h"
#include "import-commodity-matcher.h"
#include "import-utilities.h"
#include "import-main-matcher.h"

#include "Account.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "gnc-ofx-import.h"
#include "gnc-file.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
#include "gnc-string-utils.h"
#include "gnc-prefs.h"
#include "gnc-gnome-utils.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "window-reconcile.h"

#include <string>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <vector>
#include <utility>

#define GNC_PREFS_GROUP "dialogs.import.ofx"
#define GNC_PREF_AUTO_COMMODITY "auto-create-commodity"

static QofLogModule log_module = GNC_MOD_IMPORT;

/********************************************************************\
 * gnc_file_ofx_import
 * Entry point
\********************************************************************/

static gboolean auto_create_commodity = FALSE;

static std::string
ofx_copy_string (const char *value)
{
    if (!value)
        return {};
    auto valid = gnc_utf8_strip_invalid_strdup (value);
    std::string result {valid ? valid : ""};
    g_free (valid);
    return result;
}

typedef struct OfxTransactionData OfxTransactionData;

struct OfxAccountChoice
{
    std::string online_id;
    std::string description;
    GncGUID commodity_guid;
    GNCAccountType type;
};

struct OfxSecurityChoice
{
    std::string cusip;
    std::string fullname;
    std::string mnemonic;
    std::string name_space;
};

struct OfxInvestmentChoice
{
    std::string online_id;
    std::string account_id;
    std::string security_id;
    std::string security_name;
    std::string currency;
    bool needs_income;
};

struct OfxStatementChoice
{
    std::string account_id;
    bool ledger_balance_valid;
    double ledger_balance;
    time64 ledger_balance_date;
};

// Structure we use to gather information about statement balance/account etc.
typedef struct _ofx_info
{
    GtkWindow* parent;
    GNCImportMainMatcher *gnc_ofx_importer_gui;
    GncGUID last_investment_guid;
    GncGUID last_income_guid;
    gint num_trans_processed;               // Number of transactions processed
    GList* statement;     // Statement, if any
    gboolean run_reconcile;                 // If TRUE the reconcile window is opened after matching.
    GSList* file_list;                      // List of OFX files to import
    GList* trans_list;                      // We store the processed ofx transactions here
    gint response;                          // Response sent by the match gui
    GncGUID book_guid;
    std::uint32_t session_lease;
    bool transaction_pass;
    bool new_book_options_required;
    bool account_selection_pending;
    bool parent_destroyed;
    bool completed;
    bool aborting;
    gulong parent_destroy_handler;
    GtkWidget *reconcile_button;
    gulong reconcile_toggled_handler;
    GtkWidget *reconcile_window;
    gulong reconcile_destroy_handler;
    std::vector<OfxAccountChoice> accounts;
    std::vector<OfxSecurityChoice> securities;
    std::vector<OfxInvestmentChoice> investments;
    std::uint32_t account_index;
    std::uint32_t security_index;
    std::uint32_t investment_index;
    std::uint32_t income_index;
    std::unordered_map<std::string, GncGUID> account_guids;
    std::unordered_map<std::string, GncGUID> commodity_guids;
    std::unordered_map<std::string, GncGUID> investment_guids;
    std::unordered_map<std::string, GncGUID> income_guids;
    std::string pending_online_id;
    GncGUID pending_account_guid;
    char *selected_filename;
} ofx_info ;

static void runMatcher(ofx_info* info, char * selected_filename, gboolean go_to_next_file);

/*
int ofx_proc_status_cb(struct OfxStatusData data)
{
  return 0;
}
*/

static const char *PROP_OFX_INCOME_ACCOUNT = "ofx-income-account";

static bool
ofx_info_is_current (const ofx_info *info)
{
    auto book = gnc_get_current_book ();
    return info && info->session_lease && book && qof_book_is_open (book) &&
           guid_equal (&info->book_guid, qof_instance_get_guid (QOF_INSTANCE (book)));
}

static Account *
ofx_account_from_map (ofx_info *info,
                      const std::unordered_map<std::string, GncGUID> &map,
                      const std::string &key)
{
    auto it = map.find (key);
    if (it == map.end () || !ofx_info_is_current (info))
        return nullptr;
    auto account = xaccAccountLookup (&it->second, gnc_get_current_book ());
    return account && !qof_instance_get_destroying (QOF_INSTANCE (account))
        ? account : nullptr;
}

static Account *
ofx_account_from_guid (ofx_info *info, const GncGUID &guid)
{
    if (!ofx_info_is_current (info) || guid_equal (&guid, guid_null ()))
        return nullptr;
    auto account = xaccAccountLookup (&guid, gnc_get_current_book ());
    return account && !qof_instance_get_destroying (QOF_INSTANCE (account))
        ? account : nullptr;
}

static gnc_commodity *
ofx_commodity_from_map (ofx_info *info, const std::string &key)
{
    auto it = info->commodity_guids.find (key);
    if (it == info->commodity_guids.end () || !ofx_info_is_current (info))
        return nullptr;
    auto commodity = gnc_commodity_find_commodity_by_guid (
        &it->second, gnc_get_current_book ());
    return commodity;
}

static Account*
get_associated_income_account(const Account* investment_account)
{
    GncGUID *income_guid = NULL;
    Account *acct = NULL;
    g_assert(investment_account);
    qof_instance_get (QOF_INSTANCE (investment_account),
                      PROP_OFX_INCOME_ACCOUNT, &income_guid,
                      NULL);
    acct = xaccAccountLookup (income_guid,
                              gnc_account_get_book(investment_account));
    guid_free (income_guid);
    return acct;
}

static void
set_associated_income_account(Account* investment_account,
                              const Account *income_account)
{
    const GncGUID * income_acc_guid;

    g_assert(investment_account);
    g_assert(income_account);

    income_acc_guid = xaccAccountGetGUID(income_account);
    xaccAccountBeginEdit(investment_account);
    qof_instance_set (QOF_INSTANCE (investment_account),
		      PROP_OFX_INCOME_ACCOUNT, income_acc_guid,
		      NULL);
    xaccAccountCommitEdit(investment_account);
}

int ofx_proc_statement_cb (struct OfxStatementData data, void * statement_user_data);
int ofx_proc_security_cb (const struct OfxSecurityData data, void * security_user_data);
int ofx_proc_transaction_cb (OfxTransactionData data, void *user_data);
int ofx_proc_account_cb (struct OfxAccountData data, void * account_user_data);
static double ofx_get_investment_amount (const OfxTransactionData* data);

static const gchar *gnc_ofx_ttype_to_string(TransactionType t)
{
    switch (t)
    {
    case OFX_CREDIT:
        return "Generic credit";
    case OFX_DEBIT:
        return "Generic debit";
    case OFX_INT:
        return "Interest earned or paid (Note: Depends on signage of amount)";
    case OFX_DIV:
        return "Dividend";
    case OFX_FEE:
        return "FI fee";
    case OFX_SRVCHG:
        return "Service charge";
    case OFX_DEP:
        return "Deposit";
    case OFX_ATM:
        return "ATM debit or credit (Note: Depends on signage of amount)";
    case OFX_POS:
        return "Point of sale debit or credit (Note: Depends on signage of amount)";
    case OFX_XFER:
        return "Transfer";
    case OFX_CHECK:
        return "Check";
    case OFX_PAYMENT:
        return "Electronic payment";
    case OFX_CASH:
        return "Cash withdrawal";
    case OFX_DIRECTDEP:
        return "Direct deposit";
    case OFX_DIRECTDEBIT:
        return "Merchant initiated debit";
    case OFX_REPEATPMT:
        return "Repeating payment/standing order";
    case OFX_OTHER:
        return "Other";
    default:
        return "Unknown transaction type";
    }
}

static const gchar *gnc_ofx_invttype_to_str(InvTransactionType t)
{
    switch (t)
    {
    case OFX_BUYDEBT:
        return "BUYDEBT (Buy debt security)";
    case OFX_BUYMF:
        return "BUYMF (Buy mutual fund)";
    case OFX_BUYOPT:
        return "BUYOPT (Buy option)";
    case OFX_BUYOTHER:
        return "BUYOTHER (Buy other security type)";
    case OFX_BUYSTOCK:
        return "BUYSTOCK (Buy stock))";
    case OFX_CLOSUREOPT:
        return "CLOSUREOPT (Close a position for an option)";
    case OFX_INCOME:
        return "INCOME (Investment income is realized as cash into the investment account)";
    case OFX_INVEXPENSE:
        return "INVEXPENSE (Misc investment expense that is associated with a specific security)";
    case OFX_JRNLFUND:
        return "JRNLFUND (Journaling cash holdings between subaccounts within the same investment account)";
    case OFX_MARGININTEREST:
        return "MARGININTEREST (Margin interest expense)";
    case OFX_REINVEST:
        return "REINVEST (Reinvestment of income)";
    case OFX_RETOFCAP:
        return "RETOFCAP (Return of capital)";
    case OFX_SELLDEBT:
        return "SELLDEBT (Sell debt security.  Used when debt is sold, called, or reached maturity)";
    case OFX_SELLMF:
        return "SELLMF (Sell mutual fund)";
    case OFX_SELLOPT:
        return "SELLOPT (Sell option)";
    case OFX_SELLOTHER:
        return "SELLOTHER (Sell other type of security)";
    case OFX_SELLSTOCK:
        return "SELLSTOCK (Sell stock)";
    case OFX_SPLIT:
        return "SPLIT (Stock or mutial fund split)";
    case OFX_TRANSFER:
        return "TRANSFER (Transfer holdings in and out of the investment account)";
#ifdef HAVE_LIBOFX_VERSION_0_10
    case OFX_INVBANKTRAN:
         return "Transfer cash in and out of the investment account";
#endif
    default:
        return "ERROR, this investment transaction type is unknown.  This is a bug in ofxdump";
    }

}

static gchar*
sanitize_string (gchar* str)
{
    gchar *inval;
    const int length = -1; /*Assumes str is null-terminated */
    while (!g_utf8_validate (str, length, (const gchar **)(&inval)))
	*inval = '@';
    return str;
}

int ofx_proc_security_cb(const struct OfxSecurityData data, void *security_user_data)
{
    auto info = static_cast<ofx_info *>(security_user_data);
    if (!info || info->transaction_pass || !data.unique_id_valid)
        return 0;
    OfxSecurityChoice choice {};
    choice.cusip = ofx_copy_string(data.unique_id);
    choice.fullname = data.secname_valid ? ofx_copy_string(data.secname) : "";
    choice.mnemonic = data.ticker_valid ? ofx_copy_string(data.ticker) : "";
    choice.name_space = data.unique_id_type_valid ? ofx_copy_string(data.unique_id_type) : "";
    for (auto &known : info->securities)
    {
        if (known.cusip != choice.cusip)
            continue;
        if (known.fullname.empty()) known.fullname = choice.fullname;
        if (known.mnemonic.empty()) known.mnemonic = choice.mnemonic;
        if (known.name_space.empty()) known.name_space = choice.name_space;
        return 0;
    }
    info->securities.emplace_back(std::move(choice));
    return 0;
}

static void gnc_ofx_set_split_memo(const OfxTransactionData* data, Split *split)
{
    g_assert(data);
    g_assert(split);
    /* Also put the ofx transaction name in
     * the splits memo field, or ofx memo if
     * name is unavailable */
    if (data->name_valid)
    {
        xaccSplitSetMemo(split, data->name);
    }
    else if (data->memo_valid)
    {
        xaccSplitSetMemo(split, data->memo);
    }
}
static gnc_numeric gnc_ofx_numeric_from_double(double value, const gnc_commodity *commodity)
{
    return double_to_gnc_numeric (value,
                                  gnc_commodity_get_fraction(commodity),
                                  GNC_HOW_RND_ROUND_HALF_UP);
}
static gnc_numeric gnc_ofx_numeric_from_double_txn(double value, const Transaction* txn)
{
    return gnc_ofx_numeric_from_double(value, xaccTransGetCurrency(txn));
}

/* LibOFX has a daylight time handling bug,
 * https://sourceforge.net/p/libofx/bugs/39/, which causes it to adjust the
 * timestamp for daylight time even when daylight time is not in
 * effect. HAVE_OFX_BUG_39 reflects the result of checking for this bug during
 * configuration, and fix_ofx_bug_39() corrects for it.
 */
static time64
fix_ofx_bug_39 (time64 t)
{
#if HAVE_OFX_BUG_39
    struct tm stm;

#ifdef __FreeBSD__
    time64 now;
    /*
     * FreeBSD has it's own libc implementation which differs from glibc. In particular:
     * There is no daylight global
     * tzname members are set to the string "   " (three spaces) when not explicitly populated
     *
     * To check that the current timezone does not observe DST I check if tzname[1] starts with a space.
     */
    now = gnc_time (NULL);
    gnc_localtime_r(&now, &stm);
    tzset();

    if (tzname[1][0] != ' ' && !stm.tm_isdst)
#else
    gnc_localtime_r(&t, &stm);
    if (daylight && !stm.tm_isdst)
#endif
        t += 3600;
#endif
    return t;
}

static void
set_transaction_dates(Transaction *transaction, OfxTransactionData *data)
{
     /* Note: Unfortunately libofx <= 0.9.5 will not report a missing
     * date field as an invalid one. Instead, it will report it as
     * valid and return a completely bogus date. Starting with
     * libofx-0.9.6 (not yet released as of 2012-09-09), it will still
     * be reported as valid but at least the date integer itself is
     * just plain zero. */

    time64 current_time = gnc_time (NULL);

    if (data->date_posted_valid && (data->date_posted != 0))
    {
        /* The hopeful case: We have a posted_date */
        data->date_posted = fix_ofx_bug_39 (data->date_posted);
        xaccTransSetDatePostedSecsNormalized(transaction, data->date_posted);
    }
    else if (data->date_initiated_valid && (data->date_initiated != 0))
    {
        /* No posted date? Maybe we have an initiated_date */
        data->date_initiated = fix_ofx_bug_39 (data->date_initiated);
        xaccTransSetDatePostedSecsNormalized(transaction, data->date_initiated);
    }
    else
    {
        /* Uh no, no valid date. As a workaround use today's date */
        xaccTransSetDatePostedSecsNormalized(transaction, current_time);
    }

    xaccTransSetDateEnteredSecs(transaction, current_time);
}

static void
fill_transaction_description(Transaction *transaction, OfxTransactionData *data)
{
    /* Put transaction name in Description, or memo if name unavailable */
    if (data->name_valid)
    {
        xaccTransSetDescription(transaction, data->name);
    }
    else if (data->memo_valid)
    {
        xaccTransSetDescription(transaction, data->memo);
    }
}

static void
fill_transaction_notes(Transaction *transaction, OfxTransactionData *data)
{
    /* Put everything else in the Notes field */
    char *notes = g_strdup_printf("OFX ext. info: ");

    if (data->transactiontype_valid)
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%s", tmp, "|Trans type:",
                                gnc_ofx_ttype_to_string(data->transactiontype));
        g_free(tmp);
    }

    if (data->invtransactiontype_valid)
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%s", tmp, "|Investment Trans type:",
                                gnc_ofx_invttype_to_str(data->invtransactiontype));
        g_free(tmp);
    }
    if (data->memo_valid && data->name_valid) /* Copy only if memo wasn't put in Description */
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%s", tmp, "|Memo:", data->memo);
        g_free(tmp);
    }
    if (data->date_funds_available_valid)
    {
        char dest_string[MAX_DATE_LENGTH];
        time64 time = data->date_funds_available;
        char *tmp = notes;

        gnc_time64_to_iso8601_buff (time, dest_string);
        notes = g_strdup_printf("%s%s%s", tmp,
				"|Date funds available:", dest_string);
        g_free(tmp);
    }
    if (data->server_transaction_id_valid)
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%s", tmp,
				"|Server trans ID (conf. number):",
				sanitize_string (data->server_transaction_id));
        g_free(tmp);
    }
    if (data->standard_industrial_code_valid)
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%ld", tmp,
				"|Standard Industrial Code:",
                                data->standard_industrial_code);
        g_free(tmp);

    }
    if (data->payee_id_valid)
    {
        char *tmp = notes;
        notes = g_strdup_printf("%s%s%s", tmp, "|Payee ID:",
				sanitize_string (data->payee_id));
        g_free(tmp);
    }
    //PERR("WRITEME: GnuCash ofx_proc_transaction():Add PAYEE and ADDRESS here once supported by libofx! Notes=%s\n", notes);

    /* Ideally, gnucash should process the corrected transactions */
    if (data->fi_id_corrected_valid)
    {
        char *tmp = notes;
        PERR("WRITEME: GnuCash ofx_proc_transaction(): WARNING: This transaction corrected a previous transaction, but we created a new one instead!\n");
        notes = g_strdup_printf("%s%s%s%s", tmp,
				"|This corrects transaction #",
				sanitize_string (data->fi_id_corrected),
				"but GnuCash didn't process the correction!");
        g_free(tmp);
    }
    xaccTransSetNotes(transaction, notes);
    g_free(notes);

}

static void
process_bank_transaction(Transaction *transaction, Account *import_account,
                         OfxTransactionData *data, ofx_info *info)
{
    Split *split;
    gnc_numeric gnc_amount;
    QofBook *book = qof_instance_get_book(QOF_INSTANCE(transaction));
    double amount = data->amount;
#ifdef HAVE_LIBOFX_VERSION_0_10
    if (data->currency_ratio_valid && data->currency_ratio != 0)
        amount *= data->currency_ratio;
#endif
    /***** Process a normal transaction ******/
    DEBUG("Adding split; Ordinary banking transaction, money flows from or into the source account");
    split = xaccMallocSplit(book);
    xaccTransAppendSplit(transaction, split);
    xaccAccountInsertSplit(import_account, split);
    gnc_amount = gnc_ofx_numeric_from_double_txn(amount, transaction);
    xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));

    /* set tran-num and/or split-action per book option */
    if (data->check_number_valid)
    {
        /* SQL will correctly interpret the string "null", but
         * the transaction num field is declared to be
         * non-null so substitute the empty string.
         */
        const char *num_value =
            strcasecmp (data->check_number, "null") == 0 ? "" :
            data->check_number;
        gnc_set_num_action(transaction, split, num_value, NULL);
    }
    else if (data->reference_number_valid)
    {
        const char *num_value =
            strcasecmp (data->reference_number, "null") == 0 ? "" :
            data->check_number;
        gnc_set_num_action(transaction, split, num_value, NULL);
    }
    /* Also put the ofx transaction's memo in the
     * split's memo field */
    if (data->memo_valid)
    {
        xaccSplitSetMemo(split, data->memo);
    }
    if (data->fi_id_valid)
    {
        xaccSplitSetOnlineID(split, sanitize_string (data->fi_id));
    }
}

static void
add_investment_split(Transaction* transaction, Account* account,
                             OfxTransactionData *data)
{
    Split *split;
    QofBook *book = gnc_account_get_book(account);
    gnc_numeric gnc_amount, gnc_units;
    gnc_commodity *commodity = xaccAccountGetCommodity(account);
    DEBUG("Adding investment split; Money flows from or into the stock account");
    split = xaccMallocSplit(book);
    xaccTransAppendSplit(transaction, split);
    xaccAccountInsertSplit(account, split);

    gnc_amount =
        gnc_ofx_numeric_from_double_txn(ofx_get_investment_amount(data),
                                        transaction);
    gnc_units = gnc_ofx_numeric_from_double (data->units, commodity);
    xaccSplitSetAmount(split, gnc_units);
    xaccSplitSetValue(split, gnc_amount);

    /* set tran-num and/or split-action per book option */
    if (data->check_number_valid)
    {
        gnc_set_num_action(transaction, split, data->check_number, NULL);
    }
    else if (data->reference_number_valid)
    {
        gnc_set_num_action(transaction, split,
                           data->reference_number, NULL);
    }
    if (data->security_data_ptr->memo_valid)
    {
        xaccSplitSetMemo(split,
                         sanitize_string (data->security_data_ptr->memo));
    }
    if (data->fi_id_valid &&
        xaccAccountTypesCompatible(xaccAccountGetType(account),
                                   ACCT_TYPE_ASSET))
    {
        xaccSplitSetOnlineID(split, sanitize_string (data->fi_id));
    }
}

static void
add_currency_split(Transaction *transaction, Account* account,
                 double amount, OfxTransactionData *data)
{
    Split *split;
    QofBook *book = gnc_account_get_book(account);
    gnc_numeric gnc_amount;

    split = xaccMallocSplit(book);
    xaccTransAppendSplit(transaction, split);
    xaccAccountInsertSplit(account, split);
    gnc_amount = gnc_ofx_numeric_from_double_txn(amount, transaction);
    xaccSplitSetBaseValue(split, gnc_amount, xaccTransGetCurrency(transaction));

    // Set split memo from ofx transaction name or memo
    gnc_ofx_set_split_memo(data, split);
    if (data->fi_id_valid)
        xaccSplitSetOnlineID (split, sanitize_string (data->fi_id));
}

/* ******** Process an investment transaction **********/
/* Note that the ACCT_TYPE_STOCK account type
   should be replaced with something derived from
   data->invtranstype*/

static bool
process_investment_transaction(Transaction *transaction, Account *import_account,
                               OfxTransactionData *data, ofx_info *info)
{
    Account *investment_account = NULL;
    Account *income_account = NULL;
    gnc_commodity *investment_commodity;
    double amount = data->amount;

    // The caller treats FALSE as a failed import and rolls the transaction
    // back. Validate before adding the cash split so malformed investment
    // data cannot leave a partial transaction behind.
    g_return_val_if_fail(data && data->invtransactiontype_valid, FALSE);

    // Set the cash split unless it's a reinvestment, which doesn't have one.
    if (data->invtransactiontype != OFX_REINVEST)
    {
        DEBUG("Adding investment cash split.");
        add_currency_split(transaction, import_account,
                           -ofx_get_investment_amount(data), data);
    }

    auto security_id = ofx_copy_string (data->unique_id);
    auto account_id = ofx_copy_string (data->account_id);
    investment_commodity = ofx_commodity_from_map (info, security_id);
    if (!investment_commodity)
    {
        PERR("Commodity not found for the investment transaction");
        return false;
    }
    auto investment_key = account_id + security_id;
    investment_account = ofx_account_from_map (info, info->investment_guids,
                                                investment_key);

    if (!investment_account)
    {
        PERR("Failed to determine an investment asset account.");
        return false;
    }

    if (data->invtransactiontype != OFX_INCOME)
    {
        if (data->unitprice_valid && data->units_valid)
            add_investment_split(transaction, investment_account, data);
        else
        {
            PERR("Unable to add investment split, unit price or units were invalid.");
            return false;
        }
    }

    if (!(data->invtransactiontype == OFX_REINVEST
          || data->invtransactiontype == OFX_INCOME))
        // Done.
        return true;

#ifdef HAVE_LIBOFX_VERSION_0_10
    if (data->currency_ratio_valid && data->currency_ratio != 0)
        amount *= data->currency_ratio;
#endif
    income_account = get_associated_income_account (investment_account);
    if (!income_account)
        income_account = ofx_account_from_map (info, info->income_guids,
                                                investment_key);
    if (!income_account)
    {
        PERR ("No income account was resolved for investment transaction.");
        return false;
    }

    DEBUG("Adding investment income split.");
    if (data->invtransactiontype == OFX_REINVEST)
        add_currency_split(transaction, income_account, amount, data);
    else
        add_currency_split(transaction, income_account, -amount, data);
    return true;
}

int ofx_proc_transaction_cb(OfxTransactionData data, void *user_data)
{
    Account *import_account;
    gnc_commodity *currency = NULL;
    QofBook *book;
    Transaction *transaction;
    ofx_info* info = (ofx_info*) user_data;


    g_assert(info->parent);

    if (!info->transaction_pass)
    {
        if (data.invtransactiontype_valid && data.account_id_valid &&
            data.unique_id_valid && data.security_data_valid &&
            data.security_data_ptr && data.security_data_ptr->secname_valid)
        {
            auto account_id = ofx_copy_string(data.account_id);
            auto security_id = ofx_copy_string(data.unique_id);
            auto security_name = ofx_copy_string(data.security_data_ptr->secname);
            auto online_id = account_id + security_id;
            auto needs_income = data.invtransactiontype == OFX_REINVEST ||
                                data.invtransactiontype == OFX_INCOME;
            auto currency = data.account_ptr && data.account_ptr->currency_valid
                ? ofx_copy_string(data.account_ptr->currency) : std::string {};
            auto found_security = std::find_if(info->securities.begin(), info->securities.end(),
                [&security_id](const OfxSecurityChoice &item) { return item.cusip == security_id; });
            if (found_security == info->securities.end())
                info->securities.push_back({security_id, security_name, {}, {}});
            else if (found_security->fullname.empty())
                found_security->fullname = security_name;
            auto found = std::find_if(info->investments.begin(), info->investments.end(),
                [&online_id](const OfxInvestmentChoice &item) { return item.online_id == online_id; });
            if (found == info->investments.end())
                info->investments.push_back({online_id, account_id, security_id,
                                             security_name, currency, needs_income});
            else
            {
                found->needs_income |= needs_income;
                if (found->currency.empty()) found->currency = currency;
                if (found->security_name.empty()) found->security_name = security_name;
            }
        }
        return 0;
    }

    if (!data.amount_valid)
    {
        PERR("The transaction doesn't have a valid amount");
        return 0;
    }

    if (!data.account_id_valid)
    {
        PERR("account ID for this transaction is unavailable!");
        return 0;
    }

    gnc_utf8_strip_invalid (data.account_id);

    import_account = ofx_account_from_map (info, info->account_guids,
                                           ofx_copy_string (data.account_id));
    if (import_account == NULL)
    {
        PERR("Unable to find account for id %s", data.account_id);
        return 0;
    }
    /***** Validate the input strings to ensure utf8 *****/
    if (data.name_valid)
        gnc_utf8_strip_invalid(data.name);
    if (data.memo_valid)
        gnc_utf8_strip_invalid(data.memo);
    if (data.check_number_valid)
        gnc_utf8_strip_invalid(data.check_number);
    if (data.reference_number_valid)
        gnc_utf8_strip_invalid(data.reference_number);

    /***** Create the transaction and setup transaction data *******/
    book = gnc_account_get_book(import_account);
    transaction = xaccMallocTransaction(book);
    xaccTransBeginEdit(transaction);

    set_transaction_dates(transaction, &data);
    fill_transaction_description(transaction, &data);
    fill_transaction_notes(transaction, &data);

    if (data.account_ptr && data.account_ptr->currency_valid)
    {
        DEBUG("Currency from libofx: %s", data.account_ptr->currency);
        currency = gnc_commodity_table_lookup( gnc_get_current_commodities (),
                                               GNC_COMMODITY_NS_CURRENCY,
                                               data.account_ptr->currency);
    }
    else
    {
        DEBUG("Currency from libofx unavailable, defaulting to account's default");
        currency = xaccAccountGetCommodity(import_account);
    }

    xaccTransSetCurrency(transaction, currency);

    if (!data.invtransactiontype_valid
#ifdef HAVE_LIBOFX_VERSION_0_10
        || data.invtransactiontype == OFX_INVBANKTRAN
#endif
        )
        process_bank_transaction(transaction, import_account, &data, info);
    else if (data.unique_id_valid
             && data.security_data_valid
             && data.security_data_ptr != NULL
             && data.security_data_ptr->secname_valid)
    {
        if (!process_investment_transaction(transaction, import_account, &data, info))
        {
            xaccTransDestroy(transaction);
            xaccTransCommitEdit(transaction);
            return 0;
        }
    }
    else
    {
        PERR("Unsupported OFX transaction type.");
        xaccTransDestroy(transaction);
        xaccTransCommitEdit(transaction);
        return 0;
    }

    /* Send transaction to importer GUI. */
    if (xaccTransCountSplits(transaction) > 0)
    {
        DEBUG("%d splits sent to the importer gui",
              xaccTransCountSplits(transaction));
        info->trans_list = g_list_prepend (info->trans_list, transaction);
    }
    else
    {
        PERR("No splits in transaction (missing account?), ignoring.");
        xaccTransDestroy(transaction);
        xaccTransCommitEdit(transaction);
    }

    info->num_trans_processed += 1;
    return 0;
}//end ofx_proc_transaction()


int ofx_proc_statement_cb (struct OfxStatementData data, void * statement_user_data)
{
    ofx_info* info = (ofx_info*) statement_user_data;
    if (!info || info->transaction_pass || !ofx_info_is_current (info))
        return 0;
    auto statement = new OfxStatementChoice {
        data.account_id_valid ? ofx_copy_string (data.account_id) : std::string {},
        data.ledger_balance_valid != 0, data.ledger_balance,
        data.ledger_balance_date
    };
    info->statement = g_list_prepend (info->statement, statement);
    return 0;
}


int ofx_proc_account_cb(struct OfxAccountData data, void *account_user_data)
{
    auto info = static_cast<ofx_info *>(account_user_data);
    if (!info || info->transaction_pass || !data.account_id_valid || !ofx_info_is_current(info))
        return 0;

    auto online_id = ofx_copy_string(data.account_id);
    for (const auto &existing : info->accounts)
        if (existing.online_id == online_id)
            return 0;

    GNCAccountType type = ACCT_TYPE_NONE;
    const gchar *type_name = _("Unknown OFX account");
    if (data.account_type_valid)
    {
        switch (data.account_type)
        {
        case OfxAccountData::OFX_CHECKING:
            type = ACCT_TYPE_BANK; type_name = _("Unknown OFX checking account"); break;
        case OfxAccountData::OFX_SAVINGS:
            type = ACCT_TYPE_BANK; type_name = _("Unknown OFX savings account"); break;
        case OfxAccountData::OFX_MONEYMRKT:
            type = ACCT_TYPE_MONEYMRKT; type_name = _("Unknown OFX money market account"); break;
        case OfxAccountData::OFX_CREDITLINE:
            type = ACCT_TYPE_CREDITLINE; type_name = _("Unknown OFX credit line account"); break;
        case OfxAccountData::OFX_CMA:
            type_name = _("Unknown OFX CMA account"); break;
        case OfxAccountData::OFX_CREDITCARD:
            type = ACCT_TYPE_CREDIT; type_name = _("Unknown OFX credit card account"); break;
        case OfxAccountData::OFX_INVESTMENT:
            type = ACCT_TYPE_BANK; type_name = _("Unknown OFX investment account"); break;
        default:
            PERR("Unknown OFX account type"); break;
        }
    }
    auto account_name = ofx_copy_string(data.account_id_valid ? data.account_name : "");
    auto description = g_strdup_printf("%s \"%s\"", type_name, account_name.c_str());
    auto commodity = data.currency_valid
        ? gnc_commodity_table_lookup(gnc_get_current_commodities(), GNC_COMMODITY_NS_CURRENCY,
                                     data.currency)
        : nullptr;
    OfxAccountChoice choice {};
    choice.online_id = std::move(online_id);
    choice.description = description;
    choice.commodity_guid = commodity ? *qof_instance_get_guid(QOF_INSTANCE(commodity)) : *guid_null();
    choice.type = type;
    info->accounts.emplace_back(std::move(choice));
    g_free(description);
    return 0;
}

double ofx_get_investment_amount(const OfxTransactionData* data)
{
    double amount = data->amount;
#ifdef HAVE_LIBOFX_VERSION_0_10
    if (data->invtransactiontype == OFX_INVBANKTRAN)
        return 0.0;
    if (data->currency_ratio_valid && data->currency_ratio != 0)
        amount *= data->currency_ratio;
#endif
    g_assert(data);
    switch (data->invtransactiontype)
    {
    case OFX_BUYDEBT:
    case OFX_BUYMF:
    case OFX_BUYOPT:
    case OFX_BUYOTHER:
    case OFX_BUYSTOCK:
        return fabs(amount);
    case OFX_SELLDEBT:
    case OFX_SELLMF:
    case OFX_SELLOPT:
    case OFX_SELLOTHER:
    case OFX_SELLSTOCK:
        return -1 * fabs(amount);
    default:
        return -1 * amount;
    }
}

// Forward declaration, required because several static functions depend on one-another.
static void
gnc_file_ofx_import_process_file (ofx_info* info);
static void
gnc_file_ofx_import_process_second_pass (ofx_info *info);

static void
ofx_resolve_next (ofx_info *info);

static void
ofx_parent_destroyed (GtkWidget *, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (info)
        info->parent_destroyed = true;
}

static void
ofx_info_free (ofx_info *info)
{
    if (!info || info->completed)
        return;
    info->completed = true;
    if (info->parent && info->parent_destroy_handler &&
        g_signal_handler_is_connected (info->parent, info->parent_destroy_handler))
        g_signal_handler_disconnect (info->parent, info->parent_destroy_handler);
    if (info->reconcile_button && info->reconcile_toggled_handler &&
        g_signal_handler_is_connected (info->reconcile_button,
                                       info->reconcile_toggled_handler))
        g_signal_handler_disconnect (info->reconcile_button,
                                     info->reconcile_toggled_handler);
    if (info->reconcile_window && info->reconcile_destroy_handler &&
        g_signal_handler_is_connected (info->reconcile_window,
                                       info->reconcile_destroy_handler))
        g_signal_handler_disconnect (info->reconcile_window,
                                     info->reconcile_destroy_handler);
    gnc_gui_end_session_operation (info->session_lease);
    info->session_lease = 0;
    g_list_free_full (info->statement, [](gpointer item) {
        delete static_cast<OfxStatementChoice *>(item);
    });
    info->statement = nullptr;
    g_list_free (info->trans_list);
    info->trans_list = nullptr;
    g_slist_free_full (info->file_list, g_free);
    info->file_list = nullptr;
    g_free (info->selected_filename);
    info->selected_filename = nullptr;
    g_clear_object (&info->reconcile_button);
    g_clear_object (&info->reconcile_window);
    g_clear_object (&info->parent);
    delete info;
}

static void
ofx_abort_import (ofx_info *info)
{
    if (!info || info->completed || info->aborting)
        return;
    info->aborting = true;
    if (info->gnc_ofx_importer_gui)
    {
        auto matcher = info->gnc_ofx_importer_gui;
        info->gnc_ofx_importer_gui = nullptr;
        info->response = GTK_RESPONSE_CANCEL;
        gnc_gen_trans_list_delete (matcher);
    }
    for (auto node = info->trans_list; node; node = node->next)
    {
        auto transaction = static_cast<Transaction *> (node->data);
        if (transaction && !qof_instance_get_destroying (QOF_INSTANCE (transaction)))
        {
            xaccTransBeginEdit (transaction);
            xaccTransDestroy (transaction);
            xaccTransCommitEdit (transaction);
        }
    }
    ofx_info_free (info);
}

static bool
ofx_resolution_current (ofx_info *info)
{
    return ofx_info_is_current (info) && !info->parent_destroyed && info->parent &&
        !gtk_widget_in_destruction (GTK_WIDGET (info->parent));
}

static void
ofx_account_selected (Account *account, gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || !accepted || !account ||
        qof_instance_get_book (QOF_INSTANCE (account)) != gnc_get_current_book () ||
        qof_instance_get_destroying (QOF_INSTANCE (account)))
    {
        ofx_abort_import (info);
        return;
    }
    info->account_guids[info->pending_online_id] = *xaccAccountGetGUID (account);
    if (info->account_selection_pending)
    {
        ++info->account_index;
        info->account_selection_pending = false;
    }
    ofx_resolve_next (info);
}

static void
ofx_commodity_selected (gnc_commodity *commodity, gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || !accepted || !commodity ||
        qof_instance_get_book (QOF_INSTANCE (commodity)) != gnc_get_current_book () ||
        qof_instance_get_destroying (QOF_INSTANCE (commodity)))
    {
        ofx_abort_import (info);
        return;
    }
    auto &choice = info->securities[info->security_index++];
    info->commodity_guids[choice.cusip] = *qof_instance_get_guid (QOF_INSTANCE (commodity));
    ofx_resolve_next (info);
}

static void
ofx_investment_retry (GtkWindow *, gint response, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || response != GTK_RESPONSE_YES)
    {
        ofx_abort_import (info);
        return;
    }
    auto account = xaccAccountLookup (&info->pending_account_guid, gnc_get_current_book ());
    if (account && !qof_instance_get_destroying (QOF_INSTANCE (account)))
        xaccAccountSetOnlineID (account, "");
    ofx_resolve_next (info);
}

static void
ofx_investment_selected (Account *account, gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || !accepted || !account ||
        qof_instance_get_book (QOF_INSTANCE (account)) != gnc_get_current_book () ||
        qof_instance_get_destroying (QOF_INSTANCE (account)))
    {
        ofx_abort_import (info);
        return;
    }
    const auto &choice = info->investments[info->investment_index];
    auto commodity = ofx_commodity_from_map (info, choice.security_id);
    if (!commodity || xaccAccountGetCommodity (account) != commodity)
    {
        info->pending_account_guid = *xaccAccountGetGUID (account);
        gnc_verify_dialog_async (info->parent, TRUE, ofx_investment_retry, info,
            _("The chosen account \"%s\" does not have the correct currency/security \"%s\". Do you want to choose again?"),
            xaccAccountGetName (account), commodity ? gnc_commodity_get_fullname (commodity) : "");
        return;
    }
    info->investment_guids[choice.online_id] = *xaccAccountGetGUID (account);
    info->last_investment_guid = *xaccAccountGetGUID (account);
    ++info->investment_index;
    ofx_resolve_next (info);
}

static void
ofx_income_selected (Account *account, gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || !accepted || !account ||
        qof_instance_get_book (QOF_INSTANCE (account)) != gnc_get_current_book () ||
        qof_instance_get_destroying (QOF_INSTANCE (account)))
    {
        ofx_abort_import (info);
        return;
    }
    const auto &choice = info->investments[info->income_index];
    info->income_guids[choice.online_id] = *xaccAccountGetGUID (account);
    auto investment = ofx_account_from_map (info, info->investment_guids, choice.online_id);
    if (investment)
        set_associated_income_account (investment, account);
    info->last_income_guid = *xaccAccountGetGUID (account);
    ++info->income_index;
    ofx_resolve_next (info);
}

static void
ofx_new_book_options_done (GtkWindow *, gint response, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!ofx_resolution_current (info) || response != GTK_RESPONSE_OK)
    {
        ofx_abort_import (info);
        return;
    }
    info->new_book_options_required = false;
    ofx_resolve_next (info);
}

static void
ofx_resolve_next (ofx_info *info)
{
    if (!ofx_resolution_current (info))
    {
        ofx_abort_import (info);
        return;
    }
    if (info->new_book_options_required)
    {
        info->new_book_options_required = false;
        gnc_new_book_option_display_async (GTK_WIDGET (info->parent),
                                           ofx_new_book_options_done, info);
        return;
    }
    while (info->account_index < info->accounts.size ())
    {
        const auto &choice = info->accounts[info->account_index];
        auto account = gnc_import_find_account_by_online_id (choice.online_id.c_str (), choice.type);
        if (account)
        {
            info->account_guids[choice.online_id] = *xaccAccountGetGUID (account);
            ++info->account_index;
            continue;
        }
        auto commodity = guid_equal (&choice.commodity_guid, guid_null ()) ? nullptr :
            gnc_commodity_find_commodity_by_guid (&choice.commodity_guid, gnc_get_current_book ());
        info->pending_online_id = choice.online_id;
        info->account_selection_pending = true;
        gnc_import_select_account_async (GTK_WIDGET (info->parent), choice.online_id.c_str (), TRUE,
            choice.description.c_str (), commodity, choice.type, nullptr, ofx_account_selected, info);
        return;
    }
    while (info->security_index < info->securities.size ())
    {
        const auto &choice = info->securities[info->security_index];
        auto commodity = gnc_import_find_commodity_by_cusip (choice.cusip.c_str ());
        if (!commodity && auto_create_commodity)
        {
            auto book = gnc_get_current_book ();
            auto name_space = choice.name_space.empty () ? nullptr : choice.name_space.c_str ();
            commodity = gnc_commodity_new (book, choice.fullname.c_str (), name_space,
                                           choice.mnemonic.c_str (), choice.cusip.c_str (), 1);
            if (commodity)
            {
                gnc_commodity_begin_edit (commodity);
                gnc_commodity_user_set_quote_flag (commodity, TRUE);
                auto source = gnc_quote_source_lookup_by_ti (SOURCE_SINGLE, 0);
                gnc_commodity_set_quote_source (commodity, source);
                gnc_commodity_commit_edit (commodity);
                gnc_commodity_table_insert (gnc_get_current_commodities (), commodity);
            }
        }
        if (commodity)
        {
            info->commodity_guids[choice.cusip] = *qof_instance_get_guid (QOF_INSTANCE (commodity));
            ++info->security_index;
            continue;
        }
        gnc_import_select_commodity_async (GTK_WIDGET (info->parent), choice.cusip.c_str (), TRUE,
            choice.fullname.c_str (), choice.mnemonic.c_str (), ofx_commodity_selected, info);
        return;
    }
    while (info->investment_index < info->investments.size ())
    {
        const auto &choice = info->investments[info->investment_index];
        auto commodity = ofx_commodity_from_map (info, choice.security_id);
        if (!commodity) { ofx_abort_import (info); return; }
        auto account = gnc_import_find_account_by_online_id (choice.online_id.c_str (), ACCT_TYPE_STOCK);
        if (account && xaccAccountGetCommodity (account) == commodity)
        {
            info->investment_guids[choice.online_id] = *xaccAccountGetGUID (account);
            info->last_investment_guid = *xaccAccountGetGUID (account);
            ++info->investment_index;
            continue;
        }
        auto description = g_strdup_printf (_("Stock account for security \"%s\""),
                                            choice.security_name.c_str ());
        info->pending_online_id = choice.online_id;
        auto last = ofx_account_from_guid (info, info->last_investment_guid);
        if (last && xaccAccountGetCommodity (last) != commodity)
            last = nullptr;
        gnc_import_select_account_async (GTK_WIDGET (info->parent), choice.online_id.c_str (), TRUE,
            description, commodity, ACCT_TYPE_STOCK, last,
            ofx_investment_selected, info);
        g_free (description);
        return;
    }
    while (info->income_index < info->investments.size ())
    {
        const auto &choice = info->investments[info->income_index];
        if (!choice.needs_income) { ++info->income_index; continue; }
        auto investment = ofx_account_from_map (info, info->investment_guids, choice.online_id);
        auto income = investment ? get_associated_income_account (investment) : nullptr;
        if (income && (qof_instance_get_book (QOF_INSTANCE (income)) != gnc_get_current_book () ||
                       qof_instance_get_destroying (QOF_INSTANCE (income))))
            income = nullptr;
        if (income)
        {
            info->income_guids[choice.online_id] = *xaccAccountGetGUID (income);
            ++info->income_index;
            continue;
        }
        auto currency = choice.currency.empty () ? nullptr :
            gnc_commodity_table_lookup (gnc_get_current_commodities (), GNC_COMMODITY_NS_CURRENCY,
                                        choice.currency.c_str ());
        if (!currency)
        {
            auto source = ofx_account_from_map (info, info->account_guids, choice.account_id);
            currency = source ? xaccAccountGetCommodity (source) : nullptr;
        }
        auto description = g_strdup_printf (_("Income account for security \"%s\""),
                                            choice.security_name.c_str ());
        gnc_import_select_account_async (GTK_WIDGET (info->parent), nullptr, TRUE, description,
            currency, ACCT_TYPE_INCOME, ofx_account_from_guid (info, info->last_income_guid),
            ofx_income_selected, info);
        g_free (description);
        return;
    }
    gnc_file_ofx_import_process_second_pass (info);
}

// gnc_ofx_process_next_file processes the next file in the info->file_list.
static void
gnc_ofx_process_next_file (GtkDialog *dialog, gpointer user_data)
{
    ofx_info* info = (ofx_info*) user_data;
    // Free the statement (if it was allocated).
    g_list_free_full (info->statement, [](gpointer item) { delete static_cast<OfxStatementChoice *>(item); });
    info->statement = NULL;

    // Done with the previous OFX file, process the next one if any.
    if (info->file_list)
        g_free (info->file_list->data);
    info->file_list = g_slist_delete_link (info->file_list, info->file_list);
    if (info->file_list)
        gnc_file_ofx_import_process_file (info);
    else
    {
        // Final cleanup.
        ofx_info_free (info);
    }
}

static void
gnc_ofx_match_done (GtkDialog *dialog, gpointer user_data);

static void
gnc_ofx_matcher_presented (gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!info || info->completed || info->aborting)
        return;
    if (info->reconcile_button && info->reconcile_toggled_handler &&
        g_signal_handler_is_connected (info->reconcile_button,
                                       info->reconcile_toggled_handler))
        g_signal_handler_disconnect (info->reconcile_button,
                                     info->reconcile_toggled_handler);
    info->reconcile_toggled_handler = 0;
    g_clear_object (&info->reconcile_button);
    info->gnc_ofx_importer_gui = nullptr;
    info->response = accepted ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL;
    gnc_ofx_match_done (nullptr, info);
}

static void
gnc_ofx_reconcile_destroyed (GtkWidget *, gpointer user_data)
{
    auto info = static_cast<ofx_info *>(user_data);
    if (!info || info->completed || info->aborting)
        return;
    info->reconcile_destroy_handler = 0;
    g_clear_object (&info->reconcile_window);
    gnc_ofx_match_done (nullptr, info);
}

static void
ofx_statement_pop (ofx_info *info)
{
    if (!info || !info->statement)
        return;
    auto first = info->statement;
    info->statement = first->next;
    if (info->statement)
        info->statement->prev = nullptr;
    delete static_cast<OfxStatementChoice *>(first->data);
    g_list_free_1 (first);
}

static void
gnc_ofx_match_done (GtkDialog *dialog, gpointer user_data)
{
    ofx_info* info = (ofx_info*) user_data;
    if (!info || info->completed || info->aborting)
        return;
    info->gnc_ofx_importer_gui = nullptr;
    if (!ofx_info_is_current (info) || info->parent_destroyed)
    {
        ofx_abort_import (info);
        return;
    }

    /* The the user did not click OK, don't process the rest of the
     * transaction, don't go to the next of xfile.
     */
    if (info->response != GTK_RESPONSE_OK)
    {
        ofx_abort_import (info);
        return;
    }

    if (info->trans_list)
    {
         /* Re-run the match dialog if there are transactions
          * remaining in our list (happens if several accounts exist
          * in the same ofx).
          */
        info->gnc_ofx_importer_gui = gnc_gen_trans_list_new (GTK_WIDGET (info->parent), NULL, FALSE, 42, FALSE);
        if (!info->gnc_ofx_importer_gui)
        {
            ofx_abort_import (info);
            return;
        }
        runMatcher (info, NULL, true);
        return;
    }

    if (info->run_reconcile && info->statement && info->statement->data)
    {
        auto statement = static_cast<OfxStatementChoice*>(info->statement->data);
        // Open a reconcile window.
        Account* account = ofx_account_from_map (info, info->account_guids,
                                                 statement->account_id);
        if (account && statement->ledger_balance_valid)
        {
            gnc_numeric value = double_to_gnc_numeric (statement->ledger_balance,
                                                       xaccAccountGetCommoditySCU (account),
                                                       GNC_HOW_RND_ROUND_HALF_UP);

            RecnWindow* rec_window = recnWindowWithBalance (GTK_WIDGET (info->parent), account, value,
                                                            statement->ledger_balance_date);

            // Connect to destroy, at which point we'll process the next OFX file..
            auto reconcile_window = gnc_ui_reconcile_window_get_window (rec_window);
            info->reconcile_window = GTK_WIDGET (g_object_ref (reconcile_window));
            info->reconcile_destroy_handler = g_signal_connect (
                info->reconcile_window, "destroy",
                G_CALLBACK (gnc_ofx_reconcile_destroyed), info);
            ofx_statement_pop (info);
            return;
        }
    }
    else
    {
        if (info->statement && info->statement->next)
        {
            ofx_statement_pop (info);
            gnc_ofx_match_done (dialog, user_data);
            return;
        }
        else
        {
            g_list_free_full (g_list_first (info->statement), [](gpointer item) { delete static_cast<OfxStatementChoice *>(item); });
            info->statement = NULL;
        }
    }
    gnc_ofx_process_next_file (NULL, info);
}

// This callback is triggered when the user checks or unchecks the reconcile after match
// check box in the matching dialog.
static void
reconcile_when_close_toggled_cb (GtkToggleButton *togglebutton, ofx_info* info)
{
    info->run_reconcile = gtk_toggle_button_get_active (togglebutton);
}

static std::string
make_date_amount_key (const Split* split)
{
    std::ostringstream ss;
    auto _amount = gnc_numeric_reduce (gnc_numeric_abs (xaccSplitGetAmount (split)));
    ss << _amount.num << '/' <<  _amount.denom << ' ' << xaccTransGetDate (xaccSplitGetParent (split));
    return ss.str();
}

static void
runMatcher (ofx_info* info, char * selected_filename, gboolean go_to_next_file)
{
    GtkWindow *parent = info->parent;
    GList* trans_list_remain = NULL;
    std::unordered_map <std::string,Account*> trans_map;

    /* If we have multiple accounts in the ofx file, we need to
     * avoid processing transfers between accounts together because this will
     * create duplicate entries.
     */
    info->num_trans_processed = 0;

    gnc_window_show_progress (_("Removing duplicate transactions…"), 100);

    // Add transactions, but verify that there isn't one that was
    // already added with identical amounts and date, and a different
    // account. To do that, create a hash table whose key is a hash of
    // amount and date, and whose value is the account in which they
    // appear.
    for(GList* node = info->trans_list; node; node=node->next)
    {
        auto trans = static_cast<Transaction*>(node->data);
        Split* split = xaccTransGetSplit (trans, 0);
        Account* account = xaccSplitGetAccount (split);
        auto date_amount_key = make_date_amount_key (split);

        auto it = trans_map.find (date_amount_key);
        if (it != trans_map.end() && it->second != account)
        {
            if (qof_log_check (G_LOG_DOMAIN, QOF_LOG_DEBUG))
            {
                // There is a transaction with identical amounts and
                // dates, but a different account.  That's a potential
                // transfer so process this transaction in a later call.
                gchar *name1 = gnc_account_get_full_name (account);
                gchar *name2 = gnc_account_get_full_name (it->second);
                gchar *amtstr = gnc_numeric_to_string (xaccSplitGetAmount (split));
                gchar *datestr = qof_print_date (xaccTransGetDate (trans));
                DEBUG ("Potential transfer %s %s %s %s\n", name1, name2, amtstr, datestr);
                g_free (name1);
                g_free (name2);
                g_free (amtstr);
                g_free (datestr);
            }
            trans_list_remain = g_list_prepend (trans_list_remain, trans);
        }
        else
        {
            trans_map[date_amount_key] = account;
            gnc_gen_trans_list_add_trans (info->gnc_ofx_importer_gui, trans);
            info->num_trans_processed ++;
        }
    }
    g_list_free (info->trans_list);
    info->trans_list = g_list_reverse (trans_list_remain);
    DEBUG("%d transactions remaining to process in file %s\n", g_list_length (info->trans_list),
          selected_filename);

    gnc_window_show_progress (nullptr, -1);

    // See whether the view has anything in it and warn the user if not.
    if (gnc_gen_trans_list_empty (info->gnc_ofx_importer_gui))
    {
        gnc_gen_trans_list_delete (info->gnc_ofx_importer_gui);
        if (info->num_trans_processed)
        {
            gnc_info_dialog (parent, _("While importing transactions from OFX file '%s' found %d previously imported transactions, no new transactions."),
                             selected_filename,
                             info->num_trans_processed);
            // This is required to ensure we don't mistakenly assume the user canceled.
            info->response = GTK_RESPONSE_OK;
            gnc_ofx_match_done (NULL, info);
            return;
        }
        info->response = GTK_RESPONSE_OK;
        gnc_ofx_match_done (NULL, info);
        return;
    }
    else
    {
        // Show or hide the check box for reconciling after match,
        // depending on whether a statement was received.
        gnc_gen_trans_list_show_reconcile_after_close_button (info->gnc_ofx_importer_gui,
                                                              info->statement != NULL,
                                                              info->run_reconcile);

        auto reconcile_button = gnc_gen_trans_list_get_reconcile_after_close_button (
            info->gnc_ofx_importer_gui);
        info->reconcile_button = GTK_WIDGET (g_object_ref (reconcile_button));
        info->reconcile_toggled_handler = g_signal_connect (
            info->reconcile_button, "toggled",
            G_CALLBACK (reconcile_when_close_toggled_cb), info);
        gnc_gen_trans_list_present (info->gnc_ofx_importer_gui,
                                    gnc_ofx_matcher_presented, info);
    }
}

// Aux function to process the OFX file in info->file_list
static void
gnc_file_ofx_import_process_file (ofx_info* info)
{
    if (!info || !info->file_list || !ofx_info_is_current (info))
        return;
    auto filename = static_cast<char *>(info->file_list->data);
    info->selected_filename = g_strdup (filename);
    info->accounts.clear ();
    info->securities.clear ();
    info->investments.clear ();
    info->account_guids.clear ();
    info->commodity_guids.clear ();
    info->investment_guids.clear ();
    info->income_guids.clear ();
    info->account_index = info->security_index = 0;
    info->investment_index = info->income_index = 0;
    info->transaction_pass = false;
    info->new_book_options_required = gnc_is_new_book ();
    info->num_trans_processed = 0;
    g_list_free_full (info->statement, [](gpointer item) {
        delete static_cast<OfxStatementChoice *>(item);
    });
    info->statement = nullptr;

    auto parser_filename = g_strdup (filename);
#ifdef G_OS_WIN32
    g_free (parser_filename);
    parser_filename = g_win32_locale_filename_from_utf8 (filename);
#endif
    auto context = libofx_get_new_context ();
    ofx_set_statement_cb (context, ofx_proc_statement_cb, info);
    ofx_set_account_cb (context, ofx_proc_account_cb, info);
    ofx_set_transaction_cb (context, ofx_proc_transaction_cb, info);
    ofx_set_security_cb (context, ofx_proc_security_cb, info);
    libofx_proc_file (context, parser_filename, AUTODETECT);
    libofx_free_context (context);
    g_free (parser_filename);
    ofx_resolve_next (info);
}

static void
gnc_file_ofx_import_process_second_pass (ofx_info *info)
{
    if (!ofx_resolution_current (info))
    {
        ofx_abort_import (info);
        return;
    }
    auto filename = static_cast<char *>(info->file_list->data);
    auto parser_filename = g_strdup (filename);
#ifdef G_OS_WIN32
    g_free (parser_filename);
    parser_filename = g_win32_locale_filename_from_utf8 (filename);
#endif
    info->transaction_pass = true;
    info->gnc_ofx_importer_gui = gnc_gen_trans_list_new (
        GTK_WIDGET (info->parent), NULL, FALSE, 42, FALSE);
    if (!info->gnc_ofx_importer_gui)
    {
        g_free (parser_filename);
        ofx_abort_import (info);
        return;
    }
    auto context = libofx_get_new_context ();
    ofx_set_statement_cb (context, ofx_proc_statement_cb, info);
    ofx_set_account_cb (context, ofx_proc_account_cb, info);
    ofx_set_transaction_cb (context, ofx_proc_transaction_cb, info);
    ofx_set_security_cb (context, ofx_proc_security_cb, info);
    libofx_proc_file (context, parser_filename, AUTODETECT);
    libofx_free_context (context);
    g_free (parser_filename);
    auto selected_filename = info->selected_filename;
    info->selected_filename = nullptr;
    runMatcher (info, selected_filename, TRUE);
    g_free (selected_filename);
}

// The main import function. Starts the chain of file imports (if there are several)
static void
gnc_file_ofx_import_files_selected (GSList *selected_filenames,
                                    gpointer user_data)
{
    GtkWindow *parent = static_cast<GtkWindow *> (user_data);
    if (!selected_filenames)
        return;

    /* Remember the directory selected by the user. */
    gchar *default_dir = g_path_get_dirname (
        static_cast<char *> (selected_filenames->data));
    gnc_set_default_directory (GNC_PREFS_GROUP, default_dir);
    g_free (default_dir);

    auto_create_commodity =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_AUTO_COMMODITY);
    DEBUG ("Opening selected file(s)");
    auto info = new ofx_info {};
    info->num_trans_processed = 0;
    info->statement = NULL;
    info->last_investment_guid = *guid_null ();
    info->last_income_guid = *guid_null ();
    info->parent = parent ? GTK_WINDOW (g_object_ref (parent)) : nullptr;
    auto book = gnc_get_current_book ();
    info->book_guid = book
        ? *qof_instance_get_guid (QOF_INSTANCE (book)) : *guid_null ();
    info->session_lease = book ? gnc_gui_begin_session_operation (book) : 0;
    if (!info->session_lease)
    {
        g_clear_object (&info->parent);
        delete info;
        g_slist_free_full (selected_filenames, g_free);
        return;
    }
    info->run_reconcile = FALSE;
    info->file_list = selected_filenames;
    info->trans_list = NULL;
    info->response = 0;
    if (info->parent)
        info->parent_destroy_handler = g_signal_connect (
            info->parent, "destroy", G_CALLBACK (ofx_parent_destroyed), info);
    gnc_file_ofx_import_process_file (info);
}

void gnc_file_ofx_import (GtkWindow *parent)
{
    extern int ofx_PARSER_msg;
    extern int ofx_DEBUG_msg;
    extern int ofx_WARNING_msg;
    extern int ofx_ERROR_msg;
    extern int ofx_INFO_msg;
    extern int ofx_STATUS_msg;
    char *default_dir;
    GList *filters = NULL;
    GtkFileFilter* filter = gtk_file_filter_new ();


    ofx_PARSER_msg = false;
    ofx_DEBUG_msg = false;
    ofx_WARNING_msg = true;
    ofx_ERROR_msg = true;
    ofx_INFO_msg = true;
    ofx_STATUS_msg = false;

    DEBUG("gnc_file_ofx_import(): Begin...\n");

    default_dir = gnc_get_default_directory(GNC_PREFS_GROUP);
    gtk_file_filter_set_name (filter, _("Open/Quicken Financial Exchange file (*.ofx, *.qfx)"));
    gtk_file_filter_add_pattern (filter, "*.[oqOQ][fF][xX]");
    filters = g_list_prepend( filters, filter );

    gnc_file_dialog_async (parent,
                          _("Select one or multiple OFX/QFX file(s) to process"),
                          filters, default_dir, GNC_FILE_DIALOG_IMPORT, TRUE,
                          gnc_file_ofx_import_files_selected, parent, NULL);
    g_free(default_dir);
}


/** @} */

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
#ifdef GNC_OFX_IMPORT_TEST_SEAM
#include "gnc-ofx-import-test-seam.h"
#endif
#include "gnc-ofx-import-teardown.h"
#include "engine-helpers.h"
#include "gnc-ofx-import.h"
#include "gnc-file.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
#include "gnc-string-utils.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "window-reconcile.h"

#include <string>
#include <sstream>
#include <unordered_map>
#include <vector>

#define GNC_PREFS_GROUP "dialogs.import.ofx"
#define GNC_PREF_AUTO_COMMODITY "auto-create-commodity"

static QofLogModule log_module = GNC_MOD_IMPORT;

/********************************************************************\
 * gnc_file_ofx_import
 * Entry point
\********************************************************************/

static gboolean auto_create_commodity = FALSE;
typedef struct OfxTransactionData OfxTransactionData;
#ifdef GNC_OFX_IMPORT_TEST_SEAM
struct GncOfxImportTestSeam;
#endif

// Structure we use to gather information about statement balance/account etc.
typedef struct _ofx_info
{
    /* Held while an asynchronous import is active; destroy is still an abort. */
    GtkWindow* parent;
    gulong parent_destroy_handler;
    gboolean parent_destroyed;
    GncOfxImportLifecycle *lifecycle;
    GncSessionOperationContext *operation_context; /* borrowed from lifecycle */
    GNCImportMainMatcher *gnc_ofx_importer_gui;
    Account *last_import_account;
    Account *last_investment_account;
    Account *last_income_account;
    gint num_trans_processed;               // Number of transactions processed
    GList* statement;     // Statement, if any
    gboolean run_reconcile;                 // If TRUE the reconcile window is opened after matching.
    GSList* file_list;                      // List of OFX files to import
    GList* trans_list;                      // We store the processed ofx transactions here
    gint response;                          // Response sent by the match gui
#ifdef GNC_OFX_IMPORT_TEST_SEAM
    GncOfxImportTestSeam *test_seam;       // Borrowed, internal test observer only
#endif
} ofx_info ;

class OfxOperationSection
{
public:
    explicit OfxOperationSection (GncSessionOperationContext *context,
                                  gboolean cleanup = FALSE)
        : m_context {gnc_session_operation_context_ref (context)},
          m_active {cleanup ? gnc_session_operation_context_begin_cleanup (m_context)
                            : gnc_session_operation_context_begin (m_context)}
    {
    }

    ~OfxOperationSection ()
    {
        end ();
        gnc_session_operation_context_unref (m_context);
    }

    explicit operator bool () const { return m_active; }
    void end ()
    {
        if (!m_active)
            return;
        gnc_session_operation_context_end (m_context);
        m_active = FALSE;
    }
    OfxOperationSection (const OfxOperationSection&) = delete;
    OfxOperationSection& operator= (const OfxOperationSection&) = delete;

private:
    GncSessionOperationContext *m_context;
    gboolean m_active;
};

struct OfxAccountSelection
{
    std::string online_id;
    std::string description;
    GncGUID commodity_guid;
    GNCAccountType account_type;
};

struct OfxSecuritySelection
{
    std::string unique_id;
    std::string unique_id_type;
    std::string fullname;
    std::string mnemonic;
};

struct OfxInvestmentSelection
{
    std::string online_id;
    std::string account_id;
    std::string security_id;
    std::string security_name;
    std::string currency;
    gboolean needs_income;
};

/* The reconcile flow outlives libofx_proc_file(), so it keeps only copied
 * statement values, never an OfxStatementData or one of its string pointers. */
struct OfxStatementSelection
{
    std::string account_id;
    gboolean ledger_balance_valid;
    double ledger_balance;
    time64 ledger_balance_date;
};

/* LibOFX owns all callback data only for the duration of the callback. This
 * state deliberately contains copied values and GUIDs, never LibOFX pointers
 * or a live parser context. */
struct OfxImportState
{
    ofx_info *info;
    GncOfxImportAsyncState *registration;
    GWeakRef parent;
    gulong parent_destroy_handler;
    gboolean has_parent;
    gboolean parent_destroyed;
    GncGUID book_guid;
    std::vector<OfxAccountSelection> accounts;
    std::vector<OfxSecuritySelection> securities;
    std::vector<OfxInvestmentSelection> investments;
    size_t account_index;
    size_t security_index;
    size_t investment_index;
    size_t income_index;
    std::unordered_map<std::string, GncGUID> account_guids;
    std::unordered_map<std::string, GncGUID> commodity_guids;
    std::unordered_map<std::string, GncGUID> investment_guids;
    std::unordered_map<std::string, GncGUID> income_guids;
    GncGUID last_investment_guid;
    GncGUID last_income_guid;
    GncGUID investment_parent_guid;
};

#ifdef GNC_OFX_IMPORT_TEST_SEAM
struct GncOfxImportTestSeam
{
    ofx_info *info;
    OfxImportState *state;
    guint metadata_cleanup_calls;
    guint payload_destroy_calls;
    guint reconcile_calls;
    GncImportOperationTeardownResult result;
};
#endif

static std::string
ofx_utf8_string (const char *value)
{
    if (!value)
        return {};
    auto utf8 = gnc_utf8_strip_invalid_strdup (value);
    std::string result {utf8};
    g_free (utf8);
    return result;
}

static gboolean
ofx_import_state_book_is_current (const OfxImportState *state)
{
    if (!state || !state->info || !state->registration ||
        !gnc_ofx_import_async_state_is_active (state->registration) ||
        state->info->lifecycle !=
            gnc_ofx_import_async_state_get_lifecycle (
                state->registration) ||
        !gnc_session_operation_context_is_current (
            state->info->operation_context))
        return FALSE;
    auto book = gnc_get_current_book ();
    return book && guid_equal (&state->book_guid,
                               qof_instance_get_guid (QOF_INSTANCE (book)));
}

static Account *
ofx_import_state_account (const OfxImportState *state, const GncGUID &guid)
{
    auto account = ofx_import_state_book_is_current (state) &&
                   !guid_equal (&guid, guid_null ())
        ? xaccAccountLookup (&guid, gnc_get_current_book ()) : nullptr;
    return account && !qof_instance_get_destroying (QOF_INSTANCE (account))
        ? account : nullptr;
}

static gboolean
ofx_import_state_account_is_current (const OfxImportState *state,
                                     const Account *account)
{
    return ofx_import_state_book_is_current (state) && account &&
           gnc_account_get_book (account) == gnc_get_current_book () &&
           !qof_instance_get_destroying (QOF_INSTANCE (account));
}

static gboolean
ofx_import_state_commodity_is_current (const OfxImportState *state,
                                       const gnc_commodity *commodity)
{
    return ofx_import_state_book_is_current (state) && commodity &&
           qof_instance_get_book (QOF_INSTANCE (commodity)) == gnc_get_current_book () &&
           !qof_instance_get_destroying (QOF_INSTANCE (commodity));
}

static gnc_commodity *
ofx_import_state_commodity (const OfxImportState *state, const std::string &key)
{
    if (!state || !ofx_import_state_book_is_current (state))
        return nullptr;
    auto iterator = state->commodity_guids.find (key);
    if (iterator == state->commodity_guids.end ())
        return nullptr;
    auto commodity = gnc_commodity_find_commodity_by_guid (&iterator->second,
                                                            gnc_get_current_book ());
    return ofx_import_state_commodity_is_current (state, commodity) ? commodity : nullptr;
}

static Account *
ofx_import_state_mapped_account (const OfxImportState *state,
                                 const std::unordered_map<std::string, GncGUID> &map,
                                 const std::string &key)
{
    auto iterator = map.find (key);
    return iterator == map.end () ? nullptr : ofx_import_state_account (state, iterator->second);
}

static void
ofx_import_state_store_account (OfxImportState *state,
                                std::unordered_map<std::string, GncGUID> &map,
                                const std::string &key, const Account *account)
{
    if (ofx_import_state_account_is_current (state, account))
        map[key] = *xaccAccountGetGUID (account);
}

static void
ofx_import_state_store_commodity (OfxImportState *state, const std::string &key,
                                  const gnc_commodity *commodity)
{
    if (ofx_import_state_commodity_is_current (state, commodity))
        state->commodity_guids[key] = *qof_instance_get_guid (QOF_INSTANCE (commodity));
}

static void runMatcher(ofx_info* info, char * selected_filename, gboolean go_to_next_file);

/*
int ofx_proc_status_cb(struct OfxStatusData data)
{
  return 0;
}
*/

static const char *PROP_OFX_INCOME_ACCOUNT = "ofx-income-account";

static Account*
get_associated_income_account(const Account* investment_account)
{
    GncGUID *income_guid = NULL;
    Account *acct = NULL;
    g_assert(investment_account);
    qof_instance_get (QOF_INSTANCE (investment_account),
                      PROP_OFX_INCOME_ACCOUNT, &income_guid,
                      NULL);
    if (income_guid)
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

int
ofx_proc_security_cb (const struct OfxSecurityData data, void *security_user_data)
{
    auto state = static_cast<OfxImportState *> (security_user_data);
    if (!data.unique_id_valid)
        return 0;

    auto unique_id = ofx_utf8_string (data.unique_id);
    if (!ofx_import_state_commodity (state, unique_id))
        PERR ("No preselected commodity for OFX security %s", unique_id.c_str ());
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

static std::string
ofx_investment_key (const OfxTransactionData *data)
{
    if (!data || !data->account_id_valid || !data->unique_id_valid)
        return {};
    return ofx_utf8_string (data->account_id) + ofx_utf8_string (data->unique_id);
}

static Account *
ofx_preselected_investment_account (OfxImportState *state,
                                    const OfxTransactionData *data)
{
    return ofx_import_state_mapped_account (state, state->investment_guids,
                                            ofx_investment_key (data));
}

static Account *
ofx_preselected_income_account (OfxImportState *state,
                                const OfxTransactionData *data)
{
    return ofx_import_state_mapped_account (state, state->income_guids,
                                            ofx_investment_key (data));
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

static void
process_investment_transaction (Transaction *transaction, Account *import_account,
                                OfxTransactionData *data, OfxImportState *state)
{
    Account *investment_account;
    Account *income_account;
    auto investment_commodity = ofx_import_state_commodity (
        state, ofx_utf8_string (data->unique_id));
    double amount = data->amount;

    g_return_if_fail (data->invtransactiontype_valid);
    if (!investment_commodity)
    {
        PERR ("No preselected commodity for the investment transaction");
        return;
    }

    investment_account = ofx_preselected_investment_account (state, data);
    if (!investment_account)
    {
        PERR ("No preselected investment asset account");
        return;
    }
    auto investment_id = ofx_investment_key (data);
    if (investment_id.empty ())
    {
        PERR ("No preselected investment online ID");
        return;
    }
    xaccAccountSetOnlineID (investment_account, investment_id.c_str ());

    if (data->invtransactiontype != OFX_REINVEST)
        add_currency_split (transaction, import_account,
                            -ofx_get_investment_amount (data), data);

    if (data->invtransactiontype != OFX_INCOME)
    {
        if (data->unitprice_valid && data->units_valid)
            add_investment_split (transaction, investment_account, data);
        else
            PERR ("Unable to add investment split, unit price or units were invalid.");
    }

    if (data->invtransactiontype != OFX_REINVEST && data->invtransactiontype != OFX_INCOME)
        return;

#ifdef HAVE_LIBOFX_VERSION_0_10
    if (data->currency_ratio_valid && data->currency_ratio != 0)
        amount *= data->currency_ratio;
#endif
    income_account = ofx_preselected_income_account (state, data);
    if (!income_account)
    {
        PERR ("No preselected investment income account");
        return;
    }
    set_associated_income_account (investment_account, income_account);

    if (data->invtransactiontype == OFX_REINVEST)
        add_currency_split (transaction, income_account, amount, data);
    else
        add_currency_split (transaction, income_account, -amount, data);
}
int ofx_proc_transaction_cb(OfxTransactionData data, void *user_data)
{
    Account *import_account;
    gnc_commodity *currency = NULL;
    QofBook *book;
    Transaction *transaction;
        auto state = static_cast<OfxImportState *> (user_data);
    auto info = state ? state->info : nullptr;

    if (!info || !ofx_import_state_book_is_current (state))
        return 0;

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

    auto account_id = ofx_utf8_string (data.account_id);
    import_account = ofx_import_state_mapped_account (state, state->account_guids,
                                                      account_id);
    if (import_account == NULL)
    {
        PERR("Unable to find account for id %s", data.account_id);
        return 0;
    }
    info->last_import_account = import_account;
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
        process_investment_transaction(transaction, import_account,
                                       &data, state);
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


static void
ofx_statement_selection_free (gpointer data)
{
    delete static_cast<OfxStatementSelection *> (data);
}

int ofx_proc_statement_cb (struct OfxStatementData data, void * statement_user_data)
{
    auto state = static_cast<OfxImportState *> (statement_user_data);
    auto info = state ? state->info : nullptr;
    if (!info || !ofx_import_state_book_is_current (state))
        return 0;

    auto statement = new OfxStatementSelection {
        data.account_id_valid ? ofx_utf8_string (data.account_id) : std::string {},
        data.ledger_balance_valid,
        data.ledger_balance,
        data.ledger_balance_date
    };
    info->statement = g_list_prepend (info->statement, statement);
    return 0;
}


static void
ofx_account_defaults (const struct OfxAccountData& data,
                      GNCAccountType *default_type,
                      const gchar **account_type_name)
{
    *default_type = ACCT_TYPE_NONE;
    *account_type_name = _("Unknown OFX account");

    if (!data.account_type_valid)
        return;

    switch (data.account_type)
    {
    case OfxAccountData::OFX_CHECKING:
    case OfxAccountData::OFX_SAVINGS:
        *default_type = ACCT_TYPE_BANK;
        *account_type_name = data.account_type == OfxAccountData::OFX_CHECKING
            ? _("Unknown OFX checking account")
            : _("Unknown OFX savings account");
        break;
    case OfxAccountData::OFX_MONEYMRKT:
        *default_type = ACCT_TYPE_MONEYMRKT;
        *account_type_name = _("Unknown OFX money market account");
        break;
    case OfxAccountData::OFX_CREDITLINE:
        *default_type = ACCT_TYPE_CREDITLINE;
        *account_type_name = _("Unknown OFX credit line account");
        break;
    case OfxAccountData::OFX_CMA:
        *account_type_name = _("Unknown OFX CMA account");
        break;
    case OfxAccountData::OFX_CREDITCARD:
        *default_type = ACCT_TYPE_CREDIT;
        *account_type_name = _("Unknown OFX credit card account");
        break;
    case OfxAccountData::OFX_INVESTMENT:
        *default_type = ACCT_TYPE_BANK;
        *account_type_name = _("Unknown OFX investment account");
        break;
    default:
        PERR("ofx_proc_account(): unknown OFX account type");
        break;
    }
}

static gnc_commodity *
ofx_account_default_commodity (const struct OfxAccountData& data)
{
    if (!data.currency_valid)
        return NULL;

    DEBUG("Currency from libofx: %s", data.currency);
    return gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                       GNC_COMMODITY_NS_CURRENCY,
                                       data.currency);
}

static gchar *
ofx_account_description (const struct OfxAccountData& data,
                         const gchar *account_type_name)
{
    gchar *account_name = data.account_id_valid
        ? gnc_utf8_strip_invalid_strdup (data.account_name) : g_strdup ("");
    gchar *description = g_strdup_printf ("%s \"%s\"", account_type_name,
                                          account_name);

    g_free (account_name);
    return description;
}

int
ofx_proc_account_cb (struct OfxAccountData data, void *account_user_data)
{
    auto state = static_cast<OfxImportState *> (account_user_data);
    if (!data.account_id_valid || !ofx_import_state_book_is_current (state))
        return 0;

    auto online_id = ofx_utf8_string (data.account_id);
    auto account = ofx_import_state_mapped_account (state, state->account_guids,
                                                     online_id);
    if (account)
    {
        xaccAccountSetOnlineID (account, online_id.c_str ());
        state->info->last_import_account = account;
    }
    else
        PERR ("No preselected account for OFX online ID %s", online_id.c_str ());
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
gnc_file_ofx_import_parse_current_file (OfxImportState *state);
static void
ofx_info_free (gpointer user_data);
static void
ofx_info_release (ofx_info *info);
static void
gnc_ofx_abort_import (ofx_info *info);
static void
gnc_ofx_match_done (GtkWidget *widget, gpointer user_data);

// gnc_ofx_process_next_file processes the next file in the info->file_list.
static void
gnc_ofx_process_next_file (GtkWidget *widget, gpointer user_data)
{
    ofx_info* info = (ofx_info*) user_data;
    if (!info || info->parent_destroyed ||
        !gnc_session_operation_context_is_current (info->operation_context))
    {
        gnc_ofx_abort_import (info);
        return;
    }
    // Free the statement (if it was allocated)
    g_list_free_full (info->statement, ofx_statement_selection_free);
    info->statement = NULL;

    // Done with the previous OFX file, process the next one if any.
    auto *completed_filename = static_cast<gchar*> (info->file_list->data);
    info->file_list = g_slist_delete_link (info->file_list, info->file_list);
    g_free (completed_filename);
    if (info->file_list)
        gnc_file_ofx_import_process_file (info);
    else
    {
        // Final cleanup.
        ofx_info_release (info);
    }
    (void)widget;
}

static void
gnc_ofx_matcher_finished (gboolean accepted, gpointer user_data)
{
    auto info = static_cast<ofx_info *> (user_data);
    if (!info)
        return;
    info->gnc_ofx_importer_gui = nullptr;
    info->response = accepted ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL;
    gnc_ofx_match_done (nullptr, info);
}

static gboolean
gnc_ofx_create_matcher (ofx_info *info)
{
    if (!info ||
        !gnc_session_operation_context_is_current (info->operation_context))
        return FALSE;
    info->gnc_ofx_importer_gui = gnc_gen_trans_list_new (
        GTK_WIDGET (info->parent), nullptr, FALSE, 42, FALSE);
    if (!info->gnc_ofx_importer_gui)
        return FALSE;
    if (gnc_gen_trans_list_bind_operation_teardown (
            info->gnc_ofx_importer_gui,
            gnc_ofx_import_lifecycle_get_teardown (info->lifecycle)))
        return TRUE;
    gnc_gen_trans_list_delete (info->gnc_ofx_importer_gui);
    info->gnc_ofx_importer_gui = nullptr;
    return FALSE;
}

static void
gnc_ofx_reconcile_destroyed (GObject *window, gpointer user_data)
{
    auto info = static_cast<ofx_info *> (user_data);
#ifdef GNC_OFX_IMPORT_TEST_SEAM
    if (info && info->test_seam)
        info->test_seam->reconcile_calls++;
#endif
    gnc_ofx_match_done (GTK_WIDGET (window), info);
}

static void
gnc_ofx_match_done (GtkWidget *widget, gpointer user_data)
{
    ofx_info* info = (ofx_info*) user_data;

    if (!info || info->parent_destroyed ||
        !gnc_session_operation_context_is_current (info->operation_context))
    {
        gnc_ofx_abort_import (info);
        return;
    }

    /* The the user did not click OK, don't process the rest of the
     * transaction, don't go to the next of xfile.
     */
    if (info->response != GTK_RESPONSE_OK)
    {
        gnc_ofx_abort_import (info);
        return;
    }

    if (info->trans_list)
    {
         /* Re-run the match dialog if there are transactions
          * remaining in our list (happens if several accounts exist
          * in the same ofx).
          */
        if (!gnc_ofx_create_matcher (info))
        {
            gnc_ofx_abort_import (info);
            return;
        }
        runMatcher (info, NULL, true);
        return;
    }

    if (info->run_reconcile && info->statement && info->statement->data)
    {
        auto statement = static_cast<OfxStatementSelection *> (info->statement->data);
        // Open a reconcile window.
        Account* account = gnc_import_select_account (GTK_WIDGET (info->parent),
                                                      statement->account_id.c_str (),
                                                      0, NULL, NULL, ACCT_TYPE_NONE, NULL, NULL);
        if (account && statement->ledger_balance_valid)
        {
            gnc_numeric value = double_to_gnc_numeric (statement->ledger_balance,
                                                       xaccAccountGetCommoditySCU (account),
                                                       GNC_HOW_RND_ROUND_HALF_UP);

            RecnWindow* rec_window = recnWindowWithBalance (GTK_WIDGET (info->parent), account, value,
                                                            statement->ledger_balance_date);

            /* The continuation owns a lifecycle reference and is centrally
             * disconnected by terminal cleanup before the payload can die. */
            if (!gnc_ofx_import_lifecycle_connect_destroy (
                    info->lifecycle,
                    G_OBJECT (gnc_ui_reconcile_window_get_window (rec_window)),
                    gnc_ofx_reconcile_destroyed, info))
            {
                gnc_ofx_abort_import (info);
                return;
            }
            if (info->statement->next)
                info->statement = info->statement->next;
            else
            {
                g_list_free_full (g_list_first (info->statement), ofx_statement_selection_free);
                info->statement = NULL;
            }
            return;
        }
    }
    else
    {
        if (info->statement && info->statement->next)
        {
            info->statement = info->statement->next;
            gnc_ofx_match_done (widget, user_data);
            return;
        }
        else
        {
            g_list_free_full (g_list_first (info->statement), ofx_statement_selection_free);
            info->statement = NULL;
        }
    }
    gnc_ofx_process_next_file (NULL, info);
}

// This callback is triggered when the user checks or unchecks the reconcile after match
// check box in the matching dialog.
static void
reconcile_when_close_toggled_cb (GtkCheckButton *togglebutton, ofx_info* info)
{
    info->run_reconcile = gtk_check_button_get_active (togglebutton);
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
    if (!info || info->parent_destroyed ||
        !gnc_session_operation_context_is_current (info->operation_context))
    {
        gnc_ofx_abort_import (info);
        return;
    }
    OfxOperationSection operation {info->operation_context};
    if (!operation)
    {
        gnc_ofx_abort_import (info);
        return;
    }
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
            if (gnc_gen_trans_list_add_trans_with_operation (
                    info->gnc_ofx_importer_gui, trans))
                info->num_trans_processed ++;
            else
                trans_list_remain = g_list_prepend (trans_list_remain, trans);
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
        gnc_gen_trans_list_delete_with_operation (info->gnc_ofx_importer_gui);
        info->gnc_ofx_importer_gui = nullptr;
        if (info->num_trans_processed)
            gnc_info_dialog (parent, _("While importing transactions from OFX file '%s' found %d previously imported transactions, no new transactions."),
                             selected_filename,
                             info->num_trans_processed);
        info->response = GTK_RESPONSE_OK;
        operation.end ();
        gnc_ofx_match_done (nullptr, info);
        return;
    }
    else
    {
        gnc_gen_trans_list_show_all (info->gnc_ofx_importer_gui);
        
        // Show or hide the check box for reconciling after match,
        // depending on whether a statement was received.
        gnc_gen_trans_list_show_reconcile_after_close_button (info->gnc_ofx_importer_gui,
                                                              info->statement != NULL,
                                                              info->run_reconcile);
        
        // Finally connect to the reconcile after match check box so
        // we can be notified if the user wants/does not want to
        // reconcile.
        g_signal_connect (G_OBJECT (gnc_gen_trans_list_get_reconcile_after_close_button
                                    (info->gnc_ofx_importer_gui)),
                          "toggled",
                          G_CALLBACK (reconcile_when_close_toggled_cb),
                          info);
        gnc_gen_trans_list_present (info->gnc_ofx_importer_gui,
                                    gnc_ofx_matcher_finished, info);
    }
}

static void
ofx_info_parent_destroyed (GtkWidget *window, gpointer user_data)
{
    auto info = static_cast<ofx_info *> (user_data);
    if (info)
    {
        info->parent_destroyed = TRUE;
        gnc_ofx_abort_import (info);
    }
    (void)window;
}

static void
ofx_info_free (gpointer user_data)
{
    auto info = static_cast<ofx_info *> (user_data);
    if (!info)
        return;
    if (info->parent && info->parent_destroy_handler)
        g_signal_handler_disconnect (info->parent, info->parent_destroy_handler);
    g_clear_object (&info->parent);
#ifdef GNC_OFX_IMPORT_TEST_SEAM
    if (info->test_seam)
    {
        info->test_seam->info = nullptr;
        info->test_seam->payload_destroy_calls++;
    }
#endif
    g_free (info);
}

static void
ofx_info_release (ofx_info *info)
{
    if (!info || !info->lifecycle)
        return;
    auto lifecycle = info->lifecycle;
    info->lifecycle = nullptr;
    info->operation_context = nullptr;
    gnc_ofx_import_lifecycle_finish (lifecycle);
}

static void
gnc_ofx_abort_import_metadata_cleanup (
    GncOfxImportLifecycle *lifecycle,
    GncImportOperationTeardownResult result,
    gpointer user_data)
{
    auto info = static_cast<ofx_info *> (user_data);
#ifdef GNC_OFX_IMPORT_TEST_SEAM
    if (info->test_seam)
    {
        info->test_seam->metadata_cleanup_calls++;
        info->test_seam->result = result;
    }
#endif
    g_assert (!info->gnc_ofx_importer_gui);
    g_assert (!info->trans_list);
    g_list_free_full (info->statement, ofx_statement_selection_free);
    info->statement = nullptr;
    g_slist_free_full (info->file_list, g_free);
    info->file_list = nullptr;
    g_assert (lifecycle == info->lifecycle);
    (void)result;
}

static void
gnc_ofx_abort_import (ofx_info *info)
{
    if (!info || !info->lifecycle)
        return;
    gnc_ofx_import_lifecycle_request (info->lifecycle);
}

static void
ofx_import_state_parent_destroyed (GtkWidget *window, gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (state)
        state->parent_destroyed = TRUE;
    (void)window;
}

static void
ofx_import_state_free (OfxImportState *state)
{
    if (!state)
        return;
    auto parent = GTK_WINDOW (g_weak_ref_get (&state->parent));
    if (parent && state->parent_destroy_handler)
        g_signal_handler_disconnect (parent, state->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&state->parent);
    auto registration = state->registration;
    state->registration = nullptr;
    state->info = nullptr;
    delete state;
    gnc_ofx_import_async_state_unref (registration);
}

static void
ofx_import_state_abort (OfxImportState *state)
{
    if (!state)
        return;
    gnc_ofx_import_async_state_request_teardown (state->registration);
    ofx_import_state_free (state);
}

static gboolean
ofx_import_state_ready (OfxImportState *state)
{
    if (!ofx_import_state_book_is_current (state) || state->parent_destroyed)
        return FALSE;
    auto parent = GTK_WINDOW (g_weak_ref_get (&state->parent));
    auto ready = !state->has_parent || parent != nullptr;
    g_clear_object (&parent);
    return ready;
}

static GtkWindow *
ofx_import_state_parent (OfxImportState *state)
{
    return state ? GTK_WINDOW (g_weak_ref_get (&state->parent)) : nullptr;
}

static gnc_commodity *
ofx_account_selection_commodity (OfxImportState *state,
                                 const OfxAccountSelection &selection)
{
    if (!ofx_import_state_book_is_current (state) ||
        guid_equal (&selection.commodity_guid, guid_null ()))
        return nullptr;
    auto commodity = gnc_commodity_find_commodity_by_guid (&selection.commodity_guid,
                                                            gnc_get_current_book ());
    return ofx_import_state_commodity_is_current (state, commodity) ? commodity : nullptr;
}

static gnc_commodity *
ofx_create_commodity (const OfxSecuritySelection &selection)
{
    auto commodity = gnc_import_find_commodity_by_cusip (selection.unique_id.c_str ());
    if (commodity)
        return commodity;

    auto book = gnc_get_current_book ();
    auto name_space = selection.unique_id_type.empty () ? nullptr
                                                        : selection.unique_id_type.c_str ();
    commodity = gnc_commodity_new (book, selection.fullname.c_str (), name_space,
                                   selection.mnemonic.c_str (),
                                   selection.unique_id.c_str (), 1);
    if (!commodity)
        return nullptr;
    gnc_commodity_begin_edit (commodity);
    gnc_commodity_user_set_quote_flag (commodity, TRUE);
    auto source = gnc_quote_source_lookup_by_ti (SOURCE_SINGLE, 0);
    gnc_commodity_set_quote_source (commodity, source);
    gnc_commodity_commit_edit (commodity);
    gnc_commodity_table_insert (gnc_get_current_commodities (), commodity);
    return commodity;
}

static int
ofx_collect_account_cb (struct OfxAccountData data, void *user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    GNCAccountType account_type;
    const gchar *account_type_name;

    if (!data.account_id_valid)
        return 0;
    auto online_id = ofx_utf8_string (data.account_id);
    for (const auto &selection : state->accounts)
        if (selection.online_id == online_id)
            return 0;

    ofx_account_defaults (data, &account_type, &account_type_name);
    auto description = ofx_account_description (data, account_type_name);
    auto commodity = ofx_account_default_commodity (data);
    OfxAccountSelection selection {};
    selection.online_id = online_id;
    selection.description = description;
    selection.commodity_guid = commodity ? *qof_instance_get_guid (QOF_INSTANCE (commodity))
                                         : *guid_null ();
    selection.account_type = account_type;
    state->accounts.emplace_back (selection);
    g_free (description);
    return 0;
}

static int
ofx_collect_security_cb (const struct OfxSecurityData data, void *user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!data.unique_id_valid)
        return 0;

    auto unique_id = ofx_utf8_string (data.unique_id);
    auto fullname = data.secname_valid ? ofx_utf8_string (data.secname) : std::string {};
    auto mnemonic = data.ticker_valid ? ofx_utf8_string (data.ticker) : std::string {};
    auto unique_id_type = data.unique_id_type_valid ? ofx_utf8_string (data.unique_id_type)
                                                    : std::string {};
    for (auto &selection : state->securities)
    {
        if (selection.unique_id != unique_id)
            continue;
        if (selection.fullname.empty ()) selection.fullname = fullname;
        if (selection.mnemonic.empty ()) selection.mnemonic = mnemonic;
        if (selection.unique_id_type.empty ()) selection.unique_id_type = unique_id_type;
        return 0;
    }
    state->securities.emplace_back (OfxSecuritySelection {unique_id, unique_id_type,
                                                           fullname, mnemonic});
    return 0;
}

static int
ofx_collect_transaction_cb (OfxTransactionData data, void *user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!data.invtransactiontype_valid || !data.account_id_valid || !data.unique_id_valid ||
        !data.security_data_valid || !data.security_data_ptr ||
        !data.security_data_ptr->secname_valid)
        return 0;

    auto account_id = ofx_utf8_string (data.account_id);
    auto security_id = ofx_utf8_string (data.unique_id);
    auto security_name = ofx_utf8_string (data.security_data_ptr->secname);
    auto online_id = account_id + security_id;
    auto needs_income = data.invtransactiontype == OFX_REINVEST ||
                        data.invtransactiontype == OFX_INCOME;
    auto currency = data.account_ptr && data.account_ptr->currency_valid
        ? ofx_utf8_string (data.account_ptr->currency) : std::string {};

    gboolean has_security = FALSE;
    for (const auto &security : state->securities)
        if (security.unique_id == security_id)
        {
            has_security = TRUE;
            break;
        }
    if (!has_security)
        state->securities.emplace_back (OfxSecuritySelection {security_id, {}, security_name, {}});

    for (auto &selection : state->investments)
    {
        if (selection.online_id != online_id)
            continue;
        selection.needs_income |= needs_income;
        if (selection.currency.empty ()) selection.currency = currency;
        return 0;
    }
    state->investments.emplace_back (OfxInvestmentSelection {online_id, account_id,
        security_id, security_name, currency, needs_income});
    return 0;
}

static void ofx_import_state_continue (OfxImportState *state);
static void gnc_file_ofx_import_parse_current_file (OfxImportState *state);

static void
ofx_import_state_account_selected (Account *account, gboolean accepted,
                                   gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_account_is_current (state, account) || !accepted)
    {
        ofx_import_state_abort (state);
        return;
    }
    const auto &selection = state->accounts[state->account_index];
    ofx_import_state_store_account (state, state->account_guids, selection.online_id, account);
    state->info->last_import_account = account;
    ++state->account_index;
    ofx_import_state_continue (state);
}

static void
ofx_import_state_security_selected (gnc_commodity *commodity, gboolean accepted,
                                    gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_commodity_is_current (state, commodity) || !accepted)
    {
        ofx_import_state_abort (state);
        return;
    }
    ofx_import_state_store_commodity (state,
                                      state->securities[state->security_index].unique_id,
                                      commodity);
    ++state->security_index;
    ofx_import_state_continue (state);
}

static void
ofx_import_state_accept_investment (OfxImportState *state, Account *account)
{
    const auto &selection = state->investments[state->investment_index];
    auto parent = gnc_account_get_parent (account);
    ofx_import_state_store_account (state, state->investment_guids, selection.online_id, account);
    state->last_investment_guid = *xaccAccountGetGUID (account);
    state->info->last_investment_account = account;
    if (guid_equal (&state->investment_parent_guid, guid_null ()) && parent &&
        !gnc_account_is_root (parent) &&
        xaccAccountTypesCompatible (xaccAccountGetType (parent), ACCT_TYPE_STOCK))
        state->investment_parent_guid = *xaccAccountGetGUID (parent);
    ++state->investment_index;
    ofx_import_state_continue (state);
}

static void
ofx_import_state_retry_investment (GtkWindow *parent, gint response, gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_ready (state) || response != GTK_RESPONSE_YES)
        ofx_import_state_abort (state);
    else
        ofx_import_state_continue (state);
    (void)parent;
}

static void
ofx_import_state_investment_created (Account *account, gboolean accepted,
                                     gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_account_is_current (state, account) || !accepted)
    {
        ofx_import_state_abort (state);
        return;
    }
    ofx_import_state_accept_investment (state, account);
}

static void
ofx_import_state_create_investment (OfxImportState *state, Account *parent,
                                    gnc_commodity *commodity)
{
    const auto &selection = state->investments[state->investment_index];
    auto description = g_strdup_printf (_("Stock account for security \"%s\""),
                                        selection.security_name.c_str ());
    GList *types = g_list_prepend (nullptr, GINT_TO_POINTER (ACCT_TYPE_STOCK));
    if (!xaccAccountTypesCompatible (xaccAccountGetType (parent), ACCT_TYPE_STOCK))
        types = g_list_prepend (types, GINT_TO_POINTER (xaccAccountGetType (parent)));
    auto window = ofx_import_state_parent (state);
    gnc_ui_new_accounts_from_name_with_defaults_async_with_operation_context (
        window, description, types, commodity, parent,
        state->info->operation_context, ofx_import_state_investment_created,
        state);
    g_clear_object (&window);
    g_list_free (types);
    g_free (description);
}

static void
ofx_import_state_investment_selected (Account *account, gboolean accepted,
                                      gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_account_is_current (state, account) || !accepted)
    {
        ofx_import_state_abort (state);
        return;
    }
    const auto &selection = state->investments[state->investment_index];
    auto commodity = ofx_import_state_commodity (state, selection.security_id);
    if (commodity && xaccAccountGetCommodity (account) == commodity)
    {
        ofx_import_state_accept_investment (state, account);
        return;
    }

    auto parent = ofx_import_state_account (state, state->investment_parent_guid);
    if (auto_create_commodity && parent && commodity)
    {
        ofx_import_state_create_investment (state, parent, commodity);
        return;
    }
    auto window = ofx_import_state_parent (state);
    gnc_verify_dialog_async (
        window, TRUE, ofx_import_state_retry_investment, state,
        _("The chosen account \"%s\" does not have the correct currency/security \"%s\" "
          "(it has \"%s\" instead). This account cannot be used. "
          "Do you want to choose again?"),
        xaccAccountGetName (account), gnc_commodity_get_fullname (commodity),
        gnc_commodity_get_fullname (xaccAccountGetCommodity (account)));
    g_clear_object (&window);
}

static void
ofx_import_state_income_selected (Account *account, gboolean accepted,
                                  gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!ofx_import_state_account_is_current (state, account) || !accepted)
    {
        ofx_import_state_abort (state);
        return;
    }
    const auto &selection = state->investments[state->income_index];
    auto investment = ofx_import_state_mapped_account (state, state->investment_guids,
                                                        selection.online_id);
    if (!investment)
    {
        ofx_import_state_abort (state);
        return;
    }
    ofx_import_state_store_account (state, state->income_guids, selection.online_id, account);
    state->last_income_guid = *xaccAccountGetGUID (account);
    state->info->last_income_account = account;
    ++state->income_index;
    ofx_import_state_continue (state);
}

static void
ofx_import_state_continue (OfxImportState *state)
{
    if (!ofx_import_state_ready (state))
    {
        ofx_import_state_abort (state);
        return;
    }

    while (state->account_index < state->accounts.size ())
    {
        const auto &selection = state->accounts[state->account_index];
        auto account = gnc_import_select_account (nullptr, selection.online_id.c_str (), FALSE,
                                                  nullptr, nullptr, selection.account_type,
                                                  nullptr, nullptr);
        if (account)
        {
            if (!ofx_import_state_account_is_current (state, account))
            {
                ofx_import_state_abort (state);
                return;
            }
            ofx_import_state_store_account (state, state->account_guids, selection.online_id, account);
            state->info->last_import_account = account;
            ++state->account_index;
            continue;
        }
        auto window = ofx_import_state_parent (state);
        gnc_import_select_account_async_no_mutation_with_operation_context (
            GTK_WIDGET (window), selection.online_id.c_str (), TRUE,
            selection.description.c_str (), ofx_account_selection_commodity (state, selection),
            selection.account_type, nullptr, state->info->operation_context,
            ofx_import_state_account_selected, state);
        g_clear_object (&window);
        return;
    }

    while (state->security_index < state->securities.size ())
    {
        const auto &selection = state->securities[state->security_index];
        auto commodity = gnc_import_find_commodity_by_cusip (selection.unique_id.c_str ());
        if (!commodity && auto_create_commodity)
        {
            OfxOperationSection operation {state->info->operation_context};
            if (!operation)
            {
                ofx_import_state_abort (state);
                return;
            }
            commodity = ofx_create_commodity (selection);
        }
        if (commodity)
        {
            if (!ofx_import_state_commodity_is_current (state, commodity))
            {
                ofx_import_state_abort (state);
                return;
            }
            ofx_import_state_store_commodity (state, selection.unique_id, commodity);
            ++state->security_index;
            continue;
        }
        auto window = ofx_import_state_parent (state);
        gnc_import_select_commodity_async_with_operation_context (
            GTK_WIDGET (window), selection.unique_id.c_str (), TRUE,
            selection.fullname.c_str (), selection.mnemonic.c_str (), nullptr,
            state->info->operation_context, ofx_import_state_security_selected,
            state);
        g_clear_object (&window);
        return;
    }

    while (state->investment_index < state->investments.size ())
    {
        const auto &selection = state->investments[state->investment_index];
        auto commodity = ofx_import_state_commodity (state, selection.security_id);
        if (!commodity)
        {
            ofx_import_state_abort (state);
            return;
        }
        auto account = gnc_import_select_account (nullptr, selection.online_id.c_str (), FALSE,
                                                  nullptr, nullptr, ACCT_TYPE_STOCK,
                                                  nullptr, nullptr);
        if (account)
        {
            ofx_import_state_investment_selected (account, TRUE, state);
            return;
        }
        auto last = ofx_import_state_account (state, state->last_investment_guid);
        auto parent = last && xaccAccountGetCommodity (last) == commodity
            ? last : ofx_import_state_account (state, state->investment_parent_guid);
        auto window = ofx_import_state_parent (state);
        auto description = g_strdup_printf (_("Stock account for security \"%s\""),
                                            selection.security_name.c_str ());
        gnc_import_select_account_async_no_mutation_with_operation_context (
            GTK_WIDGET (window), selection.online_id.c_str (), TRUE, description, commodity,
            ACCT_TYPE_STOCK, parent, state->info->operation_context,
            ofx_import_state_investment_selected, state);
        g_free (description);
        g_clear_object (&window);
        return;
    }

    while (state->income_index < state->investments.size ())
    {
        const auto &selection = state->investments[state->income_index];
        if (!selection.needs_income)
        {
            ++state->income_index;
            continue;
        }
        auto investment = ofx_import_state_mapped_account (state, state->investment_guids,
                                                            selection.online_id);
        auto income = investment ? get_associated_income_account (investment) : nullptr;
        if (income)
        {
            if (!ofx_import_state_account_is_current (state, income))
            {
                ofx_import_state_abort (state);
                return;
            }
            ofx_import_state_store_account (state, state->income_guids, selection.online_id, income);
            ++state->income_index;
            continue;
        }
        auto currency = selection.currency.empty () ? nullptr :
            gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                        GNC_COMMODITY_NS_CURRENCY,
                                        selection.currency.c_str ());
        if (!currency)
        {
            auto source = ofx_import_state_mapped_account (state, state->account_guids,
                                                            selection.account_id);
            currency = source ? xaccAccountGetCommodity (source) : nullptr;
        }
        auto description = g_strdup_printf (_("Income account for security \"%s\""),
                                            selection.security_name.c_str ());
        auto window = ofx_import_state_parent (state);
        auto last = ofx_import_state_account (state, state->last_income_guid);
        gnc_import_select_account_async_no_mutation_with_operation_context (
            GTK_WIDGET (window), nullptr, TRUE, description, currency, ACCT_TYPE_INCOME, last,
            state->info->operation_context, ofx_import_state_income_selected,
            state);
        g_free (description);
        g_clear_object (&window);
        return;
    }

    if (!gnc_ofx_create_matcher (state->info))
    {
        ofx_import_state_abort (state);
        return;
    }
    gnc_file_ofx_import_parse_current_file (state);
}

static void
ofx_import_state_new_book_options_finished (GtkWindow *parent, gboolean applied,
                                            gpointer user_data)
{
    auto state = static_cast<OfxImportState *> (user_data);
    if (!applied || !ofx_import_state_ready (state))
        ofx_import_state_abort (state);
    else
        ofx_import_state_continue (state);
    (void)parent;
}

static OfxImportState *
ofx_import_state_new (ofx_info *info)
{
    if (!info || !info->lifecycle ||
        gnc_ofx_import_lifecycle_is_terminal (info->lifecycle))
        return nullptr;
    auto state = new OfxImportState {};
    state->info = info;
    state->registration = gnc_ofx_import_async_state_new (info->lifecycle);
    if (!state->registration)
    {
        delete state;
        return nullptr;
    }
    state->has_parent = info->parent != nullptr;
    state->parent_destroyed = info->parent_destroyed;
    g_weak_ref_init (&state->parent, info->parent);
    if (info->parent)
        state->parent_destroy_handler = g_signal_connect (
            info->parent, "destroy",
            G_CALLBACK (ofx_import_state_parent_destroyed), state);
    state->book_guid = *qof_instance_get_guid (QOF_INSTANCE (gnc_get_current_book ()));
    state->last_investment_guid = *guid_null ();
    state->last_income_guid = *guid_null ();
    state->investment_parent_guid = *guid_null ();
    return state;
}

static void
gnc_file_ofx_import_process_file (ofx_info *info)
{
    if (!info || !info->file_list)
        return;
    if (info->parent_destroyed ||
        !gnc_session_operation_context_is_current (info->operation_context))
    {
        gnc_ofx_abort_import (info);
        return;
    }

    auto state = ofx_import_state_new (info);
    if (!state)
    {
        gnc_ofx_abort_import (info);
        return;
    }

    auto context = libofx_get_new_context ();
    auto filename = static_cast<gchar *> (info->file_list->data);
#ifdef G_OS_WIN32
    auto parser_filename = g_win32_locale_filename_from_utf8 (filename);
#else
    auto parser_filename = filename;
#endif
    ofx_set_account_cb (context, ofx_collect_account_cb, state);
    ofx_set_security_cb (context, ofx_collect_security_cb, state);
    ofx_set_transaction_cb (context, ofx_collect_transaction_cb, state);
    libofx_proc_file (context, parser_filename, AUTODETECT);
    libofx_free_context (context);
#ifdef G_OS_WIN32
    g_free (parser_filename);
#endif

    if (!ofx_import_state_ready (state))
    {
        ofx_import_state_abort (state);
        return;
    }
    if (gnc_is_new_book ())
    {
        auto parent = ofx_import_state_parent (state);
        gnc_new_book_option_display_async (GTK_WIDGET (parent),
                                           ofx_import_state_new_book_options_finished,
                                           state);
        g_clear_object (&parent);
        return;
    }
    ofx_import_state_continue (state);
}

static void
gnc_file_ofx_import_parse_current_file (OfxImportState *state)
{
    auto info = state ? state->info : nullptr;
    if (!info || !info->file_list || !ofx_import_state_ready (state))
    {
        ofx_import_state_abort (state);
        return;
    }
    OfxOperationSection operation {info->operation_context};
    if (!operation)
    {
        ofx_import_state_abort (state);
        return;
    }

    auto filename = static_cast<char *> (info->file_list->data);
    auto libofx_context = libofx_get_new_context ();
#ifdef G_OS_WIN32
    auto selected_filename = g_win32_locale_filename_from_utf8 (filename);
#else
    auto selected_filename = filename;
#endif
    info->num_trans_processed = 0;
    info->statement = nullptr;
    ofx_set_statement_cb (libofx_context, ofx_proc_statement_cb, state);
    ofx_set_account_cb (libofx_context, ofx_proc_account_cb, state);
    ofx_set_transaction_cb (libofx_context, ofx_proc_transaction_cb, state);
    ofx_set_security_cb (libofx_context, ofx_proc_security_cb, state);
    libofx_proc_file (libofx_context, selected_filename, AUTODETECT);
    libofx_free_context (libofx_context);
    operation.end ();
    ofx_import_state_free (state);
    runMatcher (info, selected_filename, TRUE);
#ifdef G_OS_WIN32
    g_free (selected_filename);
#endif
}
// The main import function. Starts the chain of file imports (if there are several)
typedef struct
{
    GWeakRef parent;
    gboolean had_parent;
} OfxFileDialogData;

static void
ofx_file_dialog_data_free (OfxFileDialogData *data)
{
    g_weak_ref_clear (&data->parent);
    g_free (data);
}

static ofx_info *
ofx_info_new (GtkWindow *parent, GSList *selected_filenames,
              GApplication *application,
              GncSessionOperationContext *operation_context)
{
    g_return_val_if_fail (G_IS_APPLICATION (application), nullptr);
    g_return_val_if_fail (operation_context, nullptr);

    auto info = g_new0 (ofx_info, 1);
    info->num_trans_processed = 0;
    info->statement = nullptr;
    info->last_investment_account = nullptr;
    info->last_import_account = nullptr;
    info->last_income_account = nullptr;
    info->parent = parent ? GTK_WINDOW (g_object_ref (parent)) : nullptr;
    info->run_reconcile = FALSE;
    info->file_list = selected_filenames;
    info->trans_list = nullptr;
    info->response = 0;
    info->lifecycle = gnc_ofx_import_lifecycle_new (
        operation_context, application,
        &info->gnc_ofx_importer_gui, &info->trans_list,
        gnc_ofx_abort_import_metadata_cleanup, info, ofx_info_free);
    if (!info->lifecycle)
    {
        info->file_list = nullptr;
        ofx_info_free (info);
        return nullptr;
    }
    info->operation_context = gnc_ofx_import_lifecycle_get_context (
        info->lifecycle);
    if (info->parent)
        info->parent_destroy_handler = g_signal_connect (
            info->parent, "destroy", G_CALLBACK (ofx_info_parent_destroyed),
            info);
    return info;
}

static void
ofx_import_selected_files (GtkWindow *parent, GSList *selected_filenames)
{
    char *default_dir;

    if (!selected_filenames)
        return;

    /* Remember the directory as the default. */
    default_dir = g_path_get_dirname (static_cast<char *> (selected_filenames->data));
    gnc_set_default_directory (GNC_PREFS_GROUP, default_dir);
    g_free (default_dir);

    /* Look up the needed preferences. */
    auto_create_commodity =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_IMPORT, GNC_PREF_AUTO_COMMODITY);

    DEBUG ("Opening selected file(s)");
    if (!gnc_current_session_exist ())
    {
        g_slist_free_full (selected_filenames, g_free);
        return;
    }
    auto operation_context = gnc_session_operation_context_new (
        qof_session_get_book (gnc_get_current_session ()),
        QOF_SESSION_OPERATION_IMPORT);
    if (!operation_context)
    {
        PWARN ("Refusing OFX import while another session operation is active");
        g_slist_free_full (selected_filenames, g_free);
        return;
    }

    auto application = g_application_get_default ();
    if (!G_IS_APPLICATION (application))
    {
        PWARN ("Refusing asynchronous OFX import without a GApplication lifecycle");
        gnc_session_operation_context_unref (operation_context);
        g_slist_free_full (selected_filenames, g_free);
        return;
    }

    auto info = ofx_info_new (parent, selected_filenames,
                              G_APPLICATION (application),
                              operation_context);
    gnc_session_operation_context_unref (operation_context);
    if (!info)
    {
        g_slist_free_full (selected_filenames, g_free);
        return;
    }
    gnc_file_ofx_import_process_file (info);
}

#ifdef GNC_OFX_IMPORT_TEST_SEAM
GncOfxImportTestSeam *
gnc_ofx_import_test_seam_new (GApplication *application)
{
    g_return_val_if_fail (G_IS_APPLICATION (application), nullptr);
    if (!gnc_current_session_exist ())
        return nullptr;
    auto operation_context = gnc_session_operation_context_new (
        qof_session_get_book (gnc_get_current_session ()),
        QOF_SESSION_OPERATION_IMPORT);
    if (!operation_context)
        return nullptr;
    auto seam = g_new0 (GncOfxImportTestSeam, 1);
    seam->result = GNC_IMPORT_OPERATION_TEARDOWN_STALE;
    seam->info = ofx_info_new (nullptr, nullptr, application,
                               operation_context);
    gnc_session_operation_context_unref (operation_context);
    if (!seam->info)
    {
        g_free (seam);
        return nullptr;
    }
    seam->info->test_seam = seam;
    return seam;
}

void
gnc_ofx_import_test_seam_free (GncOfxImportTestSeam *seam)
{
    if (!seam)
        return;
    g_return_if_fail (!seam->info);
    g_return_if_fail (!seam->state);
    g_free (seam);
}

static gboolean
gnc_ofx_import_test_begin_state (GncOfxImportTestSeam *seam)
{
    g_return_val_if_fail (seam && seam->info && !seam->state, FALSE);
    seam->state = ofx_import_state_new (seam->info);
    return seam->state != nullptr;
}

gboolean
gnc_ofx_import_test_begin_account_state (GncOfxImportTestSeam *seam)
{
    return gnc_ofx_import_test_begin_state (seam);
}

gboolean
gnc_ofx_import_test_begin_commodity_state (GncOfxImportTestSeam *seam)
{
    return gnc_ofx_import_test_begin_state (seam);
}

void
gnc_ofx_import_test_parent_destroy (GncOfxImportTestSeam *seam)
{
    g_return_if_fail (seam && seam->info);
    ofx_info_parent_destroyed (nullptr, seam->info);
}

void
gnc_ofx_import_test_complete_account_cancel (GncOfxImportTestSeam *seam)
{
    g_return_if_fail (seam && seam->state);
    auto state = seam->state;
    seam->state = nullptr;
    ofx_import_state_account_selected (nullptr, FALSE, state);
}

void
gnc_ofx_import_test_complete_commodity_cancel (GncOfxImportTestSeam *seam)
{
    g_return_if_fail (seam && seam->state);
    auto state = seam->state;
    seam->state = nullptr;
    ofx_import_state_security_selected (nullptr, FALSE, state);
}

gboolean
gnc_ofx_import_test_create_matcher (GncOfxImportTestSeam *seam)
{
    g_return_val_if_fail (seam && seam->info, FALSE);
    return gnc_ofx_create_matcher (seam->info);
}

gboolean
gnc_ofx_import_test_add_open_transaction (GncOfxImportTestSeam *seam)
{
    g_return_val_if_fail (seam && seam->info, FALSE);
    auto book = gnc_get_current_book ();
    if (!book)
        return FALSE;
    auto transaction = xaccMallocTransaction (book);
    if (!transaction)
        return FALSE;
    xaccTransBeginEdit (transaction);
    seam->info->trans_list = g_list_append (seam->info->trans_list,
                                            transaction);
    return TRUE;
}

GtkWindow *
gnc_ofx_import_test_attach_reconcile (GncOfxImportTestSeam *seam)
{
    g_return_val_if_fail (seam && seam->info, nullptr);
    auto window = GTK_WINDOW (gtk_window_new ());
    g_object_ref_sink (window);
    if (!gnc_ofx_import_lifecycle_connect_destroy (
            seam->info->lifecycle, G_OBJECT (window),
            gnc_ofx_reconcile_destroyed, seam->info))
    {
        gtk_window_destroy (window);
        g_object_unref (window);
        return nullptr;
    }
    return window;
}

guint
gnc_ofx_import_test_metadata_cleanup_calls (const GncOfxImportTestSeam *seam)
{
    return seam ? seam->metadata_cleanup_calls : 0;
}

guint
gnc_ofx_import_test_payload_destroy_calls (const GncOfxImportTestSeam *seam)
{
    return seam ? seam->payload_destroy_calls : 0;
}

guint
gnc_ofx_import_test_reconcile_calls (const GncOfxImportTestSeam *seam)
{
    return seam ? seam->reconcile_calls : 0;
}

GncImportOperationTeardownResult
gnc_ofx_import_test_cleanup_result (const GncOfxImportTestSeam *seam)
{
    return seam ? seam->result : GNC_IMPORT_OPERATION_TEARDOWN_STALE;
}
#endif

static GSList *
ofx_file_list_from_selection (GListModel *files)
{
    GSList *selected_filenames = NULL;
    guint position;

    for (position = 0; position < g_list_model_get_n_items (files); position++)
    {
        GFile *file = G_FILE (g_list_model_get_item (files, position));
        gchar *filename = g_file_get_path (file);

        g_object_unref (file);
        if (!filename)
        {
            g_slist_free_full (selected_filenames, g_free);
            return NULL;
        }
        selected_filenames = g_slist_append (selected_filenames, filename);
    }

    return selected_filenames;
}

static void
ofx_file_dialog_finished (GObject *source, GAsyncResult *result,
                          gpointer user_data)
{
    OfxFileDialogData *data = static_cast<OfxFileDialogData *> (user_data);
    auto request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GListModel *files;
    GtkWindow *parent;
    GSList *selected_filenames;

    files = gnc_file_dialog_request_finish_multiple (request, result, &error);
    parent = GTK_WINDOW (g_weak_ref_get (&data->parent));
    if (!files)
    {
        if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
            gnc_error_dialog (parent, "%s", error->message);
        g_clear_error (&error);
        g_clear_object (&parent);
        ofx_file_dialog_data_free (data);
        return;
    }

    selected_filenames = ofx_file_list_from_selection (files);
    g_object_unref (files);
    if (!selected_filenames)
        gnc_error_dialog (parent, "%s", _("The selected file has no local path."));
    else if (parent || !data->had_parent)
        ofx_import_selected_files (parent, selected_filenames);
    else
        g_slist_free_full (selected_filenames, g_free);

    g_clear_object (&parent);
    ofx_file_dialog_data_free (data);
}

void
gnc_file_ofx_import (GtkWindow *parent)
{
    extern int ofx_PARSER_msg;
    extern int ofx_DEBUG_msg;
    extern int ofx_WARNING_msg;
    extern int ofx_ERROR_msg;
    extern int ofx_INFO_msg;
    extern int ofx_STATUS_msg;
    GncFileDialogRequest *request;
    OfxFileDialogData *data;
    GtkFileFilter *filter;
    GList *filters;
    char *default_dir;

    ofx_PARSER_msg = false;
    ofx_DEBUG_msg = false;
    ofx_WARNING_msg = true;
    ofx_ERROR_msg = true;
    ofx_INFO_msg = true;
    ofx_STATUS_msg = false;

    DEBUG ("gnc_file_ofx_import(): Begin...");
    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter,
                              _("Open/Quicken Financial Exchange file (*.ofx, *.qfx)"));
    gtk_file_filter_add_pattern (filter, "*.[oqOQ][fF][xX]");
    filters = g_list_prepend (NULL, filter);
    default_dir = gnc_get_default_directory (GNC_PREFS_GROUP);
    request = gnc_file_dialog_request_new (
        parent, _("Select one or multiple OFX/QFX file(s) to process"), filters,
        default_dir, GNC_FILE_DIALOG_IMPORT);
    g_free (default_dir);

    data = g_new0 (OfxFileDialogData, 1);
    data->had_parent = parent != NULL;
    g_weak_ref_init (&data->parent, parent);
    gnc_file_dialog_request_open_multiple_async (request, NULL,
                                                 ofx_file_dialog_finished, data);
    g_object_unref (request);
}


/** @} */

/********************************************************************\
 * assistant-stock-transaction.cpp -- stock assistant for GnuCash   *
 * Copyright (C) 2022 Christopher Lam                               *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <memory>
#include <vector>
#include <string>
#include <numeric>
#include <algorithm>
#include <optional>
#include <stdexcept>
#include <sstream>

#include "Account.h"
#include "Transaction.h"
#include "engine-helpers.h"
#include "dialog-utils.h"
#include "assistant-stock-transaction.h"
#include "gnc-account-sel.h"
#include "gnc-amount-edit.h"
#include "gnc-date.h"
#include <gnc-date-edit.h>
#include "gnc-engine.h"
#include "gnc-numeric.h"
#include "gnc-prefs.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui-util.h"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

/** The Stock Transaction Assistant guides the user through collecting the
 * information needed to properly account for one of several types of securities
 * investment transaction, including opening and closing long and short
 * positions, oridinary and capital gains dividends, returns of capital,
 * notional dividends, splits, and reverse splits. It tailors the available
 * transaction types and what information it requests based on the current state
 * of the account: For example, if the account holds an open short position it
 * will offer buy to cover short and dividend payments to the owner of the
 * stock.
 *
 * The Assistant is built with the Model-View-Controller pattern where
 * StockAssistantModel manages the data for the transaction, StockAssistantView
 * creates the visuals using a GtkAssistant and a class for each page type, and
 * StockAssistantController handles user input events.
 *
 * Depending on type a transaction is composed of some of the following splits:
 * A stock split representing the amount of units and their value in currency, a
 * cash split representing the source or disposition of that value, a fees split
 * representing commissions, fees, and taxes paid on the transaction, a dividend
 * entry representing currency paid by the issuer to its holders (or in the case
 * of a short position the currency paid by the short seller to the entity
 * borrowed from compensating them for the dividend that they would have been
 * paid had they not lent the shares), and two capital gains split representing
 * the change in currency value from the opening transaction.
*/

extern "C"
{
// These functions are the GtkAssistant primary button callbacks. They're
// connected to their signals in assistant-stock-transaction.glade so they
// mustn't be name-mangled.
void stock_assistant_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                                 gpointer user_data);
void stock_assistant_finish_cb  (GtkAssistant *assistant, gpointer user_data);
void stock_assistant_cancel_cb  (GtkAssistant *gtkassistant, gpointer user_data);
}

static const char* GNC_PREFS_GROUP = "dialogs.stock-assistant";
static const char* ASSISTANT_STOCK_TRANSACTION_CM_CLASS = "assistant-stock-transaction";

enum assistant_pages
{
    PAGE_INTRO = 0,
    PAGE_TRANSACTION_DETAILS,
    PAGE_TRANSACTION_TYPE,
    PAGE_STOCK_AMOUNT,
    PAGE_STOCK_VALUE,
    PAGE_CASH,
    PAGE_FEES,
    PAGE_DIVIDEND,
    PAGE_CAPGAINS,
    PAGE_FINISH
};

enum split_cols
{
    SPLIT_COL_ACCOUNT = 0,
    SPLIT_COL_MEMO,
    SPLIT_COL_TOOLTIP,
    SPLIT_COL_DEBIT,
    SPLIT_COL_CREDIT,
    SPLIT_COL_UNITS,
    SPLIT_COL_UNITS_COLOR,
    NUM_SPLIT_COLS
};
/* StockAssistantModel contains a (subclassed) StockTransactionEntry for each split to be created.
 *
 * A StockTransactionEntry contains some boolean parameters that are set from a
 * fieldmask, obtained from the corresponding element in the TxnTypeInfo for the
 * selected transaction type.
 *
 * The available transaction types are populated into
 * StockAssistantModel::m_txn_types by
 * StockAssistantModel::maybe_reset_txn_types() based on the state of the
 * account (long, short, or empty) on the seelected
 * date. StockAssistantModel::set_txn_type() then sets m_txn_type to the
 * selected template and calls StockTransactionEntry::set_fieldmask() on each of
 * its StockTransactionEntry members.
 */

enum class FieldMask : unsigned
{
    DISABLED = 0,
    ENABLED_DEBIT        = 1,
    ENABLED_CREDIT       = 1 << 1,
    AMOUNT_DEBIT         = 1 << 2, // stock only
    AMOUNT_CREDIT        = 1 << 3, // stock only
    INPUT_NEW_BALANCE    = 1 << 4, // stock_amt only: instead of amount, get new balance
    ALLOW_ZERO           = 1 << 5,
    ALLOW_NEGATIVE       = 1 << 6,
    CAPITALIZE_DEFAULT   = 1 << 7, // fees only: capitalize by default into stock acct
    CAPGAINS_IN_STOCK    = 1 << 8, // capg only: add a balancing split in stock acct
};

static FieldMask
operator |(FieldMask lhs, FieldMask rhs)
{
    return static_cast<FieldMask> (static_cast<unsigned>(lhs) |
                                   static_cast<unsigned>(rhs));
};

static bool
operator &(FieldMask lhs, FieldMask rhs)
{
    return (static_cast<unsigned>(lhs) & static_cast<unsigned>(rhs));
};

/* The pages displayed by the assistant and which fields are enabled on each
 * page is controlled by TxnTypeInfos, one for each transaction type.
 */
struct TxnTypeInfo
{
    FieldMask stock_amount;
    FieldMask cash_value;
    FieldMask fees_value;
    FieldMask dividend_value;
    FieldMask capgains_value;
    const char* friendly_name;
    const char* explanation;
};

using StringVec = std::vector<std::string>;
using TxnTypeVec = std::vector<TxnTypeInfo>;
using AccountVec = std::vector<Account*>;

static const TxnTypeVec starting_types
{
    {
        FieldMask::ENABLED_DEBIT | FieldMask::AMOUNT_DEBIT,          // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing an
        // Initial stock long purchase
        N_("Open buy"),
        N_("Initial stock long purchase.")
    },
    {
        FieldMask::ENABLED_CREDIT | FieldMask::AMOUNT_CREDIT,         // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing an
        // initial stock short sale
        N_("Open short"),
        N_("Initial stock short sale.")
    }
};

static const TxnTypeVec long_types
{
    {
        FieldMask::ENABLED_DEBIT | FieldMask::AMOUNT_DEBIT,          // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // new purchase of stock.
        N_("Buy"),
        N_("Buying stock long.")
    },
    {
        FieldMask::ENABLED_CREDIT | FieldMask::AMOUNT_CREDIT,         // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE | FieldMask::CAPGAINS_IN_STOCK, // capgains_amt
        // Translators: this is a stock transaction describing new
        // sale of stock, and recording capital gain/loss
        N_("Sell"),
        N_("Selling stock long, and record capital gain/loss.")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends issued to holder
        N_("Dividend"),
        N_("Company issues cash dividends to holder.\n\nAny dividend being \
reinvested must be subsequently recorded as a regular stock purchase.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing return
        // of capital
        N_("Return of capital"),
        N_("Company returns capital, reducing the cost basis without affecting # units.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing return
        // of capital, reclassifying a dividend into return of capital
        N_("Return of capital (reclassification)"),
        N_("Company returns capital, reducing the cost basis without affecting # units. A distribution previously recorded as a dividend is reclassified to return of capital, often due to end-of-year tax information.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution recorded as dividend
        N_("Notional distribution (dividend)"),
        N_("Company issues a notional distribution, which is recorded as dividend income and increases the cost basis without affecting # units.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_CREDIT,         // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution recorded as capital gain
        N_("Notional distribution (capital gain)"),
        N_("Company issues a notional distribution, which is recorded as capital gain and increases the cost basis without affecting # units.")
    },
    {
        FieldMask::DISABLED | FieldMask::AMOUNT_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split
        N_("Stock split"),
        N_("Company issues additional units, thereby reducing the stock price by a divisor, while keeping the total monetary value of the overall investment constant.")
    },
    {
        FieldMask::DISABLED | FieldMask::AMOUNT_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a reverse split
        N_("Reverse split"),
        N_("Company redeems units, thereby increasing the stock price by a \
multiple, while keeping the total monetary value of the overall investment \
constant.\n\nIf the reverse split results in a cash in lieu for remainder \
units, please record the sale using the Stock Transaction Assistant first, then \
record the reverse split.")
    }
};

static const TxnTypeVec short_types
{
    {
        FieldMask::ENABLED_CREDIT | FieldMask::AMOUNT_CREDIT,         // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // shorting of stock.
        N_("Short sell"),
        N_("Selling stock short.")
    },
    {
        FieldMask::ENABLED_DEBIT | FieldMask::AMOUNT_DEBIT,          // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE | FieldMask::CAPGAINS_IN_STOCK,          // capg_amt
        // Translators: this is a stock transaction describing cover
        // buying stock, and recording capital gain/loss
        N_("Buy to cover short"),
        N_("Buy back stock to cover short position, and record capital gain/loss.")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends retrieved from holder when shorting stock
        N_("Compensatory dividend"),
        N_("Company issues dividends, and the short stock holder must make a compensatory payment for the dividend.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing return
        // of capital retrieved from holder when shorting stock
        N_("Compensatory return of capital"),
        N_("Company returns capital, and the short stock holder must make a compensatory payment for the returned capital. This reduces the cost basis (less negative, towards 0.00 value) without affecting # units.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // reclassifying a compensatory dividend into compensatory
        // return of capital when shorting stock
        N_("Compensatory return of capital (reclassification)"),
        N_("Company returns capital, and the short stock holder must make a compensatory payment for the returned capital. This reduces the cost basis (less negative, towards 0.00 value) without affecting # units. A distribution previously recorded as a compensatory dividend is reclassified to compensatory return of capital, often due to end-of-year tax information.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution recorded as dividend when shorting
        // stock
        N_("Compensatory notional distribution (dividend)"),
        N_("Company issues a notional distribution, and the short stock holder must make a compensatory payment for the notional distribution. This is recorded as a loss/negative dividend income amount, and increases the cost basis (more negative, away from 0.00 value) without affecting # units.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_DEBIT,          // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution recorded as capital gain when
        // shorting stock
        N_("Compensatory notional distribution (capital gain)"),
        N_("Company issues a notional distribution, and the short stock holder must make a compensatory payment for the notional distribution. This is recorded as a capital loss amount, and increases the cost basis (more negative, away from 0.00 value) without affecting # units.")
    },
    {
        FieldMask::DISABLED | FieldMask::AMOUNT_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split when shorting stock
        N_("Stock split"),
        N_("Company issues additional units, thereby reducing the stock price by a divisor, while keeping the total monetary value of the overall investment constant.")
    },
    {
        FieldMask::DISABLED | FieldMask::ENABLED_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // reverse split when shorting stock.
        N_("Reverse split"),
        N_("Company redeems units, thereby increasing the stock price by \
a multiple, while keeping the total monetary value of the overall investment \
constant.\n\nIf the reverse split results in a cash in lieu for remainder \
units, please record the cover buy using the Stock Transaction Assistant first, \
then record the reverse split.")
    }
};

enum class LogMsgType
{
    info,
    warning,
    error
};

class LogMessage
{
    LogMsgType m_type;
    const std::string m_message;
public:
    LogMessage(LogMsgType type, std::string&& message) :
        m_type{type}, m_message(std::move(message)) {}
    LogMessage(LogMsgType type, const char* message) :
        m_type{type}, m_message(message) {}
    LogMessage(const LogMessage&) = default;
    LogMessage(LogMessage&&) = default;
    ~LogMessage() = default;
    LogMsgType type() { return m_type; }
    const std::string& message() { return m_message; }
};

using Log = std::vector<LogMessage>;

class Logger
{
    Log m_log;
public: // compiler generated ctors & dtor are fine
    void info(const char* message) { m_log.emplace_back(LogMsgType::info, message); }
    void warn(const char* message) { m_log.emplace_back(LogMsgType::warning, message); }
    void error(const char* message) { m_log.emplace_back(LogMsgType::error, message); }
    void clear() { m_log.clear(); }
    bool has_errors();
    bool has_warnings();
    void write_log(std::stringstream& stream, LogMsgType type);
    void infos(std::stringstream& stream) { return write_log(stream, LogMsgType::info); }
    void warnings(std::stringstream& stream) { return write_log(stream, LogMsgType::warning); }
    void errors(std::stringstream& stream) { return write_log(stream, LogMsgType::error); }
    std::string report();
};

void
Logger::write_log(std::stringstream& stream, LogMsgType type)
{
    std::for_each(m_log.begin(), m_log.end(),
                  [&](auto& msg){
                      if (msg.type() == type)
                          stream << "\n * " << msg.message();
                  });
}

bool
Logger::has_warnings()
{
    return std::any_of(m_log.begin(), m_log.end(),
                       [](auto& msg){ return msg.type() == LogMsgType::warning;
                       });
}

bool
Logger::has_errors()
{
    return std::any_of(m_log.begin(), m_log.end(),
                       [](auto& msg){ return msg.type() == LogMsgType::error;
                       });
}

std::string
Logger::report()
{
    std::stringstream summary;
    if (!has_errors())
    {
        summary << _("No errors found. Click Apply to create transaction.");
        infos(summary);
    }
    else
    {
        summary << _("The following errors must be fixed:");
        errors(summary);
    }
    if (has_warnings())
    {
        summary << "\n\n" << _("The following warnings exist:");
        warnings(summary);
    }
    return summary.str();
}

/** Possibly misnamed. Collects the required information to create a single
 * split in a transaction. This is the base class; there are child classes for
 * many split types.
 */

struct StockTransactionEntry
{
    bool m_enabled;
    bool m_debit_side;
    bool m_allow_zero;
    bool m_allow_negative;
    Account *m_account;
    gnc_numeric m_value;
    const char* m_memo;

    StockTransactionEntry() :
        m_enabled{false}, m_debit_side{false}, m_allow_zero{false},  m_account{nullptr},
        m_value{gnc_numeric_error(GNC_ERROR_ARG)}, m_memo{nullptr} {}
    StockTransactionEntry(bool debit_side, bool allow_zero, bool allow_negative, Account* account, gnc_numeric value) :
        m_enabled{false}, m_debit_side{debit_side}, m_allow_zero{allow_zero}, m_allow_negative{allow_negative},
        m_account{account}, m_value{value}, m_memo{nullptr} {}
    virtual ~StockTransactionEntry() = default;

    virtual void set_fieldmask(FieldMask mask);
    virtual void set_capitalize(bool capitalize) {}
    virtual void set_value(gnc_numeric amount, const char* page, Logger& logger);
    virtual gnc_numeric amount() { return m_value; }
    virtual void set_amount(gnc_numeric, Logger&) {}
    virtual void create_split(Transaction* trans, const char* action,
                              AccountVec& commits);
    virtual const char* print_value(GNCPrintAmountInfo info);
    virtual const char* print_amount(gnc_numeric amt);
    virtual gnc_numeric calculate_price(bool) { return gnc_numeric_error(GNC_ERROR_ARG); }
};

using StockTransactionEntryPtr = std::unique_ptr<StockTransactionEntry>;

void
StockTransactionEntry::set_fieldmask(FieldMask mask)
{
    m_enabled = mask != FieldMask::DISABLED;
    m_debit_side = mask & FieldMask::ENABLED_DEBIT;
    m_allow_zero = mask & FieldMask::ALLOW_ZERO;
    m_allow_negative = mask & FieldMask::ALLOW_NEGATIVE;
}


void
StockTransactionEntry::set_value(gnc_numeric amount, const char* page, Logger& logger)
{
    DEBUG ("checking value %s page %s",
           gnc_num_dbg_to_string (amount),
           page);

    auto add_error = [&logger](const char* format_str, const char* arg)
    {
        char *buf = g_strdup_printf (_(format_str),
                                      g_dpgettext2 (nullptr, "Stock Assistant: Page name", arg));
        logger.error(buf);
        g_free (buf);
    };


    if (gnc_numeric_check (amount))
    {
        add_error (N_("Amount for %s is missing."), page);
        return;
    }

    if (gnc_numeric_negative_p (amount))
    {
        if (m_allow_negative)
        {
            m_value = gnc_numeric_neg(amount);
            m_debit_side = !m_debit_side;
        }
        else
        {
            if (m_allow_zero)
                add_error (N_("Amount for %s must not be negative."), page);
        }
    }

    if (!m_allow_zero && !gnc_numeric_positive_p (amount))
    {
        add_error (N_("Amount for %s must be positive."), page);
        return;
    }

    m_value = m_debit_side ? amount : gnc_numeric_neg (amount);
}

const char *
StockTransactionEntry::print_value(GNCPrintAmountInfo pinfo)
{
    if (gnc_numeric_check(m_value) ||
        (gnc_numeric_zero_p(m_value) && !m_allow_zero))
        return _("missing");
    return xaccPrintAmount(m_value, pinfo);
}

const char *
StockTransactionEntry::print_amount(gnc_numeric amt)
{
    if (!m_account || gnc_numeric_check(amt))
        return nullptr;
    auto commodity{xaccAccountGetCommodity(m_account)};
    auto pinfo{gnc_commodity_print_info(commodity, TRUE)};
    return xaccPrintAmount(amt, pinfo);
}

void
StockTransactionEntry::create_split(Transaction *trans, const char* action,
                                    AccountVec &account_commits) {
  g_return_if_fail(trans);
  if (!m_account || gnc_numeric_check(m_value))
    return;
  auto split = xaccMallocSplit(qof_instance_get_book(trans));
  xaccSplitSetParent(split, trans);
  xaccAccountBeginEdit(m_account);
  account_commits.push_back(m_account);
  xaccSplitSetAccount(split, m_account);
  xaccSplitSetMemo(split, m_memo);
  xaccSplitSetValue(split, m_debit_side ? m_value : gnc_numeric_neg(m_value));
  xaccSplitSetAmount(split, amount());
  PINFO("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
        action, xaccAccountGetName (m_account),
        gnc_num_dbg_to_string(m_value),
        gnc_num_dbg_to_string(amount()),
        gnc_num_dbg_to_string(xaccSplitGetValue(split)),
        gnc_num_dbg_to_string(xaccSplitGetAmount(split)));
  gnc_set_num_action(nullptr, split, nullptr,
                     g_dpgettext2(nullptr, "Stock Assistant: Action field",
                                  action));
}

struct StockTransactionStockEntry : public StockTransactionEntry
{
    bool m_amount_enabled;
    gnc_numeric m_amount;

    StockTransactionStockEntry() :
        StockTransactionEntry{},
        m_amount{gnc_numeric_error(GNC_ERROR_ARG)} {
        PINFO("Stock Entry");
    }
    void set_fieldmask(FieldMask mask) override;
    gnc_numeric amount() override { return m_amount; }
    void set_amount(gnc_numeric amount, Logger& logger) override;
    gnc_numeric calculate_price(bool new_balance) override;
};

void
StockTransactionStockEntry::set_fieldmask(FieldMask mask)
{
    StockTransactionEntry::set_fieldmask(mask);
    m_enabled = mask & (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT |
                        FieldMask::AMOUNT_CREDIT | FieldMask::AMOUNT_DEBIT);
    m_amount_enabled = mask & (FieldMask::AMOUNT_CREDIT | FieldMask::AMOUNT_DEBIT);
    m_debit_side = mask & (FieldMask::ENABLED_DEBIT | FieldMask::AMOUNT_DEBIT);
}

void
StockTransactionStockEntry::set_amount(gnc_numeric amount, Logger& logger)
{
     if (!m_amount_enabled)
        return;

    if (gnc_numeric_check(amount) || gnc_numeric_zero_p(amount))
    {
        const char* err{_("Amount for stock value is missing.")};

        logger.error(err);
        return;
    }

    bool neg{gnc_numeric_negative_p(amount) == TRUE};

    if ((m_debit_side && !neg) || (!m_debit_side && neg))
        m_amount = amount;
    else
        m_amount = gnc_numeric_neg(amount);
    PINFO("%s set amount %s", m_memo, print_amount(amount));
}

gnc_numeric
StockTransactionStockEntry::calculate_price(bool new_balance)
{
    if (new_balance ||
        !m_amount_enabled || gnc_numeric_check(m_amount) ||
        !m_enabled || gnc_numeric_check(m_value) ||
        gnc_numeric_zero_p(m_amount) || gnc_numeric_zero_p(m_value))
        return gnc_numeric_error(GNC_ERROR_ARG);

    return gnc_numeric_div(m_value, m_amount,
                           GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
}

struct StockTransactionsStockCapGainsEntry : public StockTransactionEntry
{
    StockTransactionsStockCapGainsEntry(const StockTransactionEntry* cg_entry,
                                        const StockTransactionEntry* stk_entry);
    gnc_numeric amount() { return gnc_numeric_zero(); }
};

StockTransactionsStockCapGainsEntry::StockTransactionsStockCapGainsEntry(const StockTransactionEntry* cg_entry,
                                                                         const StockTransactionEntry* stk_entry) :
    StockTransactionEntry(!cg_entry->m_debit_side, cg_entry->m_allow_zero, cg_entry->m_allow_negative,
                          stk_entry->m_account, cg_entry->m_value) {}

struct StockTransactionFeesEntry : public StockTransactionEntry
{
    bool m_capitalize;

    StockTransactionFeesEntry() :
        StockTransactionEntry{},
        m_capitalize{false} {}
    void set_fieldmask(FieldMask mask) override;
    void set_capitalize(bool capitalize) override { m_capitalize = capitalize; }
    void create_split(Transaction *trans, const char *action,
                      AccountVec &commits) override;
};

void
StockTransactionFeesEntry::set_fieldmask(FieldMask mask)
{
    StockTransactionEntry::set_fieldmask(mask);
    m_capitalize = mask & FieldMask::CAPITALIZE_DEFAULT;
}

void
StockTransactionFeesEntry::create_split(Transaction* trans, const char* action,
                              AccountVec& commits)
{
    if (!m_capitalize)
        StockTransactionEntry::create_split(trans, action, commits);
}

struct StockTransactionSplitInfo
{
    StockTransactionEntry* m_entry;
    bool m_units_in_red = false;
    const char* m_action;
    static const char* s_missing_str;

    StockTransactionSplitInfo(StockTransactionEntry* entry, const char* page) :
        m_entry{entry}, m_action{page}
    {
        DEBUG ("StockTransactionSplitInfo constructor\n");
    }

    ~StockTransactionSplitInfo () { DEBUG ("StockTransactionSplitInfo destructor\n"); }
};

// Translators: (missing) denotes that the amount or account is
// not provided, or incorrect, in the Stock Transaction Assistant.
const char *StockTransactionSplitInfo::s_missing_str = N_("(missing)");

using SplitInfoVec = std::vector<StockTransactionSplitInfo>;

/** Manages the data and actions for the assistant. */
struct StockAssistantModel
{
    Account* m_acct;
    gnc_commodity* m_currency;

    GNCPrintAmountInfo m_curr_pinfo;

    time64 m_transaction_date;
    const char* m_transaction_description;
    std::optional<TxnTypeVec> m_txn_types;

    std::optional<TxnTypeInfo> m_txn_type;

    gnc_numeric m_balance_at_date = gnc_numeric_create (1, 0);

    bool m_input_new_balance;
    StockTransactionEntryPtr m_stock_entry;
    StockTransactionEntryPtr m_cash_entry;
    StockTransactionEntryPtr m_fees_entry;
    StockTransactionEntryPtr m_dividend_entry;
    StockTransactionEntryPtr m_capgains_entry;
    StockTransactionEntryPtr m_stock_cg_entry; // Required at this level for lifetime management
    Logger m_logger;

    StockAssistantModel (Account *account) :
        m_acct{account},
        m_currency{gnc_account_get_currency_or_parent(account)},
        m_curr_pinfo (gnc_commodity_print_info (m_currency, true)),
        m_stock_entry{std::make_unique<StockTransactionStockEntry>()},
        m_cash_entry{std::make_unique<StockTransactionEntry>()},
        m_fees_entry{std::make_unique<StockTransactionFeesEntry>()},
        m_dividend_entry{std::make_unique<StockTransactionEntry>()},
        m_capgains_entry{std::make_unique<StockTransactionEntry>()}
    {
        DEBUG ("StockAssistantModel constructor\n");
        m_stock_entry->m_account = m_acct;
    };

    ~StockAssistantModel()
    {
        DEBUG ("StockAssistantModel destructor\n");
    };

    // consider reset txn_types. return false if txn_types are still
    // current (i.e. transaction_date hasn't changed).
    bool maybe_reset_txn_types ();
    bool set_txn_type (guint type_idx);
    std::string get_stock_balance_str ()
    {
        return m_stock_entry->print_amount(m_balance_at_date);
    };

    std::string get_new_amount_str ();
    std::tuple<bool, gnc_numeric, const char*> calculate_price ();
    std::tuple<bool, std::string, SplitInfoVec> generate_list_of_splits ();
    std::tuple<bool, Transaction*> create_transaction ();

private:
    std::optional<time64>     m_txn_types_date;
    bool m_ready_to_create = false;

    SplitInfoVec m_list_of_splits;

    void add_price (QofBook *book);
    StockTransactionSplitInfo make_stock_split_info();
};

bool
StockAssistantModel::maybe_reset_txn_types ()
{
    auto new_bal = xaccAccountGetBalanceAsOfDate
        (m_acct, gnc_time64_get_day_end (m_transaction_date));
    if (m_txn_types_date && m_txn_types_date == m_transaction_date &&
        gnc_numeric_equal (m_balance_at_date, new_bal))
        return false;
    m_balance_at_date = new_bal;
    m_txn_types_date = m_transaction_date;
    m_txn_types = gnc_numeric_zero_p (m_balance_at_date) ? starting_types
        : gnc_numeric_positive_p (m_balance_at_date) ? long_types
        : short_types;
    return true;
};

bool
StockAssistantModel::set_txn_type (guint type_idx)
{
    if (!m_txn_types_date || m_txn_types_date != m_transaction_date)
    {
        PERR ("transaction_date has changed. rerun maybe_reset_txn_types!");
        return false;
    }
    try
    {
        m_txn_type = m_txn_types->at (type_idx);
    }
    catch (const std::out_of_range&)
    {
        PERR ("out of range type_idx=%d", type_idx);
        return false;
    }

    m_input_new_balance = m_txn_type->stock_amount & FieldMask::INPUT_NEW_BALANCE;
    m_stock_entry->set_fieldmask(m_txn_type->stock_amount);
    m_fees_entry->set_fieldmask(m_txn_type->fees_value);
    m_capgains_entry->set_fieldmask(m_txn_type->capgains_value);
    m_dividend_entry->set_fieldmask(m_txn_type->dividend_value);
    m_cash_entry->set_fieldmask(m_txn_type->cash_value);
    return true;
};

std::string
StockAssistantModel::get_new_amount_str ()
{
    std::string rv{""};
    auto stock_entry = dynamic_cast<StockTransactionStockEntry*>(m_stock_entry.get());

    if (gnc_numeric_check (stock_entry->m_amount))
        return rv;

    if (m_input_new_balance)
    {
        auto ratio = gnc_numeric_div (stock_entry->m_amount, m_balance_at_date,
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        if (gnc_numeric_check (ratio) || !gnc_numeric_positive_p (ratio))
            return rv;

        std::ostringstream ret;
        ret << ratio.num << ':' << ratio.denom;
        rv = ret.str();
    }
    else
    {
        auto stock_entry = dynamic_cast<StockTransactionStockEntry*>(m_stock_entry.get());
        auto amount = (m_txn_type->stock_amount & FieldMask::ENABLED_CREDIT) ?
            gnc_numeric_neg (stock_entry->m_amount) : stock_entry->m_amount;
        amount = gnc_numeric_add_fixed (amount, m_balance_at_date);
        rv = m_stock_entry->print_amount(amount);
    }

    return rv;
};

std::tuple<bool, gnc_numeric, const char*>
StockAssistantModel::calculate_price ()
{
    auto price{m_stock_entry->calculate_price(m_input_new_balance)};
    if (gnc_numeric_check(price))
        return {false, price, nullptr};
    auto pinfo{gnc_price_print_info (m_currency, true)};
    return {true, price, xaccPrintAmount (price, pinfo)};
}

static void
check_txn_date(GList* last_split_node, time64 txn_date, Logger& logger)
{
    auto last_split = static_cast<const Split *>(last_split_node->data);
    auto last_split_date = xaccTransGetDate(xaccSplitGetParent(last_split));
    if (txn_date <= last_split_date) {
        auto last_split_date_str = qof_print_date(last_split_date);
        auto new_date_str = qof_print_date(txn_date);
        // Translators: the first %s is the new transaction date;
        // the second %s is the current stock account's latest
        // transaction date.
        auto warn_txt = g_strdup_printf(
            _("You will enter a transaction "
              "with date %s which is earlier than the latest transaction in this account, "
              "dated %s. Doing so may affect the cost basis, and therefore capital gains, "
              "of transactions dated after the new entry. Please review all transactions "
              "to ensure proper recording."),
            new_date_str, last_split_date_str);
        logger.warn(warn_txt);
        g_free(warn_txt);
        g_free(new_date_str);
        g_free(last_split_date_str);
    }
}

StockTransactionSplitInfo
StockAssistantModel::make_stock_split_info()
{
    auto add_error_str = [this]
        (const char* str) { m_logger.error (_(str)); };

    StockTransactionSplitInfo line{m_stock_entry.get(),
        NC_ ("Stock Assistant: Page name", "stock value")};
    auto stock_entry = dynamic_cast<StockTransactionStockEntry*>(m_stock_entry.get());
    bool negative_in_red = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_NEGATIVE_IN_RED);

    if (m_input_new_balance)
    {
        auto& stock_amount = stock_entry->m_amount;
        auto credit_side = (m_txn_type->stock_amount & FieldMask::AMOUNT_CREDIT);
        auto delta = gnc_numeric_sub_fixed(stock_amount, m_balance_at_date);
        auto ratio = gnc_numeric_div(stock_amount, m_balance_at_date,
                                     GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        stock_amount = gnc_numeric_sub_fixed(stock_amount, m_balance_at_date);
        line.m_entry->set_amount(stock_amount, m_logger);
        line.m_units_in_red =
            negative_in_red && gnc_numeric_negative_p(stock_amount);
        if (gnc_numeric_check(ratio) || !gnc_numeric_positive_p(ratio))
            add_error_str(N_("Invalid stock new balance."));
        else if (gnc_numeric_negative_p(delta) && !credit_side)
            add_error_str(N_("New balance must be higher than old balance."));
        else if (gnc_numeric_positive_p(delta) && credit_side)
            add_error_str(N_("New balance must be lower than old balance."));
    }
    else if (stock_entry->m_amount_enabled)
    {
        auto& stock_amount = stock_entry->m_amount;
        if (!gnc_numeric_positive_p(stock_amount))
            add_error_str(N_("Stock amount must be positive."));
        if (m_txn_type->stock_amount & FieldMask::AMOUNT_CREDIT)
            stock_amount = gnc_numeric_neg(stock_amount);
        line.m_units_in_red =
            negative_in_red && gnc_numeric_negative_p(stock_amount);
        auto new_bal = gnc_numeric_add_fixed(m_balance_at_date, stock_amount);
        if (gnc_numeric_positive_p(m_balance_at_date) &&
            gnc_numeric_negative_p(new_bal))
            add_error_str(N_("Cannot sell more units than owned."));
        else if (gnc_numeric_negative_p(m_balance_at_date) &&
                 gnc_numeric_positive_p(new_bal))
            add_error_str(N_("Cannot cover buy more units than owed."));
    }
    return line;
}

std::tuple<bool, std::string, SplitInfoVec>
StockAssistantModel::generate_list_of_splits() {
    if (!m_txn_types || !m_txn_type)
        return { false, "Error: txn_type not initialized", {} };

    m_logger.clear();
    m_list_of_splits.clear();

    gnc_numeric debit = gnc_numeric_zero ();
    gnc_numeric credit = gnc_numeric_zero ();

    // check the stock transaction date. If there are existing stock
    // transactions dated after the date specified, it is very likely
    // the later stock transactions will be invalidated. warn the user
    // to review them.
    auto last_split_node = g_list_last (xaccAccountGetSplitList (m_acct));
    if (last_split_node)
        check_txn_date(last_split_node, m_transaction_date, m_logger);


    m_list_of_splits.push_back (make_stock_split_info());

    auto [has_price, price, price_str] = calculate_price ();
    if (has_price)
    {
        // Translators: %s refer to: stock mnemonic, broker currency,
        // date of transaction.
        auto tmpl = N_("A price of 1 %s = %s on %s will be recorded.");
        auto date_str = qof_print_date (m_transaction_date);
        auto price_msg = g_strdup_printf
            (_(tmpl),
             gnc_commodity_get_mnemonic (xaccAccountGetCommodity (m_acct)),
             price_str, date_str);
        m_logger.info(price_msg);
        g_free (date_str);
    }

    if (m_cash_entry->m_enabled)
        m_list_of_splits.push_back (StockTransactionSplitInfo{m_cash_entry.get(),
             NC_ ("Stock Assistant: Page name", "cash")});

    if (m_fees_entry->m_enabled)
        m_list_of_splits.push_back (StockTransactionSplitInfo{m_fees_entry.get(),
             NC_ ("Stock Assistant: Page name", "fees")});

    if (m_dividend_entry->m_enabled)
        m_list_of_splits.push_back (StockTransactionSplitInfo{m_dividend_entry.get(),
             NC_ ("Stock Assistant: Page name", "dividend")});

    if (m_capgains_entry->m_enabled)
    {
        m_stock_cg_entry =
            std::make_unique<StockTransactionsStockCapGainsEntry>(m_capgains_entry.get(),
                                                                  m_stock_entry.get());
        m_list_of_splits.push_back(StockTransactionSplitInfo{m_stock_cg_entry.get(),
             NC_ ("Stock Assistant: Page name", "capital gains")});
        m_list_of_splits.push_back (StockTransactionSplitInfo{m_capgains_entry.get(),
             NC_ ("Stock Assistant: Page name", "capital gains")});
    }

    std::for_each(m_list_of_splits.begin(), m_list_of_splits.end(),
                  [&debit, &credit](const auto& splitinfo) {
                      if (splitinfo.m_entry->m_debit_side)
                          debit = gnc_numeric_add(debit, splitinfo.m_entry->m_value,
                                                   GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
                      else
                          credit = gnc_numeric_add(credit, splitinfo.m_entry->m_value,
                                                    GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
                  });

    if (gnc_numeric_check(debit) || gnc_numeric_check(credit) ||!gnc_numeric_equal (debit, credit))
    {
        const char *err_act = NULL, *err_reason = NULL;
        if (gnc_numeric_check(debit))
        {
            err_act = "debit";
            err_reason = gnc_numeric_errorCode_to_string(gnc_numeric_check(debit));
        }
        else if (gnc_numeric_check(credit))
        {
            err_act = "credit";
            err_reason = gnc_numeric_errorCode_to_string(gnc_numeric_check(credit));
        }

        if (err_act)
        {
            auto err_str = g_strdup_printf (N_("Transaction can't balance, %s is error value %s"), err_act, err_reason);
            m_logger.error(err_str);
            g_free (err_str);
        }
        else
        {
            auto imbalance_str = N_("Total Debits of %s does not balance with total Credits of %s.");
            auto debit_str = g_strdup (xaccPrintAmount (debit, m_curr_pinfo));
            auto credit_str = g_strdup (xaccPrintAmount (credit, m_curr_pinfo));
            auto error_str = g_strdup_printf (_(imbalance_str), debit_str, credit_str);
            m_logger.error (error_str);
            g_free (error_str);
            g_free (credit_str);
            g_free (debit_str);
        }
    }

    // generate final summary message. Collates a header, the errors
    // and warnings. Then allow completion if errors is empty.
    m_ready_to_create = !m_logger.has_errors();
    return { m_ready_to_create, m_logger.report(), m_list_of_splits };
}

std::tuple<bool, Transaction*>
StockAssistantModel::create_transaction ()
{
    if (!m_ready_to_create)
    {
        PERR ("errors exist. cannot create transaction.");
        return {false, nullptr};
    }
    auto book = qof_instance_get_book (m_acct);
    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, m_currency);
    xaccTransSetDescription (trans, m_transaction_description);
    xaccTransSetDatePostedSecsNormalized (trans, m_transaction_date);
    AccountVec accounts;
    std::for_each (m_list_of_splits.begin(), m_list_of_splits.end(),
                   [&](auto& line){ line.m_entry->create_split (trans, line.m_action, accounts); });
    add_price (book);
    xaccTransCommitEdit (trans);
    std::for_each (accounts.begin(), accounts.end(), xaccAccountCommitEdit);
    m_ready_to_create = false;
    return {true, trans};
}

void
StockAssistantModel::add_price (QofBook *book)
{
    auto [has_price, p, price_str] = calculate_price ();
    if (!has_price)
        return;

    auto price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, xaccAccountGetCommodity (m_acct));
    gnc_price_set_currency (price, m_currency);
    gnc_price_set_time64 (price, m_transaction_date);
    gnc_price_set_source (price, PRICE_SOURCE_STOCK_TRANSACTION);
    gnc_price_set_typestr (price, PRICE_TYPE_UNK);
    gnc_price_set_value (price, p);
    gnc_price_commit_edit (price);

    auto pdb = gnc_pricedb_get_db (book);
    if (!gnc_pricedb_add_price (pdb, price))
        PWARN ("error adding price");

    gnc_price_unref (price);
}

/* ********************* View Classes ************************/

/* ***************** Generic Event Callbacks ****************/
static void
text_entry_changed_cb (GtkWidget *widget, const gchar **model_text)
{
    *model_text = gtk_entry_get_text (GTK_ENTRY (widget));
}


static inline GtkWidget*
get_widget (GtkBuilder *builder, const gchar * ID)
{
    g_return_val_if_fail (builder && ID, nullptr);
    auto obj = gtk_builder_get_object (builder, ID);
    if (!obj)
        PWARN ("get_widget ID '%s' not found. it may be a typo?", ID);
    return GTK_WIDGET (obj);
}

/* Editor widgets used in assistant pages. */

struct GncDateEdit
{
    GtkWidget *m_edit;
    GncDateEdit(GtkBuilder *builder) :
        m_edit{gnc_date_edit_new(gnc_time(nullptr), FALSE, FALSE)} {}
    void attach(GtkBuilder *builder, const char *table_ID, const char *label_ID,
                int row);
    time64 get_date_time() { return gnc_date_edit_get_date_end(GNC_DATE_EDIT(m_edit)); }
    void connect(time64 *target);
};

static void
gnc_date_edit_changed_cb (GtkWidget* widget, time64 *target)
{
    g_return_if_fail(GNC_IS_DATE_EDIT(widget));
    *target = gnc_date_edit_get_date_end(GNC_DATE_EDIT(widget));
}

void
GncDateEdit::attach(GtkBuilder *builder, const char *table_ID,
                    const char *label_ID, int row)
{
    auto table = get_widget(builder, table_ID);
    auto label = get_widget (builder, label_ID);
    gtk_grid_attach(GTK_GRID(table), m_edit, 1, row, 1, 1);
    gtk_widget_show(m_edit);
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(m_edit), label);
}

void
GncDateEdit::connect(time64 *time)
{
    g_signal_connect(m_edit, "date_changed", G_CALLBACK (gnc_date_edit_changed_cb), time);
}

struct GncAmountEdit
{
    GtkWidget *m_edit;

    GncAmountEdit (GtkBuilder *builder, gnc_commodity *commodity);
    void attach (GtkBuilder *builder, const char *table_id,
                 const char *label_ID, int row);
    gnc_numeric get ();
    void connect (gnc_numeric *value);
    void connect (GCallback cb, gpointer data);
    void set_focus();
    void set_owner (gpointer obj);
};

static void
gnc_amount_edit_changed_cb (GtkWidget* widget, gnc_numeric *value)
{
    g_return_if_fail(GNC_IS_AMOUNT_EDIT(widget));
    gnc_numeric amt;
    if (!gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(widget), &amt, true, nullptr))
        *value =  amt;
    else
        *value = gnc_numeric_error(GNC_ERROR_ARG);
}

GncAmountEdit::GncAmountEdit (GtkBuilder *builder, gnc_commodity *commodity) :
    m_edit{gnc_amount_edit_new()}
{
    // shares amount
    auto info = gnc_commodity_print_info(commodity, true);
    gnc_amount_edit_set_evaluate_on_enter(GNC_AMOUNT_EDIT(m_edit), TRUE);
    gnc_amount_edit_set_print_info(GNC_AMOUNT_EDIT(m_edit), info);
}

void
GncAmountEdit::attach (GtkBuilder *builder, const char *table_ID,
                       const char* label_ID, int row)
{
    auto table = get_widget(builder, table_ID);
    auto label = get_widget(builder, label_ID);
    gtk_grid_attach(GTK_GRID(table), m_edit, 1, row, 1, 1);
    gtk_widget_show(m_edit);
    gnc_amount_edit_make_mnemonic_target(GNC_AMOUNT_EDIT(m_edit), label);
}

gnc_numeric
GncAmountEdit::get ()
{
    gnc_numeric amt;
    if (!gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(m_edit), &amt, true, nullptr))
        return amt;
    return gnc_numeric_error(GNC_ERROR_ARG);
}

void
GncAmountEdit::connect (gnc_numeric *value)
{
    g_signal_connect(m_edit, "changed", G_CALLBACK (gnc_amount_edit_changed_cb), value);
}

void
GncAmountEdit::connect (GCallback cb, gpointer data)
{
    g_signal_connect(m_edit, "changed", cb, data);
}

void
GncAmountEdit::set_focus()
{
    gtk_widget_grab_focus (GTK_WIDGET (gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (m_edit))));
}

void
GncAmountEdit::set_owner(gpointer obj)
{
    g_object_set_data(G_OBJECT (m_edit), "owner", obj);
}

using AccountTypeList = std::vector<GNCAccountType>;

struct GncAccountSelector
{
    GtkWidget* m_selector;

    GncAccountSelector (GtkBuilder *builder, AccountTypeList types,
                        gnc_commodity *currency);
    void attach (GtkBuilder *builder, const char *table_id,
                 const char *label_ID, int row);
    void connect (Account **acct);
    void set (Account *acct) { gnc_account_sel_set_account (GNC_ACCOUNT_SEL (m_selector), acct, TRUE); }
    Account *get () { return gnc_account_sel_get_account (GNC_ACCOUNT_SEL (m_selector)); }
};

static void
gnc_account_sel_changed_cb (GtkWidget* widget, Account **acct)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (widget));
    *acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL (widget));
}

GncAccountSelector::GncAccountSelector (GtkBuilder *builder, AccountTypeList types,
                                        gnc_commodity *currency) :
    m_selector{gnc_account_sel_new ()}
{
    auto accum = [](auto a, auto b) { return g_list_prepend(a, (gpointer)b); };
    auto null_glist = static_cast<GList *>(nullptr);
    auto acct_list = std::accumulate(types.begin(), types.end(), null_glist, accum);
    auto curr_list = accum(null_glist, currency);
    gnc_account_sel_set_new_account_ability(GNC_ACCOUNT_SEL(m_selector), true);
    gnc_account_sel_set_acct_filters(GNC_ACCOUNT_SEL(m_selector), acct_list, curr_list);
    g_list_free(acct_list);
    g_list_free(curr_list);
}

void
GncAccountSelector::attach (GtkBuilder *builder, const char *table_ID,
                            const char *label_ID, int row)
{
    auto table = get_widget(builder, table_ID);
    auto label = get_widget(builder, label_ID);
    gtk_grid_attach(GTK_GRID(table), m_selector, 1, row, 1, 1);
    gtk_widget_show(m_selector);
    gtk_label_set_mnemonic_widget(GTK_LABEL(label), m_selector);
}

void
GncAccountSelector::connect (Account **acct)
{
    g_signal_connect(m_selector, "account_sel_changed", G_CALLBACK (gnc_account_sel_changed_cb), acct);
}

/* Assistant page classes. */

struct PageTransType {
    // transaction type page
    GtkWidget * m_page;
    GtkWidget * m_type;
    GtkWidget * m_explanation;
    PageTransType(GtkBuilder *builder);
    void prepare(StockAssistantModel* model);
    int get_transaction_type_index ();
    void set_transaction_types (const TxnTypeVec& txn_types);
    void set_txn_type_explanation (const gchar *txt);
    void set_focus() { gtk_widget_grab_focus (m_type); }
    void connect (StockAssistantModel *model);
    void change_txn_type (StockAssistantModel *model);
};

PageTransType::PageTransType(GtkBuilder *builder)
    : m_page(get_widget(builder, "transaction_type_page")),
      m_type(get_widget(builder, "transaction_type_page_combobox")),
      m_explanation(get_widget(builder, "transaction_type_page_explanation"))
{
    g_object_set_data(G_OBJECT(m_type), "owner", this);
}

static void
page_trans_type_changed_cb (GtkWidget* widget, StockAssistantModel *model)
{
    auto me = static_cast<PageTransType *>(g_object_get_data (G_OBJECT (widget), "owner"));
    g_return_if_fail (me);
    me->change_txn_type (model);
}

void
PageTransType::prepare(StockAssistantModel *model)
{
    if (!model->m_txn_types)
        return;

    set_transaction_types(model->m_txn_types.value());
    change_txn_type (model);
    set_focus();
}

int
PageTransType::get_transaction_type_index ()
{
    return gtk_combo_box_get_active (GTK_COMBO_BOX (m_type));
}

void
PageTransType::set_transaction_types (const TxnTypeVec& txn_types)
{
    auto combo = GTK_COMBO_BOX_TEXT (m_type);
    gtk_combo_box_text_remove_all (combo);
    std::for_each (txn_types.begin(), txn_types.end(),
                   [&combo](const auto& it)
                   { gtk_combo_box_text_append_text (combo, _(it.friendly_name)); });
    gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 0);
}

void
PageTransType::set_txn_type_explanation (const gchar *txt)
{
    gtk_label_set_text (GTK_LABEL (this->m_explanation), txt);
}

void
PageTransType::change_txn_type (StockAssistantModel *model)
{
    auto type_idx = get_transaction_type_index();
    if (type_idx < 0)           // combo isn't initialized yet.
        return;

    if (!model->set_txn_type (type_idx))
        return;

    set_txn_type_explanation (model->m_txn_type->explanation);
}

void
PageTransType::connect(StockAssistantModel *model)
{
    g_signal_connect(m_type, "changed",
                     G_CALLBACK (page_trans_type_changed_cb), model);
}

struct PageTransDeets
{
    // transaction details page
    GtkWidget *m_page;
    GncDateEdit m_date;
    GtkWidget *m_description;
    PageTransDeets (GtkBuilder *builder);
    time64 get_date_time () { return m_date.get_date_time(); }
    const char* get_description () { return gtk_entry_get_text (GTK_ENTRY (m_description)); }
    void set_focus () { gtk_widget_grab_focus (m_description); }
    void connect (time64 *date, const char **description);
    void prepare(time64 *date, const char** description);
};

PageTransDeets::PageTransDeets (GtkBuilder *builder) :
    m_page (get_widget (builder, "transaction_details_page")),
    m_date (builder),
    m_description (get_widget (builder, "transaction_description_entry"))
{
    m_date.attach(builder,  "transaction_details_table", "transaction_date_label", 0);
}

void
PageTransDeets::connect(time64 *date, const char **description)
{
    m_date.connect(date);
    g_signal_connect(m_description, "changed", G_CALLBACK (text_entry_changed_cb), description);
}

void
PageTransDeets::prepare(time64 *date, const char** description)
{
    *date = get_date_time();
    *description = get_description();
    set_focus ();
}

struct PageStockAmount
{
    // stock amount page
    GtkWidget * m_page;
    GtkWidget * m_title;
    GtkWidget * m_prev_amount;
    GtkWidget * m_next_amount;
    GtkWidget * m_next_amount_label;
    GncAmountEdit m_amount;
    GtkWidget * m_amount_label;
    PageStockAmount (GtkBuilder *builder, Account* account);
    void prepare (StockTransactionStockEntry*, StockAssistantModel*, Logger&);
    gnc_numeric get_stock_amount () { return m_amount.get(); }
    void set_stock_amount (std::string new_amount_str);
    void connect(StockAssistantModel *model);
};

PageStockAmount::PageStockAmount (GtkBuilder *builder, Account* account) :
    m_page (get_widget (builder, "stock_amount_page")),
    m_title (get_widget (builder, "stock_amount_title")),
    m_prev_amount (get_widget (builder, "prev_balance_amount")),
    m_next_amount (get_widget (builder, "next_balance_amount")),
    m_next_amount_label (get_widget (builder, "next_balance_label")),
    m_amount (builder, xaccAccountGetCommodity(account)),
    m_amount_label (get_widget (builder, "stock_amount_label"))
{
    m_amount.attach (builder, "stock_amount_table", "stock_amount_label", 1);
}

void
PageStockAmount::prepare (StockTransactionStockEntry* entry, StockAssistantModel* model,
                          Logger& logger)
{
    gtk_label_set_text_with_mnemonic
        (GTK_LABEL (m_amount_label),
         model->m_input_new_balance ? _("Ne_w Balance") : _("_Shares"));
    gtk_label_set_text
        (GTK_LABEL (m_next_amount_label),
         model->m_input_new_balance ? _("Ratio") : _("Next Balance"));
    gtk_label_set_text (GTK_LABEL (m_title),
         model->m_input_new_balance ?
         _("Enter the new balance of shares after the stock split.") :
         _("Enter the number of shares you gained or lost in the transaction."));
    gtk_label_set_text (GTK_LABEL (m_prev_amount), model->get_stock_balance_str().c_str());
    if (!gnc_numeric_check(get_stock_amount()))
        entry->set_amount(get_stock_amount(), logger);
    set_stock_amount(model->get_new_amount_str());
    m_amount.set_focus();

}

static void
page_stock_amount_changed_cb(GtkWidget *widget, StockAssistantModel *model)
{
    auto me = static_cast<PageStockAmount*>(g_object_get_data (G_OBJECT (widget), "owner"));
    model->m_stock_entry->set_amount(me->m_amount.get(), model->m_logger);
    me->set_stock_amount (model->get_new_amount_str());
}

void
PageStockAmount::connect(StockAssistantModel *model)
{
    m_amount.connect(G_CALLBACK (page_stock_amount_changed_cb), model);
    m_amount.set_owner(static_cast<gpointer>(this));
}

void
PageStockAmount::set_stock_amount (std::string new_amount_str)
{
    gtk_label_set_text (GTK_LABEL(m_next_amount), new_amount_str.c_str());
}

struct PageStockValue
{
    // stock value page
    GtkWidget * m_page;
    GncAmountEdit m_value;
    GtkWidget * m_price;
    GtkWidget * m_memo;
    PageStockValue (GtkBuilder *builder, Account* account);
    const char* get_memo ();
    void connect(StockAssistantModel *model);
    void prepare(StockTransactionEntry*, StockAssistantModel*, Logger&);
    void set_price(const gchar *val);
    void set_price (std::tuple<bool, gnc_numeric, const char*> price_tuple);
};

static void
page_stock_value_changed_cb(GtkWidget *widget, StockAssistantModel *model)
{
    auto me = static_cast<PageStockValue*>(g_object_get_data (G_OBJECT (widget), "owner"));
    auto value = me->m_value.get ();
    model->m_stock_entry->set_value (value, "stocks", model->m_logger);
    me->set_price (model->calculate_price());
}

PageStockValue::PageStockValue(GtkBuilder *builder, Account* account)
    : m_page(get_widget(builder, "stock_value_page")),
      m_value(builder, gnc_account_get_currency_or_parent(account)),
      m_price(get_widget(builder, "stock_price_amount")),
      m_memo(get_widget(builder, "stock_memo_entry"))
{
    m_value.attach(builder, "stock_value_table", "stock_value_label", 0);
}

void
PageStockValue::connect(StockAssistantModel *model)
{
    m_value.connect(G_CALLBACK (page_stock_value_changed_cb), model);
    m_value.set_owner (static_cast<gpointer>(this));
    g_signal_connect (m_memo, "changed", G_CALLBACK(text_entry_changed_cb), &model->m_stock_entry->m_memo);
}

void
PageStockValue::prepare(StockTransactionEntry* entry, StockAssistantModel* model, Logger& logger)
{
    entry->m_memo = get_memo();
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get(), "stock", logger);
    set_price(model->calculate_price());
    m_value.set_focus();
}

const char *
PageStockValue::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

void
PageStockValue::set_price (const gchar *val)
{
    gtk_label_set_text(GTK_LABEL(this->m_price), val);
};

void
PageStockValue::set_price (std::tuple<bool, gnc_numeric, const char*> price_tuple)
{
    auto [has_price, price, price_str] = price_tuple;
    // Translators: StockAssistant: N/A denotes stock price is not computable
    set_price(has_price ? price_str : _("N/A"));
}

struct PageCash
{
    // cash page
    GtkWidget * m_page;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
    PageCash (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry* entry);
    void prepare(StockTransactionEntry* entry, Logger& logger);
    const char* get_memo();
};

PageCash::PageCash(GtkBuilder *builder, Account* account)
    : m_page(get_widget(builder, "cash_details_page")),
      m_account(builder, {ACCT_TYPE_ASSET, ACCT_TYPE_BANK},
                gnc_account_get_currency_or_parent(account)),
      m_memo(get_widget(builder, "cash_memo_entry")),
      m_value(builder, gnc_account_get_currency_or_parent(account))
{
    m_account.attach (builder, "cash_table", "cash_account_label", 0);
    m_value.attach (builder, "cash_table", "cash_label", 1);
}

void
PageCash::connect(StockTransactionEntry* entry)
{
    m_account.connect(&entry->m_account);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), &entry->m_memo);
    m_value.connect(&entry->m_value);
}

void
PageCash::prepare(StockTransactionEntry* entry, Logger& logger)
{
    entry->m_memo = get_memo();
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value (m_value.get(), "cash", logger);
    entry->m_account = m_account.get();
    m_value.set_focus();
}

const char *
PageCash::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

struct PageFees
{
    // fees page
    GtkWidget * m_page;
    GtkWidget * m_capitalize;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
    PageFees (GtkBuilder *builder, Account* account);
    void connect(StockTransactionFeesEntry*);
    bool get_capitalize_fees ();
    const char* get_memo();
    void set_capitalize_fees (bool state);
    void set_capitalize_fees (StockTransactionFeesEntry*);
    void set_account (Account *acct) { m_account.set(acct); }
    void update_fees_acct_sensitive (bool sensitive);
    void prepare(StockTransactionFeesEntry*, Logger&);
};

PageFees::PageFees(GtkBuilder *builder, Account* account)
    : m_page(get_widget(builder, "fees_details_page")),
      m_capitalize(
          get_widget(builder, "capitalize_fees_checkbutton")),
      m_account(builder, {ACCT_TYPE_EXPENSE}, gnc_account_get_currency_or_parent(account)),
      m_memo(get_widget(builder, "fees_memo_entry")),
      m_value(builder, gnc_account_get_currency_or_parent(account))
{
    m_account.attach (builder, "fees_table", "fees_account_label", 1);
    m_value.attach(builder, "fees_table", "fees_label", 2);
}

bool
PageFees::get_capitalize_fees()
{
    return gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(m_capitalize));
}

const char *
PageFees::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

void
PageFees::set_capitalize_fees(bool state)
{
    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(m_capitalize), state);
}

void
PageFees::set_capitalize_fees(StockTransactionFeesEntry* entry)
{
    set_capitalize_fees (entry->m_capitalize);
}

void
PageFees::update_fees_acct_sensitive(bool sensitive)
{
    gtk_widget_set_sensitive(m_account.m_selector, sensitive);
}

static void
capitalize_fees_toggled_cb (GtkWidget *widget, StockTransactionFeesEntry *entry)
{
    g_return_if_fail (entry);
    auto me = static_cast<PageFees *>(g_object_get_data (G_OBJECT (widget), "owner"));
    g_return_if_fail (me);
    bool cap =  me->get_capitalize_fees();
    entry->set_capitalize(cap);
    me->update_fees_acct_sensitive (!cap);
}

void
PageFees::connect(StockTransactionFeesEntry* entry)
{
    m_account.connect(&entry->m_account);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb),  &entry->m_memo);
    m_value.connect(&entry->m_value);
    g_object_set_data(G_OBJECT (m_capitalize), "owner", this);
    g_signal_connect (m_capitalize, "toggled", G_CALLBACK (capitalize_fees_toggled_cb), entry);
}

void
PageFees::prepare(StockTransactionFeesEntry* entry, Logger& logger)
{
        set_capitalize_fees (entry);
        entry->m_memo = get_memo();
        if (!gnc_numeric_check(m_value.get()))
            entry->set_value (m_value.get(), "fees", logger);
        entry->m_account = m_account.get();
        m_value.set_focus();
}

struct PageDividend
{
    // dividend page
    GtkWidget *m_page;
    GncAccountSelector m_account;
    GtkWidget *m_memo;
    GncAmountEdit m_value;
    PageDividend (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry*);
    void prepare(StockTransactionEntry*, Logger&);
    const char* get_memo();
};

PageDividend::PageDividend(GtkBuilder *builder, Account* account)
    : m_page(get_widget(builder, "dividend_details_page")),
      m_account(builder, {ACCT_TYPE_INCOME}, gnc_account_get_currency_or_parent(account)),
      m_memo(get_widget(builder, "dividend_memo_entry")),
      m_value(builder, gnc_account_get_currency_or_parent(account))
{
    m_account.attach(builder, "dividend_table", "dividend_account_label", 0);
    m_value.attach(builder, "dividend_table", "dividend_label", 1);
}


void
PageDividend::connect(StockTransactionEntry* entry)
{
    m_account.connect(&entry->m_account);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), &entry->m_memo);
    m_value.connect(&entry->m_value);
}

void
PageDividend::prepare(StockTransactionEntry* entry, Logger& logger)
{
    entry->m_memo = get_memo();
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get(), "dividend", logger);
    entry->m_account = m_account.get();
    m_value.set_focus();
}

const char *
PageDividend::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

struct PageCapGain
{
    // capgains page
    GtkWidget * m_page;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
    PageCapGain (GtkBuilder *builder, Account* account);
    void connect(StockTransactionsStockCapGainsEntry* entry);
    void prepare(StockTransactionsStockCapGainsEntry* entry, Logger& logger);
    const char* get_memo();
};

PageCapGain::PageCapGain (GtkBuilder *builder, Account* account) :
    m_page (get_widget (builder, "capgains_details_page")),
    m_account (builder, { ACCT_TYPE_INCOME }, gnc_account_get_currency_or_parent(account)),
    m_memo (get_widget (builder, "capgains_memo_entry")),
    m_value (builder, gnc_account_get_currency_or_parent(account))
{
    m_account.attach(builder, "capgains_table", "capgains_account_label", 0);
    m_value.attach(builder, "capgains_table", "capgains_label", 1);
}

const char *
PageCapGain::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}


void
PageCapGain::connect(StockTransactionsStockCapGainsEntry*entry)
{
    m_account.connect(&entry->m_account);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), &entry->m_memo);
    m_value.connect(&entry->m_value);
}

void
PageCapGain::prepare(StockTransactionsStockCapGainsEntry* entry, Logger& logger)
{
    entry->m_memo = get_memo();
    if (gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get(), "capgains", logger);
        entry->m_account = m_account.get();
        m_value.set_focus();
}

/* The last page of the assistant shows what the resulting transaction will look
 * like.
*/
/* The GncFinishtreeview lays out the transaction.*/
struct GncFinishTreeview
{
    GtkWidget *m_treeview;
    GncFinishTreeview(GtkBuilder *builder);
    void set_tooltip_column(int);
};

GncFinishTreeview::GncFinishTreeview (GtkBuilder *builder) :
    m_treeview{get_widget (builder, "transaction_view")}
{
    auto view = GTK_TREE_VIEW (m_treeview);
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());

    auto store = gtk_list_store_new (NUM_SPLIT_COLS, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    gtk_tree_selection_set_mode (gtk_tree_view_get_selection (view),
                                     GTK_SELECTION_NONE);
    g_object_unref(store);

    auto renderer = gtk_cell_renderer_text_new();
    auto column = gtk_tree_view_column_new_with_attributes
        (_("Account"), renderer, "text", SPLIT_COL_ACCOUNT, nullptr);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    g_object_set (renderer, "ellipsize", PANGO_ELLIPSIZE_END, nullptr);
    column = gtk_tree_view_column_new_with_attributes
        (_("Memo"), renderer, "text", SPLIT_COL_MEMO, nullptr);
    gtk_tree_view_column_set_expand (column, true);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    gtk_cell_renderer_set_alignment (renderer, 1.0, 0.5);
    gtk_cell_renderer_set_padding (renderer, 5, 0);
    column = gtk_tree_view_column_new_with_attributes
        (_("Debit"), renderer, "text", SPLIT_COL_DEBIT, nullptr);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    gtk_cell_renderer_set_alignment (renderer, 1.0, 0.5);
    gtk_cell_renderer_set_padding (renderer, 5, 0);
    column = gtk_tree_view_column_new_with_attributes
        (_("Credit"), renderer, "text", SPLIT_COL_CREDIT, nullptr);
    gtk_tree_view_append_column(view, column);

    renderer = gtk_cell_renderer_text_new();
    gtk_cell_renderer_set_alignment (renderer, 1.0, 0.5);
    gtk_cell_renderer_set_padding (renderer, 5, 0);
    column = gtk_tree_view_column_new_with_attributes
        (_("Units"), renderer,
         "text", SPLIT_COL_UNITS,
         "foreground", SPLIT_COL_UNITS_COLOR,
         nullptr);
    gtk_tree_view_append_column(view, column);
}

void
GncFinishTreeview::set_tooltip_column(int column)
{
    gtk_tree_view_set_tooltip_column(GTK_TREE_VIEW(m_treeview), column);
}

struct PageFinish
{
    // finish page
    GtkWidget * m_page;
    GncFinishTreeview m_view;
    GtkWidget * m_summary;
    PageFinish (GtkBuilder *builder);
    void prepare (GtkWidget *window, StockAssistantModel *model);
};

PageFinish::PageFinish (GtkBuilder *builder) :
    m_page (get_widget (builder, "finish_page")), m_view (builder),
    m_summary (get_widget (builder, "finish_summary")) {}


void
PageFinish::prepare (GtkWidget *window, StockAssistantModel *model)
{
    auto [success, summary, list_of_splits] = model->generate_list_of_splits ();
    auto gtv = GTK_TREE_VIEW(m_view.m_treeview);
    auto list = GTK_LIST_STORE(gtk_tree_view_get_model(gtv));
    gtk_list_store_clear(list);
    for (const auto &line : list_of_splits) {
        GtkTreeIter iter;
        auto tooltip = g_markup_escape_text(line.m_entry->m_memo, -1);
        gtk_list_store_append(list, &iter);
        gtk_list_store_set(
            list, &iter, SPLIT_COL_ACCOUNT,
            xaccAccountGetName(line.m_entry->m_account), SPLIT_COL_MEMO,
            line.m_entry->m_memo, SPLIT_COL_TOOLTIP, tooltip, SPLIT_COL_DEBIT,
            line.m_entry->m_debit_side ? line.m_entry->print_value(model->m_curr_pinfo) : nullptr,
            SPLIT_COL_CREDIT,
            line.m_entry->m_debit_side ? nullptr : line.m_entry->print_value(model->m_curr_pinfo),
            SPLIT_COL_UNITS, line.m_entry->print_amount(line.m_entry->amount()),
            SPLIT_COL_UNITS_COLOR, line.m_units_in_red ? "red" : nullptr, -1);
        g_free(tooltip);
    }
    gtk_label_set_text(GTK_LABEL(m_summary), summary.c_str());
    gtk_assistant_set_page_complete(GTK_ASSISTANT(window), m_page, success);
}

/* The StockAssistantView contains the pages and manages displaying them one at a time. */

struct StockAssistantView {
    GtkWidget * m_window;

    PageTransType m_type_page;
    PageTransDeets m_deets_page;
    PageStockAmount m_stock_amount_page;
    PageStockValue m_stock_value_page;
    PageCash m_cash_page;
    PageFees m_fees_page;
    PageDividend m_dividend_page;
    PageCapGain m_capgain_page;
    PageFinish m_finish_page;

    StockAssistantView(GtkBuilder *builder, Account* account, GtkWidget *parent);
    ~StockAssistantView();
    void set_focus (GtkWidget *widget) { gtk_widget_grab_focus (widget); }
    void set_focus_gae (GtkWidget *gae) { set_focus (GTK_WIDGET (gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (gae)))); }

};

StockAssistantView::StockAssistantView (GtkBuilder *builder, Account* account, GtkWidget *parent) :
    m_window (get_widget (builder, "stock_transaction_assistant")), m_type_page(builder), m_deets_page(builder),
    m_stock_amount_page (builder, account), m_stock_value_page (builder, account), m_cash_page (builder, account),
    m_fees_page (builder, account), m_dividend_page (builder, account), m_capgain_page (builder, account),
    m_finish_page (builder)
{
    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(m_window), "gnc-id-assistant-stock-transaction");
    m_finish_page.m_view.set_tooltip_column(SPLIT_COL_TOOLTIP);
    gtk_window_set_transient_for (GTK_WINDOW (m_window), GTK_WINDOW(parent));
    gnc_window_adjust_for_screen (GTK_WINDOW(m_window));
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(m_window),
                             GTK_WINDOW(parent));
    gtk_widget_show_all (m_window);
    DEBUG ("StockAssistantView constructor\n");
};

StockAssistantView::~StockAssistantView()
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(m_window));
    DEBUG ("StockAssistantView destructor\n");
};

/* The StockAssistantController manages the event handlers and user input. */

struct StockAssistantController
{
    std::unique_ptr<StockAssistantModel> m_model;
    StockAssistantView m_view;
    StockAssistantController (GtkWidget *parent, GtkBuilder* builder, Account* acct)
        : m_model{std::make_unique<StockAssistantModel>(acct)},
          m_view{builder, acct, parent}
    {
        connect_signals (builder);
        DEBUG ("StockAssistantController constructor\n");
    };
    ~StockAssistantController (){ DEBUG ("StockAssistantController destructor\n"); };
    void connect_signals(GtkBuilder *builder);
    void prepare(GtkAssistant* assistant, GtkWidget *page);
};

static gint forward_page_func(int32_t, StockAssistantController*);
static void stock_assistant_window_destroy_cb(GtkWidget *object, gpointer user_data);
static void refresh_handler (GHashTable *changes, gpointer user_data);
static void close_handler (gpointer user_data);

void
StockAssistantController::connect_signals (GtkBuilder *builder)
{
    m_view.m_type_page.connect(m_model.get());
    m_view.m_deets_page.connect(&m_model->m_transaction_date, &m_model->m_transaction_description);
    m_view.m_stock_amount_page.connect(m_model.get());
    m_view.m_stock_value_page.connect(m_model.get());
    m_view.m_cash_page.connect(m_model->m_cash_entry.get());
    auto fees_entry = dynamic_cast<StockTransactionFeesEntry *>(m_model->m_fees_entry.get());
    if (fees_entry)
      m_view.m_fees_page.connect(fees_entry);
    m_view.m_dividend_page.connect(m_model->m_dividend_entry.get());
    auto capgains_entry = dynamic_cast<StockTransactionsStockCapGainsEntry *>(m_model->m_capgains_entry.get());
    if (capgains_entry)
        m_view.m_capgain_page.connect(capgains_entry);

    g_signal_connect (m_view.m_window, "destroy", G_CALLBACK (stock_assistant_window_destroy_cb), this);

    gtk_assistant_set_forward_page_func (GTK_ASSISTANT(m_view.m_window),
                                         (GtkAssistantPageFunc)forward_page_func,
                                         this, nullptr);
    gtk_builder_connect_signals (builder, this); //Stock Assistant View: cancel, close, prepare

    auto component_id = gnc_register_gui_component
        (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, refresh_handler, close_handler, this);
    gnc_gui_component_watch_entity_type (component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
}

void
StockAssistantController::prepare(GtkAssistant* assistant, GtkWidget* page)
{
    auto currentpage = gtk_assistant_get_current_page(assistant);

    switch (currentpage)
    {
    case PAGE_TRANSACTION_TYPE:
        if (!m_model->maybe_reset_txn_types())
            break;
        m_view.m_type_page.prepare(m_model.get());
        break;
    case PAGE_TRANSACTION_DETAILS:
        m_view.m_deets_page.prepare(&m_model->m_transaction_date, &m_model->m_transaction_description);
        break;
    case PAGE_STOCK_AMOUNT:
    {
        auto stock_entry = dynamic_cast<StockTransactionStockEntry*>(m_model->m_stock_entry.get());
        if (stock_entry)
            m_view.m_stock_amount_page.prepare(stock_entry, m_model.get(), m_model->m_logger);
        break;
    }
    case PAGE_STOCK_VALUE:
        m_view.m_stock_value_page.prepare(m_model->m_stock_entry.get(), m_model.get(), m_model->m_logger);
        break;
    case PAGE_CASH:
        m_view.m_cash_page.prepare(m_model->m_cash_entry.get(), m_model->m_logger);
        break;
    case PAGE_FEES:
    {
        auto fees_entry = dynamic_cast<StockTransactionFeesEntry*>(m_model->m_fees_entry.get());
        if (fees_entry)
            m_view.m_fees_page.prepare(fees_entry, m_model->m_logger);
        break;
    }
    case PAGE_DIVIDEND:
        m_view.m_dividend_page.prepare(m_model->m_dividend_entry.get(), m_model->m_logger);
        break;
    case PAGE_CAPGAINS:
    {
        auto capgain_entry = dynamic_cast<StockTransactionsStockCapGainsEntry*>(m_model->m_capgains_entry.get());
        if (capgain_entry)
            m_view.m_capgain_page.prepare(capgain_entry, m_model->m_logger);
        break;
    }
    case PAGE_FINISH:
    {
        m_view.m_finish_page.prepare (m_view.m_window, m_model.get());
        break;
    }
    default:
        break;
    }
}


// These callbacks must be registered with the GtkAssistant so they can't be member functions.

static void
stock_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    delete info;
}

void
stock_assistant_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                         gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    info->prepare(assistant, page);
}

static gint
forward_page_func (gint current_page, StockAssistantController* info)
{
    auto model = info->m_model.get();

    current_page++;
    if (!model->m_txn_type)
        return current_page;
    auto stock_entry = dynamic_cast<StockTransactionStockEntry*>(model->m_stock_entry.get());

    if (!stock_entry->m_amount_enabled && current_page == PAGE_STOCK_AMOUNT)
        current_page++;
    if (!model->m_stock_entry->m_enabled && current_page == PAGE_STOCK_VALUE)
        current_page++;
    if (!model->m_cash_entry->m_enabled && current_page == PAGE_CASH)
        current_page++;
    if (!model->m_fees_entry->m_enabled && current_page == PAGE_FEES)
        current_page++;
    if (!model->m_dividend_entry->m_enabled && current_page == PAGE_DIVIDEND)
        current_page++;
    if (!model->m_capgains_entry->m_enabled && current_page == PAGE_CAPGAINS)
        current_page++;

    return current_page;
}

void
stock_assistant_finish_cb (GtkAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    g_return_if_fail (info->m_model->m_txn_type);

    gnc_suspend_gui_refresh ();
    [[maybe_unused]] auto [success, trans] = info->m_model->create_transaction();
    gnc_resume_gui_refresh ();

    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
}


void
stock_assistant_cancel_cb (GtkAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);

    if (!GNC_IS_ACCOUNT (info->m_model->m_acct))
    {
        PWARN ("account %p does not exist anymore. abort", info->m_model->m_acct);
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    }
}

static void
close_handler (gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    gtk_widget_destroy (info->m_view.m_window);
}

/********************************************************************\
 * gnc_stock_transaction_assistant                                  *
 *   opens up a assistant to record a stock transaction             *
 *                                                                  *
 * Args:   parent  - the parent ofthis window                       *
 *         initial - the initial account to use                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_stock_transaction_assistant (GtkWidget *parent, Account *account)
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file(builder, "assistant-stock-transaction.glade",
                              "stock_transaction_assistant");

    [[maybe_unused]] auto info = new StockAssistantController(parent, builder, account);
    g_object_unref(builder);
}

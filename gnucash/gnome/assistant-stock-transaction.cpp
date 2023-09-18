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

#include <cstddef>
#include <exception>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <cinttypes>
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
#include "gnc-numeric.hpp"
#include "gnc-prefs.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui-util.h"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

/**@addtogroup Stock Transaction Assistant
 * The Stock Transaction Assistant guides the user through collecting the
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

/** A mask-enumerator for defining what information will be collected for a split.
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
    MARKER_SPLIT         = 1 << 9, // stock only, place a no-amount, no-value split in the
                                   // stock account to associate the income.
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

/** class TxnTypeinfo has no functions. It contains a FieldMask
 * corresponding to each entry in the model detailing what data will
 * be collected for a particular transaction type, along with a name
 * to use in the selector box and an explanation of the transaction
 * displayed when it is selected.
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
        N_("Selling stock long, and record capital gain/loss."
           "\n\nIf you are unable to calculate capital gains you can enter a"
           "placeholder amount and correct it in the transaction later.")
    },
    {
        FieldMask::MARKER_SPLIT,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends issued to holder
        N_("Dividend"),
        N_("Company issues cash dividends to holder.\n\nAny dividend being "
           "reinvested must be subsequently recorded as a regular stock purchase.")
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
        N_("Company returns capital, reducing the cost basis without affecting # units. "
           "A distribution previously recorded as a dividend is reclassified to return "
           "of capital, often due to end-of-year tax information.")
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
        N_("Company issues a notional distribution, which is recorded as dividend "
           "income and increases the cost basis without affecting # units.")
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
        N_("Company issues a notional distribution, which is recorded as capital gain "
           "and increases the cost basis without affecting # units.")
    },
    {
        FieldMask::AMOUNT_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split
        N_("Stock split"),
        N_("Company issues additional units, thereby reducing the stock price by a divisor "
           ", while keeping the total monetary value of the overall investment constant. "
           "\n\nIf the split results in a cash in lieu for remainder units, please "
           "record the sale using the Stock Transaction Assistant first, then record the split.")
    },
    {
        FieldMask::AMOUNT_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a reverse split
        N_("Reverse split"),
        N_("Company redeems units, thereby increasing the stock price by a multiple, while "
           "keeping the total monetary value of the overall investment constant.\n\nIf the "
           "reverse split results in a cash in lieu for remainder units, please record the "
           "sale using the Stock Transaction Assistant first, then record the reverse split.")
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
        N_("Buy back stock to cover short position, and record capital gain/loss. "
           "\n\nIf you are unable to calculate capital gains you can enter a placeholder "
           "amount and correct it in the transaction later.")
    },
    {
        FieldMask::MARKER_SPLIT,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends retrieved from holder when shorting stock
        N_("Compensatory dividend"),
        N_("Company issues dividends, and the short stock holder must make a compensatory "
           "payment for the dividend.")
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
        N_("Company returns capital, and the short stock holder must make a compensatory "
           "payment for the returned capital. This reduces the cost basis (less negative, "
           "towards 0.00 value) without affecting # units.")
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
        N_("Company returns capital, and the short stock holder must make a compensatory "
           "payment for the returned capital. This reduces the cost basis (less negative, "
           "towards 0.00 value) without affecting # units. A distribution previously recorded "
           "as a compensatory dividend is reclassified to compensatory return of capital,"
           "often due to end-of-year tax information.")
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
        N_("Company issues a notional distribution, and the short stock holder must make a "
           "compensatory payment for the notional distribution. This is recorded as a "
           "loss/negative dividend income amount, and increases the cost basis (more "
           "negative, away from 0.00 value) without affecting # units.")
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
        N_("Company issues a notional distribution, and the short stock holder must make "
           "a compensatory payment for the notional distribution. This is recorded as a "
           "capital loss amount, and increases the cost basis (more negative, away from "
           "0.00 value) without affecting # units.")
    },
    {
        FieldMask::AMOUNT_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split when shorting stock
        N_("Stock split"),
        N_("Company issues additional units, thereby reducing the stock price by a divisor, "
           "while keeping the total monetary value of the overall investment constant. "
           "\n\nIf the split results in a cash in lieu for remainder units, please "
           "record the cover buy using the Stock Transaction Assistant first, then record the split.")
    },
    {
        FieldMask::AMOUNT_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::CAPITALIZE_DEFAULT,          // fees_amt
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // reverse split when shorting stock.
        N_("Reverse split"),
        N_("Company redeems units, thereby increasing the stock price by a multiple, while "
           "keeping the total monetary value of the overall investment constant.\n\nIf the "
           "reverse split results in a cash in lieu for remainder units, please record the "
           "cover buy using the Stock Transaction Assistant first, then record the reverse split.")
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

/** @class Logger collects diagnostic messages for later display to
 * the user. Proveds 3 categories: error, warning, and info.
 *
 * Functions are simple accessors and setters unless noted.
 */
 
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
    /** Compose all of the logged messages into a bullet list, errors
     * first, then warnings, infos last.
     *
     * @return std::string containing the messages.
     */
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

/** @class StockTransactionEntry
 *
 * Holds the configuration information from the fieldmask and the data
 * to create a single split. The base class is used for cash splits to
 * currency accounts. Except as noted the functions are simple
 * accessors and setters that don't need much documentation.
*/

class StockTransactionEntry
{
protected:
    bool m_enabled;
    bool m_debit_side;
    bool m_allow_zero;
    bool m_allow_negative;
    bool m_input_new_balance = false;
    Account *m_account;
    gnc_numeric m_value;
    const char* m_memo;
    const char* m_action;
    gnc_numeric m_balance = gnc_numeric_zero();
public:
    StockTransactionEntry() :
        m_enabled{false}, m_debit_side{false}, m_allow_zero{false},  m_account{nullptr},
        m_value{gnc_numeric_error(GNC_ERROR_ARG)}, m_memo{nullptr}, m_action{nullptr} {}
    StockTransactionEntry(const char* action) :
        m_enabled{false}, m_debit_side{false}, m_allow_zero{false},  m_account{nullptr},
        m_value{gnc_numeric_error(GNC_ERROR_ARG)}, m_memo{nullptr}, m_action{action} {}
    StockTransactionEntry(const StockTransactionEntry&) = default;
    virtual ~StockTransactionEntry() = default;
    /** Set up the state variables from the FieldMask.
     *
     * @param A Fieldmast to configure the StockTransactionEntry.
     */
    virtual void set_fieldmask(FieldMask mask);
    virtual bool enabled() const { return m_enabled; }
    virtual bool debit_side() const { return m_debit_side; }
    virtual void set_capitalize(bool capitalize) {}
    virtual bool input_new_balance() const { return m_input_new_balance; }
    virtual bool do_capitalize() const { return false; }
    virtual void set_account(Account* account) { m_account = account; }
    virtual Account* account() const { return m_account; }
    virtual const char* print_account() const;
    virtual void set_memo(const char* memo) { m_memo = memo; }
    virtual const char* memo() const { return m_memo; }
    virtual void set_value(gnc_numeric amount);
    virtual GncNumeric value() { return (gnc_numeric_check(m_value) ? GncNumeric{} : GncNumeric(m_value)); }
    virtual void set_amount(gnc_numeric) {}
    virtual gnc_numeric amount() const { return m_value; }
    virtual bool has_amount() const { return false; }
    virtual bool marker_split() const { return false; }
    /* Validates that the value and for stock entry the amount meet
     * the criteria set for the entry by the field mask.
     *
     * @param logger collects any emitted diagnostics.
     */
    virtual void validate_amount(Logger&) const;
    virtual void set_balance(gnc_numeric balance) { m_balance = balance; }
    virtual gnc_numeric get_balance() const { return m_balance; }
    /* Creates a GnuCash split from the data in the entry and adds it
     * to the transaction, adding the account to the account vector so
     * that it can be committed when the transaction is completed.
     *
     * @param trans the transaction to which to add the split
     * @param commits the list of accounts to have edits committed later.
     */
    virtual void create_split(Transaction* trans,  AccountVec& commits) const;
    /**
     * @return a string representation of the value.
     */
    virtual const char* print_value() const;
    /**
     * @return a string representation of the amount.
     */
    virtual const char* print_amount(gnc_numeric amt) const;
    /** Generate a string representation of the value. Internally uses
     * xaccPrintAmount, which writes to a static string, so the result
     * is copied to a std::string to prevent it being replaced by
     * subsequent calls.
     *
     * @return a std:sstring containing a representation of the value.
     */
    virtual std::string amount_str_for_display() const { return ""; }
    /** Calculate the price (amount/value) for non-currency
     * accounts. Note that multiple currencies in stock transaction s
     * are not supported.
     *
     * @return The calculated price for the Stock entry, GNC_ERROR_ARG otherwise.
     */
    virtual gnc_numeric calculate_price() const { return gnc_numeric_error(GNC_ERROR_ARG); }
    /**
     * @return a string representation of the price if valid.
     */
    virtual  const char* print_price() const;
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

const char *
StockTransactionEntry::print_account() const
{
    auto acct_required = m_enabled &&
        !(m_allow_zero && (gnc_numeric_zero_p(m_value) ||
                           gnc_numeric_check(m_value)));
    return m_account ? xaccAccountGetName(m_account) :
        acct_required ? _("missing") : "";
}

void
StockTransactionEntry::set_value(gnc_numeric amount)
{
    if (gnc_numeric_check (amount))
    {
        m_value = gnc_numeric_error(GNC_ERROR_ARG);
        return;
    }

    if (gnc_numeric_negative_p (amount))
    {
        m_value = gnc_numeric_neg(amount);
        m_debit_side = !m_debit_side;
    }
    else
    {
        m_value = amount;
    }
    PINFO("Set %s value to %" PRId64 "/%" PRId64, m_action, m_value.num, m_value.denom);
}

void
StockTransactionEntry::validate_amount(Logger& logger) const
{
    auto add_error = [&logger](const char* format_str, const char* arg)
    {
        char *buf = g_strdup_printf (_(format_str),
                                      g_dpgettext2 (nullptr, "Stock Assistant: Page name", arg));
        logger.error(buf);
        g_free (buf);
    };


    if (gnc_numeric_check (m_value))
    {
        if (!m_allow_zero)
            add_error (N_("Amount for %s is missing."), m_action);
        return;
    }

    if (gnc_numeric_negative_p (m_value) && !m_allow_negative && m_allow_zero)
        add_error (N_("Amount for %s must not be negative."), m_action);

    if (!m_allow_zero && !gnc_numeric_positive_p (m_value))
        add_error (N_("Amount for %s must be positive."), m_action);

    if (!gnc_numeric_zero_p(m_value) && !m_account)
        add_error(N_("The %s amount has no associated account."), m_action);
}

const char *
StockTransactionEntry::print_value() const
{
    if (!m_enabled || (gnc_numeric_check(m_value) && m_allow_zero))
        return nullptr;

    if ((gnc_numeric_check(m_value) || gnc_numeric_zero_p(m_value))
        && !m_allow_zero)
        return _("missing");

    /* Don't combine this with the first if, it would prevent showing
     * "missing" when the value is required.
     */
    if (!m_account)
        return nullptr;

    auto currency{gnc_account_get_currency_or_parent(m_account)};
    auto pinfo{gnc_commodity_print_info(currency, TRUE)};
    return xaccPrintAmount(m_value, pinfo);
}

const char *
StockTransactionEntry::print_amount(gnc_numeric amt) const
{
    if (!m_account || gnc_numeric_check(amt))
        return nullptr;
    auto commodity{xaccAccountGetCommodity(m_account)};
    auto pinfo{gnc_commodity_print_info(commodity, TRUE)};
    return xaccPrintAmount(amt, pinfo);
}

void
StockTransactionEntry::create_split(Transaction *trans,  AccountVec &account_commits) const
{
  g_return_if_fail(trans);
  if (!m_account || gnc_numeric_check(m_value))
    return;
  auto split = xaccMallocSplit(qof_instance_get_book(trans));
  xaccSplitSetParent(split, trans);
  xaccAccountBeginEdit(m_account);
  account_commits.push_back(m_account);
  xaccSplitSetAccount(split, m_account);
  xaccSplitSetMemo(split, m_memo);
  if (m_enabled)
      xaccSplitSetValue(split, m_debit_side ? m_value : gnc_numeric_neg(m_value));
  xaccSplitSetAmount(split, amount());
  PINFO("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
        m_action, m_account ? xaccAccountGetName (m_account) : "Empty!",
        gnc_num_dbg_to_string(m_value),
        gnc_num_dbg_to_string(amount()),
        gnc_num_dbg_to_string(xaccSplitGetValue(split)),
        gnc_num_dbg_to_string(xaccSplitGetAmount(split)));
  gnc_set_num_action(nullptr, split, nullptr,
                     g_dpgettext2(nullptr, "Stock Assistant: Action field",
                                  m_action));
}

const char *
StockTransactionEntry::print_price() const
{
    auto price{calculate_price()};
    if (gnc_numeric_check(price))
//Translators: "N/A" here means that a commodity doesn't have a valid price.
        return _("N/A");
    auto currency{gnc_account_get_currency_or_parent(m_account)};
    auto pinfo{gnc_price_print_info(currency, TRUE)};
    return xaccPrintAmount(price, pinfo);
}

/** Specialized StockTransactionEntry for the stock split. Unlike the
 * base class it has an amount separate from the value and set amount
 * can optionally take a post-transaction balance, used to calculate
 * the amount and split ratio for split and reverse-split
 * transactions. Its validate_amount method first calls the base class
 * member to validate the value then performs addtional checks on the
 * amount and price.
 */
class StockTransactionStockEntry : public StockTransactionEntry
{
    bool m_amount_enabled;
    gnc_numeric m_amount;
    bool m_marker = false;
public:
    StockTransactionStockEntry() :
        StockTransactionEntry{}, m_amount{gnc_numeric_error(GNC_ERROR_ARG)}
    {
        PINFO("Stock Entry");
    }
    StockTransactionStockEntry(const char* action) :
        StockTransactionEntry{action}, m_amount{gnc_numeric_error(GNC_ERROR_ARG)}
    {
        PINFO("Stock Entry");
    }
    void set_fieldmask(FieldMask mask) override;
    void set_amount(gnc_numeric amount) override;
    gnc_numeric amount() const override { return m_amount; }
    bool has_amount() const override { return m_amount_enabled; }
    void validate_amount(Logger& logger) const override;
    void create_split(Transaction *trans, AccountVec &account_commits) const override;
    std::string amount_str_for_display() const override;
    gnc_numeric calculate_price() const override;
    bool marker_split() const override { return m_marker; }
};

void
StockTransactionStockEntry::set_fieldmask(FieldMask mask)
{
    StockTransactionEntry::set_fieldmask(mask);
    m_enabled = mask & (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT);
    m_amount_enabled = mask & (FieldMask::AMOUNT_CREDIT | FieldMask::AMOUNT_DEBIT);
    m_debit_side = mask & (FieldMask::ENABLED_DEBIT | FieldMask::AMOUNT_DEBIT);
    m_input_new_balance = mask & FieldMask::INPUT_NEW_BALANCE;
    m_marker = mask & FieldMask::MARKER_SPLIT;
}


void
StockTransactionStockEntry::set_amount(gnc_numeric amount)
{
     if (!m_amount_enabled || gnc_numeric_check(amount))
     {
         m_amount = gnc_numeric_error(GNC_ERROR_ARG);
         return;
     }

    if (m_input_new_balance)
    {
        if (m_debit_side)
            m_amount = gnc_numeric_sub_fixed(amount, m_balance);
        else
            m_amount = gnc_numeric_sub_fixed(m_balance, amount);

        PINFO("%s set amount for new balance %s", m_memo, print_amount(m_amount));
    }
    else
    {
        m_amount = amount;
        PINFO("%s set amount %s", m_memo, print_amount(m_amount));
    }
}

void
StockTransactionStockEntry::validate_amount(Logger& logger) const
{
    if (m_enabled)
        StockTransactionEntry::validate_amount(logger);

    if (!m_amount_enabled)
        return;

    auto add_error_str = [&logger]
        (const char* str) { logger.error (_(str)); };

    if (gnc_numeric_check(m_amount) || gnc_numeric_zero_p(m_amount))
    {
        add_error_str(_("Amount for stock value is missing."));
        return;
    }

    if (m_input_new_balance)
    {
        auto amount = gnc_numeric_add_fixed(m_debit_side ? m_amount : gnc_numeric_neg(m_amount), m_balance);
        auto delta = gnc_numeric_sub_fixed(amount, m_balance);
        auto ratio = gnc_numeric_div(amount, m_balance,
                                     GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);

        if (gnc_numeric_check(ratio) || !gnc_numeric_positive_p(ratio))
            add_error_str(N_("Invalid stock new balance."));
        else if (gnc_numeric_negative_p(delta) && m_debit_side)
            add_error_str(N_("New balance must be higher than old balance."));
        else if (gnc_numeric_positive_p(delta) && !m_debit_side)
            add_error_str(N_("New balance must be lower than old balance."));

        PINFO("Delta %" PRId64 "/%" PRId64 ", Ratio %" PRId64 "/%" PRId64, delta.num, delta.denom, ratio.num, ratio.denom);
        return;
    }

    if (!gnc_numeric_positive_p(m_amount))
        add_error_str(N_("Stock amount must be positive."));

    auto new_bal = gnc_numeric_add_fixed(m_balance, m_amount);
    if (gnc_numeric_positive_p(m_balance) && gnc_numeric_negative_p(new_bal))
        add_error_str(N_("Cannot sell more units than owned."));
    else if (gnc_numeric_negative_p(m_balance) && gnc_numeric_positive_p(new_bal))
        add_error_str(N_("Cannot cover buy more units than owed."));
}

std::string
StockTransactionStockEntry::amount_str_for_display() const
{
    std::string rv{""};

    if (gnc_numeric_check (m_amount))
        return rv;

    if (m_input_new_balance)
    {
        auto amount = gnc_numeric_add(m_debit_side ? m_amount : gnc_numeric_neg(m_amount), m_balance,
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        auto ratio = gnc_numeric_div (amount, m_balance,
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        PINFO("Computed ratio %" PRId64 "/%" PRId64 "; amount %" PRId64
              "/%" PRId64 " and balance %" PRId64 "/%" PRId64,
              ratio.num, ratio.denom, amount.num, amount.denom, m_balance.num, m_balance.denom);
        if (gnc_numeric_check (ratio) || !gnc_numeric_positive_p (ratio))
            return rv;

        std::ostringstream ret;
        ret << ratio.num << ':' << ratio.denom;
        rv = ret.str();
    }
    else
    {
        auto amount = m_debit_side ? m_amount : gnc_numeric_neg (m_amount);
        amount = gnc_numeric_add_fixed (amount, m_balance);
        rv = print_amount(amount);
    }

    return rv;
};


void
StockTransactionStockEntry::create_split(Transaction *trans, AccountVec &account_commits) const
{
  g_return_if_fail(trans);
  if (!m_account)
      return;
  auto split = xaccMallocSplit(qof_instance_get_book(trans));
  xaccSplitSetParent(split, trans);
  xaccAccountBeginEdit(m_account);
  account_commits.push_back(m_account);
  xaccSplitSetAccount(split, m_account);
  xaccSplitSetMemo(split, m_memo);
  if (m_enabled)
      xaccSplitSetValue(split, m_debit_side ? m_value : gnc_numeric_neg(m_value));
  if (m_amount_enabled)
      xaccSplitSetAmount(split, m_debit_side ? m_amount : gnc_numeric_neg(m_amount));
  if (m_amount_enabled && !m_enabled) // It's a stock split
      xaccSplitMakeStockSplit(split);
  PINFO("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
        m_action, m_account ? xaccAccountGetName (m_account) : "Empty!",
        gnc_num_dbg_to_string(m_value),
        gnc_num_dbg_to_string(amount()),
        gnc_num_dbg_to_string(xaccSplitGetValue(split)),
        gnc_num_dbg_to_string(xaccSplitGetAmount(split)));
  gnc_set_num_action(nullptr, split, nullptr,
                     g_dpgettext2(nullptr, "Stock Assistant: Action field",
                                  m_action));
}

gnc_numeric
StockTransactionStockEntry::calculate_price() const
{
    if (m_input_new_balance ||
        !m_amount_enabled || gnc_numeric_check(m_amount) ||
        !m_enabled || gnc_numeric_check(m_value) ||
        gnc_numeric_zero_p(m_amount) || gnc_numeric_zero_p(m_value))
        return gnc_numeric_error(GNC_ERROR_ARG);

    auto price = gnc_numeric_div(m_value, m_amount,
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

    auto comm{xaccAccountGetCommodity(m_account)};
    auto curr{gnc_account_get_currency_or_parent(m_account)};
    auto ainfo{gnc_commodity_print_info (comm, true)};
    auto pinfo{gnc_price_print_info (curr, true)};
    auto vinfo{gnc_commodity_print_info (curr, true)};

    PINFO("Calculated price %s from value %s and amount %s",
          xaccPrintAmount(price, pinfo), xaccPrintAmount(m_value, vinfo),
          xaccPrintAmount(m_amount, ainfo));
    return price;
}

/** Specialized Entry for the stock account's capital gains split. It
 * has only a special constructor that copies the capital cains entry
 * then sets its account from the stock entry and flips the sign so
 * that if the cash capital gains split is debit-side this one will be
 * credit-side and vice-versa.
 */
class StockTransactionStockCapGainsEntry : public StockTransactionEntry
{
public:
    StockTransactionStockCapGainsEntry(const StockTransactionEntry* cg_entry,
                                        const StockTransactionEntry* stk_entry);
    gnc_numeric amount() const { return gnc_numeric_zero(); }
};

StockTransactionStockCapGainsEntry::StockTransactionStockCapGainsEntry(const StockTransactionEntry *cg_entry,
                                                                       const StockTransactionEntry *stk_entry) :
    StockTransactionEntry(*cg_entry)
{
    m_debit_side = !m_debit_side;
    m_account = stk_entry->account();
}

/** Specialized Entry for fees, taxes, commissions, and so on. It can
 * optionally create its balancing split in a user-provided account or
 * as a 0-amount split on the stock account.
 */
class StockTransactionFeesEntry : public StockTransactionEntry
{
    bool m_capitalize;
public:
    StockTransactionFeesEntry() : StockTransactionEntry{}, m_capitalize{false} {}
    StockTransactionFeesEntry(const char* action) : StockTransactionEntry{action}, m_capitalize{false} {}
    void set_fieldmask(FieldMask mask) override;
    void set_capitalize(bool capitalize) override { m_capitalize = capitalize; }
    bool do_capitalize() const override { return m_capitalize; }
    void validate_amount(Logger &logger) const override;
    void create_split(Transaction *trans,  AccountVec &commits) const override;
};

void
StockTransactionFeesEntry::set_fieldmask(FieldMask mask)
{
    StockTransactionEntry::set_fieldmask(mask);
    m_capitalize = mask & FieldMask::CAPITALIZE_DEFAULT;
}

void
StockTransactionFeesEntry::validate_amount(Logger& logger) const
{
    auto add_error = [&logger](const char* format_str, const char* arg)
    {
        char *buf = g_strdup_printf (_(format_str),
                                      g_dpgettext2 (nullptr, "Stock Assistant: Page name", arg));
        logger.error(buf);
        g_free (buf);
    };


    if (gnc_numeric_check (m_value))
    {
        if (!m_allow_zero)
            add_error (N_("Amount for %s is missing."), m_action);
        return;
    }

    if (gnc_numeric_negative_p (m_value) && !m_allow_negative && m_allow_zero)
        add_error (N_("Amount for %s must not be negative."), m_action);

    if (!m_allow_zero && !gnc_numeric_positive_p (m_value))
        add_error (N_("Amount for %s must be positive."), m_action);

    if (!gnc_numeric_zero_p(m_value) && !m_account && !m_capitalize)
        add_error(N_("The %s amount has no associated account."), m_action);
}

void
StockTransactionFeesEntry::create_split(Transaction* trans,  AccountVec& commits) const
{
  g_return_if_fail(trans);
  if ((!m_account && !m_capitalize) || gnc_numeric_check(m_value))
      return;
  auto split = xaccMallocSplit(qof_instance_get_book(trans));
  xaccSplitSetParent(split, trans);
  if (m_capitalize)
  {
      xaccSplitSetAccount(split, commits[0]); // Should be the stock account
  }
  else
  {
      xaccAccountBeginEdit(m_account);
      commits.push_back(m_account);
      xaccSplitSetAccount(split, m_account);
      xaccSplitSetAmount(split, amount());
  }
  xaccSplitSetMemo(split, m_memo);
  xaccSplitSetValue(split, m_debit_side ? m_value : gnc_numeric_neg(m_value));
  PINFO("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
        m_action, m_account ? xaccAccountGetName (m_account) : "Empty!",
        gnc_num_dbg_to_string(m_value),
        gnc_num_dbg_to_string(amount()),
        gnc_num_dbg_to_string(xaccSplitGetValue(split)),
        gnc_num_dbg_to_string(xaccSplitGetAmount(split)));
  gnc_set_num_action(nullptr, split, nullptr,
                     g_dpgettext2(nullptr, "Stock Assistant: Action field",
                                  m_action));
}

using EntryVec = std::vector<StockTransactionEntry*>;

static void stock_assistant_model_date_changed_cb(GtkWidget*, void*);
static void stock_assistant_model_description_changed_cb(GtkWidget*, void*);

/** @class StockAssistantModel Manages the available transaction types
 * based on the state of the account, the collection and validation of
 * input data from the StockAssistantView and conversion of the data
 * into a GnuCash transaction.
 */

class StockAssistantModel
{
    Account* m_acct;
    gnc_commodity* m_currency;
    time64 m_transaction_date;
    const char* m_transaction_description;
    std::optional<TxnTypeVec> m_txn_types;

    std::optional<TxnTypeInfo> m_txn_type;

    StockTransactionEntryPtr m_stock_entry;
    StockTransactionEntryPtr m_cash_entry;
    StockTransactionEntryPtr m_fees_entry;
    StockTransactionEntryPtr m_dividend_entry;
    StockTransactionEntryPtr m_capgains_entry;
    StockTransactionEntryPtr m_stock_cg_entry; // Required at this level for lifetime management
    Logger m_logger;

    std::optional<time64>     m_txn_types_date;
    bool m_ready_to_create = false;

    EntryVec m_list_of_splits;

public:
    StockAssistantModel (Account *account) :
        m_acct{account},
        m_currency{gnc_account_get_currency_or_parent(account)},
        m_stock_entry{std::make_unique<StockTransactionStockEntry>(NC_ ("Stock Assistant: Page name","Stock"))},
        m_cash_entry{std::make_unique<StockTransactionEntry>(NC_ ("Stock Assistant: Page name","Cash"))},
        m_fees_entry{std::make_unique<StockTransactionFeesEntry>(NC_ ("Stock Assistant: Page name","Fees"))},
        m_dividend_entry{std::make_unique<StockTransactionEntry>(NC_ ("Stock Assistant: Page name","Dividend"))},
        m_capgains_entry{std::make_unique<StockTransactionEntry>(NC_ ("Stock Assistant: Page name","Capital Gains"))}
    {
        DEBUG ("StockAssistantModel constructor\n");
        m_stock_entry->set_account(m_acct);
    };

    ~StockAssistantModel()
    {
        DEBUG ("StockAssistantModel destructor\n");
    };

    /** Selects a TxnTypevec for the user to pick from depending on
     * whether the account has a positive, negative, or zero share
     * balance on the selected transaction date.
     *
     * @return true if the account balance had changed.
     */
    bool maybe_reset_txn_types ();
    /** Accessor function.
     *
     * @return The currently available transaction types or std::nullopt if it's unset.
     */
    const std::optional<TxnTypeVec>& get_txn_types() { return m_txn_types; }
    /** Setter
     *
     * @param the index into the current Transaction Types indicating the selection.
     * @return true if the selection succeeded.
     */
    bool set_txn_type (guint type_idx);
    /** Accessor
     *
     * @return true if the transaction type has been set.
     */
    bool txn_type_valid() { return m_txn_type.has_value(); }
    /** Setter
     *
     * @param time64 for the transaction date.
     */
    void set_transaction_date(time64 date) { m_transaction_date = date;}
    /** Setter
     *
     * @param null-terminated string containing the transaction description.
     */
    void set_transaction_desc(const char* desc) { m_transaction_description = desc; }
    /** Accessor
     *
     * @return the selected transaction type or std::nullopt if it hasn't been set.
     */
    const std::optional<TxnTypeInfo>& txn_type() { return m_txn_type; }
    /** Accessor
     *
     * return string representing the new balance in a split/reverse split transaction.
     */
    std::string get_new_amount_str () const;
    /** Accessor.
     *
     * @return the Stock entry.
     */
    StockTransactionEntry* stock_entry() { return m_stock_entry.get(); }
    /** Accessor.
     *
     * @return the Cash entry.
     */
    StockTransactionEntry* cash_entry() { return m_cash_entry.get(); }
    /** Accessor.
     *
     * @return the Fees  entry.
     */
    StockTransactionEntry* fees_entry() { return m_fees_entry.get(); }
    /** Accessor.
     *
     * @return the Dividend entry.
     */
    StockTransactionEntry* dividend_entry() { return m_dividend_entry.get(); }
    /** Accessor.
     *
     * @return the Capital Gains entry.
     */
    StockTransactionEntry* capgains_entry() { return m_capgains_entry.get(); }
    /** Accessor.
     *
     * @return the logger.
     */
    Logger& logger() { return m_logger; }
    /** Generate the proposed list of splits.
     *
     * This is used to display the proposal to the user in the last
     * page of the assistant and to select on which entries to call
     * `create_split`.
     *
     * @return A tuple containing a boolean indicating that the data
     * passed validation, a string containing diagnostics, and a
     * vector of the Entries to be used in the transacion.
     */
    std::tuple<bool, std::string, EntryVec> generate_list_of_splits ();
    /** Generate a GnuCash transaction from the active entries.
     *
     * @return A tuple containing a boolean indicating that the
     * transaction was created and a pointer to the new transaction.
     */
    std::tuple<bool, Transaction*> create_transaction ();
private:
    /** Private function that adds the calculated price to the book's
     * price database.
     */
    void add_price (QofBook *book);
};

bool
StockAssistantModel::maybe_reset_txn_types ()
{
    auto old_bal = m_stock_entry->get_balance();
    auto new_bal = xaccAccountGetBalanceAsOfDate
        (m_acct, gnc_time64_get_day_end (m_transaction_date));
    if (m_txn_types_date && m_txn_types_date == m_transaction_date &&
        gnc_numeric_equal (old_bal, new_bal))
        return false;
    m_stock_entry->set_balance(new_bal);
    m_txn_types_date = m_transaction_date;
    m_txn_types = gnc_numeric_zero_p (new_bal) ? starting_types
        : gnc_numeric_positive_p (new_bal) ? long_types
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

    m_stock_entry->set_fieldmask(m_txn_type->stock_amount);
    m_fees_entry->set_fieldmask(m_txn_type->fees_value);
    m_capgains_entry->set_fieldmask(m_txn_type->capgains_value);
    m_dividend_entry->set_fieldmask(m_txn_type->dividend_value);
    m_cash_entry->set_fieldmask(m_txn_type->cash_value);
    return true;
};

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

std::tuple<bool, std::string, EntryVec>
StockAssistantModel::generate_list_of_splits() {
    if (!m_txn_types || !m_txn_type)
        return { false, "Error: txn_type not initialized", {} };

    m_logger.clear();
    m_list_of_splits.clear();

    GncNumeric debit{};
    GncNumeric credit{};

    // check the stock transaction date. If there are existing stock
    // transactions dated after the date specified, it is very likely
    // the later stock transactions will be invalidated. warn the user
    // to review them.
    auto last_split_node = g_list_last (xaccAccountGetSplitList (m_acct));
    if (last_split_node)
        check_txn_date(last_split_node, m_transaction_date, m_logger);

    if (m_stock_entry->enabled()  || m_stock_entry->has_amount())
    {
        m_stock_entry->validate_amount(m_logger);
        m_list_of_splits.push_back(m_stock_entry.get());

        auto price{m_stock_entry->calculate_price()};
        if (!gnc_numeric_check(price))
        {
            // Translators: %s refer to: stock mnemonic, broker currency,
            // date of transaction.
            auto tmpl = N_("A price of 1 %s = %s on %s will be recorded.");
            auto date_str = qof_print_date (m_transaction_date);
            auto price_msg = g_strdup_printf
                (_(tmpl),
                 gnc_commodity_get_mnemonic (xaccAccountGetCommodity (m_acct)),
                 m_stock_entry->print_price(), date_str);
            m_logger.info(price_msg);
            g_free (date_str);
            g_free (price_msg);
        }
    }

    if (m_stock_entry->marker_split())
        m_list_of_splits.push_back(m_stock_entry.get());

    if (m_cash_entry->enabled())
    {
        m_cash_entry->validate_amount(m_logger);
        m_list_of_splits.push_back (m_cash_entry.get());
    }

    if (m_fees_entry->enabled())
    {
        m_fees_entry->validate_amount(m_logger);
        if (m_fees_entry->do_capitalize())
            m_fees_entry->set_account(m_acct);
        m_list_of_splits.push_back (m_fees_entry.get());
    }

    if (m_dividend_entry->enabled())
    {
        m_dividend_entry->validate_amount(m_logger);
        m_list_of_splits.push_back (m_dividend_entry.get());
    }

    if (m_capgains_entry->enabled())
    {
        m_stock_cg_entry =
            std::make_unique<StockTransactionStockCapGainsEntry>(m_capgains_entry.get(),
                                                                  m_stock_entry.get());
        m_stock_cg_entry->validate_amount(m_logger);
        m_capgains_entry->validate_amount(m_logger);
        m_list_of_splits.push_back(m_stock_cg_entry.get());
        m_list_of_splits.push_back (m_capgains_entry.get());
    }

    std::for_each(m_list_of_splits.begin(), m_list_of_splits.end(),
                    [&debit, &credit](auto& entry) {
                        if (entry->debit_side())
                            debit += entry->value();
                        else
                            credit += entry->value();
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
            auto pinfo{gnc_commodity_print_info (m_currency, true)};
            auto debit_str = g_strdup (xaccPrintAmount (debit, pinfo));
            auto credit_str = g_strdup (xaccPrintAmount (credit, pinfo));
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
        m_list_of_splits.clear();
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
                   [&](auto& entry){ entry->create_split (trans, accounts); });
    add_price (book);
    xaccTransCommitEdit (trans);
    std::for_each (accounts.begin(), accounts.end(), xaccAccountCommitEdit);
    m_list_of_splits.clear();
    m_ready_to_create = false;
    return {true, trans};
}

void
StockAssistantModel::add_price (QofBook *book)
{
    auto stock_price{m_stock_entry->calculate_price()};
    if (gnc_numeric_check(stock_price))
        return;

    auto price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, xaccAccountGetCommodity (m_acct));
    gnc_price_set_currency (price, m_currency);
    gnc_price_set_time64 (price, m_transaction_date);
    gnc_price_set_source (price, PRICE_SOURCE_STOCK_TRANSACTION);
    gnc_price_set_typestr (price, PRICE_TYPE_UNK);
    gnc_price_set_value (price, stock_price);
    gnc_price_commit_edit (price);

    auto pdb = gnc_pricedb_get_db (book);
    if (!gnc_pricedb_add_price (pdb, price))
        PWARN ("error adding price");

    gnc_price_unref (price);
}

static void
stock_assistant_model_date_changed_cb(GtkWidget* widget, void* data)
{
    auto model{static_cast<StockAssistantModel*>(data)};
    model->set_transaction_date(gnc_date_edit_get_date_end(GNC_DATE_EDIT(widget)));
}

static void
stock_assistant_model_description_changed_cb(GtkWidget* widget, void* data)
{
    auto model{static_cast<StockAssistantModel*>(data)};
    model->set_transaction_desc(gtk_entry_get_text(GTK_ENTRY(widget)));
}

/* ********************* View Classes ************************/

/* ***************** Generic Event Callbacks ****************/
static void
text_entry_changed_cb (GtkWidget *widget, StockTransactionEntry* entry)
{
    entry->set_memo(gtk_entry_get_text (GTK_ENTRY (widget)));
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

/** C++ wrapper for the GncDateEdit control (see
 * gnucash/gnome-utils/gnc-date-edit.h).
 */

class GncDateEdit
{
    GtkWidget *m_edit;
public:
    GncDateEdit(GtkBuilder *builder) :
        m_edit{gnc_date_edit_new(gnc_time(nullptr), FALSE, FALSE)} {}
    void attach(GtkBuilder *builder, const char *table_ID, const char *label_ID,
                int row);
    time64 get_date_time() { return gnc_date_edit_get_date_end(GNC_DATE_EDIT(m_edit)); }
    void connect(GCallback, gpointer);
};

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
GncDateEdit::connect(GCallback cb, gpointer data)
{
    g_signal_connect(m_edit, "date_changed", cb, data);
}

/** C++ wrapper for GncAmountEdit, see
 * gnucash/gnome-utils/gnc-amount-edit.h.
 */
class GncAmountEdit
{
    GtkWidget *m_edit;
public:
    GncAmountEdit (GtkBuilder *builder, gnc_commodity *commodity);
    void attach (GtkBuilder *builder, const char *table_id,
                 const char *label_ID, int row);
    GtkWidget* widget() {
        return gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(m_edit));
    }
    gnc_numeric get ();
    void connect (GCallback cb, gpointer data);
    void set_owner (gpointer obj);
};

static void
value_changed_cb (GtkWidget* widget, StockTransactionEntry* entry)
{
    g_return_if_fail(GNC_IS_AMOUNT_EDIT(widget));
    gnc_numeric value;
    auto invalid{gnc_amount_edit_expr_is_valid(GNC_AMOUNT_EDIT(widget),
                                             &value, true, nullptr)};
    entry->set_value(invalid ? gnc_numeric_error(GNC_ERROR_ARG) : value);
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
GncAmountEdit::connect (GCallback cb, gpointer data)
{
    g_signal_connect(m_edit, "changed", cb, data);
}

void
GncAmountEdit::set_owner(gpointer obj)
{
    g_object_set_data(G_OBJECT (m_edit), "owner", obj);
}

using AccountTypeList = std::vector<GNCAccountType>;

/** C++ wrapper for GncAccounSel, see
 * gnucash/gnome-utils/gnc-account-sel.h.
 */
class GncAccountSelector
{
    GtkWidget* m_selector;
public:
    GncAccountSelector (GtkBuilder *builder, AccountTypeList types,
                        gnc_commodity *currency);
    void attach (GtkBuilder *builder, const char *table_id,
                 const char *label_ID, int row);
    void connect (StockTransactionEntry*);
    void set (Account *acct) { gnc_account_sel_set_account (GNC_ACCOUNT_SEL (m_selector), acct, TRUE); }
    void set_sensitive(bool sensitive);
    Account *get () { return gnc_account_sel_get_account (GNC_ACCOUNT_SEL (m_selector)); }
};

static void
gnc_account_sel_changed_cb (GtkWidget* widget, StockTransactionEntry* entry)
{
    g_return_if_fail (GNC_IS_ACCOUNT_SEL (widget));
    entry->set_account(gnc_account_sel_get_account (GNC_ACCOUNT_SEL (widget)));
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
    gnc_account_sel_set_default_new_commodity(GNC_ACCOUNT_SEL(m_selector), currency);
    gnc_account_sel_set_new_account_modal (GNC_ACCOUNT_SEL(m_selector), true);
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
GncAccountSelector::connect (StockTransactionEntry* entry)
{
    g_signal_connect(m_selector, "account_sel_changed", G_CALLBACK (gnc_account_sel_changed_cb), entry);
}

void
GncAccountSelector::set_sensitive(bool sensitive)
{
       gtk_widget_set_sensitive(m_selector, sensitive);
}


/** GtkContainer focus signal handler.
 *
 * When an assistant page (a GtkContainer) is displayed it emits a
 * focus signal. This handler grabs the passed-in widget so that it
 * will have the initial focus instead of the first item on the
 * page. The focus signal is also used by GtkContainer to handle tab
 * and arrow keys, so we immediately disconnect it to allow them to
 * function. It's connected in the page's prepare function instead of
 * the connect one so that it can set the initial focus every time the
 * user visits the page.
 */
static void
assistant_page_set_focus(GtkWidget* page, [[maybe_unused]]GtkDirectionType type,  GtkWidget* entry)
{
    gtk_widget_grab_focus(entry);
    g_signal_handlers_disconnect_by_data(page, entry);
}
/** Page classes generate the several pages of the assistant. In
 * general they have two functions, prepare and connect, plus a
 * callback for each widget on the page and helper functions.
 *
 * Pages are displayed only if the split is enabled in the transaction
 * info. For most pages that means that the info is set to
 * ENABLED_CREDIT or ENABLED_DEBIT, but for the stock amount page the
 * info must be set to AMOUNT_CREDIT or AMOUNT_DEBIT. DISABLED is a
 * placeholder as 0 is not testable.
 *
 * Empty entries in amount edits are treated as 0 if the TxnTypeInfo
 * for the split has ALLOW_ZERO set. Negative numbers are permitted
 * only if ALLOW_NEGATIVE is set; that's used for amounts where the
 * sign cannot be determined from the transaction type (like capital
 * gains) or where it makes sense for consistency (like the new
 * balance for a stock split in a short position.)
 *
 * Prepare is called before a page is displayed and performs
 * specializtion for the transaction type and populates widgets with
 * already-known values; the latter may be available if the user has
 * visited the page before for this transaction.
 *
 * Connect connects the page's widgets signals with the handlers that
 * transfer data to the model.
 */

/** The Transaction Type page.
 *
 * This page collects the transaction type that's used to select the TxnTypeInfo that drives the rest of the process.
 */
class PageTransType {
    // transaction type page
    GtkWidget * m_page;
    GtkWidget * m_type;
    GtkWidget * m_explanation;
public:
    PageTransType(GtkBuilder *builder);
    void prepare(StockAssistantModel* model);
    int get_transaction_type_index ();
    void set_transaction_types (const TxnTypeVec& txn_types);
/** Sets the explanation text for the selected transaction type,
 * allowing the user to make sure that the selected transaction
 * matches what they want to do.
 *
 * @param transaction explanation from the tranaction type's txntypeinfo.
 */
    void set_txn_type_explanation (const gchar *txt);
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
    const auto& txn_types{model->get_txn_types()};
    if (!txn_types)
        return;

    set_transaction_types(txn_types.value());
    change_txn_type (model);
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_type);
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

    if (!model->set_txn_type(type_idx))
        return;
    auto txn_type{model->txn_type()};
    set_txn_type_explanation (_(txn_type->explanation));
}

void
PageTransType::connect(StockAssistantModel *model)
{
    g_signal_connect(m_type, "changed",
                     G_CALLBACK (page_trans_type_changed_cb), model);
}

/** Transaction Details page. Collects the transaction date (changing
 * of which may trigger the model to run maybe_change_txn_types if the
 * balance on the new date is different from the balance on the prior
 * or default date) and the transaction description.
 */
class PageTransDeets
{
    // transaction details page
    GtkWidget *m_page;
    GncDateEdit m_date;
    GtkWidget *m_description;
public:
    PageTransDeets (GtkBuilder *builder);
    time64 get_date_time () { return m_date.get_date_time(); }
    const char* get_description () { return gtk_entry_get_text (GTK_ENTRY (m_description)); }
    void connect (StockAssistantModel*);
    void prepare(StockAssistantModel*);
};

PageTransDeets::PageTransDeets (GtkBuilder *builder) :
    m_page (get_widget (builder, "transaction_details_page")),
    m_date (builder),
    m_description (get_widget (builder, "transaction_description_entry"))
{
    m_date.attach(builder,  "transaction_details_table", "transaction_date_label", 0);
}

void
PageTransDeets::connect(StockAssistantModel* model)
{
    m_date.connect(G_CALLBACK (stock_assistant_model_date_changed_cb),
                   static_cast<void*>(model));
    g_signal_connect(m_description, "changed",
                     G_CALLBACK (stock_assistant_model_description_changed_cb),
                     static_cast<void*>(model));
}

void
PageTransDeets::prepare(StockAssistantModel* model)
{
    model->set_transaction_date(get_date_time());
    model->set_transaction_desc(get_description());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_description);
}

/** Stock Amount page. Display and the amount entered depend on the
 * type of the transaction. It always displays the balanance on the
 * transaction date before the transaction. For buy and sell
 * transactions it collects the number of units bought or sold (always
 * as a positive number) and displays the new balance. For split and
 * reverse-split transactions it collects the balance after the
 * transaction and computes the change in the amount of shares and the
 * split ratio, displaying the latter.
 */
class PageStockAmount
{
    // stock amount page
    GtkWidget * m_page;
    GtkWidget * m_title;
    GtkWidget * m_prev_amount;
    GtkWidget * m_next_amount;
    GtkWidget * m_next_amount_label;
    GncAmountEdit m_amount;
    GtkWidget * m_amount_label;
public:
    PageStockAmount (GtkBuilder *builder, Account* account);
    void prepare (StockTransactionEntry*);
    gnc_numeric get_stock_amount () { return m_amount.get(); }
    void set_stock_amount (std::string new_amount_str);
    void connect(StockTransactionEntry* entry);
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
PageStockAmount::prepare (StockTransactionEntry* entry)
{
    gtk_label_set_text_with_mnemonic
        (GTK_LABEL (m_amount_label),
         entry->input_new_balance() ? _("Ne_w Balance") : _("_Shares"));
    gtk_label_set_text
        (GTK_LABEL (m_next_amount_label),
         entry->input_new_balance() ? _("Ratio") : _("Next Balance"));
    gtk_label_set_text
        (GTK_LABEL (m_title),
         entry->input_new_balance() ?
         _("Enter the new balance of shares after the stock split.") :
         _("Enter the number of shares you gained or lost in the transaction."));
    gtk_label_set_text (GTK_LABEL (m_prev_amount), entry->print_amount(entry->get_balance()));
    if (!gnc_numeric_check(get_stock_amount()))
        entry->set_amount(get_stock_amount());
    set_stock_amount(entry->amount_str_for_display());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_amount.widget());
}

static void
page_stock_amount_changed_cb(GtkWidget *widget, StockTransactionEntry* entry)
{
    auto me = static_cast<PageStockAmount*>(g_object_get_data (G_OBJECT (widget), "owner"));
    entry->set_amount(me->get_stock_amount());
    me->set_stock_amount(entry->amount_str_for_display());
}

void
PageStockAmount::connect(StockTransactionEntry* entry)
{
    m_amount.connect(G_CALLBACK (page_stock_amount_changed_cb), entry);
    m_amount.set_owner(static_cast<gpointer>(this));
}

void
PageStockAmount::set_stock_amount (std::string new_amount_str)
{
    gtk_label_set_text (GTK_LABEL(m_next_amount), new_amount_str.c_str());
}

/** Stock Value page. It collects the currency value of the stock
 * traded and computes and displays the resulting price. It also
 * collects the memo for the primary stock split.
 */
class PageStockValue
{
    // stock value page
    GtkWidget * m_page;
    GncAmountEdit m_value;
    GtkWidget * m_price;
    GtkWidget * m_memo;
public:
    PageStockValue (GtkBuilder *builder, Account* account);
    const char* get_memo ();
    void connect(StockTransactionEntry* entry);
    void prepare(StockTransactionEntry* entry);
    GncAmountEdit& value_edit() { return m_value; }
    void set_price(const gchar *val);
};

static void
page_stock_value_changed_cb(GtkWidget *widget, StockTransactionEntry* entry)
{
    auto me = static_cast<PageStockValue*>(g_object_get_data (G_OBJECT (widget), "owner"));
    entry->set_value (me->value_edit().get());
    me->set_price(entry->print_price());
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
PageStockValue::connect(StockTransactionEntry* entry)
{
    m_value.connect(G_CALLBACK (page_stock_value_changed_cb), entry);
    m_value.set_owner (static_cast<gpointer>(this));
    g_signal_connect (m_memo, "changed", G_CALLBACK(text_entry_changed_cb), entry);
}

void
PageStockValue::prepare(StockTransactionEntry* entry)
{
    entry->set_memo(get_memo());
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get());
    set_price(entry->print_price());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_value.widget());
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

/** The Cash page collects the cash account (usually corresponds the
 * broker's cash management account), the amount of cash, and the memo
 * for the cash split. Accounts are restricted to types ASSET and
 * BANK.
 */
class PageCash
{
    // cash page
    GtkWidget * m_page;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
public:
    PageCash (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry* entry);
    void prepare(StockTransactionEntry* entry);
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
    m_account.connect(entry);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), entry);
    m_value.connect(G_CALLBACK(value_changed_cb), entry);
}

void
PageCash::prepare(StockTransactionEntry* entry)
{
    entry->set_memo(get_memo());
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get());
    entry->set_account(m_account.get());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_value.widget());
}

const char *
PageCash::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

/** Fees page. Controls for selecting whether to capitalize
 * (i.e. charge them to the stock value), an account selector
 * restricted to EXPENSE accounts if not capitalized, an amount and a
 * memo.
 */
class PageFees
{
    // fees page
    GtkWidget * m_page;
    GtkWidget * m_capitalize;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
    Account* m_stock_account;
public:
    PageFees (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry*);
    bool get_capitalize_fees ();
    const char* get_memo();
    void set_capitalize_fees (bool state);
    void set_account (Account *acct) { m_account.set(acct); }
    Account* stock_account() { return m_stock_account; }
    void update_fees_acct_sensitive (bool sensitive);
    void prepare(StockTransactionEntry*);
};

PageFees::PageFees(GtkBuilder *builder, Account* account)
    : m_page(get_widget(builder, "fees_details_page")),
      m_capitalize(
          get_widget(builder, "capitalize_fees_checkbutton")),
      m_account(builder, {ACCT_TYPE_EXPENSE}, gnc_account_get_currency_or_parent(account)),
      m_memo(get_widget(builder, "fees_memo_entry")),
      m_value(builder, gnc_account_get_currency_or_parent(account)),
      m_stock_account(account)
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
PageFees::update_fees_acct_sensitive(bool sensitive)
{
    m_account.set_sensitive(sensitive);
}

static void
capitalize_fees_toggled_cb (GtkWidget *widget, StockTransactionEntry *entry)
{
    g_return_if_fail (entry);
    auto me = static_cast<PageFees *>(g_object_get_data (G_OBJECT (widget), "owner"));
    g_return_if_fail (me);
    bool cap =  me->get_capitalize_fees();
    entry->set_capitalize(cap);
    if (cap)
        entry->set_account(me->stock_account());
    me->update_fees_acct_sensitive(!cap);
}

void
PageFees::connect(StockTransactionEntry* entry)
{
    m_account.connect(entry);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb),  entry);
    m_value.connect(G_CALLBACK(value_changed_cb), entry);
    g_object_set_data(G_OBJECT (m_capitalize), "owner", this);
    g_signal_connect (m_capitalize, "toggled", G_CALLBACK (capitalize_fees_toggled_cb), entry);
}

void
PageFees::prepare(StockTransactionEntry* entry)
{
    set_capitalize_fees (entry->do_capitalize());
    entry->set_memo(get_memo());
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value (m_value.get());
    entry->set_account(m_account.get());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_value.widget());
}

/** Dividend page, collects an amount, an INCOME account, and a memo.
 */
class PageDividend
{
    // dividend page
    GtkWidget *m_page;
    GncAccountSelector m_account;
    GtkWidget *m_memo;
    GncAmountEdit m_value;
public:
    PageDividend (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry*);
    void prepare(StockTransactionEntry*);
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
    m_account.connect(entry);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), entry);
    m_value.connect(G_CALLBACK(value_changed_cb), entry);
}

void
PageDividend::prepare(StockTransactionEntry* entry)
{
    entry->set_memo(get_memo());
    if (!gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get());
    entry->set_account(m_account.get());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_value.widget());
}

const char *
PageDividend::get_memo()
{
    return gtk_entry_get_text(GTK_ENTRY (m_memo));
}

class PageCapGain
{
    // capgains page
    GtkWidget * m_page;
    GncAccountSelector m_account;
    GtkWidget * m_memo;
    GncAmountEdit m_value;
public:
    PageCapGain (GtkBuilder *builder, Account* account);
    void connect(StockTransactionEntry* entry);
    void prepare(StockTransactionEntry* entry);
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
PageCapGain::connect(StockTransactionEntry*entry)
{
    m_account.connect(entry);
    g_signal_connect(m_memo, "changed", G_CALLBACK(text_entry_changed_cb), entry);
    m_value.connect(G_CALLBACK(value_changed_cb), entry);
}

void
PageCapGain::prepare(StockTransactionEntry* entry)
{
    entry->set_memo(get_memo());
    if (gnc_numeric_check(m_value.get()))
        entry->set_value(m_value.get());
    entry->set_account(m_account.get());
    g_signal_connect(m_page, "focus", G_CALLBACK(assistant_page_set_focus), m_value.widget());
}


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

/* Displays a summary of the transactions as a list. */
class GncFinishTreeview
{
    GtkWidget *m_treeview;
public:
    GncFinishTreeview(GtkBuilder *builder);
    /** Extract the information from the StockTransactionEntries in
     * the vector created by the model's `make_list_of_splits`
     * function and write it into the list view.
     */
    void load(const EntryVec& list_of_splits);
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
    gtk_tree_view_set_tooltip_column(GTK_TREE_VIEW(m_treeview),
                                     SPLIT_COL_TOOLTIP);}

void
GncFinishTreeview::load(const EntryVec& list_of_splits)
{
    auto gtv = GTK_TREE_VIEW(m_treeview);
    bool negative_in_red = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_NEGATIVE_IN_RED);
    auto list = GTK_LIST_STORE(gtk_tree_view_get_model(gtv));
    gtk_list_store_clear(list);
    for (const auto &entry : list_of_splits) {
        GtkTreeIter iter;
        auto memo{entry->memo()};
        auto tooltip = (memo && *memo ?
                        g_markup_escape_text(memo, -1) : strdup(""));
        /* print_value and print_amount rely on xaccPrintAmount that
         * uses static memory so the result needs to be copied
         * immediately or the second call overwrites the results of
         * the first one.
         */
        auto char2str{[](const char* str) -> std::string {
            return std::string{ str ? str : "" }; }};
        auto amount{char2str(entry->print_value())};
        auto units{char2str(entry->has_amount() ?
                            entry->print_amount(entry->debit_side() ? entry->amount() :
                                                       gnc_numeric_neg(entry->amount())) : "")};
        auto units_in_red{negative_in_red && !entry->debit_side()};
        gtk_list_store_append(list, &iter);
        gtk_list_store_set(
            list, &iter,
            SPLIT_COL_ACCOUNT,
            entry->print_account(), SPLIT_COL_MEMO,
            entry->memo(), SPLIT_COL_TOOLTIP, tooltip, SPLIT_COL_DEBIT,
            entry->debit_side() ? amount.c_str() : nullptr,
            SPLIT_COL_CREDIT,
            entry->debit_side() ? nullptr : amount.c_str(),
            SPLIT_COL_UNITS, units.c_str(),
            SPLIT_COL_UNITS_COLOR, units_in_red ? "red" : nullptr, -1);
        g_free(tooltip);
    }
}

/** Finish page. Displays the List View summarizing the transaction
 * along with any diagnostic messages recorded by the model's logger.
 */
class PageFinish
{
    // finish page
    GtkWidget * m_page;
    GncFinishTreeview m_view;
    GtkWidget * m_summary;
public:
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
    m_view.load(list_of_splits);
    gtk_label_set_text(GTK_LABEL(m_summary), summary.c_str());
    gtk_assistant_set_page_complete(GTK_ASSISTANT(window), m_page, success);
}

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

/** Contains the pages and manages displaying them one at a time. */
class StockAssistantView {
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
public:
    StockAssistantView(GtkBuilder *builder, Account* account, GtkWidget *parent);
    ~StockAssistantView();
    /** Calls each page's connect function.
     *
     * @param The model.
     */
    void connect(StockAssistantModel*);
    /** Calls the specified page's prepare function. As with connect
     * the association with the model's entry might be better at the
     * Assistant level.
     *
     * @param page The page who's prepare function to call.
     * @param model
     */
    void prepare(int page, StockAssistantModel*);
    GtkWidget* window() { return m_window; }
};

StockAssistantView::StockAssistantView (GtkBuilder *builder, Account* account, GtkWidget *parent) :
    m_window (get_widget (builder, "stock_transaction_assistant")), m_type_page(builder), m_deets_page(builder),
    m_stock_amount_page (builder, account), m_stock_value_page (builder, account), m_cash_page (builder, account),
    m_fees_page (builder, account), m_dividend_page (builder, account), m_capgain_page (builder, account),
    m_finish_page (builder)
{
    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(m_window), "gnc-id-assistant-stock-transaction");
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
    gtk_widget_destroy (m_window);
    DEBUG ("StockAssistantView destructor\n");
};

/** Callback for determining the next page.
 *
 * @param current page: Inout parameter.
 * @param data The model as a void*.
 */

static gint
forward_page_func (gint current_page, void* data)
{
    auto model{static_cast<StockAssistantModel*>(data)};
    current_page++;
    if (!model->txn_type_valid())
        return current_page;

    if (!model->stock_entry()->has_amount() && current_page == PAGE_STOCK_AMOUNT)
        current_page++;
    if (!model->stock_entry()->enabled() && current_page == PAGE_STOCK_VALUE)
        current_page++;
    if (!model->cash_entry()->enabled() && current_page == PAGE_CASH)
        current_page++;
    if (!model->fees_entry()->enabled() && current_page == PAGE_FEES)
        current_page++;
    if (!model->dividend_entry()->enabled() && current_page == PAGE_DIVIDEND)
        current_page++;
    if (!model->capgains_entry()->enabled() && current_page == PAGE_CAPGAINS)
        current_page++;

    return current_page;
}

void
StockAssistantView::connect(StockAssistantModel* model)
{
    m_type_page.connect(model);
    m_deets_page.connect(model);
    m_stock_amount_page.connect(model->stock_entry());
    m_stock_value_page.connect(model->stock_entry());
    m_cash_page.connect(model->cash_entry());
    m_fees_page.connect(model->fees_entry());
    m_dividend_page.connect(model->dividend_entry());
    m_capgain_page.connect(model->capgains_entry());

    gtk_assistant_set_forward_page_func (GTK_ASSISTANT(m_window),
                                         (GtkAssistantPageFunc)forward_page_func,
                                         model, nullptr);
}

void
StockAssistantView::prepare(int page, StockAssistantModel* model)
{
    g_return_if_fail (page < PAGE_STOCK_AMOUNT || model->txn_type_valid());
    switch (page)
    {
    case PAGE_TRANSACTION_TYPE:
        if (!model->maybe_reset_txn_types())
            break;
        m_type_page.prepare(model);
        break;
    case PAGE_TRANSACTION_DETAILS:
        m_deets_page.prepare(model);
        break;
    case PAGE_STOCK_AMOUNT:
    {
        m_stock_amount_page.prepare(model->stock_entry());
        break;
    }
    case PAGE_STOCK_VALUE:
        m_stock_value_page.prepare(model->stock_entry());
        break;
    case PAGE_CASH:
        m_cash_page.prepare(model->cash_entry());
        break;
    case PAGE_FEES:
    {
        m_fees_page.prepare(model->fees_entry());
        break;
    }
    case PAGE_DIVIDEND:
        m_dividend_page.prepare(model->dividend_entry());
        break;
    case PAGE_CAPGAINS:
    {
        m_capgain_page.prepare(model->capgains_entry());
        break;
    }
    case PAGE_FINISH:
    {
        m_finish_page.prepare (m_window, model);
        break;
    }
    default:
        break;
    }
}

/** The overall manager for the assistant, contains the model and view
 * objects and is responsible for creating, connecting, and destroying
 * both.
 */
class StockAssistantController
{
    std::unique_ptr<StockAssistantModel> m_model;
    StockAssistantView m_view;
    bool m_destroying = false;
public:
    StockAssistantController (GtkWidget *parent, GtkBuilder* builder, Account* acct)
        : m_model{std::make_unique<StockAssistantModel>(acct)},
          m_view{builder, acct, parent}
    {
        connect_signals (builder);
        DEBUG ("StockAssistantController constructor\n");
    };
    ~StockAssistantController ();
    void connect_signals(GtkBuilder *builder);
    void prepare(GtkAssistant* assistant, GtkWidget *page);
    void finish();
    bool destroying() { return m_destroying; }
};

static void stock_assistant_window_destroy_cb(GtkWidget *object, gpointer user_data);
static void refresh_handler (GHashTable *changes, gpointer user_data);
static void close_handler (gpointer user_data);

StockAssistantController::~StockAssistantController()
{
    m_destroying = true;
    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, this);
}

void
StockAssistantController::connect_signals (GtkBuilder *builder)
{
    m_view.connect(m_model.get());
    gtk_builder_connect_signals (builder, this); //Stock Assistant View: cancel, close, prepare
    g_signal_connect (m_view.window(), "destroy",
                      G_CALLBACK (stock_assistant_window_destroy_cb), this);


    auto component_id = gnc_register_gui_component
        (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, refresh_handler, close_handler, this);
    gnc_gui_component_watch_entity_type (component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
}

void
StockAssistantController::prepare(GtkAssistant* assistant, GtkWidget* page)
{
    auto currentpage = gtk_assistant_get_current_page(assistant);
    m_view.prepare(currentpage, m_model.get());
}

void
StockAssistantController::finish()
{
    g_return_if_fail (m_model->txn_type_valid());

    gnc_suspend_gui_refresh ();
    [[maybe_unused]] auto [success, trans] = m_model->create_transaction();
    gnc_resume_gui_refresh ();

    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, this);
}

// These callbacks must be registered with the GtkAssistant so they can't be member functions.
/* The StockAssistantController manages the event handlers and user input. */
void
stock_assistant_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                         gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    info->prepare(assistant, page);
}


static void
stock_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data) //crashes before this gets called.
{
    auto controller = static_cast<StockAssistantController*>(user_data);
    if (controller->destroying())
        return;

    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, controller);
}


void
stock_assistant_finish_cb (GtkAssistant *assistant, gpointer user_data)
{
    auto controller = static_cast<StockAssistantController*>(user_data);
    controller->finish();
}


void
stock_assistant_cancel_cb (GtkAssistant *assistant, gpointer user_data)
{
    auto controller = static_cast<StockAssistantController*>(user_data);
    if (controller->destroying())
        return;
    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, controller);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    if (!changes) // None of our watches fired, we don't need to do anything.
        return;

/* We have only one watch so we don't need to check GUIDs. There
 * should be only one entry, so just get the value and see if it
 * matches QOF_EVENT_DESTROY.
 */
    auto list = g_hash_table_get_values(changes);
    for (auto node = list; node; node = g_list_next(node))
    {
        auto change{static_cast<EventInfo*>(node->data)};
        if (change->event_mask & QOF_EVENT_DESTROY)
        {
            PWARN ("Stock account destroyed, cancelling assistant.");
            auto controller = static_cast<StockAssistantController*>(user_data);
            gnc_close_gui_component_by_data(ASSISTANT_STOCK_TRANSACTION_CM_CLASS, controller);
        }
    }
    g_list_free (list);
}

static void
close_handler (gpointer user_data)
{
    auto controller = static_cast<StockAssistantController*>(user_data);
    if (controller->destroying())
        return;
    delete controller;
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

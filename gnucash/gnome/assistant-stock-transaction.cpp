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

#include "Transaction.h"
#include "engine-helpers.h"
#include "dialog-utils.h"
#include "assistant-stock-transaction.h"
#include "gnc-account-sel.h"
#include "gnc-amount-edit.h"
#include "gnc-prefs.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-tree-view-account.h"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

extern "C"
{
// These functions are the GtkAssistant primary button callbacks. They're
// connected to their signals in assistant-stock-transaction.glade.
void stock_assistant_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                                 gpointer user_data);
void stock_assistant_finish_cb  (GtkAssistant *assistant, gpointer user_data);
void stock_assistant_cancel_cb  (GtkAssistant *gtkassistant, gpointer user_data);
}

enum class FieldMask : unsigned;
bool operator &(FieldMask lhs, FieldMask rhs);
FieldMask operator |(FieldMask lhs, FieldMask rhs);
FieldMask operator ^(FieldMask lhs, FieldMask rhs);

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

/** structures *********************************************************/

enum class FieldMask : unsigned
{
    DISABLED = 0,
    ENABLED_DEBIT,
    ENABLED_CREDIT,
    ALLOW_ZERO = 4,
    ALLOW_NEGATIVE = 8,
    INPUT_NEW_BALANCE = 16,     // stock_amt only: instead of amount, get new balance
    CAPITALIZE_DEFAULT = 32,    // fees only: capitalize by default into stock acct
    CAPGAINS_IN_STOCK = 64,     // capg only: add a balancing split in stock acct
};

FieldMask operator |(FieldMask lhs, FieldMask rhs)
{
    return static_cast<FieldMask> (static_cast<unsigned>(lhs) |
                                   static_cast<unsigned>(rhs));
};

bool operator &(FieldMask lhs, FieldMask rhs)
{
    return (static_cast<unsigned>(lhs) & static_cast<unsigned>(rhs));
};

FieldMask operator ^(FieldMask lhs, FieldMask rhs)
{
    return static_cast<FieldMask> (static_cast<unsigned>(lhs) ^
                                   static_cast<unsigned>(rhs));
};

struct TxnTypeInfo
{
    FieldMask stock_amount;
    FieldMask stock_value;
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
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::DISABLED,               // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::ENABLED_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::DISABLED,               // stock_val
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
        FieldMask::ENABLED_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::DISABLED,               // stock_val
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
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::DISABLED,               // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
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
        FieldMask::ENABLED_CREDIT | FieldMask::INPUT_NEW_BALANCE,         // stock_amt
        FieldMask::DISABLED,               // stock_val
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
        FieldMask::ENABLED_DEBIT | FieldMask::INPUT_NEW_BALANCE,          // stock_amt
        FieldMask::DISABLED,               // stock_val
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

struct StockTransactionSplitInfo
{
    bool m_debit_side;
    std::string m_account_str;
    std::string m_memo_str;
    std::string m_action_str;
    std::string m_value_str;
    std::string m_units_str;
    bool m_units_in_red = false;
    Account* m_account = nullptr;
    gnc_numeric m_value_numeric = gnc_numeric_create (1, 0); // invalid gnc_numerics
    gnc_numeric m_units_numeric = gnc_numeric_create (1, 0);

    static const char* s_missing_str;

    StockTransactionSplitInfo () { DEBUG ("StockTransactionSplitInfo constructor\n"); };
    StockTransactionSplitInfo (Account *acct, gnc_numeric val)
        : m_account_str{xaccAccountGetName (acct)} , m_account{acct} , m_value_numeric{val}
    { DEBUG ("StockTransactionSplitInfo constructor\n"); }
    StockTransactionSplitInfo(gnc_numeric& debit, gnc_numeric& credit, StringVec& errors,
                              FieldMask splitfield, Account *acct, const char *memo,
                              gnc_numeric amount, const char* page, GNCPrintAmountInfo curr_pinfo);
    ~StockTransactionSplitInfo () { DEBUG ("StockTransactionSplitInfo destructor\n"); }
    void create_split(Transaction *trans, AccountVec &account_commits);
};

// Translators: (missing) denotes that the amount or account is
// not provided, or incorrect, in the Stock Transaction Assistant.
const char* StockTransactionSplitInfo::s_missing_str = N_("(missing)");

StockTransactionSplitInfo::StockTransactionSplitInfo (gnc_numeric& debit, gnc_numeric& credit,
                                                      StringVec& errors, FieldMask splitfield,
                                                      Account *acct, const char *memo,
                                                      gnc_numeric amount, const char* page,
                                                      GNCPrintAmountInfo curr_pinfo) :
    m_debit_side{splitfield & FieldMask::ENABLED_DEBIT},
    m_account_str{acct ? xaccAccountGetName (acct) : ""},
    m_memo_str{memo ? memo : ""},
    m_action_str{page ? page : ""},
    m_value_str{gnc_numeric_check(amount) ? "" : xaccPrintAmount (amount, curr_pinfo)},
    m_account{acct}

{
    auto add_error = [&errors](const char* format_str, const char* arg)
    {
        gchar *buf = g_strdup_printf (_(format_str),
                                      g_dpgettext2 (nullptr, "Stock Assistant: Page name", arg));
        errors.emplace_back (buf);
        g_free (buf);
    };

    DEBUG ("page=%s, amount=%s", page, gnc_num_dbg_to_string (amount));
    if (memo)
        m_memo_str = memo;
    m_debit_side = (splitfield & FieldMask::ENABLED_DEBIT);
    if (page)
        m_action_str = page;

    if (gnc_numeric_check (amount))
    {
        if (splitfield & FieldMask::ALLOW_ZERO)
            // m_value_numeric contains an invalid gnc_numeric
            m_value_str = "";
        else
        {
            add_error (N_("Amount for %s is missing."), page);
            m_value_str = _(s_missing_str);
        }
    }
    else
    {
        if (!(splitfield & FieldMask::ALLOW_NEGATIVE))
        {
            if ((splitfield & FieldMask::ALLOW_ZERO) && gnc_numeric_negative_p (amount))
                add_error (N_("Amount for %s must not be negative."), page);
            else if (!(splitfield & FieldMask::ALLOW_ZERO) && !gnc_numeric_positive_p (amount))
                add_error (N_("Amount for %s must be positive."), page);
        }
        if (gnc_numeric_negative_p (amount))
        {
            amount = gnc_numeric_neg (amount);
            m_debit_side = !m_debit_side;
        }
        if (m_debit_side)
            debit = gnc_numeric_add_fixed (debit, amount);
        else
            credit = gnc_numeric_add_fixed (credit, amount);
        m_units_numeric = m_debit_side ? amount : gnc_numeric_neg (amount);
        m_value_numeric = m_debit_side ? amount : gnc_numeric_neg (amount);
        m_value_str = xaccPrintAmount (amount, curr_pinfo);
    }

    if (acct)
    {
        m_account = acct;
        m_account_str = xaccAccountGetName (acct);
    }
    else if ((splitfield & FieldMask::ALLOW_ZERO) &&
             (gnc_numeric_check (amount) || gnc_numeric_zero_p (amount)))
        m_account_str = "";
    else
    {
        add_error (N_("Account for %s is missing."), page);
        m_account_str = _(s_missing_str);
    }
}

void
StockTransactionSplitInfo::create_split(Transaction *trans, AccountVec &account_commits) {
  g_return_if_fail(trans);
  if (!m_account || gnc_numeric_check(m_value_numeric) ||
      gnc_numeric_check(m_units_numeric))
    return;
  auto split = xaccMallocSplit(qof_instance_get_book(trans));
  xaccSplitSetParent(split, trans);
  xaccAccountBeginEdit(m_account);
  account_commits.emplace_back(m_account);
  xaccSplitSetAccount(split, m_account);
  xaccSplitSetMemo(split, m_memo_str.c_str());
  xaccSplitSetValue(split, m_value_numeric);
  xaccSplitSetAmount(split, m_units_numeric);
  DEBUG("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
        m_action_str.c_str(), m_account_str.c_str(),
        gnc_num_dbg_to_string(m_value_numeric),
        gnc_num_dbg_to_string(m_units_numeric),
        gnc_num_dbg_to_string(xaccSplitGetValue(split)),
        gnc_num_dbg_to_string(xaccSplitGetAmount(split)));
  gnc_set_num_action(nullptr, split, nullptr,
                     g_dpgettext2(nullptr, "Stock Assistant: Action field",
                                  m_action_str.c_str()));
}

using SplitInfoVec = std::vector<StockTransactionSplitInfo>;

struct StockAssistantModel
{
    Account   * acct;

    gnc_commodity * currency;
    GNCPrintAmountInfo curr_pinfo;
    GNCPrintAmountInfo price_pinfo;
    GNCPrintAmountInfo stock_pinfo;

    time64      transaction_date;
    std::optional<TxnTypeVec> txn_types;

    std::optional<TxnTypeInfo> txn_type;

    const gchar *transaction_description;
    gnc_numeric balance_at_date = gnc_numeric_create (1, 0);

    bool input_new_balance;
    bool stock_amount_enabled;
    gnc_numeric stock_amount = gnc_numeric_create (1, 0);

    bool stock_value_enabled;
    gnc_numeric stock_value = gnc_numeric_create (1, 0);
    const gchar* stock_memo = nullptr;

    bool cash_enabled;
    Account *cash_account = nullptr;
    const gchar* cash_memo = nullptr;
    gnc_numeric cash_value = gnc_numeric_create (1, 0);

    bool fees_enabled;
    bool fees_capitalize;
    Account *fees_account = nullptr;
    const gchar* fees_memo = nullptr;
    gnc_numeric fees_value = gnc_numeric_create (1, 0);

    bool dividend_enabled;
    Account *dividend_account = nullptr;
    const gchar* dividend_memo = nullptr;
    gnc_numeric dividend_value = gnc_numeric_create (1, 0);

    bool capgains_enabled;
    Account *capgains_account = nullptr;
    const gchar* capgains_memo = nullptr;
    gnc_numeric capgains_value = gnc_numeric_create (1, 0);

    StockAssistantModel (Account *account) :
     acct (account), currency (gnc_account_get_currency_or_parent (account)),
     curr_pinfo (gnc_commodity_print_info (this->currency, true)),
     price_pinfo (gnc_price_print_info (this->currency, true)),
     stock_pinfo (gnc_commodity_print_info (xaccAccountGetCommodity (account), true))
    {
        DEBUG ("StockAssistantModel constructor\n");
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
        return xaccPrintAmount (this->balance_at_date, this->stock_pinfo);
    };

    std::string get_new_amount_str ();
    std::tuple<bool, gnc_numeric, const char*> calculate_price ();
    std::tuple<bool, std::string, SplitInfoVec> generate_list_of_splits ();
    std::tuple<bool, Transaction*> create_transaction ();

private:
    std::optional<time64>     txn_types_date;
    bool ready_to_create = false;

    SplitInfoVec list_of_splits;

    void add_price (QofBook *book);
};

bool
StockAssistantModel::maybe_reset_txn_types ()
{
    auto new_bal = xaccAccountGetBalanceAsOfDate
        (this->acct, gnc_time64_get_day_end (this->transaction_date));
    if (this->txn_types_date && this->txn_types_date == this->transaction_date &&
        gnc_numeric_equal (this->balance_at_date, new_bal))
        return false;
    this->balance_at_date = new_bal;
    this->txn_types_date = this->transaction_date;
    this->txn_types = gnc_numeric_zero_p (this->balance_at_date) ? starting_types
        : gnc_numeric_positive_p (this->balance_at_date) ? long_types
        : short_types;
    return true;
};

bool
StockAssistantModel::set_txn_type (guint type_idx)
{
    if (!this->txn_types_date || this->txn_types_date != this->transaction_date)
    {
        PERR ("transaction_date has changed. rerun maybe_reset_txn_types!");
        return false;
    }
    try
    {
        this->txn_type = this->txn_types->at (type_idx);
    }
    catch (const std::out_of_range&)
    {
        PERR ("out of range type_idx=%d", type_idx);
        return false;
    }
    this->input_new_balance = this->txn_type->stock_amount & FieldMask::INPUT_NEW_BALANCE;
    this->stock_amount_enabled = this->txn_type->stock_amount != FieldMask::DISABLED;
    this->stock_value_enabled = this->txn_type->stock_value != FieldMask::DISABLED;
    this->fees_capitalize = this->txn_type->fees_value & FieldMask::CAPITALIZE_DEFAULT;
    this->fees_enabled = this->txn_type->fees_value != FieldMask::DISABLED;
    this->capgains_enabled = this->txn_type->capgains_value != FieldMask::DISABLED;
    this->dividend_enabled = this->txn_type->dividend_value != FieldMask::DISABLED;
    this->cash_enabled = this->txn_type->cash_value != FieldMask::DISABLED;
    return true;
};

std::string
StockAssistantModel::get_new_amount_str ()
{
    if (gnc_numeric_check (this->stock_amount))
        return "";

    if (this->input_new_balance)
    {
        auto ratio = gnc_numeric_div (this->stock_amount, this->balance_at_date,
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        if (gnc_numeric_check (ratio) || !gnc_numeric_positive_p (ratio))
            return "";

        std::ostringstream ret;
        ret << ratio.num << ':' << ratio.denom;
        return ret.str();
    }
    else
    {
        auto amount = (this->txn_type->stock_amount & FieldMask::ENABLED_CREDIT) ?
            gnc_numeric_neg (this->stock_amount) : this->stock_amount;
        amount = gnc_numeric_add_fixed (amount, this->balance_at_date);
        return xaccPrintAmount (amount, stock_pinfo);
    }
};

std::tuple<bool, gnc_numeric, const char*>
StockAssistantModel::calculate_price ()
{
    if (this->input_new_balance ||
        !this->stock_amount_enabled || gnc_numeric_check (this->stock_amount) ||
        !this->stock_value_enabled || gnc_numeric_check (this->stock_value) ||
        gnc_numeric_zero_p (this->stock_amount) ||
        gnc_numeric_zero_p (this->stock_value))
        return { false, gnc_numeric_create (1, 0), nullptr };

    auto price = gnc_numeric_div (this->stock_value, this->stock_amount,
                                  GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    return {true, price, xaccPrintAmount (price, this->price_pinfo)};
}

std::tuple<bool, std::string, SplitInfoVec>
StockAssistantModel::generate_list_of_splits ()
{
    if (!this->txn_types || !this->txn_type)
        return { false, "Error: txn_type not initialized", {} };

    this->list_of_splits.clear();

    gnc_numeric debit = gnc_numeric_zero ();
    gnc_numeric credit = gnc_numeric_zero ();
    StringVec errors, warnings, infos;
    StockTransactionSplitInfo line;
    bool negative_in_red = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_NEGATIVE_IN_RED);
    auto add_error_str = [&errors]
        (const char* str) { errors.emplace_back (_(str)); };

    // check the stock transaction date. If there are existing stock
    // transactions dated after the date specified, it is very likely
    // the later stock transactions will be invalidated. warn the user
    // to review them.
    auto last_split_node = g_list_last (xaccAccountGetSplitList (this->acct));
    if (last_split_node)
    {
        auto last_split = static_cast<const Split*> (last_split_node->data);
        auto last_split_date = xaccTransGetDate (xaccSplitGetParent (last_split));
        if (this->transaction_date <= last_split_date)
        {
            auto last_split_date_str = qof_print_date (last_split_date);
            auto new_date_str = qof_print_date (this->transaction_date);
            // Translators: the first %s is the new transaction date;
            // the second %s is the current stock account's latest
            // transaction date.
            auto warn_txt =  g_strdup_printf (_("You will enter a transaction \
with date %s which is earlier than the latest transaction in this account, \
dated %s. Doing so may affect the cost basis, and therefore capital gains, \
of transactions dated after the new entry. Please review all transactions \
to ensure proper recording."), new_date_str, last_split_date_str);
            warnings.push_back (warn_txt);
            g_free (warn_txt);
            g_free (new_date_str);
            g_free (last_split_date_str);
        }
    }

    if (!this->stock_value_enabled)
        line = StockTransactionSplitInfo (this->acct, gnc_numeric_zero());
    else
        line = StockTransactionSplitInfo(debit, credit, errors, this->txn_type->stock_value,
                                         this->acct, this->stock_memo, this->stock_value,
                                         NC_ ("Stock Assistant: Page name", "stock value"),
                                         this->curr_pinfo);


    if (!this->stock_amount_enabled)
        line.m_units_numeric = gnc_numeric_zero();
    else if (gnc_numeric_check (this->stock_amount))
    {
        line.m_units_str = _("(missing)");
        line.m_units_numeric = gnc_numeric_zero();
        add_error_str (N_("Amount for stock units is missing"));
    }
    else if (this->input_new_balance)
    {
        auto stock_amount = this->stock_amount;
        auto credit_side = (this->txn_type->stock_amount & FieldMask::ENABLED_CREDIT);
        auto delta = gnc_numeric_sub_fixed (stock_amount, this->balance_at_date);
        auto ratio = gnc_numeric_div (stock_amount, this->balance_at_date,
                                      GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        stock_amount = gnc_numeric_sub_fixed (stock_amount, this->balance_at_date);
        line.m_units_numeric = stock_amount;
        line.m_units_str = xaccPrintAmount (stock_amount, this->stock_pinfo);
        line.m_units_in_red = negative_in_red && gnc_numeric_negative_p (stock_amount);
        if (gnc_numeric_check (ratio) || !gnc_numeric_positive_p (ratio))
            add_error_str (N_("Invalid stock new balance."));
        else if (gnc_numeric_negative_p (delta) && !credit_side)
            add_error_str (N_("New balance must be higher than old balance."));
        else if (gnc_numeric_positive_p (delta) && credit_side)
            add_error_str (N_("New balance must be lower than old balance."));
    }
    else
    {
        auto stock_amount = this->stock_amount;
        if (!gnc_numeric_positive_p (stock_amount))
            add_error_str (N_("Stock amount must be positive."));
        if (this->txn_type->stock_amount & FieldMask::ENABLED_CREDIT)
            stock_amount = gnc_numeric_neg (stock_amount);
        line.m_units_numeric = stock_amount;
        line.m_units_str = xaccPrintAmount (stock_amount, this->stock_pinfo);
        line.m_units_in_red = negative_in_red && gnc_numeric_negative_p (stock_amount);
        auto new_bal = gnc_numeric_add_fixed (this->balance_at_date, stock_amount);
        if (gnc_numeric_positive_p (this->balance_at_date) &&
            gnc_numeric_negative_p (new_bal))
            add_error_str (N_("Cannot sell more units than owned."));
        else if (gnc_numeric_negative_p (this->balance_at_date) &&
                 gnc_numeric_positive_p (new_bal))
            add_error_str (N_("Cannot cover buy more units than owed."));
    }

    this->list_of_splits.push_back (std::move (line));

    auto [has_price, price, price_str] = this->calculate_price ();
    if (has_price)
    {
        // Translators: %s refer to: stock mnemonic, broker currency,
        // date of transaction.
        auto tmpl = N_("A price of 1 %s = %s on %s will be recorded.");
        auto date_str = qof_print_date (this->transaction_date);
        auto price_msg = g_strdup_printf
            (_(tmpl),
             gnc_commodity_get_mnemonic (xaccAccountGetCommodity (this->acct)),
             price_str, date_str);
        infos.emplace_back (price_msg);
        g_free (date_str);
    }

    if (this->cash_enabled)
    {
        line = StockTransactionSplitInfo (debit, credit, errors, this->txn_type->cash_value,
                                          this->cash_account, this->cash_memo, this->cash_value,
                                          NC_ ("Stock Assistant: Page name", "cash"),
                                          this->curr_pinfo);
        this->list_of_splits.push_back (std::move (line));
    }

    if (this->fees_enabled)
    {
        line = StockTransactionSplitInfo (debit, credit, errors, this->txn_type->fees_value,
                                          this->fees_capitalize ? this->acct : this->fees_account,
                                          this->fees_memo, this->fees_value,
                                          NC_ ("Stock Assistant: Page name", "fees"),
                                          this->curr_pinfo);
        if (this->fees_capitalize)
            line.m_units_numeric = gnc_numeric_zero();
        this->list_of_splits.push_back (std::move (line));
    }

    if (this->dividend_enabled)
    {
        line = StockTransactionSplitInfo (debit, credit, errors, this->txn_type->dividend_value,
                                          this->dividend_account, this->dividend_memo,
                                          this->dividend_value,
                                          NC_ ("Stock Assistant: Page name", "dividend"),
                                          this->curr_pinfo);
        this->list_of_splits.push_back (std::move (line));
    }

    // the next two checks will involve the two capgains splits:
    // income side and stock side. The capgains_value ^
    // (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT) will
    // swap the debit/credit flags.
    if (this->capgains_enabled)
    {
        if (this->txn_type->capgains_value & FieldMask::CAPGAINS_IN_STOCK)
        {
            line = StockTransactionSplitInfo (debit, credit, errors, this->txn_type->capgains_value ^
                                              (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT),
                                              this->acct, this->capgains_memo, this->capgains_value,
                                              NC_ ("Stock Assistant: Page name", "capital gains"),
                                              this->curr_pinfo);
            line.m_units_numeric = gnc_numeric_zero();
            this->list_of_splits.push_back (std::move (line));
        }

        line = StockTransactionSplitInfo(debit, credit, errors, this->txn_type->capgains_value,
                                         this->capgains_account, this->capgains_memo,
                                         this->capgains_value,
                                         NC_ ("Stock Assistant: Page name", "capital gains"),
                                         this->curr_pinfo);
        this->list_of_splits.push_back (std::move (line));
    }

    if (!gnc_numeric_equal (debit, credit))
    {
        auto imbalance_str = N_("Total Debits of %s does not balance with total Credits of %s.");
        auto debit_str = g_strdup (xaccPrintAmount (debit, this->curr_pinfo));
        auto credit_str = g_strdup (xaccPrintAmount (credit, this->curr_pinfo));
        auto error_str = g_strdup_printf (_(imbalance_str), debit_str, credit_str);
        errors.emplace_back (error_str);
        g_free (error_str);
        g_free (credit_str);
        g_free (debit_str);
    }

    // generate final summary message. Collates a header, the errors
    // and warnings. Then allow completion if errors is empty.
    std::ostringstream summary;
    auto summary_add = [&summary](auto a) { summary << "\n• " << a; };
    if (errors.empty())
    {
        summary << _("No errors found. Click Apply to create transaction.");
        std::for_each (infos.begin(), infos.end(), summary_add);
    }
    else
    {
        summary << _("The following errors must be fixed:");
        std::for_each (errors.begin(), errors.end(), summary_add);
    }
    if (!warnings.empty())
    {
        summary << "\n\n" << _("The following warnings exist:");
        std::for_each (warnings.begin(), warnings.end(), summary_add);
    }
    this->ready_to_create = errors.empty();
    return { this->ready_to_create, summary.str(), this->list_of_splits };
}

std::tuple<bool, Transaction*>
StockAssistantModel::create_transaction ()
{
    if (!this->ready_to_create)
    {
        PERR ("errors exist. cannot create transaction.");
        return { false, nullptr };
    }
    auto book = qof_instance_get_book (acct);
    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, this->currency);
    xaccTransSetDescription (trans, this->transaction_description);
    xaccTransSetDatePostedSecsNormalized (trans, this->transaction_date);
    AccountVec accounts;
    std::for_each (this->list_of_splits.begin(), this->list_of_splits.end(),
                   [&](auto& line){ line.create_split (trans, accounts); });
    this->add_price (book);
    xaccTransCommitEdit (trans);
    std::for_each (accounts.begin(), accounts.end(), xaccAccountCommitEdit);
    this->ready_to_create = false;
    return { true, trans };
}

void
StockAssistantModel::add_price (QofBook *book)
{
    auto [has_price, p, price_str] = this->calculate_price ();
    if (!has_price)
        return;

    auto price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, xaccAccountGetCommodity (this->acct));
    gnc_price_set_currency (price, this->currency);
    gnc_price_set_time64 (price, this->transaction_date);
    gnc_price_set_source (price, PRICE_SOURCE_STOCK_TRANSACTION);
    gnc_price_set_typestr (price, PRICE_TYPE_UNK);
    gnc_price_set_value (price, p);
    gnc_price_commit_edit (price);

    auto pdb = gnc_pricedb_get_db (book);
    if (!gnc_pricedb_add_price (pdb, price))
        PWARN ("error adding price");

    gnc_price_unref (price);
}


struct StockAssistantView
{
    GtkWidget * window;

    // transaction type page
    GtkWidget * transaction_type_page;
    GtkWidget * transaction_type_combo;
    GtkWidget * transaction_type_explanation;

    // transaction details page
    GtkWidget * transaction_details_page;
    GtkWidget * transaction_date;
    GtkWidget * transaction_description;

    // stock amount page
    GtkWidget * stock_amount_page;
    GtkWidget * stock_amount_title;
    GtkWidget * prev_amount;
    GtkWidget * next_amount;
    GtkWidget * next_amount_label;
    GtkWidget * stock_amount_edit;
    GtkWidget * stock_amount_label;

    // stock value page
    GtkWidget * stock_value_page;
    GtkWidget * stock_value_edit;
    GtkWidget * price_value;
    GtkWidget * stock_memo_edit;

    // cash page
    GtkWidget * cash_page;
    GtkWidget * cash_account;
    GtkWidget * cash_memo_edit;
    GtkWidget * cash_value;

    // fees page
    GtkWidget * fees_page;
    GtkWidget * capitalize_fees_checkbox;
    GtkWidget * fees_account;
    GtkWidget * fees_memo_edit;
    GtkWidget * fees_value;

    // dividend page
    GtkWidget * dividend_page;
    GtkWidget * dividend_account;
    GtkWidget * dividend_memo_edit;
    GtkWidget * dividend_value;

    // capgains page
    GtkWidget * capgains_page;
    GtkWidget * capgains_account;
    GtkWidget * capgains_memo_edit;
    GtkWidget * capgains_value;

    // finish page
    GtkWidget * finish_page;
    GtkWidget * finish_split_view;
    GtkWidget * finish_summary;

    void set_focus (GtkWidget *widget) { gtk_widget_grab_focus (widget); }
    void set_focus_gae (GtkWidget *gae) { set_focus (GTK_WIDGET (gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (gae)))); }

    int get_transaction_type_index ()
    { return gtk_combo_box_get_active (GTK_COMBO_BOX (transaction_type_combo)); }

    void set_transaction_types (const TxnTypeVec& txn_types)
    {
        auto combo = GTK_COMBO_BOX_TEXT (this->transaction_type_combo);
        gtk_combo_box_text_remove_all (combo);
        std::for_each (txn_types.begin(), txn_types.end(),
                       [&combo](const auto& it)
                       { gtk_combo_box_text_append_text (combo, _(it.friendly_name)); });
        gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 0);
    };

    void set_txn_type_explanation (const gchar *txt)
    { gtk_label_set_text (GTK_LABEL (this->transaction_type_explanation), txt); };

    void
    prepare_stock_amount_page (bool input_new_balance, const std::string prev_balance)
    {
        gtk_label_set_text_with_mnemonic
            (GTK_LABEL (this->stock_amount_label),
             input_new_balance ? _("Ne_w Balance") : _("_Shares"));
        gtk_label_set_text
            (GTK_LABEL (this->next_amount_label),
             input_new_balance ? _("Ratio") : _("Next Balance"));
        gtk_label_set_text
            (GTK_LABEL (this->stock_amount_title),
             input_new_balance ?
             _("Enter the new balance of shares after the stock split.") :
             _("Enter the number of shares you gained or lost in the transaction."));
        gtk_label_set_text (GTK_LABEL (this->prev_amount), prev_balance.c_str());
    };

    void set_stock_amount (std::string new_amount_str)
    {
        gtk_label_set_text (GTK_LABEL(this->next_amount), new_amount_str.c_str());
    };

    void set_price_value (const gchar *val)
    { gtk_label_set_text (GTK_LABEL (this->price_value), val); };

    bool get_capitalize_fees ()
    { return gtk_toggle_button_get_active
            (GTK_TOGGLE_BUTTON (this->capitalize_fees_checkbox)); }

    void set_capitalize_fees (bool state)
    {
        gtk_toggle_button_set_active
            (GTK_TOGGLE_BUTTON (this->capitalize_fees_checkbox), state);
    }

    void update_fees_acct_sensitive (bool sensitive)
    { gtk_widget_set_sensitive (this->fees_account, sensitive); }

    void prepare_finish_page (bool success, const std::string& summary,
                              const SplitInfoVec& list_of_splits)
    {
        auto gtv = GTK_TREE_VIEW (this->finish_split_view);
        auto list = GTK_LIST_STORE (gtk_tree_view_get_model (gtv));
        gtk_list_store_clear (list);
        for (const auto& line : list_of_splits)
        {
            GtkTreeIter iter;
            auto tooltip = g_markup_escape_text (line.m_memo_str.c_str(), -1);
            gtk_list_store_append (list, &iter);
            gtk_list_store_set (list, &iter,
                                SPLIT_COL_ACCOUNT, line.m_account_str.c_str(),
                                SPLIT_COL_MEMO, line.m_memo_str.c_str(),
                                SPLIT_COL_TOOLTIP, tooltip,
                                SPLIT_COL_DEBIT, line.m_debit_side ? line.m_value_str.c_str() : nullptr,
                                SPLIT_COL_CREDIT, line.m_debit_side ? nullptr : line.m_value_str.c_str(),
                                SPLIT_COL_UNITS, line.m_units_str.c_str(),
                                SPLIT_COL_UNITS_COLOR, line.m_units_in_red ? "red" : nullptr,
                                -1);
            g_free (tooltip);
        }
        gtk_assistant_set_page_complete (GTK_ASSISTANT (this->window),
                                         this->finish_page, success);
        gtk_label_set_text (GTK_LABEL (this->finish_summary), summary.c_str());
    }

    StockAssistantView (GtkBuilder *builder, gnc_commodity *stock_commodity,
                        gnc_commodity *currency, GtkWidget *parent)
        : window (get_widget (builder, "stock_transaction_assistant"))
        , transaction_type_page (get_widget (builder, "transaction_type_page"))
        , transaction_type_combo (get_widget (builder, "transaction_type_page_combobox"))
        , transaction_type_explanation (get_widget (builder, "transaction_type_page_explanation"))
        , transaction_details_page (get_widget (builder, "transaction_details_page"))
        , transaction_date (create_date (builder, 0, "transaction_date_label", "transaction_details_table"))
        , transaction_description (get_widget (builder, "transaction_description_entry"))
        , stock_amount_page (get_widget (builder, "stock_amount_page"))
        , stock_amount_title (get_widget (builder, "stock_amount_title"))
        , prev_amount (get_widget (builder, "prev_balance_amount"))
        , next_amount (get_widget (builder, "next_balance_amount"))
        , next_amount_label (get_widget (builder, "next_balance_label"))
        , stock_amount_edit (create_gae (builder, 1, stock_commodity, "stock_amount_table", "stock_amount_label"))
        , stock_amount_label (get_widget (builder, "stock_amount_label"))
        , stock_value_page (get_widget (builder, "stock_value_page"))
        , stock_value_edit (create_gae (builder, 0, currency, "stock_value_table", "stock_value_label"))
        , price_value (get_widget (builder, "stock_price_amount"))
        , stock_memo_edit (get_widget (builder, "stock_memo_entry"))
        , cash_page (get_widget (builder, "cash_details_page"))
        , cash_account (create_gas (builder, 0, { ACCT_TYPE_ASSET, ACCT_TYPE_BANK }, currency,  "cash_table", "cash_account_label"))
        , cash_memo_edit (get_widget (builder, "cash_memo_entry"))
        , cash_value (create_gae (builder, 1, currency, "cash_table", "cash_label"))
        , fees_page (get_widget (builder, "fees_details_page"))
        , capitalize_fees_checkbox (get_widget (builder, "capitalize_fees_checkbutton"))
        , fees_account (create_gas (builder, 1, { ACCT_TYPE_EXPENSE }, currency, "fees_table", "fees_account_label"))
        , fees_memo_edit (get_widget (builder, "fees_memo_entry"))
        , fees_value (create_gae (builder, 2, currency, "fees_table", "fees_label"))
        , dividend_page (get_widget (builder, "dividend_details_page"))
        , dividend_account (create_gas (builder, 0, { ACCT_TYPE_INCOME }, currency, "dividend_table", "dividend_account_label"))
        , dividend_memo_edit (get_widget (builder, "dividend_memo_entry"))
        , dividend_value (create_gae (builder, 1, currency, "dividend_table", "dividend_label"))
        , capgains_page (get_widget (builder, "capgains_details_page"))
        , capgains_account (create_gas (builder, 0, { ACCT_TYPE_INCOME }, currency, "capgains_table", "capgains_account_label"))
        , capgains_memo_edit (get_widget (builder, "capgains_memo_entry"))
        , capgains_value (create_gae (builder, 1, currency, "capgains_table", "capgains_label"))
        , finish_page (get_widget (builder, "finish_page"))
        , finish_split_view (get_treeview (builder, "transaction_view"))
        , finish_summary (get_widget (builder, "finish_summary"))
    {
        // Set the name for this assistant so it can be easily manipulated with css
        gtk_widget_set_name (GTK_WIDGET(this->window), "gnc-id-assistant-stock-transaction");
        gtk_tree_view_set_tooltip_column (GTK_TREE_VIEW (this->finish_split_view),
                                          SPLIT_COL_TOOLTIP);
        gtk_window_set_transient_for (GTK_WINDOW (this->window), GTK_WINDOW(parent));
        gnc_window_adjust_for_screen (GTK_WINDOW(this->window));
        gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(this->window),
                                 GTK_WINDOW(parent));
        gtk_widget_show_all (this->window);
        DEBUG ("StockAssistantView constructor\n");
    };
    ~StockAssistantView(){
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(this->window));
        DEBUG ("StockAssistantView destructor\n");
    };

private:
    GtkWidget* get_widget (GtkBuilder *builder, const gchar * ID)
    {
        g_return_val_if_fail (builder && ID, nullptr);
        auto obj = gtk_builder_get_object (builder, ID);
        if (!obj)
            PWARN ("get_widget ID '%s' not found. it may be a typo?", ID);
        return GTK_WIDGET (obj);
    }

    GtkWidget* create_gas (GtkBuilder *builder, gint row,
                           std::vector<GNCAccountType> type, gnc_commodity *currency,
                           const gchar *table_ID, const gchar *label_ID)
    {
        auto table = get_widget (builder, table_ID);
        auto label = get_widget (builder, label_ID);
        auto gas = gnc_account_sel_new ();
        auto accum = [](auto a, auto b){ return g_list_prepend (a, (gpointer)b); };
        auto null_glist = static_cast<GList*>(nullptr);
        auto acct_list = std::accumulate (type.begin(), type.end(), null_glist, accum);
        auto curr_list = accum (null_glist, currency);
        gnc_account_sel_set_new_account_ability (GNC_ACCOUNT_SEL (gas), true);
        gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL (gas), acct_list, curr_list);
        gtk_widget_show (gas);
        gtk_grid_attach (GTK_GRID(table), gas, 1, row, 1, 1);
        gtk_label_set_mnemonic_widget (GTK_LABEL(label), gas);
        g_list_free (acct_list);
        g_list_free (curr_list);
        return gas;
    }

    GtkWidget* create_gae (GtkBuilder *builder, gint row, gnc_commodity *comm,
                           const gchar *table_ID, const gchar *label_ID)
    {
        // shares amount
        auto table = get_widget (builder, table_ID);
        auto label = get_widget (builder, label_ID);
        auto info = gnc_commodity_print_info (comm, true);
        auto gae = gnc_amount_edit_new ();
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (gae), TRUE);
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (gae), info);
        gtk_grid_attach (GTK_GRID(table), gae, 1, row, 1, 1);
        gtk_widget_show (gae);
        gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT (gae), label);
        return gae;
    }

    GtkWidget* create_date (GtkBuilder *builder, guint row,
                            const gchar *date_label, const gchar *table_label)
    {
        auto date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
        auto label = get_widget (builder, date_label);
        gtk_grid_attach (GTK_GRID(get_widget (builder, table_label)), date, 1, row, 1, 1);
        gtk_widget_show (date);
        gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);
        return date;
    }

    GtkWidget* get_treeview (GtkBuilder *builder, const gchar *treeview_label)
    {
        auto view = GTK_TREE_VIEW (get_widget (builder, "transaction_view"));
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

        return GTK_WIDGET (view);
    }
};

static void connect_signals (gpointer, GtkBuilder*);

struct StockAssistantController
{
    std::unique_ptr<StockAssistantModel> model;
    std::unique_ptr<StockAssistantView> view;
    StockAssistantController (GtkWidget *parent, Account* acct)
        : model (std::make_unique<StockAssistantModel>(acct))
    {
        auto builder = gtk_builder_new();
        gnc_builder_add_from_file (builder, "assistant-stock-transaction.glade",
                                   "stock_transaction_assistant");
        this->view = std::make_unique<StockAssistantView>
            (builder, xaccAccountGetCommodity (acct), this->model->currency, parent);
        connect_signals (this, builder);
        g_object_unref (builder);
        DEBUG ("StockAssistantController constructor\n");
    };
    ~StockAssistantController (){ DEBUG ("StockAssistantController destructor\n"); };
};

/******* implementations ***********************************************/
static void
stock_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    delete info;
}

static void
controller_transaction_type (GtkWidget *widget, StockAssistantController* info)
{
    if (!info->model->txn_types)
        return;

    auto type_idx = info->view->get_transaction_type_index();
    if (type_idx < 0)           // combo isn't initialized yet.
        return;

    if (!info->model->set_txn_type (type_idx))
        return;

    info->view->set_txn_type_explanation (info->model->txn_type->explanation);
    info->view->set_capitalize_fees (info->model->fees_capitalize);
}

static void controller_gde (GtkWidget *widget, time64* date)
{
    *date = gnc_date_edit_get_date_end (GNC_DATE_EDIT (widget));
}

static void controller_gtk_entry (GtkWidget *widget, const gchar **model_text)
{
    *model_text = gtk_entry_get_text (GTK_ENTRY (widget));
}

static void controller_gae (GtkWidget *widget, gnc_numeric *num)
{
    gnc_numeric amt;
    if (!gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT(widget), &amt, true, nullptr))
        *num = amt;
    else
        num->denom = 0;
}

static void controller_gas (GtkWidget *widget, Account **acct)
{
    *acct = gnc_account_sel_get_account (GNC_ACCOUNT_SEL (widget));
}

static void
controller_stock_amount (GtkWidget *widget, StockAssistantController* info)
{
    g_return_if_fail (info && info->model->txn_type);

    controller_gae (widget, &info->model->stock_amount);
    info->view->set_stock_amount (info->model->get_new_amount_str());
}

static void
controller_stock_value (GtkWidget *widget, StockAssistantController* info)
{
    g_return_if_fail (info && info->model->txn_type);

    controller_gae (widget, &info->model->stock_value);
    auto [has_price, price, price_str] = info->model->calculate_price ();
    // Translators: StockAssistant: N/A denotes stock price is not computable
    info->view->set_price_value (has_price ? price_str : _("N/A"));
}

static void
controller_capitalize_fees (GtkWidget *widget, StockAssistantController* info)
{
    g_return_if_fail (info && info->model->txn_type);
    info->model->fees_capitalize = info->view->get_capitalize_fees ();
    info->view->update_fees_acct_sensitive (!info->model->fees_capitalize);
}

void
stock_assistant_prepare_cb (GtkAssistant  *assistant, GtkWidget *page,
                         gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    g_return_if_fail (info && info->model);
    auto model = info->model.get();
    auto view = info->view.get();

    auto currentpage = gtk_assistant_get_current_page(assistant);

    switch (currentpage)
    {
    case PAGE_TRANSACTION_TYPE:
        if (!model->maybe_reset_txn_types())
            break;
        view->set_transaction_types (model->txn_types.value());
        controller_transaction_type (view->transaction_type_combo, info);
        view->set_focus (view->transaction_type_combo);
        break;
    case PAGE_TRANSACTION_DETAILS:
        controller_gde (view->transaction_date, &model->transaction_date);
        controller_gtk_entry (view->transaction_description, &model->transaction_description);
        view->set_focus (view->transaction_description);
        break;
    case PAGE_STOCK_AMOUNT:
        view->prepare_stock_amount_page (model->input_new_balance,
                                         model->get_stock_balance_str());
        controller_stock_amount (view->stock_amount_edit, info);
        view->set_focus_gae (view->stock_amount_edit);
        break;
    case PAGE_STOCK_VALUE:
        controller_gtk_entry (view->stock_memo_edit, &model->stock_memo);
        controller_stock_value (view->stock_value_edit, info);
        view->set_focus_gae (view->stock_value_edit);
        break;
    case PAGE_CASH:
        controller_gtk_entry (view->cash_memo_edit, &model->cash_memo);
        controller_gae (view->cash_value, &model->cash_value);
        controller_gas (view->cash_account, &model->cash_account);
        view->set_focus_gae (view->cash_value);
        break;
    case PAGE_FEES:
        controller_capitalize_fees (view->capitalize_fees_checkbox, info);
        controller_gtk_entry (view->fees_memo_edit, &model->fees_memo);
        controller_gae (view->fees_value, &model->fees_value);
        controller_gas (view->fees_account, &model->fees_account);
        view->set_focus_gae (view->fees_value);
        break;
    case PAGE_DIVIDEND:
        controller_gtk_entry (view->dividend_memo_edit, &model->dividend_memo);
        controller_gae (view->dividend_value, &model->dividend_value);
        controller_gas (view->dividend_account, &model->dividend_account);
        view->set_focus_gae (view->dividend_value);
        break;
    case PAGE_CAPGAINS:
        controller_gtk_entry (view->capgains_memo_edit, &model->capgains_memo);
        controller_gae (view->capgains_value, &model->capgains_value);
        controller_gas (view->capgains_account, &model->capgains_account);
        view->set_focus_gae (view->capgains_value);
        break;
    case PAGE_FINISH:
    {
        auto [success, summary, list_of_splits] = model->generate_list_of_splits ();
        view->prepare_finish_page (success, summary, list_of_splits);
        break;
    }
    default:
        break;
    }
}

static gint
forward_page_func (gint current_page, StockAssistantController* info)
{
    auto model = info->model.get();

    current_page++;
    if (!model->txn_type)
        return current_page;

    if (!model->stock_amount_enabled && current_page == PAGE_STOCK_AMOUNT)
        current_page++;
    if (!model->stock_value_enabled && current_page == PAGE_STOCK_VALUE)
        current_page++;
    if (!model->cash_enabled && current_page == PAGE_CASH)
        current_page++;
    if (!model->fees_enabled && current_page == PAGE_FEES)
        current_page++;
    if (!model->dividend_enabled && current_page == PAGE_DIVIDEND)
        current_page++;
    if (!model->capgains_enabled && current_page == PAGE_CAPGAINS)
        current_page++;

    return current_page;
}

void
stock_assistant_finish_cb (GtkAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    g_return_if_fail (info->model->txn_type);

    gnc_suspend_gui_refresh ();
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

    if (!GNC_IS_ACCOUNT (info->model->acct))
    {
        PWARN ("account %p does not exist anymore. abort", info->model->acct);
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    }
}

static void
close_handler (gpointer user_data)
{
    auto info = static_cast<StockAssistantController*>(user_data);
    gtk_widget_destroy (info->view->window);
}

static void connect_signals (gpointer data, GtkBuilder *builder)
{
    auto info = static_cast<StockAssistantController*>(data);
    auto model = info->model.get();
    auto view = info->view.get();

    struct SignalData { GtkWidget* widget; const char* signal; GCallback callback; gpointer data; };
    std::vector<SignalData> signals =
    {
        { view->transaction_type_combo  , "changed"            , G_CALLBACK (controller_transaction_type)      , info },
        { view->transaction_date        , "date_changed"       , G_CALLBACK (controller_gde)                   , &model->transaction_date },
        { view->transaction_description , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->transaction_description },
        { view->stock_amount_edit       , "changed"            , G_CALLBACK (controller_stock_amount)          , info },
        { view->stock_value_edit        , "changed"            , G_CALLBACK (controller_stock_value)           , info },
        { view->stock_memo_edit         , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->stock_memo },
        { view->cash_account            , "account_sel_changed", G_CALLBACK (controller_gas)                   , &model->cash_account },
        { view->cash_memo_edit          , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->cash_memo },
        { view->cash_value              , "changed"            , G_CALLBACK (controller_gae)                   , &model->cash_value },
        { view->capitalize_fees_checkbox, "toggled"            , G_CALLBACK (controller_capitalize_fees)       , info },
        { view->fees_account            , "account_sel_changed", G_CALLBACK (controller_gas)                   , &model->fees_account },
        { view->fees_memo_edit          , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->fees_memo },
        { view->fees_value              , "changed"            , G_CALLBACK (controller_gae)                   , &model->fees_value },
        { view->dividend_account        , "account_sel_changed", G_CALLBACK (controller_gas)                   , &model->dividend_account },
        { view->dividend_memo_edit      , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->dividend_memo },
        { view->dividend_value          , "changed"            , G_CALLBACK (controller_gae)                   , &model->dividend_value },
        { view->capgains_account        , "account_sel_changed", G_CALLBACK (controller_gas)                   , &model->capgains_account },
        { view->capgains_memo_edit      , "changed"            , G_CALLBACK (controller_gtk_entry)             , &model->capgains_memo },
        { view->capgains_value          , "changed"            , G_CALLBACK (controller_gae)                   , &model->capgains_value },
        { view->window                  , "destroy"            , G_CALLBACK (stock_assistant_window_destroy_cb), info }
    };
    for (const auto& [widget, signal, callback, data] : signals)
        g_signal_connect (widget, signal, callback, data);
    gtk_assistant_set_forward_page_func (GTK_ASSISTANT(view->window),
                                         (GtkAssistantPageFunc)forward_page_func,
                                         info, nullptr);
    gtk_builder_connect_signals (builder, info);

    auto component_id = gnc_register_gui_component
        (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, refresh_handler, close_handler, info);
    gnc_gui_component_watch_entity_type (component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
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
    [[maybe_unused]]auto info = new StockAssistantController (parent, account);
}

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
#include <vector>
#include <string>
#include <numeric>
#include <algorithm>
#include <optional>
#include <stdexcept>

extern "C" {
#include "Transaction.h"
#include "engine-helpers.h"
#include "dialog-utils.h"
#include "assistant-stock-transaction.h"
#include "gnc-account-sel.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-tree-view-account.h"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

void stock_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page,
                              gpointer user_data);
void stock_assistant_finish  (GtkAssistant *assistant, gpointer user_data);
void stock_assistant_cancel  (GtkAssistant *gtkassistant, gpointer user_data);
}

enum class FieldMask : unsigned;
bool operator &(FieldMask lhs, FieldMask rhs);
FieldMask operator |(FieldMask lhs, FieldMask rhs);
FieldMask operator ^(FieldMask lhs, FieldMask rhs);

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
    SPLIT_COL_MEMO_ESCAPED,
    SPLIT_COL_DEBIT,
    SPLIT_COL_CREDIT,
    NUM_SPLIT_COLS
};

/** structures *********************************************************/

enum class FieldMask : unsigned
{
    DISABLED = 0,
    ENABLED_DEBIT,
    ENABLED_CREDIT,
    ALLOW_ZERO = 4,
    ALLOW_NEGATIVE = 8
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
    bool fees_capitalize;
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
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing an
        // Initial stock purchase
        N_("Open buy"),
        N_("Initial stock purchase")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing an
        // initial stock short-sale
        N_("Open short"),
        N_("Initial stock short-sale")
    }
};

static const TxnTypeVec long_types
{
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // purchase of stock.
        N_("Buy"),
        N_("Buying stock.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE, // capgains_amt
        // Translators: this is a stock transaction describing sale of
        // stock, and recording capital gains/loss
        N_("Sell"),
        N_("Selling stock, and record capital gains/loss")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends issued to holder
        N_("Dividend"),
        N_("Company issues dividends to holder")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividend issued to holder, and is reinvested. Some
        // dividends are distributed as cash.
        N_("Dividend reinvestment (with remainder)"),
        N_("Company issues dividend which is reinvested. Some dividends are paid to holder")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividend which is wholly reinvested.
        N_("Dividend reinvestment (without remainder)"),
        N_("Company issues dividend which is wholly reinvested.")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing return
        // of capital
        N_("Return of Capital"),
        N_("Stock returns capital to holder")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::ENABLED_CREDIT,         // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution
        N_("Notional distribution"),
        N_("Stock returns a notional distribution")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split
        N_("Stock split"),
        N_("Stock price is fractionally reduced")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a reverse split
        N_("Reverse split"),
        N_("Stock price is fractionally increased.")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_CREDIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE, // capgains_amt
        // Translators: this is a stock transaction describing a
        // reverse split. Some fractional stock is returned as cash.
        N_("Reverse split with cash in lieu for fractionals"),
        N_("Stock price is fractionally increased. Fractional remaining stock is returned as cash.")
    },
};

static const TxnTypeVec short_types
{
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // shorting of stock.
        N_("Short sell"),
        N_("Shorting stock.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE,          // capg_amt
        // Translators: this is a stock transaction describing cover
        // buying stock, and recording capital gains/loss
        N_("Cover buy"),
        N_("Cover buying stock, and record capital gains/loss")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends retrieved from holder when shorting stock
        N_("Compensatory Dividend"),
        N_("Company issues dividends to holder when shorting stock")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::ENABLED_DEBIT,          // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends retrieved from holder when shorting stock. Some
        // dividends are recovered from holder
        N_("Dividend reinvestment (with remainder)"),
        N_("Company issues dividends to holder when shorting stock. Some dividends are recovered from holder")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing
        // dividends retrieved from holder when shorting stock,
        N_("Dividend reinvestment (without remainder)"),
        N_("Company issues dividend when shorting stock, which are wholly recovered from holder.")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing return
        // of capital retrieved from holder when shorting stock
        N_("Compensatory Return of Capital"),
        N_("Return retrieves capital from holder when shorting stock")
    },
    {
        FieldMask::DISABLED,               // stock_amt
        FieldMask::ENABLED_CREDIT,         // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::ENABLED_DEBIT,          // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // notional distribution when shorting stock
        N_("Compensatory Notional distribution"),
        N_("Stock retrieves a notional distribution")
    },
    {
        FieldMask::ENABLED_CREDIT,         // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a stock
        // split when shorting stock
        N_("Stock split"),
        N_("Stock price is fractionally reduced when shorting stock")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::DISABLED,               // stock_val
        FieldMask::DISABLED,               // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        true,                   // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::DISABLED,               // capg_amt
        // Translators: this is a stock transaction describing a
        // reverse split when shorting stock.
        N_("Reverse split"),
        N_("Stock price is fractionally increased when shorting stock.")
    },
    {
        FieldMask::ENABLED_DEBIT,          // stock_amt
        FieldMask::ENABLED_DEBIT,          // stock_val
        FieldMask::ENABLED_CREDIT,         // cash_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO,          // fees_amt
        false,                  // fees_capitalize
        FieldMask::DISABLED,               // dividend_amt
        FieldMask::ENABLED_DEBIT | FieldMask::ALLOW_ZERO | FieldMask::ALLOW_NEGATIVE,          // capg_amt
        // Translators: this is a stock transaction describing a
        // reverse split when shorting stock. Fractional remaining
        // stock is retrieved as cash.
        N_("Reverse split with cash in lieu for fractionals"),
        N_("Stock price is fractionally increased when shorting stock. Fractional remaining stock is retrieved as cash.")
    },
};

typedef struct
{
    GtkWidget * window;
    GtkWidget * assistant;

    std::optional<TxnTypeVec> txn_types;
    // the following stores date at which the txn_types were set. If
    // the GNCDateEdit date is modified, it will trigger recreation of
    // the txn_types above.
    std::optional<time64>     txn_types_date;
    Account   * acct;
    gnc_commodity * currency;

    // transaction type page
    GtkWidget * transaction_type_page;
    GtkWidget * transaction_type_combo;
    GtkWidget * transaction_type_explanation;
    std::optional<TxnTypeInfo> txn_type;

    // transaction details page
    GtkWidget * transaction_details_page;
    GtkWidget * date_edit;
    GtkWidget * transaction_description_entry;

    // stock amount page
    gnc_numeric balance_at_date;
    GtkWidget * stock_amount_page;
    GtkWidget * prev_amount;
    GtkWidget * next_amount;
    GtkWidget * stock_amount_edit;

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
} StockTransactionInfo;


/******* implementations ***********************************************/
static void
stock_assistant_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    gnc_unregister_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    g_free (info);
}

static void
refresh_page_transaction_type (GtkWidget *widget, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);

    auto type_idx = gtk_combo_box_get_active (GTK_COMBO_BOX (widget));
    if (type_idx < 0)           // combo isn't initialized yet.
        return;

    if (!info->txn_types)
        return;

    try
    {
        info->txn_type = info->txn_types->at (type_idx);
    }
    catch (const std::out_of_range&)
    {
        PERR ("out of range type_idx=%d", type_idx);
        return;
    }

    g_return_if_fail (info->txn_type);

    gtk_label_set_text (GTK_LABEL (info->transaction_type_explanation),
                        _(info->txn_type->explanation));

    // set default capitalize fees setting
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (info->capitalize_fees_checkbox),
                                  info->txn_type->fees_capitalize);
}


static void
refresh_page_stock_amount (GtkWidget *widget, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    g_return_if_fail (info->txn_type);

    auto pinfo = gnc_commodity_print_info (xaccAccountGetCommodity (info->acct), true);
    auto bal = info->balance_at_date;
    gtk_label_set_text (GTK_LABEL(info->prev_amount), xaccPrintAmount (bal, pinfo));

    gnc_numeric stock_amount;

    if (gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->stock_amount_edit),
                                       &stock_amount, true, nullptr))
        gtk_label_set_text (GTK_LABEL(info->next_amount), nullptr);
    else
    {
        if (info->txn_type->stock_amount == FieldMask::ENABLED_CREDIT)
            stock_amount = gnc_numeric_neg (stock_amount);
        bal = gnc_numeric_add_fixed (bal, stock_amount);

        gtk_label_set_text (GTK_LABEL(info->next_amount),
                            xaccPrintAmount (bal, pinfo));
    }
}


static void
refresh_page_stock_value (GtkWidget *widget, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    gnc_numeric amount, value;
    g_return_if_fail (info->txn_type);

    if (info->txn_type->stock_amount == FieldMask::DISABLED ||
        info->txn_type->stock_value == FieldMask::DISABLED ||
        gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->stock_amount_edit), &amount, true, nullptr) ||
        gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (info->stock_value_edit),  &value,  true, nullptr) ||
        gnc_numeric_zero_p (value))
    {
        // Translators: StockAssistant: N/A denotes stock price is not computable
        const char* na_label =  N_("N/A");
        gtk_label_set_text (GTK_LABEL (info->price_value), _(na_label));
        return;
    }

    auto price = gnc_numeric_div (value, amount, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
    auto pinfo = gnc_commodity_print_info (info->currency, true);
    gtk_label_set_text (GTK_LABEL (info->price_value), xaccPrintAmount (price, pinfo));
}

static void
refresh_page_cash (GtkWidget *widget, gpointer user_data)
{
    return;
}

static void
refresh_page_fees (GtkWidget *widget, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    auto capitalize_fees = gtk_toggle_button_get_active
        (GTK_TOGGLE_BUTTON (info->capitalize_fees_checkbox));
    gtk_widget_set_sensitive (info->fees_account, !capitalize_fees);
}

static void
refresh_page_dividend (GtkWidget *widget, gpointer user_data)
{
    return;
}

static void
refresh_page_capgains (GtkWidget *widget, gpointer user_data)
{
    return;
}

static void
add_error (StringVec& errors, const char* format_str, const char* arg)
{
    gchar *buf = g_strdup_printf (_(format_str),
                                  g_dpgettext2 (nullptr, "Stock Assistant: Page name", arg));
    errors.emplace_back (buf);
    g_free (buf);
}

static void
add_error_str (StringVec& errors, const char* str)
{
    errors.emplace_back (_(str));
}

static void
check_page (GtkListStore *list, gnc_numeric& debit, gnc_numeric& credit,
            FieldMask splitfield, Account *acct, GtkWidget *memo, GtkWidget *gae,
            gnc_commodity *comm, bool ignore_account,
            const char* page, StringVec& errors)
{
    if (splitfield == FieldMask::DISABLED)
        return;
    const char* missing_str = N_("(missing)");
    const gchar* amtstr;
    gnc_numeric amount;
    bool debit_side = (splitfield & FieldMask::ENABLED_DEBIT);

    if (gnc_amount_edit_expr_is_valid (GNC_AMOUNT_EDIT (gae), &amount, true, nullptr))
    {
        if (splitfield & FieldMask::ALLOW_ZERO)
            amtstr = "";
        else
        {
            add_error (errors, N_("Amount for %s is missing."), page);
            amtstr = _(missing_str);
        }
    }
    else
    {
        if (!(splitfield & FieldMask::ALLOW_NEGATIVE))
        {
            if ((splitfield & FieldMask::ALLOW_ZERO) && gnc_numeric_negative_p (amount))
                add_error (errors, N_("Amount for %s must not be negative."), page);
            else if (!(splitfield & FieldMask::ALLOW_ZERO) && !gnc_numeric_positive_p (amount))
                add_error (errors, N_("Amount for %s must be positive."), page);
        }
        if (gnc_numeric_negative_p (amount))
        {
            amount = gnc_numeric_neg (amount);
            debit_side = !debit_side;
        }
        if (debit_side)
            debit = gnc_numeric_add_fixed (debit, amount);
        else
            credit = gnc_numeric_add_fixed (credit, amount);
        amtstr = xaccPrintAmount (amount, gnc_commodity_print_info (comm, true));
    }

    auto memostr = gtk_entry_get_text (GTK_ENTRY (memo));
    auto memostr_escaped = g_markup_escape_text (memostr, -1);
    const gchar *acctstr;

    if (ignore_account)
        acctstr = "";
    else if (acct)
        acctstr = xaccAccountGetName (acct);
    else if ((splitfield & FieldMask::ALLOW_ZERO) && gnc_numeric_zero_p (amount))
        acctstr = "";
    else
    {
        add_error (errors, N_("Account for %s is missing"), page);
        acctstr = _(missing_str);
    }

    GtkTreeIter iter;
    gtk_list_store_append (list, &iter);
    gtk_list_store_set (list, &iter,
                        SPLIT_COL_ACCOUNT, acctstr,
                        SPLIT_COL_MEMO, memostr,
                        SPLIT_COL_MEMO_ESCAPED, memostr_escaped,
                        SPLIT_COL_DEBIT, debit_side ? amtstr : "",
                        SPLIT_COL_CREDIT, !debit_side ? amtstr : "",
                        -1);
    g_free (memostr_escaped);
}

static inline Account*
gas_account (GtkWidget *gas)
{
    return gnc_account_sel_get_account (GNC_ACCOUNT_SEL (gas));
}

static void
refresh_page_finish (StockTransactionInfo *info)
{
    g_return_if_fail (info->txn_type);
    auto view = GTK_TREE_VIEW (info->finish_split_view);
    auto list = GTK_LIST_STORE (gtk_tree_view_get_model(view));
    gtk_list_store_clear (list);

    gnc_numeric debit = gnc_numeric_zero ();
    gnc_numeric credit = gnc_numeric_zero ();
    StringVec errors, warnings;

    // check the stock transaction date. If there are existing stock
    // transactions dated after the date specified, it is very likely
    // the later stock transactions will be invalidated. warn the user
    // to review them.
    auto new_date = gnc_date_edit_get_date_end (GNC_DATE_EDIT (info->date_edit));
    auto last_split_node = g_list_last (xaccAccountGetSplitList (info->acct));
    if (last_split_node)
    {
        auto last_split = static_cast<const Split*> (last_split_node->data);
        auto last_split_date = xaccTransGetDate (xaccSplitGetParent (last_split));
        if (new_date <= last_split_date)
        {
            auto last_split_date_str = qof_print_date (last_split_date);
            auto new_date_str = qof_print_date (new_date);
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

    if (info->txn_type->stock_amount != FieldMask::DISABLED)
    {
        auto stock_amount = gnc_amount_edit_get_amount
            (GNC_AMOUNT_EDIT(info->stock_amount_edit));
        if (!gnc_numeric_positive_p (stock_amount))
            add_error_str (errors, N_("Stock amount must be positive"));
        if (info->txn_type->stock_amount & FieldMask::ENABLED_CREDIT)
            stock_amount = gnc_numeric_neg (stock_amount);
        auto new_bal = gnc_numeric_add_fixed (info->balance_at_date, stock_amount);
        if (gnc_numeric_positive_p (info->balance_at_date) &&
            gnc_numeric_negative_p (new_bal))
            add_error_str (errors, N_("Cannot sell more units than owned"));
        else if (gnc_numeric_negative_p (info->balance_at_date) &&
                 gnc_numeric_positive_p (new_bal))
            add_error_str (errors, N_("Cannot cover buy more units than owed"));
    }

    check_page (list, debit, credit, info->txn_type->stock_value, info->acct,
                info->stock_memo_edit, info->stock_value_edit, info->currency,
                false, NC_ ("Stock Assistant: Page name", "stock value"), errors);

    check_page (list, debit, credit, info->txn_type->cash_value,
                gas_account (info->cash_account), info->cash_memo_edit,
                info->cash_value, info->currency, false,
                NC_ ("Stock Assistant: Page name", "cash"), errors);

    check_page (list, debit, credit, info->txn_type->fees_value,
                gas_account (info->fees_account), info->fees_memo_edit,
                info->fees_value, info->currency,
                gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
                                              (info->capitalize_fees_checkbox)),
                NC_ ("Stock Assistant: Page name", "fees"), errors);

    check_page (list, debit, credit, info->txn_type->dividend_value,
                gas_account (info->dividend_account),
                info->dividend_memo_edit, info->dividend_value, info->currency,
                false, NC_ ("Stock Assistant: Page name", "dividend"), errors);

    // the next two checks will involve the two capgains splits:
    // income side and stock side. The capgains_value ^
    // (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT) will swap the debit/credit
    // flags.
    if (info->txn_type->capgains_value != FieldMask::DISABLED)
    {
        check_page (list, debit, credit, info->txn_type->capgains_value,
                    gas_account (info->capgains_account),
                    info->capgains_memo_edit, info->capgains_value, info->currency,
                    false, NC_ ("Stock Assistant: Page name", "capital gains"), errors);

        check_page (list, debit, credit,
                    info->txn_type->capgains_value ^ (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT),
                    info->acct, info->capgains_memo_edit, info->capgains_value,
                    info->currency, false,
                    NC_ ("Stock Assistant: Page name", "capital gains"), errors);
    }

    if (!gnc_numeric_equal (debit, credit))
    {
        auto imbalance_str = N_("Total Debits of %s does not balance with total Credits of %s.");
        auto print_info = gnc_commodity_print_info (info->currency, true);
        auto debit_str = g_strdup (xaccPrintAmount (debit, print_info));
        auto credit_str = g_strdup (xaccPrintAmount (credit, print_info));
        auto error_str = g_strdup_printf (_(imbalance_str), debit_str, credit_str);
        errors.emplace_back (error_str);
        g_free (error_str);
        g_free (credit_str);
        g_free (debit_str);
    }

    // generate final summary message. Collates a header, the errors
    // and warnings. Then allow completion if errors is empty.
    auto header = errors.empty() ?
        N_("No errors found. Click Apply to create transaction.") :
        N_("The following errors must be fixed:");
    auto summary = std::string { _(header) };
    auto summary_add = [&summary](auto a) { summary += "\n• "; summary += a; };
    std::for_each (errors.begin(), errors.end(), summary_add);
    if (!warnings.empty())
    {
        auto warnings_header = N_ ("The following warnings exist:");
        summary += "\n\n";
        summary += _(warnings_header);
        std::for_each (warnings.begin(), warnings.end(), summary_add);
    }
    gtk_label_set_text (GTK_LABEL (info->finish_summary), summary.c_str());
    gtk_assistant_set_page_complete (GTK_ASSISTANT (info->window),
                                     info->finish_page, errors.empty());
}

void
stock_assistant_prepare (GtkAssistant  *assistant, GtkWidget *page,
                         gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    gint currentpage = gtk_assistant_get_current_page(assistant);

    switch (currentpage)
    {
    case PAGE_TRANSACTION_TYPE:;
        // initialize transaction types.
        gnc_numeric balance;
        time64 date;
        date = gnc_date_edit_get_date_end (GNC_DATE_EDIT (info->date_edit));
        if (info->txn_types_date && (info->txn_types_date == date))
            break;
        info->txn_types_date = date;
        balance = xaccAccountGetBalanceAsOfDate (info->acct, date);
        info->txn_types = gnc_numeric_zero_p (balance) ? starting_types
            : gnc_numeric_positive_p (balance) ? long_types
            : short_types;
        gtk_combo_box_text_remove_all (GTK_COMBO_BOX_TEXT (info->transaction_type_combo));
        for (auto& it : *(info->txn_types))
            gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (info->transaction_type_combo),
                                            _(it.friendly_name));
        gtk_combo_box_set_active (GTK_COMBO_BOX (info->transaction_type_combo), 0);
        refresh_page_transaction_type (info->transaction_type_combo, info);
        gtk_widget_grab_focus (info->transaction_type_combo);
        break;
    case PAGE_STOCK_AMOUNT:
        info->balance_at_date = xaccAccountGetBalanceAsOfDate
            (info->acct, gnc_date_edit_get_date_end (GNC_DATE_EDIT (info->date_edit)));
        refresh_page_stock_amount (info->stock_amount_edit, info);
        // fixme: the following doesn't work???
        gtk_widget_grab_focus (gnc_amount_edit_gtk_entry
                               (GNC_AMOUNT_EDIT (info->stock_amount_edit)));
        break;
    case PAGE_STOCK_VALUE:
        refresh_page_stock_value (info->stock_value_edit, info);
        // fixme: ditto
        gtk_widget_grab_focus (gnc_amount_edit_gtk_entry
                               (GNC_AMOUNT_EDIT (info->stock_value_edit)));
        break;
    case PAGE_CASH:
        refresh_page_cash (info->cash_value, info);
        break;
    case PAGE_FEES:
        refresh_page_fees (info->fees_value, info);
        break;
    case PAGE_DIVIDEND:
        refresh_page_dividend (info->fees_value, info);
        break;
    case PAGE_CAPGAINS:
        refresh_page_capgains (info->capgains_value, info);
        break;
    case PAGE_FINISH:
        refresh_page_finish (info);
        break;
    }
}

static gint
forward_page_func (gint current_page, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    auto& txn_type = info->txn_type;

    current_page++;
    if (!txn_type)
        return current_page;

    if (txn_type->stock_amount == FieldMask::DISABLED && current_page == PAGE_STOCK_AMOUNT)
        current_page++;
    if (txn_type->stock_value == FieldMask::DISABLED && current_page == PAGE_STOCK_VALUE)
        current_page++;
    if (txn_type->cash_value == FieldMask::DISABLED && current_page == PAGE_CASH)
        current_page++;
    if (txn_type->fees_value == FieldMask::DISABLED && current_page == PAGE_FEES)
        current_page++;
    if (txn_type->dividend_value == FieldMask::DISABLED && current_page == PAGE_DIVIDEND)
        current_page++;
    if (txn_type->capgains_value == FieldMask::DISABLED && current_page == PAGE_CAPGAINS)
        current_page++;

    return current_page;
}

static void
create_split (Transaction *trans, FieldMask splitfield,
              const gchar *action, Account *account,
              AccountVec& account_commits, GtkWidget *memo_entry,
              GtkWidget *amount, GtkWidget *value,
              bool skip_if_zero)
{
    auto amount_numeric = amount ? gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (amount)) : gnc_numeric_zero ();
    auto value_numeric = value ? gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (value)) : gnc_numeric_zero ();

    if (skip_if_zero && gnc_numeric_zero_p (value_numeric))
        return;

    if (splitfield & FieldMask::ENABLED_CREDIT)
    {
        amount_numeric = gnc_numeric_neg (amount_numeric);
        value_numeric = gnc_numeric_neg (value_numeric);
    }

    auto split = xaccMallocSplit (gnc_get_current_book ());
    xaccSplitSetParent (split, trans);
    xaccAccountBeginEdit (account);
    account_commits.emplace_back (account);
    xaccSplitSetAccount (split, account);
    xaccSplitSetMemo (split, gtk_entry_get_text (GTK_ENTRY (memo_entry)));
    xaccSplitSetValue (split, value_numeric);
    xaccSplitSetAmount (split, amount_numeric);
    DEBUG ("creating %s split in Acct(%s): Val(%s), Amt(%s) => Val(%s), Amt(%s)",
           action, xaccAccountGetName (account),
           gnc_num_dbg_to_string (value_numeric),
           gnc_num_dbg_to_string (amount_numeric),
           gnc_num_dbg_to_string (xaccSplitGetValue (split)),
           gnc_num_dbg_to_string (xaccSplitGetAmount (split)));
    gnc_set_num_action (nullptr, split,
                        nullptr, g_dpgettext2 (nullptr, "Stock Assistant: Action field", action));
}

static void
add_price (GtkWidget *amount, GtkWidget *value,
           gnc_commodity *commodity, gnc_commodity *currency, time64 date)
{
    auto amt = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (amount));
    auto val = gnc_amount_edit_get_amount (GNC_AMOUNT_EDIT (value));
    auto p = gnc_numeric_div (val, amt, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
    auto price = gnc_price_create (gnc_get_current_book ());

    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, commodity);
    gnc_price_set_currency (price, currency);
    gnc_price_set_time64 (price, date);
    gnc_price_set_source (price, PRICE_SOURCE_STOCK_TRANSACTION);
    gnc_price_set_typestr (price, PRICE_TYPE_UNK);
    gnc_price_set_value (price, p);
    gnc_price_commit_edit (price);

    auto book = gnc_get_current_book ();
    auto pdb = gnc_pricedb_get_db (book);

    if (!gnc_pricedb_add_price (pdb, price))
        PWARN ("error adding price");

    gnc_price_unref (price);
}

void
stock_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    AccountVec account_commits;
    auto book = gnc_get_current_book ();
    g_return_if_fail (info->txn_type);

    gnc_suspend_gui_refresh ();

    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, info->currency);
    xaccTransSetDescription (trans, gtk_entry_get_text
                             (GTK_ENTRY (info->transaction_description_entry)));

    auto date = gnc_date_edit_get_date (GNC_DATE_EDIT (info->date_edit));
    xaccTransSetDatePostedSecsNormalized (trans, date);

    create_split (trans, info->txn_type->stock_value,
                  NC_ ("Stock Assistant: Action field", "Stock"),
                  info->acct, account_commits, info->stock_memo_edit,
                  info->txn_type->stock_amount != FieldMask::DISABLED ? info->stock_amount_edit : nullptr,
                  info->txn_type->stock_value != FieldMask::DISABLED ? info->stock_value_edit : nullptr,
                  false);

    if (info->txn_type->cash_value != FieldMask::DISABLED)
        create_split (trans, info->txn_type->cash_value,
                      NC_ ("Stock Assistant: Action field", "Cash"),
                      gas_account (info->cash_account),
                      account_commits, info->cash_memo_edit, info->cash_value,
                      info->cash_value, false);

    if (info->txn_type->fees_value != FieldMask::DISABLED)
    {
        auto capitalize = gtk_toggle_button_get_active
            (GTK_TOGGLE_BUTTON (info->capitalize_fees_checkbox));
        create_split (trans, info->txn_type->fees_value,
                      NC_ ("Stock Assistant: Action field", "Fees"),
                      capitalize ? info->acct : gas_account (info->fees_account),
                      account_commits, info->fees_memo_edit,
                      capitalize ? nullptr : info->fees_value,
                      info->fees_value, true);
    }

    if (info->txn_type->dividend_value != FieldMask::DISABLED)
        create_split (trans, info->txn_type->dividend_value,
                      NC_ ("Stock Assistant: Action field", "Dividend"),
                      gas_account (info->dividend_account),
                      account_commits, info->dividend_memo_edit,
                      info->dividend_value, info->dividend_value, false);

    if (info->txn_type->capgains_value != FieldMask::DISABLED)
    {
        create_split (trans, info->txn_type->capgains_value,
                      NC_ ("Stock Assistant: Action field", "Capital Gains"),
                      gas_account (info->capgains_account),
                      account_commits, info->capgains_memo_edit,
                      info->capgains_value, info->capgains_value, false);

        create_split (trans,
                      info->txn_type->capgains_value ^ (FieldMask::ENABLED_CREDIT | FieldMask::ENABLED_DEBIT),
                      NC_ ("Stock Assistant: Action field", "Capital Gains"),
                      info->acct, account_commits, info->capgains_memo_edit,
                      nullptr, info->capgains_value, false);
    }

    if (info->txn_type->stock_amount != FieldMask::DISABLED &&
        info->txn_type->stock_value != FieldMask::DISABLED)
        add_price (info->stock_amount_edit, info->stock_value_edit,
                   xaccAccountGetCommodity (info->acct), info->currency, date);

    xaccTransCommitEdit (trans);

    std::for_each (account_commits.begin(), account_commits.end(), xaccAccountCommitEdit);

    gnc_resume_gui_refresh ();

    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
}


void
stock_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
}

static GtkWidget*
get_widget (GtkBuilder *builder, const gchar * ID)
{
    g_return_val_if_fail (builder && ID, nullptr);
    auto obj = gtk_builder_get_object (builder, ID);
    if (!obj)
        PWARN ("get_widget ID '%s' not found. it may be a typo?", ID);
    return GTK_WIDGET (obj);
}

static GtkWidget*
create_gas (GtkBuilder *builder, gint row,
            std::vector<GNCAccountType> type, gnc_commodity *currency,
            const gchar *table_ID, const gchar *label_ID)
{
    auto table = get_widget (builder, table_ID);
    auto label = get_widget (builder, label_ID);
    auto gas = gnc_account_sel_new ();
    GList *acct_list = nullptr;
    for (auto& it : type)
        acct_list = g_list_prepend (acct_list, (gpointer)it);
    auto curr_list = g_list_prepend (nullptr, currency);
    gnc_account_sel_set_new_account_ability (GNC_ACCOUNT_SEL (gas), true);
    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL (gas), acct_list, curr_list);
    gtk_widget_show (gas);
    gtk_grid_attach (GTK_GRID(table), gas, 1, row, 1, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), gas);
    g_list_free (acct_list);
    g_list_free (curr_list);
    return gas;
}

static GtkWidget*
create_gae (GtkBuilder *builder, gint row, gnc_commodity *comm,
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

static GtkWidget*
create_date (GtkBuilder *builder, guint row,
             const gchar *date_label, const gchar *table_label)
{
    auto date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    auto label = get_widget (builder, date_label);
    gtk_grid_attach (GTK_GRID(get_widget (builder, table_label)), date, 1, row, 1, 1);
    gtk_widget_show (date);
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);
    return date;
}

static GtkWidget*
get_treeview (GtkBuilder *builder, const gchar *treeview_label)
{
    auto view = GTK_TREE_VIEW (get_widget (builder, "transaction_view"));
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());

    auto store = gtk_list_store_new (NUM_SPLIT_COLS, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
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

    return GTK_WIDGET (view);
}

static GtkWidget *
stock_assistant_create (StockTransactionInfo *info)
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-stock-transaction.glade", "stock_transaction_assistant");
    info->window = get_widget (builder, "stock_transaction_assistant");

    // Set the name for this assistant so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(info->window), "gnc-id-assistant-stock-transaction");

    info->currency = gnc_account_get_currency_or_parent (info->acct);

    /* Transaction Page Widgets */
    info->transaction_type_page = get_widget (builder, "transaction_type_page");
    info->transaction_type_combo = get_widget (builder, "transaction_type_page_combobox");
    info->transaction_type_explanation = get_widget (builder, "transaction_type_page_explanation");
    g_signal_connect (info->transaction_type_combo, "changed",
                      G_CALLBACK (refresh_page_transaction_type), info);

    /* Transaction Details Widgets */
    info->transaction_details_page = get_widget (builder, "transaction_details_page");
    info->date_edit = create_date (builder, 0, "transaction_date_label", "transaction_details_table");
    info->transaction_description_entry = get_widget (builder, "transaction_description_entry");

    /* Stock Amount Page Widgets */
    info->stock_amount_page = get_widget (builder, "stock_amount_page");
    info->prev_amount = get_widget (builder, "prev_balance_amount");
    info->stock_amount_edit = create_gae (builder, 1, xaccAccountGetCommodity (info->acct), "stock_amount_table", "stock_amount_label");
    info->next_amount = get_widget (builder, "next_balance_amount");
    g_signal_connect (info->stock_amount_edit, "changed",
                      G_CALLBACK (refresh_page_stock_amount), info);

    /* Stock Value Page Widgets */
    info->stock_value_page = get_widget (builder, "stock_value_page");
    info->stock_value_edit = create_gae (builder, 0, info->currency, "stock_value_table", "stock_value_label");
    info->price_value = get_widget (builder, "stock_price_amount");
    info->stock_memo_edit = get_widget (builder, "stock_memo_entry");
    g_signal_connect (info->stock_value_edit, "changed",
                      G_CALLBACK (refresh_page_stock_value), info);

    /* Cash Page Widgets */
    info->cash_page = get_widget (builder, "cash_details_page");
    info->cash_account = create_gas (builder, 0, { ACCT_TYPE_ASSET, ACCT_TYPE_BANK }, info->currency,  "cash_table", "cash_account_label");
    info->cash_value = create_gae (builder, 1, info->currency, "cash_table", "cash_label");
    info->cash_memo_edit = get_widget (builder, "cash_memo_entry");

    /* Fees Page Widgets */
    info->fees_page = get_widget (builder, "fees_details_page");
    info->capitalize_fees_checkbox = get_widget (builder, "capitalize_fees_checkbutton");
    info->fees_account = create_gas (builder, 1, { ACCT_TYPE_EXPENSE }, info->currency, "fees_table", "fees_account_label");
    info->fees_value = create_gae (builder, 2, info->currency, "fees_table", "fees_label");
    info->fees_memo_edit = get_widget (builder, "fees_memo_entry");
    g_signal_connect (info->capitalize_fees_checkbox, "toggled",
                      G_CALLBACK (refresh_page_fees), info);

    /* Divi Page Widgets */
    info->dividend_page = get_widget (builder, "dividend_details_page");
    info->dividend_account = create_gas (builder, 0, { ACCT_TYPE_INCOME }, info->currency, "dividend_table", "dividend_account_label");
    info->dividend_value = create_gae (builder, 1, info->currency, "dividend_table", "dividend_label");
    info->dividend_memo_edit = get_widget (builder, "dividend_memo_entry");

    /* Capgains Page Widgets */
    info->capgains_page = get_widget (builder, "capgains_details_page");
    info->capgains_account = create_gas (builder, 0, { ACCT_TYPE_INCOME }, info->currency, "capgains_table", "capgains_account_label");
    info->capgains_value = create_gae (builder, 1, info->currency, "capgains_table", "capgains_label");
    info->capgains_memo_edit = get_widget (builder, "capgains_memo_entry");

    /* Finish Page Widgets */
    info->finish_page = get_widget (builder, "finish_page");
    info->finish_split_view = get_treeview (builder, "transaction_view");
    info->finish_summary = get_widget (builder, "finish_summary");
    g_signal_connect (G_OBJECT(info->window), "destroy",
                      G_CALLBACK (stock_assistant_window_destroy_cb), info);
    gtk_tree_view_set_tooltip_column (GTK_TREE_VIEW (info->finish_split_view),
                                      SPLIT_COL_MEMO_ESCAPED);

    gtk_assistant_set_forward_page_func (GTK_ASSISTANT(info->window),
                                         (GtkAssistantPageFunc)forward_page_func,
                                         info, nullptr);
    gtk_builder_connect_signals(builder, info);
    g_object_unref(G_OBJECT(builder));

    return info->window;
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);

    if (!GNC_IS_ACCOUNT (info->acct))
    {
        PWARN ("account %p does not exist anymore. abort", info->acct);
        gnc_close_gui_component_by_data (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, info);
    }
}

static void
close_handler (gpointer user_data)
{
    auto info = static_cast<StockTransactionInfo*>(user_data);
    gtk_widget_destroy (info->window);
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
    StockTransactionInfo *info = g_new0 (StockTransactionInfo, 1);
    info->acct = account;
    stock_assistant_create (info);
    auto component_id = gnc_register_gui_component
        (ASSISTANT_STOCK_TRANSACTION_CM_CLASS, refresh_handler, close_handler, info);
    gnc_gui_component_watch_entity_type (component_id, GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    gtk_window_set_transient_for (GTK_WINDOW (info->window), GTK_WINDOW(parent));
    gtk_widget_show_all (info->window);

    // gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}

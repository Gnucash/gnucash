/********************************************************************\
 * gnc-imp-props-tx.cpp - import transactions from csv files        *
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
 *                                                                  *
\********************************************************************/

extern "C" {
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>

#include "engine-helpers.h"
#include "gnc-csv-account-map.h"
#include "gnc-ui-util.h"
#include "Account.h"
#include "Transaction.h"
#include "gnc-pricedb.h"

}

#include <string>
#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>
#include "gnc-imp-props-tx.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

/* This map contains a set of strings representing the different column types. */
std::map<GncTransPropType, const char*> gnc_csv_col_type_strs = {
        { GncTransPropType::NONE, N_("None") },
        { GncTransPropType::UNIQUE_ID, N_("Transaction ID") },
        { GncTransPropType::DATE, N_("Date") },
        { GncTransPropType::NUM, N_("Num") },
        { GncTransPropType::DESCRIPTION, N_("Description") },
        { GncTransPropType::NOTES, N_("Notes") },
        { GncTransPropType::COMMODITY, N_("Transaction Commodity") },
        { GncTransPropType::VOID_REASON, N_("Void Reason") },
        { GncTransPropType::ACTION, N_("Action") },
        { GncTransPropType::ACCOUNT, N_("Account") },
        { GncTransPropType::DEPOSIT, N_("Deposit") },
        { GncTransPropType::WITHDRAWAL, N_("Withdrawal") },
        { GncTransPropType::PRICE, N_("Price") },
        { GncTransPropType::MEMO, N_("Memo") },
        { GncTransPropType::REC_STATE, N_("Reconciled") },
        { GncTransPropType::REC_DATE, N_("Reconcile Date") },
        { GncTransPropType::TACTION, N_("Transfer Action") },
        { GncTransPropType::TACCOUNT, N_("Transfer Account") },
        { GncTransPropType::TMEMO, N_("Transfer Memo") },
        { GncTransPropType::TREC_STATE, N_("Transfer Reconciled") },
        { GncTransPropType::TREC_DATE, N_("Transfer Reconcile Date") }
};

/* Below two vectors define which properties the user *can't* select
 * in two-split or multi-split mode (mostly because they don't make
 * sense in that context).
 */
std::vector<GncTransPropType> twosplit_blacklist = {
        GncTransPropType::UNIQUE_ID };
std::vector<GncTransPropType> multisplit_blacklist = {
        GncTransPropType::TACTION,
        GncTransPropType::TACCOUNT,
        GncTransPropType::TMEMO,
        GncTransPropType::TREC_STATE,
        GncTransPropType::TREC_DATE
};

GncTransPropType sanitize_trans_prop (GncTransPropType prop, bool multi_split)
{
    auto bl = multi_split ? multisplit_blacklist : twosplit_blacklist;
    if (std::find(bl.begin(), bl.end(), prop) == bl.end())
        return prop;
    else
        return GncTransPropType::NONE;
}


/** Convert str into a GncRational using the user-specified (import) currency format.
 * @param str The string to be parsed
 * @param currency_format The currency format to use.
 * @return a GncNumeric
 * @exception May throw std::invalid argument if string can't be parsed properly
 */
GncNumeric parse_amount (const std::string &str, int currency_format)
{
    /* If a cell is empty or just spaces return invalid amount */
    if(!boost::regex_search(str, boost::regex("[0-9]")))
        throw std::invalid_argument (_("Value doesn't appear to contain a valid number."));

    auto expr = boost::make_u32regex("[[:Sc:]]");
    std::string str_no_symbols = boost::u32regex_replace(str, expr, "");

    /* Convert based on user chosen currency format */
    gnc_numeric val;
    char *endptr;
    switch (currency_format)
    {
    case 0:
        /* Currency locale */
        if (!(xaccParseAmount (str_no_symbols.c_str(), TRUE, &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    case 1:
        /* Currency decimal period */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    case 2:
        /* Currency decimal comma */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    }

    return GncNumeric(val);
}

static char parse_reconciled (const std::string& reconcile)
{
    if (g_strcmp0 (reconcile.c_str(), gnc_get_reconcile_str(NREC)) == 0) // Not reconciled
        return NREC;
    else if (g_strcmp0 (reconcile.c_str(), gnc_get_reconcile_str(CREC)) == 0) // Cleared
        return CREC;
    else if (g_strcmp0 (reconcile.c_str(), gnc_get_reconcile_str(YREC)) == 0) // Reconciled
        return YREC;
    else if (g_strcmp0 (reconcile.c_str(), gnc_get_reconcile_str(FREC)) == 0) // Frozen
        return FREC;
    else if (g_strcmp0 (reconcile.c_str(), gnc_get_reconcile_str(VREC)) == 0) // Voided will be handled at the transaction level
        return NREC;                                                          // so return not reconciled here
    else
        throw std::invalid_argument (_("Value can't be parsed into a valid reconcile state."));
}

gnc_commodity* parse_commodity (const std::string& comm_str)
{
    if (comm_str.empty())
        return nullptr;

    auto table = gnc_commodity_table_get_table (gnc_get_current_book());
    gnc_commodity* comm = nullptr;

    /* First try commodity as a unique name. */
    if (comm_str.find("::"))
        comm = gnc_commodity_table_lookup_unique (table, comm_str.c_str());

    /* Then try mnemonic in the currency namespace */
    if (!comm)
        comm = gnc_commodity_table_lookup (table,
                GNC_COMMODITY_NS_CURRENCY, comm_str.c_str());

    if (!comm)
    {
        /* If that fails try mnemonic in all other namespaces */
        auto namespaces = gnc_commodity_table_get_namespaces(table);
        for (auto ns = namespaces; ns; ns = ns->next)
        {
            gchar* ns_str = (gchar*)ns->data;
            if (g_utf8_collate(ns_str, GNC_COMMODITY_NS_CURRENCY) == 0)
                continue;

            comm = gnc_commodity_table_lookup (table,
                    ns_str, comm_str.c_str());
            if (comm)
                break;
        }
    }

    if (!comm)
        throw std::invalid_argument (_("Value can't be parsed into a valid commodity."));
    else
        return comm;
}

void GncPreTrans::set (GncTransPropType prop_type, const std::string& value)
{
    try
    {
        // Drop any existing error for the prop_type we're about to set
        m_errors.erase(prop_type);

        gnc_commodity *comm = nullptr;
        switch (prop_type)
        {
            case GncTransPropType::UNIQUE_ID:
                m_differ = boost::none;
                if (!value.empty())
                    m_differ = value;
                break;

            case GncTransPropType::DATE:
                m_date = boost::none;
                m_date = GncDate(value, GncDate::c_formats[m_date_format].m_fmt); // Throws if parsing fails
                break;

            case GncTransPropType::NUM:
                m_num = boost::none;
                if (!value.empty())
                    m_num = value;
                break;

            case GncTransPropType::DESCRIPTION:
                m_desc = boost::none;
                if (!value.empty())
                    m_desc = value;
                break;

            case GncTransPropType::NOTES:
                m_notes = boost::none;
                if (!value.empty())
                    m_notes = value;
                break;

            case GncTransPropType::COMMODITY:
                m_commodity = boost::none;
                comm = parse_commodity (value); // Throws if parsing fails
                if (comm)
                    m_commodity = comm;
                break;

            case GncTransPropType::VOID_REASON:
                m_void_reason = boost::none;
                if (!value.empty())
                    m_void_reason = value;
                break;

            default:
                /* Issue a warning for all other prop_types. */
                PWARN ("%d is an invalid property for a transaction", static_cast<int>(prop_type));
                break;
        }
    }
    catch (const std::invalid_argument& e)
    {
        auto err_str = std::string(_(gnc_csv_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
    catch (const std::out_of_range& e)
    {
        auto err_str = std::string(_(gnc_csv_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }

}

void GncPreTrans::reset (GncTransPropType prop_type)
{
    try
    {
        set (prop_type, std::string());
    }
    catch (...)
    {
        // Set with an empty string will effectively clear the property
        // but can also set an error for the property. Clear that error here.
        m_errors.erase(prop_type);
    }
}

std::string GncPreTrans::verify_essentials (void)
{
    /* Make sure this transaction has the minimum required set of properties defined */
    if (!m_date)
        return _("No date column.");
    else
        return std::string();
}

Transaction* GncPreTrans::create_trans (QofBook* book, gnc_commodity* currency)
{
    if (created)
        return nullptr;

    /* Gently refuse to create the transaction if the basics are not set correctly
     * This should have been tested before calling this function though!
     */
    auto check = verify_essentials();
    if (!check.empty())
    {
        PWARN ("Refusing to create transaction because essentials not set properly: %s", check.c_str());
        return nullptr;
    }

    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, m_commodity ? *m_commodity : currency);
    xaccTransSetDatePostedSecsNormalized (trans,
                        static_cast<time64>(GncDateTime(*m_date, DayPart::neutral)));

    if (m_num)
        xaccTransSetNum (trans, m_num->c_str());

    if (m_desc)
        xaccTransSetDescription (trans, m_desc->c_str());

    if (m_notes)
        xaccTransSetNotes (trans, m_notes->c_str());


    created = true;
    return trans;
}

bool GncPreTrans::is_part_of (std::shared_ptr<GncPreTrans> parent)
{
    if (!parent)
        return false;

    return (!m_differ || m_differ == parent->m_differ) &&
            (!m_date || m_date == parent->m_date) &&
            (!m_num || m_num == parent->m_num) &&
            (!m_desc || m_desc == parent->m_desc) &&
            (!m_notes || m_notes == parent->m_notes) &&
            (!m_commodity || m_commodity == parent->m_commodity) &&
            (!m_void_reason || m_void_reason == parent->m_void_reason) &&
            parent->m_errors.empty(); // A GncPreTrans with errors can never be a parent
}

/* Declare two translatable error strings here as they will be used in several places */
const char *bad_acct = N_("Account value can't be mapped back to an account.");
const char *bad_tacct = N_("Transfer account value can't be mapped back to an account.");

static std::string gen_err_str (std::map<GncTransPropType, std::string>& errors,
        bool check_accts_mapped = false)
{
    auto full_error = std::string();
    for (auto error : errors)
    {
        auto err_str = error.second;
        if (!check_accts_mapped &&
                ((err_str.find (_(bad_acct)) != std::string::npos) ||
                 (err_str.find (_(bad_tacct)) != std::string::npos)))
            continue;
        full_error += (full_error.empty() ? "" : "\n") + error.second;
    }

    return full_error;
}

std::string GncPreTrans::errors ()
{
    return gen_err_str (m_errors);
}

void GncPreSplit::set (GncTransPropType prop_type, const std::string& value)
{
    try
    {
        // Drop any existing error for the prop_type we're about to set
        m_errors.erase(prop_type);

        Account *acct = nullptr;
        switch (prop_type)
        {
            case GncTransPropType::ACTION:
                m_action = boost::none;
                if (!value.empty())
                    m_action = value;
                break;

            case GncTransPropType::TACTION:
                m_taction = boost::none;
                if (!value.empty())
                    m_taction = value;
                break;

            case GncTransPropType::ACCOUNT:
                m_account = boost::none;
                if (value.empty())
                    throw std::invalid_argument (_("Account value can't be empty."));
                acct = gnc_csv_account_map_search (value.c_str());
                if (acct)
                    m_account = acct;
                else
                    throw std::invalid_argument (_(bad_acct));
                break;

            case GncTransPropType::TACCOUNT:
                m_taccount = boost::none;
                if (value.empty())
                    throw std::invalid_argument (_("Transfer account value can't be empty."));

                acct = gnc_csv_account_map_search (value.c_str());
                if (acct)
                    m_taccount = acct;
                else
                    throw std::invalid_argument (_(bad_tacct));
                break;

            case GncTransPropType::MEMO:
                m_memo = boost::none;
                if (!value.empty())
                    m_memo = value;
                break;

            case GncTransPropType::TMEMO:
                m_tmemo = boost::none;
                if (!value.empty())
                    m_tmemo = value;
                break;

            case GncTransPropType::DEPOSIT:
                m_deposit = boost::none;
                m_deposit = parse_amount (value, m_currency_format); // Will throw if parsing fails
                break;
            case GncTransPropType::WITHDRAWAL:
                m_withdrawal = boost::none;
                m_withdrawal = parse_amount (value, m_currency_format); // Will throw if parsing fails
                break;

            case GncTransPropType::PRICE:
                m_price = boost::none;
                m_price = parse_amount (value, m_currency_format); // Will throw if parsing fails
                break;

            case GncTransPropType::REC_STATE:
                m_rec_state = boost::none;
                m_rec_state = parse_reconciled (value); // Throws if parsing fails
                break;

            case GncTransPropType::TREC_STATE:
                m_trec_state = boost::none;
                m_trec_state = parse_reconciled (value); // Throws if parsing fails
                break;

            case GncTransPropType::REC_DATE:
                m_rec_date = boost::none;
                if (!value.empty())
                    m_rec_date = GncDate (value,
                                          GncDate::c_formats[m_date_format].m_fmt); // Throws if parsing fails
                break;

            case GncTransPropType::TREC_DATE:
                m_trec_date = boost::none;
                if (!value.empty())
                    m_trec_date = GncDate (value,
                                           GncDate::c_formats[m_date_format].m_fmt); // Throws if parsing fails
                break;

            default:
                /* Issue a warning for all other prop_types. */
                PWARN ("%d is an invalid property for a split", static_cast<int>(prop_type));
                break;
        }
    }
    catch (const std::invalid_argument& e)
    {
        auto err_str = std::string(_(gnc_csv_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
    catch (const std::out_of_range& e)
    {
        auto err_str = std::string(_(gnc_csv_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
}

void GncPreSplit::reset (GncTransPropType prop_type)
{
    try
    {
        set (prop_type, std::string());
    }
    catch (...)
    {
        // Set with an empty string will effectively clear the property
        // but can also set an error for the property. Clear that error here.
        m_errors.erase(prop_type);
    }
}

std::string GncPreSplit::verify_essentials (void)
{
    auto err_msg = std::string();
    /* Make sure this split has the minimum required set of properties defined. */
    if (!m_deposit && !m_withdrawal)
        err_msg = _("No deposit or withdrawal column.");

    if (m_rec_state && *m_rec_state == YREC && !m_rec_date)
    {
        if (!err_msg.empty())
            err_msg += "\n";
        err_msg += _("Split is reconciled but reconcile date column is missing or invalid.");
    }

    if (m_trec_state && *m_trec_state == YREC && !m_trec_date)
    {
        if (!err_msg.empty())
            err_msg += "\n";
        err_msg += _("Transfer split is reconciled but transfer reconcile date column is missing or invalid.");
    }

    return err_msg;
}

/** Adds a split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The split's account
 * @param amount The split's amount
 * @param rec_state The split's reconcile status
 * @param rec_date The split's reconcile date
 * @param price The split's conversion rate from account commodity to transaction commodity
 */
static void trans_add_split (Transaction* trans, Account* account, GncNumeric amount,
                            const boost::optional<std::string>& action,
                            const boost::optional<std::string>& memo,
                            const boost::optional<char>& rec_state,
                            const boost::optional<GncDate>& rec_date,
                            const boost::optional<GncNumeric> price)
{
    QofBook* book = xaccTransGetBook (trans);
    auto split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, static_cast<gnc_numeric>(amount));
    auto trans_curr = xaccTransGetCurrency(trans);
    auto acct_comm = xaccAccountGetCommodity(account);
    GncNumeric value;
    if (gnc_commodity_equiv(trans_curr, acct_comm))
        value = amount;
    else if (price)
        value = amount * *price;
    else
    {
        Timespec ts = {xaccTransRetDatePosted (trans), 0};
        /* Import data didn't specify price, let's lookup the nearest in time */
        auto nprice = gnc_pricedb_lookup_nearest_in_time(gnc_pricedb_get_db(book),
                acct_comm, trans_curr, ts);
        if (nprice)
        {
            /* Found a usable price. Let's check if the conversion direction is right */
            GncNumeric rate;
            if (gnc_commodity_equiv(gnc_price_get_currency(nprice), trans_curr))
                rate = gnc_price_get_value(nprice);
            else
                rate = static_cast<GncNumeric>(gnc_price_get_value(nprice)).inv();

            value = amount * rate;
        }
        else
        {
            PWARN("No price found, using a price of 1.");
            value = amount;
        }
    }
    xaccSplitSetValue (split, static_cast<gnc_numeric>(value));

    if (memo)
        xaccSplitSetMemo (split, memo->c_str());
    /* Note, this function assumes the num/action switch is done at a higher level
     * if needed by the book option */
    if (action)
        xaccSplitSetAction (split, action->c_str());

    if (rec_state && *rec_state != 'n')
        xaccSplitSetReconcile (split, *rec_state);
    if (rec_state && *rec_state == YREC && rec_date)
        xaccSplitSetDateReconciledSecs (split,
                static_cast<time64>(GncDateTime(*rec_date, DayPart::neutral)));

}

void GncPreSplit::create_split (Transaction* trans)
{
    if (created)
        return;

    /* Gently refuse to create the split if the basics are not set correctly
     * This should have been tested before calling this function though!
     */
    auto check = verify_essentials();
    if (!check.empty())
    {
        PWARN ("Not creating split because essentials not set properly: %s", check.c_str());
        return;
    }

    Account *account = nullptr;
    Account *taccount = nullptr;
    auto deposit = GncNumeric();
    auto withdrawal = GncNumeric();
    auto amount = GncNumeric();

    if (m_account)
        account = *m_account;
    if (m_taccount)
        taccount = *m_taccount;
    if (m_deposit)
        deposit = *m_deposit;
    if (m_withdrawal)
        withdrawal = *m_withdrawal;

    amount = deposit + withdrawal;

    /* Add a split with the cumulative amount value. */
    trans_add_split (trans, account, amount, m_action, m_memo, m_rec_state, m_rec_date, m_price);

    if (taccount)
    {
        /* Note: the current importer assumes at most 2 splits. This means the second split amount
         * will be the negative of the the first split amount.
         */
        auto inv_price = m_price;
        if (m_price)
            inv_price = m_price->inv();
        trans_add_split (trans, taccount, -amount, m_taction, m_tmemo, m_trec_state, m_trec_date, inv_price);
    }

    created = true;
}

std::string GncPreSplit::errors (bool check_accts_mapped)
{
    return gen_err_str (m_errors, check_accts_mapped);
}

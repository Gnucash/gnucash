/********************************************************************\
 * gnc-csv-imp-trans.cpp - import transactions from csv files       *
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
#include "gnc-trans-props.hpp"

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
        { GncTransPropType::BALANCE, N_("Balance") },
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

/* Regular expressions used to parse dates per date format */
const char* date_regex[] = {
                             "(?:"                                   // either y-m-d
                                 "(?<YEAR>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)"
                             "|"                                     // or CCYYMMDD
                                 "(?<YEAR>[0-9]{4})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                             ")",

                             "(?:"                                   // either d-m-y
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<YEAR>[0-9]+)"
                             "|"                                     // or DDMMCCYY
                                 "(?<DAY>[0-9]{2})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<YEAR>[0-9]{4})"
                             ")",

                             "(?:"                                   // either m-d-y
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<YEAR>[0-9]+)"
                             "|"                                     // or MMDDCCYY
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                                 "(?<YEAR>[0-9]{4})"
                             ")",

                             "(?:"                                   // either d-m(-y)
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)(?:[-/.' ]+"
                                 "(?<YEAR>[0-9]+))?"
                             "|"                                     // or DDMM(CCYY)
                                 "(?<DAY>[0-9]{2})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<YEAR>[0-9]+)?"
                             ")",

                             "(?:"                                   // either m-d(-y)
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)(?:[-/.' ]+"
                                 "(?<YEAR>[0-9]+))?"
                             "|"                                     // or MMDD(CCYY)
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                                 "(?<YEAR>[0-9]+)?"
                             ")",
};

/** Parses a string into a date, given a format. This function
 * requires only knowing the order in which the year, month and day
 * appear. For example, 01-02-2003 will be parsed the same way as
 * 01/02/2003.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @exception std::invalid_argument if the string can't be parsed into a date.
 * @return The parsed value of date_str on success or -1 on failure
 */

time64 parse_date (const std::string &date_str, int format)
{
    boost::regex r(date_regex[format]);
    boost::smatch what;
    if(!boost::regex_search(date_str, what, r))
        throw std::invalid_argument ("String doesn't appear to be formatted as a date.");  // regex didn't find a match

    // Attention: different behavior from 2.6.x series !
    // If date format without year was selected, the match
    // should NOT have found a year.
    if ((format >= 3) && (what.length("YEAR") != 0))
        throw std::invalid_argument ("String appears to contain a year while the selected format forbids this.");

    auto day = std::stoi (what.str("DAY"));
    auto month = std::stoi (what.str("MONTH"));

    int year;
    if (format < 3)
    {
        /* The input dates have a year, so use that one */
        year = std::stoi (what.str("YEAR"));

        /* Handle two-digit years. */
        if (year < 100)
        {
            /* We allow two-digit years in the range 1969 - 2068. */
            if (year < 69)
                year += 2000;
            else
                year += 1900;
        }
    }
    else
    {
        /* The input dates don't have a year, so work with today's year.
         */
        gnc_timespec2dmy(timespec_now(), nullptr, nullptr, &year);
    }

    auto ts = gnc_dmy2timespec_neutral(day, month, year);
    return ts.tv_sec;
}


/** Convert str into a gnc_numeric using the user-specified (import) currency format.
 * @param str The string to be parsed
 * @param currency_format The currency format to use.
 * @return a gnc_numeric on success, boost::none on failure
 */
static boost::optional<gnc_numeric> parse_amount (const std::string &str, int currency_format)
{
    /* If a cell is empty or just spaces return invalid amount */
    if(!boost::regex_search(str, boost::regex("[0-9]")))
        throw std::invalid_argument ("String doesn't appear to contain a valid number.");

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
            throw std::invalid_argument ("String can't be parsed into a number using the selected currency format.");
        break;
    case 1:
        /* Currency decimal period */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument ("String can't be parsed into a number using the selected currency format.");
        break;
    case 2:
        /* Currency decimal comma */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument ("String can't be parsed into a number using the selected currency format.");
        break;
    }

    return val;
}

static char parse_reconciled (const std::string& reconcile)
{
    if (g_strcmp0 (reconcile.c_str(), _("n")) == 0) // Not reconciled
        return NREC;
    else if (g_strcmp0 (reconcile.c_str(), _("c")) == 0) // Cleared
        return CREC;
    else if (g_strcmp0 (reconcile.c_str(), _("y")) == 0) // Reconciled
        return YREC;
    else if (g_strcmp0 (reconcile.c_str(), _("f")) == 0) // Frozen
        return FREC;
    else if (g_strcmp0 (reconcile.c_str(), _("v")) == 0) // Voided will be handled at the transaction level
        return NREC;                                      // so return not reconciled here
    else
        throw std::invalid_argument ("String can't be parsed into a valid reconcile state.");
}

static gnc_commodity* parse_commodity (const std::string& comm_str)
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
        throw std::invalid_argument ("String can't be parsed into a valid commodity.");
    else
        return comm;
}

void GncPreTrans::set_property (GncTransPropType prop_type, const std::string& value)
{
    switch (prop_type)
    {
        case GncTransPropType::UNIQUE_ID:
            if (!value.empty())
                m_differ = value;
            else
                m_differ = boost::none;
            break;

        case GncTransPropType::DATE:
            m_date = parse_date (value, m_date_format); // Throws if parsing fails
            break;

        case GncTransPropType::NUM:
            if (!value.empty())
                m_num = value;
            else
                m_num = boost::none;
            break;

        case GncTransPropType::DESCRIPTION:
            if (!value.empty())
                m_desc = value;
            else
                m_desc = boost::none;
            break;

        case GncTransPropType::NOTES:
            if (!value.empty())
                m_notes = value;
            else
                m_notes = boost::none;
            break;

        case GncTransPropType::COMMODITY:
            m_commodity = parse_commodity (value); // Throws if parsing fails
            break;

        case GncTransPropType::VOID_REASON:
            if (!value.empty())
                m_void_reason = value;
            else
                m_void_reason = boost::none;
            break;

        default:
            /* Issue a warning for all other prop_types. */
            PWARN ("%d is an invalid property for a transaction", static_cast<int>(prop_type));
            break;
    }

}

std::string GncPreTrans::verify_essentials (void)
{
    /* Make sure this transaction has the minimum required set of properties defined */
    if (m_date == boost::none)
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
    xaccTransSetDatePostedSecsNormalized (trans, *m_date);

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
            (!m_void_reason || m_void_reason == parent->m_void_reason);
}

void GncPreSplit::set_property (GncTransPropType prop_type, const std::string& value)
{
    Account *acct = nullptr;
    switch (prop_type)
    {
        case GncTransPropType::ACTION:
            if (!value.empty())
                m_action = value;
            else
                m_action = boost::none;
            break;

        case GncTransPropType::TACTION:
            if (!value.empty())
                m_taction = value;
            else
                m_taction = boost::none;
            break;

        case GncTransPropType::ACCOUNT:
            acct = gnc_csv_account_map_search (value.c_str());
            if (acct)
                m_account = acct;
            else
                throw std::invalid_argument ("String can't be mapped back to an account.");
            break;

        case GncTransPropType::TACCOUNT:
            acct = gnc_csv_account_map_search (value.c_str());
            if (acct)
                m_taccount = acct;
            else
                throw std::invalid_argument ("String can't be mapped back to an account.");
            break;

        case GncTransPropType::MEMO:
            if (!value.empty())
                m_memo = value;
            else
                m_memo = boost::none;
            break;

        case GncTransPropType::TMEMO:
            if (!value.empty())
                m_tmemo = value;
            else
                m_tmemo = boost::none;
            break;

        case GncTransPropType::BALANCE:
            m_balance = parse_amount (value, m_currency_format); // Will throw if parsing fails
            break;
        case GncTransPropType::DEPOSIT:
            m_deposit = parse_amount (value, m_currency_format); // Will throw if parsing fails
            break;
        case GncTransPropType::WITHDRAWAL:
            m_withdrawal = parse_amount (value, m_currency_format); // Will throw if parsing fails
            break;

        case GncTransPropType::PRICE:
            m_price = parse_amount (value, m_currency_format); // Will throw if parsing fails
            break;

        case GncTransPropType::REC_STATE:
            m_rec_state = parse_reconciled (value); // Throws if parsing fails
            break;

        case GncTransPropType::TREC_STATE:
            m_trec_state = parse_reconciled (value); // Throws if parsing fails
            break;

        case GncTransPropType::REC_DATE:
            if (!value.empty())
                m_rec_date = parse_date (value, m_date_format); // Throws if parsing fails
            break;

        case GncTransPropType::TREC_DATE:
            m_trec_date = parse_date (value, m_date_format); // Throws if parsing fails
            break;

        default:
            /* Issue a warning for all other prop_types. */
            PWARN ("%d is an invalid property for a split", static_cast<int>(prop_type));
            break;
    }

}

std::string GncPreSplit::verify_essentials (void)
{
    auto err_msg = std::string();
    /* Make sure this split has the minimum required set of properties defined. */
    if ((m_deposit == boost::none) &&
        (m_withdrawal == boost::none) &&
        (m_balance == boost::none))
        err_msg = _("No balance, deposit, or withdrawal column.");

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
 * @param account The account used for the split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_split (Transaction* trans, Account* account, gnc_numeric amount,
                            const boost::optional<std::string>& action,
                            const boost::optional<std::string>& memo,
                            const boost::optional<char>& rec_state,
                            const boost::optional<time64> rec_date,
                            const boost::optional<gnc_numeric> price)
{
    QofBook* book = xaccTransGetBook (trans);
    auto split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    auto trans_curr = xaccTransGetCurrency(trans);
    auto acct_comm = xaccAccountGetCommodity(account);
    if (gnc_commodity_equiv(trans_curr, acct_comm))
        xaccSplitSetValue (split, amount);
    else if (price)
    {
        gnc_numeric value = gnc_numeric_mul (amount, *price, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
        xaccSplitSetValue (split, value);
    }
    else
    {
        auto tts = xaccTransRetDatePostedTS (trans);
        /* Import data didn't specify price, let's lookup the nearest in time */
        auto nprice = gnc_pricedb_lookup_nearest_in_time(gnc_pricedb_get_db(book),
                acct_comm, trans_curr, tts);
        if (!nprice)
        {
            PWARN("No price found, using a price of 1.");
            xaccSplitSetValue (split, amount);
        }
        else
        {
            /* Found a usable price. Let's check if the conversion direction is right */
            gnc_numeric rate = {0, 1};
            if (gnc_commodity_equiv(gnc_price_get_currency(nprice), trans_curr))
                rate = gnc_price_get_value(nprice);
            else
                rate = gnc_numeric_invert(gnc_price_get_value(nprice));

            gnc_numeric value = gnc_numeric_mul (amount, rate, GNC_DENOM_AUTO, GNC_HOW_RND_ROUND);
            xaccSplitSetValue (split, value);
        }
    }

    if (memo)
        xaccSplitSetMemo (split, memo->c_str());
    /* Note, this function assumes the num/action switch is done at a higher level
     * if needed by the book option */
    if (action)
        xaccSplitSetAction (split, action->c_str());

    if (rec_state && *rec_state != ' ')
        xaccSplitSetReconcile (split, *rec_state);
    if (rec_state && *rec_state == YREC)
        xaccSplitSetDateReconciledSecs (split, *rec_date);

}

boost::optional<gnc_numeric> GncPreSplit::create_split (Transaction* trans)
{
    if (created)
        return boost::none;

    /* Gently refuse to create the split if the basics are not set correctly
     * This should have been tested before calling this function though!
     */
    auto check = verify_essentials();
    if (!check.empty())
    {
        PWARN ("Refusing to create split because essentials not set properly: %s", check.c_str());
        return boost::none;
    }

    Account *account = nullptr;
    Account *taccount = nullptr;
    bool amount_set = false;
    gnc_numeric deposit = { 0, 1 };
    gnc_numeric withdrawal = { 0, 1 };
    gnc_numeric amount = { 0, 1 };

    if (m_account)
        account = *m_account;
    if (m_taccount)
        taccount = *m_taccount;
    if (m_deposit)
    {
        deposit = *m_deposit;
        amount_set = true;
    }
    if (m_withdrawal)
    {
        withdrawal = *m_withdrawal;
        amount_set = true;
    }
    if (amount_set)
        amount = gnc_numeric_add (deposit, withdrawal,
                xaccAccountGetCommoditySCU (account),
                GNC_HOW_RND_ROUND_HALF_UP);

    /* Add a split with the cumulative amount value. */
    trans_add_split (trans, account, amount, m_action, m_memo, m_rec_state, m_rec_date, m_price);

    if (taccount)
    {
        /* Note: the current importer assumes at most 2 splits. This means the second split amount
         * will be the negative of the the first split amount.
         */
        auto inv_price = m_price;
        if (inv_price)
            inv_price = gnc_numeric_invert(*inv_price);
        trans_add_split (trans, taccount, gnc_numeric_neg(amount), m_taction, m_tmemo, m_trec_state, m_trec_date, inv_price);
    }


    created = true;

    if (amount_set)
        return boost::none;
    else
        return m_balance;
}

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

#include <glib/gi18n.h>

#include "engine-helpers.h"
#include "gnc-csv-account-map.h"
#include "gnc-ui-util.h"
#include "Account.h"
#include "Transaction.h"

}

#include <string>
#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>
#include "gnc-trans-props.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

/* This map contains a set of strings representing the different column types. */
std::map<GncTransPropType, const char*> gnc_csv_col_type_strs = {
        { GncTransPropType::NONE, N_("None") },
        { GncTransPropType::DATE, N_("Date") },
        { GncTransPropType::NUM, N_("Num") },
        { GncTransPropType::DESCRIPTION, N_("Description") },
        { GncTransPropType::UNIQUE_ID, N_("Transaction ID") },
        { GncTransPropType::NOTES, N_("Notes") },
        { GncTransPropType::ACTION, N_("Action") },
        { GncTransPropType::ACCOUNT, N_("Account") },
        { GncTransPropType::DEPOSIT, N_("Deposit") },
        { GncTransPropType::WITHDRAWAL, N_("Withdrawal") },
        { GncTransPropType::BALANCE, N_("Balance") },
        { GncTransPropType::MEMO, N_("Memo") },
        { GncTransPropType::TACTION, N_("Transfer Action") },
        { GncTransPropType::TACCOUNT, N_("Transfer Account") },
        { GncTransPropType::TMEMO, N_("Transfer Memo") }
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


void GncPreTrans::set_property (GncTransPropType prop_type, const std::string& value)
{
    switch (prop_type)
    {
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

        case GncTransPropType::UNIQUE_ID:
            if (!value.empty())
                m_differ = value;
            else
                m_differ = boost::none;
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

    auto trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, currency);

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

    return (!m_date || m_date == parent->m_date) &&
            (!m_desc || m_desc == parent->m_desc) &&
            (!m_notes || m_notes == parent->m_notes) &&
            (!m_differ || m_differ == parent->m_differ);
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

        default:
            /* Issue a warning for all other prop_types. */
            PWARN ("%d is an invalid property for a split", static_cast<int>(prop_type));
            break;
    }

}

std::string GncPreSplit::verify_essentials (void)
{
    /* Make sure this split has the minimum required set of properties defined. */
    if ((m_deposit == boost::none) &&
        (m_withdrawal == boost::none) &&
        (m_balance == boost::none))
        return _("No balance, deposit, or withdrawal column.");
    else
        return std::string();
}

/** Adds a split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_split (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const std::string& action, const std::string& memo)
{
    auto split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);
    if (!memo.empty())
        xaccSplitSetMemo (split, memo.c_str());
    /* Note, this function assumes the num/action switch is done at a higher level
     * if needed by the book option */
    if (!action.empty())
        xaccSplitSetAction (split, action.c_str());
}

boost::optional<gnc_numeric> GncPreSplit::create_split (Transaction* trans)
{
    if (created)
        return boost::none;

    auto book = xaccTransGetBook (trans);
    std::string action;
    std::string taction;
    std::string memo;
    std::string tmemo;
    Account *account = nullptr;
    Account *taccount = nullptr;
    bool amount_set = false;
    gnc_numeric deposit = { 0, 1 };
    gnc_numeric withdrawal = { 0, 1 };
    gnc_numeric amount = { 0, 1 };

    if (m_taction)
        taction = *m_taction;
    if (m_action)
        action = *m_action;
    if (m_account)
        account = *m_account;
    if (m_taccount)
        taccount = *m_taccount;
    if (m_memo)
        memo = *m_memo;
    if (m_tmemo)
        tmemo = *m_tmemo;
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
    trans_add_split (trans, account, book, amount, action, memo);

    if (taccount)
        /* Note: the current importer assumes at most 2 splits. This means the second split amount
         * will be the negative of the the first split amount.
         */
        trans_add_split (trans, taccount, book, gnc_numeric_neg(amount), taction, tmemo);


    created = true;

    if (amount_set)
        return boost::none;
    else
        return m_balance;
}

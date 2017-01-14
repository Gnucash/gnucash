/********************************************************************\
 * gnc-trans-props.hpp - encapsulate transaction properties for use *
 *                       in the csv importer                        *
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

#ifndef GNC_TRANS_PROPS_HPP
#define GNC_TRANS_PROPS_HPP

extern "C" {
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>

#include "Account.h"
#include "Transaction.h"
}

#include <string>
#include <map>
#include <memory>
#include <boost/optional.hpp>

/** Enumeration for column types. These are the different types of
 * columns that can exist in a CSV/Fixed-Width file. There should be
 * no two columns with the same type except for the GncTransPropType::NONE
 * type. */
enum class GncTransPropType {
    NONE,
    DATE,
    NUM,
    DESCRIPTION,
    NOTES,
    UNIQUE_ID,
    TRANS_PROPS = UNIQUE_ID,

    ACTION,
    ACCOUNT,
    DEPOSIT,
    WITHDRAWAL,
    BALANCE,
    MEMO,
    TACTION,
    TACCOUNT,
    TMEMO,
    SPLIT_PROPS = TMEMO
};

/** Maps all column types to a string representation.
 *  The actual definition is in gnc-csv-imp-trans.cpp.
 *  Attention: that definition should be adjusted for any
 *  changes to enum class GncTransPropType ! */
extern std::map<GncTransPropType, const char*> gnc_csv_col_type_strs;


/** Functor to check if the above map has an element of which
 *  the value equals name. To be used with std::find_if.
 */
struct test_prop_type_str
{
    test_prop_type_str( const char* name ) : m_name(name) {}
    bool operator()( const std::pair<GncTransPropType, const char*>& v ) const
    {
        return !g_strcmp0(v.second, m_name);
    }
private:
    const char *m_name;
};

time64 parse_date (const std::string &date_str, int format);

struct GncPreTrans
{
public:
    void set_property (GncTransPropType prop_type, const std::string& value, int date_format = 0);
    std::string verify_essentials (void);
    Transaction *create_trans (QofBook* book, gnc_commodity* currency);

    /** Check whether the harvested transaction properties for this instance
     *  match those of another one (the "parent"). Note this function is *not*
     *  symmetrical. This instance can have empty properties and still be considered
     *  part of the parent if the other properties match the parent's.
     *  A fully empty instance will will equally be considered part of the parent.
     *
     *  This function is intended to discover multi-split transaction lines in an import
     *  file where the first line defines the transaction (with a first split) and subsequent
     *  lines add splits. These subsequent lines can either have all transaction related
     *  columns be empty or the same as the first line.
     *
     *  @param parent the parent transaction property object to test against
     *  @returns true if this object is considered to be part of the parent, false otherwise.
     */
    bool is_part_of (std::shared_ptr<GncPreTrans> parent);

private:
    boost::optional<time64> m_date;
    boost::optional<std::string> m_num;
    boost::optional<std::string> m_desc;
    boost::optional<std::string> m_notes;
    boost::optional<std::string> m_differ;
    bool created = false;
};

struct GncPreSplit
{
public:
    void set_property (GncTransPropType prop_type, const std::string& value, int currency_format = 0);
    std::string verify_essentials (void);
    boost::optional<gnc_numeric> create_split(Transaction* trans);

    Account* get_account () { if (m_account) return *m_account; else return nullptr; }
    void set_account (Account* acct) { if (acct) m_account = acct; else m_account = boost::none; }

private:
    boost::optional<std::string> m_action;
    boost::optional<Account*> m_account;
    boost::optional<gnc_numeric> m_deposit;
    boost::optional<gnc_numeric> m_withdrawal;
    boost::optional<gnc_numeric> m_balance;
    boost::optional<std::string> m_memo;
    boost::optional<std::string> m_taction;
    boost::optional<Account*> m_taccount;
    boost::optional<std::string> m_tmemo;
    bool created = false;
};

#endif

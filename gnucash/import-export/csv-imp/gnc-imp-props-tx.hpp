/********************************************************************\
 * gnc-imp-props-tx.hpp - encapsulate transaction properties for    *
 *                        use in the csv importer                   *
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
#include "gnc-commodity.h"
}

#include <string>
#include <map>
#include <memory>
#include <boost/optional.hpp>
#include <gnc-datetime.hpp>
#include <gnc-numeric.hpp>

/** Enumeration for column types. These are the different types of
 * columns that can exist in a CSV/Fixed-Width file. There should be
 * no two columns with the same type except for the GncTransPropType::NONE
 * type. */
enum class GncTransPropType {
    NONE,
    UNIQUE_ID,
    DATE,
    NUM,
    DESCRIPTION,
    NOTES,
    COMMODITY,
    VOID_REASON,
    TRANS_PROPS = VOID_REASON,

    ACTION,
    ACCOUNT,
    DEPOSIT,
    WITHDRAWAL,
    PRICE,
    MEMO,
    REC_STATE,
    REC_DATE,
    TACTION,
    TACCOUNT,
    TMEMO,
    TREC_STATE,
    TREC_DATE,
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

/** Some properties only make sense in a multi-split context.
 *  Inversely some also only make sense in a two-split context.
 *  Below function will test a property against a given context
 *  and will return that property if it makes sense
 *  or GncTransPropType::NONE if not.
 */
GncTransPropType sanitize_trans_prop (GncTransPropType prop, bool multi_split);


gnc_commodity* parse_commodity (const std::string& comm_str);
GncNumeric parse_amount (const std::string &str, int currency_format);

struct GncPreTrans
{
public:
    GncPreTrans(int date_format) : m_date_format{date_format} {};

    void set (GncTransPropType prop_type, const std::string& value);
    void set_date_format (int date_format) { m_date_format = date_format ;}
    void reset (GncTransPropType prop_type);
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
    boost::optional<std::string> get_void_reason() { return m_void_reason; }
    std::string errors();

private:
    int m_date_format;
    boost::optional<std::string> m_differ;
    boost::optional<GncDate> m_date;
    boost::optional<std::string> m_num;
    boost::optional<std::string> m_desc;
    boost::optional<std::string> m_notes;
    boost::optional<gnc_commodity*> m_commodity;
    boost::optional<std::string> m_void_reason;
    bool created = false;

    std::map<GncTransPropType, std::string> m_errors;
};

struct GncPreSplit
{
public:
    GncPreSplit (int date_format, int currency_format) : m_date_format{date_format},
        m_currency_format{currency_format}{};
    void set (GncTransPropType prop_type, const std::string& value);
    void reset (GncTransPropType prop_type);
    void set_date_format (int date_format) { m_date_format = date_format ;}
    void set_currency_format (int currency_format) { m_currency_format = currency_format; }
    std::string verify_essentials (void);
    void create_split(Transaction* trans);

    Account* get_account () { if (m_account) return *m_account; else return nullptr; }
    void set_account (Account* acct) { if (acct) m_account = acct; else m_account = boost::none; }
    std::string errors(bool check_accts_mapped);

private:
    int m_date_format;
    int m_currency_format;
    boost::optional<std::string> m_action;
    boost::optional<Account*> m_account;
    boost::optional<GncNumeric> m_deposit;
    boost::optional<GncNumeric> m_withdrawal;
    boost::optional<GncNumeric> m_price;
    boost::optional<std::string> m_memo;
    boost::optional<char> m_rec_state;
    boost::optional<GncDate> m_rec_date;
    boost::optional<std::string> m_taction;
    boost::optional<Account*> m_taccount;
    boost::optional<std::string> m_tmemo;
    boost::optional<char> m_trec_state;
    boost::optional<GncDate> m_trec_date;
    bool created = false;

    std::map<GncTransPropType, std::string> m_errors;
};

#endif

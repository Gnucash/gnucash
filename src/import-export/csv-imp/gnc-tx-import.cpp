/********************************************************************\
 * gnc-tx-import.cpp - import transactions from csv or fixed-width  *
 *                     files                                        *
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

#include <guid.hpp>

extern "C" {
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>

#include "gnc-csv-account-map.h"
#include "gnc-ui-util.h"
#include "engine-helpers.h"

#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <math.h>
}

#include <algorithm>
#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>

#include "gnc-tx-import.hpp"
#include "gnc-csv-tokenizer.hpp"
#include "gnc-fw-tokenizer.hpp"

GQuark
gnc_csv_imp_error_quark (void)
{
  return g_quark_from_static_string ("g-tx-import-error-quark");
}

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

//const int num_date_formats = 5;
//const gchar* date_format_user[] = {N_("y-m-d"),
//                                   N_("d-m-y"),
//                                   N_("m-d-y"),
//                                   N_("d-m"),
//                                   N_("m-d")
//                                  };
//
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
//const int num_currency_formats = 3;
//const gchar* currency_format_user[] = {N_("Locale"),
//                                       N_("Period: 123,456.78"),
//                                       N_("Comma: 123.456,78")
//                                      };
//
/* This map contains a set of strings representing the different column types. */
std::map<GncTransPropType, const char*> gnc_csv_col_type_strs = {
        { GncTransPropType::NONE, N_("None") },
        { GncTransPropType::DATE, N_("Date") },
        { GncTransPropType::NUM, N_("Num") },
        { GncTransPropType::DESCRIPTION, N_("Description") },
        { GncTransPropType::NOTES, N_("Notes") },
        { GncTransPropType::ACCOUNT, N_("Account") },
        { GncTransPropType::DEPOSIT, N_("Deposit") },
        { GncTransPropType::WITHDRAWAL, N_("Withdrawal") },
        { GncTransPropType::BALANCE, N_("Balance") },
        { GncTransPropType::MEMO, N_("Memo") },
        { GncTransPropType::OACCOUNT, N_("Other Account") },
        { GncTransPropType::OMEMO, N_("Other Memo") }
};

/** Parses a string into a date, given a format. This function
 * requires only knowing the order in which the year, month and day
 * appear. For example, 01-02-2003 will be parsed the same way as
 * 01/02/2003.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @exception std::invalid_argument if the string can't be parsed into a date.
 * @return The parsed value of date_str on success, throws on failure
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

/** Constructor for GncTxImport.
 * @return Pointer to a new GncCSvParseData
 */
GncTxImport::GncTxImport(GncImpFileFormat format)
{
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    date_format = -1;
    currency_format = 0;
    start_row = 0;
    end_row = 1000;
    skip_rows = FALSE;
    parse_errors = false;

    file_fmt = format;
    tokenizer = gnc_tokenizer_factory(file_fmt);
}

/** Destructor for GncTxImport.
 */
GncTxImport::~GncTxImport()
{
}

/** Sets the file format for the file to import, which
 *  may cause the file to be reloaded as well if the
 *  previously set file format was different and a
 *  filename was already set.
 *  @param format the new format to set
 *  @exception the reloading of the file may throw std::ifstream::failure
 */
void GncTxImport::file_format(GncImpFileFormat format)
{
    if (file_fmt == format)
        return;

    auto new_encoding = std::string("UTF-8");
    auto new_imp_file = std::string();

    // Recover common settings from old tokenizer
    if (tokenizer)
    {
        new_encoding = tokenizer->encoding();
        new_imp_file = tokenizer->current_file();
    }

    file_fmt = format;
    tokenizer = gnc_tokenizer_factory(file_fmt);

    // Set up new tokenizer with common settings
    // recovered from old tokenizer
    tokenizer->encoding(new_encoding);
    load_file(new_imp_file);
}
GncImpFileFormat GncTxImport::file_format()
{
    return file_fmt;
}

/** Converts raw file data using a new encoding. This function must be
 * called after load_file only if load_file guessed
 * the wrong encoding.
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 * @param error Will point to an error on failure
 * @return 0 on success, 1 on failure
 */
void GncTxImport::convert_encoding (const std::string& encoding)
{
    // TODO investigate if we can catch conversion errors and report them
    if (tokenizer)
        tokenizer->encoding(encoding);
}

/** Loads a file into a GncTxImport. This is the first function
 * that must be called after creating a new GncTxImport. If this
 * fails because the file couldn't be opened, no more functions can be
 * called on the parse data until this succeeds (or until it fails
 * because of an encoding guess error). If it fails because the
 * encoding could not be guessed, gnc_csv_convert_encoding must be
 * called until it succeeds.
 * @param parse_data Data that is being parsed
 * @param filename Name of the file that should be opened
 * @param error Will contain an error if there is a failure
 * @exception may throw std::ifstream::failure on any io error
 * @return 0 on success, 1 on failure
 */
void GncTxImport::load_file (const std::string& filename)
{

    /* Get the raw data first and handle an error if one occurs. */
    try
    {
        tokenizer->load_file (filename);
        return;
    }
    catch (std::ifstream::failure& ios_err)
    {
        // Just log the error and pass it on the call stack for proper handling
        PWARN ("Error: %s", ios_err.what());
        throw;
    }
}

/** Parses a file into cells. This requires having an encoding that
 * works (see gnc_csv_convert_encoding). options should be
 * set according to how the user wants before calling this
 * function. (Note: this function must be called with guessColTypes as
 * TRUE before it is ever called with it as FALSE.) (Note: if
 * guessColTypes is TRUE, all the column types will be GncTransPropType::NONE
 * right now.)
 * @param parse_data Data that is being parsed
 * @param guessColTypes TRUE to guess what the types of columns are based on the cell contents
 * @param error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
int GncTxImport::parse (bool guessColTypes, GError** error)
{
    uint max_cols = 0;
    tokenizer->tokenize();
    orig_lines.clear();
    for (auto tokenized_line : tokenizer->get_tokens())
    {
        orig_lines.push_back (std::make_pair (tokenized_line, std::string()));
        auto length = tokenized_line.size();
        if (length > max_cols)
            max_cols = length;
    }

    /* If it failed, generate an error. */
    if (orig_lines.size() == 0)
    {
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_PARSE, "Parsing failed.");
        return 1;
    }

    if (guessColTypes)
    {
        /* Free column_types if it's already been created. */
        column_types.clear();
    }
    column_types.resize(max_cols, GncTransPropType::NONE);

    if (guessColTypes)
    {
        /* Guess column_types based
         * on the contents of each column. */
        /* TODO Make it actually guess. */
    }
    return 0;
}


/** Convert str into a time64 using the user-specified (import) date format.
 * @param str The string to be parsed
 * @param date_format The date format to use.
 * @return a pointer to a time64 on success, nullptr on failure
 */
static time64* convert_date_col_str (const std::string &str, int date_format)
{
    try
    {
        auto parsed_date = parse_date (str.c_str(), date_format);
        auto mydate = new time64;
        *mydate = parsed_date;
        return mydate;
    }
    catch (std::invalid_argument)
    {
        return nullptr;
    }
}


/** Convert str into a gnc_numeric using the user-specified (import) currency format.
 * @param str The string to be parsed
 * @param currency_format The currency format to use.
 * @return a pointer to a gnc_numeric on success, nullptr on failure
 */
static gnc_numeric* convert_amount_col_str (const std::string &str, int currency_format)
{
    /* If a cell is empty or just spaces return 0 as amount */
    if(!boost::regex_search(str, boost::regex("[0-9]")))
        return nullptr;

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
            return nullptr;
        break;
    case 1:
        /* Currency decimal period */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            return nullptr;
        break;
    case 2:
        /* Currency decimal comma */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
            return nullptr;
        break;
    }

    auto amount = new gnc_numeric;
    *amount = val;
    return amount;
}

/* Define a class hierarchy to temporarily store transaction/split properties
 * found in one import line. There is a generic parent class and an implementation
 * template class. This template class is further specialized for each data type
 * we support (currently time64, Account, string and gnc_numeric).
 */
struct GncTransProperty
{
    virtual ~GncTransProperty()
      //Remove pure designation.
        {}
    bool m_valid = false;
};

template<class T>
struct GncTransPropImpl
: public GncTransProperty
{
public:
    ~GncTransPropImpl(){};
    GncTransPropImpl(const std::string& val, int fmt)
    {
        m_valid = false;
    };

    static GncTransProperty* make_new(const std::string& val,int fmt = 0)
        { return nullptr; }

    T m_value;
};

template<>
struct GncTransPropImpl<time64*>
: public GncTransProperty
{
    GncTransPropImpl(const std::string& val, int fmt)
    {
        m_value = convert_date_col_str (val, fmt);
        m_valid = (m_value != nullptr);
    }
    ~GncTransPropImpl()
        { if (m_value) delete m_value; }

    static std::shared_ptr<GncTransProperty> make_new(const std::string& val,int fmt)
    { return std::shared_ptr<GncTransProperty>(new GncTransPropImpl<time64*>(val, fmt)); }

    time64* m_value;
};


template<>
struct GncTransPropImpl<std::string*>
: public GncTransProperty
{
    GncTransPropImpl(const std::string& val, int fmt = 0)
    {
        m_value = new std::string(val);
        m_valid = (m_value != nullptr);
    }
    ~GncTransPropImpl()
        { if (m_value) delete m_value; }

    static std::shared_ptr<GncTransProperty> make_new(const std::string& val,int fmt = 0)
        { return std::shared_ptr<GncTransProperty>(new GncTransPropImpl<std::string*>(val)); } /* Note fmt is not used for strings */

    std::string* m_value;
};

template<>
struct GncTransPropImpl<Account *>
: public GncTransProperty
{
    GncTransPropImpl(const std::string& val, int fmt = 0)
    {
        m_value = gnc_csv_account_map_search (val.c_str());
        m_valid = (m_value != nullptr);
    }
    GncTransPropImpl(Account* val)
        { m_value = val; }
    ~GncTransPropImpl(){};

    static std::shared_ptr<GncTransProperty> make_new(const std::string& val,int fmt = 0)
        { return std::shared_ptr<GncTransProperty>(new GncTransPropImpl<Account*>(val)); } /* Note fmt is not used in for accounts */

    Account * m_value;
};

template<>
struct GncTransPropImpl<gnc_numeric *>
: public GncTransProperty
{
    GncTransPropImpl(const std::string& val, int fmt)
    {
        m_value = convert_amount_col_str (val, fmt);
        m_valid = (m_value != nullptr);
    }
    ~GncTransPropImpl()
        { if (m_value) delete m_value; }

    static std::shared_ptr<GncTransProperty> make_new(const std::string& val,int fmt)
    { return std::shared_ptr<GncTransProperty>(new GncTransPropImpl<gnc_numeric*>(val, fmt)); }

    gnc_numeric * m_value;
};

/** Adds a split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_split (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const std::string& num, const std::string& memo)
{
    auto split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);
    if (!memo.empty())
        xaccSplitSetMemo (split, memo.c_str());
    /* set tran-num and/or split-action per book option
     * note this function does nothing if num is NULL also */
    if (!num.empty())
        gnc_set_num_action (trans, split, num.c_str(), NULL);
}


/* Shorthand aliases for the container to keep track of property types (a map)
 * and its iterator (a pair)
 */
using prop_pair_t = std::pair<GncTransPropType, std::shared_ptr<GncTransProperty>>;
using prop_map_t = std::map<GncTransPropType, std::shared_ptr<GncTransProperty>>;

/** Tests a TransPropertyList for having enough essential properties.
 * Essential properties are
 * - "Date"
 * - at least one of "Balance", "Deposit", or "Withdrawal"
 * - "Account"
 * Note account isn't checked for here as this has been done before
 * @param list The list we are checking
 * @param error Contains an error message on failure
 * @return true if there are enough essentials; false otherwise
 */
static bool trans_properties_verify_essentials (prop_map_t& trans_props, gchar** error)
{
    /* Make sure this is a transaction with all the columns we need. */
    bool have_date = (trans_props.find (GncTransPropType::DATE) != trans_props.end());
    bool have_amount = ((trans_props.find (GncTransPropType::DEPOSIT) != trans_props.end()) ||
                        (trans_props.find (GncTransPropType::WITHDRAWAL) != trans_props.end()) ||
                        (trans_props.find (GncTransPropType::BALANCE) != trans_props.end()));

    std::string error_message {""};
    if (!have_date)
        error_message += N_("No date column.");
    if (!have_amount)
    {
        if (!have_date)
            error_message += "\n";
        error_message += N_("No balance, deposit, or withdrawal column.");
    }
    if (!have_date || !have_amount)
        *error = g_strdup (error_message.c_str());

    return have_amount && have_date;
}

/** Create a Transaction from a map of transaction properties.
 * Note: this function assumes all properties in the map have been verified
 *       to be valid. No further checks are performed here other than that
 *       the required properties are in the map
 * @param transprops The map of transaction properties
 * @param error Contains an error on failure
 * @return On success, a GncCsvTransLine; on failure, the trans pointer is NULL
 */
static GncCsvTransLine* trans_properties_to_trans (prop_map_t& trans_props, gchar** error)
{

    if (!trans_properties_verify_essentials(trans_props, error))
        return NULL;

    auto property = trans_props.find (GncTransPropType::ACCOUNT)->second;
    auto account = dynamic_cast<GncTransPropImpl<Account*>*>(property.get())->m_value;

    GncCsvTransLine* trans_line = g_new (GncCsvTransLine, 1);

    /* The balance is 0 by default. */
    trans_line->balance_set = false;
    trans_line->balance = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (account),
                          GNC_HOW_RND_ROUND_HALF_UP);

    /* We make the line_no -1 just to mark that it hasn't been set. We
     * may get rid of line_no soon anyway, so it's not particularly
     * important. */
    trans_line->line_no = -1;

    QofBook* book = gnc_account_get_book (account);
    gnc_commodity* currency = xaccAccountGetCommodity (account);
    trans_line->trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (trans_line->trans);
    xaccTransSetCurrency (trans_line->trans, currency);

    /* Go through each of the properties and edit the transaction accordingly. */
    std::string num;
    std::string memo;
    std::string omemo;
    Account *oaccount = NULL;
    bool amount_set = false;
    gnc_numeric amount = trans_line->balance;

    for (auto prop_pair : trans_props)
    {
        auto type = prop_pair.first;
        auto prop = prop_pair.second;
        switch (type)
        {
        case GncTransPropType::DATE:
            {
                auto transdate = dynamic_cast<GncTransPropImpl<time64*>*>(prop.get())->m_value;
                xaccTransSetDatePostedSecsNormalized (trans_line->trans, *transdate);
            }
            break;

        case GncTransPropType::DESCRIPTION:
            {
                auto propstring = dynamic_cast<GncTransPropImpl<std::string*>*>(prop.get())->m_value;
                xaccTransSetDescription (trans_line->trans, propstring->c_str());
            }
            break;

        case GncTransPropType::NOTES:
            {
                auto propstring = dynamic_cast<GncTransPropImpl<std::string*>*>(prop.get())->m_value;
                xaccTransSetNotes (trans_line->trans, propstring->c_str());
            }
            break;

        case GncTransPropType::OACCOUNT:
            oaccount = dynamic_cast<GncTransPropImpl<Account*>*>(prop.get())->m_value;
            break;

        case GncTransPropType::MEMO:
            memo = *dynamic_cast<GncTransPropImpl<std::string*>*>(prop.get())->m_value;
            break;

        case GncTransPropType::OMEMO:
            omemo = *dynamic_cast<GncTransPropImpl<std::string*>*>(prop.get())->m_value;
            break;

        case GncTransPropType::NUM:
            /* the 'num' is saved and passed to 'trans_add_split' below where
             * 'gnc_set_num_action' is used to set tran-num and/or split-action
             * per book option */
            num = *dynamic_cast<GncTransPropImpl<std::string*>*>(prop.get())->m_value;
            break;

        case GncTransPropType::DEPOSIT: /* Add deposits to the existing amount. */
            {
                auto propval = dynamic_cast<GncTransPropImpl<gnc_numeric*>*>(prop.get())->m_value;
                amount = gnc_numeric_add (*propval,
                                         amount,
                                         xaccAccountGetCommoditySCU (account),
                                         GNC_HOW_RND_ROUND_HALF_UP);
                amount_set = true;
                /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
                trans_line->balance_set = false;
            }
            break;

        case GncTransPropType::WITHDRAWAL: /* Withdrawals are just negative deposits. */
            {
                auto propval = dynamic_cast<GncTransPropImpl<gnc_numeric*>*>(prop.get())->m_value;
                amount = gnc_numeric_add (gnc_numeric_neg(*propval),
                                         amount,
                                         xaccAccountGetCommoditySCU (account),
                                         GNC_HOW_RND_ROUND_HALF_UP);
                amount_set = true;
                /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
                trans_line->balance_set = false;
            }
            break;

        case GncTransPropType::BALANCE: /* The balance gets stored in a separate field in trans_line. */
            /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
            if (!amount_set)
            {
                auto propval = dynamic_cast<GncTransPropImpl<gnc_numeric*>*>(prop.get())->m_value;
                /* This gets put into the actual transaction at the end of gnc_csv_parse_to_trans. */
                trans_line->balance = *propval;
                trans_line->balance_set = true;
            }
            break;
        default:
            break;
        }
    }

    /* Add a split with the cumulative amount value. */
    trans_add_split (trans_line->trans, account, book, amount, num, memo);

    if (oaccount)
        /* Note: the current importer assumes at most 2 splits. This means the second split amount
         * will be the negative of the the first split amount. We also only set the num field once,
         * for the first split.
         */
        trans_add_split (trans_line->trans, oaccount, book, gnc_numeric_neg(amount), "", omemo);

    return trans_line;
}

/** Creates a list of transactions from parsed data. Transactions that
 * could be created from rows are placed in transactions;
 * rows that fail are placed in error_lines. (Note: there
 * is no way for this function to "fail," i.e. it only returns 0, so
 * it may be changed to a void function in the future.)
 * @param parse_data Data that is being parsed
 * @param account Account with which transactions are created
 * @param redo_errors TRUE to convert only error data, FALSE for all data
 * @return 0 on success, 1 on failure
 */
int GncTxImport::parse_to_trans (Account* account,
                                     bool redo_errors)
{
    /* Free error_lines and transactions if they
     * already exist. */
    if (!redo_errors) /* If we're redoing errors, we save freeing until the end. */
    {
        for (auto orig_line : orig_lines)
            orig_line.second.clear();

        /* FIXME handle memory leak here!
         * Existing transactions in the map should probably removed before emptying the map
         */
        if (!transactions.empty())
            transactions.clear();
    }

    /* compute start and end iterators based on user-set restrictions */
    auto orig_lines_it = orig_lines.begin();
    std::advance(orig_lines_it, start_row);

    auto orig_lines_max = orig_lines.begin();
    if (end_row > orig_lines.size())
        orig_lines_max = orig_lines.end();
    else
        std::advance(orig_lines_max, end_row);

    Account *home_account = NULL;
    auto odd_line = false;
    parse_errors = false;
    for (orig_lines_it, odd_line;
            orig_lines_it != orig_lines_max;
            ++orig_lines_it, odd_line = !odd_line)
    {
        prop_map_t trans_props;

        /* Skip current line if:
           1. only looking for lines with error AND no error on current line
           OR
           2. looking for all lines AND
              skip_rows is enabled AND
              current line is an odd line */
        if ((redo_errors && orig_lines_it->second.empty()) ||
           (!redo_errors && skip_rows && odd_line))
            continue;

        auto line = orig_lines_it->first;
        GncCsvTransLine* trans_line = NULL;

        /* Convert this import line into a map of transaction/split properties. */
        auto loop_err = false;
        auto col_types_it = column_types.cbegin();
        auto line_it = line.cbegin();
        for (col_types_it, line_it;
                col_types_it != column_types.cend() &&
                line_it != line.cend();
                ++col_types_it, ++line_it)
        {
            std::shared_ptr<GncTransProperty> property;
            switch (*col_types_it)
            {
                case GncTransPropType::DATE:
                    property = GncTransPropImpl<time64*>::make_new (*line_it, date_format);
                break;

                case GncTransPropType::DESCRIPTION:
                case GncTransPropType::NOTES:
                case GncTransPropType::MEMO:
                case GncTransPropType::OMEMO:
                case GncTransPropType::NUM:
                    property = GncTransPropImpl<std::string*>::make_new (*line_it);
                    break;

                case GncTransPropType::ACCOUNT:
                case GncTransPropType::OACCOUNT:
                    property = GncTransPropImpl<Account*>::make_new (*line_it);
                    if (*col_types_it == GncTransPropType::ACCOUNT)
                        home_account = dynamic_cast<GncTransPropImpl<Account*>*>(property.get())->m_value;
                    break;

                case GncTransPropType::BALANCE:
                case GncTransPropType::DEPOSIT:
                case GncTransPropType::WITHDRAWAL:
                    property = GncTransPropImpl<gnc_numeric*>::make_new (*line_it, currency_format);
                    break;

                default:
                    continue; /* We do nothing with "None"-type columns. */
                    break;
            }

            if (property->m_valid)
                trans_props.insert(prop_pair_t(*col_types_it, property));
            else
            {
                parse_errors = loop_err = true;
                std::string error_message {_(gnc_csv_col_type_strs[*col_types_it])};
                error_message += _(" column could not be understood.");
                orig_lines_it->second = std::move(error_message);
                break;
            }
        }

        if (loop_err)
            continue;

        // Add an ACCOUNT property with the default account if no account column was set by the user
        if (std::find (column_types.begin(), column_types.end(), GncTransPropType::ACCOUNT) == column_types.end())
        {
            // If there is no ACCOUNT property by now, try to use the default account passed in
            if (account)
            {
                auto property = std::shared_ptr<GncTransProperty>(new GncTransPropImpl<Account*>(account));
                trans_props.insert(prop_pair_t(GncTransPropType::ACCOUNT, property));
                home_account = account;
            }
            else
            {
                // Oops - the user didn't select an Account column *and* we didn't get a default value either!
                // Note if you get here this suggests a bug in the code!
                parse_errors = true;
                orig_lines_it->second = _("No account column selected and no default account specified either.");
                continue;
            }
        }

        /* If column parsing was successful, convert trans properties into a trans line. */
        gchar *error_message = NULL;
        trans_line = trans_properties_to_trans (trans_props, &error_message);
        if (trans_line == NULL)
        {
            parse_errors = true;
            orig_lines_it->second = error_message;
            g_free (error_message);
            continue;
        }

        /* If all went well, add this transaction to the list. */
        /* We want to keep the transactions sorted by date in case we have
         * to calculate the transaction's amount based on the user provided balances.
         * The multimap should deal with this for us. */
        auto trans_date = xaccTransGetDate (trans_line->trans);
        transactions.insert (std::pair<time64, GncCsvTransLine*>(trans_date,trans_line));
    }

    if (std::find(column_types.begin(),column_types.end(), GncTransPropType::BALANCE) !=
        column_types.end()) // This is only used if we have one home account
    {
        Split      *split, *osplit;

        if (account != NULL)
            home_account = account;

        /* balance_offset is how much the balance currently in the account
         * differs from what it will be after the transactions are
         * imported. This will be sum of all the previous transactions for
         * any given transaction. */
        auto balance_offset = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (home_account),
                                     GNC_HOW_RND_ROUND_HALF_UP);
        for (auto trans_iter : transactions)
        {
            auto trans_line = trans_iter.second;
            if (trans_line->balance_set)
            {
                time64 date = xaccTransGetDate (trans_line->trans);
                /* Find what the balance should be by adding the offset to the actual balance. */
                gnc_numeric existing_balance = gnc_numeric_add (balance_offset,
                                               xaccAccountGetBalanceAsOfDate (home_account, date),
                                               xaccAccountGetCommoditySCU (home_account),
                                               GNC_HOW_RND_ROUND_HALF_UP);

                /* The amount of the transaction is the difference between the new and existing balance. */
                gnc_numeric amount = gnc_numeric_sub (trans_line->balance,
                                                     existing_balance,
                                                     xaccAccountGetCommoditySCU (home_account),
                                                     GNC_HOW_RND_ROUND_HALF_UP);

                // Find home account split
                split  = xaccTransFindSplitByAccount (trans_line->trans, home_account);
                xaccSplitSetAmount (split, amount);
                xaccSplitSetValue (split, amount);

                // If we have two splits, change other side
                if (xaccTransCountSplits (trans_line->trans) == 2)
                {
                    osplit = xaccSplitGetOtherSplit (split);
                    xaccSplitSetAmount (split, amount);
                    xaccSplitSetValue (split, gnc_numeric_neg (amount));
                }

                /* This new transaction needs to be added to the balance offset. */
                balance_offset = gnc_numeric_add (balance_offset,
                                                 amount,
                                                 xaccAccountGetCommoditySCU (home_account),
                                                 GNC_HOW_RND_ROUND_HALF_UP);
            }
        }
    }

    return 0;
}


bool
GncTxImport::check_for_column_type (GncTransPropType type)
{
    return (std::find (column_types.begin(), column_types.end(), type) != column_types.end());
}

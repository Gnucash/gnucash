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
#include <regex.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <math.h>
}

#include <algorithm>
#include <boost/regex.hpp>

#include "gnc-csv-imp-trans.hpp"
#include "gnc-csv-tokenizer.hpp"
#include "gnc-fw-tokenizer.hpp"

GQuark
gnc_csv_imp_error_quark (void)
{
  return g_quark_from_static_string ("g-csv-imp-error-quark");
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
 * @return The parsed value of date_str on success or -1 on failure
 */
time64 parse_date (const std::string &date_str, int format)
{
    time64 rawtime; /* The integer time */
    struct tm retvalue, test_retvalue; /* The time in a broken-down structure */
    int orig_year = -1, orig_month = -1, orig_day = -1;

    boost::regex r(date_regex[format]);
    boost::smatch what;
    if(!boost::regex_search(date_str.cbegin(), date_str.cend(), what, r))
        return -1;  // regex didn't find a match

    // xxx Different behavior from 2.6.x series !
    // If date format without year was selected, the match
    // should NOT have found a year.
    if ((format >= 3) && (what.length("YEAR") != 0))
        return -1;


    /* Put some sane values in retvalue by using a fixed time for
     * the non-year-month-day parts of the date. */
    gnc_time (&rawtime);
    gnc_localtime_r (&rawtime, &retvalue);
    retvalue.tm_hour = 11;
    retvalue.tm_min = 0;
    retvalue.tm_sec = 0;
    retvalue.tm_isdst = -1;

    retvalue.tm_mday = std::stoi (what.str("DAY"));
    retvalue.tm_mon = std::stoi (what.str("MONTH")) - 1;

    if (format < 3)
    {
        retvalue.tm_year = std::stoi (what.str("YEAR"));

        /* Handle two-digit years. */
        if (retvalue.tm_year < 100)
        {
            /* We allow two-digit years in the range 1969 - 2068. */
            if (retvalue.tm_year < 69)
                retvalue.tm_year += 100;
        }
        else
            retvalue.tm_year -= 1900;
    }

    /* Convert back to an integer. If gnc_mktime leaves retvalue unchanged,
     * everything is okay; otherwise, an error has occurred. */
    /* We have to use a "test" date value to account for changes in
     * daylight savings time, which can cause a date change with gnc_mktime
     * near midnight, causing the code to incorrectly think a date is
     * incorrect. */

    orig_day   = retvalue.tm_mday;
    orig_month = retvalue.tm_mon;
    orig_year  = retvalue.tm_year;

    test_retvalue = retvalue;
    gnc_mktime (&test_retvalue);
    retvalue.tm_isdst = test_retvalue.tm_isdst;
    rawtime = gnc_mktime (&retvalue);
    if (retvalue.tm_mday == orig_day &&
            retvalue.tm_mon == orig_month &&
            retvalue.tm_year == orig_year)
        return rawtime;
    else
        return -1;
}

/** Constructor for GncCsvParseData.
 * @return Pointer to a new GncCSvParseData
 */
GncCsvParseData::GncCsvParseData(GncImpFileFormat format)
{
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    transactions = NULL;
    date_format = -1;
    currency_format = 0;
    start_row = 0;
    end_row = 1000;
    skip_rows = FALSE;
    parse_errors = false;

    file_fmt = format;
    tokenizer = GncTokenizerFactory(file_fmt);
}

/** Destructor for GncCsvParseData.
 */
GncCsvParseData::~GncCsvParseData()
{
    /* All non-NULL pointers have been initialized and must be freed. */

    if (transactions != NULL)
        g_list_free_full (transactions, g_free);
}

int GncCsvParseData::file_format(GncImpFileFormat format,
                                  GError** error)
{
    if (file_fmt == format)
        return 0;

    std::string new_encoding = "UTF-8";
    std::string new_imp_file;

    // Recover common settings from old tokenizer
    if (tokenizer)
    {
        new_encoding = tokenizer->encoding();
        new_imp_file = tokenizer->current_file();
    }

    file_fmt = format;
    tokenizer = GncTokenizerFactory(file_fmt);

    // Set up new tokenizer with common settings
    // recovered from old tokenizer
    tokenizer->encoding(new_encoding);
    return load_file(new_imp_file.c_str(), error);
}
GncImpFileFormat GncCsvParseData::file_format()
{
    return file_fmt;
}

/** Converts raw file data using a new encoding. This function must be
 * called after gnc_csv_load_file only if gnc_csv_load_file guessed
 * the wrong encoding.
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 * @param error Will point to an error on failure
 * @return 0 on success, 1 on failure
 */
void GncCsvParseData::convert_encoding (const std::string& encoding)
{
    // TODO investigate if we can catch conversion errors and report them
    if (tokenizer)
        tokenizer->encoding(encoding);
}

/** Loads a file into a GncCsvParseData. This is the first function
 * that must be called after creating a new GncCsvParseData. If this
 * fails because the file couldn't be opened, no more functions can be
 * called on the parse data until this succeeds (or until it fails
 * because of an encoding guess error). If it fails because the
 * encoding could not be guessed, gnc_csv_convert_encoding must be
 * called until it succeeds.
 * @param parse_data Data that is being parsed
 * @param filename Name of the file that should be opened
 * @param error Will contain an error if there is a failure
 * @return 0 on success, 1 on failure
 */
int GncCsvParseData::load_file (const char* filename,
                                GError** error)
{

    /* Get the raw data first and handle an error if one occurs. */
    try
    {
        tokenizer->load_file (filename);
        return 0;
    }
    catch (std::ifstream::failure& ios_err)
    {
        /* TODO Handle file opening errors more specifically,
         * e.g. inexistent file versus no read permission. */
        PWARN ("Error: %s", ios_err.what());
        g_set_error (error, GNC_CSV_IMP_ERROR, GNC_CSV_IMP_ERROR_OPEN, "%s", _("File opening failed."));
        return 1;
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
int GncCsvParseData::parse (gboolean guessColTypes, GError** error)
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
static time64* convert_date_col_str (const char* str, int date_format)
{
    auto parsed_date = parse_date(str, date_format);
    if (parsed_date == -1)
        return nullptr;
    else
    {
        auto mydate = new time64;
        *mydate = parsed_date;
        return mydate;
    }
}


/** Convert str into a gnc_numeric using the user-specified (import) currency format.
 * @param str The string to be parsed
 * @param currency_format The currency format to use.
 * @return a pointer to a gnc_numeric on success, nullptr on failure
 */
static gnc_numeric* convert_amount_col_str (const char* str, int currency_format)
{
    auto str_dupe = g_strdup (str); /* First, we make a copy so we can't mess up real data. */
    /* If a cell is empty or just spaces make its value = "0" */
    regex_t regex;
    int reti = regcomp(&regex, "[0-9]", 0);
    reti = regexec(&regex, str_dupe, 0, NULL, 0);
    if (reti == REG_NOMATCH)
    {
        g_free (str_dupe);
        str_dupe = g_strdup ("0");
    }
    /* Go through str_dupe looking for currency symbols. */
    for (auto possible_currency_symbol = str_dupe; *possible_currency_symbol;
            possible_currency_symbol = g_utf8_next_char (possible_currency_symbol))
    {
        if (g_unichar_type (g_utf8_get_char (possible_currency_symbol)) == G_UNICODE_CURRENCY_SYMBOL)
        {
            /* If we find a currency symbol, save the position just ahead
             * of the currency symbol (next_symbol), and find the null
             * terminator of the string (last_symbol). */
            char *next_symbol = g_utf8_next_char (possible_currency_symbol), *last_symbol = next_symbol;
            while (*last_symbol)
                last_symbol = g_utf8_next_char (last_symbol);

            /* Move all of the string (including the null byte, which is
             * why we have +1 in the size parameter) following the
             * currency symbol back one character, thereby overwriting the
             * currency symbol. */
            memmove (possible_currency_symbol, next_symbol, last_symbol - next_symbol + 1);
            break;
        }
    }

    /* Currency format */
    gnc_numeric val;
    char *endptr;
    switch (currency_format)
    {
    case 0:
        /* Currency locale */
        if (!(xaccParseAmount (str_dupe, TRUE, &val, &endptr)))
        {
            g_free (str_dupe);
            return nullptr;
        }
        break;
    case 1:
        /* Currency decimal period */
        if (!(xaccParseAmountExtended (str_dupe, TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
        {
            g_free (str_dupe);
            return nullptr;
        }
        break;
    case 2:
        /* Currency decimal comma */
        if (!(xaccParseAmountExtended (str_dupe, TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
        {
            g_free (str_dupe);
            return nullptr;
        }
        break;
    }

    g_free (str_dupe);
    auto amount = new gnc_numeric;
    *amount = val;
    return amount;
}

/** Adds a split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_split (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const char *num, const char *memo)
{
    Split* split = xaccMallocSplit (book);
    xaccSplitSetAccount (split, account);
    xaccSplitSetParent (split, trans);
    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);
    xaccSplitSetMemo (split, memo);
    /* set tran-num and/or split-action per book option */
    gnc_set_num_action (trans, split, num, NULL);
}

/** Adds a other split to a transaction.
 * @param trans The transaction to add a split to
 * @param account The account used for the other split
 * @param book The book where the split should be stored
 * @param amount The amount of the split
 */
static void trans_add_osplit (Transaction* trans, Account* account, QofBook* book,
                            gnc_numeric amount, const char *num, const char *memo)
{
    Split *osplit = xaccMallocSplit (book);
    xaccSplitSetAccount (osplit, account);
    xaccSplitSetParent (osplit, trans);
    xaccSplitSetAmount (osplit, amount);
    xaccSplitSetValue (osplit, gnc_numeric_neg (amount));
    xaccSplitSetMemo (osplit, memo);
}

using prop_pair_t = std::pair<GncTransPropType, void*>;
using prop_map_t = std::map<GncTransPropType, void*>;
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

/** Create a Transaction from a TransPropertyList.
 * @param list The list of properties
 * @param error Contains an error on failure
 * @return On success, a GncCsvTransLine; on failure, the trans pointer is NULL
 */
static GncCsvTransLine* trans_properties_to_trans (prop_map_t& trans_props, gchar** error)
{

    if (!trans_properties_verify_essentials(trans_props, error))
        return NULL;

    auto property = trans_props.find (GncTransPropType::ACCOUNT)->second;
    Account* account = static_cast<Account*> (property);

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
    gchar *num = NULL;
    gchar *memo = NULL;
    gchar *omemo = NULL;
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
            xaccTransSetDatePostedSecsNormalized (trans_line->trans, *((time64*)(prop)));
            break;

        case GncTransPropType::DESCRIPTION:
            xaccTransSetDescription (trans_line->trans, (char*)(prop));
            break;

        case GncTransPropType::NOTES:
            xaccTransSetNotes (trans_line->trans, (char*)(prop));
            break;

        case GncTransPropType::OACCOUNT:
            oaccount = ((Account*)(prop));
            break;

        case GncTransPropType::MEMO:
            memo = g_strdup ((char*)(prop));
            break;

        case GncTransPropType::OMEMO:
            omemo = g_strdup ((char*)(prop));
            break;

        case GncTransPropType::NUM:
            /* the 'num' is saved and passed to 'trans_add_split' below where
             * 'gnc_set_num_action' is used to set tran-num and/or split-action
             * per book option */
            num = g_strdup ((char*)(prop));
            break;

        case GncTransPropType::DEPOSIT: /* Add deposits to the existing amount. */
            if (prop != NULL)
            {
                amount = gnc_numeric_add (*((gnc_numeric*)(prop)),
                                         amount,
                                         xaccAccountGetCommoditySCU (account),
                                         GNC_HOW_RND_ROUND_HALF_UP);
                amount_set = true;
                /* We will use the "Deposit" and "Withdrawal" columns in preference to "Balance". */
                trans_line->balance_set = false;
            }
            break;

        case GncTransPropType::WITHDRAWAL: /* Withdrawals are just negative deposits. */
            if (prop != NULL)
            {
                amount = gnc_numeric_add (gnc_numeric_neg(*((gnc_numeric*)(prop))),
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
            if (!amount_set && prop != NULL)
            {
                /* This gets put into the actual transaction at the end of gnc_csv_parse_to_trans. */
                trans_line->balance = *((gnc_numeric*)(prop));
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
        trans_add_osplit (trans_line->trans, oaccount, book, amount, num, omemo);

    if (num)
        g_free (num);
    if (memo)
        g_free (memo);
    if (omemo)
        g_free (omemo);

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
int GncCsvParseData::parse_to_trans (Account* account,
                                     gboolean redo_errors)
{
    /* Free error_lines and transactions if they
     * already exist. */
    if (!redo_errors) /* If we're redoing errors, we save freeing until the end. */
    {
        for (auto orig_line : orig_lines)
            orig_line.second.clear();

        if (transactions != NULL)
            g_list_free (transactions);
    }

    /* last_transaction points to the last element in
     * transactions, or NULL if it's empty. */
    GList* last_transaction = NULL;
    if (redo_errors) /* If we're looking only at error data ... */
    {
        if (transactions == NULL)
            last_transaction = NULL;
        else
        {
            /* Move last_transaction to the end. */
            last_transaction = transactions;
            while (g_list_next (last_transaction) != NULL)
            {
                last_transaction = g_list_next (last_transaction);
            }
        }
    }
    else /* Otherwise, we look at all the data. */
    {
        last_transaction = NULL;
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
    bool odd_line = false;
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

        // If account = NULL, we should have an Account column
        home_account = account;
        if (home_account == NULL)
        {
            for (uint j = 0; j < line.size(); j++)
            {
                /* Look for "Account" columns. */
                if (column_types[j] == GncTransPropType::ACCOUNT)
                    home_account = gnc_csv_account_map_search (line[j].c_str());
            }
        }

        if (home_account == NULL)
        {
            parse_errors = true;
            orig_lines_it->second = _("Account column could not be understood.");
            continue;
        }

        /* Affect the transaction appropriately. */
        bool loop_err = false;
        for (uint j = 0; j < line.size(); j++)
        {
            void* property;
            switch (column_types[j])
            {
                case GncTransPropType::DATE:
                    property = static_cast<void*> (convert_date_col_str (line[j].c_str(), date_format));
                break;

                case GncTransPropType::DESCRIPTION:
                case GncTransPropType::NOTES:
                case GncTransPropType::MEMO:
                case GncTransPropType::OMEMO:
                case GncTransPropType::NUM:
                    property = static_cast<void*> (g_strdup (line[j].c_str()));
                    break;

                case GncTransPropType::ACCOUNT:
                case GncTransPropType::OACCOUNT:
                    property = static_cast<void*> (gnc_csv_account_map_search (line[j].c_str()));
                    break;

                case GncTransPropType::BALANCE:
                case GncTransPropType::DEPOSIT:
                case GncTransPropType::WITHDRAWAL:
                    property = static_cast<void*> (convert_amount_col_str (line[j].c_str(), currency_format));
                    break;

                default:
                    continue; /* We do nothing with "None"-type columns. */
                    break;
            }

            if (property)
                trans_props.insert(prop_pair_t(column_types[j], property));
            else
            {
                parse_errors = loop_err = true;
                gchar *error_message = g_strdup_printf (_("%s column could not be understood."),
                                                _(gnc_csv_col_type_strs[column_types[j]]));
                orig_lines_it->second = error_message;

                g_free (error_message);
                break;
            }
        }
        // Add an ACCOUNT property with the home_account if no account column was set by the user
        if (std::find (column_types.begin(), column_types.end(), GncTransPropType::ACCOUNT) == column_types.end())
            trans_props.insert(prop_pair_t(GncTransPropType::ACCOUNT, static_cast<void*> (home_account)));

        if (loop_err)
            /* FIXME huge memory leak here: the trans_props for this line aren't freed! */
            continue;

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
        /* We keep the transactions sorted by date. We start at the end
         * of the list and go backward, simply because the file itself
         * is probably also sorted by date (but we need to handle the
         * exception anyway). */

        /* If we can just put it at the end, do so and increment last_transaction. */
        if (last_transaction == NULL ||
                xaccTransGetDate (((GncCsvTransLine*)(last_transaction->data))->trans) <= xaccTransGetDate (trans_line->trans))
        {
            transactions = g_list_append (transactions, trans_line);
            /* If this is the first transaction, we need to get last_transaction on track. */
            if (last_transaction == NULL)
                last_transaction = transactions;
            else /* Otherwise, we can just continue. */
                last_transaction = g_list_next (last_transaction);
        }
        /* Otherwise, search backward for the correct spot. */
        else
        {
            GList* insertion_spot = last_transaction;
            while (insertion_spot != NULL &&
                    xaccTransGetDate (((GncCsvTransLine*)(insertion_spot->data))->trans) > xaccTransGetDate (trans_line->trans))
            {
                insertion_spot = g_list_previous (insertion_spot);
            }
            /* Move insertion_spot one location forward since we have to
             * use the g_list_insert_before function. */
            if (insertion_spot == NULL) /* We need to handle the case of inserting at the beginning of the list. */
                insertion_spot = transactions;
            else
                insertion_spot = g_list_next (insertion_spot);

            transactions = g_list_insert_before (transactions, insertion_spot, trans_line);
        }
    }

    if (std::find(column_types.begin(),column_types.end(), GncTransPropType::BALANCE) !=
        column_types.end()) // This is only used if we have one home account
    {
        Split      *split, *osplit;
        gnc_numeric balance_offset;
        GList* tx_iter = transactions;

        if (account != NULL)
            home_account = account;

        /* balance_offset is how much the balance currently in the account
         * differs from what it will be after the transactions are
         * imported. This will be sum of all the previous transactions for
         * any given transaction. */
        balance_offset = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (home_account),
                                     GNC_HOW_RND_ROUND_HALF_UP);
        while (tx_iter != NULL)
        {
            GncCsvTransLine* trans_line = (GncCsvTransLine*)tx_iter->data;
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
            tx_iter = g_list_next (tx_iter);
        }
    }

    return 0;
}


bool
GncCsvParseData::check_for_column_type (GncTransPropType type)
{
    return (std::find (column_types.begin(), column_types.end(), type) != column_types.end());
}

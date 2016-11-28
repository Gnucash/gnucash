/********************************************************************\
 * gnc-tx-import.hpp - import transactions from csv files       *
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

/** @file
     @brief Class to import transactions from CSV or fixed width files
     *
     gnc-tx-import.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_TX_IMPORT_HPP
#define GNC_TX_IMPORT_HPP

extern "C" {
#include "config.h"

#include "Account.h"
#include "Transaction.h"
}

#include <vector>
#include <map>
#include <memory>

#include "gnc-tokenizer.hpp"


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
    ACCOUNT,
    DEPOSIT,
    WITHDRAWAL,
    BALANCE,
    MEMO,
    OACCOUNT,
    OMEMO
};

/** Maps all column types to a string representation.
 *  The actual definition is in gnc-tx-import.cpp.
 *  Attention: that definition should be adjusted for any
 *  changes to enum class GncTransPropType ! */
extern std::map<GncTransPropType, const char*> gnc_csv_col_type_strs;

/* TODO We now sort transactions by date, not line number, so we
 * should probably get rid of this struct and uses of it. */

/** Struct pairing a transaction with a line number. This struct is
 * used to keep the transactions in order. When rows are separated
 * into "valid" and "error" lists (in case some of the rows have cells
 * that are unparseable), we want the user to still be able to
 * "correct" the error list. If we keep the line numbers of valid
 * transactions, we can then put transactions created from the newly
 * corrected rows into the right places. */
struct GncCsvTransLine
{
    Transaction* trans;
    gnc_numeric balance;  /**< The (supposed) balance after this transaction takes place */
    bool balance_set;     /**< true if balance has been set from user data, false otherwise */
};

/* A set of currency formats that the user sees. */
extern const int num_currency_formats;
extern const gchar* currency_format_user[];

/* A set of date formats that the user sees. */
extern const int num_date_formats;
extern const gchar* date_format_user[];

/** Tuple to hold a tokenized line of input and an optional error string */
using parse_line_t = std::tuple<StrVec, std::string>;

/** The actual TxImport class
 * It's intended to use in the following sequence of actions:
 * - set a file format
 * - load a file
 * - optionally convert it's encoding
 * - parse the file into lines, which in turn are split up in columns
 *   the result of this step can be queried from tokenizer
 * - the user should now map the columns to types, which is stored in column_types
 * - last step is convert the mapped columns into a list of transactions
 * - this list will then be passed on the the generic importer for further processing */
class GncTxImport
{
public:
    // Constructor - Destructor
    GncTxImport(GncImpFileFormat format = GncImpFileFormat::UNKNOWN);
    ~GncTxImport();

    void file_format(GncImpFileFormat format);
    GncImpFileFormat file_format();

    void load_file (const std::string& filename);
    void convert_encoding (const std::string& encoding);

    void parse (bool guessColTypes);
    int parse_to_trans (Account* account, bool redo_errors);
    bool check_for_column_type (GncTransPropType type);

    std::unique_ptr<GncTokenizer> tokenizer;    /**< Will handle file loading/encoding conversion/splitting into fields */
    std::vector<parse_line_t> orig_lines;      /**< file_str parsed into a two-dimensional array of strings */
    std::vector<GncTransPropType> column_types;       /**< Vector of values from the GncCsvColumnType enumeration */
    std::multimap <time64, GncCsvTransLine*> transactions;        /**< List of GncCsvTransLine*s created using orig_lines and column_types */
    int date_format;            /**< The format of the text in the date columns from date_format_internal. */
    guint start_row;            /**< The start row to generate transactions from. */
    guint end_row;              /**< The end row to generate transactions from. */
    gboolean skip_rows;         /**< Skip Alternate Rows from start row. */
    int currency_format;        /**< The currency format, 0 for locale, 1 for comma dec and 2 for period */
    bool parse_errors;          /**< Indicates whether the last parse_to_trans run had any errors */

private:
    void adjust_balances (Account *account);

    GncImpFileFormat file_fmt = GncImpFileFormat::UNKNOWN;
};

time64 parse_date (const std::string &date_str, int format);

#endif

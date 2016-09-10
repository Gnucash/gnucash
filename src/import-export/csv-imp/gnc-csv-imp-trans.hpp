/********************************************************************\
 * gnc-csv-imp-trans.hpp - import transactions from csv files       *
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
     @brief Class to import transactions from CSV files
     *
     gnc-csv-imp-trans.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_CSV_IMP_TRANS_HPP
#define GNC_CSV_IMP_TRANS_HPP

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
 *  The actual definition is in gnc-csv-imp-trans.cpp.
 *  Attention: that definition should be adjusted for any
 *  changes to enum class GncTransPropType ! */
extern std::map<GncTransPropType, const char*> gnc_csv_col_type_strs;

/** Error domain for the csv importer. */
#define GNC_CSV_IMP_ERROR gnc_csv_imp_error_quark ()
GQuark gnc_csv_imp_error_quark (void);

/** Enumeration for error types. These are the different types of
 * errors that various functions used for the CSV/Fixed-Width importer
 * can have. */
enum GncCsvErrorType {
    GNC_CSV_IMP_ERROR_OPEN,
    GNC_CSV_IMP_ERROR_ENCODING,
    GNC_CSV_IMP_ERROR_PARSE
};

/* TODO We now sort transactions by date, not line number, so we
 * should probably get rid of this struct and uses of it. */

/** Struct pairing a transaction with a line number. This struct is
 * used to keep the transactions in order. When rows are separated
 * into "valid" and "error" lists (in case some of the rows have cells
 * that are unparseable), we want the user to still be able to
 * "correct" the error list. If we keep the line numbers of valid
 * transactions, we can then put transactions created from the newly
 * corrected rows into the right places. */
typedef struct
{
    int line_no;
    Transaction* trans;
    gnc_numeric balance;  /**< The (supposed) balance after this transaction takes place */
    bool balance_set;     /**< true if balance has been set from user data, false otherwise */
} GncCsvTransLine;

/* A set of currency formats that the user sees. */
extern const int num_currency_formats;
extern const gchar* currency_format_user[];

/* A set of date formats that the user sees. */
extern const int num_date_formats;
extern const gchar* date_format_user[];

/** Pair to hold a tokenized line of input and an optional error string */
using parse_line_t = std::pair<str_vec, std::string>;

/** Struct containing data for parsing a CSV/Fixed-Width file. */
class GncCsvParseData
{
public:
    // Constructor - Destructor
    GncCsvParseData(GncImpFileFormat format = GncImpFileFormat::UNKNOWN);
    ~GncCsvParseData();

    int file_format(GncImpFileFormat format, GError** error);
    GncImpFileFormat file_format();

    int load_file (const std::string& filename, GError** error);
    void convert_encoding (const std::string& encoding);

    int parse (bool guessColTypes, GError** error);
    int parse_to_trans (Account* account, bool redo_errors);
    bool check_for_column_type (GncTransPropType type);

    std::unique_ptr<GncTokenizer> tokenizer;    /**< Will handle file loading/encoding conversion/splitting into fields */
    std::vector<parse_line_t> orig_lines;      /**< file_str parsed into a two-dimensional array of strings */
    std::vector<GncTransPropType> column_types;       /**< Vector of values from the GncCsvColumnType enumeration */
    GList* transactions;        /**< List of GncCsvTransLine*s created using orig_lines and column_types */
    int date_format;            /**< The format of the text in the date columns from date_format_internal. */
    guint start_row;            /**< The start row to generate transactions from. */
    guint end_row;              /**< The end row to generate transactions from. */
    gboolean skip_rows;         /**< Skip Alternate Rows from start row. */
    int currency_format;        /**< The currency format, 0 for locale, 1 for comma dec and 2 for period */
    bool parse_errors;          /**< Indicates whether the last parse_to_trans run had any errors */

private:
    GncImpFileFormat file_fmt = GncImpFileFormat::UNKNOWN;
};

time64 parse_date (const std::string &date_str, int format);

#endif

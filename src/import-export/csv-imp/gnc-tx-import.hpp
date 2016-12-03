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
#include "gnc-trans-props.hpp"


/** This struct stores a possibly incomplete transaction
 *  optionally together with its intended balance in case
 *  the user had selected a balance column. */
struct DraftTransaction
{
    DraftTransaction (Transaction* tx) : trans(tx), balance(gnc_numeric_zero()), balance_set(false) {}
    ~DraftTransaction () { if (trans) { xaccTransDestroy (trans); trans = nullptr; } }
    Transaction* trans;
    gnc_numeric balance;  /**< The expected balance after this transaction takes place */
    bool balance_set;     /**< true if balance has been set from user data, false otherwise */
};

/* A set of currency formats that the user sees. */
extern const int num_currency_formats;
extern const gchar* currency_format_user[];

/* A set of date formats that the user sees. */
extern const int num_date_formats;
extern const gchar* date_format_user[];

/** Tuple to hold
 *  - a tokenized line of input
 *  - an optional error string
 *  - a struct to hold user selected properties for a transaction
 *  - a struct to hold user selected properties for one or two splits in the above transaction */
using parse_line_t = std::tuple<StrVec,
                                std::string,
                                std::shared_ptr<GncPreTrans>,
                                std::shared_ptr<GncPreSplit>>;

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

    void tokenize (bool guessColTypes);

    /** This function will attempt to convert all tokenized lines into
     *  transactions using the column types the user has set.
     */
    void create_transactions (Account* account, bool redo_errors);
    bool check_for_column_type (GncTransPropType type);

    std::unique_ptr<GncTokenizer> tokenizer;    /**< Will handle file loading/encoding conversion/splitting into fields */
    std::vector<parse_line_t> parsed_lines;     /**< source file parsed into a two-dimensional array of strings.
                                                     Per line also holds possible error messages and objects with extracted transaction
                                                     and split properties. */
    std::vector<GncTransPropType> column_types; /**< Vector of values from the GncCsvColumnType enumeration */
    std::multimap <time64, std::shared_ptr<DraftTransaction>> transactions;  /**< map of transaction objects created
                                                     from parsed_lines and column_types, ordered by date */
    int date_format;            /**< The format of the text in the date columns from date_format_internal. */
    guint skip_start_lines;     /**< Number of lines to skip at the beginning of the parse data. */
    guint skip_end_lines;       /**< Number of lines to skip at the end of the parse data. */
    gboolean skip_alt_lines;         /**< Skip Alternate Rows from start row. */
    bool multi_split;           /**< If false, each line in the import data defines exactly one transaction.
                                     If true, a transaction can span multiple lines, with each line defining exactly one split.
                                     In this case the first line should hold the transaction related details in
                                     addition to the first split details. On each following line for the same
                                     transaction the transaction related columns should be empty or have
                                     the same value as the first line. */
    int currency_format;        /**< The currency format, 0 for locale, 1 for comma dec and 2 for period */
    bool parse_errors;          /**< Indicates whether the last parse_to_trans run had any errors */

private:
    /** A helper function used by create_transactions. It will attempt
     *  to convert a single tokenized line into a transaction using
     *  the column types the user has set.
     */
    void create_transaction (std::vector<parse_line_t>::iterator& parsed_line);

    /** A helper function used by create_transactions. If the input data has
     *  a balance column (an no deposit and withdrawal columns)
     *  it will iterate over all created transactions
     *  to set the split amount(s) based on the desired balance for that line.
     */
    void adjust_balances (void);

    /* Internal helper function that does the actual conversion from property lists
     * to real (possibly unbalanced) transaction with splits.
     */
    std::shared_ptr<DraftTransaction> trans_properties_to_trans (std::vector<parse_line_t>::iterator& parsed_line);

    GncImpFileFormat file_fmt = GncImpFileFormat::UNKNOWN;

    /* The variables below are only used while creating
     * transactions. They keep state information during the conversion.
     */
    Account *base_account = nullptr;
    std::shared_ptr<GncPreTrans> parent = nullptr;
    std::shared_ptr<DraftTransaction> current_draft = nullptr;
};


#endif

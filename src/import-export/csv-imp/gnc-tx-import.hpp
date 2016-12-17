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
#include "gnc-csv-trans-settings.hpp"


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
    boost::optional<std::string> void_reason;
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

    void multi_split (bool multi_split);
    bool multi_split ();

    void base_account (Account *base_account);
    Account *base_account ();

    void currency_format (int currency_format);
    int currency_format ();

    void date_format (int date_format);
    int date_format ();

    void encoding (const std::string& encoding);
    std::string encoding ();

    void skip_start_lines (uint num);
    uint skip_start_lines ();

    void skip_end_lines (uint num);
    uint skip_end_lines ();

    void skip_alt_lines (bool skip);
    bool skip_alt_lines ();

    void separators (std::string separators);
    std::string separators ();

    void settings (const CsvTransSettings& settings);
    bool save_settings ();

    void settings_name (std::string name);
    std::string settings_name ();


    void load_file (const std::string& filename);

    void tokenize (bool guessColTypes);

    std::string verify();

    /** This function will attempt to convert all tokenized lines into
     *  transactions using the column types the user has set.
     */
    void create_transactions (bool redo_errors);
    bool check_for_column_type (GncTransPropType type);
    void set_column_type (uint position, GncTransPropType type);
    std::vector<GncTransPropType> column_types ();

    std::unique_ptr<GncTokenizer> tokenizer;    /**< Will handle file loading/encoding conversion/splitting into fields */
    std::vector<parse_line_t> parsed_lines;     /**< source file parsed into a two-dimensional array of strings.
                                                     Per line also holds possible error messages and objects with extracted transaction
                                                     and split properties. */
    std::multimap <time64, std::shared_ptr<DraftTransaction>> transactions;  /**< map of transaction objects created
                                                     from parsed_lines and column_types, ordered by date */
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

    struct CsvTranSettings;
    CsvTransSettings m_settings;

    /* The parameters below are only used while creating
     * transactions. They keep state information during the conversion.
     */
    std::shared_ptr<GncPreTrans> parent = nullptr;
    std::shared_ptr<DraftTransaction> current_draft = nullptr;
};


#endif

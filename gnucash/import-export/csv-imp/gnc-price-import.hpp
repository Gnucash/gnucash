/********************************************************************\
 * gnc-price-import.hpp - import prices from csv files              *
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
     @brief Class to import prices from CSV or fixed width files
     *
     gnc-price-import.hpp
     @author Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>
     @author Copyright (c) 2017 Robert Fewell
 */

#ifndef GNC_PRICE_IMPORT_HPP
#define GNC_PRICE_IMPORT_HPP

extern "C" {
#include "config.h"

}

#include <vector>
#include <set>
#include <map>
#include <memory>

#include "gnc-tokenizer.hpp"
#include "gnc-price-props.hpp"
#include "gnc-csv-trans-settings.hpp"
#include <boost/optional.hpp>

/* A set of currency formats that the user sees. */
extern const int num_currency_formats;
extern const gchar* currency_format_user[];

/* A set of date formats that the user sees. */
extern const int num_date_formats;
extern const gchar* date_format_user[];

/** Tuple to hold
 *  - a tokenized line of input
 *  - an optional error string
 *  - a struct to hold user selected properties for a price */
using parse_line_t = std::tuple<StrVec,
                                std::string,
                                std::shared_ptr<GncImportPrice>,
                                bool>;
struct ErrorListPrice;

/** The actual PriceImport class
 * It's intended to use in the following sequence of actions:
 * - set a file format
 * - load a file
 * - optionally convert it's encoding
 * - parse the file into lines, which in turn are split up in columns
 *   the result of this step can be queried from tokenizer
 * - the user should now map the columns to types, which is stored in column_types
 * - last step is convert the mapped columns into a list of transactions
 * - this list will then be passed on the the generic importer for further processing */
class GncPriceImport
{
public:
    // Constructor - Destructor
    GncPriceImport(GncImpFileFormat format = GncImpFileFormat::UNKNOWN);
    ~GncPriceImport();

    void file_format(GncImpFileFormat format);
    GncImpFileFormat file_format();

    void over_write (bool over);
    bool over_write ();

    void currency_format (int currency_format);
    int currency_format ();

    void date_format (int date_format);
    int date_format ();

    void encoding (const std::string& encoding);
    std::string encoding ();

    void update_skipped_lines (boost::optional<uint32_t> start, boost::optional<uint32_t> end,
                               boost::optional<bool> alt, boost::optional<bool> errors);
    uint32_t skip_start_lines ();
    uint32_t skip_end_lines ();
    bool skip_alt_lines ();
    bool skip_err_lines ();

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
     *  prices using the column types the user has set.
     */
    void create_prices ();
    bool check_for_column_type (GncPricePropType type);
    void set_column_type_price (uint32_t position, GncPricePropType type, bool force = false);
    std::vector<GncPricePropType> column_types_price ();

    std::unique_ptr<GncTokenizer> m_tokenizer;    /**< Will handle file loading/encoding conversion/splitting into fields */
    std::vector<parse_line_t> m_parsed_lines;     /**< source file parsed into a two-dimensional array of strings.
                                                     Per line also holds possible error messages and objects with extracted
                                                     price properties. */
    int  m_prices_added;
    int  m_prices_duplicated;

private:
    /** A helper function used by create_prices. It will attempt
     *  to convert a single tokenized line into a price using
     *  the column types the user has set.
     */
    void create_price (std::vector<parse_line_t>::iterator& parsed_line);

    void verify_column_selections (ErrorListPrice& error_msg);

    /* Internal helper function to force reparsing of columns subject to format changes */
    void reset_formatted_column (std::vector<GncPricePropType>& col_types);

    /* Two internal helper functions that should only be called from within
     * set_column_type_price for consistency (otherwise error messages may not be (re)set)
     */
    void update_price_props (uint32_t row, uint32_t col, GncPricePropType prop_type);

    struct CsvTranSettings;
    CsvTransSettings m_settings;
    bool m_skip_errors;
    bool m_over_write;
};


#endif

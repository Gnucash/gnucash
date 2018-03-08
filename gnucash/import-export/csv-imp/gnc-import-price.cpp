/********************************************************************\
 * gnc-import-price.cpp - import prices from csv files              *
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

#include "gnc-ui-util.h" //get book
#include "gnc-commodity.h"
#include "gnc-pricedb.h"
}

#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>

#include "gnc-import-price.hpp"
#include "gnc-imp-props-price.hpp"
#include "gnc-tokenizer-csv.hpp"
#include "gnc-tokenizer-fw.hpp"
#include "gnc-imp-settings-csv-price.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

const int num_currency_formats_price = 3;
const gchar* currency_format_user_price[] = {N_("Locale"),
                                       N_("Period: 123,456.78"),
                                       N_("Comma: 123.456,78")
                                      };


/** Constructor for GncPriceImport.
 */
GncPriceImport::GncPriceImport(GncImpFileFormat format)
{
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    m_skip_errors = false;
    file_format(m_settings.m_file_format = format);
}

/** Destructor for GncPriceImport.
 */
GncPriceImport::~GncPriceImport()
{
}

/** Sets the file format for the file to import, which
 *  may cause the file to be reloaded as well if the
 *  previously set file format was different and a
 *  filename was already set.
 *  @param format the new format to set
 *  @exception std::ifstream::failure if file reloading fails
 */
void GncPriceImport::file_format(GncImpFileFormat format)
{
    if (m_tokenizer && m_settings.m_file_format == format)
        return;

    auto new_encoding = std::string("UTF-8");
    auto new_imp_file = std::string();

    // Recover common settings from old tokenizer
    if (m_tokenizer)
    {
        new_encoding = m_tokenizer->encoding();
        new_imp_file = m_tokenizer->current_file();
        if (file_format() == GncImpFileFormat::FIXED_WIDTH)
        {
            auto fwtok = dynamic_cast<GncFwTokenizer*>(m_tokenizer.get());
            if (!fwtok->get_columns().empty())
                m_settings.m_column_widths = fwtok->get_columns();
        }
    }

    m_settings.m_file_format = format;
    m_tokenizer = gnc_tokenizer_factory(m_settings.m_file_format);

    // Set up new tokenizer with common settings
    // recovered from old tokenizer
    m_tokenizer->encoding(new_encoding);
    load_file(new_imp_file);

    // Restore potentially previously set separators or column_widths
    if ((file_format() == GncImpFileFormat::CSV)
        && !m_settings.m_separators.empty())
        separators (m_settings.m_separators);
    else if ((file_format() == GncImpFileFormat::FIXED_WIDTH)
        && !m_settings.m_column_widths.empty())
    {
        auto fwtok = dynamic_cast<GncFwTokenizer*>(m_tokenizer.get());
        fwtok->columns (m_settings.m_column_widths);
    }
}

GncImpFileFormat GncPriceImport::file_format()
{
    return m_settings.m_file_format;
}

void GncPriceImport::over_write (bool over)
{
    m_over_write = over;
}
bool GncPriceImport::over_write () { return m_over_write; }

/** Sets a from commodity. This is the commodity all import data relates to.
 *  When a from commodity is set, there can't be any from columns selected
 *  in the import data.
 * @param from_commodity pointer to a commodity or NULL.
 */
void GncPriceImport::from_commodity (gnc_commodity* from_commodity)
{
    m_settings.m_from_commodity = from_commodity;
    if (m_settings.m_from_commodity)
    {
        auto col_type_comm = std::find (m_settings.m_column_types_price.begin(),
                m_settings.m_column_types_price.end(), GncPricePropType::FROM_COMMODITY);

        if (col_type_comm != m_settings.m_column_types_price.end())
            set_column_type_price (col_type_comm -m_settings.m_column_types_price.begin(),
                            GncPricePropType::NONE);

        // force a refresh of the to_currency if the from_commodity is changed
        std::vector<GncPricePropType> commodities = { GncPricePropType::TO_CURRENCY };
        reset_formatted_column (commodities);
    }
}
gnc_commodity *GncPriceImport::from_commodity () { return m_settings.m_from_commodity; }

/** Sets a to currency. This is the to currency all import data relates to.
 *  When a to currency is set, there can't be any to currency columns selected
 *  in the import data.
 * @param to_currency pointer to a commodity or NULL.
 */
void GncPriceImport::to_currency (gnc_commodity* to_currency)
{
    m_settings.m_to_currency = to_currency;
    if (m_settings.m_to_currency)
    {
        auto col_type_currency = std::find (m_settings.m_column_types_price.begin(),
                m_settings.m_column_types_price.end(), GncPricePropType::TO_CURRENCY);

        if (col_type_currency != m_settings.m_column_types_price.end())
            set_column_type_price (col_type_currency -m_settings.m_column_types_price.begin(),
                            GncPricePropType::NONE);

        // force a refresh of the from_commodity if the to_currency is changed
        std::vector<GncPricePropType> commodities = { GncPricePropType::FROM_COMMODITY };
        reset_formatted_column (commodities);
    }
}
gnc_commodity *GncPriceImport::to_currency () { return m_settings.m_to_currency; }

void GncPriceImport::reset_formatted_column (std::vector<GncPricePropType>& col_types)
{
    for (auto col_type: col_types)
    {
        auto col = std::find (m_settings.m_column_types_price.begin(),
                m_settings.m_column_types_price.end(), col_type);
        if (col != m_settings.m_column_types_price.end())
            set_column_type_price (col - m_settings.m_column_types_price.begin(), col_type, true);
    }
}

void GncPriceImport::currency_format (int currency_format)
{
    m_settings.m_currency_format = currency_format;

    /* Reparse all currency related columns */
    std::vector<GncPricePropType> commodities = { GncPricePropType::AMOUNT };
    reset_formatted_column (commodities);
}
int GncPriceImport::currency_format () { return m_settings.m_currency_format; }

void GncPriceImport::date_format (int date_format)
{
    m_settings.m_date_format = date_format;

    /* Reparse all date related columns */
    std::vector<GncPricePropType> dates = { GncPricePropType::DATE };
    reset_formatted_column (dates);
}
int GncPriceImport::date_format () { return m_settings.m_date_format; }

/** Converts raw file data using a new encoding. This function must be
 * called after load_file only if load_file guessed
 * the wrong encoding.
 * @param encoding Encoding that data should be translated using
 */
void GncPriceImport::encoding (const std::string& encoding)
{
    // TODO investigate if we can catch conversion errors and report them
    if (m_tokenizer)
    {
        m_tokenizer->encoding(encoding); // May throw
        try
        {
            tokenize(false);
        }
        catch (...)
        { };
    }

    m_settings.m_encoding = encoding;
}

std::string GncPriceImport::encoding () { return m_settings.m_encoding; }

void GncPriceImport::update_skipped_lines(boost::optional<uint32_t> start, boost::optional<uint32_t> end,
        boost::optional<bool> alt, boost::optional<bool> errors)
{
    if (start)
        m_settings.m_skip_start_lines = *start;
    if (end)
        m_settings.m_skip_end_lines = *end;
    if (alt)
        m_settings.m_skip_alt_lines = *alt;
    if (errors)
        m_skip_errors = *errors;

    for (uint32_t i = 0; i < m_parsed_lines.size(); i++)
    {
        std::get<PL_SKIP>(m_parsed_lines[i]) =
            ((i < skip_start_lines()) ||             // start rows to skip
             (i >= m_parsed_lines.size() - skip_end_lines()) ||          // end rows to skip
             (((i - skip_start_lines()) % 2 == 1) && // skip every second row...
                  skip_alt_lines()) ||                   // ...if requested
             (m_skip_errors && !std::get<PL_ERROR>(m_parsed_lines[i]).empty())); // skip lines with errors
    }
}

uint32_t GncPriceImport::skip_start_lines () { return m_settings.m_skip_start_lines; }
uint32_t GncPriceImport::skip_end_lines () { return m_settings.m_skip_end_lines; }
bool GncPriceImport::skip_alt_lines () { return m_settings.m_skip_alt_lines; }
bool GncPriceImport::skip_err_lines () { return m_skip_errors; }

void GncPriceImport::separators (std::string separators)
{
    if (file_format() != GncImpFileFormat::CSV)
        return;

    m_settings.m_separators = separators;
    auto csvtok = dynamic_cast<GncCsvTokenizer*>(m_tokenizer.get());
    csvtok->set_separators (separators);

}
std::string GncPriceImport::separators () { return m_settings.m_separators; }

void GncPriceImport::settings (const CsvPriceImpSettings& settings)
{
    /* First apply file format as this may recreate the tokenizer */
    file_format (settings.m_file_format);
    /* Only then apply the other settings */
    m_settings = settings;
    from_commodity (m_settings.m_from_commodity);
    to_currency (m_settings.m_to_currency);
    encoding (m_settings.m_encoding);

    if (file_format() == GncImpFileFormat::CSV)
        separators (m_settings.m_separators);
    else if (file_format() == GncImpFileFormat::FIXED_WIDTH)
    {
        auto fwtok = dynamic_cast<GncFwTokenizer*>(m_tokenizer.get());
        fwtok->columns (m_settings.m_column_widths);
    }
    try
    {
        tokenize(false);
    }
    catch (...)
    { };

    /* Tokenizing will clear column types, reset them here
     * based on the loaded settings.
     */
    std::copy_n (settings.m_column_types_price.begin(),
            std::min (m_settings.m_column_types_price.size(), settings.m_column_types_price.size()),
            m_settings.m_column_types_price.begin());
}

bool GncPriceImport::save_settings ()
{
    if (preset_is_reserved_name (m_settings.m_name))
        return true;

    /* separators are already copied to m_settings in the separators
     * function above. However this is not the case for the column
     * widths in fw mode, so do this now.
     */
    if (file_format() == GncImpFileFormat::FIXED_WIDTH)
    {
        auto fwtok = dynamic_cast<GncFwTokenizer*>(m_tokenizer.get());
        m_settings.m_column_widths = fwtok->get_columns();
    }
    return m_settings.save();
}

void GncPriceImport::settings_name (std::string name) { m_settings.m_name = name; }
std::string GncPriceImport::settings_name () { return m_settings.m_name; }

/** Loads a file into a GncPriceImport. This is the first function
 * that must be called after creating a new GncPriceImport. As long as
 * this function didn't run successfully, the importer can't proceed.
 * @param filename Name of the file that should be opened
 * @exception may throw std::ifstream::failure on any io error
 */
void GncPriceImport::load_file (const std::string& filename)
{
    /* Get the raw data first and handle an error if one occurs. */
    try
    {
        m_tokenizer->load_file (filename);
        return;
    }
    catch (std::ifstream::failure& ios_err)
    {
        // Just log the error and pass it on the call stack for proper handling
        PWARN ("Error: %s", ios_err.what());
        throw;
    }
}

/** Splits a file into cells. This requires having an encoding that
 * works (see GncPriceImport::convert_encoding). Tokenizing related options
 * should be set to the user's selections before calling this
 * function.
 * Notes: - this function must be called with guessColTypes set to true once
 *          before calling it with guessColTypes set to false.
 *        - if guessColTypes is true, all the column types will be set
 *          GncPricePropType::NONE right now as real guessing isn't implemented yet
 * @param guessColTypes true to guess what the types of columns are based on the cell contents
 * @exception std::range_error if tokenizing failed
 */
void GncPriceImport::tokenize (bool guessColTypes)
{
    if (!m_tokenizer)
        return;

    uint32_t max_cols = 0;
    m_tokenizer->tokenize();
    m_parsed_lines.clear();
    for (auto tokenized_line : m_tokenizer->get_tokens())
    {
        m_parsed_lines.push_back (std::make_tuple (tokenized_line, std::string(),
                std::make_shared<GncImportPrice>(date_format(), currency_format()),
                false));
        auto length = tokenized_line.size();
        if (length > max_cols)
            max_cols = length;
    }

    /* If it failed, generate an error. */
    if (m_parsed_lines.size() == 0)
    {
        throw (std::range_error ("Tokenizing failed."));
        return;
    }

    m_settings.m_column_types_price.resize(max_cols, GncPricePropType::NONE);

    /* Force reinterpretation of already set columns and/or base_account */
    for (uint32_t i = 0; i < m_settings.m_column_types_price.size(); i++)
        set_column_type_price (i, m_settings.m_column_types_price[i], true);

    if (guessColTypes)
    {
        /* Guess column_types based
         * on the contents of each column. */
        /* TODO Make it actually guess. */
    }
}

struct ErrorListPrice
{
public:
    void add_error (std::string msg);
    std::string str();
    bool empty() { return m_error.empty(); }
private:
    std::string m_error;
};

void ErrorListPrice::add_error (std::string msg)
{
    m_error += "- " + msg + "\n";
}

std::string ErrorListPrice::str()
{
    return m_error.substr(0, m_error.size() - 1);
}

/* Test for the required minimum number of columns selected and
 * the selection is consistent.
 * @param An ErrorListPrice object to which all found issues are added.
 */
void GncPriceImport::verify_column_selections (ErrorListPrice& error_msg)
{
    /* Verify if a date column is selected and it's parsable.
     */
    if (!check_for_column_type(GncPricePropType::DATE))
        error_msg.add_error( _("Please select a date column."));

    /* Verify an amount column is selected.
     */
    if (!check_for_column_type(GncPricePropType::AMOUNT))
        error_msg.add_error( _("Please select an amount column."));

    /* Verify a Currency to column is selected.
     */
    if (!check_for_column_type(GncPricePropType::TO_CURRENCY))
    {
        if (!m_settings.m_to_currency)
            error_msg.add_error( _("Please select a 'Currency to' column or set a Currency in the 'Currency To' field."));
    }

    /* Verify a Commodity from column is selected.
     */
    if (!check_for_column_type(GncPricePropType::FROM_COMMODITY))
    {
        if (!m_settings.m_from_commodity)
            error_msg.add_error( _("Please select a 'Commodity from' column or set a Commodity in the 'Commodity From' field."));
    }

    /* Verify a 'Commodity from' does not equal 'Currency to'.
     */
    if ((m_settings.m_to_currency) && (m_settings.m_from_commodity))
    {
        if (gnc_commodity_equal (m_settings.m_to_currency, m_settings.m_from_commodity))
            error_msg.add_error( _("'Commodity From' can not be the same as 'Currency To'."));
    }
}

/* Check whether the chosen settings can successfully parse
 * the import data. This will check:
 * - there's at least one line selected for import
 * - the minimum number of columns is selected
 * - the values in the selected columns can be parsed meaningfully.
 * @return An empty string if all checks passed or the reason
 *         verification failed otherwise.
 */
std::string GncPriceImport::verify ()
{
    auto newline = std::string();
    auto error_msg = ErrorListPrice();

    /* Check if the import file did actually contain any information */
    if (m_parsed_lines.size() == 0)
    {
        error_msg.add_error(_("No valid data found in the selected file. It may be empty or the selected encoding is wrong."));
        return error_msg.str();
    }

    /* Check if at least one line is selected for importing */
    auto skip_alt_offset = m_settings.m_skip_alt_lines ? 1 : 0;
    if (m_settings.m_skip_start_lines + m_settings.m_skip_end_lines + skip_alt_offset >= m_parsed_lines.size())
    {
        error_msg.add_error(_("No lines are selected for importing. Please reduce the number of lines to skip."));
        return error_msg.str();
    }

    verify_column_selections (error_msg);

    update_skipped_lines (boost::none, boost::none, boost::none, boost::none);

    auto have_line_errors = false;
    for (auto line : m_parsed_lines)
    {
        if (!std::get<PL_SKIP>(line) && !std::get<PL_ERROR>(line).empty())
        {
            have_line_errors = true;
            break;
        }
    }

    if (have_line_errors)
        error_msg.add_error( _("Not all fields could be parsed. Please correct the issues reported for each line or adjust the lines to skip."));

    return error_msg.str();
}

/** Checks whether the parsed line contains all essential properties.
 * @param parsed_line The line we are checking
 * @exception std::invalid_argument in an essential property is missing
 */
static void price_properties_verify_essentials (std::vector<parse_line_t>::iterator& parsed_line)
{
    std::string error_message;
    std::shared_ptr<GncImportPrice> price_props;
    std::tie(std::ignore, error_message, price_props, std::ignore) = *parsed_line;

    auto price_error = price_props->verify_essentials();

    error_message.clear();
    if (!price_error.empty())
    {
        error_message += price_error;
        error_message += "\n";
    }

    if (!error_message.empty())
        throw std::invalid_argument(error_message);
}

void GncPriceImport::create_price (std::vector<parse_line_t>::iterator& parsed_line)
{
    StrVec line;
    std::string error_message;
    std::shared_ptr<GncImportPrice> price_props = nullptr;
    bool skip_line = false;
    std::tie(line, error_message, price_props, skip_line) = *parsed_line;

    if (skip_line)
        return;

    error_message.clear();

    // Add a TO_CURRENCY property with the selected 'currency to' if no 'currency to' column was set by the user
    auto line_to_currency = price_props->get_to_currency();
    if (!line_to_currency)
    {
        if (m_settings.m_to_currency)
            price_props->set_to_currency(m_settings.m_to_currency);
        else
        {
            // Oops - the user didn't select a 'currency to' column *and* we didn't get a selected value either!
            // Note if you get here this suggests a bug in the code!
            error_message = _("No 'Currency to' column selected and no selected Currency specified either.\n"
                                       "This should never happen. Please report this as a bug.");
            PINFO("User warning: %s", error_message.c_str());
            throw std::invalid_argument(error_message);
        }
    }

    // Add a FROM_COMMODITY property with the selected 'commodity from' if no 'commodity from' column was set by the user
    auto line_from_commodity = price_props->get_from_commodity();
    if (!line_from_commodity)
    {
        if (m_settings.m_from_commodity)
            price_props->set_from_commodity(m_settings.m_from_commodity);
        else
        {
            // Oops - the user didn't select a 'commodity from' column *and* we didn't get a selected value either!
            // Note if you get here this suggests a bug in the code!
            error_message = _("No 'Commodity from' column selected and no selected Commodity specified either.\n"
                                       "This should never happen. Please report this as a bug.");
            PINFO("User warning: %s", error_message.c_str());
            throw std::invalid_argument(error_message);
        }
    }

    /* If column parsing was successful, convert price properties into a price. */
    try
    {
        price_properties_verify_essentials (parsed_line);

        QofBook* book = gnc_get_current_book();
        GNCPriceDB *pdb = gnc_pricedb_get_db (book);

        /* If all went well, add this price to the list. */
        auto price_created = price_props->create_price (book, pdb, m_over_write);
        if (price_created == ADDED)
            m_prices_added++;
        else if (price_created == DUPLICATED)
            m_prices_duplicated++;
        else if (price_created == REPLACED)
            m_prices_replaced++;
    }
    catch (const std::invalid_argument& e)
    {
        error_message = e.what();
        PINFO("User warning: %s", error_message.c_str());
    }
}

/** Creates a list of prices from parsed data. The parsed data
 * will first be validated. If any errors are found in lines that are marked
 * for processing (ie not marked to skip) this function will
 * throw an error.
 * @param skip_errors true skip over lines with errors
 * @exception throws std::invalid_argument if data validation or processing fails.
 */
void GncPriceImport::create_prices ()
{
    /* Start with verifying the current data. */
    auto verify_result = verify();
    if (!verify_result.empty())
        throw std::invalid_argument (verify_result);

    m_prices_added = 0;
    m_prices_duplicated = 0;
    m_prices_replaced = 0;

    /* Iterate over all parsed lines */
    for (auto parsed_lines_it = m_parsed_lines.begin();
            parsed_lines_it != m_parsed_lines.end();
            ++parsed_lines_it)
    {
        /* Skip current line if the user specified so */
        if ((std::get<PL_SKIP>(*parsed_lines_it)))
            continue;

        /* Should not throw anymore, otherwise verify needs revision */
        create_price (parsed_lines_it);
    }
    PINFO("Number of lines is %d, added %d, duplicated %d, replaced %d",
         (int)m_parsed_lines.size(), m_prices_added, m_prices_duplicated, m_prices_replaced);
}

bool
GncPriceImport::check_for_column_type (GncPricePropType type)
{
    return (std::find (m_settings.m_column_types_price.begin(),
                       m_settings.m_column_types_price.end(), type)
                        != m_settings.m_column_types_price.end());
}

/* A helper function intended to be called only from set_column_type_price */
void GncPriceImport::update_price_props (uint32_t row, uint32_t col, GncPricePropType prop_type)
{
    if (prop_type == GncPricePropType::NONE)
        return; /* Only deal with price related properties. */

    auto price_props = std::make_shared<GncImportPrice> (*(std::get<PL_PREPRICE>(m_parsed_lines[row])).get());

    if (col >= std::get<PL_INPUT>(m_parsed_lines[row]).size())
        price_props->reset (prop_type); //reset errors
    else
    {
        auto value = std::get<PL_INPUT>(m_parsed_lines[row]).at(col);
        bool enable_test_empty = true;
        try
        {
            // set the from_commodity based on combo so we can test for same.
            if (prop_type == GncPricePropType::TO_CURRENCY)
            {
                if (m_settings.m_from_commodity)
                    price_props->set_from_commodity (m_settings.m_from_commodity);

                if (m_settings.m_to_currency)
                    enable_test_empty = false;
            }
            // set the to_currency based on combo so we can test for same.
            if (prop_type == GncPricePropType::FROM_COMMODITY)
            {
                if (m_settings.m_to_currency)
                    price_props->set_to_currency (m_settings.m_to_currency);

                if (m_settings.m_from_commodity)
                    enable_test_empty = false;
            }
            price_props->set(prop_type, value, enable_test_empty);
        }
        catch (const std::exception& e)
        {
            /* Do nothing, just prevent the exception from escalating up
             * However log the error if it happens on a row that's not skipped
             */
            if (!std::get<PL_SKIP>(m_parsed_lines[row]))
                PINFO("User warning: %s", e.what());
        }
    }
    /* Store the result */
    std::get<PL_PREPRICE>(m_parsed_lines[row]) = price_props;
}

void
GncPriceImport::set_column_type_price (uint32_t position, GncPricePropType type, bool force)
{
    if (position >= m_settings.m_column_types_price.size())
        return;

    auto old_type = m_settings.m_column_types_price[position];
    if ((type == old_type) && !force)
        return; /* Nothing to do */

    // Column types should be unique, so remove any previous occurrence of the new type
    std::replace(m_settings.m_column_types_price.begin(), m_settings.m_column_types_price.end(),
            type, GncPricePropType::NONE);

    m_settings.m_column_types_price.at (position) = type;

    // If the user has set a 'commodity from' column, we can't have a commodity from selected
    if (type == GncPricePropType::FROM_COMMODITY)
        from_commodity (nullptr);

    // If the user has set a 'currency to' column, we can't have a currency to selected
    if (type == GncPricePropType::TO_CURRENCY)
        to_currency (nullptr);

    /* Update the preparsed data */
    for (auto parsed_lines_it = m_parsed_lines.begin();
            parsed_lines_it != m_parsed_lines.end();
            ++parsed_lines_it)
    {
        /* Reset date and currency formats for each price props object
         * to ensure column updates use the most recent one
         */
        std::get<PL_PREPRICE>(*parsed_lines_it)->set_date_format (m_settings.m_date_format);
        std::get<PL_PREPRICE>(*parsed_lines_it)->set_currency_format (m_settings.m_currency_format);

        uint32_t row = parsed_lines_it - m_parsed_lines.begin();

        /* If the column type actually changed, first reset the property
         * represented by the old column type
         */
        if (old_type != type)
        {
            auto old_col = std::get<PL_INPUT>(*parsed_lines_it).size(); // Deliberately out of bounds to trigger a reset!
            if ((old_type > GncPricePropType::NONE)
                    && (old_type <= GncPricePropType::PRICE_PROPS))
                update_price_props (row, old_col, old_type);
        }
        /* Then set the property represented by the new column type */
        if ((type > GncPricePropType::NONE)
                && (type <= GncPricePropType::PRICE_PROPS))
            update_price_props (row, position, type);

        /* Report errors if there are any */
        auto price_errors = std::get<PL_PREPRICE>(*parsed_lines_it)->errors();
        std::get<PL_ERROR>(*parsed_lines_it) =
                price_errors +
                (price_errors.empty() ? std::string() : "\n");
    }
}

std::vector<GncPricePropType> GncPriceImport::column_types_price ()
{
    return m_settings.m_column_types_price;
}


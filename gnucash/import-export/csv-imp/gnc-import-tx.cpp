/********************************************************************\
 * gnc-import-tx.cpp - import transactions from csv or fixed-width  *
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

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>

#include <algorithm>
#include <exception>
#include <iostream>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>

#include "gnc-import-tx.hpp"
#include "gnc-imp-props-tx.hpp"
#include "gnc-tokenizer-csv.hpp"
#include "gnc-tokenizer-fw.hpp"
#include "gnc-imp-settings-csv-tx.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

const int num_currency_formats = 3;
const gchar* currency_format_user[] = {N_("Locale"),
                                       N_("Period: 123,456.78"),
                                       N_("Comma: 123.456,78")
                                      };


/** Constructor for GncTxImport.
 */
GncTxImport::GncTxImport(GncImpFileFormat format)
{
    /* All of the data pointers are initially NULL. This is so that, if
     * gnc_csv_parse_data_free is called before all of the data is
     * initialized, only the data that needs to be freed is freed. */
    m_skip_errors = false;
    file_format(m_settings.m_file_format = format);
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
 *  @exception std::ifstream::failure if file reloading fails
 */
void GncTxImport::file_format(GncImpFileFormat format)
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

GncImpFileFormat GncTxImport::file_format()
{
    return m_settings.m_file_format;
}

/** Toggles the multi-split state of the importer and will subsequently
 *  sanitize the column_types list. All types that don't make sense
 *  in the new state are reset to type GncTransPropType::NONE.
 *  Additionally the interpretation of the columns with transaction
 *  properties changes when changing multi-split mode. So this function
 *  will force a reparsing of the transaction properties (if there are
 *  any) by resetting the first column with a transaction property
 *  it encounters.
 * @param multi_split Boolean value with desired state (multi-split
 * vs two-split).
 */
void GncTxImport::multi_split (bool multi_split)
{
    auto trans_prop_seen = false;
    m_settings.m_multi_split = multi_split;
    for (uint32_t i = 0; i < m_settings.m_column_types.size(); i++)
    {
        auto old_prop = m_settings.m_column_types[i];
        auto is_trans_prop = ((old_prop > GncTransPropType::NONE)
                && (old_prop <= GncTransPropType::TRANS_PROPS));
        auto san_prop = sanitize_trans_prop (old_prop, m_settings.m_multi_split);
        if (san_prop != old_prop)
            set_column_type (i, san_prop);
        else if (is_trans_prop && !trans_prop_seen)
            set_column_type (i, old_prop, true);
        trans_prop_seen |= is_trans_prop;

    }
    if (m_settings.m_multi_split)
        m_settings.m_base_account = nullptr;
}

bool GncTxImport::multi_split () { return m_settings.m_multi_split; }

/** Sets a base account. This is the account all import data relates to.
 *  As such at least one split of each transaction that will be generated
 *  will be in this account.
 *  When a base account is set, there can't be an account column selected
 *  in the import data.
 *  In multi-split mode the user has to select an account column so in
 *  that mode the base_account can't be set.
 * @param base_account Pointer to an account or NULL.
 */
void GncTxImport::base_account (Account* base_account)
{
    if (m_settings.m_multi_split)
    {
        m_settings.m_base_account = nullptr;
        return;
    }

    m_settings.m_base_account = base_account;

    if (m_settings.m_base_account)
    {
        auto col_type_it = std::find (m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::ACCOUNT);
        if (col_type_it != m_settings.m_column_types.end())
            set_column_type(col_type_it - m_settings.m_column_types.begin(),
                            GncTransPropType::NONE);

        /* Set default account for each line's split properties */
        for (auto line : m_parsed_lines)
            std::get<PL_PRESPLIT>(line)->set_account (m_settings.m_base_account);


    }
}

Account *GncTxImport::base_account () { return m_settings.m_base_account; }

void GncTxImport::reset_formatted_column (std::vector<GncTransPropType>& col_types)
{
    for (auto col_type: col_types)
    {
        auto col = std::find (m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), col_type);
        if (col != m_settings.m_column_types.end())
            set_column_type (col - m_settings.m_column_types.begin(), col_type, true);
    }
}

void GncTxImport::currency_format (int currency_format)
{
    m_settings.m_currency_format = currency_format;

    /* Reparse all currency related columns */
    std::vector<GncTransPropType> commodities = {
        GncTransPropType::AMOUNT,
        GncTransPropType::AMOUNT_NEG,
        GncTransPropType::TAMOUNT,
        GncTransPropType::TAMOUNT_NEG,
        GncTransPropType::PRICE};
    reset_formatted_column (commodities);
}
int GncTxImport::currency_format () { return m_settings.m_currency_format; }

void GncTxImport::date_format (int date_format)
{
    m_settings.m_date_format = date_format;

    /* Reparse all date related columns */
    std::vector<GncTransPropType> dates = { GncTransPropType::DATE,
            GncTransPropType::REC_DATE,
            GncTransPropType::TREC_DATE};
    reset_formatted_column (dates);
}
int GncTxImport::date_format () { return m_settings.m_date_format; }

/** Converts raw file data using a new encoding. This function must be
 * called after load_file only if load_file guessed
 * the wrong encoding.
 * @param encoding Encoding that data should be translated using
 */
void GncTxImport::encoding (const std::string& encoding)
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

std::string GncTxImport::encoding () { return m_settings.m_encoding; }

void GncTxImport::update_skipped_lines(std::optional<uint32_t> start, std::optional<uint32_t> end,
        std::optional<bool> alt, std::optional<bool> errors)
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

uint32_t GncTxImport::skip_start_lines () { return m_settings.m_skip_start_lines; }
uint32_t GncTxImport::skip_end_lines () { return m_settings.m_skip_end_lines; }
bool GncTxImport::skip_alt_lines () { return m_settings.m_skip_alt_lines; }
bool GncTxImport::skip_err_lines () { return m_skip_errors; }

void GncTxImport::separators (std::string separators)
{
    if (file_format() != GncImpFileFormat::CSV)
        return;

    m_settings.m_separators = separators;
    auto csvtok = dynamic_cast<GncCsvTokenizer*>(m_tokenizer.get());
    csvtok->set_separators (separators);

}
std::string GncTxImport::separators () { return m_settings.m_separators; }

void GncTxImport::settings (const CsvTransImpSettings& settings)
{
    /* First apply file format as this may recreate the tokenizer */
    file_format (settings.m_file_format);
    /* Only then apply the other settings */
    m_settings = settings;
    multi_split (m_settings.m_multi_split);
    base_account (m_settings.m_base_account);
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
    std::copy_n (settings.m_column_types.begin(),
            std::min (m_settings.m_column_types.size(), settings.m_column_types.size()),
            m_settings.m_column_types.begin());

}

bool GncTxImport::save_settings ()
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

void GncTxImport::settings_name (std::string name) { m_settings.m_name = name; }
std::string GncTxImport::settings_name () { return m_settings.m_name; }

/** Loads a file into a GncTxImport. This is the first function
 * that must be called after creating a new GncTxImport. As long as
 * this function didn't run successfully, the importer can't proceed.
 * @param filename Name of the file that should be opened
 * @exception may throw std::ifstream::failure on any io error
 */
void GncTxImport::load_file (const std::string& filename)
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
 * works (see GncTxImport::convert_encoding). Tokenizing related options
 * should be set to the user's selections before calling this
 * function.
 * Notes: - this function must be called with guessColTypes set to true once
 *          before calling it with guessColTypes set to false.
 *        - if guessColTypes is true, all the column types will be set
 *          GncTransPropType::NONE right now as real guessing isn't implemented yet
 * @param guessColTypes true to guess what the types of columns are based on the cell contents
 * @exception std::range_error if tokenizing failed
 */
void GncTxImport::tokenize (bool guessColTypes)
{
    if (!m_tokenizer)
        return;

    uint32_t max_cols = 0;
    m_tokenizer->tokenize();
    m_parsed_lines.clear();
    for (auto tokenized_line : m_tokenizer->get_tokens())
    {
        auto length = tokenized_line.size();
        if (length > 0)
        {
            auto pretrans = std::make_shared<GncPreTrans>(date_format(), m_settings.m_multi_split);
            auto presplit = std::make_shared<GncPreSplit>(date_format(), currency_format());
            presplit->set_pre_trans (std::move (pretrans));
            m_parsed_lines.push_back (std::make_tuple (tokenized_line, ErrMap(),
                                      std::move (presplit), false));
        }
        if (length > max_cols)
            max_cols = length;
    }

    /* If it failed, generate an error. */
    if (m_parsed_lines.size() == 0)
    {
        throw (std::range_error (N_("There was an error parsing the file.")));
        return;
    }

    m_settings.m_column_types.resize(max_cols, GncTransPropType::NONE);

    /* Force reinterpretation of already set columns and/or base_account */
    for (uint32_t i = 0; i < m_settings.m_column_types.size(); i++)
        set_column_type (i, m_settings.m_column_types[i], true);
    if (m_settings.m_base_account)
    {
        for (auto line : m_parsed_lines)
            std::get<PL_PRESPLIT>(line)->set_account (m_settings.m_base_account);
    }

    if (guessColTypes)
    {
        /* Guess column_types based
         * on the contents of each column. */
        /* TODO Make it actually guess. */
    }
}


struct ErrorList
{
public:
    void add_error (std::string msg);
    std::string str();
private:
    StrVec m_error;
};

void ErrorList::add_error (std::string msg)
{
    m_error.emplace_back (msg);
}

std::string ErrorList::str()
{
    auto err_msg = std::string();
    if (!m_error.empty())
    {
        auto add_bullet_item = [](std::string& a, std::string& b)->std::string { return std::move(a) + "\n• " + b; };
        err_msg = std::accumulate (m_error.begin(), m_error.end(), std::move (err_msg), add_bullet_item);
        err_msg.erase (0, 1);
    }

    return err_msg;
}


/* Test for the required minimum number of columns selected and
 * the selection is consistent.
 * @param An ErrorList object to which all found issues are added.
 */
void GncTxImport::verify_column_selections (ErrorList& error_msg)
{

    /* Verify if a date column is selected and it's parsable.
     */
    if (!check_for_column_type(GncTransPropType::DATE))
        error_msg.add_error( _("Please select a date column."));

    /* Verify if an account is selected either in the base account selector
     * or via a column in the import data.
     */
    if (!check_for_column_type(GncTransPropType::ACCOUNT))
    {
        if (m_settings.m_multi_split)
            error_msg.add_error( _("Please select an account column."));
        else if (!m_settings.m_base_account)
            error_msg.add_error( _("Please select an account column or set a base account in the Account field."));
    }

    /* Verify a description column is selected.
     */
    if (!check_for_column_type(GncTransPropType::DESCRIPTION))
        error_msg.add_error( _("Please select a description column."));

    /* Verify at least one amount column (amount or amount_neg) column is selected.
     */
    if (!check_for_column_type(GncTransPropType::AMOUNT) &&
        !check_for_column_type(GncTransPropType::AMOUNT_NEG))
        error_msg.add_error( _("Please select a (negated) amount column."));

    /* In multisplit mode and where current account selections imply multi-
     * currency transactions, we require extra columns to ensure each split is
     * fully defined.
     * Note this check only involves splits created by the csv importer
     * code. The generic import matcher may add a balancing split
     * optionally using Transfer <something> properties. The generic
     * import matcher has its own tools to balance that split so
     * we won't concern ourselves with that one here.
     */
    if (m_multi_currency)
    {
        if (m_settings.m_multi_split &&
            !check_for_column_type(GncTransPropType::PRICE) &&
            !check_for_column_type(GncTransPropType::VALUE) &&
            !check_for_column_type(GncTransPropType::VALUE_NEG))
            error_msg.add_error( _("The current account selections will generate multi-currency transactions. Please select one of the following columns: price, (negated) value."));
        else if (!m_settings.m_multi_split &&
            !check_for_column_type(GncTransPropType::PRICE) &&
            !check_for_column_type(GncTransPropType::TAMOUNT) &&
            !check_for_column_type(GncTransPropType::TAMOUNT_NEG) &&
            !check_for_column_type(GncTransPropType::VALUE) &&
            !check_for_column_type(GncTransPropType::VALUE_NEG))
            error_msg.add_error( _("The current account selections will generate multi-currency transactions. Please select one of the following columns: price, (negated) value, (negated) transfer amount."));
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
std::string GncTxImport::verify (bool with_acct_errors)
{
    auto newline = std::string();
    auto error_msg = ErrorList();

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

    update_skipped_lines (std::nullopt, std::nullopt, std::nullopt, std::nullopt);

    auto have_line_errors = false;
    for (auto line : m_parsed_lines)
    {
        auto errors = std::get<PL_ERROR>(line);
        if (std::get<PL_SKIP>(line))
            continue;
        if (with_acct_errors && !errors.empty())
        {
            have_line_errors = true;
            break;
        }
        auto non_acct_error = [](ErrPair curr_err)
        {
            return !((curr_err.first == GncTransPropType::ACCOUNT) ||
                     (curr_err.first == GncTransPropType::TACCOUNT));
        };
        if (!with_acct_errors &&
            std::any_of(errors.cbegin(), errors.cend(), non_acct_error))
        {
            have_line_errors = true;
            break;
        }
    }

    if (have_line_errors)
        error_msg.add_error( _("Not all fields could be parsed. Please correct the issues reported for each line or adjust the lines to skip."));

    return error_msg.str();
}


/** Create a transaction and splits from a pair of trans and split property objects.
 * Note: this function assumes all properties have been verified
 *       to be valid and the required properties are available.
 * @param parsed_line The current line being parsed
 * @return On success, a shared pointer to a DraftTransaction object; on failure a nullptr
 */
std::shared_ptr<DraftTransaction> GncTxImport::trans_properties_to_trans (std::vector<parse_line_t>::iterator& parsed_line)
{
    auto created_trans = false;
    std::shared_ptr<GncPreSplit> split_props;
    std::tie(std::ignore, std::ignore, split_props, std::ignore) = *parsed_line;
    auto trans_props = split_props->get_pre_trans();
    auto account = split_props->get_account();

    QofBook* book = gnc_account_get_book (account);
    gnc_commodity* currency = xaccAccountGetCommodity (account);
    if (!gnc_commodity_is_currency(currency))
        currency = gnc_account_get_currency_or_parent (account);

    auto draft_trans = trans_props->create_trans (book, currency);

    if (draft_trans)
    {
        /* We're about to continue with a new transaction
         * Time to do some closing actions on the previous one
         */
        if (m_current_draft && m_current_draft->void_reason)
        {
            /* The import data specifies this transaction was voided.
             * So void the created transaction as well.
             * Attention: this assumes the imported transaction was balanced.
             * If not, this will cause an imbalance split to be added automatically!
             */
            xaccTransCommitEdit (m_current_draft->trans);
            xaccTransVoid (m_current_draft->trans, m_current_draft->void_reason->c_str());
        }
        m_current_draft = draft_trans;
        m_current_draft->void_reason = trans_props->get_void_reason();
        created_trans = true;
    }
    else if (m_settings.m_multi_split)  // in multi_split mode create_trans will return a nullptr for all but the first split
        draft_trans = m_current_draft;
    else // in non-multi-split mode each line should be a transaction, so not having one here is an error
        throw std::invalid_argument ("Failed to create transaction from selected columns.");

    if (!draft_trans)
        return nullptr;

    split_props->create_split (draft_trans);

    /* Only return the draft transaction if we really created a new one
     * The return value will be added to a list for further processing,
     * we want each transaction to appear only once in that list.
     */
    return created_trans ? m_current_draft : nullptr;
}

void GncTxImport::create_transaction (std::vector<parse_line_t>::iterator& parsed_line)
{
    StrVec line;
    ErrMap errors;
    std::shared_ptr<GncPreSplit> split_props = nullptr;
    bool skip_line = false;
    std::tie(line, errors, split_props, skip_line) = *parsed_line;
    auto trans_props = split_props->get_pre_trans();

    if (skip_line)
        return;

    // We still have errors for this line. That shouldn't happen!
    if(!errors.empty())
    {
        auto error_message = _("Current line still has parse errors.\n"
                               "This should never happen. Please report this as a bug.");
        throw GncCsvImpParseError(error_message, errors);
    }

    // Add an ACCOUNT property with the default account if no account column was set by the user
    auto line_acct = split_props->get_account();
    if (!line_acct)
    {
        // Oops - the user didn't select an Account column *and* we didn't get a default value either!
        // Note if you get here this suggests a bug in the code!
        auto error_message = _("No account column selected and no base account specified either.\n"
                                "This should never happen. Please report this as a bug.");
        PINFO("User warning: %s", error_message);
        auto errs = ErrMap { ErrPair { GncTransPropType::NONE, error_message},};
        throw GncCsvImpParseError(_("Parse Error"), errs);
    }

    /* If column parsing was successful, convert trans properties into a draft transaction. */
    try
    {
        /* If all went well, add this transaction to the list. */
        auto draft_trans = trans_properties_to_trans (parsed_line);
        if (draft_trans)
        {
            auto trans_date = xaccTransGetDate (draft_trans->trans);
            m_transactions.insert (std::pair<time64, std::shared_ptr<DraftTransaction>>(trans_date,std::move(draft_trans)));
        }
    }
    catch (const std::invalid_argument& e)
    {
        auto err_str = _("Problem creating preliminary transaction");
        PINFO("%s: %s", err_str, e.what());
        auto errs = ErrMap { ErrPair { GncTransPropType::NONE, err_str},};
        throw (GncCsvImpParseError(err_str, errs));
    }
}


/** Creates a list of transactions from parsed data. The parsed data
 * will first be validated. If any errors are found in lines that are marked
 * for processing (ie not marked to skip) this function will
 * throw an error.
 * @param skip_errors true skip over lines with errors
 * @exception throws std::invalid_argument if data validation or processing fails.
 */
void GncTxImport::create_transactions ()
{
    /* Start with verifying the current data. */
    auto verify_result = verify (true);
    if (!verify_result.empty())
        throw std::invalid_argument (verify_result);

    /* Drop all existing draft transactions */
    m_transactions.clear();

    m_parent = nullptr;

    /* Iterate over all parsed lines */
    for (auto parsed_lines_it = m_parsed_lines.begin();
            parsed_lines_it != m_parsed_lines.end();
            ++parsed_lines_it)
    {
        /* Skip current line if the user specified so */
        if ((std::get<PL_SKIP>(*parsed_lines_it)))
            continue;

        /* Should not throw anymore, otherwise verify needs revision */
        create_transaction (parsed_lines_it);
    }
}


bool
GncTxImport::check_for_column_type (GncTransPropType type)
{
    return (std::find (m_settings.m_column_types.begin(),
                       m_settings.m_column_types.end(), type)
                        != m_settings.m_column_types.end());
}

/* A helper function intended to be called only from set_column_type */
void GncTxImport::update_pre_trans_split_props (uint32_t row, uint32_t col, GncTransPropType old_type, GncTransPropType new_type)
{
    /* Deliberately make a copy of the GncPreTrans. It may be the original one was shared
     * with a previous line and should no longer be after the transprop is changed.
     * This doesn't apply for the GncPreSplit so we just get a pointer to it for easier processing.
     */
    auto split_props = std::get<PL_PRESPLIT>(m_parsed_lines[row]);
    auto trans_props = std::make_shared<GncPreTrans> (*(split_props->get_pre_trans()).get());

    /* Deal with trans properties first as this may change the trans->split relationships
        * in case of multi-split imports */
    if ((old_type > GncTransPropType::NONE) && (old_type <= GncTransPropType::TRANS_PROPS))
        trans_props->reset (old_type);
    if ((new_type > GncTransPropType::NONE) && (new_type <= GncTransPropType::TRANS_PROPS))
    {
        auto value = std::string();

        if (col < std::get<PL_INPUT>(m_parsed_lines[row]).size())
            value = std::get<PL_INPUT>(m_parsed_lines[row]).at(col);

        trans_props->set(new_type, value);
    }

    /* In the trans_props we also keep track of currencies/commodities for further
     * multi-currency checks. These come from a PreSplit's account property.
     * If that's the property that we are about to modify, the current
     * counters should be reset. */
    if ((old_type == GncTransPropType::ACCOUNT) || (new_type == GncTransPropType::ACCOUNT))
        trans_props->reset_cross_split_counters();

    /* All transaction related value updates are finished now,
     * time to determine what to do with the updated GncPreTrans copy.
     *
     * For multi-split input data this line may be part of a transaction
     * that has already been started by a previous line. In that case
     * reuse the GncPreTrans from that previous line (which we track
     * in m_parent).
     * In all other cases our new GncPreTrans should be used for this line
     * and be marked as the new potential m_parent for subsequent lines.
     */
    if (m_settings.m_multi_split && trans_props->is_part_of( m_parent))
        split_props->set_pre_trans (m_parent);
    else
    {
        split_props->set_pre_trans (trans_props);
        m_parent = trans_props;
    }

    /* Finally handle any split related property changes */
    if ((old_type > GncTransPropType::TRANS_PROPS) && (old_type <= GncTransPropType::SPLIT_PROPS))
    {
        split_props->reset (old_type);
        if (is_multi_col_prop(old_type))
        {

            /* All amount columns may appear more than once. The net amount
            * needs to be recalculated rather than just reset if one column
            * is unset. */
            for (auto col_it = m_settings.m_column_types.cbegin();
                    col_it < m_settings.m_column_types.cend();
                    col_it++)
                if (*col_it == old_type)
                {
                    auto col_num = col_it - m_settings.m_column_types.cbegin();
                    auto value = std::get<PL_INPUT>(m_parsed_lines[row]).at(col_num);
                    split_props->add (old_type, value);
                }
        }
    }
    if ((new_type > GncTransPropType::TRANS_PROPS) && (new_type <= GncTransPropType::SPLIT_PROPS))
    {
        /* All amount columns may appear more than once. The net amount
            * needs to be recalculated rather than just reset if one column
            * is set. */
        if (is_multi_col_prop(new_type))
        {
            split_props->reset(new_type);
            /* For Deposits and Withdrawal we have to sum all columns with this property */
            for (auto col_it = m_settings.m_column_types.cbegin();
                    col_it < m_settings.m_column_types.cend();
                    col_it++)
                if (*col_it == new_type)
                {
                    auto col_num = col_it - m_settings.m_column_types.cbegin();
                    auto value = std::get<PL_INPUT>(m_parsed_lines[row]).at(col_num);
                    split_props->add (new_type, value);
                }
        }
        else
        {
            auto value = std::get<PL_INPUT>(m_parsed_lines[row]).at(col);
            split_props->set(new_type, value);
        }
    }
    m_multi_currency |= split_props->get_pre_trans()->is_multi_currency();
}


void
GncTxImport::set_column_type (uint32_t position, GncTransPropType type, bool force)
{
    if (position >= m_settings.m_column_types.size())
        return;

    auto old_type = m_settings.m_column_types[position];
    if ((type == old_type) && !force)
        return; /* Nothing to do */

    // Column types except amount and negated amount should be unique,
    // so remove any previous occurrence of the new type
    if (!is_multi_col_prop(type))
        std::replace(m_settings.m_column_types.begin(), m_settings.m_column_types.end(),
            type, GncTransPropType::NONE);

    m_settings.m_column_types.at (position) = type;

    // If the user has set an Account column, we can't have a base account set
    if (type == GncTransPropType::ACCOUNT)
        base_account (nullptr);

    /* Update the preparsed data */
    m_parent = nullptr;
    m_multi_currency = false;
    for (auto parsed_lines_it = m_parsed_lines.begin();
            parsed_lines_it != m_parsed_lines.end();
            ++parsed_lines_it)
    {
        /* Reset date and currency formats for each trans/split props object
         * to ensure column updates use the most recent one
         */
        auto split_props = std::get<PL_PRESPLIT>(*parsed_lines_it);
        split_props->get_pre_trans()->set_date_format (m_settings.m_date_format);
        split_props->get_pre_trans()->set_multi_split (m_settings.m_multi_split);
        split_props->set_date_format (m_settings.m_date_format);
        split_props->set_currency_format (m_settings.m_currency_format);

        uint32_t row = parsed_lines_it - m_parsed_lines.begin();

        /* Update the property represented by the new column type */
        update_pre_trans_split_props (row, position, old_type, type);

        /* Report errors if there are any */
        auto all_errors = split_props->get_pre_trans()->errors();
        all_errors.merge (split_props->errors());
        std::get<PL_ERROR>(*parsed_lines_it) = std::move(all_errors);
    }
}

std::vector<GncTransPropType> GncTxImport::column_types ()
{
    return m_settings.m_column_types;
}

std::set<std::string>
GncTxImport::accounts ()
{
    auto accts = std::set<std::string>();
    auto acct_col_it = std::find (m_settings.m_column_types.begin(),
                           m_settings.m_column_types.end(), GncTransPropType::ACCOUNT);
    uint32_t acct_col = acct_col_it - m_settings.m_column_types.begin();
    auto tacct_col_it = std::find (m_settings.m_column_types.begin(),
                           m_settings.m_column_types.end(), GncTransPropType::TACCOUNT);
    uint32_t tacct_col = tacct_col_it - m_settings.m_column_types.begin();

    /* Iterate over all parsed lines */
    for (auto parsed_line : m_parsed_lines)
    {
        /* Skip current line if the user specified so */
        if ((std::get<PL_SKIP>(parsed_line)))
            continue;

        auto col_strs = std::get<PL_INPUT>(parsed_line);
        if ((acct_col_it != m_settings.m_column_types.end()) &&
            (acct_col < col_strs.size()) &&
            !col_strs[acct_col].empty())
            accts.insert(col_strs[acct_col]);
        if ((tacct_col_it != m_settings.m_column_types.end()) &&
            (tacct_col < col_strs.size()) &&
            !col_strs[tacct_col].empty())
            accts.insert(col_strs[tacct_col]);
    }

    return accts;
}

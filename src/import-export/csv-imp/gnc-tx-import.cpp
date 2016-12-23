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
}

#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>

#include "gnc-tx-import.hpp"
#include "gnc-trans-props.hpp"
#include "gnc-csv-tokenizer.hpp"
#include "gnc-fw-tokenizer.hpp"
#include "gnc-csv-trans-settings.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

const int num_date_formats = 5;
const gchar* date_format_user[] = {N_("y-m-d"),
                                   N_("d-m-y"),
                                   N_("m-d-y"),
                                   N_("d-m"),
                                   N_("m-d")
                                  };

const int num_currency_formats = 3;
const gchar* currency_format_user[] = {N_("Locale"),
                                       N_("Period: 123,456.78"),
                                       N_("Comma: 123.456,78")
                                      };


/** Constructor for GncTxImport.
 * @return Pointer to a new GncCSvParseData
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
    }

    m_settings.m_file_format = format;
    m_tokenizer = gnc_tokenizer_factory(m_settings.m_file_format);

    // Set up new tokenizer with common settings
    // recovered from old tokenizer
    m_tokenizer->encoding(new_encoding);
    load_file(new_imp_file);
}

GncImpFileFormat GncTxImport::file_format()
{
    return m_settings.m_file_format;
}

/** Toggles the multi-split state of the importer and will subsequently
 *  sanitize the column_types list. All types that don't make sense
 *  in the new state are reset to type GncTransPropType::NONE.
 * @param multi_split_val Boolean value with desired state (multi-split
 * vs two-split).
 */
void GncTxImport::multi_split (bool multi_split)
{
    m_settings.m_multi_split = multi_split;
    for (auto col_it = m_settings.m_column_types.begin(); col_it != m_settings.m_column_types.end();
            col_it++)
    {
        auto san_prop = sanitize_trans_prop (*col_it, m_settings.m_multi_split);
        if (san_prop != *col_it)
            *col_it = san_prop;
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
 * @param base_acct Pointer to an account or NULL.
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
        auto col_type = std::find (m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::ACCOUNT);
        if (col_type != m_settings.m_column_types.end())
            *col_type = GncTransPropType::NONE;
    }
}

Account *GncTxImport::base_account () { return m_settings.m_base_account; }

void GncTxImport::currency_format (int currency_format)
    { m_settings.m_currency_format = currency_format; }
int GncTxImport::currency_format () { return m_settings.m_currency_format; }

void GncTxImport::date_format (int date_format)
    { m_settings.m_date_format = date_format; }
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
        m_tokenizer->encoding(encoding); // May throw

    m_settings.m_encoding = encoding;
}

std::string GncTxImport::encoding () { return m_settings.m_encoding; }

void GncTxImport::update_skipped_lines()
{
    for (uint i = 0; i < m_parsed_lines.size(); i++)
    {
        std::get<4>(m_parsed_lines[i]) =
            ((i < skip_start_lines()) ||             // start rows to skip
             (i >= m_parsed_lines.size() - skip_end_lines()) ||          // end rows to skip
             (((i - skip_start_lines()) % 2 == 1) && // skip every second row...
                  skip_alt_lines()) ||                   // ...if requested
             (m_skip_errors && !std::get<1>(m_parsed_lines[i]).empty())); // skip lines with errors
    }
}

void GncTxImport::skip_start_lines (uint num)
{
    m_settings.m_skip_start_lines = num;
    update_skipped_lines();
}

uint GncTxImport::skip_start_lines () { return m_settings.m_skip_start_lines; }

void GncTxImport::skip_end_lines (uint num)
{
    m_settings.m_skip_end_lines = num;
    update_skipped_lines();
}

uint GncTxImport::skip_end_lines () { return m_settings.m_skip_end_lines; }

void GncTxImport::skip_alt_lines (bool skip)
{
    m_settings.m_skip_alt_lines = skip;
    update_skipped_lines();
}

bool GncTxImport::skip_alt_lines () { return m_settings.m_skip_alt_lines; }

void GncTxImport::skip_errors (bool skip)
{
    m_skip_errors = skip;
    update_skipped_lines();
}

bool GncTxImport::skip_errors () { return m_skip_errors; }

void GncTxImport::separators (std::string separators)
{
    if (file_format() != GncImpFileFormat::CSV)
        return;

    m_settings.m_separators = separators;
    auto csvtok = dynamic_cast<GncCsvTokenizer*>(m_tokenizer.get());
    csvtok->set_separators (separators);

}
std::string GncTxImport::separators () { return m_settings.m_separators; }

void GncTxImport::settings (const CsvTransSettings& settings)
{
    m_settings = settings;
    file_format (m_settings.m_file_format);
    multi_split (m_settings.m_multi_split);
    base_account (m_settings.m_base_account);
    encoding (m_settings.m_encoding);
    separators (m_settings.m_separators);
    try
    {
        tokenize(false);
    }
    catch (...)
    { };

}

bool GncTxImport::save_settings ()
{

    if (trans_preset_is_reserved_name (m_settings.m_name))
        return true;
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
 *        - if guessColTypes is TRUE, all the column types will be set
 *          GncTransPropType::NONE right now as real guessing isn't implemented yet
 * @param guessColTypes true to guess what the types of columns are based on the cell contents
 * @exception std::range_error if tokenizing failed
 */
void GncTxImport::tokenize (bool guessColTypes)
{
    if (!m_tokenizer)
        return;

    uint max_cols = 0;
    m_tokenizer->tokenize();
    m_parsed_lines.clear();
    for (auto tokenized_line : m_tokenizer->get_tokens())
    {
        m_parsed_lines.push_back (std::make_tuple (tokenized_line, std::string(),
                nullptr, nullptr, false));
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

    m_settings.m_column_types.resize(max_cols, GncTransPropType::NONE);

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
    bool empty() { return m_error.empty(); }
private:
    std::string m_error;
};

void ErrorList::add_error (std::string msg)
{
    m_error += "- " + msg + "\n";
}

std::string ErrorList::str()
{
    return m_error.substr(0, m_error.size() - 1);
}

void GncTxImport::verify_data(ErrorList& error_msg)
{
    auto have_date_errors = false;
    auto have_amount_errors = false;
    for (uint i = 0; i < m_parsed_lines.size(); i++)
    {
        if (std::get<4>(m_parsed_lines[i])) // Ignore skipped lines
            continue;
        else
        {
            auto line_err = ErrorList();
            auto line_data = std::get<0>(m_parsed_lines[i]);

            /* Attempt to parse date column values */
            auto date_col_it = std::find(m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::DATE);
            if (date_col_it != m_settings.m_column_types.end())
            try
            {
                auto date_col = date_col_it -m_settings.m_column_types.begin();
                auto date_str = line_data[date_col];
                if (!m_settings.m_multi_split || !date_str.empty())
                    parse_date (date_str, date_format());
            }
            catch (...)
            {
                have_date_errors = true;
                line_err.add_error(_("Date could not be understood"));
            }

            /* Attempt to parse reconcile date column values */
            date_col_it = std::find(m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::REC_DATE);
            if (date_col_it != m_settings.m_column_types.end())
            try
            {
                auto date_col = date_col_it -m_settings.m_column_types.begin();
                auto date_str = line_data[date_col];
                if (!date_str.empty())
                    parse_date (date_str, date_format());
            }
            catch (...)
            {
                have_date_errors = true;
                line_err.add_error(_("Reconcile date could not be understood"));
            }

            /* Attempt to parse transfer reconcile date column values */
            date_col_it = std::find(m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::REC_DATE);
            if (date_col_it != m_settings.m_column_types.end())
            try
            {
                auto date_col = date_col_it -m_settings.m_column_types.begin();
                auto date_str = line_data[date_col];
                if (!date_str.empty())
                    parse_date (date_str, date_format());
            }
            catch (...)
            {
                have_date_errors = true;
                line_err.add_error(_("Transfer reconcile date could not be understood"));
            }

            /* Attempt to parse deposit column values */
            auto num_col_it = std::find(m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::DEPOSIT);
            if (num_col_it != m_settings.m_column_types.end())
            try
            {
                auto num_col = num_col_it -m_settings.m_column_types.begin();
                auto num_str = line_data[num_col];
                if (!m_settings.m_multi_split || !num_str.empty())
                    parse_amount (num_str, currency_format());
            }
            catch (...)
            {
                have_amount_errors = true;
                line_err.add_error(_("Deposit amount could not be understood"));
            }

            /* Attempt to parse withdrawal column values */
            num_col_it = std::find(m_settings.m_column_types.begin(),
                m_settings.m_column_types.end(), GncTransPropType::WITHDRAWAL);
            if (num_col_it != m_settings.m_column_types.end())
            try
            {
                auto num_col = num_col_it -m_settings.m_column_types.begin();
                auto num_str = line_data[num_col];
                if (!m_settings.m_multi_split || !num_str.empty())
                    parse_amount (num_str, currency_format());
            }
            catch (...)
            {
                have_amount_errors = true;
                line_err.add_error(_("Withdrawal amount could not be understood"));
            }

            if (!line_err.empty())
                std::get<1>(m_parsed_lines[i]) = line_err.str();
            else
                std::get<1>(m_parsed_lines[i]).clear();
        }
    }

    if (have_date_errors)
        error_msg.add_error( _("Not all dates could be parsed. Please verify your chosen date format or adjust the lines to skip."));
    if (have_amount_errors)
        error_msg.add_error( _("Not all amounts could be parsed. Please verify your chosen currency format or adjust the lines to skip."));
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

    /* Verify at least one amount column (deposit or withdrawal) column is selected.
     */
    if (!check_for_column_type(GncTransPropType::DEPOSIT) &&
        !check_for_column_type(GncTransPropType::WITHDRAWAL))
        error_msg.add_error( _("Please select a deposit or withdrawal column."));

    /* Verify a transfer account is selected if any of the other transfer properties
     * are selected.
     */
    if ((check_for_column_type(GncTransPropType::TACTION) ||
         check_for_column_type(GncTransPropType::TMEMO) ||
         check_for_column_type(GncTransPropType::TREC_STATE) ||
         check_for_column_type(GncTransPropType::TREC_DATE)) &&
        !check_for_column_type(GncTransPropType::TACCOUNT))
        error_msg.add_error( _("Please select a transfer account column or remove the other transfer related columns."));
}


/* Test for the required minimum number of columns selected and
 * a valid date format.
 * @return An empty string if all checks passed or the reason
 *         verification failed otherwise.
 */
std::string GncTxImport::verify ()
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
    verify_data (error_msg);
    return error_msg.str();
}


/** Checks whether the parsed line contains all essential properties.
 * Essential properties are
 * - "Date"
 * - at least one of "Deposit", or "Withdrawal"
 * - "Account"
 * Note account isn't checked for here as this has been done before
 * @param parsed_line The line we are checking
 * @exception std::invalid_argument in an essential property is missing
 */
static void trans_properties_verify_essentials (std::vector<parse_line_t>::iterator& parsed_line)
{
    std::string error_message;
    std::shared_ptr<GncPreTrans> trans_props;
    std::shared_ptr<GncPreSplit> split_props;

    std::tie(std::ignore, error_message, trans_props, split_props, std::ignore) = *parsed_line;

    auto trans_error = trans_props->verify_essentials();
    auto split_error = split_props->verify_essentials();

    error_message.clear();
    if (!trans_error.empty())
    {
        error_message += trans_error;
        if (!split_error.empty())
            error_message += "\n";
    }
    if (!split_error.empty())
        error_message += split_error;

    if (!error_message.empty())
        throw std::invalid_argument(error_message);
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
    std::string error_message;
    std::shared_ptr<GncPreTrans> trans_props;
    std::shared_ptr<GncPreSplit> split_props;
    std::tie(std::ignore, error_message, trans_props, split_props, std::ignore) = *parsed_line;
    auto account = split_props->get_account();

    QofBook* book = gnc_account_get_book (account);
    gnc_commodity* currency = xaccAccountGetCommodity (account);

    auto trans = trans_props->create_trans (book, currency);

    if (trans)
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
        m_current_draft = std::make_shared<DraftTransaction>(trans);
        m_current_draft->void_reason = trans_props->get_void_reason();
        created_trans = true;
    }
    else if (m_settings.m_multi_split)  // in multi_split mode create_trans will return a nullptr for all but the first split
        trans = m_current_draft->trans;
    else // in non-multi-split mode each line should be a transaction, so not having one here is an error
        throw std::invalid_argument ("Failed to create transaction from selected columns.");

    if (!trans)
        return nullptr;

    split_props->create_split(trans);

    /* Only return the draft transaction if we really created a new one
     * The return value will be added to a list for further processing,
     * we want each transaction to appear only once in that list.
     */
    return created_trans ? m_current_draft : nullptr;
}

void GncTxImport::create_transaction (std::vector<parse_line_t>::iterator& parsed_line)
{
    StrVec line;
    std::string error_message;
    auto trans_props = std::make_shared<GncPreTrans>(date_format());
    auto split_props = std::make_shared<GncPreSplit>(date_format(), currency_format());
    std::tie(line, error_message, std::ignore, std::ignore, std::ignore) = *parsed_line;
    error_message.clear();

    /* Convert all tokens in this line into transaction/split properties. */
    auto col_types_it = m_settings.m_column_types.cbegin();
    auto line_it = line.cbegin();
    for (col_types_it, line_it;
            col_types_it != m_settings.m_column_types.cend() &&
            line_it != line.cend();
            ++col_types_it, ++line_it)
    {
        try
        {
            if (*col_types_it == GncTransPropType::NONE)
                continue; /* We do nothing with "None"-type columns. */
            else if  (*col_types_it <= GncTransPropType::TRANS_PROPS)
            {
                if (m_settings.m_multi_split && line_it->empty())
                    continue; // In multi-split mode, transaction properties can be empty
                trans_props->set_property(*col_types_it, *line_it);
            }
            else
                split_props->set_property(*col_types_it, *line_it);
        }
        catch (const std::exception& e)
        {
            if (!error_message.empty())
                error_message += "\n";
            error_message += _(gnc_csv_col_type_strs[*col_types_it]);
            error_message += _(" column could not be understood.");
            PINFO("User warning: %s", error_message.c_str());
        }
    }
    std::get<2>(*parsed_line) = trans_props;

    /* For multi-split input data, we need to check whether this line is part of a transaction that
     * has already be started by a previous line. */
    if (m_settings.m_multi_split)
    {
        if (trans_props->is_part_of(m_parent))
        {
            /* This line is part of an already started transaction
             * continue with that one instead to make sure the split from this line
             * gets added to the proper transaction */
            std::get<2>(*parsed_line) = m_parent;

            /* Check if the parent line is ready for conversion. If not,
             * this child line can't be converted either.
             */
            if (!m_parent->verify_essentials().empty())
                error_message = _("First line of this transaction has errors.");
        }
        else
        {
            /* This line starts a new transaction, set it as parent for
             * subsequent lines. */
            m_parent = trans_props;
        }
    }

    if (!error_message.empty())
        throw std::invalid_argument (error_message);

    // Add an ACCOUNT property with the default account if no account column was set by the user
    auto line_acct = split_props->get_account();
    if (!line_acct)
    {
        if (m_settings.m_base_account)
            split_props->set_account(m_settings.m_base_account);
        else
        {
            // Oops - the user didn't select an Account column *and* we didn't get a default value either!
            // Note if you get here this suggests a bug in the code!
            error_message = _("No account column selected and no default account specified either.\n"
                                       "This should never happen. Please report this as a bug.");
            PINFO("User warning: %s", error_message.c_str());
            throw std::invalid_argument(error_message);
        }
    }
    std::get<3>(*parsed_line) = split_props;

    /* If column parsing was successful, convert trans properties into a draft transaction. */
    try
    {
        trans_properties_verify_essentials (parsed_line);

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
        error_message = e.what();
        PINFO("User warning: %s", error_message.c_str());
    }
}


/** Creates a list of transactions from parsed data. The parsed data
 * will first be validated. If any errors are found this function will
 * throw an error unless skip_errors was set.
 * @param skip_errors true skip over lines with errors
 * @exception throws std::invalid_argument if data validation fails and
 *            skip_errors wasn't set.
 */
void GncTxImport::create_transactions ()
{
    /* Start with verifying the current data. */
    auto verify_result = verify();
    if (!verify_result.empty() && !m_skip_errors)
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
        if ((std::get<4>(*parsed_lines_it)))
            continue;

        try
        {
            create_transaction (parsed_lines_it);
        }
        catch (const std::invalid_argument& e)
        {
            std::get<1>(*parsed_lines_it) = e.what();
            continue;
        }
    }
}


bool
GncTxImport::check_for_column_type (GncTransPropType type)
{
    return (std::find (m_settings.m_column_types.begin(),
                       m_settings.m_column_types.end(), type)
                        != m_settings.m_column_types.end());
}

void
GncTxImport::set_column_type (uint position, GncTransPropType type)
{
    if (position >= m_settings.m_column_types.size())
        return;

    // Column types should be unique, so remove any previous occurrence of the new type
    std::replace(m_settings.m_column_types.begin(), m_settings.m_column_types.end(),
            type, GncTransPropType::NONE);
    m_settings.m_column_types.at (position) = type;

    // If the user has set an Account column, we can't have a base account set
    if (type == GncTransPropType::ACCOUNT)
        base_account (nullptr);
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
    uint acct_col = acct_col_it - m_settings.m_column_types.begin();
    auto tacct_col_it = std::find (m_settings.m_column_types.begin(),
                           m_settings.m_column_types.end(), GncTransPropType::TACCOUNT);
    uint tacct_col = tacct_col_it - m_settings.m_column_types.begin();

    /* Iterate over all parsed lines */
    auto odd_line = false;
    for (auto parsed_line : m_parsed_lines)
    {
        /* Skip current line if the user specified so */
        if ((std::get<4>(parsed_line)))
            continue;

        auto col_strs = std::get<0>(parsed_line);
        if ((acct_col_it != m_settings.m_column_types.end()) && !col_strs[acct_col].empty())
            accts.insert(col_strs[acct_col]);
        if ((tacct_col_it != m_settings.m_column_types.end()) && !col_strs[tacct_col].empty())
            accts.insert(col_strs[tacct_col]);
    }

    return accts;
}

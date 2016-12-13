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
    date_format = -1;
    currency_format = 0;
    skip_start_lines = 0;
    skip_end_lines = 0;
    skip_alt_lines = FALSE;
    parse_errors = false;
    multi_split = false;

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
 *  @exception std::ifstream::failure if file reloading fails
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
 * @param encoding Encoding that data should be translated using
 */
void GncTxImport::convert_encoding (const std::string& encoding)
{
    // TODO investigate if we can catch conversion errors and report them
    if (tokenizer)
        tokenizer->encoding(encoding);
}

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
    uint max_cols = 0;
    tokenizer->tokenize();
    parsed_lines.clear();
    for (auto tokenized_line : tokenizer->get_tokens())
    {
        parsed_lines.push_back (std::make_tuple (tokenized_line, std::string(),
                nullptr, nullptr));
        auto length = tokenized_line.size();
        if (length > max_cols)
            max_cols = length;
    }

    /* If it failed, generate an error. */
    if (parsed_lines.size() == 0)
    {
        throw (std::range_error ("Tokenizing failed."));
        return;
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

    std::tie(std::ignore, error_message, trans_props, split_props) = *parsed_line;

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
    std::tie(std::ignore, error_message, trans_props, split_props) = *parsed_line;
    auto account = split_props->get_account();

    QofBook* book = gnc_account_get_book (account);
    gnc_commodity* currency = xaccAccountGetCommodity (account);

    auto trans = trans_props->create_trans (book, currency);

    if (trans)
    {
        /* We're about to continue with a new transaction
         * Time to do some closing actions on the previous one
         */
        if (current_draft && current_draft->void_reason)
        {
            /* The import data specifies this transaction was voided.
             * So void the created transaction as well.
             * Attention: this assumes the imported transaction was balanced.
             * If not, this will cause an imbalance split to be added automatically!
             */
            xaccTransCommitEdit (current_draft->trans);
            xaccTransVoid (current_draft->trans, current_draft->void_reason->c_str());
        }
        current_draft = std::make_shared<DraftTransaction>(trans);
        current_draft->void_reason = trans_props->get_void_reason();
        created_trans = true;
    }
    else if (multi_split)  // in multi_split mode create_trans will return a nullptr for all but the first split
        trans = current_draft->trans;
    else // in non-multi-split mode each line should be a transaction, so not having one here is an error
        throw std::invalid_argument ("Failed to create transaction from selected columns.");

    if (!trans)
        return nullptr;

    split_props->create_split(trans);

    /* Only return the draft transaction if we really created a new one
     * The return value will be added to a list for further processing,
     * we want each transaction to appear only once in that list.
     */
    return created_trans ? current_draft : nullptr;
}

void GncTxImport::create_transaction (std::vector<parse_line_t>::iterator& parsed_line)
{
    StrVec line;
    std::string error_message;
    auto trans_props = std::make_shared<GncPreTrans>(date_format);
    auto split_props = std::make_shared<GncPreSplit>(date_format, currency_format);
    std::tie(line, error_message, std::ignore, std::ignore) = *parsed_line;
    error_message.clear();

    /* Convert all tokens in this line into transaction/split properties. */
    auto col_types_it = column_types.cbegin();
    auto line_it = line.cbegin();
    for (col_types_it, line_it;
            col_types_it != column_types.cend() &&
            line_it != line.cend();
            ++col_types_it, ++line_it)
    {
        try
        {
            if (*col_types_it == GncTransPropType::NONE)
                continue; /* We do nothing with "None"-type columns. */
            else if  (*col_types_it <= GncTransPropType::TRANS_PROPS)
            {
                if (multi_split && line_it->empty())
                    continue; // In multi-split mode, transaction properties can be empty
                trans_props->set_property(*col_types_it, *line_it);
            }
            else
                split_props->set_property(*col_types_it, *line_it);
        }
        catch (const std::exception& e)
        {
            parse_errors = true;
            if (!error_message.empty())
                error_message += "\n";
            error_message += _(gnc_csv_col_type_strs[*col_types_it]);
            error_message += _(" column could not be understood.");
            PINFO("User warning: %s", error_message.c_str());
        }
    }

    /* For multi-split input data, we need to check whether this line is part of a transaction that
     * has already be started by a previous line. */
    if (multi_split)
    {
        if (trans_props->is_part_of(parent))
        {
            /* This line is part of an already started transaction
             * continue with that one instead to make sure the split from this line
             * gets added to the proper transaction */
            std::get<2>(*parsed_line) = parent;

            /* Check if the parent line is ready for conversion. If not,
             * this child line can't be converted either.
             */
            if (!parent->verify_essentials().empty())
                error_message = _("First line of this transaction has errors.");
        }
        else
        {
            std::get<2>(*parsed_line) = trans_props;
            /* This line starts a new transaction, set it as parent for
             * subsequent lines. */
            parent = trans_props;
        }
    }

    if (!error_message.empty())
        throw std::invalid_argument (error_message);

    // Add an ACCOUNT property with the default account if no account column was set by the user
    auto line_acct = split_props->get_account();
    if (!line_acct)
    {
        if (base_account)
            split_props->set_account(base_account);
        else
        {
            // Oops - the user didn't select an Account column *and* we didn't get a default value either!
            // Note if you get here this suggests a bug in the code!
            parse_errors = true;
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
            transactions.insert (std::pair<time64, std::shared_ptr<DraftTransaction>>(trans_date,std::move(draft_trans)));
        }
    }
    catch (const std::invalid_argument& e)
    {
        parse_errors = true;
        error_message = e.what();
        PINFO("User warning: %s", error_message.c_str());
    }
}


/** Creates a list of transactions from parsed data. Transactions that
 * could be created from rows are placed in transactions; Lines that couldn't
 * be converted are marked with the failure reason. These can be redone in
 * a subsequent run with redo_errors set to true.
 * @param account Account with which transactions are created
 * @param redo_errors true to convert only error data, false to convert all data
 */
void GncTxImport::create_transactions (Account* account,
                                       bool redo_errors)
{
    /* If a full conversion is requested (as opposed to only
     * attempting to convers the lines which had errors in the previous run)
     * clear all errors and possibly already created transactions. */
    if (!redo_errors)
    {
        /* Clear error messages on full run */
        for (auto orig_line : parsed_lines)
            std::get<1>(orig_line).clear();

        /* Drop all existing draft transactions on a full run */
        transactions.clear();
    }

    /* compute start and end iterators based on user-set restrictions */
    auto parsed_lines_it = parsed_lines.begin();
    std::advance(parsed_lines_it, skip_start_lines);

    auto parsed_lines_max = parsed_lines.begin();
    std::advance(parsed_lines_max, parsed_lines.size() - skip_end_lines);

    base_account = account;
    auto odd_line = false;
    parse_errors = false;
    parent = nullptr;

    /* Iterate over all parsed lines */
    for (parsed_lines_it, odd_line;
            parsed_lines_it < parsed_lines_max;
            ++parsed_lines_it, odd_line = !odd_line)
    {
        /* Skip current line if:
           1. only looking for lines with error AND no error on current line
           OR
           2. looking for all lines AND
              skip_rows is enabled AND
              current line is an odd line */
        if ((redo_errors && std::get<1>(*parsed_lines_it).empty()) ||
           (!redo_errors && skip_alt_lines && odd_line))
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
    return (std::find (column_types.begin(), column_types.end(), type) != column_types.end());
}

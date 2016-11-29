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

//const int num_date_formats = 5;
//const gchar* date_format_user[] = {N_("y-m-d"),
//                                   N_("d-m-y"),
//                                   N_("m-d-y"),
//                                   N_("d-m"),
//                                   N_("m-d")
//                                  };
//
//const int num_currency_formats = 3;
//const gchar* currency_format_user[] = {N_("Locale"),
//                                       N_("Period: 123,456.78"),
//                                       N_("Comma: 123.456,78")
//                                      };
//

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
    start_row = 0;
    end_row = 1000;
    skip_rows = FALSE;
    parse_errors = false;

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
 *  @exception the reloading of the file may throw std::ifstream::failure
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
 * @param parse_data Data that is being parsed
 * @param encoding Encoding that data should be translated using
 */
void GncTxImport::convert_encoding (const std::string& encoding)
{
    // TODO investigate if we can catch conversion errors and report them
    if (tokenizer)
        tokenizer->encoding(encoding);
}

/** Loads a file into a GncTxImport. This is the first function
 * that must be called after creating a new GncTxImport. If this
 * fails because the file couldn't be opened, no more functions can be
 * called on the parse data until this succeeds (or until it fails
 * because of an encoding guess error). If it fails because the
 * encoding could not be guessed, gnc_csv_convert_encoding must be
 * called until it succeeds.
 * @param parse_data Data that is being parsed
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

/** Parses a file into cells. This requires having an encoding that
 * works (see gnc_csv_convert_encoding). options should be
 * set according to how the user wants before calling this
 * function. (Note: this function must be called with guessColTypes as
 * TRUE before it is ever called with it as FALSE.) (Note: if
 * guessColTypes is TRUE, all the column types will be GncTransPropType::NONE
 * right now.)
 * @param guessColTypes TRUE to guess what the types of columns are based on the cell contents
 * @exception throws std::range_error if parsing failed
 */
void GncTxImport::parse (bool guessColTypes)
{
    uint max_cols = 0;
    tokenizer->tokenize();
    orig_lines.clear();
    for (auto tokenized_line : tokenizer->get_tokens())
    {
        orig_lines.push_back (std::make_tuple (tokenized_line, std::string(),
                std::make_shared<GncPreTrans>(), std::make_shared<GncPreSplit>()));
        auto length = tokenized_line.size();
        if (length > max_cols)
            max_cols = length;
    }

    /* If it failed, generate an error. */
    if (orig_lines.size() == 0)
    {
        throw (std::range_error ("Parsing failed."));
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
 * - at least one of "Balance", "Deposit", or "Withdrawal"
 * - "Account"
 * Note account isn't checked for here as this has been done before
 * @param parse_line The line we are checking
 * @exception std::invalid_argument in an essential property is missing
 */
static void trans_properties_verify_essentials (parse_line_t& orig_line)
{
    std::string error_message;
    std::shared_ptr<GncPreTrans> trans_props;
    std::shared_ptr<GncPreSplit> split_props;

    std::tie(std::ignore, error_message, trans_props, split_props) = orig_line;

    auto trans_error = trans_props->verify_essentials();
    auto split_error = split_props->verify_essentials();

    error_message.clear();
    if (!trans_error.empty())
    {
        error_message = trans_error;
        if (!split_error.empty())
            error_message += "\n";
    }
    if (!split_error.empty())
        error_message += split_error;

    if (!error_message.empty())
        throw std::invalid_argument(error_message);
}
/** Create a Transaction from a map of transaction properties.
 * Note: this function assumes all properties in the map have been verified
 *       to be valid. No further checks are performed here other than that
 *       the required properties are in the map
 * @param orig_line The current line being parsed
 * @return On success, a GncCsvTransLine; on failure, the trans pointer is NULL
 */
static GncCsvTransLine* trans_properties_to_trans (parse_line_t& orig_line)
{
    std::string error_message;
    std::shared_ptr<GncPreTrans> trans_props;
    std::shared_ptr<GncPreSplit> split_props;
    std::tie(std::ignore, error_message, trans_props, split_props) = orig_line;
    auto account = split_props->get_account();

    QofBook* book = gnc_account_get_book (account);
    gnc_commodity* currency = xaccAccountGetCommodity (account);

    auto trans = trans_props->create_trans (book, currency);

    if (!trans)
        return nullptr;

    GncCsvTransLine* trans_line = g_new (GncCsvTransLine, 1);
    trans_line->balance_set = false;
    trans_line->balance = gnc_numeric_zero();

    auto balance = split_props->create_split(trans);
    if (balance)
    {
        trans_line->balance_set = true;
        trans_line->balance = *balance;
    }

    return trans_line;
}

void GncTxImport::adjust_balances (Account *account)
{
    Split      *split, *osplit;

    /* balance_offset is how much the balance currently in the account
     * differs from what it will be after the transactions are
     * imported. This will be sum of all the previous transactions for
     * any given transaction. */
    auto balance_offset = double_to_gnc_numeric (0.0, xaccAccountGetCommoditySCU (account),
                                 GNC_HOW_RND_ROUND_HALF_UP);
    for (auto trans_iter : transactions)
    {
        auto trans_line = trans_iter.second;
        if (trans_line->balance_set)
        {
            time64 date = xaccTransGetDate (trans_line->trans);
            /* Find what the balance should be by adding the offset to the actual balance. */
            gnc_numeric existing_balance = gnc_numeric_add (balance_offset,
                                           xaccAccountGetBalanceAsOfDate (account, date),
                                           xaccAccountGetCommoditySCU (account),
                                           GNC_HOW_RND_ROUND_HALF_UP);

            /* The amount of the transaction is the difference between the new and existing balance. */
            gnc_numeric amount = gnc_numeric_sub (trans_line->balance,
                                                 existing_balance,
                                                 xaccAccountGetCommoditySCU (account),
                                                 GNC_HOW_RND_ROUND_HALF_UP);

            // Find home account split
            split  = xaccTransFindSplitByAccount (trans_line->trans, account);
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
                                             xaccAccountGetCommoditySCU (account),
                                             GNC_HOW_RND_ROUND_HALF_UP);
        }
    }

}

void GncTxImport::parse_line_to_trans (parse_line_t& orig_line)
{
    StrVec line;
    std::string error_message;
    std::shared_ptr<GncPreTrans> trans_props;
    std::shared_ptr<GncPreSplit> split_props;
    std::tie(line, error_message, trans_props, split_props) = orig_line;
    error_message.clear();

    /* Convert this import line into a map of transaction/split properties. */
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
                trans_props->set_property(*col_types_it, *line_it, date_format);
            else
                split_props->set_property(*col_types_it, *line_it, currency_format);
        }
        catch (const std::invalid_argument&)
        {
            parse_errors = true;
            error_message += _(gnc_csv_col_type_strs[*col_types_it]);
            error_message += _(" column could not be understood.");
            error_message += "\n";
        }
    }

    if (!error_message.empty())
        throw std::invalid_argument(error_message);

    // Add an ACCOUNT property with the default account if no account column was set by the user
    auto line_acct = split_props->get_account();
    if (!line_acct)
    {
        if (home_account)
            split_props->set_account(home_account);
        else
        {
            // Oops - the user didn't select an Account column *and* we didn't get a default value either!
            // Note if you get here this suggests a bug in the code!
            parse_errors = true;
            error_message = _("No account column selected and no default account specified either.\n"
                                       "This should never happen. Please report this as a bug.");
            throw std::invalid_argument(error_message);
        }
    }

    /* If column parsing was successful, convert trans properties into a trans line. */
    try
    {
        trans_properties_verify_essentials (orig_line);

        /* If all went well, add this transaction to the list. */
        /* We want to keep the transactions sorted by date in case we have
         * to calculate the transaction's amount based on the user provided balances.
         * The multimap should deal with this for us. */
        auto trans_line = trans_properties_to_trans (orig_line);
        if (trans_line)
        {
            auto trans_date = xaccTransGetDate (trans_line->trans);
            transactions.insert (std::pair<time64, GncCsvTransLine*>(trans_date,trans_line));
        }
    }
    catch (const std::invalid_argument& e)
    {
        parse_errors = true;
        error_message = e.what();
    }
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
int GncTxImport::parse_to_trans (Account* account,
                                     bool redo_errors)
{
    /* Free error_lines and transactions if they
     * already exist. */
    if (!redo_errors) /* If we're redoing errors, we save freeing until the end. */
    {
        for (auto orig_line : orig_lines)
            std::get<1>(orig_line).clear();

        /* FIXME handle memory leak here!
         * Existing transactions in the map should probably removed before emptying the map
         */
        if (!transactions.empty())
            transactions.clear();
    }

    /* compute start and end iterators based on user-set restrictions */
    auto orig_lines_it = orig_lines.begin();
    std::advance(orig_lines_it, start_row);

    auto orig_lines_max = orig_lines.begin();
    if (end_row > orig_lines.size())
        orig_lines_max = orig_lines.end();
    else
        std::advance(orig_lines_max, end_row);

    home_account = account;
    auto odd_line = false;
    parse_errors = false;
    for (orig_lines_it, odd_line;
            orig_lines_it != orig_lines_max;
            ++orig_lines_it, odd_line = !odd_line)
    {
        auto orig_line = *orig_lines_it;

        /* Skip current line if:
           1. only looking for lines with error AND no error on current line
           OR
           2. looking for all lines AND
              skip_rows is enabled AND
              current line is an odd line */
        if ((redo_errors && std::get<1>(orig_line).empty()) ||
           (!redo_errors && skip_rows && odd_line))
            continue;

        try
        {
            parse_line_to_trans (orig_line);
        }
        catch (const std::invalid_argument&)
        {
            continue;
        }
    }

    if (std::find(column_types.begin(),column_types.end(), GncTransPropType::BALANCE) !=
        column_types.end()) // This is only used if we have one home account
        adjust_balances (home_account);

    return 0;
}


bool
GncTxImport::check_for_column_type (GncTransPropType type)
{
    return (std::find (column_types.begin(), column_types.end(), type) != column_types.end());
}

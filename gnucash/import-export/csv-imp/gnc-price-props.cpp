/********************************************************************\
 * gnc-price-props.cpp - encapsulate price properties for use       *
 *                       in the csv importer                        *
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

extern "C" {
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib.h>
#include <glib/gi18n.h>

#include "engine-helpers.h"
#include "gnc-ui-util.h"
#include "gnc-pricedb.h"

}

#include <string>
#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>
#include "gnc-price-props.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

/* This map contains a set of strings representing the different column types. */
std::map<GncPricePropType, const char*> gnc_csv_price_col_type_strs = {
        { GncPricePropType::NONE, N_("None") },
        { GncPricePropType::DATE, N_("Date") },
        { GncPricePropType::AMOUNT, N_("Amount") },
        { GncPricePropType::CURRENCY_FROM, N_("Currency From") },
        { GncPricePropType::CURRENCY_TO, N_("Currency To") },
        { GncPricePropType::SYMBOL_FROM, N_("Symbol From") },
};

/* Regular expressions used to parse dates per date format */
const char* date_regex_price[] = {
                             "(?:"                                   // either y-m-d
                                 "(?<YEAR>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)"
                             "|"                                     // or CCYYMMDD
                                 "(?<YEAR>[0-9]{4})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                             ")",

                             "(?:"                                   // either d-m-y
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<YEAR>[0-9]+)"
                             "|"                                     // or DDMMCCYY
                                 "(?<DAY>[0-9]{2})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<YEAR>[0-9]{4})"
                             ")",

                             "(?:"                                   // either m-d-y
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<YEAR>[0-9]+)"
                             "|"                                     // or MMDDCCYY
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                                 "(?<YEAR>[0-9]{4})"
                             ")",

                             "(?:"                                   // either d-m(-y)
                                 "(?<DAY>[0-9]+)[-/.' ]+"
                                 "(?<MONTH>[0-9]+)(?:[-/.' ]+"
                                 "(?<YEAR>[0-9]+))?"
                             "|"                                     // or DDMM(CCYY)
                                 "(?<DAY>[0-9]{2})"
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<YEAR>[0-9]+)?"
                             ")",

                             "(?:"                                   // either m-d(-y)
                                 "(?<MONTH>[0-9]+)[-/.' ]+"
                                 "(?<DAY>[0-9]+)(?:[-/.' ]+"
                                 "(?<YEAR>[0-9]+))?"
                             "|"                                     // or MMDD(CCYY)
                                 "(?<MONTH>[0-9]{2})"
                                 "(?<DAY>[0-9]{2})"
                                 "(?<YEAR>[0-9]+)?"
                             ")",
};

/** Parses a string into a date, given a format. This function
 * requires only knowing the order in which the year, month and day
 * appear. For example, 01-02-2003 will be parsed the same way as
 * 01/02/2003.
 * @param date_str The string containing a date being parsed
 * @param format An index specifying a format in date_format_user
 * @exception std::invalid_argument if the string can't be parsed into a date.
 * @return The parsed value of date_str on success, throws on failure
 */

time64 parse_date_price (const std::string &date_str, int format)
{
    boost::regex r(date_regex_price[format]);
    boost::smatch what;
    if(!boost::regex_search(date_str, what, r))
        throw std::invalid_argument (_("Value can't be parsed into a date using the selected date format."));  // regex didn't find a match

    // Attention: different behavior from 2.6.x series !
    // If date format without year was selected, the match
    // should NOT have found a year.
    if ((format >= 3) && (what.length("YEAR") != 0))
        throw std::invalid_argument (_("Value appears to contain a year while the selected format forbids this."));

    auto day = std::stoi (what.str("DAY"));
    auto month = std::stoi (what.str("MONTH"));

    int year;
    if (format < 3)
    {
        /* The input dates have a year, so use that one */
        year = std::stoi (what.str("YEAR"));

        /* Handle two-digit years. */
        if (year < 100)
        {
            /* We allow two-digit years in the range 1969 - 2068. */
            if (year < 69)
                year += 2000;
            else
                year += 1900;
        }
    }
    else
    {
        /* The input dates don't have a year, so work with today's year.
         */
        gnc_timespec2dmy(timespec_now(), nullptr, nullptr, &year);
    }

    auto ts = gnc_dmy2timespec_neutral(day, month, year);
    if (ts.tv_sec == INT64_MAX)
        throw std::invalid_argument (_("Value can't be parsed into a date using the selected date format."));

    return ts.tv_sec;
}


/** Convert str into a GncRational using the user-specified (import) currency format.
 * @param str The string to be parsed
 * @param currency_format The currency format to use.
 * @return a GncNumeric
 * @exception May throw std::invalid argument if string can't be parsed properly
 */
GncNumeric parse_amount_price (const std::string &str, int currency_format)
{
    /* If a cell is empty or just spaces return invalid amount */
    if(!boost::regex_search(str, boost::regex("[0-9]")))
        throw std::invalid_argument (_("Value doesn't appear to contain a valid number."));

    auto expr = boost::make_u32regex("[[:Sc:]]");
    std::string str_no_symbols = boost::u32regex_replace(str, expr, "");

    /* Convert based on user chosen currency format */
    gnc_numeric val;
    char *endptr;
    switch (currency_format)
    {
    case 0:
        /* Currency locale */
        if (!(xaccParseAmount (str_no_symbols.c_str(), TRUE, &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    case 1:
        /* Currency decimal period */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', '.', ',', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    case 2:
        /* Currency decimal comma */
        if (!(xaccParseAmountExtended (str_no_symbols.c_str(), TRUE, '-', ',', '.', "\003\003", "$+", &val, &endptr)))
            throw std::invalid_argument (_("Value can't be parsed into a number using the selected currency format."));
        break;
    }

    return GncNumeric(val);
}

gnc_commodity* parse_commodity_price_comm (const std::string& comm_str)
{
    if (comm_str.empty())
        return nullptr;

    auto table = gnc_commodity_table_get_table (gnc_get_current_book());
    gnc_commodity* comm = nullptr;

    /* First try commodity as a unique name. */
    if (comm_str.find("::"))
        comm = gnc_commodity_table_lookup_unique (table, comm_str.c_str());

    /* Then try mnemonic in the currency namespace */
    if (!comm)
        comm = gnc_commodity_table_lookup (table,
                GNC_COMMODITY_NS_CURRENCY, comm_str.c_str());

    if (!comm)
    {
        /* If that fails try mnemonic in all other namespaces */
        auto namespaces = gnc_commodity_table_get_namespaces(table);
        for (auto ns = namespaces; ns; ns = ns->next)
        {
            gchar* ns_str = (gchar*)ns->data;
            if (g_utf8_collate(ns_str, GNC_COMMODITY_NS_CURRENCY) == 0)
                continue;

            comm = gnc_commodity_table_lookup (table,
                    ns_str, comm_str.c_str());
            if (comm)
                break;
        }
    }

    if (!comm)
        throw std::invalid_argument (_("Value can't be parsed into a valid commodity."));
    else
        return comm;
}

gnc_commodity * parse_commodity_price_sym (const std::string& sym_str, bool is_currency)
{
    if (sym_str.empty())
        return nullptr;

    auto commodity_table = gnc_get_current_commodities ();
    GList         *namespaces;
    gnc_commodity *retval = nullptr;
    gnc_commodity *tmp_commodity = nullptr;
    char  *tmp_namespace = nullptr;
    GList *commodity_list = NULL;
    GList *namespace_list = gnc_commodity_table_get_namespaces (commodity_table);

    namespace_list = g_list_first (namespace_list);
    while (namespace_list != NULL && retval == NULL)
    {
        tmp_namespace = (char*)namespace_list->data;
        DEBUG("Looking at namespace %s", tmp_namespace);
        commodity_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace);
        commodity_list  = g_list_first (commodity_list);
        while (commodity_list != NULL && retval == NULL)
        {
            const char* tmp_mnemonic = NULL;
            tmp_commodity = (gnc_commodity*)commodity_list->data;
            DEBUG("Looking at commodity %s", gnc_commodity_get_fullname (tmp_commodity));
            tmp_mnemonic = gnc_commodity_get_mnemonic (tmp_commodity);
            if (g_strcmp0 (tmp_mnemonic, sym_str.c_str()) == 0)
            {
                retval = tmp_commodity;
                DEBUG("Commodity %s%s", gnc_commodity_get_fullname (retval), " matches.");
            }
            commodity_list = g_list_next (commodity_list);
        }
        namespace_list = g_list_next (namespace_list);
    }
    g_list_free (commodity_list);
    g_list_free (namespace_list);

    if (!retval)
        throw std::invalid_argument (_("Value can't be parsed into a valid commodity."));
    else
    {
        if (gnc_commodity_is_currency (retval) != is_currency)
            throw std::invalid_argument (_("Value parsed into an invalid commodity for column type."));
        else
            return retval;
    }
}

void GncImportPrice::set (GncPricePropType prop_type, const std::string& value)
{
    try
    {
        // Drop any existing error for the prop_type we're about to set
        m_errors.erase(prop_type);

        gnc_commodity *comm = nullptr;
        switch (prop_type)
        {
            case GncPricePropType::DATE:
                m_date = boost::none;
                m_date = parse_date_price (value, m_date_format); // Throws if parsing fails
                break;

            case GncPricePropType::AMOUNT:
                m_amount = boost::none;
                m_amount = parse_amount_price (value, m_currency_format); // Will throw if parsing fails
                break;

            case GncPricePropType::CURRENCY_FROM:
                m_currency_from = boost::none;
                comm = parse_commodity_price_sym (value, true); // Throws if parsing fails
                if (comm)
                    m_currency_from = comm;
                break;

            case GncPricePropType::CURRENCY_TO:
                m_currency_to = boost::none;
                comm = parse_commodity_price_sym (value, true); // Throws if parsing fails
                if (comm)
                    m_currency_to = comm;
                break;

            case GncPricePropType::SYMBOL_FROM:
                m_symbol_from = boost::none;
                comm = parse_commodity_price_sym (value, false); // Throws if parsing fails
                if (comm)
                    m_symbol_from = comm;
                break;

            default:
                /* Issue a warning for all other prop_types. */
                PWARN ("%d is an invalid property for a Price", static_cast<int>(prop_type));
                break;
        }
    }
    catch (const std::invalid_argument& e)
    {
        auto err_str = std::string(_(gnc_csv_price_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
    catch (const std::out_of_range& e)
    {
        auto err_str = std::string(_(gnc_csv_price_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
}

void GncImportPrice::reset (GncPricePropType prop_type)
{
    try
    {
        set (prop_type, std::string());
    }
    catch (...)
    {
        // Set with an empty string will effectively clear the property
        // but can also set an error for the property. Clear that error here.
        m_errors.erase(prop_type);
    }
}

std::string GncImportPrice::verify_essentials (void)
{
    /* Make sure this price has the minimum required set of properties defined */
    if (m_date == boost::none)
        return _("No date column.");
    else if (m_amount == boost::none)
        return _("No amount column.");
    else if (m_currency_to == boost::none)
        return _("No Currency to column.");
    else if ((m_symbol_from == boost::none) && (m_currency_from == boost::none))
        return _("No from column.");
    else
        return std::string();
}

bool GncImportPrice::create_price (QofBook* book, GNCPriceDB *pdb, bool over)
{
    /* Gently refuse to create the price if the basics are not set correctly
     * This should have been tested before calling this function though!
     */
    auto check = verify_essentials();
    if (!check.empty())
    {
        PWARN ("Refusing to create price because essentials not set properly: %s", check.c_str());
        return false;
    }

    Timespec date;
    timespecFromTime64 (&date, *m_date);
    date.tv_nsec = 0;

#ifdef skip
//FIXME Numeric needs changing, copied from old version...
    bool rev = false;
    gnc_commodity *comm_from = nullptr;

    if (m_currency_from != boost::none) // Currency Import
    {
        // Check for currency in reverse direction.
        GNCPrice *rev_price = gnc_pricedb_lookup_day (pdb, *m_currency_to, *m_currency_from, date);
        if (rev_price != nullptr)
            rev = true;
        gnc_price_unref (rev_price);

        // Check for price less than 1, reverse if so.
        if (gnc_numeric_compare (*m_amount, gnc_numeric_create (1, 1)) != 1)
            rev = true;

        comm_from = *m_currency_from;
        DEBUG("Commodity from is a Currency");
    }
    else
        comm_from = *m_symbol_from;

    DEBUG("Date is %s, Rev is %d, Commodity from is '%s', Currency is '%s', Amount is %s", gnc_print_date (date),
          rev, gnc_commodity_get_fullname (comm_from), gnc_commodity_get_fullname (*m_currency_to),
          gnc_num_dbg_to_string (*m_amount)           );

    GNCPrice *old_price = nullptr;

    // Should the commodities be reversed
    if (rev)
        old_price = gnc_pricedb_lookup_day (pdb, *m_currency_to, comm_from, date);
    else
        old_price = gnc_pricedb_lookup_day (pdb, comm_from, *m_currency_to, date);

    // Should old price be over writen
    if ((old_price != nullptr) && (over == true))
    {
        DEBUG("Over write");
        gnc_pricedb_remove_price (pdb, old_price);
        gnc_price_unref (old_price);
        old_price = nullptr;
    }
#endif
    bool ret_val = true;
#ifdef skip
    // Create the new price
    if (old_price == nullptr)
    {
        DEBUG("Create");
        GNCPrice *price = gnc_price_create (book);
        gnc_price_begin_edit (price);

        if (rev)
        {
            gnc_price_set_commodity (price, *m_currency_to);
            gnc_price_set_currency (price, comm_from);
            *m_amount = gnc_numeric_convert (gnc_numeric_invert (*m_amount),
                                          CURRENCY_DENOM, GNC_HOW_RND_ROUND_HALF_UP);
            gnc_price_set_value (price, *m_amount);
        }
        else
        {
            gnc_price_set_commodity (price, comm_from);
            gnc_price_set_currency (price, *m_currency_to);
            gnc_price_set_value (price, *m_amount);
        }
        gnc_price_set_time (price, date);
        gnc_price_set_source (price, PRICE_SOURCE_USER_PRICE);
//FIXME Not sure which one        gnc_price_set_source (price, PRICE_SOURCE_FQ);
        gnc_price_set_typestr (price, PRICE_TYPE_LAST);
        gnc_price_commit_edit (price);

        bool perr = gnc_pricedb_add_price (pdb, price);

        gnc_price_unref (price);

         if (perr == false)
            throw std::invalid_argument (_("Failed to create price from selected columns."));
//FIXME Not sure about this, should this be a PWARN
    }
    else

#endif
        ret_val = false;

    return ret_val;
}

static std::string gen_err_str (std::map<GncPricePropType, std::string>& errors)
{
    auto full_error = std::string();
    for (auto error : errors)
    {
        full_error += (full_error.empty() ? "" : "\n") + error.second;
    }
    return full_error;
}

std::string GncImportPrice::errors ()
{
    return gen_err_str (m_errors);
}


/********************************************************************\
 * gnc-imp-props-price.cpp - encapsulate price properties for use   *
 *                           in the csv importer                    *
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
}

#include <string>
#include <boost/regex.hpp>
#include <boost/regex/icu.hpp>
#include "gnc-imp-props-price.hpp"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_IMPORT;

/* This map contains a set of strings representing the different column types. */
std::map<GncPricePropType, const char*> gnc_price_col_type_strs = {
        { GncPricePropType::NONE, N_("None") },
        { GncPricePropType::DATE, N_("Date") },
        { GncPricePropType::AMOUNT, N_("Amount") },
        { GncPricePropType::FROM_COMMODITY, N_("Commodity From") },
        { GncPricePropType::TO_CURRENCY, N_("Currency To") },
};

/** Convert str into a GncNumeric using the user-specified (import) currency format.
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

/** Convert comm_str into a gnc_commodity.
 * @param comm_str The string to be parsed
 * @return a gnc_commodity
 * @exception May throw std::invalid argument if string can't be parsed properly
 */
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

void GncImportPrice::set (GncPricePropType prop_type, const std::string& value, bool enable_test_empty)
{
    try
    {
        // Drop any existing error for the prop_type we're about to set
        m_errors.erase(prop_type);

        // conditional test for empty values
        if (value.empty() && enable_test_empty)
            throw std::invalid_argument (_("Column value can not be empty."));

        gnc_commodity *comm = nullptr;
        switch (prop_type)
        {
            case GncPricePropType::DATE:
                m_date = boost::none;
                m_date = GncDate(value, GncDate::c_formats[m_date_format].m_fmt); // Throws if parsing fails
                break;

            case GncPricePropType::AMOUNT:
                m_amount = boost::none;
                m_amount = parse_amount_price (value, m_currency_format); // Throws if parsing fails
                break;

            case GncPricePropType::FROM_COMMODITY:
                m_from_commodity = boost::none;
                comm = parse_commodity_price_comm (value); // Throws if parsing fails
                if (comm)
                {
                    if (m_to_currency == comm)
                        throw std::invalid_argument (_("'Commodity From' can not be the same as 'Currency To' column type."));
                    m_from_commodity = comm;
                }
                break;

            case GncPricePropType::TO_CURRENCY:
                m_to_currency = boost::none;
                comm = parse_commodity_price_comm (value); // Throws if parsing fails
                if (comm)
                {
                    if (m_from_commodity == comm)
                        throw std::invalid_argument (_("'Currency To' can not be the same as 'Commodity From' column type."));
                    if (gnc_commodity_is_currency (comm) != true)
                        throw std::invalid_argument (_("Value parsed into an invalid currency for a currency column type."));
                    m_to_currency = comm;
                }
                break;

            default:
                /* Issue a warning for all other prop_types. */
                PWARN ("%d is an invalid property for a Price", static_cast<int>(prop_type));
                break;
        }
    }
    catch (const std::invalid_argument& e)
    {
        auto err_str = std::string(_(gnc_price_col_type_strs[prop_type])) +
                       std::string(_(" could not be understood.\n")) +
                       e.what();
        m_errors.emplace(prop_type, err_str);
        throw std::invalid_argument (err_str);
    }
    catch (const std::out_of_range& e)
    {
        auto err_str = std::string(_(gnc_price_col_type_strs[prop_type])) +
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
        // set enable_test_empty to false to allow empty values
        set (prop_type, std::string(), false);
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
    else if (m_to_currency == boost::none)
        return _("No 'Currency to' column.");
    else if (m_from_commodity == boost::none)
        return _("No 'Commodity from' column.");
    else if (gnc_commodity_equal (*m_from_commodity, *m_to_currency))
        return _("'Commodity from' can not be the same as 'Currency to'.");
    else
        return std::string();
}

Result GncImportPrice::create_price (QofBook* book, GNCPriceDB *pdb, bool over)
{
    /* Gently refuse to create the price if the basics are not set correctly
     * This should have been tested before calling this function though!
     */
    auto check = verify_essentials();
    if (!check.empty())
    {
        PWARN ("Refusing to create price because essentials not set properly: %s", check.c_str());
        return FAILED;
    }

    Timespec date;
    timespecFromTime64 (&date, static_cast<time64>(GncDateTime(*m_date, DayPart::neutral)));
    date.tv_nsec = 0;

    bool rev = false;
    auto amount = *m_amount;
    Result ret_val = ADDED;

    GNCPrice *old_price = gnc_pricedb_lookup_day (pdb, *m_from_commodity, *m_to_currency, date);

    // Should old price be over writen
    if ((old_price != nullptr) && (over == true))
    {
        DEBUG("Over write");
        gnc_pricedb_remove_price (pdb, old_price);
        gnc_price_unref (old_price);
        old_price = nullptr;
        ret_val = REPLACED;
    }

    if (gnc_commodity_is_currency (*m_from_commodity)) // Currency Import
    {
        // Check for currency in reverse direction.
        if (old_price != nullptr)
        {
            // Check for price in reverse direction.
            if (gnc_commodity_equiv (gnc_price_get_currency (old_price), *m_from_commodity))
                rev = true;
        }
        DEBUG("Commodity from is a Currency");

        // Check for price less than 1, reverse if so.
        if (*m_amount < GncNumeric(1,1))
            rev = true;

    }
    DEBUG("Date is %s, Rev is %d, Commodity from is '%s', Currency is '%s', Amount is %s", gnc_print_date (date),
        rev, gnc_commodity_get_fullname (*m_from_commodity), gnc_commodity_get_fullname (*m_to_currency),
        amount.to_string().c_str());

    // Create the new price
    if (old_price == nullptr)
    {
        DEBUG("Create");
        GNCPrice *price = gnc_price_create (book);
        gnc_price_begin_edit (price);
        if (rev)
        {
            amount = amount.inv(); //invert the amount
            gnc_price_set_commodity (price, *m_to_currency);
            gnc_price_set_currency (price, *m_from_commodity);
        }
        else
        {
            gnc_price_set_commodity (price, *m_from_commodity);
            gnc_price_set_currency (price, *m_to_currency);
        }
        auto amount_conv = amount.convert<RoundType::half_up>(CURRENCY_DENOM);
        gnc_price_set_value (price, static_cast<gnc_numeric>(amount_conv));

        gnc_price_set_time (price, date);
        gnc_price_set_source (price, PRICE_SOURCE_USER_PRICE);
//FIXME Not sure which one        gnc_price_set_source (price, PRICE_SOURCE_FQ);
        gnc_price_set_typestr (price, PRICE_TYPE_LAST);
        gnc_price_commit_edit (price);

        bool perr = gnc_pricedb_add_price (pdb, price);

        gnc_price_unref (price);

        if (perr == false)
            throw std::invalid_argument (_("Failed to create price from selected columns."));
    }
    else
    {
        gnc_price_unref (old_price);
        ret_val = DUPLICATED;
    }
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


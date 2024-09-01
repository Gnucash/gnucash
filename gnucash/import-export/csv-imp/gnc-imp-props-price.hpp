/********************************************************************\
 * gnc-imp-props-price.hpp - encapsulate price properties for use   *
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

#ifndef GNC_PRICE_PROPS_HPP
#define GNC_PRICE_PROPS_HPP

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>
#include "gnc-pricedb.h"
#include "gnc-commodity.h"

#include <string>
#include <map>
#include <memory>
#include <optional>
#include <gnc-datetime.hpp>
#include <gnc-numeric.hpp>

/** Enumeration for column types. These are the different types of
 * columns that can exist in a CSV/Fixed-Width file. There should be
 * no two columns with the same type except for the GncPricePropType::NONE
 * type. */
enum class GncPricePropType {
    NONE,
    DATE,
    AMOUNT,
    FROM_SYMBOL,
    FROM_NAMESPACE,
    TO_CURRENCY,
    PRICE_PROPS = TO_CURRENCY
};

enum Result { FAILED, ADDED, DUPLICATED, REPLACED };

/** Maps all column types to a string representation.
 *  The actual definition is in gnc-imp-props-price.cpp.
 *  Attention: that definition should be adjusted for any
 *  changes to enum class GncPricePropType ! */
extern std::map<GncPricePropType, const char*> gnc_price_col_type_strs;

/** Functor to check if the above map has an element of which
 *  the value equals name. To be used with std::find_if.
 */
struct test_price_prop_type_str
{
    test_price_prop_type_str( const char* name ) : m_name(name) {}
    bool operator()( const std::pair<GncPricePropType, const char*>& v ) const
    {
        return !g_strcmp0(v.second, m_name);
    }
private:
    const char *m_name;
};

gnc_commodity* parse_commodity_price_comm (const std::string& symbol_str, const std::string& namespace_str);
bool parse_namespace (const std::string& namespace_str);
GncNumeric parse_amount_price (const std::string &str, int currency_format);

struct GncImportPrice
{
public:
    GncImportPrice (std::string date_locale, int currency_format) : m_date_locale{date_locale},
        m_currency_format{currency_format}{};

    void set (GncPricePropType prop_type, const std::string& value, bool enable_test_empty);
    void set_date_locale (std::string date_locale) { m_date_locale = date_locale ;}
    void set_currency_format (int currency_format) { m_currency_format = currency_format ;}
    void reset (GncPricePropType prop_type);
    std::string verify_essentials (void);
    Result create_price (QofBook* book, GNCPriceDB *pdb, bool over);

    gnc_commodity* get_from_commodity () { if (m_from_commodity) return *m_from_commodity; else return nullptr; }
    void set_from_commodity (gnc_commodity* comm) { if (comm) m_from_commodity = comm; else m_from_commodity.reset(); }

    gnc_commodity* get_to_currency () { if (m_to_currency) return *m_to_currency; else return nullptr; }
    void set_to_currency (gnc_commodity* curr) { if (curr) m_to_currency = curr; else m_to_currency.reset(); }

    std::string errors();

private:
    std::string m_date_locale;
    int m_currency_format;
    std::optional<GncDate> m_date;
    std::optional<GncNumeric> m_amount;
    std::optional<gnc_commodity*> m_from_commodity;
    std::optional<std::string> m_from_namespace;
    std::optional<std::string> m_from_symbol;
    std::optional<gnc_commodity*> m_to_currency;

    std::map<GncPricePropType, std::string> m_errors;
};

#endif

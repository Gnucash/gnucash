/********************************************************************\
 * gnc-quotes.hpp -- proxy for Finance::Quote                       *
 * Copyright (C) 2021 Geert Janssens <geert@kobaltwit.be>           *
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
#ifndef GNC_QUOTES_HPP
#define GNC_QUOTES_HPP

#include <memory>
#include <string>
#include <vector>
#include <gnc-commodity.hpp>  // For CommVec alias
#include <glib.h>
#include <stdexcept>

extern  "C" {
#include <qofbook.h>
}

using StrVec = std::vector<std::string>;
using QuoteSources = StrVec;

enum class GncQuoteError
{
    SUCCESS,
    NO_RESULT,
    QUOTE_FAILED,
    NO_CURRENCY,
    UNKNOWN_CURRENCY,
    NO_PRICE,
    UNKNOWN_PRICE_TYPE,
    PRICE_PARSE_FAILURE,
};

/** QuoteFailure elements are namespace, mnemonic, error code, and
 * F::Q errormsg if there is one.
 */
using QuoteFailure = std::tuple<std::string, std::string,
                                GncQuoteError, std::string>;
using QFVec = std::vector<QuoteFailure>;

struct GncQuoteException : public std::runtime_error
{
    GncQuoteException(const std::string& msg) : std::runtime_error(msg) {}
};

class GncQuotesImpl;

class GncQuotes
{
public:
    /** Create a GncQuotes object.
     *
     * Throws a GncQuoteException if Finance::Quote is not installed or fails to initialize.
     */
    GncQuotes ();
    ~GncQuotes ();

    /** Fetch quotes for all commodities in our db that have a quote source set
     *
     * @param book The current book.
     */
    void fetch (QofBook *book);
    /** Fetch quotes for a vector of commodities
     *
     * @param commodities std::vector of the gnc_commodity* to get quotes for.
     * @note Commodities without a quote source will be silently ignored.
     */
    void fetch (CommVec& commodities);
    /** Fetch quote for a single commodity
     *
     * @param comm Commodity for which to retrieve a quote
     * @note Commodity must have a quote source set or the call will silently fail.
     */
    void fetch (gnc_commodity *comm);
    /** Report quote results from Finance::Quote to std::cout.
     *
     * @param source A valid quote source
     * @param commodities A std::vector of symbols to request quotes for.
     * @note If requesting currency rates the first symbol is the to-currency and the rest are from-currencies. For example, {"USD", "EUR", "CAD"} will print the price of 1 Euro and 1 Canadian Dollar in US Dollars.
     * @param verbose Ignored for currency queries. If false it will print the six fields GnuCash uses regardless of whether a value was returned; if true it will print all of the fields for which Finanace::Quote returned values.
     */
    void report (const char* source, const StrVec& commodities, bool verbose = false);
    /** Get the installed Finance::Quote version
     *
     * @return the Finance::Quote version string
     */
    const std::string& version() noexcept;

    /** Get the available Finance::Quote sources as a std::vector
     *
     * @return The quote sources configured in Finance::Quote
     */
    const QuoteSources& sources() noexcept;

    /** Report if there were quotes requested but not retrieved.
     *
     * @returns True if there were quote failures.
     */
    bool had_failures() noexcept;

    /** Report the commodities for which quotes were requested but not successfully retrieved.
     *
     * This does not include requested commodities that didn't have a quote source.
     *
     * @return a reference to a vector of QuoteFailure tuples.
     * @note The vector and its contents belong to the GncQuotes object and will be destroyed with it.
     */
    const QFVec& failures() noexcept;

    /* Report the commodities for which quotes were requested but not successfully retrieved.
     *
     * This does not include requested commodities that didn't have a quote source.
     *
     * @return A localized std::string with an intro and a list of the quote failures with a cause. The string is owned by the caller.
     */
    const std::string report_failures() noexcept;

private:
    std::unique_ptr<GncQuotesImpl> m_impl;
};

#endif /* GNC_QUOTES_HPP */

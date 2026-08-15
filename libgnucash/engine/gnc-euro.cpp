/********************************************************************\
 * gnc-euro.c -- utilities for EURO currency                        *
 *                                                                  *
 * Copyright (C) 2000 Herbert Thoma                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include <string>
#include <boost/container/flat_map.hpp>
#include <optional>

#include "gnc-euro.h"
#include "gnc-session.h"
#include <gnc-numeric.hpp>
#include <gnc-rational-rounding.hpp>

/* The rates are per EURO and are denoted in GncNumeric  */
static const boost::container::flat_map <std::string, GncNumeric> gnc_euro_rates =
{
    { "ATS", {137603,   10000} }, /* austrian schilling */
    { "BEF", {403399,   10000} }, /* belgian franc */
    { "BGN", {195583,  100000} }, /* Bulgarian lev */
    { "CYP", {585274, 1000000} }, /* cyprus pound */
    { "DEM", {195583,  100000} }, /* german mark */
    { "EEK", {156466,   10000} }, /* Estonian Kroon */
    { "ESP", {166386,    1000} }, /* spanish peseta */
    { "EUR", {     1,       1} }, /* euro */
    { "FIM", {594573,  100000} }, /* finnmark */
    { "FRF", {655957,  100000} }, /* french franc */
    { "GRD", {340750,    1000} }, /* greek drachma */
    { "HRK", {753450,  100000} }, /* Croatian kuna */
    { "IEP", {787564, 1000000} }, /* irish pound */
    { "ITL", {193627,     100} }, /* italian lira */
    { "LUF", {403399,   10000} }, /* luxembourg franc */
    { "LVL", {702804, 1000000} }, /* latvian lats */
    { "MTL", {429300, 1000000} }, /* maltese lira */
    { "NLG", {220371,  100000} }, /* netherland gulden */
    { "PTE", {200482,    1000} }, /* portuguese escudo */
    { "SIT", {239640,    1000} }, /* slovenian tolar */
    { "SKK", {301260,   10000} }  /* slovak koruna */
};

static std::optional<GncNumeric>
get_euro_rate (const gnc_commodity * currency)
{
    if (!currency || !gnc_commodity_is_iso(currency))
        return {};

    auto it = gnc_euro_rates.find (gnc_commodity_get_mnemonic(currency));
    if (it == gnc_euro_rates.end())
        return {};

    return it->second;
}

/* ------------------------------------------------------ */

gboolean
gnc_is_euro_currency(const gnc_commodity * currency)
{
    return get_euro_rate (currency).has_value();
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_to_euro(const gnc_commodity * currency, gnc_numeric value)
{
    auto euro_rate = get_euro_rate (currency);
    if (!euro_rate)
        return gnc_numeric_zero();

    /* round to 2 decimal places */
    /* EC Regulation 1103/97 states we should use "Round half away from zero"
     * See https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A31997R1103&qid=1662917247821
     */
    return (value / *euro_rate).convert<RoundType::half_up>(100);
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_convert_from_euro(const gnc_commodity * currency, gnc_numeric value)
{
    auto euro_rate = get_euro_rate (currency);
    if (!euro_rate)
        return gnc_numeric_zero();

    /* EC Regulation 1103/97 states we should use "Round half away from zero"
     * See http://europa.eu/legislation_summaries/economic_and_monetary_affairs/institutional_and_economic_framework/l25025_en.htm */
    return (value * *euro_rate).convert<RoundType::half_up>(gnc_commodity_get_fraction (currency));
}

/* ------------------------------------------------------ */

gnc_numeric
gnc_euro_currency_get_rate (const gnc_commodity *currency)
{
    auto euro_rate = get_euro_rate (currency);
    if (!euro_rate)
        return gnc_numeric_zero();

    return static_cast<gnc_numeric>(*euro_rate);
}

/* ------------------------------------------------------ */

gnc_commodity *
gnc_get_euro (void)
{
    QofBook* book = qof_session_get_book (gnc_get_current_session ());
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);

    return gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "EUR");
}

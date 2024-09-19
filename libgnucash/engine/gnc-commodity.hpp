/**********************************************************************
 * gnc-commodity.hpp -- API for tradable commodities (incl. currency) *
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 *                                                                    *
 *********************************************************************/

/** @addtogroup Engine
    @{ */
/** @addtogroup Commodity Commodities

    @{ */
/** @file gnc-commodity.hpp
 *  @brief Commodity handling public routines (C++ api)
 *  @author Copyright (C) 2021 Geert Janssens
 */

#ifndef GNC_COMMODITY_HPP
#define GNC_COMMODITY_HPP

#include <vector>
#include <string>

#include <gnc-commodity.h>

using CommVec = std::vector<gnc_commodity*>;

/** Update gnucash internal tables based on what Finance::Quote
 *  sources are installed.  Sources that have been explicitly coded
 *  into gnucash are marked sensitive/insensitive based upon whether
 *  they are present. New sources that gnucash doesn't know about are
 *  added to its internal tables.
 *
 *  @param sources_list A list of strings containing the source names
 *  as they are known to F::Q.
 */
void gnc_quote_source_set_fq_installed (const char* version_string,
                                        const std::vector<std::string>& sources_list);


/** Return a list of all namespaces in the commodity table.  This
 *  returns both system and user defined namespaces.
 *
 *  @return A vector to the list of names. An empty vector if an
 *  invalid argument was supplied. */
std::vector<std::string> gnc_commodity_table_get_namespaces (const gnc_commodity_table * t);


/** Return a vector of all commodities in the commodity table that are
 *  in the given namespace.
 *
 *  @param table A pointer to the commodity table
 *
 *  @param commodity_namespace A string indicating which commodities should be
 *  returned. It is a required argument.
 *
 *  @return A pointer to the list of commodities.  NULL if an invalid
 *  argument was supplied, or the namespace could not be found.
 */

CommVec gnc_commodity_table_get_commodities (const gnc_commodity_table *, const char *);

/** This function returns a vector of commodities for which price
 *  quotes should be retrieved.  It will scan the entire commodity
 *  table (or a subset) and check each commodity to see if the
 *  price_quote_flag field has been set.  All matching commodities are
 *  queued onto a list, and the head of that list is returned.  Use
 *  the command-line given expression as a filter on the commodities
 *  to be returned. If non-null, only commodities in namespace that
 *  match the specified regular expression are checked.  If none was
 *  given, all commodities are checked.
 *
 *  @param table A pointer to the commodity table
 *
 *  @return A pointer to a list of commodities.  NULL if invalid
 *  arguments were supplied or if there no commodities are flagged for
 *  quote retrieval.
 */

CommVec gnc_commodity_table_get_quotable_commodities (const gnc_commodity_table*);

#endif /* GNC_COMMODITY_HPP */
/** @} */
/** @} */

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

#endif /* GNC_COMMODITY_HPP */
/** @} */
/** @} */

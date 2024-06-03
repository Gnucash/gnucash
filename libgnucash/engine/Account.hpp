/**********************************************************************
 * Account.hpp
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
/** @addtogroup Account

    @{ */
/** @file Account.hpp
 *  @brief Account public routines (C++ api)
 */

#ifndef GNC_ACCOUNT_HPP
#define GNC_ACCOUNT_HPP

#include <vector>
#include <functional>

#include <Account.h>

using SplitsVec = std::vector<Split*>;
using AccountVec = std::vector<Account*>;

const SplitsVec xaccAccountGetSplits (const Account*);

void gnc_account_foreach_split (const Account*, std::function<void(Split*)>, bool);

void gnc_account_foreach_split_until_date (const Account *acc, time64 end_date,
                                           std::function<void(Split*)> f);

/** scans account split list (in forward or reverse order) until
 *    predicate split->bool returns true. Maybe return the split.
 *
 *  @param acc The account to which the split should be added.
 *
 *  @param predicate A split->bool predicate.
 *
 *  @param reverse To scan in reverse order
 *
 *  @result Split* or nullptr if not found */
Split* gnc_account_find_split (const Account*, std::function<bool(const Split*)>, bool);

#endif /* GNC_COMMODITY_HPP */
/** @} */
/** @} */

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
#include <algorithm>
#include <optional>

#include <Account.h>
#include <SplitP.hpp>
#include <TransactionP.hpp>

using SplitsVec = std::vector<Split*>;
using AccountVec = std::vector<Account*>;

const SplitsVec& xaccAccountGetSplits (const Account*);

void gnc_account_foreach_descendant (const Account *, std::function<void(Account*)> func);


static inline SplitsVec::const_iterator
splits_start (const SplitsVec& splits, std::optional<time64> start_date)
{
    if (!start_date) return splits.begin();
    return std::lower_bound (splits.begin(), splits.end(), *start_date,
                             [](auto s, time64 t){ return s->parent->date_posted < t; });
}

static inline SplitsVec::const_iterator
splits_end (const SplitsVec& splits, std::optional<time64> end_date)
{
    if (!end_date) return splits.end();
    return std::upper_bound (splits.begin(), splits.end(), *end_date,
                             [](time64 t, auto s){ return t < s->parent->date_posted; });
}

template <typename Fn>
void gnc_account_foreach_split_between_dates (const Account* account,
                                              std::optional<time64> start_date,
                                              std::optional<time64> end_date,
                                              bool include_descendants, Fn&& fn)
{
    g_return_if_fail (GNC_IS_ACCOUNT (account));
    auto scan_account = [&](const Account* acc)
    {
        const auto& splits = xaccAccountGetSplits (acc);
        std::for_each (splits_start (splits, start_date), splits_end (splits, end_date),
                       fn);
    };

    scan_account (account);
    if (include_descendants)
        gnc_account_foreach_descendant (account, scan_account);
}

void gnc_account_foreach_split (const Account*, std::function<void(Split*)>);

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

std::vector<const Account*> gnc_account_get_all_parents (const Account *account);

#endif /* GNC_COMMODITY_HPP */
/** @} */
/** @} */

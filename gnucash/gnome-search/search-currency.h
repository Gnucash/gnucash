/*
 * Currency selection for search criteria.
 * Copyright (C) 2026 Étienne Beaulé
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#ifndef GNC_SEARCH_CURRENCY_H
#define GNC_SEARCH_CURRENCY_H

#include "search-core-type.h"

G_BEGIN_DECLS

#define CURRENCY_MATCH_TYPE "currency-match"
#define GNC_TYPE_SEARCH_CURRENCY (gnc_search_currency_get_type ())
G_DECLARE_FINAL_TYPE (GNCSearchCurrency, gnc_search_currency, GNC, SEARCH_CURRENCY,
                      GNCSearchCoreType)

/** Create a currency search criterion using the standard currency selector.
 * @return A new currency search criterion owned by the caller.
 */
GNCSearchCurrency *gnc_search_currency_new (void);

G_END_DECLS

#endif

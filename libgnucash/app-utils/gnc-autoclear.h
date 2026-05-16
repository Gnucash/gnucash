/********************************************************************
 * gnc-autoclear.h -- Knapsack algorithm functions
 *
 * Copyright 2020 Cristian Klein <cristian@kleinlabs.eu>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02111-1301,  USA       gnu@gnu.org
 *******************************************************************/

#ifndef GNC_AUTOCLEAR_H
#define GNC_AUTOCLEAR_H

#include <glib.h>
#include <stdint.h>
#include <Account.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    ABORT_NONE = 0,
    ABORT_NOP,
    ABORT_MULTI,
    ABORT_TIMEOUT,
    ABORT_UNREACHABLE,
} Autoclear;

/** Account splits are analysed; attempts to find a unique combination
 *  of uncleared splits which would set cleared balance to
 *  toclear_value. If this is not possible, GError will be error
 *  message.
 * @param account: account whose unreconciled splits must be assessed.
 * @param toclear_value: gnc_numeric target cleared balance
 * @param end_date: latest date for splits to be assesed
 * @param error:    a GError* to collect error conditions
 * @param max_seconds: timeout limit. 0 or less will disable timeout monitor.
 */
GList * gnc_account_get_autoclear_splits (Account *account,
                                          gnc_numeric toclear_value,
                                          time64 end_date, GError **error,
                                          double max_seconds);

#ifdef __cplusplus
}
#endif

#endif

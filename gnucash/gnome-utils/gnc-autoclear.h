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
#include <Account.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Account splits are analysed; attempts to find a unique combination
 *  of uncleared splits which would set cleared balance to
 *  toclear_value. If this is not possible, *errmsg will be error
 *  message. errmsg must be a pointer to a gchar. If it is set, it
 *  must be freed by the caller.
 */
GList * gnc_account_get_autoclear_splits (Account *account, gnc_numeric toclear_value,
                                          gchar **errmsg);

#ifdef __cplusplus
}
#endif

#endif

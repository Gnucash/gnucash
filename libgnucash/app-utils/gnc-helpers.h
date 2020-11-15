/********************************************************************\
 * gnc-helpers.h -- gnucash app-util helper functions               *
 * Copyright (C) 2000 Linas Vepstas                                 *
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

#ifndef GNC_HELPERS
#define GNC_HELPERS

#include <libguile.h>

#include "gnc-ui-util.h"

SCM  gnc_printinfo2scm(GNCPrintAmountInfo info);
GNCPrintAmountInfo gnc_scm2printinfo(SCM info_scm);

/** Given a pointer to a gnc-commodity data structure, build a Scheme
 *  list containing the data needed by the code in price-quotes.scm.
 *  This prevents flipping back and forth from Scheme to C while
 *  extracting values from a pointer.
 *
 * @param com A pointer to the commodity to convert.
 *
 * @return A pointer to a Scheme list, or SCM_EOL on error.
 */
SCM  gnc_quoteinfo2scm(gnc_commodity *com);

/** Given an account, gets a "linked" account. The linked account is
 *  identified by an 'id' string. e.g. The source may be a STOCK
 *  account, and has metadata to identify linked dividends, capital
 *  gains, broker cash, fees accounts. These accounts are identified
 *  by the id gchar string. If the guid does not exist the getter will
 *  return NULL.
 *
 * @param acc Source account
 *
 * @param id The string identifier
 *
 * @return A pointer to the linked account, or NULL
 *
 */

Account * gnc_account_get_linked_account (const Account *acc, const gchar *id);

/** Given an account, sets a "linked" account. The linked account is
 *  identified by an 'id' string. e.g. The source may be a STOCK
 *  account, and has metadata to identify linked dividends, capital
 *  gains, broker cash, fees accounts.
 *
 * @param acc Source account
 *
 * @param id The string identifier
 *
 * @param target Target account
 *
 */
void gnc_account_set_linked_account (Account *acc, const gchar *id,
                                     const Account *target);

#endif

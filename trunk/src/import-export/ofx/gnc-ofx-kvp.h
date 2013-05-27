/*******************************************************************\
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
/*
 * gnc-ofx-kvp.h
 *
 *  Created on: 13.03.2011
 *      Author: cs
 */

#ifndef GNC_OFX_KVP_H_
#define GNC_OFX_KVP_H_

#include <glib.h>
#include <engine/Account.h>

Account *gnc_ofx_kvp_get_assoc_account(const Account* investment_account);

void gnc_ofx_kvp_set_assoc_account(Account* investment_account,
                                   const Account *associated_income_accout);


#endif /* GNC_OFX_CONVERSIONS_H_ */

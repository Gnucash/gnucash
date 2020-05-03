/*
 * gncBusGuile.h -- Business Guile Helper Functions
 * Copyright (C) 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_BUSINESS_GUILE_H_
#define GNC_BUSINESS_GUILE_H_

#include <gncTaxTable.h>	/* for GncAccountValue */
#include <libguile.h>

GncAccountValue * gnc_scm_to_account_value_ptr (SCM valuearg);
SCM gnc_account_value_ptr_to_scm (GncAccountValue *);

#endif /* GNC_BUSINESS_GUILE_H_ */

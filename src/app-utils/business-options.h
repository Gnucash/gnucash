/*
 * business-options.h -- non-GUI Option Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2003 Derek Atkins <warlord@MIT.EDU>
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

#ifndef GNC_BUSINESS_OPTIONS_UTILS_H_
#define GNC_BUSINESS_OPTIONS_UTILS_H_

#include "option-util.h"
#include "gncTaxTable.h"
#include "gncInvoice.h"
#include "gncCustomer.h"
#include "gncVendor.h"


GncTaxTable* gnc_option_db_lookup_taxtable_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        GncTaxTable * default_value);

GncInvoice* gnc_option_db_lookup_invoice_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        GncInvoice * default_value);

GncCustomer* gnc_option_db_lookup_customer_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        GncCustomer * default_value);

GncVendor* gnc_option_db_lookup_vendor_option(GNCOptionDB *odb,
        const char *section,
        const char *name,
        GncVendor * default_value);


#endif /* GNC_BUSINESS_OPTIONS_UTILS_H_ */

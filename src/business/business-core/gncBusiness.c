/*
 * gncBusiness.c -- Business helper functions
 * Copyright (C) 2002 Derek Atkins
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

#include "config.h"

#include "gncBusiness.h"

#include <glib.h>

#include "gncAddressP.h"
#include "gncBillTermP.h"
#include "gncCustomerP.h"
#include "gncEmployeeP.h"
#include "gncEntryP.h"
#include "gncInvoiceP.h"
#include "gncJobP.h"
#include "gncOrderP.h"
#include "gncOwnerP.h"
#include "gncTaxTableP.h"
#include "gncVendorP.h"

void
gnc_module_init_business_core_init(void)
{
    /* initialize known types */
    gncInvoiceRegister ();
    gncJobRegister ();
    gncBillTermRegister ();
    gncCustomerRegister ();
    gncAddressRegister ();
    gncEmployeeRegister ();
    gncEntryRegister ();
    gncOrderRegister ();
    gncOwnerRegister ();
    gncTaxTableRegister ();
    gncVendorRegister ();
}

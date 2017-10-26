/***************************************************************************
 *            cashobjects.c
 *
 *  Mon Aug 22 09:49:52 2005
 *  Copyright  2005  Neil Williams linux@codehelp.co.uk
 *
 * Copyright (C) 2002 Derek Atkins
 * Copyright (C) 2010 Christian Stimming
 *
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include "config.h"
#include "cashobjects.h"
#include "gnc-engine.h"
#include "Account.h"
#include "TransactionP.h"
#include "SchedXaction.h"
#include "SX-book-p.h"
#include "gnc-pricedb-p.h"
#include "gnc-lot-p.h"
#include "gnc-budget.h"

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

static void
business_core_init(void)
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

gboolean
cashobjects_register(void)
{
    g_return_val_if_fail(gnc_commodity_table_register(), FALSE);
    g_return_val_if_fail(xaccAccountRegister(), FALSE);
    g_return_val_if_fail ( xaccTransRegister(), FALSE);
    g_return_val_if_fail ( xaccSplitRegister(), FALSE);
    g_return_val_if_fail ( SXRegister (),       FALSE);
    g_return_val_if_fail ( gnc_sxtt_register(), FALSE);
    g_return_val_if_fail(gnc_pricedb_register(), FALSE);
    g_return_val_if_fail (gnc_budget_register(), FALSE);
    g_return_val_if_fail ( gnc_lot_register (), FALSE);

    /* And the business objects */
    business_core_init();

    return TRUE;
}


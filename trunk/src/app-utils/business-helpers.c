/*
 * business-options.c -- non-GUI helper functions for business features
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2003  Derek Atkins
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

#include "business-options.h"
#include "business-helpers.h"

GncTaxTable* gnc_business_get_default_tax_table (QofBook *book, GncOwnerType ownertype)
{
    GncTaxTable *table = NULL;
    GNCOptionDB *odb;

    odb = gnc_option_db_new_for_type (GNC_ID_BOOK);
    gnc_option_db_load_from_kvp (odb, qof_book_get_slots (book));

    switch (ownertype)
    {
    case GNC_OWNER_CUSTOMER:
        table = gnc_option_db_lookup_taxtable_option (odb,
                "Business",
                "Default Customer TaxTable",
                NULL);
        break;

    case GNC_OWNER_VENDOR:
        table = gnc_option_db_lookup_taxtable_option (odb,
                "Business",
                "Default Vendor TaxTable",
                NULL);
        break;

    default:
        break;
    }

    gnc_option_db_destroy (odb);
    return table;
}

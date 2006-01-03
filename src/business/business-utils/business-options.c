/*
 * business-options.c -- Non-GUI Option Utilities for GNC Business Objects
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

#include <g-wrap-wct.h>


#define LOOKUP_OPTION(fcn) { \
  GNCOption *option; \
  SCM getter; \
  SCM value; \
  \
  option = gnc_option_db_get_option_by_name (odb, section, name); \
  \
  if (option == NULL) \
    return default_value; \
  \
  getter = gnc_option_getter (option); \
  if (getter == SCM_UNDEFINED) \
    return default_value; \
  \
  value = scm_call_0 (getter); \
  if (value == SCM_BOOL_F) \
    return NULL; \
  \
  if (!gw_wcp_p(value)) \
    scm_misc_error(fcn, "Item is not a gw:wcp.", value); \
  \
  return gw_wcp_get_ptr(value); \
}
	

GncTaxTable*
gnc_option_db_lookup_taxtable_option(GNCOptionDB *odb,
				     const char *section,
				     const char *name,
				     GncTaxTable * default_value)
{
  LOOKUP_OPTION("gnc_option_db_lookup_taxtable_option");
}

GncInvoice*
gnc_option_db_lookup_invoice_option(GNCOptionDB *odb,
				    const char *section,
				    const char *name,
				    GncInvoice * default_value)
{
  LOOKUP_OPTION("gnc_option_db_lookup_invoice_option");
}

GncCustomer*
gnc_option_db_lookup_customer_option(GNCOptionDB *odb,
				     const char *section,
				     const char *name,
				     GncCustomer * default_value)
{
  LOOKUP_OPTION("gnc_option_db_lookup_customer_option");
}

GncVendor*
gnc_option_db_lookup_vendor_option(GNCOptionDB *odb,
				   const char *section,
				   const char *name,
				   GncVendor * default_value)
{
  LOOKUP_OPTION("gnc_option_db_lookup_vendor_option");
}

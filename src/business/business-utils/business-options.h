/*
 * business-options.h -- non-GUI Option Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2003
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

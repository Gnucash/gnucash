/*
 * gncBusGuile.h -- Business Guile Helper Functions
 * Copyright (C) 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_GUILE_H_
#define GNC_BUSINESS_GUILE_H_

#include <gncTaxTable.h>	/* for GncAccountValue */
#include <libguile.h>

int gnc_account_value_pointer_p (SCM arg);
GncAccountValue * gnc_scm_to_account_value_ptr (SCM valuearg);
SCM gnc_account_value_ptr_to_scm (GncAccountValue *);

#endif /* GNC_BUSINESS_GUILE_H_ */

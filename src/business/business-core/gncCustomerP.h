/*
 * gncCustomerP.h -- the Core Customer Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_CUSTOMERP_H_
#define GNC_CUSTOMERP_H_

#include "gncCustomer.h"

gboolean gncCustomerRegister (void);
gint64 gncCustomerNextID (GNCBook *book);
void gncCustomerSetGUID (GncCustomer *customer, const GUID *guid);

#endif /* GNC_CUSTOMERP_H_ */

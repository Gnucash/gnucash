/*
 * gncBusiness.h -- Business Helper Functions
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BUSINESS_H_
#define GNC_BUSINESS_H_

#include "gncObject.h"
#include "GNCId.h"

void gncBusinessForeach (GNCBook *book, GNCIdType mod_name,
			 foreachObjectCB cb, gpointer user_data);

#endif /* GNC_BUSINESS_H_ */

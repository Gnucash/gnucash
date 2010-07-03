/********************************************************************\
 * gncCustomerP.h -- the Core Customer Interface                    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_CUSTOMERP_H_
#define GNC_CUSTOMERP_H_

#include "gncCustomer.h"

gboolean gncCustomerRegister (void);
gint64 gncCustomerNextID (QofBook *book);

/** The gncCloneCustomer() routine makes a copy of the indicated
 *  customer, placing it in the indicated book.  It copies
 *  the addresses, credits, currency, billing terms and jobs.
 *  It then adds a pair of 'gemini' kvp pointers so that each copy
 *  can be found from the other.
 */
GncCustomer * gncCloneCustomer (GncCustomer *from,  QofBook *book);

/** The gncCustomerObtainTwin() will find the 'twin' of the
 *  indicated customer in the indicated book.  If the twin doesn't
 *  yet exist in the book, it will be created (by calling
 *  gncCloneCustomer()) and placed into the book.
 *
 * We called this routine 'Obtain' instead of "Get" to distinguish
 * it from the other Get routines, which work in fundamentally
 * different ways.
 */
GncCustomer * gncCustomerObtainTwin (GncCustomer *from, QofBook *book);

#define gncCustomerSetGUID(E,G) qof_instance_set_guid(QOF_INSTANCE(E),(G))

#endif /* GNC_CUSTOMERP_H_ */

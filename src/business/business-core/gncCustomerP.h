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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
void gncCustomerSetGUID (GncCustomer *customer, const GUID *guid);

/** The gncCloneCustomer() routine makes a copy of the indicated
 *  customer, placing it in the indicated book.  It copies
 *  the addresses, credits, etc.
 *  It copies the bill term, if needed.
 *  It does not copy jobs??  Or tax terms ???
 * XXX the above need fixin....
 *  It then adds a pair of 'gemini' kvp pointers so that each copy 
 *  can be found from the other.
 */
GncCustomer * gncCloneCustomer (GncCustomer *from,  QofBook *book);

#endif /* GNC_CUSTOMERP_H_ */

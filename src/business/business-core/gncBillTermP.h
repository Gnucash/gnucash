/********************************************************************\
 * gncBillTermP.h -- the Gnucash Billing Term private interface     *
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
 * Copyright (C) 2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_BILLTERMP_H_
#define GNC_BILLTERMP_H_

#include "gncBillTerm.h"

gboolean gncBillTermRegister (void);

void gncBillTermSetGUID (GncBillTerm *term, const GUID *guid);
void gncBillTermSetParent (GncBillTerm *term, GncBillTerm *parent);
void gncBillTermSetChild (GncBillTerm *term, GncBillTerm *child);
void gncBillTermSetRefcount (GncBillTerm *term, gint64 refcount);
void gncBillTermMakeInvisible (GncBillTerm *term);

gboolean gncBillTermGetInvisible (GncBillTerm *term);

/** The gncCloneBillTerm() routine makes a copy of the indicated
 *  bill term, placing it in the indicated book.  It copies
 *  the etc. 
 *  It does not copy parent/child relationships ???
 * XXX the above need fixin....
 *  It then adds a pair of 'gemini' kvp pointers so that each copy
 *  can be found from the other.
 */

GncBillTerm * gncCloneBillTerm (GncBillTerm *from, QofBook *);
#endif /* GNC_BILLTERMP_H_ */

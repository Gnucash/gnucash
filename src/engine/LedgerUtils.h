/*******************************************************************\
 * LedgerUtils.h -- utilities for the ledger window (X-Accountant)  *
 * Copyright (C) 1997 Linas Vepstas                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#ifndef __XACC_LEDGER_UTILS_H__
#define __XACC_LEDGER_UTILS_H__

#include "config.h"

#include "Account.h"

/** PROTOTYPES ******************************************************/

int        accListCount (Account **list);
Account ** accListCopy (Account **list);
Split   ** accListGetSortedSplits (Account **list);
Account ** xaccGroupToList (Account *);


#endif /* __XACC_LEDGER_UTILS_H__ */

/************************** END OF FILE *************************/

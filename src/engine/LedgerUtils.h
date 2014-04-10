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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __XACC_LEDGER_UTILS_H__
#define __XACC_LEDGER_UTILS_H__

#include "config.h"

#include "gnc-common.h"
#include "Account.h"

/** PROTOTYPES ******************************************************/

gncBoolean accListHasAccount (Account **list, Account *findme);
int        accListCount (Account **list);
Account ** accListCopy (Account **list);
Account ** xaccGroupToList (Account *);


#endif /* __XACC_LEDGER_UTILS_H__ */

/************************** END OF FILE *************************/

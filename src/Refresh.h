/*******************************************************************\
 * Refresh.h -- utilities for window refresh for GnuCash            *
 * Copyright (C) 1999,2000 Linas Vepstas                            *
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


#ifndef __XACC_REFRESH_H__
#define __XACC_REFRESH_H__

#include "config.h"

#include <glib.h>

#include "top-level.h"


void gnc_account_ui_refresh(Account *account);

void gnc_account_list_ui_refresh(Account **account_list);

void gnc_account_glist_ui_refresh(GList *accounts);

void gnc_group_ui_refresh(AccountGroup *group);

void gnc_transaction_ui_refresh(Transaction *trans);


#endif /* __XACC_REFRESH_H__ */

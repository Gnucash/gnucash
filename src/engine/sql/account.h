/********************************************************************\
 * account.h -- implements accounts for postgres backend            *
 * Copyright (c) 2000, 2001 Linas Vepstas                           *
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
\********************************************************************/


#ifndef __POSTGRES_ACCOUNT_H__
#define __POSTGRES_ACCOUNT_H__

#include "Group.h"
#include "guid.h"

#include "PostgresBackend.h"

AccountGroup * pgendGetAllAccounts (PGBackend *be, AccountGroup *topgrp);
void pgendStoreGroup (PGBackend *be, AccountGroup *grp);
void pgendStoreGroupNoLock (PGBackend *be, AccountGroup *grp,
                       gboolean do_mark, gboolean do_check_version);
int pgend_account_commit_edit (Backend * bend, Account * acct);

#endif /* __POSTGRES_ACCOUNT_H__ */

/********************************************************************\
 * account.h -- implements accounts for postgres backend            *
 * Copyright (c) 2000, 2001 Linas Vepstas <linas@linas.org>         *
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
\********************************************************************/


#ifndef POSTGRES_ACCOUNT_H
#define POSTGRES_ACCOUNT_H

#include "Account.h"
#include "qof.h"

#include "PostgresBackend.h"

void pgendGetAllAccountsInBook (PGBackend *be, QofBook *);

void pgendGetAllAccounts (PGBackend *be);

void pgendStoreAccountTree (PGBackend *be, Account *root);
void pgendStoreAccountTreeNoLock (PGBackend *be, Account *root,
                                  gboolean do_mark, gboolean do_check_version);
Account * pgendCopyAccountToEngine (PGBackend *be, const GUID *acct_guid);

void pgend_account_commit_edit (QofBackend * bend, Account * acct);

#endif /* POSTGRES_ACCOUNT_H */

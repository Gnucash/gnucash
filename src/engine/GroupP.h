/********************************************************************\
 * GroupP.h -- private header file for chart of accounts            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999, 2000 Linas Vepstas               *
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
 * FILE:
 * GroupP.h
 *
 * FUNCTION:
 * This is the *private* account group structure.
 * This header should *not* be included by any code outside of the
 * engine.
 *
 */

#ifndef XACC_GROUP_P_H
#define XACC_GROUP_P_H

#include "config.h"

#include "BackendP.h"
#include "GNCIdP.h"
#include "Transaction.h"
#include "gnc-book.h"
#include "gnc-numeric.h"


/** STRUCTS *********************************************************/
struct account_group_s
{
  /* The flags: */
  unsigned int saved : 1;

  Account *parent;         /* back-pointer to parent */

  AccountList *accounts;   /* list of account pointers */

  GNCBook *book;           /* The book which this group belongs to */

  /* keep track of nesting level of begin/end edit calls */
  gint32 editlevel;
};

/* 
 * The xaccGroupSetBook() routine merely sets the 'book' member
 *    in the structure above.  It does not actually invoke the
 *    backend to physically move the group from one book to another,
 *    and thus is a 'dangerous' routine, because it may not have
 *    the effect that the user was intending (of actually moving 
 *    the book).  
 */

void xaccGroupSetBook (AccountGroup *group, GNCBook *book);

/*
 * The xaccAccountRemoveGroup() subroutine will remove the indicated
 *    account group from its parent account. It will NOT free the
 *    associated memory or otherwise alter the account group: the
 *    account group can now be reparented to a new location.
 *    Note, however, that it will mark the old parents as having
 *    been modified.
 *
 * The xaccGroupRemoveAccount() subroutine will remove the indicated
 *    account from its parent account group. It will NOT free the
 *    associated memory or otherwise alter the account: the account
 *    can now be reparented to a new location.
 *    Note, however, that it will mark the old parents as having
 *    been modified.
 *
 * Both of the above routines are private routines, since they are slightly
 * dangerous:  If the removed group/account is not immediately reparented, 
 * it can be lost, i.e. turn into a memory leak.  If the GUI or other engine
 * user needs to move an account or group from here to there, it should use
 * the 'Insert' routines, such as xaccGroupInsertAccount(), to make the move.
 * The 'Insert' routines will automatically remove the account from its
 * previous location.
 */

void    xaccAccountRemoveGroup (Account *acc);
void    xaccGroupRemoveAccount (AccountGroup *grp, Account *account);

/*
 * The xaccFreeAccountGroup() subroutine will  ...
 * 
 */
void     xaccFreeAccountGroup (AccountGroup *account_group);

#endif

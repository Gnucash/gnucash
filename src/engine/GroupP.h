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

  GNCEntityTable *entity_table; /* table which child accounts must
                                 * be stored in. */

  Account *parent;         /* back-pointer to parent */

  AccountList *accounts;   /* list of account pointers */

  Backend *backend;        /* persistant storage backend */

  GNCBook *book;           /* The book which this group belongs to */
};


void xaccGroupSetBook (AccountGroup *group, GNCBook *book);

#endif

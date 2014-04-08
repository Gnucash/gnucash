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
/********************************************************************\
 * GroupP.h -- the main data structure of the program               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __XACC_ACCOUNT_GROUP_P_H__
#define __XACC_ACCOUNT_GROUP_P_H__

#include "config.h"
#include "Transaction.h"

/** STRUCTS *********************************************************/
struct _account_group {
  /* The flags: */
  unsigned int saved : 1;
  unsigned int new   : 1;
  
  Account *parent;                 /* back-pointer to parent */

  int      numAcc;                 /* number of accounts in array */
  Account  **account;              /* array of account pointers   */

  /* cached parameters */
  double balance;

};

#endif /* __XACC_ACCOUNT_GROUP_P_H__ */

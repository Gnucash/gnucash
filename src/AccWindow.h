/********************************************************************\
 * AccWindow.h -- window for creating new accounts for GnuCash      *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 2000 Dave Peticolas                                *
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

#ifndef __ACC_WINDOW_H__
#define __ACC_WINDOW_H__

#include "config.h"

#include "Account.h"
#include "Group.h"


/** PROTOTYPES ******************************************************/
typedef struct _AccountWindow  AccountWindow;

AccountWindow * gnc_ui_new_account_window (AccountGroup *group);
AccountWindow * gnc_ui_edit_account_window (Account *account);

Account * gnc_ui_new_accounts_from_name_window (const char *name);

void gnc_ui_set_default_new_account_currency (const char *currency);


#endif /* __ACC_WINDOW_H__ */

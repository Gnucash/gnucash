/********************************************************************\
 * window-reconcile.h -- the reconcile window                       *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#ifndef __WINDOW_RECONCILE_H__
#define __WINDOW_RECONCILE_H__

#include <gtk/gtk.h>

#include "config.h"

#include "top-level.h"


/** GLOBALS *********************************************************/

/** STRUCTS *********************************************************/
typedef struct _RecnWindow RecnWindow;

/** PROTOTYPES ******************************************************/
void        recnRefresh(Account *account);
RecnWindow *recnWindow(GtkWidget *parent, Account *account);

void gnc_ui_reconcile_window_raise(RecnWindow * recnData);

/*
 * The xaccDestroyRecnWindow() subroutine can be called from 
 * anywhere to shut down the Register window.  Used primarily when
 * destroying the underlying account.
 */
void       xaccDestroyRecnWindow(Account *);

#endif

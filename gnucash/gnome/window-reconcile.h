/********************************************************************\
 * window-reconcile.h -- the reconcile window                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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

#ifndef WINDOW_RECONCILE_H
#define WINDOW_RECONCILE_H

#include "Account.h"
#include <gtk/gtk.h>


/** STRUCTS *********************************************************/
typedef struct _RecnWindow RecnWindow;


/** PROTOTYPES ******************************************************/

/********************************************************************\
 * recnWindow                                                       *
 *   opens up the window to reconcile an account                    *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to reconcile                       *
 *
 * Return: recnData - the instance of this RecnWindow, or NULL if the
 * user pressed Cancel in the initial date query.
\********************************************************************/
RecnWindow *recnWindow (GtkWidget *parent, Account *account);

/********************************************************************\
 * recnWindowWithBalance
 *
 *   Opens up the window to reconcile an account, but with ending
 *   balance and statement date already given.
 *
 * Args:   parent         - The parent widget of the new window
 *         account        - The account to reconcile
 *         new_ending     - The amount for ending balance
 *         statement_date - The date of the statement
 * Return: recnData - the instance of this RecnWindow
\********************************************************************/
RecnWindow *recnWindowWithBalance (GtkWidget *parent,
                                   Account *account,
                                   gnc_numeric new_ending,
                                   time64 statement_date);

void gnc_ui_reconcile_window_raise(RecnWindow * recnData);

#endif /* WINDOW_RECONCILE_H */

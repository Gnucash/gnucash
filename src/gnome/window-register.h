/*******************************************************************\
 * window-register.h -- public GnuCash register functions           *
 * Copyright (C) 1998,1999,2000 Linas Vepstas                       *
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

#ifndef WINDOW_REGISTER_H
#define WINDOW_REGISTER_H

#include "gnc-ledger-display.h"
#include "gnc-split-reg.h"

/** STRUCTS *********************************************************/
typedef struct _RegWindow RegWindow;
/* Getters */
GtkWidget *gnc_RegWindow_window (RegWindow *data);
GNCLedgerDisplay *gnc_RegWindow_ledger (RegWindow *data);

/** PROTOTYPES ******************************************************/
GNCSplitReg* regWindowSimple(Account *account);
GNCSplitReg* regWindowAccGroup(Account *account_group);

RegWindow* regWindowLedger(GNCLedgerDisplay *ledger);

gpointer gnc_RegWindow_get_pcd (RegWindow *data);
void     gnc_RegWindow_set_pcd (RegWindow *data, gpointer);

void gnc_register_raise(RegWindow *regData);
void gnc_register_jump_to_blank(RegWindow *regData);
void gnc_register_jump_to_split(RegWindow *regData, Split *split);
void gnc_register_jump_to_split_amount(RegWindow *regData, Split *split);

#endif

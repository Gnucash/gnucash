/********************************************************************\
 * gnc-hbci-cb.h -- hbci callback functions                         *
 * Copyright (C) 2002 Christian Stimming                            *
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

#ifndef GNC_HBCI_CB_H
#define GNC_HBCI_CB_H

#include <gnome.h>

/* Callback from account tree window to invoke Get Balance. */
void
gnc_hbci_acct_tree_menu_getbalance_cb (GtkWidget * widget, 
                                       GnomeMDIChild * child);

/* Callback from account tree window to invoke Get Transactions (not
 * yet implemented). */
void
gnc_hbci_acct_tree_menu_gettrans_cb (GtkWidget * widget, 
				     GnomeMDIChild * child);

#endif /* GNC_HBCI_CB_H */

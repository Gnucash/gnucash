/*
 * gnc-ab-get-balance.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file gnc-ab-getbalance.h
 * @brief AqBanking getbalance functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_AB_GETBALANCE_H
#define GNC_AB_GETBALANCE_H

#include <gtk/gtk.h>

#include "Account.h"

G_BEGIN_DECLS

/**
 * Execute a GetBalance job, show the resulting balance and offer to reconcile
 * the GnuCash account.
 *
 * @param parent Widget to use as parent, may be NULL
 * @param gnc_acc GnuCash account to fetch balance for
 */
void gnc_ab_getbalance(GtkWidget *parent, Account *gnc_acc);

G_END_DECLS

#endif /* GNC_AB_GETBALANCE_H */
/** @} */
/** @} */

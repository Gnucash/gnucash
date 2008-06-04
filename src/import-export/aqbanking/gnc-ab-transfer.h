/*
 * gnc-ab-transfer.h --
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
 * @file gnc-ab-transfer.h
 * @brief Dialog for AqBanking transaction data
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_AB_TRANSFER_H
#define GNC_AB_TRANSFER_H

#include <gtk/gtk.h>

#include "Account.h"
#include "dialog-ab-trans.h"

G_BEGIN_DECLS

/**
 * FIXME
 *
 * @param parent Widget to use as parent, may be NULL
 * @param gnc_acc GnuCash account to fetch balance for
 * @param trans_type Type of transaction
 */
void gnc_ab_maketrans(GtkWidget *parent, Account *gnc_acc,
                      GncABTransType trans_type);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_TRANSFER_H */

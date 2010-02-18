/********************************************************************\
 * gnc-hbci-transfer.h -- hbci transfer functions                   *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_HBCI_TRANSFER_H
#define GNC_HBCI_TRANSFER_H

#include <aqbanking/jobsingletransfer.h>
#include "Account.h"
#include "dialog-hbcitrans.h"

void
gnc_hbci_maketrans (GtkWidget *parent, Account *gnc_acc,
                    GNC_HBCI_Transtype trans_type);

/** Open a gnucash transfer dialog for gnucash Account gnc_acc and
 * fill in all the values from the AB_TRANSACTION inside the
 * HBCITransDialog. Returns TRUE if the gnucash transaction has been
 * successfully created, FALSE if e.g. the user pressed cancel. */
gboolean
gnc_hbci_maketrans_final(HBCITransDialog *td, Account *gnc_acc,
                         GNC_HBCI_Transtype trans_type);


#endif /* GNC_HBCI_TRANSFER_H */

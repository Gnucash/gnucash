/********************************************************************\
 * gnc-hbci-getbalance.h -- hbci getbalance function                *
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

#ifndef GNC_HBCI_GETBALANCE_H
#define GNC_HBCI_GETBALANCE_H

#include <gnome.h>
#include "Account.h"
#include <openhbci/outboxaccjobs.h>

/** Starts a GetBalance job, adds the job to the HBCI_API, and
 * (currently) calls executeOutbox. */
void
gnc_hbci_getbalance (GtkWidget *parent, Account *gnc_acc);

/** Finalizes all the things that have to be done with a GetBalance
 * job.  Returns true if everything has been finished succesfully. */
gboolean
gnc_hbci_getbalance_finish (GtkWidget *parent, 
			    Account *gnc_acc,
			    const HBCI_OutboxJobGetBalance *balance_job);


#endif /* GNC_HBCI_GETBALANCE_H */

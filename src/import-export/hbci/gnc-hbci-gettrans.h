/********************************************************************\
 * gnc-hbci-gettrans.h -- hbci get transactions function            *
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

#ifndef GNC_HBCI_GETTRANS_H
#define GNC_HBCI_GETTRANS_H

#include <aqbanking/jobgettransactions.h>

#include "import-main-matcher.h"
#include "Account.h"

/** Start a GetTrans job. */
void
gnc_hbci_gettrans (GtkWidget *parent, Account *gnc_acc);

/** Finalizes all the things that have to be done with a GetTrans
 * job.  Returns true if everything has been finished succesfully. */
gboolean
gnc_hbci_gettrans_final(GtkWidget *parent,
                        Account *gnc_acc,
                        const AB_JOB *trans_job,
                        gboolean run_until_done);

/** Finalize the final importing part of a GetTrans job.  Returns true
 * if everything has been finished succesfully. */
gboolean
gnc_hbci_import_final(GtkWidget *parent,
                      Account *gnc_acc,
                      AB_TRANSACTION_LIST2 *trans_list,
                      gboolean run_until_done);

/** user_data struct for the gnc_hbci_trans_list_cb() structure */
struct trans_list_data
{
    Account *gnc_acc;
    GNCImportMainMatcher *importer_generic;
};

/** AB_TRANSACTION_LIST2_foreach callback. The Conversion from HBCI to
   GNC transaction is done here, once for each AB_TRANSACTION.  */
AB_TRANSACTION *gnc_hbci_trans_list_cb(AB_TRANSACTION *imported_trans,
                                       void *user_data);


#endif /* GNC_HBCI_GETTRANS_H */

/********************************************************************\
 * gnc-hbci-utils.h -- hbci utility functions                       *
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

#ifndef GNC_HBCI_UTILS_H
#define GNC_HBCI_UTILS_H

#include <glib.h>
#include <gnome.h>
#include <openhbci/account.h>
#include <openhbci/api.h>
#include "Account.h"
#include "gnc-book.h"

#include "hbci-interaction.h"

/** Create a new HBCI_API and let it load its environment from the
 * configuration file filename. If the file doesn't exist and
 * allowNewFile is set to FALSE, this function returns NULL. If the
 * file exists, but OpenHBCI encountered an error upon opening, then
 * an error will be displayed, and NULL will be returned. 
 * 
 * @param filename The name of the OpenHBCI configuration file to use.
 * @param allowNewFile If true, non-existent filename is accepted as well.
 * @param parent When displaying dialogs, use this GtkWidget as parent.
 * @param inter Reference to a GNCInteractor-pointer in order to use this later. 
 * May be NULL.
 */
HBCI_API * gnc_hbci_api_new (const char *filename, 
			     gboolean allowNewFile, 
			     GtkWidget *parent,
			     GNCInteractor **inter);

/** Same as above, but takes the filename already from the current
 * book's kvp frame AND caches a pointer to the api. Returns NULL if
 * the file from the book's kvp frame doesn't exist. Returns NULL also
 * when there was an error upon opening that file.
 *
 * @param parent When displaying dialogs, use this GtkWidget as parent.
 * @param inter Reference to a GNCInteractor-pointer in order to use this later. 
 * May be NULL.
 */ 
HBCI_API * gnc_hbci_api_new_currentbook (GtkWidget *parent,
					 GNCInteractor **inter);

/** Delete the given HBCI_API. If this is also the one that was cached
    by gnc_hbci_api_new_currentbook, then that reference is deleted, too. */
void gnc_hbci_api_delete (HBCI_API *api);


/** Save this API to the config file given in the current book. Return
 * an error if one occurred, or if no filename was found in the
 * current book. */
HBCI_Error * gnc_hbci_api_save (const HBCI_API *api);


/* Get the corresponding HBCI account to a gnucash account. Of course
 * this only works after the gnucash account has been set up for HBCI
 * use, i.e. the kvp_frame "hbci/..." have been filled with
 * information. Returns NULL if no HBCI_Account was found.
 *
 * @param api The HBCI_API to get the HBCI_Account from.
 * @param gnc_acc The gnucash account to query for HBCI_Account reference data. */
const HBCI_Account *
gnc_hbci_get_hbci_acc (const HBCI_API *api, Account *gnc_acc);


/* Make a lot of debugging messages about this outboxjob.  */
void 
gnc_hbci_debug_outboxjob (HBCI_OutboxJob *job);

/* Check HBCI_Error on whether some feedback should be given to the
 * user. Returns true if the HBCI action should be tried again; on the
 * other hand, returns false if the user can't do anything about this
 * error right now. */
gboolean
gnc_hbci_error_retry (GtkWidget *parent, HBCI_Error *error, 
		      GNCInteractor *inter);

/* Calls HBCI_API_executeQueue with some supplementary stuff around
 * it: set the debugLevel, show the GNCInteractor, and do some error
 * checking. Returns TRUE upon success or FALSE if the calling dialog
 * should abort. */
gboolean
gnc_hbci_api_execute (GtkWidget *parent, HBCI_API *api,
		      HBCI_OutboxJob *job, GNCInteractor *inter);



#endif

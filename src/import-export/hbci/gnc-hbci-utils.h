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
#include <openhbci/account.h>
#include <openhbci/api.h>
#include "Account.h"
#include "gnc-book.h"

/* Create a new HBCI_API and let it load its environment from the
 * configuration file filename. If the file doesn't exist, this
 * function returns NULL. If the file exists, but OpenHBCI encountered
 * an error upon opening, then an error will be displayed, and NULL
 * will be returned.*/
HBCI_API * gnc_hbci_api_new (const char *filename);

/* Same as above, but takes the filename already from the current
   book's kvp frame. */ 
HBCI_API * gnc_hbci_api_new_currentbook ();


/* Get the corresponding HBCI account to a gnucash account. Of course
 * this only works after the gnucash account has been set up for HBCI
 * use, i.e. the kvp_frame "hbci/..." have been filled with
 * information. Returns NULL if no HBCI_Account was found. */
const HBCI_Account *
gnc_hbci_get_hbci_acc (const HBCI_API *api, Account *gnc_acc);



#endif

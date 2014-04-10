/********************************************************************\
 * druid-hbci-utils.h -- hbci  creation functionality               *
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

#ifndef DRUID_HBCI_UTILS_H
#define DRUID_HBCI_UTILS_H

#include <gnome.h>
#include <openhbci/api.h>
#include "hbci-interaction.h"


/** Save the reference strings to the HBCI accounts in the kvp's of
 * the gnucash accounts. Each value will only be set if it is
 * different to the value stored beforehand. If any value has been
 * set, the book and/or accounts will be marked 'dirty'. */
void accounts_save_kvp (GHashTable *hash);


/** Update the account list in all the banks stored in this
 * HBCI_API. Straightforward, if we have only one bank and one user
 * with one customer. All other cases are not currently
 * implemented. */
void update_accounts (GtkWidget *parent, HBCI_API *api, 
		      GNCInteractor *inter);

/** Builds a new hash table mapping all HBCI accounts to Gnucash
 * accounts, where the Gnucash accounts already have the reference
 * strings stored in their kvp's. */
GHashTable *
gnc_hbci_new_hash_from_kvp (HBCI_API *api);

gboolean 
gnc_verify_exist_or_new_file (GtkWidget *parent, const char *filename);

gboolean
gnc_test_dir_exist_error (GtkWidget *parent, const char *filename);


#endif

/********************************************************************\
 * gncAddress.h -- an Address object                                *
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

/*
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ADDRESS_H_
#define GNC_ADDRESS_H_

#include "qofbook.h"
#include "qofid.h"

#define GNC_ADDRESS_MODULE_NAME        "gncAddress"

typedef struct _gncAddress GncAddress;

/* Create/Destroy functions */
GncAddress * gncAddressCreate (QofBook *book, QofEntity *parent);
void gncAddressDestroy (GncAddress *addr);

/* Set functions */

void gncAddressSetName (GncAddress *addr, const char *name);
void gncAddressSetAddr1 (GncAddress *addr, const char *addr1);
void gncAddressSetAddr2 (GncAddress *addr, const char *addr2);
void gncAddressSetAddr3 (GncAddress *addr, const char *addr3);
void gncAddressSetAddr4 (GncAddress *addr, const char *addr4);
void gncAddressSetPhone (GncAddress *addr, const char *phone);
void gncAddressSetFax (GncAddress *addr, const char *fax);
void gncAddressSetEmail (GncAddress *addr, const char *email);
void gncAddressClearDirty (GncAddress *address);

/* Get Functions */

const char * gncAddressGetName (const GncAddress *addr);
const char * gncAddressGetAddr1 (const GncAddress *addr);
const char * gncAddressGetAddr2 (const GncAddress *addr);
const char * gncAddressGetAddr3 (const GncAddress *addr);
const char * gncAddressGetAddr4 (const GncAddress *addr);
const char * gncAddressGetPhone (const GncAddress *addr);
const char * gncAddressGetFax (const GncAddress *addr);
const char * gncAddressGetEmail (const GncAddress *addr);
gboolean gncAddressIsDirty (const GncAddress *addr);

int gncAddressCompare (const GncAddress *a, const GncAddress *b);

#define ADDRESS_NAME    "name"
#define ADDRESS_PHONE   "phone"
#define ADDRESS_FAX     "fax"
#define ADDRESS_EMAIL   "email"

#endif /* GNC_ADDRESS_H_ */

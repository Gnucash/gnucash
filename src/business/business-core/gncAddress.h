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
/** @addtogroup Business
    @{ */
/** @addtogroup Address

An address belongs to another object, determined by the ::GncOwner.
It is the owner that assigns a name and identifier to the address.
In effect, an address is just a building - to make it useful to
GnuCash, it needs to be tied to a person. After all, you cannot
invoice a building, you invoice a person working / living in the
building.

QOF needs to handle all objects generically and to tie the address
to an owner, QOF must be able to find each - as entities.

This allows QOF to follow the hierarchy of objects without having
to call any application-specific routines.

To achieve this, new GncAddress routines have been added. An address
is now created with a NULL parent and the parent set explicitly using
the QOF object declaration. Whilst this adds functionality, it is
important that a valid ::GncOwner entity is always set as a parent.
This is an API issue - QOF will always set the parent provided that
a suitable entity is passed to the qofAddressSetOwner routine. It is
up to you to pass a suitable entity.

    @{ */
/** @file gncAddress.h
    @brief an Address object
    @author Copyright (C) 2001 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#ifndef GNC_ADDRESS_H_
#define GNC_ADDRESS_H_

#include "qofbook.h"
#include "qofid.h"
#include "qofobject.h"
#include "qofinstance.h"
#include "qofid-p.h"

#define GNC_ADDRESS_MODULE_NAME        "gncAddress"
#define GNC_ID_ADDRESS GNC_ADDRESS_MODULE_NAME
/** \struct GncAddress

@param	QofBook *	book;
@param	QofEntity * parent;
@param	gboolean	dirty;
@param	char *	name;
@param	char *	addr1;
@param	char *	addr2;
@param	char *	addr3;
@param	char *	addr4;
@param	char *	phone;
@param	char *	fax;
@param	char *	email;
*/
typedef struct _gncAddress GncAddress;

/** @name Create/Destroy functions 
 @{ */
/** create a new address */
GncAddress * gncAddressCreate (QofBook *book, QofEntity *parent);
/** \brief QOF address creation 

An address cannot exist without a parent, yet to merge and 
export the parent, a QOF address object must exist and it 
must be created using standard QOF calls. QOF will always 
set the parent.
*/
GncAddress* qofAddressCreate (QofBook *book);

/** destroy an address */
void gncAddressDestroy (GncAddress *addr);

/** @} */

/** @name Set functions 
 @{ */

void gncAddressSetName (GncAddress *addr, const char *name);
void gncAddressSetAddr1 (GncAddress *addr, const char *addr1);
void gncAddressSetAddr2 (GncAddress *addr, const char *addr2);
void gncAddressSetAddr3 (GncAddress *addr, const char *addr3);
void gncAddressSetAddr4 (GncAddress *addr, const char *addr4);
void gncAddressSetPhone (GncAddress *addr, const char *phone);
void gncAddressSetFax (GncAddress *addr, const char *fax);
void gncAddressSetEmail (GncAddress *addr, const char *email);
void gncAddressClearDirty (GncAddress *address);
/** \brief Set the address owner.

\note Every address must have a genuine ::GncOwner as this 
provides the name or identifier to use the address.

In order to export a Customer, QOF must be able to find the
address as an entity.
*/
void qofAddressSetOwner (GncAddress *address, QofEntity *owner);
/** @} */

/** @name Get Functions 
 @{ */

const char * gncAddressGetName (const GncAddress *addr);
const char * gncAddressGetAddr1 (const GncAddress *addr);
const char * gncAddressGetAddr2 (const GncAddress *addr);
const char * gncAddressGetAddr3 (const GncAddress *addr);
const char * gncAddressGetAddr4 (const GncAddress *addr);
const char * gncAddressGetPhone (const GncAddress *addr);
const char * gncAddressGetFax (const GncAddress *addr);
const char * gncAddressGetEmail (const GncAddress *addr);
QofEntity*   qofAddressGetOwner (GncAddress *addr);
/** @} */

gboolean gncAddressIsDirty (const GncAddress *addr);

/** \brief compare two addresses 

\return 0 if identical, -1 if a is empty or less than b 
and +1 if a is more than b or if b is empty. 
*/
int gncAddressCompare (const GncAddress *a, const GncAddress *b);

#define ADDRESS_NAME    "name"
#define ADDRESS_ONE		"number"
#define ADDRESS_TWO		"street"
#define ADDRESS_THREE   "locality"
#define ADDRESS_FOUR	"city"
#define ADDRESS_PHONE   "phone"
#define ADDRESS_FAX     "fax"
#define ADDRESS_EMAIL   "email"
#define ADDRESS_OWNER   "owner"

#endif /* GNC_ADDRESS_H_ */
/** @} */
/** @} */

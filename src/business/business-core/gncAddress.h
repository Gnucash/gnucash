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
    @{ */
/** @file gncAddress.h
    @brief an Address object
    @author Copyright (C) 2001 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_ADDRESS_H_
#define GNC_ADDRESS_H_

#include "qofbook.h"
#include "qofid.h"
#include "qofobject.h"
#include "qofinstance.h"
#include "qofid-p.h"

#define GNC_ADDRESS_MODULE_NAME        "gncAddress"

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

/** @name Create/Destroy functions */
/** @{ */
/** create a new address */
GncAddress * gncAddressCreate (QofBook *book, QofEntity *parent);
/** destroy an address */
void gncAddressDestroy (GncAddress *addr);

/** @} */

/** @name Set functions */
/** @{ */

void gncAddressSetName (GncAddress *addr, const char *name);
void gncAddressSetAddr1 (GncAddress *addr, const char *addr1);
void gncAddressSetAddr2 (GncAddress *addr, const char *addr2);
void gncAddressSetAddr3 (GncAddress *addr, const char *addr3);
void gncAddressSetAddr4 (GncAddress *addr, const char *addr4);
void gncAddressSetPhone (GncAddress *addr, const char *phone);
void gncAddressSetFax (GncAddress *addr, const char *fax);
void gncAddressSetEmail (GncAddress *addr, const char *email);
void gncAddressClearDirty (GncAddress *address);
/** @} */

/** @name Get Functions */
/** @{ */

const char * gncAddressGetName (const GncAddress *addr);
const char * gncAddressGetAddr1 (const GncAddress *addr);
const char * gncAddressGetAddr2 (const GncAddress *addr);
const char * gncAddressGetAddr3 (const GncAddress *addr);
const char * gncAddressGetAddr4 (const GncAddress *addr);
const char * gncAddressGetPhone (const GncAddress *addr);
const char * gncAddressGetFax (const GncAddress *addr);
const char * gncAddressGetEmail (const GncAddress *addr);
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

#endif /* GNC_ADDRESS_H_ */
/** @} */
/** @} */

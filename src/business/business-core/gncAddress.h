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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include "qof.h"
#ifdef GNUCASH_MAJOR_VERSION
#include "gncBusiness.h"
#endif

#define GNC_ADDRESS_MODULE_NAME        "gncAddress"
#define GNC_ID_ADDRESS GNC_ADDRESS_MODULE_NAME
/** \struct GncAddress

@param  QofInstance The address instance.
@param	QofBook*	  Copy of the book pointer.
@param	QofInstance* parent entity.
@param	gboolean	dirty flag
@param	char*	name of addressee
@param	char*	first line of address
@param	char*	second line of address
@param	char*	third line of address
@param	char*	fourth line of address
@param	char*	phone number
@param	char*	fax number
@param	char*	email address
*/
typedef struct _gncAddress GncAddress;
typedef struct _gncAddressClass GncAddressClass;

/* --- type macros --- */
#define GNC_TYPE_ADDRESS            (gnc_address_get_type ())
#define GNC_ADDRESS(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_ADDRESS, GncAddress))
#define GNC_ADDRESS_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_ADDRESS, GncAddressClass))
#define GNC_IS_ADDRESS(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_ADDRESS))
#define GNC_IS_ADDRESS_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_ADDRESS))
#define GNC_ADDRESS_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_ADDRESS, GncAddressClass))
GType gnc_address_get_type(void);

/** @name Create/Destroy functions
 @{ */
GncAddress *gncAddressCreate (QofBook *book, QofInstance *parent);
void gncAddressDestroy (GncAddress *addr);
void gncAddressBeginEdit (GncAddress *addr);
void gncAddressCommitEdit (GncAddress *addr);

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
/** @} */

gboolean gncAddressIsDirty (const GncAddress *addr);

/** \brief compare two addresses

\return 0 if identical, -1 if a is empty or less than b
and +1 if a is more than b or if b is empty.
*/
int gncAddressCompare (const GncAddress *a, const GncAddress *b);

/** \brief Deeply compare two addresses

\return TRUE if all fields match, FALSE otherwise
*/
gboolean gncAddressEqual(const GncAddress *a, const GncAddress *b);

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

/********************************************************************\
 * gnc-addr-quickfill.h -- Create an address line quick-fill  *
 * Copyright (C) 2011 Christian Stimming <christian@cstimming.de>   *
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
/** @addtogroup QuickFill Auto-complete typed user input.
   @{
*/
/** Similar to the @ref Account_QuickFill account name quickfill, we
 * create a cached quickfill with the address lines of all GncAddress.
*/

#ifndef GNC_ADDR_QUICKFILL_H
#define GNC_ADDR_QUICKFILL_H

#include "qof.h"
#include "app-utils/QuickFill.h"

/** Create/fetch a quickfill GncAddress description strings on the Addr2 part.
 *
 *  Multiple, distinct quickfills, for different uses, are allowed.
 *  Each is identified with the 'key'.  Be sure to use distinct,
 *  unique keys that don't conflict with other users of QofBook.
 *
 *  This code listens to GncAddress creation events, and automatically
 *  adds new items to the quickfill list.  This code also listens to
 *  the item deletion events and removes those entries from the
 *  quickfill; however, this does not yet seem to fully remove them
 *  from the GUI.
 *
 * \param book The book
 * \param key The identifier to look up the shared object in the book
 *
 * \return The shared QuickFill object which is created on first
 * calling of this function and subsequently looked up in the book by
 * using the key.
 */
QuickFill * gnc_get_shared_address_addr2_quickfill (QofBook *book,
        const char * key);

/** Create/fetch a quickfill GncAddress description strings on the
 * Addr3 part.
 *
 * Identical to gnc_get_shared_address_addr2_quickfill(). You should
 * also use the same key as for the other function because the
 * internal quickfills are updated simultaneously.
 */
QuickFill * gnc_get_shared_address_addr3_quickfill (QofBook *book,
        const char * key);

/** Create/fetch a quickfill GncAddress description strings on the
 * Addr4 part.
 *
 * Identical to gnc_get_shared_address_addr2_quickfill(). You should
 * also use the same key as for the other function because the
 * internal quickfills are updated simultaneously.
 */
QuickFill * gnc_get_shared_address_addr4_quickfill (QofBook *book,
        const char * key);

#endif

/** @} */
/** @} */

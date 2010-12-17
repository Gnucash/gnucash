/********************************************************************\
 * gnc-entry-quickfill.h -- Create an entry description quick-fill  *
 * Copyright (C) 2010 Christian Stimming <christian@cstimming.de>   *
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
 * create a cached quickfill with the description of all entries.
*/

#ifndef GNC_ENTRY_QUICKFILL_H
#define GNC_ENTRY_QUICKFILL_H

#include "qof.h"
#include "engine/gncEntry.h"
#include "gnome-utils/QuickFill.h"

/** Create/fetch a quickfill GncEntry description strings.
 *
 *  Multiple, distinct quickfills, for different uses, are allowed.
 *  Each is identified with the 'key'.  Be sure to use distinct,
 *  unique keys that don't conflict with other users of QofBook.
 *
 *  This code listens to entry creation events, and automatically adds
 *  new entry's descriptions to the quickfill list.  This code also
 *  listens to the entry's deletion events and removes those
 *  descriptions from the quickfill; however, this does not yet seem
 *  to fully remove them from the GUI.
 */
QuickFill * gnc_get_shared_entry_desc_quickfill (QofBook *book,
        const char * key);

#endif

/** @} */
/** @} */

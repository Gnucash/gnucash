/********************************************************************\
 * gncOrder.h -- the Core Business Order Interface                  *
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
/* NOTE: Removed from doxygen by warlord on 2004-05-07 because
 * this module is not fully implemented at this time.
 */
/* @addtogroup Business
    @{ */
/* @addtogroup Order
    @{ */
/* @file gncOrder.h
    @brief Business Order Interface
    @author Copyright (C) 2001 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_ORDER_H_
#define GNC_ORDER_H_

typedef struct _gncOrder GncOrder;
typedef struct _gncOrderClass GncOrderClass;

#include "gncEntry.h"
#include "gncOwner.h"
#include "qof.h"

#define GNC_ID_ORDER "gncOrder"

/* --- type macros --- */
#define GNC_TYPE_ORDER            (gnc_order_get_type ())
#define GNC_ORDER(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_ORDER, GncOrder))
#define GNC_ORDER_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_ORDER, GncOrderClass))
#define GNC_IS_ORDER(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_ORDER))
#define GNC_IS_ORDER_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_ORDER))
#define GNC_ORDER_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_ORDER, GncOrderClass))
GType gnc_order_get_type(void);

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (QofBook *book);
void gncOrderDestroy (GncOrder *order);

/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id);
void gncOrderSetOwner (GncOrder *order, GncOwner *owner);
void gncOrderSetDateOpened (GncOrder *order, Timespec date);
void gncOrderSetDateClosed (GncOrder *order, Timespec date);
void gncOrderSetNotes (GncOrder *order, const char *notes);
void gncOrderSetReference (GncOrder *order, const char *reference);
void gncOrderSetActive (GncOrder *order, gboolean active);

/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry);
void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry);

/* Get Functions */

const char * gncOrderGetID (const GncOrder *order);
GncOwner * gncOrderGetOwner (GncOrder *order);
Timespec gncOrderGetDateOpened (const GncOrder *order);
Timespec gncOrderGetDateClosed (const GncOrder *order);
const char * gncOrderGetNotes (const GncOrder *order);
const char * gncOrderGetReference (const GncOrder *order);
gboolean gncOrderGetActive (const GncOrder *order);

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order);

void gncOrderBeginEdit (GncOrder *order);
void gncOrderCommitEdit (GncOrder *order);
int gncOrderCompare (const GncOrder *a, const GncOrder *b);

gboolean gncOrderIsClosed (const GncOrder *order);

/** Return a pointer to the instance gncOrder that is identified
 *  by the guid, and is residing in the book. Returns NULL if the 
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncOrder * gncOrderLookup (QofBook *book, const GUID *guid);
 */
#define gncOrderLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_ORDER, GncOrder)

#define ORDER_ID	"id"
#define ORDER_REFERENCE	"reference"
#define ORDER_OWNER	"owner"
#define ORDER_OPENED	"date_opened"
#define ORDER_CLOSED	"date_closed"
#define ORDER_IS_CLOSED	"is_closed?"
#define ORDER_NOTES	"notes"

/** deprecated functions */
#define gncOrderGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))
#define gncOrderGetBook(x) qof_instance_get_book(QOF_INSTANCE(x))

#endif /* GNC_ORDER_H_ */
/** @} */
/** @} */

/********************************************************************
 * gnc-event.h  -- engine-level events for Gnucash                  *
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
 ********************************************************************/

#ifndef GNC_EVENT_H
#define GNC_EVENT_H

#include <glib.h>
#include <qof.h>

typedef struct
{
    gpointer node;
    gint idx;
} GncEventData;

/* These events are used when a split is added to an account.
 * The event subject is the Account, the Object is the Split.
 */
#define GNC_EVENT_ITEM_ADDED	QOF_MAKE_EVENT(QOF_EVENT_BASE+0)
#define GNC_EVENT_ITEM_REMOVED	QOF_MAKE_EVENT(QOF_EVENT_BASE+1)
#define GNC_EVENT_ITEM_CHANGED	QOF_MAKE_EVENT(QOF_EVENT_BASE+2)

#endif

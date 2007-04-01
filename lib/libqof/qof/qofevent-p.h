/********************************************************************
 * gnc-event-p.h -- private engine event handling interface         *
 * Copyright 2000 Dave Peticolas <dave@krondo.com>                  *
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

#ifndef QOF_EVENT_P_H
#define QOF_EVENT_P_H

#include "qofevent.h"
#include "qofid.h"

/* for backwards compatibility - to be moved back to qofevent.c in libqof2 */
typedef struct
{
#ifndef QOF_DISABLE_DEPRECATED
  GNCEngineEventHandler old_handler;        /** \deprecated */
#endif
  QofEventHandler handler;
  gpointer user_data;

  gint handler_id;
} HandlerInfo;

/** \deprecated Prevents handlers locating the QofCollection or casting
to the QofInstance and locating the book, editlevel or dirty flag.
Use qof_event_gen instead.
*/
void
qof_event_generate (const GUID *guid, QofIdType e_type, 
					QofEventId event_id);

/* generates an event even when events are suspended! */
void qof_event_force (QofInstance *entity, QofEventId event_id, gpointer event_data);

#endif

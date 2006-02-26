/********************************************************************
 * qofevent.h -- QOF event handling interface                       *
 * Copyright 2000 Dave Peticolas <dave@krondo.com>                  *
 * Copyright 2006 Neil Williams  <linux@codehelp.co.uk>             *
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

/** @addtogroup Event
@{
*/
/** @file qofevent.h
    @brief QOF event handling interface
	@author Copyright 2000 Dave Peticolas <dave@krondo.com>
	@author Copyright 2006 Neil Williams  <linux@codehelp.co.uk>
*/

#ifndef QOF_EVENT_H
#define QOF_EVENT_H

#include <glib.h>
#include "qof.h"

/** Define the type of events allowed. */
typedef gint QofEventId;

/** \brief Default events for backwards compatibility.

These defaults merely replicate previous behaviour,
any process can define their own events. 
*/
#define QOF_EVENT_NONE     (0)
#define QOF_EVENT_CREATE   (1 << 0)
#define QOF_EVENT_MODIFY   (1 << 1)
#define QOF_EVENT_DESTROY  (1 << 2)
#define QOF_EVENT_ADD      (1 << 3)
#define QOF_EVENT_REMOVE   (1 << 4)
#define QOF_EVENT__LAST    QOF_EVENT_REMOVE 
#define QOF_EVENT_ALL      (0xff)

/** Allow scope for more defaults in future. Additional
event identifiers must be larger than this. */
#define QOF_DEFAULT_EVENT_LIMIT  QOF_EVENT__LAST

/** \brief Handler invoked when an event is generated.
 *
 * @param ent:      Entity generating the event
 * @param event_type:  The id of the event, including additional identifiers and
 	the older defaults.
 * @param handler_data:   data supplied when handler was registered.
 * @param event_data:   data to be supplied when handler is invoked.
 */
typedef void (*QofEventHandler) (QofEntity *ent,  QofEventId event_type,
                               gpointer handler_data, gpointer event_data);

/** \brief Register a handler for events.
 *
 * @param handler:   handler to register
 * @param handler_data: data provided when handler is invoked
 *
 * @return id identifying handler
 */
gint qof_event_register_handler (QofEventHandler handler, gpointer handler_data);

/** \brief Unregister an event handler.
 *
 * @param handler_id: the id of the handler to unregister
 */
void qof_event_unregister_handler (gint handler_id);

/** \brief Invoke all registered event handlers using the given arguments.

   Certain default events are used by QOF:

-QOF_EVENT_DEFAULT_CREATE events should be generated after the object
    has been created and registered in the engine entity table.
-QOF_EVENT_DEFAULT_MODIFY events should be generated whenever any data
     member or submember (i.e., splits) is changed.
-QOF_EVENT_DEFAULT_DESTROY events should be called before the object
     has been destroyed or removed from the entity table.

   Any other events are entirely the concern of the application.

 \note QofEventHandler routines do \b NOT support generating
 events from a GUID and QofIdType - you must specify a genuine QofEntity.

 @param entity:     the entity generating the event
 @param event_type: the name of the event.
 @param event_data: Data to be passed to the event handler just for
 this one event. Can be NULL.
*/
void qof_event_gen (QofEntity *entity, QofEventId event_type, 
                    gpointer event_data);

/** \brief  Suspend all engine events.
 *
 *    This function may be called multiple times. To resume event generation,
 *   an equal number of calls to gnc_engine_resume_events
 *   must be made.
 */
void qof_event_suspend (void);

/** Resume engine event generation. */
void qof_event_resume (void);

#endif
/** @} */

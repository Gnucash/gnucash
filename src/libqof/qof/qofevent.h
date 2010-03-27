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

#include "qof.h"

/** Define the type of events allowed. */
typedef gint QofEventId;

/** \brief Allow application-specific events to be created.

Used together with QOF_EVENT_BASE to simplify creation
of application events without interfering with any new
events added within QOF.

\verbatim
#define APP_EVENT_A QOF_MAKE_EVENT(QOF_EVENT_BASE+0)
#define APP_EVENT_B QOF_MAKE_EVENT(QOF_EVENT_BASE+1)
\endverbatim
*/
#define QOF_MAKE_EVENT(x)    (1<<(x))

/** Allow scope for more defaults in future. Additional
event identifiers must be based on this when using QOF_MAKE_EVENT(). */
#define QOF_EVENT_BASE 8

/** \brief Default events for backwards compatibility.

These defaults merely replicate previous behaviour,
any process can define their own events.

\note events 5, 6, and 7 are "undefined" as of v0.6.3
for future libqof1 or libqof2 usage.
*/
#define QOF_EVENT_NONE     (0)
#define QOF_EVENT_CREATE   QOF_MAKE_EVENT(0)
#define QOF_EVENT_MODIFY   QOF_MAKE_EVENT(1)
#define QOF_EVENT_DESTROY  QOF_MAKE_EVENT(2)
#define QOF_EVENT_ADD      QOF_MAKE_EVENT(3)
#define QOF_EVENT_REMOVE   QOF_MAKE_EVENT(4)
#define QOF_EVENT__LAST    QOF_MAKE_EVENT(QOF_EVENT_BASE-1)
#define QOF_EVENT_ALL      (0xff)

/** \brief Handler invoked when an event is generated.
 *
 * @param ent:      Entity generating the event
 * @param event_type:  The id of the event, including additional identifiers and
 	the older defaults.
 * @param handler_data:   data supplied when handler was registered.
 * @param event_data:   data to be supplied when handler is invoked.
 */
typedef void (*QofEventHandler) (QofInstance *ent,  QofEventId event_type,
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

- QOF_EVENT_DEFAULT_CREATE events should be generated after the object
    has been created and registered in the engine entity table.
- QOF_EVENT_DEFAULT_MODIFY events should be generated whenever any data
     member or submember (i.e., splits) is changed.
- QOF_EVENT_DEFAULT_DESTROY events should be called before the object
     has been destroyed or removed from the entity table.

   Any other events are entirely the concern of the application.

 \note QofEventHandler routines do \b NOT support generating
 events from a GncGUID and QofIdType - you must specify a genuine QofInstance.

 @param entity:     the entity generating the event
 @param event_type: the name of the event.
 @param event_data: Data to be passed to the event handler just for
 this one event. Can be NULL.
*/
void qof_event_gen (QofInstance *entity, QofEventId event_type,
                    gpointer event_data);

/** \brief  Suspend all engine events.
 *
 *    This function may be called multiple times. To resume event generation,
 *   an equal number of calls to qof_event_resume
 *   must be made.
 */
void qof_event_suspend (void);

/** Resume engine event generation. */
void qof_event_resume (void);

#endif
/** @} */

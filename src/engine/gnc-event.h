/********************************************************************
 * gnc-event.h -- engine event handling interface                   *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#ifndef __GNC_EVENT_H__
#define __GNC_EVENT_H__

#include <glib.h>

#include "guid.h"


typedef enum
{
  GNC_EVENT_CREATE  = 1 << 0,
  GNC_EVENT_MODIFY  = 1 << 1,
  GNC_EVENT_DESTROY = 1 << 2
} GNCEngineEventType;

/* GNCEngineEventCallback
 *   Callback invoked when an engine event occurs.
 *
 * entity:      GUID of entity generating event
 * event_type:  one of GNCEngineEventType, not a combination
 * user_data:   user_data supplied when callback was registered.
 */
typedef void (*GNCEngineEventHandler) (GUID *entity,
                                       GNCEngineEventType event_type,
                                       gpointer user_data);

/* gnc_engine_register_event_handler
 *   Register a handler for engine events.
 *
 * handler:   handler to register
 * user_data: data provided in future callbacks
 *
 * Returns: id identifying callback
 */
gint gnc_engine_register_event_handler (GNCEngineEventHandler handler,
                                        gpointer user_data);

/* gnc_engine_unregister_event_handler
 *   Unregister an engine event handler.
 *
 * handler_id: the id of the handler to unregister
 */
void gnc_engine_unregister_event_handler (gint handler_id);

/* gnc_engine_suspend_events
 *   Suspend all engine events. This function may be
 *   called multiple times. To resume event generation,
 *   an equal number of calls to gnc_engine_resume_events
 *   must be made.
 */
void gnc_engine_suspend_events (void);

/* gnc_engine_resume_events
 *   Resume engine event generation.
 */
void gnc_engine_resume_events (void);

#endif

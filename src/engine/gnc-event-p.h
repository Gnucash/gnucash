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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#ifndef __GNC_EVENT_P_H__
#define __GNC_EVENT_P_H__

#include "gnc-event.h"

/* gnc_engine_generate_event
 *   Invoke all registered event handlers using the given arguments.
 *
 *   GNC_EVENT_CREATE events should be generated after the object
 *     has been created and registered in the engine entity table.
 *   GNC_EVENT_MODIFY events should be generated whenever any data
 *     member or submember (i.e., splits) is changed.
 *   GNC_EVENT_DESTROY events should be called before the object
 *     has been destroyed or removed from the entity table.
 *
 * entity:     the GUID of the entity generating the event
 * event_type: the type of event -- this should be one of the
 *             GNCEngineEventType values, not a combination.
 */
void gnc_engine_generate_event (GUID *entity, GNCEngineEventType event_type);

#endif

/********************************************************************\
 * gnc-component-manager.h - GUI component manager interface        *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef GNC_COMPONENT_MANAGER_H
#define GNC_COMPONENT_MANAGER_H

#include <glib.h>

#include "qof.h"


#define NO_COMPONENT (-1)

typedef struct
{
    QofEventId event_mask;
} EventInfo;


/* GNCComponentRefreshHandler
 *   Handler invoked to inform the component that a refresh
 *   may be needed.
 *
 * changes: if NULL, the component should perform a refresh.
 *
 *          if non-NULL, changes is a GncGUID hash that maps
 *          GUIDs to EventInfo structs describing which
 *          events have been received. Entities not in
 *          the hash have not generated any events.
 *          Entities which have been destroyed will be in
 *          the hash, but may not exist anymore.
 *
 *          Note since refreshes may not occur with every change,
 *          an entity may have all three change values.
 *
 *          The component should use 'changes' to determine whether
 *          or not a refresh is needed. The hash table must not be
 *          changed.
 *
 * Notes on refreshing: when the handler is invoked any engine
 *                      entities used by the component may have
 *                      already been deleted. 'Refreshing' the
 *                      component may require closing the component.
 *
 * Notes on dealing with destroyed entities: As stated above, entities
 * in the changes GHashTable may no longer exist.  So how can you
 * determine if this has happened?  Well, it's a good idea to check
 * for the QOF_EVENT_DESTROY bit in the EventInfo structure.  Of
 * course, that means you need the hash key (GncGUID) for the destroyed
 * entity.  How are you going to get the GncGUID from the entity if the
 * entity has already been destroyed?  You're not.  So, you have to
 * save a COPY of the key (GncGUID) away beforehand.
 *
 * user_data: user_data supplied when component was registered.
 */
typedef void (*GNCComponentRefreshHandler) (GHashTable *changes,
        gpointer user_data);

/* GNCComponentCloseHandler
 *   Handler invoked to close the component.
 *
 * user_data: user_data supplied when component was registered.
 *
 * Notes on closing: components are not automatically unregistered
 *                   when the close handler is invoked. Components
 *                   may not ignore the handler.
 */
typedef void (*GNCComponentCloseHandler) (gpointer user_data);


/* GNCComponentFindHandler
 *   Handler invoked when searching for a component.
 *
 * find_data: find_data supplied when search was started.
 * user_data: user_data supplied when component was registered.
 *
 * Return: TRUE if the component matches the search criteria.
 */
typedef gboolean (*GNCComponentFindHandler) (gpointer find_data,
        gpointer user_data);

/* GNCComponentHandler
 *   Generic handler used in iterating over components.
 *
 * component_class: class of component
 * component_id:    id of component
 * iter_data:       user_data supplied by caller
 *
 * Return: TRUE if the callback did something
 */
typedef gboolean (*GNCComponentHandler) (const char *component_class,
        gint component_id,
        gpointer user_data,
        gpointer iter_data);

/* gnc_component_manager_init
 *   Initialize the component manager.
 */
void gnc_component_manager_init (void);

/* gnc_component_manager_shutdown
 *   Shutdown the component manager.
 */
void gnc_component_manager_shutdown (void);

/* gnc_register_gui_component
 *   Register a GUI component with the manager.
 *
 * component_class: a string defining a class of components
 *                  certain component functions can be performed
 *                  on all components in a class. For that reason,
 *                  components in the same class should all use
 *                  the same type for user_data.
 *
 * refresh_cb:      refresh handler, may be NULL
 * close_cb:        close handler, may be NULL
 * user_data:       user_data argument for handlers
 *
 *
 * Notes:           After a refresh handler is registered, the
 *                  component must use the API calls below to
 *                  inform the component manager which engine
 *                  entities are being 'watched', i.e., which
 *                  engine entities may cause the component
 *                  to need refreshing.
 *
 *                  When a component is first registered, it
 *                  is not watching anything, and thus will
 *                  not receive refresh events.
 *
 * Return:          id of component, or NO_COMPONENT, if error
 */
gint gnc_register_gui_component (const char *component_class,
                                 GNCComponentRefreshHandler refresh_handler,
                                 GNCComponentCloseHandler close_handler,
                                 gpointer user_data);

/* gnc_gui_component_set_session
 *   Set the associated session of this component
 *
 * component_id: id of component which is watching the entity
 * session:      the session this component is associated with
 */
void gnc_gui_component_set_session (gint component_id, gpointer session);

/* gnc_gui_component_watch_entity
 *   Add an entity to the list of those being watched by the component.
 *   Only entities with refresh handlers should add watches.
 *
 * component_id: id of component which is watching the entity
 * entity:       id of entity to watch
 * event_mask:   mask which determines which kinds of events are watched
 *               setting the mask to 0 turns off watching for the entity.
 */
void gnc_gui_component_watch_entity (gint component_id,
                                     const GncGUID *entity,
                                     QofEventId event_mask);

/* gnc_gui_component_watch_entity_type
 *   Watch all entities of a particular type.
 *
 * component_id: id of component which is watching the entity type
 * entity_type:  type of entity to watch, either GNC_ID_TRANS or
 *               GNC_ID_ACCOUNT
 * event_mask:   mask which determines which kinds of events are watched
 *               setting the mask to 0 turns off watching for the entity type
 */
void gnc_gui_component_watch_entity_type (gint component_id,
        QofIdTypeConst entity_type,
        QofEventId event_mask);

/* gnc_gui_get_entity_events
 *   Return the event info of the events which have been generated by
 *   the given entity.
 *
 * changes: a hash of changes as in the refresh handler
 * entity:  the GncGUID of the entity to get the event mask for
 *
 * Returns: the event info of the entity, or NULL
 *          if it is not found.
 */
const EventInfo * gnc_gui_get_entity_events (GHashTable *changes,
        const GncGUID *entity);

/* gnc_gui_component_clear_watches
 *   Clear all watches for the component.
 *
 * component_id: id of component to clear watches for.
 */
void gnc_gui_component_clear_watches (gint component_id);

/* gnc_unregister_gui_component
 *   Unregister a gui component from the manager.
 *
 * component_id: id of component to unregister
 */
void gnc_unregister_gui_component (gint component_id);

/* gnc_unregister_gui_component_by_data
 *   Unregister a gui component using the user_data pointer.
 *
 * component_class: class component is in
 * user_data:       user_data pointer of component to unregister
 *                  all components with that user_data in the
 *                  class are unregistered.
 */
void gnc_unregister_gui_component_by_data (const char *component_class,
        gpointer user_data);

/* gnc_suspend_gui_refresh
 *   Suspend refresh handlers by the component manager.
 *   This routine may be called multiple times. Each call
 *   increases the suspend counter (starts at zero).
 */
void gnc_suspend_gui_refresh (void);

/* gnc_resume_gui_refresh
 *   Resume refresh handlers by the component manager.
 *   Each call reduces the suspend counter by one. When
 *   the counter reaches zero, all changes which have
 *   occured since the last refresh are collected and
 *   passed to the components in refresh handlers.
 */
void gnc_resume_gui_refresh (void);

/* gnc_gui_refresh_all
 *   Force all components to refresh.
 *
 *   This routine may only be invoked when the suspend counter
 *   is zero. It should never be mixed with the suspend/resume
 *   refresh routines.
 */
void gnc_gui_refresh_all (void);

/* gnc_gui_refresh_suspended
 *   Return TRUE if gui refreshes are suspended.
 */
gboolean gnc_gui_refresh_suspended (void);

/* gnc_close_gui_component
 *   Invoke the close handler for the indicated component.
 *
 * component_id: id of component to close
 */
void gnc_close_gui_component (gint component_id);

/* gnc_close_gui_component_by_data
 *   Invoke the close handler for components in the given
 *   class with the given user_data.
 *
 * component_class: class to close components in
 * user_data:       user_data of component to close
 *                  all components with that user_data
 *                  are closed.
 */
void gnc_close_gui_component_by_data (const char *component_class,
                                      gpointer user_data);

/* gnc_close_gui_component_by_session
 *   Invoke the close handler for components with the given session
 *
 * session:         session to close
 *                  all components with that session
 *                  are closed.
 */
void gnc_close_gui_component_by_session (gpointer session);

/* gnc_find_gui_components
 *   Search for components in the specified class.
 *
 * component_class:     the class to search for components in
 *                      must be non-NULL
 * find_cb:             the handler used to search for the component
 *                      if NULL, all components in class are returned
 * find_data:           find_data passed to find_cb
 *
 * Returns: GList of user_data of found components, or NULL if none found
 *          The list should be freed with g_list_free().
 *
 * Notes on finding: components should not be registered or unregistered
 *                   by the find callback.
 */
GList * gnc_find_gui_components (const char *component_class,
                                 GNCComponentFindHandler find_handler,
                                 gpointer find_data);

/* gnc_find_first_gui_component
 *   Search for the first matching component in the specified class.
 *
 * component_class:     the class to search for components in
 *                      must be non-NULL
 * find_cb:             the handler used to search for the component
 *                      must be non-null
 * find_data:           find_data passed to find_cb
 *
 * Returns: user_data of first found component, or NULL if none found
 *
 * Notes on finding: components should not be registered or unregistered
 *                   by the find callback.
 */
gpointer gnc_find_first_gui_component (const char *component_class,
                                       GNCComponentFindHandler find_handler,
                                       gpointer find_data);

/* gnc_forall_gui_components
 *   Invoke 'handler' for components in the database.
 *
 * component_class: class to iterate over, if NULL then
 *                  all classes are iterated over
 * handler:         handler to invoke
 * iter_data:       data passed to handler
 *
 * Notes on forall: components may be unregistered by the handler,
 *                  but no components should be registered.
 */
gint gnc_forall_gui_components (const char *component_class,
                                GNCComponentHandler handler,
                                gpointer iter_data);

#endif

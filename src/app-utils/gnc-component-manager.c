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

#include "config.h"

#include <stdio.h>

#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-ui-util.h"


/** Declarations ****************************************************/

#define CM_DEBUG 0

typedef struct
{
    QofIdType entity_type;
    QofEventId event_mask;
} EntityTypeEventInfo;

typedef struct
{
    GHashTable * event_masks;
    GHashTable * entity_events;

    gboolean match;
} ComponentEventInfo;

typedef struct
{
    GNCComponentRefreshHandler refresh_handler;
    GNCComponentCloseHandler close_handler;
    gpointer user_data;

    ComponentEventInfo watch_info;

    char *component_class;
    gint component_id;
    gpointer session;
} ComponentInfo;


/** Static Variables ************************************************/
static guint  suspend_counter = 0;
/* Some code foolishly uses 0 instead of NO_COMPONENT, so we start with 1. */
static gint   next_component_id = 1;
static GList *components = NULL;

static ComponentEventInfo changes = { NULL, NULL, FALSE };
static ComponentEventInfo changes_backup = { NULL, NULL, FALSE };


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


/** Prototypes ******************************************************/
static void gnc_gui_refresh_internal (gboolean force);
static GList * find_component_ids_by_class (const char *component_class);
static gboolean got_events = FALSE;


/** Implementations *************************************************/

#if CM_DEBUG
static void
dump_components (void)
{
    GList *node;

    fprintf (stderr, "Components:\n");

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        fprintf (stderr, "  %s:\t%d\n",
                 ci->component_class ? ci->component_class : "(null)",
                 ci->component_id);
    }

    fprintf (stderr, "\n");
}
#endif

static void
clear_mask_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    QofEventId * et = value;

    *et = 0;
}

/* clear a hash table of the form string --> QofEventId,
 * where the values are g_malloced and the keys are in the engine
 * string cache. */
static void
clear_mask_hash (GHashTable *hash)
{
    if (hash == NULL)
        return;

    g_hash_table_foreach (hash, clear_mask_hash_helper, NULL);
}

static gboolean
destroy_mask_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    qof_string_cache_remove (key);
    g_free (value);

    return TRUE;
}

static void
destroy_mask_hash (GHashTable *hash)
{
    g_hash_table_foreach_remove (hash, destroy_mask_hash_helper, NULL);
    g_hash_table_destroy (hash);
}

static gboolean
destroy_event_hash_helper (gpointer key, gpointer value, gpointer user_data)
{
    GncGUID *guid = key;
    EventInfo *ei = value;

    guid_free (guid);
    g_free (ei);

    return TRUE;
}

/* clear a hash table of the form GncGUID --> EventInfo, where
 * both keys and values are g_malloced */
static void
clear_event_hash (GHashTable *hash)
{
    if (hash == NULL)
        return;

    g_hash_table_foreach_remove (hash, destroy_event_hash_helper, NULL);
}

static void
destroy_event_hash (GHashTable *hash)
{
    clear_event_hash (hash);
    g_hash_table_destroy (hash);
}

static void
clear_event_info (ComponentEventInfo *cei)
{
    if (!cei)
        return;

    clear_mask_hash (cei->event_masks);
    clear_event_hash (cei->entity_events);
}

static void
add_event (ComponentEventInfo *cei, const GncGUID *entity,
           QofEventId event_mask, gboolean or_in)
{
    GHashTable *hash;

    if (!cei || !cei->entity_events || !entity)
        return;

    hash = cei->entity_events;

    if (event_mask == 0)
    {
        gpointer key;
        gpointer value;

        if (or_in)
            return;

        if (g_hash_table_lookup_extended (hash, entity, &key, &value))
        {
            g_hash_table_remove (hash, entity);
            guid_free (key);
            g_free (value);
        }
    }
    else
    {
        EventInfo *ei;

        ei = g_hash_table_lookup (hash, entity);
        if (ei == NULL)
        {
            GncGUID *key;

            key = guid_malloc ();
            *key = *entity;

            ei = g_new (EventInfo, 1);
            ei->event_mask = 0;

            g_hash_table_insert (hash, key, ei);
        }

        if (or_in)
            ei->event_mask |= event_mask;
        else
            ei->event_mask = event_mask;
    }
}

static void
add_event_type (ComponentEventInfo *cei, QofIdTypeConst entity_type,
                QofEventId event_mask, gboolean or_in)
{
    QofEventId *mask;

    g_return_if_fail (cei);
    g_return_if_fail (cei->event_masks);
    g_return_if_fail (entity_type);

    mask = g_hash_table_lookup (cei->event_masks, entity_type);
    if (!mask)
    {
        char * key = qof_string_cache_insert ((gpointer) entity_type);
        mask = g_new0 (QofEventId, 1);
        g_hash_table_insert (cei->event_masks, key, mask);
    }

    if (or_in)
        *mask |= event_mask;
    else
        *mask = event_mask;
}

static void
gnc_cm_event_handler (QofInstance *entity,
                      QofEventId event_type,
                      gpointer user_data,
                      gpointer event_data)
{
    const GncGUID *guid = qof_entity_get_guid(entity);
#if CM_DEBUG
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    guid_to_string_buff (guid, guidstr);
    fprintf (stderr, "event_handler: event %d, entity %p, guid %s\n", event_type,
             entity, guidstr);
#endif
    add_event (&changes, guid, event_type, TRUE);

    if (QOF_CHECK_TYPE(entity, GNC_ID_SPLIT))
    {
        /* split events are never generated by the engine, but might
         * be generated by a backend (viz. the postgres backend.)
         * Handle them like a transaction modify event. */
        add_event_type (&changes, GNC_ID_TRANS, QOF_EVENT_MODIFY, TRUE);
    }
    else
        add_event_type (&changes, entity->e_type, event_type, TRUE);

    got_events = TRUE;

    if (suspend_counter == 0)
        gnc_gui_refresh_internal (FALSE);
}

static gint handler_id;

void
gnc_component_manager_init (void)
{
    if (changes.entity_events)
    {
        PERR ("component manager already initialized");
        return;
    }

    changes.event_masks = g_hash_table_new (g_str_hash, g_str_equal);
    changes.entity_events = guid_hash_table_new ();

    changes_backup.event_masks = g_hash_table_new (g_str_hash, g_str_equal);
    changes_backup.entity_events = guid_hash_table_new ();

    handler_id = qof_event_register_handler (gnc_cm_event_handler, NULL);
}

void
gnc_component_manager_shutdown (void)
{
    if (!changes.entity_events)
    {
        PERR ("component manager not initialized");
        return;
    }

    destroy_mask_hash (changes.event_masks);
    changes.event_masks = NULL;

    destroy_event_hash (changes.entity_events);
    changes.entity_events = NULL;

    destroy_mask_hash (changes_backup.event_masks);
    changes_backup.event_masks = NULL;

    destroy_event_hash (changes_backup.entity_events);
    changes_backup.entity_events = NULL;

    qof_event_unregister_handler (handler_id);
}

static ComponentInfo *
find_component (gint component_id)
{
    GList *node;

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (ci->component_id == component_id)
            return ci;
    }

    return NULL;
}

static GList *
find_components_by_data (gpointer user_data)
{
    GList *list = NULL;
    GList *node;

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (ci->user_data == user_data)
            list = g_list_prepend (list, ci);
    }

    return list;
}

static GList *
find_components_by_session (gpointer session)
{
    GList *list = NULL;
    GList *node;

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (ci->session == session)
            list = g_list_prepend (list, ci);
    }

    return list;
}

static ComponentInfo *
gnc_register_gui_component_internal (const char * component_class)
{
    ComponentInfo *ci;
    gint component_id;

    g_return_val_if_fail (component_class, NULL);

    /* look for a free handler id */
    component_id = next_component_id;

    /* design warning: if we ever get 2^32-1 components,
       this loop is infinite.  Instead of fixing it, we'll just
       complain when (if) we get half way there (probably never).
    */
    while (find_component (component_id))
        if (++component_id == NO_COMPONENT)
            component_id++;

    if (component_id < 0)
        PERR("Amazing! Half way to running out of component_ids.");

    /* found one, add the handler */
    ci = g_new0 (ComponentInfo, 1);

    ci->watch_info.event_masks = g_hash_table_new (g_str_hash, g_str_equal);
    ci->watch_info.entity_events = guid_hash_table_new ();

    ci->component_class = g_strdup (component_class);
    ci->component_id = component_id;
    ci->session = NULL;

    components = g_list_prepend (components, ci);

    /* update id for next registration */
    next_component_id = component_id + 1;

#if CM_DEBUG
    fprintf (stderr, "Register component %d in class %s\n",
             component_id, component_class ? component_class : "(null)");
    dump_components ();
#endif

    return ci;
}

gint
gnc_register_gui_component (const char *component_class,
                            GNCComponentRefreshHandler refresh_handler,
                            GNCComponentCloseHandler close_handler,
                            gpointer user_data)
{
    ComponentInfo *ci;

    /* sanity check */
    if (!component_class)
    {
        PERR ("no class specified");
        return NO_COMPONENT;
    }

    ci = gnc_register_gui_component_internal (component_class);
    g_return_val_if_fail (ci, NO_COMPONENT);

    ci->refresh_handler = refresh_handler;
    ci->close_handler = close_handler;
    ci->user_data = user_data;

    return ci->component_id;
}

void
gnc_gui_component_watch_entity (gint component_id,
                                const GncGUID *entity,
                                QofEventId event_mask)
{
    ComponentInfo *ci;

    if (entity == NULL)
        return;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component not found");
        return;
    }

    add_event (&ci->watch_info, entity, event_mask, FALSE);
}

void
gnc_gui_component_watch_entity_type (gint component_id,
                                     QofIdTypeConst entity_type,
                                     QofEventId event_mask)
{
    ComponentInfo *ci;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component not found");
        return;
    }

    add_event_type (&ci->watch_info, entity_type, event_mask, FALSE);
}

const EventInfo *
gnc_gui_get_entity_events (GHashTable *changes, const GncGUID *entity)
{
    if (!changes || !entity)
        return QOF_EVENT_NONE;

    return g_hash_table_lookup (changes, entity);
}

void
gnc_gui_component_clear_watches (gint component_id)
{
    ComponentInfo *ci;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component not found");
        return;
    }

    clear_event_info (&ci->watch_info);
}

void
gnc_unregister_gui_component (gint component_id)
{
    ComponentInfo *ci;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component %d not found", component_id);
        return;
    }

#if CM_DEBUG
    fprintf (stderr, "Unregister component %d in class %s\n",
             ci->component_id,
             ci->component_class ? ci->component_class : "(null)");
#endif

    gnc_gui_component_clear_watches (component_id);

    components = g_list_remove (components, ci);

    destroy_mask_hash (ci->watch_info.event_masks);
    ci->watch_info.event_masks = NULL;

    destroy_event_hash (ci->watch_info.entity_events);
    ci->watch_info.entity_events = NULL;

    g_free (ci->component_class);
    ci->component_class = NULL;

    g_free (ci);

#if CM_DEBUG
    dump_components ();
#endif
}

void
gnc_unregister_gui_component_by_data (const char *component_class,
                                      gpointer user_data)
{
    GList *list;
    GList *node;

    list = find_components_by_data (user_data);

    for (node = list; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (component_class &&
                g_strcmp0 (component_class, ci->component_class) != 0)
            continue;

        gnc_unregister_gui_component (ci->component_id);
    }

    g_list_free (list);
}

void
gnc_suspend_gui_refresh (void)
{
    suspend_counter++;

    if (suspend_counter == 0)
    {
        PERR ("suspend counter overflow");
    }
}

void
gnc_resume_gui_refresh (void)
{
    if (suspend_counter == 0)
    {
        PERR ("suspend counter underflow");
        return;
    }

    suspend_counter--;

    if (suspend_counter == 0)
        gnc_gui_refresh_internal (FALSE);
}

static void
match_type_helper (gpointer key, gpointer value, gpointer user_data)
{
    ComponentEventInfo *cei = user_data;
    QofIdType id_type = key;
    QofEventId * et = value;
    QofEventId * et_2;

    et_2 = g_hash_table_lookup (cei->event_masks, id_type);
    if (!et_2)
        return;

    if (*et & *et_2)
        cei->match = TRUE;
}

static void
match_helper (gpointer key, gpointer value, gpointer user_data)
{
    GncGUID *guid = key;
    EventInfo *ei_1 = value;
    EventInfo *ei_2;
    ComponentEventInfo *cei = user_data;

    ei_2 = g_hash_table_lookup (cei->entity_events, guid);
    if (!ei_2)
        return;

    if (ei_1->event_mask & ei_2->event_mask)
        cei->match = TRUE;
}

static gboolean
changes_match (ComponentEventInfo *cei, ComponentEventInfo *changes)
{
    ComponentEventInfo *big_cei;
    GHashTable *smalltable;

    if (cei == NULL)
        return FALSE;

    /* check types first, for efficiency */
    cei->match = FALSE;
    g_hash_table_foreach (changes->event_masks, match_type_helper, cei);
    if (cei->match)
        return TRUE;

    if (g_hash_table_size (cei->entity_events) <=
            g_hash_table_size (changes->entity_events))
    {
        smalltable = cei->entity_events;
        big_cei = changes;
    }
    else
    {
        smalltable = changes->entity_events;
        big_cei = cei;
    }

    big_cei->match = FALSE;

    g_hash_table_foreach (smalltable, match_helper, big_cei);

    return big_cei->match;
}

static void
gnc_gui_refresh_internal (gboolean force)
{
    GList *list;
    GList *node;

    if (!got_events && !force)
        return;

    gnc_suspend_gui_refresh ();

    {
        GHashTable *table;

        table = changes_backup.event_masks;
        changes_backup.event_masks = changes.event_masks;
        changes.event_masks = table;

        table = changes_backup.entity_events;
        changes_backup.entity_events = changes.entity_events;
        changes.entity_events = table;
    }

#if CM_DEBUG
    fprintf (stderr, "%srefresh!\n", force ? "forced " : "");
#endif

    list = find_component_ids_by_class (NULL);

    for (node = list; node; node = node->next)
    {
        ComponentInfo *ci = find_component (GPOINTER_TO_INT (node->data));

        if (!ci)
            continue;

        if (!ci->refresh_handler)
        {
#if CM_DEBUG
            fprintf (stderr, "no handlers for %s:%d\n", ci->component_class, ci->component_id);
#endif
            continue;
        }

        if (force)
        {
            if (ci->refresh_handler)
            {
#if CM_DEBUG
                fprintf (stderr, "calling %s:%d C handler\n", ci->component_class, ci->component_id);
#endif
                ci->refresh_handler (NULL, ci->user_data);
            }
        }
        else if (changes_match (&ci->watch_info, &changes_backup))
        {
            if (ci->refresh_handler)
            {
#if CM_DEBUG
                fprintf (stderr, "calling %s:%d C handler\n", ci->component_class, ci->component_id);
#endif
                ci->refresh_handler (changes_backup.entity_events, ci->user_data);
            }
        }
        else
        {
#if CM_DEBUG
            fprintf (stderr, "no match for %s:%d\n", ci->component_class, ci->component_id);
#endif
        }
    }

    clear_event_info (&changes_backup);
    got_events = FALSE;

    g_list_free (list);

    gnc_resume_gui_refresh ();
}

void
gnc_gui_refresh_all (void)
{
    if (suspend_counter != 0)
    {
        PERR ("suspend counter not zero");
        return;
    }

    gnc_gui_refresh_internal (TRUE);
}

gboolean
gnc_gui_refresh_suspended (void)
{
    return suspend_counter != 0;
}

void
gnc_close_gui_component (gint component_id)
{
    ComponentInfo *ci;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component not found");
        return;
    }

    if (!ci->close_handler)
        return;

    if (ci->close_handler)
        ci->close_handler (ci->user_data);
}

void
gnc_close_gui_component_by_data (const char *component_class,
                                 gpointer user_data)
{
    GList *list;
    GList *node;

    list = find_components_by_data (user_data);

    for (node = list; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (component_class &&
                g_strcmp0 (component_class, ci->component_class) != 0)
            continue;

        gnc_close_gui_component (ci->component_id);
    }

    g_list_free (list);
}

void
gnc_gui_component_set_session (gint component_id, gpointer session)
{
    ComponentInfo *ci;

    ci = find_component (component_id);
    if (!ci)
    {
        PERR ("component not found");
        return;
    }

    ci->session = session;
}

void
gnc_close_gui_component_by_session (gpointer session)
{
    GList *list;
    GList *node;

    list = find_components_by_session (session);

    for (node = list; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        gnc_close_gui_component (ci->component_id);
    }

    g_list_free (list);
}

GList *
gnc_find_gui_components (const char *component_class,
                         GNCComponentFindHandler find_handler,
                         gpointer find_data)
{
    GList *list = NULL;
    GList *node;

    if (!component_class)
        return NULL;

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (g_strcmp0 (component_class, ci->component_class) != 0)
            continue;

        if (find_handler && !find_handler (find_data, ci->user_data))
            continue;

        list = g_list_prepend (list, ci->user_data);
    }

    return list;
}

gpointer
gnc_find_first_gui_component (const char *component_class,
                              GNCComponentFindHandler find_handler,
                              gpointer find_data)
{
    GList *list;
    gpointer user_data;

#if CM_DEBUG
    fprintf (stderr, "find: class %s, fn %p, data %p\n", component_class,
             find_handler, find_data);
#endif
    if (!component_class)
        return NULL;

    list = gnc_find_gui_components (component_class, find_handler, find_data);
    if (!list)
        return NULL;

    user_data = list->data;

    g_list_free (list);

#if CM_DEBUG
    fprintf (stderr, "found: data %p\n", user_data);
#endif
    return user_data;
}

static GList *
find_component_ids_by_class (const char *component_class)
{
    GList *list = NULL;
    GList *node;

    for (node = components; node; node = node->next)
    {
        ComponentInfo *ci = node->data;

        if (component_class &&
                g_strcmp0 (component_class, ci->component_class) != 0)
            continue;

        list = g_list_prepend (list, GINT_TO_POINTER (ci->component_id));
    }

    return list;
}

gint
gnc_forall_gui_components (const char *component_class,
                           GNCComponentHandler handler,
                           gpointer iter_data)
{
    GList *list;
    GList *node;
    gint count = 0;

    if (!handler)
        return(0);

    /* so components can be destroyed during the forall */
    list = find_component_ids_by_class (component_class);

    for (node = list; node; node = node->next)
    {
        ComponentInfo *ci = find_component (GPOINTER_TO_INT (node->data));

        if (!ci)
            continue;

        if (handler (ci->component_class, ci->component_id, ci->user_data, iter_data))
            count++;
    }

    g_list_free (list);
    return(count);
}

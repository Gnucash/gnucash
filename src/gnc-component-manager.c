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

#include "gnc-component-manager.h"
#include "gnc-engine-util.h"


/** Declarations ****************************************************/

typedef struct
{
  GNCIdType entity_type;
  GNCEngineEventType event_mask;
} EntityTypeEventInfo;

typedef struct
{
  GNCEngineEventType trans_event_mask;
  GNCEngineEventType account_event_mask;

  GHashTable *entity_events;

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
} ComponentInfo;


/** Static Variables ************************************************/
static guint  suspend_counter = 0;
static gint   next_component_id = 0;
static GList *components = NULL;

static ComponentEventInfo changes = { 0, 0, NULL };


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


/** Prototypes ******************************************************/
static void gnc_gui_refresh_internal (void);


/** Implementations *************************************************/

static gboolean
destroy_helper (gpointer key, gpointer value, gpointer user_data)
{
  GUID *guid = key;
  EventInfo *ei = value;

  g_free (guid);
  g_free (ei);

  return TRUE;
}

/* clear a hash table of the form GUID --> EventInfo, where
 * both keys and values are g_malloced */
static void
clear_event_hash (GHashTable *hash)
{
  if (hash == NULL)
    return;

  g_hash_table_foreach_remove (hash, destroy_helper, NULL);
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

  cei->trans_event_mask = 0;
  cei->account_event_mask = 0;

  clear_event_hash (cei->entity_events);
}

static void
add_event (ComponentEventInfo *cei, const GUID *entity,
           GNCEngineEventType event_mask, gboolean or_in)
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
      g_free (key);
      g_free (value);
    }
  }
  else
  {
    EventInfo *ei;

    ei = g_hash_table_lookup (hash, entity);
    if (ei == NULL)
    {
      GUID *key;

      key = g_new (GUID, 1);
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
add_event_type (ComponentEventInfo *cei, GNCIdType entity_type,
                GNCEngineEventType event_mask, gboolean or_in)
{
  if (cei == NULL)
    return;

  switch (entity_type)
  {
    case GNC_ID_TRANS:
      if (or_in)
        cei->trans_event_mask |= event_mask;
      else
        cei->trans_event_mask = event_mask;
      break;

    case GNC_ID_ACCOUNT:
      if (or_in)
        cei->account_event_mask |= event_mask;
      else
        cei->account_event_mask = event_mask;
      break;

    default:
      PERR ("unexpected entity type: %d", entity_type);
      break;
  }
}

void
gnc_component_manager_init (void)
{
  if (changes.entity_events)
  {
    PERR ("component manager already initialized");
    return;
  }

  changes.trans_event_mask = 0;
  changes.account_event_mask = 0;
  changes.entity_events = guid_hash_table_new ();
}

void
gnc_component_manager_shutdown (void)
{
  if (!changes.entity_events)
  {
    PERR ("component manager not initialized");
    return;
  }

  clear_event_info (&changes);
  destroy_event_hash (changes.entity_events);
  changes.entity_events = NULL;
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

gint
gnc_register_gui_component (const char *component_class,
                            GNCComponentRefreshHandler refresh_handler,
                            GNCComponentCloseHandler close_handler,
                            gpointer user_data)
{
  ComponentInfo *ci;
  gint component_id;

  /* sanity check */
  if (!component_class)
  {
    PERR ("no class specified");
    return G_MININT;
  }

  /* look for a free handler id */
  component_id = next_component_id;

  while (find_component (component_id))
    component_id++;

  /* found one, add the handler */
  ci = g_new0 (ComponentInfo, 1);

  ci->refresh_handler = refresh_handler;
  ci->close_handler = close_handler;
  ci->user_data = user_data;

  ci->watch_info.trans_event_mask = 0;
  ci->watch_info.account_event_mask = 0;
  ci->watch_info.entity_events = guid_hash_table_new ();

  ci->component_class = g_strdup (component_class);
  ci->component_id = component_id;

  components = g_list_prepend (components, ci);

  /* update id for next registration */
  next_component_id = component_id + 1;

  return component_id;
}

void
gnc_gui_component_watch_entity (gint component_id,
                                const GUID *entity,
                                GNCEngineEventType event_mask)
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
                                     GNCIdType entity_type,
                                     GNCEngineEventType event_mask)
{
  ComponentInfo *ci;

  if (entity_type != GNC_ID_TRANS &&
      entity_type != GNC_ID_ACCOUNT)
  {
    PERR ("bad entity type: %d", entity_type);
    return;
  }

  ci = find_component (component_id);
  if (!ci)
  {
    PERR ("component not found");
    return;
  }

  add_event_type (&ci->watch_info, entity_type, event_mask, FALSE);
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
    PERR ("component not found");
    return;
  }

  gnc_gui_component_clear_watches (component_id);

  components = g_list_remove (components, ci);

  destroy_event_hash (ci->watch_info.entity_events);
  ci->watch_info.entity_events = NULL;

  g_free (ci->component_class);
  ci->component_class = NULL;

  g_free (ci);
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
        safe_strcmp (component_class, ci->component_class) != 0)
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
    gnc_gui_refresh_internal ();
}

static void
match_helper (gpointer key, gpointer value, gpointer user_data)
{
  GUID *guid = key;
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
changes_match (ComponentEventInfo *cei)
{
  ComponentEventInfo *big_cei;
  GHashTable *small;

  if (cei == NULL)
    return FALSE;

  /* check types first, for efficiency */

  if (cei->trans_event_mask & changes.trans_event_mask)
    return TRUE;

  if (cei->account_event_mask & changes.account_event_mask)
    return TRUE;

  if (g_hash_table_size (cei->entity_events) <=
      g_hash_table_size (changes.entity_events))
  {
    small = cei->entity_events;
    big_cei = &changes;
  }
  else
  {
    small = changes.entity_events;
    big_cei = cei;
  }

  big_cei->match = FALSE;

  g_hash_table_foreach (small, match_helper, big_cei);

  return big_cei->match;
}

static void
gnc_gui_refresh_internal (void)
{
  GList *node;

  for (node = components; node; node = node->next)
  {
    ComponentInfo *ci = node->data;

    if (!ci->refresh_handler)
      continue;

    if (changes_match (&ci->watch_info))
      ci->refresh_handler (changes.entity_events, ci->user_data);
  }

  clear_event_info (&changes);
}

void
gnc_gui_refresh_all (void)
{
  if (suspend_counter != 0)
  {
    PERR ("suspend counter not zero");
    return;
  }

  gnc_gui_refresh_internal ();
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
  {
    PERR ("no close handler");
    return;
  }

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
        safe_strcmp (component_class, ci->component_class) != 0)
      continue;

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

    if (safe_strcmp (component_class, ci->component_class) != 0)
      continue;

    if (find_handler && !find_handler (find_data, ci->user_data))
      continue;

    list = g_list_prepend (list, ci->user_data);
  }

  return list;
}

void
gnc_forall_gui_components (const char *component_class,
                           GNCComponentHandler handler,
                           gpointer iter_data)
{
  GList *node;

  if (!handler)
    return;

  for (node = components; node; node = node->next)
  {
    ComponentInfo *ci = node->data;

    if (component_class &&
        safe_strcmp (component_class, ci->component_class) != 0)
      continue;

    handler (ci->component_class, ci->component_id, iter_data);
  }
}

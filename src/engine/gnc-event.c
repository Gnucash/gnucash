/********************************************************************
 * gnc-event.c -- engine event handling implementation              *
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

#include "config.h"

#include "gnc-engine-util.h"
#include "gnc-event-p.h"


/** Declarations ****************************************************/

typedef struct
{
  GNCEngineEventHandler handler;
  gpointer user_data;

  gint handler_id;
} HandlerInfo;


/** Static Variables ************************************************/
static guint  suspend_counter = 0;
static gint   next_handler_id = 0;
static GList *handlers = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;


/** Implementations *************************************************/

gint
gnc_engine_register_event_handler (GNCEngineEventHandler handler,
                                   gpointer user_data)
{
  HandlerInfo *hi;
  gint handler_id;
  GList *node;

  /* sanity check */
  if (!handler)
  {
    PERR ("no handler specified");
    return 0;
  }

  /* look for a free handler id */
  handler_id = next_handler_id;
  node = handlers;

  while (node)
  {
    hi = node->data;

    if (hi->handler_id == handler_id)
    {
      handler_id++;
      node = handlers;
      continue;
    }

    node = node->next;
  }

  /* found one, add the handler */
  hi = g_new0 (HandlerInfo, 1);

  hi->handler = handler;
  hi->user_data = user_data;
  hi->handler_id = handler_id;

  handlers = g_list_prepend (handlers, hi);

  /* update id for next registration */
  next_handler_id = handler_id + 1;

  return handler_id;
}

void
gnc_engine_unregister_event_handler (gint handler_id)
{
  GList *node;

  for (node = handlers; node; node = node->next)
  {
    HandlerInfo *hi = node->data;

    if (hi->handler_id != handler_id)
      continue;

    /* found it */

    /* take out of list */
    handlers = g_list_remove_link (handlers, node);

    /* safety */
    hi->handler = NULL;

    g_list_free_1 (node);
    g_free (hi);

    return;
  }

  PERR ("no such handler: %d", handler_id);
}

void
gnc_engine_suspend_events (void)
{
  suspend_counter++;

  if (suspend_counter == 0)
  {
    PERR ("suspend counter overflow");
  }
}

void
gnc_engine_resume_events (void)
{
  if (suspend_counter == 0)
  {
    PERR ("suspend counter underflow");
    return;
  }

  suspend_counter--;
}

void
gnc_engine_generate_event (GUID *entity, GNCEngineEventType event_type)
{
  GList *node;

  if (!entity)
    return;

  if (suspend_counter)
    return;

  switch (event_type)
  {
    case GNC_EVENT_NONE:
      return;

    case GNC_EVENT_CREATE:
    case GNC_EVENT_MODIFY:
    case GNC_EVENT_DESTROY:
      break;

    default:
      PERR ("bad event type %d", event_type);
      return;
  }

  for (node = handlers; node; node = node->next)
  {
    HandlerInfo *hi = node->data;

    if (hi->handler)
      hi->handler (entity, event_type, hi->user_data);
  }
}

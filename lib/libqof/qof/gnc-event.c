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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include "config.h"
#include "qof.h"
#include "gnc-event-p.h"

/* Declarations ****************************************************/

typedef struct
{
  GNCEngineEventHandler handler;
  gpointer user_data;

  gint handler_id;
} HandlerInfo;

/* Static Variables ************************************************/
static guint  suspend_counter = 0;
static gint   next_handler_id = 1;
static guint  handler_run_level = 0;
static guint  pending_deletes = 0;
static GList *handlers = NULL;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = QOF_MOD_ENGINE;

/* Implementations *************************************************/

gint
gnc_engine_register_event_handler (GNCEngineEventHandler handler,
                                   gpointer user_data)
{
  HandlerInfo *hi;
  gint handler_id;
  GList *node;

  ENTER ("(handler=%p, data=%p)", handler, user_data);
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

  /* Found one, add the handler */
  hi = g_new0 (HandlerInfo, 1);

  hi->handler = handler;
  hi->user_data = user_data;
  hi->handler_id = handler_id;

  handlers = g_list_prepend (handlers, hi);

  /* Update id for next registration */
  next_handler_id = handler_id + 1;

  LEAVE ("(handler=%p, data=%p) handler_id=%d", handler, user_data, handler_id);
  return handler_id;
}

void
gnc_engine_unregister_event_handler (gint handler_id)
{
  GList *node;

  ENTER ("(handler_id=%d)", handler_id);
  for (node = handlers; node; node = node->next)
  {
    HandlerInfo *hi = node->data;

    if (hi->handler_id != handler_id)
      continue;

    /* Normally, we could actually remove the handler's node from the
       list, but we may be unregistering the event handler as a result
       of a generated event, such as GNC_EVENT_DESTROY.  In that case,
       we're in the middle of walking the GList and it is wrong to
       modify the list. So, instead, we just NULL the handler. */ 

    LEAVE ("(handler_id=%d) handler=%p data=%p", handler_id,
	   hi->handler, hi->user_data);

    /* safety -- clear the handler in case we're running events now */
    hi->handler = NULL;

    if (handler_run_level == 0) {
      handlers = g_list_remove_link (handlers, node);
      g_list_free_1 (node);
      g_free (hi);
    } else {
      pending_deletes++;
    }

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

static void
gnc_engine_generate_event_internal (QofEntity *entity, 
				    GNCEngineEventType event_type)
{
  GList *node;
  GList *next_node = NULL;

  g_return_if_fail(entity);

  switch (event_type)
  {
    case GNC_EVENT_NONE:
      return;

    case GNC_EVENT_CREATE:
    case GNC_EVENT_MODIFY:
    case GNC_EVENT_DESTROY:
    case GNC_EVENT_ADD:
    case GNC_EVENT_REMOVE:
      break;

    default:
      PERR ("bad event type %d", event_type);
      return;
  }

  handler_run_level++;
  for (node = handlers; node; node = next_node)
  {
    HandlerInfo *hi = node->data;

    next_node = node->next;
    PINFO ("id=%d hi=%p han=%p", hi->handler_id, hi, hi->handler);
    if (hi->handler)
      hi->handler ((GUID *)&entity->guid, entity->e_type,
		   event_type, hi->user_data);
  }
  handler_run_level--;

  /* If we're the outtermost event runner and we have pending deletes
   * then go delete the handlers now.
   */
  if (handler_run_level == 0 && pending_deletes)
  {
    for (node = handlers; node; node = next_node)
    {
      HandlerInfo *hi = node->data;
      next_node = node->next;
      if (hi->handler == NULL)
      {
	/* remove this node from the list, then free this node */
	handlers = g_list_remove_link (handlers, node);
	g_list_free_1 (node);
	g_free (hi);
      }
    }
    pending_deletes = 0;
  }
}

void
gnc_engine_force_event (QofEntity *entity, 
			GNCEngineEventType event_type)
{
  if (!entity)
    return;

  gnc_engine_generate_event_internal (entity, event_type);
}

void
gnc_engine_gen_event (QofEntity *entity, GNCEngineEventType event_type)
{
  if (!entity)
    return;

  if (suspend_counter)
    return;

  gnc_engine_generate_event_internal (entity, event_type);
}

void 
gnc_engine_generate_event (const GUID *guid, QofIdType e_type, 
         GNCEngineEventType event_type)
{
  QofEntity ent;
  ent.guid = *guid;
  ent.e_type = e_type;
  if (suspend_counter) return;
  gnc_engine_generate_event_internal (&ent, event_type);
}

/* =========================== END OF FILE ======================= */

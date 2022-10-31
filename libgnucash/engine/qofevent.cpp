/********************************************************************
 * qofevent.c -- QOF event handling implementation                  *
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

#include <vector>
#include <algorithm>
#include <config.h>

#include "qof.h"
#include "qofevent-p.h"

struct HandlerInfo
{
    QofEventHandler handler;
    gpointer user_data;
    gint handler_id;
    HandlerInfo (QofEventHandler handler, gpointer user_data, gint handler_id)
        : handler (handler)
        , user_data (user_data)
        , handler_id (handler_id) {};
    ~HandlerInfo () {};
};

/* Static Variables ************************************************/
static guint   suspend_counter   = 0;
static gint    next_handler_id   = 1;
static guint   handler_run_level = 0;
static guint   pending_deletes   = 0;
static std::vector<HandlerInfo> handlers;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = QOF_MOD_ENGINE;

/* Implementations *************************************************/

static gint
find_next_handler_id(void)
{
    /* look for a free handler id */
    auto handler_id = next_handler_id;

 restart:
    for (const auto& hi : handlers)
    {
        if (hi.handler_id == handler_id)
        {
            handler_id++;
            goto restart;
        }
    }
    /* Update id for next registration */
    next_handler_id = handler_id + 1;
    return handler_id;
}

gint
qof_event_register_handler (QofEventHandler handler, gpointer user_data)
{

    ENTER ("(handler=%p, data=%p)", handler, user_data);

    /* sanity check */
    if (!handler)
    {
        PERR ("no handler specified");
        return 0;
    }

    /* look for a free handler id */
    auto handler_id = find_next_handler_id();

    handlers.emplace_back (handler, user_data, handler_id);
    LEAVE ("(handler=%p, data=%p) handler_id=%d", handler, user_data, handler_id);
    return handler_id;
}

void
qof_event_unregister_handler (gint handler_id)
{
    ENTER ("(handler_id=%d)", handler_id);

    auto iter = std::find_if (handlers.begin(), handlers.end(),
                              [&handler_id](const auto& it)
                              { return it.handler_id == handler_id; });
    if (iter == handlers.end())
    {
        PERR ("no such handler: %d", handler_id);
        return;
    }

    /* Normally, we could actually remove the handler from the vector,
       but we may be unregistering the event handler as a result of a
       generated event, such as QOF_EVENT_DESTROY.  In that case,
       we're in the middle of walking the std::vector and it is wrong
       to modify the vector. So, instead, we just NULL the handler. */
    if (iter->handler)
        LEAVE ("(handler_id=%d) handler=%p data=%p", handler_id,
               iter->handler, iter->user_data);

    /* safety -- clear the handler in case we're running events now */
    iter->handler = nullptr;

    if (handler_run_level == 0)
        handlers.erase (iter);
    else
        pending_deletes++;
}

void
qof_event_suspend (void)
{
    suspend_counter++;

    if (suspend_counter == 0)
    {
        PERR ("suspend counter overflow");
    }
}

void
qof_event_resume (void)
{
    if (suspend_counter == 0)
    {
        PERR ("suspend counter underflow");
        return;
    }

    suspend_counter--;
}

static void
qof_event_generate_internal (QofInstance *entity, QofEventId event_id,
                             gpointer event_data)
{
    g_return_if_fail(entity);

    switch (event_id)
    {
    case QOF_EVENT_NONE:
    {
        /* if none, don't log, just return. */
        return;
    }
    }

    handler_run_level++;
    for (const auto &hi : handlers)
    {
        if (!hi.handler)
            continue;
        PINFO("id=%d hi=%p han=%p data=%p", hi.handler_id, &hi, hi.handler, event_data);
        hi.handler (entity, event_id, hi.user_data, event_data);
    }
    handler_run_level--;

    /* If we're the outermost event runner and we have pending deletes
     * then go delete the handlers now.
     */
    if (handler_run_level == 0 && pending_deletes)
    {
        std::remove_if (handlers.begin(), handlers.end(),
                        [](const auto& hi){ return (!hi.handler); });
        pending_deletes = 0;
    }
}

void
qof_event_force (QofInstance *entity, QofEventId event_id, gpointer event_data)
{
    if (!entity)
        return;

    qof_event_generate_internal (entity, event_id, event_data);
}

void
qof_event_gen (QofInstance *entity, QofEventId event_id, gpointer event_data)
{
    if (!entity)
        return;

    if (suspend_counter)
        return;

    qof_event_generate_internal (entity, event_id, event_data);
}

/* =========================== END OF FILE ======================= */

/********************************************************************
 * unittest-support.c: Support structures for GLib Unit Testing     *
 * Copyright 2011-13 John Ralls <jralls@ceridwen.us>		    *
 * Copyright 2011 Muslim Chochlov <muslim.chochlov@gmail.com>       *
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
********************************************************************/

#include <config.h>
#include <glib/gprintf.h>
#include "unittest-support.h"

typedef struct
{
    gpointer data;
    gboolean called;
    char *msg;
} TestStruct;

static TestStruct tdata;
static gboolean
test_checked_nohit_handler (const char *log_domain, GLogLevelFlags log_level,
                            const gchar *msg, gpointer user_data);
static gboolean
test_list_nohit_handler (const char *log_domain, GLogLevelFlags log_level,
                         const gchar *msg, gpointer user_data);

TestErrorStruct*
test_error_struct_new (const char *log_domain, GLogLevelFlags log_level,
                       const char *msg)
{
    TestErrorStruct *err = g_slice_new0 (TestErrorStruct);
    err->log_domain = g_strdup (log_domain);
    err->log_level = log_level;
    err->msg = g_strdup (msg);
    return err;
}

void
test_error_struct_free (TestErrorStruct *err)
{
    g_free (err->log_domain);
    g_free (err->msg);
    g_slice_free (TestErrorStruct, err);
}

GSList*
test_log_set_handler (GSList *list, TestErrorStruct *error, GLogFunc handler)
{
    TestLogHandler *hdlr = g_slice_new0 (TestLogHandler);
    hdlr->error = error;
    hdlr->handler = g_log_set_handler (error->log_domain, error->log_level,
                                       handler, error);
    return g_slist_prepend (list, hdlr);
}

GSList*
test_log_set_fatal_handler (GSList *list, TestErrorStruct *error,
                            GLogFunc handler)
{
    TestLogHandler *hdlr = g_slice_new0 (TestLogHandler);
    GTestLogFatalFunc f_hdlr = handler == (GLogFunc)test_list_handler ?
                               (GTestLogFatalFunc)test_list_nohit_handler :
                               (GTestLogFatalFunc)test_checked_nohit_handler;
    hdlr->error = error;
    hdlr->handler = g_log_set_handler (error->log_domain, error->log_level,
                                       handler, error);
    g_test_log_set_fatal_handler (f_hdlr, error);
    return g_slist_prepend (list, hdlr);
}

void
test_free_log_handler (gpointer item)
{
    TestLogHandler *handler = (TestLogHandler*)item;
    g_log_remove_handler (handler->error->log_domain, handler->handler);
    test_error_struct_free (handler->error);
    g_slice_free (TestLogHandler, handler);
}

gboolean
test_null_handler (const char *log_domain, GLogLevelFlags log_level,
                   const gchar *msg, gpointer user_data )
{
    //Silent, remember?

    return FALSE;
}

static gchar*
test_log_level (GLogLevelFlags flags)
{
    const gchar *message[] = {"RECURSIVE", "FATAL", "ERROR", "CRITICAL",
                              "WARNING", "MESSAGE", "INFO",  "DEBUG"
                             };
    guint i = 0, last = 0, max_bit = 7;
    gchar *msg = NULL;

    for (i = 0; i <= max_bit; i++)
        if (flags & 1 << i)
        {
            gchar *tmp_msg = msg;
            gchar *sep = (last < 2 ? " " : "|");
            last = i;
            msg = (tmp_msg ? g_strjoin (sep, tmp_msg, message[i], NULL)
                   : g_strdup (message[i]));
            if (tmp_msg)
                g_free (tmp_msg);
        }

    if (msg == NULL)
        msg = g_strdup ("");
    return msg;
}

static GList *message_queue = NULL;

void
test_add_error (TestErrorStruct *error)
{
    message_queue = g_list_append (message_queue, error);
}

void
test_clear_error_list (void)
{
    g_list_free (message_queue);
    message_queue = NULL;
}


gboolean
test_list_substring_handler (const char *log_domain, GLogLevelFlags log_level,
                      const gchar *msg, gpointer user_data)
{
    GList *list = g_list_first (message_queue);
    const guint fatal = G_LOG_FLAG_FATAL;
    while (list)
    {
        TestErrorStruct *error = (TestErrorStruct*)list->data;
        if (!g_strcmp0 (log_domain, error->log_domain)
                && ((log_level | fatal) == (error->log_level | fatal))
                && g_strrstr (msg, error->msg))
        {
            ++(error->hits);
            return FALSE;
        }
        list = g_list_next (list);
    }
    /* No list or no matches, fall through */
    return test_checked_substring_handler (log_domain, log_level, msg, user_data);
}

static gboolean
do_test_list_handler (const char *log_domain, GLogLevelFlags log_level,
                      const gchar *msg, gpointer user_data, gboolean hits)
{
    GList *list = g_list_first (message_queue);
    const guint fatal = G_LOG_FLAG_FATAL;

    while (list)
    {
        TestErrorStruct *error = (TestErrorStruct*)list->data;
        if (!g_strcmp0 (log_domain, error->log_domain)
                && ((log_level | fatal) == (error->log_level | fatal))
                && !g_strcmp0 (msg, error->msg))
        {
            if (hits)
                ++(error->hits);
            return FALSE;
        }
        list = g_list_next (list);
    }
    /* No list or no matches, fall through */
    return test_checked_handler (log_domain, log_level, msg, user_data);
}

gboolean
test_list_handler (const char *log_domain, GLogLevelFlags log_level,
                   const gchar *msg, gpointer user_data)
{
    return do_test_list_handler (log_domain, log_level, msg, user_data, TRUE);
}

gboolean
test_list_nohit_handler (const char *log_domain, GLogLevelFlags log_level,
                         const gchar *msg, gpointer user_data)
{
    return do_test_list_handler (log_domain, log_level, msg, user_data, FALSE);
}

static gboolean
do_test_checked_handler (const char *log_domain, GLogLevelFlags log_level,
                         const gchar *msg, gpointer user_data, gboolean hits)
{
    TestErrorStruct *tdata = (TestErrorStruct*)user_data;

    if ((tdata == NULL)
            || (tdata->log_domain != NULL
                && g_strcmp0 (tdata->log_domain, log_domain))
            || (tdata->log_level && tdata->log_level != log_level)
            || (tdata->msg && g_strcmp0 (tdata->msg, msg)))
    {
        gchar *level = test_log_level (log_level);
        g_printf ( "<%s> (%s) %s\n", level, log_domain, msg);
        g_free (level);
        g_assert (log_level ^ G_LOG_FLAG_FATAL);
        return FALSE;
    }
    if (hits)
        ++(tdata->hits);
    return FALSE;

}

gboolean
test_checked_substring_handler (const char *log_domain, GLogLevelFlags log_level,
                                         const gchar *msg, gpointer user_data)
{
    TestErrorStruct *tdata = (TestErrorStruct*)user_data;
    if ((tdata == NULL)
            || (tdata->log_domain != NULL
                && g_strcmp0 (log_domain, tdata->log_domain))
            || (tdata->log_level && tdata->log_level != log_level)
            || (tdata->msg && !g_strrstr (msg, tdata->msg)))
    {
        gchar *level = test_log_level (log_level);
        g_printf ( "<%s> (%s) %s\n", level, log_domain, msg);
        g_free (level);
        g_assert (log_level ^ G_LOG_FLAG_FATAL);
        return FALSE;
    }
    ++(tdata->hits);
    return FALSE;
}

gboolean
test_checked_handler (const char *log_domain, GLogLevelFlags log_level,
                      const gchar *msg, gpointer user_data )
{
    return do_test_checked_handler (log_domain, log_level, msg,
                                    user_data, TRUE);
}

static gboolean
test_checked_nohit_handler (const char *log_domain, GLogLevelFlags log_level,
                            const gchar *msg, gpointer user_data )
{
    return do_test_checked_handler (log_domain, log_level, msg,
                                    user_data, FALSE);
}

gboolean
test_log_handler (const char *log_domain, GLogLevelFlags log_level,
                  const gchar *msg, gpointer user_data )
{
    gchar *level = test_log_level (log_level);
    g_printf ( "<%s> (%s) %s\n", level, log_domain, msg);
    g_free (level);
    g_assert (log_level ^ G_LOG_FLAG_FATAL);
    return FALSE;
}

void
test_set_called( const gboolean val )
{
    tdata.called = val;
}

gboolean
test_reset_called( void )
{
    const gboolean called  = tdata.called;
    tdata.called = FALSE;
    return called;
}

void
test_set_data( const gpointer val )
{
    tdata.data = val;
}

gpointer
test_reset_data( void )
{
    const gpointer data  = tdata.data;
    tdata.data = NULL;
    return data;
}

void
test_free( gpointer data )
{
    if (!data) return;
    g_free(data);
}


typedef struct
{
    QofInstance *entity;
    QofEventId event_type;
    gpointer event_data;
    gint hdlr;
    guint hits;
} _TestSignal;

static void
mock_signal_handler (QofInstance *entity, QofEventId event_type,
                     gpointer handler_data, gpointer event_data)
{
    _TestSignal *signal = (_TestSignal*)handler_data;
    if ((signal->entity == entity || signal->entity == NULL)
            && signal->event_type == event_type)
    {
        if (signal->event_data)
            g_assert (signal->event_data == event_data);
        signal->hits += 1;
    }
}

TestSignal
test_signal_new (QofInstance *entity, QofEventId event_type,
                 gpointer event_data)
{
    _TestSignal *sig = g_slice_new (_TestSignal);
    sig->entity = entity;
    sig->event_type = event_type;
    sig->event_data = event_data;
    sig->hits = 0;
    sig->hdlr = qof_event_register_handler (mock_signal_handler, (gpointer)sig);
    return (TestSignal)sig;
}

void
test_signal_free (TestSignal sigp)
{
    _TestSignal *sig = (_TestSignal *)sigp;
    qof_event_unregister_handler (sig->hdlr);
    g_slice_free (_TestSignal, sig);
}

guint
test_signal_return_hits (TestSignal sigp)
{
    _TestSignal *sig = (_TestSignal *)sigp;
    return sig->hits;
}

static void
notify_destroy (gpointer pdata, GObject *obj)
{
    gboolean *data = (gboolean*)pdata;
    if (! (*data)) *data = TRUE;
}

gboolean
test_object_checked_destroy (GObject *obj)
{
    gboolean is_destroyed = FALSE;
    if (!obj || ! G_IS_OBJECT (obj)) return FALSE;
    g_object_weak_ref (obj, notify_destroy, &is_destroyed);
    g_object_unref (obj);
    return is_destroyed;
}

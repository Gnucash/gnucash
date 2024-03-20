/********************************************************************
 * gnc-ws-server.c -- basic websocket server                        *
 * Copyright (C) 2024 Bob Fewell                                    *
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
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gio/gio.h>

#include "gnc-ws-server.h"
#include "gnc-ws-protocol.h"

#define GNC_WS_SERVER_PATH "gnc-ws-server-path"

G_DEFINE_TYPE (GncWsServer, gnc_ws_server, G_TYPE_OBJECT)

#define PORT 8080
#define BLOCK_SIZE 1024

static GncWsServer *gws = NULL;

/* Signal codes */
enum
{
    OPEN,
    MESSAGE,
    CLOSE,
    LAST_SIGNAL
};

static guint ws_server_signals [LAST_SIGNAL] = { 0 };

static void
gnc_ws_server_init (GncWsServer *self)
{
g_print("%s called\n",__FUNCTION__);
}

static void
gnc_ws_server_dispose (GObject *object)
{
g_print("%s called\n",__FUNCTION__);
    g_return_if_fail (object != NULL);

    GncWsServer *gws = (GncWsServer*)object;

g_print("ws service active %d\n", g_socket_service_is_active (gws->service));

    if (gws->incoming_id != 0)
        g_signal_handler_disconnect (G_OBJECT(gws->service), gws->incoming_id);
    gws->incoming_id = 0;

    g_socket_service_stop (gws->service);

g_print(" ws service active %d\n", g_socket_service_is_active (gws->service));

    G_OBJECT_CLASS(gnc_ws_server_parent_class)->dispose (object);
}

static void
gnc_ws_server_finalize (GObject *object)
{
g_print("%s called\n",__FUNCTION__);

    gws = NULL;

    G_OBJECT_CLASS(gnc_ws_server_parent_class)->finalize (object);
}

static void
gnc_ws_server_class_init (GncWsServerClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
g_print("%s called\n",__FUNCTION__);
    gobject_class->dispose = gnc_ws_server_dispose;
    gobject_class->finalize = gnc_ws_server_finalize;

    ws_server_signals [OPEN] =
        g_signal_new ("open",
                      G_OBJECT_CLASS_TYPE(gobject_class),
                      G_SIGNAL_RUN_FIRST,
                      0,
                      NULL,
                      NULL,
                      NULL,
                      G_TYPE_NONE,
                      1,
                      G_TYPE_STRING);

    ws_server_signals [MESSAGE] =
        g_signal_new ("message",
                      G_OBJECT_CLASS_TYPE(gobject_class),
                      G_SIGNAL_RUN_FIRST,
                      0,
                      NULL,
                      NULL,
                      NULL,
                      G_TYPE_NONE,
                      2,
                      G_TYPE_STRING,
                      G_TYPE_STRING);

    ws_server_signals [CLOSE] =
        g_signal_new ("close",
                      G_OBJECT_CLASS_TYPE(gobject_class),
                      G_SIGNAL_RUN_FIRST,
                      0,
                      NULL,
                      NULL,
                      NULL,
                      G_TYPE_NONE,
                      1,
                      G_TYPE_STRING);
}


static gboolean
send_message_bytes (GSocketConnection *connection, GBytes *bytes)
{
    gboolean ret = TRUE;
    GError *error = NULL;
    GOutputStream *ostream = g_io_stream_get_output_stream (G_IO_STREAM(connection));

    gssize data_out = g_output_stream_write_bytes (ostream, bytes, NULL, &error);
#ifdef G_OS_WIN32
g_print("%s called, connection %p, data out sent %d\n",__FUNCTION__, connection, data_out);
#else
g_print("%s called, connection %p, data out sent %ld\n",__FUNCTION__, connection, data_out);
#endif
    if (error != NULL)
    {
        g_error ("%s", error->message);
g_print("send error '%s'\n", error->message);
        g_clear_error (&error);
        ret = FALSE;
    }
    return ret;
}


struct ConnHashData
{
    GSocketConnection *connection;
    const gchar *id;
};

static gboolean
connection_hash_table_find (gpointer key, gpointer value, gpointer user_data)
{
    struct ConnHashData *data = user_data;

    if (g_str_has_suffix (data->id, value))
    {
        data->connection = key;
        return TRUE;
    }
    return FALSE;
}

void
gnc_ws_server_send_message (GncWsServer *gws, const gchar *id, const gchar *message)
{
    struct ConnHashData *data = g_new (struct ConnHashData, 1);
    data->id = id;
    data->connection = NULL;

g_print("%s called, id '%s', message '%s'\n",__FUNCTION__, id, message);

    g_hash_table_foreach (gws->connections_hash,
                          (GHFunc)connection_hash_table_find,
                          data);

    if (data->connection)
    {
        GBytes *bytes = gnc_ws_send_message (message);
        send_message_bytes (data->connection, bytes);
        g_bytes_unref (bytes);
    }
    g_free (data);
}


static gboolean
send_handshake_message (GSocketConnection *connection, const gchar *message)
{
    gboolean ret = TRUE;
    GError *error = NULL;
    GOutputStream *ostream = g_io_stream_get_output_stream (G_IO_STREAM(connection));

#ifdef G_OS_WIN32
g_print("%s called, connection %p, msg len %d\n",__FUNCTION__, connection, strlen (message));
#else
g_print("%s called, connection %p, msg len %ld\n",__FUNCTION__, connection, strlen (message));
#endif
    g_output_stream_write (ostream,
                           message,
                           strlen (message),
                           NULL,
                           &error);

    if (error != NULL)
    {
        g_error ("%s", error->message);
g_print("send error '%s'\n", error->message);
        g_clear_error (&error);
        ret = FALSE;
    }
    return ret;
}


struct ConnData
{
    GncWsServer *gws;
    GSocketConnection *connection;
    char message[BLOCK_SIZE];
};

static void
bytes_ready_cb (GObject *source_object,
                GAsyncResult *res,
                gpointer user_data)
{
    GInputStream *istream = G_INPUT_STREAM(source_object);
    GError *error = NULL;
    struct ConnData *data = user_data;
    GncWsServer *gws = data->gws;

    GBytes *bytes_in = g_input_stream_read_bytes_finish (istream, res, &error);

g_print("%s called, istream %p\n",__FUNCTION__, istream);

    if (error != NULL)
    {
        g_error ("%s", error->message);
g_print("bytes service error is: %s\n", error->message);
        g_clear_error (&error);
        return;
    }

    if (g_bytes_get_size (bytes_in) != 0)
    {
        gint opcode;
        gchar *message = gnc_ws_parse_bytes_in (bytes_in, &opcode);

        if ((opcode == OPCODE_TEXT) && message)
        {
            gchar *id = g_hash_table_lookup (gws->connections_hash, data->connection);

            if (id)
                g_signal_emit_by_name (data->gws, "message", id, message);
            else
g_print("Error: Lookup Error\n");
        }
        g_free (message);
        g_bytes_unref (bytes_in);

        if (opcode == OPCODE_CLOSE)
        {
            gchar *id = g_hash_table_lookup (gws->connections_hash, data->connection);

            if (id)
                g_signal_emit_by_name (data->gws, "close", id);
            else
g_print("Error: Lookup Error\n");

            g_hash_table_remove (gws->connections_hash, data->connection);

            return;
        }
        g_input_stream_read_bytes_async (istream, 8192, G_PRIORITY_DEFAULT,
                                         NULL, bytes_ready_cb, user_data);
    }
}

static void
message_ready_cb (GObject *source_object,
                  GAsyncResult *res,
                  gpointer user_data)
{
    GInputStream *istream = G_INPUT_STREAM(source_object);
    GError *error = NULL;
    struct ConnData *data = user_data;
    int count;

    count = g_input_stream_read_finish (istream, res, &error);

g_print("%s called, istream %p, count %d\n",__FUNCTION__, istream, count);

    if (count == -1)
    {
g_print ("Error: receiving message\n");
        if (error != NULL)
        {
g_print("Error: incoming stream error: %s\n", error->message);
            g_clear_error (&error);
            return;
        }
    }

    gchar *id = NULL;
    gchar *handshake_message = gnc_ws_make_handshake (data->message, &id);

    if (handshake_message)
    {
       if (send_handshake_message (data->connection, handshake_message))
            g_input_stream_read_bytes_async (istream, 8192, G_PRIORITY_DEFAULT,
                                             NULL, bytes_ready_cb, user_data);
    }

    if (id)
    {
        g_hash_table_insert (gws->connections_hash, data->connection, g_strdup(id));
        g_signal_emit_by_name (data->gws, "open", id);
    }
    g_free (handshake_message);
    g_free (id);

//    g_object_unref (G_SOCKET_CONNECTION(data->connection));
//    g_free (data);
}


static gboolean
incoming_callback (GSocketService *service,
                   GSocketConnection *connection,
                   GObject *source_object,
                   gpointer user_data)
{
    GncWsServer *gws = user_data;
    GInputStream *istream = g_io_stream_get_input_stream (G_IO_STREAM(connection));
    struct ConnData *data = g_new (struct ConnData, 1);

g_print("%s called, connection %p\n",__FUNCTION__, connection);

    data->connection = g_object_ref (connection);
    data->gws = gws;

    g_input_stream_read_async (istream, data->message, sizeof (data->message),
                               G_PRIORITY_DEFAULT, NULL, message_ready_cb, data);

    return FALSE;
}

GncWsServer *
gnc_ws_server_new (void)
{
g_print("%s called\n",__FUNCTION__);

    if (gws)
        return gws;

    gws = g_object_new (GNC_TYPE_WS_SERVER, NULL);

g_print(" ws gws %p\n", gws);

    gws->service = g_socket_service_new ();
    GError *error = NULL;
    gboolean ret = g_socket_listener_add_inet_port (G_SOCKET_LISTENER(gws->service),
                                                    PORT, NULL, &error);

    if (ret && error != NULL)
    {
        g_error ("%s", error->message);
g_print("service error is: %s\n", error->message);
        g_clear_error (&error);
        return NULL;
    }

    gws->connections_hash = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                                   NULL, g_free);

    g_socket_service_start (gws->service);

    gws->incoming_id = g_signal_connect (gws->service, "incoming",
                                         G_CALLBACK(incoming_callback), gws);

g_print(" ws service active %d\n", g_socket_service_is_active (gws->service));

    return gws;
}

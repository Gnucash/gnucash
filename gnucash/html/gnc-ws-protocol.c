/********************************************************************
 * gnc-ws-protocol.c -- basic websocket server protocol             *
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

#include "gnc-ws-protocol.h"

#define WS_KEY "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

#define WS_HS_ACCEPT "HTTP/1.1 101 Switching Protocols\r\n"\
                     "Upgrade: websocket\r\n"\
                     "Connection: Upgrade\r\n"\
                     "Sec-WebSocket-Accept: "

#ifdef skip
static void
print_hex (const gchar *s)
{
  while (*s)
    g_printf("%02x", (unsigned int) *s++);
  g_printf("\n");
}
#endif

#ifdef skip
static void
print_bytes (GBytes *bytes_in)
{
    gsize length;
    const guint8 *array = g_bytes_get_data (bytes_in, &length);
    GString *printable;
    guint i = 0;

    printable = g_string_new ("[");

    while (i < length)
    {
        if (i > 0)
            g_string_append_c (printable, ' ');
        g_string_append_printf (printable, "%02x", array[i++]);
    }
    g_string_append_c (printable, ']');

    gchar *test = g_string_free (printable, FALSE);

g_print("Print bytes: '%s' %" G_GUINT64_FORMAT "\n", test, length);

    g_free (test);
}
#endif


static gchar*
find_header_text (gchar **header, const gchar *find_text)
{
    gchar *ret_text = NULL;
    guint parts = g_strv_length (header);

    for (guint i = 0; i < parts; i++)
    {
        if (g_strstr_len (header[i], -1, find_text))
        {
            ret_text = g_strdup (header[i]);
            break;
        }
    }
    return ret_text;
}

#define BUFFER_LEN 200 // only dealing with max message of 125

static GBytes *
make_frame (guint8 opcode, const gchar *message)
{
    guint8 first_byte = (0x80 | opcode);
    guint8 msg_len = strlen (message);

    if (msg_len > 125)
    {
g_print("## Not supporting messages lonmger than 125 characters ##\n");
        return NULL;
    }

    guint8 header[2];

    header[0] = first_byte;
    header[1] = msg_len;

    static guint8 array_buf[BUFFER_LEN];

    memset (array_buf, 0, BUFFER_LEN);
    memcpy (array_buf, header, 2);
    memcpy (array_buf + 2, message, msg_len);

    GBytes *test = g_bytes_new (array_buf, msg_len + 2);

    return test;
}

GBytes *
gnc_ws_send_message (const gchar *message)
{
    return make_frame (OPCODE_TEXT, message);
}


static gboolean
parse_frame (GBytes *bytes_in, gint *opcode, GBytes **bytes_out)
{
    gsize length;
    const guint8 *array = g_bytes_get_data (bytes_in, &length);

    const guint8 first_byte = array[0];
    const guint8 second_byte = array[1];

    gboolean fin = (first_byte >> 7) & 1;
    gboolean rsv1 = (first_byte >> 6) & 1;
    gboolean rsv2 = (first_byte >> 5) & 1;
    gboolean rsv3 = (first_byte >> 4) & 1;
    *opcode = first_byte & 0xf;

    if (rsv1 || rsv2 || rsv3)
    {
g_print("WebSocketError: Received frame with non-zero reserved bits\n");
         return FALSE;
    }

    if (fin == 0 && *opcode == OPCODE_CONTINUATION)
    {
g_print("WebSocketError: Received new fragment frame with non-zero opcode\n");
        return FALSE;
    }

    gboolean has_mask = (second_byte >> 7) & 1;
    guint8 payload_len = second_byte & 0x7f;
    gint offset = 2;

    if ((*opcode > 0x7) && (payload_len > 125))
    {
g_print("WebSocketError: Control frame payload cannot be larger than 125 bytes\n");
       return FALSE;
    }

    if (payload_len > 125)
    {
g_print("## Payload length greater than 125 bytes, not suported ##\n");
       return FALSE;
    }

    if (has_mask)
    {
        guint8 masks[4];
        gint i = 0;

        masks[0] = array[offset];
        masks[1] = array[offset + 1];
        masks[2] = array[offset + 2];
        masks[3] = array[offset + 3];

        offset = offset + 4;

        guint8 mypayload[length];

        while ((i + offset) < length)
        {
            guint8 data = array[i + offset] ^ masks[i % 4];
            mypayload[i] = data;
            i++;
        }
        *bytes_out = g_bytes_new (mypayload, length - offset);
    }
    else
        *bytes_out = g_bytes_new_from_bytes (bytes_in, offset, length - offset);

    return TRUE;
}

gchar *
gnc_ws_parse_bytes_in (GBytes *bytes_in, gint *opcode_out)
{
    GBytes *bytes_out = NULL;
    gint opcode;
    gboolean ok = FALSE;

    ok = parse_frame (bytes_in, &opcode, &bytes_out);

    if (!ok)
    {
g_print("## Parsing bytes in failed ##\n");
        return NULL;
    }

    *opcode_out = opcode;

    if (opcode == OPCODE_TEXT)
    {
        gsize length;
        const guint8 *array = g_bytes_get_data (bytes_out, &length);
        GString *data_out = g_string_new (NULL);
        gint i = 0;

        while (i < length)
        {
            guint8 data = array[i];

            g_string_append_printf (data_out, "%c", data);
            i++;
        }
        return  g_string_free (data_out, FALSE);
    }
    return NULL;
}


gchar *
gnc_ws_make_handshake (const gchar *message, gchar **id)
{
    gchar **message_split = g_strsplit (message, "\r\n", -1);
    gchar *message_out = NULL;
    gchar *key = NULL;

    gchar *message_test = find_header_text (message_split, "GET");

    if (!message_test)
    {
g_print("  Error: No Get\n");
        return NULL;
    }
    else
    {
        gchar *ptr = g_strrstr (message_test, " ");

        if (ptr)
            *id = g_utf8_substring (message_test, 5, ptr - message_test);
    }
    g_free (message_test);

    message_test = find_header_text (message_split, "Upgrade");
    if (!message_test)
    {
g_print("  Error: No upgrade hdr\n");
        return NULL;
    }
    g_free (message_test);

    message_test = find_header_text (message_split, "Sec-WebSocket-Version");
    if (message_test)
    {
         if (!g_strstr_len (message_test , -1, "13"))
         {
g_print("  Error: Unsupported version\n");
            g_free (message_test);
            return NULL;
         }
    }
    else
    {
g_print("  Error: Did not find websocket version\n");
        return NULL;
    }
    g_free (message_test);

    message_test = find_header_text (message_split, "Sec-WebSocket-Key");

    if (message_test)
    {
        gchar *ptr = g_strstr_len (message_test , -1, ":");

        if (ptr)
        {
            key = g_strstrip (g_strdup (ptr + 1));

            gsize out_len;
            guchar *decoded = g_base64_decode (key, &out_len);

            if (out_len != 16)
            {
g_print("  Error: Key wrong length\n");
                g_free (message_test);
                return NULL;
            }
            g_free (decoded);
        }
        else
        {
g_print("  Error: Key not found\n");
            g_free (message_test);
            return NULL;
        }
    }
    else
    {
g_print("  Error: Key not found\n");
        g_free (message_test);
        return NULL;
    }
    g_free (message_test);

    guint8 buf[100];
    gsize bsize = 100;

    GChecksum *cs = g_checksum_new (G_CHECKSUM_SHA1);

    gchar *digest = g_strconcat (key, WS_KEY, NULL);

    g_checksum_update (cs, (guint8*)digest, -1);
    g_checksum_get_digest (cs, buf, &bsize);
    gchar *str = g_base64_encode (buf, bsize);
    message_out = g_strconcat (WS_HS_ACCEPT, str, "\r\n\r\n", NULL);

    g_checksum_free (cs);
    g_strfreev (message_split);
    g_free (key);

    return message_out;
}

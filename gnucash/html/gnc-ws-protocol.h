/********************************************************************
 * gnc-ws-protocol.h -- basic websocket server protocol             *
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

#ifndef WS_PROTOCOL_H
#define WS_PROTOCOL_H

#include <config.h>

#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gio/gio.h>

#define OPCODE_CONTINUATION 0x0
#define OPCODE_TEXT 0x1
#define OPCODE_BINARY 0x2
#define OPCODE_CLOSE 0x8
#define OPCODE_PING 0x9
#define OPCODE_PONG 0xa

gchar *gnc_ws_make_handshake (const gchar *message, gchar **id);

gchar *gnc_ws_parse_bytes_in (GBytes *bytes_in, gint *opcode);

GBytes *gnc_ws_send_message (const gchar *message);

#endif

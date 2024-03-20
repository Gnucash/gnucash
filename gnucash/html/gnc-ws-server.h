/********************************************************************
 * gnc-ws-server.h -- basic websocket server                        *
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
\********************************************************************/

#ifndef WS_SERVER_H
#define WS_SERVER_H

#include <config.h>

#define GNC_TYPE_WS_SERVER            (gnc_ws_server_get_type ())
#define GNC_WS_SERVER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_WS_SERVER, GncWsServer))
#define GNC_WS_SERVER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_WS_SERVER, GncWsServerClass))
#define GNC_IS_WS_SERVER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_WS_SERVER))
#define GNC_IS_WS_SERVER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), GNC_TYPE_WS_SERVER))
#define GNC_WS_SERVER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_WS_SERVER, GncWsServerClass))

typedef struct _GncWsServer      GncWsServer;
typedef struct _GncWsServerClass GncWsServerClass;

struct _GncWsServer
{
    GObject parent;
    GSocketService *service;
    gulong incoming_id;
    GHashTable *connections_hash;
};

struct _GncWsServerClass
{
    GObjectClass parent;
};

GType gnc_ws_server_get_type (void) G_GNUC_CONST;

GncWsServer *gnc_ws_server_new (void);

void gnc_ws_server_send_message (GncWsServer *gws, const gchar *id, const gchar *message);

#endif

/********************************************************************
 * gnc-http.h -- handle HTTP requests.  thin wrapper on gnome-http. * 
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

#ifndef GNC_HTTP_H
#define GNC_HTTP_H

#include <glib.h>

typedef struct _gnc_http gnc_http;

typedef void (* GncHTTPRequestCB)(const char * uri, int completed_ok,
                                  const char * body, int body_len,
                                  gpointer user_data);
gnc_http    * gnc_http_new(void);
void          gnc_http_destroy(gnc_http * html);
void          gnc_http_start_request(gnc_http * http, const char * uri,
                                     GncHTTPRequestCB cb, gpointer user_data);
void          gnc_http_start_post(gnc_http * http, const char * uri,
                                  const char * content_type,
                                  const char * body, int body_len, 
                                  GncHTTPRequestCB cb, gpointer user_data);
void          gnc_http_cancel_requests(gnc_http * http);

#endif

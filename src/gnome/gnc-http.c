/********************************************************************
 * gnc-http.c -- handle processing of HTTP requests via gnome-http  *
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

#include "config.h"

#ifdef HAVE_OPENSSL
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/crypto.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <ghttp_ssl.h>
#endif

#include <ghttp.h>
#include <glib.h>
#include <gtk/gtkmain.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-http.h"
#include "messages.h"


struct _gnc_http {
  GList  * requests; 
  guint  callback_tag;
  gint   callback_enabled;
};

struct request_info {
  gchar            * uri;
  ghttp_request    * request;
  GncHTTPRequestCB callback;
  gpointer         callback_data;
};

gnc_http *
gnc_http_new(void) {
  gnc_http * ret = g_new0(struct _gnc_http, 1);
  ret->requests         = NULL;
  ret->callback_tag     = 0;
  ret->callback_enabled = FALSE;
  return ret;
}

void
gnc_http_destroy(gnc_http * http) {
  gnc_http_cancel_requests(http);
  g_free(http);
}

void
gnc_http_cancel_requests(gnc_http * http) {
  GList * current;
  
  if(http->callback_enabled == TRUE) {
    /* FIXME : should replace this with glib idle function -- bg */
    gtk_timeout_remove(http->callback_tag);
    http->callback_enabled = FALSE;
    http->callback_tag = 0;
    
    /* go through and destroy all the requests */
    for(current = http->requests; current; current = current->next) {
      if(current->data) {
        struct request_info * r = current->data;
        current->data = NULL;
        ghttp_request_destroy(r->request);
        r->request = NULL;
        g_free(r->uri);
        g_free(r);
      }
    }
    
    /* free the list backbone */
    g_list_free(http->requests);
    http->requests = NULL;
  }  
}

static gint
ghttp_check_callback(gpointer data) {
  gnc_http            * http = data;
  GList               * current = NULL; 
  ghttp_status        status;
  struct request_info * req = NULL;
  
  /* walk the request list to deal with any complete requests */
  for(current = http->requests; current; current = current->next) {
    req = current->data;

    if(!req) {
      g_warning("NULL request.");
      continue;
    }

    status = ghttp_process(req->request);
    switch(status) {
    case ghttp_done:
      if(req->callback) {
        (req->callback)(req->uri, TRUE, 
                        ghttp_get_body(req->request),
                        ghttp_get_body_len(req->request),
                        req->callback_data);
      }
      ghttp_request_destroy(req->request);
      req->request   = NULL;
      current->data  = NULL;
      g_free(req);
      break;

    case ghttp_error:
      if(req->callback) {
        (req->callback)(req->uri, FALSE,
                        ghttp_get_error(req->request),
                        strlen(ghttp_get_error(req->request)),
                        req->callback_data);
      }
      ghttp_request_destroy(req->request);
      req->request   = NULL;
      current->data  = NULL;
      g_free(req);
      break;

    case ghttp_not_done:
      break;
    }
  }
  
  /* walk the list again to remove dead requests */
  current = http->requests;
  while(current) {
    if(current->data == NULL) {
      http->requests = g_list_remove_link(http->requests, current);
      current = http->requests;
    }
    else {
      current = current->next;
    }
  }

  /* if all requests are done, disable the timeout */
  if(http->requests == NULL) {
    http->callback_enabled = FALSE;
    http->callback_tag = 0;
    return FALSE;
  }
  else {
    return TRUE;
  }
}


#ifdef HAVE_OPENSSL
static int
gnc_http_certificate_check_cb(ghttp_request * req, X509 * cert, 
                              void * user_data) {
  printf(_("checking SSL certificate..."));
  X509_print_fp(stdout, cert);
  printf(_(" ... done\n"));
  return TRUE;
}
#endif

void 
gnc_http_start_request(gnc_http * http, const gchar * uri, 
                       GncHTTPRequestCB cb, gpointer user_data) {
  
  struct request_info * info = g_new0(struct request_info, 1);
  
  info->request = ghttp_request_new();
  info->uri  = g_strdup (uri);
  info->callback = cb;
  info->callback_data = user_data;

#ifdef HAVE_OPENSSL
  ghttp_enable_ssl(info->request);
  ghttp_set_ssl_certificate_callback(info->request, 
                                     gnc_http_certificate_check_cb, 
                                     (void *)http);
#endif
  ghttp_set_uri(info->request, strdup(uri));
  ghttp_set_header(info->request, http_hdr_User_Agent,
                   strdup(PACKAGE "/" VERSION " ; http://gnucash.org"));
  ghttp_set_sync(info->request, ghttp_async);
  ghttp_set_type(info->request, ghttp_type_get);
  ghttp_prepare(info->request);
  ghttp_process(info->request);

  http->requests = g_list_append(http->requests, info);
  
  /* start the gtk timeout if not started */
  /* FIXME : should replace this with glib idle function -- bg */
  if(!http->callback_enabled) {
    
    http->callback_tag = 
      gtk_timeout_add(100, ghttp_check_callback, (gpointer)http);
    http->callback_enabled = TRUE;
  }
}

void
gnc_http_start_post(gnc_http * http, const char * uri, 
                    const char * content_type, 
                    const char * data, int datalen,
                    GncHTTPRequestCB cb, gpointer user_data) {
  struct request_info * info = g_new0(struct request_info, 1);
  char                * new_body = malloc(datalen);
  
  info->request = ghttp_request_new();
  info->uri     = g_strdup (uri);
  info->callback = cb;
  info->callback_data = user_data;
#ifdef HAVE_OPENSSL
  ghttp_enable_ssl(info->request);
  ghttp_set_ssl_certificate_callback(info->request, 
                                     gnc_http_certificate_check_cb, 
                                     (void *)http);
#endif
  ghttp_set_uri(info->request, strdup(uri));
  ghttp_set_header(info->request, http_hdr_User_Agent,
                   strdup(PACKAGE "/" VERSION " ; http://gnucash.org"));
  ghttp_set_header(info->request, http_hdr_Content_Type, content_type);
  ghttp_set_sync(info->request, ghttp_async);
  ghttp_set_type(info->request, ghttp_type_post);
  memcpy(new_body, data, datalen);
  ghttp_set_body(info->request, (char*)new_body, datalen);

  ghttp_prepare(info->request);
  ghttp_process(info->request);

  http->requests = g_list_append(http->requests, info);
  
  /* start the gtk timeout if not started */
  if(!http->callback_enabled) {
    http->callback_tag = 
      gtk_timeout_add(100, ghttp_check_callback, (gpointer)http);
    http->callback_enabled = TRUE;
  }  
}


/********************************************************************\
 * gnc-html.h -- display html with gnc special tags
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
\********************************************************************/

#ifndef __GNC_HTML_H__
#define __GNC_HTML_H__

#include <gnome.h>
#include <ghttp.h>
#include <gtkhtml/gtkhtml.h>

typedef enum { URL_TYPE_FILE, URL_TYPE_JUMP, 
               URL_TYPE_HTTP, URL_TYPE_FTP, 
               URL_TYPE_SECURE, 
               URL_TYPE_REGISTER,   /* for gnucash register popups */
               URL_TYPE_REPORT,     /* for gnucash report popups */
               URL_TYPE_SCHEME,     /* for scheme code evaluation */
               URL_TYPE_HELP,       /* for a gnucash help window */
               URL_TYPE_OTHER } URLType;

#include "gnc-html-history.h"

typedef struct _gnc_html gnc_html;

typedef int  (*GncHTMLUrltypeCB)(URLType ut);
typedef void (* GncHTMLFlyoverCB)(gnc_html * html, const char * url,
                                  gpointer data);
typedef void (* GncHTMLLoadCB)(gnc_html * html, URLType type, 
                               const char * location, const char * label,
                               gpointer data);
typedef int  (* GncHTMLButtonCB)(gnc_html * html, GdkEventButton * event,
                                 gpointer data);

gnc_html    * gnc_html_new(void);
void          gnc_html_destroy(gnc_html * html);
void          gnc_html_show_url(gnc_html * html, 
                                URLType type, const char * location, 
                                const char * label, int newwin_hint);
void          gnc_html_reload(gnc_html * html);
void          gnc_html_print(gnc_html * html);
void          gnc_html_cancel(gnc_html * html);

URLType
gnc_html_parse_url(gnc_html * html, const gchar * url, 
                   char ** url_location, char ** url_label);

gnc_html_history * gnc_html_get_history(gnc_html * html);
GtkWidget   * gnc_html_get_widget(gnc_html * html);

/* setting callbacks */
void gnc_html_set_urltype_cb(gnc_html * html, GncHTMLUrltypeCB urltype_cb);
void gnc_html_set_load_cb(gnc_html * html, GncHTMLLoadCB load_cb,
                          gpointer data);
void gnc_html_set_flyover_cb(gnc_html * html, GncHTMLFlyoverCB newwin_cb,
                             gpointer data);
void gnc_html_set_button_cb(gnc_html * html, GncHTMLButtonCB button_cb,
                            gpointer data);

GtkWidget   * gnc_html_get_container_widget(gnc_html * html);
GtkWidget   * gnc_html_get_html_widget(gnc_html * html); 

#endif

/********************************************************************
 * gnc-html.h -- display html with gnc special tags                 *
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

#include <glib.h>
#include <gtkhtml/gtkhtml.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>

typedef enum { URL_TYPE_FILE, URL_TYPE_JUMP, 
               URL_TYPE_HTTP, URL_TYPE_FTP, 
               URL_TYPE_SECURE, 
               URL_TYPE_REGISTER,   /* for gnucash register popups */
               URL_TYPE_REPORT,     /* for gnucash report popups */
               URL_TYPE_SCHEME,     /* for scheme code evaluation */
               URL_TYPE_HELP,       /* for a gnucash help window */
               URL_TYPE_XMLDATA,    /* links to gnucash XML data files */ 
               URL_TYPE_ACTION,     /* for special SUBMIT actions */ 
               URL_TYPE_OTHER } URLType;

#include "gnc-html-history.h"

typedef struct _gnc_html gnc_html;

typedef int  (* GncHTMLUrltypeCB)(URLType ut);
typedef void (* GncHTMLFlyoverCB)(gnc_html * html, const char * url,
                                  gpointer data);
typedef void (* GncHTMLLoadCB)(gnc_html * html, URLType type, 
                               const char * location, const char * label,
                               gpointer data);
typedef int  (* GncHTMLButtonCB)(gnc_html * html, GdkEventButton * event,
                                 gpointer data);
typedef int  (* GncHTMLObjectCB)(gnc_html * html, GtkHTMLEmbedded * eb,
                                 gpointer data); 
typedef int  (* GncHTMLActionCB)(gnc_html * html, const char * method,
                                 const char * action, GHashTable * form_data);

gnc_html    * gnc_html_new(void);
void          gnc_html_destroy(gnc_html * html);
void          gnc_html_show_url(gnc_html * html, 
                                URLType type, const char * location, 
                                const char * label, int newwin_hint);
void          gnc_html_show_data(gnc_html * html, 
                                 const char * data, int datalen);
void          gnc_html_reload(gnc_html * html);
void          gnc_html_export(gnc_html * html);
void          gnc_html_print(gnc_html * html);
void          gnc_html_cancel(gnc_html * html);

/* object handlers deal with <object classid="foo"> objects in HTML.
 * the handlers are looked up at object load time. */
void          gnc_html_register_object_handler(const char * classid, 
                                               GncHTMLObjectCB hand);
void          gnc_html_unregister_object_handler(const char * classid);

/* action handlers deal with submitting forms of the type 
 * <FORM action="gnc-action:action?args">.  Normal get/post http:
 * forms are handled as would be expected, with no callback. */
void          gnc_html_register_action_handler(const char * action, 
                                               GncHTMLActionCB hand);
void          gnc_html_unregister_action_handler(const char * action);

/* default action handlers for GET and POST methods.  'generic_post'
 * is the trivial application/x-www-form-urlencoded submit,
 * multipart-post is a multipart/form-data submit. */
void          gnc_html_generic_get_submit(gnc_html * html, const char * act, 
                                          GHashTable * form_data);
void          gnc_html_generic_post_submit(gnc_html * html, const char * act, 
                                           GHashTable * form_data);
void          gnc_html_multipart_post_submit(gnc_html * html, const char * a, 
                                             GHashTable * form_data);

URLType       gnc_html_parse_url(gnc_html * html, const gchar * url, 
                                 char ** url_location, char ** url_label);

/* some string coding/decoding routines */
char          * gnc_html_encode_string(const char * in);
char          * gnc_html_decode_string(const char * in);
char          * gnc_html_escape_newlines(const char * in);
char          * gnc_html_unescape_newlines(const char * in);

/* utilities for dealing with encoded argument strings for forms */
char          * gnc_html_pack_form_data(GHashTable * form_data);
GHashTable    * gnc_html_unpack_form_data(const char * encoding);
void            gnc_html_merge_form_data(GHashTable * fdata, const char * enc);
void            gnc_html_free_form_data(GHashTable * fdata);

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

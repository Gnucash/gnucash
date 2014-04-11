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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_HTML_H
#define GNC_HTML_H

#include <gtkhtml/gtkhtml.h>

typedef char * URLType;

#define URL_TYPE_FILE	"file"
#define URL_TYPE_JUMP	"jump"
#define URL_TYPE_HTTP	"http"
#define URL_TYPE_FTP	"ftp"
#define URL_TYPE_SECURE	"secure"
#define URL_TYPE_REGISTER	"register"   /* for gnucash register popups */
#define URL_TYPE_ACCTTREE	"accttree"   /* for account tree windows */
#define URL_TYPE_REPORT	"report"     /* for gnucash report popups */
#define URL_TYPE_OPTIONS	"options"    /* for editing report options */ 
#define URL_TYPE_SCHEME	"scheme"     /* for scheme code evaluation */
#define URL_TYPE_HELP	"help"       /* for a gnucash help window */
#define URL_TYPE_XMLDATA	"xmldata"    /* links to gnucash XML data files */ 
#define URL_TYPE_PRICE	"price"      /* for price editor popups */
#define URL_TYPE_OTHER	"other"
#define URL_TYPE_BUDGET "budget"

#include "gnc-html-history.h"

typedef struct gnc_html_struct gnc_html;

/* The result structure of url handlers. Strings should be g_malloc'd
 * by the handler and will be freed by gnc_html. */
typedef struct
{
  /* The following members are used if the handler succeeds (returns TRUE). */

  gboolean load_to_stream; /* If TRUE, the url should be loaded from
                            * a stream using the rest of the data in
                            * the struct into the original gnc_html
                            * object. If FALSE, the handler will
                            * perform all needed actions itself. */

  URLType url_type;        /* Defaults to original */
  char * location;         /* If NULL, use original (NULL is default) */
  char * label;            /* If NULL, use original (NULL is default) */

  URLType base_type;
  char * base_location;

  /* The following members are used if the handler fails (returns FALSE). */
  char * error_message;
} GNCURLResult;

typedef int  (* GncHTMLUrltypeCB)(URLType ut);
typedef void (* GncHTMLFlyoverCB)(gnc_html * html, const char * url,
                                  gpointer data);
typedef void (* GncHTMLLoadCB)(gnc_html * html, URLType type, 
                               const char * location, const char * label,
                               gpointer data);
typedef int  (* GncHTMLButtonCB)(gnc_html * html, GdkEventButton * event,
                                 gpointer data);

typedef gboolean (* GncHTMLObjectCB)(gnc_html * html, GtkHTMLEmbedded * eb,
                                 gpointer data); 
typedef int  (* GncHTMLActionCB)(gnc_html * html, const char * method,
                                 const char * action, GHashTable * form_data);
typedef gboolean (* GncHTMLStreamCB)(const char *location, char **data, int *datalen);
typedef gboolean (* GncHTMLUrlCB)(const char *location, const char *label,
                                  gboolean new_window, GNCURLResult * result);

gnc_html    * gnc_html_new(GtkWindow *parent);
void          gnc_html_destroy(gnc_html * html);
void          gnc_html_show_url(gnc_html * html, 
                                URLType type,
                                const char * location, 
                                const char * label,
                                gboolean new_window_hint);
void          gnc_html_show_data(gnc_html * html, 
                                 const char * data, int datalen);
void          gnc_html_reload(gnc_html * html);
void          gnc_html_copy(gnc_html *html);
gboolean      gnc_html_export(gnc_html * html, const char *file);
void          gnc_html_print(gnc_html * html);
void          gnc_html_cancel(gnc_html * html);

char  * gnc_build_url (URLType type, const gchar * location,
                       const gchar * label);

/* Register a new URLType.
 * returns TRUE if succesful, FALSE if type already exists.
 *
 * protocol should be an empty string if there is no corresponding protocol.
 * if protocol is NULL, this function returns FALSE.
 */
gboolean      gnc_html_register_urltype (URLType type, const char *protocol);

/* object handlers deal with <object classid="foo"> objects in HTML.
 * the handlers are looked up at object load time. */
//#if 0
void          gnc_html_register_object_handler(const char * classid, 
                                               GncHTMLObjectCB hand);
//#endif
void          gnc_html_unregister_object_handler(const char * classid);

/* action handlers deal with submitting forms of the type 
 * <FORM action="gnc-action:action?args">.  Normal get/post http:
 * forms are handled as would be expected, with no callback. */
void          gnc_html_register_action_handler(const char * action, 
                                               GncHTMLActionCB hand);
void          gnc_html_unregister_action_handler(const char * action);

/* stream handlers load data for particular URLTypes. */
void          gnc_html_register_stream_handler(URLType url_type,
                                               GncHTMLStreamCB hand);
void          gnc_html_unregister_stream_handler(URLType url_type);

/* handlers for particular URLTypes. */
void          gnc_html_register_url_handler(URLType url_type,
                                            GncHTMLUrlCB hand);
void          gnc_html_unregister_url_handler(URLType url_type);

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

/* Initialize the html subsystem */
void gnc_html_initialize (void);

#endif

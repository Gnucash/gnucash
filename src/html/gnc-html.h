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

#include <glib-object.h>

G_BEGIN_DECLS

#define GNC_TYPE_HTML         (gnc_html_get_type())
#define GNC_HTML(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML, GncHtml))
#define GNC_HTML_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML, GncHtmlClass))
#define GNC_IS_HTML(o)        (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML))
#define GNC_IS_HTML_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE((o), GNC_TYPE_HTML))
#define GNC_HTML_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML, GncHtmlClass))

GType gnc_html_get_type(void);

typedef struct _GncHtml GncHtml;
typedef struct _GncHtmlClass GncHtmlClass;
typedef struct _GncHtmlPrivate GncHtmlPrivate;

#include "gnc-html-extras.h"

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
	gchar* location;         /* If NULL, use original (NULL is default) */
	gchar* label;            /* If NULL, use original (NULL is default) */

	URLType base_type;
	gchar* base_location;

	/* The following members are used if the handler fails (returns FALSE). */
	gchar* error_message;
} GNCURLResult;

typedef gboolean (* GncHTMLObjectCB)(GncHtml* html, gpointer eb,
                                 gpointer data); 
typedef int  (* GncHTMLActionCB)(GncHtml* html, const gchar* method,
                                 const gchar* action, GHashTable* form_data);
typedef gboolean (* GncHTMLStreamCB)(const gchar* location, gchar** data, int* datalen);
typedef gboolean (* GncHTMLUrlCB)(const gchar* location, const gchar* label,
                                  gboolean new_window, GNCURLResult* result);

/**
 * Registers a new URLType.
 * returns TRUE if succesful, FALSE if type already exists.
 *
 * @param type New URL type
 * @param prococol Protocol - should be an empty string if there is no corresponding protocol.
 * @return TRUE if successful, FALSE if type already exists or protocol is NULL.
 */
gboolean gnc_html_register_urltype( URLType type, const gchar* protocol );

/**
 * Initializes the html subsystem
 */
void gnc_html_initialize( void );

gchar* gnc_html_encode_string( const gchar* in );
gchar* gnc_html_decode_string( const gchar* in );
gchar* gnc_html_escape_newlines( const gchar* in );
gchar* gnc_html_unescape_newlines( const gchar* in );

/* utilities for dealing with encoded argument strings for forms */
gchar* gnc_html_pack_form_data( GHashTable* form_data );
GHashTable* gnc_html_unpack_form_data( const gchar* encoding );
void gnc_html_merge_form_data( GHashTable* fdata, const gchar* enc );
void gnc_html_free_form_data( GHashTable* fdata );

/* object handlers deal with <object classid="foo"> objects in HTML.
 * the handlers are looked up at object load time. */
void gnc_html_register_object_handler( const gchar* classid, GncHTMLObjectCB hand );
void gnc_html_unregister_object_handler( const gchar* classid );

/* action handlers deal with submitting forms of the type 
 * <FORM action="gnc-action:action?args">.  Normal get/post http:
 * forms are handled as would be expected, with no callback. */
void gnc_html_register_action_handler( const gchar* action, GncHTMLActionCB hand );
void gnc_html_unregister_action_handler( const gchar* action );

/* stream handlers load data for particular URLTypes. */
void gnc_html_register_stream_handler( URLType url_type, GncHTMLStreamCB hand );
void gnc_html_unregister_stream_handler( URLType url_type );

/* handlers for particular URLTypes. */
void gnc_html_register_url_handler( URLType url_type, GncHTMLUrlCB hand );
void gnc_html_unregister_url_handler( URLType url_type );

#include "gnc-html-history.h"

typedef int  (* GncHTMLUrltypeCB)(URLType ut);
typedef void (* GncHTMLFlyoverCB)(GncHtml* html, const gchar* url,
                                  gpointer data);
typedef void (* GncHTMLLoadCB)(GncHtml* html, URLType type, 
                               const gchar* location, const gchar* label,
                               gpointer data);
typedef int  (* GncHTMLButtonCB)(GncHtml* html, GdkEventButton* event,
                                 gpointer data);

struct _GncHtmlClass
{
	GtkBinClass parent_class;

	/* Methods */
	void (*show_url)( GncHtml* html, 
                      URLType type,
                      const gchar* location, 
                      const gchar* label,
                      gboolean new_window_hint );
	void (*show_data)( GncHtml* html, const gchar* data, int datalen );
	void (*reload)( GncHtml* html );
	void (*copy)( GncHtml* html );
	gboolean (*export)( GncHtml* html, const gchar* file );
	void (*print)( GncHtml* html );
	void (*cancel)( GncHtml* html );
	URLType (*parse_url)( GncHtml* html, const gchar* url, 
                          gchar** url_location, gchar** url_label );
	void (*set_parent)( GncHtml* html, GtkWindow* parent );
};

struct _GncHtml
{
	GtkBin parent_instance;

	/*< private >*/
	GncHtmlPrivate* priv;
};

/**
 * Destroys a GncHtml object.
 *
 * @param html GncHtml object to destroy
 */
void gnc_html_destroy( GncHtml* html );

/**
 * Displays a URL in a GncHtml object.
 *
 * @param html GncHtml object
 */
void gnc_html_show_url( GncHtml* html, URLType type, const gchar* location, 
						const gchar* label, gboolean new_window_hint );

/**
 * Displays an HTML string in a GncHtml object.
 *
 * @param html GncHtml object
 */
void gnc_html_show_data( GncHtml* html, const gchar* data, int datalen );

/**
 * Reloads the current GncHtml object.
 *
 * @param html GncHtml object
 */
void gnc_html_reload( GncHtml* html );

/**
 * 
 * @param html GncHtml object
 */
void gnc_html_copy( GncHtml* html );

/**
 *
 * @param html GncHtml object
 */
gboolean gnc_html_export( GncHtml* html, const gchar* file );

/**
 *
 * @param html GncHtml object
 */
void gnc_html_print( GncHtml* html );

/**
 *
 * @param html GncHtml object
 */
void gnc_html_cancel( GncHtml* html );

/**
 *
 * @param html GncHtml object
 */
URLType gnc_html_parse_url( GncHtml* html, const gchar* url, 
							gchar** url_location, gchar** url_label );

/**
 *
 * @param html GncHtml object
 */
gnc_html_history* gnc_html_get_history( GncHtml* html );

/**
 *
 * @param html GncHtml object
 */
GtkWidget* gnc_html_get_widget( GncHtml* html );

/**
 *
 * @param html GncHtml object
 */
void gnc_html_set_parent( GncHtml* html, GtkWindow* parent );

/* setting callbacks */
void gnc_html_set_urltype_cb( GncHtml* html, GncHTMLUrltypeCB urltype_cb );
void gnc_html_set_load_cb( GncHtml* html, GncHTMLLoadCB load_cb, gpointer data );
void gnc_html_set_flyover_cb( GncHtml* html, GncHTMLFlyoverCB newwin_cb, gpointer data );
void gnc_html_set_button_cb( GncHtml* html, GncHTMLButtonCB button_cb, gpointer data );

/* object handlers deal with <object classid="foo"> objects in HTML.
 * the handlers are looked up at object load time. */
void gnc_html_register_object_handler( const gchar* classid, GncHTMLObjectCB hand );
void gnc_html_unregister_object_handler( const gchar* classid );

/* action handlers deal with submitting forms of the type 
 * <FORM action="gnc-action:action?args">.  Normal get/post http:
 * forms are handled as would be expected, with no callback. */
void gnc_html_register_action_handler( const gchar* action, GncHTMLActionCB hand );
void gnc_html_unregister_action_handler( const gchar* action );

/* stream handlers load data for particular URLTypes. */
void gnc_html_register_stream_handler( URLType url_type, GncHTMLStreamCB hand );
void gnc_html_unregister_stream_handler( URLType url_type );

/* handlers for particular URLTypes. */
void gnc_html_register_url_handler( URLType url_type, GncHTMLUrlCB hand );
void gnc_html_unregister_url_handler( URLType url_type );

const gchar* gnc_html_get_embedded_param( gpointer eb, const gchar* param_name );

#endif

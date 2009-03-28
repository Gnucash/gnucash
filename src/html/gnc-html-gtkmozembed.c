/********************************************************************
 * gnc-html_gtkmozembed.c -- display HTML with some special gnucash *
 *                           tags.                                  *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>
#include <libguile.h>

#include <gtkmozembed.h>

#include "Account.h"
#include "print-session.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-html.h"
#include "gnc-html-gtkmozembed.h"
#include "gnc-html-history.h"
#include "gnc-html-graph-gog-gtkmozembed.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

G_DEFINE_TYPE(GncHtmlGtkmozembed, gnc_html_gtkmozembed, GNC_TYPE_HTML )

static void gnc_html_gtkmozembed_dispose( GObject* obj );
static void gnc_html_gtkmozembed_finalize( GObject* obj );
static void gnc_html_gtkmozembed_class_init( GncHtmlGtkmozembedClass* klass );
static void gnc_html_gtkmozembed_init( GncHtmlGtkmozembed* gs );

//#define GNC_HTML_GTKMOZEMBED_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_HTML_GTKMOZEMBED, GncHtmlGtkmozembedPrivate))
#define GNC_HTML_GTKMOZEMBED_GET_PRIVATE(o) (GNC_HTML_GTKMOZEMBED(o)->priv)

#include "gnc-html-gtkmozembed-p.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
//extern GHashTable* gnc_html_type_to_proto_hash;
extern GHashTable* gnc_html_proto_to_type_hash;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes an action name from a FORM definition to a handler function.
 * <form method=METHOD action=gnc-action:ACTION-NAME?ACTION-ARGS>
 * action-args is what gets passed to the handler. */
extern GHashTable* gnc_html_action_handlers;

/* hashes handlers for loading different URLType data */
extern GHashTable* gnc_html_stream_handlers;

/* hashes handlers for handling different URLType data */
extern GHashTable* gnc_html_url_handlers;

static char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] = N_("The specified URL could not be loaded.");

#if 0
static void webkit_navigation_requested_cb( WebKitWebView* web_view, GObject* arg1,
												GObject* arg2, gpointer data );
static void webkit_on_url_cb( WebKitWebView* web_view, gchar* title, gchar* url,
							gpointer data );
#endif
#if 0
static void gnc_html_set_base_cb( GtkHTML* gtkhtml, const gchar* base, gpointer data );
static void gnc_html_link_clicked_cb( GtkHTML* html, const gchar* url, gpointer data );
static gboolean gnc_html_object_requested_cb( GtkHTML* html, GtkHTMLEmbedded* eb,
                             gpointer data );
#endif
static int gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                         gpointer user_data );
#if 0
static int gnc_html_submit_cb( GtkHTML* html, const gchar* method,
                   const gchar* action, const gchar* encoded_form_data,
                   gpointer user_data );
#endif
static void impl_gtkmozembed_show_url( GncHtml* self, URLType type,
                  const gchar* location, const gchar* label,
                  gboolean new_window_hint );
static void impl_gtkmozembed_show_data( GncHtml* self, const gchar* data, int datalen );
static void impl_gtkmozembed_reload( GncHtml* self );
static void impl_gtkmozembed_copy( GncHtml* self );
static gboolean impl_gtkmozembed_export( GncHtml* self, const gchar* filepath );
static void impl_gtkmozembed_print( GncHtml* self );
static void impl_gtkmozembed_cancel( GncHtml* self );
static void impl_gtkmozembed_set_parent( GncHtml* self, GtkWindow* parent );

static void
gnc_html_gtkmozembed_init( GncHtmlGtkmozembed* self )
{
	GncHtmlGtkmozembedPrivate* priv;
	GncHtmlGtkmozembedPrivate* new_priv;

	new_priv = g_realloc( GNC_HTML(self)->priv, sizeof(GncHtmlGtkmozembedPrivate) );
	priv = self->priv = new_priv;

	priv->moz_embed = GTK_MOZ_EMBED(gtk_moz_embed_new());

	gtk_container_add( GTK_CONTAINER(priv->base.container),
						GTK_WIDGET(priv->moz_embed) );

#ifdef HAVE_GTK_2_10
	g_object_ref_sink( priv->base.container );
#else
	g_object_ref( priv->base.container );
	gtk_object_sink( GTK_OBJECT(priv->base.container) );
#endif

#if 0
	/* signals */
	g_signal_connect( priv->web_view, "navigation-requested",
					G_CALLBACK(webkit_navigation_requested_cb),
					self);

	g_signal_connect( priv->web_view, "hovering-over-link",
				   G_CALLBACK(webkit_on_url_cb),
				   self );

#endif
#if 0
	g_signal_connect( priv->html, "set_base",
		   G_CALLBACK(gnc_html_set_base_cb),
		   self);

	g_signal_connect(priv->html, "link_clicked",
		   G_CALLBACK(gnc_html_link_clicked_cb),
		   self);

	g_signal_connect (priv->html, "object_requested",
		    G_CALLBACK (gnc_html_object_requested_cb),
		    self);

	g_signal_connect (priv->html, "button_press_event",
		    G_CALLBACK (gnc_html_button_press_cb),
		    self);

	g_signal_connect (priv->html, "submit",
		    G_CALLBACK(gnc_html_submit_cb),
		    self);
#endif

	printf( "GTK_TYPE_MOZ_EMBED:\n" );
	show_type_signals( GTK_TYPE_MOZ_EMBED );

	LEAVE("retval %p", self);
}

static void
gnc_html_gtkmozembed_class_init( GncHtmlGtkmozembedClass* klass )
{
	GObjectClass* gobject_class = G_OBJECT_CLASS(klass);
	GncHtmlClass* html_class = GNC_HTML_CLASS(klass);

	gobject_class->dispose = gnc_html_gtkmozembed_dispose;
	gobject_class->finalize = gnc_html_gtkmozembed_finalize;

#if 0
	html_class->show_url = impl_gtkmozembed_show_url;
	html_class->show_data = impl_gtkmozembed_show_data;
	html_class->reload = impl_gtkmozembed_reload;
	html_class->copy = impl_gtkmozembed_copy;
	html_class->export = impl_gtkmozembed_export;
//	html_class->print = impl_gtkmozembed_print;
	html_class->cancel = impl_gtkmozembed_cancel;
	html_class->set_parent = impl_gtkmozembed_set_parent;
#endif

	// Initialize graphing support
	gnc_html_graph_gog_gtkmozembed_init();
}

static void
gnc_html_gtkmozembed_dispose( GObject* obj )
{
	GncHtmlGtkmozembed* self = GNC_HTML_GTKMOZEMBED(obj);
	GncHtmlGtkmozembedPrivate* priv = GNC_HTML_GTKMOZEMBED_GET_PRIVATE(self);

	if( priv->moz_embed != NULL ) {
		g_object_unref( G_OBJECT(priv->moz_embed) );
		priv->moz_embed = NULL;
	}

	G_OBJECT_CLASS(gnc_html_gtkmozembed_parent_class)->dispose( obj );
}

static void
gnc_html_gtkmozembed_finalize( GObject* obj )
{
	GncHtmlGtkmozembed* self = GNC_HTML_GTKMOZEMBED(obj);

//	if( self->priv != NULL ) {
//		g_free( self->priv );
		self->priv = NULL;
//	}

	G_OBJECT_CLASS(gnc_html_gtkmozembed_parent_class)->finalize( obj );
}
#if 0 /************************************/
/*****************************************************************************/

static char*
extract_base_name(URLType type, const gchar* path)
{
	gchar       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
	gchar       path_rexp[] = "^/*(.*)/+([^/]*)$";
	regex_t    compiled_m, compiled_p;
	regmatch_t match[4];
	gchar       * machine=NULL, * location = NULL, * base=NULL;
	gchar       * basename=NULL;

	DEBUG(" ");
	if(!path) return NULL;

	regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
	regcomp(&compiled_p, path_rexp, REG_EXTENDED);

	if (!safe_strcmp (type, URL_TYPE_HTTP) ||
			!safe_strcmp (type, URL_TYPE_SECURE) ||
			!safe_strcmp (type, URL_TYPE_FTP)) {

		/* step 1: split the machine name away from the path
		 * components */
		if(!regexec(&compiled_m, path, 4, match, 0)) {
			/* $1 is the machine name */
			if(match[1].rm_so != -1) {
				machine = g_strndup(path+match[1].rm_so,
                            match[1].rm_eo - match[1].rm_so);
			}
			/* $2 is the path */
			if(match[2].rm_so != -1) {
				location = g_strndup(path+match[2].rm_so,
                             match[2].rm_eo - match[2].rm_so);
			}
		}

	} else {
		location = g_strdup(path);
	}
	/* step 2: split up the path into prefix and file components */
	if(location) {
		if(!regexec(&compiled_p, location, 4, match, 0)) {
			if(match[1].rm_so != -1) {
				base = g_strndup(location+match[1].rm_so,
                         match[1].rm_eo - match[1].rm_so);
			} else {
				base = NULL;
			}
		}
	}

	regfree(&compiled_m);
	regfree(&compiled_p);

	if(machine) {
		if(base && (strlen(base) > 0)) {
			basename = g_strconcat(machine, "/", base, "/", NULL);
		} else {
			basename = g_strconcat(machine, "/", NULL);
		}
	} else {
		if(base && (strlen(base) > 0)) {
			basename = g_strdup(base);
		} else {
			basename = NULL;
		}
	}

	g_free(machine);
	g_free(base);
	g_free(location);
	return basename;
}

static gboolean
http_allowed()
{
	return TRUE;
}

static gboolean
https_allowed()
{
	return TRUE;
}

/********************************************************************
 * gnc_html_load_to_stream : actually do the work of loading the HTML
 * or binary data referenced by a URL and feeding it into the GtkHTML
 * widget.
 ********************************************************************/

static void
gnc_html_load_to_stream( GncHtmlWebkit* self, URLType type,
						const gchar* location, const gchar* label )
{
	gchar* fdata = NULL;
	int fdata_len = 0;
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

	DEBUG( "type %s, location %s, label %s", type ? type : "(null)",
			location ? location : "(null)", label ? label : "(null)");

	g_return_if_fail( self != NULL );

	if( gnc_html_stream_handlers != NULL ) {
		GncHTMLStreamCB stream_handler;

		stream_handler = g_hash_table_lookup( gnc_html_stream_handlers, type );
		if( stream_handler ) {
			gboolean ok = stream_handler( location, &fdata, &fdata_len );

			if( ok ) {
				fdata = fdata ? fdata : g_strdup( "" );
				webkit_web_view_load_html_string( priv->web_view, fdata, "base-uri" );
			} else {
				fdata = fdata ? fdata :
							g_strdup_printf( error_404_format,
							_(error_404_title), _(error_404_body) );
				webkit_web_view_load_html_string( priv->web_view, fdata, "base-uri" );
			}

			g_free( fdata );

			if( label ) {
				while( gtk_events_pending() ) {
					gtk_main_iteration();
				}
//				gtk_html_jump_to_anchor( GTK_HTML(priv->html), label );
				g_assert( FALSE );
			}

			return;
		}
	}

	do {
		if( !safe_strcmp( type, URL_TYPE_SECURE ) ||
			!safe_strcmp( type, URL_TYPE_HTTP ) ) {

			if( !safe_strcmp( type, URL_TYPE_SECURE ) ) {
				if( !https_allowed() ) {
					gnc_error_dialog( priv->base.parent,
                            _("Secure HTTP access is disabled. "
                              "You can enable it in the Network section of "
                              "the Preferences dialog."));
					break;
				}
			}

			if( !http_allowed() ) {
				gnc_error_dialog( priv->base.parent,
                          _("Network HTTP access is disabled. "
                            "You can enable it in the Network section of "
                            "the Preferences dialog."));
			} else {
				char *fullurl;

				fullurl = gnc_build_url( type, location, label );
			}

		} else {
			PWARN( "load_to_stream for inappropriate type\n"
					"\turl = '%s#%s'\n",
					location ? location : "(null)",
					label ? label : "(null)" );
			fdata = g_strdup_printf( error_404_format,
								_(error_404_title), _(error_404_body) );
			webkit_web_view_load_html_string( priv->web_view, fdata, "base-uri" );
			g_free( fdata );
		}

	} while( FALSE );
}

#if 0
/********************************************************************
 * gnc_html_link_clicked_cb - called when user left-clicks on html
 * anchor.
 ********************************************************************/

static void
gnc_html_link_clicked_cb( GtkHTML* html, const gchar* url, gpointer data )
{
	URLType type;
	gchar* location = NULL;
	gchar* label = NULL;
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);

	DEBUG("Clicked %s", url);
	type = gnc_html_parse_url( GNC_HTML(self), url, &location, &label );
	gnc_html_show_url( GNC_HTML(self), type, location, label, 0 );
	g_free( location );
	g_free( label );
}
#endif

/********************************************************************
 * webkit_navigation_requested_cb - called when a URL needs to be
 * loaded within the loading of a page (embedded image).
 ********************************************************************/

static void
webkit_navigation_requested_cb( WebKitWebView* web_view, GObject* arg1,
												GObject* arg2, gpointer data )
{
	URLType type;
	gchar* location = NULL;
	gchar* label = NULL;
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);
	WebKitNetworkRequest* req = WEBKIT_NETWORK_REQUEST(arg2);
	const gchar* url = webkit_network_request_get_uri( req );

	DEBUG( "requesting %s", url );
	type = gnc_html_parse_url( GNC_HTML(self), url, &location, &label );
	gnc_html_show_url( GNC_HTML(self), type, location, label, 0 );
//	gnc_html_load_to_stream( self, type, location, label );
	g_free( location );
	g_free( label );
}

#if 0
/********************************************************************
 * gnc_html_object_requested_cb - called when an applet needs to be
 * loaded.
 ********************************************************************/

static gboolean
gnc_html_object_requested_cb( GtkHTML* html, GtkHTMLEmbedded* eb,
                             gpointer data )
{
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);
	GncHTMLObjectCB h;

	DEBUG( " " );
	if( !eb || !(eb->classid) || !gnc_html_object_handlers ) return FALSE;

	h = g_hash_table_lookup( gnc_html_object_handlers, eb->classid );
	if( h ) {
		return h( GNC_HTML(self), eb, data );
	} else {
		return FALSE;
	}
}
#endif

/********************************************************************
 * webkit_on_url_cb - called when user rolls over html anchor
 ********************************************************************/

static void
webkit_on_url_cb( WebKitWebView* web_view, gchar* title, gchar* url, gpointer data )
{
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

	DEBUG( "Rollover %s", url ? url : "(null)" );
	g_free( priv->base.current_link );
	priv->base.current_link = g_strdup( url );
	if( priv->base.flyover_cb ) {
		(priv->base.flyover_cb)( GNC_HTML(self), url, priv->base.flyover_cb_data );
	}
}

#if 0
/********************************************************************
 * gnc_html_set_base_cb
 ********************************************************************/

static void
gnc_html_set_base_cb( GtkHTML* gtkhtml, const gchar* base,
                     gpointer data )
{
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
	URLType type;
	gchar* location = NULL;
	gchar* label = NULL;

	DEBUG( "Setting base location to %s", base );
	type = gnc_html_parse_url( GNC_HTML(self), base, &location, &label );

	g_free( priv->base.base_location );
	g_free( label );

	priv->base.base_type = type;
	priv->base.base_location = location;
}
#endif

/********************************************************************
 * gnc_html_button_press_cb
 * mouse button callback (if any)
 ********************************************************************/

static int
gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                         gpointer user_data )
{
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

	DEBUG( "Button Press" );
	if( priv->base.button_cb != NULL ) {
		(priv->base.button_cb)( GNC_HTML(self), event, priv->base.button_cb_data );
		return TRUE;
	} else {
		return FALSE;
	}
}

/********************************************************************
 * gnc_html_button_submit_cb
 * form submission callback
 ********************************************************************/

#if 0
static int
gnc_html_submit_cb( GtkHTML* html, const gchar* method,
                   const gchar* action, const gchar* encoded_form_data,
                   gpointer user_data )
{
	GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
	gchar* location = NULL;
	gchar* new_loc = NULL;
	gchar* label = NULL;
	GHashTable * form_data;
	URLType  type;

	DEBUG(" ");
	form_data = gnc_html_unpack_form_data( encoded_form_data );
	type = gnc_html_parse_url( GNC_HTML(self), action, &location, &label );

	g_critical( "form submission hasn't been supported in years." );

	g_free( location );
	g_free( label );
	g_free( new_loc );
	gnc_html_free_form_data( form_data );
	return TRUE;
}
#endif

/********************************************************************
 * gnc_html_open_scm
 * insert some scheme-generated HTML
 ********************************************************************/

static void
gnc_html_open_scm( GncHtmlWebkit* self, const gchar * location,
                  const gchar * label, int newwin )
{
	PINFO("location='%s'", location ? location : "(null)");
}


/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from
 * somewhere.
 ********************************************************************/

static void
impl_webkit_show_data( GncHtml* self, const gchar* data, int datalen )
{
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

	DEBUG( "datalen %d, data %20.20s", datalen, data );
	webkit_web_view_load_html_string( priv->web_view, data, "base-uri" );
}

/********************************************************************
 * gnc_html_show_url
 *
 * open a URL.  This is called when the user clicks a link or
 * for the creator of the gnc_html window to explicitly request
 * a URL.
 ********************************************************************/

static void
impl_webkit_show_url( GncHtml* self, URLType type,
                  const gchar* location, const gchar* label,
                  gboolean new_window_hint )
{
	GncHTMLUrlCB url_handler;
	gboolean new_window;
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

	DEBUG(" ");

	if( self == NULL ) return;
	if( location == NULL ) return;

	/* make sure it's OK to show this URL type in this window */
	if( new_window_hint == 0 ) {
		if( priv->base.urltype_cb ) {
			new_window = !((priv->base.urltype_cb)( type ));
		} else {
			new_window = FALSE;
		}
	} else {
		new_window = TRUE;
	}

	if( !new_window ) {
		gnc_html_cancel( GNC_HTML(self) );
	}

	if( gnc_html_url_handlers ) {
		url_handler = g_hash_table_lookup( gnc_html_url_handlers, type );
	} else {
		url_handler = NULL;
	}

	if( url_handler ) {
		GNCURLResult result;
		gboolean ok;

		result.load_to_stream = FALSE;
		result.url_type = type;
		result.location = NULL;
		result.label = NULL;
		result.base_type = URL_TYPE_FILE;
		result.base_location = NULL;
		result.error_message = NULL;

		ok = url_handler( location, label, new_window, &result );
		if( !ok ) {
			if( result.error_message ) {
			  gnc_error_dialog( priv->base.parent, "%s", result.error_message );
			} else {
				/* %s is a URL (some location somewhere). */
				gnc_error_dialog( priv->base.parent, _("There was an error accessing %s."), location );
			}

			if( priv->base.load_cb ) {
				priv->base.load_cb( GNC_HTML(self), result.url_type,
								location, label, priv->base.load_cb_data );
			}
		} else if( result.load_to_stream ) {
			gnc_html_history_node *hnode;
			const char *new_location;
			const char *new_label;

			new_location = result.location ? result.location : location;
			new_label = result.label ? result.label : label;
			hnode = gnc_html_history_node_new( result.url_type, new_location, new_label );

			gnc_html_history_append( priv->base.history, hnode );

			g_free( priv->base.base_location );
			priv->base.base_type = result.base_type;
			priv->base.base_location =
					g_strdup( extract_base_name( result.base_type, new_location ) );
			DEBUG( "resetting base location to %s",
					priv->base.base_location ? priv->base.base_location : "(null)" );

			gnc_html_load_to_stream( GNC_HTML_WEBKIT(self), result.url_type,
									new_location, new_label );

			if( priv->base.load_cb != NULL ) {
				priv->base.load_cb( GNC_HTML(self), result.url_type,
								new_location, new_label, priv->base.load_cb_data );
			}
		}

		g_free( result.location );
		g_free( result.label );
		g_free( result.base_location );
		g_free( result.error_message );

		return;
	}

	if( safe_strcmp( type, URL_TYPE_SCHEME ) == 0 ) {
		gnc_html_open_scm( GNC_HTML_WEBKIT(self), location, label, new_window );

	} else if( safe_strcmp( type, URL_TYPE_JUMP ) == 0 ) {
//		gtk_html_jump_to_anchor( GTK_HTML(priv->html), label );
		g_assert( FALSE );

	} else if( safe_strcmp( type, URL_TYPE_SECURE ) == 0 ||
				safe_strcmp( type, URL_TYPE_HTTP ) == 0 ||
				safe_strcmp( type, URL_TYPE_FILE ) == 0 ) {

		do {
			if( safe_strcmp( type, URL_TYPE_SECURE ) == 0 ) {
				if( !https_allowed() ) {
					gnc_error_dialog( priv->base.parent,
									_("Secure HTTP access is disabled. "
									"You can enable it in the Network section of "
									"the Preferences dialog.") );
					break;
				}
			}

			if( safe_strcmp( type, URL_TYPE_HTTP ) == 0 ) {
				if( !http_allowed() ) {
					gnc_error_dialog( priv->base.parent,
									_("Network HTTP access is disabled. "
									"You can enable it in the Network section of "
									"the Preferences dialog.") );
					break;
				}
			}

			priv->base.base_type = type;

			if( priv->base.base_location != NULL ) g_free( priv->base.base_location );
			priv->base.base_location = extract_base_name( type, location );

			/* FIXME : handle new_window = 1 */
			gnc_html_history_append( priv->base.history,
								gnc_html_history_node_new( type, location, label ) );
			gnc_html_load_to_stream( GNC_HTML_WEBKIT(self), type, location, label );

		} while( FALSE );

	} else {
		PERR( "URLType %s not supported.", type );
	}

	if( priv->base.load_cb != NULL ) {
		(priv->base.load_cb)( GNC_HTML(self), type, location, label, priv->base.load_cb_data );
	}
}


/********************************************************************
 * gnc_html_reload
 * reload the current page
 ********************************************************************/

static void
impl_webkit_reload( GncHtml* self )
{
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
	gnc_html_history_node * n;

	DEBUG(" ");
	n = gnc_html_history_get_current( priv->base.history );
	if( n != NULL ) {
		gnc_html_show_url( self, n->type, n->location, n->label, 0 );
	}
}

#endif
/********************************************************************
 * gnc_html_new
 * create and set up a new webkit widget.
 ********************************************************************/

GncHtml*
gnc_html_gtkmozembed_new( void )
{
	GncHtmlGtkmozembed* self = g_object_new( GNC_TYPE_HTML_GTKMOZEMBED, NULL );
	GncHtmlGtkmozembedPrivate* priv = GNC_HTML_GTKMOZEMBED_GET_PRIVATE(self);

	return GNC_HTML(self);
}

#if 0
/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

static gboolean
webkit_cancel_helper(gpointer key, gpointer value, gpointer user_data)
{
	g_free(key);
	g_list_free((GList *)value);
	return TRUE;
}

static void
impl_webkit_cancel( GncHtml* self )
{
	GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
	/* remove our own references to requests */
	//gnc_http_cancel_requests( priv->http );

	g_hash_table_foreach_remove( priv->base.request_info, webkit_cancel_helper, NULL );
}

static void
impl_webkit_copy( GncHtml* self )
{
	GncHtmlWebkitPrivate* priv;

	g_return_if_fail( self != NULL );

	priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
//	gtk_html_copy( GTK_HTML(priv->html) );
	g_assert( FALSE );
}

/**************************************************************
 * gnc_html_export : wrapper around the builtin function in webkit
 **************************************************************/

static gboolean
raw_html_receiver( gpointer engine,
                   const gchar* data,
                   size_t len,
                   gpointer user_data )
{
	FILE *fh = (FILE *) user_data;
	size_t written;

	do {
		written = fwrite (data, 1, len, fh);
		len -= written;
	} while (len > 0);
	return TRUE;
}

static gboolean
impl_webkit_export( GncHtml* self, const char *filepath )
{
	FILE *fh;
	GncHtmlWebkitPrivate* priv;

	g_return_val_if_fail( self != NULL, FALSE );
	g_return_val_if_fail( filepath != NULL, FALSE );

	priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
	fh = g_fopen( filepath, "w" );
	if( fh == 0 )
		return FALSE;

//	gtk_html_save( GTK_HTML(priv->html), GINT_TO_POINTER(raw_html_receiver), fh );
	g_assert( FALSE );
	fclose (fh);

	return TRUE;
}

#ifdef WEBKIT_USES_GTKPRINT
static void
draw_page_cb(GtkPrintOperation *operation, GtkPrintContext *context,
             gint page_nr, gpointer user_data)
{
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
	GncHtmlWebkitPrivate* priv;

	priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
//    gtk_html_print_page( GTK_HTML(priv->html), context );
	g_assert( FALSE );
}

static void
impl_webkit_print( GncHtml* self )
{
    GtkPrintOperation *print;
    GtkPrintOperationResult res;
	GncHtmlWebkitPrivate* priv;

	priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
    print = gtk_print_operation_new();

    gnc_print_operation_init(print);
    gtk_print_operation_set_use_full_page(print, FALSE);
    gtk_print_operation_set_unit(print, GTK_UNIT_POINTS);
    gtk_print_operation_set_n_pages(print, 1);
    g_signal_connect(print, "draw_page", G_CALLBACK(draw_page_cb), self);

    res = gtk_print_operation_run(print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                  GTK_WINDOW(priv->base.parent), NULL);

    if( res == GTK_PRINT_OPERATION_RESULT_APPLY ) {
        gnc_print_operation_save_print_settings( print );
	}

    g_object_unref(print);
}

#else /* !WEBKIT_USES_GTKPRINT */
#if 0
void
gnc_html_print( GncHtml* html )
{
	PrintSession *ps;

	ps = gnc_print_session_create( FALSE );
	if( ps == NULL ) {
		/* user cancelled */
		return;
	}

//	gtk_html_print( GTK_HTML(html->html), ps->context );
	g_assert( FALSE );
	gnc_print_session_done( ps );
}
#endif
#endif /* WEBKIT_USES_GTKPRINT */

static void
impl_webkit_set_parent( GncHtml* self, GtkWindow* parent )
{
	GncHtmlWebkitPrivate* priv;

	g_return_if_fail( self != NULL );

	priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
	priv->base.parent = GTK_WIDGET(parent);
}

#if 0
const gchar*
gnc_html_get_embedded_param( gpointer eb, const gchar* param_name )
{
	GtkHTMLEmbedded* gtk_eb = (GtkHTMLEmbedded*)eb;

	return (const gchar *)g_hash_table_lookup(gtk_eb->params, param_name);
}
#endif
#endif

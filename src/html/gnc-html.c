/********************************************************************
 * gnc-html.c -- display HTML with some special gnucash tags.       *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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

// libgtkhtml docs:
// http://www.fifi.org/doc/libgtkhtml-dev/html/

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

#include <gtkhtml/gtkhtml.h>
#include <gtkhtml/gtkhtml-embedded.h>

#include "Account.h"
#include "print-session.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-html.h"
#include "gnc-html-history.h"
#include "gnc-html-graph-gog.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
static GHashTable * gnc_html_type_to_proto_hash = NULL;
static GHashTable * gnc_html_proto_to_type_hash = NULL;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes an action name from a FORM definition to a handler function.
 * <form method=METHOD action=gnc-action:ACTION-NAME?ACTION-ARGS>
 * action-args is what gets passed to the handler. */
static GHashTable * gnc_html_action_handlers = NULL;

/* hashes handlers for loading different URLType data */
static GHashTable * gnc_html_stream_handlers = NULL;

/* hashes handlers for handling different URLType data */
GHashTable * gnc_html_url_handlers = NULL;

static char error_404_format[] =
"<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] =
N_("The specified URL could not be loaded.");

G_DEFINE_ABSTRACT_TYPE(GncHtml, gnc_html, GTK_TYPE_BIN)

static void gnc_html_class_init( GncHtmlClass* klass );
static void gnc_html_dispose( GObject* obj );
static void gnc_html_finalize( GObject* obj );

//#define GNC_HTML_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_HTML, GncHtmlPrivate))
#define GNC_HTML_GET_PRIVATE(o) (GNC_HTML(o)->priv)

#include "gnc-html-p.h"

static void
gnc_html_class_init( GncHtmlClass* klass )
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

	gobject_class->dispose = gnc_html_dispose;
	gobject_class->finalize = gnc_html_finalize;

	klass->show_url = NULL;
	klass->show_data = NULL;
	klass->reload = NULL;
	klass->copy = NULL;
	klass->export = NULL;
	klass->print = NULL;
	klass->cancel = NULL;
	klass->parse_url = NULL;
	klass->set_parent = NULL;
}

static void
gnc_html_init( GncHtml* self )
{
	GncHtmlPrivate* priv;
	priv = self->priv = g_new0( GncHtmlPrivate, 1 );

	priv->container = gtk_scrolled_window_new( NULL, NULL );
	gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW(priv->container),
									GTK_POLICY_AUTOMATIC,
									GTK_POLICY_AUTOMATIC );
	priv->request_info = g_hash_table_new( g_str_hash, g_str_equal );
	priv->history = gnc_html_history_new();
}

static void
gnc_html_dispose( GObject* obj )
{
	GncHtml* self = GNC_HTML(obj);
	GncHtmlPrivate* priv = GNC_HTML_GET_PRIVATE(self);

	if( priv->container != NULL ) {
		g_object_unref( G_OBJECT(priv->container) );
		priv->container = NULL;
	}
	if( priv->request_info != NULL ) {
		g_hash_table_destroy( priv->request_info );
		priv->request_info = NULL;
	}
	if( priv->history != NULL ) {
		gnc_html_history_destroy( priv->history );
		priv->history = NULL;
	}

	G_OBJECT_CLASS(gnc_html_parent_class)->dispose( obj );
}

static void
gnc_html_finalize( GObject* obj )
{
	GncHtml* self = GNC_HTML(obj);

	if( self->priv != NULL ) {
		g_free( self->priv );
		self->priv = NULL;
	}

	G_OBJECT_CLASS(gnc_html_parent_class)->finalize( obj );
}

/***********************************************************************************/

/********************************************************************
 * gnc_html_parse_url
 * this takes a URL and determines the protocol type, location, and
 * possible anchor name from the URL.
 ********************************************************************/

URLType
gnc_html_parse_url( GncHtml* self, const gchar* url,
					gchar** url_location, gchar** url_label )
{
	g_return_val_if_fail( GNC_IS_HTML(self), NULL );

	return GNC_HTML_GET_CLASS(self)->parse_url( self, url, url_location, url_label );
}

/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from
 * somewhere.
 ********************************************************************/

void
gnc_html_show_data( GncHtml* self, const gchar* data, int datalen )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->show_data( self, data, datalen );
}


/********************************************************************
 * gnc_html_show_url
 *
 * open a URL.  This is called when the user clicks a link or
 * for the creator of the gnc_html window to explicitly request
 * a URL.
 ********************************************************************/

void
gnc_html_show_url( GncHtml* self, URLType type,
                  const gchar* location, const gchar* label,
                  gboolean new_window_hint )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->show_url( self, type, location, label, new_window_hint );
}


/********************************************************************
 * gnc_html_reload
 * reload the current page
 ********************************************************************/

void
gnc_html_reload( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->reload( self );
}

/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

void
gnc_html_cancel( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->cancel( self );
}


/********************************************************************
 * gnc_html_destroy
 * destroy the struct
 ********************************************************************/

void
gnc_html_destroy( GncHtml* self )
{
	g_object_unref( G_OBJECT(self) );
}

void
gnc_html_set_urltype_cb( GncHtml* self, GncHTMLUrltypeCB urltype_cb )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->urltype_cb = urltype_cb;
}

void
gnc_html_set_load_cb( GncHtml* self, GncHTMLLoadCB load_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->load_cb = load_cb;
	priv->load_cb_data = data;
}

void
gnc_html_set_flyover_cb( GncHtml* self, GncHTMLFlyoverCB flyover_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->flyover_cb = flyover_cb;
	priv->flyover_cb_data = data;
}

void
gnc_html_set_button_cb( GncHtml* self, GncHTMLButtonCB button_cb, gpointer data )
{
	GncHtmlPrivate* priv;
	g_return_if_fail( GNC_IS_HTML(self) );

	priv = GNC_HTML_GET_PRIVATE(self);
	priv->button_cb = button_cb;
	priv->button_cb_data = data;
}

void
gnc_html_copy( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->copy( self );
}

/**************************************************************
 * gnc_html_export : wrapper around the builtin function in gtkhtml
 **************************************************************/

gboolean
gnc_html_export( GncHtml* self, const gchar* filepath )
{
	g_return_val_if_fail( GNC_IS_HTML(self), FALSE );

	return GNC_HTML_GET_CLASS(self)->export( self, filepath );
}

void
gnc_html_print( GncHtml* self )
{
	g_return_if_fail( GNC_IS_HTML(self) );

	GNC_HTML_GET_CLASS(self)->print( self );
}

gnc_html_history *
gnc_html_get_history( GncHtml* self )
{
	g_return_val_if_fail( GNC_IS_HTML(self), NULL );
	return GNC_HTML_GET_PRIVATE(self)->history;
}


GtkWidget *
gnc_html_get_widget( GncHtml* self )
{
	g_return_val_if_fail( GNC_IS_HTML(self), NULL );
	return GNC_HTML_GET_PRIVATE(self)->container;
}

void
gnc_html_set_parent( GncHtml* self, GtkWindow* parent )
{
	g_return_if_fail( GNC_IS_HTML(self) );
	GNC_HTML_GET_CLASS(self)->set_parent( self, parent );
}

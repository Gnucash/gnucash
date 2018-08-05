/********************************************************************
 * gnc-html-webkit.c -- gnucash report renderer using webkit        *
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

#include <config.h>
#include "platform.h"

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

#include <webkit/webkit.h>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-webkit.h"
#include "gnc-html-history.h"
#include "print-session.h"

G_DEFINE_TYPE(GncHtmlWebkit, gnc_html_webkit, GNC_TYPE_HTML )

static void gnc_html_webkit_dispose( GObject* obj );
static void gnc_html_webkit_finalize( GObject* obj );
static void gnc_html_webkit_class_init( GncHtmlWebkitClass* klass );
static void gnc_html_webkit_init( GncHtmlWebkit* gs );

#define GNC_HTML_WEBKIT_GET_PRIVATE(o) (GNC_HTML_WEBKIT(o)->priv)

#include "gnc-html-webkit-p.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
//extern GHashTable* gnc_html_type_to_proto_hash;
//extern GHashTable* gnc_html_proto_to_type_hash;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes handlers for loading different URLType data */
extern GHashTable* gnc_html_stream_handlers;

/* hashes handlers for handling different URLType data */
extern GHashTable* gnc_html_url_handlers;

static char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] = N_("The specified URL could not be loaded.");

#define BASE_URI_NAME "base-uri"
#define GNC_PREF_RPT_DFLT_ZOOM "default-zoom"

static WebKitNavigationResponse webkit_navigation_requested_cb(
    WebKitWebView* web_view,
    WebKitWebFrame* frame,
    WebKitNetworkRequest* request,
    gpointer user_data );
static gboolean webkit_on_load_error (WebKitWebView *web_view,
                                      WebKitWebFrame *web_frame, gchar *uri,
                                      GError *error, gpointer data);
static void webkit_resource_load_error (WebKitWebView *web_view,
                                        WebKitWebFrame *web_frame,
                                        WebKitWebResource *resource,
                                        GError *error, gpointer data);
static void webkit_on_url_cb( WebKitWebView* web_view, gchar* title, gchar* url,
                              gpointer data );
static gchar* handle_embedded_object( GncHtmlWebkit* self, gchar* html_str );
#if 0
static void gnc_html_set_base_cb( GtkHTML* gtkhtml, const gchar* base, gpointer data );
static void gnc_html_link_clicked_cb( GtkHTML* html, const gchar* url, gpointer data );
static gboolean gnc_html_object_requested_cb( GtkHTML* html, GtkHTMLEmbedded* eb,
        gpointer data );
#endif
#if 0 /* Not Used */
static int gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                                     gpointer user_data );
#endif /* Not Used */
static void impl_webkit_show_url( GncHtml* self, URLType type,
                                  const gchar* location, const gchar* label,
                                  gboolean new_window_hint );
static void impl_webkit_show_data( GncHtml* self, const gchar* data, int datalen );
static void impl_webkit_reload( GncHtml* self, gboolean force_rebuild );
static void impl_webkit_copy_to_clipboard( GncHtml* self );
static gboolean impl_webkit_export_to_file( GncHtml* self, const gchar* filepath );
static void impl_webkit_print( GncHtml* self, const gchar* jobname, gboolean export_pdf );
static void impl_webkit_cancel( GncHtml* self );
static void impl_webkit_set_parent( GncHtml* self, GtkWindow* parent );
static void impl_webkit_default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data);

static void
gnc_html_webkit_init( GncHtmlWebkit* self )
{
    GncHtmlWebkitPrivate* priv;
    GncHtmlWebkitPrivate* new_priv;
    GtkStyleContext *stylecontext;
    WebKitWebSettings* webkit_settings = NULL;
    const char* default_font_family = NULL;
    PangoFontDescription *font_desc;
    gdouble zoom = 1.0;

    new_priv = g_realloc( GNC_HTML(self)->priv, sizeof(GncHtmlWebkitPrivate) );
    priv = self->priv = new_priv;
    GNC_HTML(self)->priv = (GncHtmlPrivate*)priv;

    priv->html_string = NULL;
    priv->web_view = WEBKIT_WEB_VIEW(webkit_web_view_new());

    /* Get the default font family from GtkStyleContext of a GtkWidget(priv->web_view). */
    stylecontext = gtk_widget_get_style_context (GTK_WIDGET(priv->web_view));
    gtk_style_context_get (stylecontext, gtk_widget_get_state_flags (GTK_WIDGET(priv->web_view)),
                           "font", &font_desc, NULL);

    default_font_family = pango_font_description_get_family (font_desc);
    pango_font_description_free (font_desc);

    /* Set default webkit settings */
    webkit_settings = webkit_web_view_get_settings (priv->web_view);
    g_object_set (G_OBJECT(webkit_settings), "default-encoding", "utf-8", NULL);
    if (default_font_family == NULL)
    {
        PWARN("webkit_settings: Cannot get default font family.");
    }
    else
    {
        g_object_set (G_OBJECT(webkit_settings),
                      "default-font-family", default_font_family,
                      NULL);
        PINFO("webkit_settings: Set default font to [%s]", default_font_family);
    }
    /* Scale everything up */
    zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM);
    webkit_web_view_set_full_content_zoom (priv->web_view, TRUE);
    webkit_web_view_set_zoom_level (priv->web_view, zoom);


    gtk_container_add( GTK_CONTAINER(priv->base.container),
                       GTK_WIDGET(priv->web_view) );

    g_object_ref_sink( priv->base.container );

    /* signals */
    g_signal_connect( priv->web_view, "navigation-requested",
                      G_CALLBACK(webkit_navigation_requested_cb),
                      self);

    g_signal_connect( priv->web_view, "hovering-over-link",
                      G_CALLBACK(webkit_on_url_cb),
                      self );

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
    g_signal_connect (priv->web_view, "load-error",
                      G_CALLBACK (webkit_on_load_error),
                      self);

    g_signal_connect (priv->web_view, "resource-load-failed",
                      G_CALLBACK (webkit_resource_load_error),
                      self);

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT,
            GNC_PREF_RPT_DFLT_ZOOM,
            impl_webkit_default_zoom_changed,
                           self);

    LEAVE("retval %p", self);
}

static void
gnc_html_webkit_class_init( GncHtmlWebkitClass* klass )
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);
    GncHtmlClass* html_class = GNC_HTML_CLASS(klass);

    gobject_class->dispose = gnc_html_webkit_dispose;
    gobject_class->finalize = gnc_html_webkit_finalize;

    html_class->show_url = impl_webkit_show_url;
    html_class->show_data = impl_webkit_show_data;
    html_class->reload = impl_webkit_reload;
    html_class->copy_to_clipboard = impl_webkit_copy_to_clipboard;
    html_class->export_to_file = impl_webkit_export_to_file;
    html_class->print = impl_webkit_print;
    html_class->cancel = impl_webkit_cancel;
    html_class->set_parent = impl_webkit_set_parent;
}

static void
gnc_html_webkit_dispose( GObject* obj )
{
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(obj);
    GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    if ( priv->web_view != NULL )
    {
        gtk_container_remove( GTK_CONTAINER(priv->base.container),
                              GTK_WIDGET(priv->web_view) );
        priv->web_view = NULL;
    }

    if ( priv->html_string != NULL )
    {
        g_free( priv->html_string );
        priv->html_string = NULL;
    }

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
            GNC_PREF_RPT_DFLT_ZOOM,
            impl_webkit_default_zoom_changed,
                                 obj);

    G_OBJECT_CLASS(gnc_html_webkit_parent_class)->dispose( obj );
}

static void
gnc_html_webkit_finalize( GObject* obj )
{
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(obj);

//	if( self->priv != NULL ) {
//		g_free( self->priv );
    self->priv = NULL;
//	}

    G_OBJECT_CLASS(gnc_html_webkit_parent_class)->finalize( obj );
}

/*****************************************************************************/

static char*
extract_base_name(URLType type, const gchar* path)
{
    gchar       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
    gchar       path_rexp[] = "^/*(.*)/+([^/]*)$";
    regex_t    compiled_m, compiled_p;
    regmatch_t match[4];
    gchar       * machine = NULL, * location = NULL, * base = NULL;
    gchar       * basename = NULL;

    DEBUG(" ");
    if (!path) return NULL;

    regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
    regcomp(&compiled_p, path_rexp, REG_EXTENDED);

    if (!g_strcmp0 (type, URL_TYPE_HTTP) ||
            !g_strcmp0 (type, URL_TYPE_SECURE) ||
            !g_strcmp0 (type, URL_TYPE_FTP))
    {

        /* step 1: split the machine name away from the path
         * components */
        if (!regexec(&compiled_m, path, 4, match, 0))
        {
            /* $1 is the machine name */
            if (match[1].rm_so != -1)
            {
                machine = g_strndup(path + match[1].rm_so,
                                    match[1].rm_eo - match[1].rm_so);
            }
            /* $2 is the path */
            if (match[2].rm_so != -1)
            {
                location = g_strndup(path + match[2].rm_so,
                                     match[2].rm_eo - match[2].rm_so);
            }
        }

    }
    else
    {
        location = g_strdup(path);
    }
    /* step 2: split up the path into prefix and file components */
    if (location)
    {
        if (!regexec(&compiled_p, location, 4, match, 0))
        {
            if (match[1].rm_so != -1)
            {
                base = g_strndup(location + match[1].rm_so,
                                 match[1].rm_eo - match[1].rm_so);
            }
            else
            {
                base = NULL;
            }
        }
    }

    regfree(&compiled_m);
    regfree(&compiled_p);

    if (machine)
    {
        if (base && (strlen(base) > 0))
        {
            basename = g_strconcat(machine, "/", base, "/", NULL);
        }
        else
        {
            basename = g_strconcat(machine, "/", NULL);
        }
    }
    else
    {
        if (base && (strlen(base) > 0))
        {
            basename = g_strdup(base);
        }
        else
        {
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

static gchar*
handle_embedded_object( GncHtmlWebkit* self, gchar* html_str )
{
    // Find the <object> tag and get the classid from it.  This will provide the correct
    // object callback handler.  Pass the <object> entity text to the handler.  What should
    // come back is embedded image information.
    gchar* remainder_str = html_str;
    gchar* object_tag;
    gchar* end_object_tag;
    gchar* object_contents;
    gchar* html_str_start = NULL;
    gchar* html_str_middle;
    gchar* html_str_result = NULL;
    gchar* classid_start;
    gchar* classid_end;
    gchar* classid_str;
    gchar* new_chunk;
    GncHTMLObjectCB h;

    object_tag = g_strstr_len( remainder_str, -1, "<object classid=" );
    while (object_tag)
    {

        classid_start = object_tag + strlen( "<object classid=" ) + 1;
        classid_end = g_strstr_len( classid_start, -1, "\"" );
        classid_str = g_strndup( classid_start, (classid_end - classid_start) );

        end_object_tag = g_strstr_len( object_tag, -1, "</object>" );
        if ( end_object_tag == NULL )
        {
            /*  Hmmm... no object end tag
                Return the original html string because we can't properly parse it */
            g_free (classid_str);
            g_free (html_str_result);
            return g_strdup (html_str);
        }
        end_object_tag += strlen( "</object>" );
        object_contents = g_strndup( object_tag, (end_object_tag - object_tag) );

        h = g_hash_table_lookup( gnc_html_object_handlers, classid_str );
        if ( h != NULL )
        {
            (void)h( GNC_HTML(self), object_contents, &html_str_middle );
        }
        else
        {
            html_str_middle = g_strdup_printf( "No handler found for classid \"%s\"", classid_str );
        }

        html_str_start = html_str_result;
        new_chunk = g_strndup (remainder_str, (object_tag - remainder_str));
        if (!html_str_start)
            html_str_result = g_strconcat (new_chunk, html_str_middle, NULL);
        else
            html_str_result = g_strconcat (html_str_start, new_chunk, html_str_middle, NULL);

        g_free( html_str_start );
        g_free( new_chunk );
        g_free( html_str_middle );

        remainder_str = end_object_tag;
        object_tag = g_strstr_len( remainder_str, -1, "<object classid=" );
    }

    if (html_str_result)
    {
        html_str_start =  html_str_result;
        html_str_result = g_strconcat (html_str_start, remainder_str, NULL);
        g_free (html_str_start);
    }
    else
        html_str_result = g_strdup (remainder_str);

    return html_str_result;
}

/********************************************************************
 * load_to_stream : actually do the work of loading the HTML
 * or binary data referenced by a URL and feeding it into the webkit
 * widget.
 ********************************************************************/

static void
load_to_stream( GncHtmlWebkit* self, URLType type,
                const gchar* location, const gchar* label )
{
    gchar* fdata = NULL;
    int fdata_len = 0;
    GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    DEBUG( "type %s, location %s, label %s", type ? type : "(null)",
           location ? location : "(null)", label ? label : "(null)");

    g_return_if_fail( self != NULL );

    if ( gnc_html_stream_handlers != NULL )
    {
        GncHTMLStreamCB stream_handler;

        stream_handler = g_hash_table_lookup( gnc_html_stream_handlers, type );
        if ( stream_handler )
        {
            gboolean ok = stream_handler( location, &fdata, &fdata_len );

            if ( ok )
            {
                fdata = fdata ? fdata : g_strdup( "" );

                // Until webkitgtk supports download requests, look for "<object classid="
                // indicating the beginning of an embedded graph.  If found, handle it
                if ( g_strstr_len( fdata, -1, "<object classid=" ) != NULL )
                {
                    gchar* new_fdata;
                    new_fdata = handle_embedded_object( self, fdata );
                    g_free( fdata );
                    fdata = new_fdata;
                }

                // Save a copy for export purposes
                if ( priv->html_string != NULL )
                {
                    g_free( priv->html_string );
                }
                priv->html_string = g_strdup( fdata );
                impl_webkit_show_data( GNC_HTML(self), fdata, strlen(fdata) );
//                webkit_web_view_load_html_string( priv->web_view, fdata, BASE_URI_NAME );
            }
            else
            {
                fdata = fdata ? fdata :
                        g_strdup_printf( error_404_format,
                                         _(error_404_title), _(error_404_body) );
                webkit_web_view_load_html_string( priv->web_view, fdata, BASE_URI_NAME );
            }

            g_free( fdata );

            if ( label )
            {
                while ( gtk_events_pending() )
                {
                    gtk_main_iteration();
                }
                /* No action required: Webkit jumps to the anchor on its own. */
            }

            return;
        }
    }

    do
    {
        if ( !g_strcmp0( type, URL_TYPE_SECURE ) ||
                !g_strcmp0( type, URL_TYPE_HTTP ) )
        {

            if ( !g_strcmp0( type, URL_TYPE_SECURE ) )
            {
                if ( !https_allowed() )
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog."));
                    break;
                }
            }

            if ( !http_allowed() )
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Network HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
            }
            else
            {
                gnc_build_url( type, location, label );
            }

        }
        else
        {
            PWARN( "load_to_stream for inappropriate type\n"
                   "\turl = '%s#%s'\n",
                   location ? location : "(null)",
                   label ? label : "(null)" );
            fdata = g_strdup_printf( error_404_format,
                                     _(error_404_title), _(error_404_body) );
            webkit_web_view_load_html_string( priv->web_view, fdata, BASE_URI_NAME );
            g_free( fdata );
        }

    }
    while ( FALSE );
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

static WebKitNavigationResponse
webkit_navigation_requested_cb( WebKitWebView* web_view, WebKitWebFrame* frame,
                                WebKitNetworkRequest* request,
                                gpointer data )
{
    URLType type;
    gchar* location = NULL;
    gchar* label = NULL;
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(data);
    const gchar* url = webkit_network_request_get_uri( request );

    ENTER( "requesting %s", url );
    if ( strcmp( url, BASE_URI_NAME ) == 0 )
    {
        LEAVE("URI is %s", BASE_URI_NAME);
        return WEBKIT_NAVIGATION_RESPONSE_ACCEPT;
    }

    type = gnc_html_parse_url( GNC_HTML(self), url, &location, &label );
    if ( strcmp( type, "file" ) == 0 )
    {
        LEAVE("URI type is 'file'");
        return WEBKIT_NAVIGATION_RESPONSE_ACCEPT;
    }
    gnc_html_show_url( GNC_HTML(self), type, location, label, 0 );
//	load_to_stream( self, type, location, label );
    g_free( location );
    g_free( label );

    LEAVE("");
    return WEBKIT_NAVIGATION_RESPONSE_IGNORE;
}

static gboolean
webkit_on_load_error (WebKitWebView *web_view, WebKitWebFrame *web_frame,
                      gchar *uri, GError *error, gpointer data)
{
     PERR ("WebKit load of %s failed due to %s\n", uri, error->message);
     return FALSE;
}

static void
webkit_resource_load_error (WebKitWebView *web_view, WebKitWebFrame *web_frame,
                            WebKitWebResource *resource, GError *error,
                            gpointer data)
{
     const gchar *uri = webkit_web_resource_get_uri (resource);
     const gchar *type = webkit_web_resource_get_mime_type (resource);
     PERR ("WebKit load of resource %s, type %s, failed due to %s\n",
              uri, type, error->message);
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
    if ( !eb || !(eb->classid) || !gnc_html_object_handlers ) return FALSE;

    h = g_hash_table_lookup( gnc_html_object_handlers, eb->classid );
    if ( h )
    {
        return h( GNC_HTML(self), eb, data );
    }
    else
    {
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
    if ( priv->base.flyover_cb )
    {
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

#if 0 /* Not Used */
static int
gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                          gpointer user_data )
{
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
    GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    DEBUG( "Button Press" );
    if ( priv->base.button_cb != NULL )
    {
        (priv->base.button_cb)( GNC_HTML(self), event, priv->base.button_cb_data );
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}
#endif /* Not Used */

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
    GncHtmlWebkitPrivate* priv;
#define TEMPLATE_REPORT_FILE_NAME "gnc-report-XXXXXX.html"
    int fd;
    gchar* uri;
    gchar *filename;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

    ENTER( "datalen %d, data %20.20s", datalen, data );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    /* Export the HTML to a file and load the file URI.   On Linux, this seems to get around some
       security problems (otherwise, it can complain that embedded images aren't permitted to be
       viewed because they are local resources).  On Windows, this allows the embedded images to
       be viewed (maybe for the same reason as on Linux, but I haven't found where it puts those
       messages. */
    filename = g_build_filename(g_get_tmp_dir(), TEMPLATE_REPORT_FILE_NAME, (gchar *)NULL);
    fd = g_mkstemp( filename );
    impl_webkit_export_to_file( self, filename );
    close( fd );
#ifdef G_OS_WIN32
    uri = g_strdup_printf( "file:///%s", filename );
#else
    uri = g_strdup_printf( "file://%s", filename );
#endif
    g_free(filename);
    DEBUG("Loading uri '%s'", uri);
    webkit_web_view_load_uri( priv->web_view, uri );
    g_free( uri );

    LEAVE("");
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
    GncHtmlWebkitPrivate* priv;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );
    g_return_if_fail( location != NULL );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    /* make sure it's OK to show this URL type in this window */
    if ( new_window_hint == 0 )
    {
        if ( priv->base.urltype_cb )
        {
            new_window = !((priv->base.urltype_cb)( type ));
        }
        else
        {
            new_window = FALSE;
        }
    }
    else
    {
        new_window = TRUE;
    }

    if ( !new_window )
    {
        gnc_html_cancel( GNC_HTML(self) );
    }

    if ( gnc_html_url_handlers )
    {
        url_handler = g_hash_table_lookup( gnc_html_url_handlers, type );
    }
    else
    {
        url_handler = NULL;
    }

    if ( url_handler )
    {
        GNCURLResult result;
        gboolean ok;

        result.load_to_stream = FALSE;
        result.url_type = type;
        result.location = NULL;
        result.label = NULL;
        result.base_type = URL_TYPE_FILE;
        result.base_location = NULL;
        result.error_message = NULL;
        result.parent = GTK_WINDOW (priv->base.parent);

        ok = url_handler( location, label, new_window, &result );
        if ( !ok )
        {
            if ( result.error_message )
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s", result.error_message );
            }
            else
            {
                /* %s is a URL (some location somewhere). */
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), _("There was an error accessing %s."), location );
            }

            if ( priv->base.load_cb )
            {
                priv->base.load_cb( GNC_HTML(self), result.url_type,
                                    location, label, priv->base.load_cb_data );
            }
        }
        else if ( result.load_to_stream )
        {
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

            load_to_stream( GNC_HTML_WEBKIT(self), result.url_type,
                            new_location, new_label );

            if ( priv->base.load_cb != NULL )
            {
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

    if ( g_strcmp0( type, URL_TYPE_SCHEME ) == 0 )
    {
        gnc_html_open_scm( GNC_HTML_WEBKIT(self), location, label, new_window );

    }
    else if ( g_strcmp0( type, URL_TYPE_JUMP ) == 0 )
    {
        /* Webkit jumps to the anchor on its own */
    }
    else if ( g_strcmp0( type, URL_TYPE_SECURE ) == 0 ||
              g_strcmp0( type, URL_TYPE_HTTP ) == 0 ||
              g_strcmp0( type, URL_TYPE_FILE ) == 0 )
    {

        do
        {
            if ( g_strcmp0( type, URL_TYPE_SECURE ) == 0 )
            {
                if ( !https_allowed() )
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog.") );
                    break;
                }
            }

            if ( g_strcmp0( type, URL_TYPE_HTTP ) == 0 )
            {
                if ( !http_allowed() )
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Network HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog.") );
                    break;
                }
            }

            priv->base.base_type = type;

            if ( priv->base.base_location != NULL ) g_free( priv->base.base_location );
            priv->base.base_location = extract_base_name( type, location );

            /* FIXME : handle new_window = 1 */
            gnc_html_history_append( priv->base.history,
                                     gnc_html_history_node_new( type, location, label ) );
            load_to_stream( GNC_HTML_WEBKIT(self), type, location, label );

        }
        while ( FALSE );

    }
    else
    {
        PERR( "URLType %s not supported.", type );
    }

    if ( priv->base.load_cb != NULL )
    {
        (priv->base.load_cb)( GNC_HTML(self), type, location, label, priv->base.load_cb_data );
    }
}


/********************************************************************
 * gnc_html_reload
 * reload the current page
 * if force_rebuild is TRUE, the report is recreated, if FALSE, report
 * is reloaded by webkit
 ********************************************************************/

static void
impl_webkit_reload( GncHtml* self, gboolean force_rebuild )
{
    GncHtmlWebkitPrivate* priv;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    if ( force_rebuild )
    {
        gnc_html_history_node *n = gnc_html_history_get_current( priv->base.history );
        if ( n != NULL )
            gnc_html_show_url( self, n->type, n->location, n->label, 0 );
    }
    else
        webkit_web_view_reload( priv->web_view );
}


/********************************************************************
 * gnc_html_new
 * create and set up a new webkit widget.
 ********************************************************************/

GncHtml*
gnc_html_webkit_new( void )
{
    GncHtmlWebkit* self = g_object_new( GNC_TYPE_HTML_WEBKIT, NULL );
    return GNC_HTML(self);
}

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
    GncHtmlWebkitPrivate* priv;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    /* remove our own references to requests */
    //gnc_http_cancel_requests( priv->http );

    g_hash_table_foreach_remove( priv->base.request_info, webkit_cancel_helper, NULL );
}

static void
impl_webkit_copy_to_clipboard( GncHtml* self )
{
    GncHtmlWebkitPrivate* priv;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
    if ( webkit_web_view_can_copy_clipboard( priv->web_view ) )
    {
        webkit_web_view_copy_clipboard( priv->web_view );
    }
}

/**************************************************************
 * gnc_html_export_to_file
 *
 * @param self GncHtmlWebkit object
 * @param filepath Where to write the HTML
 * @return TRUE if successful, FALSE if unsuccessful
 **************************************************************/
static gboolean
impl_webkit_export_to_file( GncHtml* self, const char *filepath )
{
    FILE *fh;
    GncHtmlWebkitPrivate* priv;

    g_return_val_if_fail( self != NULL, FALSE );
    g_return_val_if_fail( GNC_IS_HTML_WEBKIT(self), FALSE );
    g_return_val_if_fail( filepath != NULL, FALSE );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
    if ( priv->html_string == NULL )
    {
        return FALSE;
    }
    fh = g_fopen( filepath, "w" );
    if ( fh != NULL )
    {
        gint written;
        gint len = strlen( priv->html_string );

        written = fwrite( priv->html_string, 1, len, fh );
        fclose (fh);

        if ( written != len )
        {
            return FALSE;
        }

        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

/**
 * Prints the current page.
 *
 * If printing on WIN32, in order to prevent the font from being tiny, (see bug #591177),
 * A GtkPrintOperation object needs to be created so that the unit can be set, and then
 * webkit_web_frame_print_full() needs to be called to use that GtkPrintOperation.  On
 * other platforms (specifically linux - not sure about MacOSX), the version of webkit may
 * not contain the function webkit_web_frame_print_full(), so webkit_web_frame_print() is
 * called instead (the font size problem doesn't show up on linux).
 *
 * @param self HTML renderer object
 */
static void
impl_webkit_print( GncHtml* self, const gchar* jobname, gboolean export_pdf )
{
    gchar *export_filename = NULL;
    GncHtmlWebkitPrivate* priv;
    WebKitWebFrame* frame;
    GtkPrintOperation* op = gtk_print_operation_new();
    GError* error = NULL;
    GtkPrintSettings *print_settings;

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
    frame = webkit_web_view_get_main_frame( priv->web_view );

    gnc_print_operation_init( op, jobname );
    print_settings = gtk_print_operation_get_print_settings (op);
    if (!print_settings)
    {
        print_settings = gtk_print_settings_new();
        gtk_print_operation_set_print_settings(op, print_settings);
    }
#ifdef G_OS_WIN32
    gtk_print_operation_set_unit( op, GTK_UNIT_POINTS );
#endif

    // Make sure to generate a full export filename
    if (g_str_has_suffix(jobname, ".pdf"))
    {
        export_filename = g_strdup(jobname);
    }
    else
    {
        export_filename = g_strconcat(jobname, ".pdf", NULL);
    }

    // Two different modes of operation. Either export to PDF, or run the
    // normal print dialog
    if (export_pdf)
    {
        GtkWidget *dialog;
        gint result;
        gchar *export_dirname = NULL;
        gchar* basename;

        // Before we save the PDF file, we always ask the user for the export
        // file name. We will store the chosen directory in the gtk print settings
        // as well.
        dialog = gtk_file_chooser_dialog_new (_("Export to PDF File"),
                                              NULL,
                                              GTK_FILE_CHOOSER_ACTION_SAVE,
                                              _("_Cancel"), GTK_RESPONSE_CANCEL,
                                              _("_Save"), GTK_RESPONSE_ACCEPT,
                                              NULL);
        gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);

        // Does the jobname look like a valid full file path?
        basename = g_path_get_basename(jobname);
        if (strcmp(basename, jobname) != 0)
        {
            gchar *tmp_basename;
            gchar *tmp_dirname = g_path_get_dirname(jobname);

            if (g_file_test(tmp_dirname, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
            {
                // Yes, the jobname starts with a directory name that actually
                // exists. Hence we use this as output directory.
                export_dirname = tmp_dirname;
                tmp_dirname = NULL;

                // As the prefix part of the "jobname" is the directory path, we
                // need to extract the suffix part for the filename.
                tmp_basename = g_path_get_basename(export_filename);
                g_free(export_filename);
                export_filename = tmp_basename;
            }
            g_free(tmp_dirname);
        }
        g_free(basename);

        // Set the output file name from the given jobname
        gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER(dialog), export_filename);

        // Do we have a stored output directory?
        if (!export_dirname && gtk_print_settings_has_key(print_settings, GNC_GTK_PRINT_SETTINGS_EXPORT_DIR))
        {
            const char* tmp_dirname = gtk_print_settings_get(print_settings,
                                      GNC_GTK_PRINT_SETTINGS_EXPORT_DIR);
            // Only use the directory subsequently if it exists.
            if (g_file_test(tmp_dirname, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
            {
                export_dirname = g_strdup(tmp_dirname);
            }
        }

        // If we have an already existing directory, propose it now.
        if (export_dirname)
        {
            gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog), export_dirname);
        }
        g_free(export_dirname);

        result = gtk_dialog_run (GTK_DIALOG (dialog));
        // Weird. In gtk_dialog_run, the gtk code will run a fstat() on the
        // proposed new output filename, which of course fails with "file not
        // found" as this file doesn't exist. It will still show a warning output
        // in the trace file, though.

        if (result == GTK_RESPONSE_ACCEPT)
        {
            // The user pressed "Ok", so use the file name for the actual file output.
            gchar *dirname;
            char *tmp = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
            g_free(export_filename);
            export_filename = tmp;

            // Store the directory part of the file for later
            dirname = g_path_get_dirname(export_filename);
            if (g_file_test(dirname, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
            {
                gtk_print_settings_set(print_settings, GNC_GTK_PRINT_SETTINGS_EXPORT_DIR, dirname);
            }
            g_free(dirname);
        }
        gtk_widget_destroy (dialog);

        if (result != GTK_RESPONSE_ACCEPT)
        {
            // User pressed cancel - no saving of the PDF file here.
            g_free(export_filename);
            g_object_unref( op );
            return;
        }

        // This function expects the full filename including (absolute?) path
        gtk_print_operation_set_export_filename(op, export_filename);

        // Run the "Export to PDF" print operation
        webkit_web_frame_print_full( frame, op, GTK_PRINT_OPERATION_ACTION_EXPORT, &error );
    }
    else
    {

        // Also store this export file name as output URI in the settings
        if (gtk_print_settings_has_key(print_settings, GTK_PRINT_SETTINGS_OUTPUT_URI))
        {
            // Get the previous output URI, extract the directory part, and
            // append the current filename.
            const gchar *olduri = gtk_print_settings_get(print_settings, GTK_PRINT_SETTINGS_OUTPUT_URI);
            gchar *dirname = g_path_get_dirname(olduri);
            gchar *newuri = (g_strcmp0(dirname, ".") == 0)
                            ? g_strdup(export_filename)
                            : g_build_filename(dirname, export_filename, NULL);
            //g_warning("olduri=%s newuri=%s", olduri, newuri);

            // This function expects the full filename including protocol, path, and name
            gtk_print_settings_set(print_settings, GTK_PRINT_SETTINGS_OUTPUT_URI, newuri);

            g_free(newuri);
            g_free(dirname);
        }
        else
        {
            // No stored output URI from the print settings, so just set our export filename
            gtk_print_settings_set(print_settings, GTK_PRINT_SETTINGS_OUTPUT_URI, export_filename);
        }

        // Run the normal printing dialog
        webkit_web_frame_print_full( frame, op, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, &error );
    }

    if ( error != NULL )
    {
        GtkWidget* window = gtk_widget_get_toplevel( GTK_WIDGET(priv->web_view) );
        GtkWidget* dialog = gtk_message_dialog_new( gtk_widget_is_toplevel(window) ? GTK_WINDOW(window) : NULL,
                            GTK_DIALOG_DESTROY_WITH_PARENT,
                            GTK_MESSAGE_ERROR,
                            GTK_BUTTONS_CLOSE,
                            "%s", error->message );
        g_error_free( error );

        g_signal_connect( dialog, "response", G_CALLBACK(gtk_widget_destroy), NULL);
        gtk_widget_show( dialog );
    }

    // Remember to save the printing settings after this print job
    gnc_print_operation_save_print_settings(op);
    g_object_unref( op );
    g_free(export_filename);
}

static void
impl_webkit_set_parent( GncHtml* self, GtkWindow* parent )
{
    GncHtmlWebkitPrivate* priv;

    g_return_if_fail( self != NULL );
    g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

    priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
    priv->base.parent = GTK_WIDGET(parent);
}

static void
impl_webkit_default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data)
{
    gdouble zoom = 1.0;
    GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
    GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

    g_return_if_fail(user_data != NULL);

    zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM);
    webkit_web_view_set_zoom_level (priv->web_view, zoom);

}

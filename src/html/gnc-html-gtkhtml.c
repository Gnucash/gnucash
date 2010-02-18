/********************************************************************
 * gnc-html-gtkhtml.c -- display HTML with some special gnucash     *
 *                       tags.                                      *
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
#include "gnc-html-gtkhtml.h"
#include "gnc-html-history.h"
#include "gnc-html-graph-gog-gtkhtml.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"

G_DEFINE_TYPE(GncHtmlGtkhtml, gnc_html_gtkhtml, GNC_TYPE_HTML )

static void gnc_html_gtkhtml_dispose( GObject* obj );
static void gnc_html_gtkhtml_finalize( GObject* obj );
static void gnc_html_gtkhtml_class_init( GncHtmlGtkhtmlClass* klass );
static void gnc_html_gtkhtml_init( GncHtmlGtkhtml* gs );

//#define GNC_HTML_GTKHTML_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_HTML_GTKHTML, GncHtmlGtkhtmlPrivate))
#define GNC_HTML_GTKHTML_GET_PRIVATE(o) (GNC_HTML_GTKHTML(o)->priv)

#include "gnc-html-gtkhtml-p.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
//extern GHashTable* gnc_html_type_to_proto_hash;
extern GHashTable* gnc_html_proto_to_type_hash;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes handlers for loading different URLType data */
extern GHashTable* gnc_html_stream_handlers;

/* hashes handlers for handling different URLType data */
extern GHashTable* gnc_html_url_handlers;

static char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] = N_("The specified URL could not be loaded.");

static void gtkhtml_pre_3_10_1_bug_workaround( GtkHTMLEmbedded* eb );
static void gnc_html_url_requested_cb( GtkHTML* html, gchar* url,
                                       GtkHTMLStream* handle, gpointer data );
static void gnc_html_on_url_cb( GtkHTML* html, const gchar* url, gpointer data );
static void gnc_html_set_base_cb( GtkHTML* gtkhtml, const gchar* base, gpointer data );
static void gnc_html_link_clicked_cb( GtkHTML* html, const gchar* url, gpointer data );
static gboolean gnc_html_object_requested_cb( GtkHTML* html, GtkHTMLEmbedded* eb,
        gpointer data );
static int gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                                     gpointer user_data );
static void impl_gtkhtml_show_url( GncHtml* self, URLType type,
                                   const gchar* location, const gchar* label,
                                   gboolean new_window_hint );
static void impl_gtkhtml_show_data( GncHtml* self, const gchar* data, int datalen );
static void impl_gtkhtml_reload( GncHtml* self );
static void impl_gtkhtml_copy_to_clipboard( GncHtml* self );
static gboolean impl_gtkhtml_export_to_file( GncHtml* self, const gchar* filepath );
static void impl_gtkhtml_print( GncHtml* self );
static void impl_gtkhtml_cancel( GncHtml* self );
static void impl_gtkhtml_set_parent( GncHtml* self, GtkWindow* parent );

static void
gnc_html_gtkhtml_init( GncHtmlGtkhtml* self )
{
    GncHtmlGtkhtmlPrivate* priv;
    GncHtmlGtkhtmlPrivate* new_priv;

    new_priv = g_realloc( GNC_HTML(self)->priv, sizeof(GncHtmlGtkhtmlPrivate) );
    priv = self->priv = new_priv;

    priv->html = gtk_html_new();
    gtk_container_add( GTK_CONTAINER(priv->base.container),
                       GTK_WIDGET(priv->html) );

#ifdef HAVE_GTK_2_10
    g_object_ref_sink( priv->base.container );
#else
    g_object_ref( priv->base.container );
    gtk_object_sink( GTK_OBJECT(priv->base.container) );
#endif

    /* signals */
    g_signal_connect( priv->html, "url_requested",
                      G_CALLBACK(gnc_html_url_requested_cb),
                      self);

    g_signal_connect( priv->html, "on_url",
                      G_CALLBACK(gnc_html_on_url_cb),
                      self );

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

    gtk_html_load_empty(GTK_HTML(priv->html));

    LEAVE("retval %p", self);
}

static void
gnc_html_gtkhtml_class_init( GncHtmlGtkhtmlClass* klass )
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);
    GncHtmlClass* html_class = GNC_HTML_CLASS(klass);

    gobject_class->dispose = gnc_html_gtkhtml_dispose;
    gobject_class->finalize = gnc_html_gtkhtml_finalize;

    html_class->show_url = impl_gtkhtml_show_url;
    html_class->show_data = impl_gtkhtml_show_data;
    html_class->reload = impl_gtkhtml_reload;
    html_class->copy_to_clipboard = impl_gtkhtml_copy_to_clipboard;
    html_class->export_to_file = impl_gtkhtml_export_to_file;
    html_class->print = impl_gtkhtml_print;
    html_class->cancel = impl_gtkhtml_cancel;
    html_class->set_parent = impl_gtkhtml_set_parent;

    // Initialize graphing support
    gnc_html_graph_gog_gtkhtml_init();
}

static void
gnc_html_gtkhtml_dispose( GObject* obj )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(obj);
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    if ( priv->html != NULL )
    {
        g_object_unref( G_OBJECT(priv->html) );
        priv->html = NULL;
    }

    G_OBJECT_CLASS(gnc_html_gtkhtml_parent_class)->dispose( obj );
}

static void
gnc_html_gtkhtml_finalize( GObject* obj )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(obj);

//	if( self->priv != NULL ) {
//		g_free( self->priv );
    self->priv = NULL;
//	}

    G_OBJECT_CLASS(gnc_html_gtkhtml_parent_class)->finalize( obj );
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

    if (!safe_strcmp (type, URL_TYPE_HTTP) ||
            !safe_strcmp (type, URL_TYPE_SECURE) ||
            !safe_strcmp (type, URL_TYPE_FTP))
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

/************************************************************
 * gnc_html_start_request: starts the gnc-http object working on an
 * http/https request.
 ************************************************************/
static void
gnc_html_start_request( GncHtmlGtkhtml* self, gchar * uri, GtkHTMLStream * handle )
{
    GList * handles = NULL;
    gint  need_request = FALSE;
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    /* we want to make a list of handles to fill with this URI.
     * multiple handles with the same URI will all get filled when the
     * request comes in. */
    DEBUG("requesting %s", uri);
    handles = g_hash_table_lookup( priv->base.request_info, uri );
    if ( handles == NULL )
    {
        need_request = TRUE;
    }

    handles = g_list_append( handles, handle );
    g_hash_table_insert( priv->base.request_info, uri, handles );

    if ( need_request )
    {
        g_critical("we've not supported network requests for years");
    }
}

/********************************************************************
 * gnc_html_load_to_stream : actually do the work of loading the HTML
 * or binary data referenced by a URL and feeding it into the GtkHTML
 * widget.
 ********************************************************************/

static void
gnc_html_load_to_stream( GncHtmlGtkhtml* self, GtkHTMLStream* handle,
                         URLType type, const gchar* location,
                         const gchar* label )
{
    gchar* fdata = NULL;
    int fdata_len = 0;
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

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
                gtk_html_write( GTK_HTML(priv->html), handle, fdata, fdata_len );
                gtk_html_end( GTK_HTML(priv->html), handle, GTK_HTML_STREAM_OK );
            }
            else
            {
                fdata = fdata ? fdata :
                        g_strdup_printf( error_404_format,
                                         _(error_404_title), _(error_404_body) );
                gtk_html_write( GTK_HTML(priv->html), handle, fdata, strlen(fdata) );
                gtk_html_end( GTK_HTML(priv->html), handle, GTK_HTML_STREAM_ERROR );
            }

            g_free( fdata );

            if ( label )
            {
                while ( gtk_events_pending() )
                {
                    gtk_main_iteration();
                }
                gtk_html_jump_to_anchor( GTK_HTML(priv->html), label );
            }

            return;
        }
    }

    do
    {
        if ( !safe_strcmp( type, URL_TYPE_SECURE ) ||
                !safe_strcmp( type, URL_TYPE_HTTP ) )
        {

            if ( !safe_strcmp( type, URL_TYPE_SECURE ) )
            {
                if ( !https_allowed() )
                {
                    gnc_error_dialog( priv->base.parent, "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog."));
                    break;
                }
            }

            if ( !http_allowed() )
            {
                gnc_error_dialog( priv->base.parent, "%s",
                                  _("Network HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
            }
            else
            {
                char *fullurl;

                fullurl = gnc_build_url( type, location, label );
                gnc_html_start_request( self, fullurl, handle );
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
            gtk_html_write( GTK_HTML(priv->html), handle, fdata, strlen (fdata) );
            gtk_html_end( GTK_HTML(priv->html), handle, GTK_HTML_STREAM_ERROR );
            g_free( fdata );
        }

    }
    while ( FALSE );
}

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
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(data);

    DEBUG("Clicked %s", url);
    type = gnc_html_parse_url( GNC_HTML(self), url, &location, &label );
    gnc_html_show_url( GNC_HTML(self), type, location, label, 0 );
    g_free( location );
    g_free( label );
}


/********************************************************************
 * gnc_html_url_requested_cb - called when a URL needs to be
 * loaded within the loading of a page (embedded image).
 ********************************************************************/

static void
gnc_html_url_requested_cb( GtkHTML* html, gchar* url,
                           GtkHTMLStream* handle, gpointer data )
{
    URLType type;
    gchar* location = NULL;
    gchar* label = NULL;
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(data);

    DEBUG( "requesting %s", url );
    type = gnc_html_parse_url( GNC_HTML(self), url, &location, &label );
    gnc_html_load_to_stream( self, handle, type, location, label );
    g_free( location );
    g_free( label );
}


/********************************************************************
 * gnc_html_object_requested_cb - called when an applet needs to be
 * loaded.
 ********************************************************************/

static gboolean
gnc_html_object_requested_cb( GtkHTML* html, GtkHTMLEmbedded* eb,
                              gpointer data )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(data);
    GncHTMLObjectCB h;

    DEBUG( " " );
    if ( !eb || !(eb->classid) || !gnc_html_object_handlers ) return FALSE;

    gtkhtml_pre_3_10_1_bug_workaround( eb );
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


/********************************************************************
 * gnc_html_on_url_cb - called when user rolls over html anchor
 ********************************************************************/

static void
gnc_html_on_url_cb( GtkHTML* html, const gchar* url, gpointer data )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(data);
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    DEBUG( "Rollover %s", url ? url : "(null)" );
    g_free( priv->base.current_link );
    priv->base.current_link = g_strdup( url );
    if ( priv->base.flyover_cb )
    {
        (priv->base.flyover_cb)( GNC_HTML(self), url, priv->base.flyover_cb_data );
    }
}


/********************************************************************
 * gnc_html_set_base_cb
 ********************************************************************/

static void
gnc_html_set_base_cb( GtkHTML* gtkhtml, const gchar* base,
                      gpointer data )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(data);
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
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


/********************************************************************
 * gnc_html_button_press_cb
 * mouse button callback (if any)
 ********************************************************************/

static int
gnc_html_button_press_cb( GtkWidget* widg, GdkEventButton* event,
                          gpointer user_data )
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(user_data);
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

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

/********************************************************************
 * gnc_html_open_scm
 * insert some scheme-generated HTML
 ********************************************************************/

static void
gnc_html_open_scm( GncHtmlGtkhtml* self, const gchar * location,
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
impl_gtkhtml_show_data( GncHtml* self, const char * data, int datalen )
{
    GtkHTMLStream * handle;
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    DEBUG( "datalen %d, data %20.20s", datalen, data );
    handle = gtk_html_begin( GTK_HTML(priv->html) );
    gtk_html_write( GTK_HTML(priv->html), handle, data, datalen );
    gtk_html_end( GTK_HTML(priv->html), handle, GTK_HTML_STREAM_OK );
}

/********************************************************************
 * gnc_html_show_url
 *
 * open a URL.  This is called when the user clicks a link or
 * for the creator of the gnc_html window to explicitly request
 * a URL.
 ********************************************************************/

static void
impl_gtkhtml_show_url( GncHtml* self, URLType type,
                       const gchar* location, const gchar* label,
                       gboolean new_window_hint )
{
    GncHTMLUrlCB url_handler;
    GtkHTMLStream * handle;
    gboolean new_window;
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    DEBUG(" ");

    if ( self == NULL ) return;
    if ( location == NULL ) return;

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

        ok = url_handler( location, label, new_window, &result );
        if ( !ok )
        {
            if ( result.error_message )
            {
                gnc_error_dialog( priv->base.parent, "%s", result.error_message );
            }
            else
            {
                /* %s is a URL (some location somewhere). */
                gnc_error_dialog( priv->base.parent, _("There was an error accessing %s."), location );
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
            GtkHTMLStream * stream;

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

            stream = gtk_html_begin( GTK_HTML(priv->html) );
            gnc_html_load_to_stream( GNC_HTML_GTKHTML(self), stream, result.url_type,
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

    if ( safe_strcmp( type, URL_TYPE_SCHEME ) == 0 )
    {
        gnc_html_open_scm( GNC_HTML_GTKHTML(self), location, label, new_window );

    }
    else if ( safe_strcmp( type, URL_TYPE_JUMP ) == 0 )
    {
        gtk_html_jump_to_anchor( GTK_HTML(priv->html), label );

    }
    else if ( safe_strcmp( type, URL_TYPE_SECURE ) == 0 ||
              safe_strcmp( type, URL_TYPE_HTTP ) == 0 ||
              safe_strcmp( type, URL_TYPE_FILE ) == 0 )
    {

        do
        {
            if ( safe_strcmp( type, URL_TYPE_SECURE ) == 0 )
            {
                if ( !https_allowed() )
                {
                    gnc_error_dialog( priv->base.parent, "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog.") );
                    break;
                }
            }

            if ( safe_strcmp( type, URL_TYPE_HTTP ) == 0 )
            {
                if ( !http_allowed() )
                {
                    gnc_error_dialog( priv->base.parent, "%s",
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
            handle = gtk_html_begin( GTK_HTML(priv->html) );
            gnc_html_load_to_stream( GNC_HTML_GTKHTML(self), handle, type, location, label );

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
 ********************************************************************/

static void
impl_gtkhtml_reload( GncHtml* self )
{
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    gnc_html_history_node * n;

    DEBUG(" ");
    n = gnc_html_history_get_current( priv->base.history );
    if ( n != NULL )
    {
        gnc_html_show_url( self, n->type, n->location, n->label, 0 );
    }
}


/********************************************************************
 * gnc_html_gtkhtml_new
 * create and set up a new gtkhtml widget.
 ********************************************************************/

GncHtml*
gnc_html_gtkhtml_new( void )
{
    GncHtmlGtkhtml* self = g_object_new( GNC_TYPE_HTML_GTKHTML, NULL );
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);

    return GNC_HTML(self);
}

/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

static gboolean
gtkhtml_cancel_helper(gpointer key, gpointer value, gpointer user_data)
{
    g_free(key);
    g_list_free((GList *)value);
    return TRUE;
}

static void
impl_gtkhtml_cancel( GncHtml* self )
{
    GncHtmlGtkhtmlPrivate* priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    /* remove our own references to requests */
    //gnc_http_cancel_requests( priv->http );

    g_hash_table_foreach_remove( priv->base.request_info, gtkhtml_cancel_helper, NULL );
}

static void
impl_gtkhtml_copy_to_clipboard( GncHtml* self )
{
    GncHtmlGtkhtmlPrivate* priv;

    g_return_if_fail( self != NULL );

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    gtk_html_copy( GTK_HTML(priv->html) );
}

/**************************************************************
 * gnc_html_export_to_file : wrapper around the builtin function in gtkhtml
 **************************************************************/

static gboolean
raw_html_receiver( gpointer engine,
                   const gchar* data,
                   size_t len,
                   gpointer user_data )
{
    FILE *fh = (FILE *) user_data;
    size_t written;

    do
    {
        written = fwrite (data, 1, len, fh);
        len -= written;
    }
    while (len > 0);
    return TRUE;
}

static gboolean
impl_gtkhtml_export_to_file( GncHtml* self, const char *filepath )
{
    FILE *fh;
    GncHtmlGtkhtmlPrivate* priv;

    g_return_val_if_fail( self != NULL, FALSE );
    g_return_val_if_fail( filepath != NULL, FALSE );

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    fh = g_fopen( filepath, "w" );
    if ( fh == 0 )
        return FALSE;

    gtk_html_save( GTK_HTML(priv->html), GINT_TO_POINTER(raw_html_receiver), fh );
    fclose (fh);

    return TRUE;
}

#ifdef GTKHTML_USES_GTKPRINT
static void
draw_page_cb(GtkPrintOperation *operation, GtkPrintContext *context,
             gint page_nr, gpointer user_data)
{
    GncHtmlGtkhtml* self = GNC_HTML_GTKHTML(user_data);
    GncHtmlGtkhtmlPrivate* priv;

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    gtk_html_print_page( GTK_HTML(priv->html), context );
}

static void
impl_gtkhtml_print( GncHtml* self )
{
    GtkPrintOperation *print;
    GtkPrintOperationResult res;
    GncHtmlGtkhtmlPrivate* priv;

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    print = gtk_print_operation_new();

    gnc_print_operation_init(print);
    gtk_print_operation_set_use_full_page(print, FALSE);
    gtk_print_operation_set_unit(print, GTK_UNIT_POINTS);
    gtk_print_operation_set_n_pages(print, 1);
    g_signal_connect(print, "draw_page", G_CALLBACK(draw_page_cb), self);

    res = gtk_print_operation_run(print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                  GTK_WINDOW(priv->base.parent), NULL);

    if ( res == GTK_PRINT_OPERATION_RESULT_APPLY )
    {
        gnc_print_operation_save_print_settings( print );
    }

    g_object_unref(print);
}

#else /* !GTKHTML_USES_GTKPRINT */
static void
impl_gtkhtml_print( GncHtml* self )
{
    PrintSession *ps;
    GncHtmlGtkhtmlPrivate* priv;

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    ps = gnc_print_session_create( FALSE );
    if ( ps == NULL )
    {
        /* user cancelled */
        return;
    }

    gtk_html_print( GTK_HTML(priv->html), ps->context );
    gnc_print_session_done( ps );
}
#endif /* GTKHTML_USES_GTKPRINT */

static void
impl_gtkhtml_set_parent( GncHtml* self, GtkWindow* parent )
{
    GncHtmlGtkhtmlPrivate* priv;

    g_return_if_fail( self != NULL );

    priv = GNC_HTML_GTKHTML_GET_PRIVATE(self);
    priv->base.parent = GTK_WIDGET(parent);
}

const gchar*
gnc_html_get_embedded_param( gpointer eb, const gchar* param_name )
{
    GtkHTMLEmbedded* gtk_eb = (GtkHTMLEmbedded*)eb;

    return (const gchar *)g_hash_table_lookup(gtk_eb->params, param_name);
}

static void
gtkhtml_pre_3_10_1_bug_workaround(GtkHTMLEmbedded *eb)
{
    /* HACK ALERT! Compensate for bug in gtkhtml < 3.10.1
    	Gtkhtml set the width parameter twice (=width, =height), so both,
    	width (==height) and height (<1) were incorrect. */
    if ( eb->height < 1 )
    {
        eb->height = eb->width;  /* only squares here :( */
    }
}

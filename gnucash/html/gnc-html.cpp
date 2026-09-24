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

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <cstdlib>
#include <cstring>
#include <string>
#include <string_view>
#include <cerrno>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>

#include "Account.h"
#include "print-session.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-history.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes for URLType -> protocol and protocol -> URLType */
static GHashTable * gnc_html_type_to_proto_hash = nullptr;
GHashTable * gnc_html_proto_to_type_hash = nullptr;

/* hashes an HTML <object classid="ID"> classid to a handler function */
GHashTable* gnc_html_object_handlers = nullptr;

/* hashes handlers for loading different URLType data */
GHashTable* gnc_html_stream_handlers = nullptr;

/* hashes handlers for handling different URLType data */
GHashTable* gnc_html_url_handlers = nullptr;

static GMutex report_document_root_lock;
static gchar *report_document_root = nullptr;

static const gchar *
ensure_report_document_root (GError **error)
{
    GError *local_error = nullptr;
    const gchar *root;

    g_mutex_lock (&report_document_root_lock);
    if (!report_document_root)
        report_document_root = g_dir_make_tmp ("gnucash-report-XXXXXX",
                                               &local_error);
    root = report_document_root;
    g_mutex_unlock (&report_document_root_lock);

    if (root)
        return root;

    if (!local_error)
        g_set_error_literal (&local_error, G_FILE_ERROR, G_FILE_ERROR_FAILED,
                             "Unable to create private report directory.");

    if (error)
        g_propagate_error (error, local_error);
    else
        g_clear_error (&local_error);
    return nullptr;
}

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

G_DEFINE_ABSTRACT_TYPE(GncHtml, gnc_html, G_TYPE_OBJECT)

static void gnc_html_dispose( GObject* obj );
static void gnc_html_finalize( GObject* obj );
/*
#define GNC_HTML_GET_PRIVATE(o) \
     ((GncHtmlPrivate*)gnc_html_get_instance_private((GncHtml*)o))
*/
#define GNC_HTML_GET_PRIVATE(o) (GNC_HTML(o)->priv)

#include "gnc-html-p.h"

static void
gnc_html_class_init( GncHtmlClass* klass )
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_html_dispose;
    gobject_class->finalize = gnc_html_finalize;

    klass->show_url = nullptr;
    klass->show_data = nullptr;
    klass->reload = nullptr;
    klass->copy_to_clipboard = nullptr;
    klass->export_to_file = nullptr;
    klass->print = nullptr;
    klass->cancel = nullptr;
    klass->parse_url = nullptr;
    klass->set_parent = nullptr;
}

static void
gnc_html_init( GncHtml* self )
{
    GncHtmlPrivate *priv = self->priv = g_new0( GncHtmlPrivate, 1 );

    /* GncHtml is a controller. A backend must provide the visible widget it
     * owns; creating a default GTK container here would keep the obsolete
     * single-child widget ownership model alive and makes the controller need a
     * display even when only its URL contract is used. */
    priv->container = nullptr;

    priv->request_info = g_hash_table_new( g_str_hash, g_str_equal );
    priv->history = gnc_html_history_new();
}

static void
gnc_html_dispose( GObject* obj )
{
    GncHtml* self = GNC_HTML(obj);
    GncHtmlPrivate* priv = GNC_HTML_GET_PRIVATE(self);

    if ( priv->container != nullptr )
    {
        g_clear_object (&priv->container);
    }
    if ( priv->request_info != nullptr )
    {
        g_hash_table_destroy( priv->request_info );
        priv->request_info = nullptr;
    }
    if ( priv->history != nullptr )
    {
        gnc_html_history_destroy( priv->history );
        priv->history = nullptr;
    }

    G_OBJECT_CLASS(gnc_html_parent_class)->dispose( obj );
}

static void
gnc_html_finalize( GObject* obj )
{
    GncHtml* self = GNC_HTML(obj);

    if ( self->priv != nullptr )
    {
        g_free( self->priv );
        self->priv = nullptr;
    }

    G_OBJECT_CLASS(gnc_html_parent_class)->finalize( obj );
}

/***********************************************************************************/

static char*
extract_machine_name( const gchar* path )
{
    constexpr gchar machine_rexp[] = "^(//[^/]*)/*(.*)?$";
    regex_t compiled_m;
    constexpr size_t MATCH_LEN = 4;
    regmatch_t match[MATCH_LEN];
    gchar* machine = nullptr;

    if ( path == nullptr ) return nullptr;

    regcomp( &compiled_m, machine_rexp, REG_EXTENDED );

    /* step 1: split the machine name away from the path
     * components */
    if ( !regexec( &compiled_m, path, MATCH_LEN, match, 0 ) )
    {
        /* $1 is the machine name */
        if ( match[1].rm_so != -1 )
        {
            machine = g_strndup( path + match[1].rm_so, match[1].rm_eo - match[1].rm_so );
        }
    }
    regfree(&compiled_m);
    return machine;
}

/********************************************************************
 * gnc_html_parse_url
 * this takes a URL and determines the protocol type, location, and
 * possible anchor name from the URL.
 ********************************************************************/

URLType
gnc_html_parse_url( GncHtml* self, const gchar* url,
                    gchar** url_location, gchar** url_label ) noexcept
{
    constexpr gchar uri_rexp[] = "^(([^:][^:]+):)?([^#]+)?(#(.*))?$";
    regex_t compiled;
    constexpr size_t MATCH_LEN = 6;
    regmatch_t match[MATCH_LEN];
    gchar* protocol = nullptr;
    gchar* path = nullptr;
    gchar* label = nullptr;
    bool found_protocol = false;
    bool found_path = false;
    bool found_label = false;
    URLType retval;
    GncHtmlPrivate* priv = GNC_HTML_GET_PRIVATE(self);

    g_return_val_if_fail( self != nullptr, nullptr );
    g_return_val_if_fail( GNC_IS_HTML(self), nullptr );

    DEBUG( "parsing %s, base_location %s",
           url ? url : "(null)",
           self ? (priv->base_location ? priv->base_location
                   : "(null base_location)")
               : "(null html)");

    regcomp( &compiled, uri_rexp, REG_EXTENDED );

    if ( !regexec( &compiled, url, MATCH_LEN, match, 0 ) )
    {
        if ( match[2].rm_so != -1 )
        {
            protocol = g_new0( gchar, match[2].rm_eo - match[2].rm_so + 1 );
            strncpy( protocol, url + match[2].rm_so, match[2].rm_eo - match[2].rm_so );
            protocol[match[2].rm_eo - match[2].rm_so] = 0;
            found_protocol = true;
        }
        if ( match[3].rm_so != -1 )
        {
            path = g_new0( gchar, match[3].rm_eo - match[3].rm_so + 1 );
            strncpy( path, url + match[3].rm_so, match[3].rm_eo - match[3].rm_so );
            path[match[3].rm_eo - match[3].rm_so] = 0;
            found_path = true;
        }
        if ( match[5].rm_so != -1 )
        {
            label = g_new0( gchar, match[5].rm_eo - match[5].rm_so + 1 );
            strncpy( label, url + match[5].rm_so, match[5].rm_eo - match[5].rm_so );
            label[match[5].rm_eo - match[5].rm_so] = 0;
            found_label = true;
        }
    }

    regfree( &compiled );

    if ( found_protocol )
    {
        const gpointer p = g_hash_table_lookup( gnc_html_proto_to_type_hash, protocol );
        retval = static_cast<const char *>(p);
        if ( retval == nullptr )
        {
            PWARN( "unhandled URL type for '%s'", url ? url : "(null)" );
            retval = URL_TYPE_OTHER;
        }
    }
    else if ( found_label && !found_path )
    {
        retval = URL_TYPE_JUMP;
    }
    else
    {
        if ( self )
        {
            retval = priv->base_type;
        }
        else
        {
            retval = URL_TYPE_FILE;
        }
    }

    g_free( protocol );

    if ( !g_strcmp0( retval, URL_TYPE_FILE ) )
    {
        if ( !found_protocol && path && self && priv->base_location )
        {
            if ( g_path_is_absolute( path ) )
            {
                *url_location = g_strdup( path );
            }
            else
            {
                *url_location = g_build_filename( priv->base_location, path, nullptr );
            }
            g_free( path );
        }
        else
        {
            *url_location = g_strdup( path );
            g_free( path );
        }

    }
    else if ( !g_strcmp0( retval, URL_TYPE_JUMP ) )
    {
        *url_location = nullptr;
        g_free( path );

    }
    else
    {
        /* case URL_TYPE_OTHER: */

        if ( !found_protocol && path && self && priv->base_location )
        {
            if ( g_path_is_absolute( path ) )
            {
                *url_location = g_build_filename( extract_machine_name( priv->base_location ),
                                                  path, nullptr );
            }
            else
            {
                *url_location = g_build_filename( priv->base_location, path, nullptr );
            }
            g_free( path );
        }
        else
        {
            *url_location = g_strdup( path );
            g_free( path );
        }
    }

    *url_label = label;
    return retval;
}

/********************************************************************
 * gnc_html_show_data
 * display some HTML that the creator of the gnc-html got from
 * somewhere.
 ********************************************************************/

void
gnc_html_show_data( GncHtml* self, const gchar* data, int datalen ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->show_data != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->show_data( self, data, datalen );
    }
    else
    {
        DEBUG( "'show_data' not implemented" );
    }
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
                   gboolean new_window ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    char* lc_type = g_ascii_strdown (type, -1);

    if ( GNC_HTML_GET_CLASS(self)->show_url != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->show_url( self, lc_type, location, label, new_window );
    }
    else
    {
        DEBUG( "'show_url' not implemented" );
    }

    g_free (lc_type);
}


/********************************************************************
 * gnc_html_reload
 * reload the current page
 * if force_rebuild is TRUE, the report is recreated, if FALSE, report
 * is reloaded ib the view
 ********************************************************************/

void
gnc_html_reload( GncHtml* self, gboolean force_rebuild ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->reload != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->reload( self, force_rebuild );
    }
    else
    {
        DEBUG( "'reload' not implemented" );
    }
}

/********************************************************************
 * gnc_html_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

void
gnc_html_cancel( GncHtml* self ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->cancel != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->cancel( self );
    }
    else
    {
        DEBUG( "'cancel' not implemented" );
    }
}


/********************************************************************
 * gnc_html_destroy
 * destroy the struct
 ********************************************************************/

void
gnc_html_destroy( GncHtml* self ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( g_object_is_floating( G_OBJECT(self) ) )
    {
        (void)g_object_ref_sink( G_OBJECT(self) );
    }

    g_object_unref( G_OBJECT(self) );
}

void
gnc_html_set_urltype_cb( GncHtml* self, GncHTMLUrltypeCB urltype_cb ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    auto priv = GNC_HTML_GET_PRIVATE(self);
    priv->urltype_cb = urltype_cb;
}

void
gnc_html_set_load_cb( GncHtml* self, GncHTMLLoadCB load_cb, gpointer data ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    auto priv = GNC_HTML_GET_PRIVATE(self);
    priv->load_cb = load_cb;
    priv->load_cb_data = data;
}

void
gnc_html_set_flyover_cb( GncHtml* self, GncHTMLFlyoverCB flyover_cb, gpointer data ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    auto priv = GNC_HTML_GET_PRIVATE(self);
    priv->flyover_cb = flyover_cb;
    priv->flyover_cb_data = data;
}

void
gnc_html_set_button_cb( GncHtml* self, GncHTMLButtonCB button_cb, gpointer data ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    auto priv = GNC_HTML_GET_PRIVATE(self);
    priv->button_cb = button_cb;
    priv->button_cb_data = data;
}

void
gnc_html_copy_to_clipboard( GncHtml* self ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->copy_to_clipboard != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->copy_to_clipboard( self );
    }
    else
    {
        DEBUG( "'copy_to_clipboard' not implemented" );
    }
}

/**************************************************************
 * gnc_html_export_to_file : wrapper around the builtin function in gtkhtml
 **************************************************************/

gboolean
gnc_html_export_to_file( GncHtml* self, const gchar* filepath ) noexcept
{
    g_return_val_if_fail( self != nullptr, FALSE );
    g_return_val_if_fail( GNC_IS_HTML(self), FALSE );

    if ( GNC_HTML_GET_CLASS(self)->export_to_file != nullptr )
    {
        return GNC_HTML_GET_CLASS(self)->export_to_file( self, filepath );
    }
    else
    {
        DEBUG( "'export_to_file' not implemented" );
        return FALSE;
    }
}
void
gnc_html_print (GncHtml* self, const char *jobname, gboolean export_pdf) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( jobname != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->print != nullptr )
        GNC_HTML_GET_CLASS(self)->print (self, jobname, export_pdf);
    else
    {
        DEBUG( "'print' not implemented" );
    }
}

gnc_html_history *
gnc_html_get_history( GncHtml* self ) noexcept
{
    g_return_val_if_fail( self != nullptr, nullptr );
    g_return_val_if_fail( GNC_IS_HTML(self), nullptr );

    return GNC_HTML_GET_PRIVATE(self)->history;
}


GtkWidget *
gnc_html_get_widget( GncHtml* self ) noexcept
{
    g_return_val_if_fail( self != nullptr, nullptr );
    g_return_val_if_fail( GNC_IS_HTML(self), nullptr );

    return GNC_HTML_GET_PRIVATE(self)->container;
}


GtkWidget *
gnc_html_get_webview( GncHtml* self ) noexcept
{
    g_return_val_if_fail (self != nullptr, nullptr);
    g_return_val_if_fail (GNC_IS_HTML(self), nullptr);

    /* GncHtml is a controller. Backends own and expose their visible widget
     * directly, so there is no longer a mandatory GtkScrolledWindow layer. */
    return GNC_HTML_GET_PRIVATE(self)->container;
}


void
gnc_html_set_parent( GncHtml* self, GtkWindow* parent ) noexcept
{
    g_return_if_fail( self != nullptr );
    g_return_if_fail( GNC_IS_HTML(self) );

    if ( GNC_HTML_GET_CLASS(self)->set_parent != nullptr )
    {
        GNC_HTML_GET_CLASS(self)->set_parent( self, parent );
    }
    else
    {
        DEBUG( "'set_parent' not implemented" );
    }
}

/* Register the URLType if it doesn't already exist.
 * Returns TRUE if successful, FALSE if the type already exists.
 */
gboolean
gnc_html_register_urltype( URLType type, const char *protocol ) noexcept
{
    if (!protocol) return FALSE;

    if (!gnc_html_type_to_proto_hash)
    {
        gnc_html_type_to_proto_hash = g_hash_table_new (g_str_hash, g_str_equal);
        gnc_html_proto_to_type_hash = g_hash_table_new (g_str_hash, g_str_equal);
    }

    char *lc_type  = g_ascii_strdown (type, -1);
    if (g_hash_table_lookup (gnc_html_type_to_proto_hash, lc_type))
    {
        g_free (lc_type);
        return FALSE;
    }

    char *lc_proto = g_ascii_strdown (protocol, -1);
    g_hash_table_insert (gnc_html_type_to_proto_hash, lc_type, static_cast<gpointer>(lc_proto));
    if (*lc_proto)
        g_hash_table_insert (gnc_html_proto_to_type_hash, static_cast<gpointer>(lc_proto), lc_type);

    return TRUE;
}

gboolean
gnc_html_urltype_is_internal (URLType type) noexcept
{
    return !g_strcmp0 (type, URL_TYPE_REGISTER) ||
           !g_strcmp0 (type, URL_TYPE_ACCTTREE) ||
           !g_strcmp0 (type, URL_TYPE_REPORT) ||
           !g_strcmp0 (type, URL_TYPE_OPTIONS) ||
           !g_strcmp0 (type, URL_TYPE_SCHEME) ||
           !g_strcmp0 (type, URL_TYPE_HELP) ||
           !g_strcmp0 (type, URL_TYPE_XMLDATA) ||
           !g_strcmp0 (type, URL_TYPE_PRICE) ||
           !g_strcmp0 (type, URL_TYPE_BUDGET);
}

gboolean
gnc_html_handle_internal_url (GncHtml *html, const gchar *url,
                              gboolean new_window) noexcept
{
    gchar *location = nullptr;
    gchar *label = nullptr;

    g_return_val_if_fail (html != nullptr, FALSE);
    g_return_val_if_fail (GNC_IS_HTML (html), FALSE);
    g_return_val_if_fail (url != nullptr, FALSE);

    const auto type = gnc_html_parse_url (html, url, &location, &label);
    /* An internal action without a target is malformed. Do not report it as
     * handled because that would silently consume a navigation decision while
     * the backend's show_url implementation rejects the null location. */
    const auto handled = gnc_html_urltype_is_internal (type) && location;
    if (handled)
        gnc_html_show_url (html, type, location, label, new_window);

    g_free (location);
    g_free (label);
    return handled;
}

const gchar *
gnc_html_get_report_document_root (void) noexcept
{
    GError *error = nullptr;
    const gchar *root = ensure_report_document_root (&error);

    if (!root)
    {
        PERR ("Unable to create private report directory: %s",
              error ? error->message : "unknown error");
        g_clear_error (&error);
    }
    return root;
}

gchar *
gnc_html_create_report_document (GError **error) noexcept
{
    const gchar *root = ensure_report_document_root (error);
    gchar *filename;
    gint descriptor;

    if (!root)
        return nullptr;

    filename = g_build_filename (root, "report-XXXXXX", nullptr);
    descriptor = g_mkstemp (filename);
    if (descriptor == -1)
    {
        if (error)
            g_set_error (error, G_FILE_ERROR,
                         g_file_error_from_errno (errno),
                         "Unable to create temporary report document: %s",
                         g_strerror (errno));
        g_free (filename);
        return nullptr;
    }

    if (close (descriptor) == -1)
    {
        const auto saved_errno = errno;

        g_remove (filename);
        if (error)
            g_set_error (error, G_FILE_ERROR,
                         g_file_error_from_errno (saved_errno),
                         "Unable to close temporary report document: %s",
                         g_strerror (saved_errno));
        g_free (filename);
        return nullptr;
    }
    return filename;
}

void
gnc_html_initialize( void ) noexcept{
    static struct
    {
        URLType	type;
        const char *protocol;
    } types[] =
    {
        { URL_TYPE_FILE, "file" },
        { URL_TYPE_JUMP, "" },
        { URL_TYPE_HTTP, "http" },
        { URL_TYPE_FTP, "ftp" },
        { URL_TYPE_SECURE, "https" },
        { URL_TYPE_REGISTER, "gnc-register" },
        { URL_TYPE_ACCTTREE, "gnc-acct-tree" },
        { URL_TYPE_REPORT, "gnc-report" },
        { URL_TYPE_OPTIONS, "gnc-options" },
        { URL_TYPE_SCHEME, "gnc-scm" },
        { URL_TYPE_HELP, "gnc-help" },
        { URL_TYPE_XMLDATA, "gnc-xml" },
        { URL_TYPE_PRICE, "gnc-price" },
        { URL_TYPE_BUDGET, "gnc-budget" },
        { URL_TYPE_OTHER, "" }
    };

    for (const auto& elem : types)
    {
        (void) gnc_html_register_urltype (elem.type, elem.protocol);
    }
}

/**
 * Creates a new HMTL url.
 *
 * @param type URL type
 * @param location URL location
 * @param label URL label (optional)
 * @return Newly created URL.  This string must be freed by the caller.
 */
gchar*
gnc_build_url( URLType type, const gchar* location, const gchar* label ) noexcept
{
    DEBUG(" ");
    char *lc_type = g_ascii_strdown (type, -1);
    const gpointer p = g_hash_table_lookup (gnc_html_type_to_proto_hash, lc_type);
    const char *type_name = static_cast<const char *>(p);
    g_free (static_cast<gpointer>(lc_type));
    if (!type_name)
        type_name = "";

    if (label)
    {
        return g_strdup_printf("%s%s%s#%s", type_name, (*type_name ? ":" : ""),
                               (location ? location : ""),
                               label ? label : "");
    }
    else
    {
        return g_strdup_printf("%s%s%s", type_name, (*type_name ? ":" : ""),
                               (location ? location : ""));
    }
}

void
gnc_html_register_object_handler( const char * classid,
                                  GncHTMLObjectCB hand ) noexcept
{
    g_return_if_fail( classid != nullptr );

    if ( gnc_html_object_handlers == nullptr )
    {
        gnc_html_object_handlers = g_hash_table_new( g_str_hash, g_str_equal );
    }

    gnc_html_unregister_object_handler( classid );
    if ( hand != nullptr )
    {
        gchar *lc_id  = g_ascii_strdown (classid, -1);
        g_hash_table_insert( gnc_html_object_handlers, lc_id,
            reinterpret_cast<gpointer>(hand) );
    }
}

void
gnc_html_unregister_object_handler( const gchar* classid ) noexcept
{
    gchar* keyptr = nullptr;
    gchar* valptr = nullptr;
    gchar* lc_id = g_ascii_strdown (classid, -1);

    if ( g_hash_table_lookup_extended( gnc_html_object_handlers,
                                       lc_id,
                                       reinterpret_cast<gpointer *>(&keyptr),
                                       reinterpret_cast<gpointer *>(&valptr) ) )
    {
        g_hash_table_remove( gnc_html_object_handlers, lc_id );
        g_free( keyptr );
    }
    g_free( lc_id );
}

void
gnc_html_register_stream_handler( URLType url_type, GncHTMLStreamCB hand ) noexcept
{
    g_return_if_fail( url_type != nullptr && *url_type != '\0' );

    if ( gnc_html_stream_handlers == nullptr )
    {
        gnc_html_stream_handlers = g_hash_table_new( g_str_hash, g_str_equal );
    }

    gnc_html_unregister_stream_handler( url_type );
    if ( hand != nullptr )
    {
        char*  lc_type  = g_ascii_strdown (url_type, -1);
        g_hash_table_insert( gnc_html_stream_handlers, lc_type,
            reinterpret_cast<gpointer>(hand) );
    }
}

void
gnc_html_unregister_stream_handler( URLType url_type ) noexcept
{
    char*  lc_type = g_ascii_strdown (url_type, -1);
    g_hash_table_remove( gnc_html_stream_handlers, lc_type );
    g_free(lc_type);
}

void
gnc_html_register_url_handler( URLType url_type, GncHTMLUrlCB hand ) noexcept
{
    g_return_if_fail( url_type != nullptr && *url_type != '\0' );

    if ( gnc_html_url_handlers == nullptr )
    {
        gnc_html_url_handlers = g_hash_table_new( g_str_hash, g_str_equal );
    }

    gnc_html_unregister_url_handler( url_type );
    if ( hand != nullptr )
    {
        char* lc_type = g_ascii_strdown (url_type, -1);
        g_hash_table_insert( gnc_html_url_handlers, lc_type,
            reinterpret_cast<gpointer>(hand) );
    }
}

void
gnc_html_unregister_url_handler( URLType url_type ) noexcept
{
    char* lc_type = g_ascii_strdown (url_type, -1);
    g_hash_table_remove( gnc_html_url_handlers, lc_type );
    g_free(lc_type);
}

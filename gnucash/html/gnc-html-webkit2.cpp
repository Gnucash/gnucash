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

#include <platform.h>
#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define of close in Guile's GnuLib
#endif
#include <libguile.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <cstdlib>
#include <string.h>
#include <cerrno>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>

#include <webkit/webkit.h>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-webkit2.hpp"
#include "gnc-html-history.h"
#include "print-session.h"


G_DEFINE_TYPE(GncHtmlWebkit, gnc_html_webkit, GNC_TYPE_HTML )

static void gnc_html_webkit_dispose( GObject* obj );
static void gnc_html_webkit_finalize( GObject* obj );

#define GNC_HTML_WEBKIT_GET_PRIVATE(o) (GNC_HTML_WEBKIT(o)->priv)

#include "gnc-html-webkit-p.hpp"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

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

static gboolean webkit_decide_policy_cb (WebKitWebView* web_view,
                     WebKitPolicyDecision *decision,
                     WebKitPolicyDecisionType decision_type,
                     gpointer user_data);
static void webkit_mouse_target_cb (WebKitWebView* web_view,
                     WebKitHitTestResult *hit,
                     guint modifiers, gpointer data);
static gboolean webkit_notification_cb (WebKitWebView *web_view,
                     WebKitNotification *note,
                     gpointer user_data);
static gboolean webkit_load_failed_cb (WebKitWebView *web_view,
                     WebKitLoadEvent event,
                     gchar *uri, GError *error,
                     gpointer user_data);
static void webkit_resource_load_started_cb (WebKitWebView *web_view,
                                             WebKitWebResource *resource,
                                             WebKitURIRequest *request,
                                             gpointer data);
static gchar* handle_embedded_object( GncHtmlWebkit* self, gchar* html_str );
static void impl_webkit_show_url( GncHtml* self, URLType type,
                                  const gchar* location, const gchar* label,
                                  gboolean new_window );
static void impl_webkit_show_data( GncHtml* self, const gchar* data, int datalen );
static void impl_webkit_reload( GncHtml* self, gboolean force_rebuild );
static void impl_webkit_copy_to_clipboard( GncHtml* self );
static gboolean impl_webkit_export_to_file( GncHtml* self, const gchar* filepath );
static void impl_webkit_print (GncHtml* self, const gchar* jobname,
                               gboolean export_pdf);
static void impl_webkit_cancel( GncHtml* self );
static void impl_webkit_set_parent( GncHtml* self, GtkWindow* parent );
static void impl_webkit_default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data);

static void
gnc_html_webkit_configure_report_sandbox (void)
{
    static gsize configured = 0;

    if (g_once_init_enter (&configured))
    {
        const gchar *root = gnc_html_get_report_document_root ();

        if (root)
            webkit_web_context_add_path_to_sandbox
                (webkit_web_context_get_default (), root, TRUE);
        g_once_init_leave (&configured, 1);
    }
}

static GtkWidget*
gnc_html_webkit_webview_new (void)
{
     gnc_html_webkit_configure_report_sandbox ();
     GtkWidget *view = webkit_web_view_new ();
     WebKitSettings *webkit_settings = nullptr;
     const char *default_font_family = nullptr;
     const PangoFontDescription *font = pango_context_get_font_description
          (gtk_widget_get_pango_context (view));

     if (font != nullptr)
          default_font_family = pango_font_description_get_family (font);
/* Set default webkit settings */
     webkit_settings = webkit_web_view_get_settings (WEBKIT_WEB_VIEW (view));
     g_object_set (G_OBJECT(webkit_settings),
                   "default-charset", "utf-8",
                   "allow-universal-access-from-file-urls", FALSE,
                   "enable-java", FALSE,
                   "enable-page-cache", FALSE,
                   "enable-site-specific-quirks", FALSE,
                   "enable-developer-extras", FALSE,
                   nullptr);
     if (default_font_family != nullptr)
     {
          g_object_set (G_OBJECT (webkit_settings),
              "default-font-family", default_font_family, nullptr);
     }
     return view;
}

static void
gnc_html_webkit_init( GncHtmlWebkit* self )
{
     const gpointer p = g_realloc (GNC_HTML(self)->priv, sizeof(GncHtmlWebkitPrivate));
     auto new_priv = reinterpret_cast<GncHtmlWebkitPrivate *>(p);
     auto priv = self->priv = new_priv;
     GNC_HTML(self)->priv = (GncHtmlPrivate*)priv;

     priv->html_string = nullptr;
     priv->web_view = WEBKIT_WEB_VIEW (gnc_html_webkit_webview_new ());
     priv->temporary_report = nullptr;
     priv->pending_anchor = nullptr;

     /* GncHtml is a controller. The backend exposes its actual visible widget
      * instead of wrapping it in a legacy container. */
     g_clear_object (&priv->base.container);
     priv->base.container = GTK_WIDGET (g_object_ref_sink (priv->web_view));


     /* Scale everything up */
     gdouble zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT,
                 GNC_PREF_RPT_DFLT_ZOOM);
     webkit_web_view_set_zoom_level (priv->web_view, zoom);


     /* signals */
     g_signal_connect (priv->web_view, "decide-policy",
                       G_CALLBACK (webkit_decide_policy_cb),
                       self);

     g_signal_connect (priv->web_view, "mouse-target-changed",
                       G_CALLBACK (webkit_mouse_target_cb),
                       self);

     g_signal_connect (priv->web_view, "show-notification",
                       G_CALLBACK (webkit_notification_cb),
                       self);

     g_signal_connect (priv->web_view, "load-failed",
                       G_CALLBACK (webkit_load_failed_cb),
                       self);
     g_signal_connect (priv->web_view, "resource-load-started",
                       G_CALLBACK (webkit_resource_load_started_cb),
                       self);
     gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT,
                            GNC_PREF_RPT_DFLT_ZOOM,
                            reinterpret_cast<gpointer>(impl_webkit_default_zoom_changed),
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

     if ( priv->web_view != nullptr )
     {
          priv->web_view = nullptr;
     }

     if ( priv->html_string != nullptr )
     {
          g_free( priv->html_string );
          priv->html_string = nullptr;
     }
     if ( priv->temporary_report != nullptr )
     {
          g_remove (priv->temporary_report);
          g_clear_pointer (&priv->temporary_report, g_free);
     }
     g_clear_pointer (&priv->pending_anchor, g_free);

     gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
                                  GNC_PREF_RPT_DFLT_ZOOM,
                                  reinterpret_cast<gpointer>(impl_webkit_default_zoom_changed),
                                  obj);

     G_OBJECT_CLASS(gnc_html_webkit_parent_class)->dispose( obj );
}

static void
gnc_html_webkit_finalize( GObject* obj )
{
     GncHtmlWebkit* self = GNC_HTML_WEBKIT(obj);

     self->priv = nullptr;

     G_OBJECT_CLASS(gnc_html_webkit_parent_class)->finalize( obj );
}

/*****************************************************************************/

static char*
extract_base_name(URLType type, const gchar* path)
{
     constexpr gchar       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
     constexpr gchar       path_rexp[] = "^/*(.*)/+([^/]*)$";
     regex_t     compiled_m, compiled_p;
     constexpr size_t MATCH_LEN = 4;
     regmatch_t  match[MATCH_LEN];
     gchar       * machine = nullptr, * location = nullptr, * base = nullptr;
     gchar       * basename = nullptr;

     DEBUG(" ");
     if (!path) return nullptr;

     regcomp(&compiled_m, machine_rexp, REG_EXTENDED);
     regcomp(&compiled_p, path_rexp, REG_EXTENDED);

     if (!g_strcmp0 (type, URL_TYPE_HTTP) ||
         !g_strcmp0 (type, URL_TYPE_SECURE) ||
         !g_strcmp0 (type, URL_TYPE_FTP))
     {

          /* step 1: split the machine name away from the path
           * components */
          if (!regexec(&compiled_m, path, MATCH_LEN, match, 0))
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
          }
     }

     regfree(&compiled_m);
     regfree(&compiled_p);

     if (machine)
     {
          if (base && (strlen(base) > 0))
          {
               basename = g_strconcat(machine, "/", base, "/", nullptr);
          }
          else
          {
               basename = g_strconcat(machine, "/", nullptr);
          }
     }
     else
     {
          if (base && (strlen(base) > 0))
          {
               basename = g_strdup(base);
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
     gchar* html_str_start = nullptr;
     gchar* html_str_middle;
     gchar* html_str_result = nullptr;
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
          if ( end_object_tag == nullptr )
          {
               /*  Hmmm... no object end tag
                   Return the original html string because we can't properly parse it */
               g_free (classid_str);
               g_free (html_str_result);
               return g_strdup (html_str);
          }
          end_object_tag += strlen( "</object>" );
          object_contents = g_strndup( object_tag, (end_object_tag - object_tag) );

          const gpointer p = g_hash_table_lookup( gnc_html_object_handlers, classid_str );
          h = reinterpret_cast<GncHTMLObjectCB>(p);
          if ( h != nullptr )
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
               html_str_result = g_strconcat (new_chunk, html_str_middle, nullptr);
          else
               html_str_result = g_strconcat (html_str_start, new_chunk, html_str_middle, nullptr);

          g_free( html_str_start );
          g_free( new_chunk );
          g_free( html_str_middle );

          remainder_str = end_object_tag;
          object_tag = g_strstr_len( remainder_str, -1, "<object classid=" );
     }

     if (html_str_result)
     {
          html_str_start =  html_str_result;
          html_str_result = g_strconcat (html_str_start, remainder_str, nullptr);
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

static gboolean
load_to_stream( GncHtmlWebkit* self, URLType type,
                const gchar* location, const gchar* label )
{
     gchar* fdata = nullptr;
     int fdata_len = 0;
     GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

     DEBUG( "type %s, location %s, label %s", type ? type : "(null)",
            location ? location : "(null)", label ? label : "(null)");

     g_return_val_if_fail( self != nullptr, FALSE );

     if ( gnc_html_stream_handlers != nullptr )
     {
          const gpointer p = g_hash_table_lookup( gnc_html_stream_handlers, type );
          GncHTMLStreamCB stream_handler = reinterpret_cast<GncHTMLStreamCB>(p);
          if ( stream_handler )
          {
              GncHtml *weak_html = GNC_HTML(self);

              g_object_add_weak_pointer(G_OBJECT(self),
                                        (gpointer*)(&weak_html));
              bool ok = stream_handler( location, &fdata, &fdata_len );

              if (!weak_html) // will be nullptr if self has been destroyed
              {
                  g_free (fdata);
                  return FALSE;
              }
              else
              {
                  g_object_remove_weak_pointer(G_OBJECT(self),
                                               (gpointer*)(&weak_html));
              }

               if ( ok )
               {
                    fdata = fdata ? fdata : g_strdup( "" );

                    // Until webkitgtk supports download requests,
                    // look for "<object classid=" indicating the
                    // beginning of an embedded graph.  If found,
                    // handle it
                    if ( g_strstr_len( fdata, -1, "<object classid=" ) != nullptr )
                    {
                         gchar *new_fdata = handle_embedded_object( self, fdata );
                         g_free( fdata );
                         fdata = new_fdata;
                    }

                    // Save a copy for export purposes
                    if ( priv->html_string != nullptr )
                    {
                         g_free( priv->html_string );
                    }
                    priv->html_string = g_strdup( fdata );
                    g_free (priv->pending_anchor);
                    priv->pending_anchor = g_strdup (label);
                    impl_webkit_show_data( GNC_HTML(self), fdata, strlen(fdata) );
               }
               else
               {
                    fdata = fdata ? fdata :
                         g_strdup_printf( error_404_format,
                                          _(error_404_title), _(error_404_body) );
                    webkit_web_view_load_html (priv->web_view, fdata,
                           BASE_URI_NAME);
               }

               g_free( fdata );

               return TRUE;
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
               webkit_web_view_load_html (priv->web_view, fdata, BASE_URI_NAME);
               g_free( fdata );
          }
     }
     while ( false );
     return TRUE;
}

static gboolean
documents_match (const gchar *first, const gchar *second)
{
     gchar *current_document;
     gchar *requested_document;
     gchar *fragment;
     gboolean matches;

     if (!first || !second)
          return FALSE;

     current_document = g_strdup (first);
     requested_document = g_strdup (second);
     fragment = strchr (current_document, '#');
     if (fragment)
          *fragment = '\0';
     fragment = strchr (requested_document, '#');
     if (fragment)
          *fragment = '\0';
     matches = g_strcmp0 (current_document, requested_document) == 0;
     g_free (current_document);
     g_free (requested_document);
     return matches;
}

static gboolean
is_current_document_navigation (WebKitWebView *web_view,
                                GncHtmlWebkitPrivate *priv, const gchar *uri)
{
     const gchar *current_uri = webkit_web_view_get_uri (web_view);
     gchar *report_uri = nullptr;
     gboolean matches = documents_match (current_uri, uri);

     if (!matches && !g_strcmp0 (uri, BASE_URI_NAME))
          matches = TRUE;
     if (!matches && priv->temporary_report)
          report_uri = g_filename_to_uri (priv->temporary_report, nullptr, nullptr);
     if (!matches)
          matches = documents_match (report_uri, uri);
     g_free (report_uri);
     return matches;
}

static gboolean
perform_navigation_policy (WebKitWebView *web_view,
               WebKitNavigationPolicyDecision *decision,
               GncHtml *self, gboolean new_window)
{
     WebKitNavigationAction *action =
      webkit_navigation_policy_decision_get_navigation_action (decision);
     auto req = webkit_navigation_action_get_request (action);
     const gchar *uri = webkit_uri_request_get_uri (req);
     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
     if (gnc_html_handle_internal_url (self, uri, new_window))
     {
          /* GnuCash actions never cross the renderer boundary directly.
           * This is also used for target=_blank, which remains in this
           * controller instead of creating an unmanaged WebKit window. */
     }
     else if (!new_window && is_current_document_navigation (web_view, priv, uri))
     {
          /* Initial loads, reloads, and fragment links within the generated
           * report are safe and remain renderer-native. */
          webkit_policy_decision_use ((WebKitPolicyDecision *)decision);
          return TRUE;
     }
     else
     {
          PWARN ("Blocked report navigation to '%s'", uri ? uri : "(null)");
     }
     webkit_policy_decision_ignore ((WebKitPolicyDecision*)decision);
     return TRUE;
}
static gboolean
webkit_decide_policy_cb (WebKitWebView *web_view,
             WebKitPolicyDecision *decision,
             WebKitPolicyDecisionType decision_type,
             gpointer user_data)
{
/* This turns out to be the signal to intercept for handling a link-click. */
     if (decision_type == WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION ||
         decision_type == WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION)
     {
          return perform_navigation_policy (
              web_view, WEBKIT_NAVIGATION_POLICY_DECISION (decision),
              GNC_HTML (user_data),
              decision_type == WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION);
     }
     webkit_policy_decision_use (decision);
     return TRUE;
}

static void
webkit_mouse_target_cb (WebKitWebView *web_view, WebKitHitTestResult *hit,
            guint modifiers, gpointer user_data)
{
     if (!webkit_hit_test_result_context_is_link (hit))
         return;

     auto self = static_cast<GncHtmlWebkit*>(user_data);
     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
     gchar *uri = g_strdup (webkit_hit_test_result_get_link_uri (hit));
     g_free (priv->base.current_link);
     priv->base.current_link = uri;
     if (priv->base.flyover_cb)
     {
          (priv->base.flyover_cb) (GNC_HTML (self), uri,
                   priv->base.flyover_cb_data);
     }
}

static gboolean
webkit_notification_cb (WebKitWebView* web_view, WebKitNotification *note,
            gpointer user_data)
{
     GncHtmlWebkit *self = (GncHtmlWebkit*)user_data;
     g_return_val_if_fail (self != nullptr, FALSE);
     g_return_val_if_fail (note != nullptr, FALSE);

     auto root = gtk_widget_get_root (GTK_WIDGET (web_view));
     auto top = GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : nullptr;
     auto dialog = gtk_alert_dialog_new ("%s\n%s",
                                         webkit_notification_get_title (note),
                                         webkit_notification_get_body (note));
     gtk_alert_dialog_set_modal (dialog, TRUE);
     gtk_alert_dialog_show (dialog, top);
     g_object_unref (dialog);
     return TRUE;
}

static gboolean
webkit_load_failed_cb (WebKitWebView *web_view, WebKitLoadEvent event,
                       gchar *uri, GError *error, gpointer user_data)
{
     PERR ("WebKit load of %s failed due to %s\n", uri, error->message);
     return FALSE;
}

static void
webkit_resource_load_failed_cb (WebKitWebResource *resource,
                                GError *error,
                                gpointer data)
{
     WebKitURIResponse *response = webkit_web_resource_get_response (resource);
     const gchar * uri = webkit_web_resource_get_uri (resource);
     PERR ("Load of resource at %s failed with error %s and status code %d.\n",
           uri, error->message, webkit_uri_response_get_status_code (response));
}

static void
webkit_resource_load_finished_cb (WebKitWebResource *resource, gpointer data)
{
     DEBUG ("Load of resource %s completed.\n", webkit_web_resource_get_uri(resource));
}

static void
webkit_resource_load_started_cb (WebKitWebView *web_view,
                                 WebKitWebResource *resource,
                                 WebKitURIRequest *request,
                                 gpointer data)
{
     DEBUG ("Load of resource %s begun.\n", webkit_web_resource_get_uri(resource));
     g_signal_connect (resource, "failed",
                       G_CALLBACK (webkit_resource_load_failed_cb),
                       data);
     g_signal_connect (resource, "finished",
                       G_CALLBACK (webkit_resource_load_finished_cb),
                       data);
}

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
     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

     ENTER( "datalen %d, data %20.20s", datalen, data );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
     GError *error = nullptr;
     gchar *filename = gnc_html_create_report_document (&error);

     if (!filename)
     {
          PERR ("Unable to create the temporary report file: %s",
                error ? error->message : "unknown error");
          g_clear_error (&error);
          return;
     }

     g_free (priv->html_string);
     priv->html_string = g_strndup (data, datalen);
     if (!impl_webkit_export_to_file( self, filename ))
     {
          g_remove (filename);
          g_free (filename);
          return;
     }

     if (priv->temporary_report)
          g_remove (priv->temporary_report);
     g_clear_pointer (&priv->temporary_report, g_free);
     priv->temporary_report = filename;
     gchar *uri = g_filename_to_uri (priv->temporary_report, nullptr, &error);
     if (!uri)
     {
          PERR ("Unable to create a URI for the temporary report: %s",
                error->message);
          g_clear_error (&error);
          g_remove (priv->temporary_report);
          g_clear_pointer (&priv->temporary_report, g_free);
          return;
     }

     if (priv->pending_anchor && *priv->pending_anchor)
     {
          gchar *fragment = g_uri_escape_string (priv->pending_anchor, nullptr,
                                                  TRUE);
          gchar *anchored_uri = g_strconcat (uri, "#", fragment, nullptr);

          webkit_web_view_load_uri (priv->web_view, anchored_uri);
          g_free (anchored_uri);
          g_free (fragment);
     }
     else
          webkit_web_view_load_uri (priv->web_view, uri);
     g_clear_pointer (&priv->pending_anchor, g_free);
     DEBUG("Loading uri '%s'", uri);
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
                      gboolean new_window )
{
     GncHTMLUrlCB url_handler = nullptr;
     bool stream_loaded = false;

     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );
     g_return_if_fail( location != nullptr );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

     if ( priv->base.urltype_cb && priv->base.urltype_cb( type ) )
          gnc_html_cancel( GNC_HTML(self) );

     if ( gnc_html_url_handlers )
     {
          const gpointer p = g_hash_table_lookup( gnc_html_url_handlers, type );
          url_handler = reinterpret_cast<GncHTMLUrlCB>(p);
     }

     if ( url_handler )
     {
          GNCURLResult result;

          result.load_to_stream = FALSE;
          result.url_type = type;
          result.location = nullptr;
          result.label = nullptr;
          result.base_type = URL_TYPE_FILE;
          result.base_location = nullptr;
          result.error_message = nullptr;
          result.parent = GTK_WINDOW (priv->base.parent);

          bool ok = url_handler( location, label, new_window, &result );
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
               const char *new_location = result.location ? result.location : location;
               const char *new_label = result.label ? result.label : label;
               auto hnode = gnc_html_history_node_new( result.url_type, new_location, new_label );

               gnc_html_history_append( priv->base.history, hnode );

               g_free( priv->base.base_location );
               priv->base.base_type = result.base_type;
               priv->base.base_location =
                    g_strdup( extract_base_name( result.base_type, new_location ) );
               DEBUG( "resetting base location to %s",
                      priv->base.base_location ? priv->base.base_location : "(null)" );

               stream_loaded = load_to_stream( GNC_HTML_WEBKIT(self),
                                               result.url_type,
                                               new_location, new_label );

               if ( stream_loaded && priv->base.load_cb != nullptr )
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

               if ( priv->base.base_location != nullptr ) g_free( priv->base.base_location );
               priv->base.base_location = extract_base_name( type, location );

               /* FIXME : handle new_window = 1 */
               gnc_html_history_append( priv->base.history,
                                        gnc_html_history_node_new( type, location, label ) );
               stream_loaded = load_to_stream( GNC_HTML_WEBKIT(self),
                                               type, location, label );

          }
          while ( false );
     }
     else
     {
          PERR( "URLType %s not supported.", type );
     }

     if ( stream_loaded && priv->base.load_cb != nullptr )
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
     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

     if ( force_rebuild )
     {
          gnc_html_history_node *n = gnc_html_history_get_current( priv->base.history );
          if ( n != nullptr )
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
gnc_html_webkit_new( void ) noexcept
{
     auto self = static_cast<GncHtmlWebkit*>(g_object_new( GNC_TYPE_HTML_WEBKIT, nullptr ));
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
     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);

     webkit_web_view_stop_loading (priv->web_view);
     g_hash_table_foreach_remove( priv->base.request_info, webkit_cancel_helper, nullptr );
}

static void
impl_webkit_copy_to_clipboard( GncHtml* self )
{
     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
     webkit_web_view_execute_editing_command (priv->web_view,
                          WEBKIT_EDITING_COMMAND_COPY);
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
     g_return_val_if_fail( self != nullptr, FALSE );
     g_return_val_if_fail( GNC_IS_HTML_WEBKIT(self), FALSE );
     g_return_val_if_fail( filepath != nullptr, FALSE );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
     if ( priv->html_string == nullptr )
     {
          return FALSE;
     }
     FILE *fh = g_fopen( filepath, "w" );
     if ( fh != nullptr )
     {
          gint len = strlen( priv->html_string );
          gint written = fwrite( priv->html_string, 1, len, fh );
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

constexpr char webkit_print_request_data_key[] = "gnc-html-webkit-print-request";

struct WebkitPrintRequest
{
    GncHtmlWebkit *backend;
    WebKitWebView *web_view;
    WebKitPrintOperation *operation;
    GtkPrintDialog *dialog;
    GCancellable *cancellable;
    GWeakRef parent;
    GError *error;
};

static void
webkit_print_request_parent_destroyed (gpointer user_data,
                                       G_GNUC_UNUSED GObject *where_parent_was)
{
    auto request = static_cast<WebkitPrintRequest *> (user_data);

    g_cancellable_cancel (request->cancellable);
}

static void
webkit_print_request_free (WebkitPrintRequest *request)
{
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (parent)
    {
        g_object_weak_unref (G_OBJECT (parent),
                             webkit_print_request_parent_destroyed, request);
        g_object_unref (parent);
    }
    if (request->backend &&
        g_object_get_data (G_OBJECT (request->backend),
                           webkit_print_request_data_key) == request)
        g_object_set_data (G_OBJECT (request->backend),
                           webkit_print_request_data_key, nullptr);
    g_weak_ref_clear (&request->parent);
    g_clear_error (&request->error);
    g_clear_object (&request->cancellable);
    g_clear_object (&request->dialog);
    g_clear_object (&request->operation);
    g_clear_object (&request->web_view);
    g_clear_object (&request->backend);
    g_free (request);
}

static void
webkit_print_request_complete (WebkitPrintRequest *request)
{
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (request->error &&
        !g_error_matches (request->error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
    {
        if (parent && !gtk_widget_in_destruction (GTK_WIDGET (parent)))
            gnc_error_dialog (parent, "%s", request->error->message);
        else
            PERR ("Report printing failed: %s", request->error->message);
    }
    g_clear_object (&parent);
    webkit_print_request_free (request);
}

static void
webkit_print_operation_failed (WebKitPrintOperation *operation, GError *error,
                               gpointer user_data)
{
    auto request = static_cast<WebkitPrintRequest *> (user_data);

    g_clear_error (&request->error);
    request->error = g_error_copy (error);
    (void)operation;
}

static void
webkit_print_operation_finished (WebKitPrintOperation *operation,
                                 gpointer user_data)
{
    auto request = static_cast<WebkitPrintRequest *> (user_data);

    webkit_print_request_complete (request);
    (void)operation;
}

static void
webkit_print_request_start_operation (WebkitPrintRequest *request)
{
    g_signal_connect (request->operation, "failed",
                      G_CALLBACK (webkit_print_operation_failed), request);
    g_signal_connect (request->operation, "finished",
                      G_CALLBACK (webkit_print_operation_finished), request);
    webkit_print_operation_print (request->operation);
}

static void
webkit_print_setup_finished (GObject *source, GAsyncResult *result,
                             gpointer user_data)
{
    auto request = static_cast<WebkitPrintRequest *> (user_data);
    GError *error = nullptr;
    auto setup = gtk_print_dialog_setup_finish (GTK_PRINT_DIALOG (source), result,
                                                &error);

    if (!setup)
    {
        if (error)
            request->error = g_error_copy (error);
        g_clear_error (&error);
        webkit_print_request_complete (request);
        return;
    }

    gnc_print_setup_save (setup);
    webkit_print_operation_set_print_settings (
        request->operation, gtk_print_setup_get_print_settings (setup));
    webkit_print_operation_set_page_setup (
        request->operation, gtk_print_setup_get_page_setup (setup));
    gtk_print_setup_unref (setup);
    webkit_print_request_start_operation (request);
}

static WebkitPrintRequest *
webkit_print_request_new (GncHtmlWebkit *self)
{
    auto priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
    auto request = g_new0 (WebkitPrintRequest, 1);
    auto root = gtk_widget_get_root (GTK_WIDGET (priv->web_view));
    auto parent = GTK_IS_WINDOW (root) ? GTK_WINDOW (root) : nullptr;

    request->backend = GNC_HTML_WEBKIT (g_object_ref (self));
    request->web_view = WEBKIT_WEB_VIEW (g_object_ref (priv->web_view));
    request->operation = webkit_print_operation_new (request->web_view);
    request->cancellable = g_cancellable_new ();
    g_weak_ref_init (&request->parent, parent);
    if (parent)
        g_object_weak_ref (G_OBJECT (parent),
                           webkit_print_request_parent_destroyed, request);
    g_object_set_data (G_OBJECT (self), webkit_print_request_data_key, request);
    return request;
}

static void
impl_webkit_print (GncHtml *self, const gchar *jobname, gboolean export_pdf)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WEBKIT (self));

    auto backend = GNC_HTML_WEBKIT (self);
    auto priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
    WebkitPrintRequest *request;

    if (!priv->web_view ||
        g_object_get_data (G_OBJECT (backend), webkit_print_request_data_key))
        return;

    request = webkit_print_request_new (backend);
    if (export_pdf)
    {
        GError *error = nullptr;
        auto print_settings = gtk_print_settings_new ();

        if (!jobname || !*jobname)
        {
            request->error = g_error_new (G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT,
                                          "%s", _("A PDF export needs an output path."));
            g_object_unref (print_settings);
            webkit_print_request_complete (request);
            return;
        }

        gchar *output_uri = g_filename_to_uri (jobname, nullptr, &error);
        if (!output_uri)
        {
            request->error = error;
            g_object_unref (print_settings);
            webkit_print_request_complete (request);
            return;
        }

        gtk_print_settings_set (print_settings, GTK_PRINT_SETTINGS_PRINTER,
                                "Print to File");
        gtk_print_settings_set (print_settings,
                                GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT, "pdf");
        gtk_print_settings_set (print_settings, GTK_PRINT_SETTINGS_OUTPUT_URI,
                                output_uri);
        webkit_print_operation_set_print_settings (request->operation,
                                                    print_settings);
        g_object_unref (print_settings);
        g_free (output_uri);
        webkit_print_request_start_operation (request);
        return;
    }

    {
        auto print_operation = gtk_print_operation_new ();
        auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

        gnc_print_operation_init (print_operation,
                                  jobname && *jobname ? jobname : _("Report"));
        auto settings = gtk_print_operation_get_print_settings (print_operation);
        auto page_setup = gtk_print_operation_get_default_page_setup (print_operation);
        request->dialog = gtk_print_dialog_new ();
        gtk_print_dialog_set_title (request->dialog,
                                    jobname && *jobname ? jobname : _("Print Report"));
        gtk_print_dialog_set_accept_label (request->dialog, _("_Print"));
        gtk_print_dialog_set_modal (request->dialog, TRUE);
        if (settings)
            gtk_print_dialog_set_print_settings (request->dialog, settings);
        if (page_setup)
            gtk_print_dialog_set_page_setup (request->dialog, page_setup);
        gtk_print_dialog_setup (request->dialog, parent, request->cancellable,
                                webkit_print_setup_finished, request);
        g_clear_object (&parent);
        g_object_unref (print_operation);
    }
}
static void
impl_webkit_set_parent( GncHtml* self, GtkWindow* parent )
{
     g_return_if_fail( self != nullptr );
     g_return_if_fail( GNC_IS_HTML_WEBKIT(self) );

     auto priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
     priv->base.parent = GTK_WIDGET(parent);
}

static void
impl_webkit_default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data)
{
     g_return_if_fail(user_data != nullptr);

     GncHtmlWebkit* self = GNC_HTML_WEBKIT(user_data);
     GncHtmlWebkitPrivate* priv = GNC_HTML_WEBKIT_GET_PRIVATE(self);
     gdouble zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM);
     webkit_web_view_set_zoom_level (priv->web_view, zoom);
}

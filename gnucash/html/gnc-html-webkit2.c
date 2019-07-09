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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>

#include <webkit2/webkit2.h>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-webkit.h"
#include "gnc-html-history.h"
#include "print-session.h"
#include "gnc-state.h"
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

static gboolean webkit_decide_policy_cb (WebKitWebView* web_view,
                     WebKitPolicyDecision *decision,
                     WebKitPolicyDecisionType decision_type,
                     gpointer user_data);
static void webkit_mouse_target_cb (WebKitWebView* web_view,
                     WebKitHitTestResult *hit,
                     guint modifiers, gpointer data);
#if WEBKIT_MINOR_VERSION >= 8
static gboolean webkit_notification_cb (WebKitWebView *web_view,
                     WebKitNotification *note,
                     gpointer user_data);
#endif
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
                                  gboolean new_window_hint );
static void impl_webkit_show_data( GncHtml* self, const gchar* data, int datalen );
static void impl_webkit_reload( GncHtml* self, gboolean force_rebuild );
static void impl_webkit_copy_to_clipboard( GncHtml* self );
static gboolean impl_webkit_export_to_file( GncHtml* self, const gchar* filepath );
static void impl_webkit_print (GncHtml* self,const gchar* jobname);
static void impl_webkit_cancel( GncHtml* self );
static void impl_webkit_set_parent( GncHtml* self, GtkWindow* parent );
static void impl_webkit_default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data);

static GtkWidget*
gnc_html_webkit_webview_new (void)
{
     GtkWidget *view = webkit_web_view_new ();
     WebKitSettings *webkit_settings = NULL;
     const char *default_font_family = NULL;
     GtkStyleContext *style = gtk_widget_get_style_context (view);
     GValue val = G_VALUE_INIT;
     GtkStateFlags state = gtk_style_context_get_state (style);
     gtk_style_context_get_property (style, GTK_STYLE_PROPERTY_FONT,
                     state, &val);

     if (G_VALUE_HOLDS_BOXED (&val))
     {
      const PangoFontDescription *font =
           (const PangoFontDescription*)g_value_get_boxed (&val);
      default_font_family = pango_font_description_get_family (font);
      g_value_unset (&val);
     }
/* Set default webkit settings */
     webkit_settings = webkit_web_view_get_settings (WEBKIT_WEB_VIEW (view));
     g_object_set (G_OBJECT(webkit_settings),
                   "default-charset", "utf-8",
#if WEBKIT_MINOR_VERSION >= 10
                   "allow-file-access-from-file-urls", TRUE,
#endif
#if WEBKIT_MINOR_VERSION >= 14
                   "allow-universal-access-from-file-urls", TRUE,
#endif
                   "enable-java", FALSE,
                   "enable-page-cache", FALSE,
                   "enable-plugins", FALSE,
                   "enable-site-specific-quirks", FALSE,
                   "enable-xss-auditor", FALSE,
                   "enable-developer-extras", TRUE,
                   NULL);
     if (default_font_family != NULL)
     {
          g_object_set (G_OBJECT (webkit_settings),
              "default-font-family", default_font_family, NULL);
     }
     return view;
}

static void
gnc_html_webkit_init( GncHtmlWebkit* self )
{
     GncHtmlWebkitPrivate* priv;
     GncHtmlWebkitPrivate* new_priv;
     gdouble zoom = 1.0;

     new_priv = g_realloc (GNC_HTML(self)->priv, sizeof(GncHtmlWebkitPrivate));
     priv = self->priv = new_priv;
     GNC_HTML(self)->priv = (GncHtmlPrivate*)priv;

     priv->html_string = NULL;
     priv->web_view = WEBKIT_WEB_VIEW (gnc_html_webkit_webview_new ());


     /* Scale everything up */
     zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT,
                 GNC_PREF_RPT_DFLT_ZOOM);
     webkit_web_view_set_zoom_level (priv->web_view, zoom);


     gtk_container_add( GTK_CONTAINER(priv->base.container),
                        GTK_WIDGET(priv->web_view) );

     g_object_ref_sink( priv->base.container );

     /* signals */
     g_signal_connect (priv->web_view, "decide-policy",
                       G_CALLBACK (webkit_decide_policy_cb),
                       self);

     g_signal_connect (priv->web_view, "mouse-target-changed",
                       G_CALLBACK (webkit_mouse_target_cb),
                       self);

#if WEBKIT_MINOR_VERSION >= 8
     g_signal_connect (priv->web_view, "show-notification",
                       G_CALLBACK (webkit_notification_cb),
                       self);
#endif

     g_signal_connect (priv->web_view, "load-failed",
                       G_CALLBACK (webkit_load_failed_cb),
                       self);
     g_signal_connect (priv->web_view, "resource-load-started",
                       G_CALLBACK (webkit_resource_load_started_cb),
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
         // In Gtk Version 3.20 they relaxed the fact that the widget should be a
         // direct child of the container otherwise it would be a critical error
#if GTK_CHECK_VERSION(3,20,0)
          gtk_container_remove( GTK_CONTAINER(priv->base.container),
                                GTK_WIDGET(priv->web_view) );
#else
          GtkWidget *parent = gtk_widget_get_parent(GTK_WIDGET(priv->web_view));
          gtk_container_remove( GTK_CONTAINER(priv->base.container), parent);
#endif
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

//      if( self->priv != NULL ) {
//              g_free( self->priv );
     self->priv = NULL;
//      }

     G_OBJECT_CLASS(gnc_html_webkit_parent_class)->finalize( obj );
}

/*****************************************************************************/

static char*
extract_base_name(URLType type, const gchar* path)
{
     gchar       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
     gchar       path_rexp[] = "^/*(.*)/+([^/]*)$";
     regex_t     compiled_m, compiled_p;
     regmatch_t  match[4];
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

                    // Until webkitgtk supports download requests,
                    // look for "<object classid=" indicating the
                    // beginning of an embedded graph.  If found,
                    // handle it
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
//                webkit_web_view_load_html (priv->web_view, fdata,
//                                           BASE_URI_NAME);
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
               webkit_web_view_load_html (priv->web_view, fdata, BASE_URI_NAME);
               g_free( fdata );
          }
     }
     while ( FALSE );
}
static gboolean
perform_navigation_policy (WebKitWebView *web_view,
               WebKitNavigationPolicyDecision *decision,
               GncHtml *self)
{
     WebKitURIRequest *req = NULL;
     const gchar* uri; // Can't init it here.
     gchar *scheme = NULL, *location = NULL, *label = NULL;
     gboolean ignore = FALSE;
#if WEBKIT2_4
     WebKitNavigationAction *action =
      webkit_navigation_policy_decision_get_navigation_action (decision);
     if (webkit_navigation_action_get_navigation_type (action) !=
         WEBKIT_NAVIGATION_TYPE_LINK_CLICKED)
     {
          webkit_policy_decision_use ((WebKitPolicyDecision*)decision);
          return TRUE;
     }
     req = webkit_navigation_action_get_request (action);
#else
     req = webkit_navigation_policy_decision_get_request (decision);
#endif
     uri = webkit_uri_request_get_uri (req);
     scheme =  gnc_html_parse_url (self, uri, &location, &label);
     if (strcmp (scheme, URL_TYPE_FILE) != 0)
     {
          impl_webkit_show_url (self, scheme, location, label, FALSE);
          ignore = TRUE;
     }
     g_free (location);
     g_free (label);
     if (ignore)
          webkit_policy_decision_ignore ((WebKitPolicyDecision*)decision);
     else
          webkit_policy_decision_use ((WebKitPolicyDecision*)decision);
     return TRUE;
}

static gboolean
webkit_decide_policy_cb (WebKitWebView *web_view,
             WebKitPolicyDecision *decision,
             WebKitPolicyDecisionType decision_type,
             gpointer user_data)
{
/* This turns out to be the signal to intercept for handling a link-click. */
     if (decision_type != WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION)
     {
          webkit_policy_decision_use (decision);
          return TRUE;
     }
     return perform_navigation_policy (
      web_view, (WebKitNavigationPolicyDecision*) decision,
      GNC_HTML (user_data));
}

static void
webkit_mouse_target_cb (WebKitWebView *web_view, WebKitHitTestResult *hit,
            guint modifiers, gpointer user_data)
{
     GncHtmlWebkitPrivate* priv;
     GncHtmlWebkit *self = (GncHtmlWebkit*)user_data;
     gchar *uri;

     if (!webkit_hit_test_result_context_is_link (hit))
         return;

     priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
     uri = g_strdup (webkit_hit_test_result_get_link_uri (hit));
     g_free (priv->base.current_link);
     priv->base.current_link = uri;
     if (priv->base.flyover_cb)
     {
          (priv->base.flyover_cb) (GNC_HTML (self), uri,
                   priv->base.flyover_cb_data);
     }
}
#if WEBKIT_MINOR_VERSION >= 8
static gboolean
webkit_notification_cb (WebKitWebView* web_view, WebKitNotification *note,
            gpointer user_data)
{
     GtkWindow *top = NULL;
     GtkWidget *dialog = NULL;
     GncHtmlWebkit *self = (GncHtmlWebkit*)user_data;
     g_return_val_if_fail (self != NULL, FALSE);
     g_return_val_if_fail (note != NULL, FALSE);

     top = GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (web_view)));
     dialog = gtk_message_dialog_new (top, GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_WARNING, GTK_BUTTONS_CLOSE,
                                      "%s\n%s",
                                      webkit_notification_get_title (note),
                                      webkit_notification_get_body (note));
     gtk_dialog_run (GTK_DIALOG (dialog));
     gtk_widget_destroy (dialog);
     return TRUE;
}
#endif

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
     uri = g_strdup_printf( "file://%s", filename );
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

/* The webkit1 comment was
 * If printing on WIN32, in order to prevent the font from being tiny, (see bug
 * #591177), A GtkPrintOperation object needs to be created so that the unit can
 * be set, and then webkit_web_frame_print_full() needs to be called to use that
 * GtkPrintOperation.  On other platforms (specifically linux - not sure about
 * MacOSX), the version of webkit may not contain the function
 * webkit_web_frame_print_full(), so webkit_web_frame_print() is called instead
 * (the font size problem doesn't show up on linux).
 *
 * Webkit2 exposes only a very simple WebKitPrintOperation API. In order to
 * implement the above if it proves still to be necessary we'll have to use
 * GtkPrintOperation instead, passing it the results of
 * webkit_web_view_get_snapshot for each page.
 */
static void
impl_webkit_print (GncHtml* self,const gchar* jobname)
{
     WebKitPrintOperation *op = NULL;
     GtkWindow *top = NULL;
     GncHtmlWebkitPrivate *priv;
     GtkPrintSettings *print_settings = NULL;
     WebKitPrintOperationResponse print_response;
     gchar *export_dirname = NULL;
     gchar *export_filename = NULL;
     gchar* basename = NULL;
     GKeyFile *state_file = gnc_state_get_current();
     
     g_return_if_fail (self != NULL);
     g_return_if_fail (GNC_IS_HTML_WEBKIT (self));
     priv = GNC_HTML_WEBKIT_GET_PRIVATE (self);
     op = webkit_print_operation_new (priv->web_view);
     basename = g_path_get_basename(jobname);
     print_settings = gtk_print_settings_new();
     webkit_print_operation_set_print_settings(op, print_settings);
     export_filename = g_strdup(jobname);
     g_free(basename);
     gtk_print_settings_set(print_settings,
                    GTK_PRINT_SETTINGS_OUTPUT_BASENAME,
                    export_filename);
     webkit_print_operation_set_print_settings(op, print_settings);
     // Open a print dialog
     top = GTK_WINDOW (gtk_widget_get_toplevel (GTK_WIDGET (priv->web_view)));
     print_response = webkit_print_operation_run_dialog (op, top);
     if (print_response == WEBKIT_PRINT_OPERATION_RESPONSE_PRINT)
     {
          // Get the newly updated print settings
          g_object_unref(print_settings);
          print_settings = g_object_ref(webkit_print_operation_get_print_settings(op));
     }
     g_free(export_dirname);
     g_free(export_filename);
     g_object_unref (op);
     g_object_unref (print_settings);
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

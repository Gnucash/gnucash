/********************************************************************
 * gnc-html-wkwebview.mm -- gnucash report renderer using WKWebView  *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
 * Copyright (C) 2026 Christopher Lam                               *
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

/* This backend embeds a WKWebView -- the system WebKit view, the same
 * one Safari uses -- as an NSView subview of a plain GTK widget
 * ("socket") that has been forced to own its own native GdkWindow via
 * gdk_window_ensure_native(). The WKWebView's frame is kept in sync
 * with the socket's GTK allocation, so the browser paints itself as if
 * it were the socket's content. It is the direct macOS counterpart of
 * the WebView2 backend, and is chosen at configure time with
 * -DWITH_WKWEBVIEW=ON; without it macOS keeps using WebKitGTK.
 *
 * WKWebView creation is synchronous, unlike WebView2's, but it cannot
 * happen before the socket is realized (there is no NSView to parent it
 * to until then) while gnc_html_show_url()/gnc_html_show_data() are
 * routinely called synchronously right after construction. Requests
 * that arrive that early are therefore stashed in a
 * gnc::html::PendingNavigation and replayed from the realize handler.
 *
 * Everything reached from AppKit here runs on the GTK/UI thread:
 * WKNavigationDelegate and WKUIDelegate callbacks are delivered on the
 * main thread, which GDK's quartz backend already drives as part of the
 * GLib main loop, so no extra plumbing is required.
 *
 * Deliberate differences from the WebView2 backend:
 *
 * - The file is compiled without ARC. The GObject private struct lives
 *   in g_realloc()ed memory, which ARC is not permitted to manage, so
 *   the Objective-C references it holds are retained and released by
 *   hand -- the same discipline the WebView2 backend applies to its COM
 *   pointers.
 * - "Open Link in New Window" is not removed from the context menu for
 *   report links. macOS exposes no supported way to learn which link a
 *   WKWebView context menu was raised on before the menu is built, and
 *   picking the item is harmless: it is routed through
 *   -webView:createWebViewWithConfiguration:... below, which decodes it
 *   exactly as a plain click and lets show_url() decide, so a report
 *   link simply navigates in place.
 * - Local resources are reached through
 *   -loadFileURL:allowingReadAccessToURL:, the supported API, rather
 *   than through the allowFileAccessFromFileURLs preference, which is
 *   only reachable by key-value coding onto a private WKPreferences
 *   property.
 */

#include <config.h>

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

#include <gtk/gtk.h>
#include <gdk/gdkquartz.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>

#include <unistd.h>

#include <cstring>
#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-webkit.hpp"
#include "gnc-html-history.h"
#include "gnc-html-support.hpp"

G_DEFINE_TYPE(GncHtmlWkwebview, gnc_html_wkwebview, GNC_TYPE_HTML)

static void gnc_html_wkwebview_dispose (GObject* obj);
static void gnc_html_wkwebview_finalize (GObject* obj);

#define GNC_HTML_WKWEBVIEW_GET_PRIVATE(o) (GNC_HTML_WKWEBVIEW(o)->priv)

#include "gnc-html-wkwebview-p.hpp"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes handlers for loading different URLType data */
extern GHashTable* gnc_html_stream_handlers;

/* hashes handlers for handling different URLType data */
extern GHashTable* gnc_html_url_handlers;

static char error_404_title[] = N_("Not found");
static char error_404_body[] = N_("The specified URL could not be loaded.");

#define GNC_PREF_RPT_DFLT_ZOOM "default-zoom"

static void show_url (GncHtml* self, URLType type, const gchar* location,
                      const gchar* label, gboolean new_window);
static void show_data (GncHtml* self, const gchar* data, int datalen);
static void reload (GncHtml* self, gboolean force_rebuild);
static void copy_to_clipboard (GncHtml* self);
static gboolean export_to_file (GncHtml* self, const gchar* filepath);
static void print (GncHtml* self, const gchar* jobname);
static void cancel (GncHtml* self);
static void set_parent (GncHtml* self, GtkWindow* parent);
static void default_zoom_changed (gpointer prefs, gchar* pref, gpointer user_data);
static void load_uri (GncHtmlWkwebview* self, const gchar* uri);
static void load_html_string (GncHtmlWkwebview* self, const gchar* html);
static gboolean load_to_stream (GncHtmlWkwebview* self, URLType type,
                                const gchar* location, const gchar* label);
static void apply_zoom (GncHtmlWkwebviewPrivate* priv);

// *****************************************************************************

static NSString*
to_nsstring (const gchar* text)
{
    return text ? [NSString stringWithUTF8String: text] : @"";
}

static std::string
from_nsstring (NSString* text)
{
    const char* utf8 = text ? [text UTF8String] : nullptr;
    return utf8 ? std::string {utf8} : std::string {};
}

/* Decode a URI the page wants to visit and hand it to show_url() unless
 * it is a plain file: link, which the view may follow itself. Shared by
 * the navigation-policy and new-window paths, which differ only in
 * whether new_window is asserted. */
static gboolean
dispatch_uri (GncHtmlWkwebview* self, NSURL* url, gboolean new_window)
{
    if (!url)
        return FALSE;

    auto uri = from_nsstring ([url absoluteString]);
    if (uri.empty ())
        return FALSE;

    gchar* location = nullptr;
    gchar* label = nullptr;
    URLType scheme = gnc_html_parse_url (GNC_HTML (self), uri.c_str (), &location, &label);

    gboolean handled = FALSE;
    if (new_window || g_strcmp0 (scheme, URL_TYPE_FILE) != 0)
    {
        show_url (GNC_HTML (self), scheme, location, label, new_window);
        handled = TRUE;
    }

    g_free (location);
    g_free (label);
    return handled;
}

// *****************************************************************************

@interface GncWkWebViewDelegate : NSObject <WKNavigationDelegate, WKUIDelegate>
{
    GncHtmlWkwebview* m_html;
}
- (instancetype) initWithHtml: (GncHtmlWkwebview*) html;
@end

@implementation GncWkWebViewDelegate

- (instancetype) initWithHtml: (GncHtmlWkwebview*) html
{
    self = [super init];
    if (self)
        m_html = GNC_HTML_WKWEBVIEW (g_object_ref (G_OBJECT (html)));
    return self;
}

- (void) dealloc
{
    if (m_html)
        g_object_unref (G_OBJECT (m_html));
    [super dealloc];
}

- (void) webView: (WKWebView*) webView
    decidePolicyForNavigationAction: (WKNavigationAction*) navigationAction
                    decisionHandler: (void (^)(WKNavigationActionPolicy)) decisionHandler
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (m_html);

    /* Navigations we kicked off ourselves (loading a report, showing an
     * error page) must not be re-routed through gnc_html_show_url().
     * Only navigations we didn't initiate -- in practice, the user
     * clicking a link inside the rendered report -- are intercepted. */
    if (priv->navigating_internally)
    {
        priv->navigating_internally = FALSE;
        decisionHandler (WKNavigationActionPolicyAllow);
        return;
    }

    if (dispatch_uri (m_html, navigationAction.request.URL, FALSE))
        decisionHandler (WKNavigationActionPolicyCancel);
    else
        decisionHandler (WKNavigationActionPolicyAllow);
}

/* Fires when the user picks "Open Link in New Window" from the context
 * menu, or the page navigates with a new-window target. Returning nil
 * tells WebKit not to create a second view -- which would be a bare
 * WKWebView that can do nothing useful with our gnc-register:,
 * gnc-report: and friends -- and we decode the URI ourselves with
 * new_window asserted, so link types that can honor it (account and
 * register links) open a real GnuCash window as the user asked. */
- (WKWebView*) webView: (WKWebView*) webView
    createWebViewWithConfiguration: (WKWebViewConfiguration*) configuration
               forNavigationAction: (WKNavigationAction*) navigationAction
                    windowFeatures: (WKWindowFeatures*) windowFeatures
{
    dispatch_uri (m_html, navigationAction.request.URL, TRUE);
    return nil;
}

- (void) webView: (WKWebView*) webView
    didFailNavigation: (WKNavigation*) navigation
            withError: (NSError*) error
{
    PWARN ("WKWebView navigation failed: %s",
           from_nsstring ([error localizedDescription]).c_str ());
}

- (void) webView: (WKWebView*) webView
    didFailProvisionalNavigation: (WKNavigation*) navigation
                       withError: (NSError*) error
{
    /* NSURLErrorCancelled is what a navigation we cancelled ourselves in
     * -decidePolicyForNavigationAction: reports; it is not a failure. */
    if ([[error domain] isEqualToString: NSURLErrorDomain] &&
        [error code] == NSURLErrorCancelled)
        return;

    PWARN ("WKWebView could not start navigation: %s",
           from_nsstring ([error localizedDescription]).c_str ());
}

@end

// *****************************************************************************

/* The font GTK would have drawn this widget's text in, so reports look
 * like the rest of the application rather than like a web page. The
 * WebKitGTK backend gets this from the default-font-family setting;
 * WKPreferences has no equivalent, so it is injected as a user
 * stylesheet on the document element, where any rule the report sets
 * for its own elements still wins. */
static WKUserScript*
default_font_script (GtkWidget* widget)
{
    GtkStyleContext* style = gtk_widget_get_style_context (widget);
    GValue val = G_VALUE_INIT;
    gtk_style_context_get_property (style, GTK_STYLE_PROPERTY_FONT,
                                    gtk_style_context_get_state (style), &val);

    const char* family = nullptr;
    if (G_VALUE_HOLDS_BOXED (&val))
    {
        auto font = static_cast<const PangoFontDescription*> (g_value_get_boxed (&val));
        if (font)
            family = pango_font_description_get_family (font);
    }

    WKUserScript* script = nil;
    if (family && *family)
    {
        /* The family name is interpolated into a JavaScript string
         * literal, so quotes and backslashes in it have to go. Pango
         * family names are things like "Helvetica Neue", but a user
         * font setting is not something to trust blindly. */
        gchar* escaped = g_strescape (family, nullptr);
        auto source = std::format (
            "(function(){{var s=document.createElement('style');"
            "s.textContent=':root{{font-family:\"{}\";}}';"
            "(document.head||document.documentElement).appendChild(s);}})();",
            escaped);
        g_free (escaped);

        script = [[[WKUserScript alloc]
                      initWithSource: to_nsstring (source.c_str ())
                       injectionTime: WKUserScriptInjectionTimeAtDocumentStart
                    forMainFrameOnly: YES] autorelease];
    }

    g_value_unset (&val);
    return script;
}

static void
apply_zoom (GncHtmlWkwebviewPrivate* priv)
{
    if (!priv->web_view)
        return;

    gdouble zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT,
                                        GNC_PREF_RPT_DFLT_ZOOM);
    if (zoom <= 0.0)
        zoom = 1.0;

    if (@available (macOS 11.0, *))
        [priv->web_view setPageZoom: zoom];
    else
        [priv->web_view setMagnification: zoom];
}

static void
flush_pending (GncHtmlWkwebview* self)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);
    auto request = priv->pending->take ();

    if (auto* uri = std::get_if<gnc::html::UriRequest> (&request))
        load_uri (self, uri->uri.c_str ());
    else if (auto* html = std::get_if<gnc::html::HtmlRequest> (&request))
        load_html_string (self, html->html.c_str ());
}

static void
socket_realize_cb (GtkWidget* socket, gpointer user_data)
{
    auto self = GNC_HTML_WKWEBVIEW (user_data);
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (priv->web_view || priv->disposed)
        return;

    GdkWindow* gdk_window = gtk_widget_get_window (socket);
    if (!gdk_window)
    {
        PERR ("The WKWebView socket widget was realized without a GdkWindow.");
        return;
    }

    /* Without its own native window the socket shares the toplevel's
     * NSView, and the WKWebView would cover the whole window. */
    gdk_window_ensure_native (gdk_window);

    NSView* parent_view = gdk_quartz_window_get_nsview (gdk_window);
    if (!parent_view)
    {
        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                          _("Reports cannot be displayed: this GnuCash was built to "
                            "use the macOS WKWebView, which needs a GTK built against "
                            "the Quartz backend."));
        return;
    }

    GtkAllocation alloc;
    gtk_widget_get_allocation (socket, &alloc);

    auto config = [[[WKWebViewConfiguration alloc] init] autorelease];
    [config setSuppressesIncrementalRendering: NO];
    [[config preferences] setJavaScriptCanOpenWindowsAutomatically: NO];
    if (@available (macOS 11.0, *))
        [[config defaultWebpagePreferences] setAllowsContentJavaScript: YES];

    if (WKUserScript* font_script = default_font_script (socket))
        [[config userContentController] addUserScript: font_script];

    priv->web_view = [[WKWebView alloc]
                         initWithFrame: NSMakeRect (0, 0, alloc.width, alloc.height)
                         configuration: config];
    [priv->web_view setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
    [priv->web_view setHidden: !gtk_widget_get_mapped (socket)];

    priv->delegate = [[GncWkWebViewDelegate alloc] initWithHtml: self];
    [priv->web_view setNavigationDelegate: priv->delegate];
    [priv->web_view setUIDelegate: priv->delegate];

    [parent_view addSubview: priv->web_view];

    apply_zoom (priv);
    flush_pending (self);
}

static void
socket_size_allocate_cb (GtkWidget* socket, GtkAllocation* allocation, gpointer user_data)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (GNC_HTML_WKWEBVIEW (user_data));
    if (priv->web_view)
        [priv->web_view setFrame: NSMakeRect (0, 0, allocation->width, allocation->height)];
}

static void
socket_map_cb (GtkWidget* socket, gpointer user_data)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (GNC_HTML_WKWEBVIEW (user_data));
    if (priv->web_view)
        [priv->web_view setHidden: NO];
}

static void
socket_unmap_cb (GtkWidget* socket, gpointer user_data)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (GNC_HTML_WKWEBVIEW (user_data));
    if (priv->web_view)
        [priv->web_view setHidden: YES];
}

// *****************************************************************************

static void
load_uri (GncHtmlWkwebview* self, const gchar* uri)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (!uri)
        return;

    if (!priv->web_view)
    {
        priv->pending->set_uri (uri);
        return;
    }

    NSURL* url = [NSURL URLWithString: to_nsstring (uri)];
    if (!url)
    {
        PERR ("Could not parse '%s' as a URL.", uri);
        return;
    }

    priv->navigating_internally = TRUE;

    if ([[url scheme] isEqualToString: @"file"])
    {
        /* WKWebView will only read local files below the directory it
         * is explicitly granted. A report lives in a temporary file but
         * refers to images and stylesheets installed with GnuCash, so
         * the grant has to cover both -- which is the filesystem root.
         * The WebKitGTK backend is no narrower: it turns on
         * allow-file-access-from-file-urls and
         * allow-universal-access-from-file-urls. */
        [priv->web_view loadFileURL: url
            allowingReadAccessToURL: [NSURL fileURLWithPath: @"/" isDirectory: YES]];
    }
    else
        [priv->web_view loadRequest: [NSURLRequest requestWithURL: url]];
}

static void
load_html_string (GncHtmlWkwebview* self, const gchar* html)
{
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (!html)
        return;

    if (!priv->web_view)
    {
        priv->pending->set_html (html);
        return;
    }

    priv->navigating_internally = TRUE;
    [priv->web_view loadHTMLString: to_nsstring (html) baseURL: nil];
}

static void
show_error_page (GncHtmlWkwebview* self)
{
    auto page = gnc::html::make_error_page (_(error_404_title), _(error_404_body));
    load_html_string (self, page.c_str ());
}

// *****************************************************************************

static gboolean
http_allowed ()
{
    return TRUE;
}

static gboolean
https_allowed ()
{
    return TRUE;
}

/* Replace each <object classid="..."> in the report with whatever the
 * handler registered for that classid produces -- historically an
 * embedded chart image. No handler is registered anywhere in the tree
 * today, so in practice this only ever reports the missing handler; the
 * mechanism is kept because the other two backends keep it. */
static gchar*
handle_embedded_objects (GncHtmlWkwebview* self, const gchar* html_str)
{
    auto rewritten = gnc::html::rewrite_embedded_objects (
        html_str,
        [self] (std::string_view classid, std::string_view element) -> std::string
        {
            std::string classid_str {classid};
            const gpointer p = g_hash_table_lookup (gnc_html_object_handlers,
                                                    classid_str.c_str ());
            auto handler = reinterpret_cast<GncHTMLObjectCB> (p);
            if (!handler)
                return std::format ("No handler found for classid \"{}\"", classid_str);

            /* The callback takes the element text as a mutable buffer
             * and returns the replacement through its third argument. */
            gchar* element_copy = g_strndup (element.data (), element.size ());
            gchar* replacement = nullptr;
            (void) handler (GNC_HTML (self), element_copy, &replacement);
            g_free (element_copy);

            std::string result = replacement ? replacement : "";
            g_free (replacement);
            return result;
        });

    if (!rewritten)
    {
        /* Malformed markup: render it as it came, exactly as the other
         * backends do rather than showing a half-rewritten document. */
        PWARN ("Report HTML has an unterminated <object> element; "
               "displaying it unchanged.");
        return g_strdup (html_str);
    }

    return g_strdup (rewritten->c_str ());
}

static gboolean
load_to_stream (GncHtmlWkwebview* self, URLType type,
                const gchar* location, const gchar* label)
{
    gchar* fdata = nullptr;
    int fdata_len = 0;
    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    DEBUG ("type %s, location %s, label %s", type ? type : "(null)",
           location ? location : "(null)", label ? label : "(null)");

    g_return_val_if_fail (self != nullptr, FALSE);

    if (gnc_html_stream_handlers != nullptr)
    {
        const gpointer p = g_hash_table_lookup (gnc_html_stream_handlers, type);
        auto stream_handler = reinterpret_cast<GncHTMLStreamCB> (p);
        if (stream_handler)
        {
            GncHtml* weak_html = GNC_HTML (self);

            g_object_add_weak_pointer (G_OBJECT (self), (gpointer*) (&weak_html));
            bool ok = stream_handler (location, &fdata, &fdata_len);

            if (!weak_html) // will be nullptr if self has been destroyed
            {
                g_free (fdata);
                return FALSE;
            }

            g_object_remove_weak_pointer (G_OBJECT (self), (gpointer*) (&weak_html));

            if (ok)
            {
                fdata = fdata ? fdata : g_strdup ("");

                if (g_strstr_len (fdata, -1, "<object classid=") != nullptr)
                {
                    gchar* new_fdata = handle_embedded_objects (self, fdata);
                    g_free (fdata);
                    fdata = new_fdata;
                }

                // Save a copy for export purposes
                g_free (priv->html_string);
                priv->html_string = g_strdup (fdata);

                /* show_data() writes the report out and loads it by
                 * URI; handing it the anchor here is the only chance to
                 * get the fragment onto that URI, which is what makes
                 * WKWebView scroll to it. */
                g_free (priv->anchor);
                priv->anchor = g_strdup (label);

                show_data (GNC_HTML (self), fdata, strlen (fdata));
            }
            else
                show_error_page (self);

            g_free (fdata);
            return TRUE;
        }
    }

    do
    {
        if (!g_strcmp0 (type, URL_TYPE_SECURE) || !g_strcmp0 (type, URL_TYPE_HTTP))
        {
            if (!g_strcmp0 (type, URL_TYPE_SECURE) && !https_allowed ())
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Secure HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
                break;
            }

            if (!http_allowed ())
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Network HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
            }
            else
                gnc_build_url (type, location, label);
        }
        else
        {
            PWARN ("load_to_stream for inappropriate type\n"
                   "\turl = '%s#%s'\n",
                   location ? location : "(null)", label ? label : "(null)");
            show_error_page (self);
        }
    }
    while (false);

    return TRUE;
}

static void
show_data (GncHtml* self, const gchar* data, int datalen)
{
    constexpr char TEMPLATE_REPORT_FILE_NAME[] = "gnc-report-XXXXXX.html";
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    ENTER ("datalen %d, data %20.20s", datalen, data);

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    /* Write the report out and load it by URI, exactly as the webkit2
     * and WebView2 backends do: a document loaded from a string has no
     * base URL, so its relative links to local images and stylesheets
     * cannot resolve. */
    gchar* filename = g_build_filename (g_get_tmp_dir (), TEMPLATE_REPORT_FILE_NAME,
                                        (gchar*) nullptr);
    int fd = g_mkstemp (filename);
    if (fd == -1)
    {
        PERR ("Could not create a temporary file for the report.");
        g_free (filename);
        LEAVE ("");
        return;
    }

    export_to_file (self, filename);
    close (fd);

    auto uri = gnc::html::file_uri_from_path (filename);
    g_free (filename);

    if (priv->anchor && *priv->anchor)
        uri = gnc::html::with_fragment (uri, priv->anchor);
    g_clear_pointer (&priv->anchor, g_free);

    DEBUG ("Loading uri '%s'", uri.c_str ());
    load_uri (GNC_HTML_WKWEBVIEW (self), uri.c_str ());

    LEAVE ("");
}

static void
show_url (GncHtml* self, URLType type, const gchar* location,
          const gchar* label, gboolean new_window)
{
    GncHTMLUrlCB url_handler = nullptr;
    bool stream_loaded = false;

    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));
    g_return_if_fail (location != nullptr);

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (priv->base.urltype_cb && priv->base.urltype_cb (type))
        gnc_html_cancel (GNC_HTML (self));

    if (gnc_html_url_handlers)
    {
        const gpointer p = g_hash_table_lookup (gnc_html_url_handlers, type);
        url_handler = reinterpret_cast<GncHTMLUrlCB> (p);
    }

    if (url_handler)
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

        bool ok = url_handler (location, label, new_window, &result);
        if (!ok)
        {
            if (result.error_message)
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s", result.error_message);
            else
                /* %s is a URL (some location somewhere). */
                gnc_error_dialog (GTK_WINDOW (priv->base.parent),
                                  _("There was an error accessing %s."), location);

            if (priv->base.load_cb)
                priv->base.load_cb (GNC_HTML (self), result.url_type, location, label,
                                    priv->base.load_cb_data);
        }
        else if (result.load_to_stream)
        {
            const char* new_location = result.location ? result.location : location;
            const char* new_label = result.label ? result.label : label;
            auto hnode = gnc_html_history_node_new (result.url_type, new_location, new_label);

            gnc_html_history_append (priv->base.history, hnode);

            auto base = gnc::html::extract_base_name (result.base_type,
                                                      new_location ? new_location : "");
            g_free (priv->base.base_location);
            priv->base.base_type = result.base_type;
            priv->base.base_location = base ? g_strdup (base->c_str ()) : nullptr;
            DEBUG ("resetting base location to %s",
                   priv->base.base_location ? priv->base.base_location : "(null)");

            stream_loaded = load_to_stream (GNC_HTML_WKWEBVIEW (self), result.url_type,
                                            new_location, new_label);

            if (stream_loaded && priv->base.load_cb != nullptr)
                priv->base.load_cb (GNC_HTML (self), result.url_type, new_location,
                                    new_label, priv->base.load_cb_data);
        }

        g_free (result.location);
        g_free (result.label);
        g_free (result.base_location);
        g_free (result.error_message);

        return;
    }

    if (g_strcmp0 (type, URL_TYPE_JUMP) == 0)
    {
        /* An in-page anchor: the view is already showing the document,
         * so just move to it. */
        if (priv->web_view && label && *label)
        {
            gchar* escaped = g_strescape (label, nullptr);
            auto script = std::format ("window.location.hash=\"{}\";", escaped);
            g_free (escaped);
            [priv->web_view evaluateJavaScript: to_nsstring (script.c_str ())
                            completionHandler: nil];
        }
    }
    else if (g_strcmp0 (type, URL_TYPE_SECURE) == 0 ||
             g_strcmp0 (type, URL_TYPE_HTTP) == 0 ||
             g_strcmp0 (type, URL_TYPE_FILE) == 0)
    {
        do
        {
            if (g_strcmp0 (type, URL_TYPE_SECURE) == 0 && !https_allowed ())
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Secure HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
                break;
            }

            if (g_strcmp0 (type, URL_TYPE_HTTP) == 0 && !http_allowed ())
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Network HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
                break;
            }

            priv->base.base_type = type;

            auto base = gnc::html::extract_base_name (type, location);
            g_free (priv->base.base_location);
            priv->base.base_location = base ? g_strdup (base->c_str ()) : nullptr;

            /* FIXME : handle new_window = 1 */
            gnc_html_history_append (priv->base.history,
                                     gnc_html_history_node_new (type, location, label));
            stream_loaded = load_to_stream (GNC_HTML_WKWEBVIEW (self), type, location, label);
        }
        while (false);
    }
    else
        PERR ("URLType %s not supported.", type);

    if (stream_loaded && priv->base.load_cb != nullptr)
        (priv->base.load_cb) (GNC_HTML (self), type, location, label, priv->base.load_cb_data);
}

static void
reload (GncHtml* self, gboolean force_rebuild)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (force_rebuild)
    {
        gnc_html_history_node* n = gnc_html_history_get_current (priv->base.history);
        if (n != nullptr)
            gnc_html_show_url (self, n->type, n->location, n->label, 0);
    }
    else if (priv->web_view)
    {
        priv->navigating_internally = TRUE;
        [priv->web_view reload];
    }
}

static gboolean
cancel_helper (gpointer key, gpointer value, gpointer user_data)
{
    g_free (key);
    g_list_free ((GList*) value);
    return TRUE;
}

static void
cancel (GncHtml* self)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    if (priv->web_view)
        [priv->web_view stopLoading];

    if (priv->pending)
        priv->pending->clear ();
    g_hash_table_foreach_remove (priv->base.request_info, cancel_helper, nullptr);
}

static void
copy_to_clipboard (GncHtml* self)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);
    if (!priv->web_view)
        return;

    /* copy: is an AppKit action rather than a WKWebView method, so it is
     * sent through the application rather than called directly. */
    [NSApp sendAction: @selector (copy:) to: priv->web_view from: nil];
}

static gboolean
export_to_file (GncHtml* self, const char* filepath)
{
    g_return_val_if_fail (self != nullptr, FALSE);
    g_return_val_if_fail (GNC_IS_HTML_WKWEBVIEW (self), FALSE);
    g_return_val_if_fail (filepath != nullptr, FALSE);

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);
    if (priv->html_string == nullptr)
        return FALSE;

    FILE* fh = g_fopen (filepath, "w");
    if (fh == nullptr)
        return FALSE;

    gint len = strlen (priv->html_string);
    gint written = fwrite (priv->html_string, 1, len, fh);
    fclose (fh);

    return written == len;
}

static void
print (GncHtml* self, const gchar* jobname)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);
    if (!priv->web_view)
        return;

    /* -printOperationWithPrintInfo: is the only supported way to print a
     * WKWebView's contents, and it arrived in macOS 11. */
    if (@available (macOS 11.0, *))
    {
        NSPrintInfo* info = [[[NSPrintInfo sharedPrintInfo] copy] autorelease];
        NSPrintOperation* op = [priv->web_view printOperationWithPrintInfo: info];

        gchar* basename = g_path_get_basename (jobname);
        [op setJobTitle: to_nsstring (basename)];
        g_free (basename);

        [op setShowsPrintPanel: YES];
        [op setShowsProgressPanel: YES];

        if (NSWindow* window = [priv->web_view window])
            [op runOperationModalForWindow: window
                                  delegate: nil
                            didRunSelector: NULL
                               contextInfo: NULL];
        else
            [op runOperation];
    }
    else
        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                          _("Printing a report needs macOS 11 or later. "
                            "You can export the report to a file instead."));
}

static void
set_parent (GncHtml* self, GtkWindow* parent)
{
    g_return_if_fail (self != nullptr);
    g_return_if_fail (GNC_IS_HTML_WKWEBVIEW (self));

    auto priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);
    priv->base.parent = GTK_WIDGET (parent);
}

static void
default_zoom_changed (gpointer prefs, gchar* pref, gpointer user_data)
{
    g_return_if_fail (user_data != nullptr);

    apply_zoom (GNC_HTML_WKWEBVIEW_GET_PRIVATE (GNC_HTML_WKWEBVIEW (user_data)));
}

// *****************************************************************************

static void
gnc_html_wkwebview_init (GncHtmlWkwebview* self)
{
    const gpointer p = g_realloc (GNC_HTML (self)->priv, sizeof (GncHtmlWkwebviewPrivate));
    auto priv = self->priv = reinterpret_cast<GncHtmlWkwebviewPrivate*> (p);
    GNC_HTML (self)->priv = (GncHtmlPrivate*) priv;

    priv->web_view = nullptr;
    priv->delegate = nullptr;
    priv->navigating_internally = FALSE;
    priv->disposed = FALSE;
    priv->html_string = nullptr;
    priv->anchor = nullptr;
    /* g_realloc() moves bytes, it does not run constructors, so
     * anything with a non-trivial lifetime has to live off to the
     * side. */
    priv->pending = new gnc::html::PendingNavigation;

    /* A plain widget that owns its own native GdkWindow once realized
     * -- see socket_realize_cb() -- whose GdkQuartzView becomes the
     * WKWebView's superview. GtkEventBox rather than GtkDrawingArea
     * because nothing is ever drawn into it with Cairo; its only job is
     * to provide that native window. */
    priv->socket = gtk_event_box_new ();
    gtk_widget_set_can_focus (priv->socket, TRUE);

    gtk_container_add (GTK_CONTAINER (priv->base.container), priv->socket);

    g_object_ref_sink (priv->base.container);

    g_signal_connect (priv->socket, "realize", G_CALLBACK (socket_realize_cb), self);
    g_signal_connect (priv->socket, "size-allocate", G_CALLBACK (socket_size_allocate_cb), self);
    g_signal_connect (priv->socket, "map", G_CALLBACK (socket_map_cb), self);
    g_signal_connect (priv->socket, "unmap", G_CALLBACK (socket_unmap_cb), self);

    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM,
                           reinterpret_cast<gpointer> (default_zoom_changed), self);

    LEAVE ("retval %p", self);
}

static void
gnc_html_wkwebview_class_init (GncHtmlWkwebviewClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS (klass);
    GncHtmlClass* html_class = GNC_HTML_CLASS (klass);

    gobject_class->dispose = gnc_html_wkwebview_dispose;
    gobject_class->finalize = gnc_html_wkwebview_finalize;

    html_class->show_url = show_url;
    html_class->show_data = show_data;
    html_class->reload = reload;
    html_class->copy_to_clipboard = copy_to_clipboard;
    html_class->export_to_file = export_to_file;
    html_class->print = print;
    html_class->cancel = cancel;
    html_class->set_parent = set_parent;
}

static void
gnc_html_wkwebview_dispose (GObject* obj)
{
    GncHtmlWkwebview* self = GNC_HTML_WKWEBVIEW (obj);
    GncHtmlWkwebviewPrivate* priv = GNC_HTML_WKWEBVIEW_GET_PRIVATE (self);

    priv->disposed = TRUE;

    if (priv->web_view != nullptr)
    {
        [priv->web_view stopLoading];
        [priv->web_view setNavigationDelegate: nil];
        [priv->web_view setUIDelegate: nil];
        [priv->web_view removeFromSuperview];
        [priv->web_view release];
        priv->web_view = nullptr;
    }

    if (priv->delegate != nullptr)
    {
        [priv->delegate release];
        priv->delegate = nullptr;
    }

    if (priv->socket != nullptr)
    {
        gtk_container_remove (GTK_CONTAINER (priv->base.container), priv->socket);
        priv->socket = nullptr;
    }

    g_clear_pointer (&priv->html_string, g_free);
    g_clear_pointer (&priv->anchor, g_free);

    delete priv->pending;
    priv->pending = nullptr;

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM,
                                 reinterpret_cast<gpointer> (default_zoom_changed), obj);

    G_OBJECT_CLASS (gnc_html_wkwebview_parent_class)->dispose (obj);
}

static void
gnc_html_wkwebview_finalize (GObject* obj)
{
    GncHtmlWkwebview* self = GNC_HTML_WKWEBVIEW (obj);

    self->priv = nullptr;

    G_OBJECT_CLASS (gnc_html_wkwebview_parent_class)->finalize (obj);
}

GncHtml*
gnc_html_wkwebview_new (void) noexcept
{
    auto self = static_cast<GncHtmlWkwebview*> (g_object_new (GNC_TYPE_HTML_WKWEBVIEW, nullptr));
    return GNC_HTML (self);
}

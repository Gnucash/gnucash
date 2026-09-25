/********************************************************************
 * gnc-html-wkwebview.mm -- display reports with Apple WKWebView   *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <platform.h>
#include <libguile.h>

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

#include <gdk/macos/gdkmacos.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>

#include <cerrno>
#include <cstring>
#include <new>
#include <regex.h>
#include <unistd.h>

#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-html-history.h"
#include "gnc-html-p.h"
#include "gnc-html-native-widget-lifecycle.hpp"
#include "gnc-html-wkwebview.hpp"
#include "gnc-prefs.h"

/* indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_HTML;

extern GHashTable *gnc_html_object_handlers;
extern GHashTable *gnc_html_stream_handlers;
extern GHashTable *gnc_html_url_handlers;

G_DEFINE_TYPE (GncHtmlWKWebView, gnc_html_wkwebview, GNC_TYPE_HTML)

struct GncHtmlWKWebViewPrivate
{
    GncHtmlPrivate base;

    GtkWidget *view;
    gchar *html_string;
    gchar *temporary_report;
    gchar *temporary_report_uri;
    gchar *pending_anchor;
    GncHtmlNativeWidgetLifecycle widget_lifecycle;

    NSView *host_view;
    WKWebView *web_view;
    id navigation_delegate;
};

namespace
{
constexpr char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
constexpr char error_404_title[] = N_("Not found");
constexpr char error_404_body[] = N_("The specified URL could not be loaded.");
constexpr char default_zoom_pref[] = "default-zoom";

static void impl_wkwebview_show_url (GncHtml *html, URLType type,
                                     const gchar *location, const gchar *label,
                                     gboolean new_window);
static void impl_wkwebview_show_data (GncHtml *html, const gchar *data, int datalen);
static void impl_wkwebview_reload (GncHtml *html, gboolean force_rebuild);
static void impl_wkwebview_copy_to_clipboard (GncHtml *html);
static gboolean impl_wkwebview_export_to_file (GncHtml *html, const gchar *filepath);
static void impl_wkwebview_print (GncHtml *html, const gchar *jobname,
                                  gboolean export_pdf);
static void impl_wkwebview_cancel (GncHtml *html);
static void impl_wkwebview_set_parent (GncHtml *html, GtkWindow *parent);
static void impl_wkwebview_default_zoom_changed (gpointer prefs, gchar *pref,
                                                 gpointer user_data);
static void wkwebview_attach (GncHtmlWKWebView *self);
static void wkwebview_update_frame (GncHtmlWKWebView *self);
static void wkwebview_navigate_report (GncHtmlWKWebView *self);
static void route_internal_url (GncHtmlWKWebView *self, const gchar *uri,
                                gboolean new_window);

static GncHtmlWKWebViewPrivate *
priv_for (GncHtmlWKWebView *self)
{
    return self->priv;
}

static bool
same_document (const gchar *first, const gchar *second)
{
    if (!first || !second)
        return false;

    auto first_document = g_strdup (first);
    auto second_document = g_strdup (second);
    if (auto fragment = strchr (first_document, '#'))
        *fragment = '\0';
    if (auto fragment = strchr (second_document, '#'))
        *fragment = '\0';
    const auto matches = g_strcmp0 (first_document, second_document) == 0;
    g_free (first_document);
    g_free (second_document);
    return matches;
}

static char *
extract_base_name (URLType type, const gchar *path)
{
    constexpr gchar machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
    constexpr gchar path_rexp[] = "^/*(.*)/+([^/]*)$";
    regex_t compiled_machine, compiled_path;
    regmatch_t match[4];
    gchar *machine = nullptr, *location = nullptr, *base = nullptr;
    gchar *basename = nullptr;

    if (!path)
        return nullptr;

    regcomp (&compiled_machine, machine_rexp, REG_EXTENDED);
    regcomp (&compiled_path, path_rexp, REG_EXTENDED);
    if (!g_strcmp0 (type, URL_TYPE_HTTP) || !g_strcmp0 (type, URL_TYPE_SECURE) ||
        !g_strcmp0 (type, URL_TYPE_FTP))
    {
        if (!regexec (&compiled_machine, path, G_N_ELEMENTS (match), match, 0))
        {
            if (match[1].rm_so != -1)
                machine = g_strndup (path + match[1].rm_so,
                                     match[1].rm_eo - match[1].rm_so);
            if (match[2].rm_so != -1)
                location = g_strndup (path + match[2].rm_so,
                                      match[2].rm_eo - match[2].rm_so);
        }
    }
    else
        location = g_strdup (path);

    if (location && !regexec (&compiled_path, location, G_N_ELEMENTS (match), match, 0) &&
        match[1].rm_so != -1)
        base = g_strndup (location + match[1].rm_so, match[1].rm_eo - match[1].rm_so);

    regfree (&compiled_machine);
    regfree (&compiled_path);
    if (machine)
        basename = base && *base ? g_strconcat (machine, "/", base, "/", nullptr)
                                 : g_strconcat (machine, "/", nullptr);
    else if (base && *base)
        basename = g_strdup (base);

    g_free (machine);
    g_free (location);
    g_free (base);
    return basename;
}

static gboolean
handle_embedded_objects (GncHtmlWKWebView *self, gchar *html, gchar **result)
{
    gchar *remaining = html;
    gchar *combined = nullptr;

    while (auto object = g_strstr_len (remaining, -1, "<object classid="))
    {
        auto class_start = object + strlen ("<object classid=") + 1;
        auto class_end = g_strstr_len (class_start, -1, "\"");
        auto object_end = g_strstr_len (object, -1, "</object>");
        if (!class_end || !object_end)
        {
            g_free (combined);
            *result = g_strdup (html);
            return FALSE;
        }
        object_end += strlen ("</object>");
        auto class_id = g_strndup (class_start, class_end - class_start);
        auto object_contents = g_strndup (object, object_end - object);
        auto before = g_strndup (remaining, object - remaining);
        gchar *replacement = nullptr;
        auto handler = reinterpret_cast<GncHTMLObjectCB> (
            g_hash_table_lookup (gnc_html_object_handlers, class_id));
        if (handler)
            (void)handler (GNC_HTML (self), object_contents, &replacement);
        else
            replacement = g_strdup_printf ("No handler found for classid \"%s\"", class_id);

        auto previous = combined;
        combined = previous ? g_strconcat (previous, before, replacement, nullptr)
                            : g_strconcat (before, replacement, nullptr);
        g_free (previous);
        g_free (class_id);
        g_free (object_contents);
        g_free (before);
        g_free (replacement);
        remaining = object_end;
    }

    if (combined)
    {
        auto previous = combined;
        combined = g_strconcat (previous, remaining, nullptr);
        g_free (previous);
    }
    else
        combined = g_strdup (remaining);
    *result = combined;
    return TRUE;
}

static gboolean
load_to_stream (GncHtmlWKWebView *self, URLType type, const gchar *location,
                const gchar *label)
{
    auto priv = priv_for (self);
    auto stream_handler = gnc_html_stream_handlers
        ? reinterpret_cast<GncHTMLStreamCB> (g_hash_table_lookup (gnc_html_stream_handlers, type))
        : nullptr;
    if (!stream_handler)
        return FALSE;

    gchar *data = nullptr;
    int data_length = 0;
    GncHtml *weak_html = GNC_HTML (self);
    g_object_add_weak_pointer (G_OBJECT (self), reinterpret_cast<gpointer *> (&weak_html));
    const auto loaded = stream_handler (location, &data, &data_length);
    if (!weak_html)
    {
        g_free (data);
        return FALSE;
    }
    g_object_remove_weak_pointer (G_OBJECT (self), reinterpret_cast<gpointer *> (&weak_html));

    if (loaded)
    {
        data = data ? data : g_strdup ("");
        if (g_strstr_len (data, -1, "<object classid="))
        {
            gchar *expanded = nullptr;
            (void)handle_embedded_objects (self, data, &expanded);
            g_free (data);
            data = expanded;
        }
        g_free (priv->html_string);
        priv->html_string = g_strdup (data);
        g_free (priv->pending_anchor);
        priv->pending_anchor = g_strdup (label);
        impl_wkwebview_show_data (GNC_HTML (self), data, strlen (data));
    }
    else
    {
        auto error = data ? data
                          : g_strdup_printf (error_404_format, _(error_404_title),
                                             _(error_404_body));
        impl_wkwebview_show_data (GNC_HTML (self), error, strlen (error));
        if (!data)
            g_free (error);
    }

    g_free (data);
    return TRUE;
}
} // namespace

@interface GncWKHostView : NSView
@end

@implementation GncWKHostView
- (BOOL)isFlipped
{
    return YES;
}
@end

@interface GncWKNavigationDelegate : NSObject<WKNavigationDelegate>
{
    GWeakRef owner_ref;
}
- (id)initWithOwner:(GncHtmlWKWebView *)owner;
@end

@implementation GncWKNavigationDelegate
- (id)initWithOwner:(GncHtmlWKWebView *)owner
{
    self = [super init];
    if (self)
        g_weak_ref_init (&owner_ref, owner);
    return self;
}

- (void)dealloc
{
    g_weak_ref_clear (&owner_ref);
    [super dealloc];
}

- (void)webView:(WKWebView *)webView
    decidePolicyForNavigationAction:(WKNavigationAction *)action
                     decisionHandler:(void (^)(WKNavigationActionPolicy))decisionHandler
{
    auto owner = GNC_HTML_WKWEBVIEW (g_weak_ref_get (&owner_ref));
    if (!owner)
    {
        decisionHandler (WKNavigationActionPolicyCancel);
        return;
    }

    auto priv = priv_for (owner);
    auto uri = g_strdup ([[[action request] URL] absoluteString].UTF8String);
    if (same_document (uri, priv->temporary_report_uri))
        decisionHandler (WKNavigationActionPolicyAllow);
    else
    {
        route_internal_url (owner, uri, [action targetFrame] == nil);
        decisionHandler (WKNavigationActionPolicyCancel);
    }
    g_free (uri);
    g_object_unref (owner);
}

- (void)webView:(WKWebView *)webView didFailProvisionalNavigation:(WKNavigation *)navigation
                       withError:(NSError *)error
{
    PERR ("WKWebView provisional load failed: %s", error.localizedDescription.UTF8String);
}

- (void)webView:(WKWebView *)webView didFailNavigation:(WKNavigation *)navigation
                       withError:(NSError *)error
{
    PERR ("WKWebView load failed: %s", error.localizedDescription.UTF8String);
}
@end

namespace
{
static void
route_internal_url (GncHtmlWKWebView *self, const gchar *uri, gboolean new_window)
{
    if (!gnc_html_handle_internal_url (GNC_HTML (self), uri, new_window))
        PWARN ("Blocked report navigation to '%s'", uri ? uri : "(null)");
}
static void
wkwebview_apply_zoom (GncHtmlWKWebView *self)
{
    auto web_view = priv_for (self)->web_view;
    if (!web_view)
        return;
    if (@available(macOS 11.0, *))
        [web_view setPageZoom:gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT,
                                                   default_zoom_pref)];
}

static void
wkwebview_navigate_report (GncHtmlWKWebView *self)
{
    auto priv = priv_for (self);
    if (!priv->web_view || !priv->temporary_report || !priv->temporary_report_uri)
        return;

    auto report_uri = [[NSURL alloc] initWithString:
                       [NSString stringWithUTF8String:priv->temporary_report_uri]];
    const gchar *root = gnc_html_get_report_document_root ();
    if (!root)
    {
        [report_uri release];
        return;
    }
    auto access_uri = [[NSURL alloc] initFileURLWithPath:
                       [NSString stringWithUTF8String:root] isDirectory:YES];
    if (report_uri && access_uri)
        [priv->web_view loadFileURL:report_uri allowingReadAccessToURL:access_uri];
    [report_uri release];
    [access_uri release];
}

static void
wkwebview_update_frame (GncHtmlWKWebView *self)
{
    auto priv = priv_for (self);
    if (!priv->view)
        return;

    wkwebview_attach (self);
    if (!priv->host_view || !priv->web_view)
        return;

    auto native = gtk_widget_get_native (priv->view);
    double x = 0.0, y = 0.0;
    const graphene_point_t point = GRAPHENE_POINT_INIT (0.0f, 0.0f);
    graphene_point_t native_point;
    if (native && gtk_widget_compute_point (priv->view, GTK_WIDGET (native),
                                             &point, &native_point))
    {
        x = native_point.x;
        y = native_point.y;
    }

    auto content_view = [priv->host_view superview];
    if (!content_view)
        return;
    const auto width = static_cast<CGFloat> (gtk_widget_get_width (priv->view));
    const auto height = static_cast<CGFloat> (gtk_widget_get_height (priv->view));
    const auto cocoa_y = [content_view isFlipped] ? static_cast<CGFloat> (y)
        : NSHeight ([content_view bounds]) - static_cast<CGFloat> (y) - height;
    [priv->host_view setFrame:NSMakeRect (static_cast<CGFloat> (x), cocoa_y, width, height)];
    [priv->host_view setHidden:!gtk_widget_get_mapped (priv->view)];
    [priv->web_view setFrame:[priv->host_view bounds]];
}

static void
wkwebview_attach (GncHtmlWKWebView *self)
{
    auto priv = priv_for (self);
    if (!priv->view || !gtk_widget_get_realized (priv->view))
        return;

    auto native = gtk_widget_get_native (priv->view);
    auto surface = native ? gtk_native_get_surface (native) : nullptr;
    if (!surface || !GDK_IS_MACOS_SURFACE (surface))
    {
        PERR ("WKWebView reports require the GTK macOS backend.");
        return;
    }

    auto window = static_cast<NSWindow *> (
        gdk_macos_surface_get_native_window (GDK_MACOS_SURFACE (surface)));
    auto content_view = [window contentView];
    if (!content_view)
    {
        PERR ("GTK did not provide a native macOS content view for WKWebView.");
        return;
    }

    if (priv->host_view && [priv->host_view superview] != content_view)
    {
        [priv->host_view removeFromSuperview];
        [content_view addSubview:priv->host_view positioned:NSWindowAbove relativeTo:nil];
    }
    if (priv->host_view)
        return;

    auto configuration = [[WKWebViewConfiguration alloc] init];
    [[configuration preferences] setJavaScriptCanOpenWindowsAutomatically:NO];
    priv->host_view = [[GncWKHostView alloc] initWithFrame:NSZeroRect];
    [priv->host_view setWantsLayer:YES];
    [[priv->host_view layer] setMasksToBounds:YES];
    priv->web_view = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:configuration];
    [configuration release];
    priv->navigation_delegate = [[GncWKNavigationDelegate alloc] initWithOwner:self];
    [priv->web_view setNavigationDelegate:priv->navigation_delegate];
    [priv->host_view addSubview:priv->web_view];
    [content_view addSubview:priv->host_view positioned:NSWindowAbove relativeTo:nil];
    wkwebview_apply_zoom (self);
    wkwebview_navigate_report (self);
}

static void
wkwebview_view_realize (GtkWidget *, gpointer user_data)
{
    wkwebview_update_frame (GNC_HTML_WKWEBVIEW (user_data));
}

static gboolean
wkwebview_tick (GtkWidget *, GdkFrameClock *, gpointer user_data)
{
    wkwebview_update_frame (GNC_HTML_WKWEBVIEW (user_data));
    return G_SOURCE_CONTINUE;
}

static void
wkwebview_focus_enter (GtkEventControllerFocus *, gpointer user_data)
{
    auto web_view = priv_for (GNC_HTML_WKWEBVIEW (user_data))->web_view;
    if (web_view && [web_view window])
        [[web_view window] makeFirstResponder:web_view];
}
} // namespace

static void
gnc_html_wkwebview_dispose (GObject *object)
{
    auto self = GNC_HTML_WKWEBVIEW (object);
    auto priv = priv_for (self);

    priv->widget_lifecycle.clear ();
    if (priv->web_view)
    {
        [priv->web_view stopLoading];
        [priv->web_view setNavigationDelegate:nil];
        [priv->web_view removeFromSuperview];
        [priv->web_view release];
        priv->web_view = nil;
    }
    if (priv->host_view)
    {
        [priv->host_view removeFromSuperview];
        [priv->host_view release];
        priv->host_view = nil;
    }
    if (priv->navigation_delegate)
    {
        [priv->navigation_delegate release];
        priv->navigation_delegate = nil;
    }
    g_clear_pointer (&priv->html_string, g_free);
    if (priv->temporary_report)
        g_remove (priv->temporary_report);
    g_clear_pointer (&priv->temporary_report, g_free);
    g_clear_pointer (&priv->temporary_report_uri, g_free);
    g_clear_pointer (&priv->pending_anchor, g_free);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT, default_zoom_pref,
                                 reinterpret_cast<gpointer> (
                                     impl_wkwebview_default_zoom_changed), object);
    G_OBJECT_CLASS (gnc_html_wkwebview_parent_class)->dispose (object);
}

static void
gnc_html_wkwebview_finalize (GObject *object)
{
    auto self = GNC_HTML_WKWEBVIEW (object);
    if (self->priv)
        self->priv->~GncHtmlWKWebViewPrivate ();
    G_OBJECT_CLASS (gnc_html_wkwebview_parent_class)->finalize (object);
}

static void
gnc_html_wkwebview_init (GncHtmlWKWebView *self)
{
    const auto base = *GNC_HTML (self)->priv;
    auto private_data = static_cast<GncHtmlWKWebViewPrivate *> (
        g_realloc (GNC_HTML (self)->priv, sizeof (GncHtmlWKWebViewPrivate)));
    new (private_data) GncHtmlWKWebViewPrivate ();
    private_data->base = base;
    self->priv = private_data;
    GNC_HTML (self)->priv = &private_data->base;

    private_data->view = gtk_drawing_area_new ();
    gtk_widget_set_focusable (private_data->view, TRUE);
    gtk_widget_set_hexpand (private_data->view, TRUE);
    gtk_widget_set_vexpand (private_data->view, TRUE);
    g_clear_object (&private_data->base.container);
    private_data->base.container = GTK_WIDGET (g_object_ref_sink (private_data->view));

    private_data->widget_lifecycle.set_view (private_data->view);
    private_data->widget_lifecycle.add_signal (G_OBJECT (private_data->view), g_signal_connect (
        private_data->view, "realize", G_CALLBACK (wkwebview_view_realize), self));
    private_data->widget_lifecycle.set_tick_callback (gtk_widget_add_tick_callback (
        private_data->view, wkwebview_tick, self, nullptr));
    auto focus = gtk_event_controller_focus_new ();
    private_data->widget_lifecycle.add_signal (G_OBJECT (focus), g_signal_connect (
        focus, "enter", G_CALLBACK (wkwebview_focus_enter), self));
    gtk_widget_add_controller (private_data->view, focus);
    private_data->widget_lifecycle.add_controller (focus);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT, default_zoom_pref,
                           reinterpret_cast<gpointer> (impl_wkwebview_default_zoom_changed), self);
}

static void
gnc_html_wkwebview_class_init (GncHtmlWKWebViewClass *klass)
{
    auto object_class = G_OBJECT_CLASS (klass);
    auto html_class = GNC_HTML_CLASS (klass);
    object_class->dispose = gnc_html_wkwebview_dispose;
    object_class->finalize = gnc_html_wkwebview_finalize;
    html_class->show_url = impl_wkwebview_show_url;
    html_class->show_data = impl_wkwebview_show_data;
    html_class->reload = impl_wkwebview_reload;
    html_class->copy_to_clipboard = impl_wkwebview_copy_to_clipboard;
    html_class->export_to_file = impl_wkwebview_export_to_file;
    html_class->print = impl_wkwebview_print;
    html_class->cancel = impl_wkwebview_cancel;
    html_class->set_parent = impl_wkwebview_set_parent;
}

namespace
{
static void
impl_wkwebview_show_data (GncHtml *html, const gchar *data, int datalen)
{
    auto self = GNC_HTML_WKWEBVIEW (html);
    auto priv = priv_for (self);
    GError *error = nullptr;
    auto filename = gnc_html_create_report_document (&error);
    if (!filename)
    {
        PERR ("Unable to create the temporary report file: %s",
              error ? error->message : "unknown error");
        g_clear_error (&error);
        return;
    }

    g_free (priv->html_string);
    priv->html_string = g_strndup (data, datalen);
    if (!impl_wkwebview_export_to_file (html, filename))
    {
        g_remove (filename);
        g_free (filename);
        return;
    }
    if (priv->temporary_report)
        g_remove (priv->temporary_report);
    g_clear_pointer (&priv->temporary_report, g_free);
    g_clear_pointer (&priv->temporary_report_uri, g_free);
    priv->temporary_report = filename;
    auto uri = g_filename_to_uri (filename, nullptr, &error);
    if (!uri)
    {
        PERR ("Unable to create a URI for the temporary report: %s", error->message);
        g_clear_error (&error);
        g_remove (filename);
        g_clear_pointer (&priv->temporary_report, g_free);
        return;
    }
    if (priv->pending_anchor && *priv->pending_anchor)
    {
        auto fragment = g_uri_escape_string (priv->pending_anchor, nullptr, TRUE);
        priv->temporary_report_uri = g_strconcat (uri, "#", fragment, nullptr);
        g_free (fragment);
    }
    else
        priv->temporary_report_uri = g_strdup (uri);
    g_clear_pointer (&priv->pending_anchor, g_free);
    g_free (uri);
    wkwebview_navigate_report (self);
}

static void
impl_wkwebview_show_url (GncHtml *html, URLType type, const gchar *location,
                         const gchar *label, gboolean new_window)
{
    auto self = GNC_HTML_WKWEBVIEW (html);
    auto priv = priv_for (self);
    g_return_if_fail (location != nullptr);
    if (priv->base.urltype_cb && priv->base.urltype_cb (type))
        impl_wkwebview_cancel (html);

    auto handler = gnc_html_url_handlers
        ? reinterpret_cast<GncHTMLUrlCB> (g_hash_table_lookup (gnc_html_url_handlers, type))
        : nullptr;
    bool stream_loaded = false;
    if (handler)
    {
        GNCURLResult result = {FALSE, type, nullptr, nullptr, URL_TYPE_FILE, nullptr,
                               GTK_WINDOW (priv->base.parent), nullptr};
        if (!handler (location, label, new_window, &result))
        {
            if (result.error_message)
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s", result.error_message);
            else
                gnc_error_dialog (GTK_WINDOW (priv->base.parent),
                                  _("There was an error accessing %s."), location);
        }
        else if (result.load_to_stream)
        {
            const auto new_location = result.location ? result.location : location;
            const auto new_label = result.label ? result.label : label;
            gnc_html_history_append (priv->base.history,
                                     gnc_html_history_node_new (result.url_type, new_location,
                                                                new_label));
            g_free (priv->base.base_location);
            priv->base.base_type = result.base_type;
            priv->base.base_location = extract_base_name (result.base_type, new_location);
            stream_loaded = load_to_stream (self, result.url_type, new_location, new_label);
            if (stream_loaded && priv->base.load_cb)
                priv->base.load_cb (html, result.url_type, new_location, new_label,
                                    priv->base.load_cb_data);
        }
        g_free (result.location);
        g_free (result.label);
        g_free (result.base_location);
        g_free (result.error_message);
        return;
    }

    if (!g_strcmp0 (type, URL_TYPE_JUMP))
        return;
    if (!g_strcmp0 (type, URL_TYPE_SCHEME))
    {
        PINFO ("Scheme report URL '%s' has no registered handler", location);
        return;
    }
    if (!g_strcmp0 (type, URL_TYPE_SECURE) || !g_strcmp0 (type, URL_TYPE_HTTP) ||
        !g_strcmp0 (type, URL_TYPE_FILE))
    {
        g_free (priv->base.base_location);
        priv->base.base_type = type;
        priv->base.base_location = extract_base_name (type, location);
        gnc_html_history_append (priv->base.history,
                                 gnc_html_history_node_new (type, location, label));
        stream_loaded = load_to_stream (self, type, location, label);
    }
    else
        PERR ("URLType %s not supported.", type);

    if (stream_loaded && priv->base.load_cb)
        priv->base.load_cb (html, type, location, label, priv->base.load_cb_data);
}

static void
impl_wkwebview_reload (GncHtml *html, gboolean force_rebuild)
{
    auto self = GNC_HTML_WKWEBVIEW (html);
    auto priv = priv_for (self);
    if (force_rebuild)
    {
        if (auto current = gnc_html_history_get_current (priv->base.history))
            gnc_html_show_url (html, current->type, current->location, current->label, FALSE);
    }
    else if (priv->web_view)
        [priv->web_view reload];
}

static void
impl_wkwebview_copy_to_clipboard (GncHtml *html)
{
    auto priv = priv_for (GNC_HTML_WKWEBVIEW (html));
    if (priv->view)
        gtk_widget_grab_focus (priv->view);
    if (priv->web_view && [priv->web_view window])
    {
        [[priv->web_view window] makeFirstResponder:priv->web_view];
        [NSApp sendAction:@selector (copy:) to:nil from:priv->web_view];
    }
}

static gboolean
impl_wkwebview_export_to_file (GncHtml *html, const gchar *filepath)
{
    auto priv = priv_for (GNC_HTML_WKWEBVIEW (html));
    if (!priv->html_string)
        return FALSE;
    auto file = g_fopen (filepath, "w");
    if (!file)
        return FALSE;
    const auto length = strlen (priv->html_string);
    const auto written = fwrite (priv->html_string, 1, length, file);
    fclose (file);
    return written == length;
}

static void
impl_wkwebview_print (GncHtml *html, const gchar *jobname, gboolean export_pdf)
{
    auto web_view = priv_for (GNC_HTML_WKWEBVIEW (html))->web_view;

    if (!web_view)
        return;
    if (export_pdf)
    {
        if (!jobname || !*jobname)
        {
            PERR ("WKWebView cannot export a PDF without a local output path.");
            return;
        }
        if (![web_view respondsToSelector:@selector(createPDFWithConfiguration:completionHandler:)])
        {
            PERR ("The installed macOS WebKit runtime does not support PDF export.");
            return;
        }

        NSString *output_path = [NSString stringWithUTF8String:jobname];
        [web_view createPDFWithConfiguration:nil
                            completionHandler:^(NSData *data, NSError *error) {
            if (error || !data)
            {
                PERR ("WKWebView PDF export failed: %s",
                      error ? [[error localizedDescription] UTF8String]
                            : "no PDF data was returned");
                return;
            }

            NSError *write_error = nil;
            if (![data writeToFile:output_path options:NSDataWritingAtomic
                              error:&write_error])
                PERR ("Could not write WKWebView PDF output: %s",
                      [[write_error localizedDescription] UTF8String]);
        }];
        return;
    }

    NSPrintOperation *operation =
        [web_view printOperationWithPrintInfo:[NSPrintInfo sharedPrintInfo]];
    if (!operation)
        return;
    if (jobname && *jobname)
        [operation setJobTitle:[NSString stringWithUTF8String:jobname]];
    [operation runOperation];
}
static void
impl_wkwebview_cancel (GncHtml *html)
{
    auto priv = priv_for (GNC_HTML_WKWEBVIEW (html));
    if (priv->web_view)
        [priv->web_view stopLoading];
    g_hash_table_remove_all (priv->base.request_info);
}

static void
impl_wkwebview_set_parent (GncHtml *html, GtkWindow *parent)
{
    priv_for (GNC_HTML_WKWEBVIEW (html))->base.parent = GTK_WIDGET (parent);
}

static void
impl_wkwebview_default_zoom_changed (gpointer, gchar *, gpointer user_data)
{
    wkwebview_apply_zoom (GNC_HTML_WKWEBVIEW (user_data));
}
} // namespace

GncHtml *
gnc_html_wkwebview_new (void) noexcept
{
    return GNC_HTML (g_object_new (GNC_TYPE_HTML_WKWEBVIEW, nullptr));
}

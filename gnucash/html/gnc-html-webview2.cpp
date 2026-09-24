/********************************************************************
 * gnc-html-webview2.cpp -- display reports with Microsoft WebView2 *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
 * Copyright (C) 2026 John Ralls <jralls@ceridwen.us>                *
 * Copyright (C) 2026 The GnuCash Project                           *
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
#define _GL_UNISTD_H /* Deflect Guile's poisonous close definition. */
#endif
#include <libguile.h>

#include <windows.h>
#include <dcomp.h>
#include <WebView2.h>
#include <wrl/client.h>

#include <gdk/gdk.h>
#include <gdk/win32/gdkwin32.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gtk/gtk.h>

#include <atomic>
#include <cerrno>
#include <cstring>
#include <new>
#include <regex.h>
#include <memory>
#include <string>
#include <unistd.h>
#include <vector>

#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-html-history.h"
#include "gnc-html-p.h"
#include "gnc-html-native-widget-lifecycle.hpp"
#include "gnc-html-webview2-clipboard.hpp"
#include "gnc-html-webview2-coordinates.hpp"
#include "gnc-html-webview2-loader-state.hpp"
#include "gnc-html-webview2-visibility.hpp"
#include "gnc-html-webview2.hpp"
#include "gnc-prefs.h"

using Microsoft::WRL::ComPtr;

/* indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_HTML;

extern GHashTable *gnc_html_object_handlers;
extern GHashTable *gnc_html_stream_handlers;
extern GHashTable *gnc_html_url_handlers;

static void gnc_html_webview2_init (GncHtmlWebView2 *self);
static void gnc_html_webview2_class_init (GncHtmlWebView2Class *klass);

G_DEFINE_TYPE (GncHtmlWebView2, gnc_html_webview2, GNC_TYPE_HTML)

struct GncHtmlWebView2Private
{
    GncHtmlPrivate base;

    GtkWidget *view = nullptr;
    gchar *html_string = nullptr;
    gchar *temporary_report = nullptr;
    gchar *temporary_report_uri = nullptr;
    gchar *pending_anchor = nullptr;

    std::shared_ptr<GncHtmlWebView2LoaderState> loader;
    HWND hwnd = nullptr;
    GncHtmlNativeWidgetLifecycle widget_lifecycle;
    std::atomic<bool> disposing {false};

    ComPtr<IDCompositionDevice> composition_device;
    ComPtr<IDCompositionTarget> composition_target;
    ComPtr<IDCompositionVisual> composition_root;
    ComPtr<ICoreWebView2Environment> environment;
    ComPtr<ICoreWebView2Controller> controller;
    ComPtr<ICoreWebView2CompositionController> composition_controller;
    ComPtr<ICoreWebView2> web_view;
    ComPtr<ICoreWebView2_11> web_view11;
    EventRegistrationToken navigation_starting = {};
    EventRegistrationToken new_window_requested = {};
    EventRegistrationToken context_menu_requested = {};
    bool navigation_handler_installed = false;
    bool new_window_handler_installed = false;
    bool context_menu_handler_installed = false;
};

namespace
{
constexpr char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
constexpr char error_404_title[] = N_("Not found");
constexpr char error_404_body[] = N_("The specified URL could not be loaded.");
constexpr char default_zoom_pref[] = "default-zoom";

using CreateEnvironmentWithOptionsFn = HRESULT (STDAPICALLTYPE *)(
    PCWSTR, PCWSTR, ICoreWebView2EnvironmentOptions *,
    ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler *);

static void impl_webview2_show_url (GncHtml *self, URLType type,
                                    const gchar *location, const gchar *label,
                                    gboolean new_window);
static void impl_webview2_show_data (GncHtml *self, const gchar *data, int datalen);
static void impl_webview2_reload (GncHtml *self, gboolean force_rebuild);
static void impl_webview2_copy_to_clipboard (GncHtml *self);
static gboolean impl_webview2_export_to_file (GncHtml *self, const gchar *filepath);
static void impl_webview2_print (GncHtml *self, const gchar *jobname,
                                  gboolean export_pdf);
static void impl_webview2_cancel (GncHtml *self);
static void impl_webview2_set_parent (GncHtml *self, GtkWindow *parent);
static void impl_webview2_default_zoom_changed (gpointer prefs, gchar *pref,
                                                gpointer user_data);
static void webview2_start (GncHtmlWebView2 *self);
static void webview2_update_bounds (GncHtmlWebView2 *self);
static void webview2_update_visibility (GncHtmlWebView2 *self);
static void webview2_navigate_report (GncHtmlWebView2 *self);

static void
webview2_queue_loader_release (gpointer module)
{
    g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                     [] (gpointer data) {
                         FreeLibrary (static_cast<HMODULE> (data));
                         return G_SOURCE_REMOVE;
                     }, module, nullptr);
}

static GncHtmlWebView2Private *
priv_for (GncHtmlWebView2 *self)
{
    return self->priv;
}

static std::wstring
to_utf16 (const gchar *value)
{
    if (!value)
        return {};
    auto wide = g_utf8_to_utf16 (value, -1, nullptr, nullptr, nullptr);
    if (!wide)
        return {};
    std::wstring result (reinterpret_cast<wchar_t *> (wide));
    g_free (wide);
    return result;
}

static gchar *
to_utf8 (LPCWSTR value)
{
    if (!value)
        return nullptr;
    return g_utf16_to_utf8 (reinterpret_cast<const gunichar2 *> (value), -1,
                             nullptr, nullptr, nullptr);
}

static std::wstring
webview2_application_directory ()
{
    std::vector<wchar_t> path (32768);
    const auto length = GetModuleFileNameW (nullptr, path.data (),
                                            static_cast<DWORD> (path.size ()));
    if (!length || length == path.size ())
    {
        PERR ("Could not determine the GnuCash executable directory for WebView2.");
        return {};
    }

    std::wstring directory (path.data (), length);
    const auto separator = directory.find_last_of (L"\\/");
    if (separator == std::wstring::npos)
    {
        PERR ("Could not determine the GnuCash executable directory for WebView2.");
        return {};
    }
    directory.resize (separator);
    return directory;
}

static std::wstring
webview2_user_data_directory ()
{
    auto path = g_build_filename (g_get_user_cache_dir (), "GnuCash", "WebView2", nullptr);
    if (g_mkdir_with_parents (path, 0700) != 0)
    {
        PERR ("Could not create the WebView2 user-data directory: %s", g_strerror (errno));
        g_free (path);
        return {};
    }

    auto directory = to_utf16 (path);
    g_free (path);
    return directory;
}

static std::wstring
webview2_fixed_runtime_directory ()
{
#if defined(GNC_REPORT_WEBVIEW2_FIXED_RUNTIME)
    auto directory = webview2_application_directory ();
    if (directory.empty ())
        return {};
    directory += L"\\WebView2Runtime";
    const auto attributes = GetFileAttributesW (directory.c_str ());
    if (attributes == INVALID_FILE_ATTRIBUTES || !(attributes & FILE_ATTRIBUTE_DIRECTORY))
    {
        PERR ("The bundled WebView2 Fixed Version Runtime is missing.");
        return {};
    }
    return directory;
#else
    return {};
#endif
}

static void
log_hresult (const char *operation, HRESULT result)
{
    PERR ("WebView2 %s failed (HRESULT 0x%08lx)", operation,
          static_cast<unsigned long> (result));
}

class CallbackOwner
{
public:
    explicit CallbackOwner (GWeakRef *owner_ref) :
        owner_ (nullptr)
    {
        auto object = g_weak_ref_get (owner_ref);
        if (object)
            owner_ = GNC_HTML_WEBVIEW2 (object);
        if (owner_ && priv_for (owner_)->disposing)
            g_clear_object (&owner_);
    }

    ~CallbackOwner () { g_clear_object (&owner_); }

    explicit operator bool () const { return owner_ != nullptr; }
    GncHtmlWebView2 *get () const { return owner_; }

private:
    GncHtmlWebView2 *owner_ = nullptr;
};

template <typename Interface>
class CallbackBase : public Interface
{
public:
    CallbackBase (GncHtmlWebView2 *self,
                  std::shared_ptr<GncHtmlWebView2LoaderState> loader) :
        loader_ (std::move (loader))
    {
        g_weak_ref_init (&owner_ref_, self);
    }

    virtual ~CallbackBase ()
    {
        g_weak_ref_clear (&owner_ref_);
    }

    ULONG STDMETHODCALLTYPE AddRef () override
    {
        return ++references_;
    }

    ULONG STDMETHODCALLTYPE Release () override
    {
        const auto references = --references_;
        if (!references)
            delete this;
        return references;
    }

protected:
    HRESULT query_interface (REFIID requested, void **object, REFIID expected)
    {
        if (!object)
            return E_POINTER;
        *object = nullptr;
        if (IsEqualIID (requested, IID_IUnknown) || IsEqualIID (requested, expected))
        {
            *object = static_cast<Interface *> (this);
            AddRef ();
            return S_OK;
        }
        return E_NOINTERFACE;
    }

    CallbackOwner self () { return CallbackOwner (&owner_ref_); }

private:
    std::atomic<ULONG> references_ {1};
    GWeakRef owner_ref_;
    std::shared_ptr<GncHtmlWebView2LoaderState> loader_;
};

class EnvironmentCompletedHandler final
    : public CallbackBase<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT error,
                                      ICoreWebView2Environment *environment) override;
};

class CompositionCompletedHandler final
    : public CallbackBase<ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (
            requested, object,
            IID_ICoreWebView2CreateCoreWebView2CompositionControllerCompletedHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT error,
                                      ICoreWebView2CompositionController *controller) override;
};

class PrintToPdfCompletedHandler final
    : public CallbackBase<ICoreWebView2PrintToPdfCompletedHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2PrintToPdfCompletedHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT error, BOOL succeeded) override
    {
        if (FAILED (error))
            log_hresult ("PDF export", error);
        else if (!succeeded)
            PERR ("WebView2 did not create the requested PDF file.");
        return S_OK;
    }
};
class NavigationStartingHandler final
    : public CallbackBase<ICoreWebView2NavigationStartingEventHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2NavigationStartingEventHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2 *sender,
                                      ICoreWebView2NavigationStartingEventArgs *args) override;
};

class NewWindowRequestedHandler final
    : public CallbackBase<ICoreWebView2NewWindowRequestedEventHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2NewWindowRequestedEventHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2 *sender,
                                      ICoreWebView2NewWindowRequestedEventArgs *args) override;
};

class ContextMenuRequestedHandler final
    : public CallbackBase<ICoreWebView2ContextMenuRequestedEventHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2ContextMenuRequestedEventHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2 *sender,
                                      ICoreWebView2ContextMenuRequestedEventArgs *args) override;
};

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
handle_embedded_objects (GncHtmlWebView2 *self, gchar *html, gchar **result)
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
load_to_stream (GncHtmlWebView2 *self, URLType type, const gchar *location,
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
        impl_webview2_show_data (GNC_HTML (self), data, strlen (data));
    }
    else
    {
        auto error = data ? data
                          : g_strdup_printf (error_404_format, _(error_404_title),
                                             _(error_404_body));
        impl_webview2_show_data (GNC_HTML (self), error, strlen (error));
        if (!data)
            g_free (error);
    }

    g_free (data);
    return TRUE;
}

static void
route_internal_url (GncHtmlWebView2 *self, const gchar *uri, gboolean new_window)
{
    if (!gnc_html_handle_internal_url (GNC_HTML (self), uri, new_window))
        PWARN ("Blocked report navigation to '%s'", uri ? uri : "(null)");
}
HRESULT
EnvironmentCompletedHandler::Invoke (HRESULT error, ICoreWebView2Environment *environment)
{
    auto owner = self ();
    if (!owner)
        return S_OK;
    if (FAILED (error) || !environment)
    {
        log_hresult ("environment creation", error);
        return S_OK;
    }

    auto priv = priv_for (owner.get ());
    priv->environment = environment;
    ComPtr<ICoreWebView2Environment3> environment3;
    if (FAILED (environment->QueryInterface (IID_ICoreWebView2Environment3,
                                              reinterpret_cast<void **> (
                                                  environment3.GetAddressOf ()))))
    {
        PERR ("The installed WebView2 Runtime does not support CompositionController.");
        return S_OK;
    }

    auto handler = new CompositionCompletedHandler (owner.get (), priv->loader);
    const auto result = environment3->CreateCoreWebView2CompositionController (priv->hwnd,
                                                                                 handler);
    handler->Release ();
    if (FAILED (result))
        log_hresult ("CompositionController creation", result);
    return S_OK;
}

HRESULT
CompositionCompletedHandler::Invoke (HRESULT error,
                                     ICoreWebView2CompositionController *composition)
{
    auto owner = self ();
    if (!owner)
        return S_OK;
    if (FAILED (error) || !composition)
    {
        log_hresult ("CompositionController completion", error);
        return S_OK;
    }

    auto priv = priv_for (owner.get ());
    priv->composition_controller = composition;
    if (FAILED (composition->QueryInterface (IID_ICoreWebView2Controller,
                                              reinterpret_cast<void **> (
                                                  priv->controller.GetAddressOf ()))) ||
        FAILED (priv->controller->get_CoreWebView2 (priv->web_view.GetAddressOf ())))
    {
        PERR ("WebView2 CompositionController did not expose ICoreWebView2Controller.");
        return S_OK;
    }

    if (FAILED (composition->put_RootVisualTarget (priv->composition_root.Get ())))
    {
        PERR ("WebView2 could not attach to the DirectComposition visual.");
        return S_OK;
    }
    if (FAILED (priv->composition_device->Commit ()))
    {
        PERR ("WebView2 DirectComposition commit failed.");
        return S_OK;
    }

    auto navigation_handler = new NavigationStartingHandler (owner.get (), priv->loader);
    if (SUCCEEDED (priv->web_view->add_NavigationStarting (navigation_handler,
                                                            &priv->navigation_starting)))
        priv->navigation_handler_installed = true;
    navigation_handler->Release ();

    auto new_window_handler = new NewWindowRequestedHandler (owner.get (), priv->loader);
    if (SUCCEEDED (priv->web_view->add_NewWindowRequested (new_window_handler,
                                                            &priv->new_window_requested)))
        priv->new_window_handler_installed = true;
    new_window_handler->Release ();

    if (SUCCEEDED (priv->web_view->QueryInterface (
            IID_ICoreWebView2_11,
            reinterpret_cast<void **> (priv->web_view11.GetAddressOf ()))))
    {
        auto context_menu_handler = new ContextMenuRequestedHandler (owner.get (),
                                                                     priv->loader);
        if (SUCCEEDED (priv->web_view11->add_ContextMenuRequested (
                context_menu_handler, &priv->context_menu_requested)))
            priv->context_menu_handler_installed = true;
        context_menu_handler->Release ();
    }

    impl_webview2_default_zoom_changed (nullptr, nullptr, owner.get ());
    webview2_update_bounds (owner.get ());
    webview2_navigate_report (owner.get ());
    return S_OK;
}

HRESULT
NavigationStartingHandler::Invoke (ICoreWebView2 *,
                                   ICoreWebView2NavigationStartingEventArgs *args)
{
    auto owner = self ();
    if (!owner || !args)
        return S_OK;

    LPWSTR wide_uri = nullptr;
    if (FAILED (args->get_Uri (&wide_uri)))
        return S_OK;
    auto uri = to_utf8 (wide_uri);
    CoTaskMemFree (wide_uri);
    auto priv = priv_for (owner.get ());
    if (!same_document (uri, priv->temporary_report_uri))
    {
        args->put_Cancel (TRUE);
        route_internal_url (owner.get (), uri, FALSE);
    }
    g_free (uri);
    return S_OK;
}

HRESULT
NewWindowRequestedHandler::Invoke (ICoreWebView2 *,
                                   ICoreWebView2NewWindowRequestedEventArgs *args)
{
    auto owner = self ();
    if (!args)
        return S_OK;
    args->put_Handled (TRUE);

    LPWSTR wide_uri = nullptr;
    if (SUCCEEDED (args->get_Uri (&wide_uri)))
    {
        auto uri = to_utf8 (wide_uri);
        CoTaskMemFree (wide_uri);
        if (owner)
            route_internal_url (owner.get (), uri, TRUE);
        g_free (uri);
    }
    return S_OK;
}

HRESULT
ContextMenuRequestedHandler::Invoke (ICoreWebView2 *,
                                     ICoreWebView2ContextMenuRequestedEventArgs *args)
{
    auto owner = self ();
    if (!owner || !args)
        return S_OK;

    ComPtr<ICoreWebView2ContextMenuTarget> target;
    if (FAILED (args->get_ContextMenuTarget (target.GetAddressOf ())) || !target)
        return S_OK;

    BOOL has_link = FALSE;
    LPWSTR wide_uri = nullptr;
    if (FAILED (target->get_HasLinkUri (&has_link)) || !has_link ||
        FAILED (target->get_LinkUri (&wide_uri)) || !wide_uri)
        return S_OK;

    auto uri = to_utf8 (wide_uri);
    CoTaskMemFree (wide_uri);
    if (!uri)
        return S_OK;

    gchar *location = nullptr;
    gchar *label = nullptr;
    const auto type = gnc_html_parse_url (GNC_HTML (owner.get ()), uri,
                                          &location, &label);
    g_free (location);
    g_free (label);
    g_free (uri);
    if (g_strcmp0 (type, URL_TYPE_REPORT))
        return S_OK;

    ComPtr<ICoreWebView2ContextMenuItemCollection> items;
    if (FAILED (args->get_MenuItems (items.GetAddressOf ())) || !items)
        return S_OK;

    UINT32 count = 0;
    items->get_Count (&count);
    for (UINT32 index = 0; index < count;)
    {
        ComPtr<ICoreWebView2ContextMenuItem> item;
        if (FAILED (items->GetValueAtIndex (index, item.GetAddressOf ())) || !item)
        {
            ++index;
            continue;
        }

        bool removed = false;
        LPWSTR wide_name = nullptr;
        if (SUCCEEDED (item->get_Name (&wide_name)) && wide_name)
        {
            auto name = to_utf8 (wide_name);
            CoTaskMemFree (wide_name);
            if (!g_strcmp0 (name, "openLinkInNewWindow") ||
                !g_strcmp0 (name, "openLinkInNewTab"))
            {
                if (SUCCEEDED (items->RemoveValueAtIndex (index)))
                {
                    --count;
                    removed = true;
                }
            }
            g_free (name);
        }
        if (!removed)
            ++index;
    }
    return S_OK;
}

static void
webview2_view_realize (GtkWidget *, gpointer user_data)
{
    webview2_start (GNC_HTML_WEBVIEW2 (user_data));
}

static void
webview2_view_mapping_changed (GtkWidget *, gpointer user_data)
{
    webview2_update_visibility (GNC_HTML_WEBVIEW2 (user_data));
}

static gboolean
webview2_tick (GtkWidget *, GdkFrameClock *, gpointer user_data)
{
    webview2_update_bounds (GNC_HTML_WEBVIEW2 (user_data));
    return G_SOURCE_CONTINUE;
}

static void
webview2_focus_enter (GtkEventControllerFocus *, gpointer user_data)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (user_data));
    if (priv->controller)
        (void)priv->controller->MoveFocus (COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC);
}

static gboolean
webview2_surface_point_from_widget (GtkWidget *widget, double x, double y,
                                    double *surface_x, double *surface_y)
{
    auto native = gtk_widget_get_native (widget);
    if (!native)
        return FALSE;
    const graphene_point_t point = GRAPHENE_POINT_INIT (static_cast<float> (x),
                                                         static_cast<float> (y));
    graphene_point_t surface_point;
    if (!gtk_widget_compute_point (widget, GTK_WIDGET (native), &point, &surface_point))
        return FALSE;
    *surface_x = surface_point.x;
    *surface_y = surface_point.y;
    return TRUE;
}

static gboolean
webview2_controller_point_from_surface (GtkWidget *widget, double surface_x, double surface_y,
                                        POINT *controller_point)
{
    auto native = gtk_widget_get_native (widget);
    if (!native)
        return FALSE;
    auto surface = gtk_native_get_surface (native);
    if (!surface)
        return FALSE;
    const auto point = gnc_html_webview2_controller_point_from_surface (
        surface_x, surface_y, static_cast<double> (gdk_surface_get_scale (surface)));
    *controller_point = {point.x, point.y};
    return TRUE;
}

static gboolean
webview2_controller_point_from_widget (GtkWidget *widget, double x, double y, POINT *point)
{
    double surface_x = 0.0, surface_y = 0.0;
    if (!webview2_surface_point_from_widget (widget, x, y, &surface_x, &surface_y))
        return FALSE;
    return webview2_controller_point_from_surface (widget, surface_x, surface_y, point);
}

static void
webview2_click_pressed (GtkGestureClick *gesture, int, double x, double y,
                        gpointer user_data)
{
    auto self = GNC_HTML_WEBVIEW2 (user_data);
    auto priv = priv_for (self);
    if (!priv->composition_controller)
        return;
    POINT point;
    if (!webview2_controller_point_from_widget (priv->view, x, y, &point))
        return;
    const auto button = gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture));
    COREWEBVIEW2_MOUSE_EVENT_KIND kind = COREWEBVIEW2_MOUSE_EVENT_KIND_LEFT_BUTTON_DOWN;
    if (button == GDK_BUTTON_SECONDARY)
        kind = COREWEBVIEW2_MOUSE_EVENT_KIND_RIGHT_BUTTON_DOWN;
    else if (button == GDK_BUTTON_MIDDLE)
        kind = COREWEBVIEW2_MOUSE_EVENT_KIND_MIDDLE_BUTTON_DOWN;
    (void)priv->composition_controller->SendMouseInput (
        kind, COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_NONE, 0, point);
    gtk_widget_grab_focus (priv->view);
}

static void
webview2_click_released (GtkGestureClick *gesture, int, double x, double y,
                         gpointer user_data)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (user_data));
    if (!priv->composition_controller)
        return;
    POINT point;
    if (!webview2_controller_point_from_widget (priv->view, x, y, &point))
        return;
    const auto button = gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture));
    COREWEBVIEW2_MOUSE_EVENT_KIND kind = COREWEBVIEW2_MOUSE_EVENT_KIND_LEFT_BUTTON_UP;
    if (button == GDK_BUTTON_SECONDARY)
        kind = COREWEBVIEW2_MOUSE_EVENT_KIND_RIGHT_BUTTON_UP;
    else if (button == GDK_BUTTON_MIDDLE)
        kind = COREWEBVIEW2_MOUSE_EVENT_KIND_MIDDLE_BUTTON_UP;
    (void)priv->composition_controller->SendMouseInput (
        kind, COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_NONE, 0, point);
}

static void
webview2_motion (GtkEventControllerMotion *, double x, double y, gpointer user_data)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (user_data));
    if (!priv->composition_controller)
        return;
    POINT point;
    if (!webview2_controller_point_from_widget (priv->view, x, y, &point))
        return;
    (void)priv->composition_controller->SendMouseInput (
        COREWEBVIEW2_MOUSE_EVENT_KIND_MOVE, COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_NONE, 0,
        point);
}

static COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS
webview2_mouse_modifiers (GdkModifierType state)
{
    auto modifiers = COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_NONE;
    if (state & GDK_SHIFT_MASK)
        modifiers = static_cast<COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS> (
            modifiers | COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_SHIFT);
    if (state & GDK_CONTROL_MASK)
        modifiers = static_cast<COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS> (
            modifiers | COREWEBVIEW2_MOUSE_EVENT_VIRTUAL_KEYS_CONTROL);
    return modifiers;
}

static gboolean
webview2_scroll (GtkEventControllerScroll *controller, double delta_x, double delta_y,
                 gpointer user_data)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (user_data));
    if (!priv->composition_controller)
        return GDK_EVENT_STOP;
    auto event = gtk_event_controller_get_current_event (GTK_EVENT_CONTROLLER (controller));
    if (!event)
        return GDK_EVENT_STOP;
    auto native = gtk_widget_get_native (priv->view);
    if (!native || gdk_event_get_surface (event) != gtk_native_get_surface (native))
    {
        PERR ("WebView2 received a scroll event from a different GTK surface.");
        return GDK_EVENT_STOP;
    }
    double x = 0.0, y = 0.0;
    if (!gdk_event_get_position (event, &x, &y))
        return GDK_EVENT_STOP;
    const auto modifiers = webview2_mouse_modifiers (
        gtk_event_controller_get_current_event_state (GTK_EVENT_CONTROLLER (controller)));
    POINT point;
    if (!webview2_controller_point_from_surface (priv->view, x, y, &point))
        return GDK_EVENT_STOP;
    if (delta_x != 0.0)
    {
        const auto delta = static_cast<LONG> (-delta_x * WHEEL_DELTA);
        (void)priv->composition_controller->SendMouseInput (
            COREWEBVIEW2_MOUSE_EVENT_KIND_HORIZONTAL_WHEEL, modifiers,
            static_cast<UINT32> (delta), point);
    }
    if (delta_y != 0.0)
    {
        const auto delta = static_cast<LONG> (-delta_y * WHEEL_DELTA);
        (void)priv->composition_controller->SendMouseInput (
            COREWEBVIEW2_MOUSE_EVENT_KIND_WHEEL, modifiers, static_cast<UINT32> (delta), point);
    }
    return GDK_EVENT_STOP;
}

static void
webview2_start (GncHtmlWebView2 *self)
{
    auto priv = priv_for (self);
    if (priv->environment || priv->loader || !priv->view ||
        !gtk_widget_get_realized (priv->view))
        return;

    auto native = gtk_widget_get_native (priv->view);
    if (!native)
        return;
    auto surface = gtk_native_get_surface (native);
    if (!GDK_IS_WIN32_SURFACE (surface))
    {
        PERR ("WebView2 reports require the GTK Win32 backend.");
        return;
    }
    priv->hwnd = gdk_win32_surface_get_handle (surface);
    if (!priv->hwnd)
    {
        PERR ("GTK did not provide a native Win32 surface for WebView2.");
        return;
    }

    HRESULT result = DCompositionCreateDevice (
        nullptr, __uuidof (IDCompositionDevice),
        reinterpret_cast<void **> (priv->composition_device.GetAddressOf ()));
    if (FAILED (result) ||
        FAILED (result = priv->composition_device->CreateTargetForHwnd (
                     priv->hwnd, TRUE, priv->composition_target.GetAddressOf ())) ||
        FAILED (result = priv->composition_device->CreateVisual (
                     priv->composition_root.GetAddressOf ())) ||
        FAILED (result = priv->composition_target->SetRoot (priv->composition_root.Get ())) ||
        FAILED (result = priv->composition_device->Commit ()))
    {
        log_hresult ("DirectComposition initialization", result);
        return;
    }

    const auto application_directory = webview2_application_directory ();
    if (application_directory.empty ())
        return;
    const auto loader_path = application_directory + L"\\WebView2Loader.dll";
    auto loader_module = LoadLibraryExW (loader_path.c_str (), nullptr,
                                         LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR |
                                         LOAD_LIBRARY_SEARCH_DEFAULT_DIRS);
    if (!loader_module)
    {
        PERR ("WebView2Loader.dll could not be loaded from the GnuCash installation.");
        return;
    }
    priv->loader = std::make_shared<GncHtmlWebView2LoaderState> (
        loader_module, webview2_queue_loader_release);
    auto create_environment = reinterpret_cast<CreateEnvironmentWithOptionsFn> (
        GetProcAddress (loader_module, "CreateCoreWebView2EnvironmentWithOptions"));
    if (!create_environment)
    {
        PERR ("WebView2Loader.dll does not export CreateCoreWebView2EnvironmentWithOptions.");
        priv->loader.reset ();
        return;
    }

    const auto runtime_directory = webview2_fixed_runtime_directory ();
#if defined(GNC_REPORT_WEBVIEW2_FIXED_RUNTIME)
    if (runtime_directory.empty ())
    {
        priv->loader.reset ();
        return;
    }
#endif
    const auto user_data_directory = webview2_user_data_directory ();
    if (user_data_directory.empty ())
    {
        priv->loader.reset ();
        return;
    }
    auto handler = new EnvironmentCompletedHandler (self, priv->loader);
    result = create_environment (runtime_directory.empty () ? nullptr : runtime_directory.c_str (),
                                 user_data_directory.c_str (), nullptr, handler);
    handler->Release ();
    if (FAILED (result))
    {
        log_hresult ("environment request", result);
        priv->loader.reset ();
    }
}

static void
webview2_update_visibility (GncHtmlWebView2 *self)
{
    auto priv = priv_for (self);

    if (priv->controller && priv->view)
        (void)priv->controller->put_IsVisible (
            gnc_html_webview2_host_is_mapped (priv->view));
}

static void
webview2_update_bounds (GncHtmlWebView2 *self)
{
    auto priv = priv_for (self);
    webview2_update_visibility (self);
    if (!priv->controller || !priv->view)
        return;

    double x = 0.0, y = 0.0;
    if (!webview2_surface_point_from_widget (priv->view, 0.0, 0.0, &x, &y))
        return;
    POINT top_left, bottom_right;
    if (!webview2_controller_point_from_surface (priv->view, x, y, &top_left) ||
        !webview2_controller_point_from_surface (
            priv->view, x + gtk_widget_get_width (priv->view),
            y + gtk_widget_get_height (priv->view), &bottom_right))
        return;
    const RECT bounds = {top_left.x, top_left.y, bottom_right.x, bottom_right.y};
    (void)priv->controller->put_Bounds (bounds);
}

static void
webview2_navigate_report (GncHtmlWebView2 *self)
{
    auto priv = priv_for (self);
    if (!priv->web_view || !priv->temporary_report_uri)
        return;
    auto uri = to_utf16 (priv->temporary_report_uri);
    if (!uri.empty ())
        (void)priv->web_view->Navigate (uri.c_str ());
}

} // namespace

static void
gnc_html_webview2_dispose (GObject *object)
{
    auto self = GNC_HTML_WEBVIEW2 (object);
    auto priv = priv_for (self);
    priv->disposing = true;
    priv->widget_lifecycle.clear ();
    if (priv->web_view)
    {
        if (priv->navigation_handler_installed)
            (void)priv->web_view->remove_NavigationStarting (priv->navigation_starting);
        if (priv->new_window_handler_installed)
            (void)priv->web_view->remove_NewWindowRequested (priv->new_window_requested);
        if (priv->context_menu_handler_installed && priv->web_view11)
            (void)priv->web_view11->remove_ContextMenuRequested (
                priv->context_menu_requested);
    }
    if (priv->controller)
        (void)priv->controller->Close ();
    priv->web_view.Reset ();
    priv->controller.Reset ();
    priv->composition_controller.Reset ();
    priv->web_view11.Reset ();
    priv->environment.Reset ();
    priv->composition_root.Reset ();
    priv->composition_target.Reset ();
    priv->composition_device.Reset ();
    priv->loader.reset ();
    g_clear_pointer (&priv->html_string, g_free);
    if (priv->temporary_report)
        g_remove (priv->temporary_report);
    g_clear_pointer (&priv->temporary_report, g_free);
    g_clear_pointer (&priv->temporary_report_uri, g_free);
    g_clear_pointer (&priv->pending_anchor, g_free);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT, default_zoom_pref,
                                 reinterpret_cast<gpointer> (
                                     impl_webview2_default_zoom_changed), object);
    G_OBJECT_CLASS (gnc_html_webview2_parent_class)->dispose (object);
}

static void
gnc_html_webview2_finalize (GObject *object)
{
    auto self = GNC_HTML_WEBVIEW2 (object);
    if (self->priv)
        self->priv->~GncHtmlWebView2Private ();
    G_OBJECT_CLASS (gnc_html_webview2_parent_class)->finalize (object);
}

static void
gnc_html_webview2_init (GncHtmlWebView2 *self)
{
    const auto base = *GNC_HTML (self)->priv;
    auto private_data = static_cast<GncHtmlWebView2Private *> (
        g_realloc (GNC_HTML (self)->priv, sizeof (GncHtmlWebView2Private)));
    new (private_data) GncHtmlWebView2Private ();
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
        private_data->view, "realize", G_CALLBACK (webview2_view_realize), self));
    private_data->widget_lifecycle.add_signal (G_OBJECT (private_data->view),
        g_signal_connect_after (private_data->view, "map",
                                G_CALLBACK (webview2_view_mapping_changed), self));
    private_data->widget_lifecycle.add_signal (G_OBJECT (private_data->view),
        g_signal_connect_after (private_data->view, "unmap",
                                G_CALLBACK (webview2_view_mapping_changed), self));
    private_data->widget_lifecycle.set_tick_callback (gtk_widget_add_tick_callback (
        private_data->view, webview2_tick, self, nullptr));
    auto focus = gtk_event_controller_focus_new ();
    private_data->widget_lifecycle.add_signal (G_OBJECT (focus), g_signal_connect (
        focus, "enter", G_CALLBACK (webview2_focus_enter), self));
    gtk_widget_add_controller (private_data->view, focus);
    private_data->widget_lifecycle.add_controller (focus);
    auto click = GTK_GESTURE_CLICK (gtk_gesture_click_new ());
    private_data->widget_lifecycle.add_signal (G_OBJECT (click), g_signal_connect (
        click, "pressed", G_CALLBACK (webview2_click_pressed), self));
    private_data->widget_lifecycle.add_signal (G_OBJECT (click), g_signal_connect (
        click, "released", G_CALLBACK (webview2_click_released), self));
    gtk_widget_add_controller (private_data->view, GTK_EVENT_CONTROLLER (click));
    private_data->widget_lifecycle.add_controller (GTK_EVENT_CONTROLLER (click));
    auto motion = gtk_event_controller_motion_new ();
    private_data->widget_lifecycle.add_signal (G_OBJECT (motion), g_signal_connect (
        motion, "motion", G_CALLBACK (webview2_motion), self));
    gtk_widget_add_controller (private_data->view, motion);
    private_data->widget_lifecycle.add_controller (motion);
    auto scroll = gtk_event_controller_scroll_new (GTK_EVENT_CONTROLLER_SCROLL_BOTH_AXES);
    private_data->widget_lifecycle.add_signal (G_OBJECT (scroll), g_signal_connect (
        scroll, "scroll", G_CALLBACK (webview2_scroll), self));
    gtk_widget_add_controller (private_data->view, scroll);
    private_data->widget_lifecycle.add_controller (scroll);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT, default_zoom_pref,
                           reinterpret_cast<gpointer> (impl_webview2_default_zoom_changed), self);
}

static void
gnc_html_webview2_class_init (GncHtmlWebView2Class *klass)
{
    auto object_class = G_OBJECT_CLASS (klass);
    auto html_class = GNC_HTML_CLASS (klass);
    object_class->dispose = gnc_html_webview2_dispose;
    object_class->finalize = gnc_html_webview2_finalize;
    html_class->show_url = impl_webview2_show_url;
    html_class->show_data = impl_webview2_show_data;
    html_class->reload = impl_webview2_reload;
    html_class->copy_to_clipboard = impl_webview2_copy_to_clipboard;
    html_class->export_to_file = impl_webview2_export_to_file;
    html_class->print = impl_webview2_print;
    html_class->cancel = impl_webview2_cancel;
    html_class->set_parent = impl_webview2_set_parent;
}

namespace
{
static void
impl_webview2_show_data (GncHtml *html, const gchar *data, int datalen)
{
    auto self = GNC_HTML_WEBVIEW2 (html);
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
    if (!impl_webview2_export_to_file (html, filename))
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
    webview2_navigate_report (self);
}

static void
impl_webview2_show_url (GncHtml *html, URLType type, const gchar *location,
                        const gchar *label, gboolean new_window)
{
    auto self = GNC_HTML_WEBVIEW2 (html);
    auto priv = priv_for (self);
    g_return_if_fail (location != nullptr);
    if (priv->base.urltype_cb && priv->base.urltype_cb (type))
        impl_webview2_cancel (html);

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
impl_webview2_reload (GncHtml *html, gboolean force_rebuild)
{
    auto self = GNC_HTML_WEBVIEW2 (html);
    auto priv = priv_for (self);
    if (force_rebuild)
    {
        if (auto current = gnc_html_history_get_current (priv->base.history))
            gnc_html_show_url (html, current->type, current->location, current->label, FALSE);
    }
    else if (priv->web_view)
        (void)priv->web_view->Reload ();
}

class CopyToClipboardCompletedHandler final
    : public CallbackBase<ICoreWebView2ExecuteScriptCompletedHandler>
{
public:
    using CallbackBase::CallbackBase;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID requested, void **object) override
    {
        return query_interface (requested, object,
                                IID_ICoreWebView2ExecuteScriptCompletedHandler);
    }

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT error, LPCWSTR result) override
    {
        if (FAILED (error))
        {
            log_hresult ("copy", error);
            return S_OK;
        }

        auto owner = self ();
        if (!owner)
            return S_OK;
        auto json_result = to_utf8 (result);
        if (!json_result)
        {
            PERR ("WebView2 returned no selection data for copy.");
            return S_OK;
        }
        auto selection = gnc_html_webview2_decode_clipboard_selection (json_result);
        g_free (json_result);
        if (selection.result == GncHtmlWebView2ClipboardResult::no_selection)
        {
            DEBUG ("WebView2 copy requested without a report selection.");
            return S_OK;
        }
        if (selection.result == GncHtmlWebView2ClipboardResult::invalid_result)
        {
            PERR ("WebView2 returned invalid selection data for copy.");
            return S_OK;
        }

        auto priv = priv_for (owner.get ());
        if (!priv->view)
        {
            PERR ("WebView2 cannot copy a report selection without a GTK widget.");
            return S_OK;
        }
        auto clipboard = gtk_widget_get_clipboard (priv->view);
        if (!clipboard)
        {
            PERR ("GTK did not provide a clipboard for the WebView2 report.");
            return S_OK;
        }
        auto provider = gnc_html_webview2_clipboard_content_provider (selection);
        if (!provider)
        {
            PERR ("WebView2 could not create a content provider for report copy.");
            return S_OK;
        }
        const auto copied = gdk_clipboard_set_content (clipboard, provider);
        g_object_unref (provider);
        if (!copied)
            PERR ("GTK could not set the WebView2 report clipboard content.");
        return S_OK;
    }
};

static void
impl_webview2_copy_to_clipboard (GncHtml *html)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (html));
    if (!priv->view || !priv->web_view)
    {
        PERR ("WebView2 cannot copy a report selection before its view is ready.");
        return;
    }
    if (!gtk_widget_grab_focus (priv->view))
        DEBUG ("WebView2 report view could not take focus for copy.");
    if (priv->controller)
    {
        const auto focus_result = priv->controller->MoveFocus (
            COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC);
        if (FAILED (focus_result))
            log_hresult ("focus for copy", focus_result);
    }

    /* Encode both selection forms as ASCII-hex fields for a small, exact JSON
     * result contract. The host advertises the decoded text and HTML together. */
    static constexpr wchar_t copy_selection_script[] =
        L"(() => { const selection = window.getSelection(); if (!selection || "
        L"selection.rangeCount === 0 || selection.isCollapsed) return null; const text = "
        L"selection.toString(); if (!text) return null; const container = document.createElement('div'); "
        L"for (let index = 0; index < selection.rangeCount; ++index) container.append("
        L"selection.getRangeAt(index).cloneContents()); const html = container.innerHTML; if (!html) "
        L"return null; const encode = value => Array.from(new TextEncoder().encode(value), byte => "
        L"byte.toString(16).padStart(2, '0')).join(''); return `${encode(text)}:${encode(html)}`; })()";
    auto handler = new CopyToClipboardCompletedHandler (GNC_HTML_WEBVIEW2 (html),
                                                         priv->loader);
    const auto result = priv->web_view->ExecuteScript (copy_selection_script, handler);
    handler->Release ();
    if (FAILED (result))
        log_hresult ("copy request", result);
}

static gboolean
impl_webview2_export_to_file (GncHtml *html, const gchar *filepath)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (html));
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
impl_webview2_print (GncHtml *html, const gchar *jobname, gboolean export_pdf)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (html));

    if (!priv->web_view)
        return;
    if (export_pdf)
    {
        ComPtr<ICoreWebView2_7> printable_view;
        auto output_path = to_utf16 (jobname);

        if (output_path.empty ())
        {
            PERR ("WebView2 cannot export a PDF without a local output path.");
            return;
        }
        if (FAILED (priv->web_view->QueryInterface (IID_ICoreWebView2_7,
                                                     reinterpret_cast<void **> (
                                                         printable_view.GetAddressOf ()))))
        {
            PERR ("The installed WebView2 Runtime does not support PDF export.");
            return;
        }

        auto handler = new PrintToPdfCompletedHandler (GNC_HTML_WEBVIEW2 (html),
                                                        priv->loader);
        const auto result = printable_view->PrintToPdf (output_path.c_str (), nullptr,
                                                         handler);
        handler->Release ();
        if (FAILED (result))
            log_hresult ("PDF export start", result);
        return;
    }

    ComPtr<ICoreWebView2_16> printable_view;
    if (FAILED (priv->web_view->QueryInterface (IID_ICoreWebView2_16,
                                                 reinterpret_cast<void **> (
                                                     printable_view.GetAddressOf ()))) ||
        FAILED (printable_view->ShowPrintUI (COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM)))
        PERR ("The installed WebView2 Runtime does not support native report printing.");
}
static void
impl_webview2_cancel (GncHtml *html)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (html));
    if (priv->web_view)
        (void)priv->web_view->Stop ();
    g_hash_table_remove_all (priv->base.request_info);
}

static void
impl_webview2_set_parent (GncHtml *html, GtkWindow *parent)
{
    priv_for (GNC_HTML_WEBVIEW2 (html))->base.parent = GTK_WIDGET (parent);
}

static void
impl_webview2_default_zoom_changed (gpointer, gchar *, gpointer user_data)
{
    auto priv = priv_for (GNC_HTML_WEBVIEW2 (user_data));
    if (priv->controller)
        (void)priv->controller->put_ZoomFactor (
            gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT, default_zoom_pref));
}

} // namespace

GncHtml *
gnc_html_webview2_new (void) noexcept
{
    return GNC_HTML (g_object_new (GNC_TYPE_HTML_WEBVIEW2, nullptr));
}

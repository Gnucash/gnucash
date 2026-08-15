/********************************************************************
 * gnc-html-webview2.cpp -- gnucash report renderer using WebView2  *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
 * Copyright (C) 2026 John Ralls <jralls@ceridwen.us>                *
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

/* This backend embeds Microsoft Edge WebView2 as a native child HWND
 * parented to a plain GTK widget ("socket") that has been forced to own
 * its own native window via gdk_window_ensure_native(). WebView2's
 * controller bounds are kept in sync with the socket's GTK allocation,
 * so the browser paints itself as if it were the socket's content.
 *
 * WebView2 environment/controller creation is asynchronous and driven
 * by COM callbacks dispatched through the normal Win32 message queue,
 * which GDK's win32 backend already pumps as part of the GLib main
 * loop -- no extra plumbing is required as long as everything is
 * created on the GTK/UI thread, which it is here.
 *
 * The completion-handler classes below implement the relevant WebView2
 * callback interfaces by hand instead of via Microsoft::WRL, to avoid
 * depending on the wrl (Windows Runtime Library) headers being present
 * in the MinGW toolchain.
 * They answer QueryInterface() unconditionally with their own vtable,
 * which is safe here because each class implements exactly one
 * interface (besides IUnknown) and is only ever handed to WebView2
 * APIs that call Invoke()/AddRef()/Release() directly through the
 * typed pointer they were given -- never re-queried for another
 * interface.
 */

#include <config.h>

#include <windows.h>
#include <WebView2.h>

#include <gtk/gtk.h>
#include <gdk/gdkwin32.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>
#include <atomic>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-webkit.hpp"
#include "gnc-html-history.h"

G_DEFINE_TYPE(GncHtmlWebview2, gnc_html_webview2, GNC_TYPE_HTML)

static void gnc_html_webview2_dispose (GObject* obj);
static void gnc_html_webview2_finalize (GObject* obj);

#define GNC_HTML_WEBVIEW2_GET_PRIVATE(o) (GNC_HTML_WEBVIEW2(o)->priv)

#include "gnc-html-webview2-p.hpp"

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

#define GNC_PREF_RPT_DFLT_ZOOM "default-zoom"

static void show_url (GncHtml* self, URLType type,
                                    const gchar* location, const gchar* label,
                                    gboolean new_window);
static void show_data (GncHtml* self, const gchar* data, int datalen);
static void reload (GncHtml* self, gboolean force_rebuild);
static void copy_to_clipboard (GncHtml* self);
static gboolean export_to_file (GncHtml* self, const gchar* filepath);
static void print (GncHtml* self, const gchar* jobname);
static void cancel (GncHtml* self);
static void set_parent (GncHtml* self, GtkWindow* parent);
static void default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data);
static void navigate_uri (GncHtmlWebview2* self, const gchar* uri);
static void navigate_string (GncHtmlWebview2* self, const gchar* html);
static gboolean load_to_stream (GncHtmlWebview2* self, URLType type,
                                const gchar* location, const gchar* label);

// Minimal hand-rolled COM completion/event-handler objects.

template <typename Interface>
class GncWebView2Sink : public Interface
{
public:
    explicit GncWebView2Sink (GncHtmlWebview2* html)
        : m_html (GNC_HTML_WEBVIEW2 (g_object_ref (G_OBJECT (html)))) {}

    /* Release() below deletes through this base pointer regardless of
     * which concrete handler subclass it is. -- Claude Code, 2026  */
    virtual ~GncWebView2Sink () = default;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID, void** ppv) override
    {
        if (!ppv)
            return E_POINTER;
        *ppv = static_cast<Interface*> (this);
        AddRef ();
        return S_OK;
    }

    ULONG STDMETHODCALLTYPE AddRef () override
    {
        return ++m_refs;
    }

    ULONG STDMETHODCALLTYPE Release () override
    {
        auto refs = --m_refs;
        if (refs == 0)
        {
            g_object_unref (m_html);
            delete this;
        }
        return refs;
    }

protected:
    GncHtmlWebview2* m_html;

private:
    std::atomic<ULONG> m_refs {1};
};

class GncWebView2EnvironmentHandler final
    : public GncWebView2Sink<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>
{
public:
    using GncWebView2Sink::GncWebView2Sink;

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT result, ICoreWebView2Environment* env) override;
};

class GncWebView2ControllerHandler final
    : public GncWebView2Sink<ICoreWebView2CreateCoreWebView2ControllerCompletedHandler>
{
public:
    using GncWebView2Sink::GncWebView2Sink;

    HRESULT STDMETHODCALLTYPE Invoke (HRESULT result, ICoreWebView2Controller* controller) override;
};

class GncWebView2NavStartingHandler final
    : public GncWebView2Sink<ICoreWebView2NavigationStartingEventHandler>
{
public:
    using GncWebView2Sink::GncWebView2Sink;

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2* sender,
                                      ICoreWebView2NavigationStartingEventArgs* args) override;
};

class GncWebView2NewWindowHandler final
    : public GncWebView2Sink<ICoreWebView2NewWindowRequestedEventHandler>
{
public:
    using GncWebView2Sink::GncWebView2Sink;

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2* sender,
                                      ICoreWebView2NewWindowRequestedEventArgs* args) override;
};

class GncWebView2ContextMenuHandler final
    : public GncWebView2Sink<ICoreWebView2ContextMenuRequestedEventHandler>
{
public:
    using GncWebView2Sink::GncWebView2Sink;

    HRESULT STDMETHODCALLTYPE Invoke (ICoreWebView2* sender,
                                      ICoreWebView2ContextMenuRequestedEventArgs* args) override;
};

/* Minimal hand-rolled implementation of
 * ICoreWebView2EnvironmentOptions (verified against WebView2.h
 * provided by MSYS2 mingw-w64-webview2-loader package version
 * 1.0.3912.50-1: get/put pairs for AdditionalBrowserArguments,
 * Language, TargetCompatibleBrowserVersion,
 * AllowSingleSignOnUsingOSPrimaryAccount -- nothing else). Used
 * instead of Microsoft's own WebView2EnvironmentOptions.h
 * implementation class, which the MSYS2 webview2-loader package
 * doesn't ship, and instead of the
 * WEBVIEW2_ADDITIONAL_BROWSER_ARGUMENTS environment-variable
 * fallback, which turned out not to be honored by this loader/runtime
 * combination. Only AdditionalBrowserArguments is actually exercised;
 * the rest are implemented for interface completeness.
 *  -- Claude Code, 2026 */
class GncWebView2EnvironmentOptions final : public ICoreWebView2EnvironmentOptions
{
public:
    GncWebView2EnvironmentOptions () = default;

    HRESULT STDMETHODCALLTYPE QueryInterface (REFIID, void** ppv) override
    {
        if (!ppv)
            return E_POINTER;
        *ppv = static_cast<ICoreWebView2EnvironmentOptions*> (this);
        AddRef ();
        return S_OK;
    }

    ULONG STDMETHODCALLTYPE AddRef () override
    {
        return ++m_refs;
    }

    ULONG STDMETHODCALLTYPE Release () override
    {
        auto refs = --m_refs;
        if (refs == 0)
            delete this;
        return refs;
    }

    HRESULT STDMETHODCALLTYPE get_AdditionalBrowserArguments (LPWSTR* value) override
    {
        return dup_out (m_additional_browser_arguments, value);
    }
    HRESULT STDMETHODCALLTYPE put_AdditionalBrowserArguments (LPCWSTR value) override
    {
        return set_in (&m_additional_browser_arguments, value);
    }
    HRESULT STDMETHODCALLTYPE get_Language (LPWSTR* value) override
    {
        return dup_out (m_language, value);
    }
    HRESULT STDMETHODCALLTYPE put_Language (LPCWSTR value) override
    {
        return set_in (&m_language, value);
    }
    HRESULT STDMETHODCALLTYPE get_TargetCompatibleBrowserVersion (LPWSTR* value) override
    {
        return dup_out (m_target_compatible_browser_version, value);
    }
    HRESULT STDMETHODCALLTYPE put_TargetCompatibleBrowserVersion (LPCWSTR value) override
    {
        return set_in (&m_target_compatible_browser_version, value);
    }
    HRESULT STDMETHODCALLTYPE get_AllowSingleSignOnUsingOSPrimaryAccount (BOOL* allow) override
    {
        if (!allow)
            return E_POINTER;
        *allow = m_allow_sso;
        return S_OK;
    }
    HRESULT STDMETHODCALLTYPE put_AllowSingleSignOnUsingOSPrimaryAccount (BOOL allow) override
    {
        m_allow_sso = allow;
        return S_OK;
    }

private:
    ~GncWebView2EnvironmentOptions ()
    {
        g_free (m_additional_browser_arguments);
        g_free (m_language);
        g_free (m_target_compatible_browser_version);
    }

    /* value is already UTF-16 (LPCWSTR); gunichar2 and wchar_t are both
     * 16 bits wide on Windows, so this is a plain length+copy, not a
     * UTF-8/UTF-16 conversion.  -- Claude Code, 2026 */
    static HRESULT set_in (gunichar2** dest, LPCWSTR value)
    {
        g_free (*dest);
        *dest = nullptr;
        if (value)
        {
            size_t len = 0;
            while (value[len])
                ++len;
            auto buf = static_cast<gunichar2*> (g_malloc ((len + 1) * sizeof (gunichar2)));
            memcpy (buf, value, len * sizeof (gunichar2));
            buf[len] = 0;
            *dest = buf;
        }
        return S_OK;
    }

    /* Callers of get_* own the returned string and must CoTaskMemFree()
     * it, per COM convention. -- Claude Code, 2026 */
    static HRESULT dup_out (const gunichar2* src, LPWSTR* out)
    {
        if (!out)
            return E_POINTER;
        size_t len = 0;
        if (src)
            while (src[len])
                ++len;
        auto buf = static_cast<gunichar2*> (CoTaskMemAlloc ((len + 1) * sizeof (gunichar2)));
        if (!buf)
            return E_OUTOFMEMORY;
        if (src)
            memcpy (buf, src, len * sizeof (gunichar2));
        buf[len] = 0;
        *out = reinterpret_cast<LPWSTR> (buf);
        return S_OK;
    }

    std::atomic<ULONG> m_refs {1};
    gunichar2* m_additional_browser_arguments = nullptr;
    gunichar2* m_language = nullptr;
    gunichar2* m_target_compatible_browser_version = nullptr;
    BOOL m_allow_sso = FALSE;
};

// *****************************************************************************

static void
ensure_com_initialized ()
{
    static bool done = false;
    if (done)
        return;
    done = true;

    HRESULT hr = CoInitializeEx (nullptr, COINIT_APARTMENTTHREADED);
    if (FAILED (hr) && hr != RPC_E_CHANGED_MODE)
        PWARN ("CoInitializeEx failed: 0x%08lx", (unsigned long) hr);
    else if (hr == RPC_E_CHANGED_MODE)
        PWARN ("COM was already initialized as multi-threaded; "
               "WebView2 requires a single-threaded apartment on the UI thread.");
}

/* The MSYS2 webview2-loader package ships WebView2Loader.dll and
 * WebView2.h but no import library, so CreateCoreWebView2EnvironmentWithOptions
 * -- the loader DLL's only entry point we need -- is resolved at
 * runtime instead of being linked directly. This also means a missing
 * or incompatible WebView2Loader.dll fails gracefully at first use
 * rather than preventing the executable from starting at all.
 * -- Claude Code, 2026 */
using CreateCoreWebView2EnvironmentWithOptionsFn = HRESULT (STDAPICALLTYPE*)(
    PCWSTR browserExecutableFolder,
    PCWSTR userDataFolder,
    ICoreWebView2EnvironmentOptions* environmentOptions,
    ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler* environmentCreatedHandler);

static CreateCoreWebView2EnvironmentWithOptionsFn
get_create_environment_fn ()
{
    static CreateCoreWebView2EnvironmentWithOptionsFn fn = nullptr;
    static bool tried = false;

    if (!tried)
    {
        tried = true;
        HMODULE mod = LoadLibraryW (L"WebView2Loader.dll");
        if (mod)
            fn = reinterpret_cast<CreateCoreWebView2EnvironmentWithOptionsFn> (
                GetProcAddress (mod, "CreateCoreWebView2EnvironmentWithOptions"));
        if (!fn)
            PERR ("Could not resolve CreateCoreWebView2EnvironmentWithOptions "
                  "from WebView2Loader.dll (module %s)", mod ? "loaded" : "not found");
    }
    return fn;
}

static void
flush_pending (GncHtmlWebview2* self)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);
    auto kind = priv->pending_kind;
    gchar* payload = priv->pending_payload;

    priv->pending_kind = PENDING_NONE;
    priv->pending_payload = nullptr;

    if (kind == PENDING_URI)
        navigate_uri (self, payload);
    else if (kind == PENDING_STRING)
        navigate_string (self, payload);

    g_free (payload);
}

static void
environment_created (GncHtmlWebview2* self, HRESULT result,
                                   ICoreWebView2Environment* env)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);
    priv->environment_creating = FALSE;

    if (priv->disposed)
        return;

    if (FAILED (result) || !env)
    {
        PERR ("WebView2 environment creation failed: 0x%08lx", (unsigned long) result);
        return;
    }

    priv->environment = env;
    env->AddRef ();

    if (!priv->socket || !gtk_widget_get_window (priv->socket))
    {
        PERR ("WebView2 socket widget was destroyed before the environment was ready");
        return;
    }

    HWND hwnd = static_cast<HWND> (GDK_WINDOW_HWND (gtk_widget_get_window (priv->socket)));
    auto handler = new GncWebView2ControllerHandler (self);
    HRESULT hr = env->CreateCoreWebView2Controller (hwnd, handler);
    if (FAILED (hr))
    {
        PERR ("CreateCoreWebView2Controller failed: 0x%08lx", (unsigned long) hr);
        handler->Release ();
    }
}

HRESULT STDMETHODCALLTYPE
GncWebView2EnvironmentHandler::Invoke (HRESULT result, ICoreWebView2Environment* env)
{
    environment_created (m_html, result, env);
    return S_OK;
}

static void
controller_created (GncHtmlWebview2* self, HRESULT result,
                                  ICoreWebView2Controller* controller)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);

    if (priv->disposed)
    {
        if (controller)
            controller->Close ();
        return;
    }

    if (FAILED (result) || !controller)
    {
        PERR ("WebView2 controller creation failed: 0x%08lx", (unsigned long) result);
        return;
    }

    priv->controller = controller;
    controller->AddRef ();
    controller->get_CoreWebView2 (&priv->webview);

    GtkAllocation alloc;
    gtk_widget_get_allocation (priv->socket, &alloc);
    RECT bounds { 0, 0, alloc.width, alloc.height };
    priv->controller->put_Bounds (bounds);
    priv->controller->put_IsVisible (gtk_widget_get_mapped (priv->socket));

    gdouble zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_RPT_DFLT_ZOOM);
    priv->controller->put_ZoomFactor (zoom);

    if (priv->webview)
    {
        auto nav_handler = new GncWebView2NavStartingHandler (self);
        priv->webview->add_NavigationStarting (nav_handler, &priv->nav_starting_token);
        priv->has_nav_starting_token = TRUE;
        nav_handler->Release ();

        auto new_window_handler = new GncWebView2NewWindowHandler (self);
        priv->webview->add_NewWindowRequested (new_window_handler, &priv->new_window_token);
        priv->has_new_window_token = TRUE;
        new_window_handler->Release ();

        /* Only available on newer WebView2 runtimes; if the QI fails,
         * the context menu is simply never customized (see
         * context_menu_requested() below). */
        HRESULT hr11 = priv->webview->QueryInterface (
            IID_ICoreWebView2_11, reinterpret_cast<void**> (&priv->webview11));
        if (SUCCEEDED (hr11) && priv->webview11)
        {
            auto menu_handler = new GncWebView2ContextMenuHandler (self);
            HRESULT hr_add = priv->webview11->add_ContextMenuRequested (
                menu_handler, &priv->context_menu_token);
            priv->has_context_menu_token = SUCCEEDED (hr_add);
            menu_handler->Release ();
        }
    }

    flush_pending (self);
}

HRESULT STDMETHODCALLTYPE
GncWebView2ControllerHandler::Invoke (HRESULT result, ICoreWebView2Controller* controller)
{
    controller_created (m_html, result, controller);
    return S_OK;
}

static void
navigation_starting (GncHtmlWebview2* self,
                                   ICoreWebView2NavigationStartingEventArgs* args)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);

    /* Navigations we kicked off ourselves (loading a report, showing an
     * error page) must not be re-routed through gnc_html_show_url(). Only
     * navigations we didn't initiate -- in practice, the user clicking a
     * link inside the rendered report -- should be intercepted.
     *  -- Claude Code, 2026 */
    if (priv->navigating_internally)
    {
        priv->navigating_internally = FALSE;
        return;
    }

    LPWSTR wuri = nullptr;
    if (FAILED (args->get_Uri (&wuri)) || !wuri)
        return;

    gchar* uri = g_utf16_to_utf8 (reinterpret_cast<const gunichar2*> (wuri), -1,
                                  nullptr, nullptr, nullptr);
    CoTaskMemFree (wuri);
    if (!uri)
        return;

    gchar* location = nullptr;
    gchar* label = nullptr;
    URLType scheme = gnc_html_parse_url (GNC_HTML (self), uri, &location, &label);

    if (g_strcmp0 (scheme, URL_TYPE_FILE) != 0)
    {
        args->put_Cancel (TRUE);
        show_url (GNC_HTML (self), scheme, location, label, FALSE);
    }

    g_free (location);
    g_free (label);
    g_free (uri);
}

HRESULT STDMETHODCALLTYPE
GncWebView2NavStartingHandler::Invoke (ICoreWebView2* /*sender*/,
                                       ICoreWebView2NavigationStartingEventArgs* args)
{
    navigation_starting (m_html, args);
    return S_OK;
}

/* Fires when the user picks "Open link in new window" from the context
 * menu (or a page navigates with a new-window target). Left unhandled,
 * WebView2's default action is to pop up a bare native WebView2 window
 * that can't do anything useful with our gnc-register:/gnc-report:/etc.
 * links. Always claim the event and decode the URI the same way a
 * plain click does, but with new_window forced on so link types that
 * can safely honor it (e.g. account/register links) open a genuine
 * separate GnuCash window as the user asked. Report-type links can't
 * safely honor it -- see show_url()'s handling of URL_TYPE_REPORT --
 * so they just navigate in place, same as a plain click; the context
 * menu item is greyed out for those anyway, see
 * context_menu_requested() below. -- Claude Code, 2026 */
static void
new_window_requested (GncHtmlWebview2* self,
                                    ICoreWebView2NewWindowRequestedEventArgs* args)
{
    args->put_Handled (TRUE);

    LPWSTR wuri = nullptr;
    if (FAILED (args->get_Uri (&wuri)) || !wuri)
        return;

    gchar* uri = g_utf16_to_utf8 (reinterpret_cast<const gunichar2*> (wuri), -1,
                                  nullptr, nullptr, nullptr);
    CoTaskMemFree (wuri);
    if (!uri)
        return;

    gchar* location = nullptr;
    gchar* label = nullptr;
    URLType scheme = gnc_html_parse_url (GNC_HTML (self), uri, &location, &label);

    show_url (GNC_HTML (self), scheme, location, label, TRUE);

    g_free (location);
    g_free (label);
    g_free (uri);
}

HRESULT STDMETHODCALLTYPE
GncWebView2NewWindowHandler::Invoke (ICoreWebView2* /*sender*/,
                                     ICoreWebView2NewWindowRequestedEventArgs* args)
{
    new_window_requested (m_html, args);
    return S_OK;
}

/* "Open link in new window" can't be honored for report-type links
 * (see show_url()'s handling of URL_TYPE_REPORT and
 * new_window_requested() above), so remove that item from the context
 * menu when it's raised on one, rather than offering an action that
 * silently does nothing when picked. -- Claude Code, 2026 */
static void
context_menu_requested (GncHtmlWebview2* self,
                                      ICoreWebView2ContextMenuRequestedEventArgs* args)
{
    ICoreWebView2ContextMenuTarget* target = nullptr;
    if (FAILED (args->get_ContextMenuTarget (&target)) || !target)
        return;

    BOOL has_link = FALSE;
    LPWSTR wuri = nullptr;
    HRESULT hr = target->get_HasLinkUri (&has_link);
    if (FAILED (hr) || !has_link || FAILED (target->get_LinkUri (&wuri)) || !wuri)
    {
        target->Release ();
        return;
    }
    target->Release ();

    gchar* uri = g_utf16_to_utf8 (reinterpret_cast<const gunichar2*> (wuri), -1,
                                  nullptr, nullptr, nullptr);
    CoTaskMemFree (wuri);
    if (!uri)
        return;

    gchar* location = nullptr;
    gchar* label = nullptr;
    URLType scheme = gnc_html_parse_url (GNC_HTML (self), uri, &location, &label);
    g_free (uri);
    g_free (location);
    g_free (label);

    if (g_strcmp0 (scheme, URL_TYPE_REPORT) != 0)
        return;

    ICoreWebView2ContextMenuItemCollection* items = nullptr;
    if (FAILED (args->get_MenuItems (&items)) || !items)
        return;

    UINT32 count = 0;
    items->get_Count (&count);
    /* Loop index only advances when the current item is kept --
     * RemoveValueAtIndex() shifts everything after it down by one, so
     * re-examining the same index picks up what used to be next. */
    for (UINT32 i = 0; i < count; )
    {
        ICoreWebView2ContextMenuItem* item = nullptr;
        if (FAILED (items->GetValueAtIndex (i, &item)) || !item)
        {
            ++i;
            continue;
        }

        bool removed = false;
        LPWSTR wname = nullptr;
        if (SUCCEEDED (item->get_Name (&wname)) && wname)
        {
            gchar* name = g_utf16_to_utf8 (reinterpret_cast<const gunichar2*> (wname), -1,
                                           nullptr, nullptr, nullptr);
            CoTaskMemFree (wname);
            /* "openLinkInNewWindow" is the documented stable Name for
             * this default item; also remove "openLinkInNewTab" in
             * case a future WebView2 runtime offers it here too.
             * Disabling via put_IsEnabled(FALSE) is the documented way
             * to grey out an item, and the SDK reports it succeeding
             * (verified: get_IsEnabled() reads back FALSE afterward),
             * but this WebView2 build doesn't actually reflect that in
             * the rendered menu for this particular built-in command --
             * removing the item outright is more fundamental and does
             * take effect (verified). */
            if (!g_strcmp0 (name, "openLinkInNewWindow") ||
                !g_strcmp0 (name, "openLinkInNewTab"))
            {
                items->RemoveValueAtIndex (i);
                --count;
                removed = true;
            }
            g_free (name);
        }
        item->Release ();
        if (!removed)
            ++i;
    }
    items->Release ();
}

HRESULT STDMETHODCALLTYPE
GncWebView2ContextMenuHandler::Invoke (ICoreWebView2* /*sender*/,
                                       ICoreWebView2ContextMenuRequestedEventArgs* args)
{
    context_menu_requested (m_html, args);
    return S_OK;
}

// *****************************************************************************

static void
navigate_uri (GncHtmlWebview2* self, const gchar* uri)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);

    if (!priv->webview)
    {
        g_free (priv->pending_payload);
        priv->pending_payload = g_strdup (uri);
        priv->pending_kind = PENDING_URI;
        return;
    }

    gunichar2* wuri = g_utf8_to_utf16 (uri, -1, nullptr, nullptr, nullptr);
    priv->navigating_internally = TRUE;
    priv->webview->Navigate (reinterpret_cast<LPCWSTR> (wuri));
    g_free (wuri);
}

static void
navigate_string (GncHtmlWebview2* self, const gchar* html)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);

    if (!priv->webview)
    {
        g_free (priv->pending_payload);
        priv->pending_payload = g_strdup (html);
        priv->pending_kind = PENDING_STRING;
        return;
    }

    gunichar2* whtml = g_utf8_to_utf16 (html, -1, nullptr, nullptr, nullptr);
    priv->navigating_internally = TRUE;
    priv->webview->NavigateToString (reinterpret_cast<LPCWSTR> (whtml));
    g_free (whtml);
}

// *****************************************************************************

static void
socket_realize_cb (GtkWidget* socket, gpointer user_data)
{
    auto self = GNC_HTML_WEBVIEW2 (user_data);
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);

    if (priv->environment || priv->environment_creating)
        return;

    auto create_env_fn = get_create_environment_fn ();
    if (!create_env_fn)
    {
        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                          _("The Microsoft Edge WebView2 Runtime is required to display "
                            "reports, but its loader library (WebView2Loader.dll) could "
                            "not be found or is invalid."));
        return;
    }

    gdk_window_ensure_native (gtk_widget_get_window (socket));

    ensure_com_initialized ();
    priv->environment_creating = TRUE;

    gchar* user_data_dir = g_build_filename (g_get_user_data_dir (), "gnucash",
                                             "webview2", (gchar*)nullptr);
    gunichar2* wuser_data_dir = g_utf8_to_utf16 (user_data_dir, -1, nullptr, nullptr, nullptr);
    g_free (user_data_dir);
    /* A non-null ICoreWebView2EnvironmentOptions here reliably crashes
     * inside WebView2Loader.dll itself (verified with gdb: it reads
     * back our AdditionalBrowserArguments string correctly, then faults
     * writing through a null destination pointer while copying it into
     * its own internal buffer) with the MSYS2 mingw-w64-ucrt-x86_64-webview2-loader
     * package (1.0.3912.50) -- looks like a real bug in that build, not
     * something fixable from the caller side. Passing nullptr avoids it.
     *  -- Claude Code, 2026 */
    auto handler = new GncWebView2EnvironmentHandler (self);
    HRESULT hr = create_env_fn (
        nullptr, reinterpret_cast<LPCWSTR> (wuser_data_dir), nullptr, handler);
    g_free (wuser_data_dir);

    if (FAILED (hr))
    {
        PERR ("CreateCoreWebView2EnvironmentWithOptions failed: 0x%08lx", (unsigned long) hr);
        handler->Release ();
        priv->environment_creating = FALSE;
    }
}

static void
socket_size_allocate_cb (GtkWidget* socket, GtkAllocation* allocation, gpointer user_data)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (GNC_HTML_WEBVIEW2 (user_data));
    if (priv->controller)
    {
        RECT bounds { 0, 0, allocation->width, allocation->height };
        priv->controller->put_Bounds (bounds);
    }
}

static void
socket_map_cb (GtkWidget* socket, gpointer user_data)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (GNC_HTML_WEBVIEW2 (user_data));
    if (priv->controller)
        priv->controller->put_IsVisible (TRUE);
}

static void
socket_unmap_cb (GtkWidget* socket, gpointer user_data)
{
    auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (GNC_HTML_WEBVIEW2 (user_data));
    if (priv->controller)
        priv->controller->put_IsVisible (FALSE);
}

static void
gnc_html_webview2_init (GncHtmlWebview2* self)
{
     const gpointer p = g_realloc (GNC_HTML(self)->priv, sizeof(GncHtmlWebview2Private));
     auto new_priv = reinterpret_cast<GncHtmlWebview2Private *>(p);
     auto priv = self->priv = new_priv;
     GNC_HTML(self)->priv = (GncHtmlPrivate*)priv;

     priv->html_string = nullptr;
     priv->environment = nullptr;
     priv->controller = nullptr;
     priv->webview = nullptr;
     priv->nav_starting_token = EventRegistrationToken {};
     priv->has_nav_starting_token = FALSE;
     priv->new_window_token = EventRegistrationToken {};
     priv->has_new_window_token = FALSE;
     priv->webview11 = nullptr;
     priv->context_menu_token = EventRegistrationToken {};
     priv->has_context_menu_token = FALSE;
     priv->environment_creating = FALSE;
     priv->navigating_internally = FALSE;
     priv->disposed = FALSE;
     priv->pending_kind = PENDING_NONE;
     priv->pending_payload = nullptr;

     /* A plain widget that owns its own native HWND once realized -- see
      * webview2_socket_realize_cb() -- which becomes the WebView2
      * controller's parent window. GtkEventBox is used rather than
      * GtkDrawingArea because nothing is drawn into it via Cairo; its
      * only job is to provide a native window for WebView2 to paint
      * into as a child HWND. */
     priv->socket = gtk_event_box_new ();
     gtk_widget_set_can_focus (priv->socket, TRUE);

     gtk_container_add (GTK_CONTAINER(priv->base.container),
                        priv->socket);

     g_object_ref_sink (priv->base.container);

     g_signal_connect (priv->socket, "realize",
                       G_CALLBACK (socket_realize_cb), self);
     g_signal_connect (priv->socket, "size-allocate",
                       G_CALLBACK (socket_size_allocate_cb), self);
     g_signal_connect (priv->socket, "map",
                       G_CALLBACK (socket_map_cb), self);
     g_signal_connect (priv->socket, "unmap",
                       G_CALLBACK (socket_unmap_cb), self);

     gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REPORT,
                            GNC_PREF_RPT_DFLT_ZOOM,
                            reinterpret_cast<gpointer>(default_zoom_changed),
                            self);

     LEAVE("retval %p", self);
}

static void
gnc_html_webview2_class_init (GncHtmlWebview2Class* klass)
{
     GObjectClass* gobject_class = G_OBJECT_CLASS(klass);
     GncHtmlClass* html_class = GNC_HTML_CLASS(klass);

     gobject_class->dispose = gnc_html_webview2_dispose;
     gobject_class->finalize = gnc_html_webview2_finalize;

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
gnc_html_webview2_dispose (GObject* obj)
{
     GncHtmlWebview2* self = GNC_HTML_WEBVIEW2(obj);
     GncHtmlWebview2Private* priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);

     priv->disposed = TRUE;

     if (priv->webview != nullptr && priv->has_nav_starting_token)
     {
          priv->webview->remove_NavigationStarting (priv->nav_starting_token);
          priv->has_nav_starting_token = FALSE;
     }

     if (priv->webview != nullptr && priv->has_new_window_token)
     {
          priv->webview->remove_NewWindowRequested (priv->new_window_token);
          priv->has_new_window_token = FALSE;
     }

     if (priv->webview11 != nullptr && priv->has_context_menu_token)
     {
          priv->webview11->remove_ContextMenuRequested (priv->context_menu_token);
          priv->has_context_menu_token = FALSE;
     }

     if (priv->webview11 != nullptr)
     {
          priv->webview11->Release();
          priv->webview11 = nullptr;
     }

     if (priv->controller != nullptr)
     {
          priv->controller->Close();
          priv->controller->Release();
          priv->controller = nullptr;
     }

     if (priv->webview != nullptr)
     {
          priv->webview->Release();
          priv->webview = nullptr;
     }

     if (priv->environment != nullptr)
     {
          priv->environment->Release();
          priv->environment = nullptr;
     }

     if (priv->socket != nullptr)
     {
          gtk_container_remove (GTK_CONTAINER(priv->base.container),
                                priv->socket);

          priv->socket = nullptr;
     }

     g_clear_pointer (&priv->html_string, g_free);
     g_clear_pointer (&priv->pending_payload, g_free);

     gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REPORT,
                                  GNC_PREF_RPT_DFLT_ZOOM,
                                  reinterpret_cast<gpointer>(default_zoom_changed),
                                  obj);

     G_OBJECT_CLASS(gnc_html_webview2_parent_class)->dispose (obj);
}

static void
gnc_html_webview2_finalize (GObject* obj)
{
     GncHtmlWebview2* self = GNC_HTML_WEBVIEW2(obj);

     self->priv = nullptr;

     G_OBJECT_CLASS(gnc_html_webview2_parent_class)->finalize (obj);
}

// *****************************************************************************

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
handle_embedded_object (GncHtmlWebview2* self, gchar* html_str)
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

     object_tag = g_strstr_len (remainder_str, -1, "<object classid=" );
     while (object_tag)
     {

          classid_start = object_tag + strlen ("<object classid=" ) + 1;
          classid_end = g_strstr_len (classid_start, -1, "\"" );
          classid_str = g_strndup (classid_start, (classid_end - classid_start));

          end_object_tag = g_strstr_len (object_tag, -1, "</object>" );
          if (end_object_tag == nullptr)
          {
               /*  Hmmm... no object end tag
                   Return the original html string because we can't properly parse it */
               g_free (classid_str);
               g_free (html_str_result);
               return g_strdup (html_str);
          }
          end_object_tag += strlen ("</object>" );
          object_contents = g_strndup (object_tag, (end_object_tag - object_tag));

          const gpointer p = g_hash_table_lookup (gnc_html_object_handlers, classid_str);
          h = reinterpret_cast<GncHTMLObjectCB>(p);
          if (h != nullptr)
          {
               (void)h (GNC_HTML(self), object_contents, &html_str_middle);
          }
          else
          {
               html_str_middle = g_strdup_printf ("No handler found for classid \"%s\"", classid_str);
          }

          html_str_start = html_str_result;
          new_chunk = g_strndup (remainder_str, (object_tag - remainder_str));
          if (!html_str_start)
               html_str_result = g_strconcat (new_chunk, html_str_middle, nullptr);
          else
               html_str_result = g_strconcat (html_str_start, new_chunk, html_str_middle, nullptr);

          g_free (html_str_start);
          g_free (new_chunk);
          g_free (html_str_middle);

          remainder_str = end_object_tag;
          object_tag = g_strstr_len (remainder_str, -1, "<object classid=" );
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

static gboolean
load_to_stream (GncHtmlWebview2* self, URLType type,
                const gchar* location, const gchar* label)
{
     gchar* fdata = nullptr;
     int fdata_len = 0;
     GncHtmlWebview2Private* priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);

     DEBUG ("type %s, location %s, label %s", type ? type : "(null)",
            location ? location : "(null)", label ? label : "(null)");

     g_return_val_if_fail (self != nullptr, FALSE);

     if (gnc_html_stream_handlers != nullptr)
     {
          const gpointer p = g_hash_table_lookup (gnc_html_stream_handlers, type);
          GncHTMLStreamCB stream_handler = reinterpret_cast<GncHTMLStreamCB>(p);
          if (stream_handler)
          {
              GncHtml *weak_html = GNC_HTML(self);

              g_object_add_weak_pointer(G_OBJECT(self),
                                        (gpointer*)(&weak_html));
              bool ok = stream_handler (location, &fdata, &fdata_len);

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

               if (ok)
               {
                    fdata = fdata ? fdata : g_strdup ("" );

                    // Look for "<object classid=" indicating the
                    // beginning of an embedded graph.  If found,
                    // handle it
                    if (g_strstr_len (fdata, -1, "<object classid=" ) != nullptr)
                    {
                         gchar *new_fdata = handle_embedded_object (self, fdata);
                         g_free (fdata);
                         fdata = new_fdata;
                    }

                    // Save a copy for export purposes
                    if (priv->html_string != nullptr)
                    {
                         g_free (priv->html_string);
                    }
                    priv->html_string = g_strdup (fdata);
                    show_data (GNC_HTML(self), fdata, strlen(fdata));
               }
               else
               {
                    fdata = fdata ? fdata :
                         g_strdup_printf (error_404_format,
                                          _(error_404_title), _(error_404_body));
                    navigate_string (self, fdata);
               }

               g_free (fdata);

               if (label)
               {
                    while (gtk_events_pending())
                    {
                         gtk_main_iteration();
                    }
                    /* No action required: WebView2 jumps to the anchor on its own. */
               }
               return TRUE;
          }
     }

     do
     {
          if (!g_strcmp0 (type, URL_TYPE_SECURE) ||
               !g_strcmp0 (type, URL_TYPE_HTTP))
          {

               if (!g_strcmp0 (type, URL_TYPE_SECURE))
               {
                    if (!https_allowed())
                    {
                        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                           _("Secure HTTP access is disabled. "
                                             "You can enable it in the Network section of "
                                             "the Preferences dialog."));
                         break;
                    }
               }

               if (!http_allowed())
               {
                   gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Network HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog."));
               }
               else
               {
                    gnc_build_url (type, location, label);
               }
          }
          else
          {
               PWARN ("load_to_stream for inappropriate type\n"
                      "\turl = '%s#%s'\n",
                      location ? location : "(null)",
                      label ? label : "(null)" );
               fdata = g_strdup_printf (error_404_format,
                                        _(error_404_title), _(error_404_body));
               navigate_string (self, fdata);
               g_free (fdata);
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
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));

     ENTER ("datalen %d, data %20.20s", datalen, data);

     /* Export the HTML to a file and load the file URI, exactly as the
      * webkit2 backend does: this avoids WebView2 refusing to load
      * embedded local-file images/CSS referenced by relative path from
      * a NavigateToString() call. */
     gchar *filename = g_build_filename(g_get_tmp_dir(), TEMPLATE_REPORT_FILE_NAME, (gchar *)nullptr);
     int fd = g_mkstemp (filename);
     export_to_file (self, filename);
     close (fd);
     gchar *uri = g_strdup_printf ("file:///%s", filename);
     g_free(filename);
     DEBUG("Loading uri '%s'", uri);
     navigate_uri (GNC_HTML_WEBVIEW2(self), uri);
     g_free (uri);

     LEAVE("");
}

static void
show_url (GncHtml* self, URLType type,
                        const gchar* location, const gchar* label,
                        gboolean new_window)
{
     GncHTMLUrlCB url_handler = nullptr;
     bool stream_loaded = false;

     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));
     g_return_if_fail (location != nullptr);

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);

     if (priv->base.urltype_cb && priv->base.urltype_cb (type))
          gnc_html_cancel (GNC_HTML (self));

     if (gnc_html_url_handlers)
     {
          const gpointer p = g_hash_table_lookup (gnc_html_url_handlers, type);
          url_handler = reinterpret_cast<GncHTMLUrlCB>(p);
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
               {
                   gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s", result.error_message);
               }
               else
               {
                    /* %s is a URL (some location somewhere). */
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), _("There was an error accessing %s."), location);
               }

               if (priv->base.load_cb)
               {
                    priv->base.load_cb (GNC_HTML(self), result.url_type,
                                        location, label, priv->base.load_cb_data);
               }
          }
          else if (result.load_to_stream)
          {
               const char *new_location = result.location ? result.location : location;
               const char *new_label = result.label ? result.label : label;
               auto hnode = gnc_html_history_node_new (result.url_type, new_location, new_label);

               gnc_html_history_append (priv->base.history, hnode);

               g_free (priv->base.base_location);
               priv->base.base_type = result.base_type;
               priv->base.base_location =
                    g_strdup (extract_base_name (result.base_type, new_location));
               DEBUG ("resetting base location to %s",
                      priv->base.base_location ? priv->base.base_location : "(null)" );

               stream_loaded = load_to_stream (GNC_HTML_WEBVIEW2(self),
                                               result.url_type,
                                               new_location, new_label);

               if (stream_loaded && priv->base.load_cb != nullptr)
               {
                    priv->base.load_cb (GNC_HTML(self), result.url_type,
                                        new_location, new_label, priv->base.load_cb_data);
               }
          }

          g_free (result.location);
          g_free (result.label);
          g_free (result.base_location);
          g_free (result.error_message);

          return;
     }

     if (g_strcmp0 (type, URL_TYPE_JUMP) == 0)
     {
          /* WebView2 jumps to the anchor on its own */
     }
     else if (g_strcmp0 (type, URL_TYPE_SECURE) == 0 ||
               g_strcmp0 (type, URL_TYPE_HTTP) == 0 ||
               g_strcmp0 (type, URL_TYPE_FILE) == 0)
     {

          do
          {
               if (g_strcmp0 (type, URL_TYPE_SECURE) == 0)
               {
                    if (!https_allowed())
                    {
                        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                           _("Secure HTTP access is disabled. "
                                             "You can enable it in the Network section of "
                                             "the Preferences dialog."));
                         break;
                    }
               }

               if (g_strcmp0 (type, URL_TYPE_HTTP) == 0)
               {
                    if (!http_allowed())
                    {
                        gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                           _("Network HTTP access is disabled. "
                                             "You can enable it in the Network section of "
                                             "the Preferences dialog."));
                         break;
                    }
               }

               priv->base.base_type = type;

               if (priv->base.base_location != nullptr)
                   g_free (priv->base.base_location);
               priv->base.base_location = extract_base_name (type, location);

               /* FIXME : handle new_window = 1 */
               gnc_html_history_append (priv->base.history,
                                        gnc_html_history_node_new (type, location, label));
               stream_loaded = load_to_stream (GNC_HTML_WEBVIEW2(self),
                                               type, location, label);

          }
          while (false);
     }
     else
     {
          PERR ("URLType %s not supported.", type);
     }

     if (stream_loaded && priv->base.load_cb != nullptr)
     {
          (priv->base.load_cb)(GNC_HTML(self), type, location, label, priv->base.load_cb_data);
     }
}

static void
reload (GncHtml* self, gboolean force_rebuild)
{
     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);

     if (force_rebuild)
     {
          gnc_html_history_node *n = gnc_html_history_get_current (priv->base.history);
          if (n != nullptr)
               gnc_html_show_url (self, n->type, n->location, n->label, 0);
     }
     else if (priv->webview)
          priv->webview->Reload();
}

GncHtml*
gnc_html_webview2_new (void) noexcept
{
     auto self = static_cast<GncHtmlWebview2*>(g_object_new (GNC_TYPE_HTML_WEBVIEW2, nullptr));
     return GNC_HTML(self);
}

static gboolean
cancel_helper(gpointer key, gpointer value, gpointer user_data)
{
     g_free(key);
     g_list_free((GList *)value);
     return TRUE;
}

static void
cancel (GncHtml* self)
{
     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);

     g_hash_table_foreach_remove (priv->base.request_info, cancel_helper, nullptr);
}

static void
copy_to_clipboard (GncHtml* self)
{
     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);
     /* WebView2 has no direct "copy selection" API; ask the page's own
      * script engine to do it instead.
      */
     if (priv->webview)
          priv->webview->ExecuteScript (L"document.execCommand('copy')", nullptr);
}

static gboolean
export_to_file (GncHtml* self, const char *filepath)
{
     g_return_val_if_fail (self != nullptr, FALSE);
     g_return_val_if_fail (GNC_IS_HTML_WEBVIEW2(self), FALSE);
     g_return_val_if_fail (filepath != nullptr, FALSE);

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);
     if (priv->html_string == nullptr)
     {
          return FALSE;
     }
     FILE *fh = g_fopen (filepath, "w" );
     if (fh != nullptr)
     {
          gint len = strlen (priv->html_string);
          gint written = fwrite (priv->html_string, 1, len, fh);
          fclose (fh);

          if (written != len)
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

/* Prints the current page.
 *
 * ShowPrintUI (the interactive OS print dialog) is declared on
 * ICoreWebView2_16; PrintToPdf is declared on ICoreWebView2_7 (verified
 * against the installed WebView2 SDK header -- these version numbers
 * are not guessed). ICoreWebView2_16 inherits from ICoreWebView2_7, so
 * a runtime new enough for ShowPrintUI also has PrintToPdf; a second,
 * lower QI tier covers runtimes new enough for PrintToPdf but too old
 * for ShowPrintUI.
 */
static void
print (GncHtml* self, const gchar* jobname)
{
     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2 (self));

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE (self);
     if (!priv->webview)
          return;

     ICoreWebView2_16* webview16 = nullptr;
     HRESULT hr = priv->webview->QueryInterface (IID_ICoreWebView2_16,
                                                  reinterpret_cast<void**> (&webview16));
     if (SUCCEEDED (hr) && webview16)
     {
          webview16->ShowPrintUI (COREWEBVIEW2_PRINT_DIALOG_KIND_SYSTEM);
          webview16->Release ();
          return;
     }

     ICoreWebView2_7* webview7 = nullptr;
     hr = priv->webview->QueryInterface (IID_ICoreWebView2_7,
                                         reinterpret_cast<void**> (&webview7));
     if (!SUCCEEDED (hr) || !webview7)
     {
          gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                            _("Your WebView2 Runtime is too old to print. "
                              "Please update it via Windows Update."));
          return;
     }

     gchar *pdf_path = g_str_has_suffix (jobname, ".pdf") ? g_strdup (jobname)
                                                           : g_strconcat (jobname, ".pdf", nullptr);
     gunichar2* wpath = g_utf8_to_utf16 (pdf_path, -1, nullptr, nullptr, nullptr);
     webview7->PrintToPdf (reinterpret_cast<LPCWSTR> (wpath), nullptr, nullptr);
     g_free (wpath);
     webview7->Release ();

     gchar *msg = g_strdup_printf (
          _("Your WebView2 Runtime is too old to show a print dialog. "
            "The report was saved as a PDF instead: %s"), pdf_path);
     gnc_info_dialog (GTK_WINDOW (priv->base.parent), "%s", msg);
     g_free (msg);
     g_free (pdf_path);
}

static void
set_parent (GncHtml* self, GtkWindow* parent)
{
     g_return_if_fail (self != nullptr);
     g_return_if_fail (GNC_IS_HTML_WEBVIEW2(self));

     auto priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);
     priv->base.parent = GTK_WIDGET(parent);
}

static void
default_zoom_changed(gpointer prefs, gchar *pref, gpointer user_data)
{
     g_return_if_fail(user_data != nullptr);

     GncHtmlWebview2* self = GNC_HTML_WEBVIEW2(user_data);
     GncHtmlWebview2Private* priv = GNC_HTML_WEBVIEW2_GET_PRIVATE(self);
     gdouble zoom = gnc_prefs_get_float (GNC_PREFS_GROUP_GENERAL_REPORT,
                                         GNC_PREF_RPT_DFLT_ZOOM);
     if (priv->controller)
          priv->controller->put_ZoomFactor (zoom);
}

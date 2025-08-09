#include "gnc-html-cef.h"
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include "include/cef_app.h"
#include "include/cef_client.h"
#include "include/cef_browser.h"
#include <unistd.h>
#include <limits.h>

class SimpleHandler : public CefClient, public CefLifeSpanHandler
{
public:
    SimpleHandler() {}
    CefRefPtr<CefLifeSpanHandler> GetLifeSpanHandler() override { return this; }
    void OnAfterCreated(CefRefPtr<CefBrowser> browser) override { m_browser = browser; }
    bool DoClose(CefRefPtr<CefBrowser>) override { return false; }
    void OnBeforeClose(CefRefPtr<CefBrowser>) override { m_browser = nullptr; }
    void LoadFile(const std::string& path) { if (m_browser) m_browser->GetMainFrame()->LoadURL(path); }

private:
    CefRefPtr<CefBrowser> m_browser;
    IMPLEMENT_REFCOUNTING(SimpleHandler);
};

class SimpleCefApp : public CefApp, public CefBrowserProcessHandler
{
public:
    SimpleCefApp() {}
    CefRefPtr<CefBrowserProcessHandler> GetBrowserProcessHandler() override { return this; }
    void OnContextInitialized() override {}
    IMPLEMENT_REFCOUNTING(SimpleCefApp);
};

static CefRefPtr<SimpleHandler> g_handler;
static CefRefPtr<SimpleCefApp> g_app;

void cef_wrapper_init(int argc, char* argv[])
{
    CefMainArgs main_args(argc, argv);
    if (auto exit_code = CefExecuteProcess(main_args, nullptr, nullptr); exit_code >= 0)
        return;

    CefSettings settings;
    settings.no_sandbox = true;
    settings.multi_threaded_message_loop = true;

    g_app = new SimpleCefApp;
    printf ("cefapp=%p\n", g_app.get());
    if (!CefInitialize(main_args, settings, g_app, nullptr))
    {
        printf ("CEF initialization failed\n");
        return;
    }
    printf ("CEF initialization success\n");
}

static GtkWidget* get_cef_window ()
{
    static GtkWidget* cef_window = nullptr;
    if (!cef_window)
    {
        cef_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_default_size(GTK_WINDOW(cef_window), 1024, 768);
        g_signal_connect(cef_window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
        gtk_widget_show_all(cef_window);
    }
    return cef_window;
}

void cef_wrapper_create_browser (const char* html_file)
{
    GtkWidget *window = get_cef_window ();
    Window xid = gdk_x11_window_get_xid(gtk_widget_get_window(window));

    CefWindowInfo window_info;
    GtkAllocation alloc;
    gtk_widget_get_allocation(window, &alloc);
    window_info.SetAsChild (xid, CefRect(0, 0, alloc.width, alloc.height));

    CefBrowserSettings browser_settings;
    g_handler = new SimpleHandler();

    CefBrowserHost::CreateBrowserSync(window_info, g_handler.get(), html_file,
                                      browser_settings, nullptr, nullptr);
}

void cef_wrapper_load_file(const char* html_file)
{
    if (g_handler) g_handler->LoadFile(html_file);
}

void cef_wrapper_shutdown()
{
    CefShutdown();
}

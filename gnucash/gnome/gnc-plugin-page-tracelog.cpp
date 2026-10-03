/********************************************************************\
 * gnc-plugin-page-tracelog.cpp : in-app viewer for the trace log   *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
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
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-plugin-page-tracelog.h"
#include "gnc-plugin-page.h"
#include "gnc-main-window.h"
#include "gnc-gobject-utils.h"
#include "gnc-html.h"
#include "gnc-html-factory.hpp"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "qoflog.h"

static QofLogModule log_module = "gnc.gui.tracelog";

typedef struct GncPluginPageTracelogPrivate
{
    GncHtml *html;
    GtkContainer *container;
} GncPluginPageTracelogPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageTracelog, gnc_plugin_page_tracelog, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(o)  \
   ((GncPluginPageTracelogPrivate*)gnc_plugin_page_tracelog_get_instance_private ((GncPluginPageTracelog*)o))

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
static GtkWidget *gnc_plugin_page_tracelog_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_tracelog_destroy_widget (GncPluginPage *plugin_page);

static void tracelog_render (GncPluginPageTracelog *page);

static void gnc_plugin_page_tracelog_cmd_reload (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_tracelog_cmd_print (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

/* Command callbacks */
static GActionEntry gnc_plugin_page_tracelog_actions [] =
{
    { "TracelogReloadAction", gnc_plugin_page_tracelog_cmd_reload, nullptr, nullptr, nullptr },
    { "TracelogPrintAction", gnc_plugin_page_tracelog_cmd_print, nullptr, nullptr, nullptr },
};
static guint gnc_plugin_page_tracelog_n_actions = G_N_ELEMENTS(gnc_plugin_page_tracelog_actions);

/** The menu placeholders this page merges into when it is the current page. */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "ViewPlaceholder4",
    nullptr,
};


GncPluginPage *
gnc_plugin_page_tracelog_new (void)
{
    GncPluginPageTracelog *plugin_page;

    /* Only one trace-log page at a time: reuse the existing instance if there
       is one, so a second Tools->Show Trace Log just raises the open tab
       (gnc_main_window_open_page() displays an already-open page). */
    const GList *object = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_TRACELOG_NAME);
    if (object && GNC_IS_PLUGIN_PAGE_TRACELOG(object->data))
    {
        /* Re-read the log so that raising an already-open tab shows the
           current contents rather than the snapshot from when it was first
           opened -- a user re-invoking the menu item wants fresh output.
           (A no-op before the widget exists; tracelog_render() guards that.) */
        plugin_page = GNC_PLUGIN_PAGE_TRACELOG(object->data);
        tracelog_render (plugin_page);
    }
    else
        plugin_page = GNC_PLUGIN_PAGE_TRACELOG(g_object_new (GNC_TYPE_PLUGIN_PAGE_TRACELOG, nullptr));

    return GNC_PLUGIN_PAGE(plugin_page);
}


/* When the trace-log page becomes current, merge its menu/toolbar. */
static gboolean
gnc_plugin_page_tracelog_focus_widget (GncPluginPage *plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_TRACELOG(plugin_page))
    {
        GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(plugin_page);

        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(plugin_page->window),
                                                 plugin_page,
                                                 gnc_plugin_load_ui_items);

        if (priv->html != nullptr)
        {
            GtkWidget *widget = gnc_html_get_widget (priv->html);
            if (widget && !gtk_widget_is_focus (widget))
                gtk_widget_grab_focus (widget);
        }
    }
    return FALSE;
}

static void
gnc_plugin_page_tracelog_class_init (GncPluginPageTracelogClass *klass)
{
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    /* NB: plugin_name is deliberately left unset, so the page is never written
       to the saved-state file -- save_page/recreate_page are therefore unneeded. */
    gnc_plugin_class->create_widget       = gnc_plugin_page_tracelog_create_widget;
    gnc_plugin_class->destroy_widget      = gnc_plugin_page_tracelog_destroy_widget;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_tracelog_focus_widget;
}

static void
gnc_plugin_page_tracelog_init (GncPluginPageTracelog *plugin_page)
{
    GSimpleActionGroup *simple_action_group;
    GncPluginPage *parent = GNC_PLUGIN_PAGE(plugin_page);

    g_object_set (G_OBJECT(plugin_page),
                  "page-name",      _("Trace Log"),
                  "ui-description", "gnc-plugin-page-tracelog.ui",
                  nullptr);

    /* gnc_main_window_open_page() asserts the page is associated with a book.
       The trace log is not really book-specific, but every notebook page must
       have one, so tie it to the current book. */
    gnc_plugin_page_add_book (parent, gnc_get_current_book());

    simple_action_group = gnc_plugin_page_create_action_group (parent, "GncPluginPageTracelogActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_tracelog_actions,
                                     gnc_plugin_page_tracelog_n_actions,
                                     plugin_page);
}

static GtkWidget *
gnc_plugin_page_tracelog_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(plugin_page);
    GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);
    GtkWindow *topLvl;

    ENTER("page %p", page);

    topLvl = gnc_ui_get_main_window (nullptr);
    priv->html = gnc_html_factory_create_html ();
    gnc_html_set_parent (priv->html, topLvl);

    priv->container = GTK_CONTAINER(gtk_frame_new (nullptr));
    gtk_frame_set_shadow_type (GTK_FRAME(priv->container), GTK_SHADOW_NONE);
    gtk_widget_set_name (GTK_WIDGET(priv->container), "gnc-id-tracelog-page");
    gtk_container_add (GTK_CONTAINER(priv->container), gnc_html_get_widget (priv->html));

    tracelog_render (page);

    /* Drives focus_page_function when the page is inserted, which merges this
       page's menu items and toolbar (Reload/Print). */
    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      nullptr);

    gtk_widget_show_all (GTK_WIDGET(priv->container));
    LEAVE("container %p", priv->container);
    return GTK_WIDGET(priv->container);
}

static void
gnc_plugin_page_tracelog_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(plugin_page);
    GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);

    ENTER("page %p", page);
    if (priv->html != nullptr)
    {
        gnc_html_destroy (priv->html);
        priv->html = nullptr;
    }
    priv->container = nullptr;
    LEAVE(" ");
}

/* Classify a trace line by its level token (format: "* HH:MM:SS  LEVEL <dom> ..."). */
static const char *
tracelog_line_class (const char *line)
{
    if (line == nullptr)
        return "";
    if (strstr (line, " FATAL "))
        return "FATAL";
    if (strstr (line, " ERROR "))
        return "ERROR";
    if (strstr (line, " WARN "))
        return "WARN";
    return "";
}

/* Read the current trace file and render it as a self-contained colorized
   HTML string, loaded in memory (no temp file / file://). Called on open and
   from the Reload action. */
static void
tracelog_render (GncPluginPageTracelog *page)
{
    GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);
    gsize length = 0;
    gchar *contents;
    GString *html;

    /* The widget (and priv->html) may not exist yet: create_widget() calls us
       once it does, and gnc_plugin_page_tracelog_new() may call us on an
       already-open page. Nothing to render into before then. */
    if (priv->html == nullptr)
        return;

    contents = qof_log_read_current (&length);
    html = g_string_new (nullptr);

    g_string_append (html,
        "<html><head><meta charset=\"utf-8\"><style>"
        "body{font-family:monospace;font-size:12px;background:#fbfbfa;"
        "color:#1a1a1a;margin:0;padding:8px;}"
        ".line{white-space:pre-wrap;}"
        ".ERROR,.FATAL{color:#b00020;}"
        ".WARN{color:#a15c00;}"
        ".empty{color:#666;font-style:italic;}"
        "</style></head><body>");

    if (contents != nullptr && *contents != '\0')
    {
        gchar **lines = g_strsplit (contents, "\n", -1);
        for (gint i = 0; lines[i] != nullptr; i++)
        {
            const char *cls = tracelog_line_class (lines[i]);
            /* Escape every line before it enters the HTML so nothing in the log
               -- e.g. a maliciously crafted security name -- can inject markup.
               g_utf8_make_valid() first guards g_markup_escape_text() against
               invalid UTF-8 in the log text. */
            gchar *valid = g_utf8_make_valid (lines[i], -1);
            gchar *eline = g_markup_escape_text (valid, -1);
            g_string_append_printf (html, "<div class=\"line %s\">%s</div>",
                                    cls, eline);
            g_free (eline);
            g_free (valid);
        }
        g_strfreev (lines);
    }
    else if (contents == nullptr)
    {
        g_string_append_printf (html, "<div class=\"empty\">%s</div>",
            _("Logging is directed to the console; no trace file is available to display."));
    }
    else
    {
        g_string_append_printf (html, "<div class=\"empty\">%s</div>",
            _("No messages have been logged this session."));
    }

    g_string_append (html, "</body></html>");

    gnc_html_load_html_string (priv->html, html->str);

    g_string_free (html, TRUE);
    g_free (contents);
}

static void
gnc_plugin_page_tracelog_cmd_reload (GSimpleAction *simple,
                                     GVariant *parameter,
                                     gpointer user_data)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(user_data);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_TRACELOG(page));
    tracelog_render (page);
}

static void
gnc_plugin_page_tracelog_cmd_print (GSimpleAction *simple,
                                    GVariant *parameter,
                                    gpointer user_data)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(user_data);
    GncPluginPageTracelogPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_TRACELOG(page));
    priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);
    if (priv->html == nullptr)
        return;
    gnc_html_print (priv->html, _("GnuCash Trace Log"));
}

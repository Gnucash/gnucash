/********************************************************************\
 * gnc-plugin-page-tracelog.cpp : in-app viewer for the trace log   *
 *                                                                  *
 * Copyright 2026 GnuCash contributors                              *
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
 * along with this program.  If not, see                            *
 * <https://www.gnu.org/licenses/>.                                 *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include <algorithm>
#include <cctype>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

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
    gchar *filter;   /* case-insensitive substring filter; nullptr/empty = show all */
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
static void gnc_plugin_page_tracelog_cmd_filter (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

/* Command callbacks */
static GActionEntry gnc_plugin_page_tracelog_actions [] =
{
    { "TracelogReloadAction", gnc_plugin_page_tracelog_cmd_reload, nullptr, nullptr, nullptr },
    { "TracelogPrintAction", gnc_plugin_page_tracelog_cmd_print, nullptr, nullptr, nullptr },
    { "TracelogFilterAction", gnc_plugin_page_tracelog_cmd_filter, nullptr, nullptr, nullptr },
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
    g_free (priv->filter);
    priv->filter = nullptr;
    priv->container = nullptr;
    LEAVE(" ");
}

/* Classify a trace line by its level. The log format is fixed --
   "* HH:MM:SS <5-char level> <domain> ..." -- so test the level column directly
   (index 11, a 5-wide right-justified field) rather than scanning the whole
   line: faster, and it can't be fooled by one of these words in a message. */
static const char *
tracelog_line_class (const std::string& line)
{
    if (line.size () < 16)
        return "";
    auto level = std::string_view (line).substr (11, 5);
    if (level == "FATAL")
        return "FATAL";
    if (level == "ERROR")
        return "ERROR";
    if (level == " WARN")             /* 4-char "WARN", right-justified in 5 */
        return "WARN";
    return "";
}

/* Case-insensitive (ASCII) substring test for the viewer's text filter. */
static bool
tracelog_line_matches (const std::string& line, const std::string& needle)
{
    if (needle.empty ())
        return true;
    auto eq = [](char a, char b) {
        return std::tolower (static_cast<unsigned char>(a)) ==
               std::tolower (static_cast<unsigned char>(b));
    };
    return std::search (line.begin (), line.end (),
                        needle.begin (), needle.end (), eq) != line.end ();
}

/* Read the current trace file and render it as a self-contained colorized
   HTML string, loaded in memory (no temp file / file://). Called on open, from
   the Reload action, and when the filter changes. */
static void
tracelog_render (GncPluginPageTracelog *page)
{
    GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);

    /* The widget (and priv->html) may not exist yet: create_widget() calls us
       once it does, and gnc_plugin_page_tracelog_new() may call us on an
       already-open page. Nothing to render into before then. */
    if (priv->html == nullptr)
        return;

    gsize length = 0;
    gchar *contents = qof_log_read_current (&length);
    const std::string filter = (priv->filter != nullptr) ? priv->filter : "";

    std::string html =
        "<html><head><meta charset=\"utf-8\"><style>"
        "body{font-family:monospace;font-size:12px;background:#fbfbfa;"
        "color:#1a1a1a;margin:0;padding:8px;}"
        ".line{white-space:pre-wrap;}"
        ".ERROR,.FATAL{color:#b00020;}"
        ".WARN{color:#a15c00;}"
        ".empty{color:#666;font-style:italic;}"
        ".filterinfo{color:#444;background:#ececec;padding:2px 4px;margin-bottom:6px;}"
        "</style></head><body>";

    /* Escape every value before it enters the HTML so nothing in the log -- e.g.
       a maliciously crafted security name -- can inject markup. g_utf8_make_valid()
       first guards g_markup_escape_text() against invalid UTF-8. The class string
       is one of our own fixed tokens, so it is not user data. */
    auto append_div = [&html](const char *cls, const char *text) {
        gchar *valid = g_utf8_make_valid (text, -1);
        gchar *escaped = g_markup_escape_text (valid, -1);
        html += "<div class=\"";
        html += cls;
        html += "\">";
        html += escaped;
        html += "</div>";
        g_free (escaped);
        g_free (valid);
    };

    if (contents == nullptr)
    {
        append_div ("empty",
            _("Logging is directed to the console; no trace file is available to display."));
    }
    else
    {
        /* Split once into a vector and filter that, rather than accumulating one
           giant string and re-splitting it. */
        std::vector<std::string> lines;
        std::istringstream iss (contents);
        for (std::string line; std::getline (iss, line); )
            lines.push_back (std::move (line));

        std::vector<const std::string*> matched;
        matched.reserve (lines.size ());
        for (const auto& line : lines)
            if (tracelog_line_matches (line, filter))
                matched.push_back (&line);

        if (!filter.empty ())
        {
            gchar *note = g_strdup_printf (
                _("Filtering on '%s' - showing %zu of %zu lines."),
                filter.c_str (), matched.size (), lines.size ());
            append_div ("filterinfo", note);
            g_free (note);
        }

        if (lines.empty ())
            append_div ("empty", _("No messages have been logged this session."));
        else if (matched.empty ())
            append_div ("empty", _("No lines match the filter."));

        for (const std::string* line : matched)
        {
            std::string cls = "line ";
            cls += tracelog_line_class (*line);
            append_div (cls.c_str (), line->c_str ());
        }
    }

    html += "</body></html>";

    gnc_html_load_html_string (priv->html, html.c_str ());

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

/* Filter-dialog response handler. On Apply, read the entry and re-render; always
   destroy the dialog. Response-driven (no gtk_dialog_run). */
static void
gnc_plugin_page_tracelog_filter_response (GtkDialog *dialog,
                                          gint response,
                                          gpointer user_data)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(user_data);

    if (response == GTK_RESPONSE_ACCEPT && GNC_IS_PLUGIN_PAGE_TRACELOG(page))
    {
        GncPluginPageTracelogPrivate *priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);
        GtkEntry *entry = GTK_ENTRY(g_object_get_data (G_OBJECT(dialog), "filter-entry"));
        const gchar *text = gtk_entry_get_text (entry);
        g_free (priv->filter);
        priv->filter = (text != nullptr && *text != '\0') ? g_strdup (text) : nullptr;
        tracelog_render (page);
    }
    gtk_widget_destroy (GTK_WIDGET(dialog));
}

/* Prompt for a substring filter and re-render showing only matching lines.
   An empty entry clears the filter (shows everything). */
static void
gnc_plugin_page_tracelog_cmd_filter (GSimpleAction *simple,
                                     GVariant *parameter,
                                     gpointer user_data)
{
    GncPluginPageTracelog *page = GNC_PLUGIN_PAGE_TRACELOG(user_data);
    GncPluginPageTracelogPrivate *priv;
    GtkWidget *toplevel, *dialog, *content, *box, *label, *entry;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_TRACELOG(page));
    priv = GNC_PLUGIN_PAGE_TRACELOG_GET_PRIVATE(page);

    toplevel = (priv->html != nullptr)
        ? gtk_widget_get_toplevel (gnc_html_get_widget (priv->html)) : nullptr;

    dialog = gtk_dialog_new_with_buttons (
        _("Filter Trace Log"),
        (toplevel && GTK_IS_WINDOW(toplevel)) ? GTK_WINDOW(toplevel) : nullptr,
        GTK_DIALOG_MODAL,
        _("_Cancel"), GTK_RESPONSE_CANCEL,
        _("_Apply"),  GTK_RESPONSE_ACCEPT,
        nullptr);

    content = gtk_dialog_get_content_area (GTK_DIALOG(dialog));
    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
    gtk_container_set_border_width (GTK_CONTAINER(box), 12);
    label = gtk_label_new (
        _("Show only lines containing this text (leave empty to show all):"));
    gtk_label_set_xalign (GTK_LABEL(label), 0.0);
    entry = gtk_entry_new ();
    if (priv->filter != nullptr)
        gtk_entry_set_text (GTK_ENTRY(entry), priv->filter);
    gtk_entry_set_activates_default (GTK_ENTRY(entry), TRUE);
    gtk_box_pack_start (GTK_BOX(box), label, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX(box), entry, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER(content), box);
    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

    /* Modal + transient keeps the page from closing underneath it; the handler
       applies the filter and destroys the dialog. */
    g_object_set_data (G_OBJECT(dialog), "filter-entry", entry);
    g_signal_connect (dialog, "response",
                      G_CALLBACK(gnc_plugin_page_tracelog_filter_response), page);
    gtk_widget_show_all (dialog);
    /* show_all alone doesn't make the dialog the active window, so some keys
       (e.g. Backspace) go to the main window until it's clicked; present it. */
    gtk_window_present (GTK_WINDOW(dialog));
}

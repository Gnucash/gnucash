/* gnc-plugin-page-report.c
 * Copyright (C) 2004 Joshua Sled <jsled@asynchronous.org>
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * Originally from window-report.c:
 * Copyright (C) 1997 Robin D. Clark
 * Copyright (C) 1998 Linas Vepstas
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )
 * Copyright (C) 2000 Dave Peticolas
 * Copyright (C) 2000 Bill Gribble
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiReport Reports
    @{ */
/** @file gnc-plugin-page-report.c
    @brief  Report page.
    @author Copyright (C) 2004 Joshua Sled <jsled@asynchronous.org>
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gnc-optiondb-impl.hpp>
#include <libguile.h>

#include <config.h>

#include <sys/stat.h>
#include <errno.h>

#include <gnc-glib-utils.h>
#include "gfec.h"
#include "dialog-custom-report.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-html-history.h"
#include "gnc-html.h"
#include "gnc-html-factory.h"
#include "gnc-file.h"
#include "gnc-filepath-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-plugin.h"
#include "gnc-plugin-page-report.h"
#include "gnc-plugin-file-history.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "window-report.h"
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "gnc-icons.h"
#include "print-session.h"

#include <memory>
#include <gnc-report.h>

/* NW: you can add GNC_MOD_REPORT to gnc-engine.h
or simply define it locally. Any unique string with
a gnucash- prefix will do. Then just set a log level
with qof_log_set_level().*/
static QofLogModule log_module = GNC_MOD_GUI;

static GObjectClass *parent_class = nullptr;

// A static GHashTable to record the usage count for each printer
// output name. FIXME: Currently this isn't cleaned up at program
// shutdown because there isn't a place to easily insert a finalize()
// function for this. Oh well.
static GHashTable *static_report_printnames = nullptr;

// Property-id values.
enum
{
    PROP_0,
    PROP_REPORT_ID,
};

typedef struct GncPluginPageReportPrivate
{
    /// The report-id
    int reportId;
    gint component_manager_id;

    /// The report which this Page is satisfying
    SCM cur_report;
    /// The Option DB for this report.
    GncOptionDB *cur_odb;
    size_t option_change_cb_id = 0;

    /* initial_report is special; it's the one that's saved and
     * restored.  The name_change_callback only gets called when
     * the initial_report name is changed. */
    SCM          initial_report;
    GncOptionDB  * initial_odb;
    size_t       name_change_cb_id;

    /* keep a list of edited reports so that we can destroy them when
     * the window is closed. */
    SCM          edited_reports;

    /* The page is in the process of reloading the html */
    gboolean    reloading;
    gboolean    loaded;

    /// the gnc_html abstraction this PluginPage contains
//        gnc_html *html;
    GncHtml *html;
    gboolean webkit2;

    /// the container the above HTML widget is in.
    GtkContainer *container;
} GncPluginPageReportPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageReport, gnc_plugin_page_report, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(o)  \
   ((GncPluginPageReportPrivate*)gnc_plugin_page_report_get_instance_private((GncPluginPageReport*)o))

static void gnc_plugin_page_report_class_init( GncPluginPageReportClass *klass );
static void gnc_plugin_page_report_init( GncPluginPageReport *plugin_page );
static GObject *gnc_plugin_page_report_constructor(GType this_type, guint n_properties, GObjectConstructParam *properties);
static void gnc_plugin_page_report_finalize (GObject *object);
static void gnc_plugin_page_report_setup( GncPluginPage *ppage );

static void gnc_plugin_page_report_constr_init(GncPluginPageReport *plugin_page, gint reportId);

static GtkWidget* gnc_plugin_page_report_create_widget( GncPluginPage *plugin_page );
static void gnc_plugin_page_report_destroy_widget( GncPluginPage *plugin_page );
static void gnc_plugin_page_report_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_report_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);
static void gnc_plugin_page_report_name_changed (GncPluginPage *page, const gchar *name);
static void gnc_plugin_page_report_update_edit_menu (GncPluginPage *page, gboolean hide);
static gboolean gnc_plugin_page_report_finish_pending (GncPluginPage *page);
static void gnc_plugin_page_report_load_uri (GncPluginPage *page);

static int gnc_plugin_page_report_check_urltype(URLType t);
//static void gnc_plugin_page_report_load_cb(gnc_html * html, URLType type,
static void gnc_plugin_page_report_load_cb(GncHtml * html, URLType type,
        const gchar * location, const gchar * label,
        gpointer data);
static void gnc_plugin_page_report_refresh (gpointer data);
static void gnc_plugin_page_report_set_fwd_button(GncPluginPageReport * page, int enabled);
static void gnc_plugin_page_report_set_back_button(GncPluginPageReport * page, int enabled);
static void gnc_plugin_page_report_history_destroy_cb(gnc_html_history_node * node, gpointer user_data);
static void close_handler(gpointer user_data);
void gnc_plugin_page_report_destroy(GncPluginPageReportPrivate *priv);
static void gnc_plugin_page_report_option_change_cb(gpointer data);

void gnc_plugin_page_report_remove_edited_report(GncPluginPageReportPrivate *priv, SCM report);
void gnc_plugin_page_report_add_edited_report(GncPluginPageReportPrivate *priv, SCM report);
static void gnc_plugin_page_report_menu_updates (GncPluginPage *plugin_page);
void gnc_plugin_page_report_raise_editor(SCM report);

static void gnc_plugin_page_report_forw_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_back_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_reload_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_stop_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_save_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_save_as_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_export_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_options_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_print_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_exportpdf_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_copy_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_report_edit_tax_cb (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static GActionEntry report_actions[] =
{
    { "FilePrintAction", gnc_plugin_page_report_print_cb, nullptr, nullptr, nullptr },
    { "FilePrintPDFAction", gnc_plugin_page_report_exportpdf_cb, nullptr, nullptr, nullptr },
    { "EditCopyAction", gnc_plugin_page_report_copy_cb, nullptr, nullptr, nullptr },
    { "EditTaxOptionsAction", gnc_plugin_page_report_edit_tax_cb, nullptr, nullptr, nullptr },
    { "ViewRefreshAction", gnc_plugin_page_report_reload_cb, nullptr, nullptr, nullptr },
    { "ReportSaveAction", gnc_plugin_page_report_save_cb, nullptr, nullptr, nullptr },
    { "ReportSaveAsAction", gnc_plugin_page_report_save_as_cb, nullptr, nullptr, nullptr },
    { "ReportExportAction", gnc_plugin_page_report_export_cb, nullptr, nullptr, nullptr },
    { "ReportOptionsAction", gnc_plugin_page_report_options_cb, nullptr, nullptr, nullptr },
    { "ReportBackAction", gnc_plugin_page_report_back_cb, nullptr, nullptr, nullptr },
    { "ReportForwAction", gnc_plugin_page_report_forw_cb, nullptr, nullptr, nullptr },
    { "ReportReloadAction", gnc_plugin_page_report_reload_cb, nullptr, nullptr, nullptr },
    { "ReportStopAction", gnc_plugin_page_report_stop_cb, nullptr, nullptr, nullptr },
};
static guint num_report_actions = G_N_ELEMENTS(report_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "FilePlaceholder4",
    "FilePlaceholder5",
    "EditPlaceholder6",
    "ReportsPlaceholder1",
    NULL,
};

/** Short labels for use on the toolbar buttons. */
static GncToolBarShortNames toolbar_labels[] =
{
    { "FilePrintAction",      N_("Print") },
    { "ReportExportAction",   N_("Export") },
    { "ReportOptionsAction",  N_("Options") },
    /* Translators: This string is meant to be a short alternative for "Save Report Configuration"
       to be used as toolbar button label. */
    { "ReportSaveAction",     N_("Save Config") },
    /* Translators: This string is meant to be a short alternative for "Save Report Configuration As…"
       to be used as toolbar button label. */
    { "ReportSaveAsAction",   N_("Save Config As…") },
    { "FilePrintPDFAction",   N_("Make Pdf") },
    { nullptr, nullptr },
};

static void
gnc_plugin_page_report_get_property( GObject *obj,
                                     guint prop_id,
                                     GValue *value,
                                     GParamSpec *pspec )
{
    GncPluginPageReport *rep;
    GncPluginPageReportPrivate *priv;

    rep = GNC_PLUGIN_PAGE_REPORT( obj );
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(rep);

    switch ( prop_id )
    {
    case PROP_REPORT_ID:
        g_value_set_int( value, priv->reportId );
        break;
    default:
        PERR( "Unknown property id %d", prop_id );
        break;
    }
}

static void
gnc_plugin_page_report_set_property( GObject *obj,
                                     guint prop_id,
                                     const GValue *value,
                                     GParamSpec *pspec )
{
    GncPluginPageReport *rep;
    GncPluginPageReportPrivate *priv;

    rep = GNC_PLUGIN_PAGE_REPORT( obj );
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(rep);

    DEBUG( "setting property with id %d / %p to value %d",
           prop_id, priv, g_value_get_int( value ) );

    switch ( prop_id )
    {
    case PROP_REPORT_ID:
        priv->reportId = g_value_get_int( value );
        break;
    default:
        PERR( "unknown property id %d", prop_id );
        break;
    }

}

/**
 * Whenever the current page is changed, if a report page is
 * the current page, set focus on the report.
 */
static gboolean
gnc_plugin_page_report_focus_widget (GncPluginPage *report_plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_REPORT(report_plugin_page))
    {
        GncPluginPageReportPrivate *priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report_plugin_page);
        GtkWidget *window;
        GAction *action;

        if (!priv)
            return FALSE;

        /* Disable the Transaction Menu */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(report_plugin_page->window), "TransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        /* Disable the Schedule menu */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(report_plugin_page->window), "ScheduledAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(report_plugin_page->window),
                                                 report_plugin_page,
                                                 gnc_plugin_load_ui_items);

        // setup any short toolbar names
        gnc_main_window_init_short_names (GNC_MAIN_WINDOW(report_plugin_page->window), toolbar_labels);

        gnc_plugin_page_report_menu_updates (report_plugin_page);

        window = gnc_plugin_page_get_window (report_plugin_page);

        if (window && !gnc_main_window_is_restoring_pages (GNC_MAIN_WINDOW(window)))
        {
            GtkWidget *widget = gnc_html_get_webview (priv->html);

            if (!priv->loaded) // so we only do the load once
                gnc_plugin_page_report_load_uri (report_plugin_page);

            if (GTK_IS_WIDGET(widget))
            {
                if (!gtk_widget_is_focus (GTK_WIDGET(widget)))
                    gtk_widget_grab_focus (GTK_WIDGET(widget));
            }
        }
    }
    return FALSE;
}

static void
gnc_plugin_page_report_class_init (GncPluginPageReportClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_page_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = static_cast<GObjectClass*>(g_type_class_peek_parent (klass));

    object_class->constructor = gnc_plugin_page_report_constructor;
    object_class->finalize = gnc_plugin_page_report_finalize;

    object_class->set_property = gnc_plugin_page_report_set_property;
    object_class->get_property = gnc_plugin_page_report_get_property;

    gnc_plugin_page_class->tab_icon        = GNC_ICON_ACCOUNT_REPORT;
    gnc_plugin_page_class->plugin_name     = GNC_PLUGIN_PAGE_REPORT_NAME;

    gnc_plugin_page_class->create_widget   = gnc_plugin_page_report_create_widget;
    gnc_plugin_page_class->destroy_widget  = gnc_plugin_page_report_destroy_widget;
    gnc_plugin_page_class->save_page       = gnc_plugin_page_report_save_page;
    gnc_plugin_page_class->recreate_page   = gnc_plugin_page_report_recreate_page;
    gnc_plugin_page_class->page_name_changed = gnc_plugin_page_report_name_changed;
    gnc_plugin_page_class->update_edit_menu_actions = gnc_plugin_page_report_update_edit_menu;
    gnc_plugin_page_class->finish_pending   = gnc_plugin_page_report_finish_pending;
    gnc_plugin_page_class->focus_page_function = gnc_plugin_page_report_focus_widget;

    // create the "reportId" property
    auto paramspec{static_cast<GParamFlags>(G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE)};
    g_object_class_install_property( object_class,
                                     PROP_REPORT_ID,
                                     g_param_spec_int( "report-id",
                                             _("The numeric ID of the report."),
                                             _("The numeric ID of the report."),
                                                       -1, G_MAXINT, -1,
                                                       paramspec));

    // Also initialize the report name usage count table
    if (!static_report_printnames)
        static_report_printnames = g_hash_table_new_full(g_str_hash,
                                   g_str_equal, g_free, nullptr);
}

static void
gnc_plugin_page_report_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REPORT (object));

    ENTER("object %p", object);
    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

static void
gnc_plugin_page_report_set_progressbar (GncPluginPage *page, gboolean set)
{
    GtkWidget *progressbar;
    GtkAllocation allocation;

    progressbar = gnc_window_get_progressbar (GNC_WINDOW(page->window));
    gtk_widget_get_allocation (GTK_WIDGET(progressbar), &allocation);

    // this sets the minimum size of the progressbar to that allocated
    if (set)
        gtk_widget_set_size_request (GTK_WIDGET(progressbar), -1, allocation.height);
    else
        gtk_widget_set_size_request (GTK_WIDGET(progressbar), -1, -1); //reset
}

static void
gnc_plugin_page_report_load_uri (GncPluginPage *page)
{
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    GncPluginPage *weak_page = page;
    URLType type;
    char * id_name;
    char * child_name;
    char * url_location = nullptr;
    char * url_label = nullptr;

    report = GNC_PLUGIN_PAGE_REPORT(page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (!priv)
        return; // No priv means the page doesn't exist anymore.

    DEBUG( "Load uri id=%d", priv->reportId );
    id_name = g_strdup_printf("id=%d", priv->reportId );
    child_name = gnc_build_url( URL_TYPE_REPORT, id_name, nullptr );
    type = gnc_html_parse_url( priv->html, child_name, &url_location, &url_label);
    DEBUG( "passing id_name=[%s] child_name=[%s] type=[%s], location=[%s], label=[%s]",
           id_name, child_name ? child_name : "(null)",
           type ? type : "(null)", url_location ? url_location : "(null)",
           url_label ? url_label : "(null)" );

    g_free(id_name);
    g_free(child_name);

    g_object_add_weak_pointer(G_OBJECT(page), (gpointer*)(&weak_page));
    gtk_widget_show_all( GTK_WIDGET(priv->container) );

    priv->loaded = TRUE;

    // this sets the window for the progressbar
    gnc_window_set_progressbar_window( GNC_WINDOW(page->window) );

    // this sets the minimum size of the progressbar to that allocated
    gnc_plugin_page_report_set_progressbar( page, TRUE );

    gnc_html_show_url(priv->html, type, url_location, url_label, 0);
    g_free(url_location);

    if (weak_page)
    {
        gnc_plugin_page_report_set_progressbar( page, FALSE );
        g_object_remove_weak_pointer(G_OBJECT(page), (gpointer*)(&weak_page));
    }

    // this resets the window for the progressbar to nullptr
    gnc_window_set_progressbar_window( nullptr );
}

/* used to capture Ctrl+Alt+PgUp/Down for tab selection */
static gboolean
webkit_key_press_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    GncPluginPageReport *report = GNC_PLUGIN_PAGE_REPORT(user_data);
    GncPluginPageReportPrivate *priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask ();
    GtkWidget *window = gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(report));

    if (GNC_PLUGIN_PAGE(report) != gnc_main_window_get_current_page (GNC_MAIN_WINDOW(window)))
        return FALSE;

    if ((event->keyval == GDK_KEY_Page_Up || event->keyval == GDK_KEY_Page_Down ||
         event->keyval == GDK_KEY_KP_Page_Up || event->keyval == GDK_KEY_KP_Page_Down)
          && (event->state & modifiers) == (GDK_CONTROL_MASK | GDK_MOD1_MASK))
    {
        GtkNotebook *notebook = GTK_NOTEBOOK(gtk_widget_get_parent (GTK_WIDGET(priv->container)));
        gint pages = gtk_notebook_get_n_pages (notebook);
        gint current_page = gtk_notebook_get_current_page (notebook);

        if (event->keyval == GDK_KEY_Page_Up || event->keyval == GDK_KEY_KP_Page_Up)
        {
            if (current_page == 0)
                gtk_notebook_set_current_page (notebook, pages - 1);
            else
                gtk_notebook_prev_page (notebook);
        }
        else
        {
            if (pages == current_page + 1)
                gtk_notebook_set_current_page (notebook, 0);
            else
                gtk_notebook_next_page (notebook);
        }
        return TRUE;
    }
    return FALSE;
}

static
GtkWidget*
gnc_plugin_page_report_create_widget( GncPluginPage *page )
{
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    GtkWindow *topLvl;
    GtkWidget *webview;
    URLType type;
    char * id_name;
    char * child_name;
    char * url_location = nullptr;
    char * url_label = nullptr;

    ENTER("page %p", page);

    report = GNC_PLUGIN_PAGE_REPORT(page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);

#ifndef WEBKIT1
    /* Hide the ExportPdf action for Webkit2 */
    priv->webkit2 = TRUE;
#endif

    topLvl = gnc_ui_get_main_window (nullptr);
//        priv->html = gnc_html_new( topLvl );
    priv->html = gnc_html_factory_create_html();
    gnc_html_set_parent( priv->html, topLvl );
    priv->loaded = FALSE;

    gnc_html_history_set_node_destroy_cb(gnc_html_get_history(priv->html),
                                         gnc_plugin_page_report_history_destroy_cb,
                                         (gpointer)priv);

    priv->container = GTK_CONTAINER(gtk_frame_new(nullptr));
    gtk_frame_set_shadow_type(GTK_FRAME(priv->container), GTK_SHADOW_NONE);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->container), "gnc-id-report-page");

    gtk_container_add(GTK_CONTAINER(priv->container),
                      gnc_html_get_widget(priv->html));

    priv->component_manager_id =
        gnc_register_gui_component(WINDOW_REPORT_CM_CLASS, nullptr,
                                   close_handler, page);
    gnc_gui_component_set_session(priv->component_manager_id,
                                  gnc_get_current_session());

    gnc_html_set_urltype_cb(priv->html, gnc_plugin_page_report_check_urltype);
    gnc_html_set_load_cb(priv->html, gnc_plugin_page_report_load_cb, report);

    /* We need to call the load call back so the report appears to have been run
     so it will get saved properly if the report is not realized in session */
    id_name = g_strdup_printf("id=%d", priv->reportId );
    child_name = gnc_build_url( URL_TYPE_REPORT, id_name, nullptr );
    type = gnc_html_parse_url( priv->html, child_name, &url_location, &url_label);

    gnc_plugin_page_report_load_cb (priv->html, type, id_name, url_label, report);
    g_free(id_name);
    g_free(child_name);
    g_free (url_label);
    g_free (url_location);

    // FIXME.  This is f^-1(f(x)), isn't it?
    DEBUG( "id=%d", priv->reportId );

    g_signal_connect (G_OBJECT(page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      nullptr);

    // used to capture Ctrl+Alt+PgUp/Down for tab selection
    webview = gnc_html_get_webview (priv->html);
    if (webview)
    {
        gtk_widget_add_events (webview, gtk_widget_get_events (webview) |
                               GDK_KEY_PRESS_MASK);

        g_signal_connect (webview, "key-press-event",
                          G_CALLBACK(webkit_key_press_event_cb),
                          page);
    }

    gtk_widget_show_all( GTK_WIDGET(priv->container) );
    LEAVE("container %p", priv->container);
    return GTK_WIDGET( priv->container );
}

/********************************************************************
 * gnc_plugin_page_report_check_urltype
 * is it OK to show a certain URLType in this window?
 ********************************************************************/
static int
gnc_plugin_page_report_check_urltype(URLType t)
{
    if (!g_strcmp0 (t, URL_TYPE_REPORT))
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

/**
 * Simply get the initial report given the id, so we can do initialization
 * things like setup the tab name based on the report's name.
 **/
static void
gnc_plugin_page_report_setup( GncPluginPage *ppage )
{
    GncPluginPageReport *report = GNC_PLUGIN_PAGE_REPORT(ppage);
    GncPluginPageReportPrivate *priv;
    SCM  set_needs_save = scm_c_eval_string("gnc:report-set-needs-save?!");
    SCM  inst_report;
    int  report_id;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    priv->cur_report        = SCM_BOOL_F;
    priv->initial_report    = SCM_BOOL_F;
    priv->edited_reports    = SCM_EOL;
    priv->name_change_cb_id = 0;

    g_object_get( ppage, "report-id", &report_id, nullptr );

    PINFO("report-id: %d\n", report_id);

    /* get the inst-report from the Scheme-side hash, and get its
     * options and editor thunk */
    if ((inst_report = gnc_report_find(report_id)) == SCM_BOOL_F)
    {
        return;
    }

    if (priv->initial_report == SCM_BOOL_F)
    {
        priv->initial_report = inst_report;
        scm_gc_protect_object(priv->initial_report);
    }

    // all reports need [to be] saved immediately after they're created.
    PINFO("set needs save");
    scm_call_2(set_needs_save, inst_report, SCM_BOOL_T);
}

/********************************************************************
 * gnc_plugin_page_report_load_cb
 * called after a report is loaded into the gnc_html widget
 ********************************************************************/
static void
gnc_plugin_page_report_load_cb (GncHtml * html, URLType type,
                                const gchar * location, const gchar * label,
                                gpointer data)
{
    GncPluginPageReport *report = GNC_PLUGIN_PAGE_REPORT(data);
    GncPluginPageReportPrivate *priv;
    int  report_id;
    SCM  set_needs_save = scm_c_eval_string("gnc:report-set-needs-save?!");
    SCM  inst_report;

    ENTER( "load_cb: type=[%s], location=[%s], label=[%s]",
           type ? type : "(null)", location ? location : "(null)",
           label ? label : "(null)" );

    /* we get this callback if a new report is requested to be loaded OR
     * if any URL is clicked.  If an options URL is clicked, we want to
     * know about it */
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (!g_strcmp0 (type, URL_TYPE_REPORT)
            && location
            && (strlen(location) > 3)
            && !strncmp("id=", location, 3))
    {
        report_id = atoi(location + 3);
        DEBUG( "parsed id=%d", report_id );
    }
    else if (!g_strcmp0( type, URL_TYPE_OPTIONS)
             && location
             && (strlen(location) > 10)
             && !strncmp("report-id=", location, 10))
    {
        report_id = atoi(location + 10);
        inst_report = gnc_report_find(report_id);
        if (inst_report != SCM_BOOL_F)
        {
            gnc_plugin_page_report_add_edited_report(priv, inst_report);
        }
        LEAVE("");
        return;
    }
    else
    {
        LEAVE( " unknown URL type [%s] location [%s]", type, location );
        return;
    }

    /* get the inst-report from the hash, and get its
     * options and editor thunk */
    if ((inst_report = gnc_report_find(report_id)) == SCM_BOOL_F)
    {
        LEAVE( "error getting inst_report" );
        return;
    }

    if (priv->initial_report == SCM_BOOL_F)
    {
        priv->initial_report = inst_report;
        scm_gc_protect_object(priv->initial_report);

        DEBUG("calling set_needs_save for report with id=%d", report_id);
        scm_call_2(set_needs_save, inst_report, SCM_BOOL_T);

        priv->initial_odb = gnc_get_report_optiondb(inst_report);

        priv->name_change_cb_id =
            priv->initial_odb->register_callback(
                gnc_plugin_page_report_refresh, priv);

    }

    if ((priv->cur_report != SCM_BOOL_F) && (priv->cur_odb != nullptr))
    {
        priv->cur_odb->unregister_callback(priv->option_change_cb_id);
        priv->option_change_cb_id = 0;
        priv->cur_odb = nullptr;
    }

    if (priv->cur_report != SCM_BOOL_F)
        scm_gc_unprotect_object(priv->cur_report);
    priv->cur_report = inst_report;
    scm_gc_protect_object(priv->cur_report);

    priv->cur_odb = gnc_get_report_optiondb(inst_report);

    priv->option_change_cb_id =
        priv->cur_odb->register_callback(
            gnc_plugin_page_report_option_change_cb, report);

    if (gnc_html_history_forward_p(gnc_html_get_history(priv->html)))
    {
        gnc_plugin_page_report_set_fwd_button(report, TRUE);
    }
    else
    {
        gnc_plugin_page_report_set_fwd_button(report, FALSE);
    }

    if (gnc_html_history_back_p(gnc_html_get_history(priv->html)))
    {
        gnc_plugin_page_report_set_back_button(report, TRUE);
    }
    else
    {
        gnc_plugin_page_report_set_back_button(report, FALSE);
    }

    LEAVE( "done" );
}


/** This function is called when one of the options for a report
 *  page has changed.  It is responsible for marking the report as
 *  dirty, and causing the report to reload using the new options.
 *
 *  @note This function currently also calls the main window code to
 *  update it if the name of the report has changed.  This code should
 *  eventually go away, and the only way to change the name should be
 *  via the main window.
 *
 *  @param data A pointer to the GncPluginPageReport data structure
 *  that describes a report. */
static void
gnc_plugin_page_report_option_change_cb(gpointer data)
{
    GncPluginPage *page;
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    SCM dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REPORT(data));
    report = GNC_PLUGIN_PAGE_REPORT(data);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    page = GNC_PLUGIN_PAGE(report);

    DEBUG( "option_change" );
    if (priv->cur_report == SCM_BOOL_F)
        return;
    DEBUG( "set-dirty, queue-draw" );

    /* Update the page (i.e. the notebook tab and window title) */
    std::string old_name{gnc_plugin_page_get_page_name(GNC_PLUGIN_PAGE(report))};
    auto new_name{priv->cur_odb->lookup_string_option("General",
                                                      "Report name")};
    if (new_name != old_name)
    {
        /* Bug 727130, 760711 - remove only the non-printable
         * characters from the new name */
        char *clean_name{g_strdup(new_name.c_str())};
        gnc_utf8_strip_invalid_and_controls(clean_name);
        ENTER("Cleaned-up new report name: %s", clean_name ? clean_name : "(null)");
        main_window_update_page_name(GNC_PLUGIN_PAGE(report), clean_name);
        g_free(clean_name);
    }

    /* it's probably already dirty, but make sure */
    scm_call_2(dirty_report, priv->cur_report, SCM_BOOL_T);

    // prevent closing this page while loading...
    priv->reloading = TRUE;

    // this sets the window for the progressbar
    gnc_window_set_progressbar_window( GNC_WINDOW(page->window) );

    // this sets the minimum size of the progressbar to that allocated
    gnc_plugin_page_report_set_progressbar( page, TRUE );

    /* Now queue the fact that we need to reload this report */
    gnc_html_reload( priv->html, TRUE ); //reload by rebuild

    gnc_plugin_page_report_set_progressbar( page, FALSE );

    // this resets the window for the progressbar to nullptr
    gnc_window_set_progressbar_window( nullptr );

    priv->reloading = FALSE;
}

/* FIXME: This function does... nothing?  */
static void
gnc_plugin_page_report_history_destroy_cb(gnc_html_history_node * node,
        gpointer user_data)
{
#if 0
    static SCM         remover = SCM_BOOL_F;
    int                report_id;

    if (remover == SCM_BOOL_F)
    {
        remover = scm_c_eval_string("gnc:report-remove-by-id");
    }

    if (node
            && !g_strcmp0 (node->type, URL_TYPE_REPORT)\
            && !strncmp("id=", node->location, 3))
    {
        sscanf(node->location + 3, "%d", &report_id);
        /*    printf("unreffing report %d and children\n", report_id);
              scm_call_1(remover, scm_from_int (report_id)); */
    }
    else
    {
        return;
    }
#endif
}

// @param data is actually GncPluginPageReportPrivate
static void
gnc_plugin_page_report_refresh(gpointer data)
{
    // FIXME?
    DEBUG( "report-refresh called" );
    // something like ... gnc_plugin_page_report_redraw( nullptr, (GncPluginPageReportPrivate*)data );
    return;
}

static void
gnc_plugin_page_report_destroy_widget(GncPluginPage *plugin_page)
{
    GncPluginPageReportPrivate *priv;

    // FIXME: cleanup other resources.

    PINFO("destroy widget");
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(plugin_page);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    if (priv->component_manager_id)
    {
        gnc_unregister_gui_component(priv->component_manager_id);
        priv->component_manager_id = 0;
    }

    gnc_plugin_page_report_destroy(priv);
    gnc_report_remove_by_id(priv->reportId);
}


/** The key name used it the state file for storing the report
 *  options. */
#define SCHEME_OPTIONS   "SchemeOptions"
#define SCHEME_OPTIONS_N "SchemeOptions%d"


/** Save enough information about this report page that it can be
 *  recreated next time the user starts gnucash.
 *
 *  @param plugin_page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_report_save_page (GncPluginPage *plugin_page,
                                  GKeyFile *key_file,
                                  const gchar *group_name)
{
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    SCM gen_save_text, scm_text;
    SCM get_embedded_list, embedded, item, tmp_report;
    SCM  get_options;
    gint count, id;
    gchar *text, *key_name;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_REPORT(plugin_page));
    g_return_if_fail (key_file != nullptr);
    g_return_if_fail (group_name != nullptr);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    report = GNC_PLUGIN_PAGE_REPORT(plugin_page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);

    if (!priv || !priv->cur_report || scm_is_null(priv->cur_report) ||
            SCM_UNBNDP(priv->cur_report) || SCM_BOOL_F == priv->cur_report)
    {
        LEAVE("not saving invalid report");
        return;
    }

    gen_save_text = scm_c_eval_string("gnc:report-serialize");
    get_embedded_list = scm_c_eval_string("gnc:report-embedded-list");
    get_options    = scm_c_eval_string("gnc:report-options");
    embedded = scm_call_1(get_embedded_list, scm_call_1(get_options, priv->cur_report));
    count = scm_ilength(embedded);
    while (count-- > 0)
    {
        item = SCM_CAR(embedded);
        embedded = SCM_CDR(embedded);
        if (!scm_is_number(item))
            continue;
        id = scm_to_int (item);
        tmp_report = gnc_report_find(id);
        scm_text = scm_call_1(gen_save_text, tmp_report);
        if (!scm_is_string (scm_text))
        {
            DEBUG("child report %d: nothing to save", id);
            continue;
        }

        key_name = g_strdup_printf(SCHEME_OPTIONS_N, id);
        text = gnc_scm_strip_comments(scm_text);
        g_key_file_set_value(key_file, group_name, key_name, text);
        g_free(text);
        g_free(key_name);
    }

    scm_text = scm_call_1(gen_save_text, priv->cur_report);
    if (!scm_is_string (scm_text))
    {
        LEAVE("nothing to save");
        return;
    }

    text = gnc_scm_strip_comments(scm_text);
    g_key_file_set_value(key_file, group_name, SCHEME_OPTIONS, text);
    g_free(text);
    LEAVE(" ");
}


/** Create a new report page based on the information saved during a
 *  previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_report_recreate_page (GtkWidget *window,
                                      GKeyFile *key_file,
                                      const gchar *group_name)
{
    GncPluginPage *page;
    gchar **keys;
    gsize i, num_keys;
    GError *error = nullptr;
    gchar *option_string;
    gint report_id;
    SCM scm_id, final_id = SCM_BOOL_F;
    SCM report;

    g_return_val_if_fail(key_file, nullptr);
    g_return_val_if_fail(group_name, nullptr);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    keys = g_key_file_get_keys(key_file, group_name, &num_keys, &error);
    if (error)
    {
        g_warning("error reading group %s key list: %s",
                  group_name, error->message);
        g_error_free(error);
        LEAVE("no keys");
        return nullptr;
    }

    for (i = 0; i < num_keys; i++)
    {
        if (strncmp(keys[i], SCHEME_OPTIONS, strlen(SCHEME_OPTIONS)) != 0)
            continue;
        option_string = g_key_file_get_value(key_file, group_name,
                                              keys[i], &error);
        if (error)
        {
            g_warning("error reading group %s key %s: %s",
                      group_name, keys[i], error->message);
            g_error_free(error);
            g_strfreev (keys);
            LEAVE("bad value");
            return nullptr;
        }
        scm_id = scm_eval_string(scm_from_utf8_string(option_string));
        g_free(option_string);

        if (!scm_integer_p(scm_id))
        {
            DEBUG("report id not an integer for key %s", keys[i]);
            g_strfreev (keys);
            return nullptr;
        }

        if (final_id == SCM_BOOL_F)
        {
            if (g_strcmp0(keys[i], SCHEME_OPTIONS) == 0)
            {
                final_id = scm_id;
            }
        }
    }
    g_strfreev (keys);

    if (final_id == SCM_BOOL_F)
    {
        LEAVE("report not specified");
        return nullptr;
    }

    report_id = scm_to_int(final_id);
    report = gnc_report_find(report_id);
    if (!report)
    {
        LEAVE("report doesn't exist");
        return nullptr;
    }

    page = gnc_plugin_page_report_new( report_id );

    LEAVE(" ");
    return page;
}


/** Update a report page to reflect a name change made by external
 *  code.  This is called from the main window code when a page's name
 *  is changes.  The report code will update its copy of the name and
 *  regenerate the report.
 *
 *  @internal
 *
 *  @param page The page whose name has changed.
 *
 *  @param name The new name for the page. */
static void
gnc_plugin_page_report_name_changed (GncPluginPage *page, const gchar *name)
{
    GncPluginPageReportPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REPORT(page));
    g_return_if_fail(name != nullptr);

    ENTER("page %p, name %s", page, name);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(page);

    if (priv->cur_odb)
    {

        /* Is this a redundant call? */
        auto old_name = priv->cur_odb->lookup_string_option("General",
                                                            "Report name");
        std::string name_str{name};
        DEBUG("Comparing old name '%s' to new name '%s'",
              old_name.empty() ? "(null)" : old_name.c_str(), name);
        if (old_name == name_str)
        {
            LEAVE("no change");
            return;
        }

        /* Store the new name for the report. */
        priv->cur_odb->set_string_option("General", "Report name", name_str);

    }

    /* Have to manually call the option change hook. */
    gnc_plugin_page_report_option_change_cb(page);
    LEAVE(" ");
}

static void
gnc_plugin_page_report_update_edit_menu (GncPluginPage *page, gboolean hide)
{
    GncMainWindow *window = (GncMainWindow*)gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page));
    GAction *action;

    action = gnc_main_window_find_action (window, "EditCopyAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);
    action = gnc_main_window_find_action (window, "EditCutAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
    action = gnc_main_window_find_action (window, "EditPasteAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
}

static gboolean
gnc_plugin_page_report_finish_pending (GncPluginPage *page)
{
    GncPluginPageReportPrivate *priv;
    GncPluginPageReport *report;

    report = GNC_PLUGIN_PAGE_REPORT(page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    return !priv->reloading;
}


/********************************************************************
 * gnc_report_window_destroy
 * free and destroy a window
 ********************************************************************/
void
gnc_plugin_page_report_destroy(GncPluginPageReportPrivate * priv)
{
    SCM  get_editor = scm_c_eval_string("gnc:report-editor-widget");
    SCM  set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");
    SCM  edited, editor;

    /* close any open editors */
    for (edited = scm_list_copy(priv->edited_reports); !scm_is_null(edited);
            edited = SCM_CDR(edited))
    {
        editor = scm_call_1(get_editor, SCM_CAR(edited));
        scm_call_2(set_editor, SCM_CAR(edited), SCM_BOOL_F);
        if (editor != SCM_BOOL_F)
        {
#define FUNC_NAME "gtk_widget_destroy"
            auto w{static_cast<GtkWidget*>(SWIG_MustGetPtr(editor, SWIG_TypeQuery("_p_GtkWidget"), 1, 0))};
#undef FUNC_NAME
            gtk_widget_destroy(GTK_WIDGET(w));
        }
    }

    if (priv->initial_odb)
    {
//Remove this if there's a double-free
        gnc_option_db_destroy(priv->initial_odb);
        priv->initial_odb = nullptr;
    }

    gnc_html_destroy(priv->html);

    priv->container     = nullptr;
    priv->html          = nullptr;

    if (priv->cur_report != SCM_BOOL_F)
        scm_gc_unprotect_object(priv->cur_report);
    if (priv->edited_reports != SCM_EOL)
        scm_gc_unprotect_object(priv->edited_reports);
}

static const gchar *initially_insensitive_actions[] =
{
    nullptr
};

static void
gnc_plugin_page_report_init ( GncPluginPageReport *plugin_page )
{
}

static GObject*
gnc_plugin_page_report_constructor(GType this_type, guint n_properties, GObjectConstructParam *properties)
{
    GObject *obj;
    GncPluginPageReportClass *our_class;
    GObjectClass *parent_class;
    gint reportId = -42;

    our_class = GNC_PLUGIN_PAGE_REPORT_CLASS (
                    g_type_class_peek (GNC_TYPE_PLUGIN_PAGE_REPORT));
    parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (our_class));
    obj = parent_class->constructor(this_type, n_properties, properties);

    for (decltype(n_properties) i = 0; i < n_properties; i++)
    {
        GObjectConstructParam prop = properties[i];
        if (strcmp(prop.pspec->name, "report-id") == 0)
        {
            reportId = g_value_get_int(prop.value);
        }
    }

    gnc_plugin_page_report_constr_init(GNC_PLUGIN_PAGE_REPORT(obj), reportId);

    return obj;
}

static void
gnc_plugin_page_report_menu_update (GncPluginPage *plugin_page,
                                    action_toolbar_labels *tooltip_list)
{
    GncMainWindow *window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
    GtkWidget *tool_item;

    for (gint i = 0; (tooltip_list[i].action_name != nullptr); i++)
    {
        gnc_main_window_update_menu_for_action (window,
                                                tooltip_list[i].action_name,
                                                _(tooltip_list[i].label),
                                                _(tooltip_list[i].tooltip));

        tool_item = gnc_main_window_toolbar_find_tool_item (window,
                                                            tooltip_list[i].action_name);
        if (tool_item)
        {   // only need to update the tooltip here
            gtk_widget_set_tooltip_text (GTK_WIDGET(tool_item), _(tooltip_list[i].tooltip));
            g_object_set (G_OBJECT(tool_item), "has-tooltip", false, nullptr);
        }
    }
    // need to add the accelerator keys for the updated menu items
    gnc_main_window_menu_add_accelerator_keys (window);
}

static void
gnc_plugin_page_report_menu_updates (GncPluginPage *plugin_page)
{
    GncPluginPageReportPrivate *priv;
    GncPluginPageReport *report;
    GncMainWindow *window;
    action_toolbar_labels tooltip_list[3];
    GAction *action;

    gchar *saved_reports_path = gnc_build_userdata_path (SAVED_REPORTS_FILE);
    gchar *report_save_str = g_strdup_printf (
        _("Update the current report's saved configuration. "
          "The report configuration will be saved in the file %s."), saved_reports_path);
    gchar *report_saveas_str = g_strdup_printf (
        _("Add the current report's configuration to the 'Reports->Saved Report Configurations' menu. "
          "The report configuration will be saved in the file %s."), saved_reports_path);

    report = GNC_PLUGIN_PAGE_REPORT(plugin_page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);

    window = (GncMainWindow*)gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page));

    tooltip_list[0] = { "ReportSaveAction", N_("Save _Report Configuration"), report_save_str };
    tooltip_list[1] = { "ReportSaveAsAction", N_("Save Report Configuration As…"), report_saveas_str };
    tooltip_list[2] = { nullptr, nullptr, nullptr };

    gnc_plugin_page_report_menu_update (plugin_page, tooltip_list);

    /* Enable the FilePrintAction */
    action = gnc_main_window_find_action (window, "FilePrintAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), true);

    if (priv->webkit2)
    {
        GtkWidget *pdf_item = gnc_main_window_menu_find_menu_item (window, "FilePrintPDFAction");
        gtk_widget_hide (pdf_item);
    }
    g_free (saved_reports_path);
    g_free (report_save_str);
    g_free (report_saveas_str);
}

static void
gnc_plugin_page_report_constr_init (GncPluginPageReport *plugin_page, gint reportId)
{
    GncPluginPageReportPrivate *priv;
    GSimpleActionGroup *simple_action_group;
    GncPluginPage *parent;
    gboolean use_new;
    gchar *name;

    DEBUG("property reportId=%d", reportId);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(plugin_page);
    priv->reportId = reportId;
    priv->webkit2 = FALSE;

    gnc_plugin_page_report_setup( GNC_PLUGIN_PAGE(plugin_page));

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    use_new = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_USE_NEW);
    name = gnc_report_name (priv->initial_report);
    g_object_set (G_OBJECT(plugin_page),
                  "page-name",      name,
                  "ui-description", "gnc-plugin-page-report.ui",
                  "use-new-window", use_new,
                  nullptr);
    g_free (name);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book (parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    simple_action_group = gnc_plugin_page_create_action_group (parent, "GncPluginPageReportActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     report_actions,
                                     num_report_actions,
                                     plugin_page);

    gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), initially_insensitive_actions,
                                    FALSE);
}

GncPluginPage*
gnc_plugin_page_report_new( int reportId )
{
    DEBUG( "report id = %d", reportId );
    auto plugin_page{g_object_new(GNC_TYPE_PLUGIN_PAGE_REPORT, "report-id",
                                  reportId, nullptr)};
    DEBUG( "plugin_page: %p", plugin_page );
    DEBUG( "set %d on page %p", reportId, plugin_page );
    return GNC_PLUGIN_PAGE( plugin_page );
}

void
gnc_plugin_page_report_remove_edited_report(GncPluginPageReportPrivate *priv,
        SCM report)
{
    SCM new_edited = scm_delete(priv->edited_reports, report);
    if (priv->edited_reports != SCM_EOL)
        scm_gc_unprotect_object(priv->edited_reports);
    priv->edited_reports = new_edited;
    if (new_edited != SCM_EOL)
        scm_gc_protect_object(priv->edited_reports);
}

void
gnc_plugin_page_report_add_edited_report(GncPluginPageReportPrivate *priv,
        SCM report)
{
    SCM new_edited = scm_cons(report, priv->edited_reports);
    if (priv->edited_reports != SCM_EOL)
        scm_gc_unprotect_object(priv->edited_reports);
    priv->edited_reports = new_edited;
    if (new_edited != SCM_EOL)
        scm_gc_protect_object(priv->edited_reports);
}

void
gnc_plugin_page_report_raise_editor(SCM report)
{
    SCM get_editor = scm_c_eval_string("gnc:report-editor-widget");
    SCM editor = scm_call_1(get_editor, report);
#define FUNC_NAME "gtk_window_present"
    auto w{static_cast<GtkWidget *>(SWIG_MustGetPtr(editor, SWIG_TypeQuery("_p_GtkWidget"), 1, 0))};
#undef FUNC_NAME
    gtk_window_present(GTK_WINDOW(w));
}

static void
close_handler (gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(user_data);
    DEBUG("in close handler\n");
    gnc_main_window_close_page (plugin_page);
}

static void
gnc_plugin_page_report_set_fwd_button (GncPluginPageReport *report, int enabled)
{
    GAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(report),
                                                  "ReportForwAction");
    if (action != NULL)
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), enabled);
}

static void
gnc_plugin_page_report_set_back_button (GncPluginPageReport *report, int enabled)
{
    GAction *action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(report),
                                                  "ReportBackAction");
    if (action != NULL)
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), enabled);
}

// ------------------------------------------------------------
// GTK ACTION CALLBACKS

static void
gnc_plugin_page_report_forw_cb (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    gnc_html_history_node * node = nullptr;

    DEBUG( "forw" );
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gnc_html_history_forward(gnc_html_get_history(priv->html));
    node = gnc_html_history_get_current(gnc_html_get_history(priv->html));
    if (node)
    {
        gnc_html_show_url(priv->html, node->type, node->location,
                          node->label, 0);
    }
}

static void
gnc_plugin_page_report_back_cb (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    gnc_html_history_node * node;

    DEBUG( "back" );
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gnc_html_history_back(gnc_html_get_history(priv->html));
    node = gnc_html_history_get_current(gnc_html_get_history(priv->html));
    if (node)
    {
        gnc_html_show_url(priv->html, node->type, node->location,
                          node->label, 0);
    }
}

void
gnc_plugin_page_report_reload (GncPluginPageReport *report)
{
    gnc_plugin_page_report_reload_cb (nullptr, nullptr, report);
}

static void
gnc_plugin_page_report_reload_cb (GSimpleAction *simple,
                                  GVariant *parameter,
                                  gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPage *page;
    GncPluginPageReportPrivate *priv;
    SCM dirty_report;

    DEBUG( "reload" );
    page = GNC_PLUGIN_PAGE(report);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (priv->cur_report == SCM_BOOL_F)
        return;

    DEBUG( "reload-redraw" );
    dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
    scm_call_2(dirty_report, priv->cur_report, SCM_BOOL_T);

    /* now queue the fact that we need to reload this report */

    // prevent closing this page while loading...
    priv->reloading = TRUE;
    // this sets the window for the progressbar
    gnc_window_set_progressbar_window( GNC_WINDOW(page->window) );

    // this sets the minimum size of the progressbar to that allocated
    gnc_plugin_page_report_set_progressbar( page, TRUE );

    gnc_html_reload( priv->html, TRUE ); //reload by rebuild

    gnc_plugin_page_report_set_progressbar( page, FALSE );

    // this resets the window for the progressbar to nullptr
    gnc_window_set_progressbar_window( nullptr );
    priv->reloading = FALSE;
}

static void
gnc_plugin_page_report_stop_cb (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gnc_html_cancel(priv->html);
}

/* Returns SCM_BOOL_F if cancel. Returns SCM_BOOL_T if html.
 * Otherwise returns pair from export_types. */
static SCM
gnc_get_export_type_choice (SCM export_types, GtkWindow *parent)
{
    GList * choices = nullptr;
    gboolean bad = FALSE;
    GList * node;
    int choice;
    SCM tail;

    if (!scm_is_list (export_types))
        return SCM_BOOL_F;

    for (tail = export_types; !scm_is_null (tail); tail = SCM_CDR (tail))
    {
        SCM pair = SCM_CAR (tail);
        char * name;
        SCM scm;

        if (!scm_is_pair (pair))
        {
            g_warning ("unexpected list element");
            bad = TRUE;
            break;
        }

        scm = SCM_CAR (pair);
        if (!scm_is_string (scm))
        {
            g_warning ("unexpected pair element");
            bad = TRUE;
            break;
        }

        name = gnc_scm_to_utf8_string (scm);
        choices = g_list_prepend (choices, name);
    }

    if (!bad)
    {
        choices = g_list_reverse (choices);

        choices = g_list_prepend (choices, g_strdup (_("HTML")));

        choice = gnc_choose_radio_option_dialog
            (GTK_WIDGET (parent), _("Choose export format"),
             _("Choose the export format for this report:"),
             nullptr, 0, choices);
    }
    else
        choice = -1;

    for (node = choices; node; node = node->next)
        g_free (node->data);
    g_list_free (choices);

    if (choice < 0)
        return SCM_BOOL_F;

    if (choice == 0)
        return SCM_BOOL_T;

    choice--;
    if (choice >= scm_ilength (export_types))
        return SCM_BOOL_F;

    return scm_list_ref (export_types, scm_from_int  (choice));
}

static char *
gnc_get_export_filename (SCM choice, GtkWindow *parent)
{
    char * filepath;
    GStatBuf statbuf;
    char * title;
    const gchar * html_type = _("HTML");
    char * type;
    int rc;
    char * default_dir;

    if (choice == SCM_BOOL_T)
        type = g_strdup (html_type);
    else
        type = gnc_scm_to_utf8_string(SCM_CAR (choice));

    /* %s is the type of what is about to be saved, e.g. "HTML". */
    title = g_strdup_printf (_("Save %s To File"), type);
    default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_REPORT);

    filepath = gnc_file_dialog (parent, title, nullptr, default_dir,
                                GNC_FILE_DIALOG_EXPORT);

    if (filepath != nullptr) // test for cancel pressed
    {
        /* Try to test for extension on file name, add if missing */
        if (g_strrstr(filepath, ".") == nullptr)
            filepath = g_strconcat(filepath, ".", g_ascii_strdown(type, strlen(type)), nullptr);
    }
    g_free (type);
    g_free (title);
    g_free (default_dir);

    if (!filepath)
        return nullptr;

    default_dir = g_path_get_dirname(filepath);
    gnc_set_default_directory (GNC_PREFS_GROUP_REPORT, default_dir);
    g_free(default_dir);

    rc = g_stat (filepath, &statbuf);

    /* Check for an error that isn't a non-existent file. */
    if (rc != 0 && errno != ENOENT)
    {
        /* %s is the strerror(3) string of the error that occurred. */
        const char *format = _("You cannot save to that filename.\n\n%s");

        gnc_error_dialog (parent, format, strerror(errno));
        g_free(filepath);
        return nullptr;
    }

    /* Check for a file that isn't a regular file. */
    if (rc == 0 && !S_ISREG (statbuf.st_mode))
    {
        const char *message = _("You cannot save to that file.");

        gnc_error_dialog (parent, "%s", message);
        g_free(filepath);
        return nullptr;
    }

    if (rc == 0)
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        if (!gnc_verify_dialog (parent, FALSE, format, filepath))
        {
            g_free(filepath);
            return nullptr;
        }
    }

    return filepath;
}

static void
gnc_plugin_page_report_edit_tax_cb (GSimpleAction *simple,
                                    GVariant *parameter,
                                    gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GtkWidget *window;

    window = GNC_PLUGIN_PAGE(report)->window;
    gnc_tax_info_dialog (window, nullptr);
}

static void
gnc_plugin_page_report_save_as_cb (GSimpleAction *simple,
                                   GVariant *parameter,
                                   gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    SCM save_func;
    SCM rpt_id;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (priv->cur_report == SCM_BOOL_F)
        return;

    /* Create a new report template based on the current report's settings
     * and allow the user to rename the template.
     */
    save_func = scm_c_eval_string("gnc:report-to-template-new");
    rpt_id = scm_call_1(save_func, priv->cur_report);

    /* Open Preconfigured Reports dialog to allow user to change the name */
    if (!scm_is_null (rpt_id))
    {
        GncPluginPage *reportPage = GNC_PLUGIN_PAGE (report);
        GtkWidget *window = reportPage->window;

        if (window)
            g_return_if_fail(GNC_IS_MAIN_WINDOW(window));

        gnc_ui_custom_report_edit_name (GNC_MAIN_WINDOW (window), rpt_id);
    }

}

static void
gnc_plugin_page_report_save_cb (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    SCM check_func, save_func;
    SCM rpt_id;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (priv->cur_report == SCM_BOOL_F)
        return;

    check_func = scm_c_eval_string("gnc:is-custom-report-type");
    if (scm_is_true (scm_call_1 (check_func, priv->cur_report)))
    {
        /* The current report is already based on a custom report.
         * Replace the existing one instead of adding a new one
         */
        save_func = scm_c_eval_string("gnc:report-to-template-update");
        rpt_id = scm_call_1(save_func, priv->cur_report);
        (void)rpt_id;
    }
    else
    {
        /* The current report is not based on a custom report.
         * So let's create a new report template based on this report
         * and allow the user to change the name.
         */
        gnc_plugin_page_report_save_as_cb (simple, parameter, report);
    }
}

static void
gnc_plugin_page_report_export_cb (GSimpleAction *simple,
                                  GVariant *parameter,
                                  gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    char * filepath;
    SCM export_types;
    SCM export_thunk;
    gboolean result;
    SCM choice;
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window
                                    (GNC_PLUGIN_PAGE (report)));

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    export_types = scm_call_1 (scm_c_eval_string ("gnc:report-export-types"),
                               priv->cur_report);

    export_thunk = scm_call_1 (scm_c_eval_string ("gnc:report-export-thunk"),
                               priv->cur_report);

    if (scm_is_list (export_types) && scm_is_procedure (export_thunk))
        choice = gnc_get_export_type_choice (export_types, parent);
    else
        choice = SCM_BOOL_T;

    if (choice == SCM_BOOL_F)
        return;

    filepath = gnc_get_export_filename (choice, parent);
    if (!filepath)
        return;

    if (scm_is_pair (choice))
    {
        SCM type = scm_cdr (choice);
        SCM document = scm_call_2 (export_thunk, priv->cur_report, type);
        SCM query_result = scm_c_eval_string ("gnc:html-document?");
        SCM get_export_string = scm_c_eval_string ("gnc:html-document-export-string");
        SCM get_export_error = scm_c_eval_string ("gnc:html-document-export-error");

        if (scm_is_false (scm_call_1 (query_result, document)))
            gnc_error_dialog (parent, "%s",
                              _("This report must be upgraded to return a "
                                "document object with export-string or "
                                "export-error."));
        else
        {
            SCM export_string = scm_call_1 (get_export_string, document);
            SCM export_error = scm_call_1 (get_export_error, document);

            if (scm_is_string (export_string))
            {
                GError *err = nullptr;
                gchar *exported = scm_to_utf8_string (export_string);
                if (!g_file_set_contents (filepath, exported, -1, &err))
                    gnc_error_dialog (parent, "Error during export: %s", err->message);
                g_free (exported);
                if (err)
                    g_error_free (err);
            }
            else if (scm_is_string (export_error))
            {
                gchar *str = scm_to_utf8_string (export_error);
                gnc_error_dialog (parent, "error during export: %s", str);
                g_free (str);
            }
            else
                gnc_error_dialog (parent, "%s",
                                   _("This report must be upgraded to return a "
                                     "document object with export-string or "
                                     "export-error."));
        }
        result = TRUE;
    }
    else
        result = gnc_html_export_to_file (priv->html, filepath);

    if (!result)
    {
        const char *fmt = _("Could not open the file %s. "
                            "The error is: %s");
        gnc_error_dialog (parent, fmt, filepath ? filepath : "(null)",
                          strerror (errno) ? strerror (errno) : "" );
    }

    g_free(filepath);
    return;
}

static void
gnc_plugin_page_report_options_cb (GSimpleAction *simple,
                                   GVariant *parameter,
                                   gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (report)));
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (priv->cur_report == SCM_BOOL_F)
        return;

    if (gnc_report_edit_options (priv->cur_report, parent))
        gnc_plugin_page_report_add_edited_report(priv, priv->cur_report);
}

static GncInvoice*
lookup_invoice(GncPluginPageReportPrivate *priv)
{
    const QofInstance* opt_val =
        gnc_option_db_lookup_qofinstance_value(priv->cur_odb, "General",
                                               "Invoice Number");
    return GNC_INVOICE(opt_val);
}

#define GNC_PREFS_GROUP_REPORT_PDFEXPORT GNC_PREFS_GROUP_GENERAL_REPORT ".pdf-export"
#define GNC_PREF_FILENAME_DATE_FMT "filename-date-format"
#define GNC_PREF_FILENAME_FMT "filename-format"

static gchar *report_create_jobname(GncPluginPageReportPrivate *priv)
{
    gchar *job_name = nullptr;
    gchar *report_name = nullptr;
    const gchar *report_number = "";
    gchar *job_date;
    const gchar *default_jobname = N_("GnuCash-Report");

    g_assert(priv);

    {
        // Look up the date format that was chosen in the preferences database
        QofDateFormat date_format_here = QOF_DATE_FORMAT_ISO;
        char *format_code = gnc_prefs_get_string (GNC_PREFS_GROUP_REPORT_PDFEXPORT,
                                                  GNC_PREF_FILENAME_DATE_FMT);
        const gchar *date_format_string;
        if (!(format_code && *format_code))
        {
            g_free(format_code);
            format_code = g_strdup("locale");
        }

        if (gnc_date_string_to_dateformat (format_code, &date_format_here))
            PERR("Incorrect date format code, using ISO-8601.");

        date_format_string = qof_date_format_get_string (date_format_here);

        job_date = gnc_print_time64 (gnc_time (nullptr), date_format_string);
        g_free (format_code);
    }


    if (priv->cur_report == SCM_BOOL_F)
        report_name = g_strdup (_(default_jobname));
    else
    {
        /* Gather some information from the report to generate a
         * decent print job name.
         * FIXME: this is a bit of a hack. It would be better if each
         *        report had a hidden job name option, because the
         *        generic reporting code shouldn't know what makes
         *        a decent job name for each report.
         *
         *        Also, the "Report name" for an invoice report is
         *        "Printable Invoice", which is not what the user wants to see,
         *        so I added yet another hack below for this. cstim.
         */
        GncInvoice *invoice;
        auto report_name_str = priv->cur_odb->lookup_string_option("General",
                                                                   "Report name");
        if (!report_name_str.empty())
            report_name = g_strdup(report_name_str.c_str());
        else
            report_name = g_strdup (_(default_jobname));

        if (g_strcmp0(report_name, _("Printable Invoice")) == 0
                || g_strcmp0(report_name, _("Tax Invoice")) == 0
                || g_strcmp0(report_name, _("Easy Invoice")) == 0
                || g_strcmp0(report_name, _("Fancy Invoice")) == 0)
        {
            /* Again HACK alert: We modify this single known string here into
             * something more appropriate. */
            g_free(report_name);
            report_name = g_strdup(_("Invoice"));
        }

        invoice = lookup_invoice(priv);
        if (invoice)
        {
            // Report is for an invoice. Hence, we get a number of the invoice.
            report_number = gncInvoiceGetID(invoice);
        }
    }

    if (report_name && job_date)
    {
        // Look up the printf format of the output name from the preferences database
        char* format = gnc_prefs_get_string(GNC_PREFS_GROUP_REPORT_PDFEXPORT, GNC_PREF_FILENAME_FMT);

        if (format && *format)
        {
            job_name = g_strdup_printf(format, report_name,
                                       report_number, job_date);
        }
        else
        {
            PWARN("No GNC_PREF_FILENAME_FMT!");
            job_name = g_strdup_printf ("%s %s %s", report_name,
                                         report_number, job_date);
        }
        g_free(format);
    }
    g_free (report_name);
    g_free (job_date);

    {
        char forbidden_char = '/';
        // Now remove the characters that are not allowed in file
        // names. FIXME: Check for all disallowed characters here!
        while (strchr(job_name, forbidden_char))
        {
            *strchr(job_name, forbidden_char) = '_';
        }
    }

    {
        /* And one final checking issue: We want to avoid allocating
         * the same name twice for a saved PDF.  Hence, we keep a
         * GHashTable with the usage count of existing output
         * names. (Because I'm lazy, I just use a static GHashTable
         * for this.) */
        gpointer value;
        gboolean already_found;
        g_assert(static_report_printnames);

        // Lookup the existing usage count
        value = g_hash_table_lookup(static_report_printnames, job_name);
        already_found = (value != nullptr);
        if (!value)
        {
            value = GINT_TO_POINTER(0);
        }

        // Increment the stored usage count
        value = GINT_TO_POINTER(1 + GPOINTER_TO_INT(value));
        // and store it again
        g_hash_table_insert(static_report_printnames, g_strdup(job_name), value);

        // If the previous usage count was more than 0, append the current
        // count (which is now 2 or higher) to the resulting name
        if (already_found)
        {
            // The name was already in use, so modify the name again
            gchar *tmp = g_strdup_printf("%s_%d", job_name, (int) GPOINTER_TO_INT(value));
            g_free(job_name);
            job_name = tmp;
        }
    }

    return job_name;
}

static void
gnc_plugin_page_report_print_cb (GSimpleAction *simple,
                                 GVariant *parameter,
                                 gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gchar *job_name = report_create_jobname(priv);

    //g_warning("Setting job name=%s", job_name);

#ifdef WEBKIT1
    gnc_html_print (priv->html, job_name, FALSE);
#else
    gnc_html_print (priv->html, job_name);
#endif

    g_free (job_name);
}

static void
gnc_plugin_page_report_exportpdf_cb (GSimpleAction *simple,
                                     GVariant *parameter,
                                     gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gchar *job_name = report_create_jobname(priv);
    GncInvoice *invoice;
    GncOwner *owner = nullptr;

    // Do we have an invoice report?
    invoice = lookup_invoice(priv);
    if (invoice)
    {
        // Does this invoice also have an owner?
        owner = (GncOwner*) gncInvoiceGetOwner(invoice);
        if (owner)
        {
            QofInstance *inst = qofOwnerGetOwner (owner);
            gchar *dirname = nullptr;
            qof_instance_get (inst, "export-pdf-dir", &dirname, nullptr);
            // Yes. In the kvp, look up the key for the Export-PDF output
            // directory. If it exists, prepend this to the job name so that
            // we can export to PDF.
            if (dirname && g_file_test (dirname,
                       (GFileTest)(G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR)))
            {
                gchar *tmp = g_build_filename (dirname, job_name, nullptr);
                g_free (job_name);
                job_name = tmp;
            }
        }
    }

    //g_warning("Setting job name=%s", job_name);

#ifdef WEBKIT1
    gnc_html_print (priv->html, job_name, TRUE);
#else
    gnc_html_print (priv->html, job_name);
#endif

    if (owner)
    {
        /* As this is an invoice report with some owner, we will try
         * to look up the chosen output directory from the print
         * settings and store it again in the owner kvp.
         */
        GtkPrintSettings *print_settings = gnc_print_get_settings();
        if (print_settings && gtk_print_settings_has_key (print_settings,
                                  GNC_GTK_PRINT_SETTINGS_EXPORT_DIR))
        {
            const char* dirname = gtk_print_settings_get (print_settings,
                                      GNC_GTK_PRINT_SETTINGS_EXPORT_DIR);
            // Only store the directory if it exists.
            if (g_file_test (dirname,
                            (GFileTest)(G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR)))
            {
                QofInstance *inst = qofOwnerGetOwner (owner);
                gncOwnerBeginEdit (owner);
                qof_instance_set (inst, "export-pdf-dir", dirname);
                gncOwnerCommitEdit (owner);
            }
        }
    }
    g_free (job_name);
}

static void
gnc_plugin_page_report_copy_cb (GSimpleAction *simple,
                                GVariant *parameter,
                                gpointer user_data)
{
    GncPluginPageReport *report = (GncPluginPageReport*)user_data;
    GncPluginPageReportPrivate *priv;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gnc_html_copy_to_clipboard(priv->html);
}

/********************************************************************
 * gnc_main_window_open_report()
 * open an report in a top level window from an ID number
 ********************************************************************/

void
gnc_main_window_open_report(int report_id, GncMainWindow *window)
{
    GncPluginPage *reportPage;

    if (window)
        g_return_if_fail(GNC_IS_MAIN_WINDOW(window));

    reportPage = gnc_plugin_page_report_new( report_id );
    gnc_main_window_open_page( window, reportPage );
}

void
gnc_main_window_open_report_url(const char * url, GncMainWindow *window)
{
    GncPluginPage *reportPage;

    DEBUG( "report url: [%s]\n", url );

    if (window)
        g_return_if_fail(GNC_IS_MAIN_WINDOW(window));

    reportPage = gnc_plugin_page_report_new( 42 /* url? */ );
    gnc_main_window_open_page( window, reportPage );
}

/** @} */
/** @} */

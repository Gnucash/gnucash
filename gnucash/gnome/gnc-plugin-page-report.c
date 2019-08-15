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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <libguile.h>
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
#include "gnc-plugin.h"
#include "gnc-plugin-page-report.h"
#include "gnc-plugin-file-history.h"
#include "gnc-prefs.h"
#include "gnc-report.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-window.h"
#include "option-util.h"
#include "window-report.h"
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "business-options.h"
#include "gnc-icons.h"
#include "print-session.h"

#define WINDOW_REPORT_CM_CLASS "window-report"

/* NW: you can add GNC_MOD_REPORT to gnc-engine.h
or simply define it locally. Any unique string with
a gnucash- prefix will do. Then just set a log level
with qof_log_set_level().*/
static QofLogModule log_module = GNC_MOD_GUI;

static GObjectClass *parent_class = NULL;

// A static GHashTable to record the usage count for each printer
// output name. FIXME: Currently this isn't cleaned up at program
// shutdown because there isn't a place to easily insert a finalize()
// function for this. Oh well.
static GHashTable *static_report_printnames = NULL;

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
    GNCOptionDB *cur_odb;
    SCM option_change_cb_id;

    /* initial_report is special; it's the one that's saved and
     * restored.  The name_change_callback only gets called when
     * the initial_report name is changed. */
    SCM          initial_report;
    GNCOptionDB  * initial_odb;
    SCM          name_change_cb_id;

    /* keep a list of edited reports so that we can destroy them when
     * the window is closed. */
    SCM          edited_reports;

    /* The page is in the process of reloading the html */
    gboolean	reloading;

    /// the gnc_html abstraction this PluginPage contains
//        gnc_html *html;
    GncHtml *html;

    /// the container the above HTML widget is in.
    GtkContainer *container;
} GncPluginPageReportPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageReport, gnc_plugin_page_report, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_REPORT, GncPluginPageReportPrivate))

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
void gnc_plugin_page_report_raise_editor(SCM report);

static void gnc_plugin_page_report_forw_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_back_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_reload_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_stop_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_save_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_save_as_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_export_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_options_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_print_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_exportpdf_cb(GtkAction *action, GncPluginPageReport *rep);
static void gnc_plugin_page_report_copy_cb(GtkAction *action, GncPluginPageReport *rep);

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

static gboolean
gnc_plugin_page_report_focus (GtkWidget *widget)
{
    if (GTK_IS_WIDGET(widget))
    {
        if (!gtk_widget_is_focus (GTK_WIDGET(widget)))
            gtk_widget_grab_focus (GTK_WIDGET(widget));
    }
    return FALSE;
}

/**
 * Whenever the current page is changed, if a report page is
 * the current page, set focus on the report.
 */
static void
gnc_plugin_page_report_main_window_page_changed (GncMainWindow *window,
        GncPluginPage *plugin_page, gpointer user_data)
{
    // We continue only if the plugin_page is a valid
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    if (gnc_main_window_get_current_page (window) == plugin_page)
    {
        GncPluginPageReport *report;
        GncPluginPageReportPrivate *priv;
        GtkWidget *widget;

        if (!GNC_IS_PLUGIN_PAGE_REPORT(plugin_page))
            return;

        report = GNC_PLUGIN_PAGE_REPORT(plugin_page);
        priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
        widget = gnc_html_get_widget(priv->html);

        // The page changed signal is emitted multiple times so we need
        // to use an idle_add to change the focus to the webkit widget
        g_idle_remove_by_data (widget);
        g_idle_add ((GSourceFunc)gnc_plugin_page_report_focus, widget);
    }
}

static void
gnc_plugin_page_report_class_init (GncPluginPageReportClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_page_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

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

    // create the "reportId" property
    g_object_class_install_property( object_class,
                                     PROP_REPORT_ID,
                                     g_param_spec_int( "report-id",
                                             _("The numeric ID of the report."),
                                             _("The numeric ID of the report."),
                                             -1, G_MAXINT, -1, G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE ) );

    /* JSLED: report-selected?
    	plugin_page_signals[ACCOUNT_SELECTED] =
    	  g_signal_new ("account_selected",
    			G_OBJECT_CLASS_TYPE (object_class),
    			G_SIGNAL_RUN_FIRST,
    			G_STRUCT_OFFSET (GncPluginPageReportClass, account_selected),
    			NULL, NULL,
    			g_cclosure_marshal_VOID__POINTER,
    			G_TYPE_NONE, 1,
    			G_TYPE_POINTER);
    */

    // Also initialize the report name usage count table
    if (!static_report_printnames)
        static_report_printnames = g_hash_table_new_full(g_str_hash,
                                   g_str_equal, g_free, NULL);
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

static gboolean
gnc_plugin_page_report_load_uri (GncPluginPage *page)
{
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    URLType type;
    char * id_name;
    char * child_name;
    char * url_location = NULL;
    char * url_label = NULL;

    report = GNC_PLUGIN_PAGE_REPORT(page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (!priv)
        return FALSE; // No priv means the page doesn't exist anymore.

    DEBUG( "Load uri id=%d", priv->reportId );
    id_name = g_strdup_printf("id=%d", priv->reportId );
    child_name = gnc_build_url( URL_TYPE_REPORT, id_name, NULL );
    type = gnc_html_parse_url( priv->html, child_name, &url_location, &url_label);
    DEBUG( "passing id_name=[%s] child_name=[%s] type=[%s], location=[%s], label=[%s]",
           id_name, child_name ? child_name : "(null)",
           type ? type : "(null)", url_location ? url_location : "(null)",
           url_label ? url_label : "(null)" );

    g_free(id_name);
    g_free(child_name);

    gtk_widget_show_all( GTK_WIDGET(priv->container) );

    // this sets the window for the progressbar
    gnc_window_set_progressbar_window( GNC_WINDOW(page->window) );

    // this sets the minimum size of the progressbar to that allocated
    gnc_plugin_page_report_set_progressbar( page, TRUE );

    gnc_html_show_url(priv->html, type, url_location, url_label, 0);
    g_free(url_location);

    gnc_plugin_page_report_set_progressbar( page, FALSE );

    // this resets the window for the progressbar to NULL
    gnc_window_set_progressbar_window( NULL );

    return FALSE;
}

static void
gnc_plugin_page_report_realize_uri (GtkWidget *widget, GncPluginPage *page)
{
    GtkAllocation allocation;

    gtk_widget_get_allocation (widget, &allocation);
    PINFO("Realized Container size is %dw x %dh", allocation.width, allocation.height);

    /* load uri when view idle */
    g_idle_add ((GSourceFunc)gnc_plugin_page_report_load_uri, page);
}

static
GtkWidget*
gnc_plugin_page_report_create_widget( GncPluginPage *page )
{
    GncPluginPageReport *report;
    GncPluginPageReportPrivate *priv;
    GncMainWindow  *window;
    GtkWindow *topLvl;
    GtkAction *action;
    URLType type;
    char * id_name;
    char * child_name;
    char * url_location = NULL;
    char * url_label = NULL;

    ENTER("page %p", page);

#ifndef WEBKIT1
    /* Hide the ExportPdf action for Webkit2 */
    action = gnc_plugin_page_get_action (page, "FilePrintPDFAction");
    gtk_action_set_sensitive (action, FALSE);
    gtk_action_set_visible (action, FALSE);
#endif

    report = GNC_PLUGIN_PAGE_REPORT(page);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);

    topLvl = gnc_ui_get_main_window (NULL);
//        priv->html = gnc_html_new( topLvl );
    priv->html = gnc_html_factory_create_html();
    gnc_html_set_parent( priv->html, topLvl );

    gnc_html_history_set_node_destroy_cb(gnc_html_get_history(priv->html),
                                         gnc_plugin_page_report_history_destroy_cb,
                                         (gpointer)priv);

    priv->container = GTK_CONTAINER(gtk_frame_new(NULL));
    gtk_frame_set_shadow_type(GTK_FRAME(priv->container), GTK_SHADOW_NONE);

    // Set the style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(priv->container), "GncReportPage");

    gtk_container_add(GTK_CONTAINER(priv->container),
                      gnc_html_get_widget(priv->html));

    priv->component_manager_id =
        gnc_register_gui_component(WINDOW_REPORT_CM_CLASS, NULL,
                                   close_handler, page);
    gnc_gui_component_set_session(priv->component_manager_id,
                                  gnc_get_current_session());

    gnc_html_set_urltype_cb(priv->html, gnc_plugin_page_report_check_urltype);
    gnc_html_set_load_cb(priv->html, gnc_plugin_page_report_load_cb, report);

    /* We need to call the load call back so the report appears to have been run
     so it will get saved properly if the report is not realized in session */
    id_name = g_strdup_printf("id=%d", priv->reportId );
    child_name = gnc_build_url( URL_TYPE_REPORT, id_name, NULL );
    type = gnc_html_parse_url( priv->html, child_name, &url_location, &url_label);

    gnc_plugin_page_report_load_cb (priv->html, type, id_name, url_label, report);
    g_free(id_name);
    g_free(child_name);

    // FIXME.  This is f^-1(f(x)), isn't it?
    DEBUG( "id=%d", priv->reportId );

    /* load uri when view is realized */
    g_signal_connect (G_OBJECT(GTK_WIDGET(priv->container)), "realize",
                      G_CALLBACK(gnc_plugin_page_report_realize_uri), page);

    window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(page)->window);
    g_signal_connect(window, "page_changed",
                     G_CALLBACK(gnc_plugin_page_report_main_window_page_changed),
                     page);

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
    priv->name_change_cb_id = SCM_BOOL_F;

    g_object_get( ppage, "report-id", &report_id, NULL );

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
//gnc_plugin_page_report_load_cb(gnc_html * html, URLType type,
gnc_plugin_page_report_load_cb(GncHtml * html, URLType type,
                               const gchar * location, const gchar * label,
                               gpointer data)
{
    GncPluginPageReport *report = GNC_PLUGIN_PAGE_REPORT(data);
    GncPluginPageReportPrivate *priv;
    int  report_id;
    SCM  get_options    = scm_c_eval_string("gnc:report-options");
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

        priv->initial_odb = gnc_option_db_new(scm_call_1(get_options, inst_report));
        priv->name_change_cb_id =
            gnc_option_db_register_change_callback(priv->initial_odb,
                    gnc_plugin_page_report_refresh,
                    priv,
                    "General", "Report name");
    }

    if ((priv->cur_report != SCM_BOOL_F) && (priv->cur_odb != NULL))
    {
        gnc_option_db_unregister_change_callback_id(priv->cur_odb,
                priv->option_change_cb_id);
        gnc_option_db_destroy(priv->cur_odb);
        priv->cur_odb = NULL;
    }

    if (priv->cur_report != SCM_BOOL_F)
        scm_gc_unprotect_object(priv->cur_report);
    priv->cur_report = inst_report;
    scm_gc_protect_object(priv->cur_report);

    priv->cur_odb = gnc_option_db_new(scm_call_1(get_options, inst_report));
    priv->option_change_cb_id =
        gnc_option_db_register_change_callback(priv->cur_odb,
                gnc_plugin_page_report_option_change_cb,
                report, NULL, NULL);

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
    const gchar *old_name;
    gchar *new_name;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REPORT(data));
    report = GNC_PLUGIN_PAGE_REPORT(data);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    page = GNC_PLUGIN_PAGE(report);

    DEBUG( "option_change" );
    if (priv->cur_report == SCM_BOOL_F)
        return;
    DEBUG( "set-dirty, queue-draw" );

    /* Update the page (i.e. the notebook tab and window title) */
    old_name = gnc_plugin_page_get_page_name(GNC_PLUGIN_PAGE(report));
    new_name = gnc_option_db_lookup_string_option(priv->cur_odb, "General",
               "Report name", NULL);
    if (strcmp(old_name, new_name) != 0)
    {
        /* Bug 727130, 760711 - remove only the non-printable
         * characters from the new name */
        gnc_utf8_strip_invalid_and_controls(new_name);
        ENTER("Cleaned-up new report name: %s", new_name);
        main_window_update_page_name(GNC_PLUGIN_PAGE(report), new_name);
	}
    g_free(new_name);

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

    // this resets the window for the progressbar to NULL
    gnc_window_set_progressbar_window( NULL );

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
    // something like ... gnc_plugin_page_report_redraw( NULL, (GncPluginPageReportPrivate*)data );
    return;
}

static void
gnc_plugin_page_report_destroy_widget(GncPluginPage *plugin_page)
{
    GncPluginPageReportPrivate *priv;
    GtkWidget *widget;

    // FIXME: cleanup other resources.

    PINFO("destroy widget");
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(plugin_page);

    widget = gnc_html_get_widget(priv->html);

    // Remove the page focus idle function if present
    g_idle_remove_by_data (widget);

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
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

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
    GError *error = NULL;
    gchar *option_string;
    gint report_id;
    SCM scm_id, final_id = SCM_BOOL_F;
    SCM report;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    keys = g_key_file_get_keys(key_file, group_name, &num_keys, &error);
    if (error)
    {
        g_warning("error reading group %s key list: %s",
                  group_name, error->message);
        g_error_free(error);
        LEAVE("no keys");
        return NULL;
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
            LEAVE("bad value");
            return NULL;
        }
        scm_id = scm_eval_string(scm_from_utf8_string(option_string));
        g_free(option_string);

        if (!scm_integer_p(scm_id))
        {
            DEBUG("report id not an integer for key %s", keys[i]);
            return NULL;
        }

        if (final_id == SCM_BOOL_F)
        {
            if (g_strcmp0(keys[i], SCHEME_OPTIONS) == 0)
            {
                final_id = scm_id;
            }
        }
    }

    if (final_id == SCM_BOOL_F)
    {
        LEAVE("report not specified");
        return NULL;
    }

    report_id = scm_to_int(final_id);
    report = gnc_report_find(report_id);
    if (!report)
    {
        LEAVE("report doesn't exist");
        return NULL;
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
    const gchar *old_name;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_REPORT(page));
    g_return_if_fail(name != NULL);

    ENTER("page %p, name %s", page, name);
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(page);

    /* Is this a redundant call? */
    old_name = gnc_option_db_lookup_string_option(priv->cur_odb, "General",
               "Report name", NULL);
    DEBUG("Comparing old name '%s' to new name '%s'",
          old_name ? old_name : "(null)", name);
    if (old_name && (strcmp(old_name, name) == 0))
    {
        LEAVE("no change");
        return;
    }

    /* Store the new name for the report. */
    gnc_option_db_set_string_option(priv->cur_odb, "General",
                                    "Report name", name);

    /* Have to manually call the option change hook. */
    gnc_plugin_page_report_option_change_cb(page);
    LEAVE(" ");
}

static void
gnc_plugin_page_report_update_edit_menu (GncPluginPage *page, gboolean hide)
{
    GtkAction *action;

    action = gnc_plugin_page_get_action (page, "EditCopyAction");
    gtk_action_set_sensitive (action, TRUE);
    gtk_action_set_visible (action, TRUE);
    action = gnc_plugin_page_get_action (page, "EditCutAction");
    gtk_action_set_sensitive (action, FALSE);
    gtk_action_set_visible (action, !hide);
    action = gnc_plugin_page_get_action (page, "EditPasteAction");
    gtk_action_set_sensitive (action, FALSE);
    gtk_action_set_visible (action, !hide);
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
            GtkWidget *w = NULL;
#define FUNC_NAME "gtk_widget_destroy"
            w = SWIG_MustGetPtr(editor,
                                SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
#undef FUNC_NAME
            gtk_widget_destroy(GTK_WIDGET(w));
        }
    }

    if (priv->initial_odb)
    {
        gnc_option_db_unregister_change_callback_id(priv->initial_odb,
                priv->name_change_cb_id);

        gnc_option_db_destroy(priv->initial_odb);
        priv->initial_odb = NULL;
    }

    gnc_html_destroy(priv->html);

    priv->container     = NULL;
    priv->html          = NULL;

    if (priv->cur_report != SCM_BOOL_F)
        scm_gc_unprotect_object(priv->cur_report);
    if (priv->edited_reports != SCM_EOL)
        scm_gc_unprotect_object(priv->edited_reports);
}

/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] =
{
    { "FilePrintAction", 	    N_("Print") },
    { "ReportExportAction",   N_("Export") },
    { "ReportOptionsAction",  N_("Options") },
    /* Translators: This string is meant to be a short alternative for "Save Report Configuration"
     * to be used as toolbar button label. */
    { "ReportSaveAction", N_("Save Config") },
    /* Translators: This string is meant to be a short alternative for "Save Report Configuration As..."
     * to be used as toolbar button label. */
    { "ReportSaveAsAction", N_("Save Config As...") },
    { "FilePrintPDFAction", N_("Make Pdf") },
    { NULL, NULL },
};

static const gchar *initially_insensitive_actions[] =
{
    NULL
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
    int i;

    our_class = GNC_PLUGIN_PAGE_REPORT_CLASS (
                    g_type_class_peek (GNC_TYPE_PLUGIN_PAGE_REPORT));
    parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (our_class));
    obj = parent_class->constructor(this_type, n_properties, properties);

    for (i = 0; i < n_properties; i++)
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
gnc_plugin_page_report_constr_init(GncPluginPageReport *plugin_page, gint reportId)
{
    GncPluginPageReportPrivate *priv;
    GtkActionGroup *action_group;
    GncPluginPage *parent;
    gboolean use_new;
    gchar *name;
    gchar *saved_reports_path = gnc_build_userdata_path (SAVED_REPORTS_FILE);
    gchar *report_save_str = g_strdup_printf (
        _("Update the current report's saved configuration. "
        "The report will be saved in the file %s. "), saved_reports_path);
    gchar *report_saveas_str = g_strdup_printf (
        _("Add the current report's configuration to the `Saved Report Configurations' menu. "
        "The report will be saved in the file %s. "), saved_reports_path);

    GtkActionEntry report_actions[] =
    {
        {
            "FilePrintAction", "document-print", N_("_Print Report..."), "<primary>p",
            N_("Print the current report"),
            G_CALLBACK(gnc_plugin_page_report_print_cb)
        },
        {
            "FilePrintPDFAction", GNC_ICON_PDF_EXPORT, N_("Export as P_DF..."), NULL,
            N_("Export the current report as a PDF document"),
            G_CALLBACK(gnc_plugin_page_report_exportpdf_cb)
        },

        {
            "EditCutAction", "edit-cut", N_("Cu_t"), "<primary>X",
            N_("Cut the current selection and copy it to clipboard"),
            NULL
        },
        {
            "EditCopyAction", "edit-copy", N_("_Copy"), "<primary>C",
            N_("Copy the current selection to clipboard"),
            G_CALLBACK(gnc_plugin_page_report_copy_cb)
        },
        {
            "EditPasteAction", "edit-paste", N_("_Paste"), "<primary>V",
            N_("Paste the clipboard content at the cursor position"),
            NULL
        },
        {
            "ViewRefreshAction", "view-refresh", N_("_Refresh"), "<primary>r",
            N_("Refresh this window"),
            G_CALLBACK (gnc_plugin_page_report_reload_cb)
        },
        {
            "ReportSaveAction", "document-save", N_("Save _Report Configuration"), "<primary><alt>s",
            report_save_str, G_CALLBACK(gnc_plugin_page_report_save_cb)
        },
        {
            "ReportSaveAsAction", "document-save-as", N_("Save Report Configuration As..."), "<primary><alt><shift>s",
            report_saveas_str, G_CALLBACK(gnc_plugin_page_report_save_as_cb)
        },
        {
            "ReportExportAction", "go-next", N_("Export _Report"), NULL,
            N_("Export HTML-formatted report to file"),
            G_CALLBACK(gnc_plugin_page_report_export_cb)
        },
        {
            "ReportOptionsAction", "document-properties", N_("_Report Options"), NULL,
            N_("Edit report options"),
            G_CALLBACK(gnc_plugin_page_report_options_cb)
        },

        {
            "ReportBackAction", "go-previous", N_("Back"), NULL,
            N_("Move back one step in the history"),
            G_CALLBACK(gnc_plugin_page_report_back_cb)
        },
        {
            "ReportForwAction", "go-next", N_("Forward"), NULL,
            N_("Move forward one step in the history"),
            G_CALLBACK(gnc_plugin_page_report_forw_cb)
        },
        {
            "ReportReloadAction", "view-refresh", N_("Reload"), NULL,
            N_("Reload the current page"),
            G_CALLBACK(gnc_plugin_page_report_reload_cb)
        },
        {
            "ReportStopAction", "process-stop", N_("Stop"), NULL,
            N_("Cancel outstanding HTML requests"),
            G_CALLBACK(gnc_plugin_page_report_stop_cb)
        },
    };
    guint num_report_actions = G_N_ELEMENTS( report_actions );

    DEBUG( "property reportId=%d", reportId );
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(plugin_page);
    priv->reportId = reportId;

    gnc_plugin_page_report_setup( GNC_PLUGIN_PAGE(plugin_page) );

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    use_new = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REPORT, GNC_PREF_USE_NEW);
    name = gnc_report_name( priv->initial_report );
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      name,
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-report-ui.xml",
                 "use-new-window", use_new,
                 NULL);
    g_free(name);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book(parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    action_group =
        gnc_plugin_page_create_action_group(parent,
                                            "GncPluginPageReportActions");
    gtk_action_group_add_actions( action_group,
                                  report_actions,
                                  num_report_actions,
                                  plugin_page );
    gnc_plugin_update_actions(action_group,
                              initially_insensitive_actions,
                              "sensitive", FALSE);
    gnc_plugin_init_short_names (action_group, toolbar_labels);

    g_free (saved_reports_path);
    g_free (report_save_str);
    g_free (report_saveas_str);
}

GncPluginPage*
gnc_plugin_page_report_new( int reportId )
{
    GncPluginPageReport *plugin_page;

    DEBUG( "report id = %d", reportId );
    plugin_page = g_object_new( GNC_TYPE_PLUGIN_PAGE_REPORT,
                                "report-id", reportId, NULL );
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
    GtkWidget *w = SWIG_MustGetPtr(editor,
                                   SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
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
gnc_plugin_page_report_set_fwd_button(GncPluginPageReport *report, int enabled)
{
    GtkAction *act;

    act = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(report),
                                     "ReportForwAction" );
    gtk_action_set_sensitive(act, enabled);
}

static void
gnc_plugin_page_report_set_back_button(GncPluginPageReport *report, int enabled)
{
    GtkAction *act;

    act = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(report),
                                     "ReportBackAction" );
    gtk_action_set_sensitive(act, enabled);
}

// ------------------------------------------------------------
// GTK ACTION CALLBACKS

static void
gnc_plugin_page_report_forw_cb( GtkAction *action, GncPluginPageReport *report )
{
    GncPluginPageReportPrivate *priv;
    gnc_html_history_node * node = NULL;

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
gnc_plugin_page_report_back_cb( GtkAction *action, GncPluginPageReport *report )
{
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

static void
gnc_plugin_page_report_reload_cb( GtkAction *action, GncPluginPageReport *report )
{
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

    // this resets the window for the progressbar to NULL
    gnc_window_set_progressbar_window( NULL );
    priv->reloading = FALSE;
}

static void
gnc_plugin_page_report_stop_cb( GtkAction *action, GncPluginPageReport *report )
{
    GncPluginPageReportPrivate *priv;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gnc_html_cancel(priv->html);
}

/* Returns SCM_BOOL_F if cancel. Returns SCM_BOOL_T if html.
 * Otherwise returns pair from export_types. */
static SCM
gnc_get_export_type_choice (SCM export_types)
{
    GList * choices = NULL;
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
                 (NULL, _("Choose export format"),
                  _("Choose the export format for this report:"),
                  NULL, 0, choices);
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
gnc_get_export_filename (SCM choice)
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

    filepath = gnc_file_dialog (gnc_ui_get_main_window (NULL),
                                title, NULL, default_dir, GNC_FILE_DIALOG_EXPORT);

    if (filepath != NULL) // test for cancel pressed
    {
        /* Try to test for extension on file name, add if missing */
        if (g_strrstr(filepath, ".") == NULL)
            filepath = g_strconcat(filepath, ".", g_ascii_strdown(type, strlen(type)), NULL);
    }
    g_free (type);
    g_free (title);
    g_free (default_dir);

    if (!filepath)
        return NULL;

    default_dir = g_path_get_dirname(filepath);
    gnc_set_default_directory (GNC_PREFS_GROUP_REPORT, default_dir);
    g_free(default_dir);

    rc = g_stat (filepath, &statbuf);

    /* Check for an error that isn't a non-existent file. */
    if (rc != 0 && errno != ENOENT)
    {
        /* %s is the strerror(3) string of the error that occurred. */
        const char *format = _("You cannot save to that filename.\n\n%s");

        gnc_error_dialog (NULL, format, strerror(errno));
        g_free(filepath);
        return NULL;
    }

    /* Check for a file that isn't a regular file. */
    if (rc == 0 && !S_ISREG (statbuf.st_mode))
    {
        const char *message = _("You cannot save to that file.");

        gnc_error_dialog (NULL, "%s", message);
        g_free(filepath);
        return NULL;
    }

    if (rc == 0)
    {
        const char *format = _("The file %s already exists. "
                               "Are you sure you want to overwrite it?");

        if (!gnc_verify_dialog (NULL, FALSE, format, filepath))
        {
            g_free(filepath);
            return NULL;
        }
    }

    return filepath;
}

static void
gnc_plugin_page_report_save_as_cb( GtkAction *action, GncPluginPageReport *report )
{
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
gnc_plugin_page_report_save_cb( GtkAction *action, GncPluginPageReport *report )
{
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
        gnc_plugin_page_report_save_as_cb (action, report);
    }
}

static void
gnc_plugin_page_report_export_cb( GtkAction *action, GncPluginPageReport *report )
{
    GncPluginPageReportPrivate *priv;
    char * filepath;
    SCM export_types;
    SCM export_thunk;
    gboolean result;
    SCM choice;

    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    export_types = scm_call_1 (scm_c_eval_string ("gnc:report-export-types"),
                               priv->cur_report);

    export_thunk = scm_call_1 (scm_c_eval_string ("gnc:report-export-thunk"),
                               priv->cur_report);

    if (scm_is_list (export_types) && scm_is_procedure (export_thunk))
        choice = gnc_get_export_type_choice (export_types);
    else
        choice = SCM_BOOL_T;

    if (choice == SCM_BOOL_F)
        return;

    filepath = gnc_get_export_filename (choice);
    if (!filepath)
        return;

    if (scm_is_pair (choice))
    {
        SCM file_scm;
        SCM res;

        choice = SCM_CDR (choice);
        file_scm = scm_from_locale_string (filepath);

        res = scm_call_3 (export_thunk, priv->cur_report, choice, file_scm);

        result = (res != SCM_BOOL_F);
    }
    else
        result = gnc_html_export_to_file (priv->html, filepath);

    if (!result)
    {
        const char *fmt = _("Could not open the file %s. "
                            "The error is: %s");
        gnc_error_dialog( NULL, fmt, filepath ? filepath : "(null)",
                          strerror (errno) ? strerror (errno) : "" );
    }

    g_free(filepath);
    return;
}

static void
gnc_plugin_page_report_options_cb( GtkAction *action, GncPluginPageReport *report )
{
    GncPluginPageReportPrivate *priv;
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (report)));
    priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    if (priv->cur_report == SCM_BOOL_F)
        return;

    if (gnc_report_edit_options (priv->cur_report, parent))
        gnc_plugin_page_report_add_edited_report(priv, priv->cur_report);
}

static GncInvoice *lookup_invoice(GncPluginPageReportPrivate *priv)
{
    g_assert(priv);
    return gnc_option_db_lookup_invoice_option(priv->cur_odb, "General",
            "Invoice Number", NULL);
}

#define GNC_PREFS_GROUP_REPORT_PDFEXPORT GNC_PREFS_GROUP_GENERAL_REPORT ".pdf-export"
#define GNC_PREF_FILENAME_DATE_FMT "filename-date-format"
#define GNC_PREF_FILENAME_FMT "filename-format"

static gchar *report_create_jobname(GncPluginPageReportPrivate *priv)
{
    gchar *job_name = NULL;
    gchar *report_name = NULL;
    const gchar *report_number = "";
    gchar *job_date;
    const gchar *default_jobname = N_("GnuCash-Report");

    g_assert(priv);

    {
        // Look up the date format that was chosen in the preferences database
        QofDateFormat date_format_here;
        QofDateFormat date_format_old = qof_date_format_get();
        char *format_code = gnc_prefs_get_string(GNC_PREFS_GROUP_REPORT_PDFEXPORT,
                            GNC_PREF_FILENAME_DATE_FMT);
        if (*format_code == '\0')
        {
            g_free(format_code);
            format_code = g_strdup("locale");
        }

        if (gnc_date_string_to_dateformat(format_code, &date_format_here))
        {
            PERR("Incorrect date format code");
            if (format_code != NULL)
                free(format_code);
        }

        // To apply this chosen date format, temporarily switch the
        // process-wide default to our chosen date format. Note: It is a
        // totally brain-dead implementation of qof_print_date() to not offer a
        // variation where the QofDateFormat can be passed as an argument.
        // Hrmpf.
        qof_date_format_set(date_format_here);

        job_date = qof_print_date( time( NULL ) );

        // Restore to the original general  date format
        qof_date_format_set(date_format_old);
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
        report_name = gnc_option_db_lookup_string_option(priv->cur_odb, "General",
                      "Report name", NULL);
        if (!report_name)
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
        // Look up the sprintf format of the output name from the preferences database
        char* format = gnc_prefs_get_string(GNC_PREFS_GROUP_REPORT_PDFEXPORT, GNC_PREF_FILENAME_FMT);

        job_name = g_strdup_printf(format, report_name, report_number, job_date);

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
        already_found = (value != NULL);
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
gnc_plugin_page_report_print_cb( GtkAction *action, GncPluginPageReport *report )
{
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
gnc_plugin_page_report_exportpdf_cb( GtkAction *action, GncPluginPageReport *report )
{
    GncPluginPageReportPrivate *priv = GNC_PLUGIN_PAGE_REPORT_GET_PRIVATE(report);
    gchar *job_name = report_create_jobname(priv);
    GncInvoice *invoice;
    GncOwner *owner = NULL;

    // Do we have an invoice report?
    invoice = lookup_invoice(priv);
    if (invoice)
    {
        // Does this invoice also have an owner?
        owner = (GncOwner*) gncInvoiceGetOwner(invoice);
        if (owner)
        {
	    QofInstance *inst = qofOwnerGetOwner (owner);
	    gchar *dirname = NULL;
	    qof_instance_get (inst, "export-pdf-dir", &dirname, NULL);
            // Yes. In the kvp, look up the key for the Export-PDF output
            // directory. If it exists, prepend this to the job name so that
            // we can export to PDF.
	    if (dirname && g_file_test(dirname,
				       G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
	    {
		gchar *tmp = g_build_filename(dirname, job_name, NULL);
		g_free(job_name);
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
        if (print_settings &&
	    gtk_print_settings_has_key(print_settings,
				       GNC_GTK_PRINT_SETTINGS_EXPORT_DIR))
        {
            const char* dirname = gtk_print_settings_get(print_settings,
                                  GNC_GTK_PRINT_SETTINGS_EXPORT_DIR);
            // Only store the directory if it exists.
            if (g_file_test(dirname, G_FILE_TEST_EXISTS | G_FILE_TEST_IS_DIR))
            {
                QofInstance *inst = qofOwnerGetOwner(owner);
                gncOwnerBeginEdit(owner);
		qof_instance_set (inst, "export-pdf-dir", dirname);
		gncOwnerCommitEdit(owner);
            }
        }
    }

    g_free (job_name);
}

static void
gnc_plugin_page_report_copy_cb(GtkAction *action, GncPluginPageReport *report)
{
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

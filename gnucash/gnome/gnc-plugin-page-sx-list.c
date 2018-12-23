/********************************************************************\
 * gnc-plugin-page-sx-list.c : scheduled transaction plugin         *
 *                                                                  *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2011 Robert Fewell                                 *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
 *                                                                  *
 * As a special exception, permission is granted to link the binary *
 * module resultant from this code with the OpenSSL project's       *
 * "OpenSSL" library (or modified versions of it that use the same  *
 * license as the "OpenSSL" library), and distribute the linked     *
 * executable.  You must obey the GNU General Public License in all *
 * respects for all of the code used other than "OpenSSL". If you   *
 * modify this file, you may extend this exception to your version  *
 * of the file, but you are not obligated to do so. If you do not   *
 * wish to do so, delete this exception statement from your version *
 * of this file.                                                    *
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageSxList A SX List Plugin Page
    @{ */
/** @brief Functions providing the SX List as a plugin page.
    @author Josh Sled <jsled@asynchronous.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>

#include <gnc-gobject-utils.h>
#include "SX-book.h"
#include "Split.h"
#include "Transaction.h"
#include "dialog-sx-editor.h"
/*################## Added for Reg2 #################*/
#include "dialog-sx-editor2.h"
/*################## Added for Reg2 #################*/
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-dense-cal.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-glib-utils.h"
#include "gnc-icons.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-sx-list.h"
#include "gnc-session.h"
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-sx-instance-model.h"
#include "gnc-sx-list-tree-model-adapter.h"
#include "gnc-tree-view-sx-list.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.plugin-page.sx-list"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI_SX;

#define PLUGIN_PAGE_SX_LIST_CM_CLASS "plugin-page-sx-list"
#define STATE_SECTION "SX Transaction List"

typedef struct GncPluginPageSxListPrivate
{
    gboolean disposed;

    GtkWidget* widget;
    gint gnc_component_id;

    GncSxInstanceDenseCalAdapter *dense_cal_model;
    GncDenseCal* gdcal;

    GncSxInstanceModel* instances;
    GtkTreeView* tree_view;

} GncPluginPageSxListPrivate;

#define GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_SX_LIST, GncPluginPageSxListPrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass);
static void gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page);
static void gnc_plugin_page_sx_list_dispose(GObject *object);
static void gnc_plugin_page_sx_list_finalize(GObject *object);

static GtkWidget *gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_sx_list_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

static void gppsl_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data);

static void gnc_plugin_page_sx_list_cmd_new(GtkAction *action, GncPluginPageSxList *page);
static void gnc_plugin_page_sx_list_cmd_edit(GtkAction *action, GncPluginPageSxList *page);
#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
static void gnc_plugin_page_sx_list_cmd_new2(GtkAction *action, GncPluginPageSxList *page);
static void gnc_plugin_page_sx_list_cmd_edit2(GtkAction *action, GncPluginPageSxList *page);
/*################## Added for Reg2 #################*/
#endif
static void gnc_plugin_page_sx_list_cmd_delete(GtkAction *action, GncPluginPageSxList *page);

/* Command callbacks */
static GtkActionEntry gnc_plugin_page_sx_list_actions [] =
{
    { "SxListAction", NULL, N_("_Scheduled"), NULL, NULL, NULL },
    {
        "SxListNewAction", GNC_ICON_NEW_ACCOUNT, N_("_New"), NULL,
        N_("Create a new scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_new)
    },
#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
    {
        "SxListNewAction2", GNC_ICON_NEW_ACCOUNT, N_("_New 2"), NULL,
        N_("Create a new scheduled transaction 2"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_new2)
    },
/*################## Added for Reg2 #################*/
#endif
    {
        "SxListEditAction", GNC_ICON_EDIT_ACCOUNT, N_("_Edit"), NULL,
        N_("Edit the selected scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_edit)
    },
#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
    {
        "SxListEditAction2", GNC_ICON_EDIT_ACCOUNT, N_("_Edit 2"), NULL,
        N_("Edit the selected scheduled transaction 2"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_edit2)
    },
/*################## Added for Reg2 #################*/
#endif
    {
        "SxListDeleteAction", GNC_ICON_DELETE_ACCOUNT, N_("_Delete"), NULL,
        N_("Delete the selected scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_delete)
    },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_sx_list_n_actions = G_N_ELEMENTS (gnc_plugin_page_sx_list_actions);

GncPluginPage *
gnc_plugin_page_sx_list_new (void)
{
    GncPluginPageSxList *plugin_page;
    const GList *object = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_SX_LIST_NAME);
    if (object && GNC_IS_PLUGIN_PAGE_SX_LIST (object->data))
        plugin_page = GNC_PLUGIN_PAGE_SX_LIST (object->data);
    else
    {
        plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_SX_LIST, NULL);
    }
    return GNC_PLUGIN_PAGE(plugin_page);
}


static gboolean
gnc_plugin_page_sx_list_focus (GtkTreeView *tree_view)
{
    if (GTK_IS_TREE_VIEW(tree_view))
    {
        if (!gtk_widget_is_focus (GTK_WIDGET(tree_view)))
            gtk_widget_grab_focus (GTK_WIDGET(tree_view));
    }
    return FALSE;
}


/**
 * Whenever the current page is changed, if a schedule editor page is
 * the current page, set focus on the treeview.
 */
static void
gnc_plugin_page_sx_list_main_window_page_changed (GncMainWindow *window,
        GncPluginPage *plugin_page, gpointer user_data)
{
    // We continue only if the plugin_page is a valid
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    if (gnc_main_window_get_current_page (window) == plugin_page)
    {
        GncPluginPageSxList *page;
        GncPluginPageSxListPrivate *priv;

        if (!GNC_IS_PLUGIN_PAGE_SX_LIST(plugin_page))
            return;

        page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
        priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

        // The page changed signal is emitted multiple times so we need
        // to use an idle_add to change the focus to the tree view
        g_idle_remove_by_data (GTK_TREE_VIEW (priv->tree_view));
        g_idle_add ((GSourceFunc)gnc_plugin_page_sx_list_focus,
                      GTK_TREE_VIEW (priv->tree_view));
    }
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageSxList, gnc_plugin_page_sx_list, GNC_TYPE_PLUGIN_PAGE)

static void
gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent(klass);

    object_class->dispose = gnc_plugin_page_sx_list_dispose;
    object_class->finalize = gnc_plugin_page_sx_list_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_SX_LIST_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_sx_list_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_sx_list_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_sx_list_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_sx_list_recreate_page;
}


static void
gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPage *parent;

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
#ifdef REGISTER2_ENABLED
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Scheduled Transactions"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-sx-list2-ui.xml",
                 NULL);
#else
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Scheduled Transactions"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-sx-list-ui.xml",
                 NULL);
#endif

    gnc_plugin_page_add_book(parent, gnc_get_current_book());
    action_group =
        gnc_plugin_page_create_action_group(parent,
                                            "GncPluginPageSxListActions");
    gtk_action_group_add_actions(action_group,
                                 gnc_plugin_page_sx_list_actions,
                                 gnc_plugin_page_sx_list_n_actions,
                                 plugin_page);
    /* gnc_plugin_init_short_names (action_group, toolbar_labels); */
}


static void
gnc_plugin_page_sx_list_dispose(GObject *object)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST (object);
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST (page));
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    g_return_if_fail(priv != NULL);

    g_return_if_fail(!priv->disposed);
    priv->disposed = TRUE;

    g_object_unref(G_OBJECT(priv->dense_cal_model));
    priv->dense_cal_model = NULL;
    g_object_unref(GTK_WIDGET(priv->gdcal));
    priv->gdcal = NULL;
    g_object_unref(G_OBJECT(priv->instances));
    priv->instances = NULL;

    G_OBJECT_CLASS (parent_class)->dispose(object);
}


static void
gnc_plugin_page_sx_list_finalize (GObject *object)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST (object);
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST (page));
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    g_return_if_fail(priv != NULL);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}


/* Virtual Functions */
static void
gnc_plugin_page_sx_list_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageSxList *page = user_data;
    GncPluginPageSxListPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST(page));

    /* We're only looking for forced updates here. */
    if (changes)
        return;

    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    gtk_widget_queue_draw(priv->widget);
}


static void
gnc_plugin_page_sx_list_close_cb (gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page(plugin_page);
}


static void
gppsl_selection_changed_cb(GtkTreeSelection *selection, gpointer user_data)
{
    GncPluginPage *page;
    GtkAction *edit_action, *delete_action;
    gboolean selection_state = TRUE;

    page = GNC_PLUGIN_PAGE(user_data);
    edit_action = gnc_plugin_page_get_action(page, "SxListEditAction");
    delete_action = gnc_plugin_page_get_action(page, "SxListDeleteAction");
    selection_state
    = gtk_tree_selection_count_selected_rows(selection) == 0
      ? FALSE
      : TRUE;
    gtk_action_set_sensitive(edit_action, selection_state);
    gtk_action_set_sensitive(delete_action, selection_state);
}


static GtkWidget *
gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;
    GncMainWindow  *window;
    GtkWidget *widget;
    GtkWidget *vbox;
    GtkWidget *label;
    GtkWidget *swin;
    char *markup;
    char *text;

    page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    if (priv->widget != NULL)
        return priv->widget;

    /* Create Vpaned widget for top level */
    widget = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
    priv->widget = widget;
    gtk_widget_show (priv->widget);

    // Set the style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(priv->widget), "GncSxPage");

    /* Add vbox and label */
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_paned_pack1( GTK_PANED(widget), vbox, TRUE, FALSE);

    label = gtk_label_new(NULL);
    text = g_strdup_printf(_("Transactions"));
    markup = g_markup_printf_escaped ("<b> %s</b>", text);
    gtk_label_set_markup (GTK_LABEL (label), markup);
    g_free (markup);
    g_free (text);
    gnc_label_set_alignment (label, 0.0, 0);
    gtk_widget_show (label);
    gtk_box_pack_start ( GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (vbox);

    /* Create scrolled window for top area */
    swin = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_box_pack_start ( GTK_BOX(vbox), swin, TRUE, TRUE, 5);
    gtk_widget_show (swin);

    {
        // gint half_way;
        // half_way = plugin_page->notebook_page->allocation.height * 0.5;
        // fixme; get a real value:
        gtk_paned_set_position(GTK_PANED(priv->widget), 160);
    }

    {
        GDate end;
        g_date_clear(&end, 1);
        gnc_gdate_set_today (&end);
        g_date_add_years(&end, 1);
        priv->instances = GNC_SX_INSTANCE_MODEL(gnc_sx_get_instances(&end, TRUE));
    }

    {
        GtkAction *edit_action, *delete_action;
        edit_action = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(page), "SxListEditAction");
        delete_action = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(page), "SxListDeleteAction");
        gtk_action_set_sensitive(edit_action, FALSE);
        gtk_action_set_sensitive(delete_action, FALSE);
    }

    {
        GtkTreeSelection *selection;

        priv->tree_view = GTK_TREE_VIEW(gnc_tree_view_sx_list_new(priv->instances));
        g_object_set(G_OBJECT(priv->tree_view),
                     "state-section", STATE_SECTION,
                     "show-column-menu", TRUE,
                     NULL);
        gtk_container_add(GTK_CONTAINER( swin ), GTK_WIDGET(priv->tree_view));

        selection = gtk_tree_view_get_selection(priv->tree_view);
        gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
        g_signal_connect(G_OBJECT(selection), "changed", (GCallback)gppsl_selection_changed_cb, (gpointer)page);
        g_signal_connect(G_OBJECT(priv->tree_view), "row-activated", (GCallback)gppsl_row_activated_cb, (gpointer)page);
    }

    /* Add vbox and label */
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);
    gtk_paned_pack2( GTK_PANED(widget), vbox, TRUE, FALSE);

    label = gtk_label_new(NULL);
    text = g_strdup_printf(_("Upcoming Transactions"));
    markup = g_markup_printf_escaped ("<b> %s</b>", text);
    gtk_label_set_markup (GTK_LABEL (label), markup);
    g_free (markup);
    g_free (text);
    gnc_label_set_alignment (label, 0.0, 0);
    gtk_widget_show (label);

    gtk_box_pack_start ( GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (vbox);

    /* Create scrolled window for bottom area */
    swin = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_box_pack_start ( GTK_BOX(vbox), swin, TRUE, TRUE, 5);
    gtk_widget_show (swin);

    {
        priv->dense_cal_model = gnc_sx_instance_dense_cal_adapter_new(GNC_SX_INSTANCE_MODEL(priv->instances));
        priv->gdcal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model(GNC_DENSE_CAL_MODEL(priv->dense_cal_model)));
        g_object_ref_sink(priv->gdcal);

        gnc_dense_cal_set_months_per_col(priv->gdcal, 4);
        gnc_dense_cal_set_num_months(priv->gdcal, 12);

        gtk_container_add (GTK_CONTAINER(swin), GTK_WIDGET(priv->gdcal));
    }

    priv->gnc_component_id = gnc_register_gui_component("plugin-page-sx-list",
                             gnc_plugin_page_sx_list_refresh_cb,
                             gnc_plugin_page_sx_list_close_cb,
                             page);
    gnc_gui_component_set_session (priv->gnc_component_id,
                                   gnc_get_current_session());

    window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
    g_signal_connect(window, "page_changed",
                     G_CALLBACK(gnc_plugin_page_sx_list_main_window_page_changed),
                     plugin_page);

    return priv->widget;
}


static void
gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST (plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    // Remove the page focus idle function if present
    g_idle_remove_by_data (GTK_TREE_VIEW (priv->tree_view));

    if (priv->widget)
    {
        g_object_unref(G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    if (priv->gnc_component_id)
    {
        gnc_unregister_gui_component(priv->gnc_component_id);
        priv->gnc_component_id = 0;
    }
}


/** Save enough information about this page that it can be recreated next time
 * the user starts gnucash.
 * @param plugin_page The page to save.
 * @param key_file A pointer to the GKeyFile data structure where the
 * page information should be written.
 * @param group_name The group name to use when saving data.
 **/
static void
gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page,
                                   GKeyFile *key_file,
                                   const gchar *group_name)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST(plugin_page));
    g_return_if_fail(key_file != NULL);
    g_return_if_fail(group_name != NULL);

    page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    g_key_file_set_integer(key_file, group_name, "dense_cal_num_months",
                           gnc_dense_cal_get_num_months(priv->gdcal));

    g_key_file_set_integer(key_file, group_name, "paned_position",
                           gtk_paned_get_position(GTK_PANED(priv->widget)));
}


/**
 * Create a new sx list page based on the information saved during a previous
 * instantiation of gnucash.
 *  @param window The window where this page should be installed.
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *  @param group_name The group name to use when restoring data.
 **/
static GncPluginPage *
gnc_plugin_page_sx_list_recreate_page (GtkWidget *window,
                                       GKeyFile *key_file,
                                       const gchar *group_name)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);

    /* Create the new page. */
    page = GNC_PLUGIN_PAGE_SX_LIST(gnc_plugin_page_sx_list_new());
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    /* Install it now so we can them manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), GNC_PLUGIN_PAGE(page));

    {
        GError *err = NULL;
        gint num_months = g_key_file_get_integer(key_file, group_name, "dense_cal_num_months", &err);
        if (err == NULL)
            gnc_dense_cal_set_num_months(priv->gdcal, num_months);
        else
            g_error_free(err);
    }

    {
        GError *err = NULL;
        gint paned_position = g_key_file_get_integer(key_file, group_name,
                              "paned_position", &err);
        if (err == NULL)
            gtk_paned_set_position(GTK_PANED(priv->widget), paned_position);
        else
            g_error_free(err);
    }

    return GNC_PLUGIN_PAGE(page);
}


static void
gnc_plugin_page_sx_list_cmd_new(GtkAction *action, GncPluginPageSxList *page)
{
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    SchedXaction *new_sx;
    gboolean new_sx_flag = TRUE;

    new_sx = xaccSchedXactionMalloc(gnc_get_current_book());
    {
        GDate now;
        Recurrence *r = g_new0(Recurrence, 1);
        GList *schedule;

        g_date_clear(&now, 1);
        gnc_gdate_set_today (&now);
        recurrenceSet(r, 1, PERIOD_MONTH, &now, WEEKEND_ADJ_NONE);
        schedule = gnc_sx_get_schedule(new_sx);
        schedule = g_list_append(schedule, r);
        gnc_sx_set_schedule(new_sx, schedule);
    }
    gnc_ui_scheduled_xaction_editor_dialog_create(window, new_sx, new_sx_flag);
}

#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
static void
gnc_plugin_page_sx_list_cmd_new2 (GtkAction *action, GncPluginPageSxList *page)
{
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    SchedXaction *new_sx;
    gboolean new_sx_flag = TRUE;

    new_sx = xaccSchedXactionMalloc (gnc_get_current_book());
    {
        GDate now;
        Recurrence *r = g_new0 (Recurrence, 1);
        GList *schedule;

        g_date_clear (&now, 1);
        gnc_gdate_set_today (&now);
        recurrenceSet(r, 1, PERIOD_MONTH, &now, WEEKEND_ADJ_NONE);
        schedule = gnc_sx_get_schedule (new_sx);
        schedule = g_list_append (schedule, r);
        gnc_sx_set_schedule (new_sx, schedule);
    }
    gnc_ui_scheduled_xaction_editor_dialog_create2 (window, new_sx, new_sx_flag);
}
/*################## Added for Reg2 #################*/
#endif

static void
_edit_sx(gpointer data, gpointer user_data)
{
    gnc_ui_scheduled_xaction_editor_dialog_create(GTK_WINDOW(user_data),
        (SchedXaction*)data, FALSE);
}

#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
static void
_edit_sx2 (gpointer data, gpointer user_data)
{
    gnc_ui_scheduled_xaction_editor_dialog_create2 (GTK_WINDOW(user_data),
        (SchedXaction*)data, FALSE);
}
/*################## Added for Reg2 #################*/
#endif

static SchedXaction*
_argument_reorder_fn(GtkTreePath* list_path_data, GncTreeViewSxList* user_tree_view)
{
    return gnc_tree_view_sx_list_get_sx_from_path(user_tree_view, list_path_data);
}


static void
gnc_plugin_page_sx_list_cmd_edit(GtkAction *action, GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    GtkTreeSelection *selection;
    GList *selected_paths, *to_edit;
    GtkTreeModel *model;

    selection = gtk_tree_view_get_selection(priv->tree_view);
    selected_paths = gtk_tree_selection_get_selected_rows(selection, &model);
    if (g_list_length(selected_paths) == 0)
    {
        g_warning("no selection edit.");
        return;
    }

    to_edit = gnc_g_list_map(selected_paths,
                             (GncGMapFunc)_argument_reorder_fn,
                             priv->tree_view);
    g_list_foreach(to_edit, (GFunc)_edit_sx, window);
    g_list_free(to_edit);
    g_list_foreach(selected_paths, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_paths);
}

#ifdef REGISTER2_ENABLED
/*################## Added for Reg2 #################*/
static void
gnc_plugin_page_sx_list_cmd_edit2 (GtkAction *action, GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    GtkTreeSelection *selection;
    GList *selected_paths, *to_edit;
    GtkTreeModel *model;

    selection = gtk_tree_view_get_selection (priv->tree_view);
    selected_paths = gtk_tree_selection_get_selected_rows (selection, &model);
    if (g_list_length (selected_paths) == 0)
    {
        g_warning ("no selection edit.");
        return;
    }

    to_edit = gnc_g_list_map (selected_paths,
                             (GncGMapFunc)_argument_reorder_fn,
                             priv->tree_view);
    g_list_foreach(to_edit, (GFunc)_edit_sx2, window);
    g_list_free (to_edit);
    g_list_foreach (selected_paths, (GFunc)gtk_tree_path_free, NULL);
    g_list_free (selected_paths);
}
/*################## Added for Reg2 #################*/
#endif

static void
gppsl_row_activated_cb(GtkTreeView *tree_view,
                       GtkTreePath *path,
                       GtkTreeViewColumn *column,
                       gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST(user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));

    SchedXaction *sx = gnc_tree_view_sx_list_get_sx_from_path(GNC_TREE_VIEW_SX_LIST(priv->tree_view), path);
    gnc_ui_scheduled_xaction_editor_dialog_create(window, sx, FALSE);
}


static void
_destroy_sx(gpointer data, gpointer user_data)
{
    SchedXactions *sxes;
    SchedXaction *sx = (SchedXaction*)data;
    QofBook *book;
    book = gnc_get_current_book();
    sxes = gnc_book_get_schedxactions(book);
    gnc_sxes_del_sx(sxes, sx);
    gnc_sx_begin_edit(sx);
    xaccSchedXactionDestroy(sx);
}


static void
gnc_plugin_page_sx_list_cmd_delete(GtkAction *action, GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    GtkTreeSelection *selection;
    GList *selected_paths, *to_delete = NULL;
    GtkTreeModel *model;
    GtkWindow *window;

    selection = gtk_tree_view_get_selection(priv->tree_view);
    selected_paths = gtk_tree_selection_get_selected_rows(selection, &model);
    if (g_list_length(selected_paths) == 0)
    {
        g_warning("no selection for delete.");
        return;
    }

    to_delete = gnc_g_list_map(selected_paths,
                               (GncGMapFunc)_argument_reorder_fn,
                               priv->tree_view);
    {
        GList *list;
        for (list = to_delete; list != NULL; list = list->next)
        {
            g_debug("to-delete [%s]\n", xaccSchedXactionGetName((SchedXaction*)list->data));
        }
    }

    window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    /* FIXME: Does this always refer to only one transaction? Or could
       multiple SXs be deleted as well? Ideally, the number of
       to-be-deleted SXs should be mentioned here; see
       dialog-sx-since-last-run.c:807 */
    if (gnc_verify_dialog (window, FALSE, "%s", _("Do you really want to delete this scheduled transaction?")))
    {
        g_list_foreach(to_delete, (GFunc)_destroy_sx, NULL);
    }

    g_list_free(to_delete);
    g_list_foreach(selected_paths, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_paths);
}

/** @} */
/** @} */

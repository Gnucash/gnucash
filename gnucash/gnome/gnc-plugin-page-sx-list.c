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
#include "gnc-plugin.h"
#include "gnc-plugin-page-sx-list.h"
#include "gnc-session.h"
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-sx-instance-model.h"
#include "gnc-sx-list-tree-model-adapter.h"
#include "gnc-tree-view-sx-list.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-window.h"

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
    GList *selected_list;

} GncPluginPageSxListPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageSxList, gnc_plugin_page_sx_list, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(o)  \
   ((GncPluginPageSxListPrivate*)gnc_plugin_page_sx_list_get_instance_private((GncPluginPageSxList*)o))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass);
static void gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page);
static void gnc_plugin_page_sx_list_dispose (GObject *object);
static void gnc_plugin_page_sx_list_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_sx_list_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

static void gppsl_row_activated_cb (GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data);

static void gnc_plugin_page_sx_list_cmd_new (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_edit (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_delete (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_refresh (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_edit_tax_options (GSimpleAction *simple, GVariant *paramter, gpointer user_data);

/* Command callbacks */
static GActionEntry gnc_plugin_page_sx_list_actions [] =
{
    { "SxListAction", NULL, NULL, NULL, NULL },
    { "SxListNewAction", gnc_plugin_page_sx_list_cmd_new, NULL, NULL, NULL },
    { "SxListEditAction", gnc_plugin_page_sx_list_cmd_edit, NULL, NULL, NULL },
    { "SxListDeleteAction", gnc_plugin_page_sx_list_cmd_delete, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_sx_list_cmd_refresh, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_sx_list_cmd_edit_tax_options, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_sx_list_n_actions = G_N_ELEMENTS(gnc_plugin_page_sx_list_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3",
    "EditPlaceholder3",
    "EditPlaceholder5",
    "ViewPlaceholder4",
    "SchedulePlaceholder0",
    NULL,
};


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


/**
 * Whenever the current page is changed, if a sx page is
 * the current page, set focus on the tree view.
 */
static gboolean
gnc_plugin_page_sx_list_focus_widget (GncPluginPage *sx_plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_SX_LIST(sx_plugin_page))
    {
        GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(sx_plugin_page);
        GtkTreeView *tree_view = priv->tree_view;

        /* Disable the Transaction Menu */
        GAction *action = gnc_main_window_find_action (GNC_MAIN_WINDOW(sx_plugin_page->window), "TransactionAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
        /* Enable the Schedule Menu */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(sx_plugin_page->window), "ScheduledAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);
        /* Disable the FilePrintAction */
        action = gnc_main_window_find_action (GNC_MAIN_WINDOW(sx_plugin_page->window), "FilePrintAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);

        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW(sx_plugin_page->window),
                                                 sx_plugin_page,
                                                 gnc_plugin_load_ui_items);

        if (GTK_IS_TREE_VIEW(tree_view))
        {
            if (!gtk_widget_is_focus (GTK_WIDGET(tree_view)))
                gtk_widget_grab_focus (GTK_WIDGET(tree_view));
        }
    }
    return FALSE;
}

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
    gnc_plugin_class->focus_page_function = gnc_plugin_page_sx_list_focus_widget;
}


static void
gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page)
{
    GSimpleActionGroup *simple_action_group;
    GncPluginPage *parent;

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Scheduled Transactions"),
                 "ui-description", "gnc-plugin-page-sx-list.ui",
                 NULL);

    gnc_plugin_page_add_book (parent, gnc_get_current_book());
    simple_action_group = gnc_plugin_page_create_action_group (parent, "GncPluginPageSxListActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_sx_list_actions,
                                     gnc_plugin_page_sx_list_n_actions,
                                     plugin_page);
}


static void
gnc_plugin_page_sx_list_dispose (GObject *object)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST(object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST(page));
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    g_return_if_fail (priv != NULL);

    g_return_if_fail (!priv->disposed);
    priv->disposed = TRUE;

    g_object_unref (G_OBJECT(priv->dense_cal_model));
    priv->dense_cal_model = NULL;
    g_object_unref (GTK_WIDGET(priv->gdcal));
    priv->gdcal = NULL;
    g_object_unref (G_OBJECT(priv->instances));
    priv->instances = NULL;

    G_OBJECT_CLASS(parent_class)->dispose (object);
}


static void
gnc_plugin_page_sx_list_finalize (GObject *object)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST(object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST(page));
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    g_return_if_fail (priv != NULL);

    G_OBJECT_CLASS(parent_class)->finalize (object);
}


/* Virtual Functions */
static void
gnc_plugin_page_sx_list_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageSxList *page = user_data;
    GncPluginPageSxListPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST(page));

    /* We're only looking for forced updates here. */
    if (changes)
        return;

    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    gtk_widget_queue_draw (priv->widget);
}


static void
gnc_plugin_page_sx_list_close_cb (gpointer user_data)
{
    GncPluginPage *plugin_page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page (plugin_page);
}


static void
gppsl_selection_changed_cb (GtkTreeSelection *selection, gpointer user_data)
{
    GncPluginPage *page;
    GAction *edit_action, *delete_action;
    gboolean selection_state = TRUE;

    page = GNC_PLUGIN_PAGE(user_data);
    edit_action = gnc_plugin_page_get_action (page, "SxListEditAction");
    delete_action = gnc_plugin_page_get_action (page, "SxListDeleteAction");
    selection_state = gtk_tree_selection_count_selected_rows (selection) == 0
                      ? FALSE
                      : TRUE;
    g_simple_action_set_enabled (G_SIMPLE_ACTION(edit_action), selection_state);
    g_simple_action_set_enabled (G_SIMPLE_ACTION(delete_action), selection_state);
}


static void
gppsl_update_selected_list (GncPluginPageSxList *page, gboolean reset, SchedXaction *sx)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    if (reset && priv->selected_list)
    {
        g_list_free (priv->selected_list);
        priv->selected_list = NULL;
    }
    if (sx)
        priv->selected_list = g_list_prepend (priv->selected_list, sx);
}


static void
gppsl_model_populated_cb (GtkTreeModel *tree_model, GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(priv->tree_view));
    gboolean found = FALSE;

    if (priv->selected_list)
    {
        // walk the list to see if we can reselect the sx
        for (GList *list = priv->selected_list; list != NULL; list = list->next)
        {
            SchedXaction *sx = list->data;
            GtkTreePath *path = gtk_tree_path_new_first ();

            // loop through the model trying to find selected sx's
            while (gnc_tree_view_path_is_valid (GNC_TREE_VIEW(priv->tree_view), path))
            {
                SchedXaction *sx_tmp = gnc_tree_view_sx_list_get_sx_from_path (
                                           GNC_TREE_VIEW_SX_LIST(priv->tree_view), path);
                if (sx_tmp == sx)
                {
                    found = TRUE;
                    break;
                }
                gtk_tree_path_next (path);
            }
            if (found)
                gtk_tree_selection_select_path (selection, path);

            gtk_tree_path_free (path);
        }
    }
    // this could be on load or if sx is deleted
    if (!found)
    {
        GtkTreePath *path = gtk_tree_path_new_first ();
        gtk_tree_selection_select_path (selection, path);
        gtk_tree_path_free (path);
    }
}


static void
treeview_popup (GtkTreeView *treeview, GdkEvent *event, GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GtkWidget *menu, *menuitem;
    gchar *full_action_name;
    const gchar *group_name = gnc_plugin_page_get_simple_action_group_name (GNC_PLUGIN_PAGE(page));

    menu = gtk_menu_new();

    menuitem = gtk_menu_item_new_with_mnemonic (_("_New"));
    full_action_name = g_strconcat (group_name, ".SxListNewAction", NULL);
    gtk_actionable_set_action_name (GTK_ACTIONABLE(menuitem), full_action_name);
    g_free (full_action_name);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    menuitem = gtk_menu_item_new_with_mnemonic (_("_Edit"));
    full_action_name = g_strconcat (group_name, ".SxListEditAction", NULL);
    gtk_actionable_set_action_name (GTK_ACTIONABLE(menuitem), full_action_name);
    g_free (full_action_name);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    menuitem = gtk_menu_item_new_with_mnemonic (_("_Delete"));
    full_action_name = g_strconcat (group_name, ".SxListDeleteAction", NULL);
    gtk_actionable_set_action_name (GTK_ACTIONABLE(menuitem), full_action_name);
    g_free (full_action_name);
    gtk_menu_shell_append (GTK_MENU_SHELL(menu), menuitem);

    gtk_menu_attach_to_widget (GTK_MENU (menu), GTK_WIDGET (priv->tree_view), NULL);
    gtk_widget_show_all (menu);
    gtk_menu_popup_at_pointer (GTK_MENU (menu), event);
}

static gboolean
treeview_button_press (GtkTreeView *treeview, GdkEvent *event,
                       GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GtkTreeView *tree_view = GTK_TREE_VIEW (priv->tree_view);

    if (event->type == GDK_BUTTON_PRESS)
    {
        GdkEventButton *event_button = (GdkEventButton*)event;
        if (event_button->button == GDK_BUTTON_SECONDARY)
        {
            treeview_popup (tree_view, event, page);
            return TRUE;
        }
    }
    return FALSE;
}

static gboolean
treeview_popup_menu (GtkTreeView *treeview, GncPluginPageSxList *page)
{
    treeview_popup (treeview, NULL, page);
    return TRUE;
}

static GtkWidget *
gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;
    GtkWidget *widget;
    GtkWidget *vbox;
    GtkWidget *label;
    GtkWidget *swin;
    GtkWindow *window;

    page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    if (priv->widget != NULL)
        return priv->widget;

    window = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page)));

    /* Create Vpaned widget for top level */
    widget = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
    priv->widget = widget;
    gtk_widget_show (priv->widget);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->widget), "gnc-id-sx-page");

    /* Add vbox and label */
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);
    gtk_paned_pack1 (GTK_PANED(widget), vbox, TRUE, FALSE);

    label = gtk_label_new (_("Transactions"));
    gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-strong");
    gtk_widget_set_margin_start (GTK_WIDGET(label), 6);
    gnc_label_set_alignment (label, 0.0, 0);
    gtk_widget_show (label);
    gtk_box_pack_start (GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (vbox);

    /* Create scrolled window for top area */
    swin = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(swin),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_box_pack_start (GTK_BOX(vbox), swin, TRUE, TRUE, 5);
    gtk_widget_show (swin);

    {
        // gint half_way;
        // half_way = plugin_page->notebook_page->allocation.height * 0.5;
        // fixme; get a real value:
        gtk_paned_set_position (GTK_PANED(priv->widget), 160);
    }

    {
        GDate end;
        g_date_clear (&end, 1);
        gnc_gdate_set_today (&end);
        g_date_add_years (&end, 1);
        priv->instances = GNC_SX_INSTANCE_MODEL(gnc_sx_get_instances (&end, TRUE));
    }

    {
        GAction *edit_action, *delete_action;
        edit_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "SxListEditAction");
        delete_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(page), "SxListDeleteAction");
        g_simple_action_set_enabled (G_SIMPLE_ACTION(edit_action), FALSE);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(delete_action), FALSE);
    }

    {
        GtkTreeSelection *selection;
        GtkTreePath *path = gtk_tree_path_new_first ();

        priv->tree_view = GTK_TREE_VIEW(gnc_tree_view_sx_list_new (priv->instances));
        g_object_set (G_OBJECT(priv->tree_view),
                      "state-section", STATE_SECTION,
                      "show-column-menu", TRUE,
                      NULL);
        gtk_container_add (GTK_CONTAINER( swin ), GTK_WIDGET(priv->tree_view));

        selection = gtk_tree_view_get_selection (priv->tree_view);
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

        g_signal_connect (G_OBJECT(selection), "changed", (GCallback)gppsl_selection_changed_cb, (gpointer)page);
        g_signal_connect (G_OBJECT(priv->tree_view), "row-activated", (GCallback)gppsl_row_activated_cb, (gpointer)page);
        g_signal_connect (G_OBJECT(gtk_tree_view_get_model (GTK_TREE_VIEW(priv->tree_view))),
                          "model-populated", (GCallback)gppsl_model_populated_cb, (gpointer)page);

        gtk_tree_selection_select_path (selection, path);
        gtk_tree_path_free (path);
    }

    g_signal_connect (G_OBJECT(priv->tree_view), "button-press-event",
                      G_CALLBACK(treeview_button_press), page);
    g_signal_connect (G_OBJECT(priv->tree_view), "popup-menu",
                      G_CALLBACK(treeview_popup_menu), page);

    /* Add vbox and label */
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX(vbox), FALSE);
    gtk_paned_pack2 (GTK_PANED(widget), vbox, TRUE, FALSE);

    label = gtk_label_new (_("Upcoming Transactions"));
    gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-strong");
    gtk_widget_set_margin_start (GTK_WIDGET(label), 6);
    gnc_label_set_alignment (label, 0.0, 0);
    gtk_widget_show (label);

    gtk_box_pack_start (GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (vbox);

    /* Create scrolled window for bottom area */
    swin = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(swin),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_box_pack_start (GTK_BOX(vbox), swin, TRUE, TRUE, 5);
    gtk_widget_show (swin);

    {
        priv->dense_cal_model = gnc_sx_instance_dense_cal_adapter_new (GNC_SX_INSTANCE_MODEL(priv->instances));
        priv->gdcal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model (window, GNC_DENSE_CAL_MODEL(priv->dense_cal_model)));
        g_object_ref_sink (priv->gdcal);

        gnc_dense_cal_set_months_per_col (priv->gdcal, 4);
        gnc_dense_cal_set_num_months (priv->gdcal, 12);

        gtk_container_add (GTK_CONTAINER(swin), GTK_WIDGET(priv->gdcal));
    }

    priv->gnc_component_id = gnc_register_gui_component ("plugin-page-sx-list",
                             gnc_plugin_page_sx_list_refresh_cb,
                             gnc_plugin_page_sx_list_close_cb,
                             page);
    gnc_gui_component_set_session (priv->gnc_component_id,
                                   gnc_get_current_session ());

    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      NULL);

    return priv->widget;
}


static void
gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxList *page;
    GncPluginPageSxListPrivate *priv;

    page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    if (priv->widget)
    {
        g_object_unref (G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    if (priv->selected_list)
        g_list_free (priv->selected_list);

    if (priv->gnc_component_id)
    {
        gnc_unregister_gui_component (priv->gnc_component_id);
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

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    g_key_file_set_integer (key_file, group_name, "dense_cal_num_months",
                            gnc_dense_cal_get_num_months (priv->gdcal));

    g_key_file_set_integer (key_file, group_name, "paned_position",
                            gtk_paned_get_position (GTK_PANED(priv->widget)));
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

    g_return_val_if_fail (key_file, NULL);
    g_return_val_if_fail (group_name, NULL);

    /* Create the new page. */
    page = GNC_PLUGIN_PAGE_SX_LIST(gnc_plugin_page_sx_list_new ());
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

    /* Install it now so we can them manipulate the created widget */
    gnc_main_window_open_page (GNC_MAIN_WINDOW(window), GNC_PLUGIN_PAGE(page));

    {
        GError *err = NULL;
        gint num_months = g_key_file_get_integer (key_file, group_name, "dense_cal_num_months", &err);
        if (err == NULL)
            gnc_dense_cal_set_num_months (priv->gdcal, num_months);
        else
            g_error_free (err);
    }

    {
        GError *err = NULL;
        gint paned_position = g_key_file_get_integer (key_file, group_name,
                              "paned_position", &err);
        if (err == NULL)
            gtk_paned_set_position (GTK_PANED(priv->widget), paned_position);
        else
            g_error_free (err);
    }

    return GNC_PLUGIN_PAGE(page);
}


static void
gnc_plugin_page_sx_list_cmd_new (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    GncPluginPageSxList *plugin_page = user_data;
    GtkWindow *window = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));
    SchedXaction *new_sx;
    gboolean new_sx_flag = TRUE;

    new_sx = xaccSchedXactionMalloc (gnc_get_current_book());
    {
        GDate now;
        Recurrence *r = g_new0 (Recurrence, 1);
        GList *schedule;

        g_date_clear (&now, 1);
        gnc_gdate_set_today (&now);
        recurrenceSet (r, 1, PERIOD_MONTH, &now, WEEKEND_ADJ_NONE);
        schedule = gnc_sx_get_schedule (new_sx);
        schedule = g_list_append (schedule, r);
        gnc_sx_set_schedule (new_sx, schedule);
    }
    gnc_ui_scheduled_xaction_editor_dialog_create (window, new_sx, new_sx_flag);
    gppsl_update_selected_list (plugin_page, TRUE, new_sx);
}

static void
gnc_plugin_page_sx_list_cmd_refresh (GSimpleAction *simple,
                                     GVariant      *parameter,
                                     gpointer       user_data)
{
    GncPluginPageSxList *plugin_page = user_data;
    GncPluginPageSxListPrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST(plugin_page));

    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(plugin_page);
    gtk_widget_queue_draw (priv->widget);
}

static void
_edit_sx(gpointer data, gpointer user_data)
{
    gnc_ui_scheduled_xaction_editor_dialog_create (GTK_WINDOW(user_data),
        (SchedXaction*)data, FALSE);
}

static SchedXaction*
_argument_reorder_fn (GtkTreePath* list_path_data, GncTreeViewSxList* user_tree_view)
{
    return gnc_tree_view_sx_list_get_sx_from_path (user_tree_view, list_path_data);
}


static void
gnc_plugin_page_sx_list_cmd_edit (GSimpleAction *simple,
                                  GVariant      *parameter,
                                  gpointer       user_data)
{
    GncPluginPageSxList *plugin_page = user_data;
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(plugin_page);
    GtkWindow *window = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));
    GtkTreeSelection *selection;
    GList *selected_paths, *to_edit;
    GtkTreeModel *model;

    selection = gtk_tree_view_get_selection (priv->tree_view);
    selected_paths = gtk_tree_selection_get_selected_rows (selection, &model);
    if (!gnc_list_length_cmp (selected_paths, 0))
    {
        g_warning ("no selection edit.");
        return;
    }

    to_edit = gnc_g_list_map (selected_paths,
                              (GncGMapFunc)_argument_reorder_fn,
                              priv->tree_view);

    gppsl_update_selected_list (plugin_page, TRUE, NULL);
    for (GList *list = to_edit; list != NULL; list = list->next)
    {
        DEBUG ("to-edit [%s]\n", xaccSchedXactionGetName ((SchedXaction*)list->data));
        gppsl_update_selected_list (plugin_page, FALSE, list->data);
    }

    g_list_foreach (to_edit, (GFunc)_edit_sx, window);
    g_list_free (to_edit);
    g_list_foreach (selected_paths, (GFunc)gtk_tree_path_free, NULL);
    g_list_free (selected_paths);
}


static void
gnc_plugin_page_sx_list_cmd_edit_tax_options (GSimpleAction *simple,
                                              GVariant      *parameter,
                                              gpointer       user_data)
{
    GncPluginPageSxList *plugin_page = user_data;
    GtkWidget *window = GTK_WIDGET(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));

    ENTER ("(action %p, page %p)", simple, plugin_page);
    gnc_tax_info_dialog (window, NULL);
    LEAVE (" ");
}


static void
gppsl_row_activated_cb (GtkTreeView *tree_view,
                        GtkTreePath *path,
                        GtkTreeViewColumn *column,
                        gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST(user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
    GtkWindow *window = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page)));

    SchedXaction *sx = gnc_tree_view_sx_list_get_sx_from_path (
                           GNC_TREE_VIEW_SX_LIST(priv->tree_view), path);
    gnc_ui_scheduled_xaction_editor_dialog_create (window, sx, FALSE);
    gppsl_update_selected_list (page, TRUE, sx);
}


static void
_destroy_sx(gpointer data, gpointer user_data)
{
    SchedXactions *sxes;
    SchedXaction *sx = (SchedXaction*)data;
    QofBook *book;
    book = gnc_get_current_book ();
    sxes = gnc_book_get_schedxactions (book);
    gnc_sxes_del_sx (sxes, sx);
    gnc_sx_begin_edit (sx);
    xaccSchedXactionDestroy (sx);
}


static void
gnc_plugin_page_sx_list_cmd_delete (GSimpleAction *simple,
                                    GVariant      *parameter,
                                    gpointer       user_data)
{
    GncPluginPageSxList *plugin_page = user_data;
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(plugin_page);
    GtkTreeSelection *selection;
    GList *selected_paths, *to_delete = NULL;
    GtkTreeModel *model;
    GtkWindow *window;
    gchar *message = NULL;
    gint length;

    selection = gtk_tree_view_get_selection (priv->tree_view);
    selected_paths = gtk_tree_selection_get_selected_rows (selection, &model);
    if (!gnc_list_length_cmp (selected_paths, 0))
    {
        g_warning ("no selection for delete.");
        return;
    }

    to_delete = gnc_g_list_map (selected_paths,
                                (GncGMapFunc)_argument_reorder_fn,
                                 priv->tree_view);

    window = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));

    length = g_list_length (to_delete);

    /* Translators: This is a ngettext(3) message, %d is the number of scheduled transactions deleted */
    message = g_strdup_printf (ngettext ("Do you really want to delete this scheduled transaction?",
                                         "Do you really want to delete %d scheduled transactions?",
                                          length), length);

    if (gnc_verify_dialog (window, FALSE, "%s", message))
    {
        gppsl_update_selected_list (plugin_page, TRUE, NULL);
        for (GList *list = to_delete; list != NULL; list = list->next)
        {
            DEBUG("to-delete [%s]\n", xaccSchedXactionGetName ((SchedXaction*)list->data));
            gppsl_update_selected_list (plugin_page, FALSE, list->data);
        }
        g_list_foreach (to_delete, (GFunc)_destroy_sx, NULL);
    }

    g_free (message);
    g_list_free (to_delete);
    g_list_foreach (selected_paths, (GFunc)gtk_tree_path_free, NULL);
    g_list_free (selected_paths);
}

/** @} */
/** @} */

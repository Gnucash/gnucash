/********************************************************************\
 * gnc-plugin-page-sx-list.cpp : scheduled transaction plugin       *
 * GTK4 ColumnView port.                                            *
 *                                                                  *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2011 Robert Fewell                                 *
 * Author: Josh Sled <jsled@asynchronous.org>                       *
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
#include <config.h>

#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>

#include <gnc-gobject-utils.h>
#include "SX-book.h"
#include "SchedXaction.h"
#include "dialog-sx-editor.h"
#include "dialog-sx-since-last-run.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-dense-cal.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-icons.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-sx-list.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-sx-instance-dense-cal-adapter.h"
#include "gnc-sx-instance-model.h"
#include "gnc-tree-view-sx-list.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.plugin-page.sx-list"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI_SX;

#define PLUGIN_PAGE_SX_LIST_CM_CLASS "plugin-page-sx-list"
#define GNC_PREF_DIVIDER_POS "divider-position"
#define GNC_PREF_NUM_OF_MONTHS "number-of-months"

typedef struct GncPluginPageSxListPrivate
{
    gboolean disposed;
    GtkWidget *widget;
    gint gnc_component_id;
    GncSxInstanceDenseCalAdapter *dense_cal_model;
    GncDenseCal *gdcal;
    GncSxInstanceModel *instances;
    GtkColumnView *tree_view;
    GList *selected_list;
} GncPluginPageSxListPrivate;

typedef struct
{
    GncPluginPageSxList *page;
    GList *sxs;
} SxDeleteRequest;

G_DEFINE_TYPE_WITH_PRIVATE (GncPluginPageSxList, gnc_plugin_page_sx_list, GNC_TYPE_PLUGIN_PAGE)
#define GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(o) \
    ((GncPluginPageSxListPrivate*)gnc_plugin_page_sx_list_get_instance_private \
     ((GncPluginPageSxList*)o))

static void gnc_plugin_page_sx_list_dispose (GObject *object);
static void gnc_plugin_page_sx_list_finalize (GObject *object);
static GtkWidget *gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page, GKeyFile *file,
                                                const gchar *group);
static GncPluginPage *gnc_plugin_page_sx_list_recreate_page (GtkWidget *window,
                                                              GKeyFile *file,
                                                              const gchar *group);
static void gnc_plugin_page_sx_list_cmd_new (GSimpleAction *simple, GVariant *parameter,
                                             gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_edit (GSimpleAction *simple, GVariant *parameter,
                                              gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_delete (GSimpleAction *simple, GVariant *parameter,
                                                gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_run (GSimpleAction *simple, GVariant *parameter,
                                             gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_refresh (GSimpleAction *simple, GVariant *parameter,
                                                 gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_save_layout (GSimpleAction *simple, GVariant *parameter,
                                                     gpointer user_data);
static void gnc_plugin_page_sx_list_cmd_edit_tax_options (GSimpleAction *simple,
                                                           GVariant *parameter,
                                                           gpointer user_data);

static GActionEntry gnc_plugin_page_sx_list_actions [] =
{
    { "SxListAction", NULL, NULL, NULL, NULL },
    { "SxListNewAction", gnc_plugin_page_sx_list_cmd_new, NULL, NULL, NULL },
    { "SxListEditAction", gnc_plugin_page_sx_list_cmd_edit, NULL, NULL, NULL },
    { "SxListDeleteAction", gnc_plugin_page_sx_list_cmd_delete, NULL, NULL, NULL },
    { "SxListRunAction", gnc_plugin_page_sx_list_cmd_run, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_sx_list_cmd_refresh, NULL, NULL, NULL },
    { "ViewSaveLayoutAction", gnc_plugin_page_sx_list_cmd_save_layout, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_sx_list_cmd_edit_tax_options, NULL, NULL, NULL },
};
static guint gnc_plugin_page_sx_list_n_actions = G_N_ELEMENTS (gnc_plugin_page_sx_list_actions);
static const gchar *gnc_plugin_load_ui_items [] =
{
    "FilePlaceholder3", "EditPlaceholder3", "EditPlaceholder5", "ViewPlaceholder4",
    "SchedulePlaceholder0", NULL,
};

static void
gppsl_update_selected_list (GncPluginPageSxList *page, gboolean reset, SchedXaction *sx)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    if (reset)
        g_clear_pointer (&priv->selected_list, g_list_free);
    if (sx && !g_list_find (priv->selected_list, sx))
        priv->selected_list = g_list_append (priv->selected_list, sx);
}

static void
gppsl_update_actions (GncPluginPageSxList *page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    gboolean enabled = gnc_sx_list_view_get_selected_sxes (priv->tree_view) != NULL;
    GAction *edit = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), "SxListEditAction");
    GAction *remove = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), "SxListDeleteAction");
    GAction *run = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE (page), "SxListRunAction");

    g_simple_action_set_enabled (G_SIMPLE_ACTION (edit), enabled);
    g_simple_action_set_enabled (G_SIMPLE_ACTION (remove), enabled);
    g_simple_action_set_enabled (G_SIMPLE_ACTION (run), enabled);
}

static void
gppsl_selection_changed_cb (GtkSelectionModel *selection, guint position, guint n_items,
                            gpointer user_data)
{
    gppsl_update_actions (GNC_PLUGIN_PAGE_SX_LIST (user_data));
}

static void
gppsl_model_items_changed_cb (GListModel *model, guint position, guint removed, guint added,
                              gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    gnc_sx_list_view_select_sxes (priv->tree_view, priv->selected_list);
    gppsl_update_actions (page);
}

static void
gppsl_popup_closed (GtkPopover *popover, gpointer user_data)
{
    gtk_widget_unparent (GTK_WIDGET (popover));
}

static void
gppsl_popup (GncPluginPageSxList *page, double x, double y)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GtkWidget *popover;
    GMenu *menu = g_menu_new ();
    GdkRectangle point = { (int)x, (int)y, 1, 1 };

    g_menu_append (menu, _("_New Schedule"), "GncPluginPageSxListActions.SxListNewAction");
    g_menu_append (menu, _("_Edit Schedule"), "GncPluginPageSxListActions.SxListEditAction");
    g_menu_append (menu, _("_Delete Schedule"), "GncPluginPageSxListActions.SxListDeleteAction");
    g_menu_append (menu, _("_Run Schedule"), "GncPluginPageSxListActions.SxListRunAction");
    popover = gtk_popover_menu_new_from_model (G_MENU_MODEL (menu));
    g_object_unref (menu);
    gtk_widget_set_parent (popover, GTK_WIDGET (priv->tree_view));
    gtk_popover_set_pointing_to (GTK_POPOVER (popover), &point);
    g_signal_connect (popover, "closed", G_CALLBACK (gppsl_popup_closed), NULL);
    gtk_popover_popup (GTK_POPOVER (popover));
}

static void
gppsl_click_released (GtkGestureClick *gesture, gint n_press, gdouble x, gdouble y,
                      gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    guint button = gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture));

    if (button == GDK_BUTTON_SECONDARY)
    {
        gppsl_popup (page, x, y);
        return;
    }
    if (button == GDK_BUTTON_PRIMARY && n_press == 2)
        gnc_plugin_page_sx_list_cmd_edit (NULL, NULL, page);
}

static gboolean
gppsl_key_pressed (GtkEventControllerKey *controller, guint keyval, guint keycode,
                   GdkModifierType state, gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GList *sxs;
    gboolean enable = FALSE;

    if (keyval != GDK_KEY_space || !gnc_sx_list_view_enabled_column_visible (priv->tree_view))
        return GDK_EVENT_PROPAGATE;
    sxs = gnc_sx_list_view_get_selected_sxes (priv->tree_view);
    if (!sxs)
        return GDK_EVENT_STOP;
    for (GList *node = sxs; node; node = node->next)
        enable |= !xaccSchedXactionGetEnabled (GNC_SCHEDXACTION (node->data));
    if (sxs->next)
        qof_event_suspend ();
    for (GList *node = sxs; node; node = node->next)
        xaccSchedXactionSetEnabled (GNC_SCHEDXACTION (node->data), enable);
    if (sxs->next)
        qof_event_resume ();
    gppsl_update_selected_list (page, TRUE, NULL);
    for (GList *node = sxs; node; node = node->next)
        gppsl_update_selected_list (page, FALSE, GNC_SCHEDXACTION (node->data));
    g_list_free (sxs);
    gnc_sx_list_view_refresh (priv->tree_view);
    return GDK_EVENT_STOP;
}

GncPluginPage*
gnc_plugin_page_sx_list_new (void)
{
    const GList *objects = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_SX_LIST_NAME);
    if (objects && GNC_IS_PLUGIN_PAGE_SX_LIST (objects->data))
        return GNC_PLUGIN_PAGE (objects->data);
    return GNC_PLUGIN_PAGE (g_object_new (GNC_TYPE_PLUGIN_PAGE_SX_LIST, NULL));
}

static gboolean
gnc_plugin_page_sx_list_focus_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxListPrivate *priv;
    GAction *action;

    if (!GNC_IS_PLUGIN_PAGE_SX_LIST (plugin_page))
        return FALSE;
    priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (plugin_page);
    action = gnc_main_window_find_action (GNC_MAIN_WINDOW (plugin_page->window), "TransactionAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION (action), FALSE);
    action = gnc_main_window_find_action (GNC_MAIN_WINDOW (plugin_page->window), "ScheduledAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION (action), TRUE);
    action = gnc_main_window_find_action (GNC_MAIN_WINDOW (plugin_page->window), "FilePrintAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION (action), FALSE);
    gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW (plugin_page->window), plugin_page,
                                             gnc_plugin_load_ui_items);
    if (priv->tree_view && !gtk_widget_is_focus (GTK_WIDGET (priv->tree_view)))
        gtk_widget_grab_focus (GTK_WIDGET (priv->tree_view));
    return FALSE;
}

static void
gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *plugin_class = GNC_PLUGIN_PAGE_CLASS (klass);
    object_class->dispose = gnc_plugin_page_sx_list_dispose;
    object_class->finalize = gnc_plugin_page_sx_list_finalize;
    plugin_class->tab_icon = GNC_ICON_ACCOUNT;
    plugin_class->plugin_name = GNC_PLUGIN_PAGE_SX_LIST_NAME;
    plugin_class->create_widget = gnc_plugin_page_sx_list_create_widget;
    plugin_class->destroy_widget = gnc_plugin_page_sx_list_destroy_widget;
    plugin_class->save_page = gnc_plugin_page_sx_list_save_page;
    plugin_class->recreate_page = gnc_plugin_page_sx_list_recreate_page;
    plugin_class->focus_page_function = gnc_plugin_page_sx_list_focus_widget;
}

static void
gnc_plugin_page_sx_list_init (GncPluginPageSxList *page)
{
    GSimpleActionGroup *actions;
    g_object_set (page, "page-name", _("Scheduled Transactions"),
                  "ui-description", "gnc-plugin-page-sx-list.ui", NULL);
    gnc_plugin_page_add_book (GNC_PLUGIN_PAGE (page), gnc_get_current_book ());
    actions = gnc_plugin_page_create_action_group (GNC_PLUGIN_PAGE (page),
                                                    "GncPluginPageSxListActions");
    g_action_map_add_action_entries (G_ACTION_MAP (actions), gnc_plugin_page_sx_list_actions,
                                     gnc_plugin_page_sx_list_n_actions, page);
}

static void
gnc_plugin_page_sx_list_dispose (GObject *object)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (object);
    if (priv->disposed)
        return;
    priv->disposed = TRUE;
    g_clear_object (&priv->dense_cal_model);
    g_clear_object (&priv->gdcal);
    g_clear_object (&priv->instances);
    g_clear_pointer (&priv->selected_list, g_list_free);
    G_OBJECT_CLASS (gnc_plugin_page_sx_list_parent_class)->dispose (object);
}

static void
gnc_plugin_page_sx_list_finalize (GObject *object)
{
    G_OBJECT_CLASS (gnc_plugin_page_sx_list_parent_class)->finalize (object);
}

static void
gnc_plugin_page_sx_list_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (user_data);
    if (changes)
        return;
    gnc_sx_list_view_refresh (priv->tree_view);
    gtk_widget_queue_draw (priv->widget);
}

static void
gnc_plugin_page_sx_list_close_cb (gpointer user_data)
{
    gnc_main_window_close_page (GNC_PLUGIN_PAGE (user_data));
}

static GtkWidget*
gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (plugin_page);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GtkWidget *top_box, *bottom_box, *swin, *label;
    GtkWindow *window;
    GDate end;
    GtkEventController *key;
    GtkGesture *click;

    if (priv->widget)
        return priv->widget;
    window = GTK_WINDOW (gnc_plugin_page_get_window (plugin_page));
    priv->widget = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
    gtk_widget_set_name (priv->widget, "gnc-id-sx-page");
    top_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_paned_set_start_child (GTK_PANED (priv->widget), top_box);
    label = gtk_label_new (_("Transactions"));
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_widget_add_css_class (label, "gnc-class-strong");
    gtk_widget_set_margin_start (label, 6);
    gtk_box_append (GTK_BOX (top_box), label);
    swin = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin), GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_widget_set_vexpand (swin, TRUE);
    gtk_box_append (GTK_BOX (top_box), swin);
    gtk_paned_set_position (GTK_PANED (priv->widget),
                            gnc_prefs_get_int (GNC_PREFS_GROUP_SXED, GNC_PREF_DIVIDER_POS));

    g_date_clear (&end, 1);
    gnc_gdate_set_today (&end);
    g_date_add_years (&end, 1);
    priv->instances = GNC_SX_INSTANCE_MODEL (gnc_sx_get_instances (&end, TRUE));
    priv->tree_view = gnc_sx_list_view_new (priv->instances);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (swin), GTK_WIDGET (priv->tree_view));
    g_signal_connect (gnc_sx_list_view_get_selection (priv->tree_view), "selection-changed",
                      G_CALLBACK (gppsl_selection_changed_cb), page);
    g_signal_connect (G_LIST_MODEL (gnc_sx_list_view_get_selection (priv->tree_view)), "items-changed",
                      G_CALLBACK (gppsl_model_items_changed_cb), page);
    key = gtk_event_controller_key_new ();
    g_signal_connect (key, "key-pressed", G_CALLBACK (gppsl_key_pressed), page);
    gtk_widget_add_controller (GTK_WIDGET (priv->tree_view), key);
    click = gtk_gesture_click_new ();
    g_signal_connect (click, "released", G_CALLBACK (gppsl_click_released), page);
    gtk_widget_add_controller (GTK_WIDGET (priv->tree_view), GTK_EVENT_CONTROLLER (click));
    gnc_sx_list_view_select_sxes (priv->tree_view, NULL);
    gppsl_update_actions (page);

    bottom_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_paned_set_end_child (GTK_PANED (priv->widget), bottom_box);
    label = gtk_label_new (_("Upcoming Transactions"));
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_widget_add_css_class (label, "gnc-class-strong");
    gtk_widget_set_margin_start (label, 6);
    gtk_box_append (GTK_BOX (bottom_box), label);
    swin = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (swin), GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_widget_set_vexpand (swin, TRUE);
    gtk_box_append (GTK_BOX (bottom_box), swin);
    priv->dense_cal_model = gnc_sx_instance_dense_cal_adapter_new (priv->instances);
    priv->gdcal = GNC_DENSE_CAL (gnc_dense_cal_new_with_model (window,
        GNC_DENSE_CAL_MODEL (priv->dense_cal_model)));
    g_object_ref_sink (priv->gdcal);
    gint months = gnc_prefs_get_int (GNC_PREFS_GROUP_SXED, GNC_PREF_NUM_OF_MONTHS);
    gnc_dense_cal_set_num_months (priv->gdcal, months > 0 ? months : 12);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (swin), GTK_WIDGET (priv->gdcal));

    priv->gnc_component_id = gnc_register_gui_component ("plugin-page-sx-list",
        gnc_plugin_page_sx_list_refresh_cb, gnc_plugin_page_sx_list_close_cb, page);
    gnc_gui_component_set_session (priv->gnc_component_id, gnc_get_current_session ());
    g_signal_connect (plugin_page, "inserted", G_CALLBACK (gnc_plugin_page_inserted_cb), NULL);
    return priv->widget;
}

static void
gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (plugin_page);
    gnc_plugin_page_disconnect_page_changed (plugin_page);
    g_idle_remove_by_data (plugin_page);
    if (priv->widget)
    {
        g_object_unref (priv->widget);
        priv->widget = NULL;
        priv->tree_view = NULL;
    }
    g_clear_pointer (&priv->selected_list, g_list_free);
    if (priv->gnc_component_id)
    {
        gnc_unregister_gui_component (priv->gnc_component_id);
        priv->gnc_component_id = 0;
    }
}

static void
gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (plugin_page);
    g_key_file_set_integer (file, group, "dense_cal_num_months", gnc_dense_cal_get_num_months (priv->gdcal));
    g_key_file_set_integer (file, group, "paned_position", gtk_paned_get_position (GTK_PANED (priv->widget)));
}

static GncPluginPage*
gnc_plugin_page_sx_list_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (gnc_plugin_page_sx_list_new ());
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GError *error = NULL;
    gint value;

    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), GNC_PLUGIN_PAGE (page));
    value = g_key_file_get_integer (file, group, "dense_cal_num_months", &error);
    if (!error)
        gnc_dense_cal_set_num_months (priv->gdcal, value);
    g_clear_error (&error);
    value = g_key_file_get_integer (file, group, "paned_position", &error);
    if (!error)
        gtk_paned_set_position (GTK_PANED (priv->widget), value);
    g_clear_error (&error);
    return GNC_PLUGIN_PAGE (page);
}

static void
gnc_plugin_page_sx_list_cmd_new (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    SchedXaction *sx = xaccSchedXactionMalloc (gnc_get_current_book ());
    GDate today;
    Recurrence *recurrence = g_new0 (Recurrence, 1);
    GList *schedule;

    gnc_gdate_set_today (&today);
    recurrenceSet (recurrence, 1, PERIOD_MONTH, &today, WEEKEND_ADJ_NONE);
    schedule = g_list_append (gnc_sx_get_schedule (sx), recurrence);
    gnc_sx_set_schedule (sx, schedule);
    gnc_ui_scheduled_xaction_editor_dialog_create
        (GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page))), sx, TRUE);
    gppsl_update_selected_list (page, TRUE, sx);
}

static void
gnc_plugin_page_sx_list_cmd_edit (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    GList *sxs = gnc_sx_list_view_get_selected_sxes (priv->tree_view);
    GtkWindow *window = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));

    if (!sxs)
        return;
    gppsl_update_selected_list (page, TRUE, NULL);
    for (GList *node = sxs; node; node = node->next)
    {
        gppsl_update_selected_list (page, FALSE, GNC_SCHEDXACTION (node->data));
        gnc_ui_scheduled_xaction_editor_dialog_create (window, GNC_SCHEDXACTION (node->data), FALSE);
    }
    g_list_free (sxs);
}

static void
destroy_sx (SchedXaction *sx)
{
    SchedXactions *sxes = gnc_book_get_schedxactions (gnc_get_current_book ());
    gnc_sxes_del_sx (sxes, sx);
    gnc_sx_begin_edit (sx);
    xaccSchedXactionDestroy (sx);
}

static void
sx_delete_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    SxDeleteRequest *request = static_cast<SxDeleteRequest*> (user_data);
    if (response == GTK_RESPONSE_YES)
    {
        gppsl_update_selected_list (request->page, TRUE, NULL);
        for (GList *node = request->sxs; node; node = node->next)
            destroy_sx (GNC_SCHEDXACTION (node->data));
    }
    g_list_free (request->sxs);
    g_free (request);
}

static void
gnc_plugin_page_sx_list_cmd_delete (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST (user_data);
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (page);
    SxDeleteRequest *request;
    gchar *names = NULL, *message;
    GList *sxs = gnc_sx_list_view_get_selected_sxes (priv->tree_view);

    if (!sxs)
        return;
    for (GList *node = sxs; node; node = node->next)
    {
        const gchar *name = xaccSchedXactionGetName (GNC_SCHEDXACTION (node->data));
        gchar *next = names ? g_strjoin ("\n", names, name, NULL) : g_strdup (name);
        g_free (names);
        names = next;
    }
    message = g_strdup_printf ("%s\n\n%s",
        ngettext ("Do you really want to delete this scheduled transaction?",
                  "Do you really want to delete these scheduled transactions?", g_list_length (sxs)), names);
    g_free (names);
    request = g_new0 (SxDeleteRequest, 1);
    request->page = page;
    request->sxs = sxs;
    gnc_verify_dialog_async (GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page))), FALSE,
                             sx_delete_finished, request, "%s", message);
    g_free (message);
}

static void
gnc_plugin_page_sx_list_cmd_run (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (user_data);
    GList *sxs = gnc_sx_list_view_get_selected_sxes (priv->tree_view);
    if (!sxs)
        return;
    gnc_ui_sx_since_last_run_dialog (gnc_sx_get_select_instances (sxs));
    g_list_free (sxs);
}

static void
gnc_plugin_page_sx_list_cmd_refresh (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (user_data);
    gnc_sx_list_view_refresh (priv->tree_view);
    gtk_widget_queue_draw (priv->widget);
}

static void
gnc_plugin_page_sx_list_cmd_save_layout (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE (user_data);
    gnc_prefs_set_int (GNC_PREFS_GROUP_SXED, GNC_PREF_DIVIDER_POS,
                       gtk_paned_get_position (GTK_PANED (priv->widget)));
    gnc_prefs_set_int (GNC_PREFS_GROUP_SXED, GNC_PREF_NUM_OF_MONTHS,
                       gnc_dense_cal_get_num_months (priv->gdcal));
}

static void
gnc_plugin_page_sx_list_cmd_edit_tax_options (GSimpleAction *simple, GVariant *parameter,
                                               gpointer user_data)
{
    gnc_tax_info_dialog (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (user_data)), NULL);
}

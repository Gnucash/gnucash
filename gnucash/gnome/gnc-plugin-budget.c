/* Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * gnc-plugin-budget.c --
 *   (based on gnc-plugin-account-tree.c)
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "dialog-utils.h"
#include "gnc-plugin-budget.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-tree-model-budget.h"

#include "qof.h"
#include "gnc-features.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"

#define PLUGIN_ACTIONS_NAME "gnc-plugin-budget-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-budget.ui"

static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_budget_finalize (GObject *object);

/* Command Callbacks */
static void gnc_plugin_budget_cmd_new_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_budget_cmd_open_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_budget_cmd_copy_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_budget_cmd_delete_budget (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static GActionEntry gnc_plugin_actions [] =
{
    { "ActionsBudgetAction", NULL, NULL, NULL, NULL },
    { "NewBudgetAction", gnc_plugin_budget_cmd_new_budget, NULL, NULL, NULL },
    { "OpenBudgetAction", gnc_plugin_budget_cmd_open_budget, NULL, NULL, NULL },
    { "CopyBudgetAction", gnc_plugin_budget_cmd_copy_budget, NULL, NULL, NULL },
    { "DeleteBudgetAction", gnc_plugin_budget_cmd_delete_budget, NULL, NULL, NULL },

};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

/** The default menu items that need to be add to the menu */
static const gchar *gnc_plugin_load_ui_items [] =
{
    "ActionsPlaceholder3",
    NULL,
};

static const gchar *plugin_writeable_actions[] =
{
    /* actions which must be disabled on a readonly book. */
    "NewBudgetAction",
    "CopyBudgetAction",
    "DeleteBudgetAction",
    NULL
};

struct _GncPluginBudget
{
    GncPlugin gnc_plugin;
};

GncPlugin *
gnc_plugin_budget_new (void)
{
    GncPluginBudget *plugin;
    ENTER(" ");

    /* Reference the budget page plugin to ensure it exists in the gtk
     * type system. */
    GNC_TYPE_PLUGIN_PAGE_BUDGET;

    plugin = g_object_new (GNC_TYPE_PLUGIN_BUDGET, NULL);
    LEAVE(" ");
    return GNC_PLUGIN(plugin);
}

static void
page_changed (GncMainWindow *window, GncPluginPage *page, gpointer user_data)
{
    GSimpleActionGroup *simple_action_group =
        gnc_main_window_get_action_group (window, PLUGIN_ACTIONS_NAME);

    if (qof_book_is_readonly (gnc_get_current_book()))
        gnc_plugin_set_actions_enabled (G_ACTION_MAP(simple_action_group), plugin_writeable_actions,
                                        FALSE);
}

static void
add_to_window (GncPlugin *plugin, GncMainWindow *mainwindow, GQuark type)
{
    g_signal_connect (mainwindow, "page_changed", G_CALLBACK (page_changed), plugin);
}

static void
remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type)
{
    g_signal_handlers_disconnect_by_func (window, G_CALLBACK(page_changed), plugin);
}

G_DEFINE_TYPE(GncPluginBudget, gnc_plugin_budget, GNC_TYPE_PLUGIN)

static void
gnc_plugin_budget_class_init (GncPluginBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS(klass);

    ENTER (" ");
    object_class->finalize = gnc_plugin_budget_finalize;

    plugin_class->plugin_name        = GNC_PLUGIN_BUDGET_NAME;
    plugin_class->actions_name       = PLUGIN_ACTIONS_NAME;
    plugin_class->actions            = gnc_plugin_actions;
    plugin_class->n_actions          = gnc_plugin_n_actions;
    plugin_class->ui_filename        = PLUGIN_UI_FILENAME;
    plugin_class->ui_updates         = gnc_plugin_load_ui_items;
    plugin_class->add_to_window      = add_to_window;
    plugin_class->remove_from_window = remove_from_window;

    LEAVE (" ");
}

static void
gnc_plugin_budget_init (GncPluginBudget *plugin)
{
}

static void
gnc_plugin_budget_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_BUDGET(object));

    ENTER(" ");
    G_OBJECT_CLASS (gnc_plugin_budget_parent_class)->finalize(object);
    LEAVE(" ");

}

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

static void
copy_budget (GncBudget *bgt, GncMainWindow *window)
{
    GncBudget *copy = gnc_budget_clone (bgt);
    gchar *name = g_strdup_printf ("Copy of %s", gnc_budget_get_name (bgt));
    gnc_budget_set_name (copy, name);
    g_free (name);

    gnc_main_window_open_page (window, gnc_plugin_page_budget_new (copy));
}

static void
row_activated_cb (GtkTreeView *tv, GtkTreePath *path,
                  GtkTreeViewColumn *column, gpointer user_data)
{
    gtk_widget_activate (GTK_WIDGET(user_data)); //ok button
}

static void
select_cancel_button_cb (GtkWidget *widget, gpointer user_data)
{
    gtk_window_destroy (GTK_WINDOW(user_data));
}

static GncBudget *
get_budget_from_tree_view (GtkTreeView *tv)
{
    GncBudget *bgt = NULL;
    GtkTreeIter iter;
    GtkTreeSelection *sel = gtk_tree_view_get_selection (tv);
    GtkTreeModel *tm = gtk_tree_view_get_model (tv);

    if (gtk_tree_selection_get_selected (sel, &tm, &iter))
        bgt = gnc_tree_model_budget_get_budget (tm, &iter);

    return bgt;
}

static void
select_open_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    GtkWidget *main_window = user_data;
    GtkTreeView *tv = g_object_get_data (G_OBJECT(widget), "tree-view");
    GncBudget *bgt = get_budget_from_tree_view (tv);

    if (bgt)
        gnc_main_window_open_page (GNC_MAIN_WINDOW(main_window),
                                   gnc_plugin_page_budget_new (bgt));

    gtk_window_destroy (GTK_WINDOW(gtk_widget_get_root (GTK_WIDGET(widget))));
}

static void
select_copy_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    GtkWidget *main_window = user_data;
    GtkTreeView *tv = g_object_get_data (G_OBJECT(widget), "tree-view");
    GncBudget *bgt = get_budget_from_tree_view (tv);

    if (bgt)
        copy_budget (bgt, GNC_MAIN_WINDOW(main_window));

    gtk_window_destroy (GTK_WINDOW(gtk_widget_get_root (GTK_WIDGET(widget))));
}

static void
select_delete_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    GtkWidget *main_window = user_data;
    GtkTreeView *tv = g_object_get_data (G_OBJECT(widget), "tree-view");
    GncBudget *bgt = get_budget_from_tree_view (tv);

    if (bgt)
        gnc_budget_gui_delete_budget (bgt);

    gtk_window_destroy (GTK_WINDOW(gtk_widget_get_root (GTK_WIDGET(widget))));
}

static gboolean
select_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                            guint keycode, GdkModifierType state,
                            gpointer user_data)
{
    if (keyval == GDK_KEY_Escape)
    {
        gtk_window_destroy (GTK_WINDOW(user_data));
        return TRUE;
    }
    else
        return FALSE;
}

static GtkWidget *
gnc_budget_create_select_gui (GtkWindow *parent, QofBook *book)
{
    GtkWidget *win;
    GtkWidget *sw;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    GtkBuilder  *builder;
    GtkTreeView *tv;
    GtkTreeIter iter;
    GtkTreeSelection *sel;
    GtkTreeModel *tm;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.ui", "budget_select_window");
    win = GTK_WIDGET(gtk_builder_get_object (builder, "budget_select_window"));

   if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(win), GTK_WINDOW(parent));

    sw = GTK_WIDGET(gtk_builder_get_object (builder, "select_sw"));
    cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "select_cancel_button"));
    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "select_ok_button"));

    tv = GTK_TREE_VIEW(gtk_tree_view_new ());
    sel = gtk_tree_view_get_selection (tv);
    gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);
    g_signal_connect (tv, "row-activated", G_CALLBACK(row_activated_cb), ok_button);
    tm = gnc_tree_model_budget_new (book);
    gnc_tree_view_budget_set_model (tv, tm);

    gboolean sort_ascending = TRUE;
    GtkSortType sort_type = sort_ascending ? GTK_SORT_ASCENDING : GTK_SORT_DESCENDING;

    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(tm),
                                          BUDGET_NAME_COLUMN, sort_type);

    g_object_unref (tm);

    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(sw), GTK_WIDGET(tv));
    gtk_widget_set_vexpand (GTK_WIDGET(sw), TRUE);

    g_object_set_data (G_OBJECT(ok_button), "tree-view", tv);

    gtk_widget_set_visible (GTK_WIDGET(win), TRUE);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(win), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(select_window_key_press_cb), win);

    g_object_set_data (G_OBJECT(win), "ok-button", ok_button);

    g_signal_connect (G_OBJECT(cancel_button), "clicked",
                      G_CALLBACK(select_cancel_button_cb), win);

    // Preselect the default budget
    GncBudget *bgt = gnc_budget_get_default (book);

    if (bgt && gnc_tree_model_budget_get_iter_for_budget (tm, &iter, bgt))
    {
        GtkTreePath *path = gtk_tree_model_get_path (tm, &iter);
        gtk_tree_view_set_cursor (tv, path, NULL, FALSE);
        gtk_tree_path_free (path);
    }

    g_object_unref (G_OBJECT(builder));

    return win;
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

/* Make a new budget; put it in a page; open the page. */
static void
gnc_plugin_budget_cmd_new_budget (GSimpleAction *simple,
                                  GVariant      *parameter,
                                  gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    QofBook *book = gnc_get_current_book();

    g_return_if_fail (data != NULL);

    if (!gnc_features_check_used (book, GNC_FEATURE_BUDGET_UNREVERSED))
    {
        gnc_features_set_used (book, GNC_FEATURE_BUDGET_UNREVERSED);
        PWARN ("Setting feature BUDGET_UNREVERSED. This book now requires \
GnuCash 3.8 or later.");
    }

    GncBudget *budget = gnc_budget_new (gnc_get_current_book());
    GncPluginPage *page = gnc_plugin_page_budget_new (budget);

    gchar *date = gnc_print_time64 (gnc_time (NULL),
                      qof_date_format_get_string (QOF_DATE_FORMAT_LOCALE));
    gchar *description = g_strdup_printf ("%s: %s",  _("Created"), date);
    gnc_budget_set_description (budget, description);
    g_free (description);
    g_free (date);

    gnc_main_window_open_page (data->window, page);
}

/* If only one budget exists, open it; otherwise user selects one to open */
static void
gnc_plugin_budget_cmd_open_budget (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;

    g_return_if_fail (data != NULL);

    QofBook *book = gnc_get_current_book ();
    QofCollection *col = qof_book_get_collection (book, GNC_ID_BUDGET);
    guint count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
        {
            GncBudget *bgt = gnc_budget_get_default (book);

            if (bgt)
                gnc_main_window_open_page (data->window,
                                           gnc_plugin_page_budget_new (bgt));

        }
        else
        {
            GtkWidget *win = gnc_budget_create_select_gui (GTK_WINDOW(data->window), book);
            GtkWidget *ok_button = GTK_WIDGET(g_object_get_data (G_OBJECT(win), "ok-button"));
            g_signal_connect (G_OBJECT(ok_button), "clicked",
                              G_CALLBACK(select_open_ok_button_cb), data->window);
        }
    }
    else     /* if no budgets exist yet, just open a new budget */
        gnc_plugin_budget_cmd_new_budget (simple, parameter, user_data);
}

/* If only one budget exists, create a copy of it; otherwise user selects one to copy */
static void
gnc_plugin_budget_cmd_copy_budget (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;

    g_return_if_fail (data != NULL);

    QofBook *book = gnc_get_current_book ();
    QofCollection *col = qof_book_get_collection (book, GNC_ID_BUDGET);
    guint count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
        {
            GncBudget *bgt = gnc_budget_get_default (book);

            if (bgt)
                copy_budget (bgt, data->window);
        }
        else
        {
            GtkWidget *win = gnc_budget_create_select_gui (GTK_WINDOW(data->window), book);
            GtkWidget *ok_button = GTK_WIDGET(g_object_get_data (G_OBJECT(win), "ok-button"));
            g_signal_connect (G_OBJECT(ok_button), "clicked",
                              G_CALLBACK(select_copy_ok_button_cb), data->window);
        }
    }
    else     /* if no budgets exist yet, just open a new budget */
        gnc_plugin_budget_cmd_new_budget (simple, parameter, user_data);
}

/* user selects budget to delete */
static void
gnc_plugin_budget_cmd_delete_budget (GSimpleAction *simple,
                                     GVariant      *parameter,
                                     gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;

    g_return_if_fail (data != NULL);

    QofBook *book = gnc_get_current_book ();
    if (qof_collection_count (qof_book_get_collection (book, GNC_ID_BUDGET)) == 0)
        return;

    GtkWidget *win = gnc_budget_create_select_gui (GTK_WINDOW(data->window), book);
    GtkWidget *ok_button = GTK_WIDGET(g_object_get_data (G_OBJECT(win), "ok-button"));

    g_signal_connect (G_OBJECT(ok_button), "clicked",
                      G_CALLBACK(select_delete_ok_button_cb), data->window);
}

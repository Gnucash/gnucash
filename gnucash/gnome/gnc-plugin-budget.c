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
budget_list_item_setup_cb (GtkSignalListItemFactory *factory,
                           GtkListItem *list_item,
                           gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
budget_list_item_bind_cb (GtkSignalListItemFactory *factory,
                          GtkListItem *list_item,
                          gpointer user_data)
{
    GncBudgetListItem *item = GNC_BUDGET_LIST_ITEM (
        gtk_list_item_get_item (list_item));
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (list_item));
    gboolean description = GPOINTER_TO_INT (user_data);

    gtk_label_set_text (label, description
                         ? gnc_budget_list_item_get_description (item)
                         : gnc_budget_list_item_get_name (item));
}

static GtkListItemFactory *
budget_list_item_factory_new (gboolean description)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();

    g_signal_connect (factory, "setup", G_CALLBACK (budget_list_item_setup_cb), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (budget_list_item_bind_cb),
                      GINT_TO_POINTER (description));
    return factory;
}

static GtkOrdering
budget_name_sort_cb (gconstpointer first, gconstpointer second, gpointer user_data)
{
    GncBudgetListItem *first_item = GNC_BUDGET_LIST_ITEM ((gpointer) first);
    GncBudgetListItem *second_item = GNC_BUDGET_LIST_ITEM ((gpointer) second);

    const gchar *first_name = gnc_budget_list_item_get_name (first_item);
    const gchar *second_name = gnc_budget_list_item_get_name (second_item);

    return gtk_ordering_from_cmpfunc (g_utf8_collate (first_name ? first_name : "",
                                                  second_name ? second_name : ""));
}

static GtkOrdering
budget_description_sort_cb (gconstpointer first, gconstpointer second, gpointer user_data)
{
    GncBudgetListItem *first_item = GNC_BUDGET_LIST_ITEM ((gpointer) first);
    GncBudgetListItem *second_item = GNC_BUDGET_LIST_ITEM ((gpointer) second);

    const gchar *first_description = gnc_budget_list_item_get_description (first_item);
    const gchar *second_description = gnc_budget_list_item_get_description (second_item);

    return gtk_ordering_from_cmpfunc (g_utf8_collate (
        first_description ? first_description : "",
        second_description ? second_description : ""));
}

static void
row_activated_cb (GtkColumnView *view, guint position, gpointer user_data)
{
    gtk_widget_activate (GTK_WIDGET (user_data)); // ok button
}

static void
select_cancel_button_cb (GtkWidget *widget, gpointer user_data)
{
    gtk_window_destroy (GTK_WINDOW (user_data));
}

static GncBudget *
get_budget_from_selection (GtkSingleSelection *selection)
{
    guint position = gtk_single_selection_get_selected (selection);

    if (position == GTK_INVALID_LIST_POSITION)
        return NULL;

    GListModel *model = gtk_single_selection_get_model (selection);
    GncBudgetListItem *item = GNC_BUDGET_LIST_ITEM (
        g_list_model_get_item (model, position));
    GncBudget *budget;

    if (!item)
        return NULL;

    budget = gnc_budget_list_item_get_budget (item);
    g_object_unref (item);
    return budget;
}

static void
select_open_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    GtkWidget *main_window = GTK_WIDGET (user_data);
    GtkSingleSelection *selection = GTK_SINGLE_SELECTION (
        g_object_get_data (G_OBJECT (widget), "budget-selection"));
    GncBudget *budget = get_budget_from_selection (selection);

    if (budget)
        gnc_main_window_open_page (GNC_MAIN_WINDOW (main_window),
                                   gnc_plugin_page_budget_new (budget));

    gtk_window_destroy (GTK_WINDOW (gtk_widget_get_root (GTK_WIDGET (widget))));
}

static void
select_copy_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    GtkWidget *main_window = GTK_WIDGET (user_data);
    GtkSingleSelection *selection = GTK_SINGLE_SELECTION (
        g_object_get_data (G_OBJECT (widget), "budget-selection"));
    GncBudget *budget = get_budget_from_selection (selection);

    if (budget)
        copy_budget (budget, GNC_MAIN_WINDOW (main_window));

    gtk_window_destroy (GTK_WINDOW (gtk_widget_get_root (GTK_WIDGET (widget))));
}

static void
select_delete_ok_button_cb (GtkWidget *widget, gpointer user_data)
{
    (void)user_data;
    GtkSingleSelection *selection = GTK_SINGLE_SELECTION (
        g_object_get_data (G_OBJECT (widget), "budget-selection"));
    GncBudget *budget = get_budget_from_selection (selection);

    if (budget)
        gnc_budget_gui_delete_budget (budget);

    gtk_window_destroy (GTK_WINDOW (gtk_widget_get_root (GTK_WIDGET (widget))));
}

static gboolean
select_window_key_press_cb (GtkEventControllerKey *key, guint keyval,
                            guint keycode, GdkModifierType state,
                            gpointer user_data)
{
    if (keyval == GDK_KEY_Escape)
    {
        gtk_window_destroy (GTK_WINDOW (user_data));
        return TRUE;
    }

    return FALSE;
}

static GtkWidget *
gnc_budget_create_select_gui (GtkWindow *parent, QofBook *book)
{
    GtkWidget *win;
    GtkWidget *sw;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    GtkBuilder *builder;
    GtkColumnView *view;
    GtkSingleSelection *selection;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-budget.ui", "budget_select_window");
    win = GTK_WIDGET (gtk_builder_get_object (builder, "budget_select_window"));

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (win), parent);

    sw = GTK_WIDGET (gtk_builder_get_object (builder, "select_sw"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "select_cancel_button"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "select_ok_button"));

    GListModel *model = gnc_budget_list_model_new (book);
    GtkSortListModel *sorted_model = gtk_sort_list_model_new (model, NULL);
    GtkColumnViewColumn *name_column;
    GtkColumnViewColumn *description_column;
    GtkSorter *name_sorter;
    GtkSorter *description_sorter;

    selection = gtk_single_selection_new (G_LIST_MODEL (sorted_model));
    view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (selection)));

    name_sorter = GTK_SORTER (gtk_custom_sorter_new (budget_name_sort_cb, NULL, NULL));
    name_column = gtk_column_view_column_new (_("Name"),
                                              budget_list_item_factory_new (FALSE));
    gtk_column_view_column_set_sorter (name_column, name_sorter);
    gtk_column_view_append_column (view, name_column);
    g_object_unref (name_sorter);

    description_sorter = GTK_SORTER (gtk_custom_sorter_new (budget_description_sort_cb,
                                                            NULL, NULL));
    description_column = gtk_column_view_column_new (_("Description"),
                                                      budget_list_item_factory_new (TRUE));
    gtk_column_view_column_set_sorter (description_column, description_sorter);
    gtk_column_view_append_column (view, description_column);
    g_object_unref (description_column);
    g_object_unref (description_sorter);

    gtk_sort_list_model_set_sorter (sorted_model, gtk_column_view_get_sorter (view));
    gtk_column_view_sort_by_column (view, name_column, GTK_SORT_ASCENDING);
    g_object_unref (name_column);
    g_signal_connect (view, "activate", G_CALLBACK (row_activated_cb), ok_button);

    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (sw), GTK_WIDGET (view));
    gtk_widget_set_vexpand (GTK_WIDGET (sw), TRUE);
    g_object_set_data (G_OBJECT (ok_button), "budget-selection", selection);

    gtk_widget_set_visible (win, TRUE);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (win, event_controller_window);
    g_signal_connect (event_controller_window, "key-pressed",
                      G_CALLBACK (select_window_key_press_cb), win);

    g_object_set_data (G_OBJECT (win), "ok-button", ok_button);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (select_cancel_button_cb), win);

    // Preselect the default budget after sorting by its displayed name.
    GncBudget *budget = gnc_budget_get_default (book);
    if (budget)
    {
        guint position = gnc_budget_list_model_get_position (
            gtk_single_selection_get_model (selection), budget);
        if (position != G_MAXUINT)
            gtk_single_selection_set_selected (selection, position);
    }

    g_object_unref (builder);
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

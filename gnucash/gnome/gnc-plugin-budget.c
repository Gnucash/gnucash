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

#include "gnc-plugin-budget.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-tree-model-budget.h"

#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"

#define PLUGIN_ACTIONS_NAME "gnc-plugin-budget-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-budget-ui.xml"

static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_budget_class_init (GncPluginBudgetClass *klass);
static void gnc_plugin_budget_init (GncPluginBudget *plugin);
static void gnc_plugin_budget_finalize (GObject *object);
static void gnc_plugin_budget_add_to_window (GncPlugin *plugin,
                                             GncMainWindow *window, GQuark type);

/* Command Callbacks */
static void gnc_plugin_budget_cmd_new_budget (GtkAction *action,
        GncMainWindowActionData *data);
static void gnc_plugin_budget_cmd_open_budget (GtkAction *action,
        GncMainWindowActionData *data);
static void gnc_plugin_budget_cmd_copy_budget (GtkAction *action,
        GncMainWindowActionData *data);

static GtkActionEntry gnc_plugin_actions [] =
{
    {
        "NewBudgetAction", NULL, N_("New Budget"), NULL,
        N_("Create a new Budget"),
        G_CALLBACK (gnc_plugin_budget_cmd_new_budget)
    },

    {
        "OpenBudgetAction", NULL, N_("Open Budget"), NULL,
        N_("Open an existing Budget"),
        G_CALLBACK (gnc_plugin_budget_cmd_open_budget)
    },

    {
        "CopyBudgetAction", NULL, N_("Copy Budget"), NULL,
        N_("Copy an existing Budget"),
        G_CALLBACK (gnc_plugin_budget_cmd_copy_budget)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginBudgetPrivate
{
    gpointer dummy;
} GncPluginBudgetPrivate;

#define GNC_PLUGIN_BUDGET_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_BUDGET, GncPluginBudgetPrivate))

static GObjectClass *parent_class = NULL;

GncPlugin * gnc_plugin_budget_new (void)
{
    GncPluginBudget *plugin;
    ENTER(" ");

    /* Reference the budget page plugin to ensure it exists in the gtk
     * type system. */
    GNC_TYPE_PLUGIN_PAGE_BUDGET;

    plugin = g_object_new (GNC_TYPE_PLUGIN_BUDGET, NULL);
    LEAVE(" ");
    return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_budget_main_window_page_changed (GncMainWindow *window,
        GncPluginPage *plugin_page, gpointer user_data)
{
    // We continue only if the plugin_page is a valid
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    if (gnc_main_window_get_current_page (window) == plugin_page)
    {
        if (!GNC_IS_PLUGIN_PAGE_BUDGET(plugin_page))
            return;

        // The page changed signal is emitted multiple times so we need
        // to use an idle_add to change the focus to the tree view
        g_idle_remove_by_data (GNC_PLUGIN_PAGE_BUDGET (plugin_page));
        g_idle_add ((GSourceFunc)gnc_plugin_page_budget_focus,
                      GNC_PLUGIN_PAGE_BUDGET (plugin_page));
    }
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginBudget, gnc_plugin_budget, GNC_TYPE_PLUGIN)

static void
gnc_plugin_budget_class_init (GncPluginBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    ENTER (" ");
    parent_class = g_type_class_peek_parent (klass);
    object_class->finalize = gnc_plugin_budget_finalize;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_budget_add_to_window;

    plugin_class->plugin_name  = GNC_PLUGIN_BUDGET_NAME;
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    LEAVE (" ");
}

static void
gnc_plugin_budget_init(GncPluginBudget *plugin)
{
}

static void
gnc_plugin_budget_finalize(GObject *object)
{
    g_return_if_fail(GNC_IS_PLUGIN_BUDGET (object));

    ENTER(" ");
    (parent_class->finalize)(object);
    LEAVE(" ");

}

/**
 * Called when this plugin is added to a main window.  Connect a few callbacks
 * here to track page changes.
 *
 */
static void gnc_plugin_budget_add_to_window (GncPlugin *plugin,
        GncMainWindow *mainwindow,
        GQuark type)
{
    g_signal_connect(mainwindow, "page_changed",
                     G_CALLBACK(gnc_plugin_budget_main_window_page_changed),
                     plugin);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

/* Make a new budget; put it in a page; open the page. */
static void
gnc_plugin_budget_cmd_new_budget (GtkAction *action,
                                  GncMainWindowActionData *data)
{
    GncBudget *budget;
    GncPluginPage *page;

    g_return_if_fail (data != NULL);

    budget = gnc_budget_new(gnc_get_current_book());
    page = gnc_plugin_page_budget_new(budget);
    gnc_main_window_open_page (data->window, page);
}

/* If only one budget exists, open it; otherwise user selects one to open */
static void
gnc_plugin_budget_cmd_open_budget (GtkAction *action,
                                   GncMainWindowActionData *data)
{
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;
    g_return_if_fail (data != NULL);

    book = gnc_get_current_book();
    col = qof_book_get_collection(book, GNC_ID_BUDGET);
    count = qof_collection_count(col);
    if (count > 0)
    {
        if (count == 1)
        {
            bgt = gnc_budget_get_default(book);
        }
        else
        {
            bgt = gnc_budget_gui_select_budget(GTK_WINDOW(data->window), book);
        }

        if (bgt) gnc_main_window_open_page(
                data->window, gnc_plugin_page_budget_new(bgt));
    }
    else     /* if no budgets exist yet, just open a new budget */
    {
        gnc_plugin_budget_cmd_new_budget(action, data);
    }
}

/* If only one budget exists, create a copy of it; otherwise user selects one to copy */
static void
gnc_plugin_budget_cmd_copy_budget (GtkAction *action,
                                   GncMainWindowActionData *data)
{
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;
    g_return_if_fail (data != NULL);

    book = gnc_get_current_book();
    col = qof_book_get_collection(book, GNC_ID_BUDGET);
    count = qof_collection_count(col);
    if (count > 0)
    {
        if (count == 1)
        {
            bgt = gnc_budget_get_default(book);
        }
        else
        {
            bgt = gnc_budget_gui_select_budget(GTK_WINDOW(data->window), book);
        }

        if (bgt)
        {
            GncBudget* copy;
            gchar* name;

            copy = gnc_budget_clone(bgt);
            name = g_strdup_printf("Copy of %s", gnc_budget_get_name(bgt));
            gnc_budget_set_name(copy, name);
            g_free(name);

            gnc_main_window_open_page(
                data->window, gnc_plugin_page_budget_new(copy));
        }
    }
    else     /* if no budgets exist yet, just open a new budget */
    {
        gnc_plugin_budget_cmd_new_budget(action, data);
    }
}

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

static void
row_activated_cb(GtkTreeView *tv, GtkTreePath *path, GtkTreeViewColumn *column,
                 gpointer data)
{
    gtk_dialog_response(GTK_DIALOG(data), GTK_RESPONSE_OK);
}

GncBudget *
gnc_budget_gui_select_budget(GtkWindow *parent, QofBook *book)
{
    GncBudget *bgt;
    GtkDialog *dlg;
    GtkTreeView *tv;
    GtkTreeIter iter;
    GtkTreeSelection *sel;
    GtkTreeModel *tm;
    gint response;
    gboolean ok;

    dlg = GTK_DIALOG(gtk_dialog_new_with_buttons(
                         _("Select a Budget"), parent, GTK_DIALOG_MODAL,
                         _("_OK"), GTK_RESPONSE_OK,
                         _("_Cancel"), GTK_RESPONSE_CANCEL, NULL));

    tv = GTK_TREE_VIEW(gtk_tree_view_new());
    sel = gtk_tree_view_get_selection(tv);
    gtk_tree_selection_set_mode(sel, GTK_SELECTION_BROWSE);
    g_signal_connect(tv, "row-activated", G_CALLBACK(row_activated_cb), dlg);
    tm = gnc_tree_model_budget_new(book);
    gnc_tree_view_budget_set_model(tv, tm);
    g_object_unref(tm);
    gtk_container_add(GTK_CONTAINER (gtk_dialog_get_content_area (dlg)), GTK_WIDGET(tv));
    gtk_widget_show_all(GTK_WIDGET(dlg));

    bgt = NULL;
    response = gtk_dialog_run(dlg);
    switch (response)
    {
    case GTK_RESPONSE_OK:
        ok = gtk_tree_selection_get_selected(sel, &tm, &iter);
        if (ok)
        {
            bgt = gnc_tree_model_budget_get_budget(tm, &iter);
        }
        break;
    default:
        break;
    }

    gtk_widget_destroy(GTK_WIDGET(dlg));
    return bgt;
}


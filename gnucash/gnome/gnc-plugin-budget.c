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
        G_CALLBACK(gnc_plugin_budget_cmd_new_budget)
    },

    {
        "OpenBudgetAction", NULL, N_("Open Budget"), NULL,
        N_("Open an existing Budget"),
        G_CALLBACK(gnc_plugin_budget_cmd_open_budget)
    },

    {
        "CopyBudgetAction", NULL, N_("Copy Budget"), NULL,
        N_("Copy an existing Budget"),
        G_CALLBACK(gnc_plugin_budget_cmd_copy_budget)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginBudgetPrivate
{
    gpointer dummy;
} GncPluginBudgetPrivate;

#define GNC_PLUGIN_BUDGET_GET_PRIVATE(o)  \
   ((GncPluginBudgetPrivate*)g_type_instance_get_private ((GTypeInstance*)o, GNC_TYPE_PLUGIN_BUDGET))

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
    return GNC_PLUGIN(plugin);
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginBudget, gnc_plugin_budget, GNC_TYPE_PLUGIN)

static void
gnc_plugin_budget_class_init (GncPluginBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS(klass);

    ENTER (" ");
    parent_class = g_type_class_peek_parent (klass);
    object_class->finalize = gnc_plugin_budget_finalize;

    plugin_class->plugin_name  = GNC_PLUGIN_BUDGET_NAME;
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

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
    (parent_class->finalize)(object);
    LEAVE(" ");

}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

/* Make a new budget; put it in a page; open the page. */
static void
gnc_plugin_budget_cmd_new_budget (GtkAction *action,
                                  GncMainWindowActionData *user_data)
{
    GncBudget *budget;
    GncPluginPage *page;
    gchar *description, *date;

    g_return_if_fail (user_data != NULL);

    budget = gnc_budget_new (gnc_get_current_book());
    page = gnc_plugin_page_budget_new (budget);

    date = gnc_print_time64 (gnc_time (NULL), qof_date_format_get_string (QOF_DATE_FORMAT_LOCALE));
    description = g_strdup_printf ("%s: %s",  _("Created"), date);
    gnc_budget_set_description (budget, description);
    g_free (description);
    g_free (date);

    gnc_main_window_open_page (user_data->window, page);
}

/* If only one budget exists, open it; otherwise user selects one to open */
static void
gnc_plugin_budget_cmd_open_budget (GtkAction *action,
                                   GncMainWindowActionData *user_data)
{
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;
    g_return_if_fail (user_data != NULL);

    book = gnc_get_current_book ();
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
            bgt = gnc_budget_get_default (book);
        else
            bgt = gnc_budget_gui_select_budget (GTK_WINDOW(user_data->window), book);

        if (bgt)
           gnc_main_window_open_page (user_data->window,
                                      gnc_plugin_page_budget_new (bgt));
    }
    else     /* if no budgets exist yet, just open a new budget */
        gnc_plugin_budget_cmd_new_budget (action, user_data);
}

/* If only one budget exists, create a copy of it; otherwise user selects one to copy */
static void
gnc_plugin_budget_cmd_copy_budget (GtkAction *action,
                                   GncMainWindowActionData *user_data)
{
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;
    g_return_if_fail (user_data != NULL);

    book = gnc_get_current_book ();
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
            bgt = gnc_budget_get_default(book);
        else
            bgt = gnc_budget_gui_select_budget (GTK_WINDOW(user_data->window), book);

        if (bgt)
        {
            GncBudget* copy;
            gchar* name;

            copy = gnc_budget_clone (bgt);
            name = g_strdup_printf ("Copy of %s", gnc_budget_get_name (bgt));
            gnc_budget_set_name (copy, name);
            g_free (name);

            gnc_main_window_open_page (user_data->window,
                                       gnc_plugin_page_budget_new (copy));
        }
    }
    else     /* if no budgets exist yet, just open a new budget */
        gnc_plugin_budget_cmd_new_budget (action, user_data);
}

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

static void
row_activated_cb (GtkTreeView *tv, GtkTreePath *path,
                  GtkTreeViewColumn *column, gpointer user_data)
{
    gtk_dialog_response (GTK_DIALOG(user_data), GTK_RESPONSE_OK);
}

GncBudget *
gnc_budget_gui_select_budget (GtkWindow *parent, QofBook *book)
{
    GncBudget *bgt;
    GtkDialog *dlg;
    GtkTreeView *tv;
    GtkTreeIter iter;
    GtkTreeSelection *sel;
    GtkTreeModel *tm;
    gint response;
    gboolean ok;

    dlg = GTK_DIALOG(gtk_dialog_new_with_buttons (
                         _("Select a Budget"), parent, GTK_DIALOG_MODAL,
                         _("_Cancel"), GTK_RESPONSE_CANCEL,
                         _("_OK"), GTK_RESPONSE_OK, NULL));

    tv = GTK_TREE_VIEW(gtk_tree_view_new ());
    sel = gtk_tree_view_get_selection (tv);
    gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);
    g_signal_connect (tv, "row-activated", G_CALLBACK(row_activated_cb), dlg);
    tm = gnc_tree_model_budget_new (book);
    gnc_tree_view_budget_set_model (tv, tm);
    g_object_unref (tm);
    gtk_container_add (GTK_CONTAINER(gtk_dialog_get_content_area (dlg)), GTK_WIDGET(tv));
    gtk_widget_show_all (GTK_WIDGET(dlg));

    // Preselect the default budget
    bgt = gnc_budget_get_default (book);

    if (bgt && gnc_tree_model_budget_get_iter_for_budget (tm, &iter, bgt))
        gtk_tree_view_set_cursor (tv, gtk_tree_model_get_path (tm, &iter), NULL, FALSE);

    bgt = NULL;
    response = gtk_dialog_run (dlg);
    switch (response)
    {
    case GTK_RESPONSE_OK:
        ok = gtk_tree_selection_get_selected (sel, &tm, &iter);
        if (ok)
            bgt = gnc_tree_model_budget_get_budget (tm, &iter);
        break;
    default:
        break;
    }

    gtk_widget_destroy (GTK_WIDGET(dlg));
    return bgt;
}


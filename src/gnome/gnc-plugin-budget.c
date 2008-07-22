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

#include "config.h"

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

/* Command Callbacks */
static void gnc_plugin_budget_cmd_new_budget (GtkAction *action,
					      GncMainWindowActionData *data);
static void gnc_plugin_budget_cmd_open_budget (GtkAction *action,
					      GncMainWindowActionData *data);

static GtkActionEntry gnc_plugin_actions [] = {
    { "NewBudgetAction", NULL, N_("New Budget"), NULL,
      N_("Create a new Budget"),
      G_CALLBACK (gnc_plugin_budget_cmd_new_budget) },

    { "OpenBudgetAction", NULL, N_("Open Budget"), NULL,
      N_("Open an existing Budget"),
      G_CALLBACK (gnc_plugin_budget_cmd_open_budget) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

typedef struct GncPluginBudgetPrivate {
    gpointer dummy;
} GncPluginBudgetPrivate;

#define GNC_PLUGIN_BUDGET_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_BUDGET, GncPluginBudgetPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_budget_get_type (void)
{
    static GType gnc_plugin_budget_type = 0;

    if (!gnc_plugin_budget_type) {
        static const GTypeInfo our_info = {
            sizeof (GncPluginBudgetClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_budget_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginBudget),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_budget_init
        };

        gnc_plugin_budget_type = g_type_register_static(
            GNC_TYPE_PLUGIN, "GncPluginBudget", &our_info, 0);
    }

    return gnc_plugin_budget_type;
}

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
gnc_plugin_budget_class_init (GncPluginBudgetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    ENTER (" ");
    parent_class = g_type_class_peek_parent (klass);
    object_class->finalize = gnc_plugin_budget_finalize;

    plugin_class->plugin_name  = GNC_PLUGIN_BUDGET_NAME;
    plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
    plugin_class->actions      = gnc_plugin_actions;
    plugin_class->n_actions    = gnc_plugin_n_actions;
    plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

    g_type_class_add_private(klass, sizeof(GncPluginBudgetPrivate));
    LEAVE (" ");
}

static void
gnc_plugin_budget_init(GncPluginBudget *plugin)
{
}

static void
gnc_plugin_budget_finalize(GObject *object)
{
    GncPluginBudget *plugin;
    GncPluginBudgetPrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_BUDGET (object));

    ENTER(" ");
    plugin = GNC_PLUGIN_BUDGET(object);
    priv = GNC_PLUGIN_BUDGET_GET_PRIVATE(plugin);

    (parent_class->finalize)(object);
    LEAVE(" ");

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
    if (count > 0) {
        if (count == 1) {
            bgt = gnc_budget_get_default(book);
        } else {
            bgt = gnc_budget_gui_select_budget(book);
        }

        if (bgt) gnc_main_window_open_page(
            data->window, gnc_plugin_page_budget_new(bgt));
    } else { /* if no budgets exist yet, just open a new budget */
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
gnc_budget_gui_select_budget(QofBook *book)
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
                         _("Select a Budget"), NULL, GTK_DIALOG_MODAL,
                         GTK_STOCK_OK, GTK_RESPONSE_OK,
                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL));

    tv = GTK_TREE_VIEW(gtk_tree_view_new());
    sel = gtk_tree_view_get_selection(tv);
    gtk_tree_selection_set_mode(sel, GTK_SELECTION_BROWSE);
    g_signal_connect(tv, "row-activated", G_CALLBACK(row_activated_cb), dlg);
    tm = gnc_tree_model_budget_new(book);
    gnc_tree_view_budget_set_model(tv, tm);
    g_object_unref(tm);
    gtk_container_add(GTK_CONTAINER(dlg->vbox), GTK_WIDGET(tv));
    gtk_widget_show_all(GTK_WIDGET(dlg));

    bgt = NULL;
    response = gtk_dialog_run(dlg);
    switch (response) {
    case GTK_RESPONSE_OK:
        ok = gtk_tree_selection_get_selected(sel, &tm, &iter);
        if (ok) {
            bgt = gnc_tree_model_budget_get_budget(tm, &iter);
        }
        break;
    default:
        break;
    }

    gtk_widget_destroy(GTK_WIDGET(dlg));
    return bgt;
}


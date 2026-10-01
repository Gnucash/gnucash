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
#include "gnc-features.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "dialog-utils.h"

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
 *                    Command Callbacks                     *
 ************************************************************/

/* Make a new budget; put it in a page; open the page. */
static void
gnc_plugin_budget_cmd_new_budget (GSimpleAction *simple,
                                  GVariant      *parameter,
                                  gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    GncBudget *budget;
    GncPluginPage *page;
    gchar *description, *date;
    QofBook *book = gnc_get_current_book();

    g_return_if_fail (data != NULL);

    if (!gnc_features_check_used (book, GNC_FEATURE_BUDGET_UNREVERSED))
    {
        gnc_features_set_used (book, GNC_FEATURE_BUDGET_UNREVERSED);
        PWARN ("Setting feature BUDGET_UNREVERSED. This book now requires \
GnuCash 3.8 or later.");
    }

    budget = gnc_budget_new (gnc_get_current_book());
    page = gnc_plugin_page_budget_new (budget);

    date = gnc_print_time64 (gnc_time (NULL), qof_date_format_get_string (QOF_DATE_FORMAT_LOCALE));
    description = g_strdup_printf ("%s: %s",  _("Created"), date);
    gnc_budget_set_description (budget, description);
    g_free (description);
    g_free (date);

    gnc_main_window_open_page (data->window, page);
}

/* If only one budget exists, open it; otherwise user selects one to open */
static void
open_selected_budget (GtkWindow *parent, GncBudget *budget,
                       [[maybe_unused]] gpointer user_data)
{
    if (parent && budget)
        gnc_main_window_open_page (GNC_MAIN_WINDOW (parent),
                                   gnc_plugin_page_budget_new (budget));
}

static void
copy_selected_budget (GtkWindow *parent, GncBudget *budget,
                       [[maybe_unused]] gpointer user_data)
{
    if (parent && budget && !qof_book_is_readonly (gnc_get_current_book ()))
    {
        gchar *name = g_strdup_printf ("Copy of %s", gnc_budget_get_name (budget));
        GncBudget *copy = gnc_budget_clone (budget);
        gnc_budget_set_name (copy, name);
        g_free (name);
        gnc_main_window_open_page (GNC_MAIN_WINDOW (parent),
                                   gnc_plugin_page_budget_new (copy));
    }
}

static void
delete_selected_budget ([[maybe_unused]] GtkWindow *parent, GncBudget *budget,
                         [[maybe_unused]] gpointer user_data)
{
    if (budget && !qof_book_is_readonly (gnc_get_current_book ()))
        gnc_budget_gui_delete_budget (budget);
}

static void
gnc_plugin_budget_cmd_open_budget (GSimpleAction *simple,
                                   GVariant      *parameter,
                                   gpointer       user_data)
{
    GncMainWindowActionData *data = user_data;
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;

    g_return_if_fail (data != NULL);

    book = gnc_get_current_book ();
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
            bgt = gnc_budget_get_default (book);
        else
        {
            gnc_budget_gui_select_budget_async (GTK_WINDOW(data->window), book,
                                                open_selected_budget, NULL);
            return;
        }

        if (bgt)
           gnc_main_window_open_page (data->window,
                                      gnc_plugin_page_budget_new (bgt));
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
    guint count;
    QofBook *book;
    GncBudget *bgt = NULL;
    QofCollection *col;

    g_return_if_fail (data != NULL);

    book = gnc_get_current_book ();
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    count = qof_collection_count (col);
    if (count > 0)
    {
        if (count == 1)
            bgt = gnc_budget_get_default(book);
        else
        {
            gnc_budget_gui_select_budget_async (GTK_WINDOW(data->window), book,
                                                copy_selected_budget, NULL);
            return;
        }

        if (bgt)
        {
            GncBudget* copy;
            gchar* name;

            copy = gnc_budget_clone (bgt);
            name = g_strdup_printf ("Copy of %s", gnc_budget_get_name (bgt));
            gnc_budget_set_name (copy, name);
            g_free (name);

            gnc_main_window_open_page (data->window,
                                       gnc_plugin_page_budget_new (copy));
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
    QofBook *book;

    g_return_if_fail (data != NULL);

    book = gnc_get_current_book ();
    if (qof_collection_count (qof_book_get_collection (book, GNC_ID_BUDGET)) == 0)
        return;

    gnc_budget_gui_select_budget_async (GTK_WINDOW(data->window), book,
                                        delete_selected_budget, NULL);
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

static GtkDialog *
budget_selection_dialog_new (GtkWindow *parent, QofBook *book, GtkTreeView **view)
{
    GncBudget *bgt;
    GtkDialog *dlg;
    GtkTreeView *tv;
    GtkTreeIter iter;
    GtkTreeSelection *sel;
    GtkTreeModel *tm;

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

    // Preselect the default budget
    bgt = gnc_budget_get_default (book);

    if (bgt && gnc_tree_model_budget_get_iter_for_budget (tm, &iter, bgt))
    {
        GtkTreePath *path = gtk_tree_model_get_path (tm, &iter);
        gtk_tree_view_set_cursor (tv, path, NULL, FALSE);
        gtk_tree_path_free (path);
    }

    *view = tv;
    return dlg;
}

typedef struct
{
    GtkDialog *dialog;
    GtkTreeView *view;
    GWeakRef book;
    GncGUID selected;
    gboolean has_selection;
    gulong response_id;
    GncBudgetSelectionCallback callback;
    gpointer user_data;
} BudgetSelectionRequest;

static QofBook *
budget_selection_current_book (BudgetSelectionRequest *request)
{
    QofBook *book = g_weak_ref_get (&request->book);
    if (book && (!gnc_current_session_exist () ||
                 book != gnc_get_current_book () || !qof_book_is_open (book) ||
                 qof_book_shutting_down (book)))
        g_clear_object (&book);
    return book;
}

static void
budget_selection_capture (GtkDialog *dialog, gint response,
                           BudgetSelectionRequest *request)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    QofBook *book;
    g_signal_handler_disconnect (dialog, request->response_id);
    request->response_id = 0;
    book = budget_selection_current_book (request);
    if (response == GTK_RESPONSE_OK && book &&
        gtk_tree_selection_get_selected (gtk_tree_view_get_selection (request->view),
                                          &model, &iter))
    {
        GncBudget *budget = gnc_tree_model_budget_get_budget (model, &iter);
        if (budget && qof_instance_get_book (QOF_INSTANCE (budget)) == book)
        {
            request->selected = *gnc_budget_get_guid (budget);
            request->has_selection = TRUE;
        }
    }
    g_clear_object (&book);
}

static void
budget_selection_complete (GtkWindow *parent, gint response, gpointer user_data)
{
    BudgetSelectionRequest *request = user_data;
    QofBook *book;
    GncBudget *budget;
    GncBudgetSelectionCallback callback = request->callback;
    gpointer callback_data = request->user_data;
    if (request->response_id)
        g_signal_handler_disconnect (request->dialog, request->response_id);
    g_clear_object (&request->view);
    g_clear_object (&request->dialog);
    /* Finalizing the model can emit notifications. Resolve the identity only
     * after all UI references have been released and revalidate the session. */
    book = budget_selection_current_book (request);
    budget = response == GTK_RESPONSE_OK && book && request->has_selection
        ? gnc_budget_lookup (&request->selected, book) : NULL;
    g_weak_ref_clear (&request->book);
    g_free (request);
    g_clear_object (&book);
    callback (parent, budget, callback_data);
}

void
gnc_budget_gui_select_budget_async (GtkWindow *parent, QofBook *book,
                                    GncBudgetSelectionCallback callback,
                                    gpointer user_data)
{
    BudgetSelectionRequest *request;
    g_return_if_fail (book != NULL && callback != NULL);
    request = g_new0 (BudgetSelectionRequest, 1);
    request->dialog = budget_selection_dialog_new (parent, book, &request->view);
    g_object_ref (request->dialog);
    g_object_ref (request->view);
    g_weak_ref_init (&request->book, book);
    request->callback = callback;
    request->user_data = user_data;
    request->response_id = g_signal_connect (request->dialog, "response",
                                             G_CALLBACK (budget_selection_capture), request);
    gnc_dialog_run_async (request->dialog, NULL, budget_selection_complete, request);
}

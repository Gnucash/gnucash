/*
 * dialog-ab-select-imexporter.c --
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

/**
 * @internal
 * @file dialog-ab-select-imexporter.h
 * @brief Dialog to select AQBanking importer/exporter and format profile.
 * @author  Copyright (C) 2022 John Ralls <jralls@ceridwen.us>
 */

#include <config.h>

#include <stdbool.h>
#include <glib/gi18n.h>
#include "dialog-ab-select-imexporter.h"
#include <dialog-utils.h>

__attribute__((unused)) static QofLogModule log_module = G_LOG_DOMAIN;

struct _GncABSelectImExDlg
{
    GtkWidget *dialog;
    GtkWidget *parent;
    gulong parent_destroy_handler;
    GtkListStore *imexporter_list;
    GtkListStore *profile_list;
    GtkWidget *select_imexporter;
    GtkWidget *select_profile;
    GtkWidget *ok_button;
    GtkTreeSelection *imex_selection;
    GtkTreeSelection *profile_selection;
    gchar *selected_imexporter;
    gchar *selected_profile;

    AB_BANKING* abi;
};

typedef struct
{
    GncABSelectImExDlg *dialog;
    GtkWidget *widget;
    gulong capture_handler;
    GncABSelectImExCallback completed;
    gpointer user_data;
} GncABSelectImExRunRequest;

static char *tree_view_get_name (GtkTreeView *tv);
static void gnc_ab_select_imex_capture_response (GtkDialog *dialog,
                                                  gint response,
                                                  gpointer user_data);
static void gnc_ab_select_imex_completed (GtkWindow *parent, gint response,
                                          gpointer user_data);

// Expose the selection handlers to GtkBuilder.
static gboolean imexporter_changed(GtkTreeSelection* sel,
                                   gpointer data);
static gboolean profile_changed(GtkTreeSelection* sel, gpointer data);

enum
{
    NAME_COL,
    PROF_COL
};

static void
populate_list_store (GtkListStore* model, GList* entries)
{
    gtk_list_store_clear (model);
    for (GList* node = entries; node; node = g_list_next (node))
    {
        AB_Node_Pair *pair = (AB_Node_Pair*)(node->data);
        GtkTreeIter iter;
        gtk_list_store_insert_with_values (GTK_LIST_STORE (model),
                                           &iter, -1,
                                           NAME_COL, pair->name,
                                           PROF_COL, pair->descr,
                                           -1);
        g_slice_free1 (sizeof(AB_Node_Pair), pair);
    }
}

GncABSelectImExDlg*
gnc_ab_select_imex_dlg_new (GtkWidget* parent, AB_BANKING* abi)
{
    GncABSelectImExDlg* imexd;
    GtkBuilder* builder;
    GList* imexporters;
    GtkTreeSelection *imex_select = NULL, *prof_select = NULL;

    g_return_val_if_fail (abi, NULL);
    imexporters = gnc_ab_imexporter_list (abi);
    g_return_val_if_fail (imexporters, NULL);
    imexd = g_new0(GncABSelectImExDlg, 1);
    imexd->parent = parent;
    imexd->abi = abi;

    imexd->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (gtk_widget_destroyed), &imexd->parent);
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "imexporter-list");
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "profile-list");
    gnc_builder_add_from_file (builder, "dialog-ab.glade",
                               "aqbanking-select-imexporter-dialog");
    imexd->dialog =
        GTK_WIDGET (gtk_builder_get_object (builder,
                                            "aqbanking-select-imexporter-dialog"));
    g_signal_connect (imexd->dialog, "destroy",
                      G_CALLBACK (gtk_widget_destroyed), &imexd->dialog);
    imexd->imexporter_list = g_object_ref (GTK_LIST_STORE (
        gtk_builder_get_object (builder, "imexporter-list")));
    imexd->profile_list = g_object_ref (GTK_LIST_STORE (
        gtk_builder_get_object (builder, "profile-list")));
    imexd->select_imexporter =
        GTK_WIDGET (gtk_builder_get_object (builder, "imexporter-sel"));
    imexd->select_profile =
        GTK_WIDGET (gtk_builder_get_object (builder, "profile-sel"));
    imexd->ok_button =
        GTK_WIDGET (gtk_builder_get_object (builder, "imex-okbutton"));

    imex_select = GTK_TREE_SELECTION (gtk_builder_get_object (builder, "imex-selection"));
    prof_select = GTK_TREE_SELECTION (gtk_builder_get_object (builder, "prof-selection"));
    imexd->imex_selection = g_object_ref (imex_select);
    imexd->profile_selection = g_object_ref (prof_select);
    populate_list_store (imexd->imexporter_list,
                         imexporters);

    g_signal_connect (imex_select, "changed", G_CALLBACK(imexporter_changed),
                      imexd);
    g_signal_connect (prof_select, "changed", G_CALLBACK(profile_changed),
                      imexd);
    g_list_free (imexporters);
    g_object_unref (G_OBJECT (builder));

    gtk_window_set_transient_for (GTK_WINDOW (imexd->dialog),
                                  GTK_WINDOW (imexd->parent));
    gtk_window_set_destroy_with_parent (GTK_WINDOW (imexd->dialog), TRUE);

    return imexd;
}

static void
gnc_ab_select_imex_disconnect_widget_callbacks (GtkWidget *widget,
                                               gpointer data)
{
    g_signal_handlers_disconnect_by_data (widget, data);
    if (!GTK_IS_CONTAINER (widget))
        return;
    GList *children = gtk_container_get_children (GTK_CONTAINER (widget));
    for (GList *node = children; node; node = node->next)
        gnc_ab_select_imex_disconnect_widget_callbacks (GTK_WIDGET (node->data),
                                                        data);
    g_list_free (children);
}

void
gnc_ab_select_imex_dlg_destroy (GncABSelectImExDlg* imexd)
{
    if (imexd->imex_selection)
        g_signal_handlers_disconnect_by_data (imexd->imex_selection, imexd);
    if (imexd->profile_selection)
        g_signal_handlers_disconnect_by_data (imexd->profile_selection, imexd);
    if (imexd->dialog)
        gnc_ab_select_imex_disconnect_widget_callbacks (imexd->dialog, imexd);
    if (imexd->parent && imexd->parent_destroy_handler &&
        g_signal_handler_is_connected (imexd->parent,
                                       imexd->parent_destroy_handler))
        g_signal_handler_disconnect (imexd->parent,
                                     imexd->parent_destroy_handler);

    if (imexd->imexporter_list)
    {
        gtk_list_store_clear (imexd->imexporter_list);
        g_clear_object (&imexd->imexporter_list);
    }

    if (imexd->profile_list)
    {
        gtk_list_store_clear (imexd->profile_list);
        g_clear_object (&imexd->profile_list);
    }

    g_clear_object (&imexd->imex_selection);
    g_clear_object (&imexd->profile_selection);

    if (imexd->dialog)
        gtk_widget_destroy (imexd->dialog);

    g_free (imexd->selected_imexporter);
    g_free (imexd->selected_profile);
    g_free (imexd);
}

gboolean
imexporter_changed(GtkTreeSelection* sel, gpointer data)
{
    GncABSelectImExDlg* imexd = (GncABSelectImExDlg*)data;
    GtkTreeIter iter;
    GtkTreeModel* model;

    gtk_widget_set_sensitive (imexd->ok_button, FALSE);

    if (gtk_tree_selection_get_selected (sel, &model, &iter))
    {
        char* name = NULL;
        GList* profiles = NULL;

        gtk_tree_model_get (model, &iter, NAME_COL, &name, -1);
        if (name && *name)
            profiles = gnc_ab_imexporter_profile_list (imexd->abi, name);

        g_free (name);
        gtk_list_store_clear (imexd->profile_list);

        if (profiles)
        {
             populate_list_store (imexd->profile_list, profiles);
        }
        else
        {
            gtk_widget_set_sensitive (imexd->ok_button, TRUE);
            return FALSE;
        }

        if (!profiles->next)
        {
            GtkTreePath* path = gtk_tree_path_new_first();
            GtkTreeSelection* profile_sel =
                gtk_tree_view_get_selection (GTK_TREE_VIEW (imexd->select_profile));
            gtk_tree_selection_select_path (profile_sel, path); //should call profile_changed
            gtk_tree_path_free (path);
        }
        return FALSE;
    }
    return TRUE;
}

gboolean
profile_changed (GtkTreeSelection* sel, gpointer data)
{
    GncABSelectImExDlg* imexd = (GncABSelectImExDlg*)data;
    GtkTreeIter iter;
    GtkTreeModel* model;

    gtk_widget_set_sensitive (imexd->ok_button, FALSE);

    if (gtk_tree_selection_get_selected (sel, &model, &iter))
    {
        gtk_widget_set_sensitive (imexd->ok_button, TRUE);
        return FALSE;
    }

    return TRUE;
}

void
gnc_ab_select_imex_dlg_run_async (GncABSelectImExDlg *imexd,
                                  GncABSelectImExCallback completed,
                                  gpointer user_data)
{
    g_return_if_fail (imexd && imexd->dialog && completed);
    GncABSelectImExRunRequest *request = g_new0 (GncABSelectImExRunRequest, 1);
    request->dialog = imexd;
    request->completed = completed;
    request->user_data = user_data;
    g_clear_pointer (&imexd->selected_imexporter, g_free);
    g_clear_pointer (&imexd->selected_profile, g_free);
    request->widget = g_object_ref (imexd->dialog);
    request->capture_handler = g_signal_connect (imexd->dialog, "response",
        G_CALLBACK (gnc_ab_select_imex_capture_response), imexd);
    gnc_dialog_run_async (GTK_DIALOG (imexd->dialog), NULL,
                          gnc_ab_select_imex_completed, request);
    return;
}

static void
gnc_ab_select_imex_capture_response (GtkDialog *dialog, gint response,
                                    gpointer user_data)
{
    GncABSelectImExDlg *imexd = user_data;
    if (response != GTK_RESPONSE_OK)
        return;
    g_free (imexd->selected_imexporter);
    g_free (imexd->selected_profile);
    imexd->selected_imexporter = tree_view_get_name (
        GTK_TREE_VIEW (imexd->select_imexporter));
    imexd->selected_profile = tree_view_get_name (
        GTK_TREE_VIEW (imexd->select_profile));
}

static void
gnc_ab_select_imex_completed (GtkWindow *parent, gint response,
                              gpointer user_data)
{
    GncABSelectImExRunRequest *request = user_data;
    GncABSelectImExDlg *imexd = request->dialog;
    if (request->capture_handler &&
        g_signal_handler_is_connected (request->widget,
                                       request->capture_handler))
        g_signal_handler_disconnect (request->widget, request->capture_handler);
    if (imexd->imex_selection)
        g_signal_handlers_disconnect_by_data (imexd->imex_selection, imexd);
    if (imexd->profile_selection)
        g_signal_handlers_disconnect_by_data (imexd->profile_selection, imexd);
    gnc_ab_select_imex_disconnect_widget_callbacks (request->widget, imexd);
    g_object_unref (request->widget);
    request->widget = NULL;
    gboolean accepted = parent && imexd->parent &&
                        !gtk_widget_in_destruction (imexd->parent) &&
                        response == GTK_RESPONSE_OK &&
                        imexd->selected_imexporter && imexd->selected_profile;
    request->completed (accepted,
                        accepted ? imexd->selected_imexporter : NULL,
                        accepted ? imexd->selected_profile : NULL,
                        request->user_data);
    g_free (request);
}

static char*
tree_view_get_name (GtkTreeView *tv)
{
    GtkTreeSelection* sel = gtk_tree_view_get_selection (tv);
    GtkTreeIter iter;
    GtkTreeModel* model;
    if (sel && gtk_tree_selection_get_selected (sel, &model, &iter))
    {
        char* name;
        gtk_tree_model_get(model, &iter, NAME_COL, &name, -1);
        return name;
    }

    return NULL;
}

static void
tree_view_set_name (GtkTreeView *tree, const char* name)
{
    GtkTreeIter iter;
    GtkTreeModel* model = gtk_tree_view_get_model(tree);
    bool found = false;

    if (!gtk_tree_model_get_iter_first(model, &iter))
        return;
    do
    {
        char* row_name;
        gtk_tree_model_get(model, &iter, NAME_COL, &row_name, -1);
        if (!g_strcmp0(name, row_name))
        {
            found = true;
            break;
        }
    }
    while(gtk_tree_model_iter_next(model, &iter));

    if (found)
    {
        GtkTreeSelection *sel = gtk_tree_view_get_selection(tree);
        gtk_tree_selection_select_iter(sel, &iter);
    }
}

char*
gnc_ab_select_imex_dlg_get_imexporter_name (GncABSelectImExDlg* imexd)
{
    return tree_view_get_name (GTK_TREE_VIEW (imexd->select_imexporter));
}

char*
gnc_ab_select_imex_dlg_get_profile_name (GncABSelectImExDlg* imexd)
{
    return tree_view_get_name (GTK_TREE_VIEW (imexd->select_profile));
}

void
gnc_ab_select_imex_dlg_set_imexporter_name (GncABSelectImExDlg* imexd, const char* name)
{
    if (name)
        tree_view_set_name (GTK_TREE_VIEW (imexd->select_imexporter), name);
}

void
gnc_ab_select_imex_dlg_set_profile_name (GncABSelectImExDlg* imexd, const char* name)
{
    if (name)
        tree_view_set_name (GTK_TREE_VIEW (imexd->select_profile), name);
}

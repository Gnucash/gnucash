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
    GtkListStore *imexporter_list;
    GtkListStore *profile_list;
    GtkWidget *select_imexporter;
    GtkWidget *select_profile;
    GtkWidget *ok_button;

    AB_BANKING* abi;
};

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

    g_signal_connect (parent, "destroy",
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
    imexd->imexporter_list =
        GTK_LIST_STORE (gtk_builder_get_object (builder, "imexporter-list"));
    imexd->profile_list =
        GTK_LIST_STORE (gtk_builder_get_object (builder, "profile-list"));
    imexd->select_imexporter =
        GTK_WIDGET (gtk_builder_get_object (builder, "imexporter-sel"));
    imexd->select_profile =
        GTK_WIDGET (gtk_builder_get_object (builder, "profile-sel"));
    imexd->ok_button =
        GTK_WIDGET (gtk_builder_get_object (builder, "imex-okbutton"));

    imex_select = GTK_TREE_SELECTION (gtk_builder_get_object (builder, "imex-selection"));
    prof_select = GTK_TREE_SELECTION (gtk_builder_get_object (builder, "prof-selection"));
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

    return imexd;
}

void
gnc_ab_select_imex_dlg_destroy (GncABSelectImExDlg* imexd)
{

    if (imexd->imexporter_list)
        gtk_list_store_clear (imexd->imexporter_list);

    if (imexd->profile_list)
        gtk_list_store_clear (imexd->profile_list);

    if (imexd->dialog)
        gtk_widget_destroy (imexd->dialog);

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

gboolean
gnc_ab_select_imex_dlg_run (GncABSelectImExDlg* imexd)
{

    int response = gtk_dialog_run (GTK_DIALOG (imexd->dialog));
    return response == GTK_RESPONSE_OK ? TRUE : FALSE;
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

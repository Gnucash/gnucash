/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

/*
 * test-import-selection-model.c -- Tests for import selection invariants.
 */

#include <config.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "import-selection-model.h"

typedef struct
{
    guint selected_changes;
} SelectionChanges;

static GtkStringObject *
append_string (GListStore *store, const gchar *value)
{
    GtkStringObject *row = gtk_string_object_new (value);

    g_list_store_append (store, row);
    return row;
}

static void
selection_changed_cb (GtkSelectionModel *selection, guint position,
                      guint n_items, SelectionChanges *changes)
{
    GObject *selected = gtk_single_selection_get_selected_item (
        GTK_SINGLE_SELECTION (selection));

    if (selected)
        changes->selected_changes++;
    (void)position;
    (void)n_items;
}

static void
test_prefilled_model_starts_unselected (void)
{
    GListStore *store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    GtkStringObject *first = append_string (store, "first");
    GtkStringObject *second = append_string (store, "second");
    GtkSingleSelection *selection = gnc_import_single_selection_new (
        G_LIST_MODEL (store));

    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);
    g_assert_cmpuint (
        gnc_import_single_selection_get_insert_after_position (selection), ==, 2);

    gtk_single_selection_set_selected (selection, 0);
    g_assert_true (gtk_single_selection_get_selected_item (selection) ==
                   G_OBJECT (first));
    g_assert_cmpuint (
        gnc_import_single_selection_get_insert_after_position (selection), ==, 1);

    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);

    g_object_unref (selection);
    g_object_unref (first);
    g_object_unref (second);
    g_object_unref (store);
}

static void
test_model_rebuild_does_not_select (void)
{
    GListStore *store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    GtkSingleSelection *selection = gnc_import_single_selection_new (
        G_LIST_MODEL (store));
    SelectionChanges changes = { 0 };
    GtkStringObject *first;
    GtkStringObject *replacement;

    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (selection_changed_cb), &changes);
    first = append_string (store, "first");
    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);
    g_assert_cmpuint (changes.selected_changes, ==, 0);

    gtk_single_selection_set_selected (selection, 0);
    g_assert_true (gtk_single_selection_get_selected_item (selection) ==
                   G_OBJECT (first));

    g_list_store_remove_all (store);
    changes.selected_changes = 0;
    replacement = append_string (store, "replacement");
    g_assert_cmpuint (gtk_single_selection_get_selected (selection), ==,
                      GTK_INVALID_LIST_POSITION);
    g_assert_cmpuint (changes.selected_changes, ==, 0);

    gtk_single_selection_set_selected (selection, 0);
    g_assert_true (gtk_single_selection_get_selected_item (selection) ==
                   G_OBJECT (replacement));

    g_object_unref (selection);
    g_object_unref (first);
    g_object_unref (replacement);
    g_object_unref (store);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/import-export/selection/prefilled-model-starts-unselected",
                     test_prefilled_model_starts_unselected);
    g_test_add_func ("/import-export/selection/model-rebuild-does-not-select",
                     test_model_rebuild_does_not_select);
    return g_test_run ();
}

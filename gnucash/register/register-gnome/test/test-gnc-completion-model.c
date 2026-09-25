/********************************************************************
 * test-gnc-completion-model.c -- tests for GTK4 completion model   *
 *                                                                  *
 * Copyright (C) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <glib.h>

#include "gnc-completion-model.h"

static GncSuggestionItem *
get_suggestion (GListModel *model, guint position)
{
    return g_list_model_get_item (model, position);
}

static void
test_sentinel_and_markup (void)
{
    GncCompletionModel *completion = gnc_completion_model_new ();
    GListModel *suggestions;
    GncSuggestionItem *item;

    gnc_completion_model_add_menu_item (completion, "Café");
    suggestions = gnc_completion_model_build_suggestions (completion, "cafe",
                                                           "Don't autocomplete",
                                                           TRUE);
    g_assert_cmpuint (g_list_model_get_n_items (suggestions), ==, 2);

    item = get_suggestion (suggestions, 0);
    g_assert_true (gnc_suggestion_item_is_sentinel (item));
    g_assert_cmpstr (gnc_suggestion_item_get_text (item), ==,
                     "Don't autocomplete");
    g_object_unref (item);

    item = get_suggestion (suggestions, 1);
    g_assert_false (gnc_suggestion_item_is_sentinel (item));
    g_assert_cmpstr (gnc_suggestion_item_get_text (item), ==, "Café");
    g_assert_cmpint (gnc_suggestion_item_get_found_location (item), ==, 0);
    g_assert_true (g_str_has_prefix (gnc_suggestion_item_get_markup (item),
                                     "<b>Café</b>"));
    g_object_unref (item);

    g_object_unref (suggestions);
    g_object_unref (completion);
}

static void
test_recent_items_sort_first (void)
{
    GncCompletionModel *completion = gnc_completion_model_new ();
    GListModel *suggestions;
    GncSuggestionItem *item;

    gnc_completion_model_add_menu_item (completion, "Cash");
    gnc_completion_model_add_menu_item (completion, "Card");
    suggestions = gnc_completion_model_build_suggestions (completion, "ca",
                                                           NULL, TRUE);
    g_assert_cmpuint (g_list_model_get_n_items (suggestions), ==, 2);

    item = get_suggestion (suggestions, 0);
    g_assert_cmpstr (gnc_suggestion_item_get_text (item), ==, "Card");
    g_assert_cmpint (gnc_suggestion_item_get_weight (item), ==, 1);
    g_object_unref (item);

    item = get_suggestion (suggestions, 1);
    g_assert_cmpstr (gnc_suggestion_item_get_text (item), ==, "Cash");
    g_assert_cmpint (gnc_suggestion_item_get_weight (item), ==, 2);
    g_object_unref (item);

    g_object_unref (suggestions);
    g_object_unref (completion);
}

static void
test_reverse_reset (void)
{
    GncCompletionModel *completion = gnc_completion_model_new ();

    gnc_completion_model_add_menu_item (completion, "First");
    g_assert_cmpuint (gnc_completion_model_get_menu_size (completion), ==, 1);

    gnc_completion_model_set_reversed (completion, TRUE);
    g_assert_cmpuint (gnc_completion_model_get_menu_size (completion), ==, 0);

    GListModel *suggestions;
    GncSuggestionItem *item;

    gnc_completion_model_add_menu_item (completion, "Last");
    gnc_completion_model_add_menu_item (completion, "Last");
    g_assert_cmpuint (gnc_completion_model_get_menu_size (completion), ==, 1);

    suggestions = gnc_completion_model_build_suggestions (completion, "la",
                                                           NULL, TRUE);
    item = get_suggestion (suggestions, 0);
    g_assert_cmpint (gnc_suggestion_item_get_weight (item), ==, 2);
    g_object_unref (item);
    g_object_unref (suggestions);
    g_object_unref (completion);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/completion-model/sentinel-and-markup",
                     test_sentinel_and_markup);
    g_test_add_func ("/completion-model/recent-items-sort-first",
                     test_recent_items_sort_first);
    g_test_add_func ("/completion-model/reverse-reset",
                     test_reverse_reset);
    return g_test_run ();
}
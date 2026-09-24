/********************************************************************
 * gnc-completion-model.c -- GTK4 completion data model             *
 *                                                                  *
 * Copyright (C) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <glib/gi18n.h>

#include "gnc-completion-model.h"
#include "gnc-string-utils.h"
#include <gnc-unicode.h>

struct _GncSuggestionItem
{
    GObject parent_instance;
    gchar *text;
    gchar *markup;
    gint weight;
    gint found_location;
    gboolean sentinel;
    guint sequence;
};

G_DEFINE_FINAL_TYPE (GncSuggestionItem, gnc_suggestion_item, G_TYPE_OBJECT)

static void
gnc_suggestion_item_finalize (GObject *object)
{
    GncSuggestionItem *item = GNC_SUGGESTION_ITEM (object);

    g_free (item->text);
    g_free (item->markup);
    G_OBJECT_CLASS (gnc_suggestion_item_parent_class)->finalize (object);
}

static void
gnc_suggestion_item_class_init (GncSuggestionItemClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_suggestion_item_finalize;
}

static void
gnc_suggestion_item_init (GncSuggestionItem *item)
{
    (void)item;
}

static GncSuggestionItem *
suggestion_item_new_with_sequence (const gchar *text,
                                   const gchar *markup,
                                   gint weight,
                                   gint found_location,
                                   gboolean sentinel,
                                   guint sequence)
{
    GncSuggestionItem *item;

    g_return_val_if_fail (text != NULL, NULL);
    g_return_val_if_fail (markup != NULL, NULL);

    item = g_object_new (GNC_TYPE_SUGGESTION_ITEM, NULL);
    item->text = g_strdup (text);
    item->markup = g_strdup (markup);
    item->weight = weight;
    item->found_location = found_location;
    item->sentinel = sentinel;
    item->sequence = sequence;
    return item;
}

GncSuggestionItem *
gnc_suggestion_item_new (const gchar *text,
                         const gchar *markup,
                         gint weight,
                         gint found_location,
                         gboolean sentinel)
{
    return suggestion_item_new_with_sequence (text, markup, weight,
                                              found_location, sentinel, 0);
}

const gchar *
gnc_suggestion_item_get_text (GncSuggestionItem *item)
{
    g_return_val_if_fail (GNC_IS_SUGGESTION_ITEM (item), NULL);
    return item->text;
}

const gchar *
gnc_suggestion_item_get_markup (GncSuggestionItem *item)
{
    g_return_val_if_fail (GNC_IS_SUGGESTION_ITEM (item), NULL);
    return item->markup;
}

gint
gnc_suggestion_item_get_weight (GncSuggestionItem *item)
{
    g_return_val_if_fail (GNC_IS_SUGGESTION_ITEM (item), 0);
    return item->weight;
}

gint
gnc_suggestion_item_get_found_location (GncSuggestionItem *item)
{
    g_return_val_if_fail (GNC_IS_SUGGESTION_ITEM (item), 0);
    return item->found_location;
}

gboolean
gnc_suggestion_item_is_sentinel (GncSuggestionItem *item)
{
    g_return_val_if_fail (GNC_IS_SUGGESTION_ITEM (item), FALSE);
    return item->sentinel;
}

struct _GncCompletionModel
{
    GObject parent_instance;
    GHashTable *items;
    guint occurrence;
    gboolean reversed;
};

G_DEFINE_FINAL_TYPE (GncCompletionModel, gnc_completion_model, G_TYPE_OBJECT)

static void
gnc_completion_model_finalize (GObject *object)
{
    GncCompletionModel *model = GNC_COMPLETION_MODEL (object);

    g_hash_table_destroy (model->items);
    G_OBJECT_CLASS (gnc_completion_model_parent_class)->finalize (object);
}

static void
gnc_completion_model_class_init (GncCompletionModelClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_completion_model_finalize;
}

static void
gnc_completion_model_init (GncCompletionModel *model)
{
    model->items = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
}

GncCompletionModel *
gnc_completion_model_new (void)
{
    return g_object_new (GNC_TYPE_COMPLETION_MODEL, NULL);
}

void
gnc_completion_model_clear (GncCompletionModel *model)
{
    g_return_if_fail (GNC_IS_COMPLETION_MODEL (model));

    g_hash_table_remove_all (model->items);
    model->occurrence = 0;
}

void
gnc_completion_model_add_menu_item (GncCompletionModel *model,
                                    const gchar *text)
{
    gpointer existing_item;
    gboolean update;

    g_return_if_fail (GNC_IS_COMPLETION_MODEL (model));
    g_return_if_fail (text != NULL);

    existing_item = g_hash_table_lookup (model->items, text);
    update = existing_item == NULL || !model->reversed;
    if (update)
        g_hash_table_insert (model->items, g_strdup (text),
                             GINT_TO_POINTER (model->occurrence));
    model->occurrence++;
}

void
gnc_completion_model_set_reversed (GncCompletionModel *model,
                                   gboolean reversed)
{
    g_return_if_fail (GNC_IS_COMPLETION_MODEL (model));

    reversed = !!reversed;
    if (model->reversed == reversed)
        return;

    gnc_completion_model_clear (model);
    model->reversed = reversed;
}

guint
gnc_completion_model_get_menu_size (GncCompletionModel *model)
{
    g_return_val_if_fail (GNC_IS_COMPLETION_MODEL (model), 0);
    return g_hash_table_size (model->items);
}

gchar*
gnc_completion_model_dup_only_item (GncCompletionModel *model)
{
    GList *keys;
    gchar *item;

    g_return_val_if_fail (GNC_IS_COMPLETION_MODEL (model), NULL);

    if (g_hash_table_size (model->items) != 1)
        return NULL;

    keys = g_hash_table_get_keys (model->items);
    item = g_strdup (keys->data);
    g_list_free (keys);
    return item;
}

typedef struct
{
    GncCompletionModel *model;
    const gchar *query;
    GPtrArray *suggestions;
    guint sequence;
} SuggestionBuildData;

static gint
suggestion_compare (gconstpointer first, gconstpointer second)
{
    const GncSuggestionItem *item_a = *(GncSuggestionItem * const *)first;
    const GncSuggestionItem *item_b = *(GncSuggestionItem * const *)second;

    if (item_a->weight != item_b->weight)
        return item_a->weight < item_b->weight ? -1 : 1;
    if (item_a->sequence != item_b->sequence)
        return item_a->sequence < item_b->sequence ? -1 : 1;
    return 0;
}

static gint
append_match (SuggestionBuildData *data, const gchar *text, gint start_pos,
              gint occurrence_difference)
{
    gint text_length;
    gint position = 0;
    gint match_length = 0;
    gint return_position = -1;
    gint found_location;
    gint prefix_length;
    gint weight;
    gboolean have_boundary = FALSE;
    gchar *sub_text;
    gchar *prefix;
    gchar *match;
    gchar *suffix;
    gchar *markup;

    text_length = g_utf8_strlen (text, -1);
    if (start_pos >= text_length)
        return -1;

    sub_text = g_utf8_substring (text, start_pos, text_length);
    if (!gnc_unicode_has_substring_base_chars (data->query, sub_text,
                                               &position, &match_length))
    {
        g_free (sub_text);
        return -1;
    }

    found_location = start_pos + position;
    if (found_location > 0)
        prefix = g_utf8_substring (text, 0, found_location);
    else
        prefix = g_strdup ("");
    prefix_length = g_utf8_strlen (prefix, -1);
    match = g_utf8_substring (text, found_location,
                              found_location + match_length);
    suffix = g_utf8_substring (text, found_location + match_length,
                               text_length);

    if (position >= 1)
    {
        gunichar previous = g_utf8_get_char (
            g_utf8_offset_to_pointer (sub_text, position - 1));

        if (g_unichar_isspace (previous) || g_unichar_ispunct (previous))
            have_boundary = TRUE;
        else
            return_position = found_location + 1;
    }

    if (prefix_length == 0 || have_boundary)
    {
        GncSuggestionItem *item;

        markup = g_markup_printf_escaped ("%s<b>%s</b>%s ", prefix, match,
                                          suffix);
        weight = occurrence_difference;
        if (gnc_unicode_compare_base_chars (sub_text, data->query) == 0)
            weight = 1;
        item = suggestion_item_new_with_sequence (text, markup, weight,
                                                   found_location, FALSE,
                                                   data->sequence++);
        g_ptr_array_add (data->suggestions, item);
        g_free (markup);
    }

    g_free (suffix);
    g_free (match);
    g_free (prefix);
    g_free (sub_text);
    return return_position;
}

static void
append_item_matches (gpointer key, gpointer value, gpointer user_data)
{
    SuggestionBuildData *data = user_data;
    gchar *clean_text;
    gint occurrence_difference;
    gint start_pos = 0;

    clean_text = g_strdup (key);
    gnc_utf8_strip_invalid_and_controls (clean_text);
    if (!*clean_text)
    {
        g_free (clean_text);
        return;
    }

    if (data->model->reversed)
        occurrence_difference = GPOINTER_TO_INT (value) + 1;
    else
        occurrence_difference = data->model->occurrence -
                                GPOINTER_TO_INT (value);

    do
    {
        start_pos = append_match (data, clean_text, start_pos,
                                  occurrence_difference);
    }
    while (start_pos != -1);

    g_free (clean_text);
}

GListModel *
gnc_completion_model_build_suggestions (GncCompletionModel *model,
                                        const gchar *query,
                                        const gchar *sentinel,
                                        gboolean sort_enabled)
{
    GListStore *suggestions;
    SuggestionBuildData data = { 0 };

    g_return_val_if_fail (GNC_IS_COMPLETION_MODEL (model), NULL);
    g_return_val_if_fail (query != NULL, NULL);

    suggestions = g_list_store_new (GNC_TYPE_SUGGESTION_ITEM);
    if (!*query)
        return G_LIST_MODEL (suggestions);

    if (sentinel && *sentinel)
    {
        GncSuggestionItem *item;
        gchar *markup = g_markup_printf_escaped ("<i>%s</i>", sentinel);

        item = suggestion_item_new_with_sequence (sentinel, markup, 0, 0,
                                                   TRUE, 0);
        g_list_store_append (suggestions, item);
        g_object_unref (item);
        g_free (markup);
    }

    data.model = model;
    data.query = query;
    data.suggestions = g_ptr_array_new_with_free_func (g_object_unref);
    g_hash_table_foreach (model->items, append_item_matches, &data);

    if (sort_enabled)
        g_ptr_array_sort (data.suggestions, suggestion_compare);

    for (guint i = 0; i < data.suggestions->len; i++)
    {
        GncSuggestionItem *item = g_ptr_array_index (data.suggestions, i);

        g_list_store_append (suggestions, item);
    }

    g_ptr_array_unref (data.suggestions);
    return G_LIST_MODEL (suggestions);
}
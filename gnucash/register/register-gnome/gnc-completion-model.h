/********************************************************************
 * gnc-completion-model.h -- GTK4 completion data model             *
 *                                                                  *
 * Copyright (C) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#pragma once

#include <gio/gio.h>

G_BEGIN_DECLS

#define GNC_TYPE_SUGGESTION_ITEM (gnc_suggestion_item_get_type ())
G_DECLARE_FINAL_TYPE (GncSuggestionItem, gnc_suggestion_item, GNC,
                      SUGGESTION_ITEM, GObject)

GncSuggestionItem *gnc_suggestion_item_new (const gchar *text,
                                             const gchar *markup,
                                             gint weight,
                                             gint found_location,
                                             gboolean sentinel);
const gchar *gnc_suggestion_item_get_text (GncSuggestionItem *item);
const gchar *gnc_suggestion_item_get_markup (GncSuggestionItem *item);
gint gnc_suggestion_item_get_weight (GncSuggestionItem *item);
gint gnc_suggestion_item_get_found_location (GncSuggestionItem *item);
gboolean gnc_suggestion_item_is_sentinel (GncSuggestionItem *item);

#define GNC_TYPE_COMPLETION_MODEL (gnc_completion_model_get_type ())
G_DECLARE_FINAL_TYPE (GncCompletionModel, gnc_completion_model, GNC,
                      COMPLETION_MODEL, GObject)

GncCompletionModel *gnc_completion_model_new (void);
void gnc_completion_model_clear (GncCompletionModel *model);
void gnc_completion_model_add_menu_item (GncCompletionModel *model,
                                         const gchar *text);
void gnc_completion_model_set_reversed (GncCompletionModel *model,
                                        gboolean reversed);
guint gnc_completion_model_get_menu_size (GncCompletionModel *model);
gchar *gnc_completion_model_dup_only_item (GncCompletionModel *model);

/**
 * Build the visible suggestion list for @a query.
 *
 * The returned model owns its items. @a sentinel may be NULL; otherwise it
 * is the first entry and is marked with gnc_suggestion_item_is_sentinel().
 */
GListModel *gnc_completion_model_build_suggestions (GncCompletionModel *model,
                                                     const gchar *query,
                                                     const gchar *sentinel,
                                                     gboolean sort_enabled);

G_END_DECLS
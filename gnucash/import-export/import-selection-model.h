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
 * import-selection-model.h -- Shared selection invariants for import views.
 */

#ifndef GNC_IMPORT_SELECTION_MODEL_H
#define GNC_IMPORT_SELECTION_MODEL_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/*
 * GtkSingleSelection defaults to selecting the first available item. Import
 * views must only acquire a selection from a domain default or a user action,
 * so configure that policy before attaching even an already-populated model.
 * The selection takes its own reference to @model; the caller retains
 * ownership of the reference passed here.
 */
static inline GtkSingleSelection *
gnc_import_single_selection_new (GListModel *model)
{
    GtkSingleSelection *selection = gtk_single_selection_new (NULL);

    gtk_single_selection_set_autoselect (selection, FALSE);
    gtk_single_selection_set_can_unselect (selection, TRUE);
    gtk_single_selection_set_model (selection, model);
    return selection;
}

static inline guint
gnc_import_single_selection_get_insert_after_position (GtkSingleSelection *selection)
{
    GListModel *model;
    guint position;

    g_return_val_if_fail (GTK_IS_SINGLE_SELECTION (selection), 0);

    model = gtk_single_selection_get_model (selection);
    g_return_val_if_fail (model, 0);

    position = gtk_single_selection_get_selected (selection);
    return position == GTK_INVALID_LIST_POSITION
        ? g_list_model_get_n_items (model) : position + 1;
}

G_END_DECLS

#endif /* GNC_IMPORT_SELECTION_MODEL_H */

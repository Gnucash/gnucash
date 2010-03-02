/********************************************************************\
 * table-model.c -- 2D grid table object model                      *
 * Copyright (c) 2001 Free Software Foundation                      *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "table-model.h"


#define DEFAULT_HANDLER ""

typedef struct
{
    char *cell_name;
    gpointer handler;
} HandlerNode;


static GHashTable *
gnc_table_model_handler_hash_new (void)
{
    return g_hash_table_new (g_str_hash, g_str_equal);
}

static void
hash_destroy_helper (gpointer key, gpointer value, gpointer user_data)
{
    HandlerNode *node = value;

    g_free (node->cell_name);
    node->cell_name = NULL;

    g_free (node);
}

static void
gnc_table_model_handler_hash_destroy (GHashTable *hash)
{
    if (!hash) return;

    g_hash_table_foreach (hash, hash_destroy_helper, NULL);
    g_hash_table_destroy (hash);
}

static void
gnc_table_model_handler_hash_remove (GHashTable *hash, const char *cell_name)
{
    HandlerNode *node;

    if (!hash) return;

    node = g_hash_table_lookup (hash, cell_name);
    if (!node) return;

    g_hash_table_remove (hash, cell_name);

    g_free (node->cell_name);
    node->cell_name = NULL;

    g_free (node);
}

static void
gnc_table_model_handler_hash_insert (GHashTable *hash,
                                     const char *cell_name,
                                     gpointer handler)
{
    HandlerNode *node;

    g_return_if_fail (hash != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_remove (hash, cell_name);
    if (!handler) return;

    node = g_new0 (HandlerNode, 1);

    node->cell_name = g_strdup (cell_name);
    node->handler = handler;

    g_hash_table_insert (hash, node->cell_name, node);
}

static gpointer
gnc_table_model_handler_hash_lookup (GHashTable *hash, const char *cell_name)
{
    HandlerNode *node;

    if (!hash) return NULL;

    if (cell_name)
    {
        node = g_hash_table_lookup (hash, cell_name);
        if (node) return node->handler;
    }

    cell_name = DEFAULT_HANDLER;
    node = g_hash_table_lookup (hash, cell_name);
    if (node) return node->handler;

    return NULL;
}

TableModel *
gnc_table_model_new (void)
{
    TableModel *model;

    model = g_new0 (TableModel, 1);

    model->entry_handlers = gnc_table_model_handler_hash_new ();
    model->label_handlers = gnc_table_model_handler_hash_new ();
    model->help_handlers = gnc_table_model_handler_hash_new ();
    model->io_flags_handlers = gnc_table_model_handler_hash_new ();
    model->fg_color_handlers = gnc_table_model_handler_hash_new ();
    model->bg_color_handlers = gnc_table_model_handler_hash_new ();
    model->cell_border_handlers = gnc_table_model_handler_hash_new ();
    model->confirm_handlers = gnc_table_model_handler_hash_new ();
    model->save_handlers = gnc_table_model_handler_hash_new ();

    model->read_only = FALSE;
    model->dividing_row = -1;

    return model;
}

void
gnc_table_model_destroy (TableModel *model)
{
    if (!model) return;

    gnc_table_model_handler_hash_destroy (model->entry_handlers);
    model->entry_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->label_handlers);
    model->label_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->help_handlers);
    model->help_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->io_flags_handlers);
    model->io_flags_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->fg_color_handlers);
    model->fg_color_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->bg_color_handlers);
    model->bg_color_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->cell_border_handlers);
    model->cell_border_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->confirm_handlers);
    model->confirm_handlers = NULL;

    gnc_table_model_handler_hash_destroy (model->save_handlers);
    model->save_handlers = NULL;

    g_free (model);
}

void
gnc_table_model_set_read_only (TableModel *model, gboolean read_only)
{
    g_return_if_fail (model);

    model->read_only = read_only;
}

gboolean
gnc_table_model_read_only (TableModel *model)
{
    g_return_val_if_fail (model, FALSE);

    return model->read_only;
}

void
gnc_table_model_set_entry_handler (TableModel *model,
                                   TableGetEntryHandler entry_handler,
                                   const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->entry_handlers,
                                         cell_name,
                                         entry_handler);
}

void
gnc_table_model_set_default_entry_handler
(TableModel *model, TableGetEntryHandler entry_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->entry_handlers,
                                         DEFAULT_HANDLER,
                                         entry_handler);
}

TableGetEntryHandler
gnc_table_model_get_entry_handler (TableModel *model, const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->entry_handlers,
            cell_name);
}

void
gnc_table_model_set_label_handler (TableModel *model,
                                   TableGetLabelHandler label_handler,
                                   const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->label_handlers,
                                         cell_name,
                                         label_handler);
}

void
gnc_table_model_set_default_label_handler
(TableModel *model, TableGetLabelHandler label_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->label_handlers,
                                         DEFAULT_HANDLER,
                                         label_handler);
}

TableGetLabelHandler
gnc_table_model_get_label_handler (TableModel *model, const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->label_handlers,
            cell_name);
}

void
gnc_table_model_set_help_handler (TableModel *model,
                                  TableGetHelpHandler help_handler,
                                  const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->help_handlers,
                                         cell_name,
                                         help_handler);
}

void
gnc_table_model_set_default_help_handler (TableModel *model,
        TableGetHelpHandler help_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->help_handlers,
                                         DEFAULT_HANDLER,
                                         help_handler);
}

TableGetHelpHandler
gnc_table_model_get_help_handler (TableModel *model, const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->help_handlers, cell_name);
}

void
gnc_table_model_set_io_flags_handler
(TableModel *model,
 TableGetCellIOFlagsHandler io_flags_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->io_flags_handlers,
                                         cell_name,
                                         io_flags_handler);
}

void
gnc_table_model_set_default_io_flags_handler
(TableModel *model,
 TableGetCellIOFlagsHandler io_flags_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->io_flags_handlers,
                                         DEFAULT_HANDLER,
                                         io_flags_handler);
}

TableGetCellIOFlagsHandler
gnc_table_model_get_io_flags_handler (TableModel *model,
                                      const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->io_flags_handlers,
            cell_name);
}

void
gnc_table_model_set_fg_color_handler
(TableModel *model,
 TableGetFGColorHandler fg_color_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->fg_color_handlers,
                                         cell_name,
                                         fg_color_handler);
}

void
gnc_table_model_set_default_fg_color_handler
(TableModel *model,
 TableGetFGColorHandler fg_color_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->fg_color_handlers,
                                         DEFAULT_HANDLER,
                                         fg_color_handler);
}

TableGetFGColorHandler
gnc_table_model_get_fg_color_handler (TableModel *model,
                                      const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->fg_color_handlers,
            cell_name);
}

void
gnc_table_model_set_bg_color_handler
(TableModel *model,
 TableGetBGColorHandler bg_color_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->bg_color_handlers,
                                         cell_name,
                                         bg_color_handler);
}

void
gnc_table_model_set_default_bg_color_handler
(TableModel *model,
 TableGetBGColorHandler bg_color_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->bg_color_handlers,
                                         DEFAULT_HANDLER,
                                         bg_color_handler);
}

TableGetBGColorHandler
gnc_table_model_get_bg_color_handler (TableModel *model,
                                      const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->bg_color_handlers,
            cell_name);
}

void
gnc_table_model_set_cell_border_handler
(TableModel *model,
 TableGetCellBorderHandler cell_border_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->cell_border_handlers,
                                         cell_name,
                                         cell_border_handler);
}

void
gnc_table_model_set_default_cell_border_handler
(TableModel *model,
 TableGetCellBorderHandler cell_border_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->cell_border_handlers,
                                         DEFAULT_HANDLER,
                                         cell_border_handler);
}

TableGetCellBorderHandler
gnc_table_model_get_cell_border_handler (TableModel *model,
        const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->cell_border_handlers,
            cell_name);
}

void
gnc_table_model_set_confirm_handler
(TableModel *model,
 TableConfirmHandler confirm_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->confirm_handlers,
                                         cell_name,
                                         confirm_handler);
}

void
gnc_table_model_set_default_confirm_handler
(TableModel *model,
 TableConfirmHandler confirm_handler)
{
    g_return_if_fail (model != NULL);

    gnc_table_model_handler_hash_insert (model->confirm_handlers,
                                         DEFAULT_HANDLER,
                                         confirm_handler);
}

TableConfirmHandler
gnc_table_model_get_confirm_handler (TableModel *model,
                                     const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->confirm_handlers,
            cell_name);
}

void
gnc_table_model_set_save_handler
(TableModel *model,
 TableSaveCellHandler save_handler,
 const char * cell_name)
{
    g_return_if_fail (model != NULL);
    g_return_if_fail (cell_name != NULL);

    gnc_table_model_handler_hash_insert (model->save_handlers,
                                         cell_name,
                                         save_handler);
}

void
gnc_table_model_set_pre_save_handler
(TableModel *model,
 TableSaveHandler save_handler)
{
    g_return_if_fail (model != NULL);

    model->pre_save_handler = save_handler;
}

void
gnc_table_model_set_post_save_handler
(TableModel *model,
 TableSaveHandler save_handler)
{
    g_return_if_fail (model != NULL);

    model->post_save_handler = save_handler;
}

TableSaveCellHandler
gnc_table_model_get_save_handler
(TableModel *model,
 const char * cell_name)
{
    g_return_val_if_fail (model != NULL, NULL);

    return gnc_table_model_handler_hash_lookup (model->save_handlers, cell_name);
}

TableSaveHandler
gnc_table_model_get_pre_save_handler
(TableModel *model)
{
    g_return_val_if_fail (model != NULL, NULL);

    return model->pre_save_handler;
}

TableSaveHandler
gnc_table_model_get_post_save_handler
(TableModel *model)
{
    g_return_val_if_fail (model != NULL, NULL);

    return model->post_save_handler;
}

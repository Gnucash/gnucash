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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "table-model.h"


#define DEFAULT_HANDLER (-1)

typedef struct
{
  int cell_type;
  gpointer handler;
} HandlerNode;


static GHashTable *
gnc_table_model_handler_hash_new (void)
{
  return g_hash_table_new (g_int_hash, g_int_equal);
}

static void
hash_destroy_helper (gpointer key, gpointer value, gpointer user_data)
{
  HandlerNode *node = value;

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
gnc_table_model_handler_hash_remove (GHashTable *hash, int cell_type)
{
  HandlerNode *node;

  if (!hash) return;

  node = g_hash_table_lookup (hash, &cell_type);
  if (!node) return;

  g_free (node);
}

static void
gnc_table_model_handler_hash_insert (GHashTable *hash, int cell_type,
                                     gpointer handler)
{
  HandlerNode *node;

  g_return_if_fail (hash != NULL);
  g_return_if_fail ((cell_type >= 0) || (cell_type == DEFAULT_HANDLER));

  gnc_table_model_handler_hash_remove (hash, cell_type);
  if (!handler) return;

  node = g_new0 (HandlerNode, 1);

  node->cell_type = cell_type;
  node->handler = handler;

  g_hash_table_insert (hash, &node->cell_type, node);
}

static gpointer
gnc_table_model_handler_hash_lookup (GHashTable *hash, int cell_type)
{
  HandlerNode *node;

  if (!hash) return NULL;

  node = g_hash_table_lookup (hash, &cell_type);
  if (node) return node->handler;

  cell_type = DEFAULT_HANDLER;
  node = g_hash_table_lookup (hash, &cell_type);
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
  model->io_flags_handlers = gnc_table_model_handler_hash_new ();
  model->fg_color_handlers = gnc_table_model_handler_hash_new ();
  model->bg_color_handlers = gnc_table_model_handler_hash_new ();

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

  gnc_table_model_handler_hash_destroy (model->io_flags_handlers);
  model->io_flags_handlers = NULL;

  gnc_table_model_handler_hash_destroy (model->fg_color_handlers);
  model->fg_color_handlers = NULL;

  gnc_table_model_handler_hash_destroy (model->bg_color_handlers);
  model->bg_color_handlers = NULL;

  g_free (model);
}

void
gnc_table_model_set_entry_handler (TableModel *model,
                                   TableGetEntryHandler entry_handler,
                                   int cell_type)
{
  g_return_if_fail (model != NULL);
  g_return_if_fail (cell_type >= 0);

  gnc_table_model_handler_hash_insert (model->entry_handlers,
                                       cell_type,
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
gnc_table_model_get_entry_handler (TableModel *model, int cell_type)
{
  g_return_val_if_fail (model != NULL, NULL);

  return gnc_table_model_handler_hash_lookup (model->entry_handlers,
                                              cell_type);
}

void
gnc_table_model_set_label_handler (TableModel *model,
                                   TableGetLabelHandler label_handler,
                                   int cell_type)
{
  g_return_if_fail (model != NULL);
  g_return_if_fail (cell_type >= 0);

  gnc_table_model_handler_hash_insert (model->label_handlers,
                                       cell_type,
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
gnc_table_model_get_label_handler (TableModel *model, int cell_type)
{
  g_return_val_if_fail (model != NULL, NULL);

  return gnc_table_model_handler_hash_lookup (model->label_handlers,
                                              cell_type);
}

void
gnc_table_model_set_io_flags_handler
                                  (TableModel *model,
                                   TableGetCellIOFlagsHandler io_flags_handler,
                                   int cell_type)
{
  g_return_if_fail (model != NULL);
  g_return_if_fail (cell_type >= 0);

  gnc_table_model_handler_hash_insert (model->io_flags_handlers,
                                       cell_type,
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
                                      int cell_type)
{
  g_return_val_if_fail (model != NULL, NULL);

  return gnc_table_model_handler_hash_lookup (model->io_flags_handlers,
                                              cell_type);
}

void
gnc_table_model_set_fg_color_handler
                                  (TableModel *model,
                                   TableGetFGColorHandler fg_color_handler,
                                   int cell_type)
{
  g_return_if_fail (model != NULL);
  g_return_if_fail (cell_type >= 0);

  gnc_table_model_handler_hash_insert (model->fg_color_handlers,
                                       cell_type,
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
                                      int cell_type)
{
  g_return_val_if_fail (model != NULL, NULL);

  return gnc_table_model_handler_hash_lookup (model->fg_color_handlers,
                                              cell_type);
}

void
gnc_table_model_set_bg_color_handler
                                  (TableModel *model,
                                   TableGetBGColorHandler bg_color_handler,
                                   int cell_type)
{
  g_return_if_fail (model != NULL);
  g_return_if_fail (cell_type >= 0);

  gnc_table_model_handler_hash_insert (model->bg_color_handlers,
                                       cell_type,
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
                                      int cell_type)
{
  g_return_val_if_fail (model != NULL, NULL);

  return gnc_table_model_handler_hash_lookup (model->bg_color_handlers,
                                              cell_type);
}

/********************************************************************\
 * gnc-sx-list-tree-model-adapter.h                                 *
 *                                                                  *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
 *                                                                  *
 * As a special exception, permission is granted to link the binary *
 * module resultant from this code with the OpenSSL project's       *
 * "OpenSSL" library (or modified versions of it that use the same  *
 * license as the "OpenSSL" library), and distribute the linked     *
 * executable.  You must obey the GNU General Public License in all *
 * respects for all of the code used other than "OpenSSL". If you   *
 * modify this file, you may extend this exception to your version  *
 * of the file, but you are not obligated to do so. If you do not   *
 * wish to do so, delete this exception statement from your version *
 * of this file.                                                    *
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
\********************************************************************/


/********************************************************************\
 * gnc-sx-list-tree-model-adapter.h                                 *
 * GTK4 list-model adapter for scheduled transactions.              *
\********************************************************************/
#ifndef _GNC_SX_LIST_TREE_MODEL_ADAPTER_H
#define _GNC_SX_LIST_TREE_MODEL_ADAPTER_H

#include <glib-object.h>
#include <gio/gio.h>
#include "SchedXaction.h"
#include "gnc-sx-instance-model.h"

G_BEGIN_DECLS

#define GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER (gnc_sx_list_tree_model_adapter_get_type ())
G_DECLARE_FINAL_TYPE (GncSxListTreeModelAdapter, gnc_sx_list_tree_model_adapter,
                      GNC, SX_LIST_TREE_MODEL_ADAPTER, GObject)

#define GNC_TYPE_SX_LIST_ROW (gnc_sx_list_row_get_type ())
G_DECLARE_FINAL_TYPE (GncSxListRow, gnc_sx_list_row, GNC, SX_LIST_ROW, GObject)

GncSxListTreeModelAdapter* gnc_sx_list_tree_model_adapter_new (GncSxInstanceModel *instances);
GListModel* gnc_sx_list_tree_model_adapter_get_model (GncSxListTreeModelAdapter *model);
void gnc_sx_list_tree_model_adapter_refresh (GncSxListTreeModelAdapter *model);

SchedXaction* gnc_sx_list_row_get_sx (GncSxListRow *row);
const gchar* gnc_sx_list_row_get_name (GncSxListRow *row);
gboolean gnc_sx_list_row_get_enabled (GncSxListRow *row);
const gchar* gnc_sx_list_row_get_frequency (GncSxListRow *row);
guint gnc_sx_list_row_get_num_postponed (GncSxListRow *row);
const gchar* gnc_sx_list_row_get_last_occur (GncSxListRow *row);
const gchar* gnc_sx_list_row_get_next_occur (GncSxListRow *row);

G_END_DECLS

#endif /* _GNC_SX_LIST_TREE_MODEL_ADAPTER_H */

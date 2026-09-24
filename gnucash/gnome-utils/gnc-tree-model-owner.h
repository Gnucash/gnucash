/*
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
 * Author: Geert Janssens <geert@kobaltwit.be>
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

/* GTK4 owner list model. */
#ifndef __GNC_TREE_MODEL_OWNER_H
#define __GNC_TREE_MODEL_OWNER_H

#include <gtk/gtk.h>
#include "gncOwner.h"

G_BEGIN_DECLS

#define GNC_TYPE_TREE_MODEL_OWNER (gnc_tree_model_owner_get_type ())
G_DECLARE_FINAL_TYPE (GncTreeModelOwner, gnc_tree_model_owner, GNC, TREE_MODEL_OWNER, GObject)

GncTreeModelOwner *gnc_tree_model_owner_new (GncOwnerType owner_type);
GListModel *gnc_tree_model_owner_get_model (GncTreeModelOwner *model);
GncOwner *gnc_tree_model_owner_get_row_owner (GObject *row);
guint gnc_tree_model_owner_find_owner (GncTreeModelOwner *model,
                                       const GncOwner *owner);

G_END_DECLS
#endif

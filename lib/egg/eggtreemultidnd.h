/* eggtreednd.h
 * Copyright (C) 2001  Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __EGG_TREE_MULTI_DND_H__
#define __EGG_TREE_MULTI_DND_H__

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtkdnd.h>

G_BEGIN_DECLS

#define EGG_TYPE_TREE_MULTI_DRAG_SOURCE            (egg_tree_multi_drag_source_get_type ())
#define EGG_TREE_MULTI_DRAG_SOURCE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TREE_MULTI_DRAG_SOURCE, EggTreeMultiDragSource))
#define EGG_IS_TREE_MULTI_DRAG_SOURCE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TREE_MULTI_DRAG_SOURCE))
#define EGG_TREE_MULTI_DRAG_SOURCE_GET_IFACE(obj)  (G_TYPE_INSTANCE_GET_INTERFACE ((obj), EGG_TYPE_TREE_MULTI_DRAG_SOURCE, EggTreeMultiDragSourceIface))

typedef struct _EggTreeMultiDragSource      EggTreeMultiDragSource; /* Dummy typedef */
typedef struct _EggTreeMultiDragSourceIface EggTreeMultiDragSourceIface;

struct _EggTreeMultiDragSourceIface
{
  GTypeInterface g_iface;

  /* VTable - not signals */
  gboolean     (* row_draggable)        (EggTreeMultiDragSource   *drag_source,
                                         GList                    *path_list);

  gboolean     (* drag_data_get)        (EggTreeMultiDragSource   *drag_source,
                                         GList                    *path_list,
                                         GtkSelectionData         *selection_data);

  gboolean     (* drag_data_delete)     (EggTreeMultiDragSource *drag_source,
                                         GList                  *path_list);
};

GType    egg_tree_multi_drag_source_get_type         (void) G_GNUC_CONST;

/* Returns whether the given row can be dragged */
gboolean egg_tree_multi_drag_source_row_draggable    (EggTreeMultiDragSource *drag_source,
						      GList                  *path_list);

/* Deletes the given row, or returns FALSE if it can't */
gboolean egg_tree_multi_drag_source_drag_data_delete (EggTreeMultiDragSource *drag_source,
						      GList                  *path_list);


/* Fills in selection_data with type selection_data->target based on the row
 * denoted by path, returns TRUE if it does anything
 */
gboolean egg_tree_multi_drag_source_drag_data_get    (EggTreeMultiDragSource *drag_source,
						      GList                  *path_list,
						      GtkSelectionData       *selection_data);
void     egg_tree_multi_drag_add_drag_support        (GtkTreeView            *tree_view);



G_END_DECLS

#endif /* __EGG_TREE_MULTI_DND_H__ */

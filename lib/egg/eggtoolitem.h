/* eggtoolitem.c
 *
 * Copyright (C) 2002 Anders Carlsson <andersca@codefactory.se>
 * Copyright (C) 2002 James Henstridge <james@daa.com.au>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __EGG_TOOL_ITEM_H__
#define __EGG_TOOL_ITEM_H__

#include <gtk/gtkbin.h>
#include <gtk/gtktooltips.h>
#include <gtk/gtkmenuitem.h>

#define EGG_TYPE_TOOL_ITEM            (egg_tool_item_get_type ())
#define EGG_TOOL_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TOOL_ITEM, EggToolItem))
#define EGG_TOOL_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_TOOL_ITEM, EggToolItemClass))
#define EGG_IS_TOOL_ITEM(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TOOL_ITEM))
#define EGG_IS_TOOL_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_TOOL_ITEM))
#define EGG_TOOL_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_TOOL_ITEM, EggToolItemClass))

typedef struct _EggToolItem      EggToolItem;
typedef struct _EggToolItemClass EggToolItemClass;

struct _EggToolItem
{
  GtkBin parent;

  gchar *tip_text;
  gchar *tip_private;

  GdkWindow *drag_window;

  guint visible_horizontal : 1;
  guint visible_vertical : 1;
  guint homogeneous : 1;
  guint expand : 1;
  guint pack_end : 1;
  guint use_drag_window : 1;
  guint overflow_item : 1;

  GtkWidget *menu_item;
  gchar *menu_item_id;
};

struct _EggToolItemClass
{
  GtkBinClass parent_class;

  /* signals */
  gboolean   (* create_menu_proxy)    (EggToolItem *tool_item);
  void       (* toolbar_reconfigured) (EggToolItem *tool_item);
  gboolean   (* set_tooltip)	      (EggToolItem *tool_item,
				       GtkTooltips *tooltips,
				       const gchar *tip_text,
				       const gchar *tip_private);
};

GType        egg_tool_item_get_type (void);
EggToolItem *egg_tool_item_new      (void);

void            egg_tool_item_toolbar_reconfigured     (EggToolItem *tool_item);
void            egg_tool_item_set_homogeneous          (EggToolItem *tool_item,
							gboolean     homogeneous);
void            egg_tool_item_set_expand               (EggToolItem *tool_item,
							gboolean     expand);
void            egg_tool_item_set_pack_end             (EggToolItem *tool_item,
							gboolean     pack_end);
void            egg_tool_item_set_tooltip              (EggToolItem *tool_item,
							GtkTooltips *tooltips,
							const gchar *tip_text,
							const gchar *tip_private);
void            egg_tool_item_set_use_drag_window      (EggToolItem *toolitem,
							gboolean     use_drag_window);
void            egg_tool_item_set_visible_horizontal   (EggToolItem *toolitem,
							gboolean     visible_horizontal);
gboolean        egg_tool_item_get_visible_horizontal   (EggToolItem *toolitem);
void            egg_tool_item_set_visible_vertical     (EggToolItem *toolitem,
							gboolean     visible_horizontal);
gboolean        egg_tool_item_get_visible_vertical     (EggToolItem *toolitem);
GtkIconSize     egg_tool_item_get_icon_size            (EggToolItem *tool_item);
GtkOrientation  egg_tool_item_get_orientation          (EggToolItem *tool_item);
GtkToolbarStyle egg_tool_item_get_toolbar_style        (EggToolItem *tool_item);
GtkReliefStyle  egg_tool_item_get_relief_style         (EggToolItem *tool_item);
GtkWidget *     egg_tool_item_retrieve_proxy_menu_item (EggToolItem *tool_item);
GtkWidget *     egg_tool_item_get_proxy_menu_item      (EggToolItem *tool_item,
							const gchar *menu_item_id);
void            egg_tool_item_set_proxy_menu_item      (EggToolItem *tool_item,
							const gchar *menu_item_id,
							GtkWidget   *menu_item);


#endif /* __EGG_TOOL_ITEM_H__ */

/* eggtoggletoolbutton.h
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

#ifndef __EGG_SEPARATOR_TOOL_ITEM_H__
#define __EGG_SEPARATOR_TOOL_ITEM_H__

#include "eggtoolitem.h"

#define EGG_TYPE_SEPARATOR_TOOL_ITEM            (egg_separator_tool_item_get_type ())
#define EGG_SEPARATOR_TOOL_ITEM(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_SEPARATOR_TOOL_ITEM, EggSeparatorToolItem))
#define EGG_SEPARATOR_TOOL_ITEM_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_SEPARATOR_TOOL_ITEM, EggSeparatorToolItemClass))
#define EGG_IS_SEPARATOR_TOOL_ITEM(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_SEPARATOR_TOOL_ITEM))
#define EGG_IS_SEPARATOR_TOOL_ITEM_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_SEPARATOR_TOOL_ITEM))
#define EGG_SEPARATOR_TOOL_ITEM_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_SEPARATOR_TOOL_ITEM, EggSeparatorToolItemClass))

typedef struct _EggSeparatorToolItem      EggSeparatorToolItem;
typedef struct _EggSeparatorToolItemClass EggSeparatorToolItemClass;

struct _EggSeparatorToolItem
{
  EggToolItem parent;
};

struct _EggSeparatorToolItemClass
{
  EggToolItemClass parent_class;
};

GType        egg_separator_tool_item_get_type (void) G_GNUC_CONST;
EggToolItem *egg_separator_tool_item_new      (void);

#endif /* __EGG_SEPARATOR_TOOL_ITEM_H__ */

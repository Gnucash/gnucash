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

#ifndef __EGG_TOGGLE_TOOL_BUTTON_H__
#define __EGG_TOGGLE_TOOL_BUTTON_H__

#include "eggtoolbutton.h"

G_BEGIN_DECLS

#define EGG_TYPE_TOGGLE_TOOL_BUTTON            (egg_toggle_tool_button_get_type ())
#define EGG_TOGGLE_TOOL_BUTTON(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TOGGLE_TOOL_BUTTON, EggToggleToolButton))
#define EGG_TOGGLE_TOOL_BUTTON_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_TOGGLE_TOOL_BUTTON, EggToggleToolButtonClass))
#define EGG_IS_TOGGLE_TOOL_BUTTON(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TOGGLE_TOOL_BUTTON))
#define EGG_IS_TOGGLE_TOOL_BUTTON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_TOGGLE_TOOL_BUTTON))
#define EGG_TOGGLE_TOOL_BUTTON_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_TOGGLE_TOOL_BUTTON, EggToggleToolButtonClass))

typedef struct _EggToggleToolButton      EggToggleToolButton;
typedef struct _EggToggleToolButtonClass EggToggleToolButtonClass;

struct _EggToggleToolButton
{
  EggToolButton parent;

  /*< private >*/
  GtkWidget *menu_item;

  guint active : 1;
};

struct _EggToggleToolButtonClass
{
  EggToolButtonClass parent_class;

  /* signal */
  void (* toggled) (EggToggleToolButton *button);
};

GType        egg_toggle_tool_button_get_type       (void) G_GNUC_CONST;
EggToolItem *egg_toggle_tool_button_new            (void);
EggToolItem *egg_toggle_tool_button_new_from_stock (const gchar *stock_id);

void         egg_toggle_tool_button_set_active     (EggToggleToolButton *button,
						    gboolean             is_active);
gboolean     egg_toggle_tool_button_get_active     (EggToggleToolButton *button);

G_END_DECLS

#endif /* __EGG_TOGGLE_TOOL_BUTTON_H__ */

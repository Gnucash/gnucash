/* eggradiotoolbutton.h
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

#ifndef __EGG_RADIO_TOOL_BUTTON_H__
#define __EGG_RADIO_TOOL_BUTTON_H__

#include "eggtoggletoolbutton.h"

#define EGG_TYPE_RADIO_TOOL_BUTTON            (egg_radio_tool_button_get_type ())
#define EGG_RADIO_TOOL_BUTTON(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_RADIO_TOOL_BUTTON, EggRadioToolButton))
#define EGG_RADIO_TOOL_BUTTON_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), EGG_TYPE_RADIO_TOOL_BUTTON, EggRadioToolButtonClass))
#define EGG_IS_RADIO_TOOL_BUTTON(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_RADIO_TOOL_BUTTON))
#define EGG_IS_RADIO_TOOL_BUTTON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EGG_TYPE_RADIO_TOOL_BUTTON))
#define EGG_RADIO_TOOL_BUTTON_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), EGG_TYPE_RADIO_TOOL_BUTTON, EggRadioToolButtonClass))

typedef struct _EggRadioToolButton      EggRadioToolButton;
typedef struct _EggRadioToolButtonClass EggRadioToolButtonClass;

struct _EggRadioToolButton
{
  EggToggleToolButton parent;
};

struct _EggRadioToolButtonClass
{
  EggToggleToolButtonClass parent_class;
};

GType        egg_radio_tool_button_get_type       (void) G_GNUC_CONST;

EggToolItem *egg_radio_tool_button_new                        (GSList             *group);
EggToolItem *egg_radio_tool_button_new_from_stock             (GSList             *group,
							       const gchar        *stock_id);
EggToolItem *egg_radio_tool_button_new_from_widget            (GtkWidget          *group,
							       const gchar        *stock_id);
EggToolItem *egg_radio_tool_button_new_with_stock_from_widget (GtkWidget          *group);
GSList *     egg_radio_tool_button_get_group                  (EggRadioToolButton *button);
void         egg_radio_tool_button_set_group                  (EggRadioToolButton *button,
							       GSList             *group);



#endif /* __EGG_RADIO_TOOL_BUTTON_H__ */

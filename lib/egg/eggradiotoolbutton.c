/* eggradiotoolbutton.c
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

#include "eggradiotoolbutton.h"
#include <gtk/gtkradiobutton.h>

#ifndef _
#  define _(s) (s)
#endif

static void egg_radio_tool_button_init       (EggRadioToolButton      *button);
static void egg_radio_tool_button_class_init (EggRadioToolButtonClass *klass);

GType
egg_radio_tool_button_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggRadioToolButtonClass),
	  (GBaseInitFunc) NULL,
	  (GBaseFinalizeFunc) NULL,
	  (GClassInitFunc) egg_radio_tool_button_class_init,
	  (GClassFinalizeFunc) NULL,
	  NULL,
	  sizeof (EggRadioToolButton),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) egg_radio_tool_button_init
	};

      type = g_type_register_static (EGG_TYPE_TOGGLE_TOOL_BUTTON,
				     "EggRadioToolButton", &type_info, 0);
    }
  return type;
}

     
static void
egg_radio_tool_button_class_init (EggRadioToolButtonClass *klass)
{
  EggToolButtonClass *toolbutton_class;

  toolbutton_class = (EggToolButtonClass *)klass;
  
  toolbutton_class->button_type = GTK_TYPE_RADIO_BUTTON;  
}

static void
egg_radio_tool_button_init (EggRadioToolButton *button)
{
  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (EGG_TOOL_BUTTON (button)->button), FALSE);
}

EggToolItem *
egg_radio_tool_button_new (GSList *group)
{
  EggRadioToolButton *button;
  
  button = g_object_new (EGG_TYPE_RADIO_TOOL_BUTTON,
			 NULL);

  egg_radio_tool_button_set_group (button, group);
  
  return EGG_TOOL_ITEM (button);
}

EggToolItem *
egg_radio_tool_button_new_from_stock (GSList      *group,
				      const gchar *stock_id)
{
  EggRadioToolButton *button;

  g_return_val_if_fail (stock_id != NULL, NULL);
  
  button = g_object_new (EGG_TYPE_RADIO_TOOL_BUTTON,
			 "stock_id", stock_id,
			 NULL);


  egg_radio_tool_button_set_group (button, group);
  
  return EGG_TOOL_ITEM (button);
}

EggToolItem *
egg_radio_tool_button_new_from_widget (GtkWidget   *group,
				       const gchar *stock_id)
{
  GSList *list = NULL;
  
  g_return_val_if_fail (EGG_IS_RADIO_TOOL_BUTTON (group), NULL);

  if (group)
    list = egg_radio_tool_button_get_group (EGG_RADIO_TOOL_BUTTON (group));
  
  return egg_radio_tool_button_new_from_stock (list, stock_id);
}

EggToolItem *
egg_radio_tool_button_new_with_stock_from_widget (GtkWidget *group)
{
  GSList *list = NULL;
  
  g_return_val_if_fail (EGG_IS_RADIO_TOOL_BUTTON (group), NULL);

  if (group)
    list = egg_radio_tool_button_get_group (EGG_RADIO_TOOL_BUTTON (group));
  
  return egg_radio_tool_button_new (list);
}

GSList *
egg_radio_tool_button_get_group (EggRadioToolButton *button)
{
  g_return_val_if_fail (EGG_IS_RADIO_TOOL_BUTTON (button), NULL);

  return gtk_radio_button_get_group (GTK_RADIO_BUTTON (EGG_TOOL_BUTTON (button)->button));
}

void
egg_radio_tool_button_set_group (EggRadioToolButton *button,
				 GSList             *group)
{
  g_return_if_fail (EGG_IS_RADIO_TOOL_BUTTON (button));

  gtk_radio_button_set_group (GTK_RADIO_BUTTON (EGG_TOOL_BUTTON (button)->button), group);
}


/* eggtoggletoolbutton.c
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

#include "eggtoggletoolbutton.h"
#include <gtk/gtkcheckmenuitem.h>
#include <gtk/gtklabel.h>
#include <gtk/gtktogglebutton.h>

#ifndef _
#  define _(s) (s)
#endif

#define MENU_ID "egg-toggle-tool-button-menu-id"

enum {
  TOGGLED,
  LAST_SIGNAL
};

static void egg_toggle_tool_button_init       (EggToggleToolButton      *button);
static void egg_toggle_tool_button_class_init (EggToggleToolButtonClass *klass);
static void egg_toggle_tool_button_finalize   (GObject                  *object);

static gboolean egg_toggle_tool_button_create_menu_proxy (EggToolItem *button);

static void button_toggled      (GtkWidget           *widget,
				 EggToggleToolButton *button);
static void menu_item_activated (GtkWidget           *widget,
				 EggToggleToolButton *button);

static GObjectClass *parent_class = NULL;
static guint         toggle_signals[LAST_SIGNAL] = { 0 };

GType
egg_toggle_tool_button_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggToggleToolButtonClass),
	  (GBaseInitFunc) 0,
	  (GBaseFinalizeFunc) 0,
	  (GClassInitFunc) egg_toggle_tool_button_class_init,
	  (GClassFinalizeFunc) 0,
	  NULL,
	  sizeof (EggToggleToolButton),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) egg_toggle_tool_button_init
	};

      type = g_type_register_static (EGG_TYPE_TOOL_BUTTON,
				     "EggToggleToolButton", &type_info, 0);
    }
  return type;
}


static void
egg_toggle_tool_button_class_init (EggToggleToolButtonClass *klass)
{
  GObjectClass *object_class;
  EggToolItemClass *toolitem_class;
  EggToolButtonClass *toolbutton_class;

  parent_class = g_type_class_peek_parent (klass);

  object_class = (GObjectClass *)klass;
  toolitem_class = (EggToolItemClass *)klass;
  toolbutton_class = (EggToolButtonClass *)klass;

  object_class->finalize = egg_toggle_tool_button_finalize;
  toolitem_class->create_menu_proxy = egg_toggle_tool_button_create_menu_proxy;
  toolbutton_class->button_type = GTK_TYPE_TOGGLE_BUTTON;
  
  toggle_signals[TOGGLED] =
    g_signal_new ("toggled",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (EggToggleToolButtonClass, toggled),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
}

static void
egg_toggle_tool_button_init (EggToggleToolButton *button)
{
  g_signal_connect_object (EGG_TOOL_BUTTON (button)->button, "toggled",
			   G_CALLBACK (button_toggled), button, 0);
}

static void
egg_toggle_tool_button_finalize (GObject *object)
{
  EggToggleToolButton *button = EGG_TOGGLE_TOOL_BUTTON (object);
  
  if (button->menu_item)
    g_object_remove_weak_pointer (G_OBJECT (button->menu_item),
				  (gpointer *)&(button->menu_item));

  (* G_OBJECT_CLASS (parent_class)->finalize) (object);
}

static gboolean
egg_toggle_tool_button_create_menu_proxy (EggToolItem *item)
{
  GtkWidget *menu_item = NULL;

  EggToggleToolButton *button = EGG_TOGGLE_TOOL_BUTTON (item);
  gchar *label;

  label = _egg_tool_button_get_label_text (EGG_TOOL_BUTTON (item));
  
  menu_item = gtk_check_menu_item_new_with_mnemonic (label);
  g_free (label);

  g_object_ref (menu_item);
  gtk_object_sink (GTK_OBJECT (menu_item));
  
  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item),
				  button->active);

  g_signal_connect_object (menu_item, "activate",
			   G_CALLBACK (menu_item_activated),
			   EGG_TOOL_BUTTON (button), 0);

  egg_tool_item_set_proxy_menu_item (item, MENU_ID, menu_item);

  g_object_unref (menu_item);
  
  return TRUE;
}

static void
menu_item_activated (GtkWidget           *menu_item,
		     EggToggleToolButton *toggle_tool_button)
{
  EggToolButton *tool_button = EGG_TOOL_BUTTON (toggle_tool_button);
  gboolean menu_active = GTK_CHECK_MENU_ITEM (menu_item)->active;

  if (toggle_tool_button->active != menu_active)
    {
      toggle_tool_button->active = menu_active;

      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (tool_button->button),
				    toggle_tool_button->active);

      g_signal_emit (G_OBJECT (toggle_tool_button), toggle_signals[TOGGLED], 0);
    }
}

static void
button_toggled (GtkWidget           *widget,
		EggToggleToolButton *toggle_tool_button)
{
  gboolean toggle_active = GTK_TOGGLE_BUTTON (widget)->active;

  if (toggle_tool_button->active != toggle_active)
    {
      GtkWidget *menu_item;
      
      toggle_tool_button->active = toggle_active;
       
      if ((menu_item =
	   egg_tool_item_get_proxy_menu_item (EGG_TOOL_ITEM (toggle_tool_button), MENU_ID)))
	  {
	  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (menu_item),
					  toggle_tool_button->active);
	}

      g_signal_emit (G_OBJECT (toggle_tool_button), toggle_signals[TOGGLED], 0);
    }
}

EggToolItem *
egg_toggle_tool_button_new (void)
{
  EggToolButton *button;

  button = g_object_new (EGG_TYPE_TOGGLE_TOOL_BUTTON,
			 NULL);
  
  return EGG_TOOL_ITEM (button);
}

EggToolItem *
egg_toggle_tool_button_new_from_stock (const gchar *stock_id)
{
  EggToolButton *button;

  g_return_val_if_fail (stock_id != NULL, NULL);
  
  button = g_object_new (EGG_TYPE_TOGGLE_TOOL_BUTTON,
			 "stock_id", stock_id,
			 "use_underline", TRUE,
			 NULL);
  
  return EGG_TOOL_ITEM (button);
}

void
egg_toggle_tool_button_set_active (EggToggleToolButton *button,
				   gboolean is_active)
{
  g_return_if_fail (EGG_IS_TOGGLE_TOOL_BUTTON (button));

  is_active = is_active != FALSE;

  if (button->active != is_active)
    gtk_button_clicked (GTK_BUTTON (EGG_TOOL_BUTTON (button)->button));
}

gboolean
egg_toggle_tool_button_get_active (EggToggleToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOGGLE_TOOL_BUTTON (button), FALSE);

  return button->active;
}

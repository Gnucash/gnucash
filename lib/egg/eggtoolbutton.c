/* eggtoolbutton.c
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

#include "eggtoolbutton.h"
#include <gtk/gtkbutton.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkiconfactory.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkvbox.h>

#include <string.h>

#ifndef _
#  define _(s) (s)
#endif

#define MENU_ID "egg-tool-button-menu-id"

enum {
  CLICKED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_LABEL,
  PROP_USE_UNDERLINE,
  PROP_LABEL_WIDGET,
  PROP_STOCK_ID,
  PROP_ICON_SET,
  PROP_ICON_WIDGET,
};

static void egg_tool_button_init          (EggToolButton      *button,
					   EggToolButtonClass *klass);
static void egg_tool_button_class_init    (EggToolButtonClass *klass);
static void egg_tool_button_size_request  (GtkWidget          *widget,
					   GtkRequisition     *requisition);
static void egg_tool_button_size_allocate (GtkWidget          *widget,
					   GtkAllocation      *allocation);
static void egg_tool_button_set_property  (GObject            *object,
					   guint               prop_id,
					   const GValue       *value,
					   GParamSpec         *pspec);
static void egg_tool_button_get_property  (GObject            *object,
					   guint               prop_id,
					   GValue             *value,
					   GParamSpec         *pspec);
static void egg_tool_button_finalize      (GObject            *object);

static void egg_tool_button_toolbar_reconfigured (EggToolItem *tool_item);
static gboolean   egg_tool_button_create_menu_proxy (EggToolItem     *item);
static void       button_clicked                    (GtkWidget       *widget,
						     EggToolButton   *button);

static void egg_tool_button_construct_contents (EggToolItem *tool_item);
      
static GObjectClass *parent_class = NULL;
static guint         toolbutton_signals[LAST_SIGNAL] = { 0 };

GType
egg_tool_button_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggToolButtonClass),
	  (GBaseInitFunc) NULL,
	  (GBaseFinalizeFunc) NULL,
	  (GClassInitFunc) egg_tool_button_class_init,
	  (GClassFinalizeFunc) NULL,
	  NULL,
	  sizeof (EggToolButton),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) egg_tool_button_init,
	};

      type = g_type_register_static (EGG_TYPE_TOOL_ITEM,
				     "EggToolButton",
				     &type_info, 0);
    }
  return type;
}

static void
egg_tool_button_class_init (EggToolButtonClass *klass)
{
  GObjectClass *object_class;
  GtkWidgetClass *widget_class;
  EggToolItemClass *tool_item_class;
  
  parent_class = g_type_class_peek_parent (klass);
  
  object_class = (GObjectClass *)klass;
  widget_class = (GtkWidgetClass *)klass;
  tool_item_class = (EggToolItemClass *)klass;
  
  object_class->set_property = egg_tool_button_set_property;
  object_class->get_property = egg_tool_button_get_property;
  object_class->finalize = egg_tool_button_finalize;

  widget_class->size_request = egg_tool_button_size_request;
  widget_class->size_allocate = egg_tool_button_size_allocate;

  tool_item_class->create_menu_proxy = egg_tool_button_create_menu_proxy;
  tool_item_class->toolbar_reconfigured = egg_tool_button_toolbar_reconfigured;
  
  klass->button_type = GTK_TYPE_BUTTON;

  g_object_class_install_property (object_class,
				   PROP_LABEL,
				   g_param_spec_string ("label",
							_("Label"),
							_("Text to show in the item."),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_USE_UNDERLINE,
				   g_param_spec_boolean ("use_underline",
							 _("Use underline"),
							 _("Interpret underlines in the item label"),
							 FALSE,
							 G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_LABEL_WIDGET,
				   g_param_spec_object ("label_widget",
							_("Label widget"),
							_("Widget to use as the item label"),
							GTK_TYPE_WIDGET,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_STOCK_ID,
				   g_param_spec_string ("stock_id",
							_("Stock Id"),
							_("The stock icon displayed on the item"),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_ICON_SET,
				   g_param_spec_boxed ("icon_set",
						       _("Icon set"),
						       _("Icon set to use to draw the item's icon"),
						       GTK_TYPE_ICON_SET,
						       G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_ICON_WIDGET,
				   g_param_spec_object ("icon_widget",
							_("Icon widget"),
							_("Icon widget to display in the item"),
							GTK_TYPE_WIDGET,
							G_PARAM_READWRITE));

  toolbutton_signals[CLICKED] =
    g_signal_new ("clicked",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (EggToolButtonClass, clicked),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
}

static void
egg_tool_button_init (EggToolButton      *button,
		      EggToolButtonClass *klass)
{
  EggToolItem *toolitem = EGG_TOOL_ITEM (button);
  
  toolitem->homogeneous = TRUE;

  /* create button */
  button->button = g_object_new (klass->button_type, NULL);
#if 0
  /* FIXME: enable this when we can depend on gtk+ 2.3.0 */
  gtk_button_set_focus_on_click (button->button, FALSE);
#endif
  g_signal_connect_object (button->button, "clicked",
			   G_CALLBACK (button_clicked), button, 0);

  gtk_container_add (GTK_CONTAINER (button), button->button);
  gtk_widget_show (button->button);
}

static void
egg_tool_button_size_request (GtkWidget      *widget,
			      GtkRequisition *requisition)
{
  GtkWidget *child = GTK_BIN (widget)->child;

  if (child && GTK_WIDGET_VISIBLE (child))
    {
      gtk_widget_size_request (child, requisition);
    }
  else
    {
      requisition->width = 0;
      requisition->height = 0;
    }
  
  requisition->width += GTK_CONTAINER (widget)->border_width * 2;
  requisition->height += GTK_CONTAINER (widget)->border_width * 2;  
}

static void
egg_tool_button_size_allocate (GtkWidget     *widget,
			       GtkAllocation *allocation)
{
  EggToolItem *toolitem = EGG_TOOL_ITEM (widget);
  GtkAllocation child_allocation;
  gint border_width;
  GtkWidget *child = GTK_BIN (widget)->child;

  widget->allocation = *allocation;
  border_width = GTK_CONTAINER (widget)->border_width;

  if (toolitem->drag_window && GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (toolitem->drag_window,
                            widget->allocation.x + border_width,
                            widget->allocation.y + border_width,
                            widget->allocation.width - border_width * 2,
                            widget->allocation.height - border_width * 2);
  
  if (child && GTK_WIDGET_VISIBLE (child))
    {
      child_allocation.x = allocation->x + border_width;
      child_allocation.y = allocation->y + border_width;
      child_allocation.width = allocation->width - 2 * border_width;
      child_allocation.height = allocation->height - 2 * border_width;
      
      gtk_widget_size_allocate (child, &child_allocation);
    }
}

static gchar *
elide_underscores (const gchar *original)
{
  gchar *q, *result;
  const gchar *p;
  gboolean last_underscore;

  q = result = g_malloc (strlen (original) + 1);
  last_underscore = FALSE;
  
  for (p = original; *p; p++)
    {
      if (!last_underscore && *p == '_')
	last_underscore = TRUE;
      else
	{
	  last_underscore = FALSE;
	  *q++ = *p;
	}
    }
  
  *q = '\0';
  
  return result;
}

static void
egg_tool_button_construct_contents (EggToolItem *tool_item)
{
  EggToolButton *button = EGG_TOOL_BUTTON (tool_item);
  GtkWidget *label = NULL;
  GtkWidget *icon = NULL;
  GtkToolbarStyle style;
  gboolean need_label = FALSE;
  gboolean need_icon = FALSE;
  GtkIconSize icon_size;
  GtkWidget *box = NULL;

  if (egg_tool_item_get_proxy_menu_item (tool_item, MENU_ID))
    {
      /* Remove item, so it will be recreated on the next
       * create_proxy_menu_item()
       */
      egg_tool_item_set_proxy_menu_item (tool_item, MENU_ID, NULL);
    }
  
  if (button->icon_widget && button->icon_widget->parent)
    {
      gtk_container_remove (GTK_CONTAINER (button->icon_widget->parent),
			    button->icon_widget);
    }

  if (button->label_widget && button->label_widget->parent)
    {
      gtk_container_remove (GTK_CONTAINER (button->label_widget->parent),
			    button->label_widget);
    }

  if (GTK_BIN (button->button)->child)
    {
      gtk_container_remove (GTK_CONTAINER (button->button),
			    GTK_BIN (button->button)->child);
    }

  style = egg_tool_item_get_toolbar_style (EGG_TOOL_ITEM (button));
  
  if (style != GTK_TOOLBAR_TEXT)
    need_icon = TRUE;

  if (style != GTK_TOOLBAR_ICONS)
    need_label = TRUE;

  if (need_label)
    {
      if (button->label_widget)
	{
	  label = button->label_widget;
	}
      else
	{
	  GtkStockItem stock_item;
	  gboolean elide = TRUE;
	  gchar *label_text;

	  if (button->label_text)
	    {
	      label_text = button->label_text;
	      elide = button->use_underline;
	    }
	  else if (button->stock_id && gtk_stock_lookup (button->stock_id, &stock_item))
	    label_text = stock_item.label;
	  else
	    label_text = "";

	  if (elide)
	    label_text = elide_underscores (label_text);
	  else
	    label_text = g_strdup (label_text);

	  label = gtk_label_new (label_text);

	  g_free (label_text);
	  
	  gtk_widget_show (label);
	}
    }

  icon_size = egg_tool_item_get_icon_size (EGG_TOOL_ITEM (button));
  if (need_icon)
    {
      if (button->icon_set)
	{
	  icon = gtk_image_new_from_icon_set (button->icon_set, icon_size);
	  gtk_widget_show (icon);
	}
      else if (button->icon_widget)
	{
	  icon = button->icon_widget;
	  
	  if (GTK_IS_IMAGE (icon))
	    {
	      GtkImage *image = GTK_IMAGE (icon);
	      GtkImageType storage_type = gtk_image_get_storage_type (image);
	      
	      if (storage_type == GTK_IMAGE_STOCK)
		{
		  gchar *stock_id;
		  gtk_image_get_stock (image, &stock_id, NULL);

		  icon = gtk_image_new_from_stock (stock_id, icon_size);
		  gtk_widget_show (icon);
		}
	      else if (storage_type == GTK_IMAGE_ICON_SET)
		{
		  GtkIconSet *icon_set;
		  gtk_image_get_icon_set (image, &icon_set, NULL);
		  
		  icon = gtk_image_new_from_icon_set (icon_set, icon_size);
		  gtk_widget_show (icon);
		}
	    }
	}
      else if (button->stock_id)
	{
	  icon = gtk_image_new_from_stock (button->stock_id, icon_size);
	  gtk_widget_show (icon);
	}
    }

  switch (style)
    {
    case GTK_TOOLBAR_ICONS:
      if (icon)
	gtk_container_add (GTK_CONTAINER (button->button), icon);
      break;

    case GTK_TOOLBAR_BOTH:
      box = gtk_vbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (box), icon, TRUE, TRUE, 0);
      gtk_box_pack_start (GTK_BOX (box), label, FALSE, TRUE, 0);
      gtk_container_add (GTK_CONTAINER (button->button), box);
      break;

    case GTK_TOOLBAR_BOTH_HORIZ:
      box = gtk_hbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (box), icon, FALSE, TRUE, 0);
      gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);
      gtk_container_add (GTK_CONTAINER (button->button), box);
      break;

    case GTK_TOOLBAR_TEXT:
      gtk_container_add (GTK_CONTAINER (button->button), label);
      break;
    }

  if (box)
    gtk_widget_show (box);

  gtk_button_set_relief (GTK_BUTTON (button->button),
			 egg_tool_item_get_relief_style (EGG_TOOL_ITEM (button)));

  gtk_widget_queue_resize (GTK_WIDGET (button));
}

static void
egg_tool_button_set_property (GObject         *object,
			      guint            prop_id,
			      const GValue    *value,
			      GParamSpec      *pspec)
{
  EggToolButton *button = EGG_TOOL_BUTTON (object);
  
  switch (prop_id)
    {
    case PROP_LABEL:
      egg_tool_button_set_label (button, g_value_get_string (value));
      break;
    case PROP_USE_UNDERLINE:
      egg_tool_button_set_use_underline (button, g_value_get_boolean (value));
      break;
    case PROP_LABEL_WIDGET:
      egg_tool_button_set_label_widget (button, g_value_get_object (value));
      break;
    case PROP_STOCK_ID:
      egg_tool_button_set_stock_id (button, g_value_get_string (value));
      break;
    case PROP_ICON_SET:
      egg_tool_button_set_icon_set (button, g_value_get_boxed (value));
      break;
    case PROP_ICON_WIDGET:
      egg_tool_button_set_icon_widget (button, g_value_get_object (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
egg_tool_button_get_property (GObject         *object,
			      guint            prop_id,
			      GValue          *value,
			      GParamSpec      *pspec)
{
  EggToolButton *button = EGG_TOOL_BUTTON (object);

  switch (prop_id)
    {
    case PROP_LABEL:
      g_value_set_string (value, egg_tool_button_get_label (button));
      break;
    case PROP_LABEL_WIDGET:
      g_value_set_object (value, egg_tool_button_get_label_widget (button));
      break;
    case PROP_USE_UNDERLINE:
      g_value_set_boolean (value, egg_tool_button_get_use_underline (button));
      break;
    case PROP_STOCK_ID:
      g_value_set_string (value, button->stock_id);
      break;
    case PROP_ICON_SET:
      g_value_set_boxed (value, egg_tool_button_get_icon_set (button));
      break;
    case PROP_ICON_WIDGET:
      g_value_set_object (value, button->icon_widget);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
egg_tool_button_finalize (GObject *object)
{
  EggToolButton *button = EGG_TOOL_BUTTON (object);

  g_free (button->stock_id);
  button->stock_id = NULL;

  parent_class->finalize (object);
}

static gboolean
egg_tool_button_create_menu_proxy (EggToolItem *item)
{
  EggToolButton *button = EGG_TOOL_BUTTON (item);
  GtkWidget *menu_item;
  GtkWidget *menu_image = NULL;
  GtkStockItem stock_item;
  gboolean use_mnemonic = TRUE;
  const char *label = "";

  if (button->label_widget && GTK_IS_LABEL (button->label_widget))
    label = gtk_label_get_label (GTK_LABEL (button->label_widget));
  else if (button->label_text)
    {
      label = button->label_text;
      use_mnemonic = button->use_underline;
    }
  else if (button->stock_id && gtk_stock_lookup (button->stock_id, &stock_item))
    label = stock_item.label;
  
  if (use_mnemonic)
    menu_item = gtk_image_menu_item_new_with_mnemonic (label);
  else
    menu_item = gtk_image_menu_item_new_with_label (label);

  if (button->icon_set)
    {
      menu_image = gtk_image_new_from_icon_set (button->icon_set, GTK_ICON_SIZE_MENU);
    }
  else if (button->icon_widget && GTK_IS_IMAGE (button->icon_widget))
    {
      GtkImage *image = GTK_IMAGE (button->icon_widget);
      GtkImageType storage_type = gtk_image_get_storage_type (image);
      
      if (storage_type == GTK_IMAGE_STOCK)
	{
	  gchar *stock_id;
	  gtk_image_get_stock (image, &stock_id, NULL);
	  menu_image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_MENU);
	}
      else if (storage_type == GTK_IMAGE_ICON_SET)
	{
	  GtkIconSet *icon_set;
	  gtk_image_get_icon_set (image, &icon_set, NULL);
	  menu_image = gtk_image_new_from_icon_set (icon_set, GTK_ICON_SIZE_MENU);
	}
    }
  else if (button->stock_id)
    {
      menu_image = gtk_image_new_from_stock (button->stock_id, GTK_ICON_SIZE_MENU);
    }

  if (menu_image)
    gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu_item), menu_image);

  g_signal_connect_closure_by_id (menu_item,
				  g_signal_lookup ("activate", G_OBJECT_TYPE (menu_item)), 0,
				  g_cclosure_new_object_swap (G_CALLBACK (gtk_button_clicked),
							      G_OBJECT (EGG_TOOL_BUTTON (button)->button)),
				  FALSE);

  egg_tool_item_set_proxy_menu_item (EGG_TOOL_ITEM (button), MENU_ID, menu_item);
  
  return TRUE;
}

static void
button_clicked (GtkWidget     *widget,
		EggToolButton *button)
{
  g_signal_emit_by_name (button, "clicked");
}

static void
egg_tool_button_toolbar_reconfigured (EggToolItem *tool_item)
{
  egg_tool_button_construct_contents (tool_item);
}

EggToolItem *
egg_tool_button_new_from_stock (const gchar *stock_id)
{
  EggToolButton *button;

  g_return_val_if_fail (stock_id != NULL, NULL);
    
  button = g_object_new (EGG_TYPE_TOOL_BUTTON,
			 "stock_id", stock_id,
			 NULL);

  return EGG_TOOL_ITEM (button);
}

EggToolItem *
egg_tool_button_new (void)
{
  EggToolButton *button;
  
  button = g_object_new (EGG_TYPE_TOOL_BUTTON,
			 NULL);

  return EGG_TOOL_ITEM (button);  
}

void
egg_tool_button_set_label (EggToolButton *button,
			   const gchar   *label)
{
  gchar *old_label;
  
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));

  old_label = button->label_text;

  button->label_text = g_strdup (label);
  egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));
      
  g_object_notify (G_OBJECT (button), "label");

  if (old_label)
    g_free (old_label);
}

G_CONST_RETURN gchar *
egg_tool_button_get_label (EggToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOOL_BUTTON (button), NULL);

  return button->label_text;
}

void
egg_tool_button_set_use_underline (EggToolButton *button,
				   gboolean       use_underline)
{
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));

  use_underline = use_underline != FALSE;

  if (use_underline != button->use_underline)
    {
      button->use_underline = use_underline;

      egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));

      g_object_notify (G_OBJECT (button), "use_underline");
    }
}

gboolean
egg_tool_button_get_use_underline (EggToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOOL_BUTTON (button), FALSE);

  return button->use_underline;
}

void
egg_tool_button_set_stock_id (EggToolButton *button,
			      const gchar   *stock_id)
{
  gchar *old_stock_id;
  
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));

  old_stock_id = button->stock_id;

  button->stock_id = g_strdup (stock_id);
  egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));
  
  g_object_notify (G_OBJECT (button), "stock_id");

  g_free (old_stock_id);
}

G_CONST_RETURN gchar *
egg_tool_button_get_stock_id (EggToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOOL_BUTTON (button), NULL);

  return button->stock_id;
}

void
egg_tool_button_set_icon_widget (EggToolButton *button,
				 GtkWidget     *icon)
{
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));
  g_return_if_fail (icon == NULL || GTK_IS_WIDGET (icon));

  if (icon != button->icon_widget)
    {
      g_object_freeze_notify (G_OBJECT (button));
      
      if (button->icon_widget)
	g_object_unref (G_OBJECT (button->icon_widget));

      if (icon)
	{
	  g_object_ref (icon);
	  gtk_object_sink (GTK_OBJECT (icon));
	}

      button->icon_widget = icon;
	
      if (button->icon_widget && button->icon_set)
	{
	  gtk_icon_set_unref (button->icon_set);
	  button->icon_set = NULL;
	  
	  g_object_notify (G_OBJECT (button), "icon_set");
	}

      egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));
      
      g_object_notify (G_OBJECT (button), "icon_widget");
      g_object_thaw_notify (G_OBJECT (button));
    }
}

void
egg_tool_button_set_label_widget (EggToolButton *button,
				  GtkWidget     *label_widget)
{
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));
  g_return_if_fail (label_widget == NULL || GTK_IS_WIDGET (label_widget));

  if (label_widget != button->label_widget)
    {
      if (button->label_widget)
	g_object_unref (button->label_widget);

      if (label_widget)
	{
	  g_object_ref (label_widget);
	  gtk_object_sink (GTK_OBJECT (label_widget));
	}

      button->label_widget = label_widget;

      egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));
      
      g_object_notify (G_OBJECT (button), "label_widget");
    }
}

GtkWidget *
egg_tool_button_get_label_widget (EggToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOOL_BUTTON (button), NULL);

  return button->label_widget;
}

GtkWidget *
egg_tool_button_get_icon_widget (EggToolButton *button)
{
  g_return_val_if_fail (GTK_IS_BUTTON (button), NULL);

  return button->icon_widget;
}

void
egg_tool_button_set_icon_set (EggToolButton *button,
			      GtkIconSet    *icon_set)
{
  g_return_if_fail (EGG_IS_TOOL_BUTTON (button));

  if (icon_set != button->icon_set)
    {
      g_object_freeze_notify (G_OBJECT (button));

      if (button->icon_set)
	gtk_icon_set_unref (button->icon_set);

      button->icon_set = icon_set;

      if (button->icon_set && button->icon_widget)
	{
	  g_object_unref (button->icon_widget);
	  button->icon_widget = NULL;

	  g_object_notify (G_OBJECT (button->icon_widget), "icon_widget");
	}

      egg_tool_button_construct_contents (EGG_TOOL_ITEM (button));
      
      g_object_notify (G_OBJECT (button), "icon_set");
      g_object_thaw_notify (G_OBJECT (button));
    }
}

GtkIconSet *
egg_tool_button_get_icon_set (EggToolButton *button)
{
  g_return_val_if_fail (EGG_IS_TOOL_BUTTON (button), NULL);
  
  return button->icon_set;
}

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

#include "eggtoolitem.h"
#include "eggmarshalers.h"
#include "eggtoolbar.h"
#include <gtk/gtkseparatormenuitem.h>
#include <string.h>

#ifndef _
#  define _(s) (s)
#endif

#define MENU_ID "egg-tool-item-menu-id"

enum {
  CREATE_MENU_PROXY,
  TOOLBAR_RECONFIGURED,
  SET_TOOLTIP,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_VISIBLE_HORIZONTAL,
  PROP_VISIBLE_VERTICAL,
};

static void egg_tool_item_init       (EggToolItem *toolitem);
static void egg_tool_item_class_init (EggToolItemClass *class);
static void egg_tool_item_finalize    (GObject *object);
static void egg_tool_item_parent_set   (GtkWidget   *toolitem,
				        GtkWidget   *parent);
static void egg_tool_item_set_property (GObject         *object,
					guint            prop_id,
					const GValue    *value,
					GParamSpec      *pspec);
static void egg_tool_item_get_property (GObject         *object,
					guint            prop_id,
					GValue          *value,
					GParamSpec      *pspec);
static void egg_tool_item_realize       (GtkWidget      *widget);
static void egg_tool_item_unrealize     (GtkWidget      *widget);
static void egg_tool_item_map           (GtkWidget      *widget);
static void egg_tool_item_unmap         (GtkWidget      *widget);
static void egg_tool_item_size_request  (GtkWidget      *widget,
					 GtkRequisition *requisition);
static void egg_tool_item_size_allocate (GtkWidget      *widget,
					 GtkAllocation  *allocation);
static gboolean egg_tool_item_real_set_tooltip (EggToolItem *tool_item,
						GtkTooltips *tooltips,
						const gchar *tip_text,
						const gchar *tip_private);

static gboolean egg_tool_item_create_menu_proxy (EggToolItem *item);


static GObjectClass *parent_class = NULL;
static guint         toolitem_signals[LAST_SIGNAL] = { 0 };

GType
egg_tool_item_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggToolItemClass),
	  (GBaseInitFunc) NULL,
	  (GBaseFinalizeFunc) NULL,
	  (GClassInitFunc) egg_tool_item_class_init,
	  (GClassFinalizeFunc) NULL,
	  NULL,
        
	  sizeof (EggToolItem),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) egg_tool_item_init,
	};

      type = g_type_register_static (GTK_TYPE_BIN,
				     "EggToolItem",
				     &type_info, 0);
    }
  return type;
}

static gboolean
egg_boolean_handled_accumulator (GSignalInvocationHint *ihint,
				  GValue                *return_accu,
				  const GValue          *handler_return,
				  gpointer               dummy)
{
  gboolean continue_emission;
  gboolean signal_handled;
  
  signal_handled = g_value_get_boolean (handler_return);
  g_value_set_boolean (return_accu, signal_handled);
  continue_emission = !signal_handled;
  
  return continue_emission;
}

static void
egg_tool_item_class_init (EggToolItemClass *klass)
{
  GObjectClass *object_class;
  GtkWidgetClass *widget_class;
  
  parent_class = g_type_class_peek_parent (klass);
  object_class = (GObjectClass *)klass;
  widget_class = (GtkWidgetClass *)klass;
  
  object_class->set_property = egg_tool_item_set_property;
  object_class->get_property = egg_tool_item_get_property;
  object_class->finalize = egg_tool_item_finalize;

  widget_class->realize       = egg_tool_item_realize;
  widget_class->unrealize     = egg_tool_item_unrealize;
  widget_class->map           = egg_tool_item_map;
  widget_class->unmap         = egg_tool_item_unmap;
  widget_class->size_request  = egg_tool_item_size_request;
  widget_class->size_allocate = egg_tool_item_size_allocate;
  widget_class->parent_set    = egg_tool_item_parent_set;

  klass->create_menu_proxy = egg_tool_item_create_menu_proxy;
  klass->set_tooltip       = egg_tool_item_real_set_tooltip;
  
  g_object_class_install_property (object_class,
				   PROP_VISIBLE_HORIZONTAL,
				   g_param_spec_boolean ("visible_horizontal",
							 _("Visible when horizontal"),
							 _("Whether the toolbar item is visible when the toolbar is in a horizontal orientation."),
							 TRUE,
							 G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_VISIBLE_VERTICAL,
				   g_param_spec_boolean ("visible_vertical",
							 _("Visible when vertical"),
							 _("Whether the toolbar item is visible when the toolbar is in a vertical orientation."),
							 TRUE,
							 G_PARAM_READWRITE));
  toolitem_signals[CREATE_MENU_PROXY] =
    g_signal_new ("create_menu_proxy",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolItemClass, create_menu_proxy),
		  egg_boolean_handled_accumulator, NULL, /* FIXME: use gtk_boolean_handled() when
							  * we are added to gtk+
							  */
		  _egg_marshal_BOOLEAN__VOID,
		  G_TYPE_BOOLEAN, 0);
  toolitem_signals[TOOLBAR_RECONFIGURED] =
    g_signal_new ("toolbar_reconfigured",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolItemClass, toolbar_reconfigured),
		  NULL, NULL,
		  _egg_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
  toolitem_signals[SET_TOOLTIP] =
    g_signal_new ("set_tooltip",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggToolItemClass, set_tooltip),
		  egg_boolean_handled_accumulator, NULL, /* FIXME: use gtk_boolean_handled() when
							  * we are added to gtk+
							  */
		  _egg_marshal_BOOLEAN__OBJECT_STRING_STRING,
		  G_TYPE_BOOLEAN, 3,
		  GTK_TYPE_TOOLTIPS,
		  G_TYPE_STRING,
		  G_TYPE_STRING);		  
}

static void
egg_tool_item_init (EggToolItem *toolitem)
{
  GTK_WIDGET_UNSET_FLAGS (toolitem, GTK_CAN_FOCUS);  

  toolitem->visible_horizontal = TRUE;
  toolitem->visible_vertical = TRUE;
  toolitem->homogeneous = FALSE;
  toolitem->expand = FALSE;
}

static void
egg_tool_item_finalize (GObject *object)
{
  EggToolItem *item = EGG_TOOL_ITEM (object);

  if (item->menu_item)
    g_object_unref (item->menu_item);
  
  if (G_OBJECT_CLASS (parent_class)->finalize)
    G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
egg_tool_item_parent_set   (GtkWidget   *toolitem,
			    GtkWidget   *prev_parent)
{
  egg_tool_item_toolbar_reconfigured (EGG_TOOL_ITEM (toolitem));
}

static void
egg_tool_item_set_property (GObject      *object,
			    guint         prop_id,
			    const GValue *value,
			    GParamSpec   *pspec)
{
  EggToolItem *toolitem = EGG_TOOL_ITEM (object);

  switch (prop_id)
    {
    case PROP_VISIBLE_HORIZONTAL:
      egg_tool_item_set_visible_horizontal (toolitem, g_value_get_boolean (value));
      break;
    case PROP_VISIBLE_VERTICAL:
      egg_tool_item_set_visible_horizontal (toolitem, g_value_get_boolean (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
egg_tool_item_get_property (GObject    *object,
			    guint       prop_id,
			    GValue     *value,
			    GParamSpec *pspec)
{
  EggToolItem *toolitem = EGG_TOOL_ITEM (object);

  switch (prop_id)
    {
    case PROP_VISIBLE_HORIZONTAL:
      g_value_set_boolean (value, toolitem->visible_horizontal);
      break;
    case PROP_VISIBLE_VERTICAL:
      g_value_set_boolean (value, toolitem->visible_vertical);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
create_drag_window (EggToolItem *toolitem)
{
  GtkWidget *widget;
  GdkWindowAttr attributes;
  gint attributes_mask, border_width;

  g_return_if_fail (toolitem->use_drag_window == TRUE);

  widget = GTK_WIDGET (toolitem);
  border_width = GTK_CONTAINER (toolitem)->border_width;

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x + border_width;
  attributes.y = widget->allocation.y + border_width;
  attributes.width = widget->allocation.width - border_width * 2;
  attributes.height = widget->allocation.height - border_width * 2;
  attributes.wclass = GDK_INPUT_ONLY;
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= (GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y;

  toolitem->drag_window = gdk_window_new (gtk_widget_get_parent_window (widget),
					  &attributes, attributes_mask);
  gdk_window_set_user_data (toolitem->drag_window, toolitem);
}

static void
egg_tool_item_realize (GtkWidget *widget)
{
  EggToolItem *toolitem;

  toolitem = EGG_TOOL_ITEM (widget);
  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  widget->window = gtk_widget_get_parent_window (widget);
  g_object_ref (widget->window);

  if (toolitem->use_drag_window)
    create_drag_window(toolitem);

  widget->style = gtk_style_attach (widget->style, widget->window);
}

static void
destroy_drag_window (EggToolItem *toolitem)
{
  if (toolitem->drag_window)
    {
      gdk_window_set_user_data (toolitem->drag_window, NULL);
      gdk_window_destroy (toolitem->drag_window);
      toolitem->drag_window = NULL;
    }
}

static void
egg_tool_item_unrealize (GtkWidget *widget)
{
  EggToolItem *toolitem;

  toolitem = EGG_TOOL_ITEM (widget);

  destroy_drag_window (toolitem);
  
  GTK_WIDGET_CLASS (parent_class)->unrealize (widget);
}

static void
egg_tool_item_map (GtkWidget *widget)
{
  EggToolItem *toolitem;

  toolitem = EGG_TOOL_ITEM (widget);
  GTK_WIDGET_CLASS (parent_class)->map (widget);
  if (toolitem->drag_window)
    gdk_window_show (toolitem->drag_window);
}

static void
egg_tool_item_unmap (GtkWidget *widget)
{
  EggToolItem *toolitem;

  toolitem = EGG_TOOL_ITEM (widget);
  if (toolitem->drag_window)
    gdk_window_hide (toolitem->drag_window);
  GTK_WIDGET_CLASS (parent_class)->unmap (widget);
}

static void
egg_tool_item_size_request (GtkWidget      *widget,
			    GtkRequisition *requisition)
{
  GtkWidget *child = GTK_BIN (widget)->child;
  gint xthickness = widget->style->xthickness;
  gint ythickness = widget->style->ythickness;

  if (child && GTK_WIDGET_VISIBLE (child))
    {
      gtk_widget_size_request (child, requisition);
    }
  else
    {
      requisition->height = 0;
      requisition->width = 0;
    }
  
  requisition->width += (xthickness + GTK_CONTAINER (widget)->border_width) * 2;
  requisition->height += (ythickness + GTK_CONTAINER (widget)->border_width) * 2;  
}

static void
egg_tool_item_size_allocate (GtkWidget     *widget,
			     GtkAllocation *allocation)
{
  EggToolItem *toolitem = EGG_TOOL_ITEM (widget);
  GtkAllocation child_allocation;
  gint border_width;
  GtkWidget *child = GTK_BIN (widget)->child;

  widget->allocation = *allocation;
  border_width = GTK_CONTAINER (widget)->border_width;

  if (toolitem->drag_window)
    gdk_window_move_resize (toolitem->drag_window,
                            widget->allocation.x + border_width,
                            widget->allocation.y + border_width,
                            widget->allocation.width - border_width * 2,
                            widget->allocation.height - border_width * 2);
  
  if (child && GTK_WIDGET_VISIBLE (child))
    {
      gint xthickness = widget->style->xthickness;
      gint ythickness = widget->style->ythickness;
      
      child_allocation.x = allocation->x + border_width + xthickness;
      child_allocation.y = allocation->y + border_width + ythickness;
      child_allocation.width = allocation->width - 2 * (xthickness + border_width);
      child_allocation.height = allocation->height - 2 * (ythickness + border_width);
      
      gtk_widget_size_allocate (child, &child_allocation);
    }
}

static gboolean
egg_tool_item_create_menu_proxy (EggToolItem *item)
{
  if (!GTK_BIN (item)->child)
    {
      GtkWidget *menu_item = NULL;

      menu_item = gtk_separator_menu_item_new();

      egg_tool_item_set_proxy_menu_item (item, MENU_ID, menu_item);

      return TRUE;
    }
  
  return FALSE;
}

EggToolItem *
egg_tool_item_new (void)
{
  EggToolItem *item;

  item = g_object_new (EGG_TYPE_TOOL_ITEM, NULL);

  return item;
}

GtkIconSize
egg_tool_item_get_icon_size (EggToolItem *tool_item)
{
  GtkWidget *parent;

  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), GTK_ICON_SIZE_LARGE_TOOLBAR);

  parent = GTK_WIDGET (tool_item)->parent;
  if (!parent || !EGG_IS_TOOLBAR (parent))
    return GTK_ICON_SIZE_LARGE_TOOLBAR;

  return egg_toolbar_get_icon_size (EGG_TOOLBAR (parent));
}

GtkOrientation
egg_tool_item_get_orientation (EggToolItem *tool_item)
{
  GtkWidget *parent;
  
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), GTK_ORIENTATION_HORIZONTAL);

  parent = GTK_WIDGET (tool_item)->parent;
  if (!parent || !EGG_IS_TOOLBAR (parent))
    return GTK_ORIENTATION_HORIZONTAL;

  return egg_toolbar_get_orientation (EGG_TOOLBAR (parent));
}

GtkToolbarStyle
egg_tool_item_get_toolbar_style (EggToolItem *tool_item)
{
  GtkWidget *parent;
  
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), GTK_TOOLBAR_ICONS);

  parent = GTK_WIDGET (tool_item)->parent;
  if (!parent || !EGG_IS_TOOLBAR (parent))
    return GTK_TOOLBAR_ICONS;

  return egg_toolbar_get_style (EGG_TOOLBAR (parent));
}

GtkReliefStyle 
egg_tool_item_get_relief_style (EggToolItem *tool_item)
{
  GtkWidget *parent;
  
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), GTK_RELIEF_NONE);

  parent = GTK_WIDGET (tool_item)->parent;
  if (!parent || !EGG_IS_TOOLBAR (parent))
    return GTK_RELIEF_NONE;

  return egg_toolbar_get_relief_style (EGG_TOOLBAR (parent));
}

void
egg_tool_item_toolbar_reconfigured (EggToolItem *tool_item)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));

  g_signal_emit (tool_item, toolitem_signals[TOOLBAR_RECONFIGURED], 0);
}

void
egg_tool_item_set_expand (EggToolItem *tool_item,
			  gboolean     expand)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));
    
  expand = expand != FALSE;

  if (tool_item->expand != expand)
    {
      tool_item->expand = expand;
      gtk_widget_child_notify (GTK_WIDGET (tool_item), "expand");
      gtk_widget_queue_resize (GTK_WIDGET (tool_item));
    }
}

void
egg_tool_item_set_pack_end (EggToolItem *tool_item,
			    gboolean     pack_end)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));
    
  pack_end = pack_end != FALSE;

  if (tool_item->pack_end != pack_end)
    {
      tool_item->pack_end = pack_end;
      gtk_widget_child_notify (GTK_WIDGET (tool_item), "pack_end");
      gtk_widget_queue_resize (GTK_WIDGET (tool_item));
    }
}

void
egg_tool_item_set_homogeneous (EggToolItem *tool_item,
			       gboolean     homogeneous)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));
    
  homogeneous = homogeneous != FALSE;

  if (tool_item->homogeneous != homogeneous)
    {
      tool_item->homogeneous = homogeneous;
      gtk_widget_child_notify (GTK_WIDGET (tool_item), "homogeneous");
      gtk_widget_queue_resize (GTK_WIDGET (tool_item));
    }
}

static gboolean
egg_tool_item_real_set_tooltip (EggToolItem *tool_item,
				GtkTooltips *tooltips,
				const gchar *tip_text,
				const gchar *tip_private)
{
  GtkWidget *child = GTK_BIN (tool_item)->child;

  if (!child)
    return FALSE;

  gtk_tooltips_set_tip (tooltips, child, tip_text, tip_private);

  return TRUE;
}

void
egg_tool_item_set_tooltip (EggToolItem *tool_item,
			   GtkTooltips *tooltips,
			   const gchar *tip_text,
			   const gchar *tip_private)
{
  gboolean retval;
  
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));

  g_signal_emit (tool_item, toolitem_signals[SET_TOOLTIP], 0,
		 tooltips, tip_text, tip_private, &retval);
}

void
egg_tool_item_set_use_drag_window (EggToolItem *toolitem,
				   gboolean     use_drag_window)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (toolitem));

  use_drag_window = use_drag_window != FALSE;

  if (toolitem->use_drag_window != use_drag_window)
    {
      toolitem->use_drag_window = use_drag_window;
      
      if (use_drag_window)
	{
	  if (!toolitem->drag_window && GTK_WIDGET_REALIZED (toolitem))
	    {
	      create_drag_window(toolitem);
	      if (GTK_WIDGET_MAPPED (toolitem))
		gdk_window_show (toolitem->drag_window);
	    }
	}
      else
	{
	  destroy_drag_window (toolitem);
	}
    }
}

void
egg_tool_item_set_visible_horizontal (EggToolItem *toolitem,
				      gboolean     visible_horizontal)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (toolitem));

  visible_horizontal = visible_horizontal != FALSE;

  if (toolitem->visible_horizontal != visible_horizontal)
    {
      toolitem->visible_horizontal = visible_horizontal;

      g_object_notify (G_OBJECT (toolitem), "visible_horizontal");

      gtk_widget_queue_resize (GTK_WIDGET (toolitem));
    }
}

gboolean
egg_tool_item_get_visible_horizontal (EggToolItem *toolitem)
{
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (toolitem), FALSE);

  return toolitem->visible_horizontal;
}

void
egg_tool_item_set_visible_vertical (EggToolItem *toolitem,
				    gboolean     visible_vertical)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (toolitem));

  visible_vertical = visible_vertical != FALSE;

  if (toolitem->visible_vertical != visible_vertical)
    {
      toolitem->visible_vertical = visible_vertical;

      g_object_notify (G_OBJECT (toolitem), "visible_vertical");

      gtk_widget_queue_resize (GTK_WIDGET (toolitem));
    }
}

gboolean
egg_tool_item_get_visible_vertical (EggToolItem *toolitem)
{
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (toolitem), FALSE);

  return toolitem->visible_vertical;
}

GtkWidget *
egg_tool_item_retrieve_proxy_menu_item (EggToolItem *tool_item)
{
  gboolean retval;
  
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), NULL);

  g_signal_emit (tool_item, toolitem_signals[CREATE_MENU_PROXY], 0, &retval);
  
  return tool_item->menu_item;
}

GtkWidget *
egg_tool_item_get_proxy_menu_item (EggToolItem *tool_item,
				   const gchar *menu_item_id)
{
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (tool_item), NULL);
  g_return_val_if_fail (menu_item_id != NULL, NULL);

  if (tool_item->menu_item_id && strcmp (tool_item->menu_item_id, menu_item_id) == 0)
    return tool_item->menu_item;

  return NULL;
}

void
egg_tool_item_set_proxy_menu_item (EggToolItem *tool_item,
				   const gchar *menu_item_id,
				   GtkWidget   *menu_item)
{
  g_return_if_fail (EGG_IS_TOOL_ITEM (tool_item));
  g_return_if_fail (menu_item == NULL || GTK_IS_MENU_ITEM (menu_item));
  g_return_if_fail (menu_item_id != NULL);

  if (tool_item->menu_item_id)
    g_free (tool_item->menu_item_id);
      
  tool_item->menu_item_id = g_strdup (menu_item_id);

  if (tool_item->menu_item != menu_item)
    {
      if (tool_item->menu_item)
	g_object_unref (G_OBJECT (tool_item->menu_item));
      
      if (menu_item)
	{
	  g_object_ref (menu_item);
	  gtk_object_sink (GTK_OBJECT (menu_item));
	}
      
      tool_item->menu_item = menu_item;
    }
}

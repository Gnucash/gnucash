/*
 * go-optionmenu.c
 *
 * Copyright (C) 2002 Andreas J. Guelzow <aguelzow@taliesin.ca>
 *
 * based extensively on:
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * Modified by the GTK+ Team and others 1997-2000.  See the GTK AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 *
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
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <goffice/goffice-config.h>
#include "go-optionmenu.h"

#include <gtk/gtkmenu.h>
#include <gtk/gtkmenuitem.h>
#include <gtk/gtkcheckmenuitem.h>
#include <gdk/gdkkeysyms.h>
#include <glib/gi18n.h>

#define CHILD_LEFT_SPACING        4
#define CHILD_RIGHT_SPACING       1
#define CHILD_TOP_SPACING         1
#define CHILD_BOTTOM_SPACING      1

typedef struct _GOOptionMenuProps GOOptionMenuProps;

struct _GOOptionMenuProps
{
  gboolean interior_focus;
  GtkRequisition indicator_size;
  GtkBorder indicator_spacing;
  gint focus_width;
  gint focus_pad;
};

static const GOOptionMenuProps default_props = {
  TRUE,
  { 7, 13 },
  { 7, 5, 2, 2 },		/* Left, right, top, bottom */
  1,
  0
};

static void go_option_menu_class_init      (GOOptionMenuClass *klass);
static void go_option_menu_init            (GOOptionMenu      *option_menu);
static void go_option_menu_destroy         (GtkObject          *object);
static void go_option_menu_set_property    (GObject            *object,
						  guint               prop_id,
						  const GValue       *value,
						  GParamSpec         *pspec);
static void go_option_menu_get_property    (GObject            *object,
						  guint               prop_id,
						  GValue             *value,
						  GParamSpec         *pspec);
static void go_option_menu_size_request    (GtkWidget          *widget,
						  GtkRequisition     *requisition);
static void go_option_menu_size_allocate   (GtkWidget          *widget,
						  GtkAllocation      *allocation);
static void go_option_menu_paint           (GtkWidget          *widget,
						  GdkRectangle       *area);
static gint go_option_menu_expose          (GtkWidget          *widget,
						  GdkEventExpose     *event);
static gint go_option_menu_button_press    (GtkWidget          *widget,
						  GdkEventButton     *event);
static gint go_option_menu_key_press       (GtkWidget          *widget,
						  GdkEventKey        *event);
static void go_option_menu_selection_done  (GtkMenu            *menu,
						  GOOptionMenu *option_menu);
static void go_option_menu_update_contents (GOOptionMenu *option_menu, GtkMenu *menu);
static void go_option_menu_remove_contents (GOOptionMenu *option_menu);
static void go_option_menu_calc_size       (GOOptionMenu *option_menu);
static void go_option_menu_position        (GtkMenu            *menu,
						  gint               *x,
						  gint               *y,
						  gint               *scroll_offet,
						  gpointer            user_data);
static void go_option_menu_show_all        (GtkWidget          *widget);
static void go_option_menu_hide_all        (GtkWidget          *widget);
static gboolean go_option_menu_mnemonic_activate (GtkWidget    *widget,
							gboolean      group_cycling);
static GType go_option_menu_child_type   (GtkContainer       *container);
static gint go_option_menu_scroll_event    (GtkWidget          *widget,
						  GdkEventScroll     *event);

enum
{
  CHANGED,
  LAST_SIGNAL
};

enum
{
  PROP_0,
  PROP_MENU,
  LAST_PROP
};

static GtkButtonClass *parent_class = NULL;
static guint           signals[LAST_SIGNAL] = { 0 };


GType
go_option_menu_get_type (void)
{
  static GType option_menu_type = 0;

  if (!option_menu_type)
    {
      static const GTypeInfo option_menu_info =
      {
	sizeof (GOOptionMenuClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) go_option_menu_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (GOOptionMenu),
	0,		/* n_preallocs */
	(GInstanceInitFunc) go_option_menu_init,
      };

      option_menu_type =
	g_type_register_static (GTK_TYPE_BUTTON, "GOOptionMenu",
				&option_menu_info, 0);
    }

  return option_menu_type;
}

static void
go_option_menu_class_init (GOOptionMenuClass *class)
{
  GObjectClass *gobject_class;
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkButtonClass *button_class;
  GtkContainerClass *container_class;

  gobject_class = (GObjectClass*) class;
  object_class = (GtkObjectClass*) class;
  widget_class = (GtkWidgetClass*) class;
  button_class = (GtkButtonClass*) class;
  container_class = (GtkContainerClass*) class;

  parent_class = g_type_class_peek_parent (class);

  signals[CHANGED] =
    g_signal_new ("changed",
                  G_OBJECT_CLASS_TYPE (class),
                  G_SIGNAL_RUN_LAST,
                  G_STRUCT_OFFSET (GOOptionMenuClass, changed),
                  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);

  gobject_class->set_property = go_option_menu_set_property;
  gobject_class->get_property = go_option_menu_get_property;
  object_class->destroy = go_option_menu_destroy;
  
  widget_class->size_request = go_option_menu_size_request;
  widget_class->size_allocate = go_option_menu_size_allocate;
  widget_class->expose_event = go_option_menu_expose;
  widget_class->button_press_event = go_option_menu_button_press;
  widget_class->key_press_event = go_option_menu_key_press;
  widget_class->scroll_event = go_option_menu_scroll_event;
  widget_class->show_all = go_option_menu_show_all;
  widget_class->hide_all = go_option_menu_hide_all;
  widget_class->mnemonic_activate = go_option_menu_mnemonic_activate;

  container_class->child_type = go_option_menu_child_type;

  g_object_class_install_property (gobject_class,
                                   PROP_MENU,
                                   g_param_spec_object ("menu",
                                                        _("Menu"),
                                                        _("The menu of options"),
                                                        GTK_TYPE_MENU,
                                                        G_PARAM_READABLE | G_PARAM_WRITABLE));
  
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_boxed ("indicator_size",
							       _("Indicator Size"),
							       _("Size of dropdown indicator"),
							       GTK_TYPE_REQUISITION,
							       G_PARAM_READABLE));
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_boxed ("indicator_spacing",
							       _("Indicator Spacing"),
							       _("Spacing around indicator"),
							       GTK_TYPE_BORDER,
							       G_PARAM_READABLE));
}

static GType
go_option_menu_child_type (GtkContainer       *container)
{
  return G_TYPE_NONE;
}

static void
go_option_menu_init (GOOptionMenu *option_menu)
{
  GTK_WIDGET_SET_FLAGS (option_menu, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS (option_menu, GTK_CAN_DEFAULT | GTK_RECEIVES_DEFAULT);

  option_menu->menu = NULL;
  option_menu->select_menu = NULL;
  option_menu->menu_item = NULL;
  option_menu->old_menu_item = NULL;
  option_menu->last_signaled_menu_item = NULL;
  option_menu->selection = NULL;
  option_menu->new_selection = TRUE;
  option_menu->width = 0;
  option_menu->height = 0;
}

GtkWidget*
go_option_menu_new (void)
{
  return g_object_new (GO_TYPE_OPTION_MENU, NULL);
}

GtkWidget*
go_option_menu_get_menu (GOOptionMenu *option_menu)
{
  g_return_val_if_fail (GO_IS_OPTION_MENU (option_menu), NULL);

  return option_menu->menu;
}

static void
go_option_menu_detacher (GtkWidget     *widget,
			  GtkMenu	*menu)
{
  GOOptionMenu *option_menu;

  g_return_if_fail (GO_IS_OPTION_MENU (widget));

  option_menu = GO_OPTION_MENU (widget);
  g_return_if_fail (option_menu->menu == (GtkWidget*) menu);

  go_option_menu_remove_contents (option_menu);
  g_signal_handlers_disconnect_by_func (option_menu->menu,
					go_option_menu_selection_done,
					option_menu);
  g_signal_handlers_disconnect_by_func (option_menu->menu,
					go_option_menu_calc_size,
					option_menu);
  
  option_menu->menu = NULL;
  g_object_notify (G_OBJECT (option_menu), "menu");
}

static void connect_menu_signals (GtkMenu *menu, gpointer data);

static void
connect_menu_signals_to_submenu (GtkMenuItem *item, gpointer data)
{
	GtkMenu *menu = GTK_MENU(gtk_menu_item_get_submenu (item));
	
	if (menu)
		connect_menu_signals (menu, data);
}


static void
connect_menu_signals (GtkMenu *menu, gpointer data)
{
	GList *children;
	
	g_signal_connect_after (menu, "selection_done",
				G_CALLBACK (go_option_menu_selection_done),
				data);
	children = gtk_container_get_children (GTK_CONTAINER(menu));
	g_list_foreach (children, (GFunc)connect_menu_signals_to_submenu, data);
	g_list_free (children);
}


void
go_option_menu_set_menu (GOOptionMenu *option_menu,
			       GtkWidget *menu)
{
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));
  g_return_if_fail (GTK_IS_MENU (menu));

  if (option_menu->menu != menu)
    {
      go_option_menu_remove_menu (option_menu);

      option_menu->menu = menu;
      gtk_menu_attach_to_widget (GTK_MENU (menu),
				 GTK_WIDGET (option_menu),
				 go_option_menu_detacher);

      go_option_menu_calc_size (option_menu);

      connect_menu_signals (GTK_MENU(option_menu->menu), option_menu);
      
      g_signal_connect_swapped (option_menu->menu, "size_request",
				G_CALLBACK (go_option_menu_calc_size),
				option_menu);

      if (GTK_WIDGET (option_menu)->parent)
	gtk_widget_queue_resize (GTK_WIDGET (option_menu));

      go_option_menu_update_contents (option_menu, NULL);
      
      g_object_notify (G_OBJECT (option_menu), "menu");
    }
}

void
go_option_menu_remove_menu (GOOptionMenu *option_menu)
{
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  if (option_menu->menu)
    {
      if (GTK_MENU_SHELL (option_menu->menu)->active)
	g_signal_emit_by_name (option_menu->menu, "cancel", 0);
      
      gtk_menu_detach (GTK_MENU (option_menu->menu));
    }
}

void
go_option_menu_set_history (GOOptionMenu *option_menu, GSList *selection)
{
  GtkWidget *item;
  
  g_return_if_fail (selection != NULL);
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  if (option_menu->menu)
    {
	    GtkMenu *menu = GTK_MENU(option_menu->menu);
	    
	    while (selection->next) {
		    GList *children = gtk_container_get_children (GTK_CONTAINER(menu));
		    gint index = GPOINTER_TO_INT (selection->data);
		    item = GTK_WIDGET(g_list_nth_data (children, index));
		    menu = GTK_MENU(gtk_menu_item_get_submenu (GTK_MENU_ITEM(item)));
		    selection = selection->next;
		    g_list_free (children);
	    }
	    
	    gtk_menu_set_active (menu,  GPOINTER_TO_INT (selection->data));
	    item = gtk_menu_get_active (menu);
	    if (item != option_menu->menu_item)
		    go_option_menu_update_contents (option_menu, menu);
	    g_slist_free (option_menu->selection);
	    option_menu->selection = g_slist_copy (selection);
    }
}

/**
 * go_option_menu_get_history:
 * @option_menu: a #GOOptionMenu
 * 
 * Retrieves the currently selected menu item. The menu
 * items are numbered from top to bottom, starting with 0. 
 * 
 * Return value: the selected menu_item
 **/

GtkWidget *
go_option_menu_get_history (GOOptionMenu *option_menu)
{
     return GTK_WIDGET(option_menu->menu_item);
}




static void
go_option_menu_set_property (GObject            *object,
			      guint               prop_id,
			      const GValue       *value,
			      GParamSpec         *pspec)
{
  GOOptionMenu *option_menu = GO_OPTION_MENU (object);

  switch (prop_id)
    {
    case PROP_MENU:
      go_option_menu_set_menu (option_menu, g_value_get_object (value));
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
go_option_menu_get_property (GObject            *object,
				   guint               prop_id,
				   GValue             *value,
				   GParamSpec         *pspec)
{
  GOOptionMenu *option_menu = GO_OPTION_MENU (object);

  switch (prop_id)
    {
    case PROP_MENU:
      g_value_set_object (value, option_menu->menu);
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
go_option_menu_destroy (GtkObject *object)
{
  GOOptionMenu *option_menu;

  g_return_if_fail (GO_IS_OPTION_MENU (object));

  option_menu = GO_OPTION_MENU (object);

  if (option_menu->selection) {
	  g_slist_free (option_menu->selection);
	  option_menu->selection = NULL;
  }

  if (option_menu->menu)
    gtk_widget_destroy (option_menu->menu);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
go_option_menu_get_props (GOOptionMenu       *option_menu,
			   GOOptionMenuProps  *props)
{
  GtkRequisition *indicator_size;
  GtkBorder *indicator_spacing;
  
  gtk_widget_style_get (GTK_WIDGET (option_menu),
			"indicator_size", &indicator_size,
			"indicator_spacing", &indicator_spacing,
			"interior_focus", &props->interior_focus,
			"focus_line_width", &props->focus_width,
			"focus_padding", &props->focus_pad,
			NULL);

  if (indicator_size)
    props->indicator_size = *indicator_size;
  else
    props->indicator_size = default_props.indicator_size;

  if (indicator_spacing)
    props->indicator_spacing = *indicator_spacing;
  else
    props->indicator_spacing = default_props.indicator_spacing;

  g_free (indicator_size);
  g_free (indicator_spacing);
}

static void
go_option_menu_size_request (GtkWidget      *widget,
				   GtkRequisition *requisition)
{
  GOOptionMenu *option_menu = GO_OPTION_MENU (widget);
  GOOptionMenuProps props;
  gint tmp;
  GtkRequisition child_requisition = { 0, 0 };
      
  go_option_menu_get_props (option_menu, &props);
 
  if (GTK_BIN (option_menu)->child && GTK_WIDGET_VISIBLE (GTK_BIN (option_menu)->child))
    {
      gtk_widget_size_request (GTK_BIN (option_menu)->child, &child_requisition);

      requisition->width += child_requisition.width;
      requisition->height += child_requisition.height;
    }
  
  requisition->width = ((GTK_CONTAINER (widget)->border_width +
			 GTK_WIDGET (widget)->style->xthickness + props.focus_pad) * 2 +
			MAX (child_requisition.width, option_menu->width) +
 			props.indicator_size.width +
 			props.indicator_spacing.left + props.indicator_spacing.right +
			CHILD_LEFT_SPACING + CHILD_RIGHT_SPACING + props.focus_width * 2);
  requisition->height = ((GTK_CONTAINER (widget)->border_width +
			  GTK_WIDGET (widget)->style->ythickness + props.focus_pad) * 2 +
			 MAX (child_requisition.height, option_menu->height) +
			 CHILD_TOP_SPACING + CHILD_BOTTOM_SPACING + props.focus_width * 2);

  tmp = (requisition->height - MAX (child_requisition.height, option_menu->height) +
	 props.indicator_size.height + props.indicator_spacing.top + props.indicator_spacing.bottom);
  requisition->height = MAX (requisition->height, tmp);
}

static void
go_option_menu_size_allocate (GtkWidget     *widget,
				    GtkAllocation *allocation)
{
  GtkWidget *child;
  GtkButton *button = GTK_BUTTON (widget);
  GtkAllocation child_allocation;
  GOOptionMenuProps props;
  gint border_width;
    
  go_option_menu_get_props (GO_OPTION_MENU (widget), &props);
  border_width = GTK_CONTAINER (widget)->border_width;

  widget->allocation = *allocation;
  if (GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (button->event_window,
			    allocation->x + border_width, allocation->y + border_width,
			    allocation->width - border_width * 2, allocation->height - border_width * 2);

  child = GTK_BIN (widget)->child;
  if (child && GTK_WIDGET_VISIBLE (child))
    {
      gint xthickness = GTK_WIDGET (widget)->style->xthickness;
      gint ythickness = GTK_WIDGET (widget)->style->ythickness;
      
      child_allocation.x = widget->allocation.x + border_width + xthickness + props.focus_width + props.focus_pad + CHILD_LEFT_SPACING;
      child_allocation.y = widget->allocation.y + border_width + ythickness + props.focus_width + props.focus_pad + CHILD_TOP_SPACING;
      child_allocation.width = MAX (1, allocation->width - (border_width + xthickness + props.focus_width + props.focus_pad) * 2 -
				    props.indicator_size.width - props.indicator_spacing.left - props.indicator_spacing.right -
				    CHILD_LEFT_SPACING - CHILD_RIGHT_SPACING);
      child_allocation.height = MAX (1, allocation->height - (border_width + ythickness + props.focus_width + props.focus_pad) * 2 -
				     CHILD_TOP_SPACING - CHILD_BOTTOM_SPACING);

      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL) 
	child_allocation.x += props.indicator_size.width + props.indicator_spacing.left + props.indicator_spacing.right;

      gtk_widget_size_allocate (child, &child_allocation);
    }
}

static void
go_option_menu_paint (GtkWidget    *widget,
		       GdkRectangle *area)
{
  GdkRectangle button_area;
  GOOptionMenuProps props;
  gint border_width;
  gint tab_x;

  g_return_if_fail (GO_IS_OPTION_MENU (widget));
  g_return_if_fail (area != NULL);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      border_width = GTK_CONTAINER (widget)->border_width;
      go_option_menu_get_props (GO_OPTION_MENU (widget), &props);

      button_area.x = widget->allocation.x + border_width;
      button_area.y = widget->allocation.y + border_width;
      button_area.width = widget->allocation.width - 2 * border_width;
      button_area.height = widget->allocation.height - 2 * border_width;

      if (!props.interior_focus && GTK_WIDGET_HAS_FOCUS (widget))
	{
	  button_area.x += props.focus_width + props.focus_pad;
	  button_area.y += props.focus_width + props.focus_pad;
	  button_area.width -= 2 * (props.focus_width + props.focus_pad);
	  button_area.height -= 2 * (props.focus_width + props.focus_pad);
	}
      
      gtk_paint_box (widget->style, widget->window,
		     GTK_WIDGET_STATE (widget), GTK_SHADOW_OUT,
		     area, widget, "optionmenu",
		     button_area.x, button_area.y,
		     button_area.width, button_area.height);
      
      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL) 
	tab_x = button_area.x + props.indicator_spacing.right + 
	  widget->style->xthickness;
      else
	tab_x = button_area.x + button_area.width - 
	  props.indicator_size.width - props.indicator_spacing.right -
	  widget->style->xthickness;

      gtk_paint_tab (widget->style, widget->window,
		     GTK_WIDGET_STATE (widget), GTK_SHADOW_OUT,
		     area, widget, "optionmenutab",
		     tab_x,
		     button_area.y + (button_area.height - props.indicator_size.height) / 2,
		     props.indicator_size.width, props.indicator_size.height);
      
      if (GTK_WIDGET_HAS_FOCUS (widget))
	{
	  if (props.interior_focus)
	    {
	      button_area.x += widget->style->xthickness + props.focus_pad;
	      button_area.y += widget->style->ythickness + props.focus_pad;
	      button_area.width -= 2 * (widget->style->xthickness + props.focus_pad) +
		      props.indicator_spacing.left +
		      props.indicator_spacing.right +
		      props.indicator_size.width;
	      button_area.height -= 2 * (widget->style->ythickness + props.focus_pad);
	      if (gtk_widget_get_direction (GTK_WIDGET (widget)) == GTK_TEXT_DIR_RTL) 
		button_area.x += props.indicator_spacing.left +
		  props.indicator_spacing.right +
		  props.indicator_size.width;
	    }
	  else
	    {
	      button_area.x -= props.focus_width + props.focus_pad;
	      button_area.y -= props.focus_width + props.focus_pad;
	      button_area.width += 2 * (props.focus_width + props.focus_pad);
	      button_area.height += 2 * (props.focus_width + props.focus_pad);
	    }
	    
	  gtk_paint_focus (widget->style, widget->window, GTK_WIDGET_STATE (widget),
			   area, widget, "button",
			   button_area.x, 
			   button_area.y, 
			   button_area.width,
			   button_area.height);
	}
    }
}

static gint
go_option_menu_expose (GtkWidget      *widget,
			GdkEventExpose *event)
{
  g_return_val_if_fail (GO_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      go_option_menu_paint (widget, &event->area);

      if (GTK_BIN (widget)->child)
	gtk_container_propagate_expose (GTK_CONTAINER (widget),
					GTK_BIN (widget)->child,
					event);
    }

  return FALSE;
}

static gint
go_option_menu_button_press (GtkWidget      *widget,
			      GdkEventButton *event)
{
  GOOptionMenu *option_menu;
  GtkWidget *menu_item;

  g_return_val_if_fail (GO_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  option_menu = GO_OPTION_MENU (widget);

  if ((event->type == GDK_BUTTON_PRESS) &&
      (event->button == 1))
    {
	    option_menu->new_selection = TRUE;
	    option_menu->old_menu_item = option_menu->menu_item;
	    go_option_menu_remove_contents (option_menu);
	    gtk_menu_popup (GTK_MENU (option_menu->menu), NULL, NULL,
			    go_option_menu_position, option_menu,
			    event->button, event->time);
	    menu_item = gtk_menu_get_active (GTK_MENU (option_menu->menu));
	    if (menu_item)
		    gtk_menu_shell_select_item (GTK_MENU_SHELL (option_menu->menu), menu_item);
	    else
		    gtk_menu_shell_select_item (GTK_MENU_SHELL (option_menu->select_menu), 
						option_menu->old_menu_item);
		    
      return TRUE;
    }

  return FALSE;
}

static gint
go_option_menu_key_press (GtkWidget   *widget,
			   GdkEventKey *event)
{
  GOOptionMenu *option_menu;
  GtkWidget *menu_item;

  g_return_val_if_fail (GO_IS_OPTION_MENU (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  option_menu = GO_OPTION_MENU (widget);

  switch (event->keyval)
    {
    case GDK_KP_Space:
    case GDK_space:
	    option_menu->new_selection= TRUE;
	    option_menu->old_menu_item = option_menu->menu_item;
	    go_option_menu_remove_contents (option_menu);
	    gtk_menu_popup (GTK_MENU (option_menu->menu), NULL, NULL,
			    go_option_menu_position, option_menu,
			    0, event->time);
	    menu_item = gtk_menu_get_active (GTK_MENU (option_menu->menu));
	    if (menu_item)
		    gtk_menu_shell_select_item (GTK_MENU_SHELL (option_menu->menu), menu_item);
	    else
		    gtk_menu_shell_select_item (GTK_MENU_SHELL (option_menu->select_menu), 
						option_menu->old_menu_item);
	    return TRUE;
    }
  
  return FALSE;
}

static void
go_option_menu_selection_done (GtkMenu  *menu,
				GOOptionMenu *option_menu)
{
  g_return_if_fail (menu != NULL);
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  go_option_menu_update_contents (option_menu, menu);
}

static void
go_option_menu_changed (GOOptionMenu *option_menu)
{
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  if (option_menu->last_signaled_menu_item && GTK_IS_CHECK_MENU_ITEM(option_menu->last_signaled_menu_item))
	  gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(option_menu->last_signaled_menu_item),
					  FALSE);
  
  option_menu->last_signaled_menu_item = option_menu->menu_item;
  if (option_menu->last_signaled_menu_item
      && GTK_IS_CHECK_MENU_ITEM(option_menu->last_signaled_menu_item))
          gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM(option_menu->last_signaled_menu_item),
					  TRUE);
  
  g_signal_emit (option_menu, signals[CHANGED], 0);
}

static void
go_option_menu_select_first_sensitive (GOOptionMenu *option_menu)
{
}

static void
go_option_menu_item_state_changed_cb (GtkWidget      *widget,
				       GtkStateType    previous_state,
				       GOOptionMenu  *option_menu)
{
  GtkWidget *child = GTK_BIN (option_menu)->child;

  if (child && GTK_WIDGET_SENSITIVE (child) != GTK_WIDGET_IS_SENSITIVE (widget))
    gtk_widget_set_sensitive (child, GTK_WIDGET_IS_SENSITIVE (widget));
}

static void
go_option_menu_item_destroy_cb (GtkWidget     *widget,
				 GOOptionMenu *option_menu)
{
  GtkWidget *child = GTK_BIN (option_menu)->child;

  if (child)
    {
      g_object_ref (child);
      go_option_menu_remove_contents (option_menu);
      gtk_widget_destroy (child);
      g_object_unref (child);

      go_option_menu_select_first_sensitive (option_menu);
    }
}

static void
go_option_menu_update_contents_real (GOOptionMenu *option_menu, 
					   GtkMenu *menu, GtkMenuItem *menu_item)
{
	GtkWidget *child;
	GtkRequisition child_requisition;

	g_return_if_fail (option_menu != NULL);
	g_return_if_fail (menu_item != NULL);
	g_return_if_fail (menu != NULL);
	
	go_option_menu_remove_contents (option_menu);
	option_menu->menu_item = GTK_WIDGET(menu_item);
	option_menu->old_menu_item = NULL;
	option_menu->select_menu = GTK_WIDGET(menu);
	g_object_ref (option_menu->menu_item);
	child = GTK_BIN (option_menu->menu_item)->child;
	if (child)
	{
		if (!GTK_WIDGET_IS_SENSITIVE (option_menu->menu_item))
			gtk_widget_set_sensitive (child, FALSE);
		gtk_widget_reparent (child, GTK_WIDGET (option_menu));
	}
	
	g_signal_connect (option_menu->menu_item, "state_changed",
			  G_CALLBACK (go_option_menu_item_state_changed_cb), option_menu);
	g_signal_connect (option_menu->menu_item, "destroy",
			  G_CALLBACK (go_option_menu_item_destroy_cb), option_menu);
	
	gtk_widget_size_request (child, &child_requisition);
	gtk_widget_size_allocate (GTK_WIDGET (option_menu),
				  &(GTK_WIDGET (option_menu)->allocation));
	
	if (GTK_WIDGET_DRAWABLE (option_menu))
		gtk_widget_queue_draw (GTK_WIDGET (option_menu));
}



static void
go_option_menu_update_contents (GOOptionMenu *option_menu, GtkMenu *menu)
{
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  if (option_menu->menu)
  {
	  GtkWidget *old_item = option_menu->menu_item;
	  GtkWidget *new_item;
	  
	  new_item = menu ? gtk_menu_get_active (menu) :
		  gtk_menu_get_active (GTK_MENU(option_menu->menu));
	  
 	  if (new_item && !gtk_menu_item_get_submenu (GTK_MENU_ITEM(new_item))) {
		  g_slist_free (option_menu->selection);
		  option_menu->selection = NULL;
		  
		  go_option_menu_update_contents_real (option_menu, 
							     menu ? menu : GTK_MENU(option_menu->menu), 
							     GTK_MENU_ITEM(new_item));
	  } else {
		  if (option_menu->old_menu_item)
			  go_option_menu_update_contents_real (option_menu, 
								     GTK_MENU(option_menu->select_menu), 
								     GTK_MENU_ITEM(option_menu->old_menu_item));
	  }
	  if (new_item && !(gtk_menu_item_get_submenu (GTK_MENU_ITEM(new_item)) && option_menu->new_selection)) {
		  GList *children = gtk_container_get_children (GTK_CONTAINER(menu ? menu : GTK_MENU(option_menu->menu)));
		  option_menu->selection = g_slist_prepend(option_menu->selection, 
							   GINT_TO_POINTER(g_list_position
									   (children, g_list_find (children, new_item))));
		  g_list_free (children);
		  option_menu->new_selection = FALSE;
	  }
	  
	  if (old_item != option_menu->menu_item)
		  go_option_menu_changed (option_menu);
  }
}

static void
go_option_menu_remove_contents (GOOptionMenu *option_menu)
{
  GtkWidget *child;
  
  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  if (option_menu->menu_item)
    {
      child = GTK_BIN (option_menu)->child;
  
      if (child)
	{
	  gtk_widget_set_sensitive (child, TRUE);
	  gtk_widget_reparent (child, option_menu->menu_item);
	}

      g_signal_handlers_disconnect_by_func (option_menu->menu_item,
					    go_option_menu_item_state_changed_cb,
					    option_menu);				     
      g_signal_handlers_disconnect_by_func (option_menu->menu_item,
					    go_option_menu_item_destroy_cb,
					    option_menu);
      g_object_unref (option_menu->menu_item);
      option_menu->menu_item = NULL;
    }
}

static void
go_option_menu_calc_size (GOOptionMenu *option_menu)
{
  GtkWidget *child;
  GList *children;
  GtkRequisition child_requisition;
  gint old_width = option_menu->width;
  gint old_height = option_menu->height;

  g_return_if_fail (GO_IS_OPTION_MENU (option_menu));

  option_menu->width = 0;
  option_menu->height = 0;

  if (option_menu->menu)
    {
      children = GTK_MENU_SHELL (option_menu->menu)->children;
      while (children)
	{
	  child = children->data;
	  children = children->next;

	  if (GTK_WIDGET_VISIBLE (child))
	    {
	      GtkWidget *inner = GTK_BIN (child)->child;

	      if (inner)
		{
		  gtk_widget_size_request (inner, &child_requisition);

		  option_menu->width = MAX (option_menu->width, child_requisition.width);
		  option_menu->height = MAX (option_menu->height, child_requisition.height);
		}
	    }
	}
    }

  if (old_width != option_menu->width || old_height != option_menu->height)
    gtk_widget_queue_resize (GTK_WIDGET (option_menu));
}

static void
go_option_menu_position (GtkMenu  *menu,
			  gint     *x,
			  gint     *y,
			  gboolean *push_in,
			  gpointer  user_data)
{
  GOOptionMenu *option_menu;
  GtkWidget *active;
  GtkWidget *child;
  GtkWidget *widget;
  GtkRequisition requisition;
  GList *children;
  gint screen_width;
  gint menu_xpos;
  gint menu_ypos;
  gint menu_width;

  g_return_if_fail (GO_IS_OPTION_MENU (user_data));

  option_menu = GO_OPTION_MENU (user_data);
  widget = GTK_WIDGET (option_menu);

  gtk_widget_get_child_requisition (GTK_WIDGET (menu), &requisition);
  menu_width = requisition.width;

  active = gtk_menu_get_active (GTK_MENU (option_menu->menu));
  gdk_window_get_origin (widget->window, &menu_xpos, &menu_ypos);

  menu_xpos += widget->allocation.x;
  menu_ypos += widget->allocation.y + widget->allocation.height / 2 - 2;

  if (active != NULL)
    {
      gtk_widget_get_child_requisition (active, &requisition);
      menu_ypos -= requisition.height / 2;
    }

  children = GTK_MENU_SHELL (option_menu->menu)->children;
  while (children)
    {
      child = children->data;

      if (active == child)
	break;

      if (GTK_WIDGET_VISIBLE (child))
	{
	  gtk_widget_get_child_requisition (child, &requisition);
	  menu_ypos -= requisition.height;
	}

      children = children->next;
    }

  screen_width = gdk_screen_get_width (gtk_widget_get_screen (widget));
  
  if (menu_xpos < 0)
    menu_xpos = 0;
  else if ((menu_xpos + menu_width) > screen_width)
    menu_xpos -= ((menu_xpos + menu_width) - screen_width);

  *x = menu_xpos;
  *y = menu_ypos;
  *push_in = TRUE;
}


static void
go_option_menu_show_all (GtkWidget *widget)
{
  GtkContainer *container;
  GOOptionMenu *option_menu;
  
  g_return_if_fail (GO_IS_OPTION_MENU (widget));
  container = GTK_CONTAINER (widget);
  option_menu = GO_OPTION_MENU (widget);

  gtk_widget_show (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_show_all, NULL);
  if (option_menu->menu)
    gtk_widget_show_all (option_menu->menu);
  if (option_menu->menu_item)
    gtk_widget_show_all (option_menu->menu_item);
  if (option_menu->last_signaled_menu_item)
    gtk_widget_show_all (option_menu->last_signaled_menu_item);
  if (option_menu->select_menu)
	  gtk_widget_show_all (option_menu->select_menu);
}


static void
go_option_menu_hide_all (GtkWidget *widget)
{
  GtkContainer *container;

  g_return_if_fail (GO_IS_OPTION_MENU (widget));
  container = GTK_CONTAINER (widget);

  gtk_widget_hide (widget);
  gtk_container_foreach (container, (GtkCallback) gtk_widget_hide_all, NULL);
}

static gboolean
go_option_menu_mnemonic_activate (GtkWidget *widget,
				   gboolean   group_cycling)
{
  gtk_widget_grab_focus (widget);
  return TRUE;
}

static gint
go_option_menu_scroll_event (GtkWidget          *widget,
			      GdkEventScroll     *event)
{
  return TRUE;
}


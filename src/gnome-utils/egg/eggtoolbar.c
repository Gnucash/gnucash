/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 * GtkToolbar copyright (C) Federico Mena
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

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include <gtk/gtkarrow.h>
#include "eggtoolbar.h"
#include "eggradiotoolbutton.h"
#include "eggseparatortoolitem.h"
#include <gtk/gtkmenu.h>
#include <gtk/gtkradiobutton.h>
#include <gtk/gtktoolbar.h>
#include <gtk/gtkbindings.h>
#include <gdk/gdkkeysyms.h>
#include "eggmarshalers.h"
#include <gtk/gtkmain.h>
#include <gtk/gtkstock.h>
#include <gtk/gtklabel.h>
#include <string.h>

#define DEFAULT_IPADDING 1
#define DEFAULT_SPACE_SIZE  5
#define DEFAULT_SPACE_STYLE GTK_TOOLBAR_SPACE_LINE

#define DEFAULT_ICON_SIZE GTK_ICON_SIZE_LARGE_TOOLBAR
#define DEFAULT_TOOLBAR_STYLE GTK_TOOLBAR_BOTH

#define SPACE_LINE_DIVISION 10
#define SPACE_LINE_START    3
#define SPACE_LINE_END      7

#define TOOLBAR_ITEM_VISIBLE(item) \
(GTK_WIDGET_VISIBLE (item) && \
((toolbar->orientation == GTK_ORIENTATION_HORIZONTAL && item->visible_horizontal) || \
 (toolbar->orientation == GTK_ORIENTATION_VERTICAL && item->visible_vertical)))

#ifndef _
#  define _(s) (s)
#endif

enum {
  PROP_0,
  PROP_ORIENTATION,
  PROP_TOOLBAR_STYLE,
  PROP_SHOW_ARROW
};

enum {
  CHILD_PROP_0,
  CHILD_PROP_EXPAND,
  CHILD_PROP_HOMOGENEOUS,
  CHILD_PROP_PACK_END,
};

enum {
  ORIENTATION_CHANGED,
  STYLE_CHANGED,
  POPUP_CONTEXT_MENU,
  MOVE_FOCUS,
  FOCUS_HOME,
  FOCUS_END,
  LAST_SIGNAL
};

static void egg_toolbar_init       (EggToolbar      *toolbar);
static void egg_toolbar_class_init (EggToolbarClass *klass);

static void egg_toolbar_set_property (GObject      *object,
				      guint         prop_id,
				      const GValue *value,
				      GParamSpec   *pspec);
static void egg_toolbar_get_property (GObject      *object,
				      guint         prop_id,
				      GValue       *value,
				      GParamSpec   *pspec);

static gint     egg_toolbar_expose         (GtkWidget        *widget,
					    GdkEventExpose   *event);
static void     egg_toolbar_realize        (GtkWidget        *widget);
static void     egg_toolbar_unrealize      (GtkWidget        *widget);
static void     egg_toolbar_size_request   (GtkWidget        *widget,
					    GtkRequisition   *requisition);
static void     egg_toolbar_size_allocate  (GtkWidget        *widget,
					    GtkAllocation    *allocation);
static void     egg_toolbar_style_set      (GtkWidget        *widget,
					    GtkStyle         *prev_style);
static void     egg_toolbar_direction_changed (GtkWidget        *widget,
                                               GtkTextDirection  previous_direction);
static gboolean egg_toolbar_focus          (GtkWidget        *widget,
					    GtkDirectionType  dir);
static void     egg_toolbar_screen_changed (GtkWidget        *widget,
					    GdkScreen        *previous_screen);
static void     egg_toolbar_map            (GtkWidget        *widget);
static void     egg_toolbar_unmap          (GtkWidget        *widget);

static void     egg_toolbar_drag_leave  (GtkWidget      *widget,
					 GdkDragContext *context,
					 guint           time_);
static gboolean egg_toolbar_drag_motion (GtkWidget      *widget,
					 GdkDragContext *context,
					 gint            x,
					 gint            y,
					 guint           time_);
static void     egg_toolbar_set_child_property (GtkContainer    *container,
						GtkWidget       *child,
						guint            property_id,
						const GValue    *value,
						GParamSpec      *pspec);
static void    egg_toolbar_get_child_property (GtkContainer    *container,
					       GtkWidget       *child,
					       guint            property_id,
					       GValue          *value,
					       GParamSpec      *pspec);

static void  egg_toolbar_add        (GtkContainer *container,
				     GtkWidget    *widget);
static void  egg_toolbar_remove     (GtkContainer *container,
				     GtkWidget    *widget);
static void  egg_toolbar_forall     (GtkContainer *container,
				     gboolean      include_internals,
				     GtkCallback   callback,
				     gpointer      callback_data);
static GType egg_toolbar_child_type (GtkContainer *container);

static void egg_toolbar_real_orientation_changed (EggToolbar      *toolbar,
						  GtkOrientation   orientation);
static void egg_toolbar_real_style_changed       (EggToolbar      *toolbar,
						  GtkToolbarStyle  style);

static gboolean egg_toolbar_move_focus (EggToolbar       *toolbar,
					GtkDirectionType  dir);
static gboolean egg_toolbar_focus_home (EggToolbar *toolbar);
static gboolean egg_toolbar_focus_end (EggToolbar *toolbar);

static gboolean             egg_toolbar_button_press         (GtkWidget      *button,
							      GdkEventButton *event,
							      EggToolbar     *toolbar);
static gboolean             egg_toolbar_arrow_button_press (GtkWidget      *button,
							      GdkEventButton *event,
							      EggToolbar     *toolbar);
static void                 egg_toolbar_arrow_button_clicked (GtkWidget      *button,
							       EggToolbar     *toolbar);
static void                 egg_toolbar_update_button_relief (EggToolbar     *toolbar);
static GtkReliefStyle       get_button_relief                (EggToolbar     *toolbar);
static gint                 get_space_size                   (EggToolbar     *toolbar);
static GtkToolbarSpaceStyle get_space_style                  (EggToolbar     *toolbar);
static void                 egg_toolbar_remove_tool_item     (EggToolbar     *toolbar,
							      EggToolItem    *item);

static GtkWidget *egg_toolbar_internal_insert_element (EggToolbar          *toolbar,
						       EggToolbarChildType  type,
						       GtkWidget           *widget,
						       const char          *text,
						       const char          *tooltip_text,
						       const char          *tooltip_private_text,
						       GtkWidget           *icon,
						       GtkSignalFunc        callback,
						       gpointer             user_data,
						       gint                 position,
						       gboolean             use_stock);


#define PRIVATE_KEY "egg-toolbar-private"

#define EGG_TOOLBAR_GET_PRIVATE(toolbar) (g_object_get_data (G_OBJECT (toolbar), PRIVATE_KEY))

typedef struct
{
  GList     *items;
  
  GtkWidget *arrow;
  GtkWidget *arrow_button;
  
  gboolean   show_arrow;

  gint       drop_index;
  GdkWindow *drag_highlight;
  GtkMenu   *menu;

  GdkWindow *event_window;
} EggToolbarPrivate;

static GtkContainerClass *parent_class = NULL;
static guint toolbar_signals [LAST_SIGNAL] = { 0 };

GType
egg_toolbar_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggToolbarClass),
	  (GBaseInitFunc) NULL,
	  (GBaseFinalizeFunc) NULL,
	  (GClassInitFunc) egg_toolbar_class_init,
	  (GClassFinalizeFunc) NULL,
	  NULL,
	  sizeof (EggToolbar),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) egg_toolbar_init,
	};

      type = g_type_register_static (GTK_TYPE_CONTAINER,
				     "EggToolbar",
				     &type_info, 0);
    }
  
  return type;
}

static void
add_arrow_bindings (GtkBindingSet   *binding_set,
		    guint            keysym,
		    GtkDirectionType dir)
{
  guint keypad_keysym = keysym - GDK_Left + GDK_KP_Left;
  
  gtk_binding_entry_add_signal (binding_set, keysym, 0,
                                "move_focus", 1,
                                GTK_TYPE_DIRECTION_TYPE, dir);
  gtk_binding_entry_add_signal (binding_set, keypad_keysym, 0,
                                "move_focus", 1,
                                GTK_TYPE_DIRECTION_TYPE, dir);
}

static void
egg_toolbar_class_init (EggToolbarClass *klass)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;
  GtkBindingSet *binding_set;

  parent_class = g_type_class_peek_parent (klass);
  
  gobject_class = (GObjectClass *)klass;
  widget_class = (GtkWidgetClass *)klass;
  container_class = (GtkContainerClass *)klass;
  
  gobject_class->set_property = egg_toolbar_set_property;
  gobject_class->get_property = egg_toolbar_get_property;

  widget_class->expose_event = egg_toolbar_expose;
  widget_class->size_request = egg_toolbar_size_request;
  widget_class->size_allocate = egg_toolbar_size_allocate;
  widget_class->style_set = egg_toolbar_style_set;
  widget_class->direction_changed = egg_toolbar_direction_changed;
  widget_class->focus = egg_toolbar_focus;
  widget_class->screen_changed = egg_toolbar_screen_changed;
  widget_class->realize = egg_toolbar_realize;
  widget_class->unrealize = egg_toolbar_unrealize;
  widget_class->map = egg_toolbar_map;
  widget_class->unmap = egg_toolbar_unmap;
  
  widget_class->drag_leave = egg_toolbar_drag_leave;
  widget_class->drag_motion = egg_toolbar_drag_motion;
  
  container_class->add    = egg_toolbar_add;
  container_class->remove = egg_toolbar_remove;
  container_class->forall = egg_toolbar_forall;
  container_class->child_type = egg_toolbar_child_type;
  container_class->get_child_property = egg_toolbar_get_child_property;
  container_class->set_child_property = egg_toolbar_set_child_property;
  
  klass->orientation_changed = egg_toolbar_real_orientation_changed;
  klass->style_changed = egg_toolbar_real_style_changed;
  klass->move_focus = egg_toolbar_move_focus;
  klass->focus_home = egg_toolbar_focus_home;
  klass->focus_end = egg_toolbar_focus_end;
  
  toolbar_signals[ORIENTATION_CHANGED] =
    g_signal_new ("orientation_changed",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (EggToolbarClass, orientation_changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__ENUM,
		  G_TYPE_NONE, 1,
		  GTK_TYPE_ORIENTATION);
  toolbar_signals[STYLE_CHANGED] =
    g_signal_new ("style_changed",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (EggToolbarClass, style_changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__ENUM,
		  G_TYPE_NONE, 1,
		  GTK_TYPE_TOOLBAR_STYLE);
  toolbar_signals[POPUP_CONTEXT_MENU] =
    g_signal_new ("popup_context_menu",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (EggToolbarClass, popup_context_menu),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
  toolbar_signals[MOVE_FOCUS] =
    g_signal_new ("move_focus",
                  G_TYPE_FROM_CLASS (klass),
                  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
                  G_STRUCT_OFFSET (EggToolbarClass, move_focus),
                  NULL, NULL,
                  _egg_marshal_BOOLEAN__ENUM,
                  G_TYPE_BOOLEAN, 1,
                  GTK_TYPE_DIRECTION_TYPE);
  toolbar_signals[FOCUS_HOME] =
    g_signal_new ("focus_home",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET (EggToolbarClass, focus_home),
		  NULL, NULL,
		  _egg_marshal_BOOLEAN__VOID,
		  G_TYPE_BOOLEAN, 0);
  toolbar_signals[FOCUS_END] =
    g_signal_new ("focus_end",
		  G_OBJECT_CLASS_TYPE (klass),
		  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET (EggToolbarClass, focus_end),
		  NULL, NULL,
		  _egg_marshal_BOOLEAN__VOID,
		  G_TYPE_BOOLEAN, 0);

  /* properties */
  g_object_class_install_property (gobject_class,
				   PROP_ORIENTATION,
				   g_param_spec_enum ("orientation",
 						      _("Orientation"),
 						      _("The orientation of the toolbar"),
 						      GTK_TYPE_ORIENTATION,
 						      GTK_ORIENTATION_HORIZONTAL,
 						      G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
				   PROP_TOOLBAR_STYLE,
				   g_param_spec_enum ("toolbar_style",
 						      _("Toolbar Style"),
 						      _("How to draw the toolbar"),
 						      GTK_TYPE_TOOLBAR_STYLE,
 						      GTK_TOOLBAR_ICONS,
 						      G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
				   PROP_SHOW_ARROW,
				   g_param_spec_boolean ("show_arrow",
							 _("Show Arrow"),
							 _("If an arrow should be shown if the toolbar doesn't fit"),
							 FALSE,
							 G_PARAM_READWRITE));

  /* child properties */
  gtk_container_class_install_child_property (container_class,
					      CHILD_PROP_EXPAND,
					      g_param_spec_boolean ("expand", 
								    _("Expand"), 
								    _("Whether the item should receive extra space when the toolbar grows"),
								    TRUE,
								    G_PARAM_READWRITE));

  gtk_container_class_install_child_property (container_class,
					      CHILD_PROP_HOMOGENEOUS,
					      g_param_spec_boolean ("homogeneous", 
								    _("Homogeneous"), 
								    _("Whether the item should be the same size as other homogeneous items"),
								    TRUE,
								    G_PARAM_READWRITE));

  gtk_container_class_install_child_property (container_class,
					      CHILD_PROP_PACK_END,
					      g_param_spec_uint ("pack_end", 
								 _("Pack End"), 
								 _("Whether the item is positioned at the end of the toolbar"),
								 0, G_MAXINT, 0,
								 G_PARAM_READWRITE));

  /* style properties */
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_int ("space_size",
							     _("Spacer size"),
							     _("Size of spacers"),
							     0,
							     G_MAXINT,
                                                             DEFAULT_SPACE_SIZE,
							     G_PARAM_READABLE));
  
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_int ("internal_padding",
							     _("Internal padding"),
							     _("Amount of border space between the toolbar shadow and the buttons"),
							     0,
							     G_MAXINT,
                                                             DEFAULT_IPADDING,
                                                             G_PARAM_READABLE));

  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_enum ("space_style",
							     _("Space style"),
							     _("Whether spacers are vertical lines or just blank"),
                                                              GTK_TYPE_TOOLBAR_SPACE_STYLE,
                                                              DEFAULT_SPACE_STYLE,
                                                              G_PARAM_READABLE));
  
  gtk_widget_class_install_style_property (widget_class,
					   g_param_spec_enum ("button_relief",
							      _("Button relief"),
							      _("Type of bevel around toolbar buttons"),
                                                              GTK_TYPE_RELIEF_STYLE,
                                                              GTK_RELIEF_NONE,
                                                              G_PARAM_READABLE));
  gtk_widget_class_install_style_property (widget_class,
                                           g_param_spec_enum ("shadow_type",
                                                              _("Shadow type"),
                                                              _("Style of bevel around the toolbar"),
                                                              GTK_TYPE_SHADOW_TYPE,
                                                              GTK_SHADOW_OUT,
                                                              G_PARAM_READABLE));

  gtk_settings_install_property (g_param_spec_enum ("gtk-toolbar-style",
                                                    _("Toolbar style"),
                                                    _("Whether default toolbars have text only, text and icons, icons only, etc."),
                                                    GTK_TYPE_TOOLBAR_STYLE,
                                                    DEFAULT_TOOLBAR_STYLE,
                                                    G_PARAM_READWRITE));

  gtk_settings_install_property (g_param_spec_enum ("gtk-toolbar-icon-size",
                                                    _("Toolbar icon size"),
                                                    _("Size of icons in default toolbars"),
                                                    GTK_TYPE_ICON_SIZE,
                                                    DEFAULT_ICON_SIZE,
                                                    G_PARAM_READWRITE));  

  binding_set = gtk_binding_set_by_class (klass);

  add_arrow_bindings (binding_set, GDK_Left, GTK_DIR_LEFT);
  add_arrow_bindings (binding_set, GDK_Right, GTK_DIR_RIGHT);
  add_arrow_bindings (binding_set, GDK_Up, GTK_DIR_UP);
  add_arrow_bindings (binding_set, GDK_Down, GTK_DIR_DOWN);

  gtk_binding_entry_add_signal (binding_set, GDK_KP_Home, 0,
                                "focus_home", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Home, 0,
                                "focus_home", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_KP_End, 0,
                                "focus_end", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_End, 0,
                                "focus_end", 0);
}

static void
egg_toolbar_init (EggToolbar *toolbar)
{
  EggToolbarPrivate *priv;
  
  GTK_WIDGET_UNSET_FLAGS (toolbar, GTK_CAN_FOCUS);
  GTK_WIDGET_SET_FLAGS (toolbar, GTK_NO_WINDOW);

  priv = g_new0 (EggToolbarPrivate, 1);
  g_object_set_data (G_OBJECT (toolbar), PRIVATE_KEY, priv);
  
  toolbar->orientation = GTK_ORIENTATION_HORIZONTAL;
  toolbar->style = DEFAULT_TOOLBAR_STYLE;
  toolbar->icon_size = DEFAULT_ICON_SIZE;
  toolbar->tooltips = gtk_tooltips_new ();
  g_object_ref (toolbar->tooltips);
  gtk_object_sink (GTK_OBJECT (toolbar->tooltips));
  
  priv->arrow_button = gtk_toggle_button_new ();
  g_signal_connect (priv->arrow_button, "button_press_event",
		    G_CALLBACK (egg_toolbar_arrow_button_press), toolbar);
  g_signal_connect (priv->arrow_button, "clicked",
		    G_CALLBACK (egg_toolbar_arrow_button_clicked), toolbar);
  gtk_button_set_relief (GTK_BUTTON (priv->arrow_button),
			 get_button_relief (toolbar));

#if 0
  /* FIXME: enable this when we can depend on gtk+ 2.3.0 */
  gtk_button_set_focus_on_click (GTK_BUTTON (priv->arrow_button), FALSE);
#endif
  
  priv->arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
  gtk_widget_show (priv->arrow);
  gtk_container_add (GTK_CONTAINER (priv->arrow_button), priv->arrow);
  
  gtk_widget_set_parent (priv->arrow_button, GTK_WIDGET (toolbar));

  g_signal_connect (GTK_WIDGET (toolbar), "button_press_event",
      		    G_CALLBACK (egg_toolbar_button_press), toolbar);

  /* which child position a drop will occur at */
  priv->drop_index = -1;
  priv->drag_highlight = NULL;

  priv->menu = NULL;
}

static void
egg_toolbar_set_property (GObject     *object,
			  guint        prop_id,
			  const GValue *value,
			  GParamSpec   *pspec)
{
  EggToolbar *toolbar = EGG_TOOLBAR (object);

  switch (prop_id)
    {
    case PROP_ORIENTATION:
      egg_toolbar_set_orientation (toolbar, g_value_get_enum (value));
      break;
    case PROP_TOOLBAR_STYLE:
      egg_toolbar_set_style (toolbar, g_value_get_enum (value));
      break;
    case PROP_SHOW_ARROW:
      egg_toolbar_set_show_arrow (toolbar, g_value_get_boolean (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
egg_toolbar_get_property (GObject    *object,
			  guint       prop_id,
			  GValue     *value,
			  GParamSpec *pspec)
{
  EggToolbar *toolbar = EGG_TOOLBAR (object);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  switch (prop_id)
    {
    case PROP_ORIENTATION:
      g_value_set_enum (value, toolbar->orientation);
      break;
    case PROP_TOOLBAR_STYLE:
      g_value_set_enum (value, toolbar->style);
      break;
    case PROP_SHOW_ARROW:
      g_value_set_boolean (value, priv->show_arrow);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
egg_toolbar_map (GtkWidget *widget)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (widget);

  GTK_WIDGET_CLASS (parent_class)->map (widget);

  if (priv->event_window)
    gdk_window_show_unraised (priv->event_window);
}

static void
egg_toolbar_unmap (GtkWidget *widget)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (widget);

  if (priv->event_window)
    gdk_window_hide (priv->event_window);

  GTK_WIDGET_CLASS (parent_class)->unmap (widget);
}

static void
egg_toolbar_paint_space_line (GtkWidget    *widget,
			      GdkRectangle *area,
			      EggToolItem  *item)
{
  EggToolbar *toolbar;
  GtkAllocation *allocation;
  gint space_size;
  
  g_return_if_fail (GTK_BIN (item)->child == NULL);

  toolbar = EGG_TOOLBAR (widget);

  allocation = &GTK_WIDGET (item)->allocation;
  space_size = get_space_size (toolbar);
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    gtk_paint_vline (widget->style, widget->window,
		     GTK_WIDGET_STATE (widget), area, widget,
		     "toolbar",
		     allocation->y +  allocation->height *
		     SPACE_LINE_START / SPACE_LINE_DIVISION,
		     allocation->y + allocation->height *
		     SPACE_LINE_END / SPACE_LINE_DIVISION,
		     allocation->x + (space_size-widget->style->xthickness)/2);
  else if (toolbar->orientation == GTK_ORIENTATION_VERTICAL)
    gtk_paint_hline (widget->style, widget->window,
		     GTK_WIDGET_STATE (widget), area, widget,
		     "toolbar",
		     allocation->x + allocation->width *
		     SPACE_LINE_START / SPACE_LINE_DIVISION,
		     allocation->x + allocation->width *
		     SPACE_LINE_END / SPACE_LINE_DIVISION,
		     allocation->y + (space_size-widget->style->ythickness)/2);
}

static void
egg_toolbar_realize (GtkWidget *widget)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);

  GdkWindowAttr attributes;
  gint attributes_mask;
  gint border_width;

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  border_width = GTK_CONTAINER (widget)->border_width;

  attributes.wclass = GDK_INPUT_ONLY;
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x + border_width;
  attributes.y = widget->allocation.y + border_width;
  attributes.width = widget->allocation.width - border_width * 2;
  attributes.height = widget->allocation.height - border_width * 2;
  attributes.event_mask = gtk_widget_get_events (widget);
  /* FIXME: does GDK_EXPOSURE_MASK make sense for an input-only window?
   * If it doesn't, then it should be removed here and in gtkbutton.c,
   * gtkmenuitem.c, and maybe other places
   */
  attributes.event_mask |= (GDK_EXPOSURE_MASK |
			    GDK_BUTTON_PRESS_MASK |
			    GDK_BUTTON_RELEASE_MASK |
			    GDK_ENTER_NOTIFY_MASK |
			    GDK_LEAVE_NOTIFY_MASK);

  attributes_mask = GDK_WA_X | GDK_WA_Y;

  widget->window = gtk_widget_get_parent_window (widget);
  g_object_ref (widget->window);
  
  priv->event_window = gdk_window_new (gtk_widget_get_parent_window (widget),
				       &attributes, attributes_mask);
  gdk_window_set_user_data (priv->event_window, toolbar);
}

static void
egg_toolbar_unrealize (GtkWidget *widget)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (widget);

  if (priv->drag_highlight)
    {
      gdk_window_set_user_data (priv->drag_highlight, NULL);
      gdk_window_destroy (priv->drag_highlight);
      priv->drag_highlight = NULL;
    }

  if (priv->event_window)
    {
      gdk_window_set_user_data (priv->event_window, NULL);
      gdk_window_destroy (priv->event_window);
      priv->event_window = NULL;
    }

  if (GTK_WIDGET_CLASS (parent_class)->unrealize)
    (* GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static gint
egg_toolbar_expose (GtkWidget      *widget,
		    GdkEventExpose *event)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  GList *items;
  gint border_width;
  
  border_width = GTK_CONTAINER (widget)->border_width;
  
  if (GTK_WIDGET_DRAWABLE (widget))
    {
      GtkShadowType shadow_type;

      gtk_widget_style_get (widget, "shadow_type", &shadow_type, NULL);
      
      gtk_paint_box (widget->style,
		     widget->window,
                     GTK_WIDGET_STATE (widget),
                     shadow_type,
		     &event->area, widget, "toolbar",
		     border_width + widget->allocation.x,
                     border_width + widget->allocation.y,
		     widget->allocation.width - 2 * border_width,
                     widget->allocation.height - 2 * border_width);
    }

  items = priv->items;
  while (items)
    {
      EggToolItem *item = EGG_TOOL_ITEM (items->data);

      if (GTK_BIN (item)->child)
	gtk_container_propagate_expose (GTK_CONTAINER (widget),
					GTK_WIDGET (item),
					event);
      else if (GTK_WIDGET_MAPPED (item) && get_space_style (toolbar) == GTK_TOOLBAR_SPACE_LINE)
	egg_toolbar_paint_space_line (widget, &event->area, item);
      
      items = items->next;
    }

  gtk_container_propagate_expose (GTK_CONTAINER (widget),
				  priv->arrow_button,
				  event);

  return FALSE;
}

static void
egg_toolbar_size_request (GtkWidget      *widget,
			  GtkRequisition *requisition)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  gint space_size = get_space_size (toolbar);
  GList *list;
  gint max_child_height;
  gint max_child_width;
  gint max_homogeneous_child_width;
  gint max_homogeneous_child_height;
  gint homogeneous_size;
  gint long_req;
  gint pack_end_size;
  gint pack_front_size;
  gint ipadding;
  GtkRequisition arrow_requisition;

  max_homogeneous_child_width = 0;
  max_homogeneous_child_height = 0;
  max_child_width = 0;
  max_child_height = 0;
  for (list = priv->items; list != NULL; list = list->next)
    {
      GtkRequisition requisition;
      EggToolItem *item = list->data;
      
      if (!TOOLBAR_ITEM_VISIBLE (item))
	continue;

      gtk_widget_size_request (GTK_WIDGET (item), &requisition);
      
      max_child_width = MAX (max_child_width, requisition.width);
      max_child_height = MAX (max_child_height, requisition.height);

      if (EGG_TOOL_ITEM (item)->homogeneous && GTK_BIN (item)->child)
	{
	  max_homogeneous_child_width = MAX (max_homogeneous_child_width, requisition.width);
	  max_homogeneous_child_height = MAX (max_homogeneous_child_height, requisition.height);
	}
    }
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    homogeneous_size = max_homogeneous_child_width;
  else
    homogeneous_size = max_homogeneous_child_height;
  
  pack_end_size = 0;
  pack_front_size = 0;
  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;
      guint size;
      
      if (!TOOLBAR_ITEM_VISIBLE (item))
	continue;
      
      if (!GTK_BIN (item)->child)
	{
	  size = space_size;
	}
      else if (item->homogeneous)
	{
	  size = homogeneous_size;
	}
      else
	{
	  GtkRequisition requisition;
	  
	  gtk_widget_size_request (GTK_WIDGET (item), &requisition);

	  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
	    size = requisition.width;
	  else
	    size = requisition.height;
	}
      
      if (item->pack_end)
	pack_end_size += size;
      else
	pack_front_size += size;
    }
  
  long_req = pack_end_size;
  
  if (priv->show_arrow)
    {
      gtk_widget_size_request (priv->arrow_button, &arrow_requisition);
      
      if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
	long_req = pack_end_size + MIN (pack_front_size, arrow_requisition.width);
      else
	long_req = pack_end_size + MIN (pack_front_size, arrow_requisition.height);
    }
  else
    {
      arrow_requisition.height = 0;
      arrow_requisition.width = 0;
      
      long_req = pack_end_size + pack_front_size;
    }
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      requisition->width = long_req;
      requisition->height = MAX (max_child_height, arrow_requisition.height);
    }
  else
    {
      requisition->height = long_req;
      requisition->width = MAX (max_child_width, arrow_requisition.width);
    }
  
  /* Extra spacing */
  gtk_widget_style_get (widget, "internal_padding", &ipadding, NULL);

  requisition->width += 2 * (ipadding + GTK_CONTAINER (toolbar)->border_width);
  requisition->height += 2 * (ipadding + GTK_CONTAINER (toolbar)->border_width);
  
  toolbar->button_maxw = max_homogeneous_child_width;
  toolbar->button_maxh = max_homogeneous_child_height;
}

static void
fixup_allocation_for_rtl (gint total_size, GtkAllocation *allocation)
{
  allocation->x += (total_size - (2 * allocation->x + allocation->width));
}

static void
fixup_allocation_for_vertical (GtkAllocation *allocation)
{
  gint tmp;
  
  tmp = allocation->x;
  allocation->x = allocation->y;
  allocation->y = tmp;
  
  tmp = allocation->width;
  allocation->width = allocation->height;
  allocation->height = tmp;
}

static gint
get_item_size (EggToolbar *toolbar, GtkWidget *child)
{
  GtkRequisition requisition;
  EggToolItem *item = EGG_TOOL_ITEM (child);

  if (!GTK_BIN (item)->child)
    return get_space_size (toolbar);

  gtk_widget_get_child_requisition (child, &requisition);
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      if (item->homogeneous)
	return toolbar->button_maxw;
      else
	return requisition.width;
    }
  else
    {
      if (item->homogeneous)
	return toolbar->button_maxh;
      else
	return requisition.height;
    }
}

static void
egg_toolbar_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  gint space_size;
  GtkAllocation *allocations;
  GtkAllocation arrow_allocation;
  gint arrow_size;
  gint size, pos, short_size;
  GList *list;
  gint i;
  gboolean need_arrow;
  gint n_expand_items;
  gint border_width, internal_padding;
  gint available_size;
  gint n_items;
  gint needed_size;
  GList *items;
  GtkRequisition arrow_requisition;

  widget->allocation = *allocation;

  space_size = get_space_size (toolbar);

  border_width = GTK_CONTAINER (toolbar)->border_width;

  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (priv->event_window,
                              allocation->x + border_width,
                              allocation->y + border_width,
                              allocation->width - border_width * 2,
                              allocation->height - border_width * 2);
    }
  
  gtk_widget_style_get (widget, "internal_padding", &internal_padding, NULL);
  border_width += internal_padding;
  
  gtk_widget_get_child_requisition (GTK_WIDGET (priv->arrow_button),
				    &arrow_requisition);
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      available_size = size = allocation->width - 2 * border_width;
      short_size = allocation->height - 2 * border_width;
      arrow_size = arrow_requisition.width;
    }
  else
    {
      available_size = size = allocation->height - 2 * border_width;
      short_size = allocation->width - 2 * border_width;
      arrow_size = arrow_requisition.height;
    }

  n_items = g_list_length (priv->items);
  allocations = g_new0 (GtkAllocation, n_items);

  needed_size = 0;
  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item))
	needed_size += get_item_size (toolbar, GTK_WIDGET (item));
    }

  need_arrow = (needed_size > available_size);

  if (need_arrow)
    size = available_size - arrow_size;
  else
    size = available_size;

  items = g_list_copy (priv->items);

  /* calculate widths of pack end items */
  items = g_list_reverse (items);
  for (list = items, i = 0; list != NULL; list = list->next, ++i)
    {
      EggToolItem *item = list->data;
      GtkAllocation *allocation = &(allocations[n_items - i - 1]);
      gint item_size;
      
      if (!item->pack_end || !TOOLBAR_ITEM_VISIBLE (item))
	continue;

      item_size = get_item_size (toolbar, GTK_WIDGET (item));
      if (item_size <= size)
	{
	  size -= item_size;
	  allocation->width = item_size;
	  item->overflow_item = FALSE;
	}
      else
	{
	  while (list)
	    {
	      item = list->data;
	      if (item->pack_end)
		item->overflow_item = TRUE;
	      
	      list = list->next;
	    }
	  break;
	}
    }
  items = g_list_reverse (items);

  /* calculate widths of pack front items */
  for (list = items, i = 0; list != NULL; list = list->next, ++i)
    {
      EggToolItem *item = list->data;
      gint item_size;

      if (item->pack_end || !TOOLBAR_ITEM_VISIBLE (item))
	continue;

      item_size = get_item_size (toolbar, GTK_WIDGET (item));
      if (item_size <= size)
	{
	  size -= item_size;
	  allocations[i].width = item_size;
	  item->overflow_item = FALSE;
	}
      else
	{
	  while (list)
	    {
	      item = list->data;
	      if (!item->pack_end)
		item->overflow_item = TRUE;
	      list = list->next;
	    }
	  break;
	}
    }

  if (need_arrow)
    {
      arrow_allocation.width = arrow_size;
      arrow_allocation.height = short_size;
    }
  
  /* expand expandable items */
  n_expand_items = 0;
  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item) && item->expand &&
	  !item->overflow_item && GTK_BIN (item)->child)
	{
	  n_expand_items++;
	}
    }
  
  for (list = items, i = 0; list != NULL; list = list->next, ++i)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item) && item->expand &&
	  !item->overflow_item && GTK_BIN (item)->child)
	{
	  gint extra = size / n_expand_items;
	  if (size % n_expand_items != 0)
	    extra++;

	  allocations[i].width += extra;
	  size -= extra;
	  n_expand_items--;
	}
    }

  g_assert (n_expand_items == 0);
  
  /* position pack front items */
  pos = border_width;
  for (list = items, i = 0; list != NULL; list = list->next, ++i)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item) && !item->overflow_item && !item->pack_end)
	{
	  allocations[i].x = pos;
	  allocations[i].y = border_width;
	  allocations[i].height = short_size;

	  pos += allocations[i].width;
	}
    }

  /* position pack end items */
  pos = available_size + border_width;
  items = g_list_reverse (items);
  for (list = items, i = 0; list != NULL; list = list->next, ++i)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item) && !item->overflow_item && item->pack_end)
	{
	  GtkAllocation *allocation = &(allocations[n_items - i - 1]);

	  allocation->x = pos - allocation->width;
	  allocation->y = border_width;
	  allocation->height = short_size;
	  
	  pos -= allocation->width;
	}
    }
  items = g_list_reverse (items);

  /* position arrow */
  if (need_arrow)
    {
      arrow_allocation.x = pos - arrow_allocation.width;
      arrow_allocation.y = border_width;
    }
  
  /* fix up allocations in the vertical or RTL cases */
  if (toolbar->orientation == GTK_ORIENTATION_VERTICAL)
    {
      for (i = 0; i < n_items; ++i)
	fixup_allocation_for_vertical (&(allocations[i]));
      
      if (need_arrow)
	fixup_allocation_for_vertical (&arrow_allocation);
    }
  else if (gtk_widget_get_direction (GTK_WIDGET (toolbar)) == GTK_TEXT_DIR_RTL)
    {
      for (i = 0; i < n_items; ++i)
	fixup_allocation_for_rtl (available_size, &(allocations[i]));

      if (need_arrow)
	fixup_allocation_for_rtl (available_size, &arrow_allocation);
    }
  
  /* translate the items by allocation->(x,y) */
  for (i = 0; i < n_items; ++i)
    {
      allocations[i].x += allocation->x;
      allocations[i].y += allocation->y;
    }

  if (need_arrow)
    {
      arrow_allocation.x += allocation->x;
      arrow_allocation.y += allocation->y;
    }
  
  /* finally allocate the items */
  for (list = items, i = 0; list != NULL; list = list->next, i++)
    {
      EggToolItem *item = list->data;
      
      if (TOOLBAR_ITEM_VISIBLE (item) && !item->overflow_item)
	{
	  gtk_widget_size_allocate (GTK_WIDGET (item), &(allocations[i]));
	  gtk_widget_map (GTK_WIDGET (item));
	}
      else
	{
	  gtk_widget_unmap (GTK_WIDGET (item));
	}
    }

  if (need_arrow)
    {
      gtk_widget_size_allocate (GTK_WIDGET (priv->arrow_button),
				&arrow_allocation);
      gtk_widget_show (GTK_WIDGET (priv->arrow_button));
    }
  else
    {
      gtk_widget_hide (GTK_WIDGET (priv->arrow_button));
    }
  
  g_free (allocations);
  g_list_free (items);
}

static void
egg_toolbar_style_set (GtkWidget *widget,
		       GtkStyle  *prev_style)
{
  if (GTK_WIDGET_REALIZED (widget))
    gtk_style_set_background (widget->style, widget->window, widget->state);

  if (prev_style)
    egg_toolbar_update_button_relief (EGG_TOOLBAR (widget));
}

static void 
egg_toolbar_direction_changed (GtkWidget        *widget,
		   	       GtkTextDirection  previous_dir)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  if (toolbar->orientation == GTK_ORIENTATION_VERTICAL)
    {
      if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_LTR)
	gtk_arrow_set (GTK_ARROW (priv->arrow), GTK_ARROW_RIGHT, GTK_SHADOW_NONE);
      else 
	gtk_arrow_set (GTK_ARROW (priv->arrow), GTK_ARROW_LEFT, GTK_SHADOW_NONE);
    }

  GTK_WIDGET_CLASS (parent_class)->direction_changed (widget, previous_dir);
}

static GList *
egg_toolbar_list_items_in_focus_order (EggToolbar       *toolbar,
				       GtkDirectionType  dir)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GList *result = NULL;
  GList *list;

  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;
      if (!item->pack_end)
	result = g_list_prepend (result, item);
    }

  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;

      if (item->pack_end)
	result = g_list_prepend (result, item);
    }

  result = g_list_prepend (result, priv->arrow_button);
  
  if (dir == GTK_DIR_RIGHT || dir == GTK_DIR_DOWN || dir == GTK_DIR_TAB_FORWARD)
    result = g_list_reverse (result);

  if (gtk_widget_get_direction (GTK_WIDGET (toolbar)) == GTK_TEXT_DIR_RTL)
    result = g_list_reverse (result);

  return result;
}

static gboolean
egg_toolbar_move_focus (EggToolbar       *toolbar,
			GtkDirectionType  dir)
{
  GList *list;
  gboolean retval = FALSE;
  gboolean try_focus = FALSE;
  GList *items = egg_toolbar_list_items_in_focus_order (toolbar, dir);
  
  for (list = items; list != NULL; list = list->next)
    {
      GtkWidget *tool_item = list->data;
      
      if (try_focus && gtk_widget_child_focus (tool_item, dir))
	{
	  retval = TRUE;
	  break;
	}

      if (tool_item == GTK_CONTAINER (toolbar)->focus_child)
	try_focus = TRUE;
    }

  g_list_free (items);
  return retval;
}

static gboolean
egg_toolbar_focus_home (EggToolbar *toolbar)
{
  GList *items, *list;
  GtkTextDirection direction = gtk_widget_get_direction (GTK_WIDGET (toolbar));

  if (direction == GTK_TEXT_DIR_RTL)
    items = egg_toolbar_list_items_in_focus_order (toolbar, GTK_DIR_LEFT);
  else
    items = egg_toolbar_list_items_in_focus_order (toolbar, GTK_DIR_RIGHT);
  
  for (list = items; list != NULL; list = list->next)
    {
      if (GTK_CONTAINER (toolbar)->focus_child == list->data)
	break;

      if (gtk_widget_child_focus (list->data, GTK_DIR_RIGHT))
	break;
    }

  g_list_free (items);
  
  return TRUE;
}

static gboolean
egg_toolbar_focus_end (EggToolbar *toolbar)
{
  GList *items, *list;
  GtkTextDirection direction = gtk_widget_get_direction (GTK_WIDGET (toolbar));

  if (direction == GTK_TEXT_DIR_RTL)
    items = egg_toolbar_list_items_in_focus_order (toolbar, GTK_DIR_RIGHT);
  else
    items = egg_toolbar_list_items_in_focus_order (toolbar, GTK_DIR_LEFT);

  for (list = items; list != NULL; list = list->next)
    {
      if (GTK_CONTAINER (toolbar)->focus_child == list->data)
	break;

      if (gtk_widget_child_focus (list->data, GTK_DIR_RIGHT))
	break;
    }

  g_list_free (items);
  
  return TRUE;
}   

static gboolean
egg_toolbar_focus (GtkWidget        *widget,
		   GtkDirectionType  dir)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  GList *items;
  gboolean retval = FALSE;

  if (GTK_CONTAINER (widget)->focus_child)
    return FALSE;

  items = egg_toolbar_list_items_in_focus_order (toolbar, dir);

  if (items)
    retval = gtk_widget_child_focus (items->data, dir);

  g_list_free (items);
  
  return retval;
}

static void
style_change_notify (EggToolbar *toolbar)
{
  if (!toolbar->style_set)
    {
      /* pretend it was set, then unset, thus reverting to new default */
      toolbar->style_set = TRUE; 
      egg_toolbar_unset_style (toolbar);
    }
}

static void
icon_size_change_notify (EggToolbar *toolbar)
{ 
  if (!toolbar->icon_size_set)
    {
      /* pretend it was set, then unset, thus reverting to new default */
      toolbar->icon_size_set = TRUE; 
      egg_toolbar_unset_icon_size (toolbar);
    }
}

static GtkSettings *
toolbar_get_settings (EggToolbar *toolbar)
{
  return g_object_get_data (G_OBJECT (toolbar), "egg-toolbar-settings");
}

static void
egg_toolbar_screen_changed (GtkWidget *widget,
			    GdkScreen *previous_screen)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  GtkSettings *old_settings = toolbar_get_settings (toolbar);
  GtkSettings *settings;

  if (gtk_widget_has_screen (GTK_WIDGET (toolbar)))
    settings = gtk_widget_get_settings (GTK_WIDGET (toolbar));
  else
    settings = NULL;

  if (settings == old_settings)
    return;

  if (old_settings)
    {
      g_signal_handler_disconnect (old_settings, toolbar->style_set_connection);
      g_signal_handler_disconnect (old_settings, toolbar->icon_size_connection);

      g_object_unref (old_settings);
    }

  if (settings)
    {
      toolbar->style_set_connection =
	g_signal_connect_swapped (settings,
				  "notify::gtk-toolbar-style",
				  G_CALLBACK (style_change_notify),
				  toolbar);
      toolbar->icon_size_connection =
	g_signal_connect_swapped (settings,
				  "notify::gtk-toolbar-icon-size",
				  G_CALLBACK (icon_size_change_notify),
				  toolbar);

      g_object_ref (settings);
      g_object_set_data (G_OBJECT (toolbar), "egg-toolbar-settings", settings);
    }
  else
    g_object_set_data (G_OBJECT (toolbar), "egg-toolbar-settings", NULL);

  style_change_notify (toolbar);
  icon_size_change_notify (toolbar);
}

static void
find_drop_pos(EggToolbar *toolbar, gint x, gint y,
	      gint *drop_index, gint *drop_pos)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GtkOrientation orientation;
  GtkTextDirection direction;
  GList *items;
  EggToolItem *item;
  gint border_width, ipadding;
  gint best_distance, best_pos, best_index, index;

  orientation = toolbar->orientation;
  direction = gtk_widget_get_direction (GTK_WIDGET (toolbar));
  border_width = GTK_CONTAINER (toolbar)->border_width;
  gtk_widget_style_get (GTK_WIDGET (toolbar), "internal_padding",
			&ipadding, NULL);
  border_width += ipadding;

  items = priv->items;
  if (!items)
    {
      *drop_index = 0;
      if (orientation == GTK_ORIENTATION_HORIZONTAL)
	{
	  if (direction == GTK_TEXT_DIR_LTR) 
	    *drop_pos = border_width;
	  else
	    *drop_pos = GTK_WIDGET (toolbar)->allocation.width - border_width;
	}
      else
	{
	  *drop_pos = border_width;
	}
      return;
    }

  /* initial conditions */
  item = EGG_TOOL_ITEM (items->data);
  best_index = 0;
  if (orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      if (direction == GTK_TEXT_DIR_LTR)
	best_pos = GTK_WIDGET (item)->allocation.x;
      else
	best_pos = GTK_WIDGET (item)->allocation.x +
	  GTK_WIDGET (item)->allocation.width;
      best_distance = ABS (best_pos - x);
    }
  else
    {
      best_pos = GTK_WIDGET (item)->allocation.y;
      best_distance = ABS (best_pos - y);
    }

  index = 0;
  while (items)
    {
      item = EGG_TOOL_ITEM (items->data);
      index++;
      if (GTK_WIDGET_DRAWABLE (item) && !item->pack_end)
	{
	  gint pos, distance;

	  if (orientation == GTK_ORIENTATION_HORIZONTAL)
	    {
	      if (direction == GTK_TEXT_DIR_LTR)
		pos = GTK_WIDGET (item)->allocation.x +
		  GTK_WIDGET (item)->allocation.width;
	      else
		pos = GTK_WIDGET (item)->allocation.x;
	      distance = ABS (pos - x);
	    }
	  else
	    {
	      pos = GTK_WIDGET (item)->allocation.y +
		GTK_WIDGET (item)->allocation.height;
	      distance = ABS (pos - y);
	    }
	  if (distance < best_distance)
	    {
	      best_index = index;
	      best_pos = pos;
	      best_distance = distance;
	    }
	}
      items = items->next;
    }
  *drop_index = best_index;
  *drop_pos = best_pos;
}

static void
egg_toolbar_drag_leave (GtkWidget      *widget,
			GdkDragContext *context,
			guint           time_)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);

  if (priv->drag_highlight)
    {
      gdk_window_set_user_data (priv->drag_highlight, NULL);
      gdk_window_destroy (priv->drag_highlight);
      priv->drag_highlight = NULL;
    }

  priv->drop_index = -1;
}

static gboolean
egg_toolbar_drag_motion (GtkWidget      *widget,
			 GdkDragContext *context,
			 gint            x,
			 gint            y,
			 guint           time_)
{
  EggToolbar *toolbar = EGG_TOOLBAR (widget);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  gint new_index, new_pos;

  find_drop_pos(toolbar, x, y, &new_index, &new_pos);

  if (!priv->drag_highlight)
    {
      GdkWindowAttr attributes;
      guint attributes_mask;

      attributes.window_type = GDK_WINDOW_CHILD;
      attributes.wclass = GDK_INPUT_OUTPUT;
      attributes.visual = gtk_widget_get_visual (widget);
      attributes.colormap = gtk_widget_get_colormap (widget);
      attributes.event_mask = GDK_VISIBILITY_NOTIFY_MASK | GDK_EXPOSURE_MASK | GDK_POINTER_MOTION_MASK;
      attributes_mask = GDK_WA_VISUAL | GDK_WA_COLORMAP;
      priv->drag_highlight = gdk_window_new (widget->window,
					     &attributes, attributes_mask);
      gdk_window_set_user_data (priv->drag_highlight, widget);
      gdk_window_set_background (priv->drag_highlight,
      				 &widget->style->fg[widget->state]);
    }

  if (priv->drop_index < 0 ||
      priv->drop_index != new_index)
    {
      gint border_width = GTK_CONTAINER (toolbar)->border_width;
      priv->drop_index = new_index;
      if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
	{
	  gdk_window_move_resize (priv->drag_highlight,
				  new_pos - 1, widget->allocation.y + border_width,
				  2, widget->allocation.height-border_width*2);
	}
      else
	{
	  gdk_window_move_resize (priv->drag_highlight,
				  border_width, new_pos - 1,
				  widget->allocation.width-border_width*2, 2);
	}
    }

  gdk_window_show (priv->drag_highlight);

  gdk_drag_status (context, context->suggested_action, time_);

  return TRUE;
}

static void
egg_toolbar_get_child_property (GtkContainer    *container,
				GtkWidget       *child,
				guint            property_id,
				GValue          *value,
				GParamSpec      *pspec)
{
  EggToolItem *item = EGG_TOOL_ITEM (child);
  
  switch (property_id)
    {
    case CHILD_PROP_PACK_END:
      g_value_set_boolean (value, item->pack_end);
      break;

    case CHILD_PROP_HOMOGENEOUS:
      g_value_set_boolean (value, item->homogeneous);
      break;

    case CHILD_PROP_EXPAND:
      g_value_set_boolean (value, item->expand);
      break;

    default:
      GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID (container, property_id, pspec);
      break;
    }
}

static void
egg_toolbar_set_child_property (GtkContainer    *container,
				GtkWidget       *child,
				guint            property_id,
				const GValue    *value,
				GParamSpec      *pspec)
{
  switch (property_id)
    {
    case CHILD_PROP_PACK_END:
      egg_tool_item_set_pack_end (EGG_TOOL_ITEM (child), g_value_get_boolean (value));
      break;

    case CHILD_PROP_HOMOGENEOUS:
      egg_tool_item_set_homogeneous (EGG_TOOL_ITEM (child), g_value_get_boolean (value));
      break;

    case CHILD_PROP_EXPAND:
      egg_tool_item_set_homogeneous (EGG_TOOL_ITEM (child), g_value_get_boolean (value));
      break;
      
    default:
      GTK_CONTAINER_WARN_INVALID_CHILD_PROPERTY_ID (container, property_id, pspec);
      break;
    }
}

static void
egg_toolbar_add (GtkContainer *container,
		 GtkWidget    *widget)
{
  EggToolbar *toolbar;
  
  g_return_if_fail (EGG_IS_TOOLBAR (container));
  g_return_if_fail (widget != NULL);

  toolbar = EGG_TOOLBAR (container);
  
  if (EGG_IS_TOOL_ITEM (widget))
    egg_toolbar_append (toolbar, EGG_TOOL_ITEM (widget));
  else
    egg_toolbar_append_widget (toolbar, widget, NULL, NULL);
}

static void
egg_toolbar_remove (GtkContainer *container,
		    GtkWidget    *widget)
{
  EggToolbar *toolbar;
  EggToolItem *item = NULL;
  
  g_return_if_fail (EGG_IS_TOOLBAR (container));

  toolbar = EGG_TOOLBAR (container);

  if (EGG_IS_TOOL_ITEM (widget))
    {
      item = EGG_TOOL_ITEM (widget);
    }
  else
    {
      EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
      GList *list;
      
      for (list = priv->items; list != NULL; list = list->next)
	{
	  if (GTK_BIN (list->data)->child == widget)
	    {
	      item = list->data;
	      break;
	    }
	}
    }

  g_return_if_fail (item != NULL);

  egg_toolbar_remove_tool_item (EGG_TOOLBAR (container), item);
}

static void
egg_toolbar_forall (GtkContainer *container,
		    gboolean	  include_internals,
		    GtkCallback   callback,
		    gpointer      callback_data)
{
  EggToolbar *toolbar = EGG_TOOLBAR (container);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GList *items;

  g_return_if_fail (callback != NULL);

  items = priv->items;

  while (items)
    {
      EggToolItem *item = EGG_TOOL_ITEM (items->data);
      
      items = items->next;
      (*callback) (GTK_WIDGET (item), callback_data);
    }

  if (include_internals)
    (* callback) (priv->arrow_button, callback_data);
}

static GType
egg_toolbar_child_type (GtkContainer *container)
{
  return EGG_TYPE_TOOL_ITEM;
}

static void
egg_toolbar_reconfigured (EggToolbar *toolbar)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GList *items;

  items = priv->items;
  while (items)
  {
    EggToolItem *item = EGG_TOOL_ITEM (items->data);
    
    egg_tool_item_toolbar_reconfigured (item);
    
    items = items->next;
  }
}

static void
egg_toolbar_real_orientation_changed (EggToolbar    *toolbar,
				      GtkOrientation orientation)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  if (toolbar->orientation != orientation)
    {
      toolbar->orientation = orientation;

      if (orientation == GTK_ORIENTATION_HORIZONTAL)
	gtk_arrow_set (GTK_ARROW (priv->arrow), GTK_ARROW_DOWN, GTK_SHADOW_NONE);
      else if (gtk_widget_get_direction (GTK_WIDGET (toolbar)) == GTK_TEXT_DIR_LTR)
	gtk_arrow_set (GTK_ARROW (priv->arrow), GTK_ARROW_RIGHT, GTK_SHADOW_NONE);
      else 
	gtk_arrow_set (GTK_ARROW (priv->arrow), GTK_ARROW_LEFT, GTK_SHADOW_NONE);

      egg_toolbar_reconfigured (toolbar);

      gtk_widget_queue_resize (GTK_WIDGET (toolbar));
      g_object_notify (G_OBJECT (toolbar), "orientation");
    }
}

static void
egg_toolbar_real_style_changed (EggToolbar     *toolbar,
				GtkToolbarStyle style)
{
  if (toolbar->style != style)
    {
      toolbar->style = style;

      egg_toolbar_reconfigured (toolbar);

      gtk_widget_queue_resize (GTK_WIDGET (toolbar));
      g_object_notify (G_OBJECT (toolbar), "toolbar_style");
    }
}

static void
menu_position_func (GtkMenu  *menu,
		    gint     *x,
		    gint     *y,
		    gboolean *push_in,
		    gpointer  user_data)
{
  EggToolbar *toolbar = EGG_TOOLBAR (user_data);
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GtkRequisition req;
  GtkRequisition menu_req;
  
  gdk_window_get_origin (GTK_BUTTON (priv->arrow_button)->event_window, x, y);
  gtk_widget_size_request (priv->arrow_button, &req);
  gtk_widget_size_request (GTK_WIDGET (menu), &menu_req);
  
  if (toolbar->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      *y += priv->arrow_button->allocation.height;
      if (gtk_widget_get_direction (GTK_WIDGET (toolbar)) == GTK_TEXT_DIR_LTR) 
	*x += priv->arrow_button->allocation.width - req.width;
      else 
	*x += req.width - menu_req.width;
    }
  else 
    {
      if (gtk_widget_get_direction (GTK_WIDGET (toolbar)) == GTK_TEXT_DIR_LTR) 
	*x += priv->arrow_button->allocation.width;
      else 
	*x -= menu_req.width;
      *y += priv->arrow_button->allocation.height - req.height;      
    }
  
  *push_in = TRUE;
}

static void
menu_deactivated (GtkWidget *menu, EggToolbar *toolbar)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (priv->arrow_button), FALSE);
}

static void
remove_item (GtkWidget *menu_item, gpointer data)
{
  gtk_container_remove (GTK_CONTAINER (menu_item->parent), menu_item);
}

static void
show_menu (EggToolbar *toolbar, GdkEventButton *event)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  GList *list;
  
  if (priv->menu)
    {
      gtk_container_foreach (GTK_CONTAINER (priv->menu), remove_item, NULL);
      gtk_widget_destroy (GTK_WIDGET (priv->menu));
    }

  priv->menu = GTK_MENU (gtk_menu_new ());
  g_signal_connect (priv->menu, "deactivate", G_CALLBACK (menu_deactivated), toolbar);

  for (list = priv->items; list != NULL; list = list->next)
    {
      EggToolItem *item = list->data;

      if (TOOLBAR_ITEM_VISIBLE (item) && item->overflow_item)
	{
	  GtkWidget *menu_item = egg_tool_item_retrieve_proxy_menu_item (item);

	  if (menu_item)
	    {
	      g_assert (GTK_IS_MENU_ITEM (menu_item));
	      gtk_menu_shell_append (GTK_MENU_SHELL (priv->menu), menu_item);
	    }
	}
    }

  gtk_widget_show_all (GTK_WIDGET (priv->menu));

  gtk_menu_popup (GTK_MENU (priv->menu), NULL, NULL,
		  menu_position_func, toolbar,
		  event? event->button : 0, event? event->time : gtk_get_current_event_time());
}

static void
egg_toolbar_arrow_button_clicked (GtkWidget *button, EggToolbar *toolbar)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);  

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (priv->arrow_button)) &&
      (!priv->menu || !GTK_WIDGET_VISIBLE (GTK_WIDGET (priv->menu))))
    {
      /* We only get here when the button is clicked with the keybaord,
       * because mouse button presses result in the menu being shown.
       */
      show_menu (toolbar, NULL);
      gtk_menu_shell_select_first (GTK_MENU_SHELL (priv->menu), FALSE);
    }
}

static gboolean
egg_toolbar_arrow_button_press (GtkWidget      *button,
				GdkEventButton *event,
				EggToolbar     *toolbar)
{
  show_menu (toolbar, event);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
 
  return TRUE;
}

static gboolean
egg_toolbar_button_press (GtkWidget      *button,
    			  GdkEventButton *event,
			  EggToolbar     *toolbar)
{
  if (event->button == 3)
    {
      g_print ("CONTEXT");
      g_signal_emit (toolbar, toolbar_signals[POPUP_CONTEXT_MENU], 0, NULL);
      return FALSE;
    }

  return FALSE;
}

static void
egg_toolbar_update_button_relief (EggToolbar *toolbar)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);

  egg_toolbar_reconfigured (toolbar);

  gtk_button_set_relief (GTK_BUTTON (priv->arrow_button), get_button_relief (toolbar));
}

static GtkReliefStyle
get_button_relief (EggToolbar *toolbar)
{
  GtkReliefStyle button_relief = GTK_RELIEF_NORMAL;

  gtk_widget_ensure_style (GTK_WIDGET (toolbar));
  
  gtk_widget_style_get (GTK_WIDGET (toolbar),
                        "button_relief", &button_relief,
                        NULL);

  return button_relief;
}

static gint
get_space_size (EggToolbar *toolbar)
{
  gint space_size = DEFAULT_SPACE_SIZE;

  gtk_widget_style_get (GTK_WIDGET (toolbar),
                        "space_size", &space_size,
                        NULL);

  return space_size;
}

static GtkToolbarSpaceStyle
get_space_style (EggToolbar *toolbar)
{
  GtkToolbarSpaceStyle space_style = DEFAULT_SPACE_STYLE;

  gtk_widget_style_get (GTK_WIDGET (toolbar),
                        "space_style", &space_style,
                        NULL);


  return space_style;  
}

GtkWidget *
egg_toolbar_new (void)
{
  EggToolbar *toolbar;

  toolbar = g_object_new (EGG_TYPE_TOOLBAR, NULL);

  return GTK_WIDGET (toolbar);
}

void
egg_toolbar_append (EggToolbar  *toolbar,
		    EggToolItem *item)
{
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));
  g_return_if_fail (EGG_IS_TOOL_ITEM (item));

  egg_toolbar_insert (toolbar, item, toolbar->num_children);
}

static void
egg_toolbar_remove_tool_item (EggToolbar  *toolbar,
			      EggToolItem *item)
{
  EggToolbarPrivate *priv;
  GList *tmp;

  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));
  g_return_if_fail (EGG_IS_TOOL_ITEM (item));
  
  priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  for (tmp = priv->items; tmp != NULL; tmp = tmp->next)
    {
      GtkWidget *child = tmp->data;
      
      if (child == GTK_WIDGET (item))
	{
	  gboolean was_visible;
	  
	  was_visible = GTK_WIDGET_VISIBLE (item);
	  gtk_widget_unparent (GTK_WIDGET (item));

	  priv->items = g_list_remove_link (priv->items, tmp);
	  toolbar->num_children--;

	  if (was_visible && GTK_WIDGET_VISIBLE (toolbar))
	    gtk_widget_queue_resize (GTK_WIDGET (toolbar));

	  break;
	}
    }
}

void
egg_toolbar_insert (EggToolbar  *toolbar,
		    EggToolItem *item,
		    gint         pos)
{
  EggToolbarPrivate *priv;
  
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));
  g_return_if_fail (EGG_IS_TOOL_ITEM (item));

  priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  priv->items = g_list_insert (priv->items, item, pos);
  toolbar->num_children++;

  gtk_widget_set_parent (GTK_WIDGET (item), GTK_WIDGET (toolbar));
}

gint
egg_toolbar_get_item_index (EggToolbar  *toolbar,
			    EggToolItem *item)
{
  EggToolbarPrivate *priv;

  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), -1);
  g_return_val_if_fail (EGG_IS_TOOL_ITEM (item), -1);

  priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  g_return_val_if_fail (g_list_find (priv->items, item) != NULL, -1);

  return g_list_index (priv->items, item);
}

void
egg_toolbar_set_orientation (EggToolbar     *toolbar,
			     GtkOrientation  orientation)
{
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  g_signal_emit (toolbar, toolbar_signals[ORIENTATION_CHANGED], 0, orientation);
}

GtkOrientation
egg_toolbar_get_orientation (EggToolbar *toolbar)
{
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);

  return toolbar->orientation;
}

void
egg_toolbar_set_style (EggToolbar      *toolbar,
		       GtkToolbarStyle  style)
{
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  toolbar->style_set = TRUE;  
  g_signal_emit (toolbar, toolbar_signals[STYLE_CHANGED], 0, style);
}

GtkToolbarStyle
egg_toolbar_get_style (EggToolbar *toolbar)
{
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), DEFAULT_TOOLBAR_STYLE);

  return toolbar->style;
}

void
egg_toolbar_unset_style (EggToolbar *toolbar)
{
  GtkToolbarStyle style;

  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  if (toolbar->style_set)
    {
      GtkSettings *settings = toolbar_get_settings (toolbar);

      if (settings)
	g_object_get (settings,
		      "gtk-toolbar-style", &style,
		      NULL);
      else
	style = DEFAULT_TOOLBAR_STYLE;

      if (style != toolbar->style)
	g_signal_emit (toolbar, toolbar_signals[STYLE_CHANGED], 0, style);

      toolbar->style_set = FALSE;
    }
}

void
egg_toolbar_set_tooltips (EggToolbar *toolbar,
			  gboolean    enable)
{
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  if (enable)
    gtk_tooltips_enable (toolbar->tooltips);
  else
    gtk_tooltips_disable (toolbar->tooltips);
}

gboolean
egg_toolbar_get_tooltips (EggToolbar *toolbar)
{
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), FALSE);

  return toolbar->tooltips->enabled;
}

gint
egg_toolbar_get_n_items      (EggToolbar      *toolbar)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);

  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), -1);

  return g_list_length (priv->items);
}

/*
 * returns NULL if n is out of range
 */
EggToolItem *
egg_toolbar_get_nth_item     (EggToolbar      *toolbar,
			      gint             n)
{
  EggToolbarPrivate *priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);

  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), NULL);

  return g_list_nth_data (priv->items, n);
}

void
egg_toolbar_set_icon_size (EggToolbar  *toolbar,
			   GtkIconSize  icon_size)
{
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  toolbar->icon_size_set = TRUE;

  if (toolbar->icon_size == icon_size)
    return;

  toolbar->icon_size = icon_size;

  egg_toolbar_reconfigured (toolbar);

  gtk_widget_queue_resize (GTK_WIDGET (toolbar));
}

GtkIconSize
egg_toolbar_get_icon_size (EggToolbar *toolbar)
{
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), DEFAULT_ICON_SIZE);

  return toolbar->icon_size;
}

GtkReliefStyle
egg_toolbar_get_relief_style (EggToolbar      *toolbar)
{
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), GTK_RELIEF_NONE);

  return get_button_relief (toolbar);
}

void
egg_toolbar_unset_icon_size (EggToolbar *toolbar)
{
  GtkIconSize size;

  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));
  
  if (toolbar->icon_size_set)
    {
      GtkSettings *settings = toolbar_get_settings (toolbar);

      if (settings)
	{
	  g_object_get (settings,
			"gtk-toolbar-icon-size", &size,
			NULL);
	}
      else
	size = DEFAULT_ICON_SIZE;

      if (size != toolbar->icon_size)
	egg_toolbar_set_icon_size (toolbar, size);

      toolbar->icon_size_set = FALSE;
    }
}

void
egg_toolbar_set_show_arrow (EggToolbar *toolbar,
			    gboolean    show_arrow)
{
  EggToolbarPrivate *priv;
  
  g_return_if_fail (EGG_IS_TOOLBAR (toolbar));

  priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  show_arrow = show_arrow != FALSE;

  if (priv->show_arrow != show_arrow)
    {
      priv->show_arrow = show_arrow;
      
      if (!priv->show_arrow)
	gtk_widget_hide (priv->arrow_button);
      
      gtk_widget_queue_resize (GTK_WIDGET (toolbar));      
      g_object_notify (G_OBJECT (toolbar), "show_arrow");
    }
}

gboolean
egg_toolbar_get_show_arrow (EggToolbar *toolbar)
{
  EggToolbarPrivate *priv;

  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), FALSE);

  priv = EGG_TOOLBAR_GET_PRIVATE (toolbar);
  
  return priv->show_arrow;
}

gint
egg_toolbar_get_drop_index (EggToolbar *toolbar,
			    gint        x,
			    gint        y)
{
  gint drop_index, drop_pos;

  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), FALSE);

  find_drop_pos (toolbar, x, y, &drop_index, &drop_pos);

  return drop_index;
}

GtkWidget *
egg_toolbar_append_item (EggToolbar    *toolbar,
			 const char    *text,
			 const char    *tooltip_text,
			 const char    *tooltip_private_text,
			 GtkWidget     *icon,
			 GtkSignalFunc  callback,
			 gpointer       user_data)
{
  return egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_BUTTON,
				     NULL, text,
				     tooltip_text, tooltip_private_text,
				     icon, callback, user_data,
				     toolbar->num_children);
}

GtkWidget *
egg_toolbar_prepend_item (EggToolbar    *toolbar,
			  const char    *text,
			  const char    *tooltip_text,
			  const char    *tooltip_private_text,
			  GtkWidget     *icon,
			  GtkSignalFunc  callback,
			  gpointer       user_data)
{
  return egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_BUTTON,
				     NULL, text,
				     tooltip_text, tooltip_private_text,
				     icon, callback, user_data,
				     0);
}

GtkWidget *
egg_toolbar_insert_item (EggToolbar    *toolbar,
			 const char    *text,
			 const char    *tooltip_text,
			 const char    *tooltip_private_text,
			 GtkWidget     *icon,
			 GtkSignalFunc  callback,
			 gpointer       user_data,
			 gint           position)
{
  return egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_BUTTON,
				     NULL, text,
				     tooltip_text, tooltip_private_text,
				     icon, callback, user_data,
				     position);
}

GtkWidget*
egg_toolbar_insert_stock (EggToolbar      *toolbar,
			  const gchar     *stock_id,
			  const char      *tooltip_text,
			  const char      *tooltip_private_text,
			  GtkSignalFunc    callback,
			  gpointer         user_data,
			  gint             position)
{
  return egg_toolbar_internal_insert_element (toolbar, EGG_TOOLBAR_CHILD_BUTTON,
					      NULL, stock_id,
					      tooltip_text, tooltip_private_text,
					      NULL, callback, user_data,
					      position, TRUE);
}

void
egg_toolbar_append_space (EggToolbar *toolbar)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_SPACE,
			      NULL, NULL,
			      NULL, NULL,
			      NULL, NULL, NULL,
			      toolbar->num_children);
}

void
egg_toolbar_prepend_space (EggToolbar *toolbar)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_SPACE,
			      NULL, NULL,
			      NULL, NULL,
			      NULL, NULL, NULL,
			      0);
}

void
egg_toolbar_insert_space (EggToolbar *toolbar,
			  gint        position)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_SPACE,
			      NULL, NULL,
			      NULL, NULL,
			      NULL, NULL, NULL,
			      position);
}

void
egg_toolbar_append_widget (EggToolbar  *toolbar,
			   GtkWidget   *widget,
			   const gchar *tooltip_text,
			   const gchar *tooltip_private_text)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_WIDGET,
			      widget, NULL,
			      tooltip_text, tooltip_private_text,
			      NULL, NULL, NULL,
			      toolbar->num_children);
}

void
egg_toolbar_prepend_widget (EggToolbar  *toolbar,
			    GtkWidget   *widget,
			    const gchar *tooltip_text,
			    const gchar *tooltip_private_text)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_WIDGET,
			      widget, NULL,
			      tooltip_text, tooltip_private_text,
			      NULL, NULL, NULL,
			      0);
}

void
egg_toolbar_insert_widget (EggToolbar *toolbar,
			   GtkWidget  *widget,
			   const char *tooltip_text,
			   const char *tooltip_private_text,
			   gint        position)
{
  egg_toolbar_insert_element (toolbar, EGG_TOOLBAR_CHILD_WIDGET,
			      widget, NULL,
			      tooltip_text, tooltip_private_text,
			      NULL, NULL, NULL,
			      position);
}

GtkWidget*
egg_toolbar_append_element (EggToolbar          *toolbar,
			    GtkToolbarChildType  type,
			    GtkWidget           *widget,
			    const char          *text,
			    const char          *tooltip_text,
			    const char          *tooltip_private_text,
			    GtkWidget           *icon,
			    GtkSignalFunc        callback,
			    gpointer             user_data)
{
  return egg_toolbar_insert_element (toolbar, type, widget, text,
				     tooltip_text, tooltip_private_text,
				     icon, callback, user_data,
				     toolbar->num_children);
}

GtkWidget *
egg_toolbar_prepend_element (EggToolbar          *toolbar,
			     GtkToolbarChildType  type,
			     GtkWidget           *widget,
			     const char          *text,
			     const char          *tooltip_text,
			     const char          *tooltip_private_text,
			     GtkWidget           *icon,
			     GtkSignalFunc        callback,
			     gpointer             user_data)
{
  return egg_toolbar_insert_element (toolbar, type, widget, text,
				     tooltip_text, tooltip_private_text,
				     icon, callback, user_data, 0);
}

GtkWidget *
egg_toolbar_insert_element (EggToolbar          *toolbar,
			    EggToolbarChildType  type,
			    GtkWidget           *widget,
			    const char          *text,
			    const char          *tooltip_text,
			    const char          *tooltip_private_text,
			    GtkWidget           *icon,
			    GtkSignalFunc        callback,
			    gpointer             user_data,
			    gint                 position)
{
  return egg_toolbar_internal_insert_element (toolbar, type, widget, text,
					      tooltip_text, tooltip_private_text,
					      icon, callback, user_data, position, FALSE);
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

static GtkWidget *
egg_toolbar_internal_insert_element (EggToolbar          *toolbar,
				     EggToolbarChildType  type,
				     GtkWidget           *widget,
				     const char          *text,
				     const char          *tooltip_text,
				     const char          *tooltip_private_text,
				     GtkWidget           *icon,
				     GtkSignalFunc        callback,
				     gpointer             user_data,
				     gint                 position,
				     gboolean             use_stock)
{
  EggToolbarChild *child;
  EggToolItem *item = NULL;
  
  g_return_val_if_fail (EGG_IS_TOOLBAR (toolbar), NULL);
  
  if (type == EGG_TOOLBAR_CHILD_WIDGET)
    g_return_val_if_fail (GTK_IS_WIDGET (widget), NULL);
  else if (type != EGG_TOOLBAR_CHILD_RADIOBUTTON)
    g_return_val_if_fail (widget == NULL, NULL);

  child = g_new (EggToolbarChild, 1);

  child->type = type;
  child->icon = NULL;
  child->label = NULL;

  switch (type)
    {
    case EGG_TOOLBAR_CHILD_SPACE:
      item = egg_separator_tool_item_new ();
      child->widget = NULL;
      break;

    case EGG_TOOLBAR_CHILD_WIDGET:
      item = egg_tool_item_new ();
      child->widget = widget;
      gtk_container_add (GTK_CONTAINER (item), child->widget);
      break;

    case EGG_TOOLBAR_CHILD_BUTTON:
      item = egg_tool_button_new ();
      child->widget = EGG_TOOL_BUTTON (item)->button;
      break;
      
    case EGG_TOOLBAR_CHILD_TOGGLEBUTTON:
      item = egg_toggle_tool_button_new ();
      child->widget = EGG_TOOL_BUTTON (item)->button;
      break;

    case EGG_TOOLBAR_CHILD_RADIOBUTTON:
      item = egg_radio_tool_button_new (widget
					? gtk_radio_button_get_group (GTK_RADIO_BUTTON (widget))
					: NULL);
      child->widget = EGG_TOOL_BUTTON (item)->button;
      break;
    }

  if (type == EGG_TOOLBAR_CHILD_BUTTON ||
      type == EGG_TOOLBAR_CHILD_RADIOBUTTON ||
      type == EGG_TOOLBAR_CHILD_TOGGLEBUTTON)
    {
      if (text)
	{
	  if (use_stock)
	    {
	      GtkStockItem stock_item;
	      gchar *label_text;

	      egg_tool_button_set_stock_id (EGG_TOOL_BUTTON (item), text);

	      gtk_stock_lookup (text, &stock_item);
	      label_text = elide_underscores (stock_item.label);
	      child->label = GTK_WIDGET (gtk_label_new (label_text));
	      g_free (label_text);
	    }
	  else
	    {
	      child->label = gtk_label_new (text);
	    }
	  egg_tool_button_set_label_widget (EGG_TOOL_BUTTON (item), child->label);
	}

      if (icon)
	{
	  child->icon = icon;
	  egg_tool_button_set_icon_widget (EGG_TOOL_BUTTON (item), icon);
	}

      /*
       * We need to connect to the button's clicked callback because some
       * programs may rely on that the widget in the callback is a GtkButton
       */
      if (callback)
	g_signal_connect (child->widget, "clicked",
			  callback, user_data);
    }

  if ((type != GTK_TOOLBAR_CHILD_SPACE) && tooltip_text)
    egg_tool_item_set_tooltip (item, toolbar->tooltips,
			       tooltip_text, tooltip_private_text);
  
  toolbar->children = g_list_insert (toolbar->children, child, position);

  egg_toolbar_insert (toolbar, item, position);

  return child->widget;
}

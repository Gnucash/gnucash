/*
 *  Copyright (C) 2003 Marco Pesenti Gritti
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "egg-editable-toolbar.h"
#include "egg-toolbars-model.h"
#include "eggtoolbar.h"
#include "eggtoolitem.h"
#include "eggseparatortoolitem.h"
#include "eggintl.h"

#include <string.h>

static void egg_editable_toolbar_class_init	(EggEditableToolbarClass *klass);
static void egg_editable_toolbar_init		(EggEditableToolbar *t);
static void egg_editable_toolbar_finalize	(GObject *object);

#define MIN_TOOLBAR_HEIGHT 20

static GtkTargetEntry source_drag_types[] = {
  {EGG_TOOLBAR_ITEM_TYPE, 0, 0},
};
static int n_source_drag_types = G_N_ELEMENTS (source_drag_types);

static GtkTargetEntry dest_drag_types[] = {
  {EGG_TOOLBAR_ITEM_TYPE, 0, 0},
};
static int n_dest_drag_types = G_N_ELEMENTS (dest_drag_types);

enum
{
  PROP_0,
  PROP_TOOLBARS_MODEL,
  PROP_MENU_MERGE
};

enum
{
	ACTION_REQUEST,
	LAST_SIGNAL
};

static guint egg_editable_toolbar_signals[LAST_SIGNAL] = { 0 };

static GObjectClass *parent_class = NULL;

struct EggEditableToolbarPrivate
{
  EggMenuMerge *merge;
  EggToolbarsModel *model;
  gboolean edit_mode;
  GtkWidget *selected_toolbar;
};

GType
egg_editable_toolbar_get_type (void)
{
  static GType egg_editable_toolbar_type = 0;

  if (egg_editable_toolbar_type == 0)
    {
      static const GTypeInfo our_info = {
	sizeof (EggEditableToolbarClass),
	NULL,			/* base_init */
	NULL,			/* base_finalize */
	(GClassInitFunc) egg_editable_toolbar_class_init,
	NULL,
	NULL,			/* class_data */
	sizeof (EggEditableToolbar),
	0,			/* n_preallocs */
	(GInstanceInitFunc) egg_editable_toolbar_init
      };

      egg_editable_toolbar_type = g_type_register_static (GTK_TYPE_VBOX,
							  "EggEditableToolbar",
							  &our_info, 0);
    }

  return egg_editable_toolbar_type;
}

static int
get_toolbar_position (EggEditableToolbar *etoolbar, GtkWidget *toolbar)
{
  GList *l;
  int result;

  l = gtk_container_get_children (GTK_CONTAINER (etoolbar));
  result = g_list_index (l, toolbar);
  g_list_free (l);

  return result;
}

static int
get_n_toolbars (EggEditableToolbar *etoolbar)
{
  GList *l;
  int result;

  l = gtk_container_get_children (GTK_CONTAINER (etoolbar));
  result = g_list_length (l);
  g_list_free (l);

  return result;
}

static GtkWidget *
get_toolbar_nth (EggEditableToolbar *etoolbar,
		 int                 position)
{
  GList *l;
  GtkWidget *result;

  l = gtk_container_get_children (GTK_CONTAINER (etoolbar));
  result = g_list_nth_data (l, position);
  g_list_free (l);

  return result;
}

static EggAction *
find_action (EggEditableToolbar *t,
	     const char         *name)
{
  GList *l = t->priv->merge->action_groups;
  EggAction *action = NULL;

  g_return_val_if_fail (IS_EGG_EDITABLE_TOOLBAR (t), NULL);
  g_return_val_if_fail (name != NULL, NULL);

  for (; l != NULL; l = l->next)
    {
      EggAction *tmp;

      tmp = egg_action_group_get_action (EGG_ACTION_GROUP (l->data), name);
      if (tmp)
	action = tmp;
    }

  return action;
}

static void
drag_data_received_cb (GtkWidget          *widget,
		       GdkDragContext     *context,
		       gint                x,
		       gint                y,
		       GtkSelectionData   *selection_data,
		       guint               info,
		       guint               time_,
		       EggEditableToolbar *etoolbar)
{
  int pos, toolbar_pos;

  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (etoolbar));

  pos = egg_toolbar_get_drop_index (EGG_TOOLBAR (widget), x, y);
  toolbar_pos = get_toolbar_position (etoolbar, widget);

  if (strcmp (selection_data->data, "separator") == 0)
    {
      egg_toolbars_model_add_separator (etoolbar->priv->model,
					toolbar_pos, pos);
    }
  else
    {
      GdkAtom target;
      char *type;
      char *id;

      target = gtk_drag_dest_find_target (widget, context, NULL);
      type = egg_toolbars_model_get_item_type (etoolbar->priv->model, target);
      id = egg_toolbars_model_get_item_id (etoolbar->priv->model, type,
					   selection_data->data);
      egg_toolbars_model_add_item (etoolbar->priv->model,
			           toolbar_pos, pos, id, type);
      g_free (type);
      g_free (id);
    }
}

static void
drag_data_delete_cb (GtkWidget          *widget,
		     GdkDragContext     *context,
		     EggEditableToolbar *etoolbar)
{
  int pos, toolbar_pos;

  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (etoolbar));

  pos = egg_toolbar_get_item_index (EGG_TOOLBAR (widget->parent),
				    EGG_TOOL_ITEM (widget));
  toolbar_pos = get_toolbar_position (etoolbar, widget->parent);

  egg_toolbars_model_remove_item (etoolbar->priv->model,
			          toolbar_pos, pos);
}

static void
drag_data_get_cb (GtkWidget          *widget,
		  GdkDragContext     *context,
		  GtkSelectionData   *selection_data,
		  guint               info,
		  guint32             time,
		  EggEditableToolbar *etoolbar)
{
  EggAction *action;
  const char *target;

  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (etoolbar));

  action = EGG_ACTION (g_object_get_data (G_OBJECT (widget), "egg-action"));

  if (action)
    {
      target = action->name;
    }
  else
    {
      target = "separator";
    }

  gtk_selection_data_set (selection_data,
			  selection_data->target, 8, target, strlen (target));
}

static void
remove_toolbar_cb (GtkWidget          *menuitem,
		   EggEditableToolbar *etoolbar)
{
  int pos;

  pos = get_toolbar_position (etoolbar, etoolbar->priv->selected_toolbar);
  egg_toolbars_model_remove_toolbar (etoolbar->priv->model, pos);
}

static void
popup_toolbar_context_menu_cb (GtkWidget          *toolbar,
			       EggEditableToolbar *t)
{
  GtkWidget *menu;
  GtkWidget *item;
  GtkWidget *image;

  if (t->priv->edit_mode)
    {
      EggTbModelFlags flags;
      int position;

      t->priv->selected_toolbar = toolbar;

      menu = gtk_menu_new ();

      item = gtk_image_menu_item_new_with_mnemonic (_("_Remove Toolbar"));
      gtk_widget_show (item);
      image = gtk_image_new_from_stock (GTK_STOCK_REMOVE, GTK_ICON_SIZE_MENU);
      gtk_widget_show (image);
      gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (item), image);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (item, "activate",
	                G_CALLBACK (remove_toolbar_cb),
		        t);

      position = get_toolbar_position (t, toolbar);
      flags = egg_toolbars_model_get_flags (t->priv->model, position);
      if (flags && EGG_TB_MODEL_NOT_REMOVABLE)
        {
          gtk_widget_set_sensitive (GTK_WIDGET (item), FALSE);
        }

      gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 2,
		      gtk_get_current_event_time ());
    }
}

static GtkWidget *
create_toolbar (EggEditableToolbar *t)
{
  GtkWidget *toolbar;

  toolbar = egg_toolbar_new ();
  egg_toolbar_set_show_arrow (EGG_TOOLBAR (toolbar), TRUE);
  gtk_widget_show (toolbar);
  gtk_drag_dest_set (toolbar, GTK_DEST_DEFAULT_DROP,
		     dest_drag_types, n_dest_drag_types,
		     GDK_ACTION_MOVE | GDK_ACTION_COPY);
  g_signal_connect (toolbar, "drag_data_received",
		    G_CALLBACK (drag_data_received_cb), t);
  g_signal_connect (toolbar, "popup_context_menu",
		    G_CALLBACK (popup_toolbar_context_menu_cb), t);

  return toolbar;
}

static void
set_item_drag_source (GtkWidget *item,
		      EggAction *action,
		      gboolean   is_separator)
{
  gtk_drag_source_set (item, GDK_BUTTON1_MASK,
		       source_drag_types, n_source_drag_types,
		       GDK_ACTION_MOVE);

  if (is_separator)
    {
      GtkWidget *icon;
      GdkPixbuf *pixbuf;

      icon = _egg_editable_toolbar_new_separator_image ();
      pixbuf = gtk_image_get_pixbuf (GTK_IMAGE (icon));
      gtk_drag_source_set_icon_pixbuf (item, pixbuf);
    }
  else
    {
      gtk_drag_source_set_icon_stock
	(item, action->stock_id ? action->stock_id : GTK_STOCK_DND);
    }
}

static GtkWidget *
create_item (EggEditableToolbar *t,
	     EggToolbarsModel   *model,
	     int                 toolbar_position,
	     int                 position)
{
  GtkWidget *item;
  const char *action_name;
  gboolean is_separator;
  EggAction *action;

  action_name = egg_toolbars_model_item_nth
		(model, toolbar_position, position,
		 &is_separator);

  if (is_separator)
    {
      item = GTK_WIDGET (egg_separator_tool_item_new ());
      action = NULL;
    }
  else
    {
      g_signal_emit (G_OBJECT (t), egg_editable_toolbar_signals[ACTION_REQUEST],
		     0, action_name);
      action = find_action (t, action_name);
      item = egg_action_create_tool_item (action);
      gtk_widget_set_sensitive (item, TRUE);
    }

  gtk_widget_show (item);
  g_signal_connect (item, "drag_data_get",
		    G_CALLBACK (drag_data_get_cb), t);
  g_signal_connect (item, "drag_data_delete",
		    G_CALLBACK (drag_data_delete_cb), t);

  if (t->priv->edit_mode)
    {
      set_item_drag_source (item, action, is_separator);
      egg_tool_item_set_use_drag_window (EGG_TOOL_ITEM (item), TRUE);
    }

  return item;
}

static void
toolbar_changed_cb (EggToolbarsModel   *model,
	            int                 position,
	            EggEditableToolbar *t)
{
  GtkWidget *toolbar;
  EggTbModelFlags flags;

  flags = egg_toolbars_model_get_flags (model, position);
  toolbar = get_toolbar_nth (t, position);

  if (flags & EGG_TB_MODEL_ICONS_ONLY)
  {
    egg_toolbar_set_style (EGG_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);
  }
  else
  {
    egg_toolbar_unset_style (EGG_TOOLBAR (toolbar));
  }
}

static void
toolbar_added_cb (EggToolbarsModel   *model,
	          int                 position,
	          EggEditableToolbar *t)
{
  GtkWidget *toolbar;

  toolbar = create_toolbar (t);
  gtk_widget_set_size_request (toolbar, -1, MIN_TOOLBAR_HEIGHT);
  gtk_box_pack_start (GTK_BOX (t), toolbar, FALSE, FALSE, 0);

  gtk_box_reorder_child (GTK_BOX (t), toolbar, position);
}

static void
toolbar_removed_cb (EggToolbarsModel   *model,
	            int                 position,
	            EggEditableToolbar *t)
{
  GtkWidget *toolbar;

  toolbar = get_toolbar_nth (t, position);
  gtk_widget_destroy (toolbar);
}

static void
item_added_cb (EggToolbarsModel   *model,
	       int                 toolbar_position,
	       int                 position,
	       EggEditableToolbar *t)
{
  GtkWidget *toolbar;
  GtkWidget *item;

  toolbar = get_toolbar_nth (t, toolbar_position);
  gtk_widget_set_size_request (toolbar, -1, -1);
  item = create_item (t, model, toolbar_position, position);
  egg_toolbar_insert (EGG_TOOLBAR (toolbar),
		      EGG_TOOL_ITEM (item), position);
}

static void
item_removed_cb (EggToolbarsModel   *model,
	         int                 toolbar_position,
	         int                 position,
	         EggEditableToolbar *t)
{
  GtkWidget *toolbar;
  GtkWidget *item;

  toolbar = get_toolbar_nth (t, toolbar_position);
  item = GTK_WIDGET (egg_toolbar_get_nth_item
	(EGG_TOOLBAR (toolbar), position));
  gtk_container_remove (GTK_CONTAINER (toolbar), item);

  if (egg_toolbars_model_n_items (model, toolbar_position) == 0)
  {
    gtk_widget_set_size_request (toolbar, -1, MIN_TOOLBAR_HEIGHT);
    egg_toolbars_model_remove_toolbar (model, toolbar_position);
  }
}

static void
egg_editable_toolbar_set_model (EggEditableToolbar *t,
				EggToolbarsModel   *model)
{
  g_return_if_fail (IS_EGG_TOOLBARS_MODEL (model));
  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (t));

  t->priv->model = model;

  g_signal_connect_object (model, "item_added",
			   G_CALLBACK (item_added_cb), t, 0);
  g_signal_connect_object (model, "item_removed",
			   G_CALLBACK (item_removed_cb), t, 0);
  g_signal_connect_object (model, "toolbar_added",
			   G_CALLBACK (toolbar_added_cb), t, 0);
  g_signal_connect_object (model, "toolbar_removed",
			   G_CALLBACK (toolbar_removed_cb), t, 0);
  g_signal_connect_object (model, "toolbar_changed",
			   G_CALLBACK (toolbar_changed_cb), t, 0);
}

static void
egg_editable_toolbar_construct (EggEditableToolbar *t)
{
  int i, l, n_items, n_toolbars;
  EggToolbarsModel *model = t->priv->model;

  g_return_if_fail (model != NULL);

  n_toolbars = egg_toolbars_model_n_toolbars (model);

  for (i = 0; i < n_toolbars; i++)
    {
      GtkWidget *toolbar;

      toolbar = create_toolbar (t);
      gtk_box_pack_start (GTK_BOX (t), toolbar, FALSE, FALSE, 0);

      n_items = egg_toolbars_model_n_items (model, i);
      for (l = 0; l < n_items; l++)
        {
          GtkWidget *item;

          item = create_item (t, model, i, l);
	  egg_toolbar_insert (EGG_TOOLBAR (toolbar),
			      EGG_TOOL_ITEM (item), l);
        }

      if (n_items == 0)
        {
            gtk_widget_set_size_request (toolbar, -1, MIN_TOOLBAR_HEIGHT);
        }
    }
}

static void
egg_editable_toolbar_set_merge (EggEditableToolbar *t,
				EggMenuMerge       *merge)
{
  g_return_if_fail (EGG_IS_MENU_MERGE (merge));
  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (t));

  t->priv->merge = merge;

  egg_editable_toolbar_construct (t);
}

static void
egg_editable_toolbar_set_property (GObject      *object,
				   guint         prop_id,
				   const GValue *value,
				   GParamSpec   *pspec)
{
  EggEditableToolbar *t = EGG_EDITABLE_TOOLBAR (object);

  switch (prop_id)
    {
    case PROP_MENU_MERGE:
      egg_editable_toolbar_set_merge (t, g_value_get_object (value));
      break;
    case PROP_TOOLBARS_MODEL:
      egg_editable_toolbar_set_model (t, g_value_get_object (value));
      break;
    }
}

static void
egg_editable_toolbar_get_property (GObject    *object,
				   guint       prop_id,
				   GValue     *value,
				   GParamSpec *pspec)
{
  EggEditableToolbar *t = EGG_EDITABLE_TOOLBAR (object);

  switch (prop_id)
    {
    case PROP_MENU_MERGE:
      g_value_set_object (value, t->priv->merge);
      break;
    case PROP_TOOLBARS_MODEL:
      g_value_set_object (value, t->priv->model);
      break;
    }
}

static void
egg_editable_toolbar_class_init (EggEditableToolbarClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = egg_editable_toolbar_finalize;
  object_class->set_property = egg_editable_toolbar_set_property;
  object_class->get_property = egg_editable_toolbar_get_property;

  egg_editable_toolbar_signals[ACTION_REQUEST] =
    g_signal_new ("action_request",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_LAST,
		  G_STRUCT_OFFSET (EggEditableToolbarClass, action_request),
		  NULL, NULL, g_cclosure_marshal_VOID__STRING,
		  G_TYPE_NONE, 1, G_TYPE_STRING);

  g_object_class_install_property (object_class,
				   PROP_MENU_MERGE,
				   g_param_spec_object ("MenuMerge",
							"MenuMerge",
							"Menu merge",
							EGG_TYPE_MENU_MERGE,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_TOOLBARS_MODEL,
				   g_param_spec_object ("ToolbarsModel",
							"ToolbarsModel",
							"Toolbars Model",
							EGG_TOOLBARS_MODEL_TYPE,
							G_PARAM_READWRITE));
}

static void
egg_editable_toolbar_init (EggEditableToolbar *t)
{
  t->priv = g_new0 (EggEditableToolbarPrivate, 1);

  t->priv->merge = NULL;
  t->priv->edit_mode = FALSE;
}

static void
egg_editable_toolbar_finalize (GObject *object)
{
  EggEditableToolbar *t = EGG_EDITABLE_TOOLBAR (object);

  g_return_if_fail (object != NULL);
  g_return_if_fail (IS_EGG_EDITABLE_TOOLBAR (object));

  g_free (t->priv);

  G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkWidget *
egg_editable_toolbar_new (EggMenuMerge     *merge,
			  EggToolbarsModel *model)
{
  EggEditableToolbar *t;

  t = EGG_EDITABLE_TOOLBAR (g_object_new (EGG_EDITABLE_TOOLBAR_TYPE,
					  "ToolbarsModel", model,
					  "MenuMerge", merge, NULL));

  g_return_val_if_fail (t->priv != NULL, NULL);

  return GTK_WIDGET (t);
}

void
egg_editable_toolbar_set_edit_mode (EggEditableToolbar *etoolbar,
				    gboolean            mode)
{
  int i, l, n_toolbars, n_items;

  etoolbar->priv->edit_mode = mode;

  n_toolbars = get_n_toolbars (etoolbar);
  for (i = 0; i < n_toolbars; i++)
    {
      GtkWidget *toolbar;

      toolbar = get_toolbar_nth (etoolbar, i);
      n_items = egg_toolbar_get_n_items (EGG_TOOLBAR (toolbar));
      for (l = 0; l < n_items; l++)
        {
	  EggToolItem *item;
	  const char *action_name;
          gboolean is_separator;
	  EggAction *action;

          action_name = egg_toolbars_model_item_nth
		(etoolbar->priv->model, i, l,
		 &is_separator);
	  action = find_action (etoolbar, action_name);

	  item = egg_toolbar_get_nth_item (EGG_TOOLBAR (toolbar), l);
	  egg_tool_item_set_use_drag_window (item, mode);

          if (mode)
	    {
	      gtk_widget_set_sensitive (GTK_WIDGET (item), TRUE);
              set_item_drag_source (GTK_WIDGET (item), action, is_separator);
            }
	  else
            {
              gtk_drag_source_unset (GTK_WIDGET (item));

              if (!is_separator)
                {
	          g_object_notify (G_OBJECT (action), "sensitive");
                }
	    }
        }
    }
}

void
egg_editable_toolbar_show (EggEditableToolbar *etoolbar,
			   const char         *name)
{
  int i, n_toolbars;
  EggToolbarsModel *model = etoolbar->priv->model;

  g_return_if_fail (model != NULL);

  n_toolbars = egg_toolbars_model_n_toolbars (model);
  for (i = 0; i < n_toolbars; i++)
    {
      const char *toolbar_name;

      toolbar_name = egg_toolbars_model_toolbar_nth (model, i);
      if (strcmp (toolbar_name, name) == 0)
      {
        gtk_widget_show (get_toolbar_nth (etoolbar, i));
      }
    }
}

void
egg_editable_toolbar_hide (EggEditableToolbar *etoolbar,
			   const char         *name)
{
  int i, n_toolbars;
  EggToolbarsModel *model = etoolbar->priv->model;

  g_return_if_fail (model != NULL);

  n_toolbars = egg_toolbars_model_n_toolbars (model);
  for (i = 0; i < n_toolbars; i++)
    {
      const char *toolbar_name;

      toolbar_name = egg_toolbars_model_toolbar_nth (model, i);
      if (strcmp (toolbar_name, name) == 0)
      {
        gtk_widget_hide (get_toolbar_nth (etoolbar, i));
      }
    }
}

void
egg_editable_toolbar_set_drag_dest (EggEditableToolbar   *etoolbar,
				    const GtkTargetEntry *targets,
				    gint                  n_targets,
				    const char           *toolbar_name)
{
  int i, n_toolbars;
  EggToolbarsModel *model = etoolbar->priv->model;

  g_return_if_fail (model != NULL);

  n_toolbars = egg_toolbars_model_n_toolbars (model);
  for (i = 0; i < n_toolbars; i++)
    {
      const char *name;

      name = egg_toolbars_model_toolbar_nth (model, i);
      if (strcmp (toolbar_name, name) == 0)
      {
        GtkWidget *widget = get_toolbar_nth (etoolbar, i);

        gtk_drag_dest_unset (widget);
        gtk_drag_dest_set (widget, GTK_DEST_DEFAULT_DROP,
                           targets, n_targets,
                           GDK_ACTION_MOVE | GDK_ACTION_COPY);
      }
    }
}

#define DEFAULT_ICON_HEIGHT 20
#define DEFAULT_ICON_WIDTH 0

static void
fake_expose_widget (GtkWidget *widget,
		    GdkPixmap *pixmap)
{
  GdkWindow *tmp_window;
  GdkEventExpose event;

  event.type = GDK_EXPOSE;
  event.window = pixmap;
  event.send_event = FALSE;
  event.area = widget->allocation;
  event.region = NULL;
  event.count = 0;

  tmp_window = widget->window;
  widget->window = pixmap;
  gtk_widget_send_expose (widget, (GdkEvent *) &event);
  widget->window = tmp_window;
}

/* We should probably experiment some more with this.
 * Right now the rendered icon is pretty good for most
 * themes. However, the icon is slightly large for themes
 * with large toolbar icons.
 */
static GdkPixbuf *
new_pixbuf_from_widget (GtkWidget *widget)
{
  GtkWidget *window;
  GdkPixbuf *pixbuf;
  GtkRequisition requisition;
  GtkAllocation allocation;
  GdkPixmap *pixmap;
  GdkVisual *visual;
  gint icon_width;
  gint icon_height;

  icon_width = DEFAULT_ICON_WIDTH;

  if (!gtk_icon_size_lookup_for_settings (gtk_settings_get_default (), 
					  GTK_ICON_SIZE_LARGE_TOOLBAR,
					  NULL, 
					  &icon_height))
    {
      icon_height = DEFAULT_ICON_HEIGHT;
    }

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
  gtk_container_add (GTK_CONTAINER (window), widget);
  gtk_widget_realize (window);
  gtk_widget_show (widget);
  gtk_widget_realize (widget);
  gtk_widget_map (widget);

  /* Gtk will never set the width or height of a window to 0. So setting the width to
   * 0 and than getting it will provide us with the minimum width needed to render
   * the icon correctly, without any additional window background noise.
   * This is needed mostly for pixmap based themes.
   */
  gtk_window_set_default_size (GTK_WINDOW (window), icon_width, icon_height);
  gtk_window_get_size (GTK_WINDOW (window),&icon_width, &icon_height);

  gtk_widget_size_request (window, &requisition);
  allocation.x = 0;
  allocation.y = 0;
  allocation.width = icon_width;
  allocation.height = icon_height;
  gtk_widget_size_allocate (window, &allocation);
  gtk_widget_size_request (window, &requisition);
  
  /* Create a pixmap */
  visual = gtk_widget_get_visual (window);
  pixmap = gdk_pixmap_new (NULL, icon_width, icon_height, gdk_visual_get_best_depth());
  gdk_drawable_set_colormap (GDK_DRAWABLE (pixmap), gtk_widget_get_colormap (window));

  /* Draw the window */
  gtk_widget_ensure_style (window);
  g_assert (window->style);
  g_assert (window->style->font_desc);
  
  fake_expose_widget (window, pixmap);
  fake_expose_widget (widget, pixmap);
  
  pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, icon_width, icon_height);
  gdk_pixbuf_get_from_drawable (pixbuf, pixmap, NULL, 0, 0, 0, 0, icon_width, icon_height);

  return pixbuf;
}

static GdkPixbuf *
new_separator_pixbuf ()
{
  GtkWidget *separator;
  GdkPixbuf *pixbuf;

  separator = gtk_vseparator_new ();
  pixbuf = new_pixbuf_from_widget (separator);
  gtk_widget_destroy (separator);
  return pixbuf;
}

static void
update_separator_image (GtkImage *image)
{
  GdkPixbuf *pixbuf = new_separator_pixbuf ();
  gtk_image_set_from_pixbuf (GTK_IMAGE (image), pixbuf);
  g_object_unref (pixbuf);
}

static gboolean
style_set_cb (GtkWidget *widget,
              GtkStyle *previous_style,
              GtkImage *image)
{

  update_separator_image (image);
  return FALSE;
}

GtkWidget *
_egg_editable_toolbar_new_separator_image (void)
{
  GtkWidget *image = gtk_image_new ();
  update_separator_image (GTK_IMAGE (image));
  g_signal_connect (G_OBJECT (image), "style_set",
		    G_CALLBACK (style_set_cb), GTK_IMAGE (image));

  return image;
}

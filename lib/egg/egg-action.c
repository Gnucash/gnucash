#include "egg-action.h"
#include "eggtoolbutton.h"
#include "eggintl.h"


/* some code for making arbitrary GtkButtons that act like toolbar
 * buttons */
static GtkWidget *tool_button_new       (GType        button_type,
					 const gchar *text,
					 GtkWidget   *icon);
static GtkWidget *tool_button_get_label (GtkWidget   *button);
static GtkWidget *tool_button_get_icon  (GtkWidget   *button);

enum {
  ACTIVATE,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_NAME,
  PROP_LABEL,
  PROP_SHORT_LABEL,
  PROP_TOOLTIP,
  PROP_STOCK_ID,
  PROP_SENSITIVE,
  PROP_VISIBLE,
};

static void egg_action_init       (EggAction *action);
static void egg_action_class_init (EggActionClass *class);

static GQuark       accel_path_id  = 0;
static const gchar *accel_path_key = "EggAction::accel_path";

GType
egg_action_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggActionClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_action_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggAction),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_action_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "EggAction",
				     &type_info, 0);
    }
  return type;
}

static void egg_action_finalize     (GObject *object);
static void egg_action_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void egg_action_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);

static GtkWidget *create_menu_item    (EggAction *action);
static GtkWidget *create_tool_item    (EggAction *action);
static void       connect_proxy       (EggAction *action,
				       GtkWidget *proxy);
static void       disconnect_proxy    (EggAction *action,
				       GtkWidget *proxy);

static GObjectClass *parent_class = NULL;
static guint         action_signals[LAST_SIGNAL] = { 0 };


static void
egg_action_class_init (EggActionClass *class)
{
  GObjectClass *object_class;

  accel_path_id = g_quark_from_static_string(accel_path_key);

  parent_class = g_type_class_peek_parent (class);
  object_class = G_OBJECT_CLASS(class);

  object_class->finalize     = egg_action_finalize;
  object_class->set_property = egg_action_set_property;
  object_class->get_property = egg_action_get_property;

  class->activate = NULL;

  class->create_menu_item = create_menu_item;
  class->create_tool_item = create_tool_item;
  class->connect_proxy = connect_proxy;
  class->disconnect_proxy = disconnect_proxy;

  class->menu_item_type = GTK_TYPE_IMAGE_MENU_ITEM;
  class->toolbar_item_type = EGG_TYPE_TOOL_BUTTON;

  g_object_class_install_property (object_class,
				   PROP_NAME,
				   g_param_spec_string ("name",
							_("Name"),
							_("A unique name for the action."),
							NULL,
							G_PARAM_READWRITE |
							G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (object_class,
				   PROP_LABEL,
				   g_param_spec_string ("label",
							_("Label"),
							_("The label used for menu items and buttons that activate this action."),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_SHORT_LABEL,
				   g_param_spec_string ("short_label",
							_("Short label"),
							_("A shorter label that may be used on toolbar buttons."),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_TOOLTIP,
				   g_param_spec_string ("tooltip",
							_("Tooltip"),
							_("A tooltip for this action."),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_STOCK_ID,
				   g_param_spec_string ("stock_id",
							_("Stock Icon"),
							_("The stock icon displayed in widgets representing this action."),
							NULL,
							G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
				   PROP_SENSITIVE,
				   g_param_spec_boolean ("sensitive",
							 _("Sensitive"),
							 _("Whether the action is enabled."),
							 TRUE,
							 G_PARAM_READWRITE));

  g_object_class_install_property (object_class,
				   PROP_VISIBLE,
				   g_param_spec_boolean ("visible",
							 _("Visible"),
							 _("Whether the action is visible."),
							 TRUE,
							 G_PARAM_READWRITE));

  action_signals[ACTIVATE] =
    g_signal_new ("activate",
		  G_OBJECT_CLASS_TYPE (class),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE,
		  G_STRUCT_OFFSET (EggActionClass, activate),  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
}


static void
egg_action_init (EggAction *action)
{
  action->name = NULL;
  action->label = NULL;
  action->short_label = NULL;
  action->tooltip = NULL;
  action->stock_id = NULL;

  action->sensitive = TRUE;
  action->visible = TRUE;

  action->label_set = FALSE;
  action->short_label_set = FALSE;

  action->accel_quark = 0;

  action->proxies = NULL;
}

static void
egg_action_finalize (GObject *object)
{
  EggAction *action;

  action = EGG_ACTION (object);

  g_free (action->name);
  g_free (action->label);
  g_free (action->short_label);
  g_free (action->tooltip);
  g_free (action->stock_id);
}

static void
egg_action_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  EggAction *action;

  action = EGG_ACTION (object);

  switch (prop_id)
    {
    case PROP_NAME:
      g_free (action->name);
      action->name = g_value_dup_string (value);
      break;
    case PROP_LABEL:
      g_free (action->label);
      action->label = g_value_dup_string (value);
      action->label_set = (action->label != NULL);
      /* if label is unset, then use the label from the stock item */
      if (!action->label_set && action->stock_id)
	{
	  GtkStockItem stock_item;

	  if (gtk_stock_lookup(action->stock_id, &stock_item))
	    action->label = g_strdup(stock_item.label);
	}
      /* if short_label is unset, set short_label=label */
      if (!action->short_label_set)
	{
	  g_free(action->short_label);
	  action->short_label = g_strdup(action->label);
	  g_object_notify(object, "short_label");
	}
      break;
    case PROP_SHORT_LABEL:
      g_free (action->short_label);
      action->short_label = g_value_dup_string (value);
      action->short_label_set = (action->short_label != NULL);
      /* if short_label is unset, then use the value of label */
      if (!action->short_label_set)
	{
	  action->short_label = g_strdup(action->label);
	}
      break;
    case PROP_TOOLTIP:
      g_free (action->tooltip);
      action->tooltip = g_value_dup_string (value);
      break;
    case PROP_STOCK_ID:
      g_free (action->stock_id);
      action->stock_id = g_value_dup_string (value);
      /* update label and short_label if appropriate */
      if (!action->label_set)
	{
	  GtkStockItem stock_item;

	  g_free(action->label);
	  if (gtk_stock_lookup(action->stock_id, &stock_item))
	    action->label = g_strdup(stock_item.label);
	  else
	    action->label = NULL;
	  g_object_notify(object, "label");
	}
      if (!action->short_label_set)
	{
	  g_free(action->short_label);
	  action->short_label = g_strdup(action->label);
	  g_object_notify(object, "short_label");
	}
      break;
    case PROP_SENSITIVE:
      action->sensitive = g_value_get_boolean (value);
      break;
    case PROP_VISIBLE:
      action->visible = g_value_get_boolean (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
egg_action_get_property (GObject    *object,
			 guint       prop_id,
			 GValue     *value,
			 GParamSpec *pspec)
{
  EggAction *action;

  action = EGG_ACTION (object);

  switch (prop_id)
    {
    case PROP_NAME:
      g_value_set_string (value, action->name);
      break;
    case PROP_LABEL:
      g_value_set_string (value, action->label);
      break;
    case PROP_SHORT_LABEL:
      g_value_set_string (value, action->short_label);
      break;
    case PROP_TOOLTIP:
      g_value_set_string (value, action->tooltip);
      break;
    case PROP_STOCK_ID:
      g_value_set_string (value, action->stock_id);
      break;
    case PROP_SENSITIVE:
      g_value_set_boolean (value, action->sensitive);
      break;
    case PROP_VISIBLE:
      g_value_set_boolean (value, action->visible);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static GtkWidget *
create_menu_item (EggAction *action)
{
  GType menu_item_type;

  menu_item_type = EGG_ACTION_GET_CLASS (action)->menu_item_type;

  return g_object_new (menu_item_type, NULL);
}

static GtkWidget *
create_tool_item (EggAction *action)
{
  GType toolbar_item_type;

  toolbar_item_type = EGG_ACTION_GET_CLASS (action)->toolbar_item_type;

  return g_object_new (toolbar_item_type, NULL);
}

static void
egg_action_remove_proxy (GtkWidget *widget, EggAction *action)
{
  action->proxies = g_slist_remove (action->proxies, widget);
}

static void
egg_action_sync_property (EggAction *action, GParamSpec *pspec,
			  GtkWidget *proxy)
{
  const gchar *property;
  GValue value = { 0, };

  property = g_param_spec_get_name (pspec);

  g_value_init(&value, G_PARAM_SPEC_VALUE_TYPE (pspec));
  g_object_get_property (G_OBJECT (action), property, &value);

  g_object_set_property (G_OBJECT (proxy), property, &value);
  g_value_unset (&value);
}

static void
egg_action_sync_label (EggAction *action, GParamSpec *pspec, GtkWidget *proxy)
{
  GtkWidget *label = NULL;

  g_return_if_fail (GTK_IS_MENU_ITEM (proxy));
  label = GTK_BIN(proxy)->child;

  if (GTK_IS_LABEL (label))
    gtk_label_set_label (GTK_LABEL (label), action->label);
}

static void
egg_action_sync_short_label (EggAction *action, GParamSpec *pspec,
			     GtkWidget *proxy)
{
  GValue value = { 0, };

  g_value_init(&value, G_TYPE_STRING);
  g_object_get_property (G_OBJECT (action), "short_label", &value);

  g_object_set_property (G_OBJECT (proxy), "label", &value);
  g_value_unset (&value);
}

static void
egg_action_sync_stock_id (EggAction *action, GParamSpec *pspec,
			  GtkWidget *proxy)
{
  GtkWidget *image = NULL;

  if (GTK_IS_IMAGE_MENU_ITEM (proxy))
    {
      image = gtk_image_menu_item_get_image (GTK_IMAGE_MENU_ITEM (proxy));

      if (GTK_IS_IMAGE (image))
	gtk_image_set_from_stock (GTK_IMAGE (image),
				  action->stock_id, GTK_ICON_SIZE_MENU);
    }
}

static gboolean
egg_action_create_menu_proxy (EggToolItem *tool_item, EggAction *action)
{
  GtkWidget *menu_item = egg_action_create_menu_item (action);

  g_object_ref (menu_item);
  gtk_object_sink (GTK_OBJECT (menu_item));
  
  egg_tool_item_set_proxy_menu_item (tool_item, "egg-action-menu-item", menu_item);
  g_object_unref (menu_item);

  return TRUE;
}

static void
connect_proxy (EggAction *action, GtkWidget *proxy)
{
  g_object_ref (action);
  g_object_set_data_full (G_OBJECT (proxy), "egg-action", action,
			  g_object_unref);

  /* add this widget to the list of proxies */
  action->proxies = g_slist_prepend (action->proxies, proxy);
  g_signal_connect (proxy, "destroy",
		    G_CALLBACK (egg_action_remove_proxy), action);

  g_signal_connect_object (action, "notify::sensitive",
			   G_CALLBACK (egg_action_sync_property), proxy, 0);
  gtk_widget_set_sensitive (proxy, action->sensitive);

  g_signal_connect_object (action, "notify::visible",
			   G_CALLBACK (egg_action_sync_property), proxy, 0);
  if (action->visible)
    gtk_widget_show (proxy);
  else
    gtk_widget_hide (proxy);

  if (GTK_IS_MENU_ITEM (proxy))
    {
      GtkWidget *label;
      /* menu item specific synchronisers ... */
      
      label = GTK_BIN (proxy)->child;

      /* make sure label is a label */
      if (label && !GTK_IS_LABEL (label))
	{
	  gtk_container_remove (GTK_CONTAINER(proxy), label);
	  label = NULL;
	}
      if (!label)
	{
	  label = g_object_new (GTK_TYPE_ACCEL_LABEL,
				"use_underline", TRUE,
				"xalign", 0.0,
				"visible", TRUE,
				"parent", proxy,
				"accel_widget", proxy,
				NULL);
	}
      gtk_label_set_label (GTK_LABEL (label), action->label);
      g_signal_connect_object (action, "notify::label",
			       G_CALLBACK (egg_action_sync_label), proxy, 0);

      if (GTK_IS_IMAGE_MENU_ITEM (proxy))
	{
	  GtkWidget *image;

	  image = gtk_image_menu_item_get_image (GTK_IMAGE_MENU_ITEM (proxy));
	  if (image && !GTK_IS_IMAGE(image))
	    {
	      gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (proxy),NULL);
	      image = NULL;
	    }
	  if (!image)
	    {
	      image = gtk_image_new_from_stock (NULL,
						GTK_ICON_SIZE_MENU);
	      gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (proxy),
					     image);
	      gtk_widget_show (image);
	    }
	  gtk_image_set_from_stock (GTK_IMAGE (image),
				    action->stock_id, GTK_ICON_SIZE_MENU);
	  g_signal_connect_object (action, "notify::stock_id",
				   G_CALLBACK (egg_action_sync_stock_id),
				   proxy, 0);
	}

      if (action->accel_quark)
	{
	  gtk_menu_item_set_accel_path (GTK_MENU_ITEM (proxy),
				g_quark_to_string (action->accel_quark));
	}

      g_signal_connect_object (proxy, "activate",
			       G_CALLBACK (egg_action_activate), action,
			       G_CONNECT_SWAPPED);
    }
  else if (EGG_IS_TOOL_BUTTON (proxy))
    {
      GtkWidget *label;
      GtkWidget *icon;
      /* toolbar button specific synchronisers ... */

      /* synchronise the label */
      g_object_set (G_OBJECT (proxy),
		    "label", action->short_label,
		    "use_underline", TRUE,
		    NULL);
      g_signal_connect_object (action, "notify::short_label",
			       G_CALLBACK (egg_action_sync_short_label),
			       proxy, 0);

      g_object_set (G_OBJECT (proxy), "stock_id", action->stock_id, NULL);
      g_signal_connect_object (action, "notify::stock_id",
			G_CALLBACK (egg_action_sync_property), proxy, 0);

      g_signal_connect_object (proxy, "create_menu_proxy",
			       G_CALLBACK (egg_action_create_menu_proxy),
			       action, 0);

      g_signal_connect_object (proxy, "clicked",
			       G_CALLBACK (egg_action_activate), action,
			       G_CONNECT_SWAPPED);
    }
}

static void
disconnect_proxy (EggAction *action, GtkWidget *proxy)
{
  static guint notify_id = 0;

  if (!notify_id)
    notify_id = g_signal_lookup ("notify", G_TYPE_OBJECT);

  g_object_set_data (G_OBJECT (proxy), "egg-action", NULL);

  /* remove proxy from list of proxies */
  g_signal_handlers_disconnect_by_func (proxy,
					G_CALLBACK (egg_action_remove_proxy),
					action);
  egg_action_remove_proxy (proxy, action);

  /* disconnect the activate handler */
  g_signal_handlers_disconnect_by_func (proxy,
					G_CALLBACK (egg_action_activate),
					action);

  /* disconnect handlers for notify::* signals */
  g_signal_handlers_disconnect_by_func (proxy,
					G_CALLBACK (egg_action_sync_property),
					action);

  g_signal_handlers_disconnect_by_func (action,
				G_CALLBACK (egg_action_sync_stock_id), proxy);

  /* menu item specific synchronisers ... */
  g_signal_handlers_disconnect_by_func (action,
					G_CALLBACK (egg_action_sync_label),
					proxy);

  if (GTK_IS_MENU_ITEM (proxy))
	gtk_menu_item_set_accel_path (GTK_MENU_ITEM (proxy), NULL);

  /* toolbar button specific synchronisers ... */
  g_signal_handlers_disconnect_by_func (action,
			G_CALLBACK (egg_action_sync_short_label),
			proxy);
  g_signal_handlers_disconnect_by_func (proxy,
			G_CALLBACK (egg_action_create_menu_proxy),
			action);
}

/**
 * egg_action_activate:
 * @action: the action object
 *
 * Calling this function will emit the "activate" signal on the
 * specified action.  It gets called by the proxy widgets when they
 * get activated.
 *
 * It can also be used to manually activate an action.
 */
void
egg_action_activate (EggAction *action)
{
  g_signal_emit (action, action_signals[ACTIVATE], 0);
}

/**
 * egg_action_create_icon:
 * @action: the action object
 * @icon_size: the size of the icon that should be created.
 *
 * This function is intended for use by action implementations to
 * create icons displayed in the proxy widgets.
 *
 * Returns: a widget that displays the icon for this action.
 */
GtkWidget *
egg_action_create_icon (EggAction *action, GtkIconSize icon_size)
{
  if (action->stock_id)
    return gtk_image_new_from_stock (action->stock_id, icon_size);
  else
    return NULL;
}

/**
 * egg_action_create_menu_item:
 * @action: the action object
 *
 * This function creates a menu item widget that proxies for the given
 * action.
 *
 * Returns: a menu item connected to the action.
 */
GtkWidget *
egg_action_create_menu_item (EggAction *action)
{
  GtkWidget *menu_item;

  menu_item = (* EGG_ACTION_GET_CLASS (action)->create_menu_item) (action);

  (* EGG_ACTION_GET_CLASS (action)->connect_proxy) (action, menu_item);

  return menu_item;
}

/**
 * egg_action_create_tool_item:
 * @action: the action object
 *
 * This function creates a toolbar item widget that proxies for the
 * given action.
 *
 * Returns: a toolbar item connected to the action.
 */
GtkWidget *
egg_action_create_tool_item (EggAction *action)
{
  GtkWidget *button;

  button = (* EGG_ACTION_GET_CLASS (action)->create_tool_item) (action);

  (* EGG_ACTION_GET_CLASS (action)->connect_proxy) (action, button);

  return button;
}

/**
 * egg_action_connect_proxy:
 * @action: the action object
 * @proxy: the proxy widget
 *
 * This function connects a widget to an action object as a proxy.  It
 * will synchronise various properties of the action with the widget
 * (such as label text, icon, tooltip, etc), and attaches a callback
 * so that the action gets activated when the proxy widget does.
 *
 * If the widget is already connected to an action, it is disconnected
 * first.
 */
void
egg_action_connect_proxy (EggAction *action,
			  GtkWidget *proxy)
{
  EggAction *prev_action;

  g_return_if_fail (EGG_IS_ACTION (action));
  g_return_if_fail (GTK_IS_WIDGET (proxy));

  prev_action = g_object_get_data (G_OBJECT (proxy), "egg-action");

  if (prev_action)
    {
      (* EGG_ACTION_GET_CLASS (action)->disconnect_proxy) (prev_action, proxy);  
    }

  (* EGG_ACTION_GET_CLASS (action)->connect_proxy) (action, proxy);
}

/**
 * egg_action_disconnect_proxy:
 * @action: the action object
 * @proxy: the proxy widget
 *
 * This disconnects a proxy widget from an action.  It does not
 * destroy the widget, however.
 */
void
egg_action_disconnect_proxy (EggAction *action,
			     GtkWidget *proxy)
{
  EggAction *prev_action;

  g_return_if_fail (EGG_IS_ACTION (action));
  g_return_if_fail (GTK_IS_WIDGET (proxy));

  g_return_if_fail (g_object_get_data (G_OBJECT (proxy), "egg-action") != action);

  (* EGG_ACTION_GET_CLASS (action)->disconnect_proxy) (action, proxy);  
}

/**
 * egg_action_block_activate_from:
 * @action: the action object
 * @proxy: a proxy widget
 *
 * Calling this function disables calls to the egg_action_activate()
 * function by signals on the given proxy widget.  This is used to
 * break notification loops for things like check or radio actions.
 *
 * This function is intended for use by action implementations.
 */
void
egg_action_block_activate_from (EggAction *action, GtkWidget *proxy)
{
  g_return_if_fail (EGG_IS_ACTION (action));
  
  g_signal_handlers_block_by_func (proxy, G_CALLBACK (egg_action_activate),
				   action);
}

/**
 * egg_action_unblock_activate_from:
 * @action: the action object
 * @proxy: a proxy widget
 *
 * Calling this function re-enables calls to the egg_action_activate()
 * function by signals on the given proxy widget.  This undoes the
 * blocking done by egg_action_block_activate_from().
 *
 * This function is intended for use by action implementations.
 */
void
egg_action_unblock_activate_from (EggAction *action, GtkWidget *proxy)
{
  g_return_if_fail (EGG_IS_ACTION (action));

  g_signal_handlers_unblock_by_func (proxy, G_CALLBACK (egg_action_activate),
				     action);
}

/**
 * egg_action_set_accel_path:
 * @action: the action object
 * @accel_path: the accelerator path
 *
 * Sets the accel path for this action.  All proxy widgets associated
 * with the action will have this accel path, so that their
 * accelerators are consistent.
 */
void
egg_action_set_accel_path (EggAction *action, const gchar *accel_path)
{
  action->accel_quark = g_quark_from_string(accel_path);
}

/* ---- code to create sort-of-toolbar-buttons ---- */

static GtkWidget *
tool_button_get_label (GtkWidget *button)
{
  g_return_val_if_fail (GTK_IS_BUTTON (button), NULL);

  return g_object_get_data (G_OBJECT (button), "tool-button-label");
}

static GtkWidget *
tool_button_get_icon (GtkWidget *button)
{
  g_return_val_if_fail (GTK_IS_BUTTON (button), NULL);

  return g_object_get_data (G_OBJECT (button), "tool-button-icon");
}

static void
tool_button_parent_set (GtkWidget *button, GtkWidget *old_parent)
{
  GtkWidget *box;
  GtkWidget *label;
  GtkWidget *icon;

  box   = g_object_get_data (G_OBJECT (button), "tool-button-box");
  label = g_object_get_data (G_OBJECT (button), "tool-button-label");
  icon  = g_object_get_data (G_OBJECT (button), "tool-button-icon");

  if (button->parent && GTK_IS_TOOLBAR (button->parent))
    {
      GtkReliefStyle relief = GTK_RELIEF_NORMAL;
      GList *tmp;

      /* set button relief to match toolbar */
      gtk_widget_style_get (GTK_WIDGET (button->parent),
			    "button_relief", &relief, NULL);
      gtk_button_set_relief (GTK_BUTTON (button), relief);

      /* set the button style */
      switch (gtk_toolbar_get_style (GTK_TOOLBAR (button->parent)))
	{
	case GTK_TOOLBAR_ICONS:
	  if (icon && !GTK_WIDGET_VISIBLE (icon))
	    gtk_widget_show (icon);
	  if (label && GTK_WIDGET_VISIBLE (label))
	    gtk_widget_hide (label);
	  break;

	case GTK_TOOLBAR_TEXT:
	  if (icon && GTK_WIDGET_VISIBLE (icon))
	    gtk_widget_hide (icon);
	  if (label && !GTK_WIDGET_VISIBLE (label))
	    gtk_widget_show (label);
	  break;

	case GTK_TOOLBAR_BOTH:
	  if (icon && !GTK_WIDGET_VISIBLE (icon))
	    gtk_widget_show (icon);
	  if (label && !GTK_WIDGET_VISIBLE (label))
	    gtk_widget_show (label);

	  if (GTK_IS_HBOX (box))
	    {
	      if (icon)
		{
		  g_object_ref (icon);
		  gtk_container_remove (GTK_CONTAINER (box), icon);
		}
	      if (label)
		{
		  g_object_ref (label);
		  gtk_container_remove (GTK_CONTAINER (box), label);
		}
	      gtk_container_remove (GTK_CONTAINER (button), box);
	      box = gtk_vbox_new (FALSE, 0);

	      gtk_widget_show (box);

	      if (label)
		{
		  gtk_box_pack_end (GTK_BOX (box), label, FALSE, FALSE, 0);
		  g_object_unref (label);
		}
	      if (icon)
		{
		  gtk_box_pack_end (GTK_BOX (box), icon, FALSE, FALSE, 0);
		  g_object_unref (label);
		}
	      gtk_container_add (GTK_CONTAINER (button), box);
	      g_object_set_data (G_OBJECT (button), "tool-button-box", box);
	    }
	  break;

	case GTK_TOOLBAR_BOTH_HORIZ:
	  if (icon && !GTK_WIDGET_VISIBLE (icon))
	    gtk_widget_show (icon);
	  if (label && !GTK_WIDGET_VISIBLE (label))
	    gtk_widget_show (label);

	  if (GTK_IS_VBOX (box))
	    {
	      if (icon)
		{
		  g_object_ref (icon);
		  gtk_container_remove (GTK_CONTAINER (box), icon);
		}
	      if (label)
		{
		  g_object_ref (label);
		  gtk_container_remove (GTK_CONTAINER (box), label);
		}
	      gtk_container_remove (GTK_CONTAINER (button), box);
	      box = gtk_hbox_new (FALSE, 0);

	      gtk_widget_show (box);

	      if (label)
		{
		  gtk_box_pack_end (GTK_BOX (box), label, FALSE, FALSE, 0);
		  g_object_unref (label);
		}
	      if (icon)
		{
		  gtk_box_pack_end (GTK_BOX (box), icon, FALSE, FALSE, 0);
		  g_object_unref (label);
		}
	      gtk_container_add (GTK_CONTAINER (button), box);
	      g_object_set_data (G_OBJECT (button), "tool-button-box", box);
	    }
	  break;
	}

      /* set the icon size */
      icon = tool_button_get_icon (button);
      if (GTK_IS_IMAGE (icon) &&
	  gtk_image_get_storage_type (GTK_IMAGE (icon)) == GTK_IMAGE_STOCK)
	{
	  gchar *stock_id;

	  gtk_image_get_stock (GTK_IMAGE (icon), &stock_id, NULL);
	  stock_id = g_strdup (stock_id);
	  gtk_image_set_from_stock (GTK_IMAGE (icon), stock_id,
				    GTK_TOOLBAR (button->parent)->icon_size);
	  g_free (stock_id);
	}

      /* gross hack!!! */
      for (tmp = GTK_TOOLBAR (button->parent)->children; tmp; tmp = tmp->next)
	{
	  GtkToolbarChild *tool_child = tmp->data;

	  if (tool_child->widget == button)
	    {
	      tool_child->type  = GTK_TOOLBAR_CHILD_BUTTON;
	      tool_child->icon  = icon;
	      tool_child->label = label;
	      break;
	    }
	}
    }
}

static GtkWidget *
tool_button_new (GType button_type, const gchar *text, GtkWidget *icon)
{
  GtkWidget *button;
  GtkWidget *vbox;
  GtkWidget *label;

  g_return_val_if_fail (g_type_is_a (button_type, GTK_TYPE_BUTTON), NULL);

  button = g_object_new (button_type, NULL);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (button), vbox);
  gtk_widget_show (vbox);

  label = gtk_label_new (text);
  gtk_label_set_use_underline (GTK_LABEL (label), TRUE);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), button);
  gtk_box_pack_end (GTK_BOX (vbox), label, FALSE, FALSE, 0);

  if (!icon)
    icon = gtk_image_new();
  gtk_box_pack_end (GTK_BOX (vbox), icon, FALSE, FALSE, 0);

  g_object_set_data (G_OBJECT (button), "tool-button-box", vbox);
  g_object_set_data (G_OBJECT (button), "tool-button-label", label);
  g_object_set_data (G_OBJECT (button), "tool-button-icon", icon);

  g_signal_connect (button, "parent_set",
		    G_CALLBACK (tool_button_parent_set), NULL);

  GTK_WIDGET_UNSET_FLAGS (button, GTK_CAN_FOCUS);

  return button;
}

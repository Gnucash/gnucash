#include "egg-action-group.h"
#include "egg-toggle-action.h"
#include "egg-radio-action.h"
#include "eggintl.h"

static void egg_action_group_init       (EggActionGroup *self);
static void egg_action_group_class_init (EggActionGroupClass *class);

GType
egg_action_group_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggActionGroupClass),
        (GBaseInitFunc) egg_action_group_init,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_action_group_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggActionGroup),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_action_group_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT, "EggActionGroup",
				     &type_info, 0);
    }

  return type;
}

static GObjectClass *parent_class = NULL;
static void       egg_action_group_finalize        (GObject *object);
static EggAction *egg_action_group_real_get_action (EggActionGroup *self,
						    const gchar *name);

static void
egg_action_group_class_init (EggActionGroupClass *class)
{
  GObjectClass *object_class;

  object_class = G_OBJECT_CLASS (class);
  parent_class = g_type_class_peek_parent (class);

  object_class->finalize = egg_action_group_finalize;
  class->get_action = egg_action_group_real_get_action;
}

static void
egg_action_group_init (EggActionGroup *self)
{
  self->name = NULL;
  self->actions = g_hash_table_new_full (g_str_hash, g_str_equal,
					 (GDestroyNotify) g_free,
					 (GDestroyNotify) g_object_unref);
}

/**
 * egg_action_group_new:
 * @name: the name of the action group
 *
 * Creates a new EggActionGroup object.
 *
 * Returns: the new EggActionGroup
 */
EggActionGroup *
egg_action_group_new(const gchar *name)
{
  EggActionGroup *self;

  self = g_object_new (EGG_TYPE_ACTION_GROUP, NULL);
  self->name = g_strdup (name);

  return self;
}

static void
egg_action_group_finalize (GObject *object)
{
  EggActionGroup *self;

  self = EGG_ACTION_GROUP (object);

  g_free (self->name);
  self->name = NULL;

  g_hash_table_destroy (self->actions);
  self->actions = NULL;

  if (parent_class->finalize)
    (* parent_class->finalize) (object);
}

static EggAction *
egg_action_group_real_get_action (EggActionGroup *self,
				  const gchar *action_name)
{
  return g_hash_table_lookup (self->actions, action_name);
}

/**
 * egg_action_group_get_name:
 * @action_group: the action group
 *
 * Returns: the name of the EggActionGroup
 */
const gchar *
egg_action_group_get_name (EggActionGroup *action_group)
{
  g_return_val_if_fail (EGG_IS_ACTION_GROUP (action_group), NULL);

  return action_group->name;
}

/**
 * egg_action_group_get_action:
 * @action_group: the action group
 * @action_name: the name of the action
 *
 * This function looks up an action in the action group by name.
 *
 * Returns: the action, or NULL if no action by that name exists
 */
EggAction *
egg_action_group_get_action (EggActionGroup *action_group,
			     const gchar *action_name)
{
  g_return_val_if_fail (EGG_IS_ACTION_GROUP (action_group), NULL);
  g_return_val_if_fail (EGG_ACTION_GROUP_GET_CLASS (action_group)->get_action != NULL, NULL);

  return (* EGG_ACTION_GROUP_GET_CLASS (action_group)->get_action)
    (action_group, action_name);
}

/**
 * egg_action_group_add_action:
 * @action_group: the action group
 * @action: an action
 *
 * This function adds an action object to the action group.
 */
void
egg_action_group_add_action (EggActionGroup *action_group,
			     EggAction *action)
{
  g_return_if_fail (EGG_IS_ACTION_GROUP (action_group));
  g_return_if_fail (EGG_IS_ACTION (action));
  g_return_if_fail (action->name != NULL);

  g_hash_table_insert (action_group->actions, g_strdup (action->name),
                       g_object_ref (action));
}

/**
 * egg_action_group_removes_action:
 * @action_group: the action group
 * @action: an action
 *
 * This function removes an action object to the action group.
 */
void
egg_action_group_remove_action (EggActionGroup *action_group,
				EggAction *action)
{
  g_return_if_fail (EGG_IS_ACTION_GROUP (action_group));
  g_return_if_fail (EGG_IS_ACTION (action));
  g_return_if_fail (action->name != NULL);

  /* extra protection to make sure action->name is valid */
  g_object_ref (action);
  g_hash_table_remove (action_group->actions, action->name);
  g_object_unref (action);
}

static void
add_single_action (gpointer key, gpointer value, gpointer user_data)
{
  GList **list = user_data;

  *list = g_list_prepend (*list, value);
}

/**
 * egg_action_group_list_actions:
 * @action_group: the action group
 *
 * Lists the actions in the action group.
 *
 * Returns: an allocated list of the action objects in the action group
 */
GList *
egg_action_group_list_actions (EggActionGroup *action_group)
{
  GList *actions = NULL;
  
  g_hash_table_foreach (action_group->actions, add_single_action, &actions);

  return g_list_reverse (actions);
}


/**
 * egg_action_group_add_actions:
 * @action_group: the action group
 * @entries: an array of action descriptions
 * @n_entries: the number of entries
 * @user_data: data to pass to the action callbacks
 *
 * This is a convenience function to create a number of actions and add them 
 * to the action group.
 *
 * The "activate" signals of the actions are connected to the callbacks and 
 * their accel paths are set to 
 * <literal>&lt;Actions&gt;/<replaceable>group-name</replaceable>/<replaceable>action-name</replaceable></literal>.  
 * 
 * Since: 2.4
 */
void
egg_action_group_add_actions (EggActionGroup *action_group,
			      EggActionEntry *entries,
			      guint           n_entries,
			      gpointer        user_data)
{
  egg_action_group_add_actions_full (action_group, 
				     entries, n_entries, 
				     user_data, NULL);
}


/**
 * egg_action_group_add_actions:
 * @action_group: the action group
 * @entries: an array of action descriptions
 * @n_entries: the number of entries
 *
 * This is a convenience routine to create a number of actions and add
 * them to the action group.  Each member of the array describes an
 * action to create.
 */
void
egg_action_group_add_actions_full (EggActionGroup *action_group,
				   EggActionEntry *entries,
				   guint           n_entries,
				   gpointer        user_data,
				   GDestroyNotify  destroy)
{
  guint i;

  g_return_if_fail (EGG_IS_ACTION_GROUP (action_group));

  for (i = 0; i < n_entries; i++)
    {
      EggAction *action;
      gchar *accel_path;

      action = g_object_new (EGG_TYPE_ACTION,
			     "name", entries[i].name,
			     "label", _(entries[i].label),
			     "tooltip", _(entries[i].tooltip),
			     "stock_id", entries[i].stock_id,
			     NULL);

      if (entries[i].callback)
	g_signal_connect (action, "activate",
			  entries[i].callback, user_data);

      /* set the accel path for the menu item */
      accel_path = g_strconcat ("<Actions>/", action_group->name, "/",
				entries[i].name, NULL);
      if (entries[i].accelerator)
	{
	  guint accel_key = 0;
	  GdkModifierType accel_mods;

	  gtk_accelerator_parse (entries[i].accelerator, &accel_key,
				 &accel_mods);
	  if (accel_key)
	    gtk_accel_map_add_entry (accel_path, accel_key, accel_mods);
	}

      egg_action_set_accel_path (action, accel_path);
      g_free(accel_path);

      egg_action_group_add_action (action_group, action);
      g_object_unref (action);
    }
}

/**
 * egg_action_group_add_toggle_actions:
 * @action_group: the action group
 * @entries: an array of toggle action descriptions
 * @n_entries: the number of entries
 * @user_data: data to pass to the action callbacks
 *
 * This is a convenience function to create a number of toggle actions and add them 
 * to the action group.
 *
 * The "activate" signals of the actions are connected to the callbacks and 
 * their accel paths are set to 
 * <literal>&lt;Actions&gt;/<replaceable>group-name</replaceable>/<replaceable>action-name</replaceable></literal>.  
 * 
 * Since: 2.4
 */
void
egg_action_group_add_toggle_actions (EggActionGroup       *action_group,
				     EggToggleActionEntry *entries,
				     guint                 n_entries,
				     gpointer              user_data)
{
  egg_action_group_add_toggle_actions_full (action_group, 
					    entries, n_entries, 
					    user_data, NULL);
}


/**
 * egg_action_group_add_toggle_actions_full:
 * @action_group: the action group
 * @entries: an array of toggle action descriptions
 * @n_entries: the number of entries
 * @user_data: data to pass to the action callbacks
 * @destroy: destroy notification callback for @user_data
 *
 * This variant of egg_action_group_add_toggle_actions() adds a 
 * #GDestroyNotify callback for @user_data. 
 * 
 * Since: 2.4
 */
void
egg_action_group_add_toggle_actions_full (EggActionGroup       *action_group,
					  EggToggleActionEntry *entries,
					  guint                 n_entries,
					  gpointer              user_data,
					  GDestroyNotify        destroy)
{
  guint i;

  g_return_if_fail (EGG_IS_ACTION_GROUP (action_group));

  for (i = 0; i < n_entries; i++)
    {
      EggAction *action;
      gchar *accel_path;

      action = g_object_new (EGG_TYPE_TOGGLE_ACTION,
			     "name", entries[i].name,
			     "label", entries[i].label,
			     "tooltip", entries[i].tooltip,
			     "stock_id", entries[i].stock_id,
			     NULL);

      egg_toggle_action_set_active (EGG_TOGGLE_ACTION (action), 
				    entries[i].is_active);

      if (entries[i].callback)
	g_signal_connect_data (action, "activate",
			       entries[i].callback, 
			       user_data, (GClosureNotify)destroy, 0);

      /* set the accel path for the menu item */
      accel_path = g_strconcat ("<Actions>/", action_group->name, "/",
				entries[i].name, NULL);
      if (entries[i].accelerator)
	{
	  guint accel_key = 0;
	  GdkModifierType accel_mods;

	  gtk_accelerator_parse (entries[i].accelerator, &accel_key,
				 &accel_mods);
	  if (accel_key)
	    gtk_accel_map_add_entry (accel_path, accel_key, accel_mods);
	}

      egg_action_set_accel_path (action, accel_path);
      g_free (accel_path);

      egg_action_group_add_action (action_group, action);
      g_object_unref (action);
    }
}

/**
 * egg_action_group_add_radio_actions:
 * @action_group: the action group
 * @entries: an array of radio action descriptions
 * @n_entries: the number of entries
 * @value: the value of the action to activate initially, or -1 if
 *   no action should be activated
 * @on_change: the callback to connect to the changed signal
 * @user_data: data to pass to the action callbacks
 * 
 * This is a convenience routine to create a group of radio actions and
 * add them to the action group. 
 *
 * The "changed" signal of the first radio action is connected to the 
 * @on_change callback and the accel paths of the actions are set to 
 * <literal>&lt;Actions&gt;/<replaceable>group-name</replaceable>/<replaceable>action-name</replaceable></literal>.  
 * 
 * Since: 2.4
 **/
void            
egg_action_group_add_radio_actions (EggActionGroup      *action_group,
				    EggRadioActionEntry *entries,
				    guint                n_entries,
				    gint                 value,
				    GCallback            on_change,
				    gpointer             user_data)
{
  egg_action_group_add_radio_actions_full (action_group, 
					   entries, n_entries, 
					   value,
					   on_change, user_data, NULL);
}

/**
 * egg_action_group_add_radio_actions_full:
 * @action_group: the action group
 * @entries: an array of radio action descriptions
 * @n_entries: the number of entries
 * @value: the value of the action to activate initially, or -1 if
 *   no action should be activated
 * @on_change: the callback to connect to the changed signal
 * @user_data: data to pass to the action callbacks
 * @destroy: destroy notification callback for @user_data
 *
 * This variant of egg_action_group_add_radio_actions() adds a 
 * #GDestroyNotify callback for @user_data. 
 * 
 * Since: 2.4
 **/
void            
egg_action_group_add_radio_actions_full (EggActionGroup      *action_group,
					 EggRadioActionEntry *entries,
					 guint                n_entries,
					 gint                 value,
					 GCallback            on_change,
					 gpointer             user_data,
					 GDestroyNotify       destroy)
{
  guint i;
  GSList *group = NULL;
  EggAction *first_action = NULL;

  g_return_if_fail (EGG_IS_ACTION_GROUP (action_group));

  for (i = 0; i < n_entries; i++)
    {
      EggAction *action;
      gchar *accel_path;

      action = g_object_new (EGG_TYPE_RADIO_ACTION,
			     "name", entries[i].name,
			     "label", entries[i].label,
			     "tooltip", entries[i].tooltip,
			     "stock_id", entries[i].stock_id,
			     "value", entries[i].value,
			     NULL);

      if (i == 0) 
	first_action = action;

      if (value == entries[i].value)
	egg_toggle_action_set_active (EGG_TOGGLE_ACTION (action), TRUE);
      
      egg_radio_action_set_group (EGG_RADIO_ACTION (action), group);
      group = egg_radio_action_get_group (EGG_RADIO_ACTION (action));

      /* set the accel path for the menu item */
      accel_path = g_strconcat ("<Actions>/", action_group->name, "/",
				entries[i].name, NULL);
      if (entries[i].accelerator)
	{
	  guint accel_key = 0;
	  GdkModifierType accel_mods;

	  gtk_accelerator_parse (entries[i].accelerator, &accel_key,
				 &accel_mods);
	  if (accel_key)
	    gtk_accel_map_add_entry (accel_path, accel_key, accel_mods);
	}

      egg_action_set_accel_path (action, accel_path);
      g_free (accel_path);
      
      egg_action_group_add_action (action_group, action);
      g_object_unref (action);
    }

  if (on_change)
    g_signal_connect_data (first_action, "changed",
			   on_change, user_data, 
			   (GClosureNotify)destroy, 0);
}


#include "egg-radio-action.h"
#include "eggintl.h"

enum 
{
  CHANGED,
  LAST_SIGNAL
};

enum 
{
  PROP_0,
  PROP_VALUE
};

static void egg_radio_action_init       (EggRadioAction *action);
static void egg_radio_action_class_init (EggRadioActionClass *class);

GType
egg_radio_action_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggRadioActionClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_radio_action_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggRadioAction),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_radio_action_init,
      };

      type = g_type_register_static (EGG_TYPE_TOGGLE_ACTION,
                                     "EggRadioAction",
                                     &type_info, 0);
    }
  return type;
}

static void egg_radio_action_finalize (GObject *object);
static void egg_radio_action_set_property (GObject         *object,
				           guint            prop_id,
				           const GValue    *value,
				           GParamSpec      *pspec);
static void egg_radio_action_get_property (GObject         *object,
				           guint            prop_id,
				           GValue          *value,
				           GParamSpec      *pspec);
static void egg_radio_action_activate (EggAction *action);
static GtkWidget *create_menu_item    (EggAction *action);

static GObjectClass *parent_class = NULL;
static guint         radio_action_signals[LAST_SIGNAL] = { 0 };

static void
egg_radio_action_class_init (EggRadioActionClass *class)
{
  GObjectClass *object_class;
  EggActionClass *action_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = G_OBJECT_CLASS (class);
  action_class = EGG_ACTION_CLASS (class);

  object_class->finalize = egg_radio_action_finalize;
  object_class->set_property = egg_radio_action_set_property;
  object_class->get_property = egg_radio_action_get_property;

  action_class->activate = egg_radio_action_activate;

  action_class->create_menu_item = create_menu_item;

  /**
   * EggRadioAction:value:
   *
   * The value is an arbitrary integer which can be used as a
   * convenient way to determine which action in the group is 
   * currently active in an ::activate or ::changed signal handler.
   * See egg_radio_action_get_current_value() and #EggRadioActionEntry
   * for convenient ways to get and set this property.
   *
   * Since: 2.4
   */
  g_object_class_install_property (object_class,
				   PROP_VALUE,
				   g_param_spec_int ("value",
						     _("The value"),
						     _("The value returned by egg_radio_action_get_current_value() when this action is the current action of its group."),
						     G_MININT,
						     G_MAXINT,
						     0,
						     G_PARAM_READWRITE));

  /**
   * EggRadioAction::changed:
   * @action: the action on which the signal is emitted
   * @current: the member of @action<!-- -->s group which has just been activated
   *
   * The ::changed signal is emitted on every member of a radio group when the
   * active member is changed. The signal gets emitted after the ::activate signals
   * for the previous and current active members.
   *
   * Since: 2.4
   */
  radio_action_signals[CHANGED] =
    g_signal_new ("changed",
		  G_OBJECT_CLASS_TYPE (class),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_NO_RECURSE,
		  G_STRUCT_OFFSET (EggRadioActionClass, changed),  NULL, NULL,
		  g_cclosure_marshal_VOID__OBJECT,
		  G_TYPE_NONE, 1, EGG_TYPE_RADIO_ACTION);
}

static void
egg_radio_action_init (EggRadioAction *action)
{
  action->group = g_slist_prepend (NULL, action);
}

static void
egg_radio_action_finalize (GObject *object)
{
  EggRadioAction *action;
  GSList *tmp_list;

  g_return_if_fail (EGG_IS_RADIO_ACTION (object));

  action = EGG_RADIO_ACTION (object);

  action->group = g_slist_remove (action->group, action);

  tmp_list = action->group;

  while (tmp_list)
    {
      EggRadioAction *tmp_action = tmp_list->data;

      tmp_list = tmp_list->next;
      tmp_action->group = action->group;
    }

  if (parent_class->finalize)
    (* parent_class->finalize) (object);
}

static void
egg_radio_action_set_property (GObject         *object,
			       guint            prop_id,
			       const GValue    *value,
			       GParamSpec      *pspec)
{
  EggRadioAction *radio_action;
  
  radio_action = EGG_RADIO_ACTION (object);

  switch (prop_id)
    {
    case PROP_VALUE:
      radio_action->value = g_value_get_int (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
egg_radio_action_get_property (GObject    *object,
			       guint       prop_id,
			       GValue     *value,
			       GParamSpec *pspec)
{
  EggRadioAction *radio_action;

  radio_action = EGG_RADIO_ACTION (object);

  switch (prop_id)
    {
    case PROP_VALUE:
      g_value_set_int (value, radio_action->value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
egg_radio_action_activate (EggAction *action)
{
  EggRadioAction *radio_action;
  EggToggleAction *toggle_action;
  EggToggleAction *tmp_action;
  GSList *tmp_list;

  g_return_if_fail (EGG_IS_RADIO_ACTION (action));

  radio_action = EGG_RADIO_ACTION (action);
  toggle_action = EGG_TOGGLE_ACTION (action);

  if (toggle_action->active)
    {
      tmp_list = radio_action->group;

      while (tmp_list)
	{
	  tmp_action = tmp_list->data;
	  tmp_list = tmp_list->next;

	  if (tmp_action->active && (tmp_action != toggle_action)) 
	    {
	      toggle_action->active = !toggle_action->active;
	      break;
	    }
	}
    }
  else
    {
      toggle_action->active = !toggle_action->active;

      tmp_list = radio_action->group;
      while (tmp_list)
	{
	  tmp_action = tmp_list->data;
	  tmp_list = tmp_list->next;

	  if (tmp_action->active && (tmp_action != toggle_action))
	    {
	      egg_action_activate (EGG_ACTION (tmp_action));
	      break;
	    }
	}

      tmp_list = radio_action->group;
      while (tmp_list)
	{
	  tmp_action = tmp_list->data;
	  tmp_list = tmp_list->next;
	  
	  g_signal_emit (tmp_action, radio_action_signals[CHANGED], 0, radio_action);
	}
    }

  egg_toggle_action_toggled (toggle_action);
}

static GtkWidget *
create_menu_item (EggAction *action)
{
  return g_object_new (GTK_TYPE_CHECK_MENU_ITEM, 
		       NULL);
}

/**
 * egg_radio_action_get_group:
 * @action: the action object
 *
 * Returns: the list representing the radio group for this object
 */
GSList *
egg_radio_action_get_group (EggRadioAction *action)
{
  g_return_val_if_fail (EGG_IS_RADIO_ACTION (action), NULL);

  return action->group;
}

/**
 * egg_radio_action_set_group:
 * @action: the action object
 * @group: a list representing a radio group
 *
 * Sets the radio group for the radio action object.
 */
void
egg_radio_action_set_group (EggRadioAction *action, GSList *group)
{
  g_return_if_fail (EGG_IS_RADIO_ACTION (action));
  g_return_if_fail (!g_slist_find (group, action));

  if (action->group)
    {
      GSList *slist;

      action->group = g_slist_remove (action->group, action);

      for (slist = action->group; slist; slist = slist->next)
	{
	  EggRadioAction *tmp_action = slist->data;

	  tmp_action->group = action->group;
	}
    }

  action->group = g_slist_prepend (group, action);

  if (group)
    {
      GSList *slist;

      for (slist = action->group; slist; slist = slist->next)
	{
	  EggRadioAction *tmp_action = slist->data;

	  tmp_action->group = action->group;
	}
    }
  else
    {
      EGG_TOGGLE_ACTION (action)->active = TRUE;
    }
}

/**
 * egg_radio_action_get_current_value:
 * @action: a #EggRadioAction
 * 
 * Obtains the value property of the the currently active member of 
 * the group to which @action belongs.
 * 
 * Return value: The value of the currently active group member
 *
 * Since: 2.4
 **/
gint
egg_radio_action_get_current_value (EggRadioAction *action)
{
  GSList *slist;

  g_return_val_if_fail (EGG_IS_RADIO_ACTION (action), 0);

  if (action->group)
    {
      for (slist = action->group; slist; slist = slist->next)
	{
	  EggToggleAction *toggle_action = slist->data;

	  if (toggle_action->active)
	    return EGG_RADIO_ACTION (toggle_action)->value;
	}
    }

  return action->value;
}

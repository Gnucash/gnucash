#include "egg-radio-action.h"

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
static void egg_radio_action_activate (EggAction *action);

static GObjectClass *parent_class = NULL;

static void
egg_radio_action_class_init (EggRadioActionClass *class)
{
  GObjectClass *object_class;
  EggActionClass *action_class;

  parent_class = g_type_class_peek_parent (class);
  object_class = G_OBJECT_CLASS (class);
  action_class = EGG_ACTION_CLASS (class);

  object_class->finalize = egg_radio_action_finalize;

  action_class->activate = egg_radio_action_activate;
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
    }

  egg_toggle_action_toggled (toggle_action);
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

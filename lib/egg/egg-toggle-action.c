#include "egg-toggle-action.h"
#include "eggtoggletoolbutton.h"

enum {
  TOGGLED,
  LAST_SIGNAL
};

static void egg_toggle_action_init       (EggToggleAction *action);
static void egg_toggle_action_class_init (EggToggleActionClass *class);

GType
egg_toggle_action_get_type (void)
{
  static GtkType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
      {
        sizeof (EggToggleActionClass),
        (GBaseInitFunc) NULL,
        (GBaseFinalizeFunc) NULL,
        (GClassInitFunc) egg_toggle_action_class_init,
        (GClassFinalizeFunc) NULL,
        NULL,
        
        sizeof (EggToggleAction),
        0, /* n_preallocs */
        (GInstanceInitFunc) egg_toggle_action_init,
      };

      type = g_type_register_static (EGG_TYPE_ACTION,
                                     "EggToggleAction",
                                     &type_info, 0);
    }
  return type;
}

static void egg_toggle_action_activate     (EggAction *action);
static void egg_toggle_action_real_toggled (EggToggleAction *action);
static void connect_proxy                  (EggAction *action,
					    GtkWidget *proxy);
static void disconnect_proxy               (EggAction *action,
					    GtkWidget *proxy);

static GObjectClass *parent_class = NULL;
static guint         action_signals[LAST_SIGNAL] = { 0 };

static void
egg_toggle_action_class_init (EggToggleActionClass *class)
{
  EggActionClass *action_class;

  parent_class = g_type_class_peek_parent (class);
  action_class = EGG_ACTION_CLASS (class);

  action_class->activate = egg_toggle_action_activate;
  action_class->connect_proxy = connect_proxy;
  action_class->disconnect_proxy = disconnect_proxy;

  action_class->menu_item_type = GTK_TYPE_CHECK_MENU_ITEM;
  action_class->toolbar_item_type = EGG_TYPE_TOGGLE_TOOL_BUTTON;

  class->toggled = egg_toggle_action_real_toggled;

  action_signals[TOGGLED] =
    g_signal_new ("toggled",
                  G_OBJECT_CLASS_TYPE (class),
                  G_SIGNAL_RUN_FIRST,
                  G_STRUCT_OFFSET (EggToggleActionClass, toggled),
		  NULL, NULL,
                  g_cclosure_marshal_VOID__VOID,
                  G_TYPE_NONE, 0);
}

static void
egg_toggle_action_init (EggToggleAction *action)
{
  action->active = FALSE;
}

static void
egg_toggle_action_activate (EggAction *action)
{
  EggToggleAction *toggle_action;

  g_return_if_fail (EGG_IS_TOGGLE_ACTION (action));

  toggle_action = EGG_TOGGLE_ACTION (action);

  toggle_action->active = !toggle_action->active;

  egg_toggle_action_toggled (toggle_action);
}

static void
egg_toggle_action_real_toggled (EggToggleAction *action)
{
  GSList *slist;

  g_return_if_fail (EGG_IS_TOGGLE_ACTION (action));

  for (slist = EGG_ACTION (action)->proxies; slist; slist = slist->next)
    {
      GtkWidget *proxy = slist->data;

      egg_action_block_activate_from (EGG_ACTION (action), proxy);
      if (GTK_IS_CHECK_MENU_ITEM (proxy))
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (proxy),
					action->active);
      else if (EGG_IS_TOGGLE_TOOL_BUTTON (proxy))
	egg_toggle_tool_button_set_active (EGG_TOGGLE_TOOL_BUTTON (proxy),
					   action->active);
      else {
	g_warning ("Don't know how to toggle `%s' widgets",
		   G_OBJECT_TYPE_NAME (proxy));
      }
      egg_action_unblock_activate_from (EGG_ACTION (action), proxy);
    }
}

static void
connect_proxy (EggAction *action, GtkWidget *proxy)
{
  EggToggleAction *toggle_action;

  toggle_action = EGG_TOGGLE_ACTION (action);

  /* do this before hand, so that we don't call the "activate" handler */
  if (GTK_IS_MENU_ITEM (proxy))
    gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (proxy),
				    toggle_action->active);
  else if (EGG_IS_TOGGLE_TOOL_BUTTON (proxy))
    egg_toggle_tool_button_set_active (EGG_TOGGLE_TOOL_BUTTON (proxy),
				       toggle_action->active);

  (* EGG_ACTION_CLASS (parent_class)->connect_proxy) (action, proxy);
}

static void
disconnect_proxy (EggAction *action, GtkWidget *proxy)
{
  EggToggleAction *toggle_action;

  toggle_action = EGG_TOGGLE_ACTION (action);

  (* EGG_ACTION_CLASS (parent_class)->disconnect_proxy) (action, proxy);
}

/**
 * egg_toggle_action_toggled:
 * @action: the action object
 *
 * Emits the "toggled" signal on the toggle action.
 */
void
egg_toggle_action_toggled (EggToggleAction *action)
{
  g_return_if_fail (EGG_IS_TOGGLE_ACTION (action));

  g_signal_emit (action, action_signals[TOGGLED], 0);
}

/**
 * egg_toggle_action_set_active:
 * @action: the action object
 * @is_active: whether the action should be checked or not
 *
 * Sets the checked state on the toggle action.
 */
void
egg_toggle_action_set_active (EggToggleAction *action, gboolean is_active)
{
  g_return_if_fail (EGG_IS_TOGGLE_ACTION (action));

  is_active = is_active != 0;

  if (action->active != is_active)
    {
      egg_action_activate (EGG_ACTION (action));
    }
}

/**
 * egg_toggle_action_get_active:
 * @action: the action object
 *
 * Returns: the checked state of the toggle action
 */
gboolean
egg_toggle_action_get_active (EggToggleAction *action)
{
  g_return_val_if_fail (EGG_IS_TOGGLE_ACTION (action), FALSE);

  return action->active;
}
